
/*---------------------------------------------------------------*/
/*--- begin                           host_loongarch64_isel.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2021-2022 Loongson Technology Corporation Limited

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details->

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "host_generic_regs.h"
#include "host_loongarch64_defs.h"


/*---------------------------------------------------------*/
/*--- ISelEnv                                           ---*/
/*---------------------------------------------------------*/

/* This carries around:

   - A mapping from IRTemp to IRType, giving the type of any IRTemp we
     might encounter.  This is computed before insn selection starts,
     and does not change.

   - A mapping from IRTemp to HReg.  This tells the insn selector
     which virtual register is associated with each IRTemp temporary.
     This is computed before insn selection starts, and does not
     change.  We expect this mapping to map precisely the same set of
     IRTemps as the type mapping does.

     |vregmap|   holds the primary register for the IRTemp.
     |vregmapHI| is only used for 128-bit integer-typed
                 IRTemps.  It holds the identity of a second
                 64-bit virtual HReg, which holds the high half
                 of the value.

   - The code array, that is, the insns selected so far.

   - A counter, for generating new virtual registers.

   - The host hardware capabilities word.  This is set at the start
     and does not change.

   - A Bool for indicating whether we may generate chain-me
     instructions for control flow transfers, or whether we must use
     XAssisted.

   - The maximum guest address of any guest insn in this block.
     Actually, the address of the highest-addressed byte from any insn
     in this block.  Is set at the start and does not change.  This is
     used for detecting jumps which are definitely forward-edges from
     this block, and therefore can be made (chained) to the fast entry
     point of the destination, thereby avoiding the destination's
     event check.

    - An IRExpr*, which may be NULL, holding the IR expression (an
      IRRoundingMode-encoded value) to which the FPU's rounding mode
      was most recently set.  Setting to NULL is always safe.  Used to
      avoid redundant settings of the FPU's rounding mode, as
      described in set_FPCR_rounding_mode below.

   Note, this is all (well, mostly) host-independent.
*/

typedef
   struct {
      /* Constant -- are set at the start and do not change. */
      IRTypeEnv*   type_env;

      HReg*        vregmap;
      HReg*        vregmapHI;
      Int          n_vregmap;

      UInt         hwcaps;

      Bool         chainingAllowed;
      Addr64       max_ga;

      /* These are modified as we go along. */
      HInstrArray* code;
      Int          vreg_ctr;
   }
   ISelEnv;


static HReg lookupIRTemp ( ISelEnv* env, IRTemp tmp )
{
   vassert(tmp >= 0);
   vassert(tmp < env->n_vregmap);
   return env->vregmap[tmp];
}

static void lookupIRTempPair ( HReg* vrHI, HReg* vrLO,
                               ISelEnv* env, IRTemp tmp )
{
   vassert(tmp < env->n_vregmap);
   vassert(!hregIsInvalid(env->vregmapHI[tmp]));
   *vrLO = env->vregmap[tmp];
   *vrHI = env->vregmapHI[tmp];
}

static void addInstr ( ISelEnv* env, LOONGARCH64Instr* instr )
{
   addHInstr(env->code, instr);
   if (vex_traceflags & VEX_TRACE_VCODE) {
      ppLOONGARCH64Instr(instr, True);
      vex_printf("\n");
   }
}

static HReg newVRegI ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcInt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}

static HReg newVRegF ( ISelEnv* env )
{
   HReg reg = mkHReg(True/*virtual reg*/, HRcFlt64, 0, env->vreg_ctr);
   env->vreg_ctr++;
   return reg;
}


/*---------------------------------------------------------*/
/*--- ISEL: Forward declarations                        ---*/
/*---------------------------------------------------------*/

/* These are organised as iselXXX and iselXXX_wrk pairs.  The
   iselXXX_wrk do the real work, but are not to be called directly.
   For each XXX, iselXXX calls its iselXXX_wrk counterpart, then
   checks that all returned registers are virtual.  You should not
   call the _wrk version directly.
*/

static LOONGARCH64AMode*   iselIntExpr_AMode_wrk ( ISelEnv* env,
                                                   IRExpr* e, IRType dty );
static LOONGARCH64AMode*   iselIntExpr_AMode     ( ISelEnv* env,
                                                   IRExpr* e, IRType dty );

static LOONGARCH64RI*      iselIntExpr_RI_wrk    ( ISelEnv* env, IRExpr* e,
                                                   UChar size, Bool isSigned );
static LOONGARCH64RI*      iselIntExpr_RI        ( ISelEnv* env, IRExpr* e,
                                                   UChar size, Bool isSigned );

static HReg                iselIntExpr_R_wrk     ( ISelEnv* env, IRExpr* e );
static HReg                iselIntExpr_R         ( ISelEnv* env, IRExpr* e );

static HReg                iselCondCode_R_wrk    ( ISelEnv* env, IRExpr* e );
static HReg                iselCondCode_R        ( ISelEnv* env, IRExpr* e );

static void                iselInt128Expr_wrk    ( HReg* hi, HReg* lo,
                                                   ISelEnv* env, IRExpr* e );
static void                iselInt128Expr        ( HReg* hi, HReg* lo,
                                                   ISelEnv* env, IRExpr* e );

static HReg                iselFltExpr_wrk        ( ISelEnv* env, IRExpr* e );
static HReg                iselFltExpr            ( ISelEnv* env, IRExpr* e );


/*---------------------------------------------------------*/
/*--- ISEL: Misc helpers                                ---*/
/*---------------------------------------------------------*/

/* Generate move insn */
static LOONGARCH64Instr* LOONGARCH64Instr_Move ( HReg to, HReg from )
{
   LOONGARCH64RI *ri = LOONGARCH64RI_R(hregZERO());
   return LOONGARCH64Instr_Binary(LAbin_OR, ri, from, to);
}

/* Generate LOONGARCH64AMode from HReg and UInt */
static LOONGARCH64AMode* mkLOONGARCH64AMode_RI ( HReg reg, UInt imm )
{
   vassert(imm < (1 << 12));
   return LOONGARCH64AMode_RI(reg, (UShort)imm);
}

/* Set floating point rounding mode */
static void set_rounding_mode ( ISelEnv* env, IRExpr* mode )
{
   /*
      rounding mode | LOONGARCH | IR
      ------------------------------
      to nearest    | 00        | 00
      to zero       | 01        | 11
      to +infinity  | 10        | 10
      to -infinity  | 11        | 01
   */

   /* rm = XOR(rm, (rm << 1)) & 3 */
   HReg            rm = iselIntExpr_R(env, mode);
   HReg           tmp = newVRegI(env);
   LOONGARCH64RI*  ri = LOONGARCH64RI_I(1, 5, False);
   LOONGARCH64RI* ri2 = LOONGARCH64RI_R(rm);
   LOONGARCH64RI* ri3 = LOONGARCH64RI_I(3, 12, False);
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri, rm, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_XOR, ri2, tmp, rm));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri3, rm, rm));

   /* Save old value of FCSR3 */
   HReg fcsr = newVRegI(env);
   addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFCSR2GR,
                                         hregLOONGARCH64_FCSR3(), fcsr));

   /* Store old FCSR3 to stack */
   LOONGARCH64RI* ri4 = LOONGARCH64RI_I(-4 & 0xfff, 12, True);
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ADDI_D, ri4, hregSP(), hregSP()));
   LOONGARCH64AMode* am = LOONGARCH64AMode_RI(hregSP(), 0);
   addInstr(env, LOONGARCH64Instr_Store(LAstore_ST_W, am, fcsr));

   /* Set new value of FCSR3 */
   LOONGARCH64RI* ri5 = LOONGARCH64RI_I(8, 5, False);
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri5, rm, rm));
   addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVGR2FCSR,
                                         rm, hregLOONGARCH64_FCSR3()));
}

static void set_rounding_mode_default ( ISelEnv* env )
{
   /* Load old FCSR3 from stack */
   HReg fcsr = newVRegI(env);
   LOONGARCH64AMode* am = LOONGARCH64AMode_RI(hregSP(), 0);
   addInstr(env, LOONGARCH64Instr_Load(LAload_LD_WU, am, fcsr));

   /* Restore SP */
   LOONGARCH64RI* ri = LOONGARCH64RI_I(4, 12, True);
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ADDI_D, ri, hregSP(), hregSP()));

   /* Set new value of FCSR3 */
   addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVGR2FCSR,
                                         fcsr, hregLOONGARCH64_FCSR3()));
}

/* Convert LOONGARCH FCMP cond to IR result */
static HReg convert_cond_to_IR ( ISelEnv* env, HReg src2, HReg src1, Bool size64 )
{
   HReg tmp = newVRegI(env);
   HReg dst = newVRegI(env);

   LOONGARCH64RI* ri1 = LOONGARCH64RI_I(63, 6, False);
   LOONGARCH64RI* ri2 = LOONGARCH64RI_I(0x45, 12, False);
   if (size64)
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CUN_D, src2, src1, tmp));
   else
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CUN_S, src2, src1, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri2, tmp, dst));

   LOONGARCH64RI* ri3 = LOONGARCH64RI_I(0x1, 12, False);
   LOONGARCH64RI* ri4 = LOONGARCH64RI_R(tmp);
   if (size64)
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CLT_D, src2, src1, tmp));
   else
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CLT_S, src2, src1, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri3, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri4, dst, dst));

   LOONGARCH64RI* ri5 = LOONGARCH64RI_I(0x40, 12, False);
   if (size64)
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CEQ_D, src2, src1, tmp));
   else
      addInstr(env, LOONGARCH64Instr_FpCmp(LAfpcmp_FCMP_CEQ_S, src2, src1, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri1, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri5, tmp, tmp));
   addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri4, dst, dst));

   return dst;
}


/*---------------------------------------------------------*/
/*--- ISEL: Function call helpers                       ---*/
/*---------------------------------------------------------*/

/* Used only in doHelperCall.  See big comment in doHelperCall re
   handling of register-parameter args.  This function figures out
   whether evaluation of an expression might require use of a fixed
   register.  If in doubt return True (safe but suboptimal).
*/
static Bool mightRequireFixedRegs ( IRExpr* e )
{
   if (UNLIKELY(is_IRExpr_VECRET_or_GSPTR(e))) {
      // These are always "safe" -- either a copy of SP in some
      // arbitrary vreg, or a copy of $r31, respectively.
      return False;
   }
   /* Else it's a "normal" expression. */
   switch (e->tag) {
      case Iex_RdTmp: case Iex_Const: case Iex_Get:
         return False;
      default:
         return True;
   }
}

/* Do a complete function call.  |guard| is a Ity_Bit expression
   indicating whether or not the call happens.  If guard==NULL, the
   call is unconditional.  |retloc| is set to indicate where the
   return value is after the call.  The caller (of this fn) must
   generate code to add |stackAdjustAfterCall| to the stack pointer
   after the call is done.  Returns True iff it managed to handle this
   combination of arg/return types, else returns False. */
static Bool doHelperCall( /*OUT*/UInt* stackAdjustAfterCall,
                          /*OUT*/RetLoc* retloc,
                          ISelEnv* env,
                          IRExpr* guard,
                          IRCallee* cee, IRType retTy, IRExpr** args )
{
   HReg          cond;
   HReg          argregs[LOONGARCH64_N_ARGREGS];
   HReg          tmpregs[LOONGARCH64_N_ARGREGS];
   Bool          go_fast;
   Int           n_args, i, nextArgReg;
   Addr64        target;

   vassert(LOONGARCH64_N_ARGREGS == 8);

   /* Set default returns.  We'll update them later if needed. */
   *stackAdjustAfterCall = 0;
   *retloc               = mk_RetLoc_INVALID();

   /* These are used for cross-checking that IR-level constraints on
      the use of IRExpr_VECRET() and IRExpr_GSPTR() are observed. */
   UInt nVECRETs = 0;
   UInt nGSPTRs  = 0;

   /* Marshal args for a call and do the call.

      This function only deals with a tiny set of possibilities, which
      cover all helpers in practice.  The restrictions are that only
      arguments in registers are supported, hence only
      LOONGARCH64_N_ARGREGS x 64 integer bits in total can be passed.
      In fact the only supported arg type is I64.

      The return type can be I{64,32}.  We currently do not add vector
      support.

      |args| may also contain IRExpr_GSPTR(), in which case the
      value in $r31 is passed as the corresponding argument.

      Generating code which is both efficient and correct when
      parameters are to be passed in registers is difficult, for the
      reasons elaborated in detail in comments attached to
      doHelperCall() in priv/host_x86_isel.c.  Here, we use a variant
      of the method described in those comments.

      The problem is split into two cases: the fast scheme and the
      slow scheme.  In the fast scheme, arguments are computed
      directly into the target (real) registers.  This is only safe
      when we can be sure that computation of each argument will not
      trash any real registers set by computation of any other
      argument.

      In the slow scheme, all args are first computed into vregs, and
      once they are all done, they are moved to the relevant real
      regs.  This always gives correct code, but it also gives a bunch
      of vreg-to-rreg moves which are usually redundant but are hard
      for the register allocator to get rid of.

      To decide which scheme to use, all argument expressions are
      first examined.  If they are all so simple that it is clear they
      will be evaluated without use of any fixed registers, use the
      fast scheme, else use the slow scheme.  Note also that only
      unconditional calls may use the fast scheme, since having to
      compute a condition expression could itself trash real
      registers.

      Note this requires being able to examine an expression and
      determine whether or not evaluation of it might use a fixed
      register.  That requires knowledge of how the rest of this insn
      selector works.  Currently just the following 3 are regarded as
      safe -- hopefully they cover the majority of arguments in
      practice: IRExpr_Tmp IRExpr_Const IRExpr_Get.
   */

   /* LOONGARCH64 calling convention: up to eight registers ($a0 ... $a7)
      are allowed to be used for passing integer arguments.  They correspond
      to regs $r4 ... $r11.  Note that the cee->regparms field is meaningless
      on LOONGARCH64 host (since we only implement one calling convention)
      and so we always ignore it. */

   n_args = 0;
   for (i = 0; args[i]; i++) {
      IRExpr* arg = args[i];
      if (UNLIKELY(arg->tag == Iex_VECRET)) {
         nVECRETs++;
      } else if (UNLIKELY(arg->tag == Iex_GSPTR)) {
         nGSPTRs++;
      }
      n_args++;
   }

   if (n_args > LOONGARCH64_N_ARGREGS) {
      vpanic("doHelperCall(loongarch64): cannot currently handle > 8 args");
   }

   argregs[0] = hregLOONGARCH64_R4();
   argregs[1] = hregLOONGARCH64_R5();
   argregs[2] = hregLOONGARCH64_R6();
   argregs[3] = hregLOONGARCH64_R7();
   argregs[4] = hregLOONGARCH64_R8();
   argregs[5] = hregLOONGARCH64_R9();
   argregs[6] = hregLOONGARCH64_R10();
   argregs[7] = hregLOONGARCH64_R11();

   tmpregs[0] = tmpregs[1] = tmpregs[2] = tmpregs[3] = INVALID_HREG;
   tmpregs[4] = tmpregs[5] = tmpregs[6] = tmpregs[7] = INVALID_HREG;

   /* First decide which scheme (slow or fast) is to be used. First assume the
      fast scheme, and select slow if any contraindications (wow) appear. */

   go_fast = True;

   if (guard) {
      if (guard->tag == Iex_Const
          && guard->Iex.Const.con->tag == Ico_U1
          && guard->Iex.Const.con->Ico.U1 == True) {
         /* unconditional */
      } else {
         /* Not manifestly unconditional -- be conservative. */
         go_fast = False;
      }
   }

   if (go_fast) {
      for (i = 0; i < n_args; i++) {
         if (mightRequireFixedRegs(args[i])) {
            go_fast = False;
            break;
         }
      }
   }

   if (go_fast) {
      if (retTy == Ity_V128 || retTy == Ity_V256) {
         go_fast = False;
         vpanic("doHelperCall(loongarch64): currently do not support vector");
      }
   }

   /* At this point the scheme to use has been established.  Generate
      code to get the arg values into the argument rregs.  If we run
      out of arg regs, give up. */

   if (go_fast) {
      /* FAST SCHEME */
      nextArgReg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (nextArgReg >= LOONGARCH64_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I64) {
            addInstr(env, LOONGARCH64Instr_Move(argregs[nextArgReg],
                                                iselIntExpr_R(env, args[i])));
            nextArgReg++;
         } else if (arg->tag == Iex_GSPTR) {
            addInstr(env, LOONGARCH64Instr_Move(argregs[nextArgReg], hregGSP()));
            nextArgReg++;
         } else if (arg->tag == Iex_VECRET) {
            // because of the go_fast logic above, we can't get here,
            // since vector return values makes us use the slow path
            // instead.
            vassert(0);
         } else
            return False; /* unhandled arg type */
      }

      /* Fast scheme only applies for unconditional calls.  Hence: */
      cond = INVALID_HREG;
   } else {
      /* SLOW SCHEME; move via temporaries */
      nextArgReg = 0;

      for (i = 0; i < n_args; i++) {
         IRExpr* arg = args[i];

         IRType  aTy = Ity_INVALID;
         if (LIKELY(!is_IRExpr_VECRET_or_GSPTR(arg)))
            aTy = typeOfIRExpr(env->type_env, args[i]);

         if (nextArgReg >= LOONGARCH64_N_ARGREGS)
            return False; /* out of argregs */

         if (aTy == Ity_I64) {
            tmpregs[nextArgReg] = iselIntExpr_R(env, args[i]);
            nextArgReg++;
         } else if (arg->tag == Iex_GSPTR) {
            tmpregs[nextArgReg] = hregGSP();
            nextArgReg++;
         } else if (arg->tag == Iex_VECRET) {
            vpanic("doHelperCall(loongarch64): currently do not support vector");
            nextArgReg++;
         } else
            return False; /* unhandled arg type */
      }

      /* Now we can compute the condition.  We can't do it earlier
         because the argument computations could trash the condition
         codes.  Be a bit clever to handle the common case where the
         guard is 1:Bit. */
      cond = INVALID_HREG;
      if (guard) {
         if (guard->tag == Iex_Const
             && guard->Iex.Const.con->tag == Ico_U1
             && guard->Iex.Const.con->Ico.U1 == True) {
            /* unconditional -- do nothing */
         } else {
            cond = iselCondCode_R(env, guard);
         }
      }

      /* Move the args to their final destinations. */
      for (i = 0; i < nextArgReg; i++) {
         vassert(!(hregIsInvalid(tmpregs[i])));
         /* None of these insns, including any spill code that might
            be generated, may alter the condition codes. */
         addInstr(env, LOONGARCH64Instr_Move(argregs[i], tmpregs[i]));
      }
   }

   /* Should be assured by checks above */
   vassert(nextArgReg <= LOONGARCH64_N_ARGREGS);

   /* Do final checks, set the return values, and generate the call
      instruction proper. */
   vassert(nGSPTRs == 0 || nGSPTRs == 1);
   vassert(nVECRETs == ((retTy == Ity_V128 || retTy == Ity_V256) ? 1 : 0));
   vassert(*stackAdjustAfterCall == 0);
   vassert(is_RetLoc_INVALID(*retloc));
   switch (retTy) {
      case Ity_INVALID:
         /* Function doesn't return a value. */
         *retloc = mk_RetLoc_simple(RLPri_None);
         break;
      case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64:
         *retloc = mk_RetLoc_simple(RLPri_Int);
         break;
      case Ity_V128:
      case Ity_V256:
         vpanic("doHelperCall(loongarch64): currently do not support vector");
         break;
      default:
         /* IR can denote other possible return types, but we don't
            handle those here. */
         vassert(0);
   }

   /* Finally, generate the call itself.  This needs the *retloc value
      set in the switch above, which is why it's at the end. */

   /* nextArgReg doles out argument registers.  Since these are
      assigned in the order $a0 .. $a7, its numeric value at this point,
      which must be between 0 and 8 inclusive, is going to be equal to
      the number of arg regs in use for the call.  Hence bake that
      number into the call (we'll need to know it when doing register
      allocation, to know what regs the call reads.) */

   target = (Addr)cee->addr;
   addInstr(env, LOONGARCH64Instr_Call(cond, target, nextArgReg, *retloc));

   return True; /* success */
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (64/32/16/8 bit)        ---*/
/*---------------------------------------------------------*/

/* Select insns for an integer-typed expression, and add them to the
   code list.  Return a reg holding the result.  This reg will be a
   virtual register.  THE RETURNED REG MUST NOT BE MODIFIED.  If you
   want to modify it, ask for a new vreg, copy it in there, and modify
   the copy.  The register allocator will do its best to map both
   vregs to the same real register, so the copies will often disappear
   later in the game.

   This should handle expressions of 64, 32, 16 and 8-bit type.
   All results are returned in a (mode64 ? 64bit : 32bit) register.
   For 16- and 8-bit expressions, the upper (32/48/56 : 16/24) bits
   are arbitrary, so you should mask or sign extend partial values
   if necessary.
*/

/* --------------------- AMode --------------------- */

static LOONGARCH64AMode* iselIntExpr_AMode ( ISelEnv* env,
                                             IRExpr* e, IRType dty )
{
   LOONGARCH64AMode* am = iselIntExpr_AMode_wrk(env, e, dty);

   /* sanity checks ... */
   switch (am->tag) {
      case LAam_RI:
         vassert(am->LAam.RI.index < (1 << 11)); /* The sign bit (bit 12) must be 0. */
         vassert(hregClass(am->LAam.RI.base) == HRcInt64);
         vassert(hregIsVirtual(am->LAam.RI.base));
         break;
      case LAam_RR:
         vassert(hregClass(am->LAam.RR.base) == HRcInt64);
         vassert(hregIsVirtual(am->LAam.RR.base));
         vassert(hregClass(am->LAam.RR.index) == HRcInt64);
         vassert(hregIsVirtual(am->LAam.RR.index));
         break;
      default:
         vpanic("iselIntExpr_AMode: unknown LOONGARCH64 AMode tag");
         break;
   }

   return am;
}

/* DO NOT CALL THIS DIRECTLY ! */
static LOONGARCH64AMode* iselIntExpr_AMode_wrk ( ISelEnv* env,
                                                 IRExpr* e, IRType dty )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_I64);

   /* Add64(expr, i), where i <= 0x7ff */
   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add64
       && e->Iex.Binop.arg2->tag == Iex_Const
       && e->Iex.Binop.arg2->Iex.Const.con->tag == Ico_U64
       && e->Iex.Binop.arg2->Iex.Const.con->Ico.U64 <= 0x7ff) {
      return LOONGARCH64AMode_RI(iselIntExpr_R(env, e->Iex.Binop.arg1),
                                 (UShort)e->Iex.Binop.arg2->Iex.Const.con->Ico.U64);
   }

   /* Add64(expr, expr) */
   if (e->tag == Iex_Binop && e->Iex.Binop.op == Iop_Add64) {
      HReg base = iselIntExpr_R(env, e->Iex.Binop.arg1);
      HReg index = iselIntExpr_R(env, e->Iex.Binop.arg2);
      return LOONGARCH64AMode_RR(base, index);
   }

   /* Doesn't match anything in particular.  Generate it into
      a register and use that. */
   return LOONGARCH64AMode_RI(iselIntExpr_R(env, e), 0);
}

/* --------------------- RI --------------------- */

static LOONGARCH64RI* iselIntExpr_RI ( ISelEnv* env, IRExpr* e,
                                       UChar size, Bool isSigned )
{
   LOONGARCH64RI* ri = iselIntExpr_RI_wrk(env, e, size, isSigned);

   /* sanity checks ... */
   switch (ri->tag) {
      case LAri_Imm:
         vassert(ri->LAri.I.size == 5 || ri->LAri.I.size == 6
                 || ri->LAri.I.size == 12);
         if (ri->LAri.I.size == 5) {
            vassert(ri->LAri.I.isSigned == False);
            vassert(ri->LAri.I.imm < (1 << 5));
         } else if (ri->LAri.I.size == 6) {
            vassert(ri->LAri.I.isSigned == False);
            vassert(ri->LAri.I.imm < (1 << 6));
         } else {
            vassert(ri->LAri.I.imm < (1 << 12));
         }
         break;
      case LAri_Reg:
         vassert(hregClass(ri->LAri.R.reg) == HRcInt64);
         vassert(hregIsVirtual(ri->LAri.R.reg));
         break;
      default:
         vpanic("iselIntExpr_RI: unknown LOONGARCH64 RI tag");
         break;
   }

   return ri;
}

/* DO NOT CALL THIS DIRECTLY ! */
static LOONGARCH64RI* iselIntExpr_RI_wrk ( ISelEnv* env, IRExpr* e,
                                           UChar size, Bool isSigned )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64);

   LOONGARCH64RI *ri = NULL;

   /* special case: immediate */
   if (e->tag == Iex_Const) {
      switch (e->Iex.Const.con->tag) {
         case Ico_U32:
            if (!isSigned && e->Iex.Const.con->Ico.U32 < (1 << size)) {
               UShort imm = e->Iex.Const.con->Ico.U32;
               ri = LOONGARCH64RI_I(imm, size, isSigned);
            }
            break;
         case Ico_U64:
            if (!isSigned && e->Iex.Const.con->Ico.U64 < (1 << size)) {
               UShort imm = e->Iex.Const.con->Ico.U64;
               ri = LOONGARCH64RI_I(imm, size, isSigned);
            }
            break;
         default:
            break;
      }
      /* else fail, fall through to default case */
   }

   if (ri == NULL) {
      /* default case: calculate into a register and return that */
      HReg reg = iselIntExpr_R(env, e);
      ri = LOONGARCH64RI_R(reg);
   }

   return ri;
}

/* --------------------- Reg --------------------- */

static HReg iselIntExpr_R ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselIntExpr_R_wrk(env, e);

   /* sanity checks ... */
   vassert(hregClass(r) == HRcInt64);
   vassert(hregIsVirtual(r));

   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselIntExpr_R_wrk ( ISelEnv* env, IRExpr* e )
{
   IRType ty = typeOfIRExpr(env->type_env, e);
   vassert(e);
   vassert(ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64);

   switch (e->tag) {
      /* --------- TEMP --------- */
      case Iex_RdTmp:
         return lookupIRTemp(env, e->Iex.RdTmp.tmp);

      /* --------- LOAD --------- */
      case Iex_Load: {
         if (e->Iex.Load.end != Iend_LE)
            goto irreducible;

         LOONGARCH64AMode* am = iselIntExpr_AMode(env, e->Iex.Load.addr, ty);
         HReg             dst = newVRegI(env);
         LOONGARCH64LoadOp op;
         switch(ty) {
            case Ity_I8:
               op = (am->tag == LAam_RI) ? LAload_LD_BU : LAload_LDX_BU;
               break;
            case Ity_I16:
               op = (am->tag == LAam_RI) ? LAload_LD_HU : LAload_LDX_HU;
               break;
            case Ity_I32:
               op = (am->tag == LAam_RI) ? LAload_LD_WU : LAload_LDX_WU;
               break;
            case Ity_I64:
               op = (am->tag == LAam_RI) ? LAload_LD_D : LAload_LDX_D;
               break;
            default:
               goto irreducible;
         }
         addInstr(env, LOONGARCH64Instr_Load(op, am, dst));
         return dst;
      }

      /* --------- BINARY OP --------- */
      case Iex_Binop: {
         switch (e->Iex.Binop.op) {
            case Iop_32HLto64: {
               HReg          dst = newVRegI(env);
               HReg           hi = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* lo = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(32, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, hi, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, lo, dst, dst));
               return dst;
            }
            case Iop_Add32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, True);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_ADD_W : LAbin_ADDI_W;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Add64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, True);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_ADD_D : LAbin_ADDI_D;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_And32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_AND : LAbin_ANDI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_And64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_AND : LAbin_ANDI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_DivModS32to32: {
               HReg            dst = newVRegI(env);
               HReg            tmp = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               HReg           src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
               LOONGARCH64RI*  ri1 = LOONGARCH64RI_I(0, 5, False);
               LOONGARCH64RI*  ri2 = LOONGARCH64RI_R(src2);
               LOONGARCH64RI*  ri3 = LOONGARCH64RI_I(32, 6, False);
               LOONGARCH64RI*  ri4 = LOONGARCH64RI_R(tmp);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src1, src1));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src2, src2));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_W, ri2, src1, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_MOD_W, ri2, src1, tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri3, tmp, tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri3, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri3, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri4, dst, dst));
               return dst;
            }
            case Iop_DivModU32to32: {
               HReg            dst = newVRegI(env);
               HReg            tmp = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               HReg           src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
               LOONGARCH64RI*  ri1 = LOONGARCH64RI_I(0, 5, False);
               LOONGARCH64RI*  ri2 = LOONGARCH64RI_R(src2);
               LOONGARCH64RI*  ri3 = LOONGARCH64RI_I(32, 6, False);
               LOONGARCH64RI*  ri4 = LOONGARCH64RI_R(tmp);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src1, src1));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src2, src2));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_WU, ri2, src1, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_MOD_WU, ri2, src1, tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri3, tmp, tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri3, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri3, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri4, dst, dst));
               return dst;
            }
            case Iop_DivS32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               HReg           src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
               LOONGARCH64RI*  ri1 = LOONGARCH64RI_I(0, 5, False);
               LOONGARCH64RI*  ri2 = LOONGARCH64RI_R(src2);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src1, src1));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src2, src2));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_W, ri2, src1, dst));
               return dst;
            }
            case Iop_DivS64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_D, src2, src1, dst));
               return dst;
            }
            case Iop_DivU32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               HReg           src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
               LOONGARCH64RI*  ri1 = LOONGARCH64RI_I(0, 5, False);
               LOONGARCH64RI*  ri2 = LOONGARCH64RI_R(src2);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src1, src1));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri1, src2, src2));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_WU, ri2, src1, dst));
               return dst;
            }
            case Iop_DivU64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_DU, src2, src1, dst));
               return dst;
            }
            case Iop_CmpF32: {
               HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
               HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
               return convert_cond_to_IR(env, src2, src1, False);
            }
            case Iop_CmpF64: {
               HReg src1 = iselFltExpr(env, e->Iex.Binop.arg1);
               HReg src2 = iselFltExpr(env, e->Iex.Binop.arg2);
               return convert_cond_to_IR(env, src2, src1, True);
            }
            case Iop_F32toI32S: {
               HReg tmp = newVRegF(env);
               HReg dst = newVRegI(env);
               HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
               set_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, LOONGARCH64Instr_FpUnary(LAfpun_FTINT_W_S, src, tmp));
               set_rounding_mode_default(env);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_S, tmp, dst));
               return dst;
            }
            case Iop_F32toI64S: {
               HReg tmp = newVRegF(env);
               HReg dst = newVRegI(env);
               HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
               set_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, LOONGARCH64Instr_FpUnary(LAfpun_FTINT_L_S, src, tmp));
               set_rounding_mode_default(env);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_D, tmp, dst));
               return dst;
            }
            case Iop_F64toI32S: {
               HReg tmp = newVRegF(env);
               HReg dst = newVRegI(env);
               HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
               set_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, LOONGARCH64Instr_FpUnary(LAfpun_FTINT_W_D, src, tmp));
               set_rounding_mode_default(env);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_S, tmp, dst));
               return dst;
            }
            case Iop_F64toI64S: {
               HReg tmp = newVRegF(env);
               HReg dst = newVRegI(env);
               HReg src = iselFltExpr(env, e->Iex.Binop.arg2);
               set_rounding_mode(env, e->Iex.Binop.arg1);
               addInstr(env, LOONGARCH64Instr_FpUnary(LAfpun_FTINT_L_D, src, tmp));
               set_rounding_mode_default(env);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_D, tmp, dst));
               return dst;
            }
            case Iop_Max32U: {
               HReg          cond = newVRegI(env);
               HReg           dst = newVRegI(env);
               HReg          src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               HReg          src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
               addInstr(env, LOONGARCH64Instr_Cmp(LAcc_LTU, src2, src1, cond));
               addInstr(env, LOONGARCH64Instr_CMove(cond, src1, src2, dst, True));
               return dst;
            }
            case Iop_MullS32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_MULW_D_W, src2, src1, dst));
               return dst;
            }
            case Iop_MullU32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_MULW_D_WU, src2, src1, dst));
               return dst;
            }
            case Iop_Or32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_OR : LAbin_ORI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Or64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_OR : LAbin_ORI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Sar32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 5, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SRA_W : LAbin_SRAI_W;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Sar64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 6, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SRA_D : LAbin_SRAI_D;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Shl32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 5, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SLL_W : LAbin_SLLI_W;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Shl64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 6, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SLL_D : LAbin_SLLI_D;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Shr32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 5, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SRL_W : LAbin_SRLI_W;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Shr64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 6, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_SRL_D : LAbin_SRLI_D;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Sub32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_W, src2, src1, dst));
               return dst;
            }
            case Iop_Sub64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_D, src2, src1, dst));
               return dst;
            }
            case Iop_Xor32: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_XOR : LAbin_XORI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            case Iop_Xor64: {
               HReg            dst = newVRegI(env);
               HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
               LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 12, False);
               LOONGARCH64BinOp op = (src2->tag == LAri_Reg) ? LAbin_XOR : LAbin_XORI;
               addInstr(env, LOONGARCH64Instr_Binary(op, src2, src1, dst));
               return dst;
            }
            default:
               goto irreducible;
         }
      }

      /* --------- UNARY OP --------- */
      case Iex_Unop: {
         switch (e->Iex.Unop.op) {
            case Iop_128HIto64: {
               HReg hi, lo;
               iselInt128Expr(&hi, &lo, env, e->Iex.Unop.arg);
               return hi;
            }
            case Iop_128to64: {
               HReg hi, lo;
               iselInt128Expr(&hi, &lo, env, e->Iex.Unop.arg);
               return lo;
            }
            case Iop_16Sto64: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_EXT_W_H, src, dst));
               return dst;
            }
            case Iop_16Uto64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(48, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri, dst, dst));
               return dst;
            }
            case Iop_1Sto32: {
               HReg           dst = newVRegI(env);
               HReg           src = iselCondCode_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(63, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri, dst, dst));
               return dst;
            }
            case Iop_1Sto64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselCondCode_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(63, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri, dst, dst));
               return dst;
            }
            case Iop_1Uto64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselCondCode_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0x1, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_1Uto8: {
               HReg          dst = newVRegI(env);
               HReg          src = iselCondCode_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0x1, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_32Sto64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0, 5, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri, src, dst));
               return dst;
            }
            case Iop_32Uto64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(32, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri, dst, dst));
               return dst;
            }
            case Iop_32to8: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0xff, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_64HIto32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(32, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri, src, dst));
               return dst;
            }
            case Iop_64to32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(32, 6, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRLI_D, ri, dst, dst));
               return dst;
            }
            case Iop_64to8: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0xff, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_8Sto64: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_EXT_W_B, src, dst));
               return dst;
            }
            case Iop_8Uto32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0xff, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_8Uto64: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(0xff, 12, False);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_ANDI, ri, src, dst));
               return dst;
            }
            case Iop_CmpwNEZ32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(63, 6, False);
               addInstr(env, LOONGARCH64Instr_Cmp(LAcc_NE, hregZERO(), src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri, dst, dst));
               return dst;
            }
            case Iop_CmpwNEZ64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_I(63, 6, False);
               addInstr(env, LOONGARCH64Instr_Cmp(LAcc_NE, hregZERO(), src, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_D, ri, dst, dst));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SRAI_D, ri, dst, dst));
               return dst;
            }
            case Iop_Clz32: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_CLZ_W, src, dst));
               return dst;
            }
            case Iop_Clz64: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_CLZ_D, src, dst));
               return dst;
            }
            case Iop_Ctz32: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_CTZ_W, src, dst));
               return dst;
            }
            case Iop_Ctz64: {
               HReg dst = newVRegI(env);
               HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_Unary(LAun_CTZ_D, src, dst));
               return dst;
            }
            case Iop_Left16: {
               HReg           tmp = newVRegI(env);
               HReg           dst = newVRegI(env);
               HReg           src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(src);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_D, ri, hregZERO(), tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri, tmp, dst));
               return dst;
            }
            case Iop_Left32: {
               HReg           tmp = newVRegI(env);
               HReg           dst = newVRegI(env);
               HReg           src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(src);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_D, ri, hregZERO(), tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri, tmp, dst));
               return dst;
            }
            case Iop_Left64: {
               HReg           tmp = newVRegI(env);
               HReg           dst = newVRegI(env);
               HReg           src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(src);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_D, ri, hregZERO(), tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri, tmp, dst));
               return dst;
            }
            case Iop_Left8: {
               HReg           tmp = newVRegI(env);
               HReg           dst = newVRegI(env);
               HReg           src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(src);
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_SUB_D, ri, hregZERO(), tmp));
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri, tmp, dst));
               return dst;
            }
            case Iop_ReinterpF32asI32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselFltExpr(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_S, src, dst));
               return dst;
            }
            case Iop_ReinterpF64asI64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselFltExpr(env, e->Iex.Unop.arg);
               addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_MOVFR2GR_D, src, dst));
               return dst;
            }
            case Iop_Not32: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(hregZERO());
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_NOR, ri, src, dst));
               return dst;
            }
            case Iop_Not64: {
               HReg          dst = newVRegI(env);
               HReg          src = iselIntExpr_R(env, e->Iex.Unop.arg);
               LOONGARCH64RI* ri = LOONGARCH64RI_R(hregZERO());
               addInstr(env, LOONGARCH64Instr_Binary(LAbin_NOR, ri, src, dst));
               return dst;
            }
            default:
               goto irreducible;
         }
      }

      /* --------- GET --------- */
      case Iex_Get: {
         LOONGARCH64AMode* am = mkLOONGARCH64AMode_RI(hregGSP(), e->Iex.Get.offset);
         HReg             dst = newVRegI(env);
         LOONGARCH64LoadOp op;
         switch(ty) {
            case Ity_I8:
               op = LAload_LD_BU;
               break;
            case Ity_I16:
               op = LAload_LD_HU;
               break;
            case Ity_I32:
               op = LAload_LD_WU;
               break;
            case Ity_I64:
               op = LAload_LD_D;
               break;
            default:
               goto irreducible;
         }
         addInstr(env, LOONGARCH64Instr_Load(op, am, dst));
         return dst;
      }

      /* --------- CCALL --------- */
      case Iex_CCall: {
         HReg    dst = newVRegI(env);
         vassert(ty == e->Iex.CCall.retty);

         /* be very restrictive for now.  Only 64-bit ints allowed for
            args, and 64 bits for return type.  Don't forget to change
            the RetLoc if more types are allowed in future. */
         if (e->Iex.CCall.retty != Ity_I64)
            goto irreducible;

         /* Marshal args, do the call, clear stack. */
         UInt   addToSp = 0;
         RetLoc rloc    = mk_RetLoc_INVALID();
         Bool   ok      = doHelperCall(&addToSp, &rloc, env, NULL,
                                       e->Iex.CCall.cee, e->Iex.CCall.retty,
                                       e->Iex.CCall.args);

         if (ok) {
            vassert(is_sane_RetLoc(rloc));
            vassert(rloc.pri == RLPri_Int);
            vassert(addToSp == 0);
            addInstr(env, LOONGARCH64Instr_Move(dst, hregLOONGARCH64_R4()));
            return dst;
         }
         goto irreducible;
      }

      /* --------- LITERAL --------- */
      /* 64-bit literals */
      case Iex_Const: {
         ULong imm = 0;
         HReg  dst = newVRegI(env);
         switch (e->Iex.Const.con->tag) {
            case Ico_U64:
               imm = e->Iex.Const.con->Ico.U64;
               break;
            case Ico_U32:
               imm = e->Iex.Const.con->Ico.U32;
               break;
            case Ico_U16:
               imm = e->Iex.Const.con->Ico.U16;
               break;
            case Ico_U8:
               imm = e->Iex.Const.con->Ico.U8;
               break;
            default:
               ppIRExpr(e);
               vpanic("iselIntExpr_R.Iex_Const(loongarch64)");
         }
         addInstr(env, LOONGARCH64Instr_LI(imm, dst));
         return dst;
      }

      case Iex_ITE: {
         HReg   r0 = iselIntExpr_R(env, e->Iex.ITE.iffalse);
         HReg   r1 = iselIntExpr_R(env, e->Iex.ITE.iftrue);
         HReg cond = iselCondCode_R(env, e->Iex.ITE.cond);
         HReg  dst = newVRegI(env);
         addInstr(env, LOONGARCH64Instr_CMove(cond, r0, r1, dst, True));
         return dst;
      }

      default:
         break;
   }

   /* We get here if no pattern matched. */
irreducible:
   ppIRExpr(e);
   vpanic("iselIntExpr_R(loongarch64): cannot reduce tree");
}

/* ------------------- CondCode ------------------- */

/* Generate code to evaluated a bit-typed expression, returning the
   condition code which would correspond when the expression would
   notionally have returned 1. */

static HReg iselCondCode_R ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselCondCode_R_wrk(env, e);

   /* sanity checks ... */
   vassert(hregClass(r) == HRcInt64);
   vassert(hregIsVirtual(r));

   return r;
}

/* DO NOT CALL THIS DIRECTLY ! */
static HReg iselCondCode_R_wrk ( ISelEnv* env, IRExpr* e )
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I1);

   HReg dst = newVRegI(env);

   /* var */
   if (e->tag == Iex_RdTmp) {
      HReg tmp = newVRegI(env);
      dst = lookupIRTemp(env, e->Iex.RdTmp.tmp);
      addInstr(env, LOONGARCH64Instr_LI(1, tmp));
      addInstr(env, LOONGARCH64Instr_Cmp(LAcc_EQ, dst, tmp, dst));
      return dst;
   }

   /* const */
   if (e->tag == Iex_Const && e->Iex.Const.con->tag == Ico_U1) {
      UInt imm = e->Iex.Const.con->Ico.U1;
      addInstr(env, LOONGARCH64Instr_LI(imm, dst));
      return dst;
   }

   if (e->tag == Iex_Unop) {
      if (e->Iex.Unop.op == Iop_Not1) {
         HReg          src = iselCondCode_R(env, e->Iex.Unop.arg);
         LOONGARCH64RI* ri = LOONGARCH64RI_R(hregZERO());
         addInstr(env, LOONGARCH64Instr_Binary(LAbin_NOR, ri, src, dst));
         return dst;
      }

      LOONGARCH64CondCode cc;
      switch (e->Iex.Unop.op) {
         case Iop_CmpNEZ16:
            cc = LAcc_NE;
            break;
         case Iop_CmpNEZ32:
            cc = LAcc_NE;
            break;
         case Iop_CmpNEZ64:
            cc = LAcc_NE;
            break;
         case Iop_CmpNEZ8:
            cc = LAcc_NE;
            break;
         default:
            goto irreducible;
      }
      HReg src = iselIntExpr_R(env, e->Iex.Unop.arg);
      addInstr(env, LOONGARCH64Instr_Cmp(cc, hregZERO(), src, dst));
      return dst;
   }

   if (e->tag == Iex_Binop) {
      if (e->Iex.Binop.op == Iop_And1) {
         HReg           src1 = iselCondCode_R(env, e->Iex.Binop.arg1);
         HReg           src2 = iselCondCode_R(env, e->Iex.Binop.arg2);
         LOONGARCH64RI*   ri = LOONGARCH64RI_R(src2);
         addInstr(env, LOONGARCH64Instr_Binary(LAbin_AND, ri, src1, dst));
         return dst;
      } else if (e->Iex.Binop.op == Iop_Or1) {
         HReg           src1 = iselCondCode_R(env, e->Iex.Binop.arg1);
         HReg           src2 = iselCondCode_R(env, e->Iex.Binop.arg2);
         LOONGARCH64RI*   ri = LOONGARCH64RI_R(src2);
         addInstr(env, LOONGARCH64Instr_Binary(LAbin_OR, ri, src1, dst));
         return dst;
      }

      Bool extend  = False;
      Bool reverse = False;
      LOONGARCH64CondCode cc;
      switch (e->Iex.Binop.op) {
         case Iop_CasCmpEQ32:
            cc = LAcc_EQ;
            break;
         case Iop_CasCmpEQ64:
            cc = LAcc_EQ;
            break;
         case Iop_CasCmpNE32:
            cc = LAcc_NE;
            break;
         case Iop_CasCmpNE64:
            cc = LAcc_NE;
            break;
         case Iop_CmpEQ32:
            cc = LAcc_EQ;
            break;
         case Iop_CmpEQ64:
            cc = LAcc_EQ;
            break;
         case Iop_CmpLE32S:
            cc = LAcc_GE;
            reverse = True;
            break;
         case Iop_CmpLE32U:
            cc = LAcc_GEU;
            reverse = True;
            break;
         case Iop_CmpLE64S:
            cc = LAcc_GE;
            reverse = True;
            break;
         case Iop_CmpLE64U:
            cc = LAcc_GEU;
            reverse = True;
            break;
         case Iop_CmpLT32S:
            cc = LAcc_LT;
            extend = True;
            break;
         case Iop_CmpLT32U:
            cc = LAcc_LTU;
            extend = True;
            break;
         case Iop_CmpLT64S:
            cc = LAcc_LT;
            break;
         case Iop_CmpLT64U:
            cc = LAcc_LTU;
            break;
         case Iop_CmpNE32:
            cc = LAcc_NE;
            break;
         case Iop_CmpNE64:
            cc = LAcc_NE;
            break;
         default:
            goto irreducible;
      }
      HReg src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
      HReg src2 = iselIntExpr_R(env, e->Iex.Binop.arg2);
      if (extend) {
         /* Sign-extend */
         LOONGARCH64RI* ri = LOONGARCH64RI_I(0, 5, False);
         addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri, src1, src1));
         addInstr(env, LOONGARCH64Instr_Binary(LAbin_SLLI_W, ri, src2, src2));
      }
      if (reverse) {
         addInstr(env, LOONGARCH64Instr_Cmp(cc, src1, src2, dst));
      } else {
         addInstr(env, LOONGARCH64Instr_Cmp(cc, src2, src1, dst));
      }
      return dst;
   }

   /* We get here if no pattern matched. */
irreducible:
   ppIRExpr(e);
   vpanic("iselCondCode(loongarch64): cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Integer expressions (128 bit)               ---*/
/*---------------------------------------------------------*/

/* Compute a 128-bit value into a register pair, which is returned as
   the first two parameters.  As with iselIntExpr_R, these may be
   either real or virtual regs; in any case they must not be changed
   by subsequent code emitted by the caller.  */

static void iselInt128Expr (HReg* hi, HReg* lo, ISelEnv* env, IRExpr* e)
{
   iselInt128Expr_wrk(hi, lo, env, e);

   /* sanity checks ... */
   vassert(hregClass(*hi) == HRcInt64);
   vassert(hregIsVirtual(*hi));
   vassert(hregClass(*lo) == HRcInt64);
   vassert(hregIsVirtual(*lo));
}

/* DO NOT CALL THIS DIRECTLY ! */
static void iselInt128Expr_wrk (HReg* hi, HReg* lo, ISelEnv* env, IRExpr* e)
{
   vassert(e);
   vassert(typeOfIRExpr(env->type_env, e) == Ity_I128);

   /* --------- TEMP --------- */
   if (e->tag == Iex_RdTmp) {
      lookupIRTempPair(hi, lo, env, e->Iex.RdTmp.tmp);
      return;
   }

   /* --------- BINARY OP --------- */
   if (e->tag == Iex_Binop) {
      switch (e->Iex.Binop.op) {
         case Iop_64HLto128: {
            *hi = iselIntExpr_R(env, e->Iex.Binop.arg1);
            *lo = iselIntExpr_R(env, e->Iex.Binop.arg2);
            return;
         }
         case Iop_DivModS64to64: {
            HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
            LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
            HReg          dstLo = newVRegI(env);
            HReg          dstHi = newVRegI(env);
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_D, src2, src1, dstLo));
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MOD_D, src2, src1, dstHi));
            *hi = dstHi;
            *lo = dstLo;
            return;
         }
         case Iop_DivModU64to64: {
            HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
            LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
            HReg          dstLo = newVRegI(env);
            HReg          dstHi = newVRegI(env);
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_DIV_DU, src2, src1, dstLo));
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MOD_DU, src2, src1, dstHi));
            *hi = dstHi;
            *lo = dstLo;
            return;
         }
         case Iop_MullS64: {
            HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
            LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
            HReg          dstLo = newVRegI(env);
            HReg          dstHi = newVRegI(env);
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MUL_D, src2, src1, dstLo));
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MULH_D, src2, src1, dstHi));
            *hi = dstHi;
            *lo = dstLo;
            return;
         }
         case Iop_MullU64: {
            HReg           src1 = iselIntExpr_R(env, e->Iex.Binop.arg1);
            LOONGARCH64RI* src2 = iselIntExpr_RI(env, e->Iex.Binop.arg2, 0, False);
            HReg          dstLo = newVRegI(env);
            HReg          dstHi = newVRegI(env);
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MUL_D, src2, src1, dstLo));
            addInstr(env, LOONGARCH64Instr_Binary(LAbin_MULH_DU, src2, src1, dstHi));
            *hi = dstHi;
            *lo = dstLo;
            return;
         }
         default:
            goto irreducible;
      }
   }

   /* We get here if no pattern matched. */
irreducible:
   ppIRExpr(e);
   vpanic("iselInt128Expr(loongarch64): cannot reduce tree");
}


/*---------------------------------------------------------*/
/*--- ISEL: Floating point expressions (64/32 bit)      ---*/
/*---------------------------------------------------------*/

/* Compute a floating point value into a register, the identity of
   which is returned.  As with iselIntExpr_R, the reg may be either
   real or virtual; in any case it must not be changed by subsequent
   code emitted by the caller.  */

static HReg iselFltExpr ( ISelEnv* env, IRExpr* e )
{
   HReg r = iselFltExpr_wrk(env, e);

   /* sanity checks ... */
   vassert(hregClass(r) == HRcFlt64);
   vassert(hregIsVirtual(r));

   return r;
}

/* DO NOT CALL THIS DIRECTLY */
static HReg iselFltExpr_wrk ( ISelEnv* env, IRExpr* e )
{
   HReg r;
   return r;
}


/*---------------------------------------------------------*/
/*--- ISEL: Statements                                  ---*/
/*---------------------------------------------------------*/

static void iselStmtStore ( ISelEnv* env, IRStmt* stmt )
{
   IRType tya  = typeOfIRExpr(env->type_env, stmt->Ist.Store.addr);
   IRType tyd  = typeOfIRExpr(env->type_env, stmt->Ist.Store.data);

   if (tya != Ity_I64 || stmt->Ist.Store.end != Iend_LE)
      vpanic("iselStmt(loongarch64): Ist_Store");

   Bool                 fp = False;
   LOONGARCH64AMode*    am = iselIntExpr_AMode(env, stmt->Ist.Store.addr, tyd);
   LOONGARCH64StoreOp   op;
   LOONGARCH64FpStoreOp fop;
   switch (tyd) {
      case Ity_I8:
         op = (am->tag == LAam_RI) ? LAstore_ST_B : LAstore_STX_B;
         break;
      case Ity_I16:
         op = (am->tag == LAam_RI) ? LAstore_ST_H : LAstore_STX_H;
         break;
      case Ity_I32:
         op = (am->tag == LAam_RI) ? LAstore_ST_W : LAstore_STX_W;
         break;
      case Ity_I64:
         op = (am->tag == LAam_RI) ? LAstore_ST_D : LAstore_STX_D;
         break;
      case Ity_F32:
         fop = (am->tag == LAam_RI) ? LAfpstore_FST_S : LAfpstore_FSTX_S;
         fp = True;
         break;
      case Ity_F64:
         fop = (am->tag == LAam_RI) ? LAfpstore_FST_D : LAfpstore_FSTX_D;
         fp = True;
         break;
      default:
         vpanic("iselStmt(loongarch64): Ist_Store");
         break;
   }

   if (fp) {
      HReg src = iselFltExpr(env, stmt->Ist.Store.data);
      addInstr(env, LOONGARCH64Instr_FpStore(fop, am, src));
   } else {
      HReg src = iselIntExpr_R(env, stmt->Ist.Store.data);
      addInstr(env, LOONGARCH64Instr_Store(op, am, src));
   }
}

static void iselStmtPut ( ISelEnv* env, IRStmt* stmt )
{
   IRType ty = typeOfIRExpr(env->type_env, stmt->Ist.Put.data);

   Bool                 fp = False;
   LOONGARCH64AMode*    am = mkLOONGARCH64AMode_RI(hregGSP(), stmt->Ist.Put.offset);
   LOONGARCH64StoreOp   op;
   LOONGARCH64FpStoreOp fop;
   switch (ty) {
      case Ity_I8:
         op = LAstore_ST_B;
         break;
      case Ity_I16:
         op = LAstore_ST_H;
         break;
      case Ity_I32:
         op = LAstore_ST_W;
         break;
      case Ity_I64:
         op = LAstore_ST_D;
         break;
      case Ity_F32:
         fop = LAfpstore_FST_S;
         fp = True;
         break;
      case Ity_F64:
         fop = LAfpstore_FST_D;
         fp = True;
         break;
      default:
         vpanic("iselStmt(loongarch64): Ist_Put");
         break;
   }

   if (fp) {
      HReg src = iselFltExpr(env, stmt->Ist.Put.data);
      addInstr(env, LOONGARCH64Instr_FpStore(fop, am, src));
   } else {
      HReg src = iselIntExpr_R(env, stmt->Ist.Put.data);
      addInstr(env, LOONGARCH64Instr_Store(op, am, src));
   }
}

static void iselStmtTmp ( ISelEnv* env, IRStmt* stmt )
{
   IRTemp tmp = stmt->Ist.WrTmp.tmp;
   IRType ty  = typeOfIRTemp(env->type_env, tmp);

   if (ty == Ity_I8 || ty == Ity_I16 || ty == Ity_I32 || ty == Ity_I64) {
      HReg dst = lookupIRTemp(env, tmp);
      HReg src = iselIntExpr_R(env, stmt->Ist.WrTmp.data);
      addInstr(env, LOONGARCH64Instr_Move(dst, src));
   } else if (ty == Ity_I1) {
      HReg dst = lookupIRTemp(env, tmp);
      HReg src = iselCondCode_R(env, stmt->Ist.WrTmp.data);
      addInstr(env, LOONGARCH64Instr_Move(dst, src));
   } else if (ty == Ity_F32) {
      HReg dst = lookupIRTemp(env, tmp);
      HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
      addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_FMOV_S, src, dst));
   } else if (ty == Ity_F64) {
      HReg dst = lookupIRTemp(env, tmp);
      HReg src = iselFltExpr(env, stmt->Ist.WrTmp.data);
      addInstr(env, LOONGARCH64Instr_FpMove(LAfpmove_FMOV_D, src, dst));
   } else {
      vpanic("iselStmt(loongarch64): Ist_WrTmp");
   }
}

static void iselStmtDirty ( ISelEnv* env, IRStmt* stmt )
{
   IRDirty* d = stmt->Ist.Dirty.details;

   /* Figure out the return type, if any. */
   IRType retty = Ity_INVALID;
   if (d->tmp != IRTemp_INVALID)
      retty = typeOfIRTemp(env->type_env, d->tmp);

   Bool retty_ok = False;
   switch (retty) {
      case Ity_INVALID: /* function doesn't return anything */
      case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64:
         retty_ok = True;
         break;
      default:
         break;
   }
   if (!retty_ok)
      vpanic("iselStmt(loongarch64): Ist_Dirty");

   /* Marshal args, do the call, and set the return value to 0x555..555
      if this is a conditional call that returns a value and the
      call is skipped. */
   UInt   addToSp = 0;
   RetLoc rloc    = mk_RetLoc_INVALID();
   doHelperCall(&addToSp, &rloc, env, d->guard, d->cee, retty, d->args);
   vassert(is_sane_RetLoc(rloc));

   /* Now figure out what to do with the returned value, if any. */
   switch (retty) {
      case Ity_INVALID: {
         /* No return value.  Nothing to do. */
         vassert(d->tmp == IRTemp_INVALID);
         vassert(rloc.pri == RLPri_None);
         vassert(addToSp == 0);
         break;
      }
      case Ity_I8: case Ity_I16: case Ity_I32: case Ity_I64: {
         vassert(rloc.pri == RLPri_Int);
         vassert(addToSp == 0);
         /* The returned value is in $a0.  Park it in the register
            associated with tmp. */
         HReg dst = lookupIRTemp(env, d->tmp);
         addInstr(env, LOONGARCH64Instr_Move(dst, hregLOONGARCH64_R4()));
         break;
      }
      default:
         /*NOTREACHED*/
         vassert(0);
         break;
   }
}

static void iselStmtLLSC ( ISelEnv* env, IRStmt* stmt )
{
   IRTemp res = stmt->Ist.LLSC.result;
   IRType tya = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.addr);

   /* Temporary solution; this need to be rewritten again for LOONGARCH64.
      On LOONGARCH64 you can not read from address that is locked with LL
      before SC. If you read from address that is locked than SC will fall.
    */
   if (stmt->Ist.LLSC.storedata == NULL) {
      /* LL */
      IRType ty = typeOfIRTemp(env->type_env, res);
      LOONGARCH64LLSCOp op;
      switch (ty) {
         case Ity_I32:
            op = LAllsc_LL_W;
            break;
         case Ity_I64:
            op = LAllsc_LL_D;
            break;
         default:
            vpanic("iselStmt(loongarch64): Ist_LLSC");
            break;
      }
      LOONGARCH64AMode* addr = iselIntExpr_AMode(env, stmt->Ist.LLSC.addr, tya);
      HReg               val = lookupIRTemp(env, res);
      addInstr(env, LOONGARCH64Instr_LLSC(op, True, addr, val));
   } else {
      /* SC */
      IRType tyd = typeOfIRExpr(env->type_env, stmt->Ist.LLSC.storedata);
      LOONGARCH64LLSCOp op;
      switch (tyd) {
         case Ity_I32:
            op = LAllsc_SC_W;
            break;
         case Ity_I64:
            op = LAllsc_SC_D;
            break;
         default:
            vpanic("iselStmt(loongarch64): Ist_LLSC");
            break;
      }
      LOONGARCH64AMode* addr = iselIntExpr_AMode(env, stmt->Ist.LLSC.addr, tya);
      HReg               val = iselIntExpr_R(env, stmt->Ist.LLSC.storedata);
      HReg               dst = lookupIRTemp(env, res);
      HReg               tmp = newVRegI(env);
      addInstr(env, LOONGARCH64Instr_Move(tmp, val));
      addInstr(env, LOONGARCH64Instr_LLSC(op, False, addr, tmp));
      addInstr(env, LOONGARCH64Instr_Move(dst, tmp));
   }
}

static void iselStmtCas ( ISelEnv* env, IRStmt* stmt )
{
   IRCAS* cas = stmt->Ist.CAS.details;
   if (cas->oldHi == IRTemp_INVALID && cas->end == Iend_LE) {
      /* "normal" singleton CAS */
      HReg   old = lookupIRTemp(env, cas->oldLo);
      HReg  addr = iselIntExpr_R(env, cas->addr);
      HReg  expd = iselIntExpr_R(env, cas->expdLo);
      HReg  data = iselIntExpr_R(env, cas->dataLo);
      IRType  ty = typeOfIRTemp(env->type_env, cas->oldLo);
      Bool size64;
      switch (ty) {
         case Ity_I32:
            size64 = False;
            break;
         case Ity_I64:
            size64 = True;
            break;
         default:
            vpanic("iselStmt(loongarch64): Ist_CAS");
            break;
      }
      addInstr(env, LOONGARCH64Instr_Cas(old, addr, expd, data, size64));
   } else {
      vpanic("iselStmt(loongarch64): Ist_CAS");
   }
}

static void iselStmtMBE ( ISelEnv* env, IRStmt* stmt )
{
   switch (stmt->Ist.MBE.event) {
      case Imbe_Fence:
      case Imbe_CancelReservation:
         addInstr(env, LOONGARCH64Instr_Bar(LAbar_DBAR, 0));
         break;
      case Imbe_InsnFence:
         addInstr(env, LOONGARCH64Instr_Bar(LAbar_IBAR, 0));
         break;
      default:
         vpanic("iselStmt(loongarch64): Ist_MBE");
         break;
   }
}

static void iselStmtExit ( ISelEnv* env, IRStmt* stmt )
{
   if (stmt->Ist.Exit.dst->tag != Ico_U64)
      vpanic("iselStmt(loongarch64): Ist_Exit: dst is not a 64-bit value");

   HReg            cond = iselCondCode_R(env, stmt->Ist.Exit.guard);
   LOONGARCH64AMode* am = mkLOONGARCH64AMode_RI(hregGSP(), stmt->Ist.Exit.offsIP);

   /* Case: boring transfer to known address */
   if (stmt->Ist.Exit.jk == Ijk_Boring) {
      if (env->chainingAllowed) {
         /* .. almost always true .. */
         /* Skip the event check at the dst if this is a forwards edge. */
         Bool toFastEP = ((Addr64)stmt->Ist.Exit.dst->Ico.U64) > env->max_ga;
         addInstr(env, LOONGARCH64Instr_XDirect(stmt->Ist.Exit.dst->Ico.U64,
                                                am, cond, toFastEP));
      } else {
         /* .. very occasionally .. */
         /* We can't use chaining, so ask for an assisted transfer,
            as that's the only alternative that is allowable. */
         HReg dst = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
         addInstr(env, LOONGARCH64Instr_XAssisted(dst, am, cond, Ijk_Boring));
      }
      return;
   }

   /* Case: assisted transfer to arbitrary address */
   switch (stmt->Ist.Exit.jk) {
      /* Keep this list in sync with that for iselNext below */
      case Ijk_ClientReq:
      case Ijk_Yield:
      case Ijk_NoDecode:
      case Ijk_InvalICache:
      case Ijk_NoRedir:
      case Ijk_SigILL:
      case Ijk_SigTRAP:
      case Ijk_SigSEGV:
      case Ijk_SigBUS:
      case Ijk_SigFPE_IntDiv:
      case Ijk_SigFPE_IntOvf:
      case Ijk_SigSYS:
      case Ijk_Sys_syscall: {
         HReg dst = iselIntExpr_R(env, IRExpr_Const(stmt->Ist.Exit.dst));
         addInstr(env, LOONGARCH64Instr_XAssisted(dst, am, cond, stmt->Ist.Exit.jk));
         break;
      }
      default:
         /* Do we ever expect to see any other kind? */
         ppIRJumpKind(stmt->Ist.Exit.jk);
         vpanic("iselStmt(loongarch64): Ist_Exit: unexpected jump kind");
         break;
   }
}

static void iselStmt(ISelEnv* env, IRStmt* stmt)
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- ");
      ppIRStmt(stmt);
      vex_printf("\n");
   }

   switch (stmt->tag) {
      /* --------- STORE --------- */
      /* little-endian write to memory */
      case Ist_Store:
         iselStmtStore(env, stmt);
         break;

      /* --------- PUT --------- */
      /* write guest state, fixed offset */
      case Ist_Put:
         iselStmtPut(env, stmt);
         break;

      /* --------- TMP --------- */
      /* assign value to temporary */
      case Ist_WrTmp:
         iselStmtTmp(env, stmt);
         break;

      /* --------- Call to DIRTY helper --------- */
      /* call complex ("dirty") helper function */
      case Ist_Dirty:
         iselStmtDirty(env, stmt);
         break;

      /* --------- Load Linked and Store Conditional --------- */
      case Ist_LLSC:
         iselStmtLLSC(env, stmt);
         break;

      /* --------- CAS --------- */
      case Ist_CAS:
         iselStmtCas(env, stmt);
         break;

      /* --------- MEM FENCE --------- */
      case Ist_MBE:
         iselStmtMBE(env, stmt);
         break;

      /* --------- INSTR MARK --------- */
      /* Doesn't generate any executable code ... */
      case Ist_IMark:
         break;

      /* --------- ABI HINT --------- */
      /* These have no meaning (denotation in the IR) and so we ignore
         them ... if any actually made it this far. */
      case Ist_AbiHint:
         break;

      /* --------- NO-OP --------- */
      case Ist_NoOp:
         break;

      /* --------- EXIT --------- */
      case Ist_Exit:
         iselStmtExit(env, stmt);
         break;

      default:
         ppIRStmt(stmt);
         vpanic("iselStmt(loongarch64)");
         break;
   }
}


/*---------------------------------------------------------*/
/*--- ISEL: Basic block terminators (Nexts)             ---*/
/*---------------------------------------------------------*/

static void iselNext ( ISelEnv* env, IRExpr* next, IRJumpKind jk, Int offsIP )
{
   if (vex_traceflags & VEX_TRACE_VCODE) {
      vex_printf("\n-- PUT(%d) = ", offsIP);
      ppIRExpr(next);
      vex_printf("; exit-");
      ppIRJumpKind(jk);
      vex_printf("\n");
   }

   /* Case: boring transfer to known address */
   if (next->tag == Iex_Const) {
      IRConst* cdst = next->Iex.Const.con;
      vassert(cdst->tag == Ico_U64);
      if (jk == Ijk_Boring) {
         /* Boring transfer to known address */
         LOONGARCH64AMode* am = mkLOONGARCH64AMode_RI(hregGSP(), offsIP);
         if (env->chainingAllowed) {
            /* .. almost always true .. */
            /* Skip the event check at the dst if this is a forwards edge. */
            Bool toFastEP = ((Addr64)cdst->Ico.U64) > env->max_ga;
            addInstr(env, LOONGARCH64Instr_XDirect(cdst->Ico.U64, am,
                                                   INVALID_HREG, toFastEP));
         } else {
            /* .. very occasionally .. */
            /* We can't use chaining, so ask for an assisted transfer,
               as that's the only alternative that is allowable. */
            HReg dst = iselIntExpr_R(env, next);
            addInstr(env, LOONGARCH64Instr_XAssisted(dst, am, INVALID_HREG, Ijk_Boring));
         }
         return;
      }
   }

   /* Case: boring transfer to any address */
   switch (jk) {
      case Ijk_Boring: {
         HReg dst = iselIntExpr_R(env, next);
         LOONGARCH64AMode* am = mkLOONGARCH64AMode_RI(hregGSP(), offsIP);
         if (env->chainingAllowed) {
            addInstr(env, LOONGARCH64Instr_XIndir(dst, am, INVALID_HREG));
         } else {
            addInstr(env, LOONGARCH64Instr_XAssisted(dst, am,
                                                     INVALID_HREG, Ijk_Boring));
         }
         return;
      }
      default:
         break;
   }

   /* Case: assisted transfer to arbitrary address */
   switch (jk) {
      /* Keep this list in sync with that for Ist_Exit above */
      case Ijk_ClientReq:
      case Ijk_Yield:
      case Ijk_NoDecode:
      case Ijk_InvalICache:
      case Ijk_NoRedir:
      case Ijk_SigILL:
      case Ijk_SigTRAP:
      case Ijk_SigSEGV:
      case Ijk_SigBUS:
      case Ijk_SigFPE_IntDiv:
      case Ijk_SigFPE_IntOvf:
      case Ijk_SigSYS:
      case Ijk_Sys_syscall: {
         HReg dst = iselIntExpr_R(env, next);
         LOONGARCH64AMode* am = mkLOONGARCH64AMode_RI(hregGSP(), offsIP);
         addInstr(env, LOONGARCH64Instr_XAssisted(dst, am, INVALID_HREG, jk));
         return;
      }
      default:
         break;
   }

   vex_printf("\n-- PUT(%d) = ", offsIP);
   ppIRExpr(next);
   vex_printf("; exit-");
   ppIRJumpKind(jk);
   vex_printf("\n");
   vassert(0); // are we expecting any other kind?
}


/*---------------------------------------------------------*/
/*--- Insn selector top-level                           ---*/
/*---------------------------------------------------------*/

/* Translate an entire BB to LOONGARCH64 code. */
HInstrArray* iselSB_LOONGARCH64 ( const IRSB* bb,
                                  VexArch arch_host,
                                  const VexArchInfo* archinfo_host,
                                  const VexAbiInfo* vbi,
                                  Int offs_Host_EvC_Counter,
                                  Int offs_Host_EvC_FailAddr,
                                  Bool chainingAllowed,
                                  Bool addProfInc,
                                  Addr max_ga )
{
   Int        i, j;
   HReg       hreg, hregHI;
   ISelEnv*   env;
   UInt       hwcaps_host = archinfo_host->hwcaps;
   LOONGARCH64AMode *amCounter, *amFailAddr;

   /* sanity ... */
   vassert(arch_host == VexArchLOONGARCH64);
   vassert((hwcaps_host & ~(VEX_HWCAPS_LOONGARCH_CPUCFG
                          | VEX_HWCAPS_LOONGARCH_LAM
                          | VEX_HWCAPS_LOONGARCH_UAL
                          | VEX_HWCAPS_LOONGARCH_FP
                          | VEX_HWCAPS_LOONGARCH_LSX
                          | VEX_HWCAPS_LOONGARCH_LASX
                          | VEX_HWCAPS_LOONGARCH_COMPLEX
                          | VEX_HWCAPS_LOONGARCH_CRYPTO
                          | VEX_HWCAPS_LOONGARCH_LVZP
                          | VEX_HWCAPS_LOONGARCH_X86BT
                          | VEX_HWCAPS_LOONGARCH_ARMBT
                          | VEX_HWCAPS_LOONGARCH_MIPSBT
                          | VEX_HWCAPS_LOONGARCH_ISA_32BIT
                          | VEX_HWCAPS_LOONGARCH_ISA_64BIT)) == 0);

   /* Check that the host's endianness is as expected. */
   vassert(archinfo_host->endness == VexEndnessLE);

   /* Make up an initial environment to use. */
   env = LibVEX_Alloc_inline(sizeof(ISelEnv));
   env->vreg_ctr = 0;

   /* Set up output code array. */
   env->code = newHInstrArray();

   /* Copy BB's type env. */
   env->type_env = bb->tyenv;

   /* Make up an IRTemp -> virtual HReg mapping.  This doesn't
      change as we go along. */
   env->n_vregmap = bb->tyenv->types_used;
   env->vregmap   = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));
   env->vregmapHI = LibVEX_Alloc_inline(env->n_vregmap * sizeof(HReg));

   /* and finally ... */
   env->chainingAllowed = chainingAllowed;
   env->hwcaps          = hwcaps_host;
   env->max_ga          = max_ga;

   /* For each IR temporary, allocate a suitably-kinded virtual register. */
   j = 0;
   for (i = 0; i < env->n_vregmap; i++) {
      hregHI = hreg = INVALID_HREG;
      switch (bb->tyenv->types[i]) {
         case Ity_I1:
         case Ity_I8:
         case Ity_I16:
         case Ity_I32:
         case Ity_I64:
            hreg = mkHReg(True, HRcInt64, 0, j++);
            break;
         case Ity_I128:
            hreg   = mkHReg(True, HRcInt64, 0, j++);
            hregHI = mkHReg(True, HRcInt64, 0, j++);
            break;
         case Ity_F16: // we'll use HRcFlt64 regs for F16 too
         case Ity_F32: // we'll use HRcFlt64 regs for F32 too
         case Ity_F64:
            hreg = mkHReg(True, HRcFlt64, 0, j++);
            break;
         default:
            ppIRType(bb->tyenv->types[i]);
            vpanic("iselBB(loongarch64): IRTemp type");
      }
      env->vregmap[i]   = hreg;
      env->vregmapHI[i] = hregHI;
   }
   env->vreg_ctr = j;

   /* The very first instruction must be an event check. */
   amCounter  = mkLOONGARCH64AMode_RI(hregGSP(), offs_Host_EvC_Counter);
   amFailAddr = mkLOONGARCH64AMode_RI(hregGSP(), offs_Host_EvC_FailAddr);
   addInstr(env, LOONGARCH64Instr_EvCheck(amCounter, amFailAddr));

   /* Possibly a block counter increment (for profiling).  At this
      point we don't know the address of the counter, so just pretend
      it is zero.  It will have to be patched later, but before this
      translation is used, by a call to LibVEX_patchProfCtr. */
   if (addProfInc) {
      addInstr(env, LOONGARCH64Instr_ProfInc());
   }

   /* Ok, finally we can iterate over the statements. */
   for (i = 0; i < bb->stmts_used; i++)
      iselStmt(env, bb->stmts[i]);

   iselNext(env, bb->next, bb->jumpkind, bb->offsIP);

   /* record the number of vregs we used. */
   env->code->n_vregs = env->vreg_ctr;
   return env->code;
}


/*---------------------------------------------------------------*/
/*--- end                             host_loongarch64_isel.c ---*/
/*---------------------------------------------------------------*/
