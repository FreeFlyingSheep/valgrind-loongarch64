
/*--------------------------------------------------------------------*/
/*--- begin                               guest_loongarch64_toIR.c ---*/
/*--------------------------------------------------------------------*/

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
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

/* Translates LOONGARCH64 code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_loongarch64.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_loongarch64_defs.h"


/*------------------------------------------------------------*/
/*--- Globals                                              ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly.  CONST means does
   not change during translation of the instruction. */

/* CONST: what is the host's endianness?  We need to know this in
   order to do sub-register accesses to the SIMD/FP registers
   correctly. */
static VexEndness host_endness;

/* CONST: The guest address for the instruction currently being
   translated.  */
static Addr64 guest_PC_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB* irsb;


/*------------------------------------------------------------*/
/*--- Debugging output                                     ---*/
/*------------------------------------------------------------*/

#define DIP(format, args...)           \
   if (vex_traceflags & VEX_TRACE_FE)  \
      vex_printf(format, ## args)

static const HChar* nameIReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
      "$zero",
      "$ra",
      "$tp",
      "$sp",
      "$a0", "$a1", "$a2", "$a3", "$a4", "$a5", "$a6", "$a7",
      "$t0", "$t1", "$t2", "$t3", "$t4", "$t5", "$t6", "$t7", "$t8",
      "$r21", /* Reserved */
      "$fp",
      "$s0", "$s1", "$s2", "$s3", "$s4", "$s5", "$s6", "$s7", "$s8"
   };
   return reg_names[reg];
}

static const HChar* nameFReg( UInt reg )
{
   vassert(reg < 32);
   static const HChar* reg_names[32] = {
      "$fa0",  "$fa1",  "$fa2",  "$fa3",  "$fa4",  "$fa5",  "$fa6",  "$fa7",
      "$ft0",  "$ft1",  "$ft2",  "$ft3",  "$ft4",  "$ft5",  "$ft6",  "$ft7",
      "$ft8",  "$ft9",  "$ft10", "$ft11", "$ft12", "$ft13", "$ft14", "$ft15",
      "$fs0",  "$fs1",  "$fs2",  "$fs3",  "$fs4",  "$fs5",  "$fs6",  "$fs7"
   };
   return reg_names[reg];
}

static const HChar* nameFCC( UInt reg )
{
   vassert(reg < 8);
   static const HChar* reg_names[8] = {
      "$fcc0", "$fcc1", "$fcc2", "$fcc3", "$fcc4", "$fcc5", "$fcc6", "$fcc7"
   };
   return reg_names[reg];
}

static const HChar* nameFCSR( UInt reg )
{
   vassert(reg < 4);
   static const HChar* reg_names[4] = {
      "$fcsr0", "$fcsr1", "$fcsr2", "$fcsr3"
   };
   return reg_names[reg];
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for deconstructing the        ---*/
/*--- loongarch64 insn stream.                             ---*/
/*------------------------------------------------------------*/

/* Get insn[max:min] */
#define SLICE(insn, max, min) \
   ((((UInt)(insn)) >> (min)) & (UInt)((1ULL << ((max) - (min) + 1)) - 1ULL))

/* Do a little-endian load of a 32-bit word, regardless of the
   endianness of the underlying host. */
static inline UInt getUInt ( const UChar* p )
{
   UInt w = 0;
   w = (w << 8) | p[3];
   w = (w << 8) | p[2];
   w = (w << 8) | p[1];
   w = (w << 8) | p[0];
   return w;
}


/*------------------------------------------------------------*/
/*--- Helper bits and pieces for creating IR fragments.    ---*/
/*------------------------------------------------------------*/

static inline IRExpr* mkU64 ( ULong i )
{
   return IRExpr_Const(IRConst_U64(i));
}

static inline IRExpr* mkU32 ( UInt i )
{
   return IRExpr_Const(IRConst_U32(i));
}

static inline IRExpr* mkU16 ( UInt i )
{
   vassert(i < 65536);
   return IRExpr_Const(IRConst_U16(i));
}

static inline IRExpr* mkU8 ( UInt i )
{
   vassert(i < 256);
   return IRExpr_Const(IRConst_U8((UChar)i));
}

static inline IRExpr* mkU1 ( UInt i )
{
   vassert(i == 0 || i == 1);
   return IRExpr_Const(IRConst_U1((Bool)i));
}

static inline IRExpr* mkF64i ( ULong i )
{
   return IRExpr_Const(IRConst_F64i(i));
}

static inline IRExpr* mkF32i ( UInt i )
{
   return IRExpr_Const(IRConst_F32i(i));
}

static inline IRExpr* mkexpr ( IRTemp tmp )
{
   return IRExpr_RdTmp(tmp);
}

static inline IRExpr* unop ( IROp op, IRExpr* a )
{
   return IRExpr_Unop(op, a);
}

static inline IRExpr* binop ( IROp op, IRExpr* a1, IRExpr* a2 )
{
   return IRExpr_Binop(op, a1, a2);
}

static inline IRExpr* triop ( IROp op, IRExpr* a1, IRExpr* a2, IRExpr* a3 )
{
   return IRExpr_Triop(op, a1, a2, a3);
}

static inline IRExpr* qop ( IROp op, IRExpr* a1, IRExpr* a2,
                            IRExpr* a3, IRExpr* a4 )
{
   return IRExpr_Qop(op, a1, a2, a3, a4);
}

static inline IRExpr* load ( IRType ty, IRExpr* addr )
{
   return IRExpr_Load(Iend_LE, ty, addr);
}

/* Add a statement to the list held by "irbb". */
static inline void stmt ( IRStmt* st )
{
   addStmtToIRSB(irsb, st);
}

static inline void store ( IRExpr* addr, IRExpr* data )
{
   stmt(IRStmt_Store(Iend_LE, addr, data));
}

static inline void assign ( IRTemp dst, IRExpr* e )
{
   stmt(IRStmt_WrTmp(dst, e));
}

static inline void exit ( IRExpr* e, IRJumpKind jk, ULong offs )
{
   stmt(IRStmt_Exit(e, jk, IRConst_U64(guest_PC_curr_instr + offs),
                    offsetof(VexGuestLOONGARCH64State, guest_PC)));
}

/* Generate an expression to check if addr is aligned. */
static inline IRExpr* check_align ( IRExpr* addr, IRExpr* align )
{
   return binop(Iop_CmpNE64, binop(Iop_And64, addr, align),
                IRExpr_Get(offsetof(VexGuestLOONGARCH64State, guest_R0),
                           Ity_I64));
}

/* Generate a SIGSYS if the expression evaluates to true. */
static inline void gen_SIGSYS ( IRExpr* cond )
{
   exit(cond, Ijk_SigSYS, 4);
}

static inline void cas ( IRTemp old, IRExpr* addr, IRExpr* expd, IRExpr* new )
{
   IRCAS* c = mkIRCAS(IRTemp_INVALID, old, Iend_LE, addr,
                      NULL, expd, NULL, new);
   stmt(IRStmt_CAS(c));
}

/* Generate a new temporary of the given type. */
static inline IRTemp newTemp ( IRType ty )
{
   vassert(isPlausibleIRType(ty));
   return newIRTemp(irsb->tyenv, ty);
}

/* S-extend 8/16/32 bit int expr to 64. */
static IRExpr* extendS ( IRType ty, IRExpr* e )
{
   switch (ty) {
      case Ity_I1:  return unop(Iop_1Sto64, e);
      case Ity_I8:  return unop(Iop_8Sto64, e);
      case Ity_I16: return unop(Iop_16Sto64, e);
      case Ity_I32: return unop(Iop_32Sto64, e);
      default: vassert(0);
   }
}

/* Z-extend 8/16/32 bit int expr to 64. */
static IRExpr* extendU ( IRType ty, IRExpr* e )
{
   switch (ty) {
      case Ity_I1:  return unop(Iop_1Uto64, e);
      case Ity_I8:  return unop(Iop_8Uto64, e);
      case Ity_I16: return unop(Iop_16Uto64, e);
      case Ity_I32: return unop(Iop_32Uto64, e);
      default: vassert(0);
   }
}


/*------------------------------------------------------------*/
/*--- Helpers for accessing guest registers.               ---*/
/*------------------------------------------------------------*/

/* ---------------- Integer registers ---------------- */

static Int offsetIReg ( UInt iregNo )
{
   switch (iregNo) {
      case 0:  return offsetof(VexGuestLOONGARCH64State, guest_R0);
      case 1:  return offsetof(VexGuestLOONGARCH64State, guest_R1);
      case 2:  return offsetof(VexGuestLOONGARCH64State, guest_R2);
      case 3:  return offsetof(VexGuestLOONGARCH64State, guest_R3);
      case 4:  return offsetof(VexGuestLOONGARCH64State, guest_R4);
      case 5:  return offsetof(VexGuestLOONGARCH64State, guest_R5);
      case 6:  return offsetof(VexGuestLOONGARCH64State, guest_R6);
      case 7:  return offsetof(VexGuestLOONGARCH64State, guest_R7);
      case 8:  return offsetof(VexGuestLOONGARCH64State, guest_R8);
      case 9:  return offsetof(VexGuestLOONGARCH64State, guest_R9);
      case 10: return offsetof(VexGuestLOONGARCH64State, guest_R10);
      case 11: return offsetof(VexGuestLOONGARCH64State, guest_R11);
      case 12: return offsetof(VexGuestLOONGARCH64State, guest_R12);
      case 13: return offsetof(VexGuestLOONGARCH64State, guest_R13);
      case 14: return offsetof(VexGuestLOONGARCH64State, guest_R14);
      case 15: return offsetof(VexGuestLOONGARCH64State, guest_R15);
      case 16: return offsetof(VexGuestLOONGARCH64State, guest_R16);
      case 17: return offsetof(VexGuestLOONGARCH64State, guest_R17);
      case 18: return offsetof(VexGuestLOONGARCH64State, guest_R18);
      case 19: return offsetof(VexGuestLOONGARCH64State, guest_R19);
      case 20: return offsetof(VexGuestLOONGARCH64State, guest_R20);
      case 21: return offsetof(VexGuestLOONGARCH64State, guest_R21);
      case 22: return offsetof(VexGuestLOONGARCH64State, guest_R22);
      case 23: return offsetof(VexGuestLOONGARCH64State, guest_R23);
      case 24: return offsetof(VexGuestLOONGARCH64State, guest_R24);
      case 25: return offsetof(VexGuestLOONGARCH64State, guest_R25);
      case 26: return offsetof(VexGuestLOONGARCH64State, guest_R26);
      case 27: return offsetof(VexGuestLOONGARCH64State, guest_R27);
      case 28: return offsetof(VexGuestLOONGARCH64State, guest_R28);
      case 29: return offsetof(VexGuestLOONGARCH64State, guest_R29);
      case 30: return offsetof(VexGuestLOONGARCH64State, guest_R30);
      case 31: return offsetof(VexGuestLOONGARCH64State, guest_R31);
      default: vassert(0);
   }
}

static IRExpr* getIReg8 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I8);
}

static IRExpr* getIReg16 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I16);
}

static IRExpr* getIReg32 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I32);
}

static IRExpr* getIReg64 ( UInt iregNo )
{
   return IRExpr_Get(offsetIReg(iregNo), Ity_I64);
}

static void putIReg ( UInt iregNo, IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   if (iregNo != 0) /* $r0 - constant zero */
      stmt(IRStmt_Put(offsetIReg(iregNo), e));
}

static void putPC ( IRExpr* e )
{
   vassert(typeOfIRExpr(irsb->tyenv, e) == Ity_I64);
   stmt(IRStmt_Put(offsetof(VexGuestLOONGARCH64State, guest_PC), e));
}


/*------------------------------------------------------------*/
/*--- Disassemble a single LOONGARCH64 instruction         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single LOONGARCH64 instruction into IR.  The instruction
   has is located at |guest_instr| and has guest IP of |guest_PC_curr_instr|,
   which will have been set before the call here.  Returns True iff the
   instruction was decoded, in which case *dres will be set accordingly,
   or False, in which case *dres should be ignored by the caller. */

static Bool disInstr_LOONGARCH64_WRK_special ( DisResult* dres,
                                               const UChar* guest_instr )
{
   return False;
}

static Bool disInstr_LOONGARCH64_WRK ( /*MB_OUT*/DisResult* dres,
                                       const UChar* guest_instr,
                                       const VexArchInfo* archinfo,
                                       const VexAbiInfo*  abiinfo,
                                       Bool sigill_diag )
{
   /* Set result defaults. */
   dres->whatNext    = Dis_Continue;
   dres->len         = 4;
   dres->jk_StopHere = Ijk_INVALID;
   dres->hint        = Dis_HintNone;

   /* At least this is simple on LOONGARCH64: insns are all 4 bytes long,
      and 4-aligned.  So just fish the whole thing out of memory right now
      and have done. */
   UInt insn = getUInt(guest_instr);
   DIP("\t0x%llx:\t0x%08x\t", (Addr64)guest_PC_curr_instr, insn);
   vassert((guest_PC_curr_instr & 3ULL) == 0);

   /* Spot "Special" instructions (see comment at top of file). */
   Bool ok = disInstr_LOONGARCH64_WRK_special(dres, guest_instr);
   if (ok)
      return ok;

   /* Main LOONGARCH64 instruction decoder starts here. */
   switch (SLICE(insn, 31, 30)) {
      default:
         ok = False;
         break;
   }

   /* If the next-level down decoders failed, make sure |dres| didn't
      get changed. */
   if (!ok) {
      vassert(dres->whatNext    == Dis_Continue);
      vassert(dres->len         == 4);
      vassert(dres->jk_StopHere == Ijk_INVALID);
   }
   return ok;
}


/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */

DisResult disInstr_LOONGARCH64 ( IRSB*              irsb_IN,
                                 const UChar*       guest_code_IN,
                                 Long               delta_IN,
                                 Addr               guest_IP,
                                 VexArch            guest_arch,
                                 const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo,
                                 VexEndness         host_endness_IN,
                                 Bool               sigill_diag_IN )
{
   DisResult dres;
   vex_bzero(&dres, sizeof(dres));

   /* Set globals (see top of this file) */
   vassert(guest_arch == VexArchLOONGARCH64);

   irsb                = irsb_IN;
   host_endness        = host_endness_IN;
   guest_PC_curr_instr = (Addr64)guest_IP;

   /* Try to decode */
   Bool ok = disInstr_LOONGARCH64_WRK(&dres,
                                      &guest_code_IN[delta_IN],
                                      archinfo, abiinfo, sigill_diag_IN);

   if (ok) {
      /* All decode successes end up here. */
      vassert(dres.len == 4);
      switch (dres.whatNext) {
         case Dis_Continue:
            putPC(mkU64(dres.len + guest_PC_curr_instr));
            break;
         case Dis_StopHere:
            break;
         default:
            vassert(0);
            break;
      }
      DIP("\n");
   } else {
      /* All decode failures end up here. */
      if (sigill_diag_IN) {
         Int   i, j;
         UChar buf[64];
         UInt  insn = getUInt(&guest_code_IN[delta_IN]);
         vex_bzero(buf, sizeof(buf));
         for (i = j = 0; i < 32; i++) {
            if (i > 0 && (i & 3) == 0)
               buf[j++] = ' ';
            buf[j++] = (insn & (1 << (31 - i))) ? '1' : '0';
         }
         vex_printf("disInstr(loongarch64): unhandled instruction 0x%08x\n", insn);
         vex_printf("disInstr(loongarch64): %s\n", buf);
      }

      /* Tell the dispatcher that this insn cannot be decoded, and so
         has not been executed, and (is currently) the next to be
         executed.  PC should be up-to-date since it is made so at the
         start of each insn, but nevertheless be paranoid and update
         it again right now. */
      putPC(mkU64(guest_PC_curr_instr));
      dres.len         = 0;
      dres.whatNext    = Dis_StopHere;
      dres.jk_StopHere = Ijk_NoDecode;
   }

   return dres;
}


/*--------------------------------------------------------------------*/
/*--- end                                 guest_loongarch64_toIR.c ---*/
/*--------------------------------------------------------------------*/
