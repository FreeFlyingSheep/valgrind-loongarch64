
/*---------------------------------------------------------------*/
/*--- begin                           host_loongarch64_defs.c ---*/
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
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, see <http://www.gnu.org/licenses/>.

   The GNU General Public License is contained in the file COPYING.
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_loongarch64_defs.h"


/* --------- Local helpers. --------- */

static inline void mapReg ( HRegRemap* m, HReg* r )
{
   *r = lookupHRegRemap(m, *r);
}

static inline Int extend ( UInt imm, UInt size )
{
   UInt shift = 32 - size;
   return (((Int)imm << shift) >> shift);
}


/* --------- Registers. --------- */

const RRegUniverse* getRRegUniverse_LOONGARCH64 ( void )
{
   /* The real-register universe is a big constant, so we just want to
      initialise it once. */
   static RRegUniverse rRegUniverse_LOONGARCH64;
   static Bool         rRegUniverse_LOONGARCH64_initted = False;

   /* Handy shorthand, nothing more */
   RRegUniverse* ru = &rRegUniverse_LOONGARCH64;

   /* This isn't thread-safe.  Sigh. */
   if (LIKELY(rRegUniverse_LOONGARCH64_initted == True))
      return ru;

   RRegUniverse__init(ru);

   /* Add the registers.  The initial segment of this array must be
      those available for allocation by reg-alloc, and those that
      follow are not available for allocation. */
   ru->allocable_start[HRcInt64] = ru->size;
   ru->regs[ru->size++] = hregLOONGARCH64_R23();
   ru->regs[ru->size++] = hregLOONGARCH64_R24();
   ru->regs[ru->size++] = hregLOONGARCH64_R25();
   ru->regs[ru->size++] = hregLOONGARCH64_R26();
   ru->regs[ru->size++] = hregLOONGARCH64_R27();
   ru->regs[ru->size++] = hregLOONGARCH64_R28();
   ru->regs[ru->size++] = hregLOONGARCH64_R29();
   ru->regs[ru->size++] = hregLOONGARCH64_R30();
   // $r31 is used as guest stack pointer, not available to regalloc.

   // $r12 is used as a chaining/spill/ProfInc temporary
   // $r13 is used as a ProfInc temporary
   ru->regs[ru->size++] = hregLOONGARCH64_R14();
   ru->regs[ru->size++] = hregLOONGARCH64_R15();
   ru->regs[ru->size++] = hregLOONGARCH64_R16();
   ru->regs[ru->size++] = hregLOONGARCH64_R17();
   ru->regs[ru->size++] = hregLOONGARCH64_R18();
   ru->regs[ru->size++] = hregLOONGARCH64_R19();
   ru->regs[ru->size++] = hregLOONGARCH64_R20();
   ru->allocable_end[HRcInt64] = ru->size - 1;

   ru->allocable_start[HRcFlt64] = ru->size;
   ru->regs[ru->size++] = hregLOONGARCH64_F24();
   ru->regs[ru->size++] = hregLOONGARCH64_F25();
   ru->regs[ru->size++] = hregLOONGARCH64_F26();
   ru->regs[ru->size++] = hregLOONGARCH64_F27();
   ru->regs[ru->size++] = hregLOONGARCH64_F28();
   ru->regs[ru->size++] = hregLOONGARCH64_F29();
   ru->regs[ru->size++] = hregLOONGARCH64_F30();
   ru->regs[ru->size++] = hregLOONGARCH64_F31();
   ru->allocable_end[HRcFlt64] = ru->size - 1;

   ru->allocable = ru->size;

   /* And other regs, not available to the allocator. */
   ru->regs[ru->size++] = hregLOONGARCH64_R0();
   ru->regs[ru->size++] = hregLOONGARCH64_R1();
   ru->regs[ru->size++] = hregLOONGARCH64_R2();
   ru->regs[ru->size++] = hregLOONGARCH64_R3();
   ru->regs[ru->size++] = hregLOONGARCH64_R4();
   ru->regs[ru->size++] = hregLOONGARCH64_R5();
   ru->regs[ru->size++] = hregLOONGARCH64_R6();
   ru->regs[ru->size++] = hregLOONGARCH64_R7();
   ru->regs[ru->size++] = hregLOONGARCH64_R8();
   ru->regs[ru->size++] = hregLOONGARCH64_R9();
   ru->regs[ru->size++] = hregLOONGARCH64_R10();
   ru->regs[ru->size++] = hregLOONGARCH64_R11();
   ru->regs[ru->size++] = hregLOONGARCH64_R12();
   ru->regs[ru->size++] = hregLOONGARCH64_R13();
   ru->regs[ru->size++] = hregLOONGARCH64_R21();
   ru->regs[ru->size++] = hregLOONGARCH64_R22();
   ru->regs[ru->size++] = hregLOONGARCH64_R31();
   ru->regs[ru->size++] = hregLOONGARCH64_FCSR3();

   rRegUniverse_LOONGARCH64_initted = True;

   RRegUniverse__check_is_sane(ru);
   return ru;
}

UInt ppHRegLOONGARCH64 ( HReg reg )
{
   Int r;
   Int ret = 0;
   static const HChar* ireg_names[32] = {
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
   static const HChar* freg_names[32] = {
      "$fa0",  "$fa1",  "$fa2",  "$fa3",  "$fa4",  "$fa5",  "$fa6",  "$fa7",
      "$ft0",  "$ft1",  "$ft2",  "$ft3",  "$ft4",  "$ft5",  "$ft6",  "$ft7",
      "$ft8",  "$ft9",  "$ft10", "$ft11", "$ft12", "$ft13", "$ft14", "$ft15",
      "$fs0",  "$fs1",  "$fs2",  "$fs3",  "$fs4",  "$fs5",  "$fs6",  "$fs7"
   };

   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      return ppHReg(reg);
   }

   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 4);
         ret = vex_printf("$fcsr%d", r);
         break;
      case HRcInt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         ret = vex_printf("%s", ireg_names[r]);
         break;
      case HRcFlt64:
         r = hregEncoding(reg);
         vassert(r >= 0 && r < 32);
         ret = vex_printf("%s", freg_names[r]);
         break;
      default:
         vpanic("ppHRegLOONGARCH64");
         break;
   }

   return ret;
}


/* --------- Condition codes, LOONGARCH64 encoding. --------- */

static inline const HChar* showLOONGARCH64CondCode ( LOONGARCH64CondCode cond )
{
   const HChar* ret;
   switch (cond) {
      case LAcc_EQ:
         ret = "eq";  /* equal */
         break;
      case LAcc_NE:
         ret = "ne";  /* not equal */
         break;
      case LAcc_LT:
         ret = "lt";  /* less than (signed) */
         break;
      case LAcc_GE:
         ret = "ge";  /* great equal (signed) */
         break;
      case LAcc_LTU:
         ret = "ltu"; /* less than (unsigned) */
         break;
      case LAcc_GEU:
         ret = "geu"; /* great equal (unsigned) */
         break;
      case LAcc_AL:
         ret = "al";  /* always (unconditional) */
         break;
      default:
         vpanic("showLOONGARCH64CondCode");
         break;
   }
   return ret;
}


/* --------- Memory address expressions (amodes). --------- */

LOONGARCH64AMode* LOONGARCH64AMode_RI ( HReg reg, UShort imm )
{
   LOONGARCH64AMode* am = LibVEX_Alloc_inline(sizeof(LOONGARCH64AMode));
   am->tag = LAam_RI;
   am->LAam.RI.base = reg;
   am->LAam.RI.index = imm;
   return am;
}

LOONGARCH64AMode* LOONGARCH64AMode_RR ( HReg base, HReg index )
{
   LOONGARCH64AMode* am = LibVEX_Alloc_inline(sizeof(LOONGARCH64AMode));
   am->tag = LAam_RR;
   am->LAam.RR.base = base;
   am->LAam.RR.index = index;
   return am;
}

static inline void ppLOONGARCH64AMode ( LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         ppHRegLOONGARCH64(am->LAam.RI.base);
         vex_printf(", ");
         vex_printf("%d", extend((UInt)am->LAam.RI.index, 12));
         break;
      case LAam_RR:
         ppHRegLOONGARCH64(am->LAam.RR.base);
         vex_printf(", ");
         ppHRegLOONGARCH64(am->LAam.RR.index);
         break;
      default:
         vpanic("ppLOONGARCH64AMode");
         break;
   }
}

static inline void addRegUsage_LOONGARCH64AMode( HRegUsage* u,
                                                 LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         addHRegUse(u, HRmRead, am->LAam.RI.base);
         break;
      case LAam_RR:
         addHRegUse(u, HRmRead, am->LAam.RR.base);
         addHRegUse(u, HRmRead, am->LAam.RR.index);
         break;
      default:
         vpanic("addRegUsage_LOONGARCH64AMode");
         break;
   }
}

static inline void mapRegs_LOONGARCH64AMode( HRegRemap* m,
                                             LOONGARCH64AMode* am )
{
   switch (am->tag) {
      case LAam_RI:
         mapReg(m, &am->LAam.RI.base);
         break;
      case LAam_RR:
         mapReg(m, &am->LAam.RR.base);
         mapReg(m, &am->LAam.RR.index);
         break;
      default:
         vpanic("mapRegs_LOONGARCH64AMode");
         break;
   }
}


/* --------- Operand, which can be reg or imm. --------- */

LOONGARCH64RI* LOONGARCH64RI_R ( HReg reg )
{
   LOONGARCH64RI* op = LibVEX_Alloc_inline(sizeof(LOONGARCH64RI));
   op->tag = LAri_Reg;
   op->LAri.R.reg = reg;
   return op;
}

LOONGARCH64RI* LOONGARCH64RI_I ( UShort imm, UChar size, Bool isSigned )
{
   LOONGARCH64RI* op = LibVEX_Alloc_inline(sizeof(LOONGARCH64RI));
   op->tag = LAri_Imm;
   op->LAri.I.imm = imm;
   op->LAri.I.size = size;
   op->LAri.I.isSigned = isSigned;
   vassert(imm < (1 << size));
   vassert(size == 5 || size == 6 || size == 12);
   return op;
}

static inline void ppLOONGARCH64RI ( LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         ppHRegLOONGARCH64(ri->LAri.R.reg);
         break;
      case LAri_Imm:
         if (ri->LAri.I.isSigned) {
            vex_printf("%d", extend((UInt)ri->LAri.I.imm, ri->LAri.I.size));
         } else {
            vex_printf("%u", (UInt)ri->LAri.I.imm);
         }
         break;
      default:
         vpanic("ppLOONGARCH64RI");
         break;
   }
}

static inline void addRegUsage_LOONGARCH64RI( HRegUsage* u, LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         addHRegUse(u, HRmRead, ri->LAri.R.reg);
         break;
      case LAri_Imm:
         break;
      default:
         vpanic("addRegUsage_LOONGARCH64RI");
         break;
   }
}

static inline void mapRegs_LOONGARCH64RI( HRegRemap* m, LOONGARCH64RI* ri )
{
   switch (ri->tag) {
      case LAri_Reg:
         mapReg(m, &ri->LAri.R.reg);
         break;
      case LAri_Imm:
         break;
      default:
         vpanic("mapRegs_LOONGARCH64RI");
         break;
   }
}


/* --------- Instructions. --------- */

static inline const HChar* showLOONGARCH64UnOp ( LOONGARCH64UnOp op )
{
   switch (op) {
      case LAun_CLZ_W:
         return "clz.w";
      case LAun_CTZ_W:
         return "ctz.w";
      case LAun_CLZ_D:
         return "clz.d";
      case LAun_CTZ_D:
         return "ctz.w";
      case LAun_EXT_W_H:
         return "ext.w.h";
      case LAun_EXT_W_B:
         return "ext.w.b";
      default:
         vpanic("showLOONGARCH64UnOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64BinOp ( LOONGARCH64BinOp op )
{
   switch (op) {
      case LAbin_ADD_W:
         return "add.w";
      case LAbin_ADD_D:
         return "add.d";
      case LAbin_SUB_W:
         return "sub.w";
      case LAbin_SUB_D:
         return "sub.d";
      case LAbin_NOR:
         return "nor";
      case LAbin_AND:
         return "and";
      case LAbin_OR:
         return "or";
      case LAbin_XOR:
         return "xor";
      case LAbin_SLL_W:
         return "sll.w";
      case LAbin_SRL_W:
         return "srl.w";
      case LAbin_SRA_W:
         return "sra.w";
      case LAbin_SLL_D:
         return "sll.d";
      case LAbin_SRL_D:
         return "srl.d";
      case LAbin_SRA_D:
         return "sra.d";
      case LAbin_MUL_W:
         return "mul.w";
      case LAbin_MUL_D:
         return "mul.d";
      case LAbin_MULH_W:
         return "mulh.w";
      case LAbin_MULH_WU:
         return "mulh.wu";
      case LAbin_MULH_D:
         return "mulh.d";
      case LAbin_MULH_DU:
         return "mulh.du";
      case LAbin_MULW_D_W:
         return "mulw.d.w";
      case LAbin_MULW_D_WU:
         return "mulw.d.wu";
      case LAbin_DIV_W:
         return "div.w";
      case LAbin_MOD_W:
         return "mod.w";
      case LAbin_DIV_WU:
         return "div.wu";
      case LAbin_MOD_WU:
         return "mod.wu";
      case LAbin_DIV_D:
         return "div.d";
      case LAbin_MOD_D:
         return "mod.d";
      case LAbin_DIV_DU:
         return "div.du";
      case LAbin_MOD_DU:
         return "mod.du";
      case LAbin_SLLI_W:
         return "slli.w";
      case LAbin_SLLI_D:
         return "slli.d";
      case LAbin_SRLI_W:
         return "srli.w";
      case LAbin_SRLI_D:
         return "srli.d";
      case LAbin_SRAI_W:
         return "srai.w";
      case LAbin_SRAI_D:
         return "srai.d";
      case LAbin_ADDI_W:
         return "addi.w";
      case LAbin_ADDI_D:
         return "addi.d";
      case LAbin_ANDI:
         return "andi";
      case LAbin_ORI:
         return "ori";
      case LAbin_XORI:
         return "xori";
      default:
         vpanic("showLOONGARCH64BinOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64LoadOp ( LOONGARCH64LoadOp op )
{
   switch (op) {
      case LAload_LD_D:
         return "ld.d";
      case LAload_LD_BU:
         return "ld.bu";
      case LAload_LD_HU:
         return "ld.hu";
      case LAload_LD_WU:
         return "ld.wu";
      case LAload_LDX_D:
         return "ldx.d";
      case LAload_LDX_BU:
         return "ldx.bu";
      case LAload_LDX_HU:
         return "ldx.hu";
      case LAload_LDX_WU:
         return "ldx.wu";
      default:
         vpanic("LOONGARCH64LoadOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64StoreOp ( LOONGARCH64StoreOp op )
{
   switch (op) {
      case LAstore_ST_B:
         return "st.b";
      case LAstore_ST_H:
         return "st.h";
      case LAstore_ST_W:
         return "st.w";
      case LAstore_ST_D:
         return "st.d";
      case LAstore_STX_B:
         return "stx.b";
      case LAstore_STX_H:
         return "stx.h";
      case LAstore_STX_W:
         return "stx.w";
      case LAstore_STX_D:
         return "stx.d";
      default:
         vpanic("LOONGARCH64StoreOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64LLSCOp ( LOONGARCH64LLSCOp op )
{
   switch (op) {
      case LAllsc_LL_W:
         return "ll.w";
      case LAllsc_SC_W:
         return "sc.w";
      case LAllsc_LL_D:
         return "ll.d";
      case LAllsc_SC_D:
         return "sc.d";
      default:
         vpanic("LOONGARCH64LLSCOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64BarOp ( LOONGARCH64BarOp op )
{
   const HChar* ret;
   switch (op) {
      case LAbar_DBAR:
         return "dbar";
      case LAbar_IBAR:
         return "ibar";
      default:
         vpanic("showLOONGARCH64BarOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpUnOp ( LOONGARCH64FpUnOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpun_FABS_S:
         return "fabs.s";
      case LAfpun_FABS_D:
         return "fabs.d";
      case LAfpun_FNEG_S:
         return "fneg.s";
      case LAfpun_FNEG_D:
         return "fneg.d";
      case LAfpun_FLOGB_S:
         return "flogb.s";
      case LAfpun_FLOGB_D:
         return "flogb.d";
      case LAfpun_FSQRT_S:
         return "fsqrt.s";
      case LAfpun_FSQRT_D:
         return "fsqrt.d";
      case LAfpun_FRSQRT_S:
         return "frsqrt.s";
      case LAfpun_FRSQRT_D:
         return "frsqrt.d";
      case LAfpun_FCVT_S_D:
         return "fcvt.s.d";
      case LAfpun_FCVT_D_S:
         return "fcvt.d.s";
      case LAfpun_FTINT_W_S:
         return "ftint.w.s";
      case LAfpun_FTINT_W_D:
         return "ftint.w.d";
      case LAfpun_FTINT_L_S:
         return "ftint.l.s";
      case LAfpun_FTINT_L_D:
         return "ftint.l.d";
      case LAfpun_FFINT_S_W:
         return "ffint.s.w";
      case LAfpun_FFINT_S_L:
         return "ffint.s.l";
      case LAfpun_FFINT_D_W:
         return "ffint.d.w";
      case LAfpun_FFINT_D_L:
         return "ffint.d.l";
      case LAfpun_FRINT_S:
         return "frint.s";
      case LAfpun_FRINT_D:
         return "frint.d";
      default:
         vpanic("showLOONGARCH64FpUnOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpBinOp ( LOONGARCH64FpBinOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpbin_FADD_S:
         return "fadd.s";
      case LAfpbin_FADD_D:
         return "fadd.d";
      case LAfpbin_FSUB_S:
         return "fsub.s";
      case LAfpbin_FSUB_D:
         return "fsub.d";
      case LAfpbin_FMUL_S:
         return "fmul.s";
      case LAfpbin_FMUL_D:
         return "fmul.d";
      case LAfpbin_FDIV_S:
         return "fdiv.s";
      case LAfpbin_FDIV_D:
         return "fdiv.d";
      case LAfpbin_FMAX_S:
         return "fmax.s";
      case LAfpbin_FMAX_D:
         return "fmax.d";
      case LAfpbin_FMIN_S:
         return "fmin.s";
      case LAfpbin_FMIN_D:
         return "fmin.d";
      case LAfpbin_FMAXA_S:
         return "fmaxa.s";
      case LAfpbin_FMAXA_D:
         return "fmaxa.d";
      case LAfpbin_FMINA_S:
         return "fmina.s";
      case LAfpbin_FMINA_D:
         return "fmina.d";
      case LAfpbin_FSCALEB_S:
         return "fscaleb.s";
      case LAfpbin_FSCALEB_D:
         return "fscaleb.d";
      default:
         vpanic("showLOONGARCH64FpBinOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpTriOp ( LOONGARCH64FpTriOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpbin_FMADD_S:
         return "fmadd.s";
      case LAfpbin_FMADD_D:
         return "fmadd.d";
      case LAfpbin_FMSUB_S:
         return "fmsub.s";
      case LAfpbin_FMSUB_D:
         return "fmsub.d";
      default:
         vpanic("showLOONGARCH64FpTriOp");
         break;
   }
   return ret;
}

static inline const HChar* showLOONGARCH64FpLoadOp ( LOONGARCH64FpLoadOp op )
{
   switch (op) {
      case LAfpload_FLD_S:
         return "fld.s";
      case LAfpload_FLD_D:
         return "fld.d";
      case LAfpload_FLDX_S:
         return "fldx.s";
      case LAfpload_FLDX_D:
         return "fldx.d";
      default:
         vpanic("LOONGARCH64FpLoadOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpStoreOp ( LOONGARCH64FpStoreOp op )
{
   switch (op) {
      case LAfpstore_FST_S:
         return "fst.s";
      case LAfpstore_FST_D:
         return "fst.d";
      case LAfpstore_FSTX_S:
         return "fstx.s";
      case LAfpstore_FSTX_D:
         return "fstx.d";
      default:
         vpanic("LOONGARCH64FpStoreOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpMoveOp ( LOONGARCH64FpMoveOp op )
{
   switch (op) {
      case LAfpmove_FMOV_S:
         return "fmov.s";
      case LAfpmove_FMOV_D:
         return "fmov.d";
      case LAfpmove_MOVGR2FR_W:
         return "movgr2fr.w";
      case LAfpmove_MOVGR2FR_D:
         return "movgr2fr.d";
      case LAfpmove_MOVFR2GR_S:
         return "movfr2gr.s";
      case LAfpmove_MOVFR2GR_D:
         return "movfr2gr.d";
      case LAfpmove_MOVGR2FCSR:
         return "movgr2fcsr";
      case LAfpmove_MOVFCSR2GR:
         return "movfcsr2gr";
      default:
         vpanic("showLOONGARCH64FpMoveOp");
         break;
   }
}

static inline const HChar* showLOONGARCH64FpCmpOp ( LOONGARCH64FpCmpOp op )
{
   const HChar* ret;
   switch (op) {
      case LAfpcmp_FCMP_CLT_S:
         return "fcmp.clt.s";
      case LAfpcmp_FCMP_CLT_D:
         return "fcmp.clt.d";
      case LAfpcmp_FCMP_CEQ_S:
         return "fcmp.ceq.s";
      case LAfpcmp_FCMP_CEQ_D:
         return "fcmp.ceq.d";
      case LAfpcmp_FCMP_CUN_S:
         return "fcmp.cun.s";
      case LAfpcmp_FCMP_CUN_D:
         return "fcmp.cun.d";
      default:
         vpanic("showLOONGARCH64FpCmpOp");
         break;
   }
   return ret;
}

LOONGARCH64Instr* LOONGARCH64Instr_LI ( ULong imm, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_LI;
   i->LAin.LI.imm      = imm;
   i->LAin.LI.dst      = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Unary ( LOONGARCH64UnOp op,
                                           HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Un;
   i->LAin.Unary.op    = op;
   i->LAin.Unary.src   = src;
   i->LAin.Unary.dst   = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Binary ( LOONGARCH64BinOp op,
                                            LOONGARCH64RI* src2,
                                            HReg src1, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Bin;
   i->LAin.Binary.op   = op;
   i->LAin.Binary.src2 = src2;
   i->LAin.Binary.src1 = src1;
   i->LAin.Binary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Load ( LOONGARCH64LoadOp op,
                                          LOONGARCH64AMode* src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Load;
   i->LAin.Load.op     = op;
   i->LAin.Load.src    = src;
   i->LAin.Load.dst    = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Store ( LOONGARCH64StoreOp op,
                                           LOONGARCH64AMode* dst, HReg src )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Store;
   i->LAin.Store.op    = op;
   i->LAin.Store.dst   = dst;
   i->LAin.Store.src   = src;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_LLSC ( LOONGARCH64LLSCOp op, Bool isLoad,
                                          LOONGARCH64AMode* addr, HReg val )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_LLSC;
   i->LAin.LLSC.op     = op;
   i->LAin.LLSC.isLoad = isLoad;
   i->LAin.LLSC.addr   = addr;
   i->LAin.LLSC.val    = val;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Bar ( LOONGARCH64BarOp op, UShort hint )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Bar;
   i->LAin.Bar.op      = op;
   i->LAin.Bar.hint    = hint;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpUnary ( LOONGARCH64FpUnOp op,
                                             HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpUn;
   i->LAin.FpUnary.op  = op;
   i->LAin.FpUnary.src = src;
   i->LAin.FpUnary.dst = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpBinary ( LOONGARCH64FpBinOp op, HReg src2,
                                              HReg src1, HReg dst )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_FpBin;
   i->LAin.FpBinary.op   = op;
   i->LAin.FpBinary.src2 = src2;
   i->LAin.FpBinary.src1 = src1;
   i->LAin.FpBinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpTrinary ( LOONGARCH64FpTriOp op,
                                               HReg src3, HReg src2,
                                               HReg src1, HReg dst )
{
   LOONGARCH64Instr* i    = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                 = LAin_FpTri;
   i->LAin.FpTrinary.op   = op;
   i->LAin.FpTrinary.src3 = src3;
   i->LAin.FpTrinary.src2 = src2;
   i->LAin.FpTrinary.src1 = src1;
   i->LAin.FpTrinary.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpLoad ( LOONGARCH64FpLoadOp op,
                                            LOONGARCH64AMode* src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpLoad;
   i->LAin.FpLoad.op   = op;
   i->LAin.FpLoad.src  = src;
   i->LAin.FpLoad.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpStore ( LOONGARCH64FpStoreOp op,
                                             LOONGARCH64AMode* dst, HReg src )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpStore;
   i->LAin.FpStore.op  = op;
   i->LAin.FpStore.dst = dst;
   i->LAin.FpStore.src = src;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpMove ( LOONGARCH64FpMoveOp op,
                                            HReg src, HReg dst )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_FpMove;
   i->LAin.FpMove.op   = op;
   i->LAin.FpMove.src  = src;
   i->LAin.FpMove.dst  = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_FpCmp ( LOONGARCH64FpCmpOp op, HReg src2,
                                           HReg src1, HReg dst )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_FpCmp;
   i->LAin.FpCmp.op      = op;
   i->LAin.FpCmp.src2    = src2;
   i->LAin.FpCmp.src1    = src1;
   i->LAin.FpCmp.dst     = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Cas ( HReg old, HReg addr, HReg expd,
                                         HReg data, Bool size64 )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_Cas;
   i->LAin.Cas.old     = old;
   i->LAin.Cas.addr    = addr;
   i->LAin.Cas.expd    = expd;
   i->LAin.Cas.data    = data;
   i->LAin.Cas.size64  = size64;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Cmp ( LOONGARCH64CondCode cond,
                                         HReg src2, HReg src1, HReg dst )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_Cmp;
   i->LAin.Cmp.cond     = cond;
   i->LAin.Cmp.src2     = src2;
   i->LAin.Cmp.src1     = src1;
   i->LAin.Cmp.dst      = dst;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_CMove ( HReg cond, HReg r0, HReg r1,
                                           HReg dst, Bool isInt )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_CMove;
   i->LAin.CMove.cond   = cond;
   i->LAin.CMove.r0     = r0;
   i->LAin.CMove.r1     = r1;
   i->LAin.CMove.dst    = dst;
   i->LAin.CMove.isInt  = isInt;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_Call ( HReg cond, Addr64 target,
                                          UInt nArgRegs, RetLoc rloc )
{
   LOONGARCH64Instr* i   = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                = LAin_Call;
   i->LAin.Call.cond     = cond;
   i->LAin.Call.target   = target;
   i->LAin.Call.nArgRegs = nArgRegs;
   i->LAin.Call.rloc     = rloc;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XDirect ( Addr64 dstGA,
                                             LOONGARCH64AMode* amPC,
                                             HReg cond, Bool toFastEP )
{
   LOONGARCH64Instr* i      = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                   = LAin_XDirect;
   i->LAin.XDirect.dstGA    = dstGA;
   i->LAin.XDirect.amPC     = amPC;
   i->LAin.XDirect.cond     = cond;
   i->LAin.XDirect.toFastEP = toFastEP;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XIndir ( HReg dstGA, LOONGARCH64AMode* amPC,
                                            HReg cond )
{
   LOONGARCH64Instr* i  = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag               = LAin_XIndir;
   i->LAin.XIndir.dstGA = dstGA;
   i->LAin.XIndir.amPC  = amPC;
   i->LAin.XIndir.cond  = cond;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_XAssisted ( HReg dstGA,
                                               LOONGARCH64AMode* amPC,
                                               HReg cond, IRJumpKind jk )
{
   LOONGARCH64Instr* i     = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                  = LAin_XAssisted;
   i->LAin.XAssisted.dstGA = dstGA;
   i->LAin.XAssisted.amPC  = amPC;
   i->LAin.XAssisted.cond  = cond;
   i->LAin.XAssisted.jk    = jk;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_EvCheck ( LOONGARCH64AMode* amCounter,
                                             LOONGARCH64AMode* amFailAddr )
{
   LOONGARCH64Instr* i        = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag                     = LAin_EvCheck;
   i->LAin.EvCheck.amCounter  = amCounter;
   i->LAin.EvCheck.amFailAddr = amFailAddr;
   return i;
}

LOONGARCH64Instr* LOONGARCH64Instr_ProfInc ( void )
{
   LOONGARCH64Instr* i = LibVEX_Alloc_inline(sizeof(LOONGARCH64Instr));
   i->tag              = LAin_ProfInc;
   return i;
}


/* -------- Pretty Print instructions ------------- */

static inline void ppLI ( ULong imm, HReg dst )
{
   vex_printf("li ");
   ppHRegLOONGARCH64(dst);
   vex_printf(", 0x%llx", imm);
}

static inline void ppUnary ( LOONGARCH64UnOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64UnOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppBinary ( LOONGARCH64BinOp op, LOONGARCH64RI* src2,
                              HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64BinOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppLOONGARCH64RI(src2);
}

static inline void ppLoad ( LOONGARCH64LoadOp op, LOONGARCH64AMode* src,
                            HReg dst )
{
   vex_printf("%s ", showLOONGARCH64LoadOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppLOONGARCH64AMode(src);
}

static inline void ppStore ( LOONGARCH64StoreOp op, LOONGARCH64AMode* dst,
                             HReg src )
{
   vex_printf("%s ", showLOONGARCH64StoreOp(op));
   ppHRegLOONGARCH64(src);
   vex_printf(", ");
   ppLOONGARCH64AMode(dst);
}

static inline void ppLLSC ( LOONGARCH64LLSCOp op, LOONGARCH64AMode* addr,
                            HReg val )
{
   vex_printf("%s ", showLOONGARCH64LLSCOp(op));
   ppHRegLOONGARCH64(val);
   vex_printf(", ");
   ppLOONGARCH64AMode(addr);
}

static inline void ppBar ( LOONGARCH64BarOp op, UShort hint )
{
   vex_printf("%s %u", showLOONGARCH64BarOp(op), (UInt)hint);
}

static inline void ppFpUnary ( LOONGARCH64FpUnOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpUnOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppFpBinary ( LOONGARCH64FpBinOp op, HReg src2,
                                HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpBinOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
}

static inline void ppFpTrinary ( LOONGARCH64FpTriOp op, HReg src3,
                                HReg src2, HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpTriOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf(", ");
   ppHRegLOONGARCH64(src3);
}

static inline void ppFpLoad ( LOONGARCH64FpLoadOp op, LOONGARCH64AMode* src,
                              HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpLoadOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppLOONGARCH64AMode(src);
}

static inline void ppFpStore ( LOONGARCH64FpStoreOp op, LOONGARCH64AMode* dst,
                               HReg src )
{
   vex_printf("%s ", showLOONGARCH64FpStoreOp(op));
   ppHRegLOONGARCH64(src);
   vex_printf(", ");
   ppLOONGARCH64AMode(dst);
}

static inline void ppFpMove ( LOONGARCH64FpMoveOp op, HReg src, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpMoveOp(op));
   ppHRegLOONGARCH64(dst);
   vex_printf(", ");
   ppHRegLOONGARCH64(src);
}

static inline void ppFpCmp ( LOONGARCH64FpCmpOp op, HReg src2,
                             HReg src1, HReg dst )
{
   vex_printf("%s ", showLOONGARCH64FpCmpOp(op));
   vex_printf("$fcc0, ");
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf("; movcf2gr ");
   ppHRegLOONGARCH64(dst);
   vex_printf(", $fcc0");
}

static inline void ppCas ( HReg old, HReg addr, HReg expd,
                           HReg data, Bool size64)
{
   ppHRegLOONGARCH64(old);
   vex_printf(" = cas(%dbit)(", size64 ? 64 : 32);
   ppHRegLOONGARCH64(expd);
   vex_printf(", ");
   ppHRegLOONGARCH64(data);
   vex_printf(" -> ");
   ppHRegLOONGARCH64(addr);
   vex_printf(")");
}

static inline void ppCmp ( LOONGARCH64CondCode cond, HReg src2,
                           HReg src1, HReg dst )
{
   ppHRegLOONGARCH64(dst);
   vex_printf(" = cmp%s(", showLOONGARCH64CondCode(cond));
   ppHRegLOONGARCH64(src1);
   vex_printf(", ");
   ppHRegLOONGARCH64(src2);
   vex_printf(")");
}

static inline void ppCMove ( HReg cond, HReg r0, HReg r1,
                             HReg dst, Bool isInt )
{
   if (isInt) {
      vex_printf("masknez $t0, ");
      ppHRegLOONGARCH64(r0);
      vex_printf(", ");
      ppHRegLOONGARCH64(cond);
      vex_printf("; maskeqz ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", ");
      ppHRegLOONGARCH64(r1);
      vex_printf(", ");
      ppHRegLOONGARCH64(cond);
      vex_printf("; or ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", $t0, ");
      ppHRegLOONGARCH64(dst);
   } else {
      vex_printf("movgr2cf ");
      ppHRegLOONGARCH64(cond);
      vex_printf(", $fcc0; fsel ");
      ppHRegLOONGARCH64(dst);
      vex_printf(", ");
      ppHRegLOONGARCH64(r0);
      vex_printf(", ");
      ppHRegLOONGARCH64(r1);
      vex_printf(", $fcc0");
   }
}

static inline void ppCall ( HReg cond, Addr64 target,
                            UInt nArgRegs, RetLoc rloc )
{
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("call 0x%llx [nArgRegs=%u, ", target, nArgRegs);
   ppRetLoc(rloc);
   vex_printf("]");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXDirect ( Addr64 dstGA, LOONGARCH64AMode* amPC,
                               HReg cond, Bool toFastEP )
{
   vex_printf("(xDirect) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("li $t0, 0x%llx; ", (ULong)dstGA);
   vex_printf("st.w $t0, ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; li $t0, $disp_cp_chain_me_to_%sEP; ",
              toFastEP ? "fast" : "slow");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXIndir ( HReg dstGA, LOONGARCH64AMode* amPC,
                              HReg cond )
{
   vex_printf("(xIndir) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("st.w ");
   ppHRegLOONGARCH64(dstGA);
   vex_printf(", ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; la $t0, disp_indir; ");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppXAssisted ( HReg dstGA, LOONGARCH64AMode* amPC,
                                 HReg cond, IRJumpKind jk)
{
   vex_printf("(xAssisted) ");
   if (!hregIsInvalid(cond)) {
      vex_printf("if (");
      ppHRegLOONGARCH64(cond);
      vex_printf(") { ");
   }
   vex_printf("st.w ");
   ppHRegLOONGARCH64(dstGA);
   vex_printf(", ");
   ppLOONGARCH64AMode(amPC);
   vex_printf("; li.w $s8, IRJumpKind_to_TRCVAL(%d); ", (Int)jk);
   vex_printf("la $t0, disp_assisted; ");
   vex_printf("jirl $ra, $t0, 0");
   if (!hregIsInvalid(cond))
      vex_printf(" }");
}

static inline void ppEvCheck ( LOONGARCH64AMode* amCounter,
                               LOONGARCH64AMode* amFailAddr )
{
   vex_printf("(evCheck) ");
   vex_printf("ld.w $t0, ");
   ppLOONGARCH64AMode(amCounter);
   vex_printf("; addi.d $t0, $t0, -1; ");
   vex_printf("st.w $t0, ");
   ppLOONGARCH64AMode(amCounter);
   vex_printf("; bge $t0, $zero, nofail; ");
   vex_printf("ld.d $t0, ");
   ppLOONGARCH64AMode(amFailAddr);
   vex_printf("; jirl $ra, $t0, 0");
   vex_printf("; nofail:");
}

static inline void ppProfInc ( void )
{
   vex_printf("(profInc) ");
   vex_printf("li $t0, NotKnownYet; ");
   vex_printf("ld.d $t1, $t0, 0; ");
   vex_printf("addi.d $t1, $t1, 1; ");
   vex_printf("st.d $t1, $t0, 0;");
}

void ppLOONGARCH64Instr ( const LOONGARCH64Instr* i, Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case LAin_LI:
         ppLI(i->LAin.LI.imm, i->LAin.LI.dst);
         break;
      case LAin_Un:
         ppUnary(i->LAin.Unary.op, i->LAin.Unary.src, i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         ppBinary(i->LAin.Binary.op, i->LAin.Binary.src2,
                  i->LAin.Binary.src1, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         ppLoad(i->LAin.Load.op, i->LAin.Load.src, i->LAin.Load.dst);
         break;
      case LAin_Store:
         ppStore(i->LAin.Store.op, i->LAin.Store.dst, i->LAin.Store.src);
         break;
      case LAin_LLSC:
         ppLLSC(i->LAin.LLSC.op, i->LAin.LLSC.addr, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         ppBar(i->LAin.Bar.op, i->LAin.Bar.hint);
         break;
      case LAin_FpUn:
         ppFpUnary(i->LAin.FpUnary.op, i->LAin.FpUnary.src,
                   i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         ppFpBinary(i->LAin.FpBinary.op, i->LAin.FpBinary.src2,
                    i->LAin.FpBinary.src1, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         ppFpTrinary(i->LAin.FpTrinary.op, i->LAin.FpTrinary.src3,
                     i->LAin.FpTrinary.src2, i->LAin.FpTrinary.src1,
                     i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         ppFpLoad(i->LAin.FpLoad.op, i->LAin.FpLoad.src, i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         ppFpStore(i->LAin.FpStore.op, i->LAin.FpStore.dst,
                   i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         ppFpMove(i->LAin.FpMove.op, i->LAin.FpMove.src,
                   i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         ppFpCmp(i->LAin.FpCmp.op, i->LAin.FpCmp.src2,
                 i->LAin.FpCmp.src1, i->LAin.FpCmp.dst);
         break;
      case LAin_Cas:
         ppCas(i->LAin.Cas.old, i->LAin.Cas.addr, i->LAin.Cas.expd,
               i->LAin.Cas.data, i->LAin.Cas.size64);
         break;
      case LAin_Cmp:
         ppCmp(i->LAin.Cmp.cond, i->LAin.Cmp.src2,
               i->LAin.Cmp.src1, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         ppCMove(i->LAin.CMove.cond, i->LAin.CMove.r0,
                 i->LAin.CMove.r1, i->LAin.CMove.dst,
                 i->LAin.CMove.isInt);
         break;
      case LAin_Call:
         ppCall(i->LAin.Call.cond, i->LAin.Call.target,
                i->LAin.Call.nArgRegs, i->LAin.Call.rloc);
         break;
      case LAin_XDirect:
         ppXDirect(i->LAin.XDirect.dstGA, i->LAin.XDirect.amPC,
                   i->LAin.XDirect.cond, i->LAin.XDirect.toFastEP);
         break;
      case LAin_XIndir:
         ppXIndir(i->LAin.XIndir.dstGA, i->LAin.XIndir.amPC,
                  i->LAin.XIndir.cond);
         break;
      case LAin_XAssisted:
         ppXAssisted(i->LAin.XAssisted.dstGA, i->LAin.XAssisted.amPC,
                     i->LAin.XAssisted.cond, i->LAin.XAssisted.jk);
         break;
      case LAin_EvCheck:
         ppEvCheck(i->LAin.EvCheck.amCounter, i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         ppProfInc();
         break;
      default:
         vpanic("ppLOONGARCH64Instr");
         break;
   }
}


/* --------- Helpers for register allocation. --------- */

void getRegUsage_LOONGARCH64Instr ( HRegUsage* u, const LOONGARCH64Instr* i,
                                    Bool mode64 )
{
   vassert(mode64 == True);
   initHRegUsage(u);
   switch (i->tag) {
      case LAin_LI:
         addHRegUse(u, HRmWrite, i->LAin.LI.dst);
         break;
      case LAin_Un:
         addHRegUse(u, HRmRead, i->LAin.Unary.src);
         addHRegUse(u, HRmWrite, i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         addRegUsage_LOONGARCH64RI(u, i->LAin.Binary.src2);
         addHRegUse(u, HRmRead, i->LAin.Binary.src1);
         addHRegUse(u, HRmWrite, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.Load.src);
         addHRegUse(u, HRmWrite, i->LAin.Load.dst);
         break;
      case LAin_Store:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.Store.dst);
         addHRegUse(u, HRmRead, i->LAin.Store.src);
         break;
      case LAin_LLSC:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.LLSC.addr);
         if (i->LAin.LLSC.isLoad)
            addHRegUse(u, HRmWrite, i->LAin.LLSC.val);
         else
            addHRegUse(u, HRmRead, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         /* No regs. */
         break;
      case LAin_FpUn:
         addHRegUse(u, HRmRead, i->LAin.FpUnary.src);
         addHRegUse(u, HRmWrite, i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         addHRegUse(u, HRmRead, i->LAin.FpBinary.src2);
         addHRegUse(u, HRmRead, i->LAin.FpBinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src3);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src2);
         addHRegUse(u, HRmRead, i->LAin.FpTrinary.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.FpLoad.src);
         addHRegUse(u, HRmWrite, i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.FpStore.dst);
         addHRegUse(u, HRmRead, i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         addHRegUse(u, HRmRead, i->LAin.FpMove.src);
         addHRegUse(u, HRmWrite, i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         addHRegUse(u, HRmRead, i->LAin.FpCmp.src2);
         addHRegUse(u, HRmRead, i->LAin.FpCmp.src1);
         addHRegUse(u, HRmWrite, i->LAin.FpCmp.dst);
         break;
      case LAin_Cas:
         addHRegUse(u, HRmWrite, i->LAin.Cas.old);
         addHRegUse(u, HRmRead, i->LAin.Cas.addr);
         addHRegUse(u, HRmRead, i->LAin.Cas.expd);
         addHRegUse(u, HRmModify, i->LAin.Cas.data);
         break;
      case LAin_Cmp:
         addHRegUse(u, HRmRead, i->LAin.Cmp.src2);
         addHRegUse(u, HRmRead, i->LAin.Cmp.src1);
         addHRegUse(u, HRmWrite, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         addHRegUse(u, HRmRead, i->LAin.CMove.cond);
         addHRegUse(u, HRmRead, i->LAin.CMove.r0);
         addHRegUse(u, HRmRead, i->LAin.CMove.r1);
         addHRegUse(u, HRmWrite, i->LAin.CMove.dst);
         break;
      case LAin_Call:
         /* logic and comments copied/modified from mips and arm64 back end */
         /* This is a bit subtle. */
         /* First off, we need to consider the cond register. */
         if (!hregIsInvalid(i->LAin.Call.cond))
            addHRegUse(u, HRmRead, i->LAin.Call.cond);
         /* Then, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction. */
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R14());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R15());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R16());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R17());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R18());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R19());
         addHRegUse(u, HRmWrite, hregLOONGARCH64_R20());
         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on nArgRegs. */
            switch (i->LAin.Call.nArgRegs) {
            case 8: addHRegUse(u, HRmRead, hregLOONGARCH64_R11()); /* fallthrough */
            case 7: addHRegUse(u, HRmRead, hregLOONGARCH64_R10()); /* fallthrough */
            case 6: addHRegUse(u, HRmRead, hregLOONGARCH64_R9());  /* fallthrough */
            case 5: addHRegUse(u, HRmRead, hregLOONGARCH64_R8());  /* fallthrough */
            case 4: addHRegUse(u, HRmRead, hregLOONGARCH64_R7());  /* fallthrough */
            case 3: addHRegUse(u, HRmRead, hregLOONGARCH64_R6());  /* fallthrough */
            case 2: addHRegUse(u, HRmRead, hregLOONGARCH64_R5());  /* fallthrough */
            case 1: addHRegUse(u, HRmRead, hregLOONGARCH64_R4());  /* fallthrough */
            case 0: break;
            default: vpanic("getRegUsage_LOONGARCH64:Call:regparms"); break;
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  However, we reserve $t0 for that
            purpose so there's no further complexity here.  Stating $t0
            as trashed is pointless since it's not under the control
            of the allocator, but what the hell. */
         addHRegUse(u, HRmWrite, hregT0());
         break;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case LAin_XDirect:
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XDirect.amPC);
         if (!hregIsInvalid(i->LAin.XDirect.cond))
            addHRegUse(u, HRmRead, i->LAin.XDirect.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_XIndir:
         addHRegUse(u, HRmRead, i->LAin.XIndir.dstGA);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XIndir.amPC);
         if (!hregIsInvalid(i->LAin.XIndir.cond))
            addHRegUse(u, HRmRead, i->LAin.XIndir.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_XAssisted:
         addHRegUse(u, HRmRead, i->LAin.XAssisted.dstGA);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.XAssisted.amPC);
         if (!hregIsInvalid(i->LAin.XAssisted.cond))
            addHRegUse(u, HRmRead, i->LAin.XAssisted.cond);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_EvCheck:
         /* We expect both amodes only to mention $r31, so this is in
            fact pointless, since $r31 isn't allocatable, but anyway.. */
         addRegUsage_LOONGARCH64AMode(u, i->LAin.EvCheck.amCounter);
         addRegUsage_LOONGARCH64AMode(u, i->LAin.EvCheck.amFailAddr);
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         break;
      case LAin_ProfInc:
         /* Again, pointless to actually state these since neither
            is available to RA. */
         addHRegUse(u, HRmWrite, hregT0()); /* unavail to RA */
         addHRegUse(u, HRmWrite, hregT1()); /* unavail to RA */
         break;
      default:
         ppLOONGARCH64Instr(i, mode64);
         vpanic("getRegUsage_LOONGARCH64Instr");
         break;
   }
}

void mapRegs_LOONGARCH64Instr ( HRegRemap* m, LOONGARCH64Instr* i,
                                Bool mode64 )
{
   vassert(mode64 == True);
   switch (i->tag) {
      case LAin_LI:
         mapReg(m, &i->LAin.LI.dst);
         break;
      case LAin_Un:
         mapReg(m, &i->LAin.Unary.src);
         mapReg(m, &i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         mapRegs_LOONGARCH64RI(m, i->LAin.Binary.src2);
         mapReg(m, &i->LAin.Binary.src1);
         mapReg(m, &i->LAin.Binary.dst);
         break;
      case LAin_Load:
         mapRegs_LOONGARCH64AMode(m, i->LAin.Load.src);
         mapReg(m, &i->LAin.Load.dst);
         break;
      case LAin_Store:
         mapRegs_LOONGARCH64AMode(m, i->LAin.Store.dst);
         mapReg(m, &i->LAin.Store.src);
         break;
      case LAin_LLSC:
         mapRegs_LOONGARCH64AMode(m, i->LAin.LLSC.addr);
         mapReg(m, &i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         /* No regs. */
         break;
      case LAin_FpUn:
         mapReg(m, &i->LAin.FpUnary.src);
         mapReg(m, &i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         mapReg(m, &i->LAin.FpBinary.src2);
         mapReg(m, &i->LAin.FpBinary.src1);
         mapReg(m, &i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         mapReg(m, &i->LAin.FpTrinary.src3);
         mapReg(m, &i->LAin.FpTrinary.src2);
         mapReg(m, &i->LAin.FpTrinary.src1);
         mapReg(m, &i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         mapRegs_LOONGARCH64AMode(m, i->LAin.FpLoad.src);
         mapReg(m, &i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         mapRegs_LOONGARCH64AMode(m, i->LAin.FpStore.dst);
         mapReg(m, &i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         mapReg(m, &i->LAin.FpMove.src);
         mapReg(m, &i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         mapReg(m, &i->LAin.FpCmp.src2);
         mapReg(m, &i->LAin.FpCmp.src1);
         mapReg(m, &i->LAin.FpCmp.dst);
         break;
      case LAin_Cas:
         mapReg(m, &i->LAin.Cas.old);
         mapReg(m, &i->LAin.Cas.addr);
         mapReg(m, &i->LAin.Cas.expd);
         mapReg(m, &i->LAin.Cas.data);
         break;
      case LAin_Cmp:
         mapReg(m, &i->LAin.Cmp.src2);
         mapReg(m, &i->LAin.Cmp.src1);
         mapReg(m, &i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         mapReg(m, &i->LAin.CMove.cond);
         mapReg(m, &i->LAin.CMove.r0);
         mapReg(m, &i->LAin.CMove.r1);
         mapReg(m, &i->LAin.CMove.dst);
         break;
      case LAin_Call:
         if (!hregIsInvalid(i->LAin.Call.cond))
            mapReg(m, &i->LAin.Call.cond);
         /* Hardwires $r12. */
         break;
      /* XDirect/XIndir/XAssisted are also a bit subtle.  They
         conditionally exit the block.  Hence we only need to list (1)
         the registers that they read, and (2) the registers that they
         write in the case where the block is not exited.  (2) is
         empty, hence only (1) is relevant here. */
      case LAin_XDirect:
         mapRegs_LOONGARCH64AMode(m, i->LAin.XDirect.amPC);
         if (!hregIsInvalid(i->LAin.XDirect.cond))
            mapReg(m, &i->LAin.XDirect.cond);
         break;
      case LAin_XIndir:
         mapReg(m, &i->LAin.XIndir.dstGA);
         mapRegs_LOONGARCH64AMode(m, i->LAin.XIndir.amPC);
         if (!hregIsInvalid(i->LAin.XIndir.cond))
            mapReg(m, &i->LAin.XIndir.cond);
         break;
      case LAin_XAssisted:
         mapReg(m, &i->LAin.XAssisted.dstGA);
         mapRegs_LOONGARCH64AMode(m, i->LAin.XAssisted.amPC);
         if (!hregIsInvalid(i->LAin.XAssisted.cond))
            mapReg(m, &i->LAin.XAssisted.cond);
         break;
      case LAin_EvCheck:
         /* We expect both amodes only to mention $r31, so this is in
            fact pointless, since $r31 isn't allocatable, but anyway.. */
         mapRegs_LOONGARCH64AMode(m, i->LAin.EvCheck.amCounter);
         mapRegs_LOONGARCH64AMode(m, i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         /* Hardwires $r12 and $r13 -- nothing to modify. */
         break;
      default:
         ppLOONGARCH64Instr(i, mode64);
         vpanic("mapRegs_LOONGARCH64Instr");
         break;
   }
}

/* Generate loongarch64 spill instructions under the direction of the
   register allocator. */
void genSpill_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                            HReg rreg, Int offsetB, Bool mode64 )
{
   vassert(mode64 == True);
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   LOONGARCH64AMode* am;
   *i1 = *i2 = NULL;
   am = LOONGARCH64AMode_RI(hregGSP(), offsetB);

   switch (hregClass(rreg)) {
      case HRcInt64:
         *i1 = LOONGARCH64Instr_Store(LAstore_ST_D, am, rreg);
         break;
      case HRcFlt64:
         *i1 = LOONGARCH64Instr_FpStore(LAfpstore_FST_D, am, rreg);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genSpill_LOONGARCH64: unimplemented regclass");
         break;
   }
}

/* Generate loongarch64 reload instructions under the direction of the
   register allocator. */
void genReload_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                             HReg rreg, Int offsetB, Bool mode64 )
{
   vassert(mode64 == True);
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));

   LOONGARCH64AMode* am;
   *i1 = *i2 = NULL;
   am = LOONGARCH64AMode_RI(hregGSP(), offsetB);

   switch (hregClass(rreg)) {
      case HRcInt64:
         *i1 = LOONGARCH64Instr_Load(LAload_LD_D, am, rreg);
         break;
      case HRcFlt64:
         *i1 = LOONGARCH64Instr_FpLoad(LAfpload_FLD_D, am, rreg);
         break;
      default:
         ppHRegClass(hregClass(rreg));
         vpanic("genReload_LOONGARCH64: unimplemented regclass");
         break;
   }
}

/* Generate loongarch64 move instructions under the direction of the
   register allocator. */
LOONGARCH64Instr* genMove_LOONGARCH64 ( HReg from, HReg to, Bool mode64 )
{
   vassert(mode64 == True);
   switch (hregClass(from)) {
      case HRcInt64:
         return LOONGARCH64Instr_Binary(LAbin_OR,
                                        LOONGARCH64RI_R(hregZERO()),
                                        from, to);
      case HRcFlt64:
         return LOONGARCH64Instr_FpMove(LAfpmove_FMOV_D, from, to);
      default:
         ppHRegClass(hregClass(from));
         vpanic("genMove_LOONGARCH64: unimplemented regclass");
   }
}


/* --------- The loongarch64 assembler --------- */

static inline UInt iregEnc ( HReg r )
{
   vassert(hregClass(r) == HRcInt64);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt fregEnc ( HReg r )
{
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt fcsrEnc ( HReg r )
{
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   UInt n = hregEncoding(r);
   vassert(n < 32);
   return n;
}

static inline UInt emit_op_rj_rd ( UInt op, UInt rj, UInt rd )
{
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (rj << 5) | rd;
}

static inline UInt emit_op_rk_rj_rd ( UInt op, UInt rk, UInt rj, UInt rd )
{
   vassert(rk < (1 << 5));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (rk << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_fj_fd ( UInt op, UInt fj, UInt fd )
{
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fj << 5) | fd;
}

static inline UInt emit_op_fa_fk_fj_fd ( UInt op, UInt fa, UInt fk, UInt fj, UInt fd )
{
   vassert(fa < (1 << 5));
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fa << 15) | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_fk_fj_fd ( UInt op, UInt fk, UInt fj, UInt fd )
{
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_ca_fk_fj_fd ( UInt op, UInt ca, UInt fk, UInt fj, UInt fd )
{
   vassert(ca < (1 << 3));
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (ca << 15) | (fk << 10) | (fj << 5) | fd;
}

static inline UInt emit_op_fk_fj_cd ( UInt op, UInt fk, UInt fj, UInt cd )
{
   vassert(fk < (1 << 5));
   vassert(fj < (1 << 5));
   vassert(cd < (1 << 3));
   return op | (fk << 10) | (fj << 5) | cd;
}

static inline UInt emit_op_cj_rd ( UInt op, UInt cj, UInt rd )
{
   vassert(cj < (1 << 3));
   vassert(rd < (1 << 5));
   return op | (cj << 5) | rd;
}

static inline UInt emit_op_rj_cd ( UInt op, UInt rj, UInt cd )
{
   vassert(rj < (1 << 5));
   vassert(cd < (1 << 3));
   return op | (rj << 5) | cd;
}

static inline UInt emit_op_rj_fd ( UInt op, UInt rj, UInt fd )
{
   vassert(rj < (1 << 5));
   vassert(fd < (1 << 5));
   return op | (rj << 5) | fd;
}

static inline UInt emit_op_fj_rd ( UInt op, UInt fj, UInt rd )
{
   vassert(fj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (fj << 5) | rd;
}

static inline UInt emit_op_rj_fcsr ( UInt op, UInt rj, UInt fcsr )
{
   vassert(rj < (1 << 5));
   vassert(fcsr < (1 << 5));
   return op | (rj << 5) | fcsr;
}

static inline UInt emit_op_fcsr_rd ( UInt op, UInt fcsr, UInt rd )
{
   vassert(fcsr < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (fcsr << 5) | rd;
}

static inline UInt emit_op_ui5_rj_rd ( UInt op, UInt ui5, UInt rj, UInt rd )
{
   vassert(ui5 < (1 << 5));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui5 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_ui6_rj_rd ( UInt op, UInt ui6, UInt rj, UInt rd )
{
   vassert(ui6 < (1 << 6));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui6 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_ui12_rj_rd ( UInt op, UInt ui12, UInt rj, UInt rd )
{
   vassert(ui12 < (1 << 12));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (ui12 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si12_rj_rd ( UInt op, UInt si12, UInt rj, UInt rd )
{
   vassert(si12 < (1 << 12));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (si12 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si14_rj_rd ( UInt op, UInt si14, UInt rj, UInt rd )
{
   vassert(si14 < (1 << 14));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (si14 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_si20_rd ( UInt op, UInt si20, UInt rd )
{
   vassert(si20 < (1 << 20));
   vassert(rd < (1 << 5));
   return op | (si20 << 5) | rd;
}

static inline UInt emit_op_offs16_rj_rd ( UInt op, UInt offs16, UInt rj, UInt rd )
{
   vassert(offs16 < (1 << 16));
   vassert(rj < (1 << 5));
   vassert(rd < (1 << 5));
   return op | (offs16 << 10) | (rj << 5) | rd;
}

static inline UInt emit_op_offs26 ( UInt op, UInt offs26 )
{
   vassert(offs26 < (1 << 26));
   return op | ((offs26 & 0xffff) << 10) | (offs26 >> 16);
}

static inline UInt emit_op_hint15 ( UInt op, UInt hint )
{
   vassert(hint < (1 << 15));
   return op | hint;
}

static UInt* mkLoadImm_EXACTLY4 ( UInt* p, HReg dst, ULong imm )
{
   /*
      lu12i.w dst, imm[31:12]
      ori     dst, dst, imm[11:0]
      lu32i.d dst, imm[51:32]
      lu52i.d dst, dst, imm[63:52]
    */
   UInt d = iregEnc(dst);
   *p++ = emit_op_si20_rd(LAextra_LU12I_W, (imm >> 12) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm & 0xfff, d, d);
   *p++ = emit_op_si20_rd(LAextra_LU32I_D, (imm >> 32) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAextra_LU52I_D, (imm >> 52) & 0xfff, d, d);
   return p;
}

static inline UInt* mkLoadImm_EXACTLY2 ( UInt* p, HReg dst, ULong imm )
{
   /*
      lu12i.w dst, imm[31:12]
      ori     dst, dst, imm[11:0]
    */
   UInt d = iregEnc(dst);
   *p++ = emit_op_si20_rd(LAextra_LU12I_W, (imm >> 12) & 0xfffff, d);
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm & 0xfff, d, d);
   return p;
}

static inline UInt* mkLoadImm_EXACTLY1 ( UInt* p, HReg dst, ULong imm )
{
   /* ori dst, $zero, imm[11:0] */
   *p++ = emit_op_si12_rj_rd(LAbin_ORI, imm, 0, iregEnc(dst));
   return p;
}

static UInt* mkLoadImm ( UInt* p, HReg dst, ULong imm )
{
   if ((imm >> 12) == 0)
      p = mkLoadImm_EXACTLY1(p, dst, imm);
   else if (imm < 0x80000000 || (imm >> 31) == 0x1ffffffffUL)
      p = mkLoadImm_EXACTLY2(p, dst, imm);
   else
      p = mkLoadImm_EXACTLY4(p, dst, imm);
   return p;
}

static Bool is_LoadImm_EXACTLY4 ( UInt* p, HReg dst, ULong imm )
{
   UInt expect[4];
   mkLoadImm_EXACTLY4(expect, dst, imm);
   return toBool(p[0] == expect[0] && p[1] == expect[1] &&
                 p[2] == expect[2] && p[3] == expect[3]);
}

static inline UInt* mkUnary ( UInt* p, LOONGARCH64UnOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAun_CLZ_W:
      case LAun_CTZ_W:
      case LAun_CLZ_D:
      case LAun_CTZ_D:
      case LAun_EXT_W_H:
      case LAun_EXT_W_B:
         *p++ = emit_op_rj_rd(op, iregEnc(src), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkBinary ( UInt* p, LOONGARCH64BinOp op,
                               LOONGARCH64RI* src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAbin_ADD_W:
      case LAbin_ADD_D:
      case LAbin_SUB_W:
      case LAbin_SUB_D:
      case LAbin_NOR:
      case LAbin_AND:
      case LAbin_OR:
      case LAbin_XOR:
      case LAbin_SLL_W:
      case LAbin_SRL_W:
      case LAbin_SRA_W:
      case LAbin_SLL_D:
      case LAbin_SRL_D:
      case LAbin_SRA_D:
      case LAbin_MUL_W:
      case LAbin_MUL_D:
      case LAbin_MULH_W:
      case LAbin_MULH_WU:
      case LAbin_MULH_D:
      case LAbin_MULH_DU:
      case LAbin_MULW_D_W:
      case LAbin_MULW_D_WU:
      case LAbin_DIV_W:
      case LAbin_MOD_W:
      case LAbin_DIV_WU:
      case LAbin_MOD_WU:
      case LAbin_DIV_D:
      case LAbin_MOD_D:
      case LAbin_DIV_DU:
      case LAbin_MOD_DU:
         vassert(src2->tag == LAri_Reg);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src2->LAri.R.reg),
                                 iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_SLLI_W:
      case LAbin_SRLI_W:
      case LAbin_SRAI_W:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui5_rj_rd(op, src2->LAri.I.imm,
                                  iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_SLLI_D:
      case LAbin_SRLI_D:
      case LAbin_SRAI_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui6_rj_rd(op, src2->LAri.I.imm,
                                  iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_ADDI_W:
      case LAbin_ADDI_D:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_si12_rj_rd(op, src2->LAri.I.imm,
                                   iregEnc(src1), iregEnc(dst));
         return p;
      case LAbin_ANDI:
      case LAbin_ORI:
      case LAbin_XORI:
         vassert(src2->tag == LAri_Imm);
         *p++ = emit_op_ui12_rj_rd(op, src2->LAri.I.imm,
                                   iregEnc(src1), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static UInt* mkLoad ( UInt* p, LOONGARCH64LoadOp op,
                      LOONGARCH64AMode* src, HReg dst )
{
   switch (op) {
      case LAload_LD_W:
      case LAload_LD_D:
      case LAload_LD_BU:
      case LAload_LD_HU:
      case LAload_LD_WU:
         vassert(src->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, src->LAam.RI.index,
                                   iregEnc(src->LAam.RI.base), iregEnc(dst));
         return p;
      case LAload_LDX_D:
      case LAload_LDX_BU:
      case LAload_LDX_HU:
      case LAload_LDX_WU:
         vassert(src->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src->LAam.RR.index),
                                 iregEnc(src->LAam.RR.base), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static UInt* mkStore ( UInt* p, LOONGARCH64StoreOp op,
                       LOONGARCH64AMode* dst, HReg src )
{
   switch (op) {
      case LAstore_ST_B:
      case LAstore_ST_H:
      case LAstore_ST_W:
      case LAstore_ST_D:
         vassert(dst->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, dst->LAam.RI.index,
                                   iregEnc(dst->LAam.RI.base), iregEnc(src));
         return p;
      case LAstore_STX_B:
      case LAstore_STX_H:
      case LAstore_STX_W:
      case LAstore_STX_D:
         vassert(dst->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(dst->LAam.RR.index),
                                 iregEnc(dst->LAam.RR.base), iregEnc(src));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkLLSC ( UInt* p, LOONGARCH64LLSCOp op,
                             LOONGARCH64AMode* addr, HReg val )
{
   switch (op) {
      case LAllsc_LL_W:
      case LAllsc_SC_W:
      case LAllsc_LL_D:
      case LAllsc_SC_D:
         vassert(addr->tag == LAam_RI);
         *p++ = emit_op_si14_rj_rd(op, addr->LAam.RI.index,
                                   iregEnc(addr->LAam.RI.base), iregEnc(val));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkBar ( UInt* p, LOONGARCH64BarOp op, UShort hint )
{
   switch (op) {
      case LAbar_DBAR:
      case LAbar_IBAR:
         *p++ = emit_op_hint15(op, hint);
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpUnary ( UInt* p, LOONGARCH64FpUnOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAfpun_FABS_S:
      case LAfpun_FABS_D:
      case LAfpun_FNEG_S:
      case LAfpun_FNEG_D:
      case LAfpun_FLOGB_S:
      case LAfpun_FLOGB_D:
      case LAfpun_FSQRT_S:
      case LAfpun_FSQRT_D:
      case LAfpun_FRSQRT_S:
      case LAfpun_FRSQRT_D:
      case LAfpun_FCVT_S_D:
      case LAfpun_FCVT_D_S:
      case LAfpun_FTINT_W_S:
      case LAfpun_FTINT_W_D:
      case LAfpun_FTINT_L_S:
      case LAfpun_FTINT_L_D:
      case LAfpun_FFINT_S_W:
      case LAfpun_FFINT_S_L:
      case LAfpun_FFINT_D_W:
      case LAfpun_FFINT_D_L:
      case LAfpun_FRINT_S:
      case LAfpun_FRINT_D:
         *p++ = emit_op_fj_fd(op, fregEnc(src), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpBinary ( UInt* p, LOONGARCH64FpBinOp op, HReg src2,
                                 HReg src1, HReg dst )
{
   switch (op) {
      case LAfpbin_FADD_S:
      case LAfpbin_FADD_D:
      case LAfpbin_FSUB_S:
      case LAfpbin_FSUB_D:
      case LAfpbin_FMUL_S:
      case LAfpbin_FMUL_D:
      case LAfpbin_FDIV_S:
      case LAfpbin_FDIV_D:
      case LAfpbin_FMAX_S:
      case LAfpbin_FMAX_D:
      case LAfpbin_FMIN_S:
      case LAfpbin_FMIN_D:
      case LAfpbin_FMAXA_S:
      case LAfpbin_FMAXA_D:
      case LAfpbin_FMINA_S:
      case LAfpbin_FMINA_D:
      case LAfpbin_FSCALEB_S:
      case LAfpbin_FSCALEB_D:
         *p++ = emit_op_fk_fj_fd(op, fregEnc(src2), fregEnc(src1), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpTrinary ( UInt* p, LOONGARCH64FpTriOp op, HReg src3,
                                  HReg src2, HReg src1, HReg dst )
{
   switch (op) {
      case LAfpbin_FMADD_S:
      case LAfpbin_FMADD_D:
      case LAfpbin_FMSUB_S:
      case LAfpbin_FMSUB_D:
         *p++ = emit_op_fa_fk_fj_fd(op, fregEnc(src3), fregEnc(src2),
                                    fregEnc(src1), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpLoad ( UInt* p, LOONGARCH64FpLoadOp op,
                               LOONGARCH64AMode* src, HReg dst )
{
   switch (op) {
      case LAfpload_FLD_S:
      case LAfpload_FLD_D:
         vassert(src->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, src->LAam.RI.index,
                                   iregEnc(src->LAam.RI.base), fregEnc(dst));
         return p;
      case LAfpload_FLDX_S:
      case LAfpload_FLDX_D:
         vassert(src->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(src->LAam.RR.index),
                                 iregEnc(src->LAam.RR.base), fregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpStore ( UInt* p, LOONGARCH64FpStoreOp op,
                                LOONGARCH64AMode* dst, HReg src )
{
   switch (op) {
      case LAfpstore_FST_S:
      case LAfpstore_FST_D:
         vassert(dst->tag == LAam_RI);
         *p++ = emit_op_si12_rj_rd(op, dst->LAam.RI.index,
                                   iregEnc(dst->LAam.RI.base), fregEnc(src));
         return p;
      case LAfpstore_FSTX_S:
      case LAfpstore_FSTX_D:
         vassert(dst->tag == LAam_RR);
         *p++ = emit_op_rk_rj_rd(op, iregEnc(dst->LAam.RR.index),
                                 iregEnc(dst->LAam.RR.base), fregEnc(src));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpMove ( UInt* p, LOONGARCH64FpMoveOp op, HReg src, HReg dst )
{
   switch (op) {
      case LAfpmove_FMOV_S:
      case LAfpmove_FMOV_D:
         *p++ = emit_op_fj_fd(op, fregEnc(src), fregEnc(dst));
         return p;
      case LAfpmove_MOVGR2FR_W:
      case LAfpmove_MOVGR2FR_D:
         *p++ = emit_op_rj_fd(op, iregEnc(src), fregEnc(dst));
         return p;
      case LAfpmove_MOVFR2GR_S:
      case LAfpmove_MOVFR2GR_D:
         *p++ = emit_op_fj_rd(op, fregEnc(src), iregEnc(dst));
         return p;
      case LAfpmove_MOVGR2FCSR:
         *p++ = emit_op_rj_fcsr(op, iregEnc(src), fcsrEnc(dst));
         return p;
      case LAfpmove_MOVFCSR2GR:
         *p++ = emit_op_fcsr_rd(op, fcsrEnc(src), iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkFpCmp ( UInt* p, LOONGARCH64FpCmpOp op, HReg src2,
                              HReg src1, HReg dst )
{
   /*
      fcmp.cond.[sd] $fcc0, src1, src2
      movcf2gr       dst, $fcc0
    */
   switch (op) {
      case LAfpcmp_FCMP_CLT_S:
      case LAfpcmp_FCMP_CLT_D:
      case LAfpcmp_FCMP_CEQ_S:
      case LAfpcmp_FCMP_CEQ_D:
      case LAfpcmp_FCMP_CUN_S:
      case LAfpcmp_FCMP_CUN_D:
         *p++ = emit_op_fk_fj_cd(op, fregEnc(src2), fregEnc(src1), 0);
         *p++ = emit_op_cj_rd(LAextra_MOVCF2GR, 0, iregEnc(dst));
         return p;
      default:
         return NULL;
   }
}

static inline UInt* mkCas ( UInt* p, HReg old, HReg addr, HReg expd,
                            HReg data, Bool size64 )
{
   /*
         ll.[wd] old, addr, 0
         bne     old, expd, barrier
         or      $t0, data, $zero
         sc.[wd] $t0, addr, 0
         beq     $t0, zero, fail
         or      old, expd, $zero
         b       end
      barrier:
         dbar    0
      fail:
         or      old, data, $zero
      end:
    */
   UInt o = iregEnc(old);
   UInt a = iregEnc(addr);
   UInt e = iregEnc(expd);
   UInt d = iregEnc(data);
   UInt t = 12;
   UInt z = 0;

   if (size64) {
      *p++ = emit_op_si14_rj_rd(LAllsc_LL_D, 0, a, o);
   } else {
      *p++ = emit_op_ui6_rj_rd(LAbin_SLLI_W, 0, e, e); // Sign-extend expd
      *p++ = emit_op_si14_rj_rd(LAllsc_LL_W, 0, a, o);
   }
   *p++ = emit_op_offs16_rj_rd(LAextra_BNE, 6, o, e);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, d, t);
   if (size64) {
      *p++ = emit_op_si14_rj_rd(LAllsc_SC_D, 0, a, t);
   } else {
      *p++ = emit_op_si14_rj_rd(LAllsc_SC_W, 0, a, t);
   }
   *p++ = emit_op_offs16_rj_rd(LAextra_BEQ, 4, t, z);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, e, o);
   *p++ = emit_op_offs26(LAextra_B, 3);
   *p++ = emit_op_hint15(LAbar_DBAR, 0);
   *p++ = emit_op_rk_rj_rd(LAbin_OR, z, d, o);
   return p;
}

static inline UInt* mkCmp ( UInt* p, LOONGARCH64CondCode cond,
                            HReg src2, HReg src1, HReg dst )
{
   UInt d  = iregEnc(dst);
   UInt s1 = iregEnc(src1);
   UInt s2 = iregEnc(src2);

   switch (cond) {
      case LAcc_EQ:
         /*
            xor   dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAbin_XOR, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      case LAcc_NE:
         /*
            xor   dst, src1, src2
            sltu  dst, $zero, dst
          */
         *p++ = emit_op_rk_rj_rd(LAbin_XOR, s2, s1, d);
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, d, 0, d);
         return p;
      case LAcc_LT:
         /* slt   dst, src1, src2 */
         *p++ = emit_op_rk_rj_rd(LAextra_SLT, s2, s1, d);
         return p;
      case LAcc_GE:
         /*
            slt   dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAextra_SLT, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      case LAcc_LTU:
         /* sltu  dst, src1, src2 */
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, s2, s1, d);
         return p;
      case LAcc_GEU:
         /*
            sltu  dst, src1, src2
            sltui dst, dst, 1
          */
         *p++ = emit_op_rk_rj_rd(LAextra_SLTU, s2, s1, d);
         *p++ = emit_op_si12_rj_rd(LAextra_SLTUI, 1, d, d);
         return p;
      /* No LAcc_AL here.
         case LAcc_AL:
            break;
       */
      default:
         return NULL;
   }
}

static inline UInt* mkCMove ( UInt* p, HReg cond, HReg r0,
                              HReg r1, HReg dst, Bool isInt )
{
   if (isInt) {
      /*
         masknez $t0, r0, cond
         maskeqz dst, r1, cond
         or      dst, $t0, dst
       */
      UInt c = iregEnc(cond);
      UInt d = iregEnc(dst);
      *p++ = emit_op_rk_rj_rd(LAextra_MASKNEZ, c, iregEnc(r0), 12);
      *p++ = emit_op_rk_rj_rd(LAextra_MASKEQZ, c, iregEnc(r1), d);
      *p++ = emit_op_rk_rj_rd(LAbin_OR, d, 12, d);
   } else {
      /*
         movgr2cf $fcc0, cond
         fsel     dst, r0, r1, $fcc0
       */
      *p++ = emit_op_rj_cd(LAextra_MOVGR2CF, iregEnc(cond), 0);
      *p++ = emit_op_ca_fk_fj_fd(LAextra_FSEL, 0, fregEnc(r1),
                                 fregEnc(r0), fregEnc(dst));
   }
   return p;
}

static inline UInt* mkCall ( UInt* p, HReg cond, Addr64 target, RetLoc rloc )
{
   if (!hregIsInvalid(cond) && rloc.pri != RLPri_None) {
      /* The call might not happen (it isn't unconditional) and
         it returns a result.  In this case we will need to
         generate a control flow diamond to put 0x555..555 in
         the return register(s) in the case where the call
         doesn't happen.  If this ever becomes necessary, maybe
         copy code from the 32-bit ARM equivalent.  Until that
         day, just give up. */
      return NULL;
   }

   UInt* ptmp = NULL;
   if (!hregIsInvalid(cond)) {
      /* Create a hole to put a conditional branch in.  We'll
         patch it once we know the branch length. */
      ptmp = p;
      p++;
   }

   /*
      $t0 = target
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), target);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Patch the hole if necessary */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 3 && offs <= 6);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXDirect ( UInt* p, Addr64 dstGA,
                                LOONGARCH64AMode* amPC,
                                HReg cond, Bool toFastEP,
                                const void* disp_cp_chain_me_to_slowEP,
                                const void* disp_cp_chain_me_to_fastEP )
{
   /* NB: what goes on here has to be very closely coordinated
      with chainXDirect_LOONGARCH64 and unchainXDirect_LOONGARCH64 below. */
   /* We're generating chain-me requests here, so we need to be
      sure this is actually allowed -- no-redir translations
      can't use chain-me's.  Hence: */
   vassert(disp_cp_chain_me_to_slowEP != NULL);
   vassert(disp_cp_chain_me_to_fastEP != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UInt* ptmp = NULL;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it.  Or at least, leave a space for
      it that we will shortly fill in. */
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      $t0 = dstGA
      st.d $t0, amPC
    */
   p = mkLoadImm(p, hregT0(), (ULong)dstGA);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /* --- FIRST PATCHABLE BYTE follows --- */
   /* VG_(disp_cp_chain_me_to_{slowEP,fastEP}) (where we're
      calling to) backs up the return address, so as to find the
      address of the first patchable byte.  So: don't change the
      number of instructions (5) below. */
   /*
      la   $t0, VG_(disp_cp_chain_me_to_{slowEP,fastEP})
      jirl $ra, $t0, 0
    */
   const void* disp_cp_chain_me = toFastEP ? disp_cp_chain_me_to_fastEP
                                           : disp_cp_chain_me_to_slowEP;
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);
   /* --- END of PATCHABLE BYTES --- */

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 8 && offs <= 11);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXIndir ( UInt* p, HReg dstGA, LOONGARCH64AMode* amPC,
                               HReg cond, const void* disp_cp_xindir )
{
   /* We're generating transfers that could lead indirectly to a
      chain-me, so we need to be sure this is actually allowed --
      no-redir translations are not allowed to reach normal
      translations without going through the scheduler.  That means
      no XDirects or XIndirs out from no-redir translations.
      Hence: */
   vassert(disp_cp_xindir != NULL);

   /* Use ptmp for backpatching conditional jumps. */
   UInt* ptmp = NULL;

   /* First off, if this is conditional, create a conditional
      jump over the rest of it. */
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      or   $t0, dstGA, $zero
      st.d $t0, amPC
    */
   *p++ = emit_op_rk_rj_rd(LAbin_OR, 0, iregEnc(dstGA), 12);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /*
      la   $t0, VG_(disp_cp_xindir)
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), (ULong)(Addr)disp_cp_xindir);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 5 && offs <= 8);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkXAssisted ( UInt* p, HReg dstGA, LOONGARCH64AMode* amPC,
                                  HReg cond, IRJumpKind jk,
                                  const void* disp_cp_xassisted )
{
   /* First off, if this is conditional, create a conditional jump
      over the rest of it.  Or at least, leave a space for it that
      we will shortly fill in. */
   UInt* ptmp = NULL;
   if (!hregIsInvalid(cond)) {
      ptmp = p;
      p++;
   }

   /* Update the guest PC.
      or   $t0, dstGA, $zero
      st.d $t0, amPC
    */
   *p++ = emit_op_rk_rj_rd(LAbin_OR, 0, iregEnc(dstGA), 12);
   p = mkStore(p, LAstore_ST_D, amPC, hregT0());

   /* li.w $s8, magic_number */
   UInt trcval = 0;
   switch (jk) {
      case Ijk_Boring:
         trcval = VEX_TRC_JMP_BORING;
         break;
      case Ijk_ClientReq:
         trcval = VEX_TRC_JMP_CLIENTREQ;
         break;
      case Ijk_NoDecode:
         trcval = VEX_TRC_JMP_NODECODE;
         break;
      case Ijk_InvalICache:
         trcval = VEX_TRC_JMP_INVALICACHE;
         break;
      case Ijk_NoRedir:
         trcval = VEX_TRC_JMP_NOREDIR;
         break;
      case Ijk_SigTRAP:
         trcval = VEX_TRC_JMP_SIGTRAP;
         break;
      case Ijk_SigSEGV:
         trcval = VEX_TRC_JMP_SIGSEGV;
         break;
      case Ijk_SigBUS:
         trcval = VEX_TRC_JMP_SIGBUS;
         break;
      case Ijk_SigFPE_IntDiv:
         trcval = VEX_TRC_JMP_SIGFPE_INTDIV;
         break;
      case Ijk_SigFPE_IntOvf:
         trcval = VEX_TRC_JMP_SIGFPE_INTOVF;
         break;
      case Ijk_SigSYS:
         trcval = VEX_TRC_JMP_SIGSYS;
         break;
      case Ijk_Sys_syscall:
         trcval = VEX_TRC_JMP_SYS_SYSCALL;
         break;
      /* We don't expect to see the following being assisted.
         case Ijk_Call:
         case Ijk_Ret:
         case Ijk_Yield:
         case Ijk_EmWarn:
         case Ijk_EmFail:
         case Ijk_MapFail:
         case Ijk_FlushDCache:
         case Ijk_SigILL:
         case Ijk_SigFPE:
         case Ijk_Sys_int32:
         case Ijk_Sys_int128:
         case Ijk_Sys_int129:
         case Ijk_Sys_int130:
         case Ijk_Sys_int145:
         case Ijk_Sys_int210:
         case Ijk_Sys_sysenter:
       */
      default:
         ppIRJumpKind(jk);
         vpanic("emit_LOONGARCH64Instr.LAin_XAssisted: unexpected jump kind");
   }
   vassert(trcval != 0);
   p = mkLoadImm(p, hregGSP(), trcval);

   /*
      la   $t0, VG_(disp_cp_xassisted)
      jirl $ra, $t0, 0
    */
   p = mkLoadImm(p, hregT0(), (ULong)(Addr)disp_cp_xassisted);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Fix up the conditional jump, if there was one. */
   if (!hregIsInvalid(cond)) {
      vassert(ptmp != NULL);
      UInt offs = (UInt)(p - ptmp);
      vassert(offs >= 6 && offs <= 12);
      /* beq cond, $zero, offs */
      *ptmp++ = emit_op_offs16_rj_rd(LAextra_BEQ, offs, iregEnc(cond), 0);
   }

   return p;
}

static inline UInt* mkEvCheck ( UInt* p, LOONGARCH64AMode* amCounter,
                                LOONGARCH64AMode* amFailAddr )
{
   UInt* p0 = p;

   /*
         ld.w   $t0, amCounter
         addi.d $t0, $t0, -1
         st.w   $t0, amCounter
         bge    $t0, $zero, nofail
         ld.d   $t0, amFailAddr
         jirl   $ra, $t0, 0
      nofail:
   */
   p = mkLoad(p, LAload_LD_W, amCounter, hregT0());
   *p++ = emit_op_si12_rj_rd(LAbin_ADDI_D, -1 & 0xfff, 12, 12);
   p = mkStore(p, LAstore_ST_W, amCounter, hregT0());
   *p++ = emit_op_offs16_rj_rd(LAextra_BGE, 3, 12, 0);
   p = mkLoad(p, LAload_LD_W, amFailAddr, hregT0());
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   /* Crosscheck */
   vassert(evCheckSzB_LOONGARCH64() == (UChar*)p - (UChar*)p0);
   return p;
}

static inline UInt* mkProfInc ( UInt* p )
{
   /*
      li     $t0, 0x6555755585559555UL
      ld.d   $t1, $t0, 0
      addi.d $t1, $t1, 1
      st.d   $t1, $t0, 0
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), 0x6555755585559555UL);
   *p++ = emit_op_si12_rj_rd(LAload_LD_D, 0, 12, 13);
   *p++ = emit_op_si12_rj_rd(LAbin_ADDI_D, 1, 13, 13);
   *p++ = emit_op_si12_rj_rd(LAstore_ST_D, 0, 12, 13);
   return p;
}

/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code.  If the emitted
   instruction was a profiler inc, set *is_profInc to True, else
   leave it unchanged. */
Int emit_LOONGARCH64Instr ( /*MB_MOD*/Bool* is_profInc,
                            UChar* buf,
                            Int nbuf,
                            const LOONGARCH64Instr* i,
                            Bool mode64,
                            VexEndness endness_host,
                            const void* disp_cp_chain_me_to_slowEP,
                            const void* disp_cp_chain_me_to_fastEP,
                            const void* disp_cp_xindir,
                            const void* disp_cp_xassisted )
{
   vassert(mode64 == True);

   UInt* p = (UInt*)buf;
   vassert(nbuf >= 32);
   vassert((((HWord)buf) & 3) == 0);

   switch (i->tag) {
      case LAin_LI:
         p = mkLoadImm(p, i->LAin.LI.dst, i->LAin.LI.imm);
         break;
      case LAin_Un:
         p = mkUnary(p, i->LAin.Unary.op, i->LAin.Unary.src,
                     i->LAin.Unary.dst);
         break;
      case LAin_Bin:
         p = mkBinary(p, i->LAin.Binary.op, i->LAin.Binary.src2,
                      i->LAin.Binary.src1, i->LAin.Binary.dst);
         break;
      case LAin_Load:
         p = mkLoad(p, i->LAin.Load.op, i->LAin.Load.src,
                    i->LAin.Load.dst);
         break;
      case LAin_Store:
         p = mkStore(p, i->LAin.Store.op, i->LAin.Store.dst,
                     i->LAin.Store.src);
         break;
      case LAin_LLSC:
         p = mkLLSC(p, i->LAin.LLSC.op, i->LAin.LLSC.addr, i->LAin.LLSC.val);
         break;
      case LAin_Bar:
         p = mkBar(p, i->LAin.Bar.op, i->LAin.Bar.hint);
         break;
      case LAin_FpUn:
         p = mkFpUnary(p, i->LAin.FpUnary.op, i->LAin.FpUnary.src,
                       i->LAin.FpUnary.dst);
         break;
      case LAin_FpBin:
         p = mkFpBinary(p, i->LAin.FpBinary.op, i->LAin.FpBinary.src2,
                        i->LAin.FpBinary.src1, i->LAin.FpBinary.dst);
         break;
      case LAin_FpTri:
         p = mkFpTrinary(p, i->LAin.FpTrinary.op, i->LAin.FpTrinary.src3,
                         i->LAin.FpTrinary.src2, i->LAin.FpTrinary.src1,
                         i->LAin.FpTrinary.dst);
         break;
      case LAin_FpLoad:
         p = mkFpLoad(p, i->LAin.FpLoad.op, i->LAin.FpLoad.src,
                      i->LAin.FpLoad.dst);
         break;
      case LAin_FpStore:
         p = mkFpStore(p, i->LAin.FpStore.op, i->LAin.FpStore.dst,
                       i->LAin.FpStore.src);
         break;
      case LAin_FpMove:
         p = mkFpMove(p, i->LAin.FpMove.op, i->LAin.FpMove.src,
                      i->LAin.FpMove.dst);
         break;
      case LAin_FpCmp:
         p = mkFpCmp(p, i->LAin.FpCmp.op, i->LAin.FpCmp.src2,
                     i->LAin.FpCmp.src1, i->LAin.FpCmp.dst);
         break;
      case LAin_Cas:
         p = mkCas(p, i->LAin.Cas.old, i->LAin.Cas.addr, i->LAin.Cas.expd,
                   i->LAin.Cas.data, i->LAin.Cas.size64);
         break;
      case LAin_Cmp:
         p = mkCmp(p, i->LAin.Cmp.cond, i->LAin.Cmp.src2,
                   i->LAin.Cmp.src1, i->LAin.Cmp.dst);
         break;
      case LAin_CMove:
         p = mkCMove(p, i->LAin.CMove.cond, i->LAin.CMove.r0,
                     i->LAin.CMove.r1, i->LAin.CMove.dst,
                     i->LAin.CMove.isInt);
         break;
      case LAin_Call:
         p = mkCall(p, i->LAin.Call.cond, i->LAin.Call.target,
                    i->LAin.Call.rloc);
         break;
      case LAin_XDirect:
         p = mkXDirect(p, i->LAin.XDirect.dstGA, i->LAin.XDirect.amPC,
                       i->LAin.XDirect.cond, i->LAin.XDirect.toFastEP,
                       disp_cp_chain_me_to_slowEP,
                       disp_cp_chain_me_to_fastEP);
         break;
      case LAin_XIndir:
         p = mkXIndir(p, i->LAin.XIndir.dstGA, i->LAin.XIndir.amPC,
                      i->LAin.XIndir.cond, disp_cp_xindir);
         break;
      case LAin_XAssisted:
         p = mkXAssisted(p, i->LAin.XAssisted.dstGA, i->LAin.XAssisted.amPC,
                         i->LAin.XAssisted.cond, i->LAin.XAssisted.jk,
                         disp_cp_xassisted);
         break;
      case LAin_EvCheck:
         p = mkEvCheck(p, i->LAin.EvCheck.amCounter,
                       i->LAin.EvCheck.amFailAddr);
         break;
      case LAin_ProfInc:
         p = mkProfInc(p);
         break;
      default:
         p = NULL;
         break;
   }

   if (p == NULL) {
      ppLOONGARCH64Instr(i, True);
      vpanic("emit_LOONGARCH64Instr");
      /*NOTREACHED*/
   }

   vassert(((UChar*)p) - &buf[0] <= 48);
   return ((UChar*)p) - &buf[0];
}

/* How big is an event check?  See case for mkEvCheck just above.  That
   crosschecks what this returns, so we can tell if we're inconsistent. */
Int evCheckSzB_LOONGARCH64 ( void )
{
   return 6 * 4; // 6 insns
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                         void* place_to_chain,
                                         const void* disp_cp_chain_me_EXPECTED,
                                         const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
    *  la $t0, disp_cp_chain_me_to_EXPECTED
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    */
   UInt* p = (UInt*)place_to_chain;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me_EXPECTED));
   vassert(p[4] == emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1));

   /* And what we want to change it to is:
    *  la $t0, place_to_jump_to
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    *
    * The replacement has the same length as the original.
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)place_to_jump_to);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   VexInvalRange vir = { (HWord)place_to_chain, 4 * 4 + 4 };
   return vir;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange unchainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                           void* place_to_unchain,
                                           const void* place_to_jump_to_EXPECTED,
                                           const void* disp_cp_chain_me )
{
   vassert(endness_host == VexEndnessLE);

   /* What we're expecting to see is:
    *  la $t0, place_to_jump_to_EXPECTED
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    */
   UInt* p = (UInt*)place_to_unchain;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)place_to_jump_to_EXPECTED));
   vassert(p[4] == emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1));

   /* And what we want to change it to is:
    *  la $t0, disp_cp_chain_me
    *  jirl $ra, $t0, 0
    * viz
    *  <16 bytes generated by mkLoadImm_EXACTLY4>
    *  jirl $ra, $t0, 0
    *
    * The replacement has the same length as the original.
    */
   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)disp_cp_chain_me);
   *p++ = emit_op_offs16_rj_rd(LAextra_JIRL, 0, 12, 1);

   VexInvalRange vir = { (HWord)place_to_unchain, 4 * 4 + 4 };
   return vir;
}

/* Patch the counter address into a profile inc point, as previously
   created by the mkProfInc. */
VexInvalRange patchProfInc_LOONGARCH64 ( VexEndness endness_host,
                                         void*  place_to_patch,
                                         const ULong* location_of_counter )
{
   vassert(endness_host == VexEndnessLE);
   vassert(sizeof(ULong*) == 8);

   /*
      $t0 = NotKnownYet
      ld.d   $t1, $t0, 0
      addi.d $t1, $t1, 1
      st.d   $t1, $t0, 0
    */
   UInt* p = (UInt*)place_to_patch;
   vassert(((HWord)p & 3) == 0);
   vassert(is_LoadImm_EXACTLY4(p, hregT0(), 0x6555755585559555UL));
   vassert(p[4] == emit_op_si12_rj_rd(LAload_LD_D, 0, 12, 13));
   vassert(p[5] == emit_op_si12_rj_rd(LAbin_ADDI_D, 1, 13, 13));
   vassert(p[6] == emit_op_si12_rj_rd(LAstore_ST_D, 0, 12, 13));

   p = mkLoadImm_EXACTLY4(p, hregT0(), (ULong)(Addr)location_of_counter);

   VexInvalRange vir = { (HWord)place_to_patch, 4 * 4 };
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                             host_loongarch64_defs.c ---*/
/*---------------------------------------------------------------*/
