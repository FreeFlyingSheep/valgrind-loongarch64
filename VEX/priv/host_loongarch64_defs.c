
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

   switch (hregClass(rreg)) {
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

   switch (hregClass(rreg)) {
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
   return 0;
}

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
VexInvalRange chainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                         void* place_to_chain,
                                         const void* disp_cp_chain_me_EXPECTED,
                                         const void* place_to_jump_to )
{
   vassert(endness_host == VexEndnessLE);

   VexInvalRange vir = { (HWord)place_to_chain, 0 };
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

   VexInvalRange vir = { (HWord)place_to_unchain, 0 };
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

   VexInvalRange vir = { (HWord)place_to_patch, 0 };
   return vir;
}


/*---------------------------------------------------------------*/
/*--- end                             host_loongarch64_defs.c ---*/
/*---------------------------------------------------------------*/
