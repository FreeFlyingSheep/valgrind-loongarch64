
/*---------------------------------------------------------------*/
/*--- begin                           host_loongarch64_defs.h ---*/
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

#ifndef __VEX_HOST_LOONGARCH64_DEFS_H
#define __VEX_HOST_LOONGARCH64_DEFS_H

#include "libvex_basictypes.h"
#include "libvex.h"             /* VexArch */
#include "host_generic_regs.h"  /* HReg */


/* --------- Registers. --------- */

#define ST_IN static inline

/* Integer static registers */
ST_IN HReg hregLOONGARCH64_R23 ( void ) { return mkHReg(False, HRcInt64, 23,  0); }
ST_IN HReg hregLOONGARCH64_R24 ( void ) { return mkHReg(False, HRcInt64, 24,  1); }
ST_IN HReg hregLOONGARCH64_R25 ( void ) { return mkHReg(False, HRcInt64, 25,  2); }
ST_IN HReg hregLOONGARCH64_R26 ( void ) { return mkHReg(False, HRcInt64, 26,  3); }
ST_IN HReg hregLOONGARCH64_R27 ( void ) { return mkHReg(False, HRcInt64, 27,  4); }
ST_IN HReg hregLOONGARCH64_R28 ( void ) { return mkHReg(False, HRcInt64, 28,  5); }
ST_IN HReg hregLOONGARCH64_R29 ( void ) { return mkHReg(False, HRcInt64, 29,  6); }
ST_IN HReg hregLOONGARCH64_R30 ( void ) { return mkHReg(False, HRcInt64, 30,  7); }
/* $r31 is used as guest stack pointer */

/* Integer temporary registers */
/* $r12 is used as a chaining/ProfInc/Cmove temporary */
/* $r13 is used as a ProfInc temporary */
ST_IN HReg hregLOONGARCH64_R14 ( void ) { return mkHReg(False, HRcInt64, 14,  8); }
ST_IN HReg hregLOONGARCH64_R15 ( void ) { return mkHReg(False, HRcInt64, 15,  9); }
ST_IN HReg hregLOONGARCH64_R16 ( void ) { return mkHReg(False, HRcInt64, 16, 10); }
ST_IN HReg hregLOONGARCH64_R17 ( void ) { return mkHReg(False, HRcInt64, 17, 11); }
ST_IN HReg hregLOONGARCH64_R18 ( void ) { return mkHReg(False, HRcInt64, 18, 12); }
ST_IN HReg hregLOONGARCH64_R19 ( void ) { return mkHReg(False, HRcInt64, 19, 13); }
ST_IN HReg hregLOONGARCH64_R20 ( void ) { return mkHReg(False, HRcInt64, 20, 14); }

/* Floating point static registers */
ST_IN HReg hregLOONGARCH64_F24 ( void ) { return mkHReg(False, HRcFlt64, 24, 15); }
ST_IN HReg hregLOONGARCH64_F25 ( void ) { return mkHReg(False, HRcFlt64, 25, 16); }
ST_IN HReg hregLOONGARCH64_F26 ( void ) { return mkHReg(False, HRcFlt64, 26, 17); }
ST_IN HReg hregLOONGARCH64_F27 ( void ) { return mkHReg(False, HRcFlt64, 27, 18); }
ST_IN HReg hregLOONGARCH64_F28 ( void ) { return mkHReg(False, HRcFlt64, 28, 19); }
ST_IN HReg hregLOONGARCH64_F29 ( void ) { return mkHReg(False, HRcFlt64, 29, 20); }
ST_IN HReg hregLOONGARCH64_F30 ( void ) { return mkHReg(False, HRcFlt64, 30, 21); }
ST_IN HReg hregLOONGARCH64_F31 ( void ) { return mkHReg(False, HRcFlt64, 31, 22); }

/* Other Integer registers */
ST_IN HReg hregLOONGARCH64_R0  ( void ) { return mkHReg(False, HRcInt64,  0, 23); }
ST_IN HReg hregLOONGARCH64_R1  ( void ) { return mkHReg(False, HRcInt64,  1, 24); }
ST_IN HReg hregLOONGARCH64_R2  ( void ) { return mkHReg(False, HRcInt64,  2, 25); }
ST_IN HReg hregLOONGARCH64_R3  ( void ) { return mkHReg(False, HRcInt64,  3, 26); }
ST_IN HReg hregLOONGARCH64_R4  ( void ) { return mkHReg(False, HRcInt64,  4, 27); }
ST_IN HReg hregLOONGARCH64_R5  ( void ) { return mkHReg(False, HRcInt64,  5, 28); }
ST_IN HReg hregLOONGARCH64_R6  ( void ) { return mkHReg(False, HRcInt64,  6, 29); }
ST_IN HReg hregLOONGARCH64_R7  ( void ) { return mkHReg(False, HRcInt64,  7, 30); }
ST_IN HReg hregLOONGARCH64_R8  ( void ) { return mkHReg(False, HRcInt64,  8, 31); }
ST_IN HReg hregLOONGARCH64_R9  ( void ) { return mkHReg(False, HRcInt64,  9, 32); }
ST_IN HReg hregLOONGARCH64_R10 ( void ) { return mkHReg(False, HRcInt64, 10, 33); }
ST_IN HReg hregLOONGARCH64_R11 ( void ) { return mkHReg(False, HRcInt64, 11, 34); }
ST_IN HReg hregLOONGARCH64_R12 ( void ) { return mkHReg(False, HRcInt64, 12, 35); }
ST_IN HReg hregLOONGARCH64_R13 ( void ) { return mkHReg(False, HRcInt64, 13, 36); }
ST_IN HReg hregLOONGARCH64_R21 ( void ) { return mkHReg(False, HRcInt64, 21, 37); }
ST_IN HReg hregLOONGARCH64_R22 ( void ) { return mkHReg(False, HRcInt64, 22, 38); }
ST_IN HReg hregLOONGARCH64_R31 ( void ) { return mkHReg(False, HRcInt64, 31, 39); }

/* Special registers */
ST_IN HReg hregLOONGARCH64_FCSR3 ( void ) { return mkHReg(False, HRcInt32, 3, 40); }

#undef ST_IN

#define hregZERO() hregLOONGARCH64_R0()
#define hregSP()   hregLOONGARCH64_R3()
#define hregT0()   hregLOONGARCH64_R12()
#define hregT1()   hregLOONGARCH64_R13()
#define hregGSP()  hregLOONGARCH64_R31()

extern UInt ppHRegLOONGARCH64 ( HReg reg );

/* Number of registers used arg passing in function calls */
#define LOONGARCH64_N_ARGREGS 8 /* a0 ... a7 */


/* --------- Condition codes, LOONGARCH64 encoding. --------- */
typedef enum {
   LAcc_EQ  = 0, /* equal */
   LAcc_NE  = 1, /* not equal */

   LAcc_LT  = 2, /* less than (signed) */
   LAcc_GE  = 3, /* great equal (signed) */

   LAcc_LTU = 4, /* less than (unsigned) */
   LAcc_GEU = 5, /* great equal (unsigned) */

   LAcc_AL  = 6  /* always (unconditional) */
} LOONGARCH64CondCode;


/* --------- Memory address expressions (amodes). --------- */

typedef enum {
   LAam_RI, /* Reg + Imm (signed 12-bit or signed 14-bit) */
   LAam_RR  /* Reg1 + Reg2 */
} LOONGARCH64AModeTag;

typedef struct {
   LOONGARCH64AModeTag tag;
   union {
      struct {
         HReg   base;
         UShort index;
      } RI;
      struct {
         HReg base;
         HReg index;
      } RR;
   } LAam;
} LOONGARCH64AMode;

extern LOONGARCH64AMode* LOONGARCH64AMode_RI ( HReg reg, UShort imm );
extern LOONGARCH64AMode* LOONGARCH64AMode_RR ( HReg base, HReg index );


/* --------- Operand, which can be reg or imm. --------- */

typedef enum {
   LAri_Reg,
   LAri_Imm
} LOONGARCH64RITag;

typedef struct {
   LOONGARCH64RITag tag;
   union {
      struct {
         HReg reg;
      } R;
      struct {
         UShort imm;
         UChar  size; // size == 5 || size == 6 || size == 12
         Bool   isSigned;
      } I;
   } LAri;
} LOONGARCH64RI;

extern LOONGARCH64RI* LOONGARCH64RI_R ( HReg reg );
extern LOONGARCH64RI* LOONGARCH64RI_I ( UShort imm, UChar size, Bool isSigned );


/* --------- Instructions. --------- */

/* Tags for unary operations */
typedef enum {
   LAun_CLZ_W     = 0x00001400,
   LAun_CTZ_W     = 0x00001c00,
   LAun_CLZ_D     = 0x00002400,
   LAun_CTZ_D     = 0x00002c00,
   LAun_EXT_W_H   = 0x00005800,
   LAun_EXT_W_B   = 0x00005c00
} LOONGARCH64UnOp;

/* Tags for binary operations */
typedef enum {
   LAbin_ADD_W     = 0x00100000,
   LAbin_ADD_D     = 0x00108000,
   LAbin_SUB_W     = 0x00110000,
   LAbin_SUB_D     = 0x00118000,
   LAbin_NOR       = 0x00140000,
   LAbin_AND       = 0x00148000,
   LAbin_OR        = 0x00150000,
   LAbin_XOR       = 0x00158000,
   LAbin_SLL_W     = 0x00170000,
   LAbin_SRL_W     = 0x00178000,
   LAbin_SRA_W     = 0x00180000,
   LAbin_SLL_D     = 0x00188000,
   LAbin_SRL_D     = 0x00190000,
   LAbin_SRA_D     = 0x00198000,
   LAbin_MUL_W     = 0x001c0000,
   LAbin_MUL_D     = 0x001d8000,
   LAbin_MULH_W    = 0x001c8000,
   LAbin_MULH_WU   = 0x001d0000,
   LAbin_MULH_D    = 0x001e0000,
   LAbin_MULH_DU   = 0x001e8000,
   LAbin_MULW_D_W  = 0x001f0000,
   LAbin_MULW_D_WU = 0x001f8000,
   LAbin_DIV_W     = 0x00200000,
   LAbin_MOD_W     = 0x00208000,
   LAbin_DIV_WU    = 0x00210000,
   LAbin_MOD_WU    = 0x00218000,
   LAbin_DIV_D     = 0x00220000,
   LAbin_MOD_D     = 0x00228000,
   LAbin_DIV_DU    = 0x00230000,
   LAbin_MOD_DU    = 0x00238000,
   LAbin_SLLI_W    = 0x00408000,
   LAbin_SLLI_D    = 0x00410000,
   LAbin_SRLI_W    = 0x00448000,
   LAbin_SRLI_D    = 0x00450000,
   LAbin_SRAI_W    = 0x00488000,
   LAbin_SRAI_D    = 0x00490000,
   LAbin_ADDI_W    = 0x02800000,
   LAbin_ADDI_D    = 0x02c00000,
   LAbin_ANDI      = 0x03400000,
   LAbin_ORI       = 0x03800000,
   LAbin_XORI      = 0x03c00000
} LOONGARCH64BinOp;

/* Tags for load operations */
typedef enum {
   LAload_LD_W   = 0x28800000,
   LAload_LD_D   = 0x28c00000,
   LAload_LD_BU  = 0x2a000000,
   LAload_LD_HU  = 0x2a400000,
   LAload_LD_WU  = 0x2a800000,
   LAload_LDX_D  = 0x380c0000,
   LAload_LDX_BU = 0x38200000,
   LAload_LDX_HU = 0x38240000,
   LAload_LDX_WU = 0x38280000
} LOONGARCH64LoadOp;

/* Tags for store operations */
typedef enum {
   LAstore_ST_B  = 0x29000000,
   LAstore_ST_H  = 0x29400000,
   LAstore_ST_W  = 0x29800000,
   LAstore_ST_D  = 0x29c00000,
   LAstore_STX_B = 0x38100000,
   LAstore_STX_H = 0x38140000,
   LAstore_STX_W = 0x38180000,
   LAstore_STX_D = 0x381c0000
} LOONGARCH64StoreOp;

/* Tags for ll/sc operations */
typedef enum {
   LAllsc_LL_W = 0x20000000,
   LAllsc_SC_W = 0x21000000,
   LAllsc_LL_D = 0x22000000,
   LAllsc_SC_D = 0x23000000
} LOONGARCH64LLSCOp;

/* Tags for barrier operations */
typedef enum {
   LAbar_DBAR = 0x38720000,
   LAbar_IBAR = 0x38728000
} LOONGARCH64BarOp;

/* Tags for floating point unary operations */
typedef enum {
   LAfpun_FABS_S    = 0x01140400,
   LAfpun_FABS_D    = 0x01140800,
   LAfpun_FNEG_S    = 0x01141400,
   LAfpun_FNEG_D    = 0x01141800,
   LAfpun_FLOGB_S   = 0x01142400,
   LAfpun_FLOGB_D   = 0x01142800,
   LAfpun_FSQRT_S   = 0x01144400,
   LAfpun_FSQRT_D   = 0x01144800,
   LAfpun_FRSQRT_S  = 0x01146400,
   LAfpun_FRSQRT_D  = 0x01146800,
   LAfpun_FCVT_S_D  = 0x01191800,
   LAfpun_FCVT_D_S  = 0x01192400,
   LAfpun_FTINT_W_S = 0x011b0400,
   LAfpun_FTINT_W_D = 0x011b0800,
   LAfpun_FTINT_L_S = 0x011b2400,
   LAfpun_FTINT_L_D = 0x011b2800,
   LAfpun_FFINT_S_W = 0x011d1000,
   LAfpun_FFINT_S_L = 0x011d1800,
   LAfpun_FFINT_D_W = 0x011d2000,
   LAfpun_FFINT_D_L = 0x011d2800,
   LAfpun_FRINT_S   = 0x011e4400,
   LAfpun_FRINT_D   = 0x011e4800
} LOONGARCH64FpUnOp;

/* Tags for floating point binary operations */
typedef enum {
   LAfpbin_FADD_S    = 0x01008000,
   LAfpbin_FADD_D    = 0x01010000,
   LAfpbin_FSUB_S    = 0x01028000,
   LAfpbin_FSUB_D    = 0x01030000,
   LAfpbin_FMUL_S    = 0x01048000,
   LAfpbin_FMUL_D    = 0x01050000,
   LAfpbin_FDIV_S    = 0x01068000,
   LAfpbin_FDIV_D    = 0x01070000,
   LAfpbin_FMAX_S    = 0x01088000,
   LAfpbin_FMAX_D    = 0x01090000,
   LAfpbin_FMIN_S    = 0x010a8000,
   LAfpbin_FMIN_D    = 0x010b0000,
   LAfpbin_FMAXA_S   = 0x010c8000,
   LAfpbin_FMAXA_D   = 0x010d0000,
   LAfpbin_FMINA_S   = 0x010e8000,
   LAfpbin_FMINA_D   = 0x010f0000,
   LAfpbin_FSCALEB_S = 0x01108000,
   LAfpbin_FSCALEB_D = 0x01110000
} LOONGARCH64FpBinOp;

/* Tags for floating point trinary operations */
typedef enum {
   LAfpbin_FMADD_S = 0x08100000,
   LAfpbin_FMADD_D = 0x08200000,
   LAfpbin_FMSUB_S = 0x08500000,
   LAfpbin_FMSUB_D = 0x08600000
} LOONGARCH64FpTriOp;

/* Tags for floating point load operations */
typedef enum {
   LAfpload_FLD_S  = 0x2b000000,
   LAfpload_FLD_D  = 0x2b800000,
   LAfpload_FLDX_S = 0x38300000,
   LAfpload_FLDX_D = 0x38340000
} LOONGARCH64FpLoadOp;

/* Tags for floating point store operations */
typedef enum {
   LAfpstore_FST_S  = 0x2b400000,
   LAfpstore_FST_D  = 0x2bc00000,
   LAfpstore_FSTX_S = 0x38380000,
   LAfpstore_FSTX_D = 0x383c0000
} LOONGARCH64FpStoreOp;

/* Tags for floating point move operations */
typedef enum {
   LAfpmove_FMOV_S     = 0x01149400,
   LAfpmove_FMOV_D     = 0x01149800,
   LAfpmove_MOVGR2FR_W = 0x0114a400,
   LAfpmove_MOVGR2FR_D = 0x0114a800,
   LAfpmove_MOVFR2GR_S = 0x0114b400,
   LAfpmove_MOVFR2GR_D = 0x0114b800,
   LAfpmove_MOVGR2FCSR = 0x0114c000,
   LAfpmove_MOVFCSR2GR = 0x0114c800
} LOONGARCH64FpMoveOp;

/* Tags for floating point compare operations */
typedef enum {
   LAfpcmp_FCMP_CLT_S = 0x0c110000,
   LAfpcmp_FCMP_CLT_D = 0x0c210000,
   LAfpcmp_FCMP_CEQ_S = 0x0c120000,
   LAfpcmp_FCMP_CEQ_D = 0x0c220000,
   LAfpcmp_FCMP_CUN_S = 0x0c140000,
   LAfpcmp_FCMP_CUN_D = 0x0c240000
} LOONGARCH64FpCmpOp;

/* Tags for extra operations, we only use them when emiting code directly */
typedef enum {
   LAextra_MOVGR2CF = 0x0114d800,
   LAextra_MOVCF2GR = 0x0114dc00,
   LAextra_SLT      = 0x00120000,
   LAextra_SLTU     = 0x00128000,
   LAextra_MASKEQZ  = 0x00130000,
   LAextra_MASKNEZ  = 0x00138000,
   LAextra_SLTI     = 0x02000000,
   LAextra_SLTUI    = 0x02400000,
   LAextra_LU52I_D  = 0x03000000,
   LAextra_FSEL     = 0x0d000000,
   LAextra_LU12I_W  = 0x14000000,
   LAextra_LU32I_D  = 0x16000000,
   LAextra_JIRL     = 0x4c000000,
   LAextra_B        = 0x50000000,
   LAextra_BEQ      = 0x58000000,
   LAextra_BNE      = 0x5c000000,
   LAextra_BGE      = 0x64000000
} LOONGARCH64ExtraOp;

/* Tags for instructions */
typedef enum {
   /* Pseudo-insn, used for generating a 64-bit
      literal to register */
   LAin_LI,         /* load imm */

   /* Integer insns */
   LAin_Un,         /* unary */
   LAin_Bin,        /* binary */
   LAin_Load,       /* load */
   LAin_Store,      /* store */
   LAin_LLSC,       /* ll/sc */
   LAin_Bar,        /* barrier */

   /* Floating point insns */
   LAin_FpUn,       /* floating point unary */
   LAin_FpBin,      /* floating point binary */
   LAin_FpTri,      /* floating point trinary */
   LAin_FpLoad,     /* floating point load */
   LAin_FpStore,    /* floating point store */
   LAin_FpMove,     /* floating point move */
   LAin_FpCmp,      /* floating point compare */

   /* Pseudo-insn */
   LAin_Cas,        /* compare and swap */
   LAin_Cmp,        /* word compare */
   LAin_CMove,      /* condition move */

   /* Call target (an absolute address), on given
      condition (which could be LAcc_AL). */
   LAin_Call,       /* call */

   /* The following 5 insns are mandated by translation chaining */
   LAin_XDirect,    /* direct transfer to GA */
   LAin_XIndir,     /* indirect transfer to GA */
   LAin_XAssisted,  /* assisted transfer to GA */
   LAin_EvCheck,    /* Event check */
   LAin_ProfInc     /* 64-bit profile counter increment */
} LOONGARCH64InstrTag;

typedef struct {
   LOONGARCH64InstrTag tag;
   union {
      struct {
         ULong                imm;
         HReg                 dst;
      } LI;
      struct {
         LOONGARCH64UnOp      op;
         HReg                 src;
         HReg                 dst;
      } Unary;
      struct {
         LOONGARCH64BinOp     op;
         LOONGARCH64RI*       src2;
         HReg                 src1;
         HReg                 dst;
      } Binary;
      struct {
         LOONGARCH64LoadOp    op;
         LOONGARCH64AMode*    src;
         HReg                 dst;
      } Load;
      struct {
         LOONGARCH64StoreOp   op;
         LOONGARCH64AMode*    dst;
         HReg                 src;
      } Store;
      struct {
         LOONGARCH64LLSCOp    op;
         Bool                 isLoad;
         LOONGARCH64AMode*    addr;
         HReg                 val;
      } LLSC;
      struct {
         LOONGARCH64BarOp     op;
         UShort               hint;
      } Bar;
      struct {
         LOONGARCH64FpUnOp    op;
         HReg                 src;
         HReg                 dst;
      } FpUnary;
      struct {
         LOONGARCH64FpBinOp   op;
         HReg                 src2;
         HReg                 src1;
         HReg                 dst;
      } FpBinary;
      struct {
         LOONGARCH64FpTriOp   op;
         HReg                 src3;
         HReg                 src2;
         HReg                 src1;
         HReg                 dst;
      } FpTrinary;
      struct {
         LOONGARCH64FpLoadOp  op;
         LOONGARCH64AMode*    src;
         HReg                 dst;
      } FpLoad;
      struct {
         LOONGARCH64FpStoreOp op;
         LOONGARCH64AMode*    dst;
         HReg                 src;
      } FpStore;
      struct {
         LOONGARCH64FpMoveOp  op;
         HReg                 src;
         HReg                 dst;
      } FpMove;
      struct {
         LOONGARCH64FpCmpOp   op;
         HReg                 src2;
         HReg                 src1;
         HReg                 dst;
      } FpCmp;
      struct {
         HReg                 old;
         HReg                 addr;
         HReg                 expd;
         HReg                 data;
         Bool                 size64;
      } Cas;
      struct {
         LOONGARCH64CondCode  cond;
         HReg                 dst;
         HReg                 src1;
         HReg                 src2;
      } Cmp;
      struct {
         HReg                 cond;
         HReg                 r0;
         HReg                 r1;
         HReg                 dst;
         Bool                 isInt;
      } CMove;
      struct {
         HReg                 cond;
         Addr64               target;
         UInt                 nArgRegs;
         RetLoc               rloc;
      } Call;
      struct {
         Addr64               dstGA;
         LOONGARCH64AMode*    amPC;
         HReg                 cond;
         Bool                 toFastEP;
      } XDirect;
      struct {
         HReg                 dstGA;
         LOONGARCH64AMode*    amPC;
         HReg                 cond;
      } XIndir;
      struct {
         HReg                 dstGA;
         LOONGARCH64AMode*    amPC;
         HReg                 cond;
         IRJumpKind           jk;
      } XAssisted;
      struct {
         LOONGARCH64AMode*    amCounter;
         LOONGARCH64AMode*    amFailAddr;
      } EvCheck;
      struct {
         /* No fields.  The address of the counter to inc is
            installed later, post-translation, by patching it in,
            as it is not known at translation time. */
      } ProfInc;
   } LAin;
} LOONGARCH64Instr;

extern LOONGARCH64Instr* LOONGARCH64Instr_LI        ( ULong imm, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_Unary     ( LOONGARCH64UnOp op,
                                                      HReg src, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_Binary    ( LOONGARCH64BinOp op,
                                                      LOONGARCH64RI* src2,
                                                      HReg src1, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_Load      ( LOONGARCH64LoadOp op,
                                                      LOONGARCH64AMode* src,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_Store     ( LOONGARCH64StoreOp op,
                                                      LOONGARCH64AMode* dst,
                                                      HReg src );
extern LOONGARCH64Instr* LOONGARCH64Instr_LLSC      ( LOONGARCH64LLSCOp op,
                                                      Bool isLoad,
                                                      LOONGARCH64AMode* addr,
                                                      HReg val );
extern LOONGARCH64Instr* LOONGARCH64Instr_Bar       ( LOONGARCH64BarOp op,
                                                      UShort hint );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpUnary   ( LOONGARCH64FpUnOp op,
                                                      HReg src, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpBinary  ( LOONGARCH64FpBinOp op,
                                                      HReg src2, HReg src1,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpTrinary ( LOONGARCH64FpTriOp op,
                                                      HReg src3, HReg src2,
                                                      HReg src1, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpLoad    ( LOONGARCH64FpLoadOp op,
                                                      LOONGARCH64AMode* src,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpStore   ( LOONGARCH64FpStoreOp op,
                                                      LOONGARCH64AMode* dst,
                                                      HReg src );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpMove    ( LOONGARCH64FpMoveOp op,
                                                      HReg src, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_FpCmp     ( LOONGARCH64FpCmpOp op,
                                                      HReg src2, HReg src1,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_Cas       ( HReg old, HReg addr,
                                                      HReg expd, HReg data,
                                                      Bool size64 );
extern LOONGARCH64Instr* LOONGARCH64Instr_Cmp       ( LOONGARCH64CondCode cond,
                                                      HReg src2, HReg src1,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_CMove     ( HReg cond, HReg r0, HReg r1,
                                                      HReg dst, Bool isInt );
extern LOONGARCH64Instr* LOONGARCH64Instr_Call      ( HReg cond, Addr64 target,
                                                      UInt nArgRegs, RetLoc rloc );
extern LOONGARCH64Instr* LOONGARCH64Instr_XDirect   ( Addr64 dstGA,
                                                      LOONGARCH64AMode* amPC,
                                                      HReg cond, Bool toFastEP );
extern LOONGARCH64Instr* LOONGARCH64Instr_XIndir    ( HReg dstGA,
                                                      LOONGARCH64AMode* amPC,
                                                      HReg cond );
extern LOONGARCH64Instr* LOONGARCH64Instr_XAssisted ( HReg dstGA,
                                                      LOONGARCH64AMode* amPC,
                                                      HReg cond, IRJumpKind jk );
extern LOONGARCH64Instr* LOONGARCH64Instr_EvCheck   ( LOONGARCH64AMode* amCounter,
                                                      LOONGARCH64AMode* amFailAddr );
extern LOONGARCH64Instr* LOONGARCH64Instr_ProfInc   ( void );

extern void ppLOONGARCH64Instr ( const LOONGARCH64Instr* i, Bool mode64 );

/* Some functions that insulate the register allocator from details
   of the underlying instruction set. */
extern void getRegUsage_LOONGARCH64Instr ( HRegUsage* u,
                                           const LOONGARCH64Instr* i,
                                           Bool mode64 );
extern void mapRegs_LOONGARCH64Instr ( HRegRemap* m, LOONGARCH64Instr* i,
                                       Bool mode64 );
extern Int emit_LOONGARCH64Instr (/*MB_MOD*/Bool* is_profInc,
                                  UChar* buf,
                                  Int nbuf,
                                  const LOONGARCH64Instr* i,
                                  Bool mode64,
                                  VexEndness endness_host,
                                  const void* disp_cp_chain_me_to_slowEP,
                                  const void* disp_cp_chain_me_to_fastEP,
                                  const void* disp_cp_xindir,
                                  const void* disp_cp_xassisted );

extern void genSpill_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                                   HReg rreg, Int offsetB, Bool mode64);
extern void genReload_LOONGARCH64 ( /*OUT*/ HInstr** i1, /*OUT*/ HInstr** i2,
                                    HReg rreg, Int offsetB, Bool mode64);
extern LOONGARCH64Instr* genMove_LOONGARCH64 ( HReg from, HReg to,
                                               Bool mode64 );

extern const RRegUniverse* getRRegUniverse_LOONGARCH64 ( void );

extern HInstrArray* iselSB_LOONGARCH64 ( const IRSB*,
                                         VexArch,
                                         const VexArchInfo*,
                                         const VexAbiInfo*,
                                         Int offs_Host_EvC_Counter,
                                         Int offs_Host_EvC_FailAddr,
                                         Bool chainingAllowed,
                                         Bool addProfInc,
                                         Addr max_ga );

/* How big is an event check?  See case for Min_EvCheck in
   emit_LOONGARCH64Instr just above.  That crosschecks what this returns,
   so we can tell if we're inconsistent. */
extern Int evCheckSzB_LOONGARCH64 ( void );

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
extern VexInvalRange chainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                                void* place_to_chain,
                                                const void* disp_cp_chain_me_EXPECTED,
                                                const void* place_to_jump_to );

/* NB: what goes on here has to be very closely coordinated with the
   emitInstr case for XDirect, above. */
extern VexInvalRange unchainXDirect_LOONGARCH64 ( VexEndness endness_host,
                                                  void* place_to_unchain,
                                                  const void* place_to_jump_to_EXPECTED,
                                                  const void* disp_cp_chain_me );

/* Patch the counter address into a profile inc point, as previously
   created by the Min_ProfInc case for emit_LOONGARCH64Instr. */
extern VexInvalRange patchProfInc_LOONGARCH64 ( VexEndness endness_host,
                                                void*  place_to_patch,
                                                const ULong* location_of_counter );

#endif /* ndef __VEX_HOST_LOONGARCH64_DEFS_H */


/*---------------------------------------------------------------*/
/*--- end                             host-loongarch64_defs.h ---*/
/*---------------------------------------------------------------*/
