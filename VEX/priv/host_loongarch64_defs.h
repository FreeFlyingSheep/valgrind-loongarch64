
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
/* $r12 is used as a chaining/ProfInc/Cmove/genSpill/genReload temporary */
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

/* Vector static registers */
ST_IN HReg hregLOONGARCH64_V24 ( void ) { return mkHReg(False, HRcVec128, 24, 23); }
ST_IN HReg hregLOONGARCH64_V25 ( void ) { return mkHReg(False, HRcVec128, 25, 24); }
ST_IN HReg hregLOONGARCH64_V26 ( void ) { return mkHReg(False, HRcVec128, 26, 25); }
ST_IN HReg hregLOONGARCH64_V27 ( void ) { return mkHReg(False, HRcVec128, 27, 26); }
ST_IN HReg hregLOONGARCH64_V28 ( void ) { return mkHReg(False, HRcVec128, 28, 27); }
ST_IN HReg hregLOONGARCH64_V29 ( void ) { return mkHReg(False, HRcVec128, 29, 28); }
ST_IN HReg hregLOONGARCH64_V30 ( void ) { return mkHReg(False, HRcVec128, 30, 29); }
ST_IN HReg hregLOONGARCH64_V31 ( void ) { return mkHReg(False, HRcVec128, 31, 30); }

/* Other Integer registers */
ST_IN HReg hregLOONGARCH64_R0  ( void ) { return mkHReg(False, HRcInt64,  0, 31); }
ST_IN HReg hregLOONGARCH64_R1  ( void ) { return mkHReg(False, HRcInt64,  1, 32); }
ST_IN HReg hregLOONGARCH64_R2  ( void ) { return mkHReg(False, HRcInt64,  2, 33); }
ST_IN HReg hregLOONGARCH64_R3  ( void ) { return mkHReg(False, HRcInt64,  3, 34); }
ST_IN HReg hregLOONGARCH64_R4  ( void ) { return mkHReg(False, HRcInt64,  4, 35); }
ST_IN HReg hregLOONGARCH64_R5  ( void ) { return mkHReg(False, HRcInt64,  5, 36); }
ST_IN HReg hregLOONGARCH64_R6  ( void ) { return mkHReg(False, HRcInt64,  6, 37); }
ST_IN HReg hregLOONGARCH64_R7  ( void ) { return mkHReg(False, HRcInt64,  7, 38); }
ST_IN HReg hregLOONGARCH64_R8  ( void ) { return mkHReg(False, HRcInt64,  8, 39); }
ST_IN HReg hregLOONGARCH64_R9  ( void ) { return mkHReg(False, HRcInt64,  9, 40); }
ST_IN HReg hregLOONGARCH64_R10 ( void ) { return mkHReg(False, HRcInt64, 10, 41); }
ST_IN HReg hregLOONGARCH64_R11 ( void ) { return mkHReg(False, HRcInt64, 11, 42); }
ST_IN HReg hregLOONGARCH64_R12 ( void ) { return mkHReg(False, HRcInt64, 12, 43); }
ST_IN HReg hregLOONGARCH64_R13 ( void ) { return mkHReg(False, HRcInt64, 13, 44); }
ST_IN HReg hregLOONGARCH64_R21 ( void ) { return mkHReg(False, HRcInt64, 21, 45); }
ST_IN HReg hregLOONGARCH64_R22 ( void ) { return mkHReg(False, HRcInt64, 22, 46); }
ST_IN HReg hregLOONGARCH64_R31 ( void ) { return mkHReg(False, HRcInt64, 31, 47); }

/* Special registers */
ST_IN HReg hregLOONGARCH64_FCSR3 ( void ) { return mkHReg(False, HRcInt32, 3, 48); }

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
   LAam_RI, /* Reg + Imm (signed 12-bit) */
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

/* Tags for vector unary operations */
typedef enum {
   LAvecun_VCLO_B       = 0x729c0000,
   LAvecun_VCLO_H       = 0x729c0400,
   LAvecun_VCLO_W       = 0x729c0800,
   LAvecun_VCLZ_B       = 0x729c1000,
   LAvecun_VCLZ_H       = 0x729c1400,
   LAvecun_VCLZ_W       = 0x729c1800,
   LAvecun_VCLZ_D       = 0x729c1c00,
   LAvecun_VPCNT_B      = 0x729c2000,
   LAvecun_VEXTH_H_B    = 0x729ee000,
   LAvecun_VEXTH_W_H    = 0x729ee400,
   LAvecun_VEXTH_D_W    = 0x729ee800,
   LAvecun_VEXTH_Q_D    = 0x729eec00,
   LAvecun_VEXTH_HU_BU  = 0x729ef000,
   LAvecun_VEXTH_WU_HU  = 0x729ef400,
   LAvecun_VEXTH_DU_WU  = 0x729ef800,
   LAvecun_VEXTH_QU_DU  = 0x729efc00,
   LAvecun_VREPLGR2VR_B = 0x729f0000,
   LAvecun_VREPLGR2VR_H = 0x729f0400,
   LAvecun_VREPLGR2VR_W = 0x729f0800,
   LAvecun_VREPLGR2VR_D = 0x729f0c00
} LOONGARCH64VecUnOp;

/* Tags for vector binary operations */
typedef enum {
   LAvecbin_VSEQ_B        = 0x70000000,
   LAvecbin_VSEQ_H        = 0x70008000,
   LAvecbin_VSEQ_W        = 0x70010000,
   LAvecbin_VSEQ_D        = 0x70018000,
   LAvecbin_VSLT_B        = 0x70060000,
   LAvecbin_VSLT_H        = 0x70068000,
   LAvecbin_VSLT_W        = 0x70070000,
   LAvecbin_VSLT_D        = 0x70078000,
   LAvecbin_VSLT_BU       = 0x70080000,
   LAvecbin_VSLT_HU       = 0x70088000,
   LAvecbin_VSLT_WU       = 0x70090000,
   LAvecbin_VSLT_DU       = 0x70098000,
   LAvecbin_VADD_B        = 0x700a0000,
   LAvecbin_VADD_H        = 0x700a8000,
   LAvecbin_VADD_W        = 0x700b0000,
   LAvecbin_VADD_D        = 0x700b8000,
   LAvecbin_VSUB_B        = 0x700c0000,
   LAvecbin_VSUB_H        = 0x700c8000,
   LAvecbin_VSUB_W        = 0x700d0000,
   LAvecbin_VSUB_D        = 0x700d8000,
   LAvecbin_VSADD_B       = 0x70460000,
   LAvecbin_VSADD_H       = 0x70468000,
   LAvecbin_VSADD_W       = 0x70470000,
   LAvecbin_VSADD_D       = 0x70478000,
   LAvecbin_VSSUB_B       = 0x70480000,
   LAvecbin_VSSUB_H       = 0x70488000,
   LAvecbin_VSSUB_W       = 0x70490000,
   LAvecbin_VSSUB_D       = 0x70498000,
   LAvecbin_VSADD_BU      = 0x704a0000,
   LAvecbin_VSADD_HU      = 0x704a8000,
   LAvecbin_VSADD_WU      = 0x704b0000,
   LAvecbin_VSADD_DU      = 0x704b8000,
   LAvecbin_VSSUB_BU      = 0x704c0000,
   LAvecbin_VSSUB_HU      = 0x704c8000,
   LAvecbin_VSSUB_WU      = 0x704d0000,
   LAvecbin_VSSUB_DU      = 0x704d8000,
   LAvecbin_VADDA_B       = 0x705c0000,
   LAvecbin_VADDA_H       = 0x705c8000,
   LAvecbin_VADDA_W       = 0x705d0000,
   LAvecbin_VADDA_D       = 0x705d8000,
   LAvecbin_VAVGR_B       = 0x70680000,
   LAvecbin_VAVGR_H       = 0x70688000,
   LAvecbin_VAVGR_W       = 0x70690000,
   LAvecbin_VAVGR_D       = 0x70698000,
   LAvecbin_VAVGR_BU      = 0x706a0000,
   LAvecbin_VAVGR_HU      = 0x706a8000,
   LAvecbin_VAVGR_WU      = 0x706b0000,
   LAvecbin_VAVGR_DU      = 0x706b8000,
   LAvecbin_VMAX_B        = 0x70700000,
   LAvecbin_VMAX_H        = 0x70708000,
   LAvecbin_VMAX_W        = 0x70710000,
   LAvecbin_VMAX_D        = 0x70718000,
   LAvecbin_VMIN_B        = 0x70720000,
   LAvecbin_VMIN_H        = 0x70728000,
   LAvecbin_VMIN_W        = 0x70730000,
   LAvecbin_VMIN_D        = 0x70738000,
   LAvecbin_VMAX_BU       = 0x70740000,
   LAvecbin_VMAX_HU       = 0x70748000,
   LAvecbin_VMAX_WU       = 0x70750000,
   LAvecbin_VMAX_DU       = 0x70758000,
   LAvecbin_VMIN_BU       = 0x70760000,
   LAvecbin_VMIN_HU       = 0x70768000,
   LAvecbin_VMIN_WU       = 0x70770000,
   LAvecbin_VMIN_DU       = 0x70778000,
   LAvecbin_VMUL_B        = 0x70840000,
   LAvecbin_VMUL_H        = 0x70848000,
   LAvecbin_VMUL_W        = 0x70850000,
   LAvecbin_VMUH_B        = 0x70860000,
   LAvecbin_VMUH_H        = 0x70868000,
   LAvecbin_VMUH_W        = 0x70870000,
   LAvecbin_VMUH_BU       = 0x70880000,
   LAvecbin_VMUH_HU       = 0x70888000,
   LAvecbin_VMUH_WU       = 0x70890000,
   LAvecbin_VSLL_B        = 0x70e80000,
   LAvecbin_VSLL_H        = 0x70e88000,
   LAvecbin_VSLL_W        = 0x70e90000,
   LAvecbin_VSLL_D        = 0x70e98000,
   LAvecbin_VSRL_B        = 0x70ea0000,
   LAvecbin_VSRL_H        = 0x70ea8000,
   LAvecbin_VSRL_W        = 0x70eb0000,
   LAvecbin_VSRL_D        = 0x70eb8000,
   LAvecbin_VSRA_B        = 0x70ec0000,
   LAvecbin_VSRA_H        = 0x70ec8000,
   LAvecbin_VSRA_W        = 0x70ed0000,
   LAvecbin_VSRA_D        = 0x70ed8000,
   LAvecbin_VILVL_B       = 0x711a0000,
   LAvecbin_VILVL_H       = 0x711a8000,
   LAvecbin_VILVL_W       = 0x711b0000,
   LAvecbin_VILVL_D       = 0x711b8000,
   LAvecbin_VILVH_B       = 0x711c0000,
   LAvecbin_VILVH_H       = 0x711c8000,
   LAvecbin_VILVH_W       = 0x711d0000,
   LAvecbin_VILVH_D       = 0x711d8000,
   LAvecbin_VPICKEV_B     = 0x711e0000,
   LAvecbin_VPICKEV_H     = 0x711e8000,
   LAvecbin_VPICKEV_W     = 0x711f0000,
   LAvecbin_VPICKOD_B     = 0x71200000,
   LAvecbin_VPICKOD_H     = 0x71208000,
   LAvecbin_VPICKOD_W     = 0x71210000,
   LAvecbin_VREPLVE_B     = 0x71220000,
   LAvecbin_VREPLVE_H     = 0x71228000,
   LAvecbin_VREPLVE_W     = 0x71230000,
   LAvecbin_VREPLVE_D     = 0x71238000,
   LAvecbin_VAND_V        = 0x71260000,
   LAvecbin_VOR_V         = 0x71268000,
   LAvecbin_VXOR_V        = 0x71270000,
   LAvecbin_VNOR_V        = 0x71278000,
   LAvecbin_VADD_Q        = 0x712d0000,
   LAvecbin_VSUB_Q        = 0x712d8000,
   LAvecbin_VFADD_S       = 0x71308000,
   LAvecbin_VFADD_D       = 0x71310000,
   LAvecbin_VFSUB_S       = 0x71328000,
   LAvecbin_VFSUB_D       = 0x71330000,
   LAvecbin_VFMUL_S       = 0x71388000,
   LAvecbin_VFMUL_D       = 0x71390000,
   LAvecbin_VFDIV_S       = 0x713a8000,
   LAvecbin_VFDIV_D       = 0x713b0000,
   LAvecbin_VFMAX_S       = 0x713c8000,
   LAvecbin_VFMAX_D       = 0x713d0000,
   LAvecbin_VFMIN_S       = 0x713e8000,
   LAvecbin_VFMIN_D       = 0x713f0000,
   LAvecbin_VBSLL_V       = 0x728e0000,
   LAvecbin_VBSRL_V       = 0x728e8000,
   LAvecbin_VINSGR2VR_B   = 0x72eb8000,
   LAvecbin_VINSGR2VR_H   = 0x72ebc000,
   LAvecbin_VINSGR2VR_W   = 0x72ebe000,
   LAvecbin_VINSGR2VR_D   = 0x72ebf000,
   LAvecbin_VPICKVE2GR_W  = 0x72efe000,
   LAvecbin_VPICKVE2GR_D  = 0x72eff000,
   LAvecbin_VPICKVE2GR_BU = 0x72f38000,
   LAvecbin_VPICKVE2GR_HU = 0x72f3c000,
   LAvecbin_VPICKVE2GR_WU = 0x72f3e000,
   LAvecbin_VPICKVE2GR_DU = 0x72f3f000,
   LAvecbin_VSLLI_B       = 0x732c2000,
   LAvecbin_VSLLI_H       = 0x732c4000,
   LAvecbin_VSLLI_W       = 0x732c8000,
   LAvecbin_VSLLI_D       = 0x732d0000,
   LAvecbin_VSRLI_B       = 0x73302000,
   LAvecbin_VSRLI_H       = 0x73304000,
   LAvecbin_VSRLI_W       = 0x73308000,
   LAvecbin_VSRLI_D       = 0x73310000,
   LAvecbin_VSRAI_B       = 0x73342000,
   LAvecbin_VSRAI_H       = 0x73344000,
   LAvecbin_VSRAI_W       = 0x73348000,
   LAvecbin_VSRAI_D       = 0x73350000,
   LAvecbin_VORI_B        = 0x73d40000
} LOONGARCH64VecBinOp;

/* Tags for vector load operations */
typedef enum {
   LAvecload_VLD  = 0x2c000000,
   LAvecload_VLDX = 0x38400000
} LOONGARCH64VecLoadOp;

/* Tags for vector store operations */
typedef enum {
   LAvecstore_VST  = 0x2c400000,
   LAvecstore_VSTX = 0x38440000
} LOONGARCH64VecStoreOp;

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

   /* Vector insns */
   LAin_VecUn,       /* vector unary */
   LAin_VecBin,      /* vector binary */
   LAin_VecLoad,     /* vector load */
   LAin_VecStore,    /* vector store */

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
         LOONGARCH64VecUnOp   op;
         HReg                 src;
         HReg                 dst;
      } VecUnary;
      struct {
         LOONGARCH64VecBinOp  op;
         LOONGARCH64RI*       src2;
         HReg                 src1;
         HReg                 dst;
      } VecBinary;
      struct {
         LOONGARCH64VecLoadOp op;
         LOONGARCH64AMode*    src;
         HReg                 dst;
      } VecLoad;
      struct {
         LOONGARCH64VecStoreOp op;
         LOONGARCH64AMode*     dst;
         HReg                  src;
      } VecStore;
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
extern LOONGARCH64Instr* LOONGARCH64Instr_VecUnary  ( LOONGARCH64VecUnOp op,
                                                      HReg src, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_VecBinary ( LOONGARCH64VecBinOp op,
                                                      LOONGARCH64RI* src2,
                                                      HReg src1, HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_VecLoad   ( LOONGARCH64VecLoadOp op,
                                                      LOONGARCH64AMode* src,
                                                      HReg dst );
extern LOONGARCH64Instr* LOONGARCH64Instr_VecStore  ( LOONGARCH64VecStoreOp op,
                                                      LOONGARCH64AMode* dst,
                                                      HReg src );
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
