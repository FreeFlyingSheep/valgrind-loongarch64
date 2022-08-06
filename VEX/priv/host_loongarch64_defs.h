
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

extern UInt ppHRegLOONGARCH64 ( HReg reg );

/* Number of registers used arg passing in function calls */
#define LOONGARCH64_N_ARGREGS 8 /* a0 ... a7 */


/* --------- Instructions. --------- */

/* Tags for instructions */
typedef enum {
   /* Pseudo-insn, used for generating a 64-bit
      literal to register */
   LAin_LI          /* load imm */
} LOONGARCH64InstrTag;

typedef struct {
   LOONGARCH64InstrTag tag;
   union {
      struct {
         ULong                imm;
         HReg                 dst;
      } LI;
   } LAin;
} LOONGARCH64Instr;

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
