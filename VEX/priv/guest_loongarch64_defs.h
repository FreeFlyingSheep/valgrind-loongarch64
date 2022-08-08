
/*---------------------------------------------------------------*/
/*--- begin                          guest_loongarch64_defs.h ---*/
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

/* Only to be used within the guest-loongarch64 directory. */

#ifndef __VEX_GUEST_LOONGARCH64_DEFS_H
#define __VEX_GUEST_LOONGARCH64_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"  /* DisResult */


/*---------------------------------------------------------*/
/*--- loongarch64 to IR conversion                      ---*/
/*---------------------------------------------------------*/

/* Convert one LOONGARCH64 insn to IR.  See the type DisOneInstrFn in
   guest_generic_bb_to_IR.h. */
extern DisResult disInstr_LOONGARCH64 ( IRSB*              irsb_IN,
                                        const UChar*       guest_code_IN,
                                        Long               delta,
                                        Addr               guest_IP,
                                        VexArch            guest_arch,
                                        const VexArchInfo* archinfo,
                                        const VexAbiInfo*  abiinfo,
                                        VexEndness         host_endness_IN,
                                        Bool               sigill_diag_IN );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr* guest_loongarch64_spechelper ( const HChar* function_name,
                                              IRExpr**     args,
                                              IRStmt**     precedingStmts,
                                              Int          n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern Bool guest_loongarch64_state_requires_precise_mem_exns ( Int minoff,
                                                                Int maxoff,
                                                                VexRegisterUpdates pxControl );

extern VexGuestLayout loongarch64Guest_layout;


/*---------------------------------------------------------*/
/*--- loongarch64 guest helpers                         ---*/
/*---------------------------------------------------------*/

enum fpop {
   FADD_S, FADD_D, FSUB_S, FSUB_D,
   FMUL_S, FMUL_D, FDIV_S, FDIV_D,
   FMADD_S, FMADD_D, FMSUB_S, FMSUB_D,
   FNMADD_S, FNMADD_D, FNMSUB_S, FNMSUB_D,
   FMAX_S, FMAX_D, FMIN_S, FMIN_D,
   FMAXA_S, FMAXA_D, FMINA_S, FMINA_D,
   FABS_S, FABS_D, FNEG_S, FNEG_D,
   FSQRT_S, FSQRT_D,
   FRECIP_S, FRECIP_D,
   FRSQRT_S, FRSQRT_D,
   FSCALEB_S, FSCALEB_D,
   FLOGB_S, FLOGB_D,
   FCMP_CAF_S, FCMP_CAF_D, FCMP_SAF_S, FCMP_SAF_D,
   FCMP_CLT_S, FCMP_CLT_D, FCMP_SLT_S, FCMP_SLT_D,
   FCMP_CEQ_S, FCMP_CEQ_D, FCMP_SEQ_S, FCMP_SEQ_D,
   FCMP_CLE_S, FCMP_CLE_D, FCMP_SLE_S, FCMP_SLE_D,
   FCMP_CUN_S, FCMP_CUN_D, FCMP_SUN_S, FCMP_SUN_D,
   FCMP_CULT_S, FCMP_CULT_D, FCMP_SULT_S, FCMP_SULT_D,
   FCMP_CUEQ_S, FCMP_CUEQ_D, FCMP_SUEQ_S, FCMP_SUEQ_D,
   FCMP_CULE_S, FCMP_CULE_D, FCMP_SULE_S, FCMP_SULE_D,
   FCMP_CNE_S, FCMP_CNE_D, FCMP_SNE_S, FCMP_SNE_D,
   FCMP_COR_S, FCMP_COR_D, FCMP_SOR_S, FCMP_SOR_D,
   FCMP_CUNE_S, FCMP_CUNE_D, FCMP_SUNE_S, FCMP_SUNE_D,
   FCVT_S_D, FCVT_D_S,
   FTINTRM_W_S, FTINTRM_W_D, FTINTRM_L_S, FTINTRM_L_D,
   FTINTRP_W_S, FTINTRP_W_D, FTINTRP_L_S, FTINTRP_L_D,
   FTINTRZ_W_S, FTINTRZ_W_D, FTINTRZ_L_S, FTINTRZ_L_D,
   FTINTRNE_W_S, FTINTRNE_W_D, FTINTRNE_L_S, FTINTRNE_L_D,
   FTINT_W_S, FTINT_W_D, FTINT_L_S, FTINT_L_D,
   FFINT_S_W, FFINT_D_W, FFINT_S_L, FFINT_D_L,
   FRINT_S, FRINT_D
};

extern ULong loongarch64_calculate_cpucfg    ( ULong src );
extern ULong loongarch64_calculate_revb_2h   ( ULong src );
extern ULong loongarch64_calculate_revb_4h   ( ULong src );
extern ULong loongarch64_calculate_revb_2w   ( ULong src );
extern ULong loongarch64_calculate_revb_d    ( ULong src );
extern ULong loongarch64_calculate_revh_2w   ( ULong src );
extern ULong loongarch64_calculate_revh_d    ( ULong src );
extern ULong loongarch64_calculate_bitrev_4b ( ULong src );
extern ULong loongarch64_calculate_bitrev_8b ( ULong src );
extern ULong loongarch64_calculate_bitrev_w  ( ULong src );
extern ULong loongarch64_calculate_bitrev_d  ( ULong src );
extern ULong loongarch64_calculate_crc       ( ULong old, ULong msg, ULong len );
extern ULong loongarch64_calculate_crcc      ( ULong old, ULong msg, ULong len );
extern ULong loongarch64_calculate_FCSR      ( enum fpop op, ULong src1,
                                               ULong src2, ULong src3 );

#endif /* ndef __VEX_GUEST_LOONGARCH64_DEFS_H */


/*---------------------------------------------------------------*/
/*--- end                            guest_loongarch64_defs.h ---*/
/*---------------------------------------------------------------*/
