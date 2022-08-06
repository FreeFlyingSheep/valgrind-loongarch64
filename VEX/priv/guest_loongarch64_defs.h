
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

#endif /* ndef __VEX_GUEST_LOONGARCH64_DEFS_H */


/*---------------------------------------------------------------*/
/*--- end                            guest_loongarch64_defs.h ---*/
/*---------------------------------------------------------------*/
