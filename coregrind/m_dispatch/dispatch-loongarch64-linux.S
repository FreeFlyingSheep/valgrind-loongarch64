
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                 dispatch-loongarch64-linux.S ---*/
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

#include "pub_core_basics_asm.h"

#if defined(VGP_loongarch64_linux)

#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_loongarch64_* */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(disp_run_translations) is    ---*/
/*--- used to run all translations,                        ---*/
/*--- including no-redir ones.                             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Entry and preamble (set everything up)       ---*/
/*----------------------------------------------------*/

/* signature:
void VG_(disp_run_translations)( UWord* two_words,
                                 void*  guest_state,
                                 Addr   host_addr );
*/

.text
.globl VG_(disp_run_translations)
VG_(disp_run_translations):
   /* TODO */

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
   /* TODO */

/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
.globl VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
   /* TODO */

/* ------ Chain me to fast entry point ------ */
.globl VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
   /* TODO */

/* ------ Indirect but boring jump ------ */
.globl VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
   /* TODO */

/* ------ Assisted jump ------ */
.globl VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
   /* TODO */

/* ------ Event check failed ------ */
.globl VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
   /* TODO */

.size VG_(disp_run_translations), .-VG_(disp_run_translations)

#endif // defined(VGP_loongarch64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                             dispatch-loongarch64-linux.S ---*/
/*--------------------------------------------------------------------*/
