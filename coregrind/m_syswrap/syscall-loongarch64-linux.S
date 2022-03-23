
/*--------------------------------------------------------------------*/
/*--- Support for doing system calls.  syscall-loongarch64-linux.S ---*/
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

#include "pub_core_vkiscnums_asm.h"
#include "libvex_guest_offsets.h"

/*----------------------------------------------------------------*/
/*
   Perform a syscall for the client. This will run a syscall
   with the client's specific per-thread signal mask.

   The structure of this function is such that, if the syscall is
   interrupted by a signal, we can determine exactly what
   execution state we were in with respect to the execution of
   the syscall by examining the value of PC in the signal
   handler. This means that we can always do the appropriate
   thing to precisely emulate the kernel's signal/syscall
   interactions.

   The syscall number is taken from the argument, even though it
   should also be in guest_state->guest_R11. The syscall result
   is written back to guest_state->guest_R4 on completion.

   VG_(fixup_guest_state_after_syscall_interrupted) does the
   thread state fixup in the case where we were interrupted by a
   signal.

   Prototype:

   UWord ML_(do_syscall_for_client_WRK)(
             Int syscallno,                 // $r4 - a0
             void* guest_state,             // $r5 - a1
             const vki_sigset_t *sysmask,   // $r6 - a2
             const vki_sigset_t *postmask,  // $r7 - a3
             Int nsigwords)                 // $r8 - a4
*/

/* from vki-loongarch64-linux.h */
#define VKI_SIG_SETMASK 2

.globl ML_(do_syscall_for_client_WRK)
ML_(do_syscall_for_client_WRK):

   /* TODO */
1:
2:
3:
4:
5:

.section .rodata
/* export the ranges so that
   VG_(fixup_guest_state_after_syscall_interrupted) can do the
   right thing */

.globl ML_(blksys_setup)
.globl ML_(blksys_restart)
.globl ML_(blksys_complete)
.globl ML_(blksys_committed)
.globl ML_(blksys_finished)
ML_(blksys_setup):      .quad 1b
ML_(blksys_restart):    .quad 2b
ML_(blksys_complete):   .quad 3b
ML_(blksys_committed):  .quad 4b
ML_(blksys_finished):   .quad 5b

#endif // defined(VGP_loongarch64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                              syscall-loongarch64-linux.S ---*/
/*--------------------------------------------------------------------*/
