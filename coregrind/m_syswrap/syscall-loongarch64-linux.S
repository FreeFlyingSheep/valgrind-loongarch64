
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

   /* Save regs on stack */
   addi.d  $sp, $sp, -24
   st.d    $a1, $sp, 0                      /* guest_state */
   st.d    $a3, $sp, 8                      /* postmask */
   st.d    $a4, $sp, 16                     /* sigsetSzB */

1: li.w    $a7, __NR_rt_sigprocmask
   li.w    $a0, VKI_SIG_SETMASK
   move    $a1, $a2                         /* syscall_mask */
   move    $a2, $a3                         /* postmask */
   move    $a3, $a4                         /* sigsetSzB */
   syscall 0

   bnez    $a0, 5f

   /* Actually do the client syscall */
   ld.d    $a6, $sp, 0                      /* guest_state */

   ld.d    $a0, $a6, OFFSET_loongarch64_R4  /* a0 */
   ld.d    $a1, $a6, OFFSET_loongarch64_R5  /* a1 */
   ld.d    $a2, $a6, OFFSET_loongarch64_R6  /* a2 */
   ld.d    $a3, $a6, OFFSET_loongarch64_R7  /* a3 */
   ld.d    $a4, $a6, OFFSET_loongarch64_R8  /* a4 */
   ld.d    $a5, $a6, OFFSET_loongarch64_R9  /* a5 */

   ld.d    $a7, $a6, OFFSET_loongarch64_R11 /* syscallno */

2: syscall 0

   /* Saving return values into guest state */
3: st.d    $a0, $a6, OFFSET_loongarch64_R4  /* a0 */

4: li.w    $a7, __NR_rt_sigprocmask
   li.w    $a0, VKI_SIG_SETMASK
   ld.d    $a1, $sp, 8                      /* postmask */
   move    $a2, $zero                       /* 0 (zero) */
   ld.d    $a3, $sp, 16                     /* sigsetSzB */
   syscall 0

   beqz    $a0, 6f

5: /* error */
   li.w    $a0, 0x8000

6: /* Restore sp and return */
   addi.d  $sp, $sp, 24
   jr      $ra

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
