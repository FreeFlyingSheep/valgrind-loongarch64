
/*---------------------------------------------------------------------*/
/*--- Platform-specific syscalls stuff. syswrap-loongarch64-linux.c ---*/
/*---------------------------------------------------------------------*/

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

#if defined(VGP_loongarch64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_libcsignal.h"
#include "pub_core_options.h"
#include "pub_core_scheduler.h"
#include "pub_core_sigframe.h"      // For VG_(sigframe_destroy)()
#include "pub_core_syscall.h"
#include "pub_core_syswrap.h"
#include "pub_core_tooliface.h"

#include "priv_types_n_macros.h"
#include "priv_syswrap-generic.h"   /* for decls of generic wrappers */
#include "priv_syswrap-linux.h"     /* for decls of linux-ish wrappers */


/* ---------------------------------------------------------------------
   clone() handling
   ------------------------------------------------------------------ */

/* Call f(arg1), but first switch stacks, using 'stack' as the new
   stack, and use 'retaddr' as f's return-to address.  Also, clear all
   the integer registers before entering f. */
__attribute__((noreturn))
void ML_(call_on_new_stack_0_1) ( Addr stack,
                                  Addr retaddr,
                                  void (*f) (Word),
                                  Word arg1 );
/* TODO */
asm (
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1 \n\t"
"vgModuleLocal_call_on_new_stack_0_1:       \n\t"
".previous                                  \n\t"
);

/*
   Perform a clone system call.  clone is strange because it has
   fork()-like return-twice semantics, so it needs special
   handling here.

   Upon entry, we have:

      Word (*fn)(void*)  in a0
      void*  child_stack in a1
      int    flags       in a2
      void*  arg         in a3
      pid_t* child_tid   in a4
      pid_t* parent_tid  in a5
      void*  tls_ptr     in a6

	System call requires:

      int           $__NR_clone   in a7
      unsigned long clone_flags   in a0
      unsigned long newsp         in a1
		int*          parent_tidptr in a2
		int*          child_tidptr  in a3
		unsigned long tls           in a4
*/

// See priv_syswrap-linux.h for arg profile.
/* TODO */
asm(
".text                                     \n\t"
".globl do_syscall_clone_loongarch64_linux \n\t"
"do_syscall_clone_loongarch64_linux:       \n\t"
".previous                                 \n\t"
);


/* ---------------------------------------------------------------------
   More thread stuff
   ------------------------------------------------------------------ */

// loongarch64 doesn't have any architecture specific thread stuff that
// needs to be cleaned up
void VG_(cleanup_thread) ( ThreadArchState* arch )
{
}

/* ---------------------------------------------------------------------
   PRE/POST wrappers for loongarch64/Linux-specific syscalls
   ------------------------------------------------------------------ */

/* TODO */


/* ---------------------------------------------------------------------
   The loongarch64/Linux syscall table
   ------------------------------------------------------------------ */

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   /* TODO */
   return NULL;
}

#endif  /* defined(VGP_loongarch64_linux) */

/*--------------------------------------------------------------------*/
/*--- end                              syswrap-loongarch64-linux.c ---*/
/*--------------------------------------------------------------------*/
