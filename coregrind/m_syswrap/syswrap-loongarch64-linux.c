
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
#include "priv_syswrap-main.h"


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
asm (
".text\n"
".globl vgModuleLocal_call_on_new_stack_0_1 \n\t"
"vgModuleLocal_call_on_new_stack_0_1:       \n\t"
"   move $sp, $a0                           \n\t" /* sp = stack */
"   move $ra, $a1                           \n\t" /* ra = retaddr */
"   move $t0, $a2                           \n\t" /* t0 = f */
"   move $a0, $a3                           \n\t" /* a0 = arg1 */
"   move $a1, $zero                         \n\t" /* zero all GP regs */
"   move $a2, $zero                         \n\t"
"   move $a3, $zero                         \n\t"
"   move $a4, $zero                         \n\t"
"   move $a5, $zero                         \n\t"
"   move $a6, $zero                         \n\t"
"   move $a7, $zero                         \n\t"
/* don't zero out t0 */
"   move $t1, $zero                         \n\t"
"   move $t2, $zero                         \n\t"
"   move $t3, $zero                         \n\t"
"   move $t4, $zero                         \n\t"
"   move $t5, $zero                         \n\t"
"   move $t6, $zero                         \n\t"
"   move $t7, $zero                         \n\t"
"   move $t8, $zero                         \n\t"
"   jr   $t0                                \n\t" /* jump to f */
".previous                                  \n\t"
);

/*
   Perform a clone system call.  clone is strange because it has
   fork()-like return-twice semantics, so it needs special
   handling here.

   Upon entry, we have:

      Word   (*fn)(void*) in a0
      void*  child_stack  in a1
      int    flags        in a2
      void*  arg          in a3
      pid_t* child_tid    in a4
      pid_t* parent_tid   in a5
      void*  tls_ptr      in a6

	System call requires:

      unsigned long clone_flags   in a0
      unsigned long newsp         in a1
		int*          parent_tidptr in a2
		int*          child_tidptr  in a3
		unsigned long tls           in a4
      int           __NR_clone    in a7
*/

#define __NR_CLONE VG_STRINGIFY(__NR_clone)
#define __NR_EXIT  VG_STRINGIFY(__NR_exit)

// See priv_syswrap-linux.h for arg profile.
asm(
".text                                     \n\t"
".globl do_syscall_clone_loongarch64_linux \n\t"
"do_syscall_clone_loongarch64_linux:       \n\t"
/* Save ra */
"   addi.d  $sp, $sp, -16                  \n\t"
"   st.d    $ra, $sp, 0                    \n\t"

/* Save fn and arg */
"   addi.d  $a1, $a1, -16                  \n\t"
"   st.d    $a0, $a1, 0                    \n\t" /* fn */
"   st.d    $a3, $a1, 8                    \n\t" /* arg */

/* Call sys_clone */
"   move    $a0, $a2                       \n\t" /* flags */
"   move    $a2, $a5                       \n\t" /* parent */
"   move    $a3, $a4                       \n\t" /* child */
"   move    $a4, $a6                       \n\t" /* tls */
"   li.w    $a7, " __NR_CLONE "            \n\t"
"   syscall 0                              \n\t"

/* If we are a child? */
"   bnez    $a0, 1f                        \n\t"

/* Restore fn and arg */
"   ld.d    $a1, $sp, 0                    \n\t" /* fn */
"   ld.d    $a0, $sp, 8                    \n\t" /* arg */

/* Call fn(arg) */
"   jr      $a1                            \n\t"

/* Call exit(a0) */
"   li.w    $a7, " __NR_EXIT"              \n\t"
"   syscall 0                              \n\t"

/* If we are parent or error, just return to caller */
"1:                                        \n\t"
"   ld.d    $ra, $sp, 0                    \n\t"
"   addi.d  $sp, $sp, 16                   \n\t"
"   jr      $ra                            \n\t"
".previous                                 \n\t"
);

#undef __NR_CLONE
#undef __NR_EXIT

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

#define PRE(name)       DEFN_PRE_TEMPLATE(loongarch64_linux, name)
#define POST(name)      DEFN_POST_TEMPLATE(loongarch64_linux, name)

/* Add prototypes for the wrappers declared here, so that gcc doesn't
   harass us for not having prototypes.  Really this is a kludge --
   the right thing to do is to make these wrappers 'static' since they
   aren't visible outside this file, but that requires even more macro
   magic. */
DECL_TEMPLATE(loongarch64_linux, sys_ptrace);
DECL_TEMPLATE(loongarch64_linux, sys_mmap);
DECL_TEMPLATE(loongarch64_linux, sys_rt_sigreturn);

PRE(sys_ptrace)
{
   PRINT("sys_ptrace ( %ld, %ld, %lx, %lx )",
         SARG1, SARG2, ARG3, ARG4);
   PRE_REG_READ4(int, "ptrace",
                 long, request,
                 long, pid,
                 unsigned long, addr,
                 unsigned long, data);
   switch (ARG1) {
      case VKI_PTRACE_PEEKTEXT:
      case VKI_PTRACE_PEEKDATA:
      case VKI_PTRACE_PEEKUSR:
         PRE_MEM_WRITE("ptrace(peek)", ARG4, sizeof(long));
         break;
      case VKI_PTRACE_GETEVENTMSG:
         PRE_MEM_WRITE("ptrace(geteventmsg)", ARG4, sizeof(unsigned long));
         break;
      case VKI_PTRACE_GETSIGINFO:
         PRE_MEM_WRITE("ptrace(getsiginfo)", ARG4, sizeof(vki_siginfo_t));
         break;
      case VKI_PTRACE_SETSIGINFO:
         PRE_MEM_READ("ptrace(setsiginfo)", ARG4, sizeof(vki_siginfo_t));
         break;
      case VKI_PTRACE_GETREGSET:
         ML_(linux_PRE_getregset)(tid, ARG3, ARG4);
         break;
      default:
         break;
   }
}

POST(sys_ptrace)
{
   switch (ARG1) {
      case VKI_PTRACE_TRACEME:
         ML_(linux_POST_traceme)(tid);
         break;
      case VKI_PTRACE_PEEKTEXT:
      case VKI_PTRACE_PEEKDATA:
      case VKI_PTRACE_PEEKUSR:
         POST_MEM_WRITE (ARG4, sizeof(long));
         break;
      case VKI_PTRACE_GETEVENTMSG:
         POST_MEM_WRITE (ARG4, sizeof(unsigned long));
      break;
      case VKI_PTRACE_GETSIGINFO:
         POST_MEM_WRITE (ARG4, sizeof(vki_siginfo_t));
         break;
      case VKI_PTRACE_GETREGSET:
         ML_(linux_POST_getregset)(tid, ARG3, ARG4);
         break;
      default:
      break;
   }
}

PRE(sys_mmap)
{
   SysRes r;

   PRINT("sys_mmap ( %#lx, %lu, %lu, %#lx, %lu, %lu )",
         ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   PRE_REG_READ6(long, "mmap",
                 unsigned long, addr, unsigned long, len,
                 unsigned long, prot, unsigned long, flags,
                 unsigned long, fd,   vki_off_t,     offset);

   r = ML_(generic_PRE_sys_mmap)( tid, ARG1, ARG2, ARG3, ARG4, ARG5, ARG6 );
   SET_STATUS_from_SysRes(r);
}

PRE(sys_rt_sigreturn)
{
   /* See comments on PRE(sys_rt_sigreturn) in syswrap-loongarch64-linux.c for
      an explanation of what follows. */
   ThreadState* tst;
   PRINT("rt_sigreturn ( )");

   vg_assert(VG_(is_valid_tid)(tid));
   vg_assert(tid >= 1 && tid < VG_N_THREADS);
   vg_assert(VG_(is_running_thread)(tid));

   tst = VG_(get_ThreadState)(tid);

   /* This is only so that the PC is (might be) useful to report if
      something goes wrong in the sigreturn */
   ML_(fixup_guest_state_to_restart_syscall)(&tst->arch);

   /* Restore register state from frame and remove it */
   VG_(sigframe_destroy)(tid, True);

   /* Tell the driver not to update the guest state with the "result",
      and set a bogus result to keep it happy. */
   *flags |= SfNoWriteResult;
   SET_STATUS_Success(0);

   /* Check to see if any signals arose as a result of this. */
   *flags |= SfPollAfter;
}

#undef PRE
#undef POST

/* ---------------------------------------------------------------------
   The loongarch64/Linux syscall table
   ------------------------------------------------------------------ */

#define PLAX_(sysno, name)    WRAPPER_ENTRY_X_(loongarch64_linux, sysno, name)
#define PLAXY(sysno, name)    WRAPPER_ENTRY_XY(loongarch64_linux, sysno, name)

// This table maps from __NR_xxx syscall numbers (from
// linux/include/uapi/asm-generic/unistd.h) to the appropriate PRE/POST
// sys_foo() wrappers on loongarch64 (as per sys_call_table in
// linux/arch/loongarch/kernel/syscall.c).
//
// For those syscalls not handled by Valgrind, the annotation indicate its
// arch/OS combination, eg. */* (generic), */Linux (Linux only), ?/?
// (unknown).

static SyscallTableEntry syscall_main_table[] = {
   LINXY(__NR_io_setup,                sys_io_setup),                // 0
   LINX_(__NR_io_destroy,              sys_io_destroy),              // 1
   LINX_(__NR_io_submit,               sys_io_submit),               // 2
   LINXY(__NR_io_cancel,               sys_io_cancel),               // 3
   LINXY(__NR_io_getevents,            sys_io_getevents),            // 4
   LINX_(__NR_setxattr,                sys_setxattr),                // 5
   LINX_(__NR_lsetxattr,               sys_lsetxattr),               // 6
   LINX_(__NR_fsetxattr,               sys_fsetxattr),               // 7
   LINXY(__NR_getxattr,                sys_getxattr),                // 8
   LINXY(__NR_lgetxattr,               sys_lgetxattr),               // 9
   LINXY(__NR_fgetxattr,               sys_fgetxattr),               // 10
   LINXY(__NR_listxattr,               sys_listxattr),               // 11
   LINXY(__NR_llistxattr,              sys_llistxattr),              // 12
   LINXY(__NR_flistxattr,              sys_flistxattr),              // 13
   LINX_(__NR_removexattr,             sys_removexattr),             // 14
   LINX_(__NR_lremovexattr,            sys_lremovexattr),            // 15
   LINX_(__NR_fremovexattr,            sys_fremovexattr),            // 16
   GENXY(__NR_getcwd,                  sys_getcwd),                  // 17
   LINXY(__NR_lookup_dcookie,          sys_lookup_dcookie),          // 18
   LINXY(__NR_eventfd2,                sys_eventfd2),                // 19
   LINXY(__NR_epoll_create1,           sys_epoll_create1),           // 20
   LINX_(__NR_epoll_ctl,               sys_epoll_ctl),               // 21
   LINXY(__NR_epoll_pwait,             sys_epoll_pwait),             // 22
   GENXY(__NR_dup,                     sys_dup),                     // 23
   LINXY(__NR_dup3,                    sys_dup3),                    // 24
   LINXY(__NR3264_fcntl,               sys_fcntl),                   // 25
   LINXY(__NR_inotify_init1,           sys_inotify_init1),           // 26
   LINX_(__NR_inotify_add_watch,       sys_inotify_add_watch),       // 27
   LINX_(__NR_inotify_rm_watch,        sys_inotify_rm_watch),        // 28
   LINXY(__NR_ioctl,                   sys_ioctl),                   // 29
   LINX_(__NR_ioprio_set,              sys_ioprio_set),              // 30
   LINX_(__NR_ioprio_get,              sys_ioprio_get),              // 31
   GENX_(__NR_flock,                   sys_flock),                   // 32
   LINX_(__NR_mknodat,                 sys_mknodat),                 // 33
   LINX_(__NR_mkdirat,                 sys_mkdirat),                 // 34
   LINX_(__NR_unlinkat,                sys_unlinkat),                // 35
   LINX_(__NR_symlinkat,               sys_symlinkat),               // 36
   LINX_(__NR_linkat,                  sys_linkat),                  // 37
   //   (__NR_renameat,                sys_renameat),                // 38
   LINX_(__NR_umount2,                 sys_umount),                  // 39
   LINX_(__NR_mount,                   sys_mount),                   // 40
   LINX_(__NR_pivot_root,              sys_pivot_root),              // 41
   //   (__NR_nfsservctl,              sys_ni_syscall),              // 42
   GENXY(__NR3264_statfs,              sys_statfs),                  // 43
   GENXY(__NR3264_fstatfs,             sys_fstatfs),                 // 44
   GENX_(__NR3264_truncate,            sys_truncate),                // 45
   GENX_(__NR3264_ftruncate,           sys_ftruncate),               // 46
   LINX_(__NR_fallocate,               sys_fallocate),               // 47
   LINX_(__NR_faccessat,               sys_faccessat),               // 48
   GENX_(__NR_chdir,                   sys_chdir),                   // 49
   GENX_(__NR_fchdir,                  sys_fchdir),                  // 50
   GENX_(__NR_chroot,                  sys_chroot),                  // 51
   GENX_(__NR_fchmod,                  sys_fchmod),                  // 52
   LINX_(__NR_fchmodat,                sys_fchmodat),                // 53
   LINX_(__NR_fchownat,                sys_fchownat),                // 54
   GENX_(__NR_fchown,                  sys_fchown),                  // 55
   LINXY(__NR_openat,                  sys_openat),                  // 56
   GENXY(__NR_close,                   sys_close),                   // 57
   LINX_(__NR_vhangup,                 sys_vhangup),                 // 58
   LINXY(__NR_pipe2,                   sys_pipe2),                   // 59
   LINX_(__NR_quotactl,                sys_quotactl),                // 60
   GENXY(__NR_getdents64,              sys_getdents64),              // 61
   LINX_(__NR3264_lseek,               sys_lseek),                   // 62
   GENXY(__NR_read,                    sys_read),                    // 63
   GENX_(__NR_write,                   sys_write),                   // 64
   GENXY(__NR_readv,                   sys_readv),                   // 65
   GENX_(__NR_writev,                  sys_writev),                  // 66
   GENXY(__NR_pread64,                 sys_pread64),                 // 67
   GENX_(__NR_pwrite64,                sys_pwrite64),                // 68
   LINXY(__NR_preadv,                  sys_preadv),                  // 69
   LINX_(__NR_pwritev,                 sys_pwritev),                 // 70
   LINXY(__NR3264_sendfile,            sys_sendfile),                // 71
   LINXY(__NR_pselect6,                sys_pselect6),                // 72
   LINXY(__NR_ppoll,                   sys_ppoll),                   // 73
   LINXY(__NR_signalfd4,               sys_signalfd4),               // 74
   LINX_(__NR_vmsplice,                sys_vmsplice),                // 75
   LINX_(__NR_splice,                  sys_splice),                  // 76
   LINX_(__NR_tee,                     sys_tee),                     // 77
   LINX_(__NR_readlinkat,              sys_readlinkat),              // 78
   //   (__NR3264_fstatat,             sys_newfstatat),              // 79
   //   (__NR3264_fstat,               sys_newfstat),                // 80
   GENX_(__NR_sync,                    sys_sync),                    // 81
   GENX_(__NR_fsync,                   sys_fsync),                   // 82
   GENX_(__NR_fdatasync,               sys_fdatasync),               // 83
   LINX_(__NR_sync_file_range,         sys_sync_file_range),         // 84
   LINXY(__NR_timerfd_create,          sys_timerfd_create),          // 85
   LINXY(__NR_timerfd_settime,         sys_timerfd_settime),         // 86
   LINXY(__NR_timerfd_gettime,         sys_timerfd_gettime),         // 87
   LINX_(__NR_utimensat,               sys_utimensat),               // 88
   GENX_(__NR_acct,                    sys_acct),                    // 89
   LINXY(__NR_capget,                  sys_capget),                  // 90
   LINX_(__NR_capset,                  sys_capset),                  // 91
   LINX_(__NR_personality,             sys_personality),             // 92
   GENX_(__NR_exit,                    sys_exit),                    // 93
   LINX_(__NR_exit_group,              sys_exit_group),              // 94
   LINXY(__NR_waitid,                  sys_waitid),                  // 95
   LINX_(__NR_set_tid_address,         sys_set_tid_address),         // 96
   LINX_(__NR_unshare,                 sys_unshare),                 // 97
   LINXY(__NR_futex,                   sys_futex),                   // 98
   LINX_(__NR_set_robust_list,         sys_set_robust_list),         // 99
   LINXY(__NR_get_robust_list,         sys_get_robust_list),         // 100
   GENXY(__NR_nanosleep,               sys_nanosleep),               // 101
   GENXY(__NR_getitimer,               sys_getitimer),               // 102
   GENXY(__NR_setitimer,               sys_setitimer),               // 103
   //   (__NR_kexec_load,              sys_kexec_load),              // 104
   LINX_(__NR_init_module,             sys_init_module),             // 105
   LINX_(__NR_delete_module,           sys_delete_module),           // 106
   LINXY(__NR_timer_create,            sys_timer_create),            // 107
   LINXY(__NR_timer_gettime,           sys_timer_gettime),           // 108
   LINX_(__NR_timer_getoverrun,        sys_timer_getoverrun),        // 109
   LINXY(__NR_timer_settime,           sys_timer_settime),           // 110
   LINX_(__NR_timer_delete,            sys_timer_delete),            // 111
   LINX_(__NR_clock_settime,           sys_clock_settime),           // 112
   LINXY(__NR_clock_gettime,           sys_clock_gettime),           // 113
   LINXY(__NR_clock_getres,            sys_clock_getres),            // 114
   LINXY(__NR_clock_nanosleep,         sys_clock_nanosleep),         // 115
   LINXY(__NR_syslog,                  sys_syslog),                  // 116
   PLAXY(__NR_ptrace,                  sys_ptrace),                  // 117
   LINXY(__NR_sched_setparam,          sys_sched_setparam),          // 118
   LINX_(__NR_sched_setscheduler,      sys_sched_setscheduler),      // 119
   LINX_(__NR_sched_getscheduler,      sys_sched_getscheduler),      // 120
   LINXY(__NR_sched_getparam,          sys_sched_getparam),          // 121
   LINX_(__NR_sched_setaffinity,       sys_sched_setaffinity),       // 122
   LINXY(__NR_sched_getaffinity,       sys_sched_getaffinity),       // 123
   LINX_(__NR_sched_yield,             sys_sched_yield),             // 124
   LINX_(__NR_sched_get_priority_max,  sys_sched_get_priority_max),  // 125
   LINX_(__NR_sched_get_priority_min,  sys_sched_get_priority_min),  // 126
   LINXY(__NR_sched_rr_get_interval,   sys_sched_rr_get_interval),   // 127
   //   (__NR_restart_syscall,         sys_restart_syscall),         // 128
   GENX_(__NR_kill,                    sys_kill),                    // 129
   LINXY(__NR_tkill,                   sys_tkill),                   // 130
   LINX_(__NR_tgkill,                  sys_tgkill),                  // 131
   GENXY(__NR_sigaltstack,             sys_sigaltstack),             // 132
   LINX_(__NR_rt_sigsuspend,           sys_rt_sigsuspend),           // 133
   LINXY(__NR_rt_sigaction,            sys_rt_sigaction),            // 134
   LINXY(__NR_rt_sigprocmask,          sys_rt_sigprocmask),          // 135
   LINXY(__NR_rt_sigpending,           sys_rt_sigpending),           // 136
   LINXY(__NR_rt_sigtimedwait,         sys_rt_sigtimedwait),         // 137
   LINXY(__NR_rt_sigqueueinfo,         sys_rt_sigqueueinfo),         // 138
   PLAX_(__NR_rt_sigreturn,            sys_rt_sigreturn),            // 139
   GENX_(__NR_setpriority,             sys_setpriority),             // 140
   GENX_(__NR_getpriority,             sys_getpriority),             // 141
   //   (__NR_reboot,                  sys_reboot),                  // 142
   GENX_(__NR_setregid,                sys_setregid),                // 143
   GENX_(__NR_setgid,                  sys_setgid),                  // 144
   GENX_(__NR_setreuid,                sys_setreuid),                // 145
   GENX_(__NR_setuid,                  sys_setuid),                  // 146
   LINX_(__NR_setresuid,               sys_setresuid),               // 147
   LINXY(__NR_getresuid,               sys_getresuid),               // 148
   LINX_(__NR_setresgid,               sys_setresgid),               // 149
   LINXY(__NR_getresgid,               sys_getresgid),               // 150
   LINX_(__NR_setfsuid,                sys_setfsuid),                // 151
   LINX_(__NR_setfsgid,                sys_setfsgid),                // 152
   GENXY(__NR_times,                   sys_times),                   // 153
   GENX_(__NR_setpgid,                 sys_setpgid),                 // 154
   GENX_(__NR_getpgid,                 sys_getpgid),                 // 155
   GENX_(__NR_getsid,                  sys_getsid),                  // 156
   GENX_(__NR_setsid,                  sys_setsid),                  // 157
   GENXY(__NR_getgroups,               sys_getgroups),               // 158
   GENX_(__NR_setgroups,               sys_setgroups),               // 159
   GENXY(__NR_uname,                   sys_newuname),                // 160
   GENX_(__NR_sethostname,             sys_sethostname),             // 161
   //   (__NR_setdomainname,           sys_setdomainname),           // 162
   //   (__NR_getrlimit,               sys_old_getrlimit),           // 163
   //   (__NR_setrlimit,               sys_setrlimit),               // 164
   GENXY(__NR_getrusage,               sys_getrusage),               // 165
   GENX_(__NR_umask,                   sys_umask),                   // 166
   LINXY(__NR_prctl,                   sys_prctl),                   // 167
   LINXY(__NR_getcpu,                  sys_getcpu),                  // 168
   GENXY(__NR_gettimeofday,            sys_gettimeofday),            // 169
   GENX_(__NR_settimeofday,            sys_settimeofday),            // 170
   LINXY(__NR_adjtimex,                sys_adjtimex),                // 171
   GENX_(__NR_getpid,                  sys_getpid),                  // 172
   GENX_(__NR_getppid,                 sys_getppid),                 // 173
   GENX_(__NR_getuid,                  sys_getuid),                  // 174
   GENX_(__NR_geteuid,                 sys_geteuid),                 // 175
   GENX_(__NR_getgid,                  sys_getgid),                  // 176
   GENX_(__NR_getegid,                 sys_getegid),                 // 177
   LINX_(__NR_gettid,                  sys_gettid),                  // 178
   LINXY(__NR_sysinfo,                 sys_sysinfo),                 // 179
   LINXY(__NR_mq_open,                 sys_mq_open),                 // 180
   LINX_(__NR_mq_unlink,               sys_mq_unlink),               // 181
   LINX_(__NR_mq_timedsend,            sys_mq_timedsend),            // 182
   LINXY(__NR_mq_timedreceive,         sys_mq_timedreceive),         // 183
   LINX_(__NR_mq_notify,               sys_mq_notify),               // 184
   LINXY(__NR_mq_getsetattr,           sys_mq_getsetattr),           // 185
   LINX_(__NR_msgget,                  sys_msgget),                  // 186
   LINXY(__NR_msgctl,                  sys_msgctl),                  // 187
   LINXY(__NR_msgrcv,                  sys_msgrcv),                  // 188
   LINX_(__NR_msgsnd,                  sys_msgsnd),                  // 189
   LINX_(__NR_semget,                  sys_semget),                  // 190
   LINXY(__NR_semctl,                  sys_semctl),                  // 191
   LINX_(__NR_semtimedop,              sys_semtimedop),              // 192
   LINX_(__NR_semop,                   sys_semop),                   // 193
   LINX_(__NR_shmget,                  sys_shmget),                  // 194
   LINXY(__NR_shmctl,                  sys_shmctl),                  // 195
   LINXY(__NR_shmat,                   sys_shmat),                   // 196
   LINXY(__NR_shmdt,                   sys_shmdt),                   // 197
   LINXY(__NR_socket,                  sys_socket),                  // 198
   LINXY(__NR_socketpair,              sys_socketpair),              // 199
   LINX_(__NR_bind,                    sys_bind),                    // 200
   LINX_(__NR_listen,                  sys_listen),                  // 201
   LINXY(__NR_accept,                  sys_accept),                  // 202
   LINX_(__NR_connect,                 sys_connect),                 // 203
   LINXY(__NR_getsockname,             sys_getsockname),             // 204
   LINXY(__NR_getpeername,             sys_getpeername),             // 205
   LINX_(__NR_sendto,                  sys_sendto),                  // 206
   LINXY(__NR_recvfrom,                sys_recvfrom),                // 207
   LINX_(__NR_setsockopt,              sys_setsockopt),              // 208
   LINXY(__NR_getsockopt,              sys_getsockopt),              // 209
   LINX_(__NR_shutdown,                sys_shutdown),                // 210
   LINX_(__NR_sendmsg,                 sys_sendmsg),                 // 211
   LINXY(__NR_recvmsg,                 sys_recvmsg),                 // 212
   LINX_(__NR_readahead,               sys_readahead),               // 213
   GENX_(__NR_brk,                     sys_brk),                     // 214
   GENXY(__NR_munmap,                  sys_munmap),                  // 215
   GENX_(__NR_mremap,                  sys_mremap),                  // 216
   LINX_(__NR_add_key,                 sys_add_key),                 // 217
   LINX_(__NR_request_key,             sys_request_key),             // 218
   LINXY(__NR_keyctl,                  sys_keyctl),                  // 219
   LINX_(__NR_clone,                   sys_clone),                   // 220
   GENX_(__NR_execve,                  sys_execve),                  // 221
   PLAX_(__NR3264_mmap,                sys_mmap),                    // 222
   LINX_(__NR3264_fadvise64,           sys_fadvise64),               // 223
   //   (__NR_swapon,                  sys_swapon),                  // 224
   //   (__NR_swapoff,                 sys_swapoff),                 // 225
   GENXY(__NR_mprotect,                sys_mprotect),                // 226
   GENX_(__NR_msync,                   sys_msync),                   // 227
   GENX_(__NR_mlock,                   sys_mlock),                   // 228
   GENX_(__NR_munlock,                 sys_munlock),                 // 229
   GENX_(__NR_mlockall,                sys_mlockall),                // 230
   LINX_(__NR_munlockall,              sys_munlockall),              // 231
   GENXY(__NR_mincore,                 sys_mincore),                 // 232
   GENX_(__NR_madvise,                 sys_madvise),                 // 233
   //   (__NR_remap_file_pages,        sys_remap_file_pages),        // 234
   LINX_(__NR_mbind,                   sys_mbind),                   // 235
   LINXY(__NR_get_mempolicy,           sys_get_mempolicy),           // 236
   LINX_(__NR_set_mempolicy,           sys_set_mempolicy),           // 237
   //   (__NR_migrate_pages,           sys_migrate_pages),           // 238
   LINXY(__NR_move_pages,              sys_move_pages),              // 239
   LINXY(__NR_rt_tgsigqueueinfo,       sys_rt_tgsigqueueinfo),       // 240
   LINXY(__NR_perf_event_open,         sys_perf_event_open),         // 241
   LINXY(__NR_accept4,                 sys_accept4),                 // 242
   LINXY(__NR_recvmmsg,                sys_recvmmsg),                // 243

   GENXY(__NR_wait4,                   sys_wait4),                   // 260
   LINXY(__NR_prlimit64,               sys_prlimit64),               // 261
   LINXY(__NR_fanotify_init,           sys_fanotify_init),           // 262
   LINX_(__NR_fanotify_mark,           sys_fanotify_mark),           // 263
   LINXY(__NR_name_to_handle_at,       sys_name_to_handle_at),       // 264
   LINXY(__NR_open_by_handle_at,       sys_open_by_handle_at),       // 265
   LINXY(__NR_clock_adjtime,           sys_clock_adjtime),           // 266
   LINX_(__NR_syncfs,                  sys_syncfs),                  // 267
   LINX_(__NR_setns,                   sys_setns),                   // 268
   LINXY(__NR_sendmmsg,                sys_sendmmsg),                // 269
   LINXY(__NR_process_vm_readv,        sys_process_vm_readv),        // 270
   LINX_(__NR_process_vm_writev,       sys_process_vm_writev),       // 271
   LINX_(__NR_kcmp,                    sys_kcmp),                    // 272
   LINX_(__NR_finit_module,            sys_finit_module),            // 273
   LINX_(__NR_sched_setattr,           sys_sched_setattr),           // 274
   LINXY(__NR_sched_getattr,           sys_sched_getattr),           // 275
   LINX_(__NR_renameat2,               sys_renameat2),               // 276
   //   (__NR_seccomp,                 sys_seccomp),                 // 277
   LINXY(__NR_getrandom,               sys_getrandom),               // 278
   LINXY(__NR_memfd_create,            sys_memfd_create),            // 279
   LINXY(__NR_bpf,                     sys_bpf),                     // 280
   LINX_(__NR_execveat,                sys_execveat),                // 281
   //   (__NR_userfaultfd,             sys_userfaultfd),             // 282
   LINX_(__NR_membarrier,              sys_membarrier),              // 283
   //   (__NR_mlock2,                  sys_mlock2),                  // 284
   LINX_(__NR_copy_file_range,         sys_copy_file_range),         // 285
   LINXY(__NR_preadv2,                 sys_preadv2),                 // 286
   LINX_(__NR_pwritev2,                sys_pwritev2),                // 287
   //   (__NR_pkey_mprotect,           sys_pkey_mprotect),           // 288
   //   (__NR_pkey_alloc,              sys_pkey_alloc),              // 289
   //   (__NR_pkey_free,               sys_pkey_free),               // 290
   LINXY(__NR_statx,                   sys_statx),                   // 291
   //   (__NR_io_pgetevents,           sys_io_pgetevents),           // 292
   //   (__NR_rseq,                    sys_rseq),                    // 293
   //   (__NR_kexec_file_load,         sys_kexec_file_load),         // 294

   //   (__NR_pidfd_send_signal,       sys_pidfd_send_signal),       // 424
   LINXY(__NR_io_uring_setup,          sys_io_uring_setup),          // 425
   LINXY(__NR_io_uring_enter,          sys_io_uring_enter),          // 426
   LINXY(__NR_io_uring_register,       sys_io_uring_register),       // 427
   //   (__NR_open_tree,               sys_open_tree),               // 428
   //   (__NR_move_mount,              sys_move_mount),              // 429
   //   (__NR_fsopen,                  sys_fsopen),                  // 430
   //   (__NR_fsconfig,                sys_fsconfig),                // 431
   //   (__NR_fsmount,                 sys_fsmount),                 // 432
   //   (__NR_fspick,                  sys_fspick),                  // 433
   //   (__NR_pidfd_open,              sys_pidfd_open),              // 434
   GENX_(__NR_clone3,                  sys_ni_syscall),              // 435
   LINXY(__NR_close_range,             sys_close_range),             // 436
   //   (__NR_openat2,                 sys_openat2),                 // 437
   //   (__NR_pidfd_getfd,             sys_pidfd_getfd),             // 438
   LINX_(__NR_faccessat2,              sys_faccessat2),              // 439
   //   (__NR_process_madvise,         sys_process_madvise),         // 440
   //   (__NR_epoll_pwait2,            sys_epoll_pwait2),            // 441
   //   (__NR_mount_setattr,           sys_mount_setattr),           // 442
   //   (__NR_quotactl_fd,             sys_quotactl_fd),             // 443
   //   (__NR_landlock_create_ruleset, sys_landlock_create_ruleset), // 444
   //   (__NR_landlock_add_rule,       sys_landlock_add_rule),       // 445
   //   (__NR_landlock_restrict_self,  sys_landlock_restrict_self),  // 446
   //   (__NR_memfd_secret,            sys_memfd_secret),            // 447
   //   (__NR_process_mrelease,        sys_process_mrelease),        // 448
};

SyscallTableEntry* ML_(get_linux_syscall_entry) ( UInt sysno )
{
   const UInt syscall_main_table_size
      = sizeof(syscall_main_table) / sizeof(syscall_main_table[0]);

   /* Is it in the contiguous initial section of the table? */
   if (sysno < syscall_main_table_size) {
      SyscallTableEntry* sys = &syscall_main_table[sysno];
      if (sys->before == NULL)
         return NULL; /* no entry */
      else
         return sys;
   }

   /* Can't find a wrapper */
   return NULL;
}

#endif  /* defined(VGP_loongarch64_linux) */

/*--------------------------------------------------------------------*/
/*--- end                              syswrap-loongarch64-linux.c ---*/
/*--------------------------------------------------------------------*/
