
/*--------------------------------------------------------------------*/
/*--- loongarch/Linux-specific kernel interface.                   ---*/
/*---                                      vki-loongarch64-linux.h ---*/
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
*/

#ifndef __VKI_LOONGARCH64_LINUX_H
#define __VKI_LOONGARCH64_LINUX_H

// loongarch64 is little-endian.
#define VKI_LITTLE_ENDIAN 1

//----------------------------------------------------------------------
// From linux-5.15.2/include/uapi/asm-generic/int-ll64.h
//----------------------------------------------------------------------

typedef __signed__ char __vki_s8;
typedef unsigned char __vki_u8;

typedef __signed__ short __vki_s16;
typedef unsigned short __vki_u16;

typedef __signed__ int __vki_s32;
typedef unsigned int __vki_u32;

typedef __signed__ long long __vki_s64;
typedef unsigned long long __vki_u64;

//----------------------------------------------------------------------
// From linux-5.15.2/include/asm-generic/int-ll64.h
//----------------------------------------------------------------------

typedef __vki_s8  vki_s8;
typedef __vki_u8  vki_u8;
typedef __vki_s16 vki_s16;
typedef __vki_u16 vki_u16;
typedef __vki_s32 vki_s32;
typedef __vki_u32 vki_u32;
typedef __vki_s64 vki_s64;
typedef __vki_u64 vki_u64;

//----------------------------------------------------------------------
// From linux-5.15.2/include/linux/types.h
//----------------------------------------------------------------------

typedef vki_u8  vki_u_int8_t;
typedef vki_s8  vki_int8_t;
typedef vki_u16 vki_u_int16_t;
typedef vki_s16 vki_int16_t;
typedef vki_u32 vki_u_int32_t;
typedef vki_s32 vki_int32_t;

typedef vki_u8  vki_uint8_t;
typedef vki_u16 vki_uint16_t;
typedef vki_u32 vki_uint32_t;

typedef vki_u64 vki_uint64_t;
typedef vki_u64 vki_u_int64_t;
typedef vki_s64 vki_int64_t;

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/asm/page.h
//----------------------------------------------------------------------

/* loongarch64 uses runtime pagesize detection */
extern UWord VKI_PAGE_SHIFT;
extern UWord VKI_PAGE_SIZE;
#define VKI_PAGE_MASK      (~(PAGE_SIZE - 1))
#define VKI_MAX_PAGE_SHIFT 16
#define VKI_MAX_PAGE_SIZE  (1UL << VKI_MAX_PAGE_SHIFT)

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/asm/shmparam.h
//----------------------------------------------------------------------

#define VKI_SHMLBA 0x00010000 // SZ_64K

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/uapi/asm/signal.h
//----------------------------------------------------------------------

#define VKI_MINSIGSTKSZ 4096
#define VKI_SIGSTKSZ    16384

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/signal-defs.h
//----------------------------------------------------------------------

#define VKI_SA_NOCLDSTOP      0x00000001
#define VKI_SA_NOCLDWAIT      0x00000002
#define VKI_SA_SIGINFO        0x00000004
/* 0x00000008 used on alpha, mips, parisc */
/* 0x00000010 used on alpha, parisc */
/* 0x00000020 used on alpha, parisc, sparc */
/* 0x00000040 used on alpha, parisc */
/* 0x00000080 used on parisc */
/* 0x00000100 used on sparc */
/* 0x00000200 used on sparc */
#define VKI_SA_UNSUPPORTED    0x00000400
#define VKI_SA_EXPOSE_TAGBITS 0x00000800
/* 0x00010000 used on mips */
/* 0x00800000 used for internal SA_IMMUTABLE */
/* 0x01000000 used on x86 */
/* 0x02000000 used on x86 */
/*
 * New architectures should not define the obsolete
 *      VKI_SA_RESTORER       0x04000000
 */
#define VKI_SA_ONSTACK        0x08000000
#define VKI_SA_RESTART        0x10000000
#define VKI_SA_NODEFER        0x40000000
#define VKI_SA_RESETHAND      0x80000000

#define VKI_SA_NOMASK         VKI_SA_NODEFER
#define VKI_SA_ONESHOT        VKI_SA_RESETHAND

#define VKI_SIG_BLOCK     0 /* for blocking signals */
#define VKI_SIG_UNBLOCK   1 /* for unblocking signals */
#define VKI_SIG_SETMASK   2 /* for setting the signal mask */

typedef void __vki_signalfn_t(int);
typedef __vki_signalfn_t __user *__vki_sighandler_t;

typedef void __vki_restorefn_t(void);
typedef __vki_restorefn_t __user *__vki_igrestore_t;

#define VKI_SIG_DFL ((__vki_sighandler_t)0)  /* default signal handling */
#define VKI_SIG_IGN ((__vki_sighandler_t)1)  /* ignore signal */
#define VKI_SIG_ERR ((__vki_sighandler_t)-1) /* error return from signal */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/signal.h
//----------------------------------------------------------------------

#define _VKI_NSIG       64
#define _VKI_NSIG_BPW   64 // __BITS_PER_LONG == 64
#define _VKI_NSIG_WORDS (_VKI_NSIG / _VKI_NSIG_BPW)

#define VKI_SIGHUP     1
#define VKI_SIGINT     2
#define VKI_SIGQUIT    3
#define VKI_SIGILL     4
#define VKI_SIGTRAP    5
#define VKI_SIGABRT    6
#define VKI_SIGIOT     6
#define VKI_SIGBUS     7
#define VKI_SIGFPE     8
#define VKI_SIGKILL    9
#define VKI_SIGUSR1   10
#define VKI_SIGSEGV   11
#define VKI_SIGUSR2   12
#define VKI_SIGPIPE   13
#define VKI_SIGALRM   14
#define VKI_SIGTERM   15
#define VKI_SIGSTKFLT 16
#define VKI_SIGCHLD   17
#define VKI_SIGCONT   18
#define VKI_SIGSTOP   19
#define VKI_SIGTSTP   20
#define VKI_SIGTTIN   21
#define VKI_SIGTTOU   22
#define VKI_SIGURG    23
#define VKI_SIGXCPU   24
#define VKI_SIGXFSZ   25
#define VKI_SIGVTALRM 26
#define VKI_SIGPROF   27
#define VKI_SIGWINCH  28
#define VKI_SIGIO     29
#define VKI_SIGPOLL   VKI_SIGIO
/*
#define VKI_SIGLOST   29
*/
#define VKI_SIGPWR    30
#define VKI_SIGSYS    31
#define VKI_SIGUNUSED 31

#define VKI_SIGRTMIN  32
#define VKI_SIGRTMAX  _VKI_NSIG

typedef struct {
   unsigned long sig[_VKI_NSIG_WORDS];
} vki_sigset_t;

typedef unsigned long vki_old_sigset_t;

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/linux/signal.h
//----------------------------------------------------------------------

#define VKI_SS_ONSTACK 1
#define VKI_SS_DISABLE 2

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/linux/signal_types.h
//----------------------------------------------------------------------

struct vki_sigaction {
   __vki_sighandler_t sa_handler;
	unsigned long      sa_flags;
	vki_sigset_t       sa_mask; /* mask last for extensibility */
};

struct vki_sigaction_base {
   // [[Nb: a 'k' prefix is added to "sa_handler" because
   // bits/sigaction.h (which gets dragged in somehow via signal.h)
   // #defines it as something else.  Since that is done for glibc's
   // purposes, which we don't care about here, we use our own name.]]
   __vki_sighandler_t ksa_handler;
   unsigned long      sa_flags;
   vki_sigset_t       sa_mask; /* mask last for extensibility */
};

/* On Linux we use the same type for passing sigactions to
   and from the kernel.  Hence: */
typedef struct vki_sigaction_base vki_sigaction_toK_t;
typedef struct vki_sigaction_base vki_sigaction_fromK_t;

typedef struct vki_sigaltstack {
   void __user         *ss_sp;
   int                 ss_flags;
   __vki_kernel_size_t ss_size;
} vki_stack_t;

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/uapi/asm/sigcontext.h
//----------------------------------------------------------------------

struct vki_sigcontext {
   __vki_u64 sc_pc;
   __vki_u64 sc_regs[32];
   __vki_u32 sc_flags;
   __vki_u64 sc_extcontext[0] __attribute__((__aligned__(16)));
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/linux/mman.h
//----------------------------------------------------------------------

#define VKI_MAP_SHARED          0x01       /* Share changes */
#define VKI_MAP_PRIVATE         0x02       /* Changes are private */
#define VKI_MAP_SHARED_VALIDATE 0x03       /* share + validate extension flags */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/mman-common.h
//----------------------------------------------------------------------

#define VKI_PROT_READ  0x1                 /* page can be read */
#define VKI_PROT_WRITE 0x2                 /* page can be written */
#define VKI_PROT_EXEC  0x4                 /* page can be executed */
#define VKI_PROT_SEM   0x8                 /* page may be used for atomic ops */
/*                              0x10          reserved for arch-specific use */
/*                              0x20          reserved for arch-specific use */
#define VKI_PROT_NONE           0x0        /* page can not be accessed */
#define VKI_PROT_GROWSDOWN      0x01000000 /* mprotect flag: extend change to start of growsdown vma */
#define VKI_PROT_GROWSUP        0x02000000 /* mprotect flag: extend change to end of growsup vma */

/* 0x01 - 0x03 are defined in linux/mman.h */
#define VKI_MAP_TYPE            0x0f       /* Mask for type of mapping */
#define VKI_MAP_FIXED           0x10       /* Interpret addr exactly */
#define VKI_MAP_ANONYMOUS       0x20       /* don't use a file */

/* 0x0100 - 0x4000 flags are defined in asm-generic/mman.h */
#define VKI_MAP_POPULATE        0x008000   /* populate (prefault) pagetables */
#define VKI_MAP_NONBLOCK        0x010000   /* do not block on IO */
#define VKI_MAP_STACK           0x020000   /* give out an address that is best suited for process/thread stacks */
#define VKI_MAP_HUGETLB         0x040000   /* create a huge page mapping */
#define VKI_MAP_SYNC            0x080000   /* perform synchronous page faults for the mapping */
#define VKI_MAP_FIXED_NOREPLACE 0x100000   /* MAP_FIXED which doesn't unmap underlying mapping */

#define VKI_MAP_UNINITIALIZED   0x4000000  /* For anonymous mmap, memory could be uninitialized */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/fcntl.h
//----------------------------------------------------------------------

#define VKI_O_ACCMODE    00000003
#define VKI_O_RDONLY     00000000
#define VKI_O_WRONLY     00000001
#define VKI_O_RDWR       00000002
#define VKI_O_CREAT      00000100 /* not fcntl */
#define VKI_O_EXCL       00000200 /* not fcntl */
#define VKI_O_NOCTTY     00000400 /* not fcntl */
#define VKI_O_TRUNC      00001000 /* not fcntl */
#define VKI_O_APPEND     00002000
#define VKI_O_NONBLOCK   00004000
#define VKI_O_DSYNC      00010000 /* used to be O_SYNC, see below */
#define VKI_FASYNC       00020000 /* fcntl, for BSD compatibility */
#define VKI_O_DIRECT     00040000 /* direct disk access hint */
#define VKI_O_LARGEFILE  00100000
#define VKI_O_DIRECTORY  00200000 /* must be a directory */
#define VKI_O_NOFOLLOW   00400000 /* don't follow links */
#define VKI_O_NOATIME    01000000
#define VKI_O_CLOEXEC    02000000 /* set close_on_exec */

#define __VKI_O_SYNC     04000000
#define VKI_O_SYNC       (__VKI_O_SYNC|VKI_O_DSYNC)

#define VKI_O_PATH       010000000

#define __VKI_O_TMPFILE  020000000

#define VKI_O_TMPFILE      (__VKI_O_TMPFILE | VKI_O_DIRECTORY)
#define VKI_O_TMPFILE_MASK (__VKI_O_TMPFILE | VKI_O_DIRECTORY | VKI_O_CREAT)

#define VKI_O_NDELAY     VKI_O_NONBLOCK

#define VKI_F_DUPFD         0  /* dup */
#define VKI_F_GETFD         1  /* get close_on_exec */
#define VKI_F_SETFD         2  /* set/clear close_on_exec */
#define VKI_F_GETFL         3  /* get file->f_flags */
#define VKI_F_SETFL         4  /* set file->f_flags */
#define VKI_F_GETLK         5
#define VKI_F_SETLK         6
#define VKI_F_SETLKW        7
#define VKI_F_SETOWN        8  /* for sockets. */
#define VKI_F_GETOWN        9  /* for sockets. */
#define VKI_F_SETSIG        10 /* for sockets. */
#define VKI_F_GETSIG        11 /* for sockets. */

#define VKI_F_SETOWN_EX     15
#define VKI_F_GETOWN_EX     16

#define VKI_F_GETOWNER_UIDS 17

#define VKI_F_OFD_GETLK     36
#define VKI_F_OFD_SETLK     37
#define VKI_F_OFD_SETLKW    38

#define VKI_F_OWNER_TID     0
#define VKI_F_OWNER_PID     1
#define VKI_F_OWNER_PGRP    2

struct vki_f_owner_ex {
   int   type;
   __vki_kernel_pid_t   pid;
};

#define VKI_FD_CLOEXEC 1  /* actually anything with low bit set goes */

#define VKI_F_RDLCK    0
#define VKI_F_WRLCK    1
#define VKI_F_UNLCK    2

#define VKI_F_EXLCK    4   /* or 3 */
#define VKI_F_SHLCK    8   /* or 4 */

#define VKI_LOCK_SH    1   /* shared lock */
#define VKI_LOCK_EX    2   /* exclusive lock */
#define VKI_LOCK_NB    4   /* or'd with one of the above to prevent blocking */
#define VKI_LOCK_UN    8   /* remove lock */

#define VKI_LOCK_MAND  32  /* This is a mandatory flock ... */
#define VKI_LOCK_READ  64  /* which allows concurrent read operations */
#define VKI_LOCK_WRITE 128 /* which allows concurrent write operations */
#define VKI_LOCK_RW    192 /* which allows concurrent read & write ops */

#define VKI_F_LINUX_SPECIFIC_BASE 1024

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/linux/fcntl.h
//----------------------------------------------------------------------

#define VKI_AT_FDCWD -100 /* Special value used to indicate
                             openat should use the current
                             working directory. */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/resource.h
//----------------------------------------------------------------------

#define VKI_RLIMIT_DATA   2 /* max data size */
#define VKI_RLIMIT_STACK  3 /* max stack size */
#define VKI_RLIMIT_CORE   4 /* max core file size */
#define VKI_RLIMIT_NOFILE 7 /* max number of open files */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/socket.h
//----------------------------------------------------------------------

#define VKI_SOL_SOCKET 1
#define VKI_SO_TYPE    3

#define VKI_SO_ATTACH_FILTER 26

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/sockios.h
//----------------------------------------------------------------------

#define VKI_FIOSETOWN        0x8901
#define VKI_SIOCSPGRP        0x8902
#define VKI_FIOGETOWN        0x8903
#define VKI_SIOCGPGRP        0x8904
#define VKI_SIOCATMARK       0x8905
#define VKI_SIOCGSTAMP_OLD   0x8906 /* Get stamp (timeval) */
#define VKI_SIOCGSTAMPNS_OLD 0x8907 /* Get stamp (timespec) */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/linux/sockios.h
//----------------------------------------------------------------------

#define VKI_SIOCGSTAMP       VKI_SIOCGSTAMP_OLD
#define VKI_SIOCGSTAMPNS     VKI_SIOCGSTAMPNS_OLD

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/stat.h
//----------------------------------------------------------------------

struct vki_stat {
   unsigned long st_dev;        /* Device.  */
   unsigned long st_ino;        /* File serial number.  */
   unsigned int  st_mode;       /* File mode.  */
   unsigned int  st_nlink;      /* Link count.  */
   unsigned int  st_uid;        /* User ID of the file's owner.  */
   unsigned int  st_gid;        /* Group ID of the file's group. */
   unsigned long st_rdev;       /* Device number, if device.  */
   unsigned long __pad1;
   long          st_size;       /* Size of file, in bytes.  */
   int           st_blksize;    /* Optimal block size for I/O.  */
   int           __pad2;
   long          st_blocks;     /* Number 512-byte blocks allocated. */
   long          st_atime;      /* Time of last access.  */
   unsigned long st_atime_nsec;
   long          st_mtime;      /* Time of last modification.  */
   unsigned long st_mtime_nsec;
   long          st_ctime;      /* Time of last status change.  */
   unsigned long st_ctime_nsec;
   unsigned int  __unused4;
   unsigned int  __unused5;
};

struct vki_stat64 {
   unsigned long long st_dev;        /* Device.  */
   unsigned long long st_ino;        /* File serial number.  */
   unsigned int       st_mode;       /* File mode.  */
   unsigned int       st_nlink;      /* Link count.  */
   unsigned int       st_uid;        /* User ID of the file's owner.  */
   unsigned int       st_gid;        /* Group ID of the file's group. */
   unsigned long long st_rdev;       /* Device number, if device.  */
   unsigned long long __pad1;
   long long          st_size;       /* Size of file, in bytes.  */
   int                st_blksize;    /* Optimal block size for I/O.  */
   int                __pad2;
   long long          st_blocks;     /* Number 512-byte blocks allocated. */
   int                st_atime;      /* Time of last access.  */
   unsigned int       st_atime_nsec;
   int                st_mtime;      /* Time of last modification.  */
   unsigned int       st_mtime_nsec;
   int                st_ctime;      /* Time of last status change.  */
   unsigned int       st_ctime_nsec;
   unsigned int       __unused4;
   unsigned int       __unused5;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/statfs.h
//----------------------------------------------------------------------

#define __vki_statfs_word __vki_kernel_long_t

struct vki_statfs {
   __vki_statfs_word f_type;
   __vki_statfs_word f_bsize;
   __vki_statfs_word f_blocks;
   __vki_statfs_word f_bfree;
   __vki_statfs_word f_bavail;
   __vki_statfs_word f_files;
   __vki_statfs_word f_ffree;
   __vki_kernel_fsid_t f_fsid;
   __vki_statfs_word f_namelen;
   __vki_statfs_word f_frsize;
   __vki_statfs_word f_flags;
   __vki_statfs_word f_spare[4];
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/termios.h
//----------------------------------------------------------------------

struct vki_winsize {
   unsigned short ws_row;
   unsigned short ws_col;
   unsigned short ws_xpixel;
   unsigned short ws_ypixel;
};

#define VKI_NCC 8
struct vki_termio {
   unsigned short c_iflag;       /* input mode flags */
   unsigned short c_oflag;       /* output mode flags */
   unsigned short c_cflag;       /* control mode flags */
   unsigned short c_lflag;       /* local mode flags */
   unsigned char  c_line;        /* line discipline */
   unsigned char  c_cc[VKI_NCC]; /* control characters */
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/termbits.h
//----------------------------------------------------------------------

typedef unsigned char vki_cc_t;
typedef unsigned int  vki_speed_t;
typedef unsigned int  vki_tcflag_t;

#define VKI_NCCS 19
struct vki_termios {
   vki_tcflag_t c_iflag;     /* input mode flags */
   vki_tcflag_t c_oflag;     /* output mode flags */
   vki_tcflag_t c_cflag;     /* control mode flags */
   vki_tcflag_t c_lflag;     /* local mode flags */
   vki_cc_t c_line;          /* line discipline */
   vki_cc_t c_cc[VKI_NCCS];  /* control characters */
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/ioctl.h
//----------------------------------------------------------------------

#define _VKI_IOC_NRBITS    8
#define _VKI_IOC_TYPEBITS  8
#define _VKI_IOC_SIZEBITS  14
#define _VKI_IOC_DIRBITS   2

#define _VKI_IOC_NRMASK    ((1 << _VKI_IOC_NRBITS)-1)
#define _VKI_IOC_TYPEMASK  ((1 << _VKI_IOC_TYPEBITS)-1)
#define _VKI_IOC_SIZEMASK  ((1 << _VKI_IOC_SIZEBITS)-1)
#define _VKI_IOC_DIRMASK   ((1 << _VKI_IOC_DIRBITS)-1)

#define _VKI_IOC_NRSHIFT   0
#define _VKI_IOC_TYPESHIFT (_VKI_IOC_NRSHIFT+_VKI_IOC_NRBITS)
#define _VKI_IOC_SIZESHIFT (_VKI_IOC_TYPESHIFT+_VKI_IOC_TYPEBITS)
#define _VKI_IOC_DIRSHIFT  (_VKI_IOC_SIZESHIFT+_VKI_IOC_SIZEBITS)

#define _VKI_IOC_NONE      0U
#define _VKI_IOC_WRITE     1U
#define _VKI_IOC_READ      2U

#define _VKI_IOC(dir,type,nr,size) \
        (((dir)  << _VKI_IOC_DIRSHIFT) | \
         ((type) << _VKI_IOC_TYPESHIFT) | \
         ((nr)   << _VKI_IOC_NRSHIFT) | \
         ((size) << _VKI_IOC_SIZESHIFT))

#define _VKI_IO(type,nr)            _VKI_IOC(_VKI_IOC_NONE,(type),(nr),0)
#define _VKI_IOR(type,nr,size)      _VKI_IOC(_VKI_IOC_READ,(type),(nr),(_VKI_IOC_TYPECHECK(size)))
#define _VKI_IOW(type,nr,size)      _VKI_IOC(_VKI_IOC_WRITE,(type),(nr),(_VKI_IOC_TYPECHECK(size)))
#define _VKI_IOWR(type,nr,size)     _VKI_IOC(_VKI_IOC_READ|_VKI_IOC_WRITE,(type),(nr),(_VKI_IOC_TYPECHECK(size)))
#define _VKI_IOR_BAD(type,nr,size)  _VKI_IOC(_VKI_IOC_READ,(type),(nr),sizeof(size))
#define _VKI_IOW_BAD(type,nr,size)  _VKI_IOC(_VKI_IOC_WRITE,(type),(nr),sizeof(size))
#define _VKI_IOWR_BAD(type,nr,size) _VKI_IOC(_VKI_IOC_READ|_VKI_IOC_WRITE,(type),(nr),sizeof(size))

#define _VKI_IOC_DIR(nr)  (((nr) >> _VKI_IOC_DIRSHIFT) & _VKI_IOC_DIRMASK)
#define _VKI_IOC_TYPE(nr) (((nr) >> _VKI_IOC_TYPESHIFT) & _VKI_IOC_TYPEMASK)
#define _VKI_IOC_NR(nr)   (((nr) >> _VKI_IOC_NRSHIFT) & _VKI_IOC_NRMASK)
#define _VKI_IOC_SIZE(nr) (((nr) >> _VKI_IOC_SIZESHIFT) & _VKI_IOC_SIZEMASK)

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/ioctls.h
//----------------------------------------------------------------------

#define VKI_TCGETS       0x5401
#define VKI_TCSETS       0x5402
#define VKI_TCSETSW      0x5403
#define VKI_TCSETSF      0x5404
#define VKI_TCGETA       0x5405
#define VKI_TCSETA       0x5406
#define VKI_TCSETAW      0x5407
#define VKI_TCSETAF      0x5408
#define VKI_TCSBRK       0x5409
#define VKI_TCXONC       0x540A
#define VKI_TCFLSH       0x540B
#define VKI_TIOCEXCL     0x540C
#define VKI_TIOCNXCL     0x540D
#define VKI_TIOCSCTTY    0x540E
#define VKI_TIOCGPGRP    0x540F
#define VKI_TIOCSPGRP    0x5410
#define VKI_TIOCOUTQ     0x5411
#define VKI_TIOCSTI      0x5412
#define VKI_TIOCGWINSZ   0x5413
#define VKI_TIOCSWINSZ   0x5414
#define VKI_TIOCMGET     0x5415
#define VKI_TIOCMBIS     0x5416
#define VKI_TIOCMBIC     0x5417
#define VKI_TIOCMSET     0x5418
#define VKI_TIOCGSOFTCAR 0x5419
#define VKI_TIOCSSOFTCAR 0x541A
#define VKI_FIONREAD     0x541B
#define VKI_TIOCINQ      VKI_FIONREAD
#define VKI_TIOCLINUX    0x541C
#define VKI_TIOCCONS     0x541D
#define VKI_TIOCGSERIAL  0x541E
#define VKI_TIOCSSERIAL  0x541F
#define VKI_TIOCPKT      0x5420
#define VKI_FIONBIO      0x5421
#define VKI_TIOCNOTTY    0x5422
#define VKI_TIOCSETD     0x5423
#define VKI_TIOCGETD     0x5424
#define VKI_TCSBRKP      0x5425   /* Needed for POSIX tcsendbreak() */
#define VKI_TIOCSBRK     0x5427   /* BSD compatibility */
#define VKI_TIOCCBRK     0x5428   /* BSD compatibility */
#define VKI_TIOCGSID     0x5429   /* Return the session ID of FD */
#define VKI_TCGETS2      _VKI_IOR('T', 0x2A, struct termios2)
#define VKI_TCSETS2      _VKI_IOW('T', 0x2B, struct termios2)
#define VKI_TCSETSW2     _VKI_IOW('T', 0x2C, struct termios2)
#define VKI_TCSETSF2     _VKI_IOW('T', 0x2D, struct termios2)
#define VKI_TIOCGRS485   0x542E
#define VKI_TIOCSRS485   0x542F
#define VKI_TIOCGPTN     _VKI_IOR('T', 0x30, unsigned int) /* Get Pty Number (of pty-mux device) */
#define VKI_TIOCSPTLCK   _VKI_IOW('T', 0x31, int) /* Lock/unlock Pty */
#define VKI_TIOCGDEV     _VKI_IOR('T', 0x32, unsigned int) /* Get primary device node of /dev/console */
#define VKI_TCGETX       0x5432 /* SYS5 TCGETX compatibility */
#define VKI_TCSETX       0x5433
#define VKI_TCSETXF      0x5434
#define VKI_TCSETXW      0x5435
#define VKI_TIOCSIG      _VKI_IOW('T', 0x36, int) /* pty: generate signal */
#define VKI_TIOCVHANGUP  0x5437
#define VKI_TIOCGPKT     _VKI_IOR('T', 0x38, int) /* Get packet mode state */
#define VKI_TIOCGPTLCK   _VKI_IOR('T', 0x39, int) /* Get Pty lock state */
#define VKI_TIOCGEXCL    _VKI_IOR('T', 0x40, int) /* Get exclusive mode state */
#define VKI_TIOCGPTPEER  _VKI_IO('T', 0x41)       /* Safely open the slave */
#define VKI_TIOCGISO7816 _VKI_IOR('T', 0x42, struct serial_iso7816)
#define VKI_TIOCSISO7816 _VKI_IOWR('T', 0x43, struct serial_iso7816)

#define VKI_FIONCLEX        0x5450
#define VKI_FIOCLEX         0x5451
#define VKI_FIOASYNC        0x5452
#define VKI_TIOCSERCONFIG   0x5453
#define VKI_TIOCSERGWILD    0x5454
#define VKI_TIOCSERSWILD    0x5455
#define VKI_TIOCGLCKTRMIOS  0x5456
#define VKI_TIOCSLCKTRMIOS  0x5457
#define VKI_TIOCSERGSTRUCT  0x5458 /* For debugging only */
#define VKI_TIOCSERGETLSR   0x5459 /* Get line status register */
#define VKI_TIOCSERGETMULTI 0x545A /* Get multiport config  */
#define VKI_TIOCSERSETMULTI 0x545B /* Set multiport config */

#define VKI_TIOCMIWAIT      0x545C /* wait for a change on serial input line(s) */
#define VKI_TIOCGICOUNT     0x545D /* read serial port inline interrupt counts */

#define VKI_FIOQSIZE        0x5460

#define VKI_TIOCPKT_DATA       0
#define VKI_TIOCPKT_FLUSHREAD  1
#define VKI_TIOCPKT_FLUSHWRITE 2
#define VKI_TIOCPKT_STOP       4
#define VKI_TIOCPKT_START      8
#define VKI_TIOCPKT_NOSTOP    16
#define VKI_TIOCPKT_DOSTOP    32
#define VKI_TIOCPKT_IOCTL     64

#define VKI_TIOCSER_TEMT 0x01 /* Transmitter physically empty */

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/poll.h
//----------------------------------------------------------------------

#define VKI_POLLIN 0x0001

struct vki_pollfd {
   int   fd;
   short events;
   short revents;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/asm/elf.h
//----------------------------------------------------------------------

#define VKI_ELF_NGREG  45
#define VKI_ELF_NFPREG 34

typedef unsigned long vki_elf_greg_t;
typedef vki_elf_greg_t vki_elf_gregset_t[VKI_ELF_NGREG];

typedef double vki_elf_fpreg_t;
typedef vki_elf_fpreg_t vki_elf_fpregset_t[VKI_ELF_NFPREG];

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/uapi/asm/ucontext.h
//----------------------------------------------------------------------

struct vki_ucontext {
   unsigned long         uc_flags;
   struct vki_ucontext   *uc_link;
   vki_stack_t           uc_stack;
   vki_sigset_t          uc_sigmask;
   __vki_u8              __unused[1024 / 8 - sizeof(vki_sigset_t)];
   struct vki_sigcontext uc_mcontext;
};

typedef char vki_modify_ldt_t;



//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/ipcbuf.h
//----------------------------------------------------------------------

struct vki_ipc64_perm {
   __vki_kernel_key_t   key;
   __vki_kernel_uid32_t uid;
   __vki_kernel_gid32_t gid;
   __vki_kernel_uid32_t cuid;
   __vki_kernel_gid32_t cgid;
   __vki_kernel_mode_t  mode;
   unsigned char        __pad1[4 - sizeof(__vki_kernel_mode_t)]; /* pad if mode_t is u16: */
   unsigned short       seq;
   unsigned short       __pad2;
   __vki_kernel_ulong_t __unused1;
   __vki_kernel_ulong_t __unused2;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/sembuf.h
//----------------------------------------------------------------------
struct vki_semid64_ds {
   struct vki_ipc64_perm sem_perm;  /* permissions .. see ipc.h */
   long                  sem_otime; /* last semop time */
   long                  sem_ctime; /* last change time */
   unsigned long         sem_nsems; /* no. of semaphores in array */
   unsigned long         __unused3;
   unsigned long         __unused4;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/msgbuf.h
//----------------------------------------------------------------------

struct vki_msqid64_ds {
   struct vki_ipc64_perm msg_perm;
   long                  msg_stime;  /* last msgsnd time */
   long                  msg_rtime;  /* last msgrcv time */
   long                  msg_ctime;  /* last change time */
   unsigned long         msg_cbytes; /* current number of bytes on queue */
   unsigned long         msg_qnum;   /* number of messages in queue */
   unsigned long         msg_qbytes; /* max number of bytes on queue */
   __vki_kernel_pid_t    msg_lspid;  /* pid of last msgsnd */
   __vki_kernel_pid_t    msg_lrpid;  /* last receive pid */
   unsigned long         __unused4;
   unsigned long         __unused5;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/shmbuf.h
//----------------------------------------------------------------------

struct vki_shmid64_ds {
   struct vki_ipc64_perm shm_perm;   /* operation perms */
   vki_size_t            shm_segsz;  /* size of segment (bytes) */
   long                  shm_atime;  /* last attach time */
   long                  shm_dtime;  /* last detach time */
   long                  shm_ctime;  /* last change time */
   __vki_kernel_pid_t    shm_cpid;   /* pid of creator */
   __vki_kernel_pid_t    shm_lpid;   /* pid of last operator */
   unsigned long         shm_nattch; /* no. of current attaches */
   unsigned long         __unused4;
   unsigned long         __unused5;
};

struct vki_shminfo64 {
   unsigned long shmmax;
   unsigned long shmmin;
   unsigned long shmmni;
   unsigned long shmseg;
   unsigned long shmall;
   unsigned long __unused1;
   unsigned long __unused2;
   unsigned long __unused3;
   unsigned long __unused4;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/arch/loongarch/include/uapi/asm/ptrace.h
//----------------------------------------------------------------------

struct vki_user_pt_regs {
   /* Saved main processor registers. */
   unsigned long regs[32];

   /* Original syscall arg0. */
   unsigned long orig_a0;

   /* Saved special registers. */
   unsigned long csr_era;
   unsigned long csr_badv;
   unsigned long reserved[10];
} __attribute__((aligned(8)));

#define vki_user_regs_struct vki_user_pt_regs

struct vki_user_fp_state {
   vki_uint64_t fpr[32];
   vki_uint64_t fcc;
   vki_uint32_t fcsr;
};

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/errno.h
//----------------------------------------------------------------------

#define VKI_ENOSYS     38 /* Invalid system call number */
#define VKI_EOVERFLOW  75 /* Value too large for defined data type */

#endif // __VKI_LOONGARCH64_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                                  vki-loongarch64-linux.h ---*/
/*--------------------------------------------------------------------*/
