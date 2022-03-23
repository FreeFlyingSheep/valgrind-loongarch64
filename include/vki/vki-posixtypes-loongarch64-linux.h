
/*--------------------------------------------------------------------*/
/*--- loongarch/Linux-specific kernel interface: posix types.      ---*/
/*---                           vki-posixtypes-loongarch64-linux.h ---*/
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

#ifndef __VKI_POSIXTYPES_LOONGARCH64_LINUX_H
#define __VKI_POSIXTYPES_LOONGARCH64_LINUX_H

//----------------------------------------------------------------------
// From linux-5.19-rc1/include/uapi/asm-generic/posix_types.h
//----------------------------------------------------------------------

typedef long                 __vki_kernel_long_t;
typedef unsigned long        __vki_kernel_ulong_t;
typedef __vki_kernel_ulong_t __vki_kernel_ino_t;
typedef unsigned int         __vki_kernel_mode_t;
typedef int                  __vki_kernel_pid_t;
typedef int                  __vki_kernel_ipc_pid_t;
typedef unsigned int         __vki_kernel_uid_t;
typedef unsigned int         __vki_kernel_gid_t;
typedef __vki_kernel_long_t  __vki_kernel_suseconds_t;
typedef int                  __vki_kernel_daddr_t;
typedef unsigned int         __vki_kernel_uid32_t;
typedef unsigned int         __vki_kernel_gid32_t;
typedef __vki_kernel_uid_t   __vki_kernel_old_uid_t;
typedef __vki_kernel_gid_t   __vki_kernel_old_gid_t;
typedef unsigned int         __vki_kernel_old_dev_t;

typedef __vki_kernel_ulong_t __vki_kernel_size_t;
typedef __vki_kernel_long_t  __vki_kernel_ssize_t;
typedef __vki_kernel_long_t  __vki_kernel_ptrdiff_t;

typedef struct {
   int val[2];
} __vki_kernel_fsid_t;

typedef __vki_kernel_long_t  __vki_kernel_off_t;
typedef long long            __vki_kernel_loff_t;
typedef __vki_kernel_long_t  __vki_kernel_old_time_t;
typedef __vki_kernel_long_t  __vki_kernel_time_t;
typedef long long            __vki_kernel_time64_t;
typedef __vki_kernel_long_t  __vki_kernel_clock_t;
typedef int                  __vki_kernel_timer_t;
typedef int                  __vki_kernel_clockid_t;
typedef char *               __vki_kernel_caddr_t;
typedef unsigned short       __vki_kernel_uid16_t;
typedef unsigned short       __vki_kernel_gid16_t;

#endif // __VKI_POSIXTYPES_LOONGARCH64_LINUX_H

/*--------------------------------------------------------------------*/
/*--- end                       vki-posixtypes-loongarch64-linux.h ---*/
/*--------------------------------------------------------------------*/
