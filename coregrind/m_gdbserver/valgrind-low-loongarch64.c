/* Low level interface to valgrind, for the remote server for GDB integrated
   in valgrind.
   Copyright (C) 2021
   Free Software Foundation, Inc.

   This file is part of VALGRIND.
   It has been inspired from files from gdbserver in gdb 13.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor,
   Boston, MA 02110-1301, USA.  */

#include "server.h"
#include "target.h"
#include "regdef.h"
#include "regcache.h"

#include "pub_core_machine.h"
#include "pub_core_debuginfo.h"
#include "pub_core_threadstate.h"
#include "pub_core_transtab.h"
#include "pub_core_gdbserver.h"

#include "valgrind_low.h"

#include "libvex_guest_loongarch64.h"

static struct reg regs[] = {
   { "r0",      0,    64 },
   { "r1",      64,   64 },
   { "r2",      128,  64 },
   { "r3",      192,  64 },
   { "r4",      256,  64 },
   { "r5",      320,  64 },
   { "r6",      384,  64 },
   { "r7",      448,  64 },
   { "r8",      512,  64 },
   { "r9",      576,  64 },
   { "r10",     640,  64 },
   { "r11",     704,  64 },
   { "r12",     768,  64 },
   { "r13",     832,  64 },
   { "r14",     896,  64 },
   { "r15",     960,  64 },
   { "r16",     1024, 64 },
   { "r17",     1088, 64 },
   { "r18",     1152, 64 },
   { "r19",     1216, 64 },
   { "r20",     1280, 64 },
   { "r21",     1344, 64 },
   { "r22",     1408, 64 },
   { "r23",     1472, 64 },
   { "r24",     1536, 64 },
   { "r25",     1600, 64 },
   { "r26",     1664, 64 },
   { "r27",     1728, 64 },
   { "r28",     1792, 64 },
   { "r29",     1856, 64 },
   { "r30",     1920, 64 },
   { "r31",     1984, 64 },
   { "orig_a0", 2048, 64 },
   { "pc",      2112, 64 },
   { "badv",    2176, 64 },
   { "f0",      2240, 64 },
   { "f1",      2304, 64 },
   { "f2",      2368, 64 },
   { "f3",      2432, 64 },
   { "f4",      2496, 64 },
   { "f5",      2560, 64 },
   { "f6",      2624, 64 },
   { "f7",      2688, 64 },
   { "f8",      2752, 64 },
   { "f9",      2816, 64 },
   { "f10",     2880, 64 },
   { "f11",     2944, 64 },
   { "f12",     3008, 64 },
   { "f13",     3072, 64 },
   { "f14",     3136, 64 },
   { "f15",     3200, 64 },
   { "f16",     3264, 64 },
   { "f17",     3328, 64 },
   { "f18",     3392, 64 },
   { "f19",     3456, 64 },
   { "f20",     3520, 64 },
   { "f21",     3584, 64 },
   { "f22",     3648, 64 },
   { "f23",     3712, 64 },
   { "f24",     3776, 64 },
   { "f25",     3840, 64 },
   { "f26",     3904, 64 },
   { "f27",     3968, 64 },
   { "f28",     4032, 64 },
   { "f29",     4096, 64 },
   { "f30",     4160, 64 },
   { "f31",     4224, 64 },
   { "fcc0",    4288, 8  },
   { "fcc1",    4296, 8  },
   { "fcc2",    4304, 8  },
   { "fcc3",    4312, 8  },
   { "fcc4",    4320, 8  },
   { "fcc5",    4328, 8  },
   { "fcc6",    4336, 8  },
   { "fcc7",    4344, 8  },
   { "fcsr",    4352, 32 }
};

#define num_regs (sizeof (regs) / sizeof (regs[0]))

static const char* expedite_regs[] = { "r3", "pc", NULL };

static
CORE_ADDR get_pc (void)
{
   unsigned long pc;

   collect_register_by_name ("pc", &pc);

   dlog(1, "stop pc is %p\n", (void*) pc);
   return pc;
}

static
void set_pc (CORE_ADDR newpc)
{
   supply_register_by_name ("pc", &newpc);
}

/* store registers in the guest state (gdbserver_to_valgrind)
   or fetch register from the guest state (valgrind_to_gdbserver). */
static
void transfer_register (ThreadId tid, int abs_regno, void* buf,
                        transfer_direction dir, int size, Bool* mod)
{
   ThreadState* tst = VG_(get_ThreadState)(tid);
   int set = abs_regno / num_regs;
   int regno = abs_regno % num_regs;
   *mod = False;

   VexGuestLOONGARCH64State* loongarch64 = (VexGuestLOONGARCH64State*) get_arch (set, tst);

   switch (regno) {
   // numbers here have to match the order of regs above
   // Attention: gdb order does not match valgrind order.
   case 0:  VG_(transfer) (&loongarch64->guest_R0,   buf, dir, size, mod); break;
   case 1:  VG_(transfer) (&loongarch64->guest_R1,   buf, dir, size, mod); break;
   case 2:  VG_(transfer) (&loongarch64->guest_R2,   buf, dir, size, mod); break;
   case 3:  VG_(transfer) (&loongarch64->guest_R3,   buf, dir, size, mod); break;
   case 4:  VG_(transfer) (&loongarch64->guest_R4,   buf, dir, size, mod); break;
   case 5:  VG_(transfer) (&loongarch64->guest_R5,   buf, dir, size, mod); break;
   case 6:  VG_(transfer) (&loongarch64->guest_R6,   buf, dir, size, mod); break;
   case 7:  VG_(transfer) (&loongarch64->guest_R7,   buf, dir, size, mod); break;
   case 8:  VG_(transfer) (&loongarch64->guest_R8,   buf, dir, size, mod); break;
   case 9:  VG_(transfer) (&loongarch64->guest_R9,   buf, dir, size, mod); break;
   case 10: VG_(transfer) (&loongarch64->guest_R10,  buf, dir, size, mod); break;
   case 11: VG_(transfer) (&loongarch64->guest_R11,  buf, dir, size, mod); break;
   case 12: VG_(transfer) (&loongarch64->guest_R12,  buf, dir, size, mod); break;
   case 13: VG_(transfer) (&loongarch64->guest_R13,  buf, dir, size, mod); break;
   case 14: VG_(transfer) (&loongarch64->guest_R14,  buf, dir, size, mod); break;
   case 15: VG_(transfer) (&loongarch64->guest_R15,  buf, dir, size, mod); break;
   case 16: VG_(transfer) (&loongarch64->guest_R16,  buf, dir, size, mod); break;
   case 17: VG_(transfer) (&loongarch64->guest_R17,  buf, dir, size, mod); break;
   case 18: VG_(transfer) (&loongarch64->guest_R18,  buf, dir, size, mod); break;
   case 19: VG_(transfer) (&loongarch64->guest_R19,  buf, dir, size, mod); break;
   case 20: VG_(transfer) (&loongarch64->guest_R20,  buf, dir, size, mod); break;
   case 21: VG_(transfer) (&loongarch64->guest_R21,  buf, dir, size, mod); break;
   case 22: VG_(transfer) (&loongarch64->guest_R22,  buf, dir, size, mod); break;
   case 23: VG_(transfer) (&loongarch64->guest_R23,  buf, dir, size, mod); break;
   case 24: VG_(transfer) (&loongarch64->guest_R24,  buf, dir, size, mod); break;
   case 25: VG_(transfer) (&loongarch64->guest_R25,  buf, dir, size, mod); break;
   case 26: VG_(transfer) (&loongarch64->guest_R26,  buf, dir, size, mod); break;
   case 27: VG_(transfer) (&loongarch64->guest_R27,  buf, dir, size, mod); break;
   case 28: VG_(transfer) (&loongarch64->guest_R28,  buf, dir, size, mod); break;
   case 29: VG_(transfer) (&loongarch64->guest_R29,  buf, dir, size, mod); break;
   case 30: VG_(transfer) (&loongarch64->guest_R30,  buf, dir, size, mod); break;
   case 31: VG_(transfer) (&loongarch64->guest_R31,  buf, dir, size, mod); break;
   case 32: *mod = False; break; // GDBTD?? arg0
   case 33: VG_(transfer) (&loongarch64->guest_PC,   buf, dir, size, mod); break;
   case 34: *mod = False; break; // GDBTD?? badvaddr
   case 35: VG_(transfer) (&loongarch64->guest_X0,   buf, dir, size, mod); break;
   case 36: VG_(transfer) (&loongarch64->guest_X1,   buf, dir, size, mod); break;
   case 37: VG_(transfer) (&loongarch64->guest_X2,   buf, dir, size, mod); break;
   case 38: VG_(transfer) (&loongarch64->guest_X3,   buf, dir, size, mod); break;
   case 39: VG_(transfer) (&loongarch64->guest_X4,   buf, dir, size, mod); break;
   case 40: VG_(transfer) (&loongarch64->guest_X5,   buf, dir, size, mod); break;
   case 41: VG_(transfer) (&loongarch64->guest_X6,   buf, dir, size, mod); break;
   case 42: VG_(transfer) (&loongarch64->guest_X7,   buf, dir, size, mod); break;
   case 43: VG_(transfer) (&loongarch64->guest_X8,   buf, dir, size, mod); break;
   case 44: VG_(transfer) (&loongarch64->guest_X9,   buf, dir, size, mod); break;
   case 45: VG_(transfer) (&loongarch64->guest_X10,  buf, dir, size, mod); break;
   case 46: VG_(transfer) (&loongarch64->guest_X11,  buf, dir, size, mod); break;
   case 47: VG_(transfer) (&loongarch64->guest_X12,  buf, dir, size, mod); break;
   case 48: VG_(transfer) (&loongarch64->guest_X13,  buf, dir, size, mod); break;
   case 49: VG_(transfer) (&loongarch64->guest_X14,  buf, dir, size, mod); break;
   case 50: VG_(transfer) (&loongarch64->guest_X15,  buf, dir, size, mod); break;
   case 51: VG_(transfer) (&loongarch64->guest_X16,  buf, dir, size, mod); break;
   case 52: VG_(transfer) (&loongarch64->guest_X17,  buf, dir, size, mod); break;
   case 53: VG_(transfer) (&loongarch64->guest_X18,  buf, dir, size, mod); break;
   case 54: VG_(transfer) (&loongarch64->guest_X19,  buf, dir, size, mod); break;
   case 55: VG_(transfer) (&loongarch64->guest_X20,  buf, dir, size, mod); break;
   case 56: VG_(transfer) (&loongarch64->guest_X21,  buf, dir, size, mod); break;
   case 57: VG_(transfer) (&loongarch64->guest_X22,  buf, dir, size, mod); break;
   case 58: VG_(transfer) (&loongarch64->guest_X23,  buf, dir, size, mod); break;
   case 59: VG_(transfer) (&loongarch64->guest_X24,  buf, dir, size, mod); break;
   case 60: VG_(transfer) (&loongarch64->guest_X25,  buf, dir, size, mod); break;
   case 61: VG_(transfer) (&loongarch64->guest_X26,  buf, dir, size, mod); break;
   case 62: VG_(transfer) (&loongarch64->guest_X27,  buf, dir, size, mod); break;
   case 63: VG_(transfer) (&loongarch64->guest_X28,  buf, dir, size, mod); break;
   case 64: VG_(transfer) (&loongarch64->guest_X29,  buf, dir, size, mod); break;
   case 65: VG_(transfer) (&loongarch64->guest_X30,  buf, dir, size, mod); break;
   case 66: VG_(transfer) (&loongarch64->guest_X31,  buf, dir, size, mod); break;
   case 67: VG_(transfer) (&loongarch64->guest_FCC0, buf, dir, size, mod); break;
   case 68: VG_(transfer) (&loongarch64->guest_FCC1, buf, dir, size, mod); break;
   case 69: VG_(transfer) (&loongarch64->guest_FCC2, buf, dir, size, mod); break;
   case 70: VG_(transfer) (&loongarch64->guest_FCC3, buf, dir, size, mod); break;
   case 71: VG_(transfer) (&loongarch64->guest_FCC4, buf, dir, size, mod); break;
   case 72: VG_(transfer) (&loongarch64->guest_FCC5, buf, dir, size, mod); break;
   case 73: VG_(transfer) (&loongarch64->guest_FCC6, buf, dir, size, mod); break;
   case 74: VG_(transfer) (&loongarch64->guest_FCC7, buf, dir, size, mod); break;
   case 75: VG_(transfer) (&loongarch64->guest_FCSR, buf, dir, size, mod); break;
   default: vg_assert(0);
   }
}

static
const char* target_xml (Bool shadow_mode)
{
   if (shadow_mode) {
      return "loongarch64-linux-valgrind.xml";
   } else {
      return "loongarch64-linux.xml";
   }
}

static CORE_ADDR** target_get_dtv (ThreadState* tst)
{
   VexGuestLOONGARCH64State* loongarch64 = (VexGuestLOONGARCH64State*)&tst->arch.vex;
   // Top of LoongArch tcbhead structure is located 0x0 bytes before the value
   // of $r2. Dtv is the first of two pointers in tcbhead structure.
   // More details can be found in GLIBC/sysdeps/nptl/tls.h.
   return (CORE_ADDR**)((CORE_ADDR)loongarch64->guest_R2
                        - 0x0 - 2 * sizeof(CORE_ADDR));
}

static struct valgrind_target_ops low_target = {
   num_regs,
   3, // SP
   regs,
   transfer_register,
   get_pc,
   set_pc,
   "loongarch64",
   target_xml,
   target_get_dtv
};

void loongarch64_init_architecture (struct valgrind_target_ops* target)
{
   *target = low_target;
   set_register_cache (regs, num_regs);
   gdbserver_expedite_regs = expedite_regs;
}
