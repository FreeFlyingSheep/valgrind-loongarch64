
/*---------------------------------------------------------------*/
/*--- begin                        libvex_guest_loongarch64.h ---*/
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

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __LIBVEX_PUB_GUEST_LOONGARCH64_H
#define __LIBVEX_PUB_GUEST_LOONGARCH64_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the LOONGARCH64 CPU state.      ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /* Event check fail addr and counter. */
      /*    0 */ ULong host_EvC_FAILADDR;
      /*    8 */ UInt  host_EvC_COUNTER;
      /*   12 */ UInt  _padding0;

      /* CPU Registers */
      /*   16 */ ULong guest_R0;   /* Constant zero */
      /*   24 */ ULong guest_R1;   /* Return address */
      /*   32 */ ULong guest_R2;   /* Thread pointer */
      /*   40 */ ULong guest_R3;   /* Stack pointer */
      /*   48 */ ULong guest_R4;   /* Argument registers / Return value */
      /*   56 */ ULong guest_R5;
      /*   64 */ ULong guest_R6;   /* Argument registers */
      /*   72 */ ULong guest_R7;
      /*   80 */ ULong guest_R8;
      /*   88 */ ULong guest_R9;
      /*   96 */ ULong guest_R10;
      /*  104 */ ULong guest_R11;
      /*  112 */ ULong guest_R12;  /* Temporary registers */
      /*  120 */ ULong guest_R13;
      /*  128 */ ULong guest_R14;
      /*  136 */ ULong guest_R15;
      /*  144 */ ULong guest_R16;
      /*  152 */ ULong guest_R17;
      /*  160 */ ULong guest_R18;
      /*  168 */ ULong guest_R19;
      /*  176 */ ULong guest_R20;
      /*  184 */ ULong guest_R21;  /* Reserved */
      /*  192 */ ULong guest_R22;  /* Frame pointer / Static register */
      /*  200 */ ULong guest_R23;  /* Static registers */
      /*  208 */ ULong guest_R24;
      /*  216 */ ULong guest_R25;
      /*  224 */ ULong guest_R26;
      /*  232 */ ULong guest_R27;
      /*  240 */ ULong guest_R28;
      /*  248 */ ULong guest_R29;
      /*  256 */ ULong guest_R30;
      /*  264 */ ULong guest_R31;

      /*  272 */ ULong guest_PC;   /* Program counter */

      /* FPU Registers */
      /*  280 */ ULong guest_F0;   /* Argument registers / Return value */
      /*  288 */ ULong guest_F1;
      /*  296 */ ULong guest_F2;   /* Argument registers */
      /*  304 */ ULong guest_F3;
      /*  312 */ ULong guest_F4;
      /*  320 */ ULong guest_F5;
      /*  328 */ ULong guest_F6;
      /*  336 */ ULong guest_F7;
      /*  344 */ ULong guest_F8;   /* Temporary registers */
      /*  352 */ ULong guest_F9;
      /*  360 */ ULong guest_F10;
      /*  368 */ ULong guest_F11;
      /*  376 */ ULong guest_F12;
      /*  384 */ ULong guest_F13;
      /*  392 */ ULong guest_F14;
      /*  400 */ ULong guest_F15;
      /*  408 */ ULong guest_F16;
      /*  416 */ ULong guest_F17;
      /*  424 */ ULong guest_F18;
      /*  432 */ ULong guest_F19;
      /*  440 */ ULong guest_F20;
      /*  448 */ ULong guest_F21;
      /*  456 */ ULong guest_F22;
      /*  464 */ ULong guest_F23;
      /*  472 */ ULong guest_F24;  /* Static registers */
      /*  480 */ ULong guest_F25;
      /*  488 */ ULong guest_F26;
      /*  496 */ ULong guest_F27;
      /*  504 */ ULong guest_F28;
      /*  512 */ ULong guest_F29;
      /*  520 */ ULong guest_F30;
      /*  528 */ ULong guest_F31;

      /*  536 */ UChar guest_FCC0;  /* Condition Flag Registers */
      /*  537 */ UChar guest_FCC1;
      /*  538 */ UChar guest_FCC2;
      /*  539 */ UChar guest_FCC3;
      /*  540 */ UChar guest_FCC4;
      /*  541 */ UChar guest_FCC5;
      /*  542 */ UChar guest_FCC6;
      /*  543 */ UChar guest_FCC7;
      /*  544 */ UInt  guest_FCSR;  /* FP Control and Status Register */

      /* Various pseudo-regs mandated by Vex or Valgrind. */
      /* Emulation notes */
      /*  548 */ UInt  guest_EMNOTE;

      /* For clflush: record start and length of area to invalidate */
      /*  552 */ ULong guest_CMSTART;
      /*  560 */ ULong guest_CMLEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      /*  568 */ ULong guest_NRADDR;

      /* Fallback LL/SC support. */
      /*  576 */ ULong guest_LLSC_SIZE; /* 0==no transaction, else 4 or 8. */
      /*  584 */ ULong guest_LLSC_ADDR; /* Address of the transaction. */
      /*  592 */ ULong guest_LLSC_DATA; /* Original value at ADDR. */

      /* VexGuestLOONGARCH64State should have a 16-aligned size */
      /*  600 */ ULong _padding1;
} VexGuestLOONGARCH64State;

/*---------------------------------------------------------------*/
/*--- Utility functions for LOONGARCH64 guest stuff.          ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT. */

/* Initialise all guest LOONGARCH64 state. */

extern
void LibVEX_GuestLOONGARCH64_initialise ( /*OUT*/
                                          VexGuestLOONGARCH64State* vex_state );

#endif /* ndef __LIBVEX_PUB_GUEST_LOONGARCH64_H */

/*---------------------------------------------------------------*/
/*---                              libvex_guest_loongarch64.h ---*/
/*---------------------------------------------------------------*/
