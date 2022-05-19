
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

      /*  272 */ ULong guest_PC;    /* Program counter */

      /*  280 */ UChar guest_FCC0;  /* Condition Flag Registers */
      /*  281 */ UChar guest_FCC1;
      /*  282 */ UChar guest_FCC2;
      /*  283 */ UChar guest_FCC3;
      /*  284 */ UChar guest_FCC4;
      /*  285 */ UChar guest_FCC5;
      /*  286 */ UChar guest_FCC6;
      /*  287 */ UChar guest_FCC7;
      /*  288 */ UInt  guest_FCSR;  /* FP/SIMD Control and Status Register */

      /* Various pseudo-regs mandated by Vex or Valgrind. */
      /* Emulation notes */
      /*  292 */ UInt  guest_EMNOTE;

      /* For clflush: record start and length of area to invalidate */
      /*  296 */ ULong guest_CMSTART;
      /*  304 */ ULong guest_CMLEN;

      /* Used to record the unredirected guest address at the start of
         a translation whose start has been redirected.  By reading
         this pseudo-register shortly afterwards, the translation can
         find out what the corresponding no-redirection address was.
         Note, this is only set for wrap-style redirects, not for
         replace-style ones. */
      /*  312 */ ULong guest_NRADDR;

      /* Fallback LL/SC support. */
      /*  320 */ ULong guest_LLSC_SIZE; /* 0==no transaction, else 4 or 8. */
      /*  328 */ ULong guest_LLSC_ADDR; /* Address of the transaction. */
      /*  336 */ ULong guest_LLSC_DATA; /* Original value at ADDR. */

      /*  344 */ ULong _padding1;

      /* FPU/SIMD Registers */
      /*  352 */ U256 guest_X0;
      /*  384 */ U256 guest_X1;
      /*  416 */ U256 guest_X2;
      /*  448 */ U256 guest_X3;
      /*  480 */ U256 guest_X4;
      /*  512 */ U256 guest_X5;
      /*  544 */ U256 guest_X6;
      /*  576 */ U256 guest_X7;
      /*  608 */ U256 guest_X8;
      /*  640 */ U256 guest_X9;
      /*  672 */ U256 guest_X10;
      /*  704 */ U256 guest_X11;
      /*  736 */ U256 guest_X12;
      /*  768 */ U256 guest_X13;
      /*  800 */ U256 guest_X14;
      /*  832 */ U256 guest_X15;
      /*  864 */ U256 guest_X16;
      /*  896 */ U256 guest_X17;
      /*  928 */ U256 guest_X18;
      /*  960 */ U256 guest_X19;
      /*  992 */ U256 guest_X20;
      /* 1024 */ U256 guest_X21;
      /* 1056 */ U256 guest_X22;
      /* 1088 */ U256 guest_X23;
      /* 1120 */ U256 guest_X24;
      /* 1152 */ U256 guest_X25;
      /* 1184 */ U256 guest_X26;
      /* 1216 */ U256 guest_X27;
      /* 1248 */ U256 guest_X28;
      /* 1280 */ U256 guest_X29;
      /* 1312 */ U256 guest_X30;
      /* 1244 */ U256 guest_X31;

      /* 1376 */ /* VexGuestLOONGARCH64State should have a 16-aligned size */
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
