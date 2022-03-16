
/*---------------------------------------------------------------*/
/*--- begin                       guest_loongarch64_helpers.c ---*/
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
*/

#include "libvex_basictypes.h"
#include "libvex_emnote.h"
#include "libvex_guest_loongarch64.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_loongarch64_defs.h"


/* This file contains helper functions for loongarch64 guest code.
   Calls to these functions are generated by the back end. */

IRExpr* guest_loongarch64_spechelper ( const HChar * function_name,
                                       IRExpr ** args,
                                       IRStmt ** precedingStmts,
                                       Int n_precedingStmts )
{
   return NULL;
}

/* VISIBLE TO LIBVEX CLIENT */
void LibVEX_GuestLOONGARCH64_initialise ( /*OUT*/
                                          VexGuestLOONGARCH64State* vex_state )
{
   /* Event check fail addr and counter. */
   vex_state->host_EvC_FAILADDR = 0;
   vex_state->host_EvC_COUNTER  = 0;

   /* CPU Registers */
   vex_state->guest_R0   = 0; /* Constant zero */
   vex_state->guest_R1   = 0; /* Return address */
   vex_state->guest_R2   = 0; /* Thread pointer */
   vex_state->guest_R3   = 0; /* Stack pointer */
   vex_state->guest_R4   = 0; /* Argument registers / Return value */
   vex_state->guest_R5   = 0;
   vex_state->guest_R6   = 0; /* Argument registers */
   vex_state->guest_R7   = 0;
   vex_state->guest_R8   = 0;
   vex_state->guest_R9   = 0;
   vex_state->guest_R10  = 0;
   vex_state->guest_R11  = 0;
   vex_state->guest_R12  = 0; /* Temporary registers */
   vex_state->guest_R13  = 0;
   vex_state->guest_R14  = 0;
   vex_state->guest_R15  = 0;
   vex_state->guest_R16  = 0;
   vex_state->guest_R17  = 0;
   vex_state->guest_R18  = 0;
   vex_state->guest_R19  = 0;
   vex_state->guest_R20  = 0;
   vex_state->guest_R21  = 0; /* Reserved */
   vex_state->guest_R22  = 0; /* Frame pointer / Static register */
   vex_state->guest_R23  = 0; /* Static registers */
   vex_state->guest_R24  = 0;
   vex_state->guest_R25  = 0;
   vex_state->guest_R26  = 0;
   vex_state->guest_R27  = 0;
   vex_state->guest_R28  = 0;
   vex_state->guest_R29  = 0;
   vex_state->guest_R30  = 0;
   vex_state->guest_R31  = 0;

   vex_state->guest_PC   = 0; /* Program counter */

   /* FPU Registers */
   vex_state->guest_F0   = 0xffffffffffffffffULL; /* Argument registers / Return value */
   vex_state->guest_F1   = 0xffffffffffffffffULL;
   vex_state->guest_F2   = 0xffffffffffffffffULL; /* Argument registers */
   vex_state->guest_F3   = 0xffffffffffffffffULL;
   vex_state->guest_F4   = 0xffffffffffffffffULL;
   vex_state->guest_F5   = 0xffffffffffffffffULL;
   vex_state->guest_F6   = 0xffffffffffffffffULL;
   vex_state->guest_F7   = 0xffffffffffffffffULL;
   vex_state->guest_F8   = 0xffffffffffffffffULL; /* Temporary registers */
   vex_state->guest_F9   = 0xffffffffffffffffULL;
   vex_state->guest_F10  = 0xffffffffffffffffULL;
   vex_state->guest_F11  = 0xffffffffffffffffULL;
   vex_state->guest_F12  = 0xffffffffffffffffULL;
   vex_state->guest_F13  = 0xffffffffffffffffULL;
   vex_state->guest_F14  = 0xffffffffffffffffULL;
   vex_state->guest_F15  = 0xffffffffffffffffULL;
   vex_state->guest_F16  = 0xffffffffffffffffULL;
   vex_state->guest_F17  = 0xffffffffffffffffULL;
   vex_state->guest_F18  = 0xffffffffffffffffULL;
   vex_state->guest_F19  = 0xffffffffffffffffULL;
   vex_state->guest_F20  = 0xffffffffffffffffULL;
   vex_state->guest_F21  = 0xffffffffffffffffULL;
   vex_state->guest_F22  = 0xffffffffffffffffULL;
   vex_state->guest_F23  = 0xffffffffffffffffULL;
   vex_state->guest_F24  = 0xffffffffffffffffULL; /* Static registers */
   vex_state->guest_F25  = 0xffffffffffffffffULL;
   vex_state->guest_F26  = 0xffffffffffffffffULL;
   vex_state->guest_F27  = 0xffffffffffffffffULL;
   vex_state->guest_F28  = 0xffffffffffffffffULL;
   vex_state->guest_F29  = 0xffffffffffffffffULL;
   vex_state->guest_F30  = 0xffffffffffffffffULL;
   vex_state->guest_F31  = 0xffffffffffffffffULL;

   vex_state->guest_FCC0 = 0; /* Condition Flag Registers */
   vex_state->guest_FCC1 = 0;
   vex_state->guest_FCC2 = 0;
   vex_state->guest_FCC3 = 0;
   vex_state->guest_FCC4 = 0;
   vex_state->guest_FCC5 = 0;
   vex_state->guest_FCC6 = 0;
   vex_state->guest_FCC7 = 0;
   vex_state->guest_FCSR = 0; /* FP Control and Status Register */

   /* Various pseudo-regs mandated by Vex or Valgrind. */
   /* Emulation notes */
   vex_state->guest_EMNOTE = 0;

   /* For clflush: record start and length of area to invalidate */
   vex_state->guest_CMSTART = 0;
   vex_state->guest_CMLEN   = 0;

   /* Used to record the unredirected guest address at the start of
      a translation whose start has been redirected.  By reading
      this pseudo-register shortly afterwards, the translation can
      find out what the corresponding no-redirection address was.
      Note, this is only set for wrap-style redirects, not for
      replace-style ones. */
   vex_state->guest_NRADDR = 0;
}


/*-----------------------------------------------------------*/
/*--- Describing the loongarch64 guest state, for the     ---*/
/*--- benefit of iropt and instrumenters                  ---*/
/*-----------------------------------------------------------*/

/* Figure out if any part of the guest state contained in minoff
   .. maxoff requires precise memory exceptions.  If in doubt return
   True (but this generates significantly slower code).

   We enforce precise exns for guest SP, PC and FP.

   Only SP is needed in mode VexRegUpdSpAtMemAccess.
*/

Bool guest_loongarch64_state_requires_precise_mem_exns ( Int minoff,
                                                         Int maxoff,
                                                         VexRegisterUpdates pxControl )
{
   Int sp_min = offsetof(VexGuestLOONGARCH64State, guest_R3);
   Int sp_max = sp_min + 8 - 1;
   if ( maxoff < sp_min || minoff > sp_max ) {
      /* no overlap with sp */
      if (pxControl == VexRegUpdSpAtMemAccess)
         return False;  /* We only need to check stack pointer. */
   } else {
      return True;
   }

   Int pc_min = offsetof(VexGuestLOONGARCH64State, guest_PC);
   Int pc_max = pc_min + 8 - 1;
   if ( maxoff < pc_min || minoff > pc_max ) {
      /* no overlap with pc */
   } else {
      return True;
   }

   Int fp_min = offsetof(VexGuestLOONGARCH64State, guest_R22);
   Int fp_max = fp_min + 8 - 1;
   if ( maxoff < fp_min || minoff > fp_max ) {
      /* no overlap with fp */
   } else {
      return True;
   }

   return False;
}

#define ALWAYSDEFD64(field)                            \
   { offsetof(VexGuestLOONGARCH64State, field),        \
      (sizeof ((VexGuestLOONGARCH64State*)0)->field) }

VexGuestLayout loongarch64Guest_layout = {
   /* Total size of the guest state, in bytes. */
   .total_sizeB = sizeof(VexGuestLOONGARCH64State),
   /* Describe the stack pointer. */
   .offset_SP = offsetof(VexGuestLOONGARCH64State, guest_R3),
   .sizeof_SP = 8,
   /* Describe the frame pointer. */
   .offset_FP = offsetof(VexGuestLOONGARCH64State, guest_R22),
   .sizeof_FP = 8,
   /* Describe the instruction pointer. */
   .offset_IP = offsetof(VexGuestLOONGARCH64State, guest_PC),
   .sizeof_IP = 8,
   /* Describe any sections to be regarded by Memcheck as
      'always-defined'. */
   .n_alwaysDefd = 6,
   /* ? :(  */
   .alwaysDefd = {
                  /* 0 */ ALWAYSDEFD64(guest_R0),
                  /* 1 */ ALWAYSDEFD64(guest_PC),
                  /* 2 */ ALWAYSDEFD64(guest_EMNOTE),
                  /* 3 */ ALWAYSDEFD64(guest_CMSTART),
                  /* 4 */ ALWAYSDEFD64(guest_CMLEN),
                  /* 5 */ ALWAYSDEFD64(guest_NRADDR),
                  }
};


/*---------------------------------------------------------------*/
/*--- end                         guest_loongarch64_helpers.c ---*/
/*---------------------------------------------------------------*/