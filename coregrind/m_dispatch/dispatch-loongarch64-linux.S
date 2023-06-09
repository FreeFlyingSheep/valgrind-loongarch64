
/*--------------------------------------------------------------------*/
/*--- The core dispatch loop, for jumping to a code address.       ---*/
/*---                                 dispatch-loongarch64-linux.S ---*/
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

#include "pub_core_dispatch_asm.h"
#include "pub_core_transtab_asm.h"
#include "libvex_guest_offsets.h"	/* for OFFSET_loongarch64_* */


/*------------------------------------------------------------*/
/*---                                                      ---*/
/*--- The dispatch loop.  VG_(disp_run_translations) is    ---*/
/*--- used to run all translations,                        ---*/
/*--- including no-redir ones.                             ---*/
/*---                                                      ---*/
/*------------------------------------------------------------*/

/*----------------------------------------------------*/
/*--- Entry and preamble (set everything up)       ---*/
/*----------------------------------------------------*/

/* signature:
void VG_(disp_run_translations)( UWord* two_words,
                                 void*  guest_state,
                                 Addr   host_addr );
*/

.text
.globl VG_(disp_run_translations)
VG_(disp_run_translations):
   /* a0 holds two_words   */
   /* a1 holds guest_state */
   /* a2 holds host_addr   */

   /* New stack frame.  Stack must remain 16 aligned (at least) */
   addi.d   $sp, $sp, -96

   /* Save ra */
   st.d     $ra, $sp, 0

   /* .. and s0 - s8 */
   st.d     $s0, $sp, 8
   st.d     $s1, $sp, 16
   st.d     $s2, $sp, 24
   st.d     $s3, $sp, 32
   st.d     $s4, $sp, 40
   st.d     $s5, $sp, 48
   st.d     $s6, $sp, 56
   st.d     $s7, $sp, 64
   st.d     $s8, $sp, 72

   /* ... and fp */
   st.d     $fp, $sp, 80

   /* and a0. In postamble it will be restored such that the
      return values can be written */
   st.d     $a0, $sp, 88

   /* Load address of guest state into s8 */
   move     $s8, $a1

   /* and jump into the code cache.  Chained translations in
      the code cache run, until for whatever reason, they can't
      continue.  When that happens, the translation in question
      will jump (or call) to one of the continuation points
      VG_(cp_...) below. */
   ibar     0              /* Insn sync barrier */
   jr       $a2
   /*NOTREACHED*/

/*----------------------------------------------------*/
/*--- Postamble and exit.                          ---*/
/*----------------------------------------------------*/

postamble:
   /* At this point, a0 and a1 contain two
      words to be returned to the caller.  a0
      holds a TRC value, and a1 optionally may
      hold another word (for CHAIN_ME exits, the
      address of the place to patch.) */

   /* Restore a0 from stack to t0; holds address of two_words */
   ld.d     $t0, $sp, 88
   st.d     $a0, $t0, 0    /* Store a0 to two_words[0] */
   st.d     $a1, $t0, 8    /* Store a1 to two_words[1] */

   /* Restore ra */
   ld.d     $ra, $sp, 0

   /* ... and s0 - s8 */
   ld.d     $s0, $sp, 8
   ld.d     $s1, $sp, 16
   ld.d     $s2, $sp, 24
   ld.d     $s3, $sp, 32
   ld.d     $s4, $sp, 40
   ld.d     $s5, $sp, 48
   ld.d     $s6, $sp, 56
   ld.d     $s7, $sp, 64
   ld.d     $s8, $sp, 72

   /* ... and fp */
   ld.d     $fp, $sp, 80

   addi.d   $sp, $sp, 96   /* Restore sp */
   jr       $ra
   /*NOTREACHED*/

/*----------------------------------------------------*/
/*--- Continuation points                          ---*/
/*----------------------------------------------------*/

/* ------ Chain me to slow entry point ------ */
.globl VG_(disp_cp_chain_me_to_slowEP)
VG_(disp_cp_chain_me_to_slowEP):
   /* We got called.  The return address indicates
      where the patching needs to happen.  Collect
      the return address and, exit back to C land,
      handing the caller the pair (Chain_me_S, RA) */
   li.w     $a0, VG_TRC_CHAIN_ME_TO_SLOW_EP
   move     $a1, $ra
   /* 4 * 4 = mkLoadImm_EXACTLY4
          4 = jirl $ra, $t0, 0 */
   addi.d   $a1, $a1, -20
   b        postamble
   /*NOTREACHED*/

/* ------ Chain me to fast entry point ------ */
.globl VG_(disp_cp_chain_me_to_fastEP)
VG_(disp_cp_chain_me_to_fastEP):
   /* We got called.  The return address indicates
      where the patching needs to happen.  Collect
      the return address and, exit back to C land,
      handing the caller the pair (Chain_me_S, RA) */
   li.w     $a0, VG_TRC_CHAIN_ME_TO_FAST_EP
   move     $a1, $ra
   /* 4 * 4 = mkLoadImm_EXACTLY4
      4     = jirl $ra, $t0, 0 */
   addi.d   $a1, $a1, -20
   b        postamble
   /*NOTREACHED*/

/* ------ Indirect but boring jump ------ */
.globl VG_(disp_cp_xindir)
VG_(disp_cp_xindir):
   /* Where are we going? */
   ld.d     $t0, $s8, OFFSET_loongarch64_PC

   /* Stats only */
   la.local $t4, VG_(stats__n_xIndirs_32)
   ld.d     $t1, $t4, 0
   addi.d   $t1, $t1, 1
   st.w     $t1, $t4, 0

   /* LIVE: s8 (guest state ptr), t0 (guest address to go to).
      We use 6 temporaries:
         t6 (to point at the relevant FastCacheSet),
         t1, t2, t3 (scratch, for swapping entries within a set)
         t4, t5 (other scratch)
    */

   /* Try a fast lookup in the translation cache.  This is pretty much
      a handcoded version of VG_(lookupInFastCache). */

   // Compute t6 = VG_TT_FAST_HASH(guest)
   srli.d   $t6, $t0, 2                      // g2 = guest >> 2
   srli.d   $t5, $t0, (VG_TT_FAST_BITS + 2)  // (g2 >> VG_TT_FAST_BITS)
   xor      $t6, $t6, $t5                    // (g2 >> VG_TT_FAST_BITS) ^ g2
   li.w     $t5, VG_TT_FAST_MASK
   and      $t6, $t6, $t5                    // setNo

   // Compute t6 = &VG_(tt_fast)[t6]
   la.local $t5, VG_(tt_fast)
   slli.d   $t6, $t6, VG_FAST_CACHE_SET_BITS
   add.d    $t6, $t6, $t5

   /* LIVE: s8 (guest state ptr), t0 (guest addr), t6 (cache set) */
0: // try way 0
   ld.d     $t4, $t6, FCS_g0   // .guest0
   ld.d     $t5, $t6, FCS_h0   // .host0
   bne      $t4, $t0, 1f       // cmp against .guest0
   // hit at way 0
   // goto .host0
   jr       $t5
   /*NOTREACHED*/

1: // try way 1
   ld.d     $t4, $t6, FCS_g1
   bne      $t4, $t0, 2f       // cmp against .guest1
   // hit at way 1; swap upwards
   ld.d     $t1, $t6, FCS_g0   // $t1 = old .guest0
   ld.d     $t2, $t6, FCS_h0   // $t2 = old .host0
   ld.d     $t3, $t6, FCS_h1   // $t3 = old .host1
   st.d     $t0, $t6, FCS_g0   // new .guest0 = guest
   st.d     $t3, $t6, FCS_h0   // new .host0 = old .host1
   st.d     $t1, $t6, FCS_g1   // new .guest1 = old .guest0
   st.d     $t2, $t6, FCS_h1   // new .host1 = old .host0

   // stats only
   la.local $t4, VG_(stats__n_xIndir_hits1_32)
   ld.d     $t5, $t4, 0
   addi.d   $t5, $t5, 1
   st.w     $t5, $t4, 0
   // goto old .host1 a.k.a. new .host0
   jr       $t3
   /*NOTREACHED*/

2: // try way 2
   ld.d     $t4, $t6, FCS_g2
   bne      $t4, $t0, 3f       // cmp against .guest2
   // hit at way 2; swap upwards
   ld.d     $t1, $t6, FCS_g1
   ld.d     $t2, $t6, FCS_h1
   ld.d     $t3, $t6, FCS_h2
   st.d     $t0, $t6, FCS_g1
   st.d     $t3, $t6, FCS_h1
   st.d     $t1, $t6, FCS_g2
   st.d     $t2, $t6, FCS_h2

   // stats only
   la.local $t4, VG_(stats__n_xIndir_hits2_32)
   ld.d     $t5, $t4, 0
   addi.d   $t5, $t5, 1
   st.w     $t5, $t4, 0
   // goto old .host2 a.k.a. new .host1
   jr       $t3
   /*NOTREACHED*/

3: // try way 3
   ld.d     $t4, $t6, FCS_g3
   bne      $t4, $t0, 4f       // cmp against .guest3
   // hit at way 3; swap upwards
   ld.d     $t1, $t6, FCS_g2
   ld.d     $t2, $t6, FCS_h2
   ld.d     $t3, $t6, FCS_h3
   st.d     $t0, $t6, FCS_g2
   st.d     $t3, $t6, FCS_h2
   st.d     $t1, $t6, FCS_g3
   st.d     $t2, $t6, FCS_h3

   // stats only
   la.local $t4, VG_(stats__n_xIndir_hits3_32)
   ld.d     $t5, $t4, 0
   addi.d   $t5, $t5, 1
   st.w     $t5, $t4, 0
   // goto old .host3 a.k.a. new .host2
   jr       $t3
   /*NOTREACHED*/

4: // fast lookup failed:
   /* stats only */
   la.local $t4, VG_(stats__n_xIndir_misses_32)
   ld.d     $t5, $t4, 0
   addi.d   $t5, $t5, 1
   st.w     $t5, $t4, 0

   li.w     $a0, VG_TRC_INNER_FASTMISS
   move     $a1, $zero
   b        postamble
   /*NOTREACHED*/

/* ------ Assisted jump ------ */
.globl VG_(disp_cp_xassisted)
VG_(disp_cp_xassisted):
   /* guest-state-pointer contains the TRC. Put the value into the
      return register */
   move     $a0, $s8
   move     $a1, $zero
   b        postamble

/* ------ Event check failed ------ */
.globl VG_(disp_cp_evcheck_fail)
VG_(disp_cp_evcheck_fail):
   li.w     $a0, VG_TRC_INNER_COUNTERZERO
   move     $a1, $zero
   b        postamble

.size VG_(disp_run_translations), .-VG_(disp_run_translations)

#endif // defined(VGP_loongarch64_linux)

/* Let the linker know we don't need an executable stack */
MARK_STACK_NO_EXEC

/*--------------------------------------------------------------------*/
/*--- end                             dispatch-loongarch64-linux.S ---*/
/*--------------------------------------------------------------------*/
