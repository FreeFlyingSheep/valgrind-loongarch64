
/*--------------------------------------------------------------------*/
/*--- Create/destroy signal delivery frames.                       ---*/
/*---                             sigframe-loongarch64-linux.c     ---*/
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

#if defined(VGP_loongarch64_linux)

#include "pub_core_basics.h"
#include "pub_core_vki.h"
#include "pub_core_vkiscnums.h"
#include "pub_core_threadstate.h"
#include "pub_core_aspacemgr.h"
#include "pub_core_libcbase.h"
#include "pub_core_libcassert.h"
#include "pub_core_libcprint.h"
#include "pub_core_machine.h"
#include "pub_core_options.h"
#include "pub_core_sigframe.h"
#include "pub_core_signals.h"
#include "pub_core_tooliface.h"
#include "pub_core_trampoline.h"
#include "priv_sigframe.h"


/*------------------------------------------------------------*/
/*--- Signal frame layouts                                 ---*/
/*------------------------------------------------------------*/

struct vg_sig_private {
   UInt magicPI;
   UInt sigNo_private;
   VexGuestLOONGARCH64State vex_shadow1;
   VexGuestLOONGARCH64State vex_shadow2;
};

struct rt_sigframe {
   struct vki_siginfo rs_info;
   struct vki_ucontext rs_uctx;
   struct vg_sig_private priv;
};


/*------------------------------------------------------------*/
/*--- Creating signal frames                               ---*/
/*------------------------------------------------------------*/

static void create_siginfo ( ThreadId tid,
                             struct rt_sigframe *frame,
                             const vki_siginfo_t *si)
{
   VG_TRACK(pre_mem_write, Vg_CoreSignal, tid, "signal frame siginfo",
            (Addr)&frame->rs_info, sizeof(frame->rs_info));

   VG_(memcpy)(&frame->rs_info, si, sizeof(vki_siginfo_t));

   VG_TRACK(post_mem_write, Vg_CoreSignal, tid,
            (Addr)&frame->rs_info, sizeof(frame->rs_info));
}

static void create_sigcontext ( ThreadState *tst,
                                struct vki_sigcontext **sc)
{
   struct vki_sigcontext *sctx = *sc;

   VG_TRACK(pre_mem_write, Vg_CoreSignal, tst->tid, "signal frame mcontext",
            (Addr)sctx, sizeof(ULong) * 32);

   sctx->sc_regs[1]  = tst->arch.vex.guest_R1;
   sctx->sc_regs[2]  = tst->arch.vex.guest_R2;
   sctx->sc_regs[3]  = tst->arch.vex.guest_R3;
   sctx->sc_regs[4]  = tst->arch.vex.guest_R4;
   sctx->sc_regs[5]  = tst->arch.vex.guest_R5;
   sctx->sc_regs[6]  = tst->arch.vex.guest_R6;
   sctx->sc_regs[7]  = tst->arch.vex.guest_R7;
   sctx->sc_regs[8]  = tst->arch.vex.guest_R8;
   sctx->sc_regs[9]  = tst->arch.vex.guest_R9;
   sctx->sc_regs[10] = tst->arch.vex.guest_R10;
   sctx->sc_regs[11] = tst->arch.vex.guest_R11;
   sctx->sc_regs[12] = tst->arch.vex.guest_R12;
   sctx->sc_regs[13] = tst->arch.vex.guest_R13;
   sctx->sc_regs[14] = tst->arch.vex.guest_R14;
   sctx->sc_regs[15] = tst->arch.vex.guest_R15;
   sctx->sc_regs[16] = tst->arch.vex.guest_R16;
   sctx->sc_regs[17] = tst->arch.vex.guest_R17;
   sctx->sc_regs[18] = tst->arch.vex.guest_R18;
   sctx->sc_regs[19] = tst->arch.vex.guest_R19;
   sctx->sc_regs[20] = tst->arch.vex.guest_R20;
   sctx->sc_regs[21] = tst->arch.vex.guest_R21;
   sctx->sc_regs[22] = tst->arch.vex.guest_R22;
   sctx->sc_regs[23] = tst->arch.vex.guest_R23;
   sctx->sc_regs[24] = tst->arch.vex.guest_R24;
   sctx->sc_regs[25] = tst->arch.vex.guest_R25;
   sctx->sc_regs[26] = tst->arch.vex.guest_R26;
   sctx->sc_regs[27] = tst->arch.vex.guest_R27;
   sctx->sc_regs[28] = tst->arch.vex.guest_R28;
   sctx->sc_regs[29] = tst->arch.vex.guest_R29;
   sctx->sc_regs[30] = tst->arch.vex.guest_R30;
   sctx->sc_regs[31] = tst->arch.vex.guest_R31;
   sctx->sc_pc       = tst->arch.vex.guest_PC;
}

static void create_ucontext ( ThreadState *tst,
                              ThreadId tid,
                              struct vki_ucontext *uc,
                              const vki_sigset_t *mask,
                              struct vki_sigcontext **sc,
                              const vki_siginfo_t *siginfo)
{
   VG_TRACK(pre_mem_write, Vg_CoreSignal, tid, "signal frame ucontext",
            (Addr)uc, offsetof(struct vki_ucontext, uc_mcontext));

   uc->uc_flags   = 0;
   uc->uc_link    = 0;
   uc->uc_stack   = tst->altstack;
   uc->uc_sigmask = *mask;

   VG_TRACK(post_mem_write, Vg_CoreSignal, tid, (Addr)uc,
            offsetof(struct vki_ucontext, uc_mcontext));

   create_sigcontext(tst, sc);
}

/* EXPORTED */
void VG_(sigframe_create) ( ThreadId tid,
                            Bool on_altstack,
                            Addr sp_top_of_frame,
                            const vki_siginfo_t *siginfo,
                            const struct vki_ucontext *siguc,
                            void *handler,
                            UInt flags,
                            const vki_sigset_t *mask,
                            void *restorer )
{
   UInt size = sizeof(struct rt_sigframe);
   Addr sp = VG_ROUNDDN(sp_top_of_frame - size, 16);

   ThreadState *tst = VG_(get_ThreadState)(tid);
   if (! ML_(sf_maybe_extend_stack)(tst, sp, size, flags))
      return;

   struct rt_sigframe *frame = (struct rt_sigframe *)sp;
   create_siginfo(tid, frame, siginfo);

   struct vki_ucontext *uctx = &frame->rs_uctx;
   struct vki_sigcontext *sctx = &(frame->rs_uctx.uc_mcontext);
   create_ucontext(tst, tid, uctx, mask, &sctx, siginfo);

   /*
      Arguments to signal handler:

         a0 = signal number
         a1 = pointer to siginfo
         a2 = pointer to ucontext

      csr_era point to the signal handler, $r3 (sp) points to
      the struct rt_sigframe.
	 */

   Int sigNo = siginfo->si_signo;
   tst->arch.vex.guest_R4 = sigNo;
   tst->arch.vex.guest_R5 = (Addr) &frame->rs_info;
   tst->arch.vex.guest_R6 = (Addr) &frame->rs_uctx;
   tst->arch.vex.guest_R3 = (Addr) frame;
   tst->arch.vex.guest_R1 = (Addr) &VG_(loongarch64_linux_SUBST_FOR_rt_sigreturn);

   struct vg_sig_private *priv = &frame->priv;
   priv->magicPI       = 0x31415927;
   priv->sigNo_private = sigNo;
   priv->vex_shadow1   = tst->arch.vex_shadow1;
   priv->vex_shadow2   = tst->arch.vex_shadow2;

   /* Set the thread so it will next run the handler. */
   VG_TRACK(post_reg_write, Vg_CoreSignal, tid, VG_O_STACK_PTR, sizeof(Addr));

   if (VG_(clo_trace_signals))
      VG_(printf)("handler = %p\n", handler);

   tst->arch.vex.guest_PC = (Addr) handler;
   /* This thread needs to be marked runnable, but we leave that
      the caller to do. */
}


/*------------------------------------------------------------*/
/*--- Destroying signal frames                             ---*/
/*------------------------------------------------------------*/

static void restore_regs ( ThreadState *tst,
                           struct vki_sigcontext *mc)
{
   tst->arch.vex.guest_R1  = mc->sc_regs[1];
   tst->arch.vex.guest_R2  = mc->sc_regs[2];
   tst->arch.vex.guest_R3  = mc->sc_regs[3];
   tst->arch.vex.guest_R4  = mc->sc_regs[4];
   tst->arch.vex.guest_R5  = mc->sc_regs[5];
   tst->arch.vex.guest_R6  = mc->sc_regs[6];
   tst->arch.vex.guest_R7  = mc->sc_regs[7];
   tst->arch.vex.guest_R8  = mc->sc_regs[8];
   tst->arch.vex.guest_R9  = mc->sc_regs[9];
   tst->arch.vex.guest_R10 = mc->sc_regs[10];
   tst->arch.vex.guest_R11 = mc->sc_regs[11];
   tst->arch.vex.guest_R12 = mc->sc_regs[12];
   tst->arch.vex.guest_R13 = mc->sc_regs[13];
   tst->arch.vex.guest_R14 = mc->sc_regs[14];
   tst->arch.vex.guest_R15 = mc->sc_regs[15];
   tst->arch.vex.guest_R16 = mc->sc_regs[16];
   tst->arch.vex.guest_R17 = mc->sc_regs[17];
   tst->arch.vex.guest_R18 = mc->sc_regs[18];
   tst->arch.vex.guest_R19 = mc->sc_regs[19];
   tst->arch.vex.guest_R20 = mc->sc_regs[20];
   tst->arch.vex.guest_R21 = mc->sc_regs[21];
   tst->arch.vex.guest_R22 = mc->sc_regs[22];
   tst->arch.vex.guest_R23 = mc->sc_regs[23];
   tst->arch.vex.guest_R24 = mc->sc_regs[24];
   tst->arch.vex.guest_R25 = mc->sc_regs[25];
   tst->arch.vex.guest_R26 = mc->sc_regs[26];
   tst->arch.vex.guest_R27 = mc->sc_regs[27];
   tst->arch.vex.guest_R28 = mc->sc_regs[28];
   tst->arch.vex.guest_R29 = mc->sc_regs[29];
   tst->arch.vex.guest_R30 = mc->sc_regs[30];
   tst->arch.vex.guest_R31 = mc->sc_regs[31];
   tst->arch.vex.guest_PC  = mc->sc_pc;
}

/* EXPORTED */
void VG_(sigframe_destroy)( ThreadId tid, Bool isRT )
{
   vg_assert(VG_(is_valid_tid)(tid));

   ThreadState *tst = VG_(get_ThreadState)(tid);
   Addr sp = tst->arch.vex.guest_R3;
   struct rt_sigframe *frame = (struct rt_sigframe *)sp;
   struct vki_ucontext *uc = &frame->rs_uctx;

   tst->sig_mask = uc->uc_sigmask;
   tst->tmp_sig_mask = uc->uc_sigmask;

   struct vki_sigcontext *mc = &uc->uc_mcontext;
   restore_regs(tst, mc);

   struct vg_sig_private *priv = &frame->priv;
   vg_assert(priv->magicPI == 0x31415927);
   tst->arch.vex_shadow1 = priv->vex_shadow1;
   tst->arch.vex_shadow2 = priv->vex_shadow2;

   UInt frame_size = sizeof(*frame);
   VG_TRACK(die_mem_stack_signal, sp, frame_size);

   if (VG_(clo_trace_signals))
      VG_(message)(Vg_DebugMsg,
         "VG_(signal_return) (thread %u): isRT=%d valid magic; PC=%#llx\n",
         tid, isRT, tst->arch.vex.guest_PC);

   Int sigNo = priv->sigNo_private;
   VG_TRACK( post_deliver_signal, tid, sigNo );
}

#endif /* defined(VGP_loongarch64_linux) */

/*--------------------------------------------------------------------*/
/*--- end                             sigframe-loongarch64-linux.c ---*/
/*--------------------------------------------------------------------*/
