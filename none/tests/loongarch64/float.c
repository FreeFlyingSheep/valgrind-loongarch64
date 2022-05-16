#include <stdio.h>
#include <assert.h>

#define NUM 24

const float fj_s[NUM] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const double fj_d[NUM] = {
   0,         456.25,   3,          -1,
   1384.5,    -7.25,    1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75,
   -45786.5,  456,      34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7,
   -347856.5, 356047.5, -1.0,       23.0625
};

const float fk_s[NUM] = {
   -4578.5, 456.25,   34.03125, 4578.75,
   175,     107,      -456.25,  -7.25,
   -3478.5, 356.5,    -1.0,     23.0625,
   0,       456.25,   3,        -1,
   1384.5,  -7,       100,      -5786.5,
   1752,    0.015625, 0.03125,  -248562.75
};

const double fk_d[NUM] = {
   -45786.5,  456.25,   34.03125,   45786.75,
   1752065,   107,      -45667.25,  -7.25,
   -347856.5, 356047.5, -1.0,       23.0625,
   0,         456.25,   3,          -1,
   1384.5,    -7,       1000000000, -5786.5,
   1752,      0.015625, 0.03125,    -248562.75
};

const float fa_s[NUM] = {
   -347856.5,  356047.5,  -1.0,       23.0625,
   1752,       0.015625,  0.03125,    -248562.75,
   1384.5,     -7.25,     1000000000, -5786.5,
   -347856.75, 356047.75, -1.0,       23.03125,
   0,          456.25,    3,          -1,
   -45786.5,   456,       34.03125,   45786.03125
};

const double fa_d[NUM] = {
   -347856.5,  356047.5,  -1.0,       23.0625,
   1752,       0.015625,  0.03125,    -248562.75,
   1384.5,     -7.25,     1000000000, -5786.5,
   -347856.75, 356047.75, -1.0,       23.03125,
   0,          456.25,    3,          -1,
   -45786.5,   456,       34.03125,   45786.03125
};

const int fj_w[NUM] = {
   0,          456,        3,          -1,
   0xffffffff, 356,        1000000000, -5786,
   1752,       24575,      10,         -248562,
   -45786,     456,        34,         45786,
   1752065,    107,        -45667,     -7,
   -347856,    0x80000000, 0xfffffff,  23
};

const long fj_l[NUM] = {
   18,         25,         3,          -1,
   0xffffffff, 356,        1000000,    -5786,
   -1,         24575,      10,         -125458,
   -486,       456,        34,         45786,
   0,          1700000,    -45667,     -7,
   -347856,    0x80000000, 0xfffffff,  23
};

const int cf[NUM] = {
   0, 1, 0, 1,
   1, 0, 1, 0,
   0, 0, 1, 1,
   1, 1, 0, 0,
   0, 0, 0, 0,
   1, 1, 1, 1
};

typedef enum {
   TO_NEAREST = 0,
   TO_ZERO,
   TO_PLUS_INFINITY,
   TO_MINUS_INFINITY
} round_mode_t;

typedef enum {
   FADD_S, FADD_D, FSUB_S, FSUB_D,
   FMUL_S, FMUL_D, FDIV_S, FDIV_D,
   FMADD_S, FMADD_D, FMSUB_S, FMSUB_D,
   FNMADD_S, FNMADD_D, FNMSUB_S, FNMSUB_D,
   FMAX_S, FMAX_D, FMIN_S, FMIN_D,
   FMAXA_S, FMAXA_D, FMINA_S, FMINA_D,
   FABS_S, FABS_D, FNEG_S, FNEG_D,
   FSQRT_S, FSQRT_D,
   FRECIP_S, FRECIP_D,
   FRSQRT_S, FRSQRT_D,
   FSCALEB_S, FSCALEB_D,
   FLOGB_S, FLOGB_D,
   FCVT_S_D, FCVT_D_S,
   FTINTRM_W_S, FTINTRM_W_D, FTINTRM_L_S, FTINTRM_L_D,
   FTINTRP_W_S, FTINTRP_W_D, FTINTRP_L_S, FTINTRP_L_D,
   FTINTRZ_W_S, FTINTRZ_W_D, FTINTRZ_L_S, FTINTRZ_L_D,
   FTINTRNE_W_S, FTINTRNE_W_D, FTINTRNE_L_S, FTINTRNE_L_D,
   FTINT_W_S, FTINT_W_D, FTINT_L_S, FTINT_L_D,
   FFINT_S_W, FFINT_S_L, FFINT_D_W, FFINT_D_L,
   FRINT_S, FRINT_D,
   FCMP_CAF_S, FCMP_CAF_D, FCMP_SAF_S, FCMP_SAF_D,
   FCMP_CLT_S, FCMP_CLT_D, FCMP_SLT_S, FCMP_SLT_D,
   FCMP_CEQ_S, FCMP_CEQ_D, FCMP_SEQ_S, FCMP_SEQ_D,
   FCMP_CLE_S, FCMP_CLE_D, FCMP_SLE_S, FCMP_SLE_D,
   FCMP_CUN_S, FCMP_CUN_D, FCMP_SUN_S, FCMP_SUN_D,
   FCMP_CULT_S, FCMP_CULT_D, FCMP_SULT_S, FCMP_SULT_D,
   FCMP_CUEQ_S, FCMP_CUEQ_D, FCMP_SUEQ_S, FCMP_SUEQ_D,
   FCMP_CULE_S, FCMP_CULE_D, FCMP_SULE_S, FCMP_SULE_D,
   FCMP_CNE_S, FCMP_CNE_D, FCMP_SNE_S, FCMP_SNE_D,
   FCMP_COR_S, FCMP_COR_D, FCMP_SOR_S, FCMP_SOR_D,
   FCMP_CUNE_S, FCMP_CUNE_D, FCMP_SUNE_S, FCMP_SUNE_D,
   FSEL, FMOV_S, FMOV_D
} op_t;

static inline void set_fcsr(round_mode_t mode)
{
   __asm__ __volatile__("movgr2fcsr $r0, %0" : : "r" (mode << 8));

   const char *round_mode_name[] = { "near", "zero", "+inf", "-inf" };
   printf("roundig mode: %s\n", round_mode_name[mode]);
}

#define TESTINST_FF_S(insn, v1)       \
   {                                  \
      unsigned int fcsr;              \
      float fd_s;                     \
      __asm__ __volatile__(           \
         insn " %0, %2       \n\t"    \
         "movfcsr2gr %1, $r0 \n\t"    \
         : "=f" (fd_s), "=r" (fcsr)   \
         : "f" (v1)                   \
         : "memory");                 \
      printf("%s ::\n", insn);        \
      printf("input: %.6f\n", v1);    \
      printf("output: %.6f\n", fd_s); \
      printf("fcsr: %#x\n", fcsr);    \
   }

#define TESTINST_FF_D(insn, v1)        \
   {                                   \
      unsigned int fcsr;               \
      double fd_d;                     \
      __asm__ __volatile__(            \
         insn " %0, %2       \n\t"     \
         "movfcsr2gr %1, $r0 \n\t"     \
         : "=f" (fd_d), "=r" (fcsr)    \
         : "f" (v1)                    \
         : "memory");                  \
      printf("%s ::\n", insn);         \
      printf("input: %.15f\n", v1);    \
      printf("output: %.15f\n", fd_d); \
      printf("fcsr: %#x\n", fcsr);     \
   }

#define TESTINST_FFF_S(insn, v1, v2)        \
   {                                        \
      unsigned int fcsr;                    \
      float fd_s;                           \
      __asm__ __volatile__(                 \
         insn " %0, %2, %3   \n\t"          \
         "movfcsr2gr %1, $r0 \n\t"          \
         : "=f" (fd_s), "=r" (fcsr)         \
         : "f" (v1), "f" (v2)               \
         : "memory");                       \
      printf("%s ::\n", insn);              \
      printf("input: %.6f %.6f\n", v1, v2); \
      printf("output: %.6f\n", fd_s);       \
      printf("fcsr: %#x\n", fcsr);          \
   }

#define TESTINST_FFF_D(insn, v1, v2)          \
   {                                          \
      unsigned int fcsr;                      \
      double fd_s;                            \
      __asm__ __volatile__(                   \
         insn " %0, %2, %3   \n\t"            \
         "movfcsr2gr %1, $r0 \n\t"            \
         : "=f" (fd_s), "=r" (fcsr)           \
         : "f" (v1), "f" (v2)                 \
         : "memory");                         \
      printf("%s ::\n", insn);                \
      printf("input: %.15f %.15f\n", v1, v2); \
      printf("output: %.15f\n", fd_s);        \
      printf("fcsr: %#x\n", fcsr);            \
   }

#define TESTINST_FFFF_S(insn, v1, v2, v3)            \
   {                                                 \
      unsigned int fcsr;                             \
      float fd_s;                                    \
      __asm__ __volatile__(                          \
         insn " %0, %2, %3, %4 \n\t"                 \
         "movfcsr2gr %1, $r0   \n\t"                 \
         : "=f" (fd_s), "=r" (fcsr)                  \
         : "f" (v1), "f" (v2), "f" (v3)              \
         : "memory");                                \
      printf("%s ::\n", insn);                       \
      printf("input: %.6f %.6f %.6f\n", v1, v2, v3); \
      printf("output: %.6f\n", fd_s);                \
      printf("fcsr: %#x\n", fcsr);                   \
   }

#define TESTINST_FFFF_D(insn, v1, v2, v3)               \
   {                                                    \
      unsigned int fcsr;                                \
      double fd_s;                                      \
      __asm__ __volatile__(                             \
         insn " %0, %2, %3, %4 \n\t"                    \
         "movfcsr2gr %1, $r0   \n\t"                    \
         : "=f" (fd_s), "=r" (fcsr)                     \
         : "f" (v1), "f" (v2), "f" (v3)                 \
         : "memory");                                   \
      printf("%s ::\n", insn);                          \
      printf("input: %.15f %.15f %.15f\n", v1, v2, v3); \
      printf("output: %.15f\n", fd_s);                  \
      printf("fcsr: %#x\n", fcsr);                      \
   }

#define TESTINST_FF_S_D(insn, v1)     \
   {                                  \
      unsigned int fcsr;              \
      float fd_s;                     \
      __asm__ __volatile__(           \
         insn " %0, %2       \n\t"    \
         "movfcsr2gr %1, $r0 \n\t"    \
         : "=f" (fd_s), "=r" (fcsr)   \
         : "f" (v1)                   \
         : "memory");                 \
      printf("%s ::\n", insn);        \
      printf("input: %.15f\n", v1);   \
      printf("output: %.6f\n", fd_s); \
      printf("fcsr: %#x\n", fcsr);    \
   }

#define TESTINST_FF_D_S(insn, v1)      \
   {                                   \
      unsigned int fcsr;               \
      double fd_d;                     \
      __asm__ __volatile__(            \
         insn " %0, %2       \n\t"     \
         "movfcsr2gr %1, $r0 \n\t"     \
         : "=f" (fd_d), "=r" (fcsr)    \
         : "f" (v1)                    \
         : "memory");                  \
      printf("%s ::\n", insn);         \
      printf("input: %.6f\n", v1);     \
      printf("output: %.15f\n", fd_d); \
      printf("fcsr: %#x\n", fcsr);     \
   }

#define TESTINST_FF_W_S(insn, v1)   \
   {                                \
      unsigned int fcsr;            \
      int fd_w;                     \
      __asm__ __volatile__(         \
         insn " %0, %2       \n\t"  \
         "movfcsr2gr %1, $r0 \n\t"  \
         : "=f" (fd_w), "=r" (fcsr) \
         : "f" (v1)                 \
         : "memory");               \
      printf("%s ::\n", insn);      \
      printf("input: %.6f\n", v1);  \
      printf("output: %d\n", fd_w); \
      printf("fcsr: %#x\n", fcsr);  \
   }

#define TESTINST_FF_W_D(insn, v1)   \
   {                                \
      unsigned int fcsr;            \
      int fd_w;                     \
      __asm__ __volatile__(         \
         insn " %0, %2       \n\t"  \
         "movfcsr2gr %1, $r0 \n\t"  \
         : "=f" (fd_w), "=r" (fcsr) \
         : "f" (v1)                 \
         : "memory");               \
      printf("%s ::\n", insn);      \
      printf("input: %.15f\n", v1); \
      printf("output: %d\n", fd_w); \
      printf("fcsr: %#x\n", fcsr);  \
   }
#define TESTINST_FF_L_S(insn, v1)    \
   {                                 \
      unsigned int fcsr;             \
      long fd_l;                     \
      __asm__ __volatile__(          \
         insn " %0, %2       \n\t"   \
         "movfcsr2gr %1, $r0 \n\t"   \
         : "=f" (fd_l), "=r" (fcsr)  \
         : "f" (v1)                  \
         : "memory");                \
      printf("%s ::\n", insn);       \
      printf("input: %.6f\n", v1);   \
      printf("output: %ld\n", fd_l); \
      printf("fcsr: %#x\n", fcsr);   \
   }

#define TESTINST_FF_L_D(insn, v1)     \
   {                                  \
      unsigned int fcsr;              \
      long fd_l;                      \
      __asm__ __volatile__(           \
         insn " %0, %2       \n\t"    \
         "movfcsr2gr %1, $r0 \n\t"    \
         : "=f" (fd_l), "=r" (fcsr)   \
         : "f" (v1)                   \
         : "memory");                 \
      printf("%s ::\n", insn);        \
      printf("input: %.15f\n", v1);   \
      printf("output: %ld\n", fd_l);  \
      printf("fcsr: %#x\n", fcsr);    \
   }

#define TESTINST_FF_S_W(insn, v1)     \
   {                                  \
      unsigned int fcsr;              \
      float fd_s;                     \
      __asm__ __volatile__(           \
         insn " %0, %2       \n\t"    \
         "movfcsr2gr %1, $r0 \n\t"    \
         : "=f" (fd_s), "=r" (fcsr)   \
         : "f" (v1)                   \
         : "memory");                 \
      printf("%s ::\n", insn);        \
      printf("input: %d\n", v1);      \
      printf("output: %.6f\n", fd_s); \
      printf("fcsr: %#x\n", fcsr);    \
   }

#define TESTINST_FF_S_L(insn, v1)     \
   {                                  \
      unsigned int fcsr;              \
      float fd_s;                     \
      __asm__ __volatile__(           \
         insn " %0, %2       \n\t"    \
         "movfcsr2gr %1, $r0 \n\t"    \
         : "=f" (fd_s), "=r" (fcsr)   \
         : "f" (v1)                   \
         : "memory");                 \
      printf("%s ::\n", insn);        \
      printf("input: %ld\n", v1);     \
      printf("output: %.6f\n", fd_s); \
      printf("fcsr: %#x\n", fcsr);    \
   }

#define TESTINST_FF_D_W(insn, v1)      \
   {                                   \
      unsigned int fcsr;               \
      double fd_d;                     \
      __asm__ __volatile__(            \
         insn " %0, %2       \n\t"     \
         "movfcsr2gr %1, $r0 \n\t"     \
         : "=f" (fd_d), "=r" (fcsr)    \
         : "f" (v1)                    \
         : "memory");                  \
      printf("%s ::\n", insn);         \
      printf("input: %d\n", v1);       \
      printf("output: %.15f\n", fd_d); \
      printf("fcsr: %#x\n", fcsr);     \
   }

#define TESTINST_FF_D_L(insn, v1)      \
   {                                   \
      unsigned int fcsr;               \
      double fd_d;                     \
      __asm__ __volatile__(            \
         insn " %0, %2       \n\t"     \
         "movfcsr2gr %1, $r0 \n\t"     \
         : "=f" (fd_d), "=r" (fcsr)    \
         : "f" (v1)                    \
         : "memory");                  \
      printf("%s ::\n", insn);         \
      printf("input: %ld\n", v1);      \
      printf("output: %.15f\n", fd_d); \
      printf("fcsr: %#x\n", fcsr);     \
   }

#define TESTINST_FFC_S(insn, v1, v2)        \
   {                                        \
      unsigned int fcsr;                    \
      int fcc;                              \
      __asm__ __volatile__(                 \
         insn " $fcc0, %2, %3   \n\t"       \
         "movcf2gr %0, $fcc0    \n\t"       \
         "movfcsr2gr %1, $r0    \n\t"       \
         : "=r" (fcc), "=r" (fcsr)          \
         : "f" (v1), "f" (v2)               \
         : "$fcc0", "memory");              \
      printf("%s ::\n", insn);              \
      printf("input: %.6f %.6f\n", v1, v2); \
      printf("output: %d\n", fcc);          \
      printf("fcsr: %#x\n", fcsr);          \
   }

#define TESTINST_FFC_D(insn, v1, v2)          \
   {                                          \
      unsigned int fcsr;                      \
      int fcc;                                \
      __asm__ __volatile__(                   \
         insn " $fcc0, %2, %3   \n\t"         \
         "movcf2gr %0, $fcc0    \n\t"         \
         "movfcsr2gr %1, $r0    \n\t"         \
         : "=r" (fcc), "=r" (fcsr)            \
         : "f" (v1), "f" (v2)                 \
         : "$fcc0", "memory");                \
      printf("%s ::\n", insn);                \
      printf("input: %.15f %.15f\n", v1, v2); \
      printf("output: %d\n", fcc);            \
      printf("fcsr: %#x\n", fcsr);            \
   }

#define TESTINST_FFFC(insn, v1, v2, v3)              \
   {                                                 \
      unsigned int fcsr;                             \
      double fd_s;                                   \
      __asm__ __volatile__(                          \
         "movgr2cf $fcc0, %4      \n\t"              \
         insn " %0, %2, %3, $fcc0 \n\t"              \
         "movfcsr2gr %1, $r0      \n\t"              \
         : "=f" (fd_s), "=r" (fcsr)                  \
         : "f" (v1), "f" (v2), "r" (v3)              \
         : "memory");                                \
      printf("%s ::\n", insn);                       \
      printf("input: %.15f %.15f %d\n", v1, v2, v3); \
      printf("output: %.15f\n", fd_s);               \
      printf("fcsr: %#x\n", fcsr);                   \
   }

void test(op_t op)
{
   int i;
   round_mode_t mode;
   for (mode = TO_NEAREST; mode <= TO_MINUS_INFINITY; mode++) {
      for (i = 0; i < NUM; i++) {
         set_fcsr(mode);
         switch (op) {
            case FADD_S:
               TESTINST_FFF_S("fadd.s", fj_s[i], fk_s[i]);
               break;
            case FADD_D:
               TESTINST_FFF_D("fadd.d", fj_d[i], fk_d[i]);
               break;
            case FSUB_S:
               TESTINST_FFF_S("fsub.s", fj_s[i], fk_s[i]);
               break;
            case FSUB_D:
               TESTINST_FFF_D("fsub.d", fj_d[i], fk_d[i]);
               break;
            case FMUL_S:
               TESTINST_FFF_S("fmul.s", fj_s[i], fk_s[i]);
               break;
            case FMUL_D:
               TESTINST_FFF_D("fmul.d", fj_d[i], fk_d[i]);
               break;
            case FDIV_S:
               TESTINST_FFF_S("fdiv.s", fj_s[i], fk_s[i]);
               break;
            case FDIV_D:
               TESTINST_FFF_D("fdiv.d", fj_d[i], fk_d[i]);
               break;
            case FMADD_S:
               TESTINST_FFFF_S("fmadd.s", fj_s[i], fk_s[i], fa_s[i]);
               break;
            case FMADD_D:
               TESTINST_FFFF_D("fmadd.d", fj_d[i], fk_d[i], fa_d[i]);
               break;
            case FMSUB_S:
               TESTINST_FFFF_S("fmsub.s", fj_s[i], fk_s[i], fa_s[i]);
               break;
            case FMSUB_D:
               TESTINST_FFFF_D("fmsub.d", fj_d[i], fk_d[i], fa_d[i]);
               break;
            case FNMADD_S:
               TESTINST_FFFF_S("fnmadd.s", fj_s[i], fk_s[i], fa_s[i]);
               break;
            case FNMADD_D:
               TESTINST_FFFF_D("fnmadd.d", fj_d[i], fk_d[i], fa_d[i]);
               break;
            case FNMSUB_S:
               TESTINST_FFFF_S("fnmsub.s", fj_s[i], fk_s[i], fa_s[i]);
               break;
            case FNMSUB_D:
               TESTINST_FFFF_D("fnmsub.d", fj_d[i], fk_d[i], fa_d[i]);
               break;
            case FMAX_S:
               TESTINST_FFF_S("fmax.s", fj_s[i], fk_s[i]);
               break;
            case FMAX_D:
               TESTINST_FFF_D("fmax.d", fj_d[i], fk_d[i]);
               break;
            case FMIN_S:
               TESTINST_FFF_S("fmin.s", fj_s[i], fk_s[i]);
               break;
            case FMIN_D:
               TESTINST_FFF_D("fmin.d", fj_d[i], fk_d[i]);
               break;
            case FMAXA_S:
               TESTINST_FFF_S("fmaxa.s", fj_s[i], fk_s[i]);
               break;
            case FMAXA_D:
               TESTINST_FFF_D("fmaxa.d", fj_d[i], fk_d[i]);
               break;
            case FMINA_S:
               TESTINST_FFF_S("fmina.s", fj_s[i], fk_s[i]);
               break;
            case FMINA_D:
               TESTINST_FFF_D("fmina.d", fj_d[i], fk_d[i]);
               break;
            case FABS_S:
               TESTINST_FF_S("fabs.s", fj_s[i]);
               break;
            case FABS_D:
               TESTINST_FF_D("fabs.d", fj_d[i]);
               break;
            case FNEG_S:
               TESTINST_FF_S("fneg.s", fj_s[i]);
               break;
            case FNEG_D:
               TESTINST_FF_D("fneg.d", fj_d[i]);
               break;
            case FSQRT_S:
               TESTINST_FF_S("fsqrt.s", fj_s[i]);
               break;
            case FSQRT_D:
               TESTINST_FF_D("fsqrt.d", fj_d[i]);
               break;
            case FRECIP_S:
               TESTINST_FF_S("frecip.s", fj_s[i]);
               break;
            case FRECIP_D:
               TESTINST_FF_D("frecip.d", fj_d[i]);
               break;
            case FRSQRT_S:
               TESTINST_FF_S("frsqrt.s", fj_s[i]);
               break;
            case FRSQRT_D:
               TESTINST_FF_D("frsqrt.d", fj_d[i]);
               break;
            case FSCALEB_S:
               TESTINST_FFF_S("fscaleb.s", fj_s[i], fk_s[i]);
               break;
            case FSCALEB_D:
               TESTINST_FFF_D("fscaleb.d", fj_d[i], fk_d[i]);
               break;
            case FLOGB_S:
               TESTINST_FF_S("flogb.s", fj_s[i]);
               break;
            case FLOGB_D:
               TESTINST_FF_D("flogb.d", fj_d[i]);
               break;
            case FCVT_S_D:
               TESTINST_FF_S_D("fcvt.s.d", fj_d[i]);
               break;
            case FCVT_D_S:
               TESTINST_FF_D_S("fcvt.d.s", fj_s[i]);
               break;
            case FTINTRM_W_S:
               TESTINST_FF_W_S("ftintrm.w.s", fj_s[i]);
               break;
            case FTINTRM_W_D:
               TESTINST_FF_W_D("ftintrm.w.d", fj_d[i]);
               break;
            case FTINTRM_L_S:
               TESTINST_FF_L_S("ftintrm.l.s", fj_s[i]);
               break;
            case FTINTRM_L_D:
               TESTINST_FF_L_D("ftintrm.l.d", fj_d[i]);
               break;
            case FTINTRP_W_S:
               TESTINST_FF_W_S("ftintrp.w.s", fj_s[i]);
               break;
            case FTINTRP_W_D:
               TESTINST_FF_W_D("ftintrp.w.d", fj_d[i]);
               break;
            case FTINTRP_L_S:
               TESTINST_FF_L_S("ftintrp.l.s", fj_s[i]);
               break;
            case FTINTRP_L_D:
               TESTINST_FF_L_D("ftintrp.l.d", fj_d[i]);
               break;
            case FTINTRZ_W_S:
               TESTINST_FF_W_S("ftintrz.w.s", fj_s[i]);
               break;
            case FTINTRZ_W_D:
               TESTINST_FF_W_D("ftintrz.w.d", fj_d[i]);
               break;
            case FTINTRZ_L_S:
               TESTINST_FF_L_S("ftintrz.l.s", fj_s[i]);
               break;
            case FTINTRZ_L_D:
               TESTINST_FF_L_D("ftintrz.l.d", fj_d[i]);
               break;
            case FTINTRNE_W_S:
               TESTINST_FF_W_S("ftintrne.w.s", fj_s[i]);
               break;
            case FTINTRNE_W_D:
               TESTINST_FF_W_D("ftintrne.w.d", fj_d[i]);
               break;
            case FTINTRNE_L_S:
               TESTINST_FF_L_S("ftintrne.l.s", fj_s[i]);
               break;
            case FTINTRNE_L_D:
               TESTINST_FF_L_D("ftintrne.l.d", fj_d[i]);
               break;
            case FTINT_W_S:
               TESTINST_FF_W_S("ftint.w.s", fj_s[i]);
               break;
            case FTINT_W_D:
               TESTINST_FF_W_D("ftint.w.d", fj_d[i]);
               break;
            case FTINT_L_S:
               TESTINST_FF_L_S("ftint.l.s", fj_s[i]);
               break;
            case FTINT_L_D:
               TESTINST_FF_L_D("ftint.l.d", fj_d[i]);
               break;
            case FFINT_S_W:
               TESTINST_FF_S_W("ffint.s.w", fj_w[i]);
               break;
            case FFINT_S_L:
               TESTINST_FF_S_L("ffint.s.l", fj_l[i]);
               break;
            case FFINT_D_W:
               TESTINST_FF_D_W("ffint.d.w", fj_w[i]);
               break;
            case FFINT_D_L:
               TESTINST_FF_D_L("ffint.d.l", fj_l[i]);
               break;
            case FRINT_S:
               TESTINST_FF_S("frint.s", fj_s[i]);
               break;
            case FRINT_D:
               TESTINST_FF_D("frint.d", fj_d[i]);
               break;
            case FCMP_CAF_S:
               TESTINST_FFC_S("fcmp.caf.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CAF_D:
               TESTINST_FFC_D("fcmp.caf.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SAF_S:
               TESTINST_FFC_S("fcmp.saf.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SAF_D:
               TESTINST_FFC_D("fcmp.saf.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CLT_S:
               TESTINST_FFC_S("fcmp.clt.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CLT_D:
               TESTINST_FFC_D("fcmp.clt.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SLT_S:
               TESTINST_FFC_S("fcmp.slt.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SLT_D:
               TESTINST_FFC_D("fcmp.slt.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CEQ_S:
               TESTINST_FFC_S("fcmp.ceq.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CEQ_D:
               TESTINST_FFC_D("fcmp.ceq.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SEQ_S:
               TESTINST_FFC_S("fcmp.seq.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SEQ_D:
               TESTINST_FFC_D("fcmp.seq.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CLE_S:
               TESTINST_FFC_S("fcmp.cle.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CLE_D:
               TESTINST_FFC_D("fcmp.cle.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SLE_S:
               TESTINST_FFC_S("fcmp.sle.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SLE_D:
               TESTINST_FFC_D("fcmp.sle.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CUN_S:
               TESTINST_FFC_S("fcmp.cun.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CUN_D:
               TESTINST_FFC_D("fcmp.cun.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SUN_S:
               TESTINST_FFC_S("fcmp.sun.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SUN_D:
               TESTINST_FFC_D("fcmp.sun.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CULT_S:
               TESTINST_FFC_S("fcmp.cult.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CULT_D:
               TESTINST_FFC_D("fcmp.cult.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SULT_S:
               TESTINST_FFC_S("fcmp.sult.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SULT_D:
               TESTINST_FFC_D("fcmp.sult.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CUEQ_S:
               TESTINST_FFC_S("fcmp.cueq.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CUEQ_D:
               TESTINST_FFC_D("fcmp.cueq.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SUEQ_S:
               TESTINST_FFC_S("fcmp.sueq.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SUEQ_D:
               TESTINST_FFC_D("fcmp.sueq.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CULE_S:
               TESTINST_FFC_S("fcmp.cule.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CULE_D:
               TESTINST_FFC_D("fcmp.cule.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SULE_S:
               TESTINST_FFC_S("fcmp.sule.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SULE_D:
               TESTINST_FFC_D("fcmp.sule.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CNE_S:
               TESTINST_FFC_S("fcmp.cne.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CNE_D:
               TESTINST_FFC_D("fcmp.cne.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SNE_S:
               TESTINST_FFC_S("fcmp.sne.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SNE_D:
               TESTINST_FFC_D("fcmp.sne.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_COR_S:
               TESTINST_FFC_S("fcmp.cor.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_COR_D:
               TESTINST_FFC_D("fcmp.cor.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SOR_S:
               TESTINST_FFC_S("fcmp.sor.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SOR_D:
               TESTINST_FFC_D("fcmp.sor.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_CUNE_S:
               TESTINST_FFC_S("fcmp.cune.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_CUNE_D:
               TESTINST_FFC_D("fcmp.cune.d", fj_d[i], fk_d[i]);
               break;
            case FCMP_SUNE_S:
               TESTINST_FFC_S("fcmp.sune.s", fj_s[i], fk_s[i]);
               break;
            case FCMP_SUNE_D:
               TESTINST_FFC_D("fcmp.sune.d", fj_d[i], fk_d[i]);
               break;
            case FSEL:
               TESTINST_FFFC("fsel", fj_d[i], fk_d[i], cf[i]);
               break;
            case FMOV_S:
               TESTINST_FF_S("fmov.s", fj_s[i]);
               break;
            case FMOV_D:
               TESTINST_FF_D("fmov.d", fj_d[i]);
               break;
            default:
               assert(0);
               break;
         }
      }
   }
}

int main(void)
{
   op_t op;
   for (op = FADD_S; op <= FMOV_D; op++)
      test(op);
   return 0;
}
