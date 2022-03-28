#include <stdio.h>

#define TESTINST_MOV(v1, v2, v3, v4, v5, v6, val)  \
   {                                               \
      unsigned long res1 = (unsigned long)v1;      \
      unsigned long res2 = (unsigned long)v2;      \
      unsigned long res3 = (unsigned long)v3;      \
      unsigned long res4 = (unsigned long)v4;      \
      unsigned long res5 = (unsigned long)v5;      \
      unsigned long res6 = (unsigned long)v6;      \
      __asm__ __volatile__(                        \
         "movgr2fr.w %0, %6  \n\t"                 \
         "movgr2fr.d %1, %6  \n\t"                 \
         "movgr2frh.w %2, %6 \n\t"                 \
         "movfr2gr.s %3, %7  \n\t"                 \
         "movfrh2gr.s %4, %7  \n\t"                \
         "movfr2gr.d %5, %7  \n\t"                 \
         : "+f" (res1), "+f" (res2), "+f" (res3),  \
           "+r" (res4), "+r" (res5), "+r" (res6)   \
         : "r" (val), "f" (val)                    \
         : "memory");                              \
      printf("movgr2fr.w ::\n");                   \
      printf("input: %#018lx %#018lx\n", v1, val); \
      printf("output: %#018lx\n", res1);           \
      printf("movgr2fr.d ::\n");                   \
      printf("input: %#018lx %#018lx\n", v2, val); \
      printf("output: %#018lx\n", res2);           \
      printf("movgr2frh.w ::\n");                  \
      printf("input: %#018lx %#018lx\n", v3, val); \
      printf("output: %#018lx\n", res3);           \
      printf("movfr2gr.s ::\n");                   \
      printf("input: %#018lx %#018lx\n", v4, val); \
      printf("output: %#018lx\n", res4);           \
      printf("movfrh2gr.s ::\n");                  \
      printf("input: %#018lx %#018lx\n", v5, val); \
      printf("output: %#018lx\n", res5);           \
      printf("movfr2gr.d ::\n");                   \
      printf("input: %#018lx %#018lx\n", v6, val); \
      printf("output: %#018lx\n", res6);           \
   }

#define TESTINST_FSCR(fcsr, val)            \
   {                                        \
      unsigned long res;                    \
      __asm__ __volatile__(                 \
         "movgr2fcsr " fcsr ", %1 \n\t"     \
         "movfcsr2gr %0, " fcsr " \n\t"     \
         : "=r" (res)                       \
         : "r" (val)                        \
         : "memory");                       \
      printf("movgr2fcsr movfcsr2gr ::\n"); \
      printf("input: %#018lx\n", val);      \
      printf("output: %#018lx\n", res);     \
   }

#define TESTINST_CF(fcc, v1, v2, val)              \
   {                                               \
      unsigned long res1 = (unsigned long)v1;      \
      unsigned long res2 = (unsigned long)v2;      \
      __asm__ __volatile__(                        \
         "movfr2cf " fcc ", %2 \n\t"               \
         "movcf2fr %0, " fcc " \n\t"               \
         "movgr2cf " fcc ", %3 \n\t"               \
         "movcf2gr %1, " fcc " \n\t"               \
         : "+f" (res1), "+r" (res2)                \
         : "f" (val), "r" (val)                    \
         : "memory");                              \
      printf("movfr2cf movcf2fr ::\n");            \
      printf("input: %#018lx %#018lx\n", v1, val); \
      printf("output: %lx\n", res1);               \
      printf("movgr2cf movcf2gr ::\n");            \
      printf("input: %#018lx %#018lx\n", v2, val); \
      printf("output: %lx\n", res2);               \
   }

void test(void)
{
   TESTINST_MOV(0x1234123412341234UL, 0x5678567856785678UL, 0x9abc9abc9abc9abcUL, 0xdef0def0def0def0UL, 0x2468246824682468UL, 0x3579357935793579UL, 0x0123456789abcdefUL);
   TESTINST_MOV(0x1234123412341234UL, 0x5678567856785678UL, 0x9abc9abc9abc9abcUL, 0xdef0def0def0def0UL, 0x2468246824682468UL, 0x3579357935793579UL, 0xfedcba9876543210UL);

   TESTINST_FSCR("$r0", 0x0123456789abcdefUL);
   TESTINST_FSCR("$r0", 0xfedcba9876543210UL);
   TESTINST_FSCR("$r1", 0x0123456789abcdefUL);
   TESTINST_FSCR("$r1", 0xfedcba9876543210UL);
   TESTINST_FSCR("$r2", 0x0123456789abcdefUL);
   TESTINST_FSCR("$r2", 0xfedcba9876543210UL);
   TESTINST_FSCR("$r3", 0x0123456789abcdefUL);
   TESTINST_FSCR("$r3", 0xfedcba9876543210UL);

   TESTINST_CF("$fcc0", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc0", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc1", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc1", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc2", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc2", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc3", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc3", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc4", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc4", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc5", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc5", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc6", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc6", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
   TESTINST_CF("$fcc7", 0x1234123412341234UL, 0x5678567856785678UL, 0xffffffffffffffffUL);
   TESTINST_CF("$fcc7", 0x1234123412341234UL, 0x5678567856785678UL, 0xdef0def0def0def0UL);
}

int main(void)
{
   test();
   return 0;
}
