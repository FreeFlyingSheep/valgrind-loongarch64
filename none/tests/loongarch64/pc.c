#include <stdio.h>
#include <stdbool.h>

#define TESTINST_RI(insn, imm, offs, clear)          \
   {                                                 \
      unsigned long res, exp;                        \
      __asm__ __volatile__(                          \
         "  la.local $t0, 1f   \n\t"                 \
         "  jirl %0, $t0, 0    \n\t"                 \
         "1:                   \n\t"                 \
            insn " %1," #imm " \n\t"                 \
         : "=r" (exp), "=r" (res)                    \
         :                                           \
         : "$t0", "memory");                         \
      printf("test %s\n", insn);                     \
      exp += (long)imm << 40 >> (40 - offs);         \
      if (clear)                                     \
         exp &= 0xfffffffffffff000UL;                \
      if (res != exp)                                \
         printf("res: %#lx, exp: %#lx\n", res, exp); \
   }

void test(void)
{
   /* ---------------- pcaddi rd, si20 ---------------- */
   TESTINST_RI("pcaddi", 0, 2, false);
   TESTINST_RI("pcaddi", 1, 2, false);
   TESTINST_RI("pcaddi", 100, 2, false);
   TESTINST_RI("pcaddi", 12345, 2, false);
   TESTINST_RI("pcaddi", -12345, 2, false);
   TESTINST_RI("pcaddi", 524287, 2, false);
   TESTINST_RI("pcaddi", -524288, 2, false);

   /* ---------------- pcaddu12i rd, si20 ---------------- */
   TESTINST_RI("pcaddu12i", 0, 12, false);
   TESTINST_RI("pcaddu12i", 1, 12, false);
   TESTINST_RI("pcaddu12i", 100, 12, false);
   TESTINST_RI("pcaddu12i", 12345, 12, false);
   TESTINST_RI("pcaddu12i", -12345, 12, false);
   TESTINST_RI("pcaddu12i", 524287, 12, false);
   TESTINST_RI("pcaddu12i", -524288, 12, false);

   /* ---------------- pcaddu18i rd, si20 ---------------- */
   TESTINST_RI("pcaddu18i", 0, 18, false);
   TESTINST_RI("pcaddu18i", 1, 18, false);
   TESTINST_RI("pcaddu18i", 100, 18, false);
   TESTINST_RI("pcaddu18i", 12345, 18, false);
   TESTINST_RI("pcaddu18i", -12345, 18, false);
   TESTINST_RI("pcaddu18i", 524287, 18, false);
   TESTINST_RI("pcaddu18i", -524288, 18, false);

   /* ---------------- pcalau12i rd, si20 ---------------- */
   TESTINST_RI("pcalau12i", 0, 12, true);
   TESTINST_RI("pcalau12i", 1, 12, true);
   TESTINST_RI("pcalau12i", 100, 12, true);
   TESTINST_RI("pcalau12i", 12345, 12, true);
   TESTINST_RI("pcalau12i", -12345, 12, true);
   TESTINST_RI("pcalau12i", 524287, 12, true);
   TESTINST_RI("pcalau12i", -524288, 12, true);
}

int main(void)
{
   test();
   return 0;
}
