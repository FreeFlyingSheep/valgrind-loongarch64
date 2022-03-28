#include <stdio.h>

#define TESTINST_HRI(insn, hint, addr, offs)   \
   {                                           \
      __asm__ __volatile__(                    \
         insn " " #hint ", %0, " #offs " \n\t" \
         :                                     \
         : "r" (addr)                          \
         : "memory"                            \
      );                                       \
      printf("test %s\n", insn);               \
   }

#define TESTINST_HRR(insn, hint, addr, offs) \
   {                                         \
      __asm__ __volatile__(                  \
         insn " %0, %1, \n\t"                \
         :                                   \
         : "r" (addr), "r" (offs)            \
         : "memory"                          \
      );                                     \
      printf("test %s\n", insn);             \
   }

#define TESTINST_CODE(insn, code) \
   {                              \
      __asm__ __volatile__(       \
         insn " " #code " \n\t"   \
         :                        \
         :                        \
         : "memory"               \
      );                          \
      printf("test %s\n", insn);  \
   }

#define TESTINST_RR(insn, id)    \
   {                             \
      unsigned long res = 0;     \
      __asm__ __volatile__(      \
         insn " %0, %1 \n\t"     \
         : "+r" (res)            \
         : "r" (id)              \
         : "memory"              \
      );                         \
      printf("test %s\n", insn); \
      printf("res: %ld\n", res); \
   }

unsigned long mem[8];

void test(void)
{
   /* ---------------- preld hint, rj, si12 ---------------- */
   TESTINST_HRI("preld", 0, mem, 0);
   TESTINST_HRI("preld", 1, mem, 1);
   TESTINST_HRI("preld", 2, mem, 2);
   TESTINST_HRI("preld", 3, mem, 3);
   TESTINST_HRI("preld", 4, mem, 4);
   TESTINST_HRI("preld", 5, mem, 5);
   TESTINST_HRI("preld", 6, mem, 6);
   TESTINST_HRI("preld", 7, mem, 7);
   TESTINST_HRI("preld", 8, mem, 8);
   TESTINST_HRI("preld", 9, mem, 9);

   /* ---------------- preldx hint, rj, rk ---------------- */
   TESTINST_HRI("preld", 31, mem, 10);
   TESTINST_HRI("preld", 30, mem, 12);
   TESTINST_HRI("preld", 29, mem, 14);
   TESTINST_HRI("preld", 28, mem, 16);
   TESTINST_HRI("preld", 27, mem, 18);
   TESTINST_HRI("preld", 26, mem, 20);
   TESTINST_HRI("preld", 25, mem, 22);
   TESTINST_HRI("preld", 24, mem, 24);
   TESTINST_HRI("preld", 23, mem, 26);
   TESTINST_HRI("preld", 22, mem, 28);

   /* ---------------- dbar code ---------------- */
   TESTINST_CODE("dbar", 0);
   TESTINST_CODE("dbar", 2);
   TESTINST_CODE("dbar", 4);
   TESTINST_CODE("dbar", 6);
   TESTINST_CODE("dbar", 8);

   /* ---------------- ibar code ---------------- */
   TESTINST_CODE("ibar", 9);
   TESTINST_CODE("ibar", 7);
   TESTINST_CODE("ibar", 5);
   TESTINST_CODE("ibar", 3);
   TESTINST_CODE("ibar", 1);

   /* ---------------- rdtimel.w rd, rj ---------------- */
   TESTINST_RR("rdtimel.w", 0);
   TESTINST_RR("rdtimel.w", 1);
   TESTINST_RR("rdtimel.w", 2);

   /* ---------------- rdtimeh.w rd, rj ---------------- */
   TESTINST_RR("rdtimeh.w", 0);
   TESTINST_RR("rdtimeh.w", 1);
   TESTINST_RR("rdtimeh.w", 2);


   /* ---------------- rdtime.d rd, rj ---------------- */
   TESTINST_RR("rdtime.d", 0);
   TESTINST_RR("rdtime.d", 1);
   TESTINST_RR("rdtime.d", 2);
}

int main(void)
{
   test();
   return 0;
}
