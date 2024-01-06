#include <stdio.h>

#define TESTINST_B_RR(insn, val1, val2)       \
   {                                          \
      int res;                                \
      unsigned long v1 = (unsigned long)val1; \
      unsigned long v2 = (unsigned long)val2; \
      __asm__ __volatile__(                   \
             insn " %1, %2, 1f   \n\t"        \
         "   move %0, $zero      \n\t"        \
         "   b 2f                \n\t"        \
         "1:                     \n\t"        \
         "   addi.w %0, $zero, 1 \n\t"        \
         "2:                     \n\t"        \
         : "=r" (res)                         \
         : "r" (v1), "r" (v2)                 \
         : "memory");                         \
      printf("%s::\n", insn);                 \
      printf("input: %#lx %#lx\n", v1, v2);   \
      printf("output: %d\n", res);            \
   }

#define TESTINST_B_R(insn, val)             \
   {                                        \
      int res;                              \
      unsigned long v = (unsigned long)val; \
      __asm__ __volatile__(                 \
             insn " %1, 1f       \n\t"      \
         "   move %0, $zero      \n\t"      \
         "   b 2f                \n\t"      \
         "1:                     \n\t"      \
         "   addi.w %0, $zero, 1 \n\t"      \
         "2:                     \n\t"      \
         : "=r" (res)                       \
         : "r" (v)                          \
         : "memory");                       \
      printf("%s::\n", insn);               \
      printf("input: %#lx\n", v);           \
      printf("output: %d\n", res);          \
   }

#define TESTINST_B_C(insn, val)             \
   {                                        \
      int res;                              \
      unsigned long v = (unsigned long)val; \
      __asm__ __volatile__(                 \
         "   movgr2cf $fcc0, %1  \n\t"      \
             insn " $fcc0, 1f    \n\t"      \
         "   move %0, $zero      \n\t"      \
         "   b 2f                \n\t"      \
         "1:                     \n\t"      \
         "   addi.w %0, $zero, 1 \n\t"      \
         "2:                     \n\t"      \
         : "=r" (res)                       \
         : "r" (v)                          \
         : "$fcc0", "memory");              \
      printf("%s::\n", insn);               \
      printf("input: %#lx\n", v);           \
      printf("output: %d\n", res);          \
   }

#define TESTINST_BL()               \
   {                                \
      int res;                      \
      __asm__ __volatile__(         \
         "   move %0, $zero   \n\t" \
         "   bl 1f            \n\t" \
         "   addi.w %0, %0, 1 \n\t" \
         "   b  2f            \n\t" \
         "1:                  \n\t" \
         "   addi.w %0, %0, 1 \n\t" \
         "   jr $ra \n\t"           \
         "2:        \n\t"           \
         : "=r" (res)               \
         :                          \
         : "$ra", "memory");        \
      printf("bl::\n");             \
      printf("res: %d\n", res);     \
   }

#define TESTINST_JIRL(insn)                \
   {                                       \
      unsigned long addr1, addr2;          \
      __asm__ __volatile__(                \
         "   pcaddi $t0, 2   \n\t"         \
         "   jirl %0, $t0, 0 \n\t"         \
         "   pcaddi %1, 0    \n\t"         \
         : "=r" (addr1), "=r" (addr2)      \
         :                                 \
         : "$t0", "memory");               \
      printf("jirl::\n");                  \
      printf("res: %d\n", addr1 == addr2); \
   }

void test(void)
{
   /* ---------------- beq rj, rd, offs16 ---------------- */
   TESTINST_B_RR("beq", 1, 2);
   TESTINST_B_RR("beq", 1, 1);

   /* ---------------- bne rj, rd, offs16 ---------------- */
   TESTINST_B_RR("bne", 1, 2);
   TESTINST_B_RR("bne", 1, 1);

   /* ---------------- blt rj, rd, offs16 ---------------- */
   TESTINST_B_RR("blt", 1, 2);
   TESTINST_B_RR("blt", 1, 0);

   /* ---------------- bge rj, rd, offs16 ---------------- */
   TESTINST_B_RR("bge", 1, 2);
   TESTINST_B_RR("bge", 0, 0);

   /* ---------------- bltu rj, rd, offs16 ---------------- */
   TESTINST_B_RR("bltu", -1, 2);
   TESTINST_B_RR("bltu", 0, 1);

   /* ---------------- bgeu rj, rd, offs16 ---------------- */
   TESTINST_B_RR("bgeu", -1, 2);
   TESTINST_B_RR("bgeu", 0, 1);

   /* ---------------- beqz rj, offs21 ---------------- */
   TESTINST_B_R("beqz", 0);
   TESTINST_B_R("beqz", -1);

   /* ---------------- bnez rj, offs21 ---------------- */
   TESTINST_B_R("bnez", 0);
   TESTINST_B_R("bnez", -1);

   /* ---------------- bceqz cj, offs21 ---------------- */
   TESTINST_B_C("bceqz", 0);
   TESTINST_B_C("bceqz", 1);

   /* ---------------- bcnez cj, offs21 ---------------- */
   TESTINST_B_C("bcnez", 0);
   TESTINST_B_C("bcnez", 1);

   /* ---------------- bl offs26 ---------------- */
   TESTINST_BL();

   /* ---------------- jirl rd, rj, offs16 ---------------- */
   TESTINST_JIRL();
}

int main(void)
{
   test();
   return 0;
}
