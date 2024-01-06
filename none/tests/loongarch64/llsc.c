#include <stdio.h>

#define TESTINST_LLSC_W(insn, res, addr, offs) \
   {                                           \
      __asm__ __volatile__(                    \
         "move $t1, %1             \n\t"       \
         "ll.w $t0, $t1, %2        \n\t"       \
         insn "                    \n\t"       \
         "sc.w $t0, $t1, %2        \n\t"       \
         "move %0, $t0             \n\t"       \
         : "=r" (res)                          \
         : "r" (addr), "i" (offs)              \
         : "$t0", "$t1", "memory");            \
   }

#define TESTINST_LLSC_D(insn, res, addr, offs) \
   {                                           \
      __asm__ __volatile__(                    \
         "move $t1, %1             \n\t"       \
         "ll.d $t0, $t1, %2        \n\t"       \
         insn "                    \n\t"       \
         "sc.d $t0, $t1, %2        \n\t"       \
         "move %0, $t0             \n\t"       \
         : "=r" (res)                          \
         : "r" (addr), "i" (offs)              \
         : "$t0", "$t1", "memory");            \
   }

void test(void)
{
   int res_i;
   long res_l;
   int val_i[2] = { 6, 10 };
   long val_l[2] = { 6, 10 };

   /* ---------------- ll.w rd, rj, si14 ---------------- */
   /* ---------------- sc.w rd, rj, si14 ---------------- */
   printf("ll.w sc.w ::\n");

   do {
      TESTINST_LLSC_W("addi.w $t0, $t0, 1", res_i, val_i, 0);
   } while (res_i != 1);
   printf("res: %d val: %d\n", res_i, val_i[0]);

   do {
      TESTINST_LLSC_W("sub.w $t0, $zero, $t0", res_i, val_i, 4);
   } while (res_i != 1);
   printf("res: %d val: %d\n", res_i, val_i[1]);

   /* ---------------- ll.d rd, rj, si14 ---------------- */
   /* ---------------- sc.d rd, rj, si14 ---------------- */
   printf("ll.d sc.d ::\n");

   do {
      TESTINST_LLSC_D("addi.d $t0, $t0, 1", res_l, val_l, 0);
   } while (res_l != 1);
   printf("res: %ld val: %ld\n", res_l, val_l[0]);

   do {
      TESTINST_LLSC_D("sub.d $t0, $zero, $t0", res_l, val_l, 8);
   } while (res_l != 1);
   printf("res: %ld val: %ld\n", res_l, val_l[1]);
}

int main(void)
{
   test();
   return 0;
}
