#include <stdio.h>

#define TESTINST_AM(insn, res, val, addr)  \
   {                                       \
      __asm__ __volatile__(                \
         "move $t1, %1        \n\t"        \
         "move $t2, %2        \n\t"        \
         insn " $t0, $t1, $t2 \n\t"        \
         "move %0, $t0        \n\t"        \
         : "=r" (res)                      \
         : "r" (val), "r" (addr)           \
         : "$t0", "$t1", "$t2", "memory"); \
   }

#define TESTINST_AM_4(insn, v)                      \
   {                                                \
      printf(#insn ".w ::\n");                      \
      TESTINST_AM(#insn ".w", res_i, v, &val_i);    \
      printf("old: %d new: %d\n", res_i, val_i);    \
                                                    \
      printf(#insn "_db.w ::\n");                   \
      TESTINST_AM(#insn "_db.w", res_i, v, &val_i); \
      printf("old: %d new: %d\n", res_i, val_i);    \
                                                    \
      printf(#insn ".d ::\n");                      \
      TESTINST_AM(#insn ".d", res_l, v, &val_l);    \
      printf("old: %ld new: %ld\n", res_l, val_l);  \
                                                    \
      printf(#insn "_db.d ::\n");                   \
      TESTINST_AM(#insn "_db.d", res_l, v, &val_l); \
      printf("old: %ld new: %ld\n", res_l, val_l);  \
   }

#define TESTINST_AM_U_4(insn, v)                     \
   {                                                 \
      printf(#insn ".wu ::\n");                      \
      TESTINST_AM(#insn ".wu", res_i, v, &val_i);    \
      printf("old: %u new: %u\n", res_i, val_i);     \
                                                     \
      printf(#insn "_db.wu ::\n");                   \
      TESTINST_AM(#insn "_db.wu", res_i, v, &val_i); \
      printf("old: %u new: %u\n", res_i, val_i);     \
                                                     \
      printf(#insn ".du ::\n");                      \
      TESTINST_AM(#insn ".du", res_l, v, &val_l);    \
      printf("old: %lu new: %lu\n", res_l, val_l);   \
                                                     \
      printf(#insn "_db.du ::\n");                   \
      TESTINST_AM(#insn "_db.du", res_l, v, &val_l); \
      printf("old: %lu new: %lu\n", res_l, val_l);   \
   }

void test(void)
{
   int res_i;
   long res_l;
   int val_i = 1;
   long val_l = 1;

   TESTINST_AM_4(amswap, 2);
   TESTINST_AM_4(amadd, 5);
   TESTINST_AM_4(amand, 3);
   TESTINST_AM_4(amor, 8);
   TESTINST_AM_4(amxor, 4);
   TESTINST_AM_4(ammax, 16);
   TESTINST_AM_4(ammin, -1);
   TESTINST_AM_U_4(ammax, 9);
   TESTINST_AM_U_4(ammin, 6);
}

int main(void)
{
   test();
   return 0;
}
