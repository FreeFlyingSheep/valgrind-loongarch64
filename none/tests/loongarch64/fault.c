#include <stdio.h>
#include <stdbool.h>
#include <signal.h>
#include <setjmp.h>
#include <unistd.h>

#define NUM 24

unsigned long mem[NUM] = {
   0x121f1e1f0000e680, 0x0000000000010700, 0x000000030000e7dc,
   0xffffffff0000b0d0, 0x232f2e2f2ab05fd0, 0x242c2b2b0000b6a0,
   0x252a2e2b0000be80, 0x262d2d2a0000de10, 0x3f343f3e0000df20,
   0x3e353d3c2ab05fe0, 0x363a3c3b0000dfd0, 0x3b373b3a00010300,
   0x0000e680121f1e1f, 0x0001070000000000, 0x0000e7dc00000003,
   0x0000b0d0ffffffff, 0x2ab05fd0232f2e2f, 0x0000b6a0242c2b2b,
   0x0000be80252a2e2b, 0x0000de10262d2d2a, 0x0000df203f343f3e,
   0x2ab05fe03e353d3c, 0x0000dfd0363a3c3b, 0x000103003b373b3a
};

long val1 = 0;
long val2 = 0xfdecba9087654321UL;
char *p = (char *)mem;

#define TESTINST_LOAD_RRR(n, insn, addr1, addr2) \
   void test ## n (void)                         \
   {                                             \
      printf("test %d\n", n);                    \
      printf("%s ::\n", insn);                   \
      __asm__ __volatile__(                      \
         insn " %0, %1, %2 \n\t"                 \
         : "=r" (val1)                           \
         : "r" (addr1), "r" (addr2)              \
         : "memory");                            \
      printf("output: %ld\n", val1);             \
   }

#define TESTINST_STORE_RRR(n, insn, addr1, addr2) \
   void test ## n (void)                          \
   {                                              \
      printf("test %d\n", n);                     \
      printf("%s ::\n", insn);                    \
      printf("input: %ld\n", val2);               \
      __asm__ __volatile__(                       \
         insn " %0, %1, %2 \n\t"                  \
         :                                        \
         : "r" (val2), "r" (addr1), "r" (addr2)   \
         : "memory");                             \
   }

#define TESTINST_RR(n, insn, v1, v2)                  \
   void test ## n (void)                              \
   {                                                  \
      printf("test %d\n", n);                         \
      printf("%s ::\n", insn);                        \
      printf("input: %ld %ld\n", (long)v1, (long)v2); \
      __asm__ __volatile__(                           \
         insn " %0, %1 \n\t"                          \
         :                                            \
         : "r" (v1), "r" (v2)                         \
         : "memory");                                 \
   }

#define TESTINST_I(n, insn, imm)   \
   void test ## n (void)           \
   {                               \
      printf("test %d\n", n);      \
      printf("%s ::\n", insn);     \
      printf("input: %d\n", imm);  \
      __asm__ __volatile__(        \
         insn " " #imm  " \n\t"    \
         :                         \
         :                         \
         : "memory");              \
   }

static sigjmp_buf escape;

static void handler(int sig, siginfo_t *si, void *uc)
{
   fprintf(stderr, "signal: %d\n", sig);
   fprintf(stderr, "code: %d\n", si->si_code);
   siglongjmp(escape, 1);
}

static inline void show(void)
{
   int i;
   printf("memory block:\n");
   for (i = 0; i < NUM; i++)
      printf("0x%lx:\t%#018lx\n", i * sizeof(unsigned long), mem[i]);
}

TESTINST_LOAD_RRR(1,  "ldgt.b", &p[0],  &p[64]);
TESTINST_LOAD_RRR(2,  "ldgt.b", &p[1],  &p[0] );
TESTINST_LOAD_RRR(3,  "ldgt.h", &p[1],  &p[0] );
TESTINST_LOAD_RRR(4,  "ldgt.h", &p[2],  &p[64]);
TESTINST_LOAD_RRR(5,  "ldgt.h", &p[4],  &p[0] );
TESTINST_LOAD_RRR(6,  "ldgt.w", &p[2],  &p[0] );
TESTINST_LOAD_RRR(7,  "ldgt.w", &p[8],  &p[64]);
TESTINST_LOAD_RRR(8,  "ldgt.w", &p[12], &p[0] );
TESTINST_LOAD_RRR(9,  "ldgt.d", &p[4],  &p[0] );
TESTINST_LOAD_RRR(10, "ldgt.d", &p[16], &p[64]);
TESTINST_LOAD_RRR(11, "ldgt.d", &p[32], &p[0] );

TESTINST_LOAD_RRR(12, "ldle.b", &p[64], &p[0] );
TESTINST_LOAD_RRR(13, "ldle.b", &p[65], &p[96]);
TESTINST_LOAD_RRR(14, "ldle.h", &p[65], &p[0] );
TESTINST_LOAD_RRR(15, "ldle.h", &p[66], &p[0] );
TESTINST_LOAD_RRR(16, "ldle.h", &p[68], &p[96]);
TESTINST_LOAD_RRR(17, "ldle.w", &p[66], &p[0] );
TESTINST_LOAD_RRR(18, "ldle.w", &p[72], &p[0] );
TESTINST_LOAD_RRR(19, "ldle.w", &p[76], &p[96]);
TESTINST_LOAD_RRR(20, "ldle.d", &p[68], &p[0] );
TESTINST_LOAD_RRR(21, "ldle.d", &p[80], &p[0] );
TESTINST_LOAD_RRR(22, "ldle.d", &p[88], &p[96]);

TESTINST_STORE_RRR(23, "ldgt.b", &p[0],  &p[64]);
TESTINST_STORE_RRR(24, "ldgt.b", &p[1],  &p[0] );
TESTINST_STORE_RRR(25, "ldgt.h", &p[1],  &p[0] );
TESTINST_STORE_RRR(26, "ldgt.h", &p[2],  &p[64]);
TESTINST_STORE_RRR(27, "ldgt.h", &p[4],  &p[0] );
TESTINST_STORE_RRR(28, "ldgt.w", &p[2],  &p[0] );
TESTINST_STORE_RRR(29, "ldgt.w", &p[8],  &p[64]);
TESTINST_STORE_RRR(30, "ldgt.w", &p[12], &p[0] );
TESTINST_STORE_RRR(31, "ldgt.d", &p[4],  &p[0] );
TESTINST_STORE_RRR(32, "ldgt.d", &p[16], &p[64]);
TESTINST_STORE_RRR(33, "ldgt.d", &p[32], &p[0] );

TESTINST_STORE_RRR(34, "ldle.b", &p[64], &p[0] );
TESTINST_STORE_RRR(35, "ldle.b", &p[65], &p[96]);
TESTINST_STORE_RRR(36, "ldle.h", &p[65], &p[0] );
TESTINST_STORE_RRR(37, "ldle.h", &p[66], &p[0] );
TESTINST_STORE_RRR(38, "ldle.h", &p[68], &p[96]);
TESTINST_STORE_RRR(39, "ldle.w", &p[66], &p[0] );
TESTINST_STORE_RRR(40, "ldle.w", &p[72], &p[0] );
TESTINST_STORE_RRR(41, "ldle.w", &p[76], &p[96]);
TESTINST_STORE_RRR(42, "ldle.d", &p[68], &p[0] );
TESTINST_STORE_RRR(43, "ldle.d", &p[80], &p[0] );
TESTINST_STORE_RRR(44, "ldle.d", &p[88], &p[96]);

TESTINST_RR(45, "asrtle.d", 123, 456);
TESTINST_RR(46, "asrtle.d", 789, 0);
TESTINST_RR(47, "asrtgt.d", 123, 456);
TESTINST_RR(48, "asrtgt.d", 789, 0);

TESTINST_I(49, "break", 0);
TESTINST_I(50, "break", 6);
TESTINST_I(51, "break", 7);
TESTINST_I(52, "break", 100);

struct test {
   void (*func)(void);
   bool show;
} tests[] = {
   { test1,  false },
   { test2,  false },
   { test3,  false },
   { test4,  false },
   { test5,  false },
   { test6,  false },
   { test7,  false },
   { test8,  false },
   { test9,  false },
   { test10, false },
   { test11, true  },
   { test12, false },
   { test13, false },
   { test14, false },
   { test15, false },
   { test16, false },
   { test17, false },
   { test18, false },
   { test19, false },
   { test20, false },
   { test21, false },
   { test22, true  },
   { test23, false },
   { test24, false },
   { test25, false },
   { test26, false },
   { test27, false },
   { test28, false },
   { test29, false },
   { test30, false },
   { test31, false },
   { test32, false },
   { test33, true  },
   { test34, false },
   { test35, false },
   { test36, false },
   { test37, false },
   { test38, false },
   { test39, false },
   { test40, false },
   { test41, false },
   { test42, false },
   { test43, false },
   { test44, true  },
   { test45, false },
   { test46, false },
   { test47, false },
   { test48, false },
   { test49, false },
   { test50, false },
   { test51, false },
   { test52, false }
};

int main(void)
{
   int i;
   struct sigaction sa;
   int sigs[] = { SIGSYS, SIGBUS, SIGFPE, SIGTRAP };

   sa.sa_sigaction = handler;
   sa.sa_flags = SA_SIGINFO;
   sigfillset(&sa.sa_mask);

   for(i = 0; i < sizeof(sigs) / sizeof(sigs[0]); i++)
      sigaction(sigs[i], &sa, NULL);

   show();
   for(i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
      if (sigsetjmp(escape, 1) == 0) {
         fprintf(stderr, "test %d\n", i + 1);
         tests[i].func();
         if (tests[i].show)
            show();
         fprintf(stderr, "no fault\n");
      }
   }

   return 0;
}
