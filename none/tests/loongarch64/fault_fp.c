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

#define TESTINST_LOAD_FRR_S(n, insn, addr1, addr2) \
   void test ## n (void)                           \
   {                                               \
      printf("test %d\n", n);                      \
      printf("%s ::\n", insn);                     \
      __asm__ __volatile__(                        \
         insn " %0, %1, %2 \n\t"                   \
         : "=f" (val1)                             \
         : "r" (addr1), "r" (addr2)                \
         : "memory");                              \
      printf("output: %d\n", (int)val1);           \
   }

#define TESTINST_LOAD_FRR_D(n, insn, addr1, addr2) \
   void test ## n (void)                           \
   {                                               \
      printf("test %d\n", n);                      \
      printf("%s ::\n", insn);                     \
      __asm__ __volatile__(                        \
         insn " %0, %1, %2 \n\t"                   \
         : "=f" (val1)                             \
         : "r" (addr1), "r" (addr2)                \
         : "memory");                              \
      printf("output: %ld\n", val1);               \
   }

#define TESTINST_STORE_FRR(n, insn, addr1, addr2) \
   void test ## n (void)                          \
   {                                              \
      printf("test %d\n", n);                     \
      printf("%s ::\n", insn);                    \
      printf("input: %ld\n", val2);               \
      __asm__ __volatile__(                       \
         insn " %0, %1, %2 \n\t"                  \
         :                                        \
         : "f" (val2), "r" (addr1), "r" (addr2)   \
         : "memory");                             \
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

TESTINST_LOAD_FRR_S(1,  "fldgt.s", &p[2],  &p[0] );
TESTINST_LOAD_FRR_S(2,  "fldgt.s", &p[8],  &p[64]);
TESTINST_LOAD_FRR_S(3,  "fldgt.s", &p[12], &p[0] );
TESTINST_LOAD_FRR_D(4,  "fldgt.d", &p[4],  &p[0] );
TESTINST_LOAD_FRR_D(5,  "fldgt.d", &p[16], &p[64]);
TESTINST_LOAD_FRR_D(6,  "fldgt.d", &p[32], &p[0] );

TESTINST_LOAD_FRR_S(7,  "fldle.s", &p[66], &p[0] );
TESTINST_LOAD_FRR_S(8,  "fldle.s", &p[72], &p[0] );
TESTINST_LOAD_FRR_S(9,  "fldle.s", &p[76], &p[96]);
TESTINST_LOAD_FRR_D(10, "fldle.d", &p[68], &p[0] );
TESTINST_LOAD_FRR_D(11, "fldle.d", &p[80], &p[0] );
TESTINST_LOAD_FRR_D(12, "fldle.d", &p[88], &p[96]);

TESTINST_STORE_FRR(13, "fstgt.s", &p[2],  &p[0] );
TESTINST_STORE_FRR(14, "fstgt.s", &p[8],  &p[64]);
TESTINST_STORE_FRR(15, "fstgt.s", &p[12], &p[0] );
TESTINST_STORE_FRR(16, "fstgt.d", &p[4],  &p[0] );
TESTINST_STORE_FRR(17, "fstgt.d", &p[16], &p[64]);
TESTINST_STORE_FRR(18, "fstgt.d", &p[32], &p[0] );

TESTINST_STORE_FRR(19, "fstle.s", &p[66], &p[0] );
TESTINST_STORE_FRR(20, "fstle.s", &p[72], &p[0] );
TESTINST_STORE_FRR(21, "fstle.s", &p[76], &p[96]);
TESTINST_STORE_FRR(22, "fstle.d", &p[68], &p[0] );
TESTINST_STORE_FRR(23, "fstle.d", &p[80], &p[0] );
TESTINST_STORE_FRR(24, "fstle.d", &p[88], &p[96]);

struct test {
   void (*func)(void);
   bool show;
} tests[] = {
   { test1,  false },
   { test2,  false },
   { test3,  false },
   { test4,  false },
   { test5,  false },
   { test6,  true  },
   { test7,  false },
   { test8,  false },
   { test9,  false },
   { test10, false },
   { test11, false },
   { test12, true },
   { test13, false },
   { test14, false },
   { test15, false },
   { test16, false },
   { test17, false },
   { test18, true  },
   { test19, false },
   { test20, false },
   { test21, false },
   { test22, false },
   { test23, false },
   { test24, true  }
};

int main(void)
{
   int i;
   struct sigaction sa;
   int sigs[] = { SIGSYS, SIGBUS };

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
