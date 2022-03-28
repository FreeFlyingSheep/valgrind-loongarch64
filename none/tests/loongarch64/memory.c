#include <stdio.h>

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

#define TESTINST_LOAD_RRI(insn, val, addr, offs) \
   {                                             \
      __asm__ __volatile__(                      \
         insn " %0, %1, " #offs " \n\t"          \
         : "=r" (val)                            \
         : "r" (addr)                            \
         : "memory");                            \
   }

#define TESTINST_LOAD_RRR(insn, val, addr, offs) \
   {                                             \
      __asm__ __volatile__(                      \
         insn " %0, %1, %2 \n\t"                 \
         : "=r" (val)                            \
         : "r" (addr), "r" (offs)                \
         : "memory");                            \
   }

#define TESTINST_LOAD_FRI(insn, val, addr, offs) \
   {                                             \
      __asm__ __volatile__(                      \
         insn " %0, %1, " #offs " \n\t"          \
         : "=f" (val)                            \
         : "r" (addr)                            \
         : "memory");                            \
   }

#define TESTINST_LOAD_FRR(insn, val, addr, offs) \
   {                                             \
      __asm__ __volatile__(                      \
         insn " %0, %1, %2 \n\t"                 \
         : "=f" (val)                            \
         : "r" (addr), "r" (offs)                \
         : "memory");                            \
   }

#define TESTINST_STORE_RRI(insn, val, addr, offs) \
   {                                              \
      __asm__ __volatile__(                       \
         insn " %0, %1, " #offs " \n\t"           \
         :                                        \
         : "r" (val), "r" (addr)                  \
         : "memory");                             \
   }

#define TESTINST_STORE_RRR(insn, val, addr, offs) \
   {                                              \
      __asm__ __volatile__(                       \
         insn " %0, %1, %2 \n\t"                  \
         :                                        \
         : "r" (val), "r" (addr), "r" (offs)      \
         : "memory");                             \
   }

#define TESTINST_STORE_FRI(insn, val, addr, offs) \
   {                                              \
      __asm__ __volatile__(                       \
         insn " %0, %1, " #offs " \n\t"           \
         :                                        \
         : "f" (val), "r" (addr)                  \
         : "memory");                             \
   }

#define TESTINST_STORE_FRR(insn, val, addr, offs) \
   {                                              \
      __asm__ __volatile__(                       \
         insn " %0, %1, %2 \n\t"                  \
         :                                        \
         : "f" (val), "r" (addr), "r" (offs)      \
         : "memory");                             \
   }

static inline void show(void)
{
   int i;
   printf("memory block:\n");
   for (i = 0; i < NUM; i++)
      printf("0x%lx:\t%#018lx\n", i * sizeof(unsigned long), mem[i]);
}

void test(void)
{
   char s8;
   unsigned char u8;
   short s16;
   unsigned short u16;
   int s32;
   unsigned int u32;
   long s64;
   unsigned long u64;

   show();

   /* ---------------- ld.b rd, rj, si12 ---------------- */
   printf("test ld.b: ");
   TESTINST_LOAD_RRI("ld.b", s8, mem, 0);
   printf("%d ", (int)s8);
   TESTINST_LOAD_RRI("ld.b", s8, mem, 24);
   printf("%d\n", (int)s8);

   /* ---------------- ld.bu rd, rj, si12 ---------------- */
   printf("test ld.bu: ");
   TESTINST_LOAD_RRI("ld.b", u8, mem, 0);
   printf("%u ", (unsigned)s8);
   TESTINST_LOAD_RRI("ld.b", u8, mem, 24);
   printf("%u\n", (unsigned)s8);

   /* ---------------- ld.h rd, rj, si12 ---------------- */
   printf("test ld.h: ");
   TESTINST_LOAD_RRI("ld.h", s16, mem, 0);
   printf("%hd ", s16);
   TESTINST_LOAD_RRI("ld.h", s16, mem, 24);
   printf("%hd\n", s16);

   /* ---------------- ld.hu rd, rj, si12 ---------------- */
   printf("test ld.hu: ");
   TESTINST_LOAD_RRI("ld.hu", u16, mem, 0);
   printf("%hu ", u16);
   TESTINST_LOAD_RRI("ld.hu", u16, mem, 24);
   printf("%hu\n", u16);

   /* ---------------- ld.w rd, rj, si12 ---------------- */
   printf("test ld.w: ");
   TESTINST_LOAD_RRI("ld.w", s32, mem, 0);
   printf("%d ", s32);
   TESTINST_LOAD_RRI("ld.w", s32, mem, 24);
   printf("%d\n", s32);

   /* ---------------- ld.wu rd, rj, si12 ---------------- */
   printf("test ld.wu: ");
   TESTINST_LOAD_RRI("ld.wu", u32, mem, 0);
   printf("%u ", u32);
   TESTINST_LOAD_RRI("ld.wu", u32, mem, 24);
   printf("%u\n", u32);

   /* ---------------- ld.d rd, rj, si12 ---------------- */
   printf("test ld.d: ");
   TESTINST_LOAD_RRI("ld.d", s64, mem, 0);
   printf("%ld ", s64);
   TESTINST_LOAD_RRI("ld.d", s64, mem, 24);
   printf("%ld ", s64);
   TESTINST_LOAD_RRI("ld.d", u64, mem, 0);
   printf("%lu ", u64);
   TESTINST_LOAD_RRI("ld.d", u64, mem, 24);
   printf("%lu\n", u64);

   /* ---------------- ldx.b rd, rj, rk ---------------- */
   printf("test ldx.b: ");
   TESTINST_LOAD_RRR("ldx.b", s8, mem, 0);
   printf("%d ", (int)s8);
   TESTINST_LOAD_RRR("ldx.b", s8, mem, 24);
   printf("%d\n", (int)s8);

   /* ---------------- ldx.bu rd, rj, rk ---------------- */
   printf("test ldx.bu: ");
   TESTINST_LOAD_RRR("ldx.b", u8, mem, 0);
   printf("%u ", (unsigned)s8);
   TESTINST_LOAD_RRR("ldx.b", u8, mem, 24);
   printf("%u\n", (unsigned)s8);

   /* ---------------- ldx.h rd, rj, rk ---------------- */
   printf("test ldx.h: ");
   TESTINST_LOAD_RRR("ldx.h", s16, mem, 0);
   printf("%hd ", s16);
   TESTINST_LOAD_RRR("ldx.h", s16, mem, 24);
   printf("%hd\n", s16);

   /* ---------------- ldx.hu rd, rj, rk ---------------- */
   printf("test ld.hu: ");
   TESTINST_LOAD_RRR("ldx.hu", u16, mem, 0);
   printf("%hu ", u16);
   TESTINST_LOAD_RRR("ldx.hu", u16, mem, 24);
   printf("%hu\n", u16);

   /* ---------------- ldx.w rd, rj, rk ---------------- */
   printf("test ldx.w: ");
   TESTINST_LOAD_RRR("ldx.w", s32, mem, 0);
   printf("%d ", s32);
   TESTINST_LOAD_RRR("ldx.w", s32, mem, 24);
   printf("%d\n", s32);

   /* ---------------- ldx.wu rd, rj, rk ---------------- */
   printf("test ldx.wu: ");
   TESTINST_LOAD_RRR("ldx.wu", u32, mem, 0);
   printf("%u ", u32);
   TESTINST_LOAD_RRR("ldx.wu", u32, mem, 24);
   printf("%u\n", u32);

   /* ---------------- ldx.d rd, rj, rk ---------------- */
   printf("test ldx.d: ");
   TESTINST_LOAD_RRR("ldx.d", s64, mem, 0);
   printf("%ld ", s64);
   TESTINST_LOAD_RRR("ldx.d", s64, mem, 24);
   printf("%ld ", s64);
   TESTINST_LOAD_RRR("ldx.d", u64, mem, 0);
   printf("%lu ", u64);
   TESTINST_LOAD_RRR("ldx.d", u64, mem, 24);
   printf("%lu\n", u64);

   /* ---------------- ldptr.w rd, rj, si14 ---------------- */
   printf("test ldptr.w: ");
   TESTINST_LOAD_RRI("ldptr.w", s32, mem, 0);
   printf("%d ", s32);
   TESTINST_LOAD_RRI("ldptr.w", s32, mem, 24);
   printf("%d\n", s32);

   /* ---------------- ldptr.d rd, rj, si14 ---------------- */
   printf("test ldptr.d: ");
   TESTINST_LOAD_RRI("ldptr.d", s64, mem, 0);
   printf("%ld ", s64);
   TESTINST_LOAD_RRI("ldptr.d", s64, mem, 24);
   printf("%ld\n", s64);

   /* ---------------- fld.s fd, rj, si12 ---------------- */
   printf("test fld.s: ");
   TESTINST_LOAD_FRI("fld.s", u32, mem, 0);
   printf("%#x ", u32);
   TESTINST_LOAD_FRI("fld.s", u32, mem, 24);
   printf("%#x\n", u32);

   /* ---------------- fld.d fd, rj, si12 ---------------- */
   printf("test fld.d: ");
   TESTINST_LOAD_FRI("fld.d", u64, mem, 0);
   printf("%#lx ", u64);
   TESTINST_LOAD_FRI("fld.d", u64, mem, 24);
   printf("%#lx\n", u64);

   /* ---------------- fldx.s fd, rj, rk ---------------- */
   printf("test fldx.s: ");
   TESTINST_LOAD_FRR("fldx.s", u32, mem, 0);
   printf("%#x ", u32);
   TESTINST_LOAD_FRR("fldx.s", u32, mem, 24);
   printf("%#x\n", u32);

   /* ---------------- fldx.d fd, rj, rk ---------------- */
   printf("test fldx.d: ");
   TESTINST_LOAD_FRR("fldx.d", u64, mem, 0);
   printf("%#lx ", u64);
   TESTINST_LOAD_FRR("fldx.d", u64, mem, 24);
   printf("%#lx\n", u64);

   show();

   u8 = 0xfe;
   s8 = (char)u8;
   u16 = 0xfedc;
   s16 = (short)u16;
   u32 = 0xfedcba98;
   s32 = (int)u32;
   u64 = 0xfedcba9876543210;
   s64 = (long)u64;

   /* ---------------- st.b rd, rj, si12 ---------------- */
   printf("test st.b\n");
   TESTINST_STORE_RRI("st.b", s8, mem, 0);
   TESTINST_STORE_RRI("st.b", u8, mem, 1);

   /* ---------------- st.h rd, rj, si12 ---------------- */
   printf("test st.h\n");
   TESTINST_STORE_RRI("st.h", s16, mem, 2);
   TESTINST_STORE_RRI("st.h", u16, mem, 4);

   /* ---------------- st.w rd, rj, si12 ---------------- */
   printf("test st.w\n");
   TESTINST_STORE_RRI("st.w", s32, mem, 8);
   TESTINST_STORE_RRI("st.w", u32, mem, 12);

   /* ---------------- st.d rd, rj, si12 ---------------- */
   printf("test st.d\n");
   TESTINST_STORE_RRI("st.d", s64, mem, 16);
   TESTINST_STORE_RRI("st.d", u64, mem, 24);

   /* ---------------- stx.b rd, rj, rk ---------------- */
   printf("test stx.b\n");
   TESTINST_STORE_RRR("stx.b", s8, mem, 32);
   TESTINST_STORE_RRR("stx.b", u8, mem, 33);

   /* ---------------- stx.h rd, rj, rk ---------------- */
   printf("test stx.h\n");
   TESTINST_STORE_RRR("stx.h", s16, mem, 34);
   TESTINST_STORE_RRR("stx.h", u16, mem, 36);

   /* ---------------- stx.w rd, rj, rk ---------------- */
   printf("test stx.w\n");
   TESTINST_STORE_RRR("stx.w", s32, mem, 40);
   TESTINST_STORE_RRR("stx.w", u32, mem, 44);

   /* ---------------- stx.d rd, rj, rk ---------------- */
   printf("test stx.d\n");
   TESTINST_STORE_RRR("stx.d", s64, mem, 48);
   TESTINST_STORE_RRR("stx.d", u64, mem, 56);

   /* ---------------- stptr.w rd, rj, si14 ---------------- */
   printf("test stptr.w\n");
   TESTINST_STORE_RRI("stptr.w", s64, mem, 64);
   TESTINST_STORE_RRI("stptr.w", u64, mem, 68);

   /* ---------------- stptr.d rd, rj, si14 ---------------- */
   printf("test stptr.d\n");
   TESTINST_STORE_RRI("stptr.d", s64, mem, 72);
   TESTINST_STORE_RRI("stptr.d", u64, mem, 80);

   /* ---------------- fst.s rd, rj, si12 ---------------- */
   printf("test fst.w\n");
   TESTINST_STORE_FRI("fst.s", u32, mem, 84);
   TESTINST_STORE_FRI("fst.s", u32, mem, 88);

   /* ---------------- fst.d rd, rj, si12 ---------------- */
   printf("test fst.d\n");
   TESTINST_STORE_FRI("fst.d", u64, mem, 96);
   TESTINST_STORE_FRI("fst.d", u64, mem, 104);

   /* ---------------- fstx.s rd, rj, rk ---------------- */
   printf("test fstx.w\n");
   TESTINST_STORE_FRR("fstx.s", u32, mem, 108);
   TESTINST_STORE_FRR("fstx.s", u32, mem, 112);

   /* ---------------- fstx.d rd, rj, rk ---------------- */
   printf("test fstx.d\n");
   TESTINST_STORE_FRR("fstx.d", u64, mem, 120);
   TESTINST_STORE_FRR("fstx.d", u64, mem, 128);

   show();
}

int main(void)
{
   test();
   return 0;
}
