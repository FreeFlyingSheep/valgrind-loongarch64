#include <stdio.h>
#include <assert.h>

typedef enum {
   SA2, SA2_1 /* for alsl */, SA3,
   MSBW, LSBW, MSBD, LSBD,
   UI5, UI6, UI12,
   SI12, SI14, SI16, SI20
} imm_t;

static inline void showImm (unsigned int i, imm_t ty)
{
   switch (ty) {
      case SA2:
         assert(i < (1 << 2));
         break;
      case SA2_1:
         assert(i < (1 << 3));
         break;
      case SA3:
         assert(i < (1 << 3));
         break;
      case MSBW:
         assert(i < (1 << 5));
         break;
      case LSBW:
         assert(i < (1 << 5));
         break;
      case MSBD:
         assert(i < (1 << 6));
         break;
      case LSBD:
         assert(i < (1 << 6));
         break;
      case UI5:
         assert(i < (1 << 5));
         break;
      case UI6:
         assert(i < (1 << 6));
         break;
      case UI12:
         assert(i < (1 << 12));
         break;
      case SI12:
         assert(i < (1 << 12) || (i >> 12) == 0xfffff);
         break;
      case SI14:
         assert(i < (1 << 14) || (i >> 14) == 0x3ffff);
         break;
      case SI16:
         assert(i < (1 << 16) || (i >> 16) == 0xffff);
         break;
      case SI20:
         assert(i < (1 << 20) || (i >> 20) == 0xfff);
         break;
      default:
         assert(0);
         break;
   }
   printf("%d", i);
}

#define TESTINST_RR(insn, rd, rj, v1, v2)         \
   {                                              \
      unsigned long res1, res2;                   \
      unsigned long val1 = (unsigned long)v1;     \
      unsigned long val2 = (unsigned long)v2;     \
      __asm__ __volatile__(                       \
         "move " rd ", %2      \n\t"              \
         "move " rj ", %3      \n\t"              \
         insn " " rd ", " rj " \n\t"              \
         "move %0, " rd "      \n\t"              \
         "move %1, " rj "      \n\t"              \
         : "=r" (res1), "=r" (res2)               \
         : "r" (val1), "r" (val2)                 \
         : rd, rj, "memory");                     \
      printf("%s %s, %s ::\n", insn, rd, rj);     \
      printf("before: %s=%#018lx, %s=%#018lx\n",  \
             rd, val1, rj, val2);                 \
      printf("after:  %s=%#018lx, %s=%#018lx\n",  \
             rd, res1, rj, res2);                 \
   }

#define TESTINST_RI(insn, rd, type, v1, imm)    \
   {                                            \
      unsigned long res1;                       \
      unsigned long val1 = (unsigned long)v1;   \
      __asm__ __volatile__(                     \
         "move " rd ", %1       \n\t"           \
         insn " " rd ", " #imm "\n\t"           \
         "move %0, " rd "       \n\t"           \
         : "=r" (res1)                          \
         : "r" (val1)                           \
         : rd, "memory");                       \
      printf("%s %s, ", insn, rd);              \
      showImm(imm, type);                       \
      printf(" ::\n");                          \
      printf("before: %s=%#018lx\n", rd, val1); \
      printf("after:  %s=%#018lx\n", rd, res1); \
   }

#define TESTINST_RRRI(insn, rd, rj, rk, type, v1, v2, v3, imm) \
   {                                                           \
      unsigned long res1, res2, res3;                          \
      unsigned long val1 = (unsigned long)v1;                  \
      unsigned long val2 = (unsigned long)v2;                  \
      unsigned long val3 = (unsigned long)v3;                  \
      __asm__ __volatile__(                                    \
         "move " rd ", %3                       \n\t"          \
         "move " rj ", %4                       \n\t"          \
         "move " rk ", %5                       \n\t"          \
         insn " " rd ", " rj ", " rk ", " #imm "\n\t"          \
         "move %0, " rd "                       \n\t"          \
         "move %1, " rj "                       \n\t"          \
         "move %2, " rk "                       \n\t"          \
         : "=r" (res1), "=r" (res2), "=r" (res3)               \
         : "r" (val1), "r" (val2), "r" (val3)                  \
         : rd, rj, rk, "memory");                              \
      printf("%s %s, %s, %s, ", insn, rd, rj, rk);             \
      showImm(imm, type);                                      \
      printf(" ::\n");                                         \
      printf("before: %s=%#018lx, %s=%#018lx, %s=%#018lx\n",   \
             rd, val1, rj, val2, rk, val3);                    \
      printf("after:  %s=%#018lx, %s=%#018lx, %s=%#018lx\n",   \
             rd, res1, rj, res2, rk, res3);                    \
   }

#define TESTINST_RRR(insn, rd, rj, rk, v1, v2, v3)           \
   {                                                         \
      unsigned long res1, res2, res3;                        \
      unsigned long val1 = (unsigned long)v1;                \
      unsigned long val2 = (unsigned long)v2;                \
      unsigned long val3 = (unsigned long)v3;                \
      __asm__ __volatile__(                                  \
         "move " rd ", %3             \n\t"                  \
         "move " rj ", %4             \n\t"                  \
         "move " rk ", %5             \n\t"                  \
         insn " " rd ", " rj ", " rk "\n\t"                  \
         "move %0, " rd "             \n\t"                  \
         "move %1, " rj "             \n\t"                  \
         "move %2, " rk "             \n\t"                  \
         : "=r" (res1), "=r" (res2), "=r" (res3)             \
         : "r" (val1), "r" (val2), "r" (val3)                \
         : rd, rj, rk, "memory");                            \
      printf("%s %s, %s, %s ::\n", insn, rd, rj, rk);        \
      printf("before: %s=%#018lx, %s=%#018lx, %s=%#018lx\n", \
             rd, val1, rj, val2, rk, val3);                  \
      printf("after:  %s=%#018lx, %s=%#018lx, %s=%#018lx\n", \
             rd, res1, rj, res2, rk, res3);                  \
   }

#define TESTINST_RRI(insn, rd, rj, type, v1, v2, imm) \
   {                                                  \
      unsigned long res1, res2;                       \
      unsigned long val1 = (unsigned long)v1;         \
      unsigned long val2 = (unsigned long)v2;         \
      __asm__ __volatile__(                           \
         "move " rd ", %2               \n\t"         \
         "move " rj ", %3               \n\t"         \
         insn " " rd ", " rj ", " #imm "\n\t"         \
         "move %0, " rd "               \n\t"         \
         "move %1, " rj "               \n\t"         \
         : "=r" (res1), "=r" (res2)                   \
         : "r" (val1), "r" (val2)                     \
         : rd, rj, "memory");                         \
      printf("%s %s, %s, ", insn, rd, rj);            \
      showImm(imm, type);                             \
      printf(" ::\n");                                \
      printf("before: %s=%#018lx, %s=%#018lx\n",      \
             rd, val1, rj, val2);                     \
      printf("after:  %s=%#018lx, %s=%#018lx\n",      \
             rd, res1, rj, res2);                     \
   }

#define TESTINST_RRII(insn, rd, rj, type1, type2, v1, v2, imm1, imm2) \
   {                                                                  \
      unsigned long res1, res2;                                       \
      unsigned long val1 = (unsigned long)v1;                         \
      unsigned long val2 = (unsigned long)v2;                         \
      __asm__ __volatile__(                                           \
         "move " rd ", %2                           \n\t"             \
         "move " rj ", %3                           \n\t"             \
         insn " " rd ", " rj ", " #imm1 ", " #imm2 "\n\t"             \
         "move %0, " rd "                           \n\t"             \
         "move %1, " rj "                           \n\t"             \
         : "=r" (res1), "=r" (res2)                                   \
         : "r" (val1), "r" (val2)                                     \
         : rd, rj, "memory");                                         \
      printf("%s %s, %s, ", insn, rd, rj);                            \
      showImm(imm1, type1);                                           \
      printf(", ");                                                   \
      showImm(imm2, type2);                                           \
      printf(" ::\n");                                                \
      printf("before: %s=%#018lx, %s=%#018lx\n",                      \
             rd, val1, rj, val2);                                     \
      printf("after:  %s=%#018lx, %s=%#018lx\n",                      \
             rd, res1, rj, res2);                                     \
   }

void test(void)
{
   /* ---------------- add.w rd, rj, rk ---------------- */
   TESTINST_RRR("add.w", "$r19", "$r20", "$r25", 0xf7f01ffbc9696094UL, 0xb664b1ce21c8c7fcUL, 0xd0a02b79ace85cfUL);
   TESTINST_RRR("add.w", "$r29", "$r9", "$r12", 0x5418cd6f6b640953UL, 0x6465907ca2dac58cUL, 0xefea76d0d526df3aUL);
   TESTINST_RRR("add.w", "$r23", "$r15", "$r28", 0x6ae34fbc6f2f7a9aUL, 0xbf21c48ab5c2edccUL, 0x24824ebd458ed20eUL);
   TESTINST_RRR("add.w", "$r27", "$r14", "$r26", 0x9f33e38db05616ccUL, 0xf12ee0c276c52c78UL, 0xc3054d65ecec3fe6UL);
   TESTINST_RRR("add.w", "$r14", "$r23", "$r27", 0x17eaa07c4607901fUL, 0xa5fa9d0c8472848eUL, 0xa34301227bb57f76UL);
   TESTINST_RRR("add.w", "$r19", "$r19", "$r4", 0xd2e0644d9532b5eaUL, 0x2957c6f0638238bcUL, 0xf01566d0031ee917UL);
   TESTINST_RRR("add.w", "$r19", "$r26", "$r13", 0x7b39b3f2ccbdaf79UL, 0xee877221beef9d45UL, 0x4a743034eefe075dUL);
   TESTINST_RRR("add.w", "$r29", "$r18", "$r14", 0x95214c4de7e6d3baUL, 0x26502eb481799cd1UL, 0x34d57b775083fb91UL);
   TESTINST_RRR("add.w", "$r16", "$r26", "$r8", 0xb66b18865bbb3036UL, 0x8881ccbe1e31aa8dUL, 0xffe0d2dde8325edcUL);
   TESTINST_RRR("add.w", "$r26", "$r5", "$r8", 0xc367af71c905540cUL, 0xcdcbe4860d983fe3UL, 0x6687aa19ee1fc503UL);

   /* ---------------- add.d rd, rj, rk ---------------- */
   TESTINST_RRR("add.d", "$r16", "$r18", "$r8", 0xbe5505b409ce995cUL, 0x561a85fd57e87226UL, 0x923f3293987edab0UL);
   TESTINST_RRR("add.d", "$r12", "$r7", "$r29", 0xff2682151edc3476UL, 0x90beb037eacfe3dbUL, 0xa4017082880f1151UL);
   TESTINST_RRR("add.d", "$r31", "$r31", "$r5", 0x81e38385e39d9f16UL, 0xedb2ffa50c0c8b5fUL, 0x8776f30d75fc97c2UL);
   TESTINST_RRR("add.d", "$r31", "$r6", "$r26", 0x64ff385d97b60dc2UL, 0x80f903f206f08f60UL, 0x4f5b589532e85398UL);
   TESTINST_RRR("add.d", "$r25", "$r10", "$r20", 0xdd8973d6b99634caUL, 0x34c0fe5a72dd43d9UL, 0x2494af03cf5878e7UL);
   TESTINST_RRR("add.d", "$r5", "$r10", "$r4", 0x94b272b05ffe39c8UL, 0x152d15efbbc54c04UL, 0x25afc06cf151ab29UL);
   TESTINST_RRR("add.d", "$r19", "$r30", "$r18", 0xa6e14d42459cadf6UL, 0x558620ff616141b1UL, 0x1978905697120747UL);
   TESTINST_RRR("add.d", "$r7", "$r8", "$r20", 0x2ea6f88031a29aeUL, 0x6a08c12301e00d49UL, 0xdd533acf17f59142UL);
   TESTINST_RRR("add.d", "$r24", "$r14", "$r26", 0xb88df6b8315eb7a6UL, 0x137d04f7f6fe285UL, 0x2ccb253ff7ea93d6UL);
   TESTINST_RRR("add.d", "$r7", "$r19", "$r23", 0xad464722c0967f28UL, 0x30295c1fd85ae029UL, 0x2c69edb227e01d94UL);

   /* ---------------- sub.w rd, rj, rk ---------------- */
   TESTINST_RRR("sub.w", "$r16", "$r28", "$r17", 0x8b0ba4ef20207fddUL, 0x90493cb39ff734a2UL, 0x519842bab5cc1208UL);
   TESTINST_RRR("sub.w", "$r6", "$r13", "$r15", 0x13af983aafc53691UL, 0x27bc6a037865e47fUL, 0xe20df003930575d5UL);
   TESTINST_RRR("sub.w", "$r8", "$r19", "$r23", 0x4177aec74585d42dUL, 0xba89b6aa9b7728acUL, 0xe6a089b8eaf43feUL);
   TESTINST_RRR("sub.w", "$r7", "$r10", "$r23", 0xca1b83a7ab88912UL, 0xd5e2759ea82c2c80UL, 0x76e9d6f88c2624ffUL);
   TESTINST_RRR("sub.w", "$r19", "$r24", "$r24", 0x99d63505ea0474b3UL, 0x1b53c4c34957af8eUL, 0x6146da47b731d3edUL);
   TESTINST_RRR("sub.w", "$r26", "$r31", "$r7", 0x8eca560d8234ff55UL, 0x5beb18985c3f451eUL, 0x9c9634dfaa7b9313UL);
   TESTINST_RRR("sub.w", "$r29", "$r16", "$r6", 0x229544d2cb1d5a64UL, 0xd23751d515597128UL, 0xa09dd29330aa8d15UL);
   TESTINST_RRR("sub.w", "$r12", "$r16", "$r4", 0x229f5aefe9fb7fb7UL, 0x740ed49b5e95faeUL, 0xbc6304a0df442807UL);
   TESTINST_RRR("sub.w", "$r30", "$r29", "$r26", 0x94f3a67d188df281UL, 0x48e066cdad20ac2UL, 0x1e032e60568554a7UL);
   TESTINST_RRR("sub.w", "$r18", "$r23", "$r25", 0xedb4f44fb338ba4fUL, 0xf06e698cd08c8e7bUL, 0xa22b91e88b77d4d8UL);

   /* ---------------- sub.d rd, rj, rk ---------------- */
   TESTINST_RRR("sub.d", "$r18", "$r10", "$r27", 0x68647aa06a23c8f9UL, 0xd001cb46cb78fc4fUL, 0x460cc8702b1761f9UL);
   TESTINST_RRR("sub.d", "$r7", "$r24", "$r18", 0x8d18e952fb747f43UL, 0x1e7d1a019fb96490UL, 0xb466fb9891e8c151UL);
   TESTINST_RRR("sub.d", "$r4", "$r16", "$r27", 0x5f6647277ca4c99dUL, 0xa1156b863ec98e1dUL, 0xc15612f3ce819d64UL);
   TESTINST_RRR("sub.d", "$r4", "$r25", "$r9", 0xe67b33778df480b4UL, 0xc24b2711be7e4ef1UL, 0xd940ca25b956100fUL);
   TESTINST_RRR("sub.d", "$r5", "$r12", "$r18", 0x258ae461ef798ce7UL, 0x3f4984ea3f5692deUL, 0x99fa673f30e69019UL);
   TESTINST_RRR("sub.d", "$r13", "$r10", "$r9", 0xdafb48debea5211eUL, 0xeac1d3b25f6bf8dbUL, 0x297d671b1c96e48fUL);
   TESTINST_RRR("sub.d", "$r7", "$r15", "$r23", 0xc6b03274ff37baf6UL, 0x5b37ffc2c84aec9UL, 0x74d62a52cbaaec15UL);
   TESTINST_RRR("sub.d", "$r26", "$r18", "$r26", 0x35c71e0956ffcd43UL, 0xad703a4e8078070bUL, 0x634924e8a9fdbb9eUL);
   TESTINST_RRR("sub.d", "$r16", "$r29", "$r5", 0x18bf961cba922928UL, 0x54ed9198405f8983UL, 0x977f5b65e5f86b4aUL);
   TESTINST_RRR("sub.d", "$r31", "$r28", "$r14", 0xa38a1e8cb3c7ba00UL, 0xd220d1ef3cf8f3f7UL, 0xc972df2ace170d61UL);

   /* ---------------- slt rd, rj, rk ---------------- */
   TESTINST_RRR("slt", "$r12", "$r17", "$r18", 0xd7a0e65c279e1082UL, 0x819edf00a849ba44UL, 0x41a0b2fe37d44db2UL);
   TESTINST_RRR("slt", "$r31", "$r13", "$r18", 0x2ef00a5cfd100f71UL, 0x4792cd9f9abf36d3UL, 0x2c117902110ef9a8UL);
   TESTINST_RRR("slt", "$r4", "$r30", "$r29", 0x6d8be2fb73e2c006UL, 0xf76ce97d7658995eUL, 0x3856e09bfe39df6eUL);
   TESTINST_RRR("slt", "$r4", "$r18", "$r10", 0xeddcb9dcf092c3f5UL, 0xe57b7c25d13dea8UL, 0x761d86b48cb5ce21UL);
   TESTINST_RRR("slt", "$r16", "$r18", "$r16", 0xcddd92e2340cd593UL, 0xc9a30f4707743f80UL, 0x3ff7d36f17396d3aUL);
   TESTINST_RRR("slt", "$r6", "$r14", "$r10", 0xa9e71c6376093499UL, 0x26bb3955b588461fUL, 0xfae7e7a950447826UL);
   TESTINST_RRR("slt", "$r19", "$r4", "$r17", 0x35bb27f64ebd7d62UL, 0x4a7d3941ebf88bc1UL, 0xcda32e4b1c1d5c4UL);
   TESTINST_RRR("slt", "$r19", "$r28", "$r15", 0x29419b8261e40b99UL, 0xe7e9b059033afa7dUL, 0x1ea916293b1cc3ddUL);
   TESTINST_RRR("slt", "$r31", "$r16", "$r16", 0xe0fb75047bc62c9aUL, 0xa634f6174dcced7dUL, 0xcca5a9d25b670e70UL);
   TESTINST_RRR("slt", "$r4", "$r4", "$r10", 0x724ee03fb3fcdec8UL, 0xae2587f097065e2cUL, 0x65c69548f83dd0dfUL);

   /* ---------------- sltu rd, rj, rk ---------------- */
   TESTINST_RRR("sltu", "$r14", "$r10", "$r13", 0x1956e5498db3fb6eUL, 0x2d909abfec4490bdUL, 0xa7d554ebe591d5ccUL);
   TESTINST_RRR("sltu", "$r6", "$r5", "$r18", 0xc34214447a064eb8UL, 0xad4413e45f0a226aUL, 0x4b09aab500b04bffUL);
   TESTINST_RRR("sltu", "$r31", "$r17", "$r17", 0x86e16a1618a639c4UL, 0x87917b281cef8df0UL, 0xd543115a56dee48UL);
   TESTINST_RRR("sltu", "$r20", "$r6", "$r25", 0x164fff47b8b23752UL, 0x9ad830d46b1660f6UL, 0xc5d72c146f4aba72UL);
   TESTINST_RRR("sltu", "$r6", "$r26", "$r7", 0x1428360430b7c9b5UL, 0xc2052dc6eea5a53cUL, 0xda1a8e35dd060adfUL);
   TESTINST_RRR("sltu", "$r19", "$r15", "$r26", 0xdfc9984966167604UL, 0xa9ea12b5a37dd492UL, 0x7a24be9fcf349afcUL);
   TESTINST_RRR("sltu", "$r29", "$r26", "$r29", 0x5a3822db2cc26fc5UL, 0x5985f02e77511d80UL, 0x370f15cc98f2a6c1UL);
   TESTINST_RRR("sltu", "$r7", "$r28", "$r16", 0xe4594ee2cc8c6d7UL, 0x177ac0014f5dd20UL, 0xde1724c7590a4908UL);
   TESTINST_RRR("sltu", "$r8", "$r12", "$r4", 0x1df979e50aa0ed18UL, 0x5b410cd0985fce18UL, 0x9d3c39d61e29025dUL);
   TESTINST_RRR("sltu", "$r30", "$r23", "$r25", 0x1cba022788d49d13UL, 0xd2b40941478ee865UL, 0xa503a74e41535830UL);

   /* ---------------- slti rd, rj, si12 ---------------- */
   TESTINST_RRI("slti", "$r15", "$r27", SI12, 0xe24c4ca567d1d5f4UL, 0xfef05a88adf4b892UL, 1913);
   TESTINST_RRI("slti", "$r8", "$r31", SI12, 0xfba7284a8ab83b2dUL, 0xff63b80173f1e368UL, -738);
   TESTINST_RRI("slti", "$r31", "$r31", SI12, 0xb4599a9fa734365aUL, 0x4327139de75dde1eUL, -1544);
   TESTINST_RRI("slti", "$r5", "$r4", SI12, 0xa5572272e0c04a20UL, 0x87657c1b1699936bUL, 1529);
   TESTINST_RRI("slti", "$r10", "$r28", SI12, 0x1260731618214410UL, 0xd0de0dfbafb7960aUL, 557);
   TESTINST_RRI("slti", "$r5", "$r12", SI12, 0x4c6317772a4b06b0UL, 0x7a1d4eeb507d649bUL, -222);
   TESTINST_RRI("slti", "$r4", "$r31", SI12, 0x23b4d62a21994afbUL, 0x85304cc393f6506bUL, 717);
   TESTINST_RRI("slti", "$r18", "$r26", SI12, 0x67b6f5dbf6a0c55dUL, 0x451013f9a2337f9fUL, 730);
   TESTINST_RRI("slti", "$r25", "$r8", SI12, 0xdb278cca57f1ad7bUL, 0x7371a60f5af6334bUL, 1193);
   TESTINST_RRI("slti", "$r17", "$r24", SI12, 0xffa3ed31f9ea3a29UL, 0x1138e06e1a45c4f3UL, 329);

   /* ---------------- sltui rd, rj, si12 ---------------- */
   TESTINST_RRI("sltui", "$r13", "$r26", SI12, 0x62677116040aebffUL, 0xeedd6ccd0e5e2771UL, -462);
   TESTINST_RRI("sltui", "$r24", "$r28", SI12, 0xef9500b68a87984aUL, 0xaf5922683f40599dUL, 1890);
   TESTINST_RRI("sltui", "$r9", "$r6", SI12, 0x9996aa21d2b51922UL, 0xd5214fb275e738dcUL, -1538);
   TESTINST_RRI("sltui", "$r19", "$r26", SI12, 0x3eb2777655f0f1c5UL, 0x98ed915860f0eb26UL, -215);
   TESTINST_RRI("sltui", "$r8", "$r19", SI12, 0x5c44b5807c43724cUL, 0x63a068026b529b03UL, -780);
   TESTINST_RRI("sltui", "$r19", "$r17", SI12, 0xf6926016cdbfacc1UL, 0xec04a9bcc8d1192aUL, -1041);
   TESTINST_RRI("sltui", "$r26", "$r14", SI12, 0x542f05c795aa07c2UL, 0xb634bf537df4c4ceUL, 1653);
   TESTINST_RRI("sltui", "$r8", "$r5", SI12, 0x371daf74e330ee8bUL, 0xedb0321c888ae22eUL, 441);
   TESTINST_RRI("sltui", "$r25", "$r4", SI12, 0xba813c7acc8f5621UL, 0x8d5ce4750fe7603bUL, 678);
   TESTINST_RRI("sltui", "$r17", "$r15", SI12, 0x199b641cefe0a0e2UL, 0x7ea0508a3fed3453UL, 2019);

   /* ---------------- nor rd, rj, rk ---------------- */
   TESTINST_RRR("nor", "$r14", "$r28", "$r9", 0xccf23cf02a48844dUL, 0x2608ea0069c4e9ddUL, 0x1c7a04255a2d13f8UL);
   TESTINST_RRR("nor", "$r6", "$r30", "$r4", 0xbfcc3de6da2483beUL, 0xd24e9abca28d6cb5UL, 0xbb01b508523673c6UL);
   TESTINST_RRR("nor", "$r6", "$r28", "$r13", 0x28dacd828d5736d7UL, 0xb365ff31474f736cUL, 0x593621c0f82b445cUL);
   TESTINST_RRR("nor", "$r24", "$r16", "$r31", 0x5898010a4c6cf1bbUL, 0xecac6e093ba6146aUL, 0x50e6093f19b1194UL);
   TESTINST_RRR("nor", "$r15", "$r7", "$r20", 0x2ddb1dea334fd92aUL, 0x401d7a663be0b31aUL, 0xb6c008973a85f779UL);
   TESTINST_RRR("nor", "$r18", "$r31", "$r29", 0xc987982e1d91684UL, 0x181f20f581ed38f4UL, 0xefaa786e00a2e5b9UL);
   TESTINST_RRR("nor", "$r19", "$r31", "$r13", 0x39e476d555cd20bcUL, 0xfb8fab5d35576d50UL, 0x71a92a8377c0f729UL);
   TESTINST_RRR("nor", "$r25", "$r7", "$r5", 0x7f36d0c6d173e8c8UL, 0x181763a9f9350680UL, 0x5ec5099605d7d418UL);
   TESTINST_RRR("nor", "$r30", "$r23", "$r23", 0x688e1d04976ac8dbUL, 0xd37b6d6a1c510287UL, 0x8670301ee2a715dfUL);
   TESTINST_RRR("nor", "$r5", "$r23", "$r14", 0x71c4a211dd9262f4UL, 0xcb8a4aebc2c6c4f2UL, 0x84d79a5254447c9UL);

   /* ---------------- and rd, rj, rk ---------------- */
   TESTINST_RRR("and", "$r8", "$r14", "$r31", 0xbddf22c4109e20b5UL, 0xb2d25973efd1a8ffUL, 0x28b78b59dfe641e9UL);
   TESTINST_RRR("and", "$r19", "$r23", "$r17", 0xb25e185c549f6661UL, 0xb6ccc215c2f17718UL, 0xf20669c51aee8ffeUL);
   TESTINST_RRR("and", "$r30", "$r27", "$r23", 0xa7f4ad796393e12bUL, 0xefbcf405df3e7affUL, 0x548a0141e9fe1700UL);
   TESTINST_RRR("and", "$r18", "$r31", "$r29", 0xa399c7f46c61d974UL, 0xe0fe8cca1cbab773UL, 0x49e680ddee7f666bUL);
   TESTINST_RRR("and", "$r5", "$r26", "$r25", 0x1682ca17c11f90acUL, 0x4e9706cb2c885742UL, 0x250ff6304dd87d57UL);
   TESTINST_RRR("and", "$r28", "$r14", "$r8", 0xcacf15e6ffad256fUL, 0x99527f4fa2aa8fb1UL, 0xcff546a883b63cfbUL);
   TESTINST_RRR("and", "$r28", "$r9", "$r28", 0xc60423b9cf70d112UL, 0x2fb0db47f1d8f166UL, 0x1e9cec9d13e85210UL);
   TESTINST_RRR("and", "$r18", "$r28", "$r5", 0x5059c37ee38d2f25UL, 0x74bf57d85d90af3aUL, 0x35479df0ebec9209UL);
   TESTINST_RRR("and", "$r23", "$r25", "$r12", 0x18742ef4c73416beUL, 0x8b93e775860ef52bUL, 0xa909915f60a546d2UL);
   TESTINST_RRR("and", "$r18", "$r17", "$r24", 0xadb2cc6aec909946UL, 0x3068f8b21d583e4cUL, 0xcf8aae1918f3a88eUL);

   /* ---------------- or rd, rj, rk ---------------- */
   TESTINST_RRR("or", "$r19", "$r28", "$r25", 0x46819825f87044c2UL, 0x65cb2cc7e5f5a720UL, 0x1fc0130146f13f76UL);
   TESTINST_RRR("or", "$r8", "$r25", "$r4", 0x45083dd59c60e6feUL, 0x936ecfaeb4d51c95UL, 0xdc37c27c69024f6eUL);
   TESTINST_RRR("or", "$r15", "$r16", "$r8", 0x516659e51cf19b26UL, 0x7589da0802d59510UL, 0x6b713c60390f3fbfUL);
   TESTINST_RRR("or", "$r9", "$r15", "$r6", 0x1646568625c40022UL, 0xa68db9141a88850cUL, 0x756d912fbefef973UL);
   TESTINST_RRR("or", "$r24", "$r9", "$r25", 0xda34c24d14fce443UL, 0x6ad9bf24481630b0UL, 0x2aefcdfa652395bUL);
   TESTINST_RRR("or", "$r13", "$r9", "$r14", 0x900358ad1e848728UL, 0xa0e361b5b891a62eUL, 0xddfa0c1377ce01acUL);
   TESTINST_RRR("or", "$r23", "$r16", "$r15", 0x27a55515d39aded9UL, 0xd0daf17f9cb0bf5aUL, 0xf44c4372982c4d74UL);
   TESTINST_RRR("or", "$r20", "$r16", "$r16", 0x7045887bb8325d6fUL, 0xbac771cbb78dae04UL, 0x23f4928023125a5cUL);
   TESTINST_RRR("or", "$r30", "$r5", "$r7", 0xcf609aa2057d1b98UL, 0x379641544fd1cd48UL, 0x5275ef34f265f01aUL);
   TESTINST_RRR("or", "$r23", "$r4", "$r30", 0xc43fc1c750887406UL, 0x44a3229c33d1cd65UL, 0xceaa00084fc04912UL);

   /* ---------------- xor rd, rj, rk ---------------- */
   TESTINST_RRR("xor", "$r6", "$r19", "$r31", 0x18522418b59bf8aUL, 0x270a2ec823f26e39UL, 0x99ef76e6d4495ae3UL);
   TESTINST_RRR("xor", "$r28", "$r20", "$r27", 0x57de83cac9dade15UL, 0xd39fdecdfd4ccb08UL, 0xc97b854adacdb4UL);
   TESTINST_RRR("xor", "$r4", "$r29", "$r5", 0x9f7356fff2445f77UL, 0xc3c3a34d2c226b5aUL, 0x51abdd266816b94fUL);
   TESTINST_RRR("xor", "$r14", "$r6", "$r28", 0xdd5ca0b5c6c45804UL, 0xa0ba047990ec0798UL, 0x89e6efd43651c28UL);
   TESTINST_RRR("xor", "$r8", "$r19", "$r23", 0xc3e35cd44af166faUL, 0x6affcfe12104ccc7UL, 0x4adbb3601a07a1d9UL);
   TESTINST_RRR("xor", "$r16", "$r5", "$r18", 0x685cdc5ca969c8e1UL, 0xd88d0e2a9900b8ebUL, 0xdd4dfbba723cde28UL);
   TESTINST_RRR("xor", "$r20", "$r18", "$r24", 0x2362838018fa39beUL, 0xbbc8d438b24c037aUL, 0xe020a8456a45b667UL);
   TESTINST_RRR("xor", "$r19", "$r23", "$r19", 0x637cae50fc0a1c95UL, 0x514b81a7227dd07eUL, 0x59a27a7f9c8481c3UL);
   TESTINST_RRR("xor", "$r20", "$r16", "$r18", 0xb728dd7a443bcc8fUL, 0xe2de9bf67cdbdc0cUL, 0x26687435fbe4dbf6UL);
   TESTINST_RRR("xor", "$r23", "$r14", "$r6", 0x744915919b52e27eUL, 0x16863c1d3e1cded7UL, 0x40ce8607349c380UL);

   /* ---------------- orn rd, rj, rk ---------------- */
   TESTINST_RRR("orn", "$r24", "$r9", "$r15", 0x39320ce9aa25fb73UL, 0xaaec06dc1b47cf43UL, 0x5fa36a558c884a69UL);
   TESTINST_RRR("orn", "$r12", "$r4", "$r26", 0xa9c2abcbc14e3f3cUL, 0x7c87d633528d97b0UL, 0xe383c14e72ab8677UL);
   TESTINST_RRR("orn", "$r20", "$r24", "$r28", 0xb117d8b0280738a2UL, 0x318fd949c3ba430fUL, 0xc9edab5116dc1582UL);
   TESTINST_RRR("orn", "$r8", "$r25", "$r25", 0xb140441a36f8ededUL, 0xa26782a5e34d7addUL, 0x61bdd5b78d019958UL);
   TESTINST_RRR("orn", "$r16", "$r18", "$r25", 0xcda0e2c1bce1eeecUL, 0xa4486eefd2c444d9UL, 0xbd007605c829cadcUL);
   TESTINST_RRR("orn", "$r5", "$r28", "$r19", 0x8196fca50795a2aaUL, 0xec7f689a0d676560UL, 0xb4450418c4e1b333UL);
   TESTINST_RRR("orn", "$r15", "$r14", "$r8", 0xaf1e2a9fe35ba4edUL, 0xd2207f86d89b890aUL, 0xfb31b9e37313a94dUL);
   TESTINST_RRR("orn", "$r27", "$r14", "$r14", 0x1f24566bfa353160UL, 0xc4e17319c4766becUL, 0x29a3bbaaf6b49218UL);
   TESTINST_RRR("orn", "$r17", "$r12", "$r31", 0xf5195a72c175fed7UL, 0x7aa8d4840359cbf6UL, 0xa1a42af83c82215bUL);
   TESTINST_RRR("orn", "$r16", "$r20", "$r20", 0x76bb09b5b50705e2UL, 0x613fdcbd8c1eba2aUL, 0xfb1e04641f5da4ffUL);

   /* ---------------- andn rd, rj, rk ---------------- */
   TESTINST_RRR("andn", "$r19", "$r31", "$r17", 0xbcc81a9b2e349626UL, 0x5a38a8ef9c7e30e4UL, 0xcb490976d0652986UL);
   TESTINST_RRR("andn", "$r10", "$r4", "$r10", 0x9acfa0cd6ea107fdUL, 0x1d9b572e8f6bedb7UL, 0x768fe778d2a543eaUL);
   TESTINST_RRR("andn", "$r6", "$r12", "$r26", 0x949e36cff3b5decbUL, 0x56723f7285834fc9UL, 0xf6fa544d6cd57fa8UL);
   TESTINST_RRR("andn", "$r16", "$r6", "$r4", 0x44a39d85132d6513UL, 0x3ca7f972b865b7ceUL, 0xf18819e4740308bcUL);
   TESTINST_RRR("andn", "$r19", "$r26", "$r15", 0x856d1e3162c8fa2dUL, 0xc1ef79456be3885UL, 0x3c089064e60da1dUL);
   TESTINST_RRR("andn", "$r17", "$r28", "$r9", 0x512a518c554f4b0aUL, 0x43454425b8b7755UL, 0xdc5dca386b49bdd7UL);
   TESTINST_RRR("andn", "$r16", "$r16", "$r14", 0xa9c14796fec54f89UL, 0xe31928f90d2723a4UL, 0xcf2deaf4af11410aUL);
   TESTINST_RRR("andn", "$r9", "$r4", "$r20", 0x51d79964a699ec8dUL, 0xe82135537ca93e7fUL, 0xcbadcb1dc4dd0ed0UL);
   TESTINST_RRR("andn", "$r18", "$r25", "$r25", 0xeb546ce75bcba3f5UL, 0x953d86e2bd6b136dUL, 0x4914dbeee506d8adUL);
   TESTINST_RRR("andn", "$r27", "$r15", "$r14", 0xc8b599a43b0b4683UL, 0x509638630676b88UL, 0x3d278ed22a112a89UL);

   /* ---------------- mul.w rd, rj, rk ---------------- */
   TESTINST_RRR("mul.w", "$r28", "$r12", "$r10", 0xf6fcce3e1c5b1598UL, 0xef2747013f911fe8UL, 0x14a216fd69537967UL);
   TESTINST_RRR("mul.w", "$r13", "$r18", "$r24", 0x5e8a32c1e1e12aa4UL, 0x30e007bb8dd185faUL, 0x1a74dd893af9fb5aUL);
   TESTINST_RRR("mul.w", "$r10", "$r20", "$r4", 0xf06f4af61b0e0c24UL, 0x1b3624a77f26275fUL, 0x653052ae3a1347dfUL);
   TESTINST_RRR("mul.w", "$r23", "$r19", "$r10", 0xccb5485ae4605cddUL, 0x67c67c647eaf9e6cUL, 0xfb9b6c7b49ec10cfUL);
   TESTINST_RRR("mul.w", "$r12", "$r30", "$r7", 0xc1f45aaf98ffcb39UL, 0x906f0c08c0bae02eUL, 0xdf6cf5c05b5f2d34UL);
   TESTINST_RRR("mul.w", "$r27", "$r12", "$r12", 0x9545c6d9f812c0d9UL, 0xacd016cb69e028b3UL, 0x2b68e3a280d9c0b6UL);
   TESTINST_RRR("mul.w", "$r28", "$r7", "$r19", 0x4cf68a9590da3da5UL, 0x70ed8b9b03a6325dUL, 0x1125383d12dad118UL);
   TESTINST_RRR("mul.w", "$r20", "$r12", "$r20", 0x10683d31408fb4c5UL, 0x9ef4ea79672ce58dUL, 0x960a13776923d3e4UL);
   TESTINST_RRR("mul.w", "$r26", "$r19", "$r28", 0xbf8a20b69fa4357bUL, 0xf3e9b53a654e3cbfUL, 0x20afdeb5a4b4e1c9UL);
   TESTINST_RRR("mul.w", "$r13", "$r26", "$r25", 0x78f637d350c666bfUL, 0xff742d96dc73e9e9UL, 0x94a3289b55744707UL);

   /* ---------------- mulh.w rd, rj, rk ---------------- */
   TESTINST_RRR("mulh.w", "$r18", "$r25", "$r14", 0xa988161162710d96UL, 0x37443c6f5d0625eaUL, 0x94da379219de8576UL);
   TESTINST_RRR("mulh.w", "$r13", "$r16", "$r18", 0x246298a54a25030aUL, 0x33643ceed35cff64UL, 0xc25702631b42c849UL);
   TESTINST_RRR("mulh.w", "$r20", "$r5", "$r15", 0x3b606ea986dcf13eUL, 0x269dcd16567786d2UL, 0x96c0983df45d5c03UL);
   TESTINST_RRR("mulh.w", "$r19", "$r19", "$r25", 0xab8fc1c922ba3e7aUL, 0xdec5bddca513d198UL, 0xf05e814d67d43f5aUL);
   TESTINST_RRR("mulh.w", "$r15", "$r28", "$r16", 0x82fcfa24449231baUL, 0xf37548fee13133f3UL, 0x256188ef96bb3d23UL);
   TESTINST_RRR("mulh.w", "$r24", "$r9", "$r27", 0x858ddeb68e948058UL, 0xffb64d62e202462UL, 0xe07a6dae07f46c11UL);
   TESTINST_RRR("mulh.w", "$r23", "$r20", "$r14", 0x7713930e419350ffUL, 0xd5d72e6efb86e428UL, 0x49f87e78ddcc8400UL);
   TESTINST_RRR("mulh.w", "$r28", "$r20", "$r25", 0x552a9b7f3fa0c48aUL, 0xd616afd20f193287UL, 0xbcd2ae680b131cd2UL);
   TESTINST_RRR("mulh.w", "$r16", "$r19", "$r12", 0x94b154fc890497c3UL, 0xd8217f47e4257a7cUL, 0xb47bb0e4cff83cbfUL);
   TESTINST_RRR("mulh.w", "$r23", "$r23", "$r6", 0xafb7fddb344318fUL, 0xaafee418c4267e18UL, 0x1763f686cd41d46eUL);

   /* ---------------- mulh.wu rd, rj, rk ---------------- */
   TESTINST_RRR("mulh.wu", "$r18", "$r17", "$r8", 0xa92fa2817b19786cUL, 0xaf23e3d2092f080cUL, 0x771c36ac19259f2aUL);
   TESTINST_RRR("mulh.wu", "$r16", "$r13", "$r8", 0xf4a7b7abe5f3831aUL, 0xe8beff7f8f4330cdUL, 0x38cebbe3d1af354dUL);
   TESTINST_RRR("mulh.wu", "$r8", "$r23", "$r29", 0x6ca8c7d8ec316750UL, 0xc3a59754c752c3a5UL, 0x4b77e251de7f45f1UL);
   TESTINST_RRR("mulh.wu", "$r20", "$r25", "$r30", 0x6faa5d1372250132UL, 0x68734123142c820aUL, 0xf7b4bdf342e2017UL);
   TESTINST_RRR("mulh.wu", "$r31", "$r18", "$r19", 0x8cfa67422c1c5d5UL, 0xb48ac9531206cef2UL, 0x9f9f5d925c5cf738UL);
   TESTINST_RRR("mulh.wu", "$r25", "$r7", "$r27", 0x85aa17ff1b3699baUL, 0x9a7aeabb800edb53UL, 0x4eb1ec754c7cdb59UL);
   TESTINST_RRR("mulh.wu", "$r19", "$r4", "$r28", 0x821038d7fb43149cUL, 0x44cd20261f5ae87eUL, 0xf9d8916e8eb4ecb1UL);
   TESTINST_RRR("mulh.wu", "$r30", "$r23", "$r28", 0xef34433557594fb3UL, 0x2f9401c8064c8ca0UL, 0x5de6287c2a56e507UL);
   TESTINST_RRR("mulh.wu", "$r13", "$r6", "$r17", 0xd6b38c427ad5f669UL, 0xbe04ea8987b20188UL, 0x52cee1d144e3c134UL);
   TESTINST_RRR("mulh.wu", "$r26", "$r19", "$r17", 0x2ea15eee9429b8a0UL, 0x43598be92000d9f7UL, 0x6364cfeb707aba6cUL);

   /* ---------------- mul.d rd, rj, rk ---------------- */
   TESTINST_RRR("mul.d", "$r19", "$r4", "$r10", 0xf0235819cf1bab1fUL, 0xdc7a0086353cfddfUL, 0x6f18aec465b5af87UL);
   TESTINST_RRR("mul.d", "$r19", "$r31", "$r20", 0x24d7526c5e4669e3UL, 0xaab7dd46e5af2493UL, 0xd5df6eea42205e25UL);
   TESTINST_RRR("mul.d", "$r15", "$r20", "$r4", 0x3740ba48d64cc478UL, 0xcfeffb7c35a98382UL, 0xeab050fc9bdb3c52UL);
   TESTINST_RRR("mul.d", "$r29", "$r7", "$r25", 0xe8858552c0e8eac8UL, 0xb65ed231c27efb70UL, 0xbb753de59e4ca3d1UL);
   TESTINST_RRR("mul.d", "$r5", "$r30", "$r4", 0xc4f17df5c983317dUL, 0xb2af9e86d443d8ceUL, 0xf9e3c6d18372d0d3UL);
   TESTINST_RRR("mul.d", "$r25", "$r17", "$r29", 0xa09d11d50056b350UL, 0x6609b14ca65f9affUL, 0x692def5a14a3278cUL);
   TESTINST_RRR("mul.d", "$r13", "$r15", "$r26", 0xd528ed047af75775UL, 0x896658fe826a0817UL, 0xa456f53d5f2760b1UL);
   TESTINST_RRR("mul.d", "$r23", "$r9", "$r7", 0x5d33f63ce8637a69UL, 0xad38922264c721ffUL, 0xe0514fea4ee52acaUL);
   TESTINST_RRR("mul.d", "$r25", "$r23", "$r30", 0x5d74125f059662f3UL, 0xa708100731e88710UL, 0x739e4de71fec92e0UL);
   TESTINST_RRR("mul.d", "$r26", "$r18", "$r30", 0x110a94ffa2e12f32UL, 0x1b770d6c423d4f8UL, 0x38bf04d66f91531aUL);

   /* ---------------- mulh.d rd, rj, rk ---------------- */
   TESTINST_RRR("mulh.d", "$r5", "$r15", "$r12", 0xd72f46d42ca4db6bUL, 0xe1771af0e69e49a6UL, 0xd796f52fbd01a4bbUL);
   TESTINST_RRR("mulh.d", "$r28", "$r18", "$r14", 0x904e699bcbe32b08UL, 0x9b5b69b4d817779cUL, 0xa02ca97cc4e37f13UL);
   TESTINST_RRR("mulh.d", "$r6", "$r12", "$r7", 0xc75e1065b8dbcd34UL, 0xec7d8ae6a65f2fd3UL, 0xb7e32b52f40bc8efUL);
   TESTINST_RRR("mulh.d", "$r5", "$r25", "$r19", 0x7b2e04c0c2f95e4fUL, 0x9a5037ff200e982aUL, 0xf862c0c6425ff2bcUL);
   TESTINST_RRR("mulh.d", "$r14", "$r8", "$r23", 0x5fd7ae31ad151daaUL, 0x444243172f499ec0UL, 0x9003c8aeabc39884UL);
   TESTINST_RRR("mulh.d", "$r7", "$r23", "$r13", 0xbc21ca397041a2bUL, 0xe886455c8737b2caUL, 0xd5ccec2f631a1d60UL);
   TESTINST_RRR("mulh.d", "$r26", "$r16", "$r13", 0xd3894783f187ee9cUL, 0xa7a6c4abeda9a22cUL, 0x4375f7e49ed91384UL);
   TESTINST_RRR("mulh.d", "$r17", "$r31", "$r16", 0xa93bd0cf9137745eUL, 0x3a1b2b922b7645f1UL, 0x7e33f64c19972ae3UL);
   TESTINST_RRR("mulh.d", "$r20", "$r19", "$r8", 0xda9224c9ab488939UL, 0xb7f5978bf509641dUL, 0xf6fcd615333c30c0UL);
   TESTINST_RRR("mulh.d", "$r12", "$r17", "$r20", 0xcdbd51e35d5c1df3UL, 0x254bd8eaadc946feUL, 0x9de163435088598bUL);

   /* ---------------- mulh.du rd, rj, rk ---------------- */
   TESTINST_RRR("mulh.du", "$r25", "$r28", "$r29", 0xf7ef0dbf1bf7938aUL, 0xd267d11ae422f604UL, 0x89d6fd68226e13dUL);
   TESTINST_RRR("mulh.du", "$r7", "$r28", "$r24", 0xe568cf4a6d6bc199UL, 0x6efedad6fbe95f2aUL, 0xdf55853ed22d024eUL);
   TESTINST_RRR("mulh.du", "$r25", "$r8", "$r9", 0xbf7c0226b0c2072UL, 0x794fd44a65c65ebbUL, 0xa0391c3fa3cf1e5cUL);
   TESTINST_RRR("mulh.du", "$r30", "$r16", "$r7", 0x3df3f3b3ff17f61aUL, 0xcadd1f7e7150ad7bUL, 0xbdc63d3f762cf02dUL);
   TESTINST_RRR("mulh.du", "$r6", "$r10", "$r19", 0x6601e05fc5f801cbUL, 0xbc10a70104969251UL, 0x2f50a00036fb7821UL);
   TESTINST_RRR("mulh.du", "$r17", "$r9", "$r5", 0xffabc0cbdc8aa7b0UL, 0x5288bc60da558afbUL, 0x2795644a58b2668fUL);
   TESTINST_RRR("mulh.du", "$r26", "$r8", "$r15", 0x68b64c997f561b59UL, 0xe2ed2375e64b1bf3UL, 0xe1033e583092ad96UL);
   TESTINST_RRR("mulh.du", "$r10", "$r13", "$r30", 0x6450ec488eb4753bUL, 0x4287b82860366cf8UL, 0x1c15ed3f051fe8cUL);
   TESTINST_RRR("mulh.du", "$r24", "$r13", "$r15", 0x1169fa9dd6f8273dUL, 0x6fd2cdb39e5d1fa3UL, 0xff0526e206880684UL);
   TESTINST_RRR("mulh.du", "$r8", "$r9", "$r10", 0xe9cb6416a1492fbfUL, 0xaf89960e18913df0UL, 0x76b4251409ff9830UL);

   /* ---------------- mulw.d.w rd, rj, rk ---------------- */
   TESTINST_RRR("mulw.d.w", "$r6", "$r31", "$r7", 0x50ce021eb3b3f3a4UL, 0xb859e7514e4c4d7cUL, 0x372cb1e2b3200f36UL);
   TESTINST_RRR("mulw.d.w", "$r31", "$r7", "$r28", 0x925642fa7e2de9abUL, 0x61404b6550238cebUL, 0x75ed502242ed0430UL);
   TESTINST_RRR("mulw.d.w", "$r19", "$r16", "$r10", 0xef82de697f7239fUL, 0xdf1c56dfe5c0e48dUL, 0xbc7e740fe1b1dc25UL);
   TESTINST_RRR("mulw.d.w", "$r29", "$r12", "$r27", 0xc104a400fa0d1dbfUL, 0x2aa34e8a5fad6c6fUL, 0x7f8e4d23644b0d4dUL);
   TESTINST_RRR("mulw.d.w", "$r25", "$r16", "$r25", 0x5b8ff9172c849fb9UL, 0x843f90380af6f2afUL, 0x12f7f8780cb8bfe0UL);
   TESTINST_RRR("mulw.d.w", "$r13", "$r13", "$r7", 0x6bba79a88056d891UL, 0x6757a43d403285abUL, 0x2d2ea385888c2664UL);
   TESTINST_RRR("mulw.d.w", "$r12", "$r8", "$r23", 0x5c96927dcf1fb14eUL, 0x2b3767b9e9029d4bUL, 0x252bbcc66b5d834bUL);
   TESTINST_RRR("mulw.d.w", "$r6", "$r13", "$r10", 0x5fa5a8b36e8ec3e0UL, 0xcbca4b4d518b9466UL, 0xabdf2ec674f70c5bUL);
   TESTINST_RRR("mulw.d.w", "$r16", "$r15", "$r23", 0x5b94eeb9c3c9fa01UL, 0x5c4ebef486f83b43UL, 0x73f3781c3a1e9216UL);
   TESTINST_RRR("mulw.d.w", "$r6", "$r31", "$r7", 0xbc263312a123caedUL, 0xe9aa8545d3a99a97UL, 0x71b5dbacf4f7f2b8UL);

   /* ---------------- mulw.d.wu rd, rj, rk ---------------- */
   TESTINST_RRR("mulw.d.wu", "$r14", "$r17", "$r30", 0x94452e0d7eb407b7UL, 0x629b1902a484a77dUL, 0x474359ca7f7165edUL);
   TESTINST_RRR("mulw.d.wu", "$r26", "$r7", "$r5", 0xae9771f0d59319b3UL, 0x1bcb563dea8f3a3fUL, 0x759334cc2d543103UL);
   TESTINST_RRR("mulw.d.wu", "$r25", "$r28", "$r27", 0x27ca0bf2d6cd2699UL, 0x5a015da9b52ffc64UL, 0x482a4fa5b5625914UL);
   TESTINST_RRR("mulw.d.wu", "$r8", "$r4", "$r16", 0x22f61239dad7bc92UL, 0xe8c9964b31b0e199UL, 0x99fdef421aa22322UL);
   TESTINST_RRR("mulw.d.wu", "$r29", "$r17", "$r15", 0xcc5eec6e4f2b5fdbUL, 0x2d08ada074c2ac37UL, 0x8967ce1cd4c2362eUL);
   TESTINST_RRR("mulw.d.wu", "$r27", "$r23", "$r16", 0x2d057e2ead214d6cUL, 0x987e7a10a0f3ee5dUL, 0xd515e2a2f06be633UL);
   TESTINST_RRR("mulw.d.wu", "$r15", "$r19", "$r12", 0xce24943d6fe20263UL, 0xd6bbdcb20d76de15UL, 0xcc277905bc41da62UL);
   TESTINST_RRR("mulw.d.wu", "$r4", "$r4", "$r19", 0xe37942a26dc0e882UL, 0x6a30fb04c3b5431fUL, 0x4c937bed67cb6c73UL);
   TESTINST_RRR("mulw.d.wu", "$r7", "$r12", "$r9", 0xbdebe7a7b19b7dc0UL, 0x3f6e790fb24d19f1UL, 0x7a19c4fdd0d29f3eUL);
   TESTINST_RRR("mulw.d.wu", "$r31", "$r30", "$r28", 0x690687056e169108UL, 0xa8abab5bf1d42538UL, 0x636a31884ca1e99UL);

   /* ---------------- div.w rd, rj, rk ---------------- */
   TESTINST_RRR("div.w", "$r13", "$r28", "$r23", 0x16546290UL, 0x627aa138UL, 0x534168cUL);
   TESTINST_RRR("div.w", "$r28", "$r19", "$r9", 0xffffffffbe03930dUL, 0x223d0ec7UL, 0xffffffff8404aa67UL);
   TESTINST_RRR("div.w", "$r18", "$r19", "$r30", 0xffffffffac214649UL, 0xffffffff8019c3b7UL, 0xffffffff871cbf90UL);
   TESTINST_RRR("div.w", "$r24", "$r25", "$r7", 0xffffffffa144ed80UL, 0x1c4370c7UL, 0x4695aa29UL);
   TESTINST_RRR("div.w", "$r9", "$r27", "$r4", 0x3ae8b7c7UL, 0xfffffffff3a6ebb2UL, 0x181d816aUL);
   TESTINST_RRR("div.w", "$r28", "$r15", "$r7", 0xffffffff956a7de4UL, 0xffffffff9aab217bUL, 0x3b061b78UL);
   TESTINST_RRR("div.w", "$r25", "$r24", "$r12", 0x3c6167d4UL, 0x2673145eUL, 0x1d5e391UL);
   TESTINST_RRR("div.w", "$r23", "$r15", "$r4", 0x3e0820eeUL, 0x42793c51UL, 0x286cdb51UL);
   TESTINST_RRR("div.w", "$r28", "$r16", "$r30", 0xffffffffcf8fd242UL, 0x2a76141eUL, 0x2429a52UL);
   TESTINST_RRR("div.w", "$r29", "$r8", "$r18", 0x74991388UL, 0xffffffffd594ef43UL, 0x6d3f9603UL);

   /* ---------------- mod.w rd, rj, rk ---------------- */
   TESTINST_RRR("mod.w", "$r8", "$r13", "$r14", 0x5cc9e6dbUL, 0xfffffffff7327c6dUL, 0x23eef833UL);
   TESTINST_RRR("mod.w", "$r25", "$r24", "$r25", 0x539195e4UL, 0xffffffffd94f10c8UL, 0x2c5786d9UL);
   TESTINST_RRR("mod.w", "$r10", "$r16", "$r23", 0xffffffff9b15f725UL, 0x448a831dUL, 0xffffffffd5d7d92bUL);
   TESTINST_RRR("mod.w", "$r6", "$r5", "$r29", 0x1794d969UL, 0x2fba86b0UL, 0x40e6ab6bUL);
   TESTINST_RRR("mod.w", "$r16", "$r14", "$r29", 0x6a503328UL, 0xffffffffdf0b2ad2UL, 0xffffffff90dc29c6UL);
   TESTINST_RRR("mod.w", "$r30", "$r14", "$r18", 0xffffffffc7670acdUL, 0x53f3b34fUL, 0xffffffff84b62159UL);
   TESTINST_RRR("mod.w", "$r31", "$r6", "$r18", 0xffffffff98334c95UL, 0xfffffffff241ffd8UL, 0xffffffffa73314aaUL);
   TESTINST_RRR("mod.w", "$r12", "$r8", "$r4", 0xffffffffd9f19db4UL, 0xffffffffc89f9796UL, 0xffffffffaa8e2a3bUL);
   TESTINST_RRR("mod.w", "$r23", "$r12", "$r4", 0xffffffff94e93220UL, 0xfffffffffea1587aUL, 0xffffffffb88b2b87UL);
   TESTINST_RRR("mod.w", "$r13", "$r9", "$r18", 0xf718c0UL, 0xffffffffe264a3a5UL, 0x2f29ef3UL);

   /* ---------------- div.wu rd, rj, rk ---------------- */
   TESTINST_RRR("div.wu", "$r24", "$r5", "$r16", 0xddf57c5UL, 0x6b1a808cUL, 0x576fe70UL);
   TESTINST_RRR("div.wu", "$r26", "$r7", "$r9", 0x665e82ffUL, 0x344d887fUL, 0x7fd6d6d8UL);
   TESTINST_RRR("div.wu", "$r13", "$r18", "$r15", 0xffffffffe82e2cf8UL, 0x7c66b628UL, 0x305c899UL);
   TESTINST_RRR("div.wu", "$r15", "$r14", "$r7", 0xb06b1fUL, 0x56016282UL, 0x95a8701UL);
   TESTINST_RRR("div.wu", "$r19", "$r12", "$r31", 0xffffffffb3a487d1UL, 0xffffffffbe2fe16eUL, 0xffffffff8dc0ff7fUL);
   TESTINST_RRR("div.wu", "$r6", "$r10", "$r20", 0x1bb491e9UL, 0x64e382eUL, 0x5977f9f1UL);
   TESTINST_RRR("div.wu", "$r9", "$r29", "$r28", 0x498c3349UL, 0x14cbb257UL, 0xffffffff95165a4aUL);
   TESTINST_RRR("div.wu", "$r10", "$r29", "$r15", 0xffffffffbb3f9c5dUL, 0x2755057dUL, 0x14039cc4UL);
   TESTINST_RRR("div.wu", "$r24", "$r31", "$r7", 0xffffffffe5a9a3cdUL, 0xffffffffa1f84b49UL, 0xffffffffe45bd3b9UL);
   TESTINST_RRR("div.wu", "$r23", "$r18", "$r6", 0x54e07e9fUL, 0xffffffffaccbdd8cUL, 0xfffffffff3729b57UL);

   /* ---------------- mod.wu rd, rj, rk ---------------- */
   TESTINST_RRR("mod.wu", "$r5", "$r20", "$r18", 0xffffffffa1ce2e4eUL, 0xffffffffdbeb0e2dUL, 0x70157135UL);
   TESTINST_RRR("mod.wu", "$r14", "$r30", "$r17", 0x10e75d07UL, 0x39c3080UL, 0x1658d87bUL);
   TESTINST_RRR("mod.wu", "$r28", "$r7", "$r4", 0x6df194dbUL, 0x55fae7c9UL, 0xffffffff9a87c1efUL);
   TESTINST_RRR("mod.wu", "$r6", "$r14", "$r10", 0xffffffff8feb78ccUL, 0xffffffffe5032316UL, 0x18ab441eUL);
   TESTINST_RRR("mod.wu", "$r13", "$r15", "$r9", 0xffffffffbb28952cUL, 0x2d43f57dUL, 0x2dfbf584UL);
   TESTINST_RRR("mod.wu", "$r7", "$r30", "$r5", 0x9bfb2cfUL, 0x6595d7b3UL, 0xfffffffffffd1025UL);
   TESTINST_RRR("mod.wu", "$r10", "$r9", "$r16", 0x342671c6UL, 0xfffffffff1ff8be3UL, 0xfffffffffaea052bUL);
   TESTINST_RRR("mod.wu", "$r16", "$r16", "$r23", 0xffffffffc0356055UL, 0x2ac1f414UL, 0x4a75c890UL);
   TESTINST_RRR("mod.wu", "$r19", "$r8", "$r7", 0xfffffffff8ed6580UL, 0x5fef460eUL, 0x68eedef2UL);
   TESTINST_RRR("mod.wu", "$r29", "$r25", "$r25", 0xffffffff9ea76eb0UL, 0xffffffff818904b9UL, 0xffffffffe92f4f30UL);

   /* ---------------- div.d rd, rj, rk ---------------- */
   TESTINST_RRR("div.d", "$r7", "$r17", "$r7", 0xc8f25fb958f2d668UL, 0x74a14cbaa00fdeaUL, 0xcf95f3de82ceb015UL);
   TESTINST_RRR("div.d", "$r10", "$r19", "$r12", 0x9ead8a6f6ea63534UL, 0xaf80d344d48e6cd5UL, 0xe1f40f759cbfe0e7UL);
   TESTINST_RRR("div.d", "$r23", "$r28", "$r28", 0x35481a5285093e04UL, 0xfd79e3c19b697fa8UL, 0x6ffab603b9e1b7fbUL);
   TESTINST_RRR("div.d", "$r30", "$r25", "$r4", 0x3eacf1d695a34b95UL, 0xfbff957ab051d494UL, 0x670724b8930d53fUL);
   TESTINST_RRR("div.d", "$r31", "$r29", "$r6", 0xce8d3df48871d655UL, 0xf351f7f35927e83dUL, 0x93a3085686f4101fUL);
   TESTINST_RRR("div.d", "$r17", "$r23", "$r8", 0xfc913f8b14dda5a5UL, 0x1f938af81988deUL, 0x9d021a9f06b46953UL);
   TESTINST_RRR("div.d", "$r7", "$r29", "$r15", 0x4593da2923f2ac5bUL, 0x11fc5a958b182a55UL, 0x2edafaf2857c6697UL);
   TESTINST_RRR("div.d", "$r13", "$r31", "$r27", 0x97236145608dd8c3UL, 0x1f0ee96afd23910bUL, 0xe35e4d5efd2204d3UL);
   TESTINST_RRR("div.d", "$r13", "$r26", "$r14", 0x2c057bd222f216dfUL, 0x1e006853720971c3UL, 0x81e35a993e6a15b5UL);
   TESTINST_RRR("div.d", "$r5", "$r9", "$r4", 0x93c0d85c66f2c5abUL, 0x774fbe894b2ed067UL, 0x2c46387d55732742UL);

   /* ---------------- mod.d rd, rj, rk ---------------- */
   TESTINST_RRR("mod.d", "$r19", "$r26", "$r16", 0x63304d2181f4a4daUL, 0x9ed948849ddee475UL, 0x18a360d3ab980398UL);
   TESTINST_RRR("mod.d", "$r27", "$r23", "$r13", 0xf7156e74db7a8d92UL, 0x324e7001287ce2a8UL, 0x3cc7524686bed31cUL);
   TESTINST_RRR("mod.d", "$r8", "$r26", "$r19", 0x7bda37a222135803UL, 0x1daf8fd66ff987edUL, 0x334631279104fc3bUL);
   TESTINST_RRR("mod.d", "$r25", "$r15", "$r7", 0xd1a0f45d5b463d53UL, 0x9c4cd7bef3bf0712UL, 0x420a5c702006f3ccUL);
   TESTINST_RRR("mod.d", "$r25", "$r18", "$r7", 0x93487a905cb08a75UL, 0x8c79cafa8bebf0a8UL, 0x1478409d192c144bUL);
   TESTINST_RRR("mod.d", "$r8", "$r27", "$r27", 0x8756a1690dd7896dUL, 0x35273279ea76319fUL, 0xc5292f2331abc6ddUL);
   TESTINST_RRR("mod.d", "$r15", "$r10", "$r24", 0xf8c476adbc930802UL, 0x8b5832bcd0f6c87eUL, 0x6cba54a72da38702UL);
   TESTINST_RRR("mod.d", "$r27", "$r7", "$r6", 0x2387015bddb2c076UL, 0x231e30de7a72ad90UL, 0x81f1285973e8dc11UL);
   TESTINST_RRR("mod.d", "$r16", "$r9", "$r12", 0x3388d23c07feb1daUL, 0xe8c01f744b310474UL, 0xa29071d702959009UL);
   TESTINST_RRR("mod.d", "$r13", "$r10", "$r20", 0xbd45a261f8de4fe4UL, 0x6fb0a8c9a2681a8eUL, 0x2f1b7055cf2409ecUL);

   /* ---------------- div.du rd, rj, rk ---------------- */
   TESTINST_RRR("div.du", "$r17", "$r10", "$r24", 0x4d363fd48a626fdaUL, 0x7ccdeeaa6c24885fUL, 0xfcc68e72f59750aeUL);
   TESTINST_RRR("div.du", "$r20", "$r20", "$r10", 0x808fa5cb6a75fd6fUL, 0xf3f712970031005UL, 0x1709a8adab2fa578UL);
   TESTINST_RRR("div.du", "$r15", "$r14", "$r19", 0xcd3107423486c8feUL, 0xf6bc56277282cd14UL, 0x961ac833f00f3e3UL);
   TESTINST_RRR("div.du", "$r4", "$r29", "$r18", 0xa0bfc2fc5b35fa79UL, 0x2b28c09aa5f12845UL, 0xed44da2fdf5dce00UL);
   TESTINST_RRR("div.du", "$r4", "$r6", "$r25", 0x1fc6e23fd0f09ed0UL, 0xeaa71d9fb42223caUL, 0x45689545e60381cUL);
   TESTINST_RRR("div.du", "$r10", "$r8", "$r12", 0xa3710c512d4c006cUL, 0xc011778733c50a6eUL, 0xb44475ee048d8167UL);
   TESTINST_RRR("div.du", "$r29", "$r4", "$r29", 0x46d27abff0da1972UL, 0x17a4e863a182dcd0UL, 0x59a7b82980ac6a6dUL);
   TESTINST_RRR("div.du", "$r15", "$r8", "$r30", 0x68120919dbbd9b19UL, 0x4c296c89a6f7a6dfUL, 0x9d9166c1cd0eecfaUL);
   TESTINST_RRR("div.du", "$r7", "$r18", "$r17", 0xd2389cb7af92be89UL, 0x9a1f65b2c59cfda3UL, 0xe316cf92f8f0574fUL);
   TESTINST_RRR("div.du", "$r15", "$r25", "$r17", 0x49651d72d87da955UL, 0xd22c499c27908743UL, 0x8d824b01058ecb8UL);

   /* ---------------- mod.du rd, rj, rk ---------------- */
   TESTINST_RRR("mod.du", "$r26", "$r8", "$r23", 0xb0bd66f10c34fe23UL, 0x5eb9b775d83b4893UL, 0x8867d4b638f2622UL);
   TESTINST_RRR("mod.du", "$r8", "$r10", "$r25", 0xe236349cd47eeb11UL, 0x119102fd7b236a81UL, 0x8fd72a09e4fb45fUL);
   TESTINST_RRR("mod.du", "$r25", "$r4", "$r5", 0x1b669725a0c3a970UL, 0x175359099c87b83UL, 0xcad295c79f1d835aUL);
   TESTINST_RRR("mod.du", "$r7", "$r28", "$r20", 0x7117e70798869df4UL, 0xe35b93aa0c37fe97UL, 0x741084dead7970d0UL);
   TESTINST_RRR("mod.du", "$r30", "$r24", "$r9", 0xc4d432a8ce91f693UL, 0x77c03aceb2ea6b45UL, 0xb8cd7773fb72b7caUL);
   TESTINST_RRR("mod.du", "$r23", "$r9", "$r28", 0x13f1f3e1891b6b73UL, 0x9811699becce53a9UL, 0xed15e264f0c39b88UL);
   TESTINST_RRR("mod.du", "$r13", "$r12", "$r14", 0xb8b22bcb0cb970e8UL, 0x16cdecd7c0091cd2UL, 0x4fcab819ebadbdfdUL);
   TESTINST_RRR("mod.du", "$r30", "$r17", "$r12", 0xbf96226d2de1240dUL, 0x9fe4b2c7557d6b9aUL, 0x3668e581a5de6efdUL);
   TESTINST_RRR("mod.du", "$r14", "$r4", "$r6", 0x9bc8f8a69a7f55c2UL, 0x530a9c5a21769babUL, 0x2805bef72d33cbd5UL);
   TESTINST_RRR("mod.du", "$r23", "$r28", "$r12", 0x82a854f86e642cbaUL, 0xdd0fd63485d6c3dUL, 0x56b21f15cb9d2bf2UL);

   /* ---------------- alsl.w rd, rj, rk, sa2 ---------------- */
   TESTINST_RRRI("alsl.w", "$r18", "$r10", "$r15", SA2_1, 0xafb40df16156827bUL, 0x9b0b86116a0d89cbUL, 0x80086c066ea6842bUL, 2);
   TESTINST_RRRI("alsl.w", "$r24", "$r5", "$r4", SA2_1, 0xb8b63b8205a919dfUL, 0x7319260322fa2d6dUL, 0x1efce6644a51ebf9UL, 2);
   TESTINST_RRRI("alsl.w", "$r24", "$r5", "$r27", SA2_1, 0xb4f0fd355869e078UL, 0x26abeea20b7d1ac1UL, 0x4108f7f27e321c8fUL, 2);
   TESTINST_RRRI("alsl.w", "$r24", "$r29", "$r10", SA2_1, 0x4b948e9a0b82df22UL, 0x11893c9dd43d0112UL, 0x51a030165671a055UL, 1);
   TESTINST_RRRI("alsl.w", "$r5", "$r10", "$r18", SA2_1, 0xfc253ac9e2b55590UL, 0x2682507563a85b07UL, 0xa467083f66457d1dUL, 1);
   TESTINST_RRRI("alsl.w", "$r20", "$r13", "$r10", SA2_1, 0x76e8c346a721cdabUL, 0x548f2762bfb1bc01UL, 0xa6e0d27e62dcc594UL, 3);
   TESTINST_RRRI("alsl.w", "$r16", "$r6", "$r24", SA2_1, 0x39f77b88fc3b663UL, 0x281818bf4a36a7e5UL, 0x86cd2a06ef475a61UL, 3);
   TESTINST_RRRI("alsl.w", "$r14", "$r18", "$r9", SA2_1, 0x8a58ea94346ff16UL, 0x4ff191f91397adeaUL, 0x4cda359b03c97a53UL, 4);
   TESTINST_RRRI("alsl.w", "$r8", "$r6", "$r29", SA2_1, 0xae0bfa182556c725UL, 0xda179bc2f41d03d3UL, 0x1d23e4da08af7978UL, 1);
   TESTINST_RRRI("alsl.w", "$r31", "$r26", "$r30", SA2_1, 0xd6af9fcd7ffd8e75UL, 0x3e88bb77d6665633UL, 0x23a0414c69b804c1UL, 1);

   /* ---------------- alsl.wu rd, rj, rk, sa2 ---------------- */
   TESTINST_RRRI("alsl.wu", "$r20", "$r24", "$r18", SA2_1, 0xc714872ff3c39370UL, 0xcaea31ddabb275f9UL, 0xedbfc2cedca8eb7aUL, 2);
   TESTINST_RRRI("alsl.wu", "$r13", "$r26", "$r15", SA2_1, 0xe1a0ba1adcb75aa4UL, 0x8adbed432acf321aUL, 0xeae447eaa60bb142UL, 3);
   TESTINST_RRRI("alsl.wu", "$r4", "$r17", "$r27", SA2_1, 0xb153f9ecea23068cUL, 0xd2066b089c9499a3UL, 0x36ed3c96ac4751aaUL, 3);
   TESTINST_RRRI("alsl.wu", "$r20", "$r10", "$r4", SA2_1, 0x8fb2705357e98d66UL, 0xd353329585fc71ddUL, 0x739237ed6a677f00UL, 4);
   TESTINST_RRRI("alsl.wu", "$r31", "$r12", "$r23", SA2_1, 0x6caac60acd9bc6f4UL, 0xc87131b9171530dfUL, 0x39c8e321a6e131c0UL, 2);
   TESTINST_RRRI("alsl.wu", "$r13", "$r14", "$r19", SA2_1, 0xd2c7072036f54e45UL, 0x35ea1627556f8f98UL, 0x97054728433042d3UL, 2);
   TESTINST_RRRI("alsl.wu", "$r7", "$r14", "$r5", SA2_1, 0x5a0f1fae80105d64UL, 0xd300b74879e33a53UL, 0x3a1e7389d0669d4cUL, 1);
   TESTINST_RRRI("alsl.wu", "$r28", "$r4", "$r9", SA2_1, 0xcd7fd8389b4f4062UL, 0xad1830d644c205e7UL, 0xced1c031d73f9087UL, 1);
   TESTINST_RRRI("alsl.wu", "$r13", "$r9", "$r29", SA2_1, 0x81601560f53b081UL, 0xd3ee3c45f08cd218UL, 0xa7d5a43a1df2aa1dUL, 4);
   TESTINST_RRRI("alsl.wu", "$r30", "$r29", "$r31", SA2_1, 0xf383bd5bfae7e46dUL, 0x67862a0151c65567UL, 0x9cdcbf604f46c48aUL, 2);

   /* ---------------- alsl.d rd, rj, rk, sa2 ---------------- */
   TESTINST_RRRI("alsl.d", "$r18", "$r28", "$r16", SA2_1, 0x53e533e973dfa49cUL, 0x6665a9d32abaaf55UL, 0xf70490874fb75e6eUL, 4);
   TESTINST_RRRI("alsl.d", "$r10", "$r30", "$r18", SA2_1, 0xfb14c3e6acd722c3UL, 0xcae19862ab088fccUL, 0x87c434d85259d923UL, 2);
   TESTINST_RRRI("alsl.d", "$r17", "$r25", "$r26", SA2_1, 0x95e79a567c313ec7UL, 0x83a0e706c2c4c534UL, 0x2f49f1e9d5b91fc9UL, 1);
   TESTINST_RRRI("alsl.d", "$r7", "$r24", "$r24", SA2_1, 0x35b966d0db9f681cUL, 0xc0bc97593f1054fcUL, 0x7e564928b0a53ac6UL, 2);
   TESTINST_RRRI("alsl.d", "$r6", "$r30", "$r24", SA2_1, 0x38ad1fb21e071421UL, 0xb959c439b0436d6dUL, 0x647c742c9ce02fc5UL, 3);
   TESTINST_RRRI("alsl.d", "$r18", "$r28", "$r10", SA2_1, 0x1bde2962dc5bb68bUL, 0x67c403d00c9389bdUL, 0x8fc18921f225d05aUL, 2);
   TESTINST_RRRI("alsl.d", "$r8", "$r27", "$r15", SA2_1, 0x5b8de9d8b393fa06UL, 0x393ec1c28e89e9d8UL, 0x1a59f9d852c3f8baUL, 3);
   TESTINST_RRRI("alsl.d", "$r27", "$r24", "$r6", SA2_1, 0x72195c1ca51cc4dbUL, 0x4ee5b51e1e161ab2UL, 0x8a10acb4b625fefUL, 4);
   TESTINST_RRRI("alsl.d", "$r29", "$r4", "$r18", SA2_1, 0xf3ed9e39d83d3decUL, 0xa3816509b9a6c23dUL, 0x6949e8e534450dd5UL, 2);
   TESTINST_RRRI("alsl.d", "$r16", "$r13", "$r8", SA2_1, 0x588f388f25a342dfUL, 0xde33a74109c7be30UL, 0x8b02cf06997a065aUL, 1);

   /* ---------------- lu12i.w rd, si20 ---------------- */
   TESTINST_RI("lu12i.w", "$r9", SI20, 0xdf45bd002ccf48e1UL, 94146);
   TESTINST_RI("lu12i.w", "$r10", SI20, 0xa5138a37d09ada8aUL, 129014);
   TESTINST_RI("lu12i.w", "$r18", SI20, 0xefe46a52b8b3e5eUL, -130138);
   TESTINST_RI("lu12i.w", "$r7", SI20, 0x29084adf6d033a88UL, -467080);
   TESTINST_RI("lu12i.w", "$r10", SI20, 0xe9072e7fec2a5d1cUL, 360675);
   TESTINST_RI("lu12i.w", "$r28", SI20, 0x2f7d41c7bd959cd5UL, 205272);
   TESTINST_RI("lu12i.w", "$r16", SI20, 0xcb48200d89b48566UL, -266298);
   TESTINST_RI("lu12i.w", "$r12", SI20, 0xd605223c244f4a50UL, -186346);
   TESTINST_RI("lu12i.w", "$r15", SI20, 0x22c035c8c90016beUL, 247864);
   TESTINST_RI("lu12i.w", "$r20", SI20, 0x6b2fd1aa0b603fecUL, -511005);

   /* ---------------- lu32i.d rd, si20 ---------------- */
   TESTINST_RI("lu32i.d", "$r8", SI20, 0xb331616751ed8877UL, -310956);
   TESTINST_RI("lu32i.d", "$r17", SI20, 0xe49bab8d80e1dd7UL, 35590);
   TESTINST_RI("lu32i.d", "$r4", SI20, 0x842cdc9ac0a0adf6UL, 500474);
   TESTINST_RI("lu32i.d", "$r23", SI20, 0xc9ca69b8e5ab079eUL, -447277);
   TESTINST_RI("lu32i.d", "$r12", SI20, 0x27d83e1c77dec50aUL, -503028);
   TESTINST_RI("lu32i.d", "$r26", SI20, 0xc00dcc918a89f350UL, -355708);
   TESTINST_RI("lu32i.d", "$r16", SI20, 0xd180188cdc073491UL, -231989);
   TESTINST_RI("lu32i.d", "$r26", SI20, 0x4efae034432bbb3bUL, 250642);
   TESTINST_RI("lu32i.d", "$r15", SI20, 0x7bf2141e673e336fUL, 237105);
   TESTINST_RI("lu32i.d", "$r4", SI20, 0x187c50bfc5eb8f32UL, -312071);

   /* ---------------- lu52i.d rd, rj, si12 ---------------- */
   TESTINST_RRI("lu52i.d", "$r8", "$r25", SI12, 0x1da74dfcb33d471aUL, 0x453ae9f1200f4d41UL, 1920);
   TESTINST_RRI("lu52i.d", "$r14", "$r25", SI12, 0x5e954055ebaec78fUL, 0xb7637f9119e12e31UL, -2008);
   TESTINST_RRI("lu52i.d", "$r26", "$r24", SI12, 0xead69e40b96b23bfUL, 0x779862b03d1ab575UL, -1803);
   TESTINST_RRI("lu52i.d", "$r5", "$r25", SI12, 0x452236306da7c667UL, 0x9f16a6e48cca3a7bUL, -1406);
   TESTINST_RRI("lu52i.d", "$r26", "$r23", SI12, 0x5604b9744291e45aUL, 0x70eecb3116b1795cUL, -667);
   TESTINST_RRI("lu52i.d", "$r14", "$r27", SI12, 0x6d9a8cfe459c1c48UL, 0x85452bdd40205e0dUL, -1221);
   TESTINST_RRI("lu52i.d", "$r25", "$r8", SI12, 0x1a8d72e42f68a33dUL, 0x7089b6fe4c1f7a70UL, 423);
   TESTINST_RRI("lu52i.d", "$r30", "$r10", SI12, 0x7c4fe646acac7ac0UL, 0xe7d222ba1fd5cae2UL, -177);
   TESTINST_RRI("lu52i.d", "$r6", "$r13", SI12, 0xdb3d6a615a9e492fUL, 0xaa9303648ff489f2UL, -1438);
   TESTINST_RRI("lu52i.d", "$r25", "$r4", SI12, 0x8b41b813d85b8ee8UL, 0xe4d31961e42e713cUL, -634);

   /* ---------------- addi.w rd, rj, si12 ---------------- */
   TESTINST_RRI("addi.w", "$r6", "$r27", SI12, 0x12845f036198fa6fUL, 0xda77c63c764655daUL, 1727);
   TESTINST_RRI("addi.w", "$r9", "$r8", SI12, 0x21a7e3cfa2649a4fUL, 0xc64c73b3bd4c1dcbUL, -381);
   TESTINST_RRI("addi.w", "$r16", "$r6", SI12, 0x6c47b02ef52a3502UL, 0x24ca1a646dac5cc3UL, -186);
   TESTINST_RRI("addi.w", "$r20", "$r31", SI12, 0xb6144d8f9513c78eUL, 0xc4b808764e894e6cUL, 1503);
   TESTINST_RRI("addi.w", "$r19", "$r17", SI12, 0xcf97c9215c961121UL, 0x9b714c4cb899399bUL, -1918);
   TESTINST_RRI("addi.w", "$r14", "$r8", SI12, 0xe1abf22f6c3c82ecUL, 0x4110e9c1b5f59ef6UL, -1781);
   TESTINST_RRI("addi.w", "$r29", "$r18", SI12, 0x4b64427195dda12dUL, 0xadf5af70b7b3f37bUL, 2047);
   TESTINST_RRI("addi.w", "$r4", "$r30", SI12, 0xfc785d46f5bbdff4UL, 0x1e061e9d51362d9cUL, 244);
   TESTINST_RRI("addi.w", "$r7", "$r23", SI12, 0xe037576d82c12e8dUL, 0xa77c8da72af708f1UL, -376);
   TESTINST_RRI("addi.w", "$r23", "$r17", SI12, 0xa10df57c4103efUL, 0x26d2628746ad0a3eUL, 1924);

   /* ---------------- addi.d rd, rj, si12 ---------------- */
   TESTINST_RRI("addi.d", "$r14", "$r14", SI12, 0x61b497fb58a816d9UL, 0x29eb218dd65d9d6cUL, 152);
   TESTINST_RRI("addi.d", "$r20", "$r13", SI12, 0xd80db8387a8cdd93UL, 0x5e23e4b01f2bbd6dUL, -640);
   TESTINST_RRI("addi.d", "$r13", "$r25", SI12, 0x5dfea060c6e8f587UL, 0x95f49b783954f9f9UL, -743);
   TESTINST_RRI("addi.d", "$r4", "$r30", SI12, 0xd72f370f6ce7bc4cUL, 0x148550b0f97ce601UL, 676);
   TESTINST_RRI("addi.d", "$r26", "$r8", SI12, 0xa4120a67f8d6df1aUL, 0xa83f4bbcaf5bc52eUL, 1630);
   TESTINST_RRI("addi.d", "$r20", "$r29", SI12, 0xa8f9c82780ac16d5UL, 0x7ab169a5751642bcUL, -1971);
   TESTINST_RRI("addi.d", "$r8", "$r8", SI12, 0x6f22bdb480c14540UL, 0x94e1253c331b17f2UL, 1160);
   TESTINST_RRI("addi.d", "$r15", "$r27", SI12, 0x312473547bcfe03UL, 0x7a786cbc8149d818UL, 844);
   TESTINST_RRI("addi.d", "$r8", "$r26", SI12, 0xee2b1be852671bc3UL, 0x6a36d61dfee3a6fbUL, -1185);
   TESTINST_RRI("addi.d", "$r17", "$r27", SI12, 0x70e068b54ed72e20UL, 0x922681ab8837027bUL, -2046);

   /* ---------------- addu16i.d rd, rj, si16 ---------------- */
   TESTINST_RRI("addu16i.d", "$r20", "$r29", SI16, 0x8232770e3472bdc3UL, 0x4d28c5567787c26eUL, -14564);
   TESTINST_RRI("addu16i.d", "$r29", "$r4", SI16, 0x9076403ed2f0fdf4UL, 0x471cafb4183a389fUL, -3511);
   TESTINST_RRI("addu16i.d", "$r26", "$r15", SI16, 0xdec118b1eb13234UL, 0x6ff5ce56111b301UL, 25897);
   TESTINST_RRI("addu16i.d", "$r9", "$r5", SI16, 0x73209239d98fb81aUL, 0x1dc8f0ba4710eba3UL, -21829);
   TESTINST_RRI("addu16i.d", "$r28", "$r25", SI16, 0xa39ba8429a9c13a6UL, 0x4fffb32851c13ff2UL, -23832);
   TESTINST_RRI("addu16i.d", "$r23", "$r30", SI16, 0x8abd919f5ea43b1UL, 0x40078826f7336f0eUL, -32189);
   TESTINST_RRI("addu16i.d", "$r28", "$r24", SI16, 0x695e543e25e7d3e4UL, 0x30279db606efa8ecUL, 16372);
   TESTINST_RRI("addu16i.d", "$r4", "$r18", SI16, 0xa125cadb71209757UL, 0xff287b5e7fb2a2baUL, -28041);
   TESTINST_RRI("addu16i.d", "$r5", "$r17", SI16, 0xd5d3e6da7c594ca9UL, 0x2bc9be0ef252584cUL, -11268);
   TESTINST_RRI("addu16i.d", "$r29", "$r28", SI16, 0xee0391151007613UL, 0xae616c39d87c4b6eUL, -15645);

   /* ---------------- andi rd, rj, ui12 ---------------- */
   TESTINST_RRI("andi", "$r28", "$r18", UI12, 0xd62f833fbbd483b3UL, 0xa2f268cdcf18dd00UL, 1288);
   TESTINST_RRI("andi", "$r12", "$r13", UI12, 0xc40efc9a74a3a13bUL, 0xfd609200795f877cUL, 153);
   TESTINST_RRI("andi", "$r6", "$r18", UI12, 0x79ee7ee7a7865b79UL, 0x644bec92dca1ad7fUL, 3633);
   TESTINST_RRI("andi", "$r5", "$r31", UI12, 0x2d64be0e5c2ec0f6UL, 0x87253b6589f182c7UL, 3299);
   TESTINST_RRI("andi", "$r28", "$r5", UI12, 0xf2e4ed85d98a1860UL, 0x9f58e4edd98b60d1UL, 3189);
   TESTINST_RRI("andi", "$r18", "$r29", UI12, 0x3c067920d48cf0d2UL, 0x2bf35e68c503ecfeUL, 4031);
   TESTINST_RRI("andi", "$r20", "$r24", UI12, 0xe1d95be05fd57a64UL, 0xd33e771521b24bd3UL, 3252);
   TESTINST_RRI("andi", "$r6", "$r23", UI12, 0x23341b2d86d02365UL, 0x16de10f2b4a45064UL, 1665);
   TESTINST_RRI("andi", "$r27", "$r14", UI12, 0xd7db9d77aea4dcf5UL, 0x142272b737435eb7UL, 325);
   TESTINST_RRI("andi", "$r23", "$r16", UI12, 0x57fee53581b09718UL, 0x2ace25d9e2ddbaaUL, 1056);

   /* ---------------- ori rd, rj, ui12 ---------------- */
   TESTINST_RRI("ori", "$r26", "$r13", UI12, 0x6d47cf7e5bb5c13eUL, 0x93aed4996805ba3bUL, 3251);
   TESTINST_RRI("ori", "$r10", "$r25", UI12, 0x42f0332098f938afUL, 0xd7916fe8d569567bUL, 568);
   TESTINST_RRI("ori", "$r12", "$r17", UI12, 0xc507d4150a742b76UL, 0x2b9a102a5b5b15f7UL, 1798);
   TESTINST_RRI("ori", "$r15", "$r15", UI12, 0xa54ad5ecc0e72adbUL, 0x37c18ad4ec6e678cUL, 1781);
   TESTINST_RRI("ori", "$r5", "$r4", UI12, 0x1f388b2a2b18004dUL, 0xb5fa23fbb02eeedbUL, 682);
   TESTINST_RRI("ori", "$r27", "$r24", UI12, 0x73b086f8a8b4d7b5UL, 0xd23e30ab1e45470aUL, 1931);
   TESTINST_RRI("ori", "$r28", "$r6", UI12, 0x972967beac695928UL, 0x2c701d0bc28816c5UL, 3593);
   TESTINST_RRI("ori", "$r27", "$r4", UI12, 0x54fecbbf0a06e5a6UL, 0xf0b6d846464a3331UL, 3679);
   TESTINST_RRI("ori", "$r9", "$r16", UI12, 0x71f3cd001c729062UL, 0xc5720758095e4592UL, 905);
   TESTINST_RRI("ori", "$r26", "$r7", UI12, 0xd7ce86800c3c0f4bUL, 0xc4a58f787cdf5bb2UL, 3473);

   /* ---------------- xori rd, rj, ui12 ---------------- */
   TESTINST_RRI("xori", "$r27", "$r31", UI12, 0xe6d49c2dc629fbc7UL, 0x91832665d1a898e2UL, 2690);
   TESTINST_RRI("xori", "$r15", "$r5", UI12, 0xada49c0d48beffc5UL, 0xe3cf426f1be4766UL, 697);
   TESTINST_RRI("xori", "$r9", "$r20", UI12, 0x174a71d6d3757e3eUL, 0x25ed4678037622beUL, 2268);
   TESTINST_RRI("xori", "$r31", "$r15", UI12, 0x1fac1694b40fbf2eUL, 0x4fe4fb2e0b660ca2UL, 3817);
   TESTINST_RRI("xori", "$r17", "$r14", UI12, 0x2dc443400df4e153UL, 0x1db25e602ef8ece5UL, 3929);
   TESTINST_RRI("xori", "$r4", "$r28", UI12, 0x5fb5ad5a84e97835UL, 0xc52da11293641639UL, 2735);
   TESTINST_RRI("xori", "$r5", "$r13", UI12, 0x5c5fc4ba45da005fUL, 0xe46f853b7d602b84UL, 1153);
   TESTINST_RRI("xori", "$r30", "$r26", UI12, 0x1419915b6f92678bUL, 0xa984612f1266da94UL, 3867);
   TESTINST_RRI("xori", "$r13", "$r13", UI12, 0xc2b8fd036ba6314bUL, 0x4cf49604f644713cUL, 3426);
   TESTINST_RRI("xori", "$r25", "$r23", UI12, 0xde46e3673c9a75dcUL, 0xfa1177a89f08c81eUL, 2669);

   /* ---------------- sll.w rd, rj, rk ---------------- */
   TESTINST_RRR("sll.w", "$r13", "$r8", "$r12", 0x26131fa72f4b76f1UL, 0xf34f7108538078d0UL, 0x10bbd12a8e087501UL);
   TESTINST_RRR("sll.w", "$r29", "$r8", "$r15", 0xb6f529da4017d0d9UL, 0x49fbfb11ef643171UL, 0x9d0425e747d11bdeUL);
   TESTINST_RRR("sll.w", "$r30", "$r31", "$r12", 0xcfc5236f5c070644UL, 0xba8301a1087b3a96UL, 0xff7589561824e1beUL);
   TESTINST_RRR("sll.w", "$r28", "$r10", "$r7", 0x37fa51674df87149UL, 0x39212605c5d0cf7dUL, 0x18a8e323326ce5aaUL);
   TESTINST_RRR("sll.w", "$r8", "$r9", "$r14", 0x707a9e0ece8abe40UL, 0x94b7b20a80c16c7bUL, 0x6887c46efb4cc181UL);
   TESTINST_RRR("sll.w", "$r8", "$r4", "$r24", 0xd718a01b03a53964UL, 0x8ebd8bfeec304e2aUL, 0x6b4a83a6838b5d1UL);
   TESTINST_RRR("sll.w", "$r23", "$r31", "$r27", 0xf50cab824a06d30eUL, 0xa8ee12cbd8dec935UL, 0x118002b3f0cecbabUL);
   TESTINST_RRR("sll.w", "$r8", "$r25", "$r26", 0x8163368243faadeeUL, 0x3a04f47bf19a4cc8UL, 0x6a58cd3a57b4eeb4UL);
   TESTINST_RRR("sll.w", "$r25", "$r13", "$r12", 0x3d6831e1afab1b1aUL, 0x9ee672580cb39777UL, 0x9084acd2bc7404caUL);
   TESTINST_RRR("sll.w", "$r20", "$r5", "$r29", 0x90f7ee3ff75817a6UL, 0xe4ae07989d6148d7UL, 0x3e208bfcf046fffdUL);

   /* ---------------- srl.w rd, rj, rk ---------------- */
   TESTINST_RRR("srl.w", "$r20", "$r29", "$r30", 0xff3f6b79b5e2b56dUL, 0x1195aa09fa92d26bUL, 0xa93a8fd11ad5ae99UL);
   TESTINST_RRR("srl.w", "$r8", "$r15", "$r4", 0x5d2fb7cd04ecd00cUL, 0x47bf914b6eca2852UL, 0x1bc63138cc45a75cUL);
   TESTINST_RRR("srl.w", "$r20", "$r12", "$r18", 0x61fa22abda7c7b02UL, 0x9341cf09aa2e106eUL, 0x2dea831e9e121355UL);
   TESTINST_RRR("srl.w", "$r30", "$r20", "$r26", 0x43e0249584da52dbUL, 0x482a209e436cda53UL, 0xb323a7f463f80660UL);
   TESTINST_RRR("srl.w", "$r31", "$r16", "$r28", 0x4b10d05d93bf7288UL, 0x6d0330e88122d7c1UL, 0xc531cf8c92d53d03UL);
   TESTINST_RRR("srl.w", "$r31", "$r15", "$r31", 0xd4654233c7648c3aUL, 0x12e6fc2a04cbf809UL, 0xcfe1c1b558a94808UL);
   TESTINST_RRR("srl.w", "$r10", "$r30", "$r19", 0x602dee9c45a3b99bUL, 0x3ce0a6ac2acf19faUL, 0xdb5fab4bc2f82e7aUL);
   TESTINST_RRR("srl.w", "$r17", "$r9", "$r23", 0x45106f11d4a57641UL, 0x5354795b675edacUL, 0xc67578c28ed7b6c7UL);
   TESTINST_RRR("srl.w", "$r25", "$r26", "$r29", 0x1dc3b8477fba650cUL, 0x814377a71768e75UL, 0x60276c0e316db833UL);
   TESTINST_RRR("srl.w", "$r31", "$r7", "$r30", 0x360fc92a085c2e14UL, 0x1b44ec96def89449UL, 0x56d6c5d85a81ed1fUL);

   /* ---------------- sra.w rd, rj, rk ---------------- */
   TESTINST_RRR("sra.w", "$r10", "$r17", "$r19", 0x576f2bfc771641b8UL, 0xfb1fb20b98a54405UL, 0xb20e9dae5a212078UL);
   TESTINST_RRR("sra.w", "$r12", "$r16", "$r31", 0xbfdbb9a90ccc08a0UL, 0xb5d3c7f3b1a800a6UL, 0x57c3ff79f3b4198bUL);
   TESTINST_RRR("sra.w", "$r18", "$r16", "$r5", 0xadcb6c153538b6b1UL, 0x99e245813e90b5e9UL, 0x7adff58363d5ebd2UL);
   TESTINST_RRR("sra.w", "$r17", "$r28", "$r25", 0x7faea6a29686caf9UL, 0x801d40ea40b19beeUL, 0xf5174f678600d3fUL);
   TESTINST_RRR("sra.w", "$r8", "$r27", "$r13", 0x86e5534832150e05UL, 0x47bb53d1cdc3560fUL, 0x917e2b49633a0f44UL);
   TESTINST_RRR("sra.w", "$r26", "$r18", "$r20", 0xbfb83a0d762c171aUL, 0xbf67ed78d934d37cUL, 0x9f377995293fcc6bUL);
   TESTINST_RRR("sra.w", "$r5", "$r25", "$r19", 0x266703af59334b0fUL, 0x4ed92cdab9f641c9UL, 0x5da1d0b8846d1a3dUL);
   TESTINST_RRR("sra.w", "$r19", "$r27", "$r24", 0x72557561b3b40007UL, 0xd5db278ea099b3b5UL, 0x50b4a888b898610fUL);
   TESTINST_RRR("sra.w", "$r16", "$r10", "$r4", 0xb349f888f1809ba3UL, 0x23d60a1fc100d89eUL, 0xc2846cc882dbc8e2UL);
   TESTINST_RRR("sra.w", "$r23", "$r10", "$r31", 0xd7bdeddd344bb5afUL, 0xa015a07c13ff2234UL, 0x7c0fe410ce063a85UL);

   /* ---------------- sll.d rd, rj, rk ---------------- */
   TESTINST_RRR("sll.d", "$r28", "$r17", "$r10", 0x167adf26efd66416UL, 0xb861ba6e0aadf304UL, 0xa19e21ba0f406c33UL);
   TESTINST_RRR("sll.d", "$r18", "$r29", "$r13", 0x3e8ea4dc3a9d9b44UL, 0x28ccf5dfa9cdc3b2UL, 0x33ef837a5a476bdcUL);
   TESTINST_RRR("sll.d", "$r23", "$r27", "$r29", 0x23e29c76deed70caUL, 0x9e2265d8422e78dUL, 0xe9cc62bfd8a7c913UL);
   TESTINST_RRR("sll.d", "$r16", "$r17", "$r17", 0xf5e858c7445fceddUL, 0x6735e4cf2fcb78fbUL, 0x726dd10e13b62663UL);
   TESTINST_RRR("sll.d", "$r17", "$r15", "$r29", 0xfc1dbfc0551f8813UL, 0xec45100b21a74025UL, 0x186d3b737cbfd39aUL);
   TESTINST_RRR("sll.d", "$r19", "$r15", "$r9", 0xbb01afe39a1e17b6UL, 0x3e66dd1100acc44aUL, 0xa9c74257f6e39cdfUL);
   TESTINST_RRR("sll.d", "$r23", "$r9", "$r31", 0x945b101751c38d12UL, 0x262d14baae546199UL, 0x7ccdd8a7840948dfUL);
   TESTINST_RRR("sll.d", "$r5", "$r31", "$r28", 0xa88eaecc1405995bUL, 0xd96ed500aff4596bUL, 0x6994841a196c562eUL);
   TESTINST_RRR("sll.d", "$r27", "$r10", "$r25", 0x1e9540fa8237a849UL, 0x9aad6101b2470a60UL, 0x90c95628696f752fUL);
   TESTINST_RRR("sll.d", "$r4", "$r26", "$r18", 0xb4dc3cdeab2e8454UL, 0xd27a92db3b2906cUL, 0x2bc7647c40c0b375UL);

   /* ---------------- srl.d rd, rj, rk ---------------- */
   TESTINST_RRR("srl.d", "$r6", "$r27", "$r13", 0x66ebeca9a7fad574UL, 0xdc837ce646ea6b51UL, 0xa57259e1758c564bUL);
   TESTINST_RRR("srl.d", "$r6", "$r20", "$r5", 0x91794316e6c5e65UL, 0xdc7c47d39d64a16UL, 0x35f029b9942e11c8UL);
   TESTINST_RRR("srl.d", "$r15", "$r5", "$r4", 0xbc963842b3ebc906UL, 0x42ea773b0bd19807UL, 0xd05cd2c4b01ea630UL);
   TESTINST_RRR("srl.d", "$r18", "$r25", "$r28", 0x30d908baaa31230eUL, 0x779272ae228746a5UL, 0xf7b665809a3f303bUL);
   TESTINST_RRR("srl.d", "$r5", "$r28", "$r27", 0x1f1d414f1d0f1feUL, 0x647277d3759d74bfUL, 0xa5c5fce39b4a1810UL);
   TESTINST_RRR("srl.d", "$r24", "$r9", "$r26", 0x5fa44419162fc2c8UL, 0x9d2a589e6f6b3440UL, 0x810a615115238d8dUL);
   TESTINST_RRR("srl.d", "$r31", "$r23", "$r30", 0xfa1a7ad64758b758UL, 0xe3d69d99e87b4297UL, 0x87fd8dc0a78e86bbUL);
   TESTINST_RRR("srl.d", "$r26", "$r10", "$r24", 0x540888639a787231UL, 0x168791cefeb1660aUL, 0xd02b158115db9cdfUL);
   TESTINST_RRR("srl.d", "$r23", "$r15", "$r12", 0xff3e950565409999UL, 0xe15a01fa0e34ea3bUL, 0x237aba34fe552f8eUL);
   TESTINST_RRR("srl.d", "$r8", "$r16", "$r4", 0x825bafd36cc0d32eUL, 0x321677304d1b1406UL, 0xca68c6c83dfa5837UL);

   /* ---------------- sra.d rd, rj, rk ---------------- */
   TESTINST_RRR("sra.d", "$r23", "$r19", "$r16", 0x4cab63abd8f64774UL, 0x2c007c3ac68d7c80UL, 0xd8f4ac963a8b2c01UL);
   TESTINST_RRR("sra.d", "$r18", "$r30", "$r25", 0x531de73fca30361aUL, 0x2857ba730cd281ffUL, 0xacab0fe400e4c113UL);
   TESTINST_RRR("sra.d", "$r31", "$r13", "$r10", 0x3184416bc93a5e26UL, 0xad5864bc4022de96UL, 0xf7007bdbf1f728abUL);
   TESTINST_RRR("sra.d", "$r6", "$r25", "$r23", 0x9184d2df291f3402UL, 0x7c0b117dcad80c03UL, 0x35b29b0dde1a94bdUL);
   TESTINST_RRR("sra.d", "$r16", "$r6", "$r29", 0x2849e543d35dff5fUL, 0x9f13f36a632a3fUL, 0xf31f881e12072fe2UL);
   TESTINST_RRR("sra.d", "$r7", "$r29", "$r10", 0x25c763f8366139ddUL, 0xfd77fd6e69e371c6UL, 0xcaa2ec6ad4f3b996UL);
   TESTINST_RRR("sra.d", "$r24", "$r25", "$r26", 0x472602300b4f04c9UL, 0x54ceea832a5677e9UL, 0x5f63e9d9d6eb4af0UL);
   TESTINST_RRR("sra.d", "$r23", "$r4", "$r27", 0xe8b449325a0ed51eUL, 0xd96928476f8441a5UL, 0x7e1ae8fd9c849dceUL);
   TESTINST_RRR("sra.d", "$r15", "$r9", "$r12", 0x71601a1a2b155f51UL, 0xbcbb1d162563240UL, 0x5a906ad2f4abb4c7UL);
   TESTINST_RRR("sra.d", "$r16", "$r29", "$r23", 0x1686886f27d397fbUL, 0x851328b2655e5689UL, 0x1634457590cd4033UL);

   /* ---------------- rotr.w rd, rj, rk ---------------- */
   TESTINST_RRR("rotr.w", "$r8", "$r5", "$r18", 0xc4394aae4c13908bUL, 0xa0c5728d1211b595UL, 0x3d562746b3943f3bUL);
   TESTINST_RRR("rotr.w", "$r19", "$r18", "$r10", 0x284b501639de116bUL, 0x4248ad6cc0107902UL, 0xb41907b756bf8004UL);
   TESTINST_RRR("rotr.w", "$r29", "$r8", "$r4", 0x2656b50c7d689f19UL, 0x7b5d21fdce9bcb73UL, 0x5b212fbe9e6b8522UL);
   TESTINST_RRR("rotr.w", "$r25", "$r6", "$r30", 0x4c79ed7a1695fc25UL, 0x6bac1698a978f50fUL, 0xf1d58570dfb10203UL);
   TESTINST_RRR("rotr.w", "$r14", "$r18", "$r6", 0xe894476b4ebbff23UL, 0x1398b65ae1e91c98UL, 0xebb6c3f5f689d2d8UL);
   TESTINST_RRR("rotr.w", "$r19", "$r29", "$r26", 0x2595423cc93ecd7cUL, 0x6c462c2d29d8f908UL, 0x19142efd8e0b48b8UL);
   TESTINST_RRR("rotr.w", "$r23", "$r10", "$r25", 0x68b4d913b267a3a2UL, 0x69afb673907e4506UL, 0xbd09ff2ed890862dUL);
   TESTINST_RRR("rotr.w", "$r9", "$r14", "$r27", 0x17a45b8cbdebd6efUL, 0x33effef864846356UL, 0x3f52e437f2d5da62UL);
   TESTINST_RRR("rotr.w", "$r5", "$r12", "$r23", 0x2d191b1a9707cf26UL, 0x86fa75433dac3d39UL, 0x21136a02424e5da4UL);
   TESTINST_RRR("rotr.w", "$r29", "$r18", "$r27", 0x7d989f74f9944f8dUL, 0x50fe5829a153e6UL, 0x926776f9140b06fcUL);

   /* ---------------- rotr.d rd, rj, rk ---------------- */
   TESTINST_RRR("rotr.d", "$r29", "$r19", "$r13", 0x1e02c0c28ec3f9b1UL, 0xf2e79e6ff240b188UL, 0x60f500663eddf444UL);
   TESTINST_RRR("rotr.d", "$r30", "$r4", "$r14", 0x97f6be8229e2e822UL, 0xf79aaeb2c03a2113UL, 0xbbdb2cb642605ed7UL);
   TESTINST_RRR("rotr.d", "$r6", "$r19", "$r7", 0x1611806010ce99d8UL, 0xcb64270e0fc5b4c7UL, 0xeda6972c46af03cUL);
   TESTINST_RRR("rotr.d", "$r4", "$r15", "$r30", 0xe63084e97bd0efb3UL, 0x6e1aa322e38e9b66UL, 0xa7df0f1d92106e2dUL);
   TESTINST_RRR("rotr.d", "$r16", "$r27", "$r10", 0x1ff92fbb0f10ff9aUL, 0x15c2eb91c9ae124UL, 0x8b4c97ee7f9bc2faUL);
   TESTINST_RRR("rotr.d", "$r28", "$r7", "$r25", 0xbd766a63bbead21cUL, 0xd97b509610db5e7UL, 0x3151203010315af5UL);
   TESTINST_RRR("rotr.d", "$r9", "$r20", "$r23", 0x8a2bb5eacea50d68UL, 0x947ec1930151adb9UL, 0xc2f39e045d278b7bUL);
   TESTINST_RRR("rotr.d", "$r25", "$r13", "$r23", 0xcaddb8ea7bd492c7UL, 0x416a1b790dbf45cbUL, 0x44c59965e1c6af25UL);
   TESTINST_RRR("rotr.d", "$r14", "$r7", "$r31", 0x8ca18b58047c8b5aUL, 0x93a6cdc3585b5446UL, 0x70cd84ec07e33cefUL);
   TESTINST_RRR("rotr.d", "$r14", "$r9", "$r4", 0x48bd5c133004f490UL, 0xad095be0915fe20bUL, 0xc1fff6ff603a47b3UL);

   /* ---------------- slli.w rd, rj, ui5 ---------------- */
   TESTINST_RRI("slli.w", "$r18", "$r8", UI5, 0xe7f8823a2989c395UL, 0xf0ccc85519ad1e0aUL, 10);
   TESTINST_RRI("slli.w", "$r27", "$r17", UI5, 0x2e66b550a3bb071dUL, 0x20943aa3eaa4024eUL, 30);
   TESTINST_RRI("slli.w", "$r27", "$r23", UI5, 0x70daa2bee8209243UL, 0x2e9160afd2e28a64UL, 31);
   TESTINST_RRI("slli.w", "$r10", "$r13", UI5, 0x701c424632b5dc29UL, 0x591054db6afe1725UL, 12);
   TESTINST_RRI("slli.w", "$r7", "$r15", UI5, 0xdd1d7fe3ae579499UL, 0x2e077f689088c0c7UL, 19);
   TESTINST_RRI("slli.w", "$r6", "$r8", UI5, 0xff732113ddaab79bUL, 0x9cacf8e6d9e37f97UL, 12);
   TESTINST_RRI("slli.w", "$r5", "$r19", UI5, 0xcef75ddd2adc5853UL, 0xcc24ed9167fd06eaUL, 22);
   TESTINST_RRI("slli.w", "$r17", "$r8", UI5, 0x3c8788fed3e8a049UL, 0xccf9b2d2c2e80251UL, 7);
   TESTINST_RRI("slli.w", "$r14", "$r29", UI5, 0xe1b0b077db4f08eUL, 0x76aea4b9ae43cdfbUL, 10);
   TESTINST_RRI("slli.w", "$r23", "$r30", UI5, 0x13d8514aeb0dc12bUL, 0x9c8352804e7e8ccbUL, 26);

   /* ---------------- slli.d rd, rj, ui6 ---------------- */
   TESTINST_RRI("slli.d", "$r27", "$r28", UI6, 0x689a2c4141835926UL, 0x1b6ff38e611d1e4dUL, 5);
   TESTINST_RRI("slli.d", "$r5", "$r20", UI6, 0xff3391c2323defa6UL, 0xe99a134a0c1a2574UL, 1);
   TESTINST_RRI("slli.d", "$r27", "$r7", UI6, 0xc32d8fb319ba47e6UL, 0xc6530e0e601d3631UL, 61);
   TESTINST_RRI("slli.d", "$r5", "$r26", UI6, 0x979553ff112cdf52UL, 0x931e420364fdcacaUL, 45);
   TESTINST_RRI("slli.d", "$r27", "$r5", UI6, 0xa7f70b048a4087b0UL, 0xc1b829210c3cd5a9UL, 60);
   TESTINST_RRI("slli.d", "$r23", "$r10", UI6, 0xcd547af78ac66ca7UL, 0xa2c0802de6c82645UL, 59);
   TESTINST_RRI("slli.d", "$r13", "$r30", UI6, 0x410b8f25e1234eeUL, 0xdbaacfe884cda24dUL, 56);
   TESTINST_RRI("slli.d", "$r16", "$r4", UI6, 0x44a2ff35045ec37cUL, 0xee2240010629a8eeUL, 20);
   TESTINST_RRI("slli.d", "$r19", "$r20", UI6, 0x8617d88408d75cacUL, 0xba15483820d66ae7UL, 25);
   TESTINST_RRI("slli.d", "$r24", "$r27", UI6, 0x669e0e9b99d5b604UL, 0xf5d1ffc374e53c7dUL, 13);

   /* ---------------- srli.w rd, rj, ui5 ---------------- */
   TESTINST_RRI("srli.w", "$r20", "$r16", UI5, 0x7f5310ac5eaa9924UL, 0xea8b69613d183eeUL, 10);
   TESTINST_RRI("srli.w", "$r13", "$r15", UI5, 0x5f4d9313f9224389UL, 0xd544272206f4e814UL, 0);
   TESTINST_RRI("srli.w", "$r17", "$r18", UI5, 0xd9b2c942f996cc8aUL, 0x704cd1d89de5c2b4UL, 7);
   TESTINST_RRI("srli.w", "$r27", "$r28", UI5, 0xa3eef8efc97e0d4fUL, 0x8c449e6236daa7a2UL, 18);
   TESTINST_RRI("srli.w", "$r9", "$r10", UI5, 0x6c044927152e5fc9UL, 0x592a1607944e0109UL, 29);
   TESTINST_RRI("srli.w", "$r8", "$r24", UI5, 0xcaa01b37d49db675UL, 0x5e35848bbc958164UL, 31);
   TESTINST_RRI("srli.w", "$r6", "$r16", UI5, 0xe2fbe1accb343769UL, 0x85f5e17c7d785222UL, 18);
   TESTINST_RRI("srli.w", "$r18", "$r25", UI5, 0x4653c07e0627825fUL, 0x44fffa524ffd0417UL, 31);
   TESTINST_RRI("srli.w", "$r5", "$r26", UI5, 0x817ebd7154c8ed46UL, 0xc7399a9899fc5958UL, 22);
   TESTINST_RRI("srli.w", "$r27", "$r4", UI5, 0x3e4b17b34f2b08d0UL, 0x5bedb97aefd697f4UL, 27);

   /* ---------------- srli.d rd, rj, ui6 ---------------- */
   TESTINST_RRI("srli.d", "$r31", "$r9", UI6, 0x8fc21da189af52edUL, 0x235bf33e3e612a15UL, 51);
   TESTINST_RRI("srli.d", "$r26", "$r7", UI6, 0xcd1eaac4df2531ddUL, 0xe87216fce9c75788UL, 36);
   TESTINST_RRI("srli.d", "$r6", "$r31", UI6, 0xc0282beeb7dc6618UL, 0x8b58604d6be3e8e0UL, 29);
   TESTINST_RRI("srli.d", "$r20", "$r6", UI6, 0x1546fdd9fc133e39UL, 0x74067840bb05a992UL, 18);
   TESTINST_RRI("srli.d", "$r28", "$r20", UI6, 0xaa1f88b09e13e4c6UL, 0x6e153faa5221e893UL, 28);
   TESTINST_RRI("srli.d", "$r26", "$r4", UI6, 0x2ba2151c80dbea7aUL, 0x21246f3c7063edf9UL, 55);
   TESTINST_RRI("srli.d", "$r28", "$r29", UI6, 0xcd72eff1b5aa0877UL, 0x5d9488c1d61a1544UL, 34);
   TESTINST_RRI("srli.d", "$r13", "$r7", UI6, 0x5953b78fbd8109a9UL, 0x862731652b653859UL, 62);
   TESTINST_RRI("srli.d", "$r29", "$r18", UI6, 0xab821449d149a976UL, 0xcb73553146cc4bdcUL, 25);
   TESTINST_RRI("srli.d", "$r28", "$r7", UI6, 0x31272fa88123357dUL, 0xe9359f7a9f92ec5UL, 2);

   /* ---------------- srai.w rd, rj, ui5 ---------------- */
   TESTINST_RRI("srai.w", "$r26", "$r23", UI5, 0xe73a55c2b7005c01UL, 0xfcd659254f4b3fe7UL, 2);
   TESTINST_RRI("srai.w", "$r31", "$r10", UI5, 0x2e0c4330fae0890aUL, 0xa76ca364a204c82bUL, 0);
   TESTINST_RRI("srai.w", "$r31", "$r8", UI5, 0x64790bb6e8674f68UL, 0xce5594f964c4a026UL, 0);
   TESTINST_RRI("srai.w", "$r15", "$r31", UI5, 0xccfb53c708026acdUL, 0xce185873627515b5UL, 27);
   TESTINST_RRI("srai.w", "$r16", "$r28", UI5, 0x994c4d22e90185a2UL, 0x49995d51019e1050UL, 1);
   TESTINST_RRI("srai.w", "$r13", "$r16", UI5, 0x484408b57b3ab89UL, 0x437401347e23c399UL, 16);
   TESTINST_RRI("srai.w", "$r4", "$r9", UI5, 0xd1d936105b7cca3UL, 0xd49c3c65e292b942UL, 7);
   TESTINST_RRI("srai.w", "$r24", "$r15", UI5, 0xaa9377005232ec93UL, 0xde29d0172b40f03dUL, 10);
   TESTINST_RRI("srai.w", "$r19", "$r14", UI5, 0xa49c65a4c2cde36dUL, 0x782e0d4b8a7a28d0UL, 24);
   TESTINST_RRI("srai.w", "$r24", "$r27", UI5, 0x404f816ff696bbc8UL, 0x1b6900e15f252315UL, 24);

   /* ---------------- srai.d rd, rj, ui6 ---------------- */
   TESTINST_RRI("srai.d", "$r24", "$r4", UI6, 0x96250384fede78c7UL, 0x6c501d9ec5e9e731UL, 22);
   TESTINST_RRI("srai.d", "$r30", "$r19", UI6, 0xcfc52d7caaf7bf47UL, 0x82499a30d50f8b83UL, 17);
   TESTINST_RRI("srai.d", "$r12", "$r12", UI6, 0x628a1a46bbe30c16UL, 0xaba392c50d63ea53UL, 5);
   TESTINST_RRI("srai.d", "$r24", "$r9", UI6, 0x21c1bb01f0253d8UL, 0xb35e31d92548a2feUL, 2);
   TESTINST_RRI("srai.d", "$r28", "$r7", UI6, 0x2a5ac0a983332ec3UL, 0x2297ae499a473c6dUL, 62);
   TESTINST_RRI("srai.d", "$r8", "$r17", UI6, 0xa27cf36651750e09UL, 0x1984e046b042d0cfUL, 31);
   TESTINST_RRI("srai.d", "$r25", "$r16", UI6, 0x7df3822fb20b8dedUL, 0xb4e464563029fac8UL, 37);
   TESTINST_RRI("srai.d", "$r14", "$r5", UI6, 0xe8c1939c13a2e6caUL, 0x6a22077c63497a9aUL, 57);
   TESTINST_RRI("srai.d", "$r25", "$r15", UI6, 0xf2df68e25cccf72eUL, 0xe0af648201f919fcUL, 10);
   TESTINST_RRI("srai.d", "$r6", "$r15", UI6, 0xa24591b35142aa9cUL, 0x12b20ac67de77b8dUL, 49);

   /* ---------------- rotri.w rd, rj, ui5 ---------------- */
   TESTINST_RRI("rotri.w", "$r18", "$r6", UI5, 0xf0c65b137926ba00UL, 0x95e0f5f057a212c5UL, 20);
   TESTINST_RRI("rotri.w", "$r9", "$r16", UI5, 0xe36356471d2a7e18UL, 0xb8af3071021bd869UL, 27);
   TESTINST_RRI("rotri.w", "$r5", "$r31", UI5, 0x5992fc9cfce2ebe9UL, 0x6c427c821603d01aUL, 1);
   TESTINST_RRI("rotri.w", "$r27", "$r13", UI5, 0x239c57dca2ab060UL, 0xed54e28825b25471UL, 23);
   TESTINST_RRI("rotri.w", "$r18", "$r18", UI5, 0xb84df2305a710936UL, 0x8aae5248c6d4973cUL, 7);
   TESTINST_RRI("rotri.w", "$r4", "$r27", UI5, 0x730e1701570ac9fcUL, 0xd55b9d54232536e7UL, 29);
   TESTINST_RRI("rotri.w", "$r19", "$r18", UI5, 0x36dbceffa501d8dcUL, 0x8415238fa1dd314fUL, 0);
   TESTINST_RRI("rotri.w", "$r13", "$r24", UI5, 0xc1ac428ddf5193UL, 0x3b588028fcfbb0a8UL, 21);
   TESTINST_RRI("rotri.w", "$r14", "$r25", UI5, 0x733414543ca8145eUL, 0xded24831de35be08UL, 29);
   TESTINST_RRI("rotri.w", "$r27", "$r5", UI5, 0x60afaebb36d22ba0UL, 0xfd31a16f03582b5UL, 8);

   /* ---------------- rotri.d rd, rj, ui6 ---------------- */
   TESTINST_RRI("rotri.d", "$r20", "$r7", UI6, 0xe112a6d47c0444c1UL, 0xbd9bbb91bdc381c5UL, 53);
   TESTINST_RRI("rotri.d", "$r27", "$r16", UI6, 0xf254a827c1ef7351UL, 0x3de084650f757cebUL, 62);
   TESTINST_RRI("rotri.d", "$r30", "$r17", UI6, 0x31c36a8c83999eb2UL, 0x107098a9863e85d5UL, 10);
   TESTINST_RRI("rotri.d", "$r29", "$r8", UI6, 0xf2e7a25c121af3c3UL, 0xb177c110c3dd3225UL, 46);
   TESTINST_RRI("rotri.d", "$r4", "$r26", UI6, 0xdd94ff60f2e1abffUL, 0xb76d3e4a0af02e4dUL, 45);
   TESTINST_RRI("rotri.d", "$r10", "$r9", UI6, 0x6064d48d901beca7UL, 0xea20b33360134ab2UL, 42);
   TESTINST_RRI("rotri.d", "$r4", "$r26", UI6, 0x27f1e63c8f7f71cfUL, 0xf4c5c8a69f37a1bdUL, 27);
   TESTINST_RRI("rotri.d", "$r9", "$r16", UI6, 0x7d4cb07a3ab72944UL, 0xd5ee210421c6080eUL, 20);
   TESTINST_RRI("rotri.d", "$r24", "$r26", UI6, 0x1ce66a79f3e45e6fUL, 0x6e1767144ffa6e2dUL, 4);
   TESTINST_RRI("rotri.d", "$r4", "$r18", UI6, 0x4173f8102b03399UL, 0xde7066568917d899UL, 46);

   /* ---------------- ext.w.h rd, rj ---------------- */
   TESTINST_RR("ext.w.h", "$r17", "$r14", 0x58af862c6fc4208dUL, 0x6235b0cfe4eed6edUL);
   TESTINST_RR("ext.w.h", "$r31", "$r20", 0x425af3dcd83fa9fdUL, 0x6e59403101a538f1UL);
   TESTINST_RR("ext.w.h", "$r18", "$r27", 0xcb140226bf788367UL, 0x58a5430ee4e1616eUL);
   TESTINST_RR("ext.w.h", "$r15", "$r10", 0xd3debaf05f7d909fUL, 0x6f7083340247fb12UL);
   TESTINST_RR("ext.w.h", "$r12", "$r15", 0x5dc6f7191af80bcfUL, 0xb1f1c8f4b11c03d9UL);
   TESTINST_RR("ext.w.h", "$r7", "$r15", 0x5ffe304a5c9dc9d2UL, 0x102fb4fa33193103UL);
   TESTINST_RR("ext.w.h", "$r16", "$r16", 0x533616e37505799fUL, 0xf988c7255086f4f5UL);
   TESTINST_RR("ext.w.h", "$r13", "$r25", 0x805a406557ed3facUL, 0xdc6ce0f2993b219bUL);
   TESTINST_RR("ext.w.h", "$r19", "$r20", 0xcc49c20125c4755dUL, 0xde7b765222a9703aUL);
   TESTINST_RR("ext.w.h", "$r18", "$r7", 0xe0dd9155cbe168c6UL, 0xc1063421eae07663UL);

   /* ---------------- ext.w.b rd, rj ---------------- */
   TESTINST_RR("ext.w.b", "$r16", "$r23", 0x21666e814555aa02UL, 0x926b8d68b5c40592UL);
   TESTINST_RR("ext.w.b", "$r8", "$r20", 0xf68ae0a0ac497dedUL, 0xbfb5d489716d0c5UL);
   TESTINST_RR("ext.w.b", "$r24", "$r15", 0xbc84e54c82fd6e51UL, 0x7d814b11e5eb07f6UL);
   TESTINST_RR("ext.w.b", "$r31", "$r17", 0x14e575a8dda1f0d3UL, 0x6a111e663a52244cUL);
   TESTINST_RR("ext.w.b", "$r16", "$r8", 0x911acc218fcf640bUL, 0xac1405ad05b23e43UL);
   TESTINST_RR("ext.w.b", "$r28", "$r8", 0x77fb13eaa8995607UL, 0x5c97a81f12da7d3UL);
   TESTINST_RR("ext.w.b", "$r9", "$r23", 0xb88cfdb98683e15eUL, 0x74893b34973e16cbUL);
   TESTINST_RR("ext.w.b", "$r31", "$r4", 0xc7168cb4f7d079e4UL, 0xf4fc215bc2c5273eUL);
   TESTINST_RR("ext.w.b", "$r4", "$r18", 0xe2e5dca4727b373UL, 0xa1b97136f32e452bUL);
   TESTINST_RR("ext.w.b", "$r8", "$r29", 0x625eb5236f483daaUL, 0x3ceca34ee347e7c8UL);

   /* ---------------- clo.w rd, rj ---------------- */
   TESTINST_RR("clo.w", "$r4", "$r13", 0xbcca747f77aca28UL, 0x8df71972c1a17096UL);
   TESTINST_RR("clo.w", "$r27", "$r5", 0x98a9e6d99d8e84cbUL, 0xdc59d3c8fc1540e4UL);
   TESTINST_RR("clo.w", "$r9", "$r14", 0xe8e78b162c95ed66UL, 0xdfad6854bbf442e6UL);
   TESTINST_RR("clo.w", "$r13", "$r26", 0xa3db2cf80f9112cdUL, 0x7676463dd6f13f80UL);
   TESTINST_RR("clo.w", "$r7", "$r16", 0xb5213ab31b574031UL, 0x478c19ebdeaa74c0UL);
   TESTINST_RR("clo.w", "$r13", "$r12", 0xd68d9661284fb9d7UL, 0x702bf24fddd8bfe0UL);
   TESTINST_RR("clo.w", "$r18", "$r20", 0x510cd4002aff4c6cUL, 0x4fc898e8b83669eeUL);
   TESTINST_RR("clo.w", "$r5", "$r9", 0x53c0de96f709208dUL, 0xe56d87b898438b5UL);
   TESTINST_RR("clo.w", "$r20", "$r5", 0x96187854fcce4fd1UL, 0xf1248bea6ed8be30UL);
   TESTINST_RR("clo.w", "$r20", "$r31", 0xb1abb4795d411683UL, 0x1025f914a9225e6UL);

   /* ---------------- clz.w rd, rj ---------------- */
   TESTINST_RR("clz.w", "$r19", "$r8", 0x374348642747a8dcUL, 0xd8ec1d547d95ada5UL);
   TESTINST_RR("clz.w", "$r26", "$r4", 0x741ab4d14b9ee1f8UL, 0x99e2ef840817cfffUL);
   TESTINST_RR("clz.w", "$r17", "$r4", 0x45c9ce7217f501b3UL, 0xa387a194cd03bcf1UL);
   TESTINST_RR("clz.w", "$r13", "$r26", 0x69707656f354d758UL, 0xd4a8f8ab02b876b0UL);
   TESTINST_RR("clz.w", "$r25", "$r13", 0x103ce6ee41e094c3UL, 0xd7a85bf4006e655aUL);
   TESTINST_RR("clz.w", "$r5", "$r13", 0x3910578929e7cd4aUL, 0x93c87b02b7b1b603UL);
   TESTINST_RR("clz.w", "$r18", "$r29", 0x10639f8979feefe5UL, 0x9d8b4b8f8493f844UL);
   TESTINST_RR("clz.w", "$r25", "$r16", 0x7b35b3e995b3b44dUL, 0xad953d0ae0b3e870UL);
   TESTINST_RR("clz.w", "$r6", "$r25", 0xda6cbd19f10ef86fUL, 0x1d6665db1162cfb4UL);
   TESTINST_RR("clz.w", "$r5", "$r12", 0x8a6f4d6ec8d7c00dUL, 0x19b40cb8dd8d1679UL);

   /* ---------------- cto.w rd, rj ---------------- */
   TESTINST_RR("cto.w", "$r7", "$r15", 0x7285e9c364562d11UL, 0x963655c7f58de520UL);
   TESTINST_RR("cto.w", "$r4", "$r15", 0x105dceebc6d7e641UL, 0xfc01c17baaca9c46UL);
   TESTINST_RR("cto.w", "$r31", "$r28", 0xdeff9742b93f0591UL, 0x2cf98074b0151f33UL);
   TESTINST_RR("cto.w", "$r13", "$r8", 0xeee665743cd218ffUL, 0xbdd700b2535aa3b7UL);
   TESTINST_RR("cto.w", "$r23", "$r13", 0x1cc22cfd7c0c869cUL, 0x5b848b64decbee8fUL);
   TESTINST_RR("cto.w", "$r12", "$r18", 0x5c32b3db803e5988UL, 0x2d5d1ebf93b79dd0UL);
   TESTINST_RR("cto.w", "$r17", "$r9", 0xc11d806786501f0eUL, 0xd175fe2ca41bda38UL);
   TESTINST_RR("cto.w", "$r24", "$r16", 0x504f9b43af62e2adUL, 0xfce545d98e2361daUL);
   TESTINST_RR("cto.w", "$r24", "$r8", 0xc13ac5668538f5a4UL, 0x3096912e575d64dbUL);
   TESTINST_RR("cto.w", "$r27", "$r17", 0xd27f68629dd8d4fbUL, 0x15ac43632e175a8bUL);

   /* ---------------- ctz.w rd, rj ---------------- */
   TESTINST_RR("ctz.w", "$r8", "$r12", 0xfc9bd3736a3c08bdUL, 0xaebba33c2e268daaUL);
   TESTINST_RR("ctz.w", "$r5", "$r27", 0x5dc8af7bac7db01aUL, 0xabce2f0e113597aaUL);
   TESTINST_RR("ctz.w", "$r18", "$r6", 0xe4ac5b59d8442dfeUL, 0x935d1b694e96bd04UL);
   TESTINST_RR("ctz.w", "$r9", "$r15", 0x9b760f465efbb52eUL, 0x834c9974dba65d99UL);
   TESTINST_RR("ctz.w", "$r13", "$r7", 0x95b5748f5f8bfb38UL, 0x75dd7a9890cdf2d9UL);
   TESTINST_RR("ctz.w", "$r29", "$r17", 0xa25119fd892d1b20UL, 0x38c12e795dc52acfUL);
   TESTINST_RR("ctz.w", "$r15", "$r12", 0x95c2ce0f0446807cUL, 0x623a5915ac8164b2UL);
   TESTINST_RR("ctz.w", "$r6", "$r17", 0xd9034892a300dca8UL, 0x5911fea4e6ce1df3UL);
   TESTINST_RR("ctz.w", "$r10", "$r25", 0xda1e0d0eb34884abUL, 0x8d70d49a10ba8968UL);
   TESTINST_RR("ctz.w", "$r14", "$r13", 0x207d275c076e5247UL, 0xd243debc9b557922UL);

   /* ---------------- clo.d rd, rj ---------------- */
   TESTINST_RR("clo.d", "$r7", "$r16", 0x9432ccd773e86812UL, 0x9f921ea959c97c2bUL);
   TESTINST_RR("clo.d", "$r7", "$r12", 0xaf19ef0b422b09bfUL, 0x8773ec5c72444fe2UL);
   TESTINST_RR("clo.d", "$r5", "$r10", 0xa2912bc0ca36fa58UL, 0x2c93a7506a8979b7UL);
   TESTINST_RR("clo.d", "$r7", "$r28", 0x69dd3f71121c7380UL, 0x1784b7c2c7558b4aUL);
   TESTINST_RR("clo.d", "$r15", "$r9", 0x95b40b42f113ceccUL, 0xf0cdb7b9c17bb9e1UL);
   TESTINST_RR("clo.d", "$r9", "$r27", 0x1961ee1499945d08UL, 0x23c7a2252c1cbc78UL);
   TESTINST_RR("clo.d", "$r30", "$r19", 0xda0aa8b04f719a51UL, 0x8f93c7a1b3cc9f12UL);
   TESTINST_RR("clo.d", "$r26", "$r20", 0xdd4f62bfe1237a28UL, 0xd61c7bfe05165d04UL);
   TESTINST_RR("clo.d", "$r26", "$r6", 0x44a1378e22d6ec81UL, 0x1b21543ee9abd103UL);
   TESTINST_RR("clo.d", "$r24", "$r16", 0x51efcf6ef8eb9917UL, 0x602cbdf020ee6da8UL);

   /* ---------------- clz.d rd, rj ---------------- */
   TESTINST_RR("clz.d", "$r27", "$r7", 0x91df318f7b476077UL, 0x6ca0b9cf9bb84c4aUL);
   TESTINST_RR("clz.d", "$r19", "$r30", 0x435d7fb412d9c12cUL, 0xc926e58bdb46104eUL);
   TESTINST_RR("clz.d", "$r12", "$r30", 0x906b06441b2ef62bUL, 0x4b9b91966077ef0UL);
   TESTINST_RR("clz.d", "$r28", "$r6", 0x28bb3e3324f33e14UL, 0x7628cd8752be6223UL);
   TESTINST_RR("clz.d", "$r14", "$r15", 0xb7a5ae04bf2e60c0UL, 0x41a328a79afda305UL);
   TESTINST_RR("clz.d", "$r4", "$r23", 0x5fd8327a265b1a3bUL, 0x66b92d8b5b842d4aUL);
   TESTINST_RR("clz.d", "$r18", "$r29", 0x73df6808e38c72adUL, 0x6b91b11261dd26b6UL);
   TESTINST_RR("clz.d", "$r13", "$r8", 0xd8d2dbd71d1783adUL, 0xdc50b7586ccab6a1UL);
   TESTINST_RR("clz.d", "$r17", "$r10", 0xee6f842bb7686b8dUL, 0xdf52e003cd95f02fUL);
   TESTINST_RR("clz.d", "$r13", "$r8", 0x91e717aef96cc046UL, 0x5dd0743ed560ba78UL);

   /* ---------------- cto.d rd, rj ---------------- */
   TESTINST_RR("cto.d", "$r31", "$r5", 0xf361d5d1fb232769UL, 0x1530b67240d804cfUL);
   TESTINST_RR("cto.d", "$r5", "$r26", 0xbedb393d17f69d40UL, 0xcef56269ef7aecdaUL);
   TESTINST_RR("cto.d", "$r5", "$r31", 0xadd75db878cdbf84UL, 0x8e08acc65c97f0b2UL);
   TESTINST_RR("cto.d", "$r31", "$r31", 0x6a8a89827e4929f9UL, 0x7df0f59d97924bb3UL);
   TESTINST_RR("cto.d", "$r14", "$r30", 0xefb0874ef3600b6dUL, 0x97a4b45ab971a548UL);
   TESTINST_RR("cto.d", "$r5", "$r17", 0x144271fb49c8d2d8UL, 0x787e6dbb4fec4d21UL);
   TESTINST_RR("cto.d", "$r28", "$r20", 0xd6d0953d2a12c998UL, 0xafd578caad0dfa09UL);
   TESTINST_RR("cto.d", "$r16", "$r18", 0xde650be54a7990cUL, 0x3ea8f45e10441829UL);
   TESTINST_RR("cto.d", "$r15", "$r16", 0xbbd328743f49a86UL, 0x5cafc638b6b509beUL);
   TESTINST_RR("cto.d", "$r6", "$r20", 0x598ee27859cf8d0eUL, 0x4bce530e537ad762UL);

   /* ---------------- ctz.d rd, rj ---------------- */
   TESTINST_RR("ctz.d", "$r14", "$r28", 0xf2e4d886a8fd3fe3UL, 0xdafbabdfefac692UL);
   TESTINST_RR("ctz.d", "$r6", "$r27", 0xe005a6a20d44fbcaUL, 0xe000ac4f4cfb2ce2UL);
   TESTINST_RR("ctz.d", "$r15", "$r26", 0x871c2ccd50ec0784UL, 0xa82b0d96dd72f11cUL);
   TESTINST_RR("ctz.d", "$r17", "$r20", 0xebe7d9f4ec5055d5UL, 0x65575957936d1d6eUL);
   TESTINST_RR("ctz.d", "$r19", "$r8", 0x394effa243e5f14cUL, 0xf6852349a7b00561UL);
   TESTINST_RR("ctz.d", "$r5", "$r9", 0x3c67392fc408e9dbUL, 0xeff4bf8e886d7cc3UL);
   TESTINST_RR("ctz.d", "$r31", "$r15", 0xbf5435775bd0435bUL, 0x19760246c8d1d680UL);
   TESTINST_RR("ctz.d", "$r9", "$r5", 0xccde230362ce06aUL, 0x7590c6e73077c2bcUL);
   TESTINST_RR("ctz.d", "$r28", "$r25", 0x2518777b06d608a0UL, 0xb87647dad481ba32UL);
   TESTINST_RR("ctz.d", "$r23", "$r19", 0xbe232a9fe2090e75UL, 0x2dceda5cdc990d2eUL);

   /* ---------------- revb.2h rd, rj ---------------- */
   TESTINST_RR("revb.2h", "$r29", "$r30", 0x75397084990a0745UL, 0xd4c83f5966c1c17UL);
   TESTINST_RR("revb.2h", "$r17", "$r23", 0xecfbee2a69bbe344UL, 0x5a42dc5dc5705f68UL);
   TESTINST_RR("revb.2h", "$r6", "$r14", 0xbfeffdbd68845522UL, 0x3490af5b50fd56bfUL);
   TESTINST_RR("revb.2h", "$r13", "$r6", 0x58e1821d319a1598UL, 0x4c6711d021a72be6UL);
   TESTINST_RR("revb.2h", "$r18", "$r8", 0x6e14994d4e16ff86UL, 0x9fda01513ab5ceb8UL);
   TESTINST_RR("revb.2h", "$r7", "$r30", 0x9979d3a3fcfc9323UL, 0x504c708535bc136fUL);
   TESTINST_RR("revb.2h", "$r28", "$r19", 0x9daf4aa3a33eec5fUL, 0xaa376fc54f4be6f5UL);
   TESTINST_RR("revb.2h", "$r30", "$r8", 0x2e0bba43ec83e59eUL, 0xaee8b8acd436f6daUL);
   TESTINST_RR("revb.2h", "$r14", "$r7", 0x9634787c9be10863UL, 0xe9da521d42716c0aUL);
   TESTINST_RR("revb.2h", "$r23", "$r14", 0x687b89225667081aUL, 0x9089e36a4f12f9c6UL);

   /* ---------------- revb.4h rd, rj ---------------- */
   TESTINST_RR("revb.4h", "$r4", "$r25", 0xc42859bd06b669d2UL, 0x782e4ae6ab812191UL);
   TESTINST_RR("revb.4h", "$r18", "$r19", 0x45ca4499d789fe5bUL, 0x6e558c98b95d346dUL);
   TESTINST_RR("revb.4h", "$r24", "$r10", 0x2d04871fd753c43fUL, 0xbeab033e2b5a979eUL);
   TESTINST_RR("revb.4h", "$r24", "$r8", 0xbc4deb39fb2ffe2eUL, 0x5e3e50b8025e77f3UL);
   TESTINST_RR("revb.4h", "$r7", "$r14", 0xf44a6ea6f42e0918UL, 0x9f617a848e4ad8f2UL);
   TESTINST_RR("revb.4h", "$r13", "$r12", 0xda815ff8648e92b9UL, 0xa401e74c4dd88e12UL);
   TESTINST_RR("revb.4h", "$r31", "$r19", 0x7964d861d2ecb8d5UL, 0xe402e87f73fb4c68UL);
   TESTINST_RR("revb.4h", "$r29", "$r25", 0x6beff3fa6167cdccUL, 0x11e350b71aee0229UL);
   TESTINST_RR("revb.4h", "$r4", "$r8", 0x357a56e8ae275376UL, 0xdf8ebc175f4be7e3UL);
   TESTINST_RR("revb.4h", "$r15", "$r27", 0xeb11b29acfe397d6UL, 0x42d231083cd97aa0UL);

   /* ---------------- revb.2w rd, rj ---------------- */
   TESTINST_RR("revb.2w", "$r27", "$r31", 0x978f867dd7f0cb8UL, 0x19eec2d357cd6a06UL);
   TESTINST_RR("revb.2w", "$r10", "$r10", 0x7897a40c4fda96d5UL, 0xcb849783a18de892UL);
   TESTINST_RR("revb.2w", "$r23", "$r14", 0x18338c734be53a1UL, 0x6258664ec1bb96b8UL);
   TESTINST_RR("revb.2w", "$r12", "$r19", 0x7417ec4fef3451ccUL, 0x216ad32ee149542bUL);
   TESTINST_RR("revb.2w", "$r31", "$r30", 0x8132835b9905b650UL, 0x6fac007fbefdecf2UL);
   TESTINST_RR("revb.2w", "$r25", "$r10", 0x7336ebe375c83bedUL, 0x643f76ac3010a6bbUL);
   TESTINST_RR("revb.2w", "$r31", "$r29", 0x5d99f79f18e805b8UL, 0xe65e70ca4cf299faUL);
   TESTINST_RR("revb.2w", "$r30", "$r19", 0xec10dd6d7249c5faUL, 0x3f6bb22d66caf299UL);
   TESTINST_RR("revb.2w", "$r6", "$r30", 0x2c394783817c0870UL, 0xd823cff07efd78dbUL);
   TESTINST_RR("revb.2w", "$r4", "$r15", 0xc5acf61f075cd4e4UL, 0xc154dd7479b90c6cUL);

   /* ---------------- revb.d rd, rj ---------------- */
   TESTINST_RR("revb.d", "$r6", "$r23", 0xe6e05a0dafda37ceUL, 0x2ac7d047f197f6fbUL);
   TESTINST_RR("revb.d", "$r19", "$r4", 0xc07a757bea6011ffUL, 0xcef6cef3e0f941ffUL);
   TESTINST_RR("revb.d", "$r6", "$r15", 0x711bb31e18fcb2f3UL, 0x522068042cf5be1aUL);
   TESTINST_RR("revb.d", "$r9", "$r7", 0xf9654c655c67392eUL, 0xa1b065742110e3f4UL);
   TESTINST_RR("revb.d", "$r29", "$r4", 0x70c0dcad23609060UL, 0x5d04b7b2ece6f6bbUL);
   TESTINST_RR("revb.d", "$r15", "$r4", 0x809930516f3136ebUL, 0xda33327a8d42ef55UL);
   TESTINST_RR("revb.d", "$r10", "$r4", 0x1a7ee04b354f6af5UL, 0xcda6c6943e46fed7UL);
   TESTINST_RR("revb.d", "$r20", "$r4", 0x315f95452d748459UL, 0xa001e934745758e0UL);
   TESTINST_RR("revb.d", "$r6", "$r8", 0xabbd06000374627aUL, 0x85441006689de89bUL);
   TESTINST_RR("revb.d", "$r27", "$r24", 0x2d404e69f54afa48UL, 0x46f47b822772f3cdUL);

   /* ---------------- revh.2w rd, rj ---------------- */
   TESTINST_RR("revh.2w", "$r6", "$r15", 0x5b764c7bfb1999ebUL, 0x86603fc3f96843edUL);
   TESTINST_RR("revh.2w", "$r19", "$r10", 0xf39f8e6b43dd63ceUL, 0x141d294d06276941UL);
   TESTINST_RR("revh.2w", "$r5", "$r20", 0x3ff54e5c35d83e69UL, 0xd677d6a21384278aUL);
   TESTINST_RR("revh.2w", "$r4", "$r31", 0xce463b02a2f840ccUL, 0x6f87c9636f9cfca6UL);
   TESTINST_RR("revh.2w", "$r19", "$r26", 0x34abc96ddde64e27UL, 0x723ec7ce92720502UL);
   TESTINST_RR("revh.2w", "$r8", "$r18", 0x1454a1ee8739c235UL, 0xd890efa373a6dfb0UL);
   TESTINST_RR("revh.2w", "$r12", "$r31", 0xf0c8b856751cae70UL, 0xb675dff2568e6ebfUL);
   TESTINST_RR("revh.2w", "$r24", "$r9", 0xb36984e3a7a3eaeaUL, 0xa169cfa9f35f6a8aUL);
   TESTINST_RR("revh.2w", "$r25", "$r27", 0x640b3e6b41180473UL, 0x9bc307f0a2ef368fUL);
   TESTINST_RR("revh.2w", "$r7", "$r9", 0x897e1406a0eb2dc9UL, 0x1921bcf657fecdccUL);

   /* ---------------- revh.d rd, rj ---------------- */
   TESTINST_RR("revh.d", "$r14", "$r25", 0xec3573411ea025e5UL, 0x6976d4371b08f1abUL);
   TESTINST_RR("revh.d", "$r24", "$r31", 0x9ef9e5cb1375d42aUL, 0x9ce130c8a579e11dUL);
   TESTINST_RR("revh.d", "$r9", "$r28", 0x3c8cd0055a5e7031UL, 0xf05f9381753ded16UL);
   TESTINST_RR("revh.d", "$r24", "$r26", 0x6a4e5797f19041f6UL, 0xd26a5ae65e21041cUL);
   TESTINST_RR("revh.d", "$r14", "$r24", 0xe2cb9a83aee22d97UL, 0x6405d71e0bb63321UL);
   TESTINST_RR("revh.d", "$r19", "$r23", 0x91cdf3bcd9afe76dUL, 0x171953826107396aUL);
   TESTINST_RR("revh.d", "$r23", "$r14", 0x93ed49255d084e12UL, 0x374bd76990198b43UL);
   TESTINST_RR("revh.d", "$r31", "$r12", 0x8e54a908f04882bUL, 0xf7e8756491b9d346UL);
   TESTINST_RR("revh.d", "$r31", "$r20", 0xbb7cd34502fdf01fUL, 0x906b7289a6957d3fUL);
   TESTINST_RR("revh.d", "$r27", "$r30", 0xacbca1aacdd9dd3fUL, 0x3072d9c69004d4b5UL);

   /* ---------------- bitrev.4b rd, rj ---------------- */
   TESTINST_RR("bitrev.4b", "$r23", "$r19", 0xb422f2854b491d92UL, 0x7649084cec69098aUL);
   TESTINST_RR("bitrev.4b", "$r27", "$r16", 0xd14736328d74b448UL, 0x1abee3a271c71db9UL);
   TESTINST_RR("bitrev.4b", "$r15", "$r23", 0xf17c0f0ccfbb2c38UL, 0x490107ff4155bd17UL);
   TESTINST_RR("bitrev.4b", "$r5", "$r18", 0x8408d6a30523619dUL, 0x625d5aedf0add9fbUL);
   TESTINST_RR("bitrev.4b", "$r8", "$r15", 0xc41a2fdb60ba75a6UL, 0xe2562eab3b333a00UL);
   TESTINST_RR("bitrev.4b", "$r17", "$r18", 0x6a409394f364c02aUL, 0xea970d90edb343ccUL);
   TESTINST_RR("bitrev.4b", "$r25", "$r29", 0xd8d1c9b8dcff266dUL, 0xacca47ac7597ca65UL);
   TESTINST_RR("bitrev.4b", "$r26", "$r24", 0xe2a0d11df8c5055bUL, 0xc57559d03e3e216dUL);
   TESTINST_RR("bitrev.4b", "$r8", "$r27", 0xb6a5815170d657f0UL, 0x9f60901eefa1347aUL);
   TESTINST_RR("bitrev.4b", "$r20", "$r16", 0x432a2fbf2b073732UL, 0x604b8d7ecb5e86dcUL);

   /* ---------------- bitrev.8b rd, rj ---------------- */
   TESTINST_RR("bitrev.8b", "$r25", "$r7", 0x22b2e6007f742fd1UL, 0xe8c23886def1bbc9UL);
   TESTINST_RR("bitrev.8b", "$r28", "$r30", 0xf985d7779c5ca157UL, 0x285cbdc0f47395d1UL);
   TESTINST_RR("bitrev.8b", "$r29", "$r13", 0xd9b8364a793bc50cUL, 0xded35d7c7ba73d29UL);
   TESTINST_RR("bitrev.8b", "$r12", "$r28", 0x18d7769bc1147dc5UL, 0xfb6cda8c7f12313aUL);
   TESTINST_RR("bitrev.8b", "$r23", "$r6", 0xeff84dc134b3acbeUL, 0xee7c4e89e333eda8UL);
   TESTINST_RR("bitrev.8b", "$r24", "$r20", 0xad65748f0bc46e9fUL, 0xd0d88137a6284eacUL);
   TESTINST_RR("bitrev.8b", "$r10", "$r5", 0xe0e1c1e262352e89UL, 0x9c43ebc4f7c65dc1UL);
   TESTINST_RR("bitrev.8b", "$r27", "$r13", 0x444a53aa65d317dcUL, 0x473eea7ea5691da7UL);
   TESTINST_RR("bitrev.8b", "$r13", "$r9", 0xfc48d0fdf4c7a6e5UL, 0x5dcad407df3401a5UL);
   TESTINST_RR("bitrev.8b", "$r12", "$r5", 0xebef32fcbd91e9aUL, 0xe1eeea527816355eUL);

   /* ---------------- bitrev.w rd, rj ---------------- */
   TESTINST_RR("bitrev.w", "$r18", "$r15", 0x2028b0c8691a767UL, 0x5822df2950c9c2d3UL);
   TESTINST_RR("bitrev.w", "$r30", "$r27", 0x2a2d48209d9f377bUL, 0xde9d59b836df41fcUL);
   TESTINST_RR("bitrev.w", "$r17", "$r4", 0xe6fb8b07c90464e6UL, 0x65976cb5c6c6a5b0UL);
   TESTINST_RR("bitrev.w", "$r9", "$r31", 0x1b95159ec5c37644UL, 0x62c549b741c2adadUL);
   TESTINST_RR("bitrev.w", "$r17", "$r14", 0x8b414dfa7156f0ceUL, 0x9642d0186f420e7cUL);
   TESTINST_RR("bitrev.w", "$r15", "$r8", 0x2722ecb374b4d5e3UL, 0xeaf151a286bbc4cfUL);
   TESTINST_RR("bitrev.w", "$r27", "$r19", 0x58ec913c63634a5UL, 0xe723c39df96a4fd2UL);
   TESTINST_RR("bitrev.w", "$r7", "$r26", 0xa245e7dd80a324a2UL, 0xe7d6c2b2683291eUL);
   TESTINST_RR("bitrev.w", "$r31", "$r6", 0x114292ed02ba1255UL, 0x13cd62afac5ac3d4UL);
   TESTINST_RR("bitrev.w", "$r7", "$r25", 0xbd46d88fc8d2933bUL, 0x69ce9ccb487dadd1UL);

   /* ---------------- bitrev.d rd, rj ---------------- */
   TESTINST_RR("bitrev.d", "$r4", "$r29", 0xeaacaeb60b227eabUL, 0x799f36da44887e2cUL);
   TESTINST_RR("bitrev.d", "$r29", "$r6", 0xcfbb055ab1ebf7faUL, 0x2924f63fec744b02UL);
   TESTINST_RR("bitrev.d", "$r28", "$r31", 0xaac74a398d76900dUL, 0xf6c75e45e33b4cb7UL);
   TESTINST_RR("bitrev.d", "$r24", "$r12", 0xfc8bc33fb4a8d023UL, 0xcccd98e9d53aa26aUL);
   TESTINST_RR("bitrev.d", "$r8", "$r7", 0x7502cd68289f4c3aUL, 0x746ddfd3c3a512b1UL);
   TESTINST_RR("bitrev.d", "$r6", "$r16", 0xe8b94bfe615774aeUL, 0x518770bbee53d619UL);
   TESTINST_RR("bitrev.d", "$r24", "$r4", 0x6318c17dbae816c3UL, 0x9ab684e129b57f07UL);
   TESTINST_RR("bitrev.d", "$r27", "$r23", 0x8a22909b005a86b8UL, 0x69337e8c3b1fc2bbUL);
   TESTINST_RR("bitrev.d", "$r20", "$r9", 0x9f43885d40caf0UL, 0x193cbf609dbc33d4UL);
   TESTINST_RR("bitrev.d", "$r30", "$r19", 0x30fa02e0fc390ac9UL, 0x21686c931c6260daUL);

   /* ---------------- bytepick.w rd, rj, rk, sa2 ---------------- */
   TESTINST_RRRI("bytepick.w", "$r26", "$r15", "$r19", SA2, 0x1b0b980dd3271273UL, 0x8737ca6c8106ceeeUL, 0x2807e0dcb47d6efUL, 1);
   TESTINST_RRRI("bytepick.w", "$r15", "$r17", "$r7", SA2, 0x3d2e3fbcbd032001UL, 0x5eced8cf3da8b205UL, 0xb8155b41321e09c0UL, 0);
   TESTINST_RRRI("bytepick.w", "$r12", "$r15", "$r17", SA2, 0x2670c80f12a87520UL, 0x29ab42125e3ea5c8UL, 0x32a39ac435460f2fUL, 3);
   TESTINST_RRRI("bytepick.w", "$r4", "$r20", "$r18", SA2, 0x5a64271926277c04UL, 0xcbde225cc736e5d5UL, 0x18abacc874db47e9UL, 3);
   TESTINST_RRRI("bytepick.w", "$r8", "$r5", "$r24", SA2, 0xdb41606ce3f9df94UL, 0xc3f6ce370d754a3fUL, 0x34ad5a423a5c42e3UL, 3);
   TESTINST_RRRI("bytepick.w", "$r5", "$r30", "$r14", SA2, 0xedb3aad221050d0bUL, 0x46f5823389f2581aUL, 0xf766f1e75349809eUL, 2);
   TESTINST_RRRI("bytepick.w", "$r4", "$r19", "$r18", SA2, 0xf92ed0231f25c991UL, 0xba59df0352ed6b3eUL, 0x58d6fbce4e4325e8UL, 0);
   TESTINST_RRRI("bytepick.w", "$r18", "$r28", "$r24", SA2, 0x177dcaf8fcd30180UL, 0xbdc04b3b8f707462UL, 0x6102168606deb3edUL, 3);
   TESTINST_RRRI("bytepick.w", "$r13", "$r27", "$r29", SA2, 0x383d82c5d717259bUL, 0x495e30e5e680d7fcUL, 0x1c17f315ebb3bec3UL, 2);
   TESTINST_RRRI("bytepick.w", "$r5", "$r29", "$r4", SA2, 0x26a0fb212ab80a3aUL, 0x78b167aecd81f869UL, 0x6daab499f228fef4UL, 1);

   /* ---------------- bytepick.d rd, rj, rk, sa3 ---------------- */
   TESTINST_RRRI("bytepick.d", "$r28", "$r4", "$r28", SA3, 0x794fa22d52f7e834UL, 0x2f084db071d3bcceUL, 0xa0cf51d7020f10c1UL, 7);
   TESTINST_RRRI("bytepick.d", "$r10", "$r18", "$r4", SA3, 0x9fd7a6b378604833UL, 0x37da15f8a7154cabUL, 0xaedd64328d27a0a8UL, 2);
   TESTINST_RRRI("bytepick.d", "$r7", "$r6", "$r24", SA3, 0xdee49920d429d3c2UL, 0x15e3f61f2f82a2d1UL, 0xdeba03c7761e4678UL, 3);
   TESTINST_RRRI("bytepick.d", "$r19", "$r16", "$r5", SA3, 0x53bda4d18e61fc44UL, 0xc79bd94439006673UL, 0xa8024ab452a2bd52UL, 4);
   TESTINST_RRRI("bytepick.d", "$r26", "$r19", "$r25", SA3, 0xc8aae5136d925592UL, 0xea109dd2837d3acfUL, 0x30e93a75e695666aUL, 7);
   TESTINST_RRRI("bytepick.d", "$r8", "$r14", "$r8", SA3, 0xa03db273c845b37fUL, 0xa7fd0053a136769fUL, 0x6ab932903229b035UL, 2);
   TESTINST_RRRI("bytepick.d", "$r9", "$r14", "$r23", SA3, 0x2f160a0d147b300fUL, 0xdae9d5d15bb8f5b5UL, 0xc4fdfbb29d49dfe4UL, 2);
   TESTINST_RRRI("bytepick.d", "$r20", "$r18", "$r15", SA3, 0x30cefdebc30b841aUL, 0xbfd016fb0312277cUL, 0x44269b95d496912fUL, 5);
   TESTINST_RRRI("bytepick.d", "$r12", "$r17", "$r5", SA3, 0xde32bc5d3471eed2UL, 0xdb807610c6e762e4UL, 0xb2148e34e649d1b8UL, 2);
   TESTINST_RRRI("bytepick.d", "$r5", "$r24", "$r28", SA3, 0x9ab1be6a0faa61a8UL, 0x97d4a12579967739UL, 0xaa592ef1fd606badUL, 3);

   /* ---------------- maskeqz rd, rj, rk ---------------- */
   TESTINST_RRR("maskeqz", "$r14", "$r28", "$r25", 0xc263b6b8f3404c8dUL, 0x90ef733c88c88866UL, 0xd256888d94e8d21aUL);
   TESTINST_RRR("maskeqz", "$r13", "$r9", "$r15", 0x5bdd86b962c61db4UL, 0x8a78f7b88a728d92UL, 0x69e707acb2c26a83UL);
   TESTINST_RRR("maskeqz", "$r7", "$r7", "$r13", 0xea86abdbdea660cbUL, 0xfb778deef0a5b893UL, 0xad10e23c971d1a9fUL);
   TESTINST_RRR("maskeqz", "$r8", "$r7", "$r19", 0xf64df33b6146939fUL, 0xe7376d3da44f4dfdUL, 0x7987e122af2505abUL);
   TESTINST_RRR("maskeqz", "$r10", "$r27", "$r29", 0x404a261c069b488bUL, 0x81886c523ec2658cUL, 0x3236dc83d0a27cc1UL);
   TESTINST_RRR("maskeqz", "$r23", "$r16", "$r25", 0x8671050519b7bda0UL, 0x26fa2567b106d73aUL, 0xd884011e0d767feUL);
   TESTINST_RRR("maskeqz", "$r5", "$r19", "$r18", 0xbd8d4cef53122132UL, 0x4976c047c57ec148UL, 0x602312f372049a5eUL);
   TESTINST_RRR("maskeqz", "$r29", "$r24", "$r23", 0x7f390b695d8b12eUL, 0x70043e7666a24a34UL, 0xfee8f8f90ab3ac9bUL);
   TESTINST_RRR("maskeqz", "$r25", "$r4", "$r18", 0x7eaffcb6dac1b5bUL, 0x4b12f8c6738216a2UL, 0x409acb80b7391511UL);
   TESTINST_RRR("maskeqz", "$r30", "$r6", "$r24", 0x14d829636b628dc9UL, 0xdb88a366a2271c2cUL, 0xea0d5998835940aUL);

   /* ---------------- masknez rd, rj, rk ---------------- */
   TESTINST_RRR("masknez", "$r14", "$r24", "$r5", 0x46b15bbb9507bd79UL, 0xc92af628c880a454UL, 0x846a586db0af0965UL);
   TESTINST_RRR("masknez", "$r30", "$r8", "$r8", 0x43cd20b5234db4e8UL, 0x7aeee6ab6b10561fUL, 0x45ab4fdb4ca8b325UL);
   TESTINST_RRR("masknez", "$r24", "$r19", "$r15", 0xd3d50bbb34b528e2UL, 0xdd71746b0beedae3UL, 0xa34d82fc50174094UL);
   TESTINST_RRR("masknez", "$r29", "$r26", "$r26", 0x576cb2da15b1462dUL, 0x6c669f0195b50b7aUL, 0xec1609ef36aa938fUL);
   TESTINST_RRR("masknez", "$r4", "$r29", "$r10", 0xaa220f67a02617dbUL, 0xffcd18e3016e10fUL, 0x4cf9bdd8dca7f88fUL);
   TESTINST_RRR("masknez", "$r23", "$r9", "$r29", 0x774e1c840428fbdeUL, 0x391268694388d2a7UL, 0xf06192a4e5780c53UL);
   TESTINST_RRR("masknez", "$r7", "$r25", "$r28", 0x7b75099f16135faaUL, 0xf95af681c18bf31cUL, 0x2f6122581dfdef74UL);
   TESTINST_RRR("masknez", "$r26", "$r10", "$r16", 0xe6006c9bd6bae204UL, 0x7e84e5db1181249dUL, 0x6ab2371059cdc875UL);
   TESTINST_RRR("masknez", "$r26", "$r15", "$r28", 0xb4c9c784ef74245fUL, 0x20cc1c4c169ca02cUL, 0x606eeb8ce6278d16UL);
   TESTINST_RRR("masknez", "$r19", "$r16", "$r16", 0x75a721553f7c7054UL, 0x7b63b7b7b3f5bd5fUL, 0xf8c7933e92e155eeUL);

   /* ---------------- bstrins.w rd, rj, msbw, lsbw ---------------- */
   TESTINST_RRII("bstrins.w", "$r27", "$r16", MSBW, LSBW, 0x431055863e78b187UL, 0xe18dda9620a50e9dUL, 31, 8);
   TESTINST_RRII("bstrins.w", "$r26", "$r27", MSBW, LSBW, 0x19f800eab7e1ab51UL, 0x61e7d86005d21d29UL, 30, 27);
   TESTINST_RRII("bstrins.w", "$r15", "$r4", MSBW, LSBW, 0xb141d462e777528dUL, 0xb7aebff9bcca1643UL, 17, 14);
   TESTINST_RRII("bstrins.w", "$r30", "$r17", MSBW, LSBW, 0xfac48083375844feUL, 0x6d3283ba14cc27ebUL, 24, 6);
   TESTINST_RRII("bstrins.w", "$r12", "$r12", MSBW, LSBW, 0x9b7629774f19f64aUL, 0x84ee8d65b2842686UL, 30, 25);
   TESTINST_RRII("bstrins.w", "$r15", "$r10", MSBW, LSBW, 0x290172844863090fUL, 0x85ea298976069fcdUL, 26, 1);
   TESTINST_RRII("bstrins.w", "$r10", "$r13", MSBW, LSBW, 0x66942ba1c15e85aaUL, 0xddb2dfa7474a4370UL, 23, 8);
   TESTINST_RRII("bstrins.w", "$r5", "$r20", MSBW, LSBW, 0x3dcfecca80bf0d79UL, 0x5044b246f2d3f890UL, 18, 16);
   TESTINST_RRII("bstrins.w", "$r23", "$r5", MSBW, LSBW, 0xa11723142f1472a7UL, 0xcbaaa9a23d119663UL, 25, 21);
   TESTINST_RRII("bstrins.w", "$r20", "$r31", MSBW, LSBW, 0x6a1110240ba884b8UL, 0x45cadf0ffe08cc25UL, 13, 12);

   /* ---------------- bstrpick.w rd, rj, msbw, lsbw ---------------- */
   TESTINST_RRII("bstrpick.w", "$r5", "$r23", MSBW, LSBW, 0x6885eaa89f691954UL, 0x94f8458597294f2eUL, 23, 11);
   TESTINST_RRII("bstrpick.w", "$r25", "$r8", MSBW, LSBW, 0x11be9b9923ebee96UL, 0x23deda120a49df15UL, 18, 11);
   TESTINST_RRII("bstrpick.w", "$r6", "$r6", MSBW, LSBW, 0x3546d655181289bcUL, 0x7ee84a41c952b690UL, 10, 3);
   TESTINST_RRII("bstrpick.w", "$r25", "$r5", MSBW, LSBW, 0xb2eec884ea77f548UL, 0x23992bc40919416fUL, 15, 9);
   TESTINST_RRII("bstrpick.w", "$r26", "$r14", MSBW, LSBW, 0x8e591161730ac582UL, 0xf45f4435cc1cb138UL, 21, 8);
   TESTINST_RRII("bstrpick.w", "$r9", "$r14", MSBW, LSBW, 0x1ac92d930e8361f9UL, 0xcc11dd56e96c6256UL, 7, 3);
   TESTINST_RRII("bstrpick.w", "$r19", "$r9", MSBW, LSBW, 0xd15fd80fafe60a58UL, 0xb1426a8c680d628cUL, 8, 8);
   TESTINST_RRII("bstrpick.w", "$r17", "$r13", MSBW, LSBW, 0xfa48c3cd091d2b5eUL, 0x3a2827a58a014a72UL, 30, 12);
   TESTINST_RRII("bstrpick.w", "$r6", "$r31", MSBW, LSBW, 0xca10a858ebfa78a1UL, 0x202a38722f270884UL, 16, 7);
   TESTINST_RRII("bstrpick.w", "$r20", "$r10", MSBW, LSBW, 0xc010deb269ae6ba2UL, 0x98f1d297734f9f4cUL, 31, 15);

   /* ---------------- bstrins.d rd, rj, msbd, lsbd ---------------- */
   TESTINST_RRII("bstrins.d", "$r29", "$r17", MSBD, LSBD, 0x7cf4a9ec79307e59UL, 0xb1b5afc00eef90a3UL, 60, 25);
   TESTINST_RRII("bstrins.d", "$r10", "$r27", MSBD, LSBD, 0xc708602dee32579fUL, 0x199d90a711e94375UL, 31, 22);
   TESTINST_RRII("bstrins.d", "$r4", "$r24", MSBD, LSBD, 0x4e5ce98e217a4b59UL, 0xaf25b5661daefdeaUL, 58, 58);
   TESTINST_RRII("bstrins.d", "$r12", "$r30", MSBD, LSBD, 0x9505d862c56b1708UL, 0x7f3f0c983ce27863UL, 16, 6);
   TESTINST_RRII("bstrins.d", "$r29", "$r5", MSBD, LSBD, 0x248f295ef3afe5aaUL, 0x9469277db61227b7UL, 43, 0);
   TESTINST_RRII("bstrins.d", "$r31", "$r31", MSBD, LSBD, 0xbc5f0c47c3a63a94UL, 0x4aacc1c77ad0c09aUL, 49, 23);
   TESTINST_RRII("bstrins.d", "$r6", "$r24", MSBD, LSBD, 0x79110235b8c34188UL, 0x75e3e311aef2bef9UL, 12, 2);
   TESTINST_RRII("bstrins.d", "$r6", "$r16", MSBD, LSBD, 0xaa6e63ffd80b76c5UL, 0xb1ea7dcb3af0881dUL, 43, 13);
   TESTINST_RRII("bstrins.d", "$r15", "$r25", MSBD, LSBD, 0x5b68a802f26a1804UL, 0xb4f651115b84591bUL, 53, 29);
   TESTINST_RRII("bstrins.d", "$r9", "$r9", MSBD, LSBD, 0x3394218c965d5f1aUL, 0xf3d30b5d4d4089b4UL, 61, 40);

   /* ---------------- bstrpick.d rd, rj, msbd, lsbd ---------------- */
   TESTINST_RRII("bstrpick.d", "$r27", "$r27", MSBD, LSBD, 0x503c8fae2d6d7b58UL, 0x9fd9869ca812de0cUL, 63, 33);
   TESTINST_RRII("bstrpick.d", "$r14", "$r5", MSBD, LSBD, 0x65f05eaa5e13856aUL, 0xd52c72fbeccc39f5UL, 52, 40);
   TESTINST_RRII("bstrpick.d", "$r13", "$r20", MSBD, LSBD, 0x9cea777df4d2eae0UL, 0x6326727a36499800UL, 48, 14);
   TESTINST_RRII("bstrpick.d", "$r10", "$r17", MSBD, LSBD, 0xf30a073a4a56604bUL, 0xc12d112f6a0c8f1UL, 43, 20);
   TESTINST_RRII("bstrpick.d", "$r13", "$r25", MSBD, LSBD, 0xe559d975e0d9ac85UL, 0xcf41f30cc4a46713UL, 55, 37);
   TESTINST_RRII("bstrpick.d", "$r29", "$r4", MSBD, LSBD, 0x41843db6c2a206cbUL, 0x343f795d45fcff8cUL, 34, 20);
   TESTINST_RRII("bstrpick.d", "$r27", "$r28", MSBD, LSBD, 0xb359821297377feeUL, 0x4fc51c5773e64f69UL, 27, 10);
   TESTINST_RRII("bstrpick.d", "$r24", "$r24", MSBD, LSBD, 0xed3cb5d1e8f0e55eUL, 0x9cdbb70a8b8d3945UL, 63, 20);
   TESTINST_RRII("bstrpick.d", "$r7", "$r30", MSBD, LSBD, 0x11b7344343be1ccfUL, 0xa3422c671803480fUL, 34, 30);
   TESTINST_RRII("bstrpick.d", "$r15", "$r4", MSBD, LSBD, 0x3670c6b869f28085UL, 0x2caa9d9c1351e402UL, 55, 4);

   /* ---------------- crc.w.b.w rd, rj, rk ---------------- */
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crc.w.h.w rd, rj, rk ---------------- */
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crc.w.w.w rd, rj, rk ---------------- */
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crc.w.d.w rd, rj, rk ---------------- */
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crcc.w.b.w rd, rj, rk ---------------- */
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crcc.w.b.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crcc.w.h.w rd, rj, rk ---------------- */
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crcc.w.h.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crcc.w.w.w rd, rj, rk ---------------- */
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crcc.w.w.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);

   /* ---------------- crcc.w.d.w rd, rj, rk ---------------- */
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4b154113f7d32514UL, 0xcce230caafbf9cc9UL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x33d5d595721d4f13UL, 0xf4509311f443a7ceUL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x4a3c6de6954cbc17UL, 0x111b21e39fbd7254UL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xfbb5c64ed1b044c6UL, 0x33ca4c4fb3960326UL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x2b7c5939d7c0f528UL, 0xb73870a5a6630162UL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0x02fe41918ac5cdbaUL, 0x48e0815289728f05UL);
   TESTINST_RRR("crcc.w.d.w", "$r12", "$r13", "$r14", 0x123456789abcdefUL, 0xb60a8f381f187baeUL, 0x008c208cc413ff72UL);
}

int main(void)
{
   test();
   return 0;
}
