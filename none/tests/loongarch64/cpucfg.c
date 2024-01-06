#include <stdio.h>

void test(int reg)
{
    int res;
    __asm__ __volatile__(
        "cpucfg %0, %1 \n\t"
        : "=r" (res)
        : "r" (reg)
        : "memory");
    printf("cpucfg ::\n");
    printf("input: %x\n", (unsigned)reg);
    printf("output: %x\n", (unsigned)res);
}

int main(void)
{
    int i;

    for (i = 0; i < 24; i++)
        test(i);

    return 0;
}
