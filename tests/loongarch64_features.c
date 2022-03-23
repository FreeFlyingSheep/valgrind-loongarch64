#include <stdio.h>
#include <stdlib.h>
#include <string.h>

// This file determines loongarch64 features a processor supports.
// For now, we only support loongarch64-linux.
//
// We return:
// - 0 if the machine has the asked-for feature.
// - 1 if the machine doesn't have the asked-for feature.
// - 2 if the asked-for feature isn't recognised (this will always be the case
//     for any feature if run on a non-loongarch64 machine).
// - 3 if there was a usage error (it also prints an error message).
#define FEATURE_PRESENT       0
#define FEATURE_NOT_PRESENT   1
#define UNRECOGNISED_FEATURE  2
#define USAGE_ERROR           3

#if defined(VGA_loongarch64)

static int go(const char* feature_name)
{
   int i, len, found;
   FILE* fp;
   char buf[256];
   const char* features[] = {
      "cpucfg", "lam", "ual", "fpu",
      "lsx", "lasx", "complex", "crypto",
      "lvz", "lbt_x86", "lbt_arm", "lbt_mips"
   };

   found = 0;
   len = sizeof(features) / sizeof(features[0]);
   for (i = 0; i < len; i++) {
      if (strcmp(feature_name, features[i]) == 0) {
         found = 1;
         break;
      }
   }

   if (!found)
      return UNRECOGNISED_FEATURE;

   fp = fopen("/proc/cpuinfo", "r");
   if(fp == NULL)
      return UNRECOGNISED_FEATURE;

   while (fgets(buf, sizeof(buf), fp) != NULL) {
      if (strstr(buf, feature_name) != NULL) {
         fclose(fp);
         return FEATURE_PRESENT;
      }
   }

   fclose(fp);
   return FEATURE_NOT_PRESENT;
}

#else

static int go(const char* feature_name)
{
   // Feature not recognised (non-loongarch64 machine!)
   return UNRECOGNISED_FEATURE;
}

#endif // defined(VGA_loongarch64)


//---------------------------------------------------------------------------
// main
//---------------------------------------------------------------------------
int main(int argc, char **argv)
{
   if (argc != 2) {
      fprintf(stderr, "usage: loongarch64_features <feature>\n");
      exit(USAGE_ERROR);
   }

   return go(argv[1]);
}
