#define NULL ((void *)0)
#define _CRT_INTERNAL_PRINTF_STANDARD_SNPRINTF_BEHAVIOR        0x0002ULL

typedef void* _locale_t;
typedef char *  va_list;

int __cdecl __stdio_common_vsprintf(unsigned __int64 options, char *str, size_t len, const char *format, _locale_t locale, va_list valist);
int __cdecl snprintf (char * __restrict__ __stream, size_t __n, const char * __restrict__ __format, ...)
{
  __builtin_va_list ap;
  int ret;
  __builtin_va_start(ap, __format);
  ret = __stdio_common_vsprintf(_CRT_INTERNAL_PRINTF_STANDARD_SNPRINTF_BEHAVIOR, __stream, __n, __format, NULL, ap);
  __builtin_va_end(ap);
  return ret;
}