#define NULL ((void *)0)
#define _CRTALLOC(x) __declspec(allocate(x))
#define WINAPI // __stdcall doesn't do anything on 64bit
#define DLL_PROCESS_ATTACH 1
#define DLL_THREAD_ATTACH 2
#define DLL_THREAD_DETACH 3
#define DLL_PROCESS_DETACH 0
#define DLL_PROCESS_VERIFIER 4
#define TRUE 1

typedef int BOOL;
typedef unsigned long DWORD;
typedef void* HINSTANCE;
typedef void *LPVOID;
typedef void(*_PVFV)(void);

#pragma section(".CRT$XLA", long, read)
#pragma section(".CRT$XLZ", long, read)
#pragma section(".CRT$XIA", long, read)
#pragma section(".CRT$XIZ", long, read)
#pragma section(".CRT$XCA", long, read)
#pragma section(".CRT$XCZ", long, read)
#pragma section(".CRT$XPA", long, read)
#pragma section(".CRT$XPZ", long, read)
#pragma section(".CRT$XTA", long, read)
#pragma section(".CRT$XTZ", long, read)
#pragma section(".rdata$T", long, read)

#pragma comment(linker, "/merge:.CRT=.rdata")

// // C init
_CRTALLOC(".CRT$XIA") _PVFV __xi_a[] = { NULL };
_CRTALLOC(".CRT$XIZ") _PVFV __xi_z[] = { NULL };
// // C pre-terminators
_CRTALLOC(".CRT$XPA") _PVFV __xp_a[] = { NULL };
_CRTALLOC(".CRT$XPZ") _PVFV __xp_z[] = { NULL };
// // // C terminators
_CRTALLOC(".CRT$XTA") _PVFV __xt_a[] = { NULL };
_CRTALLOC(".CRT$XTZ") _PVFV __xt_z[] = { NULL };

int _fltused = 0x9875;


#define __UNKNOWN_APP    0 // abused for DLL

#ifndef _APPTYPE
#define _APPTYPE __UNKNOWN_APP
#endif

// C init
extern _PVFV __xi_a[];
extern _PVFV __xi_z[];
// // C pre-terminators
// extern _PVFV __xp_a[];
// extern _PVFV __xp_z[];
// // C terminators
// extern _PVFV __xt_a[];
// extern _PVFV __xt_z[];

// extern void _setargv (void);


typedef struct atexit_node
{
    struct atexit_node* next;
    _PVFV pfn;
} atexit_node;

static atexit_node* atexit_list;

int atexit(_PVFV pfn)
{
    atexit_node* node = malloc(sizeof(atexit_node));
    if(!node)
        return -1;
    // TODO: not thread safe
    node->pfn = pfn;
    node->next = atexit_list;
    atexit_list = node;
    return 0;
}

void term_atexit()
{
    while(atexit_list)
    {
        atexit_node* n = atexit_list;
        atexit_list = n->next;
        (*(n->pfn))();
        free(n);
    }
}


extern BOOL WINAPI DllMain (HINSTANCE, DWORD, LPVOID);


extern void __cdecl _initterm(_PVFV *,_PVFV *);

BOOL WINAPI
_DllMainCRTStartup (HINSTANCE hDll, DWORD dwReason, LPVOID lpReserved)
{
    BOOL bRet;

    if (dwReason == DLL_PROCESS_ATTACH)
    {
        _initterm(__xi_a, __xi_z);
    }

    bRet = DllMain (hDll, dwReason, lpReserved);

    if (dwReason == DLL_PROCESS_DETACH || dwReason == DLL_PROCESS_ATTACH && !bRet)
    {
        term_atexit();
        _initterm(__xp_a, __xp_z);
        _initterm(__xt_a, __xt_z);
    }
    return bRet;
}

BOOL WINAPI __DefaultDllMain (HINSTANCE hDll, DWORD dwReason, LPVOID lpReserved)
{
    return TRUE;
}

__pragma(comment(linker, "/alternatename:DllMain=__DefaultDllMain"));


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