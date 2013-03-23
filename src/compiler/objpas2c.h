#ifndef __OBJPAS2CH_H_INCLUDED__
#define __OBJPAS2CH_H_INCLUDED__

#include "stdint.h"
#include "stdlib.h"
#include "string.h"

#ifdef _MSC_VER
// MSVC
#define ___PACKED___ __declspec(align(1))
#define ___ALIGNED(x)___ __declspec(align(x))
#else
// GCC, TCC and clang. Other C99-capable compilers are on my ignore-list
#ifdef __TINYC__
#define __GCC_COMPATIBLE__
#endif
#ifdef __GNUC__
#define __GCC_COMPATIBLE__
#endif
#ifdef __clang__
#define __GCC_COMPATIBLE__
#endif
#ifdef __GCC_COMPATIBLE__
#define ___PACKED___ __attribute__((packed))
#define ___ALIGNED___(x) __attribute__((packed,aligned(x)))
#else
#error Non-supported C compiler!
#endif
#endif

typedef void* pasLongstring;

typedef struct pasObjectVirtualMethodTable* pasObjectVirtualMethodTablePointer;

typedef struct pasObjectVirtualMethodTable {
  size_t size;
  void* dynamicMethodTable;
  pasObjectVirtualMethodTablePointer ancestorVirtualMethodTable;
} pasObjectVirtualMethodTable;

typedef struct {
	uint32_t dummy;
} pasFile;

void CheckRefLongstring(pasLongstring str);
pasLongstring AddLongstring(pasLongstring left, pasLongstring right);
void UniqueLongstring(pasLongstring *target);
pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data);
void DecRefLongstring(pasLongstring *str);
void IncRefLongstring(pasLongstring *str);
void FreeLongstring(pasLongstring *str);
uint32_t LengthLongstring(pasLongstring str);
void AssignLongstring(pasLongstring *target, pasLongstring newStr);
void UniqueLongstring(pasLongstring *target);

#define pasWriteInt(x) printf("%i", x);
#define pasWriteUInt(x) printf("%u", x);
#define pasWriteChar(x) printf("%c", x);
#define pasWriteFloat(x) printf("%f", x);
#define pasWriteBool(x) if(x) printf("TRUE"); else printf("FALSE");
#define pasWriteLongString(x) printf(x); CheckRefLongstring(x);

#endif // __OBJPAS2CH_H_INCLUDED__
