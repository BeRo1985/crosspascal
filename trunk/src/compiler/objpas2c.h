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

inline pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data);
// AssignLongstring releases old string in *target, sets newStr and increases refcount
inline void AssignLongstring(pasLongstring *target, pasLongstring newStr);
// clears string value
inline void FreeLongstring(pasLongstring *str);
// returns new string with refCount = 0
inline pasLongstring AddLongstring(pasLongstring *left, pasLongstring *right);
// compares two strings, checks refCount of input
inline uint32_t CompareLongstring(pasLongstring left, pasLongstring right);
// turns *target into an unique string
inline void UniqueLongstring(pasLongstring *target);
// returns length of string
inline uint32_t LengthLongstring(pasLongstring *str);
// increase reference count
inline void IncRefLongstring(pasLongstring *str);
// decrease reference count, free if <=zero
inline void DecRefLongstring(pasLongstring *str);
// check reference of string and free if <=0
inline void checkRefLongstring(pasLongstring str);

#endif // __OBJPAS2CH_H_INCLUDED__
