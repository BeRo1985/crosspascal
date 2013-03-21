#include "stdint.h"
#include "string.h"
#include "stdlib.h"

typedef struct {
    uint32_t codePage;     // 0x0000 - 0xffff = ansistring codepage
    uint32_t elementSize;  // 1 = ansistring, 2 = widestring and unicodestring, 4 = hugestring
	uint32_t refCount;     // 0xffffffff = widestring which is not reference counted
	uint32_t length;
} LongstringRefHeader;

#define LongstringRefHeaderSize sizeof(LongstringRefHeader)

typedef void* pasLongstring;

inline void pasWriteInt(uint32_t Value) {
  printf("%i", Value);
}

inline void pasWriteFloat(double Value) {
 printf("%f", Value);
}

inline void pasWriteBool(uint32_t Value) {
 if(Value)
  printf("TRUE");
 else
  printf("FALSE");
}

inline pasLongstring CheckRefLongstring(pasLongstring *str) {
	if(*str == NULL)
		return;
	LongstringRefHeader* header = (*str) - LongstringRefHeaderSize;
	if(0xffffffff == header->refCount) 
		return;
   	if((header->refCount) == 0)
	 	free(&header);
}

inline void pasWriteLongString(void* Value) {
 printf(Value);
 CheckRefLongstring(&Value);
}

inline pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data) {
	if(length == 0)
		return NULL;

	LongstringRefHeader* header = (void*)malloc(LongstringRefHeaderSize + (sizeof(uint32_t) + (length * elementSize)));
	void* ref = ((void*)header) + LongstringRefHeaderSize;
	header->codePage=codePage;
	header->elementSize=elementSize;
	header->refCount=1;
	header->length=length;
	if(NULL!=data)
		memcpy(ref, data, length * elementSize);
    uint32_t* zero = (void*)(&(((uint8_t*)ref)[length * elementSize]));
    *zero = 0;
	return ref;
}

inline pasLongstring DecRefLongstring(pasLongstring *str) {
	if(*str == NULL)
		return;
	LongstringRefHeader* header = (*str) - LongstringRefHeaderSize;
	if(0xffffffff == header->refCount) 
		return;
   	if(--(header->refCount) == 0)
	 	free(&header);
}

inline pasLongstring IncRefLongstring(pasLongstring *str) {
	if(*str == NULL)
		return;
    LongstringRefHeader* header = (*str) - LongstringRefHeaderSize;
	if(0xffffffff == header->refCount) 
		return;
	header->refCount++;
}

inline void FreeLongstring(pasLongstring *str) {
	DecRefLongstring(str);
	*str = NULL;
}

inline uint32_t LengthLongstring(pasLongstring *str) {
	if(*str == NULL)
		return 0;
	LongstringRefHeader* header = (*str) - LongstringRefHeaderSize;
	return header->length;
}

inline void AssignLongstring(pasLongstring *target, pasLongstring newStr) {
	DecRefLongstring(target);
	*target = newStr;
	IncRefLongstring(target);
}

inline void UniqueLongstring(pasLongstring *target) {
	if(target == NULL)
		return;
    LongstringRefHeader* header = (*target) - LongstringRefHeaderSize;
	if(header->refCount == 1)
		return;

	pasLongstring newtarget = CreateLongstring(header->codePage, header->elementSize, header->length, *target);
	DecRefLongstring(target);
	*target = newtarget;
}

inline pasLongstring AddLongstring(pasLongstring *left, pasLongstring *right) {
	uint32_t a,b;

	a = LengthLongstring(left);
	b = LengthLongstring(right);
	if(a + b == 0)
		return NULL;
	if(b == 0)
		return *left;
	if(a == 0)
		return *right;

	LongstringRefHeader* headerA = (*left) - LongstringRefHeaderSize;
	LongstringRefHeader* headerB = (*right) - LongstringRefHeaderSize;

    pasLongstring result;

    // TODO: Codepage handling

    if(headerA->elementSize == headerB->elementSize){
        result = CreateLongstring(headerA->codePage, headerA->elementSize, a + b, NULL);
        void* temp = result;
        if(a!=0){
            memcpy(temp, *left, a * headerA->elementSize);
            temp += a * headerA->elementSize;
        }
        if(b!=0)
            memcpy(temp, *right, b * headerB->elementSize);
    }else{
        // TODO: Variable-length UTF8 and UTF16 handling

        int i;

        uint32_t elementSize = (headerA->elementSize < headerB->elementSize) ? headerB->elementSize : headerA->elementSize;
        result = CreateLongstring(headerA->codePage, elementSize, a + b, NULL);

        void* temp = result;

        for(i = 0; i < a; i++){
          uint32_t v;
          switch(headerA->elementSize){
            case 1:{
              v = ((uint8_t*)(*left))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(*left))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(*left))[i];
              break;
            }
          }
          switch(elementSize){
            case 1:{
              *((uint8_t*)temp) = v;
              break;
            }
            case 2:{
              *((uint16_t*)temp) = v;
              break;
            }
            case 4:{
              *((uint32_t*)temp) = v;
              break;
            }
          }
          temp += elementSize;
        }

        for(i = 0; i < b; i++){
          uint32_t v;
          switch(headerB->elementSize){
            case 1:{
              v = ((uint8_t*)(*right))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(*right))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(*right))[i];
              break;
            }
          }
          switch(elementSize){
            case 1:{
              *((uint8_t*)temp) = v;
              break;
            }
            case 2:{
              *((uint16_t*)temp) = v;
              break;
            }
            case 4:{
              *((uint32_t*)temp) = v;
              break;
            }
          }
          temp += elementSize;
        }

    }

	return result;
}

//#endif // __OBJPAS2CMAIN__
