
#define LongstringRefHeader 16

typedef struct {
        uint32_t codePage;     // 0x0000 - 0xffff = ansistring codepage
        uint32_t elementSize;  // 1 = ansistring, 2 = widestring and unicodestring, 4 = hugestring
	uint32_t refCount;     // 0xffffffff = widestring which is not reference counted
	uint32_t length;
        union {  
	  uint8_t data[0];
	  uint8_t data8[0];
	  uint16_t data16[0];
	  uint32_t data32[0];
        }; 
}* LongstringRef;

typedef void* pasLongstring;


inline pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data) {
	if(length == 0)
		return NULL;
		
	LongstringRef ref = (LongstringRef)malloc(LongstringRefHeader + ((1 + length) * elementSize));
	ref->codePage=codePage;
	ref->elementSize=elementSize;
	ref->refCount=1;
	ref->length=length;
	if(NULL!=data)
		memcpy(&ref->data[0],data,length * elementSize);
        switch(elementSize){
          case 1:
       	    ref->data8[length] = 0;
            break;
          case 2:
       	    ref->data16[length] = 0;
            break;
          case 4:
       	    ref->data32[length] = 0;
            break;
        }
	return (pasLongstring)&ref->data[0];
}

inline pasLongstring DecRefLongstring(pasLongstring* str) {
	if(*str == NULL)
		return;
	LongstringRef ref = (LongstringRef)((size_t)(*str) - LongstringRefHeader);
	if(--ref->refCount <= 0)
		free(ref);
}

inline pasLongstring IncRefLongstring(pasLongstring* str) {
	if(*str == NULL)
		return;
	LongstringRef ref = (LongstringRef)((size_t)(*str) - LongstringRefHeader);
	ref->refCount++;
}

inline void FreeLongstring(pasLongstring* str) {
	DecRefLongstring(str);
	*str = NULL;
}

inline uint32_t LengthLongstring(pasLongstring* str) {
	if(*str == NULL)
		return 0;
	return ((LongstringRef)((size_t)(*str) - LongstringRefHeader))->length;
}

inline void AssignLongstring(pasLongstring* target, pasLongstring newStr) {
	DecRefLongstring(target);
	*target = newStr;
	IncRefLongstring(target);
}

inline void UniqueLongstring(pasLongstring* target) {
	if(target == NULL)
		return;
	if(((LongstringRef)((size_t)(*target) - LongstringRefHeader))->refCount==1)
		return;
	
	pasLongstring newtarget = CreateLongstring(LengthLongstring(target), *target);
	DecRefLongstring(target);
	*target = newtarget;
}

inline pasLongstring AddLongstring(pasLongstring* left, pasLongstring* right) {
	uint32_t a,b;
	a = LengthLongstring(left);
	b = LengthLongstring(right);
	if(a + b == 0)
		return NULL;
		
	pasLongstring result = CreateLongstring(left->codePage, left->elementSize, a + b, NULL);
	if(a!=0)
		memcpy(result, left, a * left->elementSize);
	if(b!=0)
		memcpy(result, right, b * left->elementSize);
	
	return result;
}

//#endif // __OBJPAS2CMAIN__