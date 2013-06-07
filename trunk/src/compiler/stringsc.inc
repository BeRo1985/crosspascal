[[[

uint32_t stringRefCount;

typedef struct {
    uint32_t codePage;     // 0x0000 - 0xffff = ansistring codepage
    uint32_t elementSize;  // 1 = ansistring, 2 = widestring and unicodestring, 4 = hugestring
	uint32_t refCount;     // 0xffffffff = widestring which is not reference counted
	uint32_t length;
} LongstringRefHeader;

#define LongstringRefHeaderSize sizeof(LongstringRefHeader)

void CheckRefLongstring(pasLongstring str) {
	LongstringRefHeader* header;

	if(str == NULL)
		return;
	header = (void*)((uint32_t)(str) - LongstringRefHeaderSize);

	if(0xffffffff == header->refCount)
		return;

   	if((header->refCount) == 0) {
                stringRefCount--;
	 	free(&header);
        }
}

void pasWriteLongString(pasLongstring x) {
  printf(x);
  CheckRefLongstring(x);
}

pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data) {
	LongstringRefHeader* header;
	char* ref;
	uint32_t* zero;

	if(length == 0)
		return NULL;

        stringRefCount++;
	header = (LongstringRefHeader*)malloc(LongstringRefHeaderSize + (sizeof(uint32_t) + (length * elementSize)));
	ref = ((char*)header) + LongstringRefHeaderSize;
	header->codePage=codePage;
	header->elementSize=elementSize;
	header->refCount=1;
	header->length=length;
	if(NULL!=data)
		memcpy(ref, data, length * elementSize);
        else
                header->refCount=0;

    zero = (void*)(&(((uint8_t*)ref)[length * elementSize]));
    *zero = 0;
	return ref;
}

void DecRefLongstring(pasLongstring *str) {
	LongstringRefHeader* header;
	if(*str == NULL)
		return;

	header = (pasLongstring)((uint32_t)(*str) - LongstringRefHeaderSize);
	if(0xffffffff == header->refCount)
		return;
	if((--(header->refCount)) == 0) {
                stringRefCount--;
	 	free(header);
        }
}

void IncRefLongstring(pasLongstring *str) {
	LongstringRefHeader* header;

	if(*str == NULL)
		return;
    header = (LongstringRefHeader*)((uint32_t)(*str) - LongstringRefHeaderSize);
	if(0xffffffff == header->refCount)
		return;
	header->refCount++;
}

void FreeLongstring(pasLongstring *str) {
	DecRefLongstring(str);
	*str = NULL;
}

uint32_t LengthLongstring(pasLongstring str) {
	uint32_t len;
	LongstringRefHeader* header;

	if(str == NULL)
		return 0;
	header = (LongstringRefHeader*)((uint32_t)(str) - LongstringRefHeaderSize);
	len = header->length;
	CheckRefLongstring(str);
	return len;
}

void AssignLongstring(pasLongstring *target, pasLongstring newStr) {
	DecRefLongstring(target);
	*target = newStr;
	IncRefLongstring(target);
}

void UniqueLongstring(pasLongstring *target) {
    LongstringRefHeader* header;
    pasLongstring newtarget;

    if(target == NULL)
    	return;
    header = (LongstringRefHeader*)((uint32_t)(*target) - LongstringRefHeaderSize);
    if(header->refCount == 1)
    	return;

    newtarget = CreateLongstring(header->codePage, header->elementSize, header->length, *target);
    DecRefLongstring(target);
    *target = newtarget;
}

void pasToShortstring(void* result, pasLongstring str) {
    uint32_t length,i,v;
    LongstringRefHeader* header;

    ((uint8_t*)(result))[0] = 0;
    if(NULL == str)
        return;

    header = (LongstringRefHeader*)((uint32_t)(str) - LongstringRefHeaderSize);

    if(header->length>255)
        length = 255;
    else
        length = header->length;

    ((uint8_t*)(result))[0] = length;

    for(i=0;i<length;i++) {
        switch(header->elementSize){
            case 1:{
              v = ((uint8_t*)(str))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(str))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(str))[i];
              break;
            }
        }
        ((uint8_t*)(result))[i+1] = v;
    }
    CheckRefLongstring(str);
    return;
}

pasShortstring1 pasToShortstring1(pasLongstring str) { pasShortstring1 result; pasToShortstring(&result, str); return result; }
pasShortstring2 pasToShortstring2(pasLongstring str) { pasShortstring2 result; pasToShortstring(&result, str); return result; }
pasShortstring3 pasToShortstring3(pasLongstring str) { pasShortstring3 result; pasToShortstring(&result, str); return result; }
pasShortstring4 pasToShortstring4(pasLongstring str) { pasShortstring4 result; pasToShortstring(&result, str); return result; }
pasShortstring5 pasToShortstring5(pasLongstring str) { pasShortstring5 result; pasToShortstring(&result, str); return result; }
pasShortstring6 pasToShortstring6(pasLongstring str) { pasShortstring6 result; pasToShortstring(&result, str); return result; }
pasShortstring7 pasToShortstring7(pasLongstring str) { pasShortstring7 result; pasToShortstring(&result, str); return result; }
pasShortstring8 pasToShortstring8(pasLongstring str) { pasShortstring8 result; pasToShortstring(&result, str); return result; }
pasShortstring9 pasToShortstring9(pasLongstring str) { pasShortstring9 result; pasToShortstring(&result, str); return result; }
pasShortstring10 pasToShortstring10(pasLongstring str) { pasShortstring10 result; pasToShortstring(&result, str); return result; }
pasShortstring11 pasToShortstring11(pasLongstring str) { pasShortstring11 result; pasToShortstring(&result, str); return result; }
pasShortstring12 pasToShortstring12(pasLongstring str) { pasShortstring12 result; pasToShortstring(&result, str); return result; }
pasShortstring13 pasToShortstring13(pasLongstring str) { pasShortstring13 result; pasToShortstring(&result, str); return result; }
pasShortstring14 pasToShortstring14(pasLongstring str) { pasShortstring14 result; pasToShortstring(&result, str); return result; }
pasShortstring15 pasToShortstring15(pasLongstring str) { pasShortstring15 result; pasToShortstring(&result, str); return result; }
pasShortstring16 pasToShortstring16(pasLongstring str) { pasShortstring16 result; pasToShortstring(&result, str); return result; }
pasShortstring17 pasToShortstring17(pasLongstring str) { pasShortstring17 result; pasToShortstring(&result, str); return result; }
pasShortstring18 pasToShortstring18(pasLongstring str) { pasShortstring18 result; pasToShortstring(&result, str); return result; }
pasShortstring19 pasToShortstring19(pasLongstring str) { pasShortstring19 result; pasToShortstring(&result, str); return result; }
pasShortstring20 pasToShortstring20(pasLongstring str) { pasShortstring20 result; pasToShortstring(&result, str); return result; }
pasShortstring21 pasToShortstring21(pasLongstring str) { pasShortstring21 result; pasToShortstring(&result, str); return result; }
pasShortstring22 pasToShortstring22(pasLongstring str) { pasShortstring22 result; pasToShortstring(&result, str); return result; }
pasShortstring23 pasToShortstring23(pasLongstring str) { pasShortstring23 result; pasToShortstring(&result, str); return result; }
pasShortstring24 pasToShortstring24(pasLongstring str) { pasShortstring24 result; pasToShortstring(&result, str); return result; }
pasShortstring25 pasToShortstring25(pasLongstring str) { pasShortstring25 result; pasToShortstring(&result, str); return result; }
pasShortstring26 pasToShortstring26(pasLongstring str) { pasShortstring26 result; pasToShortstring(&result, str); return result; }
pasShortstring27 pasToShortstring27(pasLongstring str) { pasShortstring27 result; pasToShortstring(&result, str); return result; }
pasShortstring28 pasToShortstring28(pasLongstring str) { pasShortstring28 result; pasToShortstring(&result, str); return result; }
pasShortstring29 pasToShortstring29(pasLongstring str) { pasShortstring29 result; pasToShortstring(&result, str); return result; }
pasShortstring30 pasToShortstring30(pasLongstring str) { pasShortstring30 result; pasToShortstring(&result, str); return result; }
pasShortstring31 pasToShortstring31(pasLongstring str) { pasShortstring31 result; pasToShortstring(&result, str); return result; }
pasShortstring32 pasToShortstring32(pasLongstring str) { pasShortstring32 result; pasToShortstring(&result, str); return result; }
pasShortstring33 pasToShortstring33(pasLongstring str) { pasShortstring33 result; pasToShortstring(&result, str); return result; }
pasShortstring34 pasToShortstring34(pasLongstring str) { pasShortstring34 result; pasToShortstring(&result, str); return result; }
pasShortstring35 pasToShortstring35(pasLongstring str) { pasShortstring35 result; pasToShortstring(&result, str); return result; }
pasShortstring36 pasToShortstring36(pasLongstring str) { pasShortstring36 result; pasToShortstring(&result, str); return result; }
pasShortstring37 pasToShortstring37(pasLongstring str) { pasShortstring37 result; pasToShortstring(&result, str); return result; }
pasShortstring38 pasToShortstring38(pasLongstring str) { pasShortstring38 result; pasToShortstring(&result, str); return result; }
pasShortstring39 pasToShortstring39(pasLongstring str) { pasShortstring39 result; pasToShortstring(&result, str); return result; }
pasShortstring40 pasToShortstring40(pasLongstring str) { pasShortstring40 result; pasToShortstring(&result, str); return result; }
pasShortstring41 pasToShortstring41(pasLongstring str) { pasShortstring41 result; pasToShortstring(&result, str); return result; }
pasShortstring42 pasToShortstring42(pasLongstring str) { pasShortstring42 result; pasToShortstring(&result, str); return result; }
pasShortstring43 pasToShortstring43(pasLongstring str) { pasShortstring43 result; pasToShortstring(&result, str); return result; }
pasShortstring44 pasToShortstring44(pasLongstring str) { pasShortstring44 result; pasToShortstring(&result, str); return result; }
pasShortstring45 pasToShortstring45(pasLongstring str) { pasShortstring45 result; pasToShortstring(&result, str); return result; }
pasShortstring46 pasToShortstring46(pasLongstring str) { pasShortstring46 result; pasToShortstring(&result, str); return result; }
pasShortstring47 pasToShortstring47(pasLongstring str) { pasShortstring47 result; pasToShortstring(&result, str); return result; }
pasShortstring48 pasToShortstring48(pasLongstring str) { pasShortstring48 result; pasToShortstring(&result, str); return result; }
pasShortstring49 pasToShortstring49(pasLongstring str) { pasShortstring49 result; pasToShortstring(&result, str); return result; }
pasShortstring50 pasToShortstring50(pasLongstring str) { pasShortstring50 result; pasToShortstring(&result, str); return result; }
pasShortstring51 pasToShortstring51(pasLongstring str) { pasShortstring51 result; pasToShortstring(&result, str); return result; }
pasShortstring52 pasToShortstring52(pasLongstring str) { pasShortstring52 result; pasToShortstring(&result, str); return result; }
pasShortstring53 pasToShortstring53(pasLongstring str) { pasShortstring53 result; pasToShortstring(&result, str); return result; }
pasShortstring54 pasToShortstring54(pasLongstring str) { pasShortstring54 result; pasToShortstring(&result, str); return result; }
pasShortstring55 pasToShortstring55(pasLongstring str) { pasShortstring55 result; pasToShortstring(&result, str); return result; }
pasShortstring56 pasToShortstring56(pasLongstring str) { pasShortstring56 result; pasToShortstring(&result, str); return result; }
pasShortstring57 pasToShortstring57(pasLongstring str) { pasShortstring57 result; pasToShortstring(&result, str); return result; }
pasShortstring58 pasToShortstring58(pasLongstring str) { pasShortstring58 result; pasToShortstring(&result, str); return result; }
pasShortstring59 pasToShortstring59(pasLongstring str) { pasShortstring59 result; pasToShortstring(&result, str); return result; }
pasShortstring60 pasToShortstring60(pasLongstring str) { pasShortstring60 result; pasToShortstring(&result, str); return result; }
pasShortstring61 pasToShortstring61(pasLongstring str) { pasShortstring61 result; pasToShortstring(&result, str); return result; }
pasShortstring62 pasToShortstring62(pasLongstring str) { pasShortstring62 result; pasToShortstring(&result, str); return result; }
pasShortstring63 pasToShortstring63(pasLongstring str) { pasShortstring63 result; pasToShortstring(&result, str); return result; }
pasShortstring64 pasToShortstring64(pasLongstring str) { pasShortstring64 result; pasToShortstring(&result, str); return result; }
pasShortstring65 pasToShortstring65(pasLongstring str) { pasShortstring65 result; pasToShortstring(&result, str); return result; }
pasShortstring66 pasToShortstring66(pasLongstring str) { pasShortstring66 result; pasToShortstring(&result, str); return result; }
pasShortstring67 pasToShortstring67(pasLongstring str) { pasShortstring67 result; pasToShortstring(&result, str); return result; }
pasShortstring68 pasToShortstring68(pasLongstring str) { pasShortstring68 result; pasToShortstring(&result, str); return result; }
pasShortstring69 pasToShortstring69(pasLongstring str) { pasShortstring69 result; pasToShortstring(&result, str); return result; }
pasShortstring70 pasToShortstring70(pasLongstring str) { pasShortstring70 result; pasToShortstring(&result, str); return result; }
pasShortstring71 pasToShortstring71(pasLongstring str) { pasShortstring71 result; pasToShortstring(&result, str); return result; }
pasShortstring72 pasToShortstring72(pasLongstring str) { pasShortstring72 result; pasToShortstring(&result, str); return result; }
pasShortstring73 pasToShortstring73(pasLongstring str) { pasShortstring73 result; pasToShortstring(&result, str); return result; }
pasShortstring74 pasToShortstring74(pasLongstring str) { pasShortstring74 result; pasToShortstring(&result, str); return result; }
pasShortstring75 pasToShortstring75(pasLongstring str) { pasShortstring75 result; pasToShortstring(&result, str); return result; }
pasShortstring76 pasToShortstring76(pasLongstring str) { pasShortstring76 result; pasToShortstring(&result, str); return result; }
pasShortstring77 pasToShortstring77(pasLongstring str) { pasShortstring77 result; pasToShortstring(&result, str); return result; }
pasShortstring78 pasToShortstring78(pasLongstring str) { pasShortstring78 result; pasToShortstring(&result, str); return result; }
pasShortstring79 pasToShortstring79(pasLongstring str) { pasShortstring79 result; pasToShortstring(&result, str); return result; }
pasShortstring80 pasToShortstring80(pasLongstring str) { pasShortstring80 result; pasToShortstring(&result, str); return result; }
pasShortstring81 pasToShortstring81(pasLongstring str) { pasShortstring81 result; pasToShortstring(&result, str); return result; }
pasShortstring82 pasToShortstring82(pasLongstring str) { pasShortstring82 result; pasToShortstring(&result, str); return result; }
pasShortstring83 pasToShortstring83(pasLongstring str) { pasShortstring83 result; pasToShortstring(&result, str); return result; }
pasShortstring84 pasToShortstring84(pasLongstring str) { pasShortstring84 result; pasToShortstring(&result, str); return result; }
pasShortstring85 pasToShortstring85(pasLongstring str) { pasShortstring85 result; pasToShortstring(&result, str); return result; }
pasShortstring86 pasToShortstring86(pasLongstring str) { pasShortstring86 result; pasToShortstring(&result, str); return result; }
pasShortstring87 pasToShortstring87(pasLongstring str) { pasShortstring87 result; pasToShortstring(&result, str); return result; }
pasShortstring88 pasToShortstring88(pasLongstring str) { pasShortstring88 result; pasToShortstring(&result, str); return result; }
pasShortstring89 pasToShortstring89(pasLongstring str) { pasShortstring89 result; pasToShortstring(&result, str); return result; }
pasShortstring90 pasToShortstring90(pasLongstring str) { pasShortstring90 result; pasToShortstring(&result, str); return result; }
pasShortstring91 pasToShortstring91(pasLongstring str) { pasShortstring91 result; pasToShortstring(&result, str); return result; }
pasShortstring92 pasToShortstring92(pasLongstring str) { pasShortstring92 result; pasToShortstring(&result, str); return result; }
pasShortstring93 pasToShortstring93(pasLongstring str) { pasShortstring93 result; pasToShortstring(&result, str); return result; }
pasShortstring94 pasToShortstring94(pasLongstring str) { pasShortstring94 result; pasToShortstring(&result, str); return result; }
pasShortstring95 pasToShortstring95(pasLongstring str) { pasShortstring95 result; pasToShortstring(&result, str); return result; }
pasShortstring96 pasToShortstring96(pasLongstring str) { pasShortstring96 result; pasToShortstring(&result, str); return result; }
pasShortstring97 pasToShortstring97(pasLongstring str) { pasShortstring97 result; pasToShortstring(&result, str); return result; }
pasShortstring98 pasToShortstring98(pasLongstring str) { pasShortstring98 result; pasToShortstring(&result, str); return result; }
pasShortstring99 pasToShortstring99(pasLongstring str) { pasShortstring99 result; pasToShortstring(&result, str); return result; }
pasShortstring100 pasToShortstring100(pasLongstring str) { pasShortstring100 result; pasToShortstring(&result, str); return result; }
pasShortstring101 pasToShortstring101(pasLongstring str) { pasShortstring101 result; pasToShortstring(&result, str); return result; }
pasShortstring102 pasToShortstring102(pasLongstring str) { pasShortstring102 result; pasToShortstring(&result, str); return result; }
pasShortstring103 pasToShortstring103(pasLongstring str) { pasShortstring103 result; pasToShortstring(&result, str); return result; }
pasShortstring104 pasToShortstring104(pasLongstring str) { pasShortstring104 result; pasToShortstring(&result, str); return result; }
pasShortstring105 pasToShortstring105(pasLongstring str) { pasShortstring105 result; pasToShortstring(&result, str); return result; }
pasShortstring106 pasToShortstring106(pasLongstring str) { pasShortstring106 result; pasToShortstring(&result, str); return result; }
pasShortstring107 pasToShortstring107(pasLongstring str) { pasShortstring107 result; pasToShortstring(&result, str); return result; }
pasShortstring108 pasToShortstring108(pasLongstring str) { pasShortstring108 result; pasToShortstring(&result, str); return result; }
pasShortstring109 pasToShortstring109(pasLongstring str) { pasShortstring109 result; pasToShortstring(&result, str); return result; }
pasShortstring110 pasToShortstring110(pasLongstring str) { pasShortstring110 result; pasToShortstring(&result, str); return result; }
pasShortstring111 pasToShortstring111(pasLongstring str) { pasShortstring111 result; pasToShortstring(&result, str); return result; }
pasShortstring112 pasToShortstring112(pasLongstring str) { pasShortstring112 result; pasToShortstring(&result, str); return result; }
pasShortstring113 pasToShortstring113(pasLongstring str) { pasShortstring113 result; pasToShortstring(&result, str); return result; }
pasShortstring114 pasToShortstring114(pasLongstring str) { pasShortstring114 result; pasToShortstring(&result, str); return result; }
pasShortstring115 pasToShortstring115(pasLongstring str) { pasShortstring115 result; pasToShortstring(&result, str); return result; }
pasShortstring116 pasToShortstring116(pasLongstring str) { pasShortstring116 result; pasToShortstring(&result, str); return result; }
pasShortstring117 pasToShortstring117(pasLongstring str) { pasShortstring117 result; pasToShortstring(&result, str); return result; }
pasShortstring118 pasToShortstring118(pasLongstring str) { pasShortstring118 result; pasToShortstring(&result, str); return result; }
pasShortstring119 pasToShortstring119(pasLongstring str) { pasShortstring119 result; pasToShortstring(&result, str); return result; }
pasShortstring120 pasToShortstring120(pasLongstring str) { pasShortstring120 result; pasToShortstring(&result, str); return result; }
pasShortstring121 pasToShortstring121(pasLongstring str) { pasShortstring121 result; pasToShortstring(&result, str); return result; }
pasShortstring122 pasToShortstring122(pasLongstring str) { pasShortstring122 result; pasToShortstring(&result, str); return result; }
pasShortstring123 pasToShortstring123(pasLongstring str) { pasShortstring123 result; pasToShortstring(&result, str); return result; }
pasShortstring124 pasToShortstring124(pasLongstring str) { pasShortstring124 result; pasToShortstring(&result, str); return result; }
pasShortstring125 pasToShortstring125(pasLongstring str) { pasShortstring125 result; pasToShortstring(&result, str); return result; }
pasShortstring126 pasToShortstring126(pasLongstring str) { pasShortstring126 result; pasToShortstring(&result, str); return result; }
pasShortstring127 pasToShortstring127(pasLongstring str) { pasShortstring127 result; pasToShortstring(&result, str); return result; }
pasShortstring128 pasToShortstring128(pasLongstring str) { pasShortstring128 result; pasToShortstring(&result, str); return result; }
pasShortstring129 pasToShortstring129(pasLongstring str) { pasShortstring129 result; pasToShortstring(&result, str); return result; }
pasShortstring130 pasToShortstring130(pasLongstring str) { pasShortstring130 result; pasToShortstring(&result, str); return result; }
pasShortstring131 pasToShortstring131(pasLongstring str) { pasShortstring131 result; pasToShortstring(&result, str); return result; }
pasShortstring132 pasToShortstring132(pasLongstring str) { pasShortstring132 result; pasToShortstring(&result, str); return result; }
pasShortstring133 pasToShortstring133(pasLongstring str) { pasShortstring133 result; pasToShortstring(&result, str); return result; }
pasShortstring134 pasToShortstring134(pasLongstring str) { pasShortstring134 result; pasToShortstring(&result, str); return result; }
pasShortstring135 pasToShortstring135(pasLongstring str) { pasShortstring135 result; pasToShortstring(&result, str); return result; }
pasShortstring136 pasToShortstring136(pasLongstring str) { pasShortstring136 result; pasToShortstring(&result, str); return result; }
pasShortstring137 pasToShortstring137(pasLongstring str) { pasShortstring137 result; pasToShortstring(&result, str); return result; }
pasShortstring138 pasToShortstring138(pasLongstring str) { pasShortstring138 result; pasToShortstring(&result, str); return result; }
pasShortstring139 pasToShortstring139(pasLongstring str) { pasShortstring139 result; pasToShortstring(&result, str); return result; }
pasShortstring140 pasToShortstring140(pasLongstring str) { pasShortstring140 result; pasToShortstring(&result, str); return result; }
pasShortstring141 pasToShortstring141(pasLongstring str) { pasShortstring141 result; pasToShortstring(&result, str); return result; }
pasShortstring142 pasToShortstring142(pasLongstring str) { pasShortstring142 result; pasToShortstring(&result, str); return result; }
pasShortstring143 pasToShortstring143(pasLongstring str) { pasShortstring143 result; pasToShortstring(&result, str); return result; }
pasShortstring144 pasToShortstring144(pasLongstring str) { pasShortstring144 result; pasToShortstring(&result, str); return result; }
pasShortstring145 pasToShortstring145(pasLongstring str) { pasShortstring145 result; pasToShortstring(&result, str); return result; }
pasShortstring146 pasToShortstring146(pasLongstring str) { pasShortstring146 result; pasToShortstring(&result, str); return result; }
pasShortstring147 pasToShortstring147(pasLongstring str) { pasShortstring147 result; pasToShortstring(&result, str); return result; }
pasShortstring148 pasToShortstring148(pasLongstring str) { pasShortstring148 result; pasToShortstring(&result, str); return result; }
pasShortstring149 pasToShortstring149(pasLongstring str) { pasShortstring149 result; pasToShortstring(&result, str); return result; }
pasShortstring150 pasToShortstring150(pasLongstring str) { pasShortstring150 result; pasToShortstring(&result, str); return result; }
pasShortstring151 pasToShortstring151(pasLongstring str) { pasShortstring151 result; pasToShortstring(&result, str); return result; }
pasShortstring152 pasToShortstring152(pasLongstring str) { pasShortstring152 result; pasToShortstring(&result, str); return result; }
pasShortstring153 pasToShortstring153(pasLongstring str) { pasShortstring153 result; pasToShortstring(&result, str); return result; }
pasShortstring154 pasToShortstring154(pasLongstring str) { pasShortstring154 result; pasToShortstring(&result, str); return result; }
pasShortstring155 pasToShortstring155(pasLongstring str) { pasShortstring155 result; pasToShortstring(&result, str); return result; }
pasShortstring156 pasToShortstring156(pasLongstring str) { pasShortstring156 result; pasToShortstring(&result, str); return result; }
pasShortstring157 pasToShortstring157(pasLongstring str) { pasShortstring157 result; pasToShortstring(&result, str); return result; }
pasShortstring158 pasToShortstring158(pasLongstring str) { pasShortstring158 result; pasToShortstring(&result, str); return result; }
pasShortstring159 pasToShortstring159(pasLongstring str) { pasShortstring159 result; pasToShortstring(&result, str); return result; }
pasShortstring160 pasToShortstring160(pasLongstring str) { pasShortstring160 result; pasToShortstring(&result, str); return result; }
pasShortstring161 pasToShortstring161(pasLongstring str) { pasShortstring161 result; pasToShortstring(&result, str); return result; }
pasShortstring162 pasToShortstring162(pasLongstring str) { pasShortstring162 result; pasToShortstring(&result, str); return result; }
pasShortstring163 pasToShortstring163(pasLongstring str) { pasShortstring163 result; pasToShortstring(&result, str); return result; }
pasShortstring164 pasToShortstring164(pasLongstring str) { pasShortstring164 result; pasToShortstring(&result, str); return result; }
pasShortstring165 pasToShortstring165(pasLongstring str) { pasShortstring165 result; pasToShortstring(&result, str); return result; }
pasShortstring166 pasToShortstring166(pasLongstring str) { pasShortstring166 result; pasToShortstring(&result, str); return result; }
pasShortstring167 pasToShortstring167(pasLongstring str) { pasShortstring167 result; pasToShortstring(&result, str); return result; }
pasShortstring168 pasToShortstring168(pasLongstring str) { pasShortstring168 result; pasToShortstring(&result, str); return result; }
pasShortstring169 pasToShortstring169(pasLongstring str) { pasShortstring169 result; pasToShortstring(&result, str); return result; }
pasShortstring170 pasToShortstring170(pasLongstring str) { pasShortstring170 result; pasToShortstring(&result, str); return result; }
pasShortstring171 pasToShortstring171(pasLongstring str) { pasShortstring171 result; pasToShortstring(&result, str); return result; }
pasShortstring172 pasToShortstring172(pasLongstring str) { pasShortstring172 result; pasToShortstring(&result, str); return result; }
pasShortstring173 pasToShortstring173(pasLongstring str) { pasShortstring173 result; pasToShortstring(&result, str); return result; }
pasShortstring174 pasToShortstring174(pasLongstring str) { pasShortstring174 result; pasToShortstring(&result, str); return result; }
pasShortstring175 pasToShortstring175(pasLongstring str) { pasShortstring175 result; pasToShortstring(&result, str); return result; }
pasShortstring176 pasToShortstring176(pasLongstring str) { pasShortstring176 result; pasToShortstring(&result, str); return result; }
pasShortstring177 pasToShortstring177(pasLongstring str) { pasShortstring177 result; pasToShortstring(&result, str); return result; }
pasShortstring178 pasToShortstring178(pasLongstring str) { pasShortstring178 result; pasToShortstring(&result, str); return result; }
pasShortstring179 pasToShortstring179(pasLongstring str) { pasShortstring179 result; pasToShortstring(&result, str); return result; }
pasShortstring180 pasToShortstring180(pasLongstring str) { pasShortstring180 result; pasToShortstring(&result, str); return result; }
pasShortstring181 pasToShortstring181(pasLongstring str) { pasShortstring181 result; pasToShortstring(&result, str); return result; }
pasShortstring182 pasToShortstring182(pasLongstring str) { pasShortstring182 result; pasToShortstring(&result, str); return result; }
pasShortstring183 pasToShortstring183(pasLongstring str) { pasShortstring183 result; pasToShortstring(&result, str); return result; }
pasShortstring184 pasToShortstring184(pasLongstring str) { pasShortstring184 result; pasToShortstring(&result, str); return result; }
pasShortstring185 pasToShortstring185(pasLongstring str) { pasShortstring185 result; pasToShortstring(&result, str); return result; }
pasShortstring186 pasToShortstring186(pasLongstring str) { pasShortstring186 result; pasToShortstring(&result, str); return result; }
pasShortstring187 pasToShortstring187(pasLongstring str) { pasShortstring187 result; pasToShortstring(&result, str); return result; }
pasShortstring188 pasToShortstring188(pasLongstring str) { pasShortstring188 result; pasToShortstring(&result, str); return result; }
pasShortstring189 pasToShortstring189(pasLongstring str) { pasShortstring189 result; pasToShortstring(&result, str); return result; }
pasShortstring190 pasToShortstring190(pasLongstring str) { pasShortstring190 result; pasToShortstring(&result, str); return result; }
pasShortstring191 pasToShortstring191(pasLongstring str) { pasShortstring191 result; pasToShortstring(&result, str); return result; }
pasShortstring192 pasToShortstring192(pasLongstring str) { pasShortstring192 result; pasToShortstring(&result, str); return result; }
pasShortstring193 pasToShortstring193(pasLongstring str) { pasShortstring193 result; pasToShortstring(&result, str); return result; }
pasShortstring194 pasToShortstring194(pasLongstring str) { pasShortstring194 result; pasToShortstring(&result, str); return result; }
pasShortstring195 pasToShortstring195(pasLongstring str) { pasShortstring195 result; pasToShortstring(&result, str); return result; }
pasShortstring196 pasToShortstring196(pasLongstring str) { pasShortstring196 result; pasToShortstring(&result, str); return result; }
pasShortstring197 pasToShortstring197(pasLongstring str) { pasShortstring197 result; pasToShortstring(&result, str); return result; }
pasShortstring198 pasToShortstring198(pasLongstring str) { pasShortstring198 result; pasToShortstring(&result, str); return result; }
pasShortstring199 pasToShortstring199(pasLongstring str) { pasShortstring199 result; pasToShortstring(&result, str); return result; }
pasShortstring200 pasToShortstring200(pasLongstring str) { pasShortstring200 result; pasToShortstring(&result, str); return result; }
pasShortstring201 pasToShortstring201(pasLongstring str) { pasShortstring201 result; pasToShortstring(&result, str); return result; }
pasShortstring202 pasToShortstring202(pasLongstring str) { pasShortstring202 result; pasToShortstring(&result, str); return result; }
pasShortstring203 pasToShortstring203(pasLongstring str) { pasShortstring203 result; pasToShortstring(&result, str); return result; }
pasShortstring204 pasToShortstring204(pasLongstring str) { pasShortstring204 result; pasToShortstring(&result, str); return result; }
pasShortstring205 pasToShortstring205(pasLongstring str) { pasShortstring205 result; pasToShortstring(&result, str); return result; }
pasShortstring206 pasToShortstring206(pasLongstring str) { pasShortstring206 result; pasToShortstring(&result, str); return result; }
pasShortstring207 pasToShortstring207(pasLongstring str) { pasShortstring207 result; pasToShortstring(&result, str); return result; }
pasShortstring208 pasToShortstring208(pasLongstring str) { pasShortstring208 result; pasToShortstring(&result, str); return result; }
pasShortstring209 pasToShortstring209(pasLongstring str) { pasShortstring209 result; pasToShortstring(&result, str); return result; }
pasShortstring210 pasToShortstring210(pasLongstring str) { pasShortstring210 result; pasToShortstring(&result, str); return result; }
pasShortstring211 pasToShortstring211(pasLongstring str) { pasShortstring211 result; pasToShortstring(&result, str); return result; }
pasShortstring212 pasToShortstring212(pasLongstring str) { pasShortstring212 result; pasToShortstring(&result, str); return result; }
pasShortstring213 pasToShortstring213(pasLongstring str) { pasShortstring213 result; pasToShortstring(&result, str); return result; }
pasShortstring214 pasToShortstring214(pasLongstring str) { pasShortstring214 result; pasToShortstring(&result, str); return result; }
pasShortstring215 pasToShortstring215(pasLongstring str) { pasShortstring215 result; pasToShortstring(&result, str); return result; }
pasShortstring216 pasToShortstring216(pasLongstring str) { pasShortstring216 result; pasToShortstring(&result, str); return result; }
pasShortstring217 pasToShortstring217(pasLongstring str) { pasShortstring217 result; pasToShortstring(&result, str); return result; }
pasShortstring218 pasToShortstring218(pasLongstring str) { pasShortstring218 result; pasToShortstring(&result, str); return result; }
pasShortstring219 pasToShortstring219(pasLongstring str) { pasShortstring219 result; pasToShortstring(&result, str); return result; }
pasShortstring220 pasToShortstring220(pasLongstring str) { pasShortstring220 result; pasToShortstring(&result, str); return result; }
pasShortstring221 pasToShortstring221(pasLongstring str) { pasShortstring221 result; pasToShortstring(&result, str); return result; }
pasShortstring222 pasToShortstring222(pasLongstring str) { pasShortstring222 result; pasToShortstring(&result, str); return result; }
pasShortstring223 pasToShortstring223(pasLongstring str) { pasShortstring223 result; pasToShortstring(&result, str); return result; }
pasShortstring224 pasToShortstring224(pasLongstring str) { pasShortstring224 result; pasToShortstring(&result, str); return result; }
pasShortstring225 pasToShortstring225(pasLongstring str) { pasShortstring225 result; pasToShortstring(&result, str); return result; }
pasShortstring226 pasToShortstring226(pasLongstring str) { pasShortstring226 result; pasToShortstring(&result, str); return result; }
pasShortstring227 pasToShortstring227(pasLongstring str) { pasShortstring227 result; pasToShortstring(&result, str); return result; }
pasShortstring228 pasToShortstring228(pasLongstring str) { pasShortstring228 result; pasToShortstring(&result, str); return result; }
pasShortstring229 pasToShortstring229(pasLongstring str) { pasShortstring229 result; pasToShortstring(&result, str); return result; }
pasShortstring230 pasToShortstring230(pasLongstring str) { pasShortstring230 result; pasToShortstring(&result, str); return result; }
pasShortstring231 pasToShortstring231(pasLongstring str) { pasShortstring231 result; pasToShortstring(&result, str); return result; }
pasShortstring232 pasToShortstring232(pasLongstring str) { pasShortstring232 result; pasToShortstring(&result, str); return result; }
pasShortstring233 pasToShortstring233(pasLongstring str) { pasShortstring233 result; pasToShortstring(&result, str); return result; }
pasShortstring234 pasToShortstring234(pasLongstring str) { pasShortstring234 result; pasToShortstring(&result, str); return result; }
pasShortstring235 pasToShortstring235(pasLongstring str) { pasShortstring235 result; pasToShortstring(&result, str); return result; }
pasShortstring236 pasToShortstring236(pasLongstring str) { pasShortstring236 result; pasToShortstring(&result, str); return result; }
pasShortstring237 pasToShortstring237(pasLongstring str) { pasShortstring237 result; pasToShortstring(&result, str); return result; }
pasShortstring238 pasToShortstring238(pasLongstring str) { pasShortstring238 result; pasToShortstring(&result, str); return result; }
pasShortstring239 pasToShortstring239(pasLongstring str) { pasShortstring239 result; pasToShortstring(&result, str); return result; }
pasShortstring240 pasToShortstring240(pasLongstring str) { pasShortstring240 result; pasToShortstring(&result, str); return result; }
pasShortstring241 pasToShortstring241(pasLongstring str) { pasShortstring241 result; pasToShortstring(&result, str); return result; }
pasShortstring242 pasToShortstring242(pasLongstring str) { pasShortstring242 result; pasToShortstring(&result, str); return result; }
pasShortstring243 pasToShortstring243(pasLongstring str) { pasShortstring243 result; pasToShortstring(&result, str); return result; }
pasShortstring244 pasToShortstring244(pasLongstring str) { pasShortstring244 result; pasToShortstring(&result, str); return result; }
pasShortstring245 pasToShortstring245(pasLongstring str) { pasShortstring245 result; pasToShortstring(&result, str); return result; }
pasShortstring246 pasToShortstring246(pasLongstring str) { pasShortstring246 result; pasToShortstring(&result, str); return result; }
pasShortstring247 pasToShortstring247(pasLongstring str) { pasShortstring247 result; pasToShortstring(&result, str); return result; }
pasShortstring248 pasToShortstring248(pasLongstring str) { pasShortstring248 result; pasToShortstring(&result, str); return result; }
pasShortstring249 pasToShortstring249(pasLongstring str) { pasShortstring249 result; pasToShortstring(&result, str); return result; }
pasShortstring250 pasToShortstring250(pasLongstring str) { pasShortstring250 result; pasToShortstring(&result, str); return result; }
pasShortstring251 pasToShortstring251(pasLongstring str) { pasShortstring251 result; pasToShortstring(&result, str); return result; }
pasShortstring252 pasToShortstring252(pasLongstring str) { pasShortstring252 result; pasToShortstring(&result, str); return result; }
pasShortstring253 pasToShortstring253(pasLongstring str) { pasShortstring253 result; pasToShortstring(&result, str); return result; }
pasShortstring254 pasToShortstring254(pasLongstring str) { pasShortstring254 result; pasToShortstring(&result, str); return result; }
pasShortstring255 pasToShortstring255(pasLongstring str) { pasShortstring255 result; pasToShortstring(&result, str); return result; }

void AssignShortstring(uint8_t* target, uint32_t maxLength, pasLongstring strInput) {
    uint32_t length,i,v;
    LongstringRefHeader* header;
    if(NULL == strInput) {
        ((uint8_t*)(target))[0] = 0;
        ((uint8_t*)(target))[1] = 0;
        return ;
    }

    header = (LongstringRefHeader*)((uint32_t)(strInput) - LongstringRefHeaderSize);

    if(maxLength<header->length)
        length = maxLength;
    else
        length = header->length;
		
   ((uint8_t*)(target))[0] = length;
   target++;

    for(i=0;i<length;i++) {
        switch(header->elementSize){
            case 1:{
              v = ((uint8_t*)(strInput))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(strInput))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(strInput))[i];
              break;
            }
        }
        ((uint8_t*)(target))[i] = v;
    }
    if(length<maxLength)
        ((uint8_t*)(target))[length] = 0;

    CheckRefLongstring(strInput);
}

pasLongstring ConvertLongstring(uint32_t codePage, uint32_t elementSize, pasLongstring strInput) {
    LongstringRefHeader* header;
    pasLongstring newtarget;
    uint32_t i,v;
    char* temp;

    if(strInput == NULL)
        return strInput;

    header = (LongstringRefHeader*)((uint32_t)(strInput) - LongstringRefHeaderSize);

    if((header->codePage == codePage)&(header->elementSize == elementSize))
        return strInput;

    newtarget = CreateLongstring(codePage, elementSize, header->length, NULL);

    temp = newtarget;

    for(i=0;i<header->length;i++) {
        switch(header->elementSize){
            case 1:{
              v = ((uint8_t*)(strInput))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(strInput))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(strInput))[i];
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
	CheckRefLongstring(strInput);

    return newtarget;
}

pasLongstring ConvertShortstring(uint32_t codePage, uint32_t elementSize, char* strInput) {
    pasLongstring newtarget;
    uint32_t i,v,len;
    char* temp;

    if(strInput == NULL)
        return strInput;

    len = strInput[0];

    newtarget = CreateLongstring(codePage, elementSize, len, NULL);

    temp = newtarget;

    for(i=1;i<=len;i++) {
        v = ((uint8_t*)(strInput))[i];

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

    return newtarget;
}

pasLongstring AddLongstring(pasLongstring left, pasLongstring right) {
    uint32_t a,b;
    LongstringRefHeader* headerA;
    LongstringRefHeader* headerB;
    char* temp;
    uint32_t elementSize;
    uint32_t v;
    pasLongstring result;

    if(NULL == left)
        return right;

    if(NULL == right)
        return left;

    headerA = (LongstringRefHeader*)((uint32_t)(left) - LongstringRefHeaderSize);
    headerB = (LongstringRefHeader*)((uint32_t)(right) - LongstringRefHeaderSize);

    a = headerA->length;
    b = headerB->length;
    // TODO: Codepage handling

    if(headerA->elementSize == headerB->elementSize){
        result = CreateLongstring(headerA->codePage, headerA->elementSize, a + b, NULL);
        temp = result;
        if(a!=0){
            memcpy(temp, left, a * headerA->elementSize);
            temp += a * headerA->elementSize;
        }
        if(b!=0)
            memcpy(temp, right, b * headerB->elementSize);
    }else{
        // TODO: Variable-length UTF8 and UTF16 handling

        int i;

        elementSize = (headerA->elementSize < headerB->elementSize) ? headerB->elementSize : headerA->elementSize;
        result = CreateLongstring(headerA->codePage, elementSize, a + b, NULL);

        temp = result;

        for(i = 0; i < a; i++){
          switch(headerA->elementSize){
            case 1:{
              v = ((uint8_t*)(left))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(left))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(left))[i];
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
          switch(headerB->elementSize){
            case 1:{
              v = ((uint8_t*)(right))[i];
              break;
            }
            case 2:{
              v = ((uint16_t*)(right))[i];
              break;
            }
            case 4:{
              v = ((uint32_t*)(right))[i];
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

    CheckRefLongstring(left);
    CheckRefLongstring(right);
    return result;
}
]]]