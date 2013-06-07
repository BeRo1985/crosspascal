unit System;
{$c+}
{$j+}

interface                                                                               

const CompilerInfoString:pchar='OBJPAS2C';
     
      fmClosed=$D7B0;
      fmInput=$D7B1;
      fmOutput=$D7B2;
      fmInOut=$D7B3;

      faReadOnly=$01;
      faHidden=$02;
      faSysFile=$04;
      faVolumeID=$08;
      faDirectory=$10;
      faArchive=$20;
      faAnyFile=$3F;

      varEmpty=$0000;
      varNull=$0001;
      varSmallint=$0002;
      varInteger=$0003;
      varSingle=$0004;
      varDouble=$0005;
      varCurrency=$0006;
      varDate=$0007;
      varOleStr=$0008;
      varDispatch=$0009;
      varError=$000A;
      varBoolean=$000B;
      varVariant=$000C;
      varUnknown=$000D;
      varShortInt=$0010;
      varByte=$0011;
      varWord=$0012;
      varLongWord=$0013;
      varInt64=$0014;

      varStrArg=$0048;
      varString=$0100;
      varAny=$0101;

      varTypeMask=$0FFF;
      varArray=$2000;
      varByRef=$4000;

      vtInteger=0;
      vtBoolean=1;
      vtChar=2;
      vtExtended=3;
      vtString=4;
      vtPointer=5;
      vtPChar=6;
      vtObject=7;
      vtClass=8;
      vtWideChar=9;
      vtPWideChar=10;
      vtAnsiString=11;
      vtCurrency=12;
      vtVariant=13;
      vtInterface=14;
      vtWideString=15;
      vtInt64=16;

      vmtSelfPtr=sizeof(pointer)*(-19);
      vmtIntfTable=sizeof(pointer)*(-18);
      vmtAutoTable=sizeof(pointer)*(-17);
      vmtInitTable=sizeof(pointer)*(-16);
      vmtTypeInfo=sizeof(pointer)*(-15);
      vmtFieldTable=sizeof(pointer)*(-14);
      vmtMethodTable=sizeof(pointer)*(-13);
      vmtDynamicTable=sizeof(pointer)*(-12);
      vmtClassName=sizeof(pointer)*(-11);
      vmtInstanceSize=sizeof(pointer)*(-10);
      vmtParent=sizeof(pointer)*(-9);
      vmtSafeCallException=sizeof(pointer)*(-8);
      vmtAfterConstruction=sizeof(pointer)*(-7);
      vmtBeforeDestruction=sizeof(pointer)*(-6);
      vmtDispatch=sizeof(pointer)*(-5);
      vmtDefaultHandler=sizeof(pointer)*(-4);
      vmtNewInstance=sizeof(pointer)*(-3);
      vmtFreeInstance=sizeof(pointer)*(-2);
      vmtDestroy=sizeof(pointer)*(-1);

      vmtQueryInterface=sizeof(pointer)*0;
      vmtAddRef=sizeof(pointer)*1;
      vmtRelease=sizeof(pointer)*2;
      vmtCreateObject=sizeof(pointer)*3;

      FileMode:integer=2;

      RandSeed:integer=0;

type DWORD=LongWord;
     Bool=LongBool;
     PBool=^Bool;

     PShortInt=^ShortInt;
     PByte=^Byte;
     PSmallInt=^SmallInt;
     PWord=^Word;
     PLongInt=^LongInt;
     PLongWord=^LongWord;
     PInt64=^Int64;
     PUInt64=^UInt64;
     PQWord=^QWord;
	 PInteger=^Integer;
	 PDouble=^Double;

     TByteArray=array[0..32767] of Byte;
     PByteArray=^TByteArray;

     TWordArray=array[0..16383] of Word;
     PWordArray=^TWordArray;
	 
	 THandle=Cardinal;

     HRESULT=type ptrint;

const S_OK=0;
      S_FALSE=$00000001;
      E_NOINTERFACE=HRESULT($80004002);
      E_UNEXPECTED=HRESULT($8000ffff);
      E_NOTIMPL=HRESULT($80004001);

type TObject=class;

     TClass=class of TObject;

     PGUID=^TGUID;
     TGUID=packed record
      D1:longword;
      D2:word;
      D3:word;
      D4:array[0..7] of byte;
     end;

     PInterfaceEntry=^TInterfaceEntry;
     TInterfaceEntry=packed record
      IID:TGUID;
      VTable:pointer;
      IOffset:longint;
      ImplGetter:longint;
      ImplGetterPtr:pointer;
     end;

     PInterfaceTable=^TInterfaceTable;
     TInterfaceTable=packed record
      EntryCount:longint;
      Entries:array[0..9999] of TInterfaceEntry;
     end;

     TMethod=record
      Code:pointer;
      Data:pointer;
     end;

     TInitContext=record
     end;

     TDispatchMessage=record
      MsgID:word;
     end;

     TObject=class
      constructor Create;
      procedure Free;
      class function InitInstance(Instance:Pointer):TObject;
      procedure CleanupInstance;
      function ClassType:TClass;
      class function ClassName:shortstring;
      class function ClassNameIs(const Name:ansistring):boolean;
      class function ClassParent:TClass;
      class function ClassInfo:pointer;
      class function InstanceSize:longint;
      class function InheritsFrom(AClass:TClass):boolean;
      class function MethodAddress(const Name:shortstring):pointer;
      class function MethodName(Address:pointer):shortstring;
      function FieldAddress(const Name:shortstring):pointer;
      function GetInterface(const IID:TGUID;out Obj):boolean;
      class function GetInterfaceEntry(const IID:TGUID):PInterfaceEntry;
      class function GetInterfaceTable:PInterfaceTable;
      function SafeCallException(ExceptObject:TObject;ExceptAddr:pointer):HResult; virtual;
      procedure AfterConstruction; virtual;
      procedure BeforeDestruction; virtual;
      procedure Dispatch(var Message); virtual;
      procedure DefaultHandler(var Message); virtual;
      class function NewInstance:TObject; virtual;
      procedure FreeInstance; virtual;
      destructor Destroy; virtual;
     end;

     IInterface=interface['{00000000-0000-0000-C000-000000000046}']
      function QueryInterface(const IID:TGUID;out Obj):HResult; stdcall;
      function _AddRef:longint; stdcall;
      function _Release:longint; stdcall;
     end;

     IUnknown=IInterface;

     TInterfacedObject=class(TObject,IInterface)
      protected
       FRefCount:longint;
       function QueryInterface(const IID:TGUID;out Obj):HResult; stdcall;
       function _AddRef:longint; stdcall;
       function _Release:longint; stdcall;
      public
       procedure AfterConstruction; override;
       procedure BeforeDestruction; override;
       class function NewInstance:TObject; override;
       property RefCount:longint read FRefCount;
     end;

     TInterfacedClass=class of TInterfacedObject;

     Exception=class
      public
       Message:ansistring;
       constructor Create(Msg:ansistring);
       destructor Destroy; override;
     end;

var Input,Output:text;

procedure Move(var Src; var Dst; Size: Cardinal);

[[[ // include in system.h

#include "stdlib.h"
#include "math.h"
#include "setjmp.h"
#include <stdio.h>

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

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 199901L
#define C99
#include <inttypes.h>
#else
#undef C99
#endif

#if defined(__STDC_VERSION__) && __STDC_VERSION__ >= 201112L
#define C11
#else
#undef C11
#endif

#ifdef _WIN64
#define __WIN__
#elif _WIN32
#define __WIN__
#elif __APPLE__
#include "TargetConditionals.h"
#include <libkern/OSAtomic.h>
#elif __android
#include <cutils/atomic.h>
#elif __linux
#elif __unix
#elif __posix
#endif

#ifdef __WIN__
#include <windows.h>
#endif

#ifdef __WIN__
#define pasAtomicInc32(x) InterlockedIncrement((int32_t*)&(x))
#define pasAtomicDec32(x) InterlockedDecrement((int32_t*)&(x))
#elif __GNUC__
#define pasAtomicInc32(x) __sync_fetch_and_add((int32_t*)&(x),1)
#define pasAtomicDec32(x) __sync_fetch_and_sub((int32_t*)&(x),1)
#elif __android
#define pasAtomicInc32(x) android_atomic_inc((int32_t*)&(x))
#define pasAtomicDec32(x) android_atomic_dec((int32_t*)&(x))
#elif __APPLE__
#define pasAtomicInc32(x) OSAtomicIncrement32Barrier((int32_t*)&(x))
#define pasAtomicDec32(x) OSAtomicDecrement32Barrier((int32_t*)&(x))
#else
#define pasAtomicInc32(x) do{ x++; }while(0)
#define pasAtomicDec32(x) do{ x--; }while(0)
#endif

#define pastkUnknown 0
#define pastkInteger 1
#define pastkAnsiChar 2
#define pastkEnumeration 3
#define pastkFloat 4
#define pastkString 5
#define pastkSet 6
#define pastkClass 7
#define pastkMethod 8
#define pastkWideChar 9
#define pastkLString 10
#define pastkWString 11
#define pastkVariant 12
#define pastkArray 13
#define pastkRecord 14
#define pastkInterface 15
#define pastkInt64 16
#define pastkDynArray 17
#define pastkHugeChar 18
#define pastkUString 19
#define pastkHString 20

typedef struct pasTypeInfo {
  size_t kind;
  size_t codePage;
  void* vmt;
  char* name;
  void* data;
} pasTypeInfo;

typedef pasTypeInfo* pasTypeInfoPointer;
typedef pasTypeInfoPointer* pasTypeInfoPointerPointer;

extern pasTypeInfo pasTypeInfoUnknown;
extern pasTypeInfoPointer pasTypeInfoUnknownPointer;

typedef struct pasFieldInfo {
  pasTypeInfoPointerPointer typeInfo;
  size_t offset;
} pasFieldInfo;

typedef pasFieldInfo* pasFieldInfoPointer;

typedef struct pasFieldTable {
  size_t x;
  size_t size;
  size_t count;
  pasFieldInfo fields[0];
} pasFieldTable;

typedef struct pasFieldTableStripped {
  size_t x;
  size_t size;
  size_t count;
} pasFieldTableStripped;

typedef pasFieldTable* pasFieldTablePointer;

typedef struct pasObjectDynamicMethodTable;

typedef struct pasObjectDynamicMethodTableItem {
  size_t index;
  void* method;
} pasObjectDynamicMethodTableItem;

typedef struct pasObjectVirtualMethodTable* pasObjectVirtualMethodTablePointer;

typedef struct pasObjectVirtualMethodTable {
  size_t size;
  void* dynamicMethodTable;
  pasObjectVirtualMethodTablePointer ancestorVirtualMethodTable;
  void* virtualMethods[0];
} pasObjectVirtualMethodTable;

typedef struct pasObjectVirtualMethodTableStripped {
  size_t size;
  void* dynamicMethodTable;
  pasObjectVirtualMethodTablePointer ancestorVirtualMethodTable;
} pasObjectVirtualMethodTableStripped;

typedef struct pasObjectWithVirtualMethodTable {
  pasObjectVirtualMethodTablePointer vmt;
} pasObjectWithVirtualMethodTable;

typedef struct pasClassDynamicMethodTableItem {
  size_t index;
  void* method;
} pasClassDynamicMethodTableItem;

typedef struct pasClassMethodTableItem {
  void* address;
  char* name;
} pasClassMethodTableItem;

typedef void** pasClassTableItem;

typedef struct pasClassTable {
  size_t count;
  pasClassTableItem classes[0];
} pasClassTable;

typedef struct pasClassTableStripped {
  size_t count;
  pasClassTableItem classes;
} pasClassTableStripped;

typedef struct pasClassFieldTableItem {
  size_t fieldOffset;
  size_t typeIndex;
  char *name;
} pasClassFieldTableItem;

typedef pasClassFieldTableItem* pasClassFieldTableItemPointer;

typedef struct pasClassFieldTable {
  size_t count;
  void* classTable;
  pasClassFieldTableItem fields[0];
} pasClassFieldTable;

typedef struct pasClassFieldTableStripped {
  size_t count;
  void* classTable;
} pasClassFieldTableStripped;

typedef struct pasClassMethodTable {
  size_t count;
  pasClassMethodTableItem methods[0];
} pasClassMethodTable;

typedef struct pasClassMethodTableStripped {
  size_t count;
} pasClassMethodTableStripped;

#pragma pack(push,1)
typedef struct pasGUID {
   uint32_t GUID[4];
} pasGUID;

typedef struct pasInterfaceEntry {
  pasGUID IID;
  void* vTable;
  uint32_t iOffset;
  uint32_t implGetter;
  void* implGetterPtr;
} pasInterfaceEntry;

typedef struct pasInterfaceTable {
  uint32_t entryCount;
  pasInterfaceEntry entries[10000];
} pasInterfaceTable;

typedef struct pasInterfaceIUnknown {
  size_t (*QueryInterface)(void* instance,pasGUID* IID,void* obj);
  int32_t (*_AddRef)(void* instance);
  int32_t (*_Release)(void* instance);
} pasInterfaceIUnknown;

#pragma pack(pop)


typedef pasInterfaceEntry* pasInterfaceEntryPointer;

typedef pasInterfaceTable* pasInterfaceTablePointer;

typedef struct pasClassVirtualMethodTable;

typedef struct pasClassVirtualMethodTable* pasClassVirtualMethodTablePointer;

typedef struct pasClassVirtualMethodTable {
  void* vmtSelfPtr;
  void* vmtIntfTable;
  void* vmtAutoTable;
  void* vmtInitTable;
  void* vmtTypeInfo;
  void* vmtFieldTable;
  void* vmtMethodTable;
  void* vmtDynamicTable;
  void* vmtClassName;
  size_t vmtInstanceSize;
  void* vmtParent;
  void* virtualMethods[0];
} pasClassVirtualMethodTable;

typedef struct pasClassVirtualMethodTableStripped {
  void* vmtSelfPtr;
  void* vmtIntfTable;
  void* vmtAutoTable;
  void* vmtInitTable;
  void* vmtTypeInfo;
  void* vmtFieldTable;
  void* vmtMethodTable;
  void* vmtDynamicTable;
  void* vmtClassName;
  size_t vmtInstanceSize;
  void* vmtParent;
} pasClassVirtualMethodTableStripped;

typedef struct pasClassVirtualMethodTableTObject {
  void* vmtSelfPtr;
  void* vmtIntfTable;
  void* vmtAutoTable;
  void* vmtInitTable;
  void* vmtTypeInfo;
  void* vmtFieldTable;
  void* vmtMethodTable;
  void* vmtDynamicTable;
  void* vmtClassName;
  size_t vmtInstanceSize;
  void* vmtParent;
  void* vmtSafeCallException;
  void* vmtAfterConstruction;
  void* vmtBeforeDestruction;
  void* vmtDispatch;
  void* vmtDefaultHandler;
  void* vmtNewInstance;
  void* vmtFreeInstance;
  void* vmtDestroy;
} pasClassVirtualMethodTableTObject;

#if 1
#define pasClassVMTMask(x) ((void*)(((void*)x) - sizeof(pasClassVirtualMethodTableTObject)))
#define pasClassVMTUnmask(x) ((void*)(((void*)x) + sizeof(pasClassVirtualMethodTableTObject)))
#else
#define pasClassVMTMask(x) ((void*)(x))
#define pasClassVMTUnmask(x) ((void*)(x))
#endif

typedef struct {
  uint32_t dummy;
} pasFile;

typedef struct pasExceptionStackJmpBufItem {
  void* next;
  jmp_buf jmpBuf;
} pasExceptionStackJmpBufItem;

typedef pasExceptionStackJmpBufItem* pasExceptionStackJmpBufItemPointer;

typedef struct pasExceptionThreadContext {
  void* previous;
  void* next;
  pasExceptionStackJmpBufItem* jmpBufItemStack;
  void* object;
  void* address;
} pasExceptionThreadContext;

typedef pasExceptionThreadContext* pasExceptionThreadContextPointer;

void* pasGetMem(size_t size);
void pasReallocMem(void** ptr,size_t size);
void pasFreeMem(void* ptr);

void pasZeroMem(void* ptr,size_t size);

void pasInitializeRecord(void* p, pasTypeInfo* t);

void pasInitializeArray(void* p, pasTypeInfo* t, size_t count);

void pasInitialize(void* p, pasTypeInfo* t);

void pasFinalizeRecord(void* p, pasTypeInfo* t);

void pasFinalizeArray(void* p, pasTypeInfo* t, size_t count);

void pasFinalize(void* p, pasTypeInfo* t);

void* pasObjectDMTDispatch(void** object,size_t index);

void* pasClassDMTDispatch(void* classVMT,size_t index);

typedef void* pasDynArray;

void pasAssignArray(pasDynArray* target, pasDynArray newValue, pasTypeInfo* t);
void pasSetLengthArray(pasDynArray* target, uint32_t length, uint32_t arraySize);
uint32_t pasLengthArray(pasDynArray target);

void pasFreeArray(pasDynArray* target, pasTypeInfo* t);

void pasExceptionInitContext(pasExceptionThreadContext* context);
void pasExceptionInit();
pasExceptionThreadContext* pasGetCurrentExceptionThreadContext();
void pasExceptionPushJmpBuf(pasExceptionStackJmpBufItem* item);
pasExceptionStackJmpBufItem* pasExceptionPopJmpBuf();
void pasExceptionRaise(void *object, void* addr);
void pasExceptionReraise();
void* pasExceptioneGetRaiseObject();

]]]

{$i stringsh.inc}

[[[
#ifdef C99
#define pasWriteInt(x) printf("%i", x);
#define pasWriteUInt(x) printf("%u", x);
#define pasWriteInt8(x) printf("%"PRIi8, x);
#define pasWriteUInt8(x) printf("%"PRIu8, x);
#define pasWriteInt16(x) printf("%"PRIi16, x);
#define pasWriteUInt16(x) printf("%"PRIu16, x);
#define pasWriteInt32(x) printf("%"PRIi32, x);
#define pasWriteUInt32(x) printf("%"PRIu32, x);
#define pasWriteInt64(x) printf("%"PRIi64, x);
#define pasWriteUInt64(x) printf("%"PRIu64, x);
#else
#define pasWriteInt(x) printf("%i", x);
#define pasWriteUInt(x) printf("%u", x);
#define pasWriteInt8(x) printf("%i", x);
#define pasWriteUInt8(x) printf("%u", x);
#define pasWriteInt16(x) printf("%i", x);
#define pasWriteUInt16(x) printf("%u", x);
#define pasWriteInt32(x) printf("%i", x);
#define pasWriteUInt32(x) printf("%u", x);
#ifdef _MSC_VER
#define pasWriteInt64(x) printf("%I64d", x);
#define pasWriteUInt64(x) printf("%I64u", x);
#else
#define pasWriteInt64(x) printf("%ll", x);
#define pasWriteUInt64(x) printf("%llu", x);
#endif
#endif
#define pasWriteChar(x) printf("%c", x);
#define pasWriteFloat(x) printf("%f", x);
#define pasWriteBool(x) { if(x) printf("TRUE"); else printf("FALSE"); }

void pasWriteLongString(pasLongstring x);
#define pasWritePChar(x) printf(x);

#define pasTRUNC(x) floor(x);
#define pasROUND(x) round(x);
#define pasSQR(x) sqr(x);
#define pasSQRT(x) sqrt(x);
]]]

procedure Randomize;
function Random(Max:integer):integer; overload;
function Random:double; overload;

procedure FillChar(var Dest;Count:integer;Value:byte); overload;
procedure FillChar(var Dest;Count:integer;Value:ansichar); overload;

procedure GetMem(var p;Size:ptrint);

procedure FreeMem(var p); overload;
procedure FreeMem(var p;Size:ptrint); overload;

procedure ReallocMem(var p;Size:ptrint);

function Power(Base,Exponent:extended):extended;
function Exp(x:extended):extended;
function Ln(x:extended):extended;

function Min(a,b:longint):longint; overload;
function Min(a,b:int64):int64; overload;
function Min(a,b:extended):extended; overload;

function Max(a,b:longint):longint; overload;
function Max(a,b:int64):int64; overload;
function Max(a,b:extended):extended; overload;

implementation

procedure Randomize;
begin
 RandSeed:=(RandSeed*$8088405)+1;
end;

function Random(Max:integer):integer;
begin
 RandSeed:=(RandSeed*$8088405)+1;
 result:=uint64((uint64(longword(RandSeed))*uint64(Max)) shr 32);
end;

function Random:double;
const DivFactor=(1.0/$10000)/$10000;
begin
 RandSeed:=(RandSeed*$8088405)+1;
 result:=RandSeed*DivFactor;
end;

procedure FillChar(var Dest;Count:integer;Value:byte);
begin
[[[ memset(&<<<Dest>>>, <<<Count>>>, <<<Value>>>); ]]]
end;

procedure FillChar(var Dest;Count:integer;Value:ansichar);
begin
[[[ memset(&<<<Dest>>>, <<<Count>>>, <<<Value>>>); ]]]
end;

procedure GetMem(var p;Size:ptrint);
begin
[[[ <<<p>>> = pasGetMem(<<<Size>>>); ]]]
end;

procedure FreeMem(var p);
begin
[[[ pasFreeMem((void*)<<<p>>>); ]]]
end;

procedure FreeMem(var p;Size:ptrint);
begin
[[[ pasFreeMem((void*)<<<p>>>); ]]]
end;

procedure ReallocMem(var p;Size:ptrint);
begin
[[[ pasReallocMem(&<<<p>>>,<<<Size>>>); ]]]
end;

function Power(Base,Exponent:extended):extended;
begin
  result := 1;
end;

function Exp(x:extended):extended;
begin
  result := 1;
end;

function Ln(x:extended):extended;
begin
  result := 1;
end;

function Min(a,b:longint):longint;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Min(a,b:int64):int64;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Min(a,b:extended):extended;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Max(a,b:longint):longint;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

function Max(a,b:int64):int64;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

function Max(a,b:extended):extended;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

[[[
#include "stdint.h"
#include "string.h"
#include "stdlib.h"
#include "stdio.h"

pasTypeInfo pasTypeInfoUnknown = {
  pastkUnknown,
  0,
  "\x03???",
  NULL
};

pasTypeInfoPointer pasTypeInfoUnknownPointer = (void*)&pasTypeInfoUnknown;

typedef struct {
    uint32_t refCount;
    uint32_t length;
} pasDynArrayHeader;

#define pasDynArrayHeaderSize sizeof(pasDynArrayHeader)

void pasFreeArray(pasDynArray* target, pasTypeInfo* t) {
    pasDynArrayHeader* header;

    if(NULL == *target)
        return;

    header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
    if(1 < header->refCount)
        header->refCount--;
    else{
        // red TODO: pasFinalize(); you do need to add a "optional" array element type info somewhere in the dynamic array data
        // structure, together with a if-check here
        if(t) {
                pasFinalizeArray(target, t, header->length);
        }
        free(header);
    }
}

void pasAssignArray(pasDynArray* target, pasDynArray newValue, pasTypeInfo* t) {
    pasDynArrayHeader* header;

    pasFreeArray(target, t);

    *target = newValue;

    if(NULL != *target) {
        header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
        header->refCount++;
    }
}

uint32_t pasLengthArray(pasDynArray target) {
    pasDynArrayHeader* header;

	if(target != NULL) {
		header = (pasDynArrayHeader*)((uint32_t)(target) - pasDynArrayHeaderSize);
		//printf("*** %i kthxbye %i ***", header->length, header->refCount);
		return header->length;
	} else
		return 0;
}
		
void pasSetLengthArray(pasDynArray* target, uint32_t length, uint32_t arraySize) {
    pasDynArrayHeader* header;
	pasDynArrayHeader* oldHeader;
    uint32_t oldLength;

    if(NULL == *target) {
        if(length==0)
            return;

        header = (pasDynArrayHeader*)malloc(pasDynArrayHeaderSize + arraySize * length);
        memset(header, 0, pasDynArrayHeaderSize + arraySize * length);
		header->refCount = 1;
        header->length = length;
        *target = (pasDynArray)((uint32_t)(header) + pasDynArrayHeaderSize);
    } else {
        header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
         if(1 == header->refCount) {
             header = realloc(header, pasDynArrayHeaderSize + arraySize*length);
			 *target = (pasDynArray)((uint32_t)(header) + pasDynArrayHeaderSize);
			 header->length = length;
			 header->refCount = 1;
             return;
         }
         oldLength = header->length;

         header = (pasDynArrayHeader*)malloc(pasDynArrayHeaderSize + arraySize * length);
         header->refCount = 1;
         header->length = length;

         if(oldLength>length)
             memcpy((void*)((uint32_t)(header) + pasDynArrayHeaderSize), &target, length * arraySize);
         else
             memcpy((void*)((uint32_t)(header) + pasDynArrayHeaderSize), &target, oldLength * arraySize);

         *target = (pasDynArray)((uint32_t)(header) + pasDynArrayHeaderSize);
    }
}

void* pasGetMem(size_t size){
  return malloc(size);
}

void pasReallocMem(void** ptr, size_t size){
  *ptr = realloc(*ptr, size);
}

void pasFreeMem(void* ptr){
  free(ptr);
}

void pasZeroMem(void* ptr, size_t size){
  memset(ptr, size, 0);
}

void pasInitializeRecord(void* p, pasTypeInfo* t){
  pasFieldTable* ft = t->data;
  size_t count = ft->count;
  while(count--){
    pasInitializeArray(p + ft->fields[count].offset, (void*)(*ft->fields[count].typeInfo), 1);
  }
  if(t->vmt){
    ((pasObjectWithVirtualMethodTable*)p)->vmt = t->vmt;
  } 
}

void pasInitializeArray(void* p, pasTypeInfo* t, size_t count){
  pasFieldTable* ft;
  if((int32_t)count > 0){
    switch(t->kind){
      case pastkLString:
      case pastkWString:
      case pastkUString:
      case pastkHString:
      case pastkInterface:
      case pastkDynArray:{
        while(count--){
          *((void**)p) = NULL;
          p += sizeof(void*);
        }
        break;
      }
      case pastkVariant:{
        count *= 4;
        while(count--){
          *((void**)p) = NULL;
          p += sizeof(void*);
        }
        break;
      }
      case pastkArray:{
        ft = t->data;
        while(count--){
          pasInitializeArray(p, (void*)(*ft->fields[0].typeInfo), ft->count);
          p += ft->size;
        }
        break;
      }
      case pastkRecord:{
        ft = t->data;
        while(count--){
          pasInitializeRecord(p, t);
          p += ft->size;
        }
        break;
      }
      default:{
        break;
      }
    }
  }
}

void pasInitialize(void* p, pasTypeInfo* t){
  pasInitializeArray(p, t, 1);
}

void pasFinalizeRecord(void* p, pasTypeInfo* t){
  pasFieldTable* ft = t->data;
  size_t count = ft->count;
  while(count--){
    pasFinalizeArray(p + ft->fields[count].offset, (void*)(*ft->fields[count].typeInfo), 1);
  }
}

void pasFinalizeArray(void* p, pasTypeInfo* t, size_t count){
  pasFieldTable* ft;
  if(((int32_t)count)>0){
    switch(t->kind){
      case pastkLString:{
        while(count--){
		  FreeLongstring((pasLongstring*)(p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkWString:{
        while(count--){
		  FreeLongstring((pasLongstring*)(p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkUString:{
        while(count--){
		  FreeLongstring((pasLongstring*)(p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkHString:{
        while(count--){
		  FreeLongstring((pasLongstring*)(p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkInterface:{
        while(count--){
          // BeRo, your task!
          *((void**)p) = NULL;
          p += sizeof(void*);
        }
        break;
      }
      case pastkDynArray:{
        ft = t->data;
        while(count--){
          pasFreeArray(*((void**)p),(void*)(*ft->fields[0].typeInfo));
          p += sizeof(void*);
        }
        break;
      }
      case pastkVariant:{
        count *= 4;
        while(count--){
          // BeRo and/or red, yours task!
          *((void**)p) = NULL;
          p += sizeof(void*);
        }
        break;
      }
      case pastkArray:{
        ft = t->data;
        while(count--){
          pasFinalizeArray(p, (void*)(*ft->fields[0].typeInfo), ft->count);
          p += ft->size;
        }
        break;
      }
      case pastkRecord:{
		ft = t->data;
        while(count--){
          pasFinalizeRecord(p, t);
          p += ft->size;
        }
        break;
      }
      default:{
        break;
      }
    }
  }
}

void pasFinalize(void* p, pasTypeInfo* t){
  pasFinalizeArray(p, t, 1);
}

void* pasObjectDMTDispatch(void** object, size_t index){
  pasObjectVirtualMethodTable* VMT = (void*)*object;
  while(VMT){
    pasObjectDynamicMethodTableItem* DMT = VMT->dynamicMethodTable;
    if(DMT){
      while(DMT->method){
        if(DMT->index == index){
          return DMT->method;
        }
        DMT++;
      }
    }
    VMT = VMT->ancestorVirtualMethodTable;
  }
  return NULL;
}

void* pasClassDMTDispatch(void* classVMT, size_t index){
  pasClassVirtualMethodTable* VMT = (void*)classVMT;
  while(VMT){
    pasClassDynamicMethodTableItem* DMT = VMT->vmtDynamicTable;
    if(DMT){
      while(DMT->method){
        if(DMT->index == index){
          return DMT->method;
        }
        DMT++;
      }
    }
    VMT = VMT->vmtParent;
  }
  return NULL;
}

pasExceptionThreadContext mainExceptionThreadContext;

void pasExceptionInitContext(pasExceptionThreadContext* context){
  context->previous = NULL;
  context->next = NULL;
  context->jmpBufItemStack = NULL;
  context->object = NULL;
  context->address = NULL;
}

void pasExceptionInit(){
  pasExceptionInitContext(&mainExceptionThreadContext);
}

pasExceptionThreadContext* pasGetCurrentExceptionThreadContext(){
  return &mainExceptionThreadContext;
}

void pasExceptionPushJmpBuf(pasExceptionStackJmpBufItem* item){
  pasExceptionThreadContext* currentCxceptionThreadContext = pasGetCurrentExceptionThreadContext();
  item->next = currentCxceptionThreadContext->jmpBufItemStack;
  currentCxceptionThreadContext->jmpBufItemStack = item;
}

pasExceptionStackJmpBufItem* pasExceptionPopJmpBuf(){
  pasExceptionThreadContext* currentCxceptionThreadContext = pasGetCurrentExceptionThreadContext();
  pasExceptionStackJmpBufItem* result = currentCxceptionThreadContext->jmpBufItemStack;
  currentCxceptionThreadContext->jmpBufItemStack = result->next;
}

void pasExceptionRaise(void *object, void* addr){
  pasExceptionThreadContext* currentCxceptionThreadContext = pasGetCurrentExceptionThreadContext();
  if(currentCxceptionThreadContext->jmpBufItemStack){
    currentCxceptionThreadContext->object = object;
    currentCxceptionThreadContext->address = addr;
    longjmp(currentCxceptionThreadContext->jmpBufItemStack->jmpBuf, 1);
  }
}

void pasExceptionReraise(){
  pasExceptionThreadContext* currentCxceptionThreadContext = pasGetCurrentExceptionThreadContext();
  pasExceptionRaise(currentCxceptionThreadContext->object, currentCxceptionThreadContext->address);
}

void* pasExceptioneGetRaiseObject(){
  pasExceptionThreadContext* currentCxceptionThreadContext = pasGetCurrentExceptionThreadContext();
  return currentCxceptionThreadContext->object;
}

]]]

{$i stringsc.inc}

procedure Move(var Src; var Dst; Size: Cardinal);
begin
  [[[
    memcpy(<<<@Dst>>>, <<<@Src>>>, <<<Size>>>);
  ]]]
end;

{$hints off}
constructor TObject.Create;
begin
end;

procedure TObject.Free;
begin
 if assigned(self) then begin
  Destroy;
 end;
end;

class function TObject.InitInstance(Instance:Pointer):TObject;
var ClassPtr: TClass;
begin
[[[
  <<<result>>> = <<<Instance>>>;
  memset(<<<result>>>, 0, <<<InstanceSize>>>);
  <<<result>>>->INTERNAL_FIELD_VMT = (void*)<<<self>>>;
  <<<ClassPtr>>> = <<<self>>>;
  while(<<<ClassPtr>>>){
    /* INTERFACE TODO */
    <<<ClassPtr>>> = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(<<<ClassPtr>>>))->vmtParent;
  }
]]]
end;

procedure TObject.CleanupInstance;
var ClassPtr: TClass;
    InitTable: pointer;
begin
[[[
  <<<ClassPtr>>> = <<<ClassType>>>;
  <<<InitTable>>> = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(<<<ClassPtr>>>))->vmtInitTable;
  while(<<<ClassPtr>>> && <<<InitTable>>>){
    pasFinalizeRecord(<<<self>>>, <<<InitTable>>>);
    <<<ClassPtr>>> = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(<<<ClassPtr>>>))->vmtParent;
    if(<<<ClassPtr>>>){
      <<<InitTable>>> = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(<<<ClassPtr>>>))->vmtInitTable;
    }
  }
]]]
end;

function TObject.ClassType:TClass;
begin
[[[
  <<<result>>> = <<<self>>>->INTERNAL_FIELD_VMT;
]]]
end;

class function TObject.ClassName:shortstring;
var len:byte;
begin
[[[
  pasClassVirtualMethodTable* VMT = (void*)pasClassVMTUnmask(<<<self>>>);
  <<<len>>> = *((uint8_t*)VMT->vmtClassName);
  memcpy(&<<<result>>>, VMT->vmtClassName, <<<len>>>);
]]]
end;

class function TObject.ClassNameIs(const Name:ansistring):boolean;
begin
[[[
  pasClassVirtualMethodTable* VMT = (void*)pasClassVMTUnmask(<<<self>>>);
  uint8_t* name = (void*)&<<<Name>>>;
  uint8_t* className = (void*)VMT->vmtClassName;
  int i = *className;
  <<<result>>> = 0;
  if(*name == i){
    for(; i > 0; i--, name++, className++){
      if(*name != *className){
        break;
      }
    }
    if(!i){
     <<<result>>> = 1;
    }
  }
]]]
end;

class function TObject.ClassParent:TClass;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)(pasClassVMTUnmask(<<<self>>>)))->vmtParent;
]]]
end;

class function TObject.ClassInfo:pointer;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)(pasClassVMTUnmask(<<<self>>>)))->vmtTypeInfo;
]]]
end;

class function TObject.InstanceSize:longint;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)(pasClassVMTUnmask(<<<self>>>)))->vmtInstanceSize;
]]]
end;

class function TObject.InheritsFrom(AClass:TClass):boolean;
begin
[[[
  <<<result>>> = 0;
  pasClassVirtualMethodTable* VMT = (void*)<<<self>>>;
  while(VMT){
    if(VMT == <<<AClass>>>){
      <<<result>>> = 1;
      break;
    }
    VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
  }
]]]
end;

class function TObject.MethodAddress(const Name:shortstring):pointer;
begin
[[[
  <<<result>>> = NULL;
  if(*((uint8_t*)&<<<Name>>>)){
    pasClassVirtualMethodTable* VMT = (void*)<<<self>>>;
    while(VMT){
      pasClassMethodTable* methodTable = (void*)(((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtMethodTable);
      for(int i = 0; i < methodTable->count; i++){
        pasClassMethodTableItem* methodTableItem = &methodTable->methods[i];
        uint8_t* name = (void*)&<<<Name>>>;
        uint8_t* methodName = (void*)methodTableItem->name;
        int j = *methodName;
        if(*name == j){
          for(; j > 0; j--, name++, methodName++){
            if(*name != *methodName){
              break;
            }
          }
          if(!j){
            <<<result>>> = (void*)methodTableItem->address;
            goto done;
          }
        }
      }
      VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
    }
  }
  done:
]]]
end;

class function TObject.MethodName(Address:pointer):shortstring;
begin
[[[
  *((uint8_t*)&<<<result>>>) = 0;
  if(<<<Address>>>){
    pasClassVirtualMethodTable* VMT = (void*)<<<self>>>;
    while(VMT){
      pasClassMethodTable* methodTable = (void*)(((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtMethodTable);
      for(int i = 0; i < methodTable->count; i++){
        pasClassMethodTableItem* methodTableItem = &methodTable->methods[i];
        uint8_t* methodName = (void*)(methodTableItem->name);
        if((void*)methodTableItem->address == (void*)<<<Address>>>){
          memcpy(&<<<result>>>, methodName, *methodName);
          goto done;
        }
      }
      VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
    }
  }
  done:
]]]
end;

function TObject.FieldAddress(const Name:shortstring):pointer;
begin
[[[
  <<<result>>> = NULL;
  if(*((uint8_t*)&<<<Name>>>)){
    pasClassVirtualMethodTable* VMT = (void*)(<<<self>>>->INTERNAL_FIELD_VMT);
    while(VMT){
      pasClassFieldTable* fieldTable = (void*)(((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtFieldTable);
      for(int i = 0; i < fieldTable->count; i++){
        pasClassFieldTableItem* fieldTableItem = &fieldTable->fields[i];
        uint8_t* name = (void*)&<<<Name>>>;
        uint8_t* fieldName = (void*)fieldTableItem->name;
        int j = *fieldName;                            
        if(*name == j){
          for(; j > 0; j--, name++, fieldName++){
            if(*name != *fieldName){
              break;
            }
          }
          if(!j){
            <<<result>>> = (void*)(((void*)<<<self>>>) + fieldTableItem->fieldOffset);
            goto done;
          }
        }
      }
      VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
    }
  }
  done:
]]]
end;

function TObject.GetInterface(const IID:TGUID;out Obj):boolean;
begin
[[[
  void* obj = NULL;
  pasClassVirtualMethodTable* VMT = (void*)(<<<self>>>->INTERNAL_FIELD_VMT);
  pasGUID* IID = (void*)&<<<IID>>>;
  pasInterfaceEntry* resultInterfaceEntry = NULL;
  while(VMT){
    pasInterfaceTable* interfaceTable = (void*)(((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtIntfTable);
    if(interfaceTable){
      for(int i = 0; i < interfaceTable->entryCount; i++){
        pasInterfaceEntry* interfaceEntry = (void*)&interfaceTable->entries[i];
        if((interfaceEntry->IID.GUID[0] == IID->GUID[0])&&
           (interfaceEntry->IID.GUID[1] == IID->GUID[1])&&
           (interfaceEntry->IID.GUID[2] == IID->GUID[2])&&
           (interfaceEntry->IID.GUID[3] == IID->GUID[3])){
          resultInterfaceEntry = (void*)interfaceEntry;
          goto done;
        }
      }
    }
    VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
  }
  done:
  if(resultInterfaceEntry){
    void* (*method)(void* instance);
    void* self = (void*)<<<self>>>;
    if(resultInterfaceEntry->iOffset){
      obj = self + resultInterfaceEntry->iOffset;
      if(obj){
        ((pasInterfaceIUnknown*)obj)->_AddRef(obj);
      }
    }else{
      uint32_t implGetter = resultInterfaceEntry->implGetter;
      if((implGetter >= 0xff000000ul) && (implGetter <= 0xfffffffful)){
        /* Field */
        obj = self + (implGetter & 0x00fffffful);
      }else if((implGetter >= 0xfe000000ul) && (implGetter <= 0xfefffffful)){
        /* Virtual method */
        method = *(void**)((void*)(self + (int16_t)(implGetter & 0x0000fffful)));
        obj = method(self);
      }else{
        /* Static method */
        method = resultInterfaceEntry->implGetterPtr;
        obj = method(self);
      }
    }
  }
  <<<Obj>>> = obj;
  <<<result>>> = obj ? 1 : 0;
]]]
end;

class function TObject.GetInterfaceEntry(const IID:TGUID):PInterfaceEntry;
begin
[[[
  pasClassVirtualMethodTable* VMT = (void*)<<<self>>>;
  pasGUID* IID = (void*)&<<<IID>>>;
  <<<result>>> = NULL;
  while(VMT){
    pasInterfaceTable* interfaceTable = (void*)(((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtIntfTable);
    if(interfaceTable){
      for(int i = 0; i < interfaceTable->entryCount; i++){
        pasInterfaceEntry* interfaceEntry = (void*)&interfaceTable->entries[i];
        if((interfaceEntry->IID.GUID[0] == IID->GUID[0])&&
           (interfaceEntry->IID.GUID[1] == IID->GUID[1])&&
           (interfaceEntry->IID.GUID[2] == IID->GUID[2])&&
           (interfaceEntry->IID.GUID[3] == IID->GUID[3])){
          <<<result>>> = (void*)interfaceEntry;
          goto done;
        }
      }
    }
    VMT = ((pasClassVirtualMethodTable*)pasClassVMTUnmask(VMT))->vmtParent;
  }
  done:
]]]
end;

class function TObject.GetInterfaceTable:PInterfaceTable;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)(pasClassVMTUnmask(<<<self>>>)))->vmtIntfTable;
]]]
end;

function TObject.SafeCallException(ExceptObject:TObject;ExceptAddr:pointer):HResult;
begin
 result:=HResult($8000ffff); // E_UNEXPECTED 
end;

procedure TObject.AfterConstruction;
begin
end;

procedure TObject.BeforeDestruction;
begin
end;

procedure TObject.Dispatch(var Message);
begin
end;

procedure TObject.DefaultHandler(var Message);
begin
end;

class function TObject.NewInstance:TObject;
begin
[[[
  <<<result>>> = pasGetMem(<<<InstanceSize>>>);
]]]
 result:=InitInstance(result);
end;

procedure TObject.FreeInstance;
begin
 CleanupInstance;
[[[
  pasFreeMem(<<<self>>>);
]]]
end;

destructor TObject.Destroy;
begin
end;

function TInterfacedObject.QueryInterface(const IID:TGUID;out Obj):HResult; stdcall;
begin
 if GetInterface(IID,Obj) then begin
  result:=0;
 end else begin
  result:=E_NOINTERFACE;
 enD;
end;

function TInterfacedObject._AddRef:longint; stdcall;
begin
[[[
  pasAtomicInc32(<<<FRefCount>>>);
]]]
end;

function TInterfacedObject._Release:longint; stdcall;
begin
[[[
  pasAtomicDec32(<<<FRefCount>>>);
]]]
 if FRefCount=0 then begin
  Destroy;
 end;
end;

procedure TInterfacedObject.AfterConstruction;
begin
[[[
  pasAtomicDec32(<<<FRefCount>>>);
]]]
end;

procedure TInterfacedObject.BeforeDestruction;
begin
 if FRefCount<>0 then begin
  // TODO: Error(reInvalidPtr);
 end;
end;

class function TInterfacedObject.NewInstance:TObject;
begin
 result:=inherited NewInstance;
 TInterfacedObject(result).FRefCount:=1;
end;

constructor Exception.Create(Msg:ansistring);
begin
 inherited Create;
 Message:=Msg;
end;

destructor Exception.Destroy;
begin
 Message:='';
 inherited Destroy;
end;

{$hints on}

initialization
[[[
 stringRefCount = 0;
 pasExceptionInit();
]]]
finalization
[[[
 printf("string references left: %i\n", stringRefCount);
]]]

end.