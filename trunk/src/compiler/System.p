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

      vmtSelfPtr=sizeof(pointer)*0;
      vmtIntfTable=sizeof(pointer)*1;
      vmtAutoTable=sizeof(pointer)*2;
      vmtInitTable=sizeof(pointer)*3;
      vmtTypeInfo=sizeof(pointer)*4;
      vmtFieldTable=sizeof(pointer)*5;
      vmtMethodTable=sizeof(pointer)*6;
      vmtDynamicTable=sizeof(pointer)*7;
      vmtClassName=sizeof(pointer)*8;
      vmtInstanceSize=sizeof(pointer)*9;
      vmtParent=sizeof(pointer)*10;
      vmtSafeCallException=sizeof(pointer)*11;
      vmtAfterConstruction=sizeof(pointer)*12;
      vmtBeforeDestruction=sizeof(pointer)*13;
      vmtDispatch=sizeof(pointer)*14;
      vmtDefaultHandler=sizeof(pointer)*15;
      vmtNewInstance=sizeof(pointer)*16;
      vmtFreeInstance=sizeof(pointer)*17;
      vmtDestroy=sizeof(pointer)*18;
      
      vmtQueryInterface=sizeof(pointer)*19;
      vmtAddRef=sizeof(pointer)*20;
      vmtRelease=sizeof(pointer)*21;
      vmtCreateObject=sizeof(pointer)*22;

      FileMode:integer=2;

      RandSeed:integer=0;

type DWORD=LongWord;
     Bool=LongBool;
     PBool=^Bool;

     TObject=class;

     TClass=class of TObject;

     HRESULT=type ptrint;

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

var Input,Output:text;

procedure Move(var Src; var Dst; Size: Cardinal);

[[[ // include in system.h

#include "stdlib.h"
#include "math.h"

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

typedef struct pasObjectWithVirtualMethodTable {
  pasObjectVirtualMethodTablePointer vmt;
} pasObjectWithVirtualMethodTable;

typedef struct pasClassDynamicMethodTableItem {
  size_t index;
  void* method;
} pasClassDynamicMethodTableItem;

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
  void* vmtSafeCallException;
  void* vmtAfterConstruction;
  void* vmtBeforeDestruction;
  void* vmtDispatch;
  void* vmtDefaultHandler;
  void* vmtNewInstance;
  void* vmtFreeInstance;
  void* vmtDestroy;
  void* virtualMethods[0];
} pasClassVirtualMethodTable;

typedef struct {
	uint32_t dummy;
} pasFile;

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

]]]

{$i stringsh.inc}

[[[
#define pasWriteInt(x) printf("%i", x);
#define pasWriteUInt(x) printf("%u", x);
#define pasWriteChar(x) printf("%c", x);
#define pasWriteFloat(x) printf("%f", x);
#define pasWriteBool(x) if(x) printf("TRUE"); else printf("FALSE");

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

procedure NextRandSeed;
begin
 RandSeed:=(RandSeed*$8088405)+1;
end;

procedure Randomize;
begin
 NextRandSeed;
end;

function Random(Max:integer):integer;
begin
 NextRandSeed;
 if Max>0 then begin
  result:=RandSeed mod Max;
 end else begin
  result:=0;
 end;
end;

function Random:double;
const DivFactor=(1.0/$10000)/$10000;
begin
 NextRandSeed;
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

void pasFreeArray(pasDynArray* target) {
    pasDynArrayHeader* header;

    if(NULL == *target)
        return;

    header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
    if(1 < header->refCount)
        header->refCount--;
    else{
        // red TODO: pasFinalize(); you do need to add a "optional" array element type info somewhere in the dynamic array data
        // structure, together with a if-check here
        free(header);
    }
}

void pasAssignArray(pasDynArray* target, pasDynArray newValue) {
    pasDynArrayHeader* header;

    pasFreeArray(target);

    *target = newValue;

    if(NULL != *target) {
        header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
        header->refCount++;
    }
}

void pasSetLengthArray(pasDynArray* target, uint32_t length, uint32_t arraySize) {
    pasDynArrayHeader* header;
    uint32_t oldLength;

    if(NULL == *target) {
        if(length==0)
            return;

        header = (pasDynArrayHeader*)malloc(pasDynArrayHeaderSize + arraySize * length);
        header->refCount = 1;
        header->length = length;
        *target = (pasDynArray)((uint32_t)(header) + pasDynArrayHeaderSize);
    } else {
        header = (void*)((uint32_t)(*target) - pasDynArrayHeaderSize);
         if(1 == header->refCount) {
             header = realloc(header, arraySize*length);
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
  if(count){
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
  if(count){
    switch(t->kind){
      case pastkLString:{
        while(count--){
          FreeLongstring(*((void**)p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkWString:{
        while(count--){
          FreeLongstring(*((void**)p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkUString:{
        while(count--){
          FreeLongstring(*((void**)p));
          p += sizeof(void*);
        }
        break;
      }
      case pastkHString:{
        while(count--){
          FreeLongstring(*((void**)p));
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
        while(count--){
          pasFreeArray(*((void**)p));
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
begin
[[[
  
]]]
end;

procedure TObject.CleanupInstance;
begin
end;

function TObject.ClassType:TClass;
begin
[[[
  <<<result>>> = <<<self>>>->INTERNAL_FIELD_VMT;
]]]
end;

class function TObject.ClassName:shortstring;
begin
end;

class function TObject.ClassNameIs(const Name:ansistring):boolean;
begin
end;

class function TObject.ClassParent:TClass;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)((void*)<<<self>>>))->vmtParent;
]]]
end;

class function TObject.ClassInfo:pointer;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)((void*)<<<self>>>))->vmtTypeInfo;
]]]
end;

class function TObject.InstanceSize:longint;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)((void*)<<<self>>>))->vmtInstanceSize;
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
    VMT = VMT->vmtParent;
  }
]]]
end;

class function TObject.MethodAddress(const Name:shortstring):pointer;
begin
end;

class function TObject.MethodName(Address:pointer):shortstring;
begin
end;

function TObject.FieldAddress(const Name:shortstring):pointer;
begin
end;

function TObject.GetInterface(const IID:TGUID;out Obj):boolean;
begin
 result:=false;
end;

class function TObject.GetInterfaceEntry(const IID:TGUID):PInterfaceEntry;
begin
 result:=nil;
end;

class function TObject.GetInterfaceTable:PInterfaceTable;
begin
[[[
  <<<result>>> = ((pasClassVirtualMethodTable*)((void*)<<<self>>>))->vmtIntfTable;
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
{$hints on}

initialization
[[[
 stringRefCount = 0;
]]]
finalization
[[[
 printf("string references left: %i\n", stringRefCount);
]]]
end.