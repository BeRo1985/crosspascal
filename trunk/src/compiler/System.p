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

typedef void* pasLongstring;

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

void* pasObjectDMTDispatch(void** object,size_t index);

void* pasClassDMTDispatch(void* classVMT,size_t index);

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

void* pasGetMem(size_t size){
  return malloc(size);
}

void pasReallocMem(void** ptr,size_t size){
  *ptr = realloc(*ptr, size);
}

void pasFreeMem(void* ptr){
  free(ptr);
}

void pasZeroMem(void* ptr,size_t size){
  memset(ptr, size, 0);
}

void* pasObjectDMTDispatch(void** object,size_t index){
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

void* pasClassDMTDispatch(void* classVMT,size_t index){
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

typedef struct {
    uint32_t codePage;     // 0x0000 - 0xffff = ansistring codepage
    uint32_t elementSize;  // 1 = ansistring, 2 = widestring and unicodestring, 4 = hugestring
	uint32_t refCount;     // 0xffffffff = widestring which is not reference counted
	uint32_t length;
} LongstringRefHeader;

#define LongstringRefHeaderSize sizeof(LongstringRefHeader)

typedef void* pasLongstring;

void CheckRefLongstring(pasLongstring str) {
	LongstringRefHeader* header;
	
	if(str == NULL)
		return;
	header = (void*)((uint32_t)(str) - LongstringRefHeaderSize);
	if(0xffffffff == header->refCount) 
		return;
   	if((header->refCount) == 0)
	 	free(&header);
}

pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data) {
	LongstringRefHeader* header;
	char* ref;
	uint32_t* zero;
	
	if(length == 0)
		return NULL;

	header = (LongstringRefHeader*)malloc(LongstringRefHeaderSize + (sizeof(uint32_t) + (length * elementSize)));
	ref = ((char*)header) + LongstringRefHeaderSize;
	header->codePage=codePage;
	header->elementSize=elementSize;
	header->refCount=1;
	header->length=length;
	if(NULL!=data)
		memcpy(ref, data, length * elementSize);
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
	
	if(--(header->refCount) == 0) 
	 	free(header);
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

pasLongstring AddLongstring(pasLongstring left, pasLongstring right) {
    uint32_t a,b;
    LongstringRefHeader* headerA;
    LongstringRefHeader* headerB;
    char* temp;
    uint32_t elementSize;
    uint32_t v;
    pasLongstring result;

    a = LengthLongstring(left);
    b = LengthLongstring(right);
    if(a + b == 0)
    	return NULL;
    if(b == 0)
    	return left;
    if(a == 0)
    	return right;

    headerA = (LongstringRefHeader*)((uint32_t)(left) - LongstringRefHeaderSize);
    headerB = (LongstringRefHeader*)((uint32_t)(right) - LongstringRefHeaderSize);

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

begin
end.