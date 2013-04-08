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

typedef struct { uint8_t data[2]; } pasShortstring1;
typedef struct { uint8_t data[3]; } pasShortstring2;
typedef struct { uint8_t data[4]; } pasShortstring3;
typedef struct { uint8_t data[5]; } pasShortstring4;
typedef struct { uint8_t data[6]; } pasShortstring5;
typedef struct { uint8_t data[7]; } pasShortstring6;
typedef struct { uint8_t data[8]; } pasShortstring7;
typedef struct { uint8_t data[9]; } pasShortstring8;
typedef struct { uint8_t data[10]; } pasShortstring9;
typedef struct { uint8_t data[11]; } pasShortstring10;
typedef struct { uint8_t data[12]; } pasShortstring11;
typedef struct { uint8_t data[13]; } pasShortstring12;
typedef struct { uint8_t data[14]; } pasShortstring13;
typedef struct { uint8_t data[15]; } pasShortstring14;
typedef struct { uint8_t data[16]; } pasShortstring15;
typedef struct { uint8_t data[17]; } pasShortstring16;
typedef struct { uint8_t data[18]; } pasShortstring17;
typedef struct { uint8_t data[19]; } pasShortstring18;
typedef struct { uint8_t data[20]; } pasShortstring19;
typedef struct { uint8_t data[21]; } pasShortstring20;
typedef struct { uint8_t data[22]; } pasShortstring21;
typedef struct { uint8_t data[23]; } pasShortstring22;
typedef struct { uint8_t data[24]; } pasShortstring23;
typedef struct { uint8_t data[25]; } pasShortstring24;
typedef struct { uint8_t data[26]; } pasShortstring25;
typedef struct { uint8_t data[27]; } pasShortstring26;
typedef struct { uint8_t data[28]; } pasShortstring27;
typedef struct { uint8_t data[29]; } pasShortstring28;
typedef struct { uint8_t data[30]; } pasShortstring29;
typedef struct { uint8_t data[31]; } pasShortstring30;
typedef struct { uint8_t data[32]; } pasShortstring31;
typedef struct { uint8_t data[33]; } pasShortstring32;
typedef struct { uint8_t data[34]; } pasShortstring33;
typedef struct { uint8_t data[35]; } pasShortstring34;
typedef struct { uint8_t data[36]; } pasShortstring35;
typedef struct { uint8_t data[37]; } pasShortstring36;
typedef struct { uint8_t data[38]; } pasShortstring37;
typedef struct { uint8_t data[39]; } pasShortstring38;
typedef struct { uint8_t data[40]; } pasShortstring39;
typedef struct { uint8_t data[41]; } pasShortstring40;
typedef struct { uint8_t data[42]; } pasShortstring41;
typedef struct { uint8_t data[43]; } pasShortstring42;
typedef struct { uint8_t data[44]; } pasShortstring43;
typedef struct { uint8_t data[45]; } pasShortstring44;
typedef struct { uint8_t data[46]; } pasShortstring45;
typedef struct { uint8_t data[47]; } pasShortstring46;
typedef struct { uint8_t data[48]; } pasShortstring47;
typedef struct { uint8_t data[49]; } pasShortstring48;
typedef struct { uint8_t data[50]; } pasShortstring49;
typedef struct { uint8_t data[51]; } pasShortstring50;
typedef struct { uint8_t data[52]; } pasShortstring51;
typedef struct { uint8_t data[53]; } pasShortstring52;
typedef struct { uint8_t data[54]; } pasShortstring53;
typedef struct { uint8_t data[55]; } pasShortstring54;
typedef struct { uint8_t data[56]; } pasShortstring55;
typedef struct { uint8_t data[57]; } pasShortstring56;
typedef struct { uint8_t data[58]; } pasShortstring57;
typedef struct { uint8_t data[59]; } pasShortstring58;
typedef struct { uint8_t data[60]; } pasShortstring59;
typedef struct { uint8_t data[61]; } pasShortstring60;
typedef struct { uint8_t data[62]; } pasShortstring61;
typedef struct { uint8_t data[63]; } pasShortstring62;
typedef struct { uint8_t data[64]; } pasShortstring63;
typedef struct { uint8_t data[65]; } pasShortstring64;
typedef struct { uint8_t data[66]; } pasShortstring65;
typedef struct { uint8_t data[67]; } pasShortstring66;
typedef struct { uint8_t data[68]; } pasShortstring67;
typedef struct { uint8_t data[69]; } pasShortstring68;
typedef struct { uint8_t data[70]; } pasShortstring69;
typedef struct { uint8_t data[71]; } pasShortstring70;
typedef struct { uint8_t data[72]; } pasShortstring71;
typedef struct { uint8_t data[73]; } pasShortstring72;
typedef struct { uint8_t data[74]; } pasShortstring73;
typedef struct { uint8_t data[75]; } pasShortstring74;
typedef struct { uint8_t data[76]; } pasShortstring75;
typedef struct { uint8_t data[77]; } pasShortstring76;
typedef struct { uint8_t data[78]; } pasShortstring77;
typedef struct { uint8_t data[79]; } pasShortstring78;
typedef struct { uint8_t data[80]; } pasShortstring79;
typedef struct { uint8_t data[81]; } pasShortstring80;
typedef struct { uint8_t data[82]; } pasShortstring81;
typedef struct { uint8_t data[83]; } pasShortstring82;
typedef struct { uint8_t data[84]; } pasShortstring83;
typedef struct { uint8_t data[85]; } pasShortstring84;
typedef struct { uint8_t data[86]; } pasShortstring85;
typedef struct { uint8_t data[87]; } pasShortstring86;
typedef struct { uint8_t data[88]; } pasShortstring87;
typedef struct { uint8_t data[89]; } pasShortstring88;
typedef struct { uint8_t data[90]; } pasShortstring89;
typedef struct { uint8_t data[91]; } pasShortstring90;
typedef struct { uint8_t data[92]; } pasShortstring91;
typedef struct { uint8_t data[93]; } pasShortstring92;
typedef struct { uint8_t data[94]; } pasShortstring93;
typedef struct { uint8_t data[95]; } pasShortstring94;
typedef struct { uint8_t data[96]; } pasShortstring95;
typedef struct { uint8_t data[97]; } pasShortstring96;
typedef struct { uint8_t data[98]; } pasShortstring97;
typedef struct { uint8_t data[99]; } pasShortstring98;
typedef struct { uint8_t data[100]; } pasShortstring99;
typedef struct { uint8_t data[101]; } pasShortstring100;
typedef struct { uint8_t data[102]; } pasShortstring101;
typedef struct { uint8_t data[103]; } pasShortstring102;
typedef struct { uint8_t data[104]; } pasShortstring103;
typedef struct { uint8_t data[105]; } pasShortstring104;
typedef struct { uint8_t data[106]; } pasShortstring105;
typedef struct { uint8_t data[107]; } pasShortstring106;
typedef struct { uint8_t data[108]; } pasShortstring107;
typedef struct { uint8_t data[109]; } pasShortstring108;
typedef struct { uint8_t data[110]; } pasShortstring109;
typedef struct { uint8_t data[111]; } pasShortstring110;
typedef struct { uint8_t data[112]; } pasShortstring111;
typedef struct { uint8_t data[113]; } pasShortstring112;
typedef struct { uint8_t data[114]; } pasShortstring113;
typedef struct { uint8_t data[115]; } pasShortstring114;
typedef struct { uint8_t data[116]; } pasShortstring115;
typedef struct { uint8_t data[117]; } pasShortstring116;
typedef struct { uint8_t data[118]; } pasShortstring117;
typedef struct { uint8_t data[119]; } pasShortstring118;
typedef struct { uint8_t data[120]; } pasShortstring119;
typedef struct { uint8_t data[121]; } pasShortstring120;
typedef struct { uint8_t data[122]; } pasShortstring121;
typedef struct { uint8_t data[123]; } pasShortstring122;
typedef struct { uint8_t data[124]; } pasShortstring123;
typedef struct { uint8_t data[125]; } pasShortstring124;
typedef struct { uint8_t data[126]; } pasShortstring125;
typedef struct { uint8_t data[127]; } pasShortstring126;
typedef struct { uint8_t data[128]; } pasShortstring127;
typedef struct { uint8_t data[129]; } pasShortstring128;
typedef struct { uint8_t data[130]; } pasShortstring129;
typedef struct { uint8_t data[131]; } pasShortstring130;
typedef struct { uint8_t data[132]; } pasShortstring131;
typedef struct { uint8_t data[133]; } pasShortstring132;
typedef struct { uint8_t data[134]; } pasShortstring133;
typedef struct { uint8_t data[135]; } pasShortstring134;
typedef struct { uint8_t data[136]; } pasShortstring135;
typedef struct { uint8_t data[137]; } pasShortstring136;
typedef struct { uint8_t data[138]; } pasShortstring137;
typedef struct { uint8_t data[139]; } pasShortstring138;
typedef struct { uint8_t data[140]; } pasShortstring139;
typedef struct { uint8_t data[141]; } pasShortstring140;
typedef struct { uint8_t data[142]; } pasShortstring141;
typedef struct { uint8_t data[143]; } pasShortstring142;
typedef struct { uint8_t data[144]; } pasShortstring143;
typedef struct { uint8_t data[145]; } pasShortstring144;
typedef struct { uint8_t data[146]; } pasShortstring145;
typedef struct { uint8_t data[147]; } pasShortstring146;
typedef struct { uint8_t data[148]; } pasShortstring147;
typedef struct { uint8_t data[149]; } pasShortstring148;
typedef struct { uint8_t data[150]; } pasShortstring149;
typedef struct { uint8_t data[151]; } pasShortstring150;
typedef struct { uint8_t data[152]; } pasShortstring151;
typedef struct { uint8_t data[153]; } pasShortstring152;
typedef struct { uint8_t data[154]; } pasShortstring153;
typedef struct { uint8_t data[155]; } pasShortstring154;
typedef struct { uint8_t data[156]; } pasShortstring155;
typedef struct { uint8_t data[157]; } pasShortstring156;
typedef struct { uint8_t data[158]; } pasShortstring157;
typedef struct { uint8_t data[159]; } pasShortstring158;
typedef struct { uint8_t data[160]; } pasShortstring159;
typedef struct { uint8_t data[161]; } pasShortstring160;
typedef struct { uint8_t data[162]; } pasShortstring161;
typedef struct { uint8_t data[163]; } pasShortstring162;
typedef struct { uint8_t data[164]; } pasShortstring163;
typedef struct { uint8_t data[165]; } pasShortstring164;
typedef struct { uint8_t data[166]; } pasShortstring165;
typedef struct { uint8_t data[167]; } pasShortstring166;
typedef struct { uint8_t data[168]; } pasShortstring167;
typedef struct { uint8_t data[169]; } pasShortstring168;
typedef struct { uint8_t data[170]; } pasShortstring169;
typedef struct { uint8_t data[171]; } pasShortstring170;
typedef struct { uint8_t data[172]; } pasShortstring171;
typedef struct { uint8_t data[173]; } pasShortstring172;
typedef struct { uint8_t data[174]; } pasShortstring173;
typedef struct { uint8_t data[175]; } pasShortstring174;
typedef struct { uint8_t data[176]; } pasShortstring175;
typedef struct { uint8_t data[177]; } pasShortstring176;
typedef struct { uint8_t data[178]; } pasShortstring177;
typedef struct { uint8_t data[179]; } pasShortstring178;
typedef struct { uint8_t data[180]; } pasShortstring179;
typedef struct { uint8_t data[181]; } pasShortstring180;
typedef struct { uint8_t data[182]; } pasShortstring181;
typedef struct { uint8_t data[183]; } pasShortstring182;
typedef struct { uint8_t data[184]; } pasShortstring183;
typedef struct { uint8_t data[185]; } pasShortstring184;
typedef struct { uint8_t data[186]; } pasShortstring185;
typedef struct { uint8_t data[187]; } pasShortstring186;
typedef struct { uint8_t data[188]; } pasShortstring187;
typedef struct { uint8_t data[189]; } pasShortstring188;
typedef struct { uint8_t data[190]; } pasShortstring189;
typedef struct { uint8_t data[191]; } pasShortstring190;
typedef struct { uint8_t data[192]; } pasShortstring191;
typedef struct { uint8_t data[193]; } pasShortstring192;
typedef struct { uint8_t data[194]; } pasShortstring193;
typedef struct { uint8_t data[195]; } pasShortstring194;
typedef struct { uint8_t data[196]; } pasShortstring195;
typedef struct { uint8_t data[197]; } pasShortstring196;
typedef struct { uint8_t data[198]; } pasShortstring197;
typedef struct { uint8_t data[199]; } pasShortstring198;
typedef struct { uint8_t data[200]; } pasShortstring199;
typedef struct { uint8_t data[201]; } pasShortstring200;
typedef struct { uint8_t data[202]; } pasShortstring201;
typedef struct { uint8_t data[203]; } pasShortstring202;
typedef struct { uint8_t data[204]; } pasShortstring203;
typedef struct { uint8_t data[205]; } pasShortstring204;
typedef struct { uint8_t data[206]; } pasShortstring205;
typedef struct { uint8_t data[207]; } pasShortstring206;
typedef struct { uint8_t data[208]; } pasShortstring207;
typedef struct { uint8_t data[209]; } pasShortstring208;
typedef struct { uint8_t data[210]; } pasShortstring209;
typedef struct { uint8_t data[211]; } pasShortstring210;
typedef struct { uint8_t data[212]; } pasShortstring211;
typedef struct { uint8_t data[213]; } pasShortstring212;
typedef struct { uint8_t data[214]; } pasShortstring213;
typedef struct { uint8_t data[215]; } pasShortstring214;
typedef struct { uint8_t data[216]; } pasShortstring215;
typedef struct { uint8_t data[217]; } pasShortstring216;
typedef struct { uint8_t data[218]; } pasShortstring217;
typedef struct { uint8_t data[219]; } pasShortstring218;
typedef struct { uint8_t data[220]; } pasShortstring219;
typedef struct { uint8_t data[221]; } pasShortstring220;
typedef struct { uint8_t data[222]; } pasShortstring221;
typedef struct { uint8_t data[223]; } pasShortstring222;
typedef struct { uint8_t data[224]; } pasShortstring223;
typedef struct { uint8_t data[225]; } pasShortstring224;
typedef struct { uint8_t data[226]; } pasShortstring225;
typedef struct { uint8_t data[227]; } pasShortstring226;
typedef struct { uint8_t data[228]; } pasShortstring227;
typedef struct { uint8_t data[229]; } pasShortstring228;
typedef struct { uint8_t data[230]; } pasShortstring229;
typedef struct { uint8_t data[231]; } pasShortstring230;
typedef struct { uint8_t data[232]; } pasShortstring231;
typedef struct { uint8_t data[233]; } pasShortstring232;
typedef struct { uint8_t data[234]; } pasShortstring233;
typedef struct { uint8_t data[235]; } pasShortstring234;
typedef struct { uint8_t data[236]; } pasShortstring235;
typedef struct { uint8_t data[237]; } pasShortstring236;
typedef struct { uint8_t data[238]; } pasShortstring237;
typedef struct { uint8_t data[239]; } pasShortstring238;
typedef struct { uint8_t data[240]; } pasShortstring239;
typedef struct { uint8_t data[241]; } pasShortstring240;
typedef struct { uint8_t data[242]; } pasShortstring241;
typedef struct { uint8_t data[243]; } pasShortstring242;
typedef struct { uint8_t data[244]; } pasShortstring243;
typedef struct { uint8_t data[245]; } pasShortstring244;
typedef struct { uint8_t data[246]; } pasShortstring245;
typedef struct { uint8_t data[247]; } pasShortstring246;
typedef struct { uint8_t data[248]; } pasShortstring247;
typedef struct { uint8_t data[249]; } pasShortstring248;
typedef struct { uint8_t data[250]; } pasShortstring249;
typedef struct { uint8_t data[251]; } pasShortstring250;
typedef struct { uint8_t data[252]; } pasShortstring251;
typedef struct { uint8_t data[253]; } pasShortstring252;
typedef struct { uint8_t data[254]; } pasShortstring253;
typedef struct { uint8_t data[255]; } pasShortstring254;
typedef struct { uint8_t data[256]; } pasShortstring255;

void* pasGetMem(size_t size);
void pasReallocMem(void** ptr,size_t size);
void pasFreeMem(void* ptr);

void pasZeroMem(void* ptr,size_t size);

void* pasObjectDMTDispatch(void** object,size_t index);

void* pasClassDMTDispatch(void* classVMT,size_t index);

pasShortstring255 pasToShortstring(pasLongstring str);

void CheckRefLongstring(pasLongstring str);
pasLongstring AddLongstring(pasLongstring left, pasLongstring right);
void UniqueLongstring(pasLongstring *target);
pasLongstring CreateLongstring(uint32_t codePage, uint32_t elementSize, uint32_t length, void* data);
void DecRefLongstring(pasLongstring *str);
void IncRefLongstring(pasLongstring *str);
void FreeLongstring(pasLongstring *str);
uint32_t LengthLongstring(pasLongstring str);
void AssignLongstring(pasLongstring *target, pasLongstring newStr);
void AssignShortstring(uint8_t* target, uint32_t maxLength, pasLongstring strInput);
void UniqueLongstring(pasLongstring *target);
pasLongstring ConvertLongstring(uint32_t codePage, uint32_t elementSize, pasLongstring strInput);

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

uint32_t stringRefCount;

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

pasShortstring255 pasToShortstring(pasLongstring str) {
    pasShortstring255 result;
    uint32_t length,i,v;
    LongstringRefHeader* header;

    ((uint8_t*)(&result))[0] = 0;
    if(NULL == str)
        return result;

    header = (LongstringRefHeader*)((uint32_t)(str) - LongstringRefHeaderSize);

    if(header->length>255)
        length = 255;
    else
        length = header->length;

    ((uint8_t*)(&result))[0] = length;

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
        ((uint8_t*)(&result))[i+1] = v;
    }
//    if(length<255)
//        ((uint8_t*)(result))[length-1] = 0;
    CheckRefLongstring(str);
    return result;
}

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