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
pasShortstring1 pasToShortstring1(pasLongstring str);
pasShortstring2 pasToShortstring2(pasLongstring str);
pasShortstring3 pasToShortstring3(pasLongstring str);
pasShortstring4 pasToShortstring4(pasLongstring str);
pasShortstring5 pasToShortstring5(pasLongstring str);
pasShortstring6 pasToShortstring6(pasLongstring str);
pasShortstring7 pasToShortstring7(pasLongstring str);
pasShortstring8 pasToShortstring8(pasLongstring str);
pasShortstring9 pasToShortstring9(pasLongstring str);
pasShortstring10 pasToShortstring10(pasLongstring str);
pasShortstring11 pasToShortstring11(pasLongstring str);
pasShortstring12 pasToShortstring12(pasLongstring str);
pasShortstring13 pasToShortstring13(pasLongstring str);
pasShortstring14 pasToShortstring14(pasLongstring str);
pasShortstring15 pasToShortstring15(pasLongstring str);
pasShortstring16 pasToShortstring16(pasLongstring str);
pasShortstring17 pasToShortstring17(pasLongstring str);
pasShortstring18 pasToShortstring18(pasLongstring str);
pasShortstring19 pasToShortstring19(pasLongstring str);
pasShortstring20 pasToShortstring20(pasLongstring str);
pasShortstring21 pasToShortstring21(pasLongstring str);
pasShortstring22 pasToShortstring22(pasLongstring str);
pasShortstring23 pasToShortstring23(pasLongstring str);
pasShortstring24 pasToShortstring24(pasLongstring str);
pasShortstring25 pasToShortstring25(pasLongstring str);
pasShortstring26 pasToShortstring26(pasLongstring str);
pasShortstring27 pasToShortstring27(pasLongstring str);
pasShortstring28 pasToShortstring28(pasLongstring str);
pasShortstring29 pasToShortstring29(pasLongstring str);
pasShortstring30 pasToShortstring30(pasLongstring str);
pasShortstring31 pasToShortstring31(pasLongstring str);
pasShortstring32 pasToShortstring32(pasLongstring str);
pasShortstring33 pasToShortstring33(pasLongstring str);
pasShortstring34 pasToShortstring34(pasLongstring str);
pasShortstring35 pasToShortstring35(pasLongstring str);
pasShortstring36 pasToShortstring36(pasLongstring str);
pasShortstring37 pasToShortstring37(pasLongstring str);
pasShortstring38 pasToShortstring38(pasLongstring str);
pasShortstring39 pasToShortstring39(pasLongstring str);
pasShortstring40 pasToShortstring40(pasLongstring str);
pasShortstring41 pasToShortstring41(pasLongstring str);
pasShortstring42 pasToShortstring42(pasLongstring str);
pasShortstring43 pasToShortstring43(pasLongstring str);
pasShortstring44 pasToShortstring44(pasLongstring str);
pasShortstring45 pasToShortstring45(pasLongstring str);
pasShortstring46 pasToShortstring46(pasLongstring str);
pasShortstring47 pasToShortstring47(pasLongstring str);
pasShortstring48 pasToShortstring48(pasLongstring str);
pasShortstring49 pasToShortstring49(pasLongstring str);
pasShortstring50 pasToShortstring50(pasLongstring str);
pasShortstring51 pasToShortstring51(pasLongstring str);
pasShortstring52 pasToShortstring52(pasLongstring str);
pasShortstring53 pasToShortstring53(pasLongstring str);
pasShortstring54 pasToShortstring54(pasLongstring str);
pasShortstring55 pasToShortstring55(pasLongstring str);
pasShortstring56 pasToShortstring56(pasLongstring str);
pasShortstring57 pasToShortstring57(pasLongstring str);
pasShortstring58 pasToShortstring58(pasLongstring str);
pasShortstring59 pasToShortstring59(pasLongstring str);
pasShortstring60 pasToShortstring60(pasLongstring str);
pasShortstring61 pasToShortstring61(pasLongstring str);
pasShortstring62 pasToShortstring62(pasLongstring str);
pasShortstring63 pasToShortstring63(pasLongstring str);
pasShortstring64 pasToShortstring64(pasLongstring str);
pasShortstring65 pasToShortstring65(pasLongstring str);
pasShortstring66 pasToShortstring66(pasLongstring str);
pasShortstring67 pasToShortstring67(pasLongstring str);
pasShortstring68 pasToShortstring68(pasLongstring str);
pasShortstring69 pasToShortstring69(pasLongstring str);
pasShortstring70 pasToShortstring70(pasLongstring str);
pasShortstring71 pasToShortstring71(pasLongstring str);
pasShortstring72 pasToShortstring72(pasLongstring str);
pasShortstring73 pasToShortstring73(pasLongstring str);
pasShortstring74 pasToShortstring74(pasLongstring str);
pasShortstring75 pasToShortstring75(pasLongstring str);
pasShortstring76 pasToShortstring76(pasLongstring str);
pasShortstring77 pasToShortstring77(pasLongstring str);
pasShortstring78 pasToShortstring78(pasLongstring str);
pasShortstring79 pasToShortstring79(pasLongstring str);
pasShortstring80 pasToShortstring80(pasLongstring str);
pasShortstring81 pasToShortstring81(pasLongstring str);
pasShortstring82 pasToShortstring82(pasLongstring str);
pasShortstring83 pasToShortstring83(pasLongstring str);
pasShortstring84 pasToShortstring84(pasLongstring str);
pasShortstring85 pasToShortstring85(pasLongstring str);
pasShortstring86 pasToShortstring86(pasLongstring str);
pasShortstring87 pasToShortstring87(pasLongstring str);
pasShortstring88 pasToShortstring88(pasLongstring str);
pasShortstring89 pasToShortstring89(pasLongstring str);
pasShortstring90 pasToShortstring90(pasLongstring str);
pasShortstring91 pasToShortstring91(pasLongstring str);
pasShortstring92 pasToShortstring92(pasLongstring str);
pasShortstring93 pasToShortstring93(pasLongstring str);
pasShortstring94 pasToShortstring94(pasLongstring str);
pasShortstring95 pasToShortstring95(pasLongstring str);
pasShortstring96 pasToShortstring96(pasLongstring str);
pasShortstring97 pasToShortstring97(pasLongstring str);
pasShortstring98 pasToShortstring98(pasLongstring str);
pasShortstring99 pasToShortstring99(pasLongstring str);
pasShortstring100 pasToShortstring100(pasLongstring str);
pasShortstring101 pasToShortstring101(pasLongstring str);
pasShortstring102 pasToShortstring102(pasLongstring str);
pasShortstring103 pasToShortstring103(pasLongstring str);
pasShortstring104 pasToShortstring104(pasLongstring str);
pasShortstring105 pasToShortstring105(pasLongstring str);
pasShortstring106 pasToShortstring106(pasLongstring str);
pasShortstring107 pasToShortstring107(pasLongstring str);
pasShortstring108 pasToShortstring108(pasLongstring str);
pasShortstring109 pasToShortstring109(pasLongstring str);
pasShortstring110 pasToShortstring110(pasLongstring str);
pasShortstring111 pasToShortstring111(pasLongstring str);
pasShortstring112 pasToShortstring112(pasLongstring str);
pasShortstring113 pasToShortstring113(pasLongstring str);
pasShortstring114 pasToShortstring114(pasLongstring str);
pasShortstring115 pasToShortstring115(pasLongstring str);
pasShortstring116 pasToShortstring116(pasLongstring str);
pasShortstring117 pasToShortstring117(pasLongstring str);
pasShortstring118 pasToShortstring118(pasLongstring str);
pasShortstring119 pasToShortstring119(pasLongstring str);
pasShortstring120 pasToShortstring120(pasLongstring str);
pasShortstring121 pasToShortstring121(pasLongstring str);
pasShortstring122 pasToShortstring122(pasLongstring str);
pasShortstring123 pasToShortstring123(pasLongstring str);
pasShortstring124 pasToShortstring124(pasLongstring str);
pasShortstring125 pasToShortstring125(pasLongstring str);
pasShortstring126 pasToShortstring126(pasLongstring str);
pasShortstring127 pasToShortstring127(pasLongstring str);
pasShortstring128 pasToShortstring128(pasLongstring str);
pasShortstring129 pasToShortstring129(pasLongstring str);
pasShortstring130 pasToShortstring130(pasLongstring str);
pasShortstring131 pasToShortstring131(pasLongstring str);
pasShortstring132 pasToShortstring132(pasLongstring str);
pasShortstring133 pasToShortstring133(pasLongstring str);
pasShortstring134 pasToShortstring134(pasLongstring str);
pasShortstring135 pasToShortstring135(pasLongstring str);
pasShortstring136 pasToShortstring136(pasLongstring str);
pasShortstring137 pasToShortstring137(pasLongstring str);
pasShortstring138 pasToShortstring138(pasLongstring str);
pasShortstring139 pasToShortstring139(pasLongstring str);
pasShortstring140 pasToShortstring140(pasLongstring str);
pasShortstring141 pasToShortstring141(pasLongstring str);
pasShortstring142 pasToShortstring142(pasLongstring str);
pasShortstring143 pasToShortstring143(pasLongstring str);
pasShortstring144 pasToShortstring144(pasLongstring str);
pasShortstring145 pasToShortstring145(pasLongstring str);
pasShortstring146 pasToShortstring146(pasLongstring str);
pasShortstring147 pasToShortstring147(pasLongstring str);
pasShortstring148 pasToShortstring148(pasLongstring str);
pasShortstring149 pasToShortstring149(pasLongstring str);
pasShortstring150 pasToShortstring150(pasLongstring str);
pasShortstring151 pasToShortstring151(pasLongstring str);
pasShortstring152 pasToShortstring152(pasLongstring str);
pasShortstring153 pasToShortstring153(pasLongstring str);
pasShortstring154 pasToShortstring154(pasLongstring str);
pasShortstring155 pasToShortstring155(pasLongstring str);
pasShortstring156 pasToShortstring156(pasLongstring str);
pasShortstring157 pasToShortstring157(pasLongstring str);
pasShortstring158 pasToShortstring158(pasLongstring str);
pasShortstring159 pasToShortstring159(pasLongstring str);
pasShortstring160 pasToShortstring160(pasLongstring str);
pasShortstring161 pasToShortstring161(pasLongstring str);
pasShortstring162 pasToShortstring162(pasLongstring str);
pasShortstring163 pasToShortstring163(pasLongstring str);
pasShortstring164 pasToShortstring164(pasLongstring str);
pasShortstring165 pasToShortstring165(pasLongstring str);
pasShortstring166 pasToShortstring166(pasLongstring str);
pasShortstring167 pasToShortstring167(pasLongstring str);
pasShortstring168 pasToShortstring168(pasLongstring str);
pasShortstring169 pasToShortstring169(pasLongstring str);
pasShortstring170 pasToShortstring170(pasLongstring str);
pasShortstring171 pasToShortstring171(pasLongstring str);
pasShortstring172 pasToShortstring172(pasLongstring str);
pasShortstring173 pasToShortstring173(pasLongstring str);
pasShortstring174 pasToShortstring174(pasLongstring str);
pasShortstring175 pasToShortstring175(pasLongstring str);
pasShortstring176 pasToShortstring176(pasLongstring str);
pasShortstring177 pasToShortstring177(pasLongstring str);
pasShortstring178 pasToShortstring178(pasLongstring str);
pasShortstring179 pasToShortstring179(pasLongstring str);
pasShortstring180 pasToShortstring180(pasLongstring str);
pasShortstring181 pasToShortstring181(pasLongstring str);
pasShortstring182 pasToShortstring182(pasLongstring str);
pasShortstring183 pasToShortstring183(pasLongstring str);
pasShortstring184 pasToShortstring184(pasLongstring str);
pasShortstring185 pasToShortstring185(pasLongstring str);
pasShortstring186 pasToShortstring186(pasLongstring str);
pasShortstring187 pasToShortstring187(pasLongstring str);
pasShortstring188 pasToShortstring188(pasLongstring str);
pasShortstring189 pasToShortstring189(pasLongstring str);
pasShortstring190 pasToShortstring190(pasLongstring str);
pasShortstring191 pasToShortstring191(pasLongstring str);
pasShortstring192 pasToShortstring192(pasLongstring str);
pasShortstring193 pasToShortstring193(pasLongstring str);
pasShortstring194 pasToShortstring194(pasLongstring str);
pasShortstring195 pasToShortstring195(pasLongstring str);
pasShortstring196 pasToShortstring196(pasLongstring str);
pasShortstring197 pasToShortstring197(pasLongstring str);
pasShortstring198 pasToShortstring198(pasLongstring str);
pasShortstring199 pasToShortstring199(pasLongstring str);
pasShortstring200 pasToShortstring200(pasLongstring str);
pasShortstring201 pasToShortstring201(pasLongstring str);
pasShortstring202 pasToShortstring202(pasLongstring str);
pasShortstring203 pasToShortstring203(pasLongstring str);
pasShortstring204 pasToShortstring204(pasLongstring str);
pasShortstring205 pasToShortstring205(pasLongstring str);
pasShortstring206 pasToShortstring206(pasLongstring str);
pasShortstring207 pasToShortstring207(pasLongstring str);
pasShortstring208 pasToShortstring208(pasLongstring str);
pasShortstring209 pasToShortstring209(pasLongstring str);
pasShortstring210 pasToShortstring210(pasLongstring str);
pasShortstring211 pasToShortstring211(pasLongstring str);
pasShortstring212 pasToShortstring212(pasLongstring str);
pasShortstring213 pasToShortstring213(pasLongstring str);
pasShortstring214 pasToShortstring214(pasLongstring str);
pasShortstring215 pasToShortstring215(pasLongstring str);
pasShortstring216 pasToShortstring216(pasLongstring str);
pasShortstring217 pasToShortstring217(pasLongstring str);
pasShortstring218 pasToShortstring218(pasLongstring str);
pasShortstring219 pasToShortstring219(pasLongstring str);
pasShortstring220 pasToShortstring220(pasLongstring str);
pasShortstring221 pasToShortstring221(pasLongstring str);
pasShortstring222 pasToShortstring222(pasLongstring str);
pasShortstring223 pasToShortstring223(pasLongstring str);
pasShortstring224 pasToShortstring224(pasLongstring str);
pasShortstring225 pasToShortstring225(pasLongstring str);
pasShortstring226 pasToShortstring226(pasLongstring str);
pasShortstring227 pasToShortstring227(pasLongstring str);
pasShortstring228 pasToShortstring228(pasLongstring str);
pasShortstring229 pasToShortstring229(pasLongstring str);
pasShortstring230 pasToShortstring230(pasLongstring str);
pasShortstring231 pasToShortstring231(pasLongstring str);
pasShortstring232 pasToShortstring232(pasLongstring str);
pasShortstring233 pasToShortstring233(pasLongstring str);
pasShortstring234 pasToShortstring234(pasLongstring str);
pasShortstring235 pasToShortstring235(pasLongstring str);
pasShortstring236 pasToShortstring236(pasLongstring str);
pasShortstring237 pasToShortstring237(pasLongstring str);
pasShortstring238 pasToShortstring238(pasLongstring str);
pasShortstring239 pasToShortstring239(pasLongstring str);
pasShortstring240 pasToShortstring240(pasLongstring str);
pasShortstring241 pasToShortstring241(pasLongstring str);
pasShortstring242 pasToShortstring242(pasLongstring str);
pasShortstring243 pasToShortstring243(pasLongstring str);
pasShortstring244 pasToShortstring244(pasLongstring str);
pasShortstring245 pasToShortstring245(pasLongstring str);
pasShortstring246 pasToShortstring246(pasLongstring str);
pasShortstring247 pasToShortstring247(pasLongstring str);
pasShortstring248 pasToShortstring248(pasLongstring str);
pasShortstring249 pasToShortstring249(pasLongstring str);
pasShortstring250 pasToShortstring250(pasLongstring str);
pasShortstring251 pasToShortstring251(pasLongstring str);
pasShortstring252 pasToShortstring252(pasLongstring str);
pasShortstring253 pasToShortstring253(pasLongstring str);
pasShortstring254 pasToShortstring254(pasLongstring str);
pasShortstring255 pasToShortstring255(pasLongstring str);

void* pasGetMem(size_t size);
void pasReallocMem(void** ptr,size_t size);
void pasFreeMem(void* ptr);

void pasZeroMem(void* ptr,size_t size);

void* pasObjectDMTDispatch(void** object,size_t index);

void* pasClassDMTDispatch(void* classVMT,size_t index);

//pasShortstring255 pasToShortstring(pasLongstring str);

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