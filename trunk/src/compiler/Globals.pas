unit Globals;
{$i Compiler.inc}

interface

const tpsVMT='VMT';
      tpsIdentifier='';
      tpsTypedConstant='';
      tpsOverload='_OVERLOAD_';
      tpsParameter='_PARAMETER_';            
      tpsDummyPointerType='_DUMMYPOINTERTYPE_';

      tfeUnit='.u';
      tfeSrc='.pas';
      tfeSrcAlternative='.p';

      UnitVersionHi=$00;
      UnitVersionLo=$00;

type PSetArray=^TSetArray;
     TSetArray=array[0..31] of byte;

     TChars=set of ansichar;

     PLocalSwitches=^TLocalSwitches;
     TLocalSwitches=packed record
      Alignment:longint;
      Assertions:boolean;
      BoolEval:boolean;
      CodePage:longint;
      DesignOnly:boolean;
      DenyPackageUnit:boolean;
      Hints:boolean;
      ImportedData:boolean;
      IOChecks:boolean;
      LongStrings:boolean;
      MinEnumSize:longint;
      OpenStrings:boolean;
      Optimization:boolean;
      OverflowChecks:boolean;
      SetPEFlags:int64;
      SetPEOptFlags:int64;
      SafeDivide:boolean;
      RangeChecks:boolean;
      RealCompatibility:boolean;
      RunOnly:boolean;
      TypeInfo:boolean;
      UnicodeStrings:boolean;
      VarStringChecks:boolean;
      Warnings:boolean;
      WeakPackageUnit:boolean;
      StackFrames:boolean;
      WriteableConst:boolean;
     end;

     TAppType=(tatCONSOLE,tatGUI);

     PGlobalSwitches=^TGlobalSwitches;
     TGlobalSwitches=record
      AppType:TAppType;
      DebugInfo:boolean;
      Description:ansistring;
      ExtendedSyntax:boolean;
      Extension:ansistring;
      ImageBase:int64;
      ImplicitBuild:boolean;
      LibPrefix:ansistring;
      LibSubfix:ansistring;
      LibVersion:ansistring;
      LocalSymbols:boolean;
      ObjExportAll:boolean;
      MinStackSize:int64;
      MaxStackSize:int64;
      ReferenceInfo:boolean;
      DefinitionInfo:boolean;
      TypedAddress:boolean;
     end;

     TTargetArchitecture=(taARM,taARMEABI,taX86,taX86WIN32,taX64,taX64WIN64,taEMSCRIPTEN);

     POptions=^TOptions;
     TOptions=record
      Dummy:byte;
      TargetCompiler:ansistring;
      TargetArchitecture:TTargetArchitecture;
     end;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
     qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

const DefaultLocalSwitches:TLocalSwitches=(
       Alignment:-1;//8;
       Assertions:true;
       BoolEval:false;
       CodePage:28591;
       DesignOnly:false;
       DenyPackageUnit:false;
       Hints:true;
       ImportedData:true;
       IOChecks:true;
       LongStrings:true;
       MinEnumSize:1;
       OpenStrings:true;
       Optimization:true;
       OverflowChecks:false;
       SetPEFlags:0;
       SetPEOptFlags:0;
       SafeDivide:false;
       RangeChecks:false;
       RealCompatibility:false;
       RunOnly:false;
       TypeInfo:true;
       UnicodeStrings:false;
       VarStringChecks:true;
       Warnings:true;
       WeakPackageUnit:false;
       StackFrames:false;
       WriteableConst:false;
      );

      DefaultGlobalSwitches:TGlobalSwitches=(
       AppType:tatGUI;
       DebugInfo:true;
       Description:'';
       ExtendedSyntax:true;
       Extension:'';
       ImageBase:$00400000;
       ImplicitBuild:true;
       LibPrefix:'';
       LibSubfix:'';
       LibVersion:'';
       LocalSymbols:true;
       ObjExportAll:false;
       MinStackSize:16384;
       MaxStackSize:1048576;
       ReferenceInfo:false;
       DefinitionInfo:true;
       TypedAddress:false;
      );

procedure FreeAndNil(var Obj);
function RoundUpToPowerOfTwo(x:ptruint):ptruint;
function RoundUpToMask(x,m:ptruint):ptruint;
function HashString(const Str:ansistring):longword;

implementation

procedure FreeAndNil(var Obj);
var ObjEx:TObject;
begin
 ObjEx:=TObject(Obj);
 if assigned(ObjEx) then begin
  pointer(Obj):=nil;
  ObjEx.Destroy;
 end;
end;

function RoundUpToPowerOfTwo(x:ptruint):ptruint;
begin
 dec(x);
 x:=x or (x shr 1);
 x:=x or (x shr 2);
 x:=x or (x shr 4);
 x:=x or (x shr 8);
 x:=x or (x shr 16);
{$ifdef cpu64}
 x:=x or (x shr 32);
{$endif}
 result:=x+1;
end;

function RoundUpToMask(x,m:ptruint):ptruint;
begin
 if (x and (m-1))<>0 then begin
  result:=(x+m) and not (m-1);
 end else begin
  result:=x;
 end;
end;

function HashString(const Str:ansistring):longword;
{$ifdef cpuarm}
var b:pansichar;
    Len,h,i:longword;
begin
 result:=2166136261;
 Len:=length(Str);
 h:=Len;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>3 do begin
   i:=longword(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,4);
   dec(Len,4);
  end;
  if Len>1 then begin
   i:=word(pointer(b)^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
   inc(b,2);
   dec(Len,2);
  end;
  if Len>0 then begin
   i:=byte(b^);
   h:=(h xor i) xor $2e63823a;
   inc(h,(h shl 15) or (h shr (32-15)));
   dec(h,(h shl 9) or (h shr (32-9)));
   inc(h,(h shl 4) or (h shr (32-4)));
   dec(h,(h shl 1) or (h shr (32-1)));
   h:=h xor (h shl 2) or (h shr (32-2));
   result:=result xor i;
   inc(result,(result shl 1)+(result shl 4)+(result shl 7)+(result shl 8)+(result shl 24));
  end;
 end;
 result:=result xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$else}
const m=longword($57559429);
      n=longword($5052acdb);
var b:pansichar;
    h,k,Len:longword;
    p:{$ifdef fpc}qword{$else}int64{$endif};
begin
 Len:=length(Str);
 h:=Len;
 k:=h+n+1;
 if Len>0 then begin
  b:=pansichar(Str);
  while Len>7 do begin
   begin
    p:=longword(pointer(b)^)*qword(n);
    h:=h xor longword(p and $ffffffff);
    k:=k xor longword(p shr 32);
    inc(b,4);
   end;
   begin
    p:=longword(pointer(b)^)*qword(m);
    k:=k xor longword(p and $ffffffff);
    h:=h xor longword(p shr 32);
    inc(b,4);
   end;
   dec(Len,8);
  end;
  if Len>3 then begin
   p:=longword(pointer(b)^)*qword(n);
   h:=h xor longword(p and $ffffffff);
   k:=k xor longword(p shr 32);
   inc(b,4);
   dec(Len,4);
  end;
  if Len>0 then begin
   if Len>1 then begin
    p:=word(pointer(b)^);
    inc(b,2);
    dec(Len,2);
   end else begin
    p:=0;
   end;
   if Len>0 then begin
    p:=p or (byte(b^) shl 16);
   end;
   p:=p*qword(m);
   k:=k xor longword(p and $ffffffff);
   h:=h xor longword(p shr 32);
  end;
 end;
 begin
  p:=(h xor (k+n))*qword(n);
  h:=h xor longword(p and $ffffffff);
  k:=k xor longword(p shr 32);
 end;
 result:=k xor h;
 if result=0 then begin
  result:=$ffffffff;
 end;
end;
{$endif}

end.
