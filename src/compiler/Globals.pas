  unit Globals;
{$i Compiler.inc}

interface

uses SysUtils, StringList;

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

     TOptions=class
      CompilerDefines,
      UnitSearchPath,
      IncludeSearchPath: TStringList;

      ShowHelp,
      ShowHints,
      ShowWarnings,
      ShowVersion,
      BuildAll: Boolean;

      TargetFilename,
      TargetCompiler,
      TargetCompilerParams: ansistring;
      TargetArchitecture:TTargetArchitecture;
     protected
      GlobalSwitches: PGlobalSwitches;
      procedure ParseOption(s: ansistring);
      function FindFile(List: TStringlist; Filename: ansistring): ansistring;
     public
      constructor Create(AGlobalSwitches: PGlobalSwitches);
      destructor Destroy; override;

      function FindUnit(const Filename: ansistring): ansistring;
      function FindInclude(const Filename: ansistring): ansistring;

      procedure Load;
      procedure ParseCommandline;
      procedure ParseConfigurationFile(const AFile: ansistring);
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

//procedure FreeAndNil(var Obj);
function RoundUpToPowerOfTwo(x:ptruint):ptruint;
function RoundUpToMask(x,m:ptruint):ptruint;
function HashString(const Str:ansistring):longword;

implementation

{procedure FreeAndNil(var Obj);
var ObjEx:TObject;
begin
 ObjEx:=TObject(Obj);
 if assigned(ObjEx) then begin
  pointer(Obj):=nil;
  ObjEx.Destroy;
 end;
end;}

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

{ TOptions }

constructor TOptions.Create(AGlobalSwitches: PGlobalSwitches);
begin
 GlobalSwitches:=AGlobalSwitches;
 UnitSearchPath:=TStringList.Create;
 IncludeSearchPath:=TStringList.Create;
 CompilerDefines:=TStringList.Create;
 ShowHelp:=False;
 BuildAll:=False;
 TargetArchitecture:=taX86WIN32;
end;

destructor TOptions.Destroy;
begin
  UnitSearchPath.Free;
  IncludeSearchPath.Free;
  CompilerDefines.Free;
  inherited;
end;

function TOptions.FindFile(List: TStringlist; Filename: ansistring): ansistring;
var i: Integer;
begin
 if FileExists(Filename) then begin
  result:=Filename;
  Exit;
 end else
 for i:=0 to List.Count-1 do
  if FileExists(List[i]+Filename) then begin
   result:=List[i]+Filename;
   Exit;
  end;
  result:='';
end;

function TOptions.FindInclude(const Filename: ansistring): ansistring;
begin
 result:=FindFile(IncludeSearchPath, Filename);
end;

function TOptions.FindUnit(const Filename: ansistring): ansistring;
begin
 result:=FindFile(UnitSearchPath, Filename);
end;

procedure TOptions.Load;
var i: Integer;
begin
 ParseConfigurationFile(ChangeFileExt(Paramstr(0), '.cfg'));
 for i:=1 to ParamCount do
  if Paramstr(i)[1]<>'-' then
  begin
   TargetFilename:=Paramstr(i);
   Break;
  end;
 ParseConfigurationFile(ChangeFileExt(TargetFilename, '.cfg'));
 ParseCommandline;
end;

procedure TOptions.ParseCommandline;
var i: Integer;
begin
 for i:=1 to ParamCount do
  ParseOption(ParamStr(i));
end;

procedure TOptions.ParseConfigurationFile(const AFile: ansistring);
var f: Textfile;
    s: ansistring;
begin
 assignfile(f, AFile);
 {$i-}reset(f);{$i+}
 if ioresult=0 then
 begin
  while not eof(f) do
  begin
   readln(f, s);
   ParseOption(s);
  end;
  closefile(f);
 end;
end;

procedure TOptions.ParseOption(s: ansistring);
var Value: ansistring;

 function DelimitPath(const s: ansistring): ansistring;
 begin
  result:=s;
  if (Length(s)>0) then
  if not(s[Length(s)] in ['/', '\']) then
   result:=result+'\';
  {$IFDEF MSWINDOWS}
   result:=StringReplace(result, '/', '\', [rfReplaceAll]);
  {$ELSE}
   result:=StringReplace(result, '\', '/', [rfReplaceAll]);
  {$ENDIF}
  end;

  function Unescape(const s: ansistring): ansistring;
  begin
   if (Length(s)>0)and(s[1]='"')and(s[Length(s)]='"') then
    result:=Copy(s, 2, Length(s)-2)
   else
    result:=s;
  end;

  procedure AddToList(List: TStringList; Value: ansistring; DoUppercase: Boolean = False);
  begin
   while(Pos(';',Value)>0) do
   begin
    if DoUppercase then
     List.Add(Uppercase(Unescape(Copy(Value,1,pos(';',Value)-1))))
    else
     List.Add(DelimitPath(Unescape(Copy(Value,1,pos(';',Value)-1))));
    Delete(Value, 1, pos(';', Value));
   end;
   if Value<>'' then
    if DoUppercase then
     List.Add(Uppercase(Unescape(Value)))
    else
     List.Add(DelimitPath(Unescape(Value)));
  end;

begin
 try
  if Length(s) = 0 then
   Exit;

  if s[1]='-' then
  begin
   Value:=Copy(s,3,Length(s)-2);
   case Upcase(s[2]) of
    'U': AddToList(UnitSearchPath, Value);
    'I': AddToList(IncludeSearchPath, Value);
    '$': if Length(s)>2 then begin
     Delete(Value, 1, 1);
     case Upcase(s[3]) of
      'X': GlobalSwitches.ExtendedSyntax:=s<>'-';
      'L': GlobalSwitches.LocalSymbols:=s<>'-';
      'T': GlobalSwitches.TypedAddress:=s<>'-';
     end;
    end;
    'K': GlobalSwitches.ImageBase:=StrToInt64Def(Value, $400000);

    'B': BuildAll := Value<>'-';
    'W': ShowWarnings := Value<>'-';
    'H': ShowHints := Value<>'-';

    '-': begin
     Value:=Uppercase(Value);
     if Value='-HELP' then
      ShowHelp:=True
     else if Value='-VERSION' then
      ShowVersion:=True
     else if Pos('-COMPILER=',Value)=1 then
      TargetCompiler:=Unescape(Copy(s,11,length(s)-11))
     else if Pos('-COMPILER-PARAMETER=',Value)=1 then
      TargetCompiler:=Unescape(Copy(s,11,length(s)-11));
    end;

    'D': AddToList(CompilerDefines, Value, True);

    'C':
     if Value='C' then
      GlobalSwitches.AppType:=tatCONSOLE
     else if Value='G' then
      GlobalSwitches.AppType:=tatGUI;

    'V': GlobalSwitches.DebugInfo:=Value<>'-';

    'S': if Length(s)>2 then begin
     Delete(Value, 1, 1);
     case Upcase(s[3]) of
      'C': TargetCompiler:=Unescape(Value);
      'P': TargetCompilerParams:=Unescape(Value);
     end;
    end;
   end;
  end;
 except
  Writeln('Error parsing: ',s);
 end;
end;

end.
