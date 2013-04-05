unit UnitManager;  
{$i Compiler.inc}

interface

uses {$ifdef WIN32}Windows,{$endif}BeRoUtils,SysUtils,BeRoStream,Globals,Error,
     Symbols,Tree,PointerList,CRC32,BeRoStringHashMap,HugeString;

type TUnitFixUpType=(tuftNone,tuftSymbol,tuftType);

     PUnitFixUp=^TUnitFixUp;
     TUnitFixUp=record
      FixUpType:TUnitFixUpType;
      PointerAddress:pointer;
      TargetUnit:longint;
      TargetIndex:longint;
     end;

     PLoadedUnit=^TLoadedUnit;
     TLoadedUnit=record
      Name:ansistring;
      Symbol:PSymbol;
     end;

     TUnitLoadResult=(ulrNone,ulrNormal,ulrRecompiled);

const MaxListSize=2147483647 div SizeOf(TUnitFixUp);

type PUnitFixUpArray=^TUnitFixUpArray;
     TUnitFixUpArray=array[0..MaxListSize-1] of TUnitFixUp;

     TUnitFixUpList=class
      private
       FList:PUnitFixUpArray;
       FCount,FSize:longint;
       function GetItem(Index:longint):TUnitFixUp;
       procedure SetItem(Index:longint;Value:TUnitFixUp);
       function GetItemPointer(Index:longint):PUnitFixUp;
      public
       constructor Create;
       destructor Destroy; override;
       procedure Clear;
       function Add(Item:TUnitFixUp):longint;
       procedure Insert(Index:longint;Item:TUnitFixUp);
       procedure Delete(Index:longint);
       procedure Exchange(Index1,Index2:longint);
       procedure SetCapacity(NewCapacity:longint);
       procedure SetCount(NewCount:longint);
       property Count:longint read FCount;
       property Capacity:longint read FSize write SetCapacity;
       property Item[Index:longint]:TUnitFixUp read GetItem write SetItem; default;
       property Items[Index:longint]:TUnitFixUp read GetItem write SetItem;
       property PItems[Index:longint]:PUnitFixUp read GetItemPointer;
     end;

     TUnitManager=class
      private
       Options:POptions;
       Error:TError;
       SymbolManager:TSymbolManager;
       GlobalSwitches:PGlobalSwitches;
       ACompiler:pointer;
       LoadedUnits:array of TLoadedUnit;
       LoadedUnitStringHashMap:TBeRoStringHashMap;
       function AddUnit(Name:ansistring;Symbol:PSymbol):longint;
       procedure ProcessFixUps;
       function LoadUnit(Stream:TBeRoStream;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean;Level:longint=0):PSymbol;
       function LoadUnitFile(FileName:ansistring;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean;Level:longint=0):PSymbol;
       function RequireUnitRecompile(UnitSymbol:PSymbol;Stream:TBeRoStream;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean):boolean;
      public
       FixUpList:TUnitFixUpList;
       RebuildAll:boolean;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheCompiler:pointer;TheOptions:POptions;TheGlobalSwitches:PGlobalSwitches);
       destructor Destroy; override;
       procedure Clear;
       function SaveUnitSymbolTable(List:TSymbolList;UnitSymbol:PSymbol):boolean;
       function SaveUnit(Stream:TBeRoStream;List:TSymbolList;UnitSymbol:PSymbol;UnitName:ansistring):boolean;
       function SaveUnitFile(FileName:ansistring;List:TSymbolList;UnitSymbol:PSymbol;UnitName:ansistring):boolean;
       function Load(AUnitName,ASrcPath:ansistring;AfterImplementation,LocalDefined:boolean;ModuleSymbol:PSymbol;Level:longint=0):TUnitLoadResult;
       function FindUnit(Name:ansistring):PLoadedUnit;
     end;

implementation

uses Compiler,Parser,DebugLogUtils;

type TUnitHeaderSignature=array[1..4] of ansichar;

     TUnitHeaderVersion=packed record
      Hi,Lo:byte;
     end;

     TUnitHeader=packed record
      Signature:TUnitHeaderSignature;
      Version:TUnitHeaderVersion;
      Symbols:longword;
      Types:longword;
      Constants:longword;
      Objects:longword;
      Resources:longword;
      SymbolExports:longword;
      SymbolIDCounter:longword;
      TypeIDCounter:longword;
     end;

const UnitHeaderSignature:TUnitHeaderSignature='CUF'#0;

      UnitHeaderVersion:TUnitHeaderVersion=(Hi:UnitVersionHi;LO:UnitVersionLo);

constructor TUnitFixUpList.Create;
begin
 inherited Create;
 FCount:=0;
 FSize:=0;
 FList:=nil;
 Clear;
end;

destructor TUnitFixUpList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TUnitFixUpList.Clear;
begin
 FCount:=0;
 FSize:=0;
 ReallocMem(FList,0);
end;

procedure TUnitFixUpList.SetCapacity(NewCapacity:longint);
begin
 if (NewCapacity>=0) and (NewCapacity<MaxListSize) then begin
  ReallocMem(FList,NewCapacity*SizeOf(TUnitFixUp));
  FSize:=NewCapacity;
 end;
end;

procedure TUnitFixUpList.SetCount(NewCount:longint);
begin
 if (NewCount>=0) and (NewCount<MaxListSize) then begin
  if NewCount<FCount then begin
   FCount:=NewCount;
  end else if NewCount>FCount then begin
   if NewCount>FSize then begin
    SetCapacity(NewCount);
   end;
   if FCount<NewCount then begin
    FillChar(FList^[FCount],(NewCount-FCount)*SizeOf(TUnitFixUp),#0);
   end;
   FCount:=NewCount;
  end;
 end;
end;

function TUnitFixUpList.Add(Item:TUnitFixUp):longint;
begin
 if FCount=FSize then begin
  if FSize>64 then begin
   inc(FSize,FSize div 4);
  end else if FSize>8 then begin
   inc(FSize,16);
  end else begin
   inc(FSize,4);
  end;
  ReallocMem(FList,FSize*SizeOf(TUnitFixUp));
 end;
 FList^[FCount]:=Item;
 result:=FCount;
 inc(FCount);
end;

procedure TUnitFixUpList.Insert(Index:longint;Item:TUnitFixUp);
var I:longint;
begin
 if (Index>=0) and (Index<FCount) then begin
  SetCount(FCount+1);
  for I:=FCount-1 downto Index do begin
   FList^[I+1]:=FList^[I];
  end;
  FList^[Index]:=Item;
 end else if Index=FCount then begin
  Add(Item);
 end else if Index>FCount then begin
  SetCount(Index);
  Add(Item);
 end;
end;

procedure TUnitFixUpList.Delete(Index:longint);
var I,J,K:longint;
begin
 if (Index>=0) and (Index<FCount) then begin
  K:=FCount-1;
  J:=Index;
  for I:=J to K-1 do begin
   FList^[I]:=FList^[I+1];
  end;
  SetCount(K);
 end;
end;

procedure TUnitFixUpList.Exchange(Index1,Index2:longint);
var Templongint:TUnitFixUp;
begin
 if (Index1>=0) and (Index1<FCount) and (Index2>=0) and (Index2<FCount) then begin
  Templongint:=FList^[Index1];
  FList^[Index1]:=FList^[Index2];
  FList^[Index2]:=Templongint;
 end;
end;

function TUnitFixUpList.GetItem(Index:longint):TUnitFixUp;
begin
 if (Index>=0) and (Index<FCount) then begin
  result:=FList^[Index];
 end else begin
  FillChar(result,SizeOf(TUnitFixUp),#0);
 end;
end;

procedure TUnitFixUpList.SetItem(Index:longint;Value:TUnitFixUp);
begin
 if (Index>=0) and (Index<FCount) then begin
  FList^[Index]:=Value;
 end;
end;

function TUnitFixUpList.GetItemPointer(Index:longint):PUnitFixUp;
begin
 if (Index>=0) and (Index<FCount) then begin
  result:=@FList^[Index];
 end else begin
  result:=nil;
 end;
end;

constructor TUnitManager.Create(TheError:TError;TheSymbolManager:TSymbolManager;TheCompiler:pointer;TheOptions:POptions;TheGlobalSwitches:PGlobalSwitches);
begin
 inherited Create;
 Options:=TheOptions;
 GlobalSwitches:=TheGlobalSwitches;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 ACompiler:=TheCompiler;
 FixUpList:=TUnitFixUpList.Create;
 LoadedUnits:=nil;
 LoadedUnitStringHashMap:=TBeRoStringHashMap.Create;
 RebuildAll:=false;
end;

destructor TUnitManager.Destroy;
begin
 inherited Destroy;
 Clear;
 FixUpList.Destroy;
 SetLength(LoadedUnits,0);
 LoadedUnitStringHashMap.Destroy;
end;

procedure TUnitManager.Clear;
begin
 FixUpList.Clear;
 SetLength(LoadedUnits,0);
 LoadedUnitStringHashMap.Clear;
end;

function TUnitManager.AddUnit(Name:ansistring;Symbol:PSymbol):longint;
var StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 StringHashMapEntity:=LoadedUnitStringHashMap.Get(Name);
 if assigned(StringHashMapEntity) then begin
  result:=StringHashMapEntity^.Value;
  if assigned(Symbol) then begin
   LoadedUnits[result].Symbol:=Symbol;
  end;
 end else begin
  result:=length(LoadedUnits);
  SetLength(LoadedUnits,result+1);
  LoadedUnits[result].Name:=Name;
  LoadedUnits[result].Symbol:=Symbol;
  StringHashMapEntity:=LoadedUnitStringHashMap.Get(Name,true);
  StringHashMapEntity^.Value:=result;
 end;
end;

function TUnitManager.FindUnit(Name:ansistring):PLoadedUnit;
var StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 StringHashMapEntity:=LoadedUnitStringHashMap.Get(Name);
 if assigned(StringHashMapEntity) then begin
  result:=@LoadedUnits[StringHashMapEntity^.Value];
 end else begin
  result:=nil;
 end;
end;

procedure TUnitManager.ProcessFixUps;
var Index:longint;
    AFixUp:PUnitFixUp;
    TargetUnitSymbol,Symbol:PSymbol;
    AType:PType;
begin
 Index:=0;
 while Index<FixUpList.Count do begin
  AFixUp:=FixUpList.PItems[Index];
  if (AFixUp^.TargetUnit>=0) and (AFixUp^.TargetUnit<length(LoadedUnits)) then begin
   TargetUnitSymbol:=LoadedUnits[AFixUp^.TargetUnit].Symbol;
   if assigned(AFixUp^.PointerAddress) and assigned(TargetUnitSymbol) then begin
    case AFixUp^.FixUpType of
     tuftSymbol:begin
      Symbol:=TargetUnitSymbol^.SymbolPointerList[AFixUp^.TargetIndex];
      if assigned(Symbol) then begin
       PSymbol(AFixUp^.PointerAddress^):=Symbol;
       FixUpList.Delete(Index);
      end else begin
       inc(Index);
      end;
     end;
     tuftType:begin
      AType:=TargetUnitSymbol^.TypePointerList[AFixUp^.TargetIndex];
      if assigned(AType) then begin
       PType(AFixUp^.PointerAddress^):=AType;
       FixUpList.Delete(Index);
      end else begin
       inc(Index);
      end;
     end;
     else begin
      Error.InternalError(200605181508000);
      break;
     end;
    end;
   end else begin
    inc(Index);
   end;
  end else begin
   inc(Index);
  end;
 end;
 SymbolManager.FixSymbolListChilds;
end;

function TUnitManager.RequireUnitRecompile(UnitSymbol:PSymbol;Stream:TBeRoStream;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean):boolean;
var UnitHeader:TUnitHeader;
    Index,UsedUnitsCount:longint;
//  Flags:byte;
    Name:ansistring;
//  Checksum:longword;
begin
 result:=true;
 if assigned(UnitSymbol) and (UnitSymbol^.SymbolType=Symbols.tstUnit) and UnitSymbol^.Loaded and assigned(ModuleSymbol) then begin
  result:=false;
  exit;
 end;
 if assigned(Stream) and assigned(ModuleSymbol) and not ModuleSymbol^.IsInterfaceReady then begin
  if Stream.Read(UnitHeader,SizeOf(TUnitHeader))<>SizeOf(TUnitHeader) then begin
   exit;
  end;
  if UnitHeader.Signature<>UnitHeaderSignature then begin
   exit;
  end;
  if (UnitHeader.Version.Hi<>UnitHeaderVersion.Hi) or (UnitHeader.Version.Lo<>UnitHeaderVersion.Lo) then begin
   exit;
  end;
  Stream.ReadString;
  Stream.ReadDWord;
  UsedUnitsCount:=Stream.ReadDWord;
  for Index:=0 to UsedUnitsCount-1 do begin
   {Flags:=}Stream.ReadByte;
   Name:=Stream.ReadString;
   {Checksum:=}Stream.ReadDWord;
   if Name=ModuleSymbol^.Name then begin
    exit;
   end;
  end;
 end;
 result:=false;
end;

function TUnitManager.LoadUnit(Stream:TBeRoStream;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean;Level:longint=0):PSymbol;
type PUsedUnit=^TUsedUnit;
     TUsedUnit=record
      Flags:byte;
      Name:ansistring;
      CheckSum:longword;
      ListIndex:longint;
     end;
     TUsedUnits=array of TUsedUnit;
var UnitHeader:TUnitHeader;
    List:TSymbolList;
    Index,SubIndex,UsedUnitsCount:longint;
    UsedUnits:TUsedUnits;
    Symbol:PSymbol;
    AType:PType;
    Flags:byte;
    UnitName:ansistring;
    UnitCheckSum:longword;
    SymbolPointerList:TPointerList;
    TypePointerList:TPointerList;
    ObjectSize:longword;
    ObjectStream:TBeRoStream;
    ResourceSize:longword;
    ResourceStream:TBeRoStream;
    SymbolExport:PSymbolExport;
    c:byte;
    wc:word;
    hc:longword;

 procedure BuildList(List:TSymbolList);
 var Symbol:PSymbol;
     StringHashMapEntity:PBeRoStringHashMapEntity;
 begin
  if assigned(List) and assigned(List.First) then begin
   Symbol:=List.First;
   while assigned(Symbol^.Next) do begin
    Symbol:=Symbol^.Next;
    StringHashMapEntity:=List.StringHashMap.Get(Symbol^.Name,true);
    StringHashMapEntity^.Value:=TBeRoStringHashMapValue(pointer(Symbol));
   end;
   List.Last:=Symbol;
  end;
 end;

 procedure ReadSymbolReference(var ASymbol:PSymbol);
 var Flags:byte;
     UnitIndex,Index:longint;
     AFixUp:TUnitFixUp;
 begin
  Flags:=Stream.ReadByte;
  if (Flags and 1)<>0 then begin
   if (Flags and 2)<>0 then begin
    UnitIndex:=longint(Stream.ReadDWord);
    Index:=longint(Stream.ReadDWord);
    if (UnitIndex>=0) and (UnitIndex<length(UsedUnits)) then begin
     AFixUp.FixUpType:=tuftSymbol;
     AFixUp.PointerAddress:=@ASymbol;
     AFixUp.TargetUnit:=UsedUnits[UnitIndex].ListIndex;
     AFixUp.TargetIndex:=Index;
     FixUpList.Add(AFixUp);
    end else begin
     Error.InternalError(200605181508001);
    end;
   end else begin
    Index:=longint(Stream.ReadDWord);
    if (Index>=0) and (Index<SymbolPointerList.Count) then begin
     ASymbol:=SymbolPointerList[Index];
    end else begin
     Error.InternalError(200605181508002);
    end;
   end;
  end;
 end;

 procedure ReadTypeReference(var AType:PType);
 var Flags:byte;
     UnitIndex,Index:longint;
     AFixUp:TUnitFixUp;
 begin
  Flags:=Stream.ReadByte;
  if (Flags and 1)<>0 then begin
   if (Flags and 2)<>0 then begin
    UnitIndex:=longint(Stream.ReadDWord);
    Index:=longint(Stream.ReadDWord);
    if (UnitIndex>=0) and (UnitIndex<length(UsedUnits)) then begin
     AFixUp.FixUpType:=tuftType;
     AFixUp.PointerAddress:=@AType;
     AFixUp.TargetUnit:=UsedUnits[UnitIndex].ListIndex;
     AFixUp.TargetIndex:=Index;
     FixUpList.Add(AFixUp);
    end else begin
     Error.InternalError(200605181508003);
    end;
   end else begin
    Index:=longint(Stream.ReadDWord);
    if (Index>=0) and (Index<TypePointerList.Count) then begin
     AType:=TypePointerList[Index];
    end else begin
     Error.InternalError(200605181508004);
    end;
   end;
  end;
 end;

begin
 result:=nil;
 if Stream.Read(UnitHeader,SizeOf(TUnitHeader))<>SizeOf(TUnitHeader) then begin
  exit;
 end;
 if UnitHeader.Signature<>UnitHeaderSignature then begin
  Error.AbortCode(49,CorrectSymbolName(UnitName));
  exit;
 end;
 if (UnitHeader.Version.Hi<>UnitHeaderVersion.Hi) or (UnitHeader.Version.Lo<>UnitHeaderVersion.Lo) then begin
  Error.AbortCode(49,CorrectSymbolName(UnitName));
  exit;
 end;

 UnitName:=Stream.ReadString;
 UnitCheckSum:=Stream.ReadDWord;

 if UnitHeader.Symbols=0 then begin
  Error.AbortCode(49,CorrectSymbolName(UnitName));
  exit;
 end;

 SymbolPointerList:=TPointerList.Create;
 TypePointerList:=TPointerList.Create;

 for Index:=1 to UnitHeader.Symbols do begin
  SymbolPointerList.Add(SymbolManager.NewSymbol);
 end;
 for Index:=1 to UnitHeader.Types do begin
  TypePointerList.Add(SymbolManager.NewType);
 end;

 Symbol:=SymbolPointerList[0];
 Symbol^.SymbolType:=tstUnit;
 Symbol^.UnitKind:=tukUNIT;
 Symbol^.Name:=UnitName;
 Symbol^.SymbolPointerList:=SymbolPointerList;
 Symbol^.TypePointerList:=TypePointerList;
 Symbol^.IsUnitCompiled:=false;
 Symbol^.IsCompileInterface:=true;
 Symbol^.IsInterfaceReady:=false;
 Symbol^.UnitCheckSum:=UnitCheckSum;
 Symbol^.SymbolIDCounter:=UnitHeader.SymbolIDCounter;
 Symbol^.TypeIDCounter:=UnitHeader.TypeIDCounter;
 HashSymbol(Symbol);
 AddUnit(UnitName,Symbol);{}

{SymbolManager.CreateSymbolList(List);
 List^.First:=Symbol;
 List^.Last:=Symbol;
 SymbolManager.PushSymbolList(List);{}

 for Index:=0 to SymbolPointerList.Count-1 do begin
  Symbol:=SymbolPointerList[Index];
  Symbol^.OwnerModule:=SymbolPointerList[0];
 end;

 for Index:=0 to TypePointerList.Count-1 do begin
  AType:=TypePointerList[Index];
  AType^.OwnerModule:=SymbolPointerList[0];
 end;

 UsedUnitsCount:=Stream.ReadDWord;
 UsedUnits:=nil;
 SetLength(UsedUnits,UsedUnitsCount);
 for Index:=0 to UsedUnitsCount-1 do begin
  UsedUnits[Index].Flags:=Stream.ReadByte;
  UsedUnits[Index].Name:=Stream.ReadString;
  UsedUnits[Index].CheckSum:=Stream.ReadDWord;
  UsedUnits[Index].ListIndex:=AddUnit(UsedUnits[Index].Name,nil);
 end;

 for Index:=0 to UsedUnitsCount-1 do begin
  if (UsedUnits[Index].Flags and 1)=0 then begin
   Load(CorrectSymbolName(UsedUnits[Index].Name),'',(UsedUnits[Index].Flags and 1)<>0,true,SymbolPointerList[0],Level+1);
   Symbol:=SymbolManager.GetSymbol(UsedUnits[Index].Name);
   if assigned(Symbol) then begin
    if Symbol^.Loaded and (Symbol^.UnitCheckSum<>UsedUnits[Index].CheckSum) then begin
     Error.AbortCode(52,CorrectSymbolName(UnitName),CorrectSymbolName(UsedUnits[Index].Name));
     SetLength(UsedUnits,0);
     exit;
    end;
   end else begin
    Error.AbortCode(212,CorrectSymbolName(UsedUnits[Index].Name));
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
 end;

 for Index:=0 to SymbolPointerList.Count-1 do begin
  Symbol:=SymbolPointerList[Index];
  if Stream.Read(Symbol^.SymbolType,SizeOf(TSymbolType))<>SizeOf(TSymbolType) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if (Flags and 1)<>0 then begin
   ReadSymbolReference(Symbol^.PreviousWithEqualName);
  end;
  if (Flags and 2)<>0 then begin
   ReadSymbolReference(Symbol^.NextWithEqualName);
  end;
  if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if (Flags and 1)<>0 then begin
   ReadSymbolReference(Symbol^.Previous);
  end;
  if (Flags and 2)<>0 then begin
   ReadSymbolReference(Symbol^.Next);
  end;
  if (Flags and 4)<>0 then begin
   Symbol^.Name:=Stream.ReadString;
  end;
  if (Flags and 8)<>0 then begin
   Symbol^.OverloadedName:=Stream.ReadString;
  end;
  if (Flags and (4 or 8))<>0 then begin
   HashSymbol(Symbol);
  end;
  if (Flags and 16)<>0 then begin
   Symbol^.LibraryName:=Stream.ReadString;
  end;
  if (Flags and 32)<>0 then begin
   if Stream.Read(Symbol^.Attributes,SizeOf(TSymbolAttributes))<>SizeOf(TSymbolAttributes) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  if (Flags and 128)<>0 then begin
   ReadTypeReference(Symbol^.OwnerObjectClass);
  end;
  if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if (Flags and 1)<>0 then begin
   if Stream.Read(Symbol^.PortabilityDirectives,SizeOf(TPortabilityDirectives))<>SizeOf(TPortabilityDirectives) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  if (Flags and 2)<>0 then begin
   Symbol^.ExternalName:=Stream.ReadString;
  end;
  if (Flags and 4)<>0 then begin
   Symbol^.OriginalName:=Stream.ReadString;
  end;
  if (Flags and 8)<>0 then begin
   Symbol^.OriginalFileName:=Stream.ReadString;
  end;
  if (Flags and 16)<>0 then begin
   if Stream.Read(Symbol^.ID,SizeOf(longword))<>SizeOf(longword) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  if (Flags and 32)<>0 then begin
   if Stream.Read(Symbol^.LexicalScopeLevel,SizeOf(longint))<>SizeOf(longint) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  if (Flags and 64)<>0 then begin
   Symbol^.ProcedureName:=Stream.ReadString;
  end;
  if (Flags and 128)<>0 then begin
   Symbol^.ParameterSuffix:=Stream.ReadString;
  end;
  if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if (Flags and 1)<>0 then begin
   ReadTypeReference(Symbol^.OwnerType);
  end;
  case Symbol^.SymbolType of
   Symbols.tstConstant:begin
    ReadTypeReference(Symbol^.ConstantTypeRecord);
    if Stream.Read(Symbol^.ConstantType,SizeOf(TConstantType))<>SizeOf(TConstantType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    case Symbol^.ConstantType of
     tctOrdinal:begin
      if Stream.Read(Symbol^.IntValue,SizeOf(int64))<>SizeOf(int64) then begin
       SetLength(UsedUnits,0);
       exit;
      end;
     end;
     tctAnsiChar:begin
      if Stream.Read(c,SizeOf(byte))<>SizeOf(byte) then begin
       SymbolPointerList.Destroy;
       TypePointerList.Destroy;
       SetLength(UsedUnits,0);
       exit;
      end;
      Symbol^.CharValue:=c;
     end;
     tctWideChar:begin
      if Stream.Read(wc,SizeOf(word))<>SizeOf(word) then begin
       SymbolPointerList.Destroy;
       TypePointerList.Destroy;
       SetLength(UsedUnits,0);
       exit;
      end;
      Symbol^.CharValue:=wc;
     end;
     tctHugeChar:begin
      if Stream.Read(hc,SizeOf(longword))<>SizeOf(longword) then begin
       SymbolPointerList.Destroy;
       TypePointerList.Destroy;
       SetLength(UsedUnits,0);
       exit;
      end;
      Symbol^.CharValue:=hc;
     end;
     tctFloat:begin
      if Stream.Read(Symbol^.FloatValue,SizeOf(extended))<>SizeOf(extended) then begin
       SetLength(UsedUnits,0);
       exit;
      end;
     end;
     tctAnsiString,tctDataString:begin
      Symbol^.StringValue:=AnsiStringToHugeString(Stream.ReadString);
     end;
     tctWideString:begin
      Symbol^.StringValue:=AnsiStringToHugeString(Stream.ReadWideString);
     end;
     tctHugeString:begin
      Symbol^.StringValue:=Stream.ReadHugeString;
     end;
     tctShortString:begin
      Symbol^.ShortStringValue:=Stream.ReadString;
     end;
     tctPointer:begin
      ReadSymbolReference(Symbol^.PointerTo);
     end;
     tctSet:begin
      if Stream.Read(Symbol^.SetArray,SizeOf(TSetArray))<>SizeOf(TSetArray) then begin
       SetLength(UsedUnits,0);
       exit;
      end;
     end;
    end;
   end;
   Symbols.tstTemp,Symbols.tstType,Symbols.tstVariable:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    Symbol^.AbsoluteReference:=(Flags and 1)<>0;
    Symbol^.TypedConstant:=(Flags and 2)<>0;
    Symbol^.TypedConstantReadOnly:=(Flags and 4)<>0;
    Symbol^.TypedTrueConstant:=(Flags and 128)<>0;
    if Stream.Read(Symbol^.VariableType,SizeOf(TVariableType))<>SizeOf(TVariableType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 8)<>0 then begin
     ReadTypeReference(Symbol^.TypeDefinition);
    end;
    if (Flags and 16)<>0 then begin
     ReadSymbolReference(Symbol^.Alias);
    end;
    if (Flags and 32)<>0 then begin
     if Stream.Read(Symbol^.Offset,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 64)<>0 then begin
     if Stream.Read(Symbol^.VariableLevel,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(Symbol^.CaseOfLevel,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Read(Symbol^.CaseOfVariant,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 4)<>0 then begin
     Symbol^.VariantPrefix:=Stream.ReadString;
    end;
   end;
   tstProcedure,tstFunction:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadSymbolReference(Symbol^.NextOverloaded);
    end;
    if (Flags and 2)<>0 then begin
     Symbol^.Parameter:=TSymbolList.Create(SymbolManager);
     ReadSymbolReference(Symbol^.Parameter.First);
    end;
    if (Flags and 4)<>0 then begin
     ReadTypeReference(Symbol^.ReturnType);
    end;
    if (Flags and 8)<>0 then begin
    end;
    if (Flags and 16)<>0 then begin
     ReadSymbolReference(Symbol^.ResultSymbol);
    end;
    if (Flags and 32)<>0 then begin
     if Stream.Read(Symbol^.ProcedureLevel,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 64)<>0 then begin
     if Stream.Read(Symbol^.VirtualIndex,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 128)<>0 then begin
     if Stream.Read(Symbol^.MessageCode,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(Symbol^.ProcedureAttributes,SizeOf(TProcedureAttributes))<>SizeOf(TProcedureAttributes) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Read(Symbol^.InternalProcedure,SizeOf(TInternalProcedure))<>SizeOf(TInternalProcedure) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 8)<>0 then begin
     ReadSymbolReference(Symbol^.ForwardSymbol);
    end;
    if (Flags and 16)<>0 then begin
     ReadSymbolReference(Symbol^.MethodSymbol);
    end;
    if (Flags and 32)<>0 then begin
     ReadTypeReference(Symbol^.MethodOfType);
    end;
    if (Flags and 64)<>0 then begin
     ReadSymbolReference(Symbol^.DefaultParameterSymbol);
    end;
    if (Flags and 128)<>0 then begin
     ReadSymbolReference(Symbol^.InheritedFrom);
    end;
   end;
   Symbols.tstProperty:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadTypeReference(Symbol^.PropertyType);
    end;
    if (Flags and 2)<>0 then begin
     Symbol^.PropertyParameter:=TSymbolList.Create(SymbolManager);
     ReadSymbolReference(Symbol^.PropertyParameter.First);
    end;
    if (Flags and 4)<>0 then begin
     ReadSymbolReference(Symbol^.PropertyRead);
    end;
    if (Flags and 8)<>0 then begin
     ReadSymbolReference(Symbol^.PropertyWrite);
    end;
    if (Flags and 16)<>0 then begin
     ReadSymbolReference(Symbol^.PropertyStored);
    end;
    if (Flags and 32)<>0 then begin
     Symbol^.PropertyImplements:=TSymbolList.Create(SymbolManager);
     ReadSymbolReference(Symbol^.PropertyImplements.First);
    end;
    if (Flags and 64)<>0 then begin
     ReadSymbolReference(Symbol^.PropertyDefault);
    end;
    Symbol^.PropertyDefaultArray:=(Flags and 128)<>0;
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    Symbol^.PropertyNoDefault:=(Flags and 1)<>0;
   end;
  end;
  HashSymbol(Symbol);
 end;

 for Index:=0 to TypePointerList.Count-1 do begin
  AType:=TypePointerList[Index];
  if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
   SetLength(UsedUnits,0);
   exit;
  end;
  if (Flags and 1)<>0 then begin
   if Stream.Read(AType^.TypeDefinition,SizeOf(TTypeDefinition))<>SizeOf(TTypeDefinition) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end else begin
   AType^.TypeDefinition:=ttdEmpty;
  end;
  if (Flags and 2)<>0 then begin
   ReadTypeReference(AType^.OwnerObjectClass);
  end else begin
   AType^.OwnerObjectClass:=nil;
  end;
  AType^.Unique:=(Flags and 4)<>0;
  if (Flags and 8)<>0 then begin
   if Stream.Read(AType^.PortabilityDirectives,SizeOf(TPortabilityDirectives))<>SizeOf(TPortabilityDirectives) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  AType^.RuntimeTypeInfo:=(Flags and 16)<>0;
  if (Flags and 32)<>0 then begin
   ReadSymbolReference(AType^.Symbol);
  end;
  if (Flags and 64)<>0 then begin
   if Stream.Read(AType^.ID,SizeOf(longword))<>SizeOf(longword) then begin
    SetLength(UsedUnits,0);
    exit;
   end;
  end;
  case AType^.TypeDefinition of
   ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.Number,SizeOf(int64))<>SizeOf(int64) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Read(AType^.LowerLimit,SizeOf(int64))<>SizeOf(int64) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 4)<>0 then begin
     if Stream.Read(AType^.UpperLimit,SizeOf(int64))<>SizeOf(int64) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if Stream.Read(AType^.SubRangeType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
   end;
   ttdArray,ttdShortString:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.length,SizeOf(int64))<>SizeOf(int64) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     ReadTypeReference(AType^.Range);
    end;
    if (Flags and 4)<>0 then begin
     ReadTypeReference(AType^.Definition);
    end;
    AType^.OpenArray:=(Flags and 8)<>0;
    AType^.DynamicArray:=(Flags and 16)<>0;
    AType^.VariantArray:=(Flags and 32)<>0;
    AType^.ConstructorArray:=(Flags and 64)<>0;
    AType^.ArrayOfConst:=(Flags and 128)<>0;
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     exit;
    end;
    AType^.OpenString:=(Flags and 1)<>0;
   end;
   ttdLongString:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if Stream.Read(AType^.LongStringType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.LongStringCodePage,SizeOf(longword))<>SizeOf(longword) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end else begin
     AType^.LongStringCodePage:=$ffff;
    end;
    AType^.LongStringReferenceCounted:=(Flags and 2)<>0;
   end;
   ttdRecord,ttdObject,ttdClass,ttdInterface:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.RecordSize,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     AType^.RecordTable:=TSymbolList.Create(SymbolManager);
     ReadSymbolReference(AType^.RecordTable.First);
    end;
    if (Flags and 4)<>0 then begin
     ReadSymbolReference(AType^.ChildOf);
    end;
    if (Flags and 8)<>0 then begin
     SetLength(AType^.InterfaceChildOf,longint(Stream.ReadDWord));
     for SubIndex:=0 to length(AType^.InterfaceChildOf)-1 do begin
      ReadSymbolReference(AType^.InterfaceChildOf[SubIndex]);
     end;
    end;
    if (Flags and 16)<>0 then begin
     if Stream.Read(AType^.GUID,SizeOf(TGUID))<>SizeOf(TGUID) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    AType^.RecordPacked:=(Flags and 32)<>0;
    if (Flags and 64)<>0 then begin
     if Stream.Read(AType^.RecordAlignment,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end else begin
     AType^.RecordAlignment:=1;
    end;
    AType^.HasVirtualTable:=(Flags and 128)<>0;
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.VirtualIndexCount,SizeOf(longint))<>SizeOf(longint) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     ReadTypeReference(AType^.ClassOfType);
    end;
   end;
   ttdClassRef:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadSymbolReference(AType^.ClassOf);
    end;
   end;
   ttdFile:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if Stream.Read(AType^.FileType,SizeOf(TFileType))<>SizeOf(TFileType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadTypeReference(AType^.FileTypeRecord);
    end;
   end;
   ttdPointer:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadSymbolReference(AType^.PointerTo);
    end;
    AType^.PointerFar:=(Flags and 2)<>0;
   end;
   ttdSet:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     ReadTypeReference(AType^.SetOf);
    end;
    if Stream.Read(AType^.SetSize,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
   end;
   ttdFloat:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Read(AType^.FloatLowerLimit,SizeOf(extended))<>SizeOf(extended) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Read(AType^.FloatUpperLimit,SizeOf(extended))<>SizeOf(extended) then begin
      SetLength(UsedUnits,0);
      exit;
     end;
    end;
    if Stream.Read(AType^.FloatType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     SetLength(UsedUnits,0);
     exit;
    end;
   end;
   ttdProcedure:begin
    if Stream.Read(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     exit;
    end;
    if (Flags and 1)<>0 then begin
     AType^.Parameter:=TSymbolList.Create(SymbolManager);
     ReadSymbolReference(AType^.Parameter.First);
    end;
    if (Flags and 2)<>0 then begin
     ReadTypeReference(AType^.ReturnType);
    end;
    AType^.MethodPointer:=(Flags and 4)<>0;
    AType^.AddressOnly:=(Flags and 8)<>0;
   end;
  end;
 end;

 for Index:=0 to SymbolPointerList.Count-1 do begin
  Symbol:=SymbolPointerList[Index];
  HashSymbol(Symbol);
  case Symbol^.SymbolType of
   tstProcedure,tstFunction:begin
    if assigned(Symbol^.Parameter) then begin
     BuildList(Symbol^.Parameter);
    end;
   end;
  end;
 end;

 for Index:=0 to TypePointerList.Count-1 do begin
  AType:=TypePointerList[Index];
  case AType^.TypeDefinition of
   ttdRecord,ttdObject,ttdClass,ttdInterface:begin
    if assigned(AType^.RecordTable) then begin
     BuildList(AType^.RecordTable);
    end;
   end;
   ttdProcedure:begin
    if assigned(AType^.Parameter) then begin
     BuildList(AType^.Parameter);
    end;
   end;
  end;
 end;

 List:=TSymbolList.Create(SymbolManager);
 List.First:=SymbolPointerList[0];
 BuildList(List);
 Symbol:=List.GetSymbol(UnitName,SymbolPointerList[0]);
 if assigned(Symbol) then begin
  for Index:=1 to UnitHeader.Objects do begin
   ObjectSize:=Stream.ReadDWord;
   ObjectStream:=TBeRoStream.Create;
   ObjectStream.AppendFrom(Stream,ObjectSize);
   SymbolManager.AddUnitObjectToSymbol(Symbol,ObjectStream);
   ObjectStream.Destroy;
  end;
  for Index:=1 to UnitHeader.Resources do begin
   ResourceSize:=Stream.ReadDWord;
   ResourceStream:=TBeRoStream.Create;
   ResourceStream.AppendFrom(Stream,ResourceSize);
   SymbolManager.AddUnitResourceToSymbol(Symbol,ResourceStream);
   ResourceStream.Destroy;
  end;
  for Index:=0 to SymbolPointerList.Count-1 do begin
   PSymbol(SymbolPointerList[Index])^.OwnerModule:=Symbol;
  end;
  for Index:=0 to TypePointerList.Count-1 do begin
   PType(TypePointerList[Index])^.OwnerModule:=Symbol;
  end;
  Symbol^.SymbolPointerList:=SymbolPointerList;
  Symbol^.TypePointerList:=TypePointerList;
  Symbol^.IsUnitCompiled:=false;
  Symbol^.UnitCheckSum:=UnitCheckSum;
  Symbol^.UnitSize:=Stream.Size;
  Symbol^.SymbolList:=List;
  Symbol^.Loaded:=true;
  Symbol^.IsCompileInterface:=false;
  Symbol^.IsInterfaceReady:=true;
  Symbol^.UnitCheckSum:=UnitCheckSum;
  HashSymbol(Symbol);
  SymbolManager.PushSymbolList(List);
  AddUnit(UnitName,Symbol);
  SymbolManager.AddUnit(ModuleSymbol,Symbol,AfterImplementation,LocalDefined);
  result:=Symbol;
 end else begin
  Error.AbortCode(530,CorrectSymbolName(UnitName));
  result:=nil;
 end;

 for Index:=0 to longint(UnitHeader.SymbolExports)-1 do begin
  New(SymbolExport);
  FillChar(SymbolExport^,SizeOf(TSymbolExport),#0);
  SymbolExport^.ModuleName:=Stream.ReadString;
  SymbolExport^.SymbolName:=Stream.ReadString;
  SymbolExport^.SymbolOverloadedName:=Stream.ReadString;
  SymbolExport^.Name:=Stream.ReadString;
  if Stream.Read(SymbolExport^.Index,SizeOf(longint))<>SizeOf(longint) then begin
   SymbolExport^.ModuleName:='';
   SymbolExport^.SymbolName:='';
   SymbolExport^.SymbolOverloadedName:='';
   SymbolExport^.Name:='';
   Dispose(SymbolExport);
   SymbolPointerList.Destroy;
   TypePointerList.Destroy;
   SetLength(UsedUnits,0);
   exit;
  end;
  SymbolExport^.Next:=Symbol^.SymbolExports;
  Symbol^.SymbolExports:=SymbolExport;
 end;

 SetLength(UsedUnits,0);
end;

function TUnitManager.SaveUnitSymbolTable(List:TSymbolList;UnitSymbol:PSymbol):boolean;
var Index,SubIndex:longint;
    Symbol:PSymbol;
    AType:PType;
    AUnit:PUnit;
    Flags:byte;
    SymbolPointerList:TPointerList;
    TypePointerList:TPointerList;
    Stream:TBeRoStream;
    CRC32:TCRC32;
    c:byte;
    wc:word;
    hc:longword;

 procedure WriteSymbolReference(Symbol:PSymbol);
 var Flags:byte;
     Index:longint;
 begin
  Flags:=0;
  if assigned(Symbol) then begin
   Flags:=Flags or 1;
   if assigned(Symbol^.OwnerModule) and (Symbol^.OwnerModule<>UnitSymbol) then begin
    if Symbol^.OwnerModule^.SymbolType=Symbols.tstUnit then begin
     Flags:=Flags or 2;
    end else begin
     Error.InternalError(200605181508005);
    end;
   end;
   if (Flags and 2)<>0 then begin
    Stream.WriteByte(Flags);
    Stream.WriteLongInt(Symbol^.OwnerModule^.UnitIndex);
    Index:=Symbol^.OwnerModule^.SymbolPointerList.IndexOf(Symbol);
    if Index>=0 then begin
     Stream.WriteLongInt(Index);
    end else begin
     Error.InternalError(200605181508006);
    end;
   end else begin
    Stream.WriteByte(Flags);
    Index:=SymbolPointerList.IndexOf(Symbol);
    if Index>=0 then begin
     Stream.WriteLongInt(Index);
    end else begin
     Error.InternalError(200605181509000);
    end;
   end;
  end else begin
   Stream.WriteByte(Flags);
  end;
 end;

 procedure WriteTypeReference(AType:PType);
 var Flags:byte;
     Index:longint;
 begin
  Flags:=0;
  if assigned(AType) then begin
   Flags:=Flags or 1;
   if assigned(AType^.OwnerModule) and (AType^.OwnerModule<>UnitSymbol) then begin
    if AType^.OwnerModule^.SymbolType=Symbols.tstUnit then begin
     Flags:=Flags or 2;
    end else begin
     Error.InternalError(200605181509001);
    end;
   end;
   if (Flags and 2)<>0 then begin
    Stream.WriteByte(Flags);
    Stream.WriteLongInt(AType^.OwnerModule^.UnitIndex);
    Index:=AType^.OwnerModule^.TypePointerList.IndexOf(AType);
    if Index>=0 then begin
     Stream.WriteLongInt(Index);
    end else begin
     Error.InternalError(200605181509002);
    end;
   end else begin
    Stream.WriteByte(Flags);
    Index:=TypePointerList.IndexOf(AType);
    if Index>=0 then begin
     Stream.WriteLongInt(Index);
    end else begin
     Error.InternalError(200605181509003);
    end;
   end;
  end else begin
   Stream.WriteByte(Flags);
  end;
 end;

begin
 result:=false;

 SymbolPointerList:=UnitSymbol^.SymbolPointerList;
 TypePointerList:=UnitSymbol^.TypePointerList;
 Stream:=UnitSymbol^.SymbolTableStream;
 UnitSymbol^.CountSymbols:=SymbolPointerList.Count;
 UnitSymbol^.CountTypes:=TypePointerList.Count;
 UnitSymbol^.CountConstants:=0;
 
 Index:=0;
 AUnit:=UnitSymbol^.FirstUnit;
 while assigned(AUnit) do begin
  Symbol:=AUnit^.Symbol;
  if Symbol^.SymbolType=Symbols.tstUnit then begin
   Symbol^.UnitIndex:=Index;
   inc(Index);
  end else begin
   Error.InternalError(200605181509004);
  end;
  AUnit:=AUnit^.Next;
 end;

 for Index:=0 to SymbolPointerList.Count-1 do begin
  Symbol:=SymbolPointerList[Index];
  if Stream.Write(Symbol^.SymbolType,SizeOf(TSymbolType))<>SizeOf(TSymbolType) then begin
   exit;
  end;
  Flags:=0;
  if assigned(Symbol^.PreviousWithEqualName) then begin
   Flags:=Flags or 1;
  end;
  if assigned(Symbol^.NextWithEqualName) then begin
   Flags:=Flags or 2;
  end;
  Stream.WriteByte(Flags);
  if (Flags and 1)<>0 then begin
   WriteSymbolReference(Symbol^.PreviousWithEqualName);
  end;
  if (Flags and 2)<>0 then begin
   WriteSymbolReference(Symbol^.NextWithEqualName);
  end;
  Flags:=0;
  if assigned(Symbol^.Previous) then begin
   Flags:=Flags or 1;
  end;
  if assigned(Symbol^.Next) then begin
   Flags:=Flags or 2;
  end;
  if length(Symbol^.Name)<>0 then begin
   Flags:=Flags or 4;
  end;
  if length(Symbol^.OverloadedName)<>0 then begin
   Flags:=Flags or 8;
  end;
  if length(Symbol^.LibraryName)<>0 then begin
   Flags:=Flags or 16;
  end;
  if Symbol^.Attributes<>[] then begin
   Flags:=Flags or 32;
  end;
  if assigned(Symbol^.OwnerObjectClass) then begin
   Flags:=Flags or 128;
  end;
  Stream.WriteByte(Flags);
  if (Flags and 1)<>0 then begin
   WriteSymbolReference(Symbol^.Previous);
  end;
  if (Flags and 2)<>0 then begin
   WriteSymbolReference(Symbol^.Next);
  end;
  if (Flags and 4)<>0 then begin
   Stream.WriteDataString(Symbol^.Name);
  end;
  if (Flags and 8)<>0 then begin
   Stream.WriteDataString(Symbol^.OverloadedName);
  end;
  if (Flags and 16)<>0 then begin
   Stream.WriteDataString(Symbol^.LibraryName);
  end;
  if (Flags and 32)<>0 then begin
   if Stream.Write(Symbol^.Attributes,SizeOf(TSymbolAttributes))<>SizeOf(TSymbolAttributes) then begin
    exit;
   end;
  end;
  if (Flags and 128)<>0 then begin
   WriteTypeReference(Symbol^.OwnerObjectClass);
  end;
  Flags:=0;
  if Symbol^.PortabilityDirectives<>[] then begin
   Flags:=Flags or 1;
  end;
  if length(Symbol^.ExternalName)>0 then begin
   Flags:=Flags or 2;
  end;
  if length(Symbol^.OriginalName)>0 then begin
   Flags:=Flags or 4;
  end;
  if length(Symbol^.OriginalFileName)>0 then begin
   Flags:=Flags or 8;
  end;
  if Symbol^.ID<>0 then begin
   Flags:=Flags or 16;
  end;
  if Symbol^.LexicalScopeLevel<>0 then begin
   Flags:=Flags or 32;
  end;
  if length(Symbol^.ProcedureName)>0 then begin
   Flags:=Flags or 64;
  end;
  if length(Symbol^.ParameterSuffix)>0 then begin
   Flags:=Flags or 128;
  end;
  Stream.WriteByte(Flags);
  if (Flags and 1)<>0 then begin
   if Stream.Write(Symbol^.PortabilityDirectives,SizeOf(TPortabilityDirectives))<>SizeOf(TPortabilityDirectives) then begin
    exit;
   end;
  end;
  if (Flags and 2)<>0 then begin
   Stream.WriteDataString(Symbol^.ExternalName);
  end;
  if (Flags and 4)<>0 then begin
   Stream.WriteDataString(Symbol^.OriginalName);
  end;
  if (Flags and 8)<>0 then begin
   Stream.WriteDataString(Symbol^.OriginalFileName);
  end;
  if (Flags and 16)<>0 then begin
   if Stream.Write(Symbol^.ID,SizeOf(longword))<>SizeOf(longword) then begin
    exit;
   end;
  end;
  if (Flags and 32)<>0 then begin
   if Stream.Write(Symbol^.LexicalScopeLevel,SizeOf(longint))<>SizeOf(longint) then begin
    exit;
   end;
  end;
  if (Flags and 64)<>0 then begin
   Stream.WriteDataString(Symbol^.ProcedureName);
  end;
  if (Flags and 128)<>0 then begin
   Stream.WriteDataString(Symbol^.ParameterSuffix);
  end;
  if assigned(Symbol^.OwnerType) then begin
   Flags:=Flags or 1;
  end;
  Stream.WriteByte(Flags);
  if (Flags and 1)<>0 then begin
   WriteTypeReference(Symbol^.OwnerType);
  end;
  case Symbol^.SymbolType of
   Symbols.tstConstant:begin
    WriteTypeReference(Symbol^.ConstantTypeRecord);
    if Stream.Write(Symbol^.ConstantType,SizeOf(TConstantType))<>SizeOf(TConstantType) then begin
     exit;
    end;
    case Symbol^.ConstantType of
     tctOrdinal:begin
      if Stream.Write(Symbol^.IntValue,SizeOf(int64))<>SizeOf(int64) then begin
       exit;
      end;
     end;
     tctAnsiChar:begin
      c:=Symbol^.CharValue and $ff;
      if Stream.Write(c,SizeOf(byte))<>SizeOf(byte) then begin
       exit;
      end;
     end;
     tctWideChar:begin
      wc:=Symbol^.CharValue and $ffff;
      if Stream.Write(wc,SizeOf(word))<>SizeOf(word) then begin
       exit;
      end;
     end;
     tctHugeChar:begin
      hc:=Symbol^.CharValue and $ffffffff;
      if Stream.Write(hc,SizeOf(longword))<>SizeOf(longword) then begin
       exit;
      end;
     end;
     tctFloat:begin
      if Stream.Write(Symbol^.FloatValue,SizeOf(extended))<>SizeOf(extended) then begin
       exit;
      end;
     end;
     tctAnsiString,tctDataString:begin
      Stream.WriteDataString(HugeStringToAnsiString(Symbol^.StringValue));
     end;
     tctWideString:begin
      Stream.WriteDataWideString(HugeStringToWideString(Symbol^.StringValue));
     end;
     tctHugeString:begin
      Stream.WriteDataHugeString(Symbol^.StringValue);
     end;
     tctShortString:begin
      Stream.WriteDataString(Symbol^.ShortStringValue);
     end;
     tctPointer:begin
      WriteSymbolReference(Symbol^.PointerTo);
     end;
     tctSet:begin
      if Stream.Write(Symbol^.SetArray,SizeOf(TSetArray))<>SizeOf(TSetArray) then begin
       exit;
      end;
     end;
    end;
   end;
   Symbols.tstTemp,Symbols.tstType,Symbols.tstVariable:begin
    Flags:=0;
    if Symbol^.AbsoluteReference then begin
     Flags:=Flags or 1;
    end;
    if Symbol^.TypedConstant then begin
     Flags:=Flags or 2;
    end;
    if Symbol^.TypedConstantReadOnly then begin
     Flags:=Flags or 4;
    end;
    if assigned(Symbol^.TypeDefinition) then begin
     Flags:=Flags or 8;
    end;
    if assigned(Symbol^.Alias) then begin
     Flags:=Flags or 16;
    end;
    if Symbol^.Offset<>0 then begin
     Flags:=Flags or 32;
    end;
    if Symbol^.VariableLevel<>0 then begin
     Flags:=Flags or 64;
    end;
    if Symbol^.TypedTrueConstant then begin
     Flags:=Flags or 128;
    end;
    Stream.WriteByte(Flags);
    if Stream.Write(Symbol^.VariableType,SizeOf(TVariableType))<>SizeOf(TVariableType) then begin
     exit;
    end;
    if (Flags and 8)<>0 then begin
     WriteTypeReference(Symbol^.TypeDefinition);
    end;
    if (Flags and 16)<>0 then begin
     WriteSymbolReference(Symbol^.Alias);
    end;
    if (Flags and 32)<>0 then begin
     if Stream.Write(Symbol^.Offset,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 64)<>0 then begin
     if Stream.Write(Symbol^.VariableLevel,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    Flags:=0;
    if Symbol^.CaseOfLevel<>0 then begin
     Flags:=Flags or 1;
    end;
    if Symbol^.CaseOfVariant<>0 then begin
     Flags:=Flags or 2;
    end;
    if length(Symbol^.VariantPrefix)<>0 then begin
     Flags:=Flags or 4;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(Symbol^.CaseOfLevel,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Write(Symbol^.CaseOfVariant,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 8)<>0 then begin
     Stream.WriteDataString(Symbol^.VariantPrefix);
    end;
   end;
   tstProcedure,tstFunction:begin
    Flags:=0;
    if assigned(Symbol^.NextOverloaded) then begin
     Flags:=Flags or 1;
    end;
    if assigned(Symbol^.Parameter) then begin
     Flags:=Flags or 2;
    end;
    if assigned(Symbol^.ReturnType) then begin
     Flags:=Flags or 4;
    end;
{   if Symbol^.??? then begin
     Flags:=Flags or 8;
    end;}
    if assigned(Symbol^.ResultSymbol) then begin
     Flags:=Flags or 16;
    end;
    if Symbol^.ProcedureLevel<>0 then begin
     Flags:=Flags or 32;
    end;
    if Symbol^.VirtualIndex<>0 then begin
     Flags:=Flags or 64;
    end;
    if Symbol^.MessageCode<>0 then begin
     Flags:=Flags or 128;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     WriteSymbolReference(Symbol^.NextOverloaded);
    end;
    if (Flags and 2)<>0 then begin
     WriteSymbolReference(Symbol^.Parameter.First);
    end;
    if (Flags and 4)<>0 then begin
     WriteTypeReference(Symbol^.ReturnType);
    end;
{   if (Flags and 8)<>0 then begin
    end;}
    if (Flags and 16)<>0 then begin
     WriteSymbolReference(Symbol^.ResultSymbol);
    end;
    if (Flags and 32)<>0 then begin
     if Stream.Write(Symbol^.ProcedureLevel,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 64)<>0 then begin
     if Stream.Write(Symbol^.VirtualIndex,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 128)<>0 then begin
     if Stream.Write(Symbol^.MessageCode,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    Flags:=0;
    if Symbol^.ProcedureAttributes<>[] then begin
     Flags:=Flags or 1;
    end;
    if Symbol^.InternalProcedure<>tipNone then begin
     Flags:=Flags or 2;
    end;
    if assigned(Symbol^.ForwardSymbol) then begin
     Flags:=Flags or 8;
    end;
    if assigned(Symbol^.MethodSymbol) then begin
     Flags:=Flags or 16;
    end;
    if assigned(Symbol^.MethodOfType) then begin
     Flags:=Flags or 32;
    end;
    if assigned(Symbol^.DefaultParameterSymbol) then begin
     Flags:=Flags or 64;
    end;
    if assigned(Symbol^.InheritedFrom) then begin
     Flags:=Flags or 128;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(Symbol^.ProcedureAttributes,SizeOf(TProcedureAttributes))<>SizeOf(TProcedureAttributes) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Write(Symbol^.InternalProcedure,SizeOf(TInternalProcedure))<>SizeOf(TInternalProcedure) then begin
      exit;
     end;
    end;
    if (Flags and 8)<>0 then begin
     WriteSymbolReference(Symbol^.ForwardSymbol);
    end;
    if (Flags and 16)<>0 then begin
     WriteSymbolReference(Symbol^.MethodSymbol);
    end;
    if (Flags and 32)<>0 then begin
     WriteTypeReference(Symbol^.MethodOfType);
    end;
    if (Flags and 64)<>0 then begin
     WriteSymbolReference(Symbol^.DefaultParameterSymbol);
    end;
    if (Flags and 128)<>0 then begin
     WriteSymbolReference(Symbol^.InheritedFrom);
    end;
   end;
   Symbols.tstProperty:begin
    Flags:=0;
    if assigned(Symbol^.PropertyType) then begin
     Flags:=Flags or 1;
    end;
    if assigned(Symbol^.PropertyParameter) and assigned(Symbol^.PropertyParameter.First) then begin
     Flags:=Flags or 2;
    end;
    if assigned(Symbol^.PropertyRead) then begin
     Flags:=Flags or 4;
    end;
    if assigned(Symbol^.PropertyWrite) then begin
     Flags:=Flags or 8;
    end;
    if assigned(Symbol^.PropertyStored) then begin
     Flags:=Flags or 16;
    end;
    if assigned(Symbol^.PropertyImplements) and assigned(Symbol^.PropertyImplements.First) then begin
     Flags:=Flags or 32;
    end;
    if assigned(Symbol^.PropertyDefault) then begin
     Flags:=Flags or 64;
    end;
    if Symbol^.PropertyDefaultArray then begin
     Flags:=Flags or 128;
    end;
    if Stream.Write(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     exit;
    end;
    if (Flags and 1)<>0 then begin
     WriteTypeReference(Symbol^.PropertyType);
    end;
    if (Flags and 2)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyParameter.First);
    end;
    if (Flags and 4)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyRead);
    end;
    if (Flags and 8)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyWrite);
    end;
    if (Flags and 16)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyStored);
    end;
    if (Flags and 32)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyImplements.First);
    end;
    if (Flags and 64)<>0 then begin
     WriteSymbolReference(Symbol^.PropertyDefault);
    end;
    Flags:=0;
    if Symbol^.PropertyNoDefault then begin
     Flags:=Flags or 1;
    end;
    if Stream.Write(Flags,SizeOf(byte))<>SizeOf(byte) then begin
     exit;
    end;
   end;
  end;
 end;

 for Index:=0 to TypePointerList.Count-1 do begin
  AType:=TypePointerList[Index];
  Flags:=0;
  if AType^.TypeDefinition<>ttdEmpty then begin
   Flags:=Flags or 1;
  end;
  if assigned(AType^.OwnerObjectClass) then begin
   Flags:=Flags or 2;
  end;
  if AType^.Unique then begin
   Flags:=Flags or 4;
  end;
  if AType^.PortabilityDirectives<>[] then begin
   Flags:=Flags or 8;
  end;
  if AType^.RuntimeTypeInfo then begin
   Flags:=Flags or 16;
  end;
  if assigned(AType^.Symbol) then begin
   Flags:=Flags or 32;
  end;
  if AType^.ID<>0 then begin
   Flags:=Flags or 64;
  end;
  Stream.WriteByte(Flags);
  if (Flags and 1)<>0 then begin
   if Stream.Write(AType^.TypeDefinition,SizeOf(TTypeDefinition))<>SizeOf(TTypeDefinition) then begin
    exit;
   end;
  end;
  if (Flags and 2)<>0 then begin
   WriteTypeReference(AType^.OwnerObjectClass);
  end;
  if (Flags and 8)<>0 then begin
   if Stream.Write(AType^.PortabilityDirectives,SizeOf(TPortabilityDirectives))<>SizeOf(TPortabilityDirectives) then begin
    exit;
   end;
  end;
  if (Flags and 32)<>0 then begin
   WriteSymbolReference(AType^.Symbol);
  end;
  if (Flags and 64)<>0 then begin
   if Stream.Write(AType^.ID,SizeOf(longword))<>SizeOf(longword) then begin
    exit;
   end;
  end;
  case AType^.TypeDefinition of
   ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
    Flags:=0;
    if AType^.Number<>0 then begin
     Flags:=Flags or 1;
    end;
    if AType^.LowerLimit<>0 then begin
     Flags:=Flags or 2;
    end;
    if AType^.UpperLimit<>0 then begin
     Flags:=Flags or 4;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.Number,SizeOf(int64))<>SizeOf(int64) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Write(AType^.LowerLimit,SizeOf(int64))<>SizeOf(int64) then begin
      exit;
     end;
    end;
    if (Flags and 4)<>0 then begin
     if Stream.Write(AType^.UpperLimit,SizeOf(int64))<>SizeOf(int64) then begin
      exit;
     end;
    end;
    if Stream.Write(AType^.SubRangeType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     exit;
    end;
   end;
   ttdArray,ttdShortString:begin
    Flags:=0;
    if AType^.Length<>0 then begin
     Flags:=Flags or 1;
    end;
    if assigned(AType^.Range) then begin
     Flags:=Flags or 2;
    end;
    if assigned(AType^.Definition) then begin
     Flags:=Flags or 4;
    end;
    if AType^.OpenArray then begin
     Flags:=Flags or 8;
    end;
    if AType^.DynamicArray then begin
     Flags:=Flags or 16;
    end;
    if AType^.VariantArray then begin
     Flags:=Flags or 32;
    end;
    if AType^.ConstructorArray then begin
     Flags:=Flags or 64;
    end;
    if AType^.ArrayOfConst then begin
     Flags:=Flags or 128;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.length,SizeOf(int64))<>SizeOf(int64) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     WriteTypeReference(AType^.Range);
    end;
    if (Flags and 4)<>0 then begin
     WriteTypeReference(AType^.Definition);
    end;
    Flags:=0;
    if AType^.OpenString then begin
     Flags:=Flags or 1;
    end;
    Stream.WriteByte(Flags);
   end;
   ttdLongString:begin
    Flags:=0;
    if AType^.LongStringCodePage<>$ffff then begin
     Flags:=Flags or 1;
    end;
    if AType^.LongStringReferenceCounted then begin
     Flags:=Flags or 2;
    end;
    Stream.WriteByte(Flags);
    if Stream.Write(AType^.LongStringType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     exit;
    end;
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.LongStringCodePage,SizeOf(longword))<>SizeOf(longword) then begin
      exit;
     end;
    end;
   end;
   ttdRecord,ttdObject,ttdClass,ttdInterface:begin
    Flags:=0;
    if AType^.RecordSize<>0 then begin
     Flags:=Flags or 1;
    end;
    if assigned(AType^.RecordTable) then begin
     Flags:=Flags or 2;
    end;
    if AType^.TypeDefinition in [ttdObject,ttdClass,ttdInterface] then begin
     if assigned(AType^.ChildOf) then begin
      Flags:=Flags or 4;
     end;
    end;
    if AType^.TypeDefinition in [ttdClass,ttdInterface] then begin
     if length(AType^.InterfaceChildOf)>0 then begin
      Flags:=Flags or 8;
     end;
    end;
    if AType^.TypeDefinition=ttdInterface then begin
     if (AType^.GUID.D1<>0) or (AType^.GUID.D2<>0) or (AType^.GUID.D3<>0) or
        (AType^.GUID.D4[0]<>0) or (AType^.GUID.D4[1]<>0) or (AType^.GUID.D4[2]<>0) or (AType^.GUID.D4[3]<>0) or
        (AType^.GUID.D4[4]<>0) or (AType^.GUID.D4[5]<>0) or (AType^.GUID.D4[6]<>0) or (AType^.GUID.D4[7]<>0) then begin
      Flags:=Flags or 16;
     end;
    end;
    if AType^.RecordPacked then begin
     Flags:=Flags or 32;
    end;
    if AType^.RecordAlignment<>1 then begin
     Flags:=Flags or 64;
    end;
    if AType^.HasVirtualTable then begin
     Flags:=Flags or 128;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.RecordSize,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     WriteSymbolReference(AType^.RecordTable.First);
    end;
    if (Flags and 4)<>0 then begin
     WriteSymbolReference(AType^.ChildOf);
    end;
    if (Flags and 8)<>0 then begin
     Stream.WriteLongInt(length(AType^.InterfaceChildOf));
     for SubIndex:=0 to length(AType^.InterfaceChildOf)-1 do begin
      WriteSymbolReference(AType^.InterfaceChildOf[SubIndex]);
     end;
    end;
    if (Flags and 16)<>0 then begin
     if Stream.Write(AType^.GUID,SizeOf(TGUID))<>SizeOf(TGUID) then begin
      exit;
     end;
    end;
    if (Flags and 64)<>0 then begin
     if Stream.Write(AType^.RecordAlignment,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    Flags:=0;
    if AType^.VirtualIndexCount<>0 then begin
     Flags:=Flags or 1;
    end;
    if (AType^.TypeDefinition=ttdClass) and assigned(AType^.ClassOfType) then begin
     Flags:=Flags or 2;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.VirtualIndexCount,SizeOf(longint))<>SizeOf(longint) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     WriteTypeReference(AType^.ClassOfType);
    end;
   end;
   ttdClassRef:begin
    Flags:=0;
    if assigned(AType^.ClassOf) then begin
     Flags:=Flags or 1;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     WriteSymbolReference(AType^.ClassOf);
    end;
   end;
   ttdFile:begin
    Flags:=0;
    if assigned(AType^.FileTypeRecord) then begin
     Flags:=Flags or 1;
    end;
    Stream.WriteByte(Flags);
    if Stream.Write(AType^.FileType,SizeOf(TFileType))<>SizeOf(TFileType) then begin
     SymbolPointerList.Destroy;
     TypePointerList.Destroy;
     exit;
    end;
    if (Flags and 1)<>0 then begin
     WriteTypeReference(AType^.FileTypeRecord);
    end;
   end;
   ttdPointer:begin
    Flags:=0;
    if assigned(AType^.PointerTo) then begin
     Flags:=Flags or 1;
    end;
    if AType^.PointerFar then begin
     Flags:=Flags or 2;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     WriteSymbolReference(AType^.PointerTo);
    end;
   end;
   ttdSet:begin
    Flags:=0;
    if assigned(AType^.SetOf) then begin
     Flags:=Flags or 1;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     WriteTypeReference(AType^.SetOf);
    end;
    if Stream.Write(AType^.SetSize,SizeOf(byte))<>SizeOf(byte) then begin
     exit;
    end;
   end;
   ttdFloat:begin
    Flags:=0;
    if AType^.FloatLowerLimit<>0 then begin
     Flags:=Flags or 1;
    end;
    if AType^.FloatUpperLimit<>0 then begin
     Flags:=Flags or 2;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     if Stream.Write(AType^.FloatLowerLimit,SizeOf(extended))<>SizeOf(extended) then begin
      exit;
     end;
    end;
    if (Flags and 2)<>0 then begin
     if Stream.Write(AType^.FloatUpperLimit,SizeOf(extended))<>SizeOf(extended) then begin
      exit;
     end;
    end;
    if Stream.Write(AType^.FloatType,SizeOf(TStandardType))<>SizeOf(TStandardType) then begin
     exit;
    end;
   end;
   ttdProcedure:begin
    Flags:=0;
    if assigned(AType^.Parameter) then begin
     Flags:=Flags or 1;
    end;
    if assigned(AType^.ReturnType) then begin
     Flags:=Flags or 2;
    end;
    if AType^.MethodPointer then begin
     Flags:=Flags or 4;
    end;
    if AType^.AddressOnly then begin
     Flags:=Flags or 8;
    end;
    Stream.WriteByte(Flags);
    if (Flags and 1)<>0 then begin
     WriteSymbolReference(AType^.Parameter.First);
    end;
    if (Flags and 2)<>0 then begin
     WriteTypeReference(AType^.ReturnType);
    end;
   end;
  end;
 end;

 CRC32:=TCRC32.Create;
 CRC32.UpdateStream(Stream);
 UnitSymbol^.UnitCheckSum:=CRC32.GetDigest{$IFDEF WIN32}xor GetTickCount;{$ENDIF};
 CRC32.Destroy;

 result:=true;
end;

function TUnitManager.SaveUnit(Stream:TBeRoStream;List:TSymbolList;UnitSymbol:PSymbol;UnitName:ansistring):boolean;
var UnitHeader:TUnitHeader;
    Index,Count:longint;
    Symbol:PSymbol;
    AUnit:PUnit;
    Flags:byte;
    UnitObject:PSymbolUnitObject;
    UnitResource:PSymbolUnitResource;
    SymbolExport:PSymbolExport;
    SymbolExports:array of PSymbolExport;
begin
 result:=false;

 UnitHeader.Signature:=UnitHeaderSignature;
 UnitHeader.Version:=UnitHeaderVersion;
 UnitHeader.Symbols:=UnitSymbol^.CountSymbols;
 UnitHeader.Types:=UnitSymbol^.CountTypes;
 UnitHeader.Constants:=UnitSymbol^.CountConstants;
 UnitHeader.Objects:=0;
 UnitHeader.Resources:=0;
 UnitHeader.SymbolExports:=0;
 UnitHeader.SymbolIDCounter:=UnitSymbol^.SymbolIDCounter;
 UnitHeader.TypeIDCounter:=UnitSymbol^.TypeIDCounter;

 UnitObject:=UnitSymbol^.ObjectList.First;
 while assigned(UnitObject) do begin
  inc(UnitHeader.Objects);
  UnitObject:=UnitObject^.Next;
 end;

 UnitResource:=UnitSymbol^.ResourceList.First;
 while assigned(UnitResource) do begin
  inc(UnitHeader.Resources);
  UnitResource:=UnitResource^.Next;
 end;

 SymbolExport:=UnitSymbol^.SymbolExports;
 while assigned(SymbolExport) do begin
  inc(UnitHeader.SymbolExports);
  SymbolExport:=SymbolExport^.Next;
 end;

 if Stream.Write(UnitHeader,SizeOf(TUnitHeader))<>SizeOf(TUnitHeader) then begin
  exit;
 end;

 Stream.WriteDataString(UnitName);

 Stream.WriteDWord(UnitSymbol^.UnitCheckSum);

 Index:=0;
 AUnit:=UnitSymbol^.FirstUnit;
 while assigned(AUnit) do begin
  inc(Index);
  AUnit:=AUnit^.Next;
 end;

 Stream.WriteDWord(Index);

 Index:=0;
 AUnit:=UnitSymbol^.FirstUnit;
 while assigned(AUnit) do begin
  Symbol:=AUnit^.Symbol;
  if Symbol^.SymbolType=Symbols.tstUnit then begin
   Flags:=0;
   if AUnit^.AfterImplementation then begin
    Flags:=Flags or 1;
   end;
   Symbol^.UnitIndex:=Index;
   Stream.WriteByte(Flags);
   Stream.WriteDataString(Symbol^.Name);
   Stream.WriteDWord(Symbol^.UnitCheckSum);
   inc(Index);
  end else begin
   Error.InternalError(200605181509005);
  end;
  AUnit:=AUnit^.Next;
 end;

 Stream.Append(UnitSymbol^.SymbolTableStream);

 UnitObject:=UnitSymbol^.ObjectList.First;
 while assigned(UnitObject) do begin
  Stream.WriteDWord(UnitObject^.Stream.Size);
  UnitObject^.Stream.Seek(0);
  Stream.Append(UnitObject^.Stream);
  UnitObject:=UnitObject^.Next;
 end;

 UnitResource:=UnitSymbol^.ResourceList.First;
 while assigned(UnitResource) do begin
  Stream.WriteDWord(UnitResource^.Stream.Size);
  UnitResource^.Stream.Seek(0);
  Stream.Append(UnitResource^.Stream);
  UnitResource:=UnitResource^.Next;
 end;

 if assigned(UnitSymbol^.SymbolExports) then begin

  SymbolExports:=nil;
  try
   Count:=0;
   SymbolExport:=UnitSymbol^.SymbolExports;
   while assigned(SymbolExport) do begin
    inc(Count);
    SymbolExport:=SymbolExport^.Next;
   end;

   SetLength(SymbolExports,Count);

   Count:=0;
   SymbolExport:=UnitSymbol^.SymbolExports;
   while assigned(SymbolExport) do begin
    SymbolExports[Count]:=SymbolExport;
    inc(Count);
    SymbolExport:=SymbolExport^.Next;
   end;

   for Index:=Count-1 downto 0 do begin
    SymbolExport:=SymbolExports[Index];
    Stream.WriteDataString(SymbolExport^.ModuleName);
    Stream.WriteDataString(SymbolExport^.SymbolName);
    Stream.WriteDataString(SymbolExport^.SymbolOverloadedName);
    Stream.WriteDataString(SymbolExport^.Name);
    Stream.WriteLongInt(SymbolExport^.Index);
   end;
  finally
   SetLength(SymbolExports,0);
  end;

 end;

 result:=true;
end;

function TUnitManager.LoadUnitFile(FileName:ansistring;ModuleSymbol:PSymbol;AfterImplementation,LocalDefined:boolean;Level:longint=0):PSymbol;
var FileStream:TBeRoFileStream;
    Stream:TBeRoStream;
begin
 FileStream:=TBeRoFileStream.Create(FileName);
 Stream:=TBeRoStream.Create;
 Stream.Assign(FileStream);
 FileStream.Destroy;
 Stream.Seek(0);
 result:=LoadUnit(Stream,ModuleSymbol,AfterImplementation,LocalDefined,Level);
 Stream.Destroy;
end;

function TUnitManager.SaveUnitFile(FileName:ansistring;List:TSymbolList;UnitSymbol:PSymbol;UnitName:ansistring):boolean;
var FileStream:TBeRoFileStream;
    Stream:TBeRoStream;
begin
 Stream:=TBeRoStream.Create;
 result:=SaveUnit(Stream,List,UnitSymbol,UnitName);
 FileStream:=TBeRoFileStream.CreateNew(FileName);
 FileStream.Assign(Stream);
 FileStream.Destroy;
 Stream.Destroy;
end;

function TUnitManager.Load(AUnitName,ASrcPath:ansistring;AfterImplementation,LocalDefined:boolean;ModuleSymbol:PSymbol;Level:longint=0):TUnitLoadResult;
var AUnitPath,LogBegin:ansistring;
    UnitCompiler:TCompiler;
    UnitParser:TParser;
    DoRebuild:boolean;
    Symbol:PSymbol;
    AUnit:PUnit;
    Index:longint;
    FileStream:TBeRoFileStream;
    Stream:TBeRoStream;
    ShowFinished:boolean;
    NewLocalSwitches:TLocalSwitches;
    OldLocalSwitches:PLocalSwitches;
begin
 ShowFinished:=true;
 result:=ulrNone;
 if length(ASrcPath)<>0 then begin
  AUnitPath:=ChangeFileExt(ASrcPath,tfeUnit);
 end else begin
  AUnitPath:=ChangeFileExt(AUnitName,tfeUnit);
  ASrcPath:=ChangeFileExt(AUnitPath,tfeSrc);
  if not FileExists(ASrcPath) then begin
   ASrcPath:=ChangeFileExt(AUnitPath,tfeSrcAlternative);
  end;
 end;

 if DebugLogActive and DebugLogDumpUnitManager then begin
  LogBegin:='  '+CorrectSymbolName(ModuleSymbol^.Name);
  for Index:=1 to Level do LogBegin:='  '+LogBegin;
  LogBegin:=LogBegin+' ';
 end else begin
  LogBegin:='';
 end;

 AddUnit(tpsIdentifier+AUnitName,nil);

 if DebugLogActive and DebugLogDumpUnitManager then begin
  DebugLog(LogBegin+'Searching "'+AUnitName+'"');
 end;

 Symbol:=SymbolManager.GetSymbol(tpsIdentifier+AUnitName);
 if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin

  AUnit:=ModuleSymbol^.FirstUnit;
  while assigned(AUnit) do begin
   if (AUnit^.Symbol=Symbol) and AUnit^.LocalDefined then begin
    Error.AbortCode(531,AUnitName);
    exit;
   end;
   AUnit:=AUnit^.Next;
  end;

  if Symbol^.Loaded or Symbol^.IsInterfaceReady then begin

   if DebugLogActive and DebugLogDumpUnitManager then begin
    DebugLog(LogBegin+'Loaded "'+AUnitName+'" instance found');
   end;
   SymbolManager.AddUnit(ModuleSymbol,Symbol,AfterImplementation,LocalDefined);
   AddUnit(tpsIdentifier+AUnitName,Symbol);
   ShowFinished:=false;
   result:=ulrNormal;

  end else begin

   if Symbol^.IsUnitCompiled and AfterImplementation and not Symbol^.IsInterfaceReady then begin

    ProcessFixUps;

    if FixUpList.Count=0 then begin
     if DebugLogActive and DebugLogDumpUnitManager then begin
      DebugLog(LogBegin+'Reset parser and recompile "'+AUnitName+'"');
     end;
     UnitCompiler:=ACompiler;
     UnitParser:=Symbol^.Parser;
     UnitParser.DoBreak:=true;
     UnitParser.Scanner.InputStream.Seek(0);
     SymbolManager.DestroySymbolUnits(Symbol);
     OldLocalSwitches:=Error.LocalSwitches;
     NewLocalSwitches:=DefaultLocalSwitches;
     UnitParser:=TParser.Create(UnitParser.Scanner.InputStream,UnitParser.Scanner.FileName,Error,SymbolManager,self,UnitCompiler,Level+1,Options,GlobalSwitches,@NewLocalSwitches);
     UnitParser.Parse;
     UnitParser.Destroy;
     Error.LocalSwitches:=OldLocalSwitches;
     Symbol:=SymbolManager.GetSymbol(tpsIdentifier+AUnitName);
     if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
      SymbolManager.AddUnit(ModuleSymbol,Symbol,AfterImplementation,LocalDefined);
      AddUnit(tpsIdentifier+AUnitName,Symbol);
     end else begin
      Error.InternalError(200605181509006);
     end;
     result:=ulrRecompiled;
    end else begin
     Error.AbortCode(532);
    end;

   end else begin

    if assigned(ModuleSymbol) then begin
     Error.AbortCode(48,AUnitName,CorrectSymbolName(ModuleSymbol^.Name));
    end else begin
     Error.InternalError(200605181509007);
    end;

   end;

  end;

 end else begin

  if FileExists(ASrcPath) then begin
   DoRebuild:=RebuildAll;
   if not DoRebuild then begin
    DoRebuild:=not FileExists(AUnitPath);
    if not DoRebuild then begin
     DoRebuild:=FileAge(AUnitPath)<FileAge(ASrcPath);
    end;
    if FileExists(AUnitPath) and not DoRebuild then begin
     FileStream:=TBeRoFileStream.Create(AUnitPath);
     Stream:=TBeRoStream.Create;
     Stream.Assign(FileStream);
     FileStream.Destroy;
     Stream.Seek(0);
     DoRebuild:=RequireUnitRecompile(Symbol,Stream,ModuleSymbol,AfterImplementation,LocalDefined);
     Stream.Destroy;
    end;
   end;
  end else begin
   DoRebuild:=false;
  end;

  if DoRebuild then begin

   ProcessFixUps;
   if FixUpList.Count=0 then begin
    UnitCompiler:=ACompiler;
    if DebugLogActive and DebugLogDumpUnitManager then begin
     DebugLog(LogBegin+'Compile "'+AUnitName+'"');
    end;
    UnitCompiler.CompileFile(ASrcPath,Level+1);
    Symbol:=SymbolManager.GetSymbol(tpsIdentifier+AUnitName);
    if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
     SymbolManager.AddUnit(ModuleSymbol,Symbol,AfterImplementation,LocalDefined);
     AddUnit(tpsIdentifier+AUnitName,Symbol);
     result:=ulrRecompiled;
    end else begin
     Error.InternalError(200605181510000);
    end;
   end else begin
    Error.AbortCode(532);
   end;
  end else if FileExists(AUnitPath) then begin
   if DebugLogActive and DebugLogDumpUnitManager then begin
    DebugLog(LogBegin+'Load "'+AUnitName+'"');
   end;
   LoadUnitFile(AUnitPath,ModuleSymbol,AfterImplementation,LocalDefined,Level);
   result:=ulrNormal;
  end else begin
   Error.AbortCode(212,AUnitName);
  end;

 end;

 if ShowFinished then begin
  if DebugLogActive and DebugLogDumpUnitManager then begin
   DebugLog(LogBegin+'Finished "'+AUnitName+'"');
  end;
 end;

 Symbol:=SymbolManager.GetSymbol(tpsIdentifier+AUnitName);
 if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
  Symbol^.OverloadedName:=AUnitPath;
 end;

 if Level=0 then begin
  for Index:=0 to length(LoadedUnits)-1 do begin
   if not assigned(LoadedUnits[Index].Symbol) then begin
    Load(CorrectSymbolName(LoadedUnits[Index].Name),'',true,false,ModuleSymbol,Level+1);
   end;
  end;
 end;

 ProcessFixUps;
 if (Level=0) and (FixUpList.Count<>0) then begin
  Error.AbortCode(532);
 end;
end;

end.
