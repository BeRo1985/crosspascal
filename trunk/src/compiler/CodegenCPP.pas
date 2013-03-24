unit CodegenCPP;

interface

uses SysUtils,Code,Symbols,Tree,IntegerList,PointerList,StringList,Error,Globals,BeRoStream,BeRoUtils,
     BeRoDoubleToString,HugeString;

const spacesNONE=0;
      spacesLEFT=1 shl 0;
      spacesRIGHT=1 shl 1;
      spacesBOTH=spacesLEFT or spacesRIGHT;

type TCodeWriter = class
      private
       FTabs: Integer;
       FCurrentLine: ansistring;
       FHeader,FStack,FData: TBeRoStream;
       // footer is a simple ansistring, because strings added in the footer
       // should come out in reverse order, i.e. FFooter := NewLine + CRLF + FFooter
       // (should be okay since AddFooter is probably only used seldomly)
       FFooter: ansistring;
       FIncludes: array of ansistring;
       FRootNestedStartPosition: longint;
       FMarker: longint;
      public
       constructor Create;
       destructor Destroy; override;

       procedure AddHeader(const s: ansistring);
       procedure AddFooter(const s: ansistring);
       procedure AddInclude(const s: ansistring);

       procedure Add(const s: ansistring; Spaces: longint = spacesNONE);
       procedure AddLn(const s: ansistring; Spaces: longint = spacesNONE);

       procedure Clear;
       procedure ExportStream(TargetStream: TBeRoStream);

       procedure SetMarker;
       procedure InsertAtMark(s: ansistring);

       // indenting, does not work with header & footer
       procedure IncTab;
       procedure DecTab;

       procedure MarkRootNested;
       procedure UnmarkRootNested(Source:TCodeWriter);
     end;

     TCodegenCPP = class(TCode)
      private
       FHeader,FCode, FProcCode, FProcStruct: TCodeWriter;
       FDepth: Integer;
       FSelf: PSymbol;
       FVariantPrefix:boolean;
       FInProc, FNeedNestedStack: boolean;
       FProcSymbol: PSymbol;
       FProcStructCount: longint;
       FStringConstCount: longint;
       FBreakCount: Integer;
       FNestedBreakCount: Integer;
       FBreakLabelNeeded: array of Integer;
       FContinueLabelNeeded: array of Integer;
      protected
       function GetModuleName(Sym: PSymbol): ansistring;
       function GetSymbolName(Sym: PSymbol): ansistring;
       function GetTypeName(Type_:PType):ansistring;

       function ConvertStdType(const StdType: TStandardType): ansistring;
       function ConvertUnsignedStdType(const StdType: TStandardType): ansistring;
       function ConvertConstSymbol(const Symbol: PSymbol): ansistring;
       procedure ConvertFuncSymbol(const Symbol: PSymbol; Target: TCodeWriter);

       function AnsiStringEscape(const Input: ansistring; Quotes: Boolean = True): ansistring;
       function WideStringEscape(const Input: widestring): ansistring;

       procedure ProcessTypeOrName(AType: PType; Target: TCodeWriter; OwnType: PType = nil);
       procedure ProcessTypedConstant(var Constant: PConstant; AType: PType; Target: TCodeWriter);
       procedure ProcessFunctionType(ReturnType: PType; Parameter: TSymbolList; const funcName: ansistring; Target: TCodeWriter);

       function TranslateStringConstant(ConstantStr: THugeString): ansistring;

       procedure TranslateCode(TreeNode:TTreeNode);

       procedure StartBreakContinuePart;
       procedure StopBreakPart;
       procedure StopContinuePart;

       procedure TranslateConstant(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateVariable(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateFunction(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateProcedure(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateObject(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateClass(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateTemp(Symbol: PSymbol; Target: TCodeWriter);
       procedure TranslateUnit(Symbol: PSymbol; Target: TCodeWriter);

       procedure TranslateSymbolList(List: TSymbolList; IgnoreTypes: boolean; ATarget: TCodeWriter = nil);
       procedure InitializeSymbolList(List: TSymbolList;Target: TCodeWriter);
       procedure FinalizeSymbolList(List: TSymbolList;Target: TCodeWriter);

       procedure TranslateConstants;
       procedure TranslateModuleTypes(ModuleSymbol:PSymbol;Target:TCodeWriter);
      public
       LocalSwitches:PLocalSwitches;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:POptions;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;

       procedure BeginRootNestedProc; override;
       procedure EndRootNestedProc; override;

       procedure GenerateProc(ProcSymbol:PSymbol;ProcCodeTree:TTreeNode); override;
       procedure GenerateProgram(ProgramSymbol:PSymbol;ProgramCodeTree:TTreeNode); override;
       procedure GenerateLibrary(LibrarySymbol:PSymbol;LibraryCodeTree:TTreeNode); override;
       procedure GenerateUnit(UnitSymbol:PSymbol;InitializationCodeTree,FinalizationCodeTree:TTreeNode); override;

       procedure SaveToStreams(CodeStream,HeaderStream:TBeRoStream); override;
     end;


implementation

function FloatToStr(f:{$ifdef HAS_TYPE_EXTENDED}extended{$else}double{$endif}):ansistring;
begin
{$ifdef HAS_TYPE_EXTENDED}
 if f=trunc(f) then begin
  result:=IntToStr(trunc(f));
 end else begin
  Str(f,result);
  result:=trim(result);
 end;
{$else}
  result:=BeRoConvertDoubleToString(f,bdtsmSTANDARD,-1);
{$endif}
end;

function TCodegenCPP.GetModuleName(Sym: PSymbol): ansistring;
begin
 case Sym.UnitKind of
  tukUNIT:
    result:='UNIT_'+Sym.Name;
  tukPROGRAM:
    result:='PROGRAM_'+Sym.Name;
  tukLIBRARY:
    result:='LIBRARY_'+Sym.Name;
  tukPACKAGE:
    result:='PACKAGE_'+Sym.Name;
  else
    result:='UNKNOWN_'+Sym.Name;
 end;
end;

function TCodegenCPP.GetSymbolName(Sym: PSymbol): ansistring;
begin
 if assigned(Sym) then begin
  case Sym.SymbolType of
   Symbols.tstType:begin
    result:=GetTypeName(Sym^.TypeDefinition);
   end;
   Symbols.tstVariable:begin
    if tsaField in Sym^.Attributes then begin
     if tsaInternalField in Sym^.Attributes then begin
      result:='INTERNAL_FIELD_'+Sym.Name;
     end else begin
      if FVariantPrefix then begin
       result:=Sym.VariantPrefix+'FIELD_'+Sym.Name;
      end else begin
       result:='FIELD_'+Sym.Name;
      end;
      if assigned(Sym^.TypeDefinition) and (Sym^.TypeDefinition^.TypeDefinition=ttdPointer) then begin
       result:='(('+GetTypeName(Sym^.TypeDefinition)+')((void*)'+result+'))';
      end;
     end;
    end else begin
     if Sym.VariableLevel=0 then begin
      if Sym^.TypedConstant then begin
       result:=GetModuleName(Sym.OwnerModule)+'_TYPEDCONSTANT_'+Sym.Name;
      end else begin
       result:=GetModuleName(Sym.OwnerModule)+'_VARIABLE_'+Sym.Name;
      end;
     end else begin
      if Sym^.VariableType=tvtResult then begin
       result:='result';
      end else if Sym^.TypedConstant then begin
       result:='LOCAL_TYPEDCONSTANT_'+Sym.Name;
      end else begin
       result:='LOCAL_VARIABLE_'+Sym.Name;
      end;
      if FInProc then begin
       if assigned(Sym^.LocalProcSymbol) and Sym^.LocalProcSymbolAccessedFromHigherNestedProc then begin
        result:='(('+GetSymbolName(Sym^.LocalProcSymbol)+'_NESTED_STACK*)(nestedLevelStack['+IntToStr(Sym^.LocalProcSymbol^.LexicalScopeLevel)+']))->'+result;
       end;
       if IsSymbolReference(Sym) then begin
        result:='(*('+result+'))';
       end;
       if (Sym^.VariableType in [tvtParameterVariable,tvtParameterResult,tvtParameterConstant,tvtParameterValue]) and
          assigned(Sym^.TypeDefinition) and (Sym^.TypeDefinition^.TypeDefinition=ttdPointer) then begin
        result:='(('+GetTypeName(Sym^.TypeDefinition)+')((void*)'+result+'))';
       end;
      end;
     end;
    end;
   end;
   Symbols.tstLabel:begin
    result:=GetModuleName(Sym.OwnerModule)+'_LABEL_'+Sym.Name
   end;
   Symbols.tstConstant:begin
    result:=GetModuleName(Sym.OwnerModule)+'_CONSTANT_'+Sym.Name
   end;
   Symbols.tstProcedure:begin
    if tpaConstructor in Sym.ProcedureAttributes then begin
     result:=GetModuleName(Sym.OwnerModule)+'_CONSTRUCTOR_'+Sym.OverloadedName;
    end else if tpaDestructor in Sym.ProcedureAttributes then begin
     result:=GetModuleName(Sym.OwnerModule)+'_DESTRUCTOR_'+Sym.OverloadedName;
    end else begin
     result:=GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.OverloadedName;
    end;
{   if (tpaOverload in Sym.ProcedureAttributes) and (Sym.OverloadedName<>'') and (Sym.OverloadedName<>Sym.Name) then begin
     result:=GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.OverloadedName;
    end else begin
     result:=GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.Name;
    end;}
   end;
   Symbols.tstFunction:begin
    result:=GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.OverloadedName;
{   if (tpaOverload in Sym.ProcedureAttributes) and (Sym.OverloadedName<>'') and (Sym.OverloadedName<>Sym.Name) then begin
     result:=GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.OverloadedName;
    end else begin
     result:=GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.Name;
    end;}
   end;
   else begin
    result:=GetModuleName(Sym.OwnerModule)+'_SYMBOL_'+Sym.Name;
   end;
  end;
 end else begin
  result:='void';
 end;
end;

function TCodeGenCPP.GetTypeName(Type_:PType):ansistring;
begin
 if assigned(Type_^.OwnerModule) then begin
  if assigned(Type_^.Symbol) and (Type_^.Symbol^.LexicalScopeLevel=0) and (tsaPublicUnitSymbol in Type_^.Symbol^.Attributes) then begin
   result:=GetModuleName(Type_^.OwnerModule)+'_TYPE_'+Type_^.Symbol^.Name;
  end else begin
   result:=GetModuleName(Type_^.OwnerModule)+'_TYPE_ID_'+IntToStr(Type_^.ID);
  end;
 end else begin
  result:=IntToStr(PtrUInt(Type_));
 end;
end;

procedure TCodegenCPP.InitializeSymbolList(List: TSymbolList;
  Target: TCodeWriter);
var sym: PSymbol;
begin
 sym := List.First;

 while Assigned(sym) do
 begin
  if(Sym.SymbolType=tstVariable)and(Sym.TypedConstant) then
   case Sym.TypeDefinition.TypeDefinition of
    ttdLongString:
    begin
     Target.Add(GetSymbolName(Sym)+' =',spacesRIGHT);
     ProcessTypedConstant(Sym.Constant,Sym.TypeDefinition,Target);
    end;
   end;

  Target.AddLn(';');
  sym := sym.Next;
 end;
end;

function FindSymbol(AType:PType): PSymbol;
begin
 if not Assigned(AType) then
 begin
   result := nil;
   exit;
 end;

 Result:=AType.OwnerModule.SymbolList.First;
 while assigned(Result) do
 begin
  if(Result.SymbolType = tstType) and (Result.TypeDefinition=AType)then
   Exit;
  Result:=Result.Next;
 end;
end;

{ TCodegenCPP }

function TCodegenCPP.ConvertConstSymbol(const Symbol: PSymbol): ansistring;
begin
 case Symbol.ConstantType of
  tctOrdinal:
   result := IntToStr(Symbol.IntValue);
  tctFloat:
   Str(Symbol.FloatValue, result);
  else result := '<ConvertConstSymbol>';
 end;
end;

procedure TCodegenCPP.ConvertFuncSymbol(const Symbol: PSymbol; Target: TCodeWriter);
var sym: PSymbol;
    HaveParameters: boolean;
begin
 if Symbol.SymbolType = tstProcedure then
  Target.Add('void',spacesRIGHT)
 else if Symbol.SymbolType = tstFunction then
 begin
  ProcessTypeOrName(Symbol.ReturnType, Target);
  Target.Add('',spacesRIGHT);
 end else
  Exit;
 Target.Add(GetSymbolName(Symbol));
 Target.Add('(');
 HaveParameters:=false;
 if FNeedNestedStack and (Symbol^.LexicalScopeLevel>0) then begin
  if HaveParameters then
   Target.Add(',',spacesRIGHT);
  Target.Add('void **nestedLevelStack');
  HaveParameters:=true;
 end;
 if assigned(Symbol^.OwnerObjectClass) then begin
  if HaveParameters then
   Target.Add(',',spacesRIGHT);
  Target.Add(GetSymbolName(Symbol^.OwnerObjectClass^.Symbol)+' *instanceData');
  HaveParameters:=true;
 end;
 if Assigned(Symbol.Parameter) then
  sym := Symbol.Parameter.First
 else sym := nil;
 while Assigned(sym) do
 begin
  if HaveParameters then
   Target.Add(',',spacesRIGHT);
  ProcessTypeOrName(sym.TypeDefinition, Target);
  if IsSymbolReference(Sym) then begin
   Target.Add('*',spacesRIGHT);
  end;
  Target.Add(GetSymbolName(Sym),spacesLEFT);
  HaveParameters:=true;
  sym:=sym.Next;
 end;
 Target.Add(')');
end;

function TCodegenCPP.ConvertStdType(const StdType: TStandardType): ansistring;
begin
 case StdType of
  tstSigned8Bit: result := 'int8_t';
  tstSigned16Bit: result := 'int16_t';
  tstSigned32Bit: result := 'int32_t';
  tstSigned64Bit: result := 'int64_t';
  tstUnsigned8Bit: result := 'uint8_t';
  tstUnsigned16Bit: result := 'uint16_t';
  tstUnsigned32Bit: result := 'uint32_t';
  tstUnsigned64Bit: result := 'uint64_t';
  tstFloat32Bit: result := 'float';
  tstFloat64Bit: result := 'double';
  tstFloat80Bit: result := 'long double';
  tstUnsignedChar: result := 'uint8_t';
  tstUnsignedWideChar: result := 'uint16_t';
 end;
end;

function TCodegenCPP.ConvertUnsignedStdType(const StdType: TStandardType): ansistring;
begin
 case StdType of
  tstSigned8Bit: result := 'uint8_t';
  tstSigned16Bit: result := 'uint16_t';
  tstSigned32Bit: result := 'uint32_t';
  tstSigned64Bit: result := 'uint64_t';
  tstUnsigned8Bit: result := 'uint8_t';
  tstUnsigned16Bit: result := 'uint16_t';
  tstUnsigned32Bit: result := 'uint32_t';
  tstUnsigned64Bit: result := 'uint64_t';
  tstFloat32Bit: result := 'float';
  tstFloat64Bit: result := 'double';
  tstFloat80Bit: result := 'long double';
  tstUnsignedChar: result := 'uint8_t';
  tstUnsignedWideChar: result := 'uint16_t';
 end;
end;

constructor TCodegenCPP.Create(TheError: TError;
  TheSymbolManager: TSymbolManager; TheTreeManager: TTreeManager;
  TheOptions: POptions; TheLocalSwitches: PLocalSwitches);
begin
 inherited;

 LocalSwitches := TheLocalSwitches;

 FHeader := TCodeWriter.Create;
 FCode := TCodeWriter.Create;
 FProcCode := TCodeWriter.Create;
 FProcStruct := TCodeWriter.Create;

 FVariantPrefix:=true;

 FInProc:=false;
 FNeedNestedStack:=false;

 FProcSymbol:=nil;
end;

destructor TCodegenCPP.Destroy;
begin
 FHeader.Free;
 FCode.Free;
 FProcCode.Free;
 FProcStruct.Free;
 inherited;
end;

procedure TCodegenCPP.FinalizeSymbolList(List: TSymbolList;
  Target: TCodeWriter);
var sym: PSymbol;
begin
 sym := List.First;

 while Assigned(sym) do
 begin
  if(Sym.SymbolType=tstVariable) then
   case Sym.TypeDefinition.TypeDefinition of
    ttdLongString:
    begin
     Target.Add('FreeLongstring(&'+GetSymbolName(Sym)+')');
    end;
   end;
  Target.AddLn(';');

  sym := sym.Next;
 end;
end;

procedure TCodegenCPP.GenerateLibrary(LibrarySymbol: PSymbol;
  LibraryCodeTree: TTreeNode);
begin
 FCode.AddLn('//library ' + LibrarySymbol.Name);
 FSelf := LibrarySymbol;
 TranslateSymbolList(FSelf.SymbolList, false);
end;

procedure TCodegenCPP.BeginRootNestedProc;
begin
 FProcCode.MarkRootNested;
 FProcStruct.Clear;
 FProcStruct.AddLn('');
end;

procedure TCodegenCPP.EndRootNestedProc;
begin
 FCode.UnmarkRootNested(FProcStruct);
end;

procedure TCodegenCPP.GenerateProc(ProcSymbol: PSymbol;
  ProcCodeTree: TTreeNode);
var s,s2: ansistring;
    ParameterSymbol:PSymbol;
begin
 FSelf := ProcSymbol.OwnerModule;

 FProcSymbol:=ProcSymbol;

 if tpaExternal in ProcSymbol.ProcedureAttributes then
 begin
  if pos('.h', ProcSymbol.LibraryName)>0 then
  begin
   // external .h function, inline function
   // will be generated in TranslateFunction()
   Exit;
  end;

  FProcCode.Add('PROC_'+ProcSymbol.OverloadedName+' '+ProcSymbol.OverloadedName);

  FProcCode.Add('// getProcAddress()');
  FProcCode.AddLn(';');
  Exit;
 end;

 FNeedNestedStack:=(ProcSymbol^.LexicalScopeLevel>1) or (ProcSymbol^.LexicalScopeLevelCount>1);

 if FNeedNestedStack then begin
  FProcStruct.AddLn('typedef struct '+GetSymbolName(ProcSymbol)+'_NESTED_STACK {');
  FProcStruct.IncTab;
  if assigned(ProcSymbol.Parameter) then begin
   ParameterSymbol:=ProcSymbol.Parameter.First;
   while assigned(ParameterSymbol) do begin
    if (ParameterSymbol^.SymbolType=Symbols.tstVariable) and assigned(ParameterSymbol.LocalProcSymbol) and ParameterSymbol.LocalProcSymbolAccessedFromHigherNestedProc then begin
     FProcStruct.Add(GetTypeName(ParameterSymbol^.TypeDefinition));
     if IsSymbolReference(ParameterSymbol) then begin
      FProcStruct.Add('*');
     end;
     FProcStruct.AddLn(' LOCAL_VARIABLE_'+ParameterSymbol^.Name+';');
    end;
    ParameterSymbol:=ParameterSymbol^.Next;
   end;
  end;
 end;

 FProcStructCount:=0;

 FProcCode.AddLn('//proc ' + GetSymbolName(ProcSymbol));
 ConvertFuncSymbol(ProcSymbol, FProcCode);
 FProcCode.AddLn('{');
 FProcCode.IncTab;
 FProcCode.SetMarker;
 if FNeedNestedStack then begin
  if SymbolManager.LexicalScopeLevel<=1 then begin
   FProcCode.AddLn('void* nestedLevelStack['+IntToStr(SymbolManager.LexicalScopeLevelCount)+'];');
  end;
  if SymbolManager.LexicalScopeLevel>1 then begin
   FProcCode.AddLn('void* nestedLevelStackLast = nestedLevelStack['+IntToStr(ProcSymbol^.LexicalScopeLevel)+'];');
  end;
  FProcCode.AddLn(GetSymbolName(ProcSymbol)+'_NESTED_STACK nestedLevelStackLocal;');
  FProcCode.AddLn('nestedLevelStack['+IntToStr(ProcSymbol^.LexicalScopeLevel)+'] = (void*)&nestedLevelStackLocal;');
  if assigned(ProcSymbol.Parameter) then begin
   ParameterSymbol:=ProcSymbol.Parameter.First;
   while assigned(ParameterSymbol) do begin
    if (ParameterSymbol^.SymbolType=Symbols.tstVariable) and assigned(ParameterSymbol.LocalProcSymbol) and ParameterSymbol.LocalProcSymbolAccessedFromHigherNestedProc then begin
     FProcCode.AddLn('nestedLevelStackLocal.LOCAL_VARIABLE_'+ParameterSymbol^.Name+' = LOCAL_VARIABLE_'+ParameterSymbol^.Name+';');
    end;
    ParameterSymbol:=ParameterSymbol^.Next;
   end;
  end;
 end;

 InitializeSymbolList(SymbolManager.CurrentList, FProcCode);

 Inc(FDepth);
 TranslateSymbolList(SymbolManager.CurrentList, false, FProcCode);
 Dec(FDepth);

 FInProc:=true;

 FinalizeSymbolList(SymbolManager.CurrentList, FProcCode);

 FProcCode.AddLn('//code');
 TranslateCode(ProcCodeTree);

 if FNeedNestedStack then begin
  if SymbolManager.LexicalScopeLevel>1 then begin
   FProcCode.AddLn('nestedLevelStack['+IntToStr(ProcSymbol^.LexicalScopeLevel)+'] = nestedLevelStackLast;');
  end;
 end;

 if Assigned(ProcSymbol.ReturnType) then begin
   FProcCode.AddLn('return '+GetSymbolName(ProcSymbol.ResultSymbol)+';');
 end;

 FProcCode.DecTab;
 FProcCode.AddLn('}');
// FCode.AddLn(IntToStr(Integer(ProcSymbol.SymbolType)));
 if FNeedNestedStack then begin
  if FProcStructCount=0 then begin
   FProcStruct.AddLn('uint32_t dummy;');
  end;
  FProcStruct.DecTab;
  FProcStruct.AddLn('} '+GetSymbolName(ProcSymbol)+'_NESTED_STACK;');
  FProcStruct.AddLn('typedef '+GetSymbolName(ProcSymbol)+'_NESTED_STACK* '+GetSymbolName(ProcSymbol)+'_NESTED_STACK_POINTER;');
  FProcStruct.AddLn('');
 end else begin
  FProcStruct.Clear;
 end;
 FInProc:=false;
 FProcSymbol:=nil;
end;

procedure TCodegenCPP.GenerateProgram(ProgramSymbol: PSymbol;
  ProgramCodeTree: TTreeNode);
var i: integer;
begin
 FInProc:=false;
 FProcSymbol:=nil;

 for i:=0 to SymbolManager.UnitList.Count-1 do
  FCode.AddInclude(Copy(SymbolManager.UnitList[i], 1, Length(SymbolManager.UnitList[i]))+'.h');
//  FCode.AddInclude(Copy(SymbolManager.UnitList[i], 2, Length(SymbolManager.UnitList[i]))+'.h');

 FHeader.AddHeader('#ifndef __OBJPAS2C'+ProgramSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#define __OBJPAS2C'+ProgramSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#ifdef __cplusplus');
 FHeader.AddHeader('extern "C" {');
 FHeader.AddHeader('#endif');

 FCode.AddLn('//program ' + ProgramSymbol.Name);
 FCode.AddHeader('#define __OBJPAS2CMAIN__');
 FCode.AddInclude('objpas2c.h');
 FSelf := ProgramSymbol;
 TranslateModuleTypes(FSelf, FHeader);
 TranslateSymbolList(FSelf.SymbolList, true);

 FHeader.AddLn('extern int main(int argc, char **argv);');

 FProcCode.AddLn('void pasStart(){');
 FProcCode.SetMarker;
 InitializeSymbolList(FSelf.SymbolList, FProcCode);
 FProcCode.IncTab;
 TranslateCode(ProgramCodeTree);
 FProcCode.DecTab;
 FinalizeSymbolList(FSelf.SymbolList, FProcCode);
 FProcCode.AddLn('}');
 FProcCode.AddLn('');
 FProcCode.AddLn('int main(int argc, char **argv){');
 FProcCode.SetMarker;
 FProcCode.IncTab;

 for i:=0 to SymbolManager.UnitList.Count-1 do
   FProcCode.AddLn(SymbolManager.UnitList[i]+'_C_INITIALIZATION();');

 FProcCode.AddLn('pasStart();');

 for i:=SymbolManager.UnitList.Count-1 downto 0 do
   FProcCode.AddLn(SymbolManager.UnitList[i]+'_C_FINALIZATION();');

 FProcCode.AddLn('return 0;');
 FProcCode.DecTab;
 FProcCode.AddLn('}');

 FHeader.AddFooter('#ifdef __cplusplus');
 FHeader.AddFooter('}');
 FHeader.AddFooter('#endif');
 FHeader.AddFooter('#endif //__OBJPAS2C'+ProgramSymbol.Name+'_H_INCLUDED__');
end;

procedure TCodegenCPP.GenerateUnit(UnitSymbol: PSymbol; InitializationCodeTree,
  FinalizationCodeTree: TTreeNode);
var i:longint;
begin
// FCode.AddHeader('// unit '+UnitSymbol.Name);
 FInProc:=false;
 FProcSymbol:=nil;

 FHeader.AddHeader('#ifndef __OBJPAS2C'+UnitSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#define __OBJPAS2C'+UnitSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#ifdef __cplusplus');
 FHeader.AddHeader('extern "C" {');
 FHeader.AddHeader('#endif');
 FHeader.AddHeader('');
 FHeader.AddHeader('// unit '+UnitSymbol.Name);
 FHeader.AddHeader('');
 FHeader.AddInclude('system.h');
 FHeader.AddInclude('stdint.h');
 FHeader.AddInclude('objpas2c.h');
 for i:=0 to SymbolManager.UnitList.Count-1 do
  FHeader.AddInclude(Copy(SymbolManager.UnitList[i], 1, Length(SymbolManager.UnitList[i]))+'.h');
 FHeader.AddLn('');
 FHeader.AddLn('extern void '+UnitSymbol.Name+'_C_INITIALIZATION();');
 FHeader.AddLn('extern void '+UnitSymbol.Name+'_C_FINALIZATION();');
 FHeader.AddLn('');

 FHeader.AddFooter('#ifdef __cplusplus');
 FHeader.AddFooter('}');
 FHeader.AddFooter('#endif');
 FHeader.AddFooter('#endif //__OBJPAS2C'+UnitSymbol.Name+'_H_INCLUDED__');

// FCode.AddHeader('#include "'+UnitSymbol.Name+'.h";');

 FSelf := UnitSymbol;
 TranslateModuleTypes(FSelf, FHeader);
 TranslateSymbolList(FSelf.SymbolList, true);

 FProcCode.AddLn('void '+UnitSymbol.Name+'_C_INITIALIZATION(){');
 TranslateCode(InitializationCodeTree);
 FProcCode.AddLn('}');

 FProcCode.AddLn('void '+UnitSymbol.Name+'_C_FINALIZATION(){');
 TranslateCode(FinalizationCodeTree);
 FProcCode.AddLn('}');

end;

procedure TCodegenCPP.ProcessFunctionType(ReturnType: PType; Parameter: TSymbolList; const funcName: ansistring; Target: TCodeWriter);
var i: integer;
    Sym: PSymbol;
begin
 ProcessTypeOrName(ReturnType, Target);
 Target.Add('(*'+funcName+')(',spacesLEFT);
 Sym:=Parameter.First;
 while Assigned(sym) do
 begin
  if Sym<>Parameter.First then begin
   Target.Add(',',spacesRIGHT);
  end;
  ProcessTypeOrName(Sym.TypeDefinition, Target);
  if IsSymbolReference(Sym) then begin
   Target.Add('*');
  end;
  Sym := Sym.Next;
 end;
 Target.Add(')');
end;

procedure TCodegenCPP.TranslateCode(TreeNode:TTreeNode);
var SubTreeNode,SubTreeNode2:TTreeNode;
    s:ansistring;
    HaveParameters:boolean;
begin
 if assigned(TreeNode) then begin
  case TreeNode.TreeNodeType of
   ttntEMPTY:begin
   end;
   ttntSubRange:begin
   end;
   ttntAssign:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if(TreeNode.Left.TreeNodeType=ttntVAR)and(TreeNode.Left.Symbol.TypeDefinition.TypeDefinition=ttdLongString) then
     begin
      FProcCode.Add('AssignLongstring(&');
      TranslateCode(TreeNode.Left);
      FProcCode.Add(',',spacesRIGHT);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(')');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('=',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
    end else begin
     Error.InternalError(201302222212000);
    end;
   end;
   ttntLess:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('<',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215000);
    end;
   end;
   ttntLessOrEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('<=',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215001);
    end;
   end;
   ttntGreater:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('>',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215002);
    end;
   end;
   ttntGreaterOrEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('>=',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216000);
    end;
   end;
   ttntEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('==',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216001);
    end;
   end;
   ttntNotEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('!=',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216002);
    end;
   end;
   ttntAdd:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if Assigned(TreeNode.Left.Return) and Assigned(TreeNode.Right.Return) and
        (TreeNode.Left.Return.TypeDefinition = ttdLongString)and
        (TreeNode.Right.Return.TypeDefinition = ttdLongString) then begin
       FProcCode.Add('AddLongstring(');
       TranslateCode(TreeNode.Left);
       FProcCode.Add(', ');
       TranslateCode(TreeNode.Right);
       FProcCode.Add(')');
     end else begin
       FProcCode.Add('(');
       TranslateCode(TreeNode.Left);
       FProcCode.Add('+',spacesBOTH);
       TranslateCode(TreeNode.Right);
       FProcCode.Add(')');
     end;
    end else begin
     Error.InternalError(201302222217000);
    end;
   end;
   ttntSub:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('-',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222217001);
    end;
   end;
   ttntCCODE:
   begin
     FProcCode.IncTab;
     FProcCode.AddLn(HugeStringToWideString(TreeNode.StringData));
     FProcCode.DecTab;
   end;
   ttntOr:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if assigned(TreeNode.Return) and (TreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
      if LocalSwitches^.BoolEval then begin
       FProcCode.Add('((');
       FProcCode.Add('((');
       TranslateCode(TreeNode.Left);
       FProcCode.Add(') != 0)');
       FProcCode.Add('|',spacesBOTH);
       FProcCode.Add('((');
       TranslateCode(TreeNode.Right);
       FProcCode.Add(') != 0)');
       FProcCode.Add(') != 0)');
      end else begin
       FProcCode.Add('(');
       TranslateCode(TreeNode.Left);
       FProcCode.Add('||',spacesBOTH);
       TranslateCode(TreeNode.Right);
       FProcCode.Add(')');
      end;
     end else begin
      FProcCode.Add('(');
      TranslateCode(TreeNode.Left);
      FProcCode.Add('|',spacesBOTH);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(')');
     end;
    end else begin
     Error.InternalError(201302222257000);
    end;
   end;
   ttntXor:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if assigned(TreeNode.Return) and (TreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
      FProcCode.Add('((');
      FProcCode.Add('((');
      TranslateCode(TreeNode.Left);
      FProcCode.Add(') != 0)');
      FProcCode.Add('^',spacesBOTH);
      FProcCode.Add('((');
      TranslateCode(TreeNode.Right);
      FProcCode.Add(') != 0)');
      FProcCode.Add(') != 0)');
     end else begin
      FProcCode.Add('(');
      TranslateCode(TreeNode.Left);
      FProcCode.Add('^',spacesBOTH);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(')');
     end;
    end else begin
     Error.InternalError(201302222200000);
    end;
   end;
   ttntIn:begin
   end;
   ttntMul:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('*',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216003);
    end;
   end;
   ttntSlash:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('/',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222305000);
    end;
   end;
   ttntDiv:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('/',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222218000);
    end;
   end;
   ttntMod:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('%',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222218001);
    end;
   end;
   ttntAnd:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if assigned(TreeNode.Return) and (TreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
      if LocalSwitches^.BoolEval then begin
       FProcCode.Add('((');
       FProcCode.Add('((');
       TranslateCode(TreeNode.Left);
       FProcCode.Add(') != 0)');
       FProcCode.Add('&',spacesBOTH);
       FProcCode.Add('((');
       TranslateCode(TreeNode.Right);
       FProcCode.Add(') != 0)');
       FProcCode.Add(') != 0)');
      end else begin
       FProcCode.Add('(');
       TranslateCode(TreeNode.Left);
       FProcCode.Add('&&',spacesBOTH);
       TranslateCode(TreeNode.Right);
       FProcCode.Add(')');
      end;
     end else begin
      FProcCode.Add('(');
      TranslateCode(TreeNode.Left);
      FProcCode.Add('&',spacesBOTH);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(')');
     end;
    end else begin
     Error.InternalError(201302222300001);
    end;
   end;
   ttntShl:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('<<',spacesBOTH);
     TranslateCode(TreeNode.Right);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222302000);
    end;
   end;
   ttntShr:begin
    if assigned(TreeNode.Left) then begin
     if TreeNode.Left.Return.TypeDefinition in [ttdEnumerated,ttdBoolean,ttdSubRange] then begin
      FProcCode.Add('(('+ConvertStdType(TreeNode.Left.Return.SubRangeType)+')');
      FProcCode.Add('(');
      FProcCode.Add('(('+ConvertUnsignedStdType(TreeNode.Left.Return.SubRangeType)+')(');
      TranslateCode(TreeNode.Left);
      FProcCode.Add('))');
      FProcCode.Add('>>',spacesBOTH);
      TranslateCode(TreeNode.Right);
      FProcCode.Add('))');
     end else begin
      FProcCode.Add('(');
      FProcCode.Add('(');
      TranslateCode(TreeNode.Left);
      FProcCode.Add(')');
      FProcCode.Add('>>',spacesBOTH);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(')');
     end;
    end else begin
     Error.InternalError(201302222302001);
    end;
   end;
   ttntMinus:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('(');
     FProcCode.Add('-');
     TranslateCode(TreeNode.Left);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222301001);
    end;
   end;
   ttntNot:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('(');
     if assigned(TreeNode.Return) and (TreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
      FProcCode.Add('!');
     end else begin
      FProcCode.Add('~');
     end;
     TranslateCode(TreeNode.Left);
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222301000);
    end;
   end;
   ttntNil:begin
    FProcCode.Add('null',spacesBOTH);
   end;
   ttntFloat:begin
   end;
   ttntBlock:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.AddLn('{');
     FProcCode.IncTab;
     SubTreeNode:=TreeNode.Left;
     while assigned(SubTreeNode) do begin
      if assigned(SubTreeNode.Right) then begin
//     FProcCode.AddLn('#line '+IntToStr(SubTreeNode.Right.LineNumber)+' "'+SubTreeNode.Right.FileName+'"');
       TranslateCode(SubTreeNode.Right);
       FProcCode.AddLn(';');
      end;
      SubTreeNode:=SubTreeNode.Left;
     end;
     FProcCode.DecTab;
     FProcCode.AddLn('}');
    end else begin
     Error.InternalError(201302222244000);
    end;
   end;
   ttntStatement:begin
    if assigned(TreeNode.Right) then begin
//   FProcCode.AddLn('#line '+IntToStr(TreeNode.LineNumber)+' "'+TreeNode.FileName+'"');
     TranslateCode(TreeNode.Right);
     FProcCode.AddLn(';');
    end;
   end;
   ttntASM:begin
   end;
   ttntVAR:begin
    if assigned(TreeNode.Symbol^.OwnerObjectClass) then begin
     FProcCode.Add('instanceData->');
    end;
    FProcCode.Add(GetSymbolName(TreeNode.Symbol));
   end;
   ttntTYPE:begin
   end;
   ttntTYPECONV:begin
    if assigned(TreeNode.Return) and (TreeNode.Left.Return<>TreeNode.Return) then begin
     case TreeNode.Return^.TypeDefinition of
      ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:begin
       if (not assigned(TreeNode.Left.Return)) or
          (TreeNode.Left.Return^.TypeDefinition<>TreeNode.Return^.TypeDefinition) or
          (TreeNode.Left.Return^.SubRangeType<>TreeNode.Return^.SubRangeType) then begin
        if TreeNode.Return^.TypeDefinition=ttdBoolean then begin
         FProcCode.Add('((');
         FProcCode.Add(ConvertStdType(TreeNode.Return^.SubRangeType));
         FProcCode.Add(')');
         FProcCode.Add('((');
         TranslateCode(TreeNode.Left);
         FProcCode.Add(') ? ');
         if TreeNode.Return^.UpperLimit=$01 then begin
          FProcCode.Add('0x01ul');
         end else if TreeNode.Return^.UpperLimit=$ff then begin
          FProcCode.Add('0xfful');
         end else if TreeNode.Return^.UpperLimit=$ffff then begin
          FProcCode.Add('0xfffful');
         end else if TreeNode.Return^.UpperLimit=$ffffffff then begin
          FProcCode.Add('0xfffffffful');
         end else begin
          FProcCode.Add('0xffffffffffffffffull');
         end;
         FProcCode.Add(' : 0x00ul');
         FProcCode.Add('))');
        end else begin
         FProcCode.Add('((');
         FProcCode.Add(ConvertStdType(TreeNode.Return^.SubRangeType));
         FProcCode.Add(')(');
         TranslateCode(TreeNode.Left);
         FProcCode.Add('))');
        end;
       end else begin
        TranslateCode(TreeNode.Left);
       end;
      end;
      ttdFloat:begin
       if (not assigned(TreeNode.Left.Return)) or
          (TreeNode.Left.Return^.TypeDefinition<>TreeNode.Return^.TypeDefinition) or
          (TreeNode.Left.Return^.FloatType<>TreeNode.Return^.FloatType) then begin
        FProcCode.Add('((');
        FProcCode.Add(ConvertStdType(TreeNode.Return^.FloatType));
        FProcCode.Add(')(');
        TranslateCode(TreeNode.Left);
        FProcCode.Add('))');
       end else begin
        TranslateCode(TreeNode.Left);
       end;
      end;
      ttdPointer:begin
       if assigned(TreeNode.Return^.PointerTo) then begin
        FProcCode.Add('((');
        FProcCode.Add(GetTypeName(TreeNode.Return));
        FProcCode.Add(')((void*)(');
        TranslateCode(TreeNode.Left);
        FProcCode.Add(')))');
       end else begin
        FProcCode.Add('((void*)(');
        TranslateCode(TreeNode.Left);
        FProcCode.Add('))');
       end;
      end;
      else begin
       if SymbolManager.CompatibleEqualTypes(TreeNode.Return,TreeNode.Left.Return) then begin
        TranslateCode(TreeNode.Left);
       end else begin
        FProcCode.Add('(*((');
        FProcCode.Add(GetTypeName(TreeNode.Return));
        FProcCode.Add('*)');
        FProcCode.Add('((void*)(&(');
        TranslateCode(TreeNode.Left);
        FProcCode.Add(')))))');
       end;
      end;
     end;
    end else begin
     TranslateCode(TreeNode.Left);
    end;
   end;
   ttntTYPECHECK:begin
   end;
   ttntFOR:begin
     FProcCode.Add('for(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('; ');
     TranslateCode(TreeNode.Right);
     if TreeNode.IsDownTo then
      FProcCode.Add('; '+GetSymbolName(TreeNode.Left.Left.Symbol)+'--')
     else
      FProcCode.Add('; '+GetSymbolName(TreeNode.Left.Left.Symbol)+'++');
     FProcCode.AddLn('){');

     StartBreakContinuePart;
     FProcCode.IncTab;
     TranslateCode(TreeNode.Block);

     FProcCode.AddLn(';');
     FProcCode.DecTab;
     StopContinuePart;
     FProcCode.Add('}');
     StopBreakPart;
   end;
   ttntWHILE:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('while(');
     TranslateCode(TreeNode.Left);
     FProcCode.AddLn('){');

     StartBreakContinuePart;
     FProcCode.IncTab;
     TranslateCode(TreeNode.Right);
     FProcCode.DecTab;
     StopContinuePart;
     FProcCode.Add('}');
     StopBreakPart;
    end else begin
     Error.InternalError(201302222306000);
    end;
   end;
   ttntREPEAT:begin
    if assigned(TreeNode.Left) then begin

     FProcCode.AddLn('do{');
     StartBreakContinuePart;
     FProcCode.IncTab;
     TranslateCode(TreeNode.Right);
     FProcCode.DecTab;
     StopContinuePart;
     FProcCode.Add('}while(!(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add('))');
     StopBreakPart;
    end else begin
     Error.InternalError(201302222306000);
    end;
   end;
   ttntIF:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('if(');
     TranslateCode(TreeNode.Left);
     FProcCode.AddLn('){');
     FProcCode.IncTab;
     TranslateCode(TreeNode.Right);
     FProcCode.DecTab;
     if assigned(TreeNode.ElseTree) then begin
      FProcCode.AddLn(';');
      FProcCode.AddLn('}else{');
      FProcCode.IncTab;
      TranslateCode(TreeNode.ElseTree);
      FProcCode.DecTab;
     end;
     FProcCode.AddLn(';');
     FProcCode.Add('}');
    end else begin
     Error.InternalError(201302222212000);
    end;
   end;
   ttntWITH:begin
   end;
   ttntCASE:if not Assigned(TreeNode.Left) then begin
    Error.InternalError(201303230327482);
   end else begin
    FProcCode.Add('switch (');
    TranslateCode(TreeNode.Left);
    FProcCode.AddLn('){');
    FProcCode.IncTab;
    SubTreeNode := TreeNode.Right;
    while Assigned(SubTreeNode) do
    begin
     FProcCode.Add('case ');
     TranslateCode(SubTreeNode.Right);
     FProcCode.Add(':');
     SubTreeNode2 := SubTreeNode.Block;
     while Assigned(SubTreeNode.Left) and (not Assigned(SubTreeNode.Left.Block)) do
     begin
      FProcCode.AddLn('');
      FProcCode.Add('case ');
      SubTreeNode := SubTreeNode.Left;
      TranslateCode(SubTreeNode.Right);
      FProcCode.Add(':');
     end;
     FProcCode.AddLn('{');
     FProcCode.IncTab;
     TranslateCode(SubTreeNode2);
     FProcCode.DecTab;
     FProcCode.AddLn('};');
     FProcCode.AddLn('break;');
     SubTreeNode := SubTreeNode.Left;
    end;
    if Assigned(TreeNode.ElseTree) then
    begin
      FProcCode.AddLn('default: {');
      FProcCode.IncTab;
      TranslateCode(TreeNode.ElseTree);
      FProcCode.DecTab;
      FProcCode.AddLn('}');
    end;
    FProcCode.DecTab;
    FProcCode.AddLn('}');
   end;
   ttntCASEBLOCK:begin
   end;
   ttntCASEValue:begin
   end;
   ttntBREAK:begin
    if FNestedBreakCount = 0 then begin
     Error.InternalError(20130324000201)
    end else begin
     if(FBreakLabelNeeded[FNestedBreakCount-1] = -1) then begin
      FBreakLabelNeeded[FNestedBreakCount-1] := FBreakCount;
      Inc(FBreakCount);
     end;
     FProcCode.AddLn('goto '+GetSymbolName(FSelf)+'_BREAKLABEL'+IntToStr(FBreakLabelNeeded[FNestedBreakCount-1])+';');
    end;
   end;
   ttntCONTINUE:begin
    if FNestedBreakCount = 0 then begin
     Error.InternalError(20130324000201)
    end else begin
     if(FContinueLabelNeeded[FNestedBreakCount-1] = -1) then begin
      FContinueLabelNeeded[FNestedBreakCount-1] := FBreakCount;
      Inc(FBreakCount);
     end;
     FProcCode.AddLn('goto '+GetSymbolName(FSelf)+'_CONTINUELABEL'+IntToStr(FContinueLabelNeeded[FNestedBreakCount-1])+';');
    end;
   end;
   ttntEXIT:begin
    if assigned(FSelf.ReturnType) then begin
     FProcCode.Add('return '+GetSymbolName(FSelf.ResultSymbol),spacesBOTH);
    end else begin
     FProcCode.Add('return',spacesBOTH);
    end;
   end;
   ttntLABEL:begin
    FProcCode.AddLn('LABEL_'+GetSymbolName(FSelf)+'_'+TreeNode.LabelName+':')
   end;
   ttntGOTO:begin
     FProcCode.Add('goto LABEL_'+GetSymbolName(FSelf)+'_'+TreeNode.LabelName);
   end;
   ttntTRY:begin
   end;
   ttntTRYONELSE:begin
   end;
   ttntRAISE:begin
   end;
   ttntPROCEDURE:begin
    case TreeNode.Symbol.InternalProcedure of
     tipNone: ;
     tipWRITE: ;
     tipWRITELN: ;
     tipREAD: ;
     tipREADLN: ;
     tipSIZEOF: ;
     tipDEC: ;
     tipINC: ;
     tipSUCC: ;
     tipPRED: ;
     tipORD: ;
     tipCHR: ;
     tipNEW: ;
     tipDISPOSE: ;
     tipSETLENGTH: ;
     tipLENGTH: ;
     tipASSIGNED: ;
    end;
   end;
   ttntCALL:begin
    if assigned(TreeNode.Symbol) then begin
     case TreeNode.Symbol.InternalProcedure of
      tipWRITE, tipWRITELN:begin
       SubTreeNode := TreeNode.Left;
       // todo: check if first element is Text-type

       while assigned(SubTreeNode) and (SubTreeNode.TreeNodeType=ttntParameter) do begin
        if SubTreeNode.Colon then begin
         Error.InternalError(201303210010000);
         break;
        end else begin
         if Assigned(SubTreeNode.Left.Return) then
          case SubTreeNode.Left.Return.TypeDefinition of
           //ttdEmpty: FProcCode.AddLn('// Empty');
           //ttdEnumerated: FProcCode.AddLn('// Enumeration');
           //ttdCurrency: FProcCode.AddLn('// Currency');
           //ttdVariant: FProcCode.AddLn('// Variant');
           //ttdArray: FProcCode.AddLn('// Array');
           //ttdRecord: FProcCode.AddLn('// Record');
           //ttdFile: FProcCode.AddLn('// File');
           //ttdPointer: FProcCode.AddLn('// Pointer');
           //ttdProcedure: FProcCode.AddLn('// Procedure');
           //ttdObject: FProcCode.AddLn('// Object');
           //ttdClass: FProcCode.AddLn('// Class');
           //ttdClassRef: FProcCode.AddLn('// ClassRef');
           //ttdInterface: FProcCode.AddLn('// Interface');
           //ttdSet: FProcCode.AddLn('// Set');
           //ttdCExpression: FProcCode.AddLn('// C-expression');
           ttdSubRange:
           begin
            case SubTreeNode.Left.Return.SubRangeType of
             tstSigned8Bit,
             tstSigned16Bit,
             tstSigned32Bit,
             tstSigned64Bit: FProcCode.Add('pasWriteInt(');
             tstUnsigned8Bit,
             tstUnsigned16Bit,
             tstUnsigned32Bit,
             tstUnsigned64Bit: FProcCode.Add('pasWriteUInt('); // WRONG! Should call an unsigned write-function
             tstUnsignedChar,
             tstUnsignedWideChar,
             tstUnsignedHugeChar: FProcCode.Add('pasWriteChar(');
             tstFloat32Bit,
             tstFloat64Bit,
             tstFloat80Bit: FProcCode.Add('pasWriteFloat(');
            end;
           end;
           ttdBoolean: FProcCode.Add('pasWriteBool(');
           ttdShortString: FProcCode.Add('pasWriteShortString(');
           ttdLongString: FProcCode.Add('pasWriteLongString(');
           ttdFloat: FProcCode.Add('pasWriteFloat(');
           else
            Error.InternalError(201303210020000);
          end
         else
          Error.InternalError(201303210030000);
         if SubTreeNode.ReferenceParameter then begin
          FProcCode.Add('((void*)(&(');
         end else if assigned(SubTreeNode.Left.Return) and (SubTreeNode.Left.Return^.TypeDefinition=ttdPointer) then begin
          FProcCode.Add('((void*)(');
         end;
         TranslateCode(SubTreeNode.Left);
         if SubTreeNode.ReferenceParameter then begin
          FProcCode.Add(')))');
         end else if assigned(SubTreeNode.Left.Return) and (SubTreeNode.Left.Return^.TypeDefinition=ttdPointer) then begin
          FProcCode.Add('))');
         end;
         SubTreeNode:=SubTreeNode.Right;
         FProcCode.AddLn(');');
         if assigned(SubTreeNode) and (SubTreeNode.TreeNodeType=ttntParameter) then begin
//          FProcCode.Add(',',spacesRIGHT);
         end else begin
          break;
         end;
        end;
       end;
       if TreeNode.Symbol.InternalProcedure = tipWRITELN then
        FPRocCode.AddLn('printf("\n");');
      end;
      tipREAD:begin
       // TODO: Implement it!
      end;
      tipREADLN:begin
       // TODO: Implement it!
      end;
      tipSIZEOF:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        // TODO: Complete it with type-variants with VMT handling! See Delphi 7 internal data structure reference!
        FProcCode.Add('(sizeof(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('))');
       end else begin
        Error.InternalError(201303130453002);
       end;
      end;
      tipDEC:begin
       // TODO: Check it, if it is right!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('--)');
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and not assigned(TreeNode.Left.Right.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('-=',spacesBOTH);
        TranslateCode(TreeNode.Left.Right.Left);
        FProcCode.Add(')',spacesLEFT);
       end else begin
        Error.InternalError(201303130453000);
       end;
      end;
      tipINC:begin
       // TODO: Check it, if it is right!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('++)');
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and not assigned(TreeNode.Left.Right.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('+=',spacesBOTH);
        TranslateCode(TreeNode.Left.Right.Left);
        FProcCode.Add(')',spacesLEFT);
       end else begin
        Error.InternalError(201303130453001);
       end;
      end;
      tipSUCC:begin
       // TODO: Check it, if it is right!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('+ 1)',spacesLEFT);
       end else begin
        Error.InternalError(201303130450000);
       end;
      end;
      tipPRED:begin
       // TODO: Check it, if it is right!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('- 1)',spacesLEFT);
       end else begin
        Error.InternalError(201303130450001);
       end;
      end;
      tipORD:begin
       // TODO: Complete it with correct type-variant handling!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add(')',spacesLEFT);
       end else begin
        Error.InternalError(201303130451000);
       end;
      end;
      tipCHR:begin
       // TODO: Complete it with correct type-variant handling!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add(')',spacesLEFT);
       end else begin
        Error.InternalError(201303130451001);
       end;
      end;
      tipNEW:begin
       // TODO: Implement it!
      end;
      tipDISPOSE:begin
       // TODO: Implement it!
      end;
      tipSETLENGTH:begin
       // TODO: Implement it!
      end;
      tipLENGTH:begin
       // TODO: Implement it!
       if assigned(TreeNode.Left) and Assigned(TreeNode.Left.Left) then
       begin
        if Assigned(TreeNode.Left.Left.Return) then
         case TreeNode.Left.Left.Return.TypeDefinition of
          ttdLongString: begin
           FProcCode.Add('LengthLongstring(');
           TranslateCode(TreeNode.Left.Left);
           FProcCode.Add(')');
          end;
          else
           Error.InternalError(20130321033001);
         end
        else
         Error.InternalError(201303210323837);
       end;
      end;
      tipASSIGNED:begin
       // TODO: Check it, if it is right!
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('!= NULL)',spacesLEFT);
       end else begin
        Error.InternalError(201303130449000);
       end;
      end;
      tipTRUNC:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        // TODO: Complete it with type-variants!
        FProcCode.Add('(pasTRUNC(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('))',spacesLEFT);
       end else begin
        Error.InternalError(201303130455000);
       end;
      end;
      tipROUND:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        // TODO: Complete it with type-variants!
        FProcCode.Add('(pasROUND(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('))',spacesLEFT);
       end else begin
        Error.InternalError(201303130455001);
       end;
      end;
      tipSQR:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        // TODO: Complete it with type-variants!
        FProcCode.Add('(pasSQR(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('))',spacesLEFT);
       end else begin
        Error.InternalError(201303130455002);
       end;
      end;
      tipSQRT:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and not assigned(TreeNode.Left.Right) then begin
        // TODO: Complete it with type-variants!
        FProcCode.Add('(pasSQRT(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.Add('))',spacesLEFT);
       end else begin
        Error.InternalError(201303130455002);
       end;
      end;
      else {tipNone:}begin
       if assigned(TreeNode.Symbol.OwnerObjectClass) then begin
        FProcCode.Add(GetSymbolName(TreeNode.Symbol));
       end else begin
        if assigned(TreeNode.Symbol.OwnerModule) then begin
         if assigned(TreeNode.MethodSymbol) then begin
          FProcCode.Add(GetSymbolName(TreeNode.MethodSymbol));
         end else begin
          FProcCode.Add(GetSymbolName(TreeNode.Symbol));
         end;
        end;
       end;
      end;
      FProcCode.Add('(');
      HaveParameters:=false;
      if TreeNode.Symbol^.LexicalScopeLevel>0 then begin
       if TreeNode.Symbol^.LexicalScopeLevel>1 then begin
        FProcCode.Add('nestedLevelStack');
       end else begin
        FProcCode.Add('(void**)&nestedLevelStack');
       end;
       HaveParameters:=true;
      end;
      if assigned(FProcSymbol) and
         assigned(TreeNode.Symbol^.OwnerObjectClass) and
         (TreeNode.Symbol.OwnerObjectClass=FProcSymbol.OwnerObjectClass) and
         not assigned(TreeNode.MethodSymbol) then begin
       if HaveParameters then
       begin
        FProcCode.Add(',',spacesRIGHT);
       end;
       FProcCode.Add('(void*)instanceData');
       HaveParameters:=true;
      end else if assigned(TreeNode.MethodSymbol) and assigned(TreeNode.Symbol^.TypeDefinition) then begin
       if HaveParameters then
       begin
        FProcCode.Add(',',spacesRIGHT);
       end;
       if TreeNode.Symbol^.TypeDefinition^.TypeDefinition=ttdOBJECT then begin
        // OBJECT
        FProcCode.Add('((void*)&('+GetSymbolName(TreeNode.Symbol)+'))');
       end else begin
        // CLASS
        FProcCode.Add('(void*)'+GetSymbolName(TreeNode.Symbol));
       end;
       HaveParameters:=true;
      end;
      if HaveParameters and Assigned(TreeNode.Left) then
      begin
        FProcCode.Add(',',spacesRIGHT);
      end;
      // Parameters in left
      TranslateCode(TreeNode.Left);
      FProcCode.Add(')');
      //FProcCode.AddLn(';');
      end;
     end;
   end;
   ttntParameter:begin
    SubTreeNode:=TreeNode;
    while assigned(SubTreeNode) and (SubTreeNode.TreeNodeType=ttntParameter) do begin
     if SubTreeNode.Colon then begin
      Error.InternalError(201303010430000);
      break;
     end else begin
      if SubTreeNode.ReferenceParameter then begin
       FProcCode.Add('((void*)(&(');
      end else if assigned(SubTreeNode.Left.Return) and (SubTreeNode.Left.Return^.TypeDefinition=ttdPointer) then begin
       FProcCode.Add('((void*)(');
      end;
      TranslateCode(SubTreeNode.Left);
      if SubTreeNode.ReferenceParameter then begin
       FProcCode.Add(')))');
      end else if assigned(SubTreeNode.Left.Return) and (SubTreeNode.Left.Return^.TypeDefinition=ttdPointer) then begin
       FProcCode.Add('))');
      end;
      SubTreeNode:=SubTreeNode.Right;
      if assigned(SubTreeNode) and (SubTreeNode.TreeNodeType=ttntParameter) then begin
       FProcCode.Add(',',spacesRIGHT);
      end else begin
       break;
      end;
     end;
    end;
   end;
   ttntIndex:begin
    TranslateCode(TreeNode.Left);
    FProcCode.Add('[');
    TranslateCode(TreeNode.Right);
    FProcCode.Add(']');
   end;
   ttntPointer:begin
    FProcCode.Add('(*(');
    TranslateCode(TreeNode.Left);
    FProcCode.Add('))');
   end;
   ttntAddress:begin
    FProcCode.Add('(&(');
    TranslateCode(TreeNode.Left);
    FProcCode.Add('))');
   end;
   ttntField:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('',spacesLEFT);
     if TreeNode.Left.TreeNodeType = ttntPointer then
     begin
       // pointer type field access in c is "myPointerTypeVar->myFieldEntry"
       TranslateCode(TreeNode.Left.Left);
       FProcCode.Add('->');
     end else
     begin
       TranslateCode(TreeNode.Left);
       FProcCode.Add('.');
     end;
     if assigned(TreeNode.SymbolField) then begin
      FProcCode.Add(GetSymbolName(TreeNode.SymbolField));
     end else begin
      Error.InternalError(201302222321000);
     end;
     FProcCode.Add('',spacesRIGHT);
    end else begin
     Error.InternalError(201302222319000);
    end;
   end;
   ttntORDConst:begin
    if TreeNode.Return.TypeDefinition in [ttdEnumerated,ttdBoolean,ttdSubRange] then begin
     case TreeNode.Return.SubRangeType of
      tstSigned8Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstSigned16Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstSigned32Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value)+'l',spacesBOTH);
      end;
      tstSigned64Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value)+'ll',spacesBOTH);
      end;
      tstUnsigned8Bit,tstUnsignedChar:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstUnsigned16Bit,tstUnsignedWideChar:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstUnsigned32Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value)+'ul',spacesBOTH);
      end;
      tstUnsigned64Bit:begin
       FProcCode.Add(IntToStr(TreeNode.Value)+'ull',spacesBOTH);
      end;
      else begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
     end;
    end else begin
     FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
    end;
   end;
   ttntCHARConst:begin
    FProcCode.Add(IntToStr(TreeNode.CharValue),spacesBOTH);
   end;
   ttntSTRINGConst:begin
    FProcCode.Add(TranslateStringConstant(TreeNode.StringData),spacesBOTH);
   end;
   ttntFloatConst:begin
    Str(TreeNode.FloatValue,s);
    FProcCode.Add(s,spacesBOTH);
   end;
   ttntSETConst:begin
   end;
   ttntPCHARConst:begin
   end;
   ttntCEXPRESSION:begin
    FProcCode.Add('(',spacesBOTH);
    FProcCode.Add(HugeStringToWideString(TreeNode.StringData),spacesBOTH);
    FProcCode.Add(')',spacesBOTH);
   end;
   else begin
    Error.InternalError(201302222201000);
   end;
  end;
 end;
end;

procedure TCodegenCPP.TranslateClass(Symbol: PSymbol; Target: TCodeWriter);
begin
 Target.AddLn('//class ' + Symbol.Name);
end;

procedure TCodegenCPP.TranslateConstant(Symbol: PSymbol; Target: TCodeWriter);
begin
 if Target = FHeader then
 begin
  Target.Add('extern const ' + GetSymbolName(Symbol) + ';// = ' + ConvertConstSymbol(Symbol));
  FCode.AddLn('const ' + GetSymbolName(Symbol) + ' = ' + ConvertConstSymbol(Symbol)+';');
 end else
 Target.Add('const ' + GetSymbolName(Symbol) + ' = ' + ConvertConstSymbol(Symbol));
end;

procedure TCodegenCPP.TranslateConstants;
var cons: PConstant;
begin
 cons := SymbolManager.ConstantList.First;
end;

procedure TCodegenCPP.TranslateFunction(Symbol: PSymbol; Target: TCodeWriter);
var Sym: PSymbol;
    s: ansistring;
begin
 if Symbol.InternalProcedure<>tipNone then
   exit;
 if (tpaExternal in Symbol.ProcedureAttributes) then
 begin
  if Pos('.h', Symbol.LibraryName)>0 then
  begin
   Target.AddInclude(Symbol.LibraryName);

   // Target.Add('__inline ');
   ProcessTypeOrName(Symbol.ReturnType, Target);
   Target.Add(' '+GetSymbolName(Symbol)+'(');
   s:='';

   if not Assigned(Symbol.Parameter) then
    Sym := nil
   else
    Sym:=Symbol.Parameter.First;
   while Assigned(sym) do
   begin
    if Sym <> Symbol.Parameter.First then
    begin
     Target.Add(',',spacesRIGHT);
     s:=s+',';
    end;
    ProcessTypeOrName(Sym.TypeDefinition,Target);
    if IsSymbolReference(Sym) then begin
     Target.Add('*',spacesRIGHT);
     s:=s+'*';
    end;
    Target.Add(GetSymbolName(Sym),spacesLEFT);
    s:=s+' '+GetSymbolName(Sym);
    Sym := Sym.Next;
   end;
   Target.AddLn('){');
   Target.IncTab;
   if Assigned(Symbol.ReturnType) then
    Target.Add('return ');

   Target.Add(Symbol.ExternalName);
// Target.Add(Copy(Symbol.OverloadedName, 1, pos('_S_', Symbol.OverloadedName)-1));
//   Target.Add(Copy(Symbol.OverloadedName, 2, pos('_S_', Symbol.OverloadedName)-2));
   Target.Add('(' + s + ')');
   Target.AddLn(';');
   Target.DecTab;
   Target.Add('}');
   Exit;
  end;
  Target.Add('typedef ');
  ProcessFunctionType(Symbol.ReturnType, Symbol.Parameter, 'PROC_'+Symbol.OverloadedName, Target);
  Target.AddLn(';');
  if Target = FHeader then
  begin
   Target.AddLn('extern PROC_'+Symbol.OverloadedName+' '+Symbol.OverloadedName+';');
   FCode.AddLn('PROC_'+Symbol.OverloadedName+' '+Symbol.OverloadedName+';');
  end else
   Target.Add('PROC_'+Symbol.OverloadedName+' '+Symbol.OverloadedName+';');
  Exit;
 end;

 if (Target <> FCode) and not ((Target = FProcCode) and (Symbol^.OwnerModule = FSelf)) then
 repeat
  Target.Add('extern ',spacesRIGHT);
  ConvertFuncSymbol(Symbol, Target);
  Symbol := Symbol.NextOverloaded;
  if Assigned(Symbol) then
   Target.AddLn(';');
 until not Assigned(Symbol);
end;

procedure TCodegenCPP.TranslateObject(Symbol: PSymbol; Target: TCodeWriter);
begin
  Target.AddLn('//object ' + Symbol.Name);

end;

procedure TCodegenCPP.TranslateProcedure(Symbol: PSymbol; Target: TCodeWriter);
begin
 TranslateFunction(Symbol, Target);
end;

procedure TCodegenCPP.TranslateTemp(Symbol: PSymbol; Target: TCodeWriter);
begin
 Target.AddLn('//Reg ' + Symbol.Name);
end;

function TCodegenCPP.TranslateStringConstant(
  ConstantStr: THugeString): ansistring;

var AStr: ansistring;

  function UIntToCString(const Value: Cardinal): ansistring;
  begin
    result := '\x'+IntToHex(Byte(Value div $1000000), 2) +
              '\x'+IntToHex(Byte(Value div $10000), 2) +
              '\x'+IntToHex(Byte(Value div $100), 2) +
              '\x'+IntToHex(Byte(Value), 2);
  end;

begin
  result := GetSymbolName(FSelf) +'_STRING_CONST_'+INTTOSTR(FStringConstCount);
  Inc(FStringConstCount);

  AStr := HugeStringToAnsiString(ConstantStr);

  FProcCode.InsertAtMark('static const char '+result+'_DATA['+IntToStr(Length(AStr)+17)+'] = "' + UIntToCString(65535)+UIntToCSTring(1)
                         +UIntToCString($FFFFFFFF)+UIntToCString(Length(AStr))+
                         AnsiStringEscape(AStr,False)+'\x00";');
  FProcCode.InsertAtMark('void* '+result+' = (void*)(&'+result+'_DATA[16]);');
end;

procedure TCodegenCPP.TranslateSymbolList(List: TSymbolList; IgnoreTypes: boolean; ATarget: TCodeWriter = nil);
var sym: PSymbol;
    Target: TCodeWriter;
    LastProcStruct:boolean;
begin
 inc(FDepth);

 sym := List.First;

 while Assigned(sym) do
 begin
  if Assigned(ATarget) then
   Target := ATarget
  else if (tsaPublicUnitSymbol in sym.Attributes) then
   Target := FHeader
  else
   Target := FCode;

  LastProcStruct:=false;

  case Sym.SymbolType of
   tstLabel: ; // labels dont need declarations
   tstConstant: ; // TranslateConstant(Sym, Target);
   tstVariable:begin
    if FNeedNestedStack and assigned(FProcSymbol) and assigned(Sym^.LocalProcSymbol) and Sym^.LocalProcSymbolAccessedFromHigherNestedProc then begin
     LastProcStruct:=true;
     TranslateVariable(Sym, FProcStruct);
     inc(FProcStructCount);
    end else begin
     TranslateVariable(Sym, Target);
    end;
   end;
   tstType: ;
   tstProcedure: TranslateProcedure(Sym, Target);
   tstFunction: TranslateFunction(Sym, Target);
   tstUnit: TranslateUnit(Sym, Target);
   tstTemp: TranslateTemp(Sym, Target);
  end;

  if Sym.SymbolType<>tstUnit then
    if LastProcStruct then
      FProcStruct.AddLn(';')
     else
      Target.AddLn(';');

  sym := sym.Next;
 end;

 dec(FDepth);
end;

procedure TCodegenCPP.ProcessTypedConstant(var Constant: PConstant; AType: PType;
  Target: TCodeWriter);
var i:integer;
    Sym: PSymbol;
begin
 while Assigned(Constant) and (Constant.ConstantType = tctAlign) do
  Constant:=Constant.Next;

 if not Assigned(Constant) then
  Exit;

 case AType.TypeDefinition of
  ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:
   Target.Add(IntToStr(Constant.IntValue));

  ttdArray:
  begin
   Target.Add('{');
   for i:=0 to AType^.Range^.UpperLimit-AType^.Range^.LowerLimit do
   begin
    if i<>0 then
     Target.Add(',',spacesRIGHT);
    ProcessTypedConstant(Constant,AType^.Definition,Target);
    Constant:=Constant.Next;
   end;
   Target.Add('}');
  end;

  ttdRecord:
  begin
   Target.Add('{');
   Sym:=AType^.RecordTable.First;
   while Assigned(Sym) do
   begin
    // Target.Add(Sym.OwnerModule.Name+Sym.Name+':');
    ProcessTypedConstant(Constant,Sym.TypeDefinition,Target);
    Sym:=Sym.Next;
    Constant:=Constant.Next;
    if Assigned(Sym) then
     Target.Add(',',spacesRIGHT);
   end;
   Target.Add('}');
  end;

  ttdShortString: Target.Add(AnsiStringEscape(Constant.ShortStringValue),spacesBOTH);
  ttdLongString:begin
   case AType^.LongStringType of
    tstUnsignedChar:begin
     Target.Add('CreateLongstring('+IntToStr(AType^.LongStringCodePage)+', 1, '+IntToStr(Length(Constant.StringValue))+', '+AnsiStringEscape(HugeStringToAnsiString(Constant.StringValue))+')',spacesLEFT);
    end;
    tstUnsignedWideChar:begin
     Target.Add('CreateLongstring('+IntToStr(AType^.LongStringCodePage)+', 2, '+IntToStr(Length(Constant.StringValue))+', L'+WideStringEscape(HugeStringToWideString(Constant.StringValue))+')',spacesLEFT);
    end;
    tstUnsignedHugeChar:begin
     // TODO
    end;
   end;
  end;
  ttdPointer: Target.Add(AnsiStringEscape(HugeStringToAnsiString(Constant.StringValue)),spacesBOTH); // is it safe to assume that all pointer typed constants are strings?
  ttdVariant: ;
  ttdFloat: Target.Add(FloatToStr(Constant.FloatValue));
  ttdCExpression: ; // IGNORE!!!
 end;
end;

procedure TCodegenCPP.ProcessTypeOrName(AType: PType; Target: TCodeWriter; OwnType: PType = nil);
var Sym: PSymbol;
begin
 if not Assigned(AType) then
 begin
   Target.Add('void');
   Exit;
 end;

 Inc(FDepth);
 if assigned(AType) and (AType^.TypeDefinition=ttdPointer) and assigned(AType^.PointerTo) and (AType^.PointerTo^.TypeDefinition=OwnType) then begin
   Target.Add('void*');
 end
 else if Assigned(Sym) and (AType.TypeDefinition = ttdEmpty) then
 begin
   Target.Add('void');
 end
 else
 begin
   Sym := FindSymbol(AType);
   if Assigned(Sym) and (AType.TypeDefinition <> ttdPointer) then
    Target.Add(GetSymbolName(Sym))
   else
    Target.Add(GetTypeName(AType));
 end;
 Dec(FDepth);
end;

procedure TCodegenCPP.TranslateUnit(Symbol: PSymbol; Target: TCodeWriter);
begin
  Target.AddLn('//unit ' + Symbol.Name);
  Target.AddInclude(CorrectSymbolName(Symbol.Name)+'.h');
end;

procedure TCodegenCPP.TranslateVariable(Symbol: PSymbol; Target: TCodeWriter);
var TypeSym: PSymbol;
    cons: PConstant;
begin
 TypeSym := FindSymbol(Symbol.TypeDefinition);

 if FDepth = 1 then
 begin
  if Symbol.TypeDefinition.TypeDefinition = ttdPointer then
  begin
   FCode.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
  end else if Assigned(TypeSym) then
   FCode.Add(GetSymbolName(TypeSym)+' '+GetSymbolName(Symbol))
  else begin
   FCode.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
  end;

  if Target = FHeader then
  begin
   if Assigned(TypeSym) and (Symbol.TypeDefinition.TypeDefinition <> ttdPointer) then
    FHeader.Add('extern '+GetSymbolName(TypeSym)+' '+GetSymbolName(Symbol))
   else begin
    FHeader.Add('extern ');
    if Symbol.TypeDefinition.TypeDefinition = ttdArray then
    begin
     FHeader.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
    end
    else begin
     FHeader.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
    end;
   end;
  end;

  if Symbol.TypedConstant then
  begin
    if Symbol.TypeDefinition.TypeDefinition = ttdLongString then
     //FCode.Add('; '+GetSymbolName(Symbol)+' = ')
    else begin
     FCode.Add('=',spacesBOTH);
     cons := Symbol.Constant;
     ProcessTypedConstant(cons, Symbol.TypeDefinition, FCode);
    end;
  end else if
  Symbol.TypeDefinition.TypeDefinition = ttdLongString then
   FCode.Add('= NULL',spacesLEFT);

  if Target = FHeader then
   FCode.AddLn(';');

 end else
 begin
  if Symbol.TypeDefinition.TypeDefinition = ttdPointer then
  begin
   Target.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
  end else if Assigned(TypeSym) then
   Target.Add(GetSymbolName(TypeSym)+' '+GetSymbolName(Symbol))
  else begin
   Target.Add(GetTypeName(Symbol.TypeDefinition)+' '+GetSymbolName(Symbol));
  end;
 end;
end;

procedure TCodegenCPP.SaveToStreams(CodeStream, HeaderStream: TBeRoStream);
begin
  FCode.ExportStream(CodeStream);
  CodeStream.Append(FProcCode.FData);
  FHeader.ExportStream(HeaderStream);
end;

procedure TCodegenCPP.StartBreakContinuePart;
begin
 inc(FNestedBreakCount);
 SetLength(FBreakLabelNeeded, FNestedBreakCount);
 SetLength(FContinueLabelNeeded, FNestedBreakCount);
 FBreakLabelNeeded[FNestedBreakCount-1] := -1;
 FContinueLabelNeeded[FNestedBreakCount-1] := -1;
end;

procedure TCodegenCPP.StopBreakPart;
begin
 if FBreakLabelNeeded[FNestedBreakCount-1] <> -1 then begin
  FProcCode.AddLn(';');
  FProcCode.AddLn(GetSymbolName(FSelf)+'_BREAKLABEL'+IntToStr(FBreakLabelNeeded[FNestedBreakCount-1])+': ;');
 end;
 dec(FNestedBreakCount);
 SetLength(FBreakLabelNeeded, FNestedBreakCount);
 SetLength(FContinueLabelNeeded, FNestedBreakCount);
end;

procedure TCodegenCPP.StopContinuePart;
begin
 if FContinueLabelNeeded[FNestedBreakCount-1] <> -1 then begin
  FProcCode.AddLn(';');
  FProcCode.AddLn(GetSymbolName(FSelf)+'_CONTINUELABEL'+IntToStr(FContinueLabelNeeded[FNestedBreakCount-1])+': ;');
 end;
end;

function TCodegenCPP.AnsiStringEscape(const Input: ansistring; Quotes: Boolean = True): ansistring;
var Counter: Integer;
    c: Ansichar;
begin
 if Quotes then
  Result:='"'
 else
  Result:='';
 for Counter:=1 to length(Input) do begin
  C:=Input[Counter];
  case C of
   #0:Result:=Result+'\0';
   #7:Result:=Result+'\b';
   #8:Result:=Result+'\u';
   #9:Result:=Result+'\t';
   #10:Result:=Result+'\n';
   #13:Result:=Result+'\r';
   #1..#6,#11..#12,#14..#31:Result:=Result+'\x'+ByteToHex(byte(C));
   '''','"','\':Result:=Result+'\'+C;
   else Result:=Result+C;
  end;
 end;
 if Quotes then
  Result:=Result+'"';
end;

function TCodegenCPP.WideStringEscape(const Input: widestring): ansistring;
var Counter: Integer;
    c: widechar;
begin
 Result:='"';
 for Counter:=1 to length(Input) do begin
  C:=Input[Counter];
  case C of
   #0:Result:=Result+'\0';
   #7:Result:=Result+'\b';
   #8:Result:=Result+'\u';
   #9:Result:=Result+'\t';
   #10:Result:=Result+'\n';
   #13:Result:=Result+'\r';
   #1..#6,#11..#12,#14..#31:Result:=Result+'\x'+ByteToHex(word(c) and $ff);
   '''','"','\':Result:=Result+'\'+C;
   #128..#65535:Result:=Result+'\X'+ByteToHex(word(c) shr 8)+ByteToHex(word(c) and $ff);
   else Result:=Result+C;
  end;
 end;
 Result:=Result+'"';
end;

procedure TCodegenCPP.TranslateModuleTypes(ModuleSymbol:PSymbol;Target:TCodeWriter);
type TTypeItems=array of PType;
var TypeItems:TTypeItems;
 procedure SortTypes;
 type TDependencyItem=record
       Visited:boolean;
       Key,Order:longint;
       Dependencies:TIntegerList;
      end;
      TDependencyItems=array of TDependencyItem;
 var DependencyItems:TDependencyItems;
     DependencyItemCount:longint;
     DependencyOrder:array of longint;
     OrderCount:longint;
     CopyTypeItems:TTypeItems;
  procedure TopoSort;
  type PLeader=^TLeader;
       PTrailer=^TTrailer;
       TLeader=record
        Key:longint;
        Count:longint;
        Trail:PTrailer;
        Next:PLeader;
        ListNext:PLeader;
       end;
       TTrailer=record
        ID:PLeader;
        Next:PTrailer;
        ListNext:PTrailer;
       end;
  var Head,Tail,RootLeader:PLeader;
      Trailer,RootTrailer:PTrailer;
      KeyCount:longint;
   function CreateReferenceToLeader(Key:longint):PLeader;
   begin
    result:=Head;
    Tail^.Key:=Key;
    while result^.Key<>Key do begin
     result:=result^.Next;
    end;
    if result=Tail then begin
     New(Tail);
     FillChar(Tail^,SizeOf(TLeader),#0);
     Tail^.ListNext:=RootLeader;
     RootLeader:=Tail;
     result^.Count:=0;
     result^.Trail:=nil;
     result^.Next:=Tail;
     inc(KeyCount);
    end;
   end;
  var Index,Counter:longint;
      CurrentLeader,KeyLeader,NextLeader,LeaderHasDependencyToID,LeaderThisID:PLeader;
      NextTrailer:PTrailer;
  begin
   begin
    New(Head);
    FillChar(Head^,SizeOf(TLeader),#0);
    Tail:=Head;
    RootLeader:=Head;
    RootTrailer:=nil;
    KeyCount:=0;
   end;
   for Index:=DependencyItemCount-1 downto 0 do begin
    LeaderThisID:=CreateReferenceToLeader(Index);
    for Counter:=0 to DependencyItems[Index].Dependencies.Count-1 do begin
     LeaderHasDependencyToID:=CreateReferenceToLeader(DependencyItems[Index].Dependencies[Counter]);
     New(Trailer);
     FillChar(Trailer^,SizeOf(TTrailer),#0);
     Trailer^.ID:=LeaderThisID;
     Trailer^.Next:=LeaderHasDependencyToID^.Trail;
     Trailer^.ListNext:=RootTrailer;
     RootTrailer:=Trailer;
     LeaderHasDependencyToID^.Trail:=Trailer;
     inc(LeaderThisID^.Count);
    end;
   end;
   begin
    CurrentLeader:=Head;
    Head:=nil;
    while CurrentLeader<>Tail do begin
     KeyLeader:=CurrentLeader;
     CurrentLeader:=CurrentLeader^.Next;
     if KeyLeader^.Count=0 then begin
      KeyLeader^.Next:=Head;
      Head:=KeyLeader;
     end;
    end;
   end;
   begin
    OrderCount:=0;
    KeyLeader:=Head;
    while assigned(KeyLeader) do begin
     DependencyItems[KeyLeader^.Key].Order:=OrderCount;
     DependencyOrder[OrderCount]:=KeyLeader^.Key;
     inc(OrderCount);
     dec(KeyCount);
     Trailer:=KeyLeader^.Trail;
     KeyLeader:=KeyLeader^.Next;
     while assigned(Trailer) do begin
      CurrentLeader:=Trailer^.ID;
      dec(CurrentLeader^.Count);
      if CurrentLeader^.Count=0 then begin
       CurrentLeader^.Next:=KeyLeader;
       KeyLeader:=CurrentLeader;
      end;
      Trailer:=Trailer^.Next;
     end;
    end;
    if KeyCount<>0 then begin
    end;
   end;
   begin
    while assigned(RootLeader) do begin
     NextLeader:=RootLeader^.ListNext;
     Dispose(RootLeader);
     RootLeader:=NextLeader;
    end;
    while assigned(RootTrailer) do begin
     NextTrailer:=RootTrailer^.ListNext;
     Dispose(RootTrailer);
     RootTrailer:=NextTrailer;
    end;
   end;
  end;
  procedure DependencySort;
  var Tries,Counter,SubCounter,Order,Index,SubIndex:longint;
      NeedNewPass:boolean;
  begin
   OrderCount:=0;
   for Index:=0 to DependencyItemCount-1 do begin
    DependencyItems[Index].Order:=Index;
    DependencyOrder[Index]:=Index;
   end;
   NeedNewPass:=false;
   for Tries:=0 to DependencyItemCount-1 do begin
    NeedNewPass:=false;
    for Index:=0 to DependencyItemCount-1 do begin
     Order:=DependencyItems[Index].Order;
     for SubCounter:=0 to DependencyItems[Index].Dependencies.Count-1 do begin
      SubIndex:=DependencyItems[Index].Dependencies[SubCounter];
      if DependencyItems[SubIndex].Order>Order then begin
       for Counter:=Order to DependencyItems[SubIndex].Order-1 do begin
        inc(DependencyItems[DependencyOrder[Counter]].Order);
       end;
       DependencyItems[SubIndex].Order:=Order;
       for Counter:=0 to DependencyItemCount-1 do begin
        DependencyOrder[DependencyItems[Counter].Order]:=Counter;
       end;
       Order:=DependencyItems[Index].Order;
       NeedNewPass:=true;
      end;
     end;
    end;
   end;
   if NeedNewPass then begin
    TopoSort;
   end;
  end;
 var Counter,Index:longint;
     Type_:PType;
     TypePointerList:TPointerList;
     SymbolList:TSymbolList;
     Symbol:PSymbol;
 begin
  CopyTypeItems:=nil;
  DependencyItems:=nil;
  DependencyOrder:=nil;
  try
   TypePointerList:=TPointerList.Create;
   try
    DependencyItemCount:=length(TypeItems);
    if DependencyItemCount>0 then begin
     SetLength(DependencyItems,DependencyItemCount);
     SetLength(DependencyOrder,DependencyItemCount);
     for Counter:=0 to DependencyItemCount-1 do begin
      DependencyItems[Counter].Key:=Counter;
      DependencyItems[Counter].Dependencies:=TIntegerList.Create;
      DependencyItems[Counter].Visited:=false;
      DependencyItems[Counter].Order:=Counter;
      DependencyOrder[Counter]:=Counter;
      TypePointerList.Add(TypeItems[Counter]);
     end;
     for Counter:=0 to DependencyItemCount-1 do begin
      Type_:=TypeItems[Counter];
      case Type_^.TypeDefinition of
       ttdPointer:begin
        if assigned(Type_^.PointerTo) then begin
         Index:=TypePointerList.IndexOf(Type_^.PointerTo^.TypeDefinition);
         if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
          DependencyItems[Counter].Dependencies.Add(Index);
         end;
        end;
       end;
       ttdArray:begin
        if assigned(Type_^.Definition) and not (((Type_^.Definition^.TypeDefinition=ttdPointer) and assigned(Type_^.Definition^.PointerTo)) and (Type_^.Definition^.PointerTo^.TypeDefinition=Type_)) then begin
         Index:=TypePointerList.IndexOf(Type_^.Definition);
         if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
          DependencyItems[Counter].Dependencies.Add(Index);
         end;
        end;
       end;
       ttdRecord,ttdObject,ttdClass,ttdInterface:begin
        SymbolList:=Type_^.RecordTable;
        if assigned(SymbolList) then begin
         Symbol:=SymbolList.First;
         while assigned(Symbol) do begin
          case Symbol^.SymbolType of
           Symbols.tstVariable:begin
            if assigned(Symbol^.TypeDefinition) and not (((Symbol^.TypeDefinition.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.PointerTo)) and (Symbol^.TypeDefinition^.PointerTo^.TypeDefinition=Type_)) then begin
             Index:=TypePointerList.IndexOf(Symbol^.TypeDefinition);
             if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
              DependencyItems[Counter].Dependencies.Add(Index);
             end;
            end;
           end;
          end;
          Symbol:=Symbol^.Next;
         end;
        end;
       end;
       ttdProcedure:begin
        SymbolList:=Type_^.Parameter;
        if assigned(SymbolList) then begin
         Symbol:=SymbolList.First;
         while assigned(Symbol) do begin
          case Symbol^.SymbolType of
           Symbols.tstVariable:begin
            if assigned(Symbol^.TypeDefinition) and not (((Symbol^.TypeDefinition.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.PointerTo)) and (Symbol^.TypeDefinition^.PointerTo^.TypeDefinition=Type_)) then begin
             Index:=TypePointerList.IndexOf(Symbol^.TypeDefinition);
             if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
              DependencyItems[Counter].Dependencies.Add(Index);
             end;
            end;
           end;
          end;
          Symbol:=Symbol^.Next;
         end;
        end;
        if assigned(Type_^.ReturnType) and not (((Type_^.ReturnType.TypeDefinition=ttdPointer) and assigned(Type_^.ReturnType^.PointerTo)) and (Type_^.ReturnType^.PointerTo^.TypeDefinition=Type_)) then begin
         Index:=TypePointerList.IndexOf(Type_^.ReturnType);
         if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
          DependencyItems[Counter].Dependencies.Add(Index);
         end;
        end;
       end;
      end;
     end;
     CopyTypeItems:=copy(TypeItems);
     DependencySort;
     for Counter:=0 to DependencyItemCount-1 do begin
      TypeItems[Counter]:=CopyTypeItems[DependencyOrder[Counter]];
     end;
     for Counter:=0 to DependencyItemCount-1 do begin
      FreeAndNil(DependencyItems[Counter].Dependencies);
     end;
    end;
   finally
    TypePointerList.Free;
   end;
  finally
   SetLength(DependencyItems,0);
   SetLength(DependencyOrder,0);
   SetLength(CopyTypeItems,0);
  end;
 end;
var i,j:longint;
    Type_:PType;
    Name:ansistring;
    SymbolList:TSymbolList;
    Symbol:PSymbol;
    CurrentVariantLevelIndex:longint;
    VariantLevelVariants:array of integer;
begin
 TypeItems:=nil;
 VariantLevelVariants:=nil;
 try

  SetLength(TypeItems,ModuleSymbol^.TypePointerList.Count);
  for i:=0 to ModuleSymbol^.TypePointerList.Count-1 do begin
   TypeItems[i]:=ModuleSymbol^.TypePointerList[i];
  end;

  for i:=0 to length(TypeItems)-1 do begin
   Type_:=TypeItems[i];
   Type_^.Dumped:=false;
  end;

  SortTypes;

  Target.AddLn('// Forward definitions');
  for i:=0 to length(TypeItems)-1 do begin
   Type_:=TypeItems[i];
   Name:=GetTypeName(Type_);
   case Type_.TypeDefinition of
    ttdRecord,ttdObject,ttdInterface:begin
     Target.AddLn('typedef struct '+Name+';');
    end;
    ttdClass:begin
     Target.AddLn('typedef struct '+Name+'_CLASS;');
    end;
   end;
  end;
  Target.AddLn('');

  FVariantPrefix:=false;

  Target.AddLn('// Real definitions');
  for i:=0 to length(TypeItems)-1 do begin
   Type_:=TypeItems[i];
   Name:=GetTypeName(Type_);
   case Type_.TypeDefinition of
    ttdEmpty:begin
     Target.AddLn('typedef void* '+Name+';');
    end;
    ttdEnumerated:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef '+ConvertStdType(Type_^.SubRangeType)+' '+Name+';');
    end;
    ttdBoolean,ttdSubRange,ttdCurrency:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef '+ConvertStdType(Type_^.SubRangeType)+' '+Name+';');
    end;
    ttdVariant:begin
     Target.AddLn('// variant');
    end;
    ttdArray:begin
     if Type_^.DynamicArray then begin
      Target.Add('// dynArray');
     end else begin
      Type_^.Dumped:=true;
      if assigned(Type_^.Definition) and ((Type_^.Definition^.TypeDefinition=ttdPointer) and assigned(Type_^.Definition^.PointerTo)) and (Type_^.Definition^.PointerTo^.TypeDefinition=Type_) then begin
       Target.AddLn('typedef '+
                    'void* '+
                    Name+
                    '['+IntToStr((Type_^.Range^.UpperLimit-Type_^.Range^.LowerLimit)+1)+'];');
      end else begin
       Target.AddLn('typedef '+
                    GetTypeName(Type_^.Definition)+' '+
                    Name+
                    '['+IntToStr((Type_^.Range^.UpperLimit-Type_^.Range^.LowerLimit)+1)+'];');
      end;
     end;
    end;
    ttdRecord,ttdObject,ttdClass,ttdInterface:begin
     Type_^.Dumped:=true;
     case Type_.TypeDefinition of
      ttdCLASS:begin
       Target.AddLn('typedef struct '+Name+'_CLASS* '+Name+';');
      end;
     end;
     if Type_^.RecordPacked then begin
      Target.AddLn('#pragma pack(push,1)');
     end;
     Target.Add('typedef ');
     if Type_^.RecordPacked then begin
      Target.Add('___PACKED___ ');
     end;
     case Type_.TypeDefinition of
      ttdCLASS:begin
       Target.AddLn('struct '+Name+'_CLASS {');
      end;
      else begin
       Target.AddLn('struct '+Name+' {');
      end;
     end;
     Target.IncTab;
     SymbolList:=Type_^.RecordTable;
     if assigned(SymbolList) then begin
      CurrentVariantLevelIndex:=0;
      Symbol:=SymbolList.First;
      while assigned(Symbol) do begin
       case Symbol^.SymbolType of
        Symbols.tstCaseVariantLevelPush:begin
         Target.AddLn('union {');
         Target.IncTab;
         inc(CurrentVariantLevelIndex);
         if CurrentVariantLevelIndex>length(VariantLevelVariants) then begin
          SetLength(VariantLevelVariants,CurrentVariantLevelIndex);
         end;
         VariantLevelVariants[CurrentVariantLevelIndex-1]:=0;
        end;
        Symbols.tstCaseVariantLevelPop:begin
         Target.DecTab;
         Target.AddLn('};');
         dec(CurrentVariantLevelIndex);
        end;
        Symbols.tstCaseVariantPush:begin
         Target.AddLn('struct {');
         Target.IncTab;
         inc(VariantLevelVariants[CurrentVariantLevelIndex-1]);
        end;
        Symbols.tstCaseVariantPop:begin
         Target.DecTab;
         Target.AddLn('} '+'L'+IntToStr(CurrentVariantLevelIndex)+'V'+IntToStr(VariantLevelVariants[CurrentVariantLevelIndex-1])+';');
        end;
        Symbols.tstVariable:begin
         if tsaObjectVMT in Symbol^.Attributes then begin
          Target.AddLn('pasObjectVirtualMethodTablePointer* '+GetSymbolName(Symbol)+';');
         end else if (Symbol^.TypeDefinition^.TypeDefinition=ttdPOINTER) and not assigned(Symbol^.TypeDefinition^.PointerTo) then begin
          Target.AddLn('void* '+GetSymbolName(Symbol)+';');
         end else if (Symbol^.TypeDefinition^.TypeDefinition=ttdPOINTER) and (Symbol^.TypeDefinition^.PointerTo=Type_^.Symbol) then begin
          case Type_.TypeDefinition of
           ttdCLASS:begin
            Target.AddLn('struct '+Name+'_CLASS** '+GetSymbolName(Symbol)+';');
           end;
           else begin
            Target.AddLn('struct '+Name+'* '+GetSymbolName(Symbol)+';');
           end;
          end;
         end else begin
          if assigned(Symbol^.TypeDefinition) and ((Symbol^.TypeDefinition^.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.PointerTo)) and (Symbol^.TypeDefinition^.Definition^.PointerTo^.TypeDefinition=Symbol^.TypeDefinition) then begin
           Target.AddLn('void* '+GetSymbolName(Symbol)+';');
          end else begin
           Target.AddLn(GetTypeName(Symbol^.TypeDefinition)+' '+GetSymbolName(Symbol)+';');
          end;
         end;
        end;
       end;
       Symbol:=Symbol^.Next;
      end;
     end;
     Target.DecTab;
     case Type_.TypeDefinition of
      ttdCLASS:begin
       Target.AddLn('} '+Name+'_CLASS;');
      end;
      else begin
       Target.AddLn('} '+Name+';');
      end;
     end;
     if Type_^.RecordPacked then begin
      Target.AddLn('#pragma pack(pop)');
     end;
    end;
    ttdShortString:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef pasShortstring '+Name+';');
    end;
    ttdLongString:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef pasLongstring '+Name+';');
    end;
    ttdFile:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef pasFile '+Name+';');
    end;
    ttdPointer:begin
     Type_^.Dumped:=true;
     Symbol:=Type_.PointerTo;
     if assigned(Symbol) then begin
      if Symbol^.TypeDefinition^.TypeDefinition in [ttdRecord,ttdObject,ttdClass,ttdInterface] then begin
       Target.AddLn('typedef struct '+GetTypeName(Symbol^.TypeDefinition)+'* '+Name+';');
      end else if assigned(Symbol^.TypeDefinition) and ((Symbol^.TypeDefinition^.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.Definition^.PointerTo)) and (Symbol^.TypeDefinition^.Definition^.PointerTo^.TypeDefinition=Symbol^.TypeDefinition) then begin
       Target.AddLn('typedef void* '+Name+';');
      end else begin
       Target.AddLn('typedef '+GetTypeName(Symbol^.TypeDefinition)+'* '+Name+';');
      end;
     end else begin
      Target.AddLn('typedef void* '+Name+';');
     end;
    end;
    ttdSet:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef '+ConvertStdType(tstUnsigned8Bit)+' '+Name+'['+IntToStr(Type_.SetSize)+'];');
    end;
    ttdProcedure:begin
     Target.AddLn('// procedure');
     Target.Add('typedef',spacesRIGHT);
     ProcessTypeOrName(Type_^.ReturnType, Target, Type_);
     Target.Add('*',spacesRIGHT);
     Target.Add(Name);
     Target.Add('(');
     Symbol:=Type_.Parameter.First;
     while Assigned(Symbol) do
     begin
      if Symbol<>Type_.Parameter.First then begin
       Target.Add(',',spacesRIGHT);
      end;
      ProcessTypeOrName(Symbol.TypeDefinition, Target, Type_);
      if IsSymbolReference(Symbol) then begin
       Target.Add('*',spacesRIGHT);
      end;
      Symbol := Symbol.Next;
     end;
     Target.Add(')',spacesRIGHT);
     Target.AddLn(';');
    end;
    ttdClassRef:begin
     Target.AddLn('// classref');
    end;
    ttdFloat:begin
     Type_^.Dumped:=true;
     Target.AddLn('typedef '+ConvertStdType(Type_^.FloatType)+' '+Name+';');
    end;
    ttdCExpression:begin
    end;
   end;
  end;
  Target.AddLn('');

 finally
  SetLength(TypeItems,0);
  SetLength(VariantLevelVariants,0);
  FVariantPrefix:=true;
 end;
end;

{ TCodeWriter }

procedure TCodeWriter.Add(const s: ansistring; Spaces: longint = spacesNONE);
begin
 if ((Spaces and spacesLEFT)<>0) and ((length(FCurrentLine)>0) and not (FCurrentLine[length(FCurrentLine)] in [#0..#32,'(','[','{'])) then
   FCurrentLine := FCurrentLine + ' ';
 if (length(s)>0) and (s[1] in [',',';',')',']','}']) then begin
  FCurrentLine := trimright(FCurrentLine);
 end;
 FCurrentLine := FCurrentLine + s;
 if ((Spaces and spacesRIGHT)<>0) and ((length(FCurrentLine)>0) and not (FCurrentLine[length(FCurrentLine)] in [#0..#32])) then
   FCurrentLine := FCurrentLine + ' ';
end;

procedure TCodeWriter.AddFooter(const s: ansistring);
begin
 if FFooter = '' then
  FFooter := s
 else
  FFooter := FFooter + #13#10 + s;
end;

procedure TCodeWriter.AddHeader(const s: ansistring);
begin
 FHeader.WriteLine(s);
end;

procedure TCodeWriter.AddInclude(const s: ansistring);
var i: Integer;
begin
 for i:=0 to Length(FIncludes)-1 do
  if FIncludes[i] = Uppercase(s) then
   Exit;
  i:=Length(FIncludes);
  SetLength(FIncludes, i + 1);
  FIncludes[i] := Uppercase(s);
  AddHeader('#include "'+s+'"');
end;

procedure TCodeWriter.AddLn(const s: ansistring; Spaces: longint = spacesNONE);
var i: Integer;
begin
 if (s = ';')and(FCurrentLine = '') then
  Exit;

 if ((Spaces and spacesLEFT)<>0) and ((length(FCurrentLine)>0) and not (FCurrentLine[length(FCurrentLine)] in [#0..#32,'(','[','{'])) then
   FCurrentLine := FCurrentLine + ' ';

 if (length(s)>0) and (s[length(s)] in [',',';',')',']','}']) then begin
  FCurrentLine := trimright(FCurrentLine);
 end;

 FCurrentLine := FCurrentLine + s;

 //if (Length(FCurrentLine)>0)and(FCurrentLine[1]<>'#') then
  for i:=0 to FTabs-1 do
   FCurrentLine := #9 + FCurrentLine;

 FData.WriteLine(FCurrentLine);
 FCurrentLine := '';
end;

procedure TCodeWriter.Clear;
begin
 FHeader.Clear;
 FFooter := '';
 FData.Clear;
end;

constructor TCodeWriter.Create;
begin
 FHeader := TBeRoStream.Create;
 FFooter := '';
 FStack := TBeRoStream.Create;
 FData := TBeRoStream.Create;

 FCurrentLine := '';

 FRootNestedStartPosition := -1;
end;

procedure TCodeWriter.DecTab;
begin
 Dec(FTabs);
end;

destructor TCodeWriter.Destroy;
begin
 FHeader.Free;
 FFooter := '';
 FStack.Free;
 FData.Free;
 inherited;
end;

procedure TCodeWriter.ExportStream(TargetStream: TBeRoStream);
begin
 TargetStream.Assign(FHeader);
 TargetStream.Append(FStack);
 TargetStream.Append(FData);
 TargetStream.WriteLine(FFooter);
end;

procedure TCodeWriter.IncTab;
begin
 Inc(FTabs);
end;

procedure TCodeWriter.InsertAtMark(s: ansistring);
var temp: string;
begin
  // this is rather ugly
  temp := FData.Text;
  Insert(s+#13#10, temp, FMarker);
  FData.Text := temp;
  inc(FMarker, Length(s) + 2);
end;

procedure TCodeWriter.MarkRootNested;
begin
 FRootNestedStartPosition := FData.Position;
end;

procedure TCodeWriter.SetMarker;
begin
  FMarker := FData.Position;
end;

procedure TCodeWriter.UnmarkRootNested(Source:TCodeWriter);
begin
 if Source.FData.Size>0 then begin
  FStack.Append(Source.FData);
 end;
end;

end.
