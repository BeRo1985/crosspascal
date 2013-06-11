unit CodeGenC;
{$i Compiler.inc}

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
       FIgnoreNextToken: Boolean;
      public
       constructor Create;
       destructor Destroy; override;

       procedure AddHeader(const s: ansistring);
       procedure AddFooter(const s: ansistring);
       procedure AddInclude(const s: ansistring);

       procedure Add(const s: ansistring; Spaces: longint = spacesNONE);
       procedure AddLn(s: ansistring; Spaces: longint = spacesNONE);

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

     TCodeGenC = class(TCode)
      private
       FHeader,FCode, FProcCode, FModuleCode, FProcStruct: TCodeWriter;
       FDepth: Integer;
       FSelf: PSymbol;
       FVariantPrefix:boolean;
       FInProc, FNeedNestedStack: boolean;
       FProcSymbol: PSymbol;
       FProcStructCount: longint;
       FStringConstCount: longint;
       FBreakCount: Integer;
       FNestedBreakCount: Integer;
       FWithStack: array of PType;
       FWithStackSize: longint;
       FBreakLabelNeeded: array of Integer;
       FContinueLabelNeeded: array of Integer;
       FBreakContinueLevel: array of Integer;
       FLevelHasTryBlock: array of boolean;
       FTryBlockCounter: longint;
       FTrySymbolCounter: longint;
       FCodeLevel: longint;
      protected
       function GetTypeSize(AType: PType): Cardinal;

       function GetModuleName(Sym: PSymbol): ansistring;
       function GetSymbolName(Sym: PSymbol; const Prefix: ansistring = ''; Wrapping: boolean = true): ansistring;
       function GetTypeName(Type_:PType):ansistring;
       function GetOriginalTypeName(Type_:PType):ansistring;

       function ConvertStdType(const StdType: TStandardType): ansistring;
       function ConvertUnsignedStdType(const StdType: TStandardType): ansistring;
       function ConvertConstSymbol(const Symbol: PSymbol): ansistring;
       procedure ConvertFuncSymbol(const Symbol: PSymbol; Target: TCodeWriter; IsConstructor: boolean = false; IsDestructor: boolean = false);

       function AnsiStringEscape(const Input: ansistring; Quotes: Boolean = True): ansistring;
       function WideStringEscape(const Input: widestring): ansistring;

       procedure ProcessTypeOrName(AType: PType; Target: TCodeWriter; OwnType: PType = nil);
       procedure ProcessTypedConstant(var Constant: PConstant; AType: PType; Target: TCodeWriter);
       procedure ProcessFunctionType(Symbol:PSymbol; ReturnType: PType; Parameter: TSymbolList; const funcName: ansistring; Target: TCodeWriter; InjectinstanceData:boolean=false);

       function TranslateStringConstant(ConstantStr: THugeString; ATarget: TCodeWriter = nil): ansistring;
       function TranslateStringConstantDataOnly(ConstantStr: THugeString; ATarget: TCodeWriter): ansistring;

       procedure TranslateShortStringConstant(const Name:ansistring; const ConstantStr: ShortString; ATarget: TCodeWriter); overload;
       procedure TranslateShortStringConstant(const ConstantStr: ShortString; ATarget: TCodeWriter); overload;
       function GetShortStringConstant(const ConstantStr: ShortString): ansistring;

       procedure TranslateStringCode(TreeNode: TTreeNode; DesiredStringType: PType);

       procedure TranslateCode(TreeNode:TTreeNode; NoTypecasting: Boolean = false);

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

       procedure TranslateMethodList(List: TSymbolList; ATarget: TCodeWriter = nil);

       procedure TranslateSymbolList(List: TSymbolList; IgnoreTypes: boolean; ATarget: TCodeWriter = nil);
       procedure InitializeSymbolList(List: TSymbolList;Target: TCodeWriter);
       procedure FinalizeSymbolList(List: TSymbolList;Target: TCodeWriter);

       procedure TranslateConstants;
       procedure TranslateModuleTypes(ModuleSymbol:PSymbol;Target,CodeTarget:TCodeWriter);
      public
       LocalSwitches:PLocalSwitches;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheTreeManager:TTreeManager;TheOptions:TOptions;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;

       procedure BeginRootNestedProc; override;
       procedure EndRootNestedProc; override;

       procedure AddHeader(const Input: string);
       procedure AddCode(const Input: string);

       procedure GenerateProc(ProcSymbol:PSymbol;ProcCodeTree:TTreeNode); override;
       procedure GenerateProgram(ProgramSymbol:PSymbol;ProgramCodeTree:TTreeNode); override;
       procedure GenerateLibrary(LibrarySymbol:PSymbol;LibraryCodeTree:TTreeNode); override;
       procedure GenerateUnit(UnitSymbol:PSymbol;InitializationCodeTree,FinalizationCodeTree:TTreeNode); override;

       procedure SaveToStreams(CodeStream,HeaderStream:TBeRoStream); override;
     end;


implementation

var AnsistringType: TType;

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

function IsStringType(AType: PType;TreatPCharAsString: Boolean = True): Boolean;
begin
 result:=False;
 if not Assigned(AType) then
  Exit;
 result:=(AType.TypeDefinition = ttdLongstring)or
         (AType.TypeDefinition = ttdShortString)or
         ((AType.TypeDefinition = ttdPointer)and Assigned(AType.PointerTo) and
          TreatPCharAsString and Assigned(AType.PointerTo.TypeDefinition) and
          ((AType.PointerTo.TypeDefinition.TypeDefinition = ttdSubRange) and
           (AType.PointerTo.TypeDefinition.SubRangeType in [tstUnsignedChar, tstUnsignedWideChar, tstUnsignedHugeChar])));
end;

function TCodeGenC.GetModuleName(Sym: PSymbol): ansistring;
begin
 if not Assigned(sym) then
  result:='HUND'
 else
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

function TCodeGenC.GetSymbolName(Sym: PSymbol; const Prefix: ansistring = ''; Wrapping: boolean = true): ansistring;
begin
 if assigned(Sym) then begin
  case Sym.SymbolType of
   Symbols.tstType:begin
    result:=Prefix+GetTypeName(Sym^.TypeDefinition);
   end;
   Symbols.tstVariable:begin
    if tsaTemporaryExceptionVariable in Sym^.Attributes then begin
     result:=Prefix+'TRY_EXCEPTION_'+GetSymbolName(FSelf)+'_'+IntToStr(Sym^.IntValue);
    end else if tsaField in Sym^.Attributes then begin
     if tsaInternalField in Sym^.Attributes then begin
      result:=Prefix+'INTERNAL_FIELD_'+Sym.Name;
     end else begin
      if FVariantPrefix and (length(Sym.VariantPrefix)>0) then begin
       result:=Prefix+GetTypeName(Sym.OwnerType)+'_'+Sym.VariantPrefix+GetTypeName(Sym.OwnerType)+'_FIELD_'+Sym.Name;
      end else begin
       result:=Prefix+GetTypeName(Sym.OwnerType)+'_FIELD_'+Sym.Name;
      end;
      if Wrapping and FInProc and assigned(Sym^.TypeDefinition) and (Sym^.TypeDefinition^.TypeDefinition=ttdPointer) then begin
       result:='(('+GetTypeName(Sym^.TypeDefinition)+')((void*)('+result+')))';
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
      case Sym^.VariableType of
       tvtResult:begin
        result:='result';
       end;
       tvtObjectInstanceSelf:begin
        if assigned(FProcSymbol) and assigned(FProcSymbol^.OwnerObjectClass) then begin
         result:='instanceData';
        end else begin
         Error.InternalError(201304050042000);
        end;
       end;
       tvtClassInstanceSelf:begin
        if assigned(FProcSymbol) and assigned(FProcSymbol^.OwnerObjectClass) then begin
         result:='instanceData';
        end else begin
         Error.InternalError(201304052356000);
        end;
       end;
       tvtClassSelf:begin
        if assigned(FProcSymbol) and assigned(FProcSymbol^.OwnerObjectClass) then begin
         result:='classReference';
        end else begin
         Error.InternalError(201304052356001);
        end;
       end;
       else begin
        if Sym^.TypedConstant then begin
         result:='LOCAL_TYPEDCONSTANT_'+Sym.Name;
        end else begin
         result:='LOCAL_VARIABLE_'+Sym.Name;
        end;
       end;
      end;
      if length(Prefix)>0 then begin
       result:=Prefix+result;
      end;
      if FInProc then begin
       if assigned(Sym^.LocalProcSymbol) and Sym^.LocalProcSymbolAccessedFromHigherNestedProc then begin
        result:='(('+GetSymbolName(Sym^.LocalProcSymbol,'',false)+'_NESTED_STACK*)(nestedLevelStack['+IntToStr(Sym^.LocalProcSymbol^.LexicalScopeLevel)+']))->'+result;
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
    result:=Prefix+GetModuleName(Sym.OwnerModule)+'_LABEL_'+Sym.Name
   end;
   Symbols.tstConstant:begin
    result:=Prefix+GetModuleName(Sym.OwnerModule)+'_CONSTANT_'+Sym.Name
   end;
   Symbols.tstProcedure:begin
    if tpaConstructor in Sym.ProcedureAttributes then begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_CONSTRUCTOR_'+Sym.OverloadedName;
    end else if tpaDestructor in Sym.ProcedureAttributes then begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_DESTRUCTOR_'+Sym.OverloadedName;
    end else begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.OverloadedName;
    end;
{   if (tpaOverload in Sym.ProcedureAttributes) and (Sym.OverloadedName<>'') and (Sym.OverloadedName<>Sym.Name) then begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.OverloadedName;
    end else begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_PROCEDURE_'+Sym.Name;
    end;}
   end;
   Symbols.tstFunction:begin
    result:=Prefix+GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.OverloadedName;
{   if (tpaOverload in Sym.ProcedureAttributes) and (Sym.OverloadedName<>'') and (Sym.OverloadedName<>Sym.Name) then begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.OverloadedName;
    end else begin
     result:=Prefix+GetModuleName(Sym.OwnerModule)+'_FUNCTION_'+Sym.Name;
    end;}
   end;
   else begin
    result:=Prefix+GetModuleName(Sym.OwnerModule)+'_SYMBOL_'+Sym.Name;
   end;
  end;
 end else begin
  result:='void';
 end;
end;

function TCodeGenC.GetTypeName(Type_:PType):ansistring;
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

function TCodeGenC.GetOriginalTypeName(Type_:PType):ansistring;
begin
 if assigned(Type_^.OwnerModule) then begin
  if assigned(Type_^.Symbol) and (Type_^.Symbol^.LexicalScopeLevel=0) and (tsaPublicUnitSymbol in Type_^.Symbol^.Attributes) then begin
   result:=Type_^.Symbol^.OriginalCaseName;
  end else begin
   result:=GetModuleName(Type_^.OwnerModule)+'_TYPE_ID_'+IntToStr(Type_^.ID);
  end;
 end else begin
  result:=IntToStr(PtrUInt(Type_));
 end;
end;

function TCodeGenC.GetTypeSize(AType: PType): Cardinal;
begin
  result := SymbolManager.GetSize(AType);
end;

procedure TCodeGenC.InitializeSymbolList(List: TSymbolList;
  Target: TCodeWriter);
var sym: PSymbol;
begin
 sym := List.First;

 while Assigned(sym) do
 begin
  if not (tsaMapped in Sym^.Attributes) then begin

   if(Sym.SymbolType=tstVariable) then begin
    case Sym.TypeDefinition.TypeDefinition of
     ttdLongString:
     begin
      Target.Add(GetSymbolName(Sym)+' =',spacesRIGHT);
      if (Sym.TypedConstant) then
       ProcessTypedConstant(Sym.Constant,Sym.TypeDefinition,Target)
      else
       Target.Add('NULL');
     end;
     else begin
      if Sym.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(Sym.TypeDefinition) then begin
       Target.Add('pasInitialize((void*)&'+GetSymbolName(Sym)+', &'+GetTypeName(Sym.TypeDefinition)+'_INITIALIZATION_TYPEINFO)');
      end;
     end;
    end;
   end;

   Target.AddLn(';');

   if(Sym.SymbolType=tstVariable) and assigned(Sym.TypeDefinition) and not Sym.TypedConstant then begin
    case Sym.TypeDefinition.TypeDefinition of
     ttdOBJECT:begin
      Target.AddLn(GetSymbolName(Sym)+'.INTERNAL_FIELD_VMT = (void*)&'+GetTypeName(Sym.TypeDefinition)+'_VMT;');
     end;
    end;
   end;
   
  end;
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

{ TCodeGenC }

function TCodeGenC.ConvertConstSymbol(const Symbol: PSymbol): ansistring;
begin
 case Symbol.ConstantType of
  tctOrdinal:
   result := IntToStr(Symbol.IntValue);
  tctFloat:
   Str(Symbol.FloatValue, result);
  else result := '<ConvertConstSymbol>';
 end;
end;

procedure TCodeGenC.ConvertFuncSymbol(const Symbol: PSymbol; Target: TCodeWriter; IsConstructor: boolean = false; IsDestructor: boolean = false);
var sym: PSymbol;
    HaveParameters: boolean;
begin
 if Symbol.SymbolType = tstProcedure then
 begin
  if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then
  begin
    Target.Add('int',spacesRIGHT);
  end
  else if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then
  begin
    Target.Add('void*',spacesRIGHT);
  end
  else
  begin
    Target.Add('void',spacesRIGHT);
  end;
 end
 else if Symbol.SymbolType = tstFunction then
 begin
  ProcessTypeOrName(Symbol.ReturnType, Target);
  Target.Add('',spacesRIGHT);
 end else
  Exit;
 if IsConstructor then
 begin
   Target.Add(GetSymbolName(Symbol)+'_CONSTRUCTOR');
 end
 else if IsDestructor then
 begin
   Target.Add(GetSymbolName(Symbol)+'_DESTRUCTOR');
 end
 else
 begin
   Target.Add(GetSymbolName(Symbol));
 end;
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
  if tpaClassProcedure in Symbol^.ProcedureAttributes then begin
   Target.Add(GetTypeName(Symbol^.OwnerObjectClass^.ClassOfType)+' classReference');
  end else begin
   if Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT then begin
    Target.Add(GetSymbolName(Symbol^.OwnerObjectClass^.Symbol)+' *instanceData');
   end else begin
    Target.Add(GetSymbolName(Symbol^.OwnerObjectClass^.Symbol)+' instanceData');
   end;
  end;
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

function TCodeGenC.ConvertStdType(const StdType: TStandardType): ansistring;
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

function TCodeGenC.ConvertUnsignedStdType(const StdType: TStandardType): ansistring;
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

constructor TCodeGenC.Create(TheError: TError;
  TheSymbolManager: TSymbolManager; TheTreeManager: TTreeManager;
  TheOptions: TOptions; TheLocalSwitches: PLocalSwitches);
begin
 inherited;

 LocalSwitches := TheLocalSwitches;

 FHeader := TCodeWriter.Create;
 FCode := TCodeWriter.Create;
 FProcCode := TCodeWriter.Create;
 FModuleCode := TCodeWriter.Create;
 FProcStruct := TCodeWriter.Create;

 FVariantPrefix:=true;

 FInProc:=false;
 FNeedNestedStack:=false;

 FProcSymbol:=nil;

 FWithStack:=nil;
 FWithStackSize:=0;

 FBreakLabelNeeded:=nil;
 FContinueLabelNeeded:=nil;
 FBreakContinueLevel:=nil;
 SetLength(FBreakLabelNeeded,1024);
 SetLength(FContinueLabelNeeded,1024);
 SetLength(FBreakContinueLevel,1024);

 FTryBlockCounter:=0;
 FTrySymbolCounter:=0;
 FCodeLevel:=-1;

 FLevelHasTryBlock:=nil;
 SetLength(FLevelHasTryBlock,1024);
end;

destructor TCodeGenC.Destroy;
begin
 SetLength(FWithStack,0);
 SetLength(FBreakLabelNeeded,0);
 SetLength(FContinueLabelNeeded,0);
 SetLength(FBreakContinueLevel,0);
 SetLength(FLevelHasTryBlock,0);
 FHeader.Free;
 FCode.Free;
 FProcCode.Free;
 FModuleCode.Free;
 FProcStruct.Free;
 inherited;
end;

procedure TCodeGenC.FinalizeSymbolList(List: TSymbolList;
  Target: TCodeWriter);
var sym: PSymbol;
begin
 sym := List.First;

 while Assigned(sym) do
 begin
  if not (tsaMapped in Sym^.Attributes) then begin
   if(Sym.SymbolType=tstVariable) then
    case Sym.TypeDefinition.TypeDefinition of
     ttdLongString:
     begin
      if(GetSymbolName(Sym))<>'result' then
        Target.Add('FreeLongstring(&'+GetSymbolName(Sym)+')');
     end;
     ttdArray:
     begin
      // todo: string-ref checks for arrays
      if Sym.TypeDefinition.DynamicArray then
        Target.Add('pasFreeArray(&'+GetSymbolName(Sym)+', &'+GetTypeName(Sym.TypeDefinition)+'_INITIALIZATION_TYPEINFO)');
     end;
     else begin
      if Sym.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(Sym.TypeDefinition) then begin
       Target.Add('pasFinalize((void*)&'+GetSymbolName(Sym)+', &'+GetTypeName(Sym.TypeDefinition)+'_INITIALIZATION_TYPEINFO)');
      end;
     end;
    end;
   Target.AddLn(';');
  end;
  sym := sym.Next;
 end;
end;

procedure TCodeGenC.GenerateLibrary(LibrarySymbol: PSymbol;
  LibraryCodeTree: TTreeNode);
begin
 FCode.AddLn('//library ' + LibrarySymbol.Name);
 FSelf := LibrarySymbol;
 TranslateSymbolList(FSelf.SymbolList, false);
end;

procedure TCodeGenC.BeginRootNestedProc;
begin
 FProcCode.MarkRootNested;
 FProcStruct.Clear;
 FProcStruct.AddLn('');
end;

procedure TCodeGenC.EndRootNestedProc;
begin
 FCode.UnmarkRootNested(FProcStruct);
end;

procedure TCodeGenC.GenerateProc(ProcSymbol: PSymbol;
  ProcCodeTree: TTreeNode);
var //s,s2: ansistring;
    ParameterSymbol,Symbol,MethodSymbol,sym:PSymbol;
begin

 FWithStackSize:=0;

 FTryBlockCounter:=0;
 FTrySymbolCounter:=0;
 FCodeLevel:=-1;

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

 if (tpaConstructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
   FProcCode.AddLn('if(!instanceData){');
   FProcCode.IncTab;
// FProcCode.AddLn('instanceData = pasClassCreate((void*)&'+GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT);');
   MethodSymbol:=FProcSymbol^.OwnerObjectClass^.RecordTable.GetSymbol('NEWINSTANCE');
   FProcCode.Add('instanceData = (void*)');
   FProcCode.Add('(('+GetTypeName(MethodSymbol^.OwnerObjectClass)+'_VMT_'+IntToStr(MethodSymbol^.VirtualIndex)+')(');
   FProcCode.Add(GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT');
   FProcCode.Add('.virtualMethods['+IntToStr(MethodSymbol^.VirtualIndex)+']))');
   FProcCode.AddLn('(pasClassVMTMask((void*)&'+GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT));');
   FProcCode.DecTab;
   FProcCode.AddLn('}');
 end else if (tpaDestructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
   MethodSymbol:=FProcSymbol^.OwnerObjectClass.RecordTable.GetSymbol('BEFOREDESTRUCTION');
   FProcCode.Add('(('+GetTypeName(MethodSymbol^.OwnerObjectClass)+'_VMT_'+IntToStr(MethodSymbol^.VirtualIndex)+')(');
   FProcCode.Add(GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT');
   FProcCode.Add('.virtualMethods['+IntToStr(MethodSymbol^.VirtualIndex)+']))');
   FProcCode.AddLn('((void*)instanceData);');
 end;

 if Assigned(ProcSymbol.Parameter) then
  ParameterSymbol := ProcSymbol.Parameter.First
 else
  ParameterSymbol := nil;

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

 Inc(FDepth);
 TranslateSymbolList(SymbolManager.CurrentList, false, FProcCode);
 Dec(FDepth);

 InitializeSymbolList(SymbolManager.CurrentList, FProcCode);

 while Assigned(ParameterSymbol) do
 begin
  if(ParameterSymbol^.SymbolType = Symbols.tstVariable) and
    ((parameterSymbol.VariableType = tvtParameterValue)or(parameterSymbol.VariableType = tvtParameterConstant)) and
    (parameterSymbol.TypeDefinition.TypeDefinition = ttdLongstring) then
  begin
   FProcCode.AddLn('IncRefLongstring(&'+GetSymbolName(ParameterSymbol)+');');
  end;
  ParameterSymbol := ParameterSymbol.Next;
 end;

 FInProc:=true;

 FProcCode.AddLn('//code');
 TranslateCode(ProcCodeTree);

 FinalizeSymbolList(SymbolManager.CurrentList, FProcCode);

 if Assigned(ProcSymbol.Parameter) then
  ParameterSymbol := ProcSymbol.Parameter.First
 else
  ParameterSymbol := nil;

 while Assigned(ParameterSymbol) do
 begin
  if(ParameterSymbol^.SymbolType = Symbols.tstVariable) and
    ((parameterSymbol.VariableType = tvtParameterValue)or(parameterSymbol.VariableType = tvtParameterConstant)) and
    (parameterSymbol.TypeDefinition.TypeDefinition = ttdLongstring) then
  begin
   FProcCode.AddLn('DecRefLongstring(&'+GetSymbolName(ParameterSymbol)+');');
  end;
  ParameterSymbol := ParameterSymbol.Next;
 end;


 if FNeedNestedStack then begin
  if SymbolManager.LexicalScopeLevel>1 then begin
   FProcCode.AddLn('nestedLevelStack['+IntToStr(ProcSymbol^.LexicalScopeLevel)+'] = nestedLevelStackLast;');
  end;
 end;

 if Assigned(ProcSymbol.ReturnType) then begin
   FProcCode.AddLn('return '+GetSymbolName(ProcSymbol.ResultSymbol)+';');
 end else if (tpaConstructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
   MethodSymbol:=FProcSymbol^.OwnerObjectClass.RecordTable.GetSymbol('AFTERCONSTRUCTION');
   FProcCode.Add('(('+GetTypeName(MethodSymbol^.OwnerObjectClass)+'_VMT_'+IntToStr(MethodSymbol^.VirtualIndex)+')(');
   FProcCode.Add(GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT');
   FProcCode.Add('.virtualMethods['+IntToStr(MethodSymbol^.VirtualIndex)+']))');
   FProcCode.AddLn('((void*)instanceData);');
   FProcCode.AddLn('return instanceData;');
 end else if (tpaDestructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
   FProcCode.AddLn('if(instanceData->INTERNAL_FIELD_VMT == pasClassVMTMask((void*)&'+GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT)){');
   MethodSymbol:=FProcSymbol^.OwnerObjectClass.RecordTable.GetSymbol('FREEINSTANCE');
   FProcCode.IncTab;
   FProcCode.Add('(('+GetTypeName(MethodSymbol^.OwnerObjectClass)+'_VMT_'+IntToStr(MethodSymbol^.VirtualIndex)+')(');
   FProcCode.Add(GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT');
   FProcCode.Add('.virtualMethods['+IntToStr(MethodSymbol^.VirtualIndex)+']))');
   FProcCode.AddLn('((void*)instanceData);');
   FProcCode.DecTab;
   FProcCode.AddLn('}');
 end else if (tpaConstructor in ProcSymbol^.ProcedureAttributes) and assigned(ProcSymbol^.OwnerObjectClass) and (ProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
   FProcCode.AddLn('return 0;');
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

procedure TCodeGenC.GenerateProgram(ProgramSymbol: PSymbol;
  ProgramCodeTree: TTreeNode);
var i: integer;
begin
 FInProc:=false;
 FProcSymbol:=nil;

 FWithStackSize:=0;
 FTryBlockCounter:=0;
 FTrySymbolCounter:=0;
 FCodeLevel:=-1;

 for i:=0 to SymbolManager.UnitList.Count-1 do
  FCode.AddInclude(Copy(SymbolManager.UnitList[i], 1, Length(SymbolManager.UnitList[i]))+'.h');
//  FCode.AddInclude(Copy(SymbolManager.UnitList[i], 2, Length(SymbolManager.UnitList[i]))+'.h');

 FHeader.AddHeader('#ifndef __OBJPAS2C'+ProgramSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#define __OBJPAS2C'+ProgramSymbol.Name+'_H_INCLUDED__');
 FHeader.AddHeader('#ifdef __cplusplus');
 FHeader.AddHeader('extern "C" {');
 FHeader.AddHeader('#endif');
  FHeader.AddHeader('#include "SYSTEM.h"');
 FCode.AddLn('//program ' + ProgramSymbol.Name);
 FCode.AddHeader('#define __OBJPAS2CMAIN__');
 FSelf := ProgramSymbol;

 TranslateModuleTypes(FSelf, FHeader, FModuleCode);
 TranslateSymbolList(FSelf.SymbolList, true);

 TranslateMethodList(FSelf.SymbolList,FHeader);

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

procedure TCodeGenC.GenerateUnit(UnitSymbol: PSymbol; InitializationCodeTree,
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
 FHeader.AddInclude('setjmp.h');
 FHeader.AddInclude('system.h');
 FHeader.AddInclude('stdint.h');
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
 TranslateModuleTypes(FSelf, FHeader, FModuleCode);
 TranslateSymbolList(FSelf.SymbolList, true);
 TranslateMethodList(FSelf.SymbolList,FHeader);

 FProcCode.AddLn('void '+UnitSymbol.Name+'_C_INITIALIZATION(){');
 FWithStackSize:=0;
 FTryBlockCounter:=0;
 FTrySymbolCounter:=0;
 FCodeLevel:=-1;
 TranslateCode(InitializationCodeTree);
 FProcCode.AddLn('}');

 FProcCode.AddLn('void '+UnitSymbol.Name+'_C_FINALIZATION(){');
 FWithStackSize:=0;
 FTryBlockCounter:=0;
 FTrySymbolCounter:=0;
 FCodeLevel:=-1;
 TranslateCode(FinalizationCodeTree);
 FProcCode.AddLn('}');

end;

procedure TCodeGenC.ProcessFunctionType(Symbol:PSymbol; ReturnType: PType; Parameter: TSymbolList; const funcName: ansistring; Target: TCodeWriter; InjectinstanceData:boolean=false);
var //i: integer;
    Sym: PSymbol;
    HasParameter:boolean;
begin
 if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
  Target.Add('int',spacesRIGHT);
 end else begin
  ProcessTypeOrName(ReturnType, Target);
 end;
 Target.Add('(*'+funcName+')(',spacesLEFT);
 if assigned(Parameter) then begin
  Sym:=Parameter.First;
 end else begin
  Sym:=nil;
 end;
 HasParameter:=false;
 if InjectinstanceData or (assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdINTERFACE)) then begin
  Target.Add('void*');
  HasParameter:=true;
 end;
 while Assigned(sym) do
 begin
  if HasParameter then begin
   Target.Add(',',spacesRIGHT);
  end;
  ProcessTypeOrName(Sym.TypeDefinition, Target);
  if IsSymbolReference(Sym) then begin
   Target.Add('*');
  end;
  HasParameter:=true;
  Sym := Sym.Next;
 end;
 Target.Add(')');
end;

procedure TCodeGenC.TranslateCode(TreeNode:TTreeNode; NoTypecasting: Boolean = false);
var SubTreeNode,SubTreeNode2:TTreeNode;
    s:ansistring;
    HaveParameters,InjectNullPointer:boolean;
    ObjectClassType:PType;
    MethodSymbol,Symbol:PSymbol;
    TryBlockCounter,Level:longint;

  procedure AddStringCompare(Code: TCodeWriter; FuncName: ansistring; Left, Right: TTreeNode);
  begin
   Code.Add(FuncName+'(');
   if Left.Return.TypeDefinition = ttdShortString then
    Code.Add('&');
   if Left.Return.TypeDefinition = ttdPointer then
    TranslateCode(Left)
   else
    TranslateStringCode(Left, Left.Return);
    Code.Add(',');

   if Right.Return.TypeDefinition = ttdShortString then
    Code.Add('&');
   if Right.Return.TypeDefinition = ttdPointer then
    TranslateCode(Right)
   else
    TranslateStringCode(Right, Left.Return);
   Code.Add(')');
  end;
  procedure DoStringCompare(Left, Right: TTreeNode);
  begin
   if(Left.Return.TypeDefinition = ttdLongString)and(Right.Return.TypeDefinition = ttdLongString)then
    AddStringCompare(FProcCode, 'pasCompareStrL', Left, Right)
   else if(Left.Return.TypeDefinition = ttdShortString)and(Right.Return.TypeDefinition = ttdShortString)then
    AddStringCompare(FProcCode, 'pasCompareStrS', Left, Right)
   else if(Left.Return.TypeDefinition = ttdPointer)and(Right.Return.TypeDefinition = ttdPointer)then
    AddStringCompare(FProcCode, 'pasCompareStrP', Left, Right)
   else if(Left.Return.TypeDefinition = ttdShortString)and(Right.Return.TypeDefinition = ttdLongString)then
    AddStringCompare(FProcCode, 'pasCompareStrSL', Left, Right)
   else if(Left.Return.TypeDefinition = ttdPointer)and(Right.Return.TypeDefinition = ttdLongString)then
    AddStringCompare(FProcCode, 'pasCompareStrPL', Left, Right)
   else if(Left.Return.TypeDefinition = ttdPointer)and(Right.Return.TypeDefinition = ttdShortstring)then
    AddStringCompare(FProcCode, 'pasCompareStrPS', Left, Right)
   else if(Right.Return.TypeDefinition = ttdShortString)and(Left.Return.TypeDefinition = ttdLongString)then
    AddStringCompare(FProcCode, 'pasCompareStrSL', Right, Left)
   else if(Right.Return.TypeDefinition = ttdPointer)and(Left.Return.TypeDefinition = ttdLongString)then
    AddStringCompare(FProcCode, 'pasCompareStrPL', Right, Left)
   else if(Right.Return.TypeDefinition = ttdPointer)and(Left.Return.TypeDefinition = ttdShortstring)then
    AddStringCompare(FProcCode, 'pasCompareStrPS', Right, Left)
   else Error.InternalError(20130608037242);
  end;


begin
 if assigned(TreeNode) then begin
  if TreeNode.LineNumber=680 then begin
   if TreeNode.LineNumber=680 then begin
   end;
  end;
  inc(FCodeLevel);
  if (FCodeLevel+1)>length(FLevelHasTryBlock) then begin
   if FCodeLevel<16 then begin
    SetLength(FLevelHasTryBlock,16);
   end else begin
    SetLength(FLevelHasTryBlock,(FCodeLevel+1) shl 1);
   end;
  end;
  FLevelHasTryBlock[FCodeLevel]:=false;
  case TreeNode.TreeNodeType of
   ttntEMPTY:begin
   end;
   ttntSubRange:begin
   end;
   ttntAssign:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     if(TreeNode.Left.Return.TypeDefinition=ttdLongString) then
     begin
      FProcCode.Add('AssignLongstring(&');
      TranslateCode(TreeNode.Left, true);
      FProcCode.Add(', (pasLongstring)',spacesRIGHT);
      TranslateStringCode(TreeNode.Right, TreeNode.Left.Return);
      FProcCode.Add(')');
     end else
     if(TreeNode.Left.Return.TypeDefinition=ttdShortstring) then
     begin
      FProcCode.Add('AssignShortstring(&');
      TranslateCode(TreeNode.Left, true);
      FProcCode.Add(', ');
      FProcCode.Add(IntToStr(TreeNode.Left.Symbol.TypeDefinition.Length));
      FProcCode.Add(', (pasLongstring)',spacesRIGHT);
      TranslateStringCode(TreeNode.Right, @Ansistringtype);
      FProcCode.Add(')');
     end else
     if(TreeNode.Left.Return.TypeDefinition=ttdArray)and
       (TreeNode.Left.Symbol.TypeDefinition.DynamicArray) then
     begin
      FProcCode.Add('pasAssignArray(&');
      TranslateCode(TreeNode.Left, true);
      FProcCode.Add(', (pasDynArray)',spacesRIGHT);
      TranslateCode(TreeNode.Right);
      FProcCode.Add(', &'+GetTypeName(TreeNode.Left.Symbol.TypeDefinition)+'_INITIALIZATION_TYPEINFO)');
     end else
     begin
      TranslateCode(TreeNode.Left, true);
      FProcCode.Add('=',spacesBOTH);
      if assigned(TreeNode.Right.Return) and (TreeNode.Return.TypeDefinition=ttdPOINTER) then begin
       FProcCode.Add('((void*)(');
       TranslateCode(TreeNode.Right);
       FProcCode.Add('))');
      end else begin
       TranslateCode(TreeNode.Right);
      end;
     end;
    end else begin
     Error.InternalError(201302222212000);
    end;
   end;
   ttntLess:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('<0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('<',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215000);
    end;
   end;
   ttntLessOrEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('<=0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('<=',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215001);
    end;
   end;
   ttntGreater:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('>0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('>',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222215002);
    end;
   end;
   ttntGreaterOrEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('>=0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('>=',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216000);
    end;
   end;
   ttntEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('==0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('==',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
     FProcCode.Add(')');
    end else begin
     Error.InternalError(201302222216001);
    end;
   end;
   ttntNotEqual:begin
    if assigned(TreeNode.Left) and assigned(TreeNode.Right) then begin
     FProcCode.Add('(');
     if IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
      DoStringCompare(TreeNode.Left, TreeNode.Right);
      FProcCode.Add('!=0');
     end else begin
      TranslateCode(TreeNode.Left);
      FProcCode.Add('!=',spacesBOTH);
      TranslateCode(TreeNode.Right);
     end;
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
       // this should happen in TranslateStringCode now and there only
       Error.InternalError(201304063283622);
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
   ttntCEXPRESSION:begin
    FProcCode.Add('(',spacesBOTH);
    while assigned(TreeNode.Left) do begin
     TranslateCode(TreeNode.Left);
     TreeNode.Left:=TreeNode.Left.Left;
    end;
    FProcCode.Add(')',spacesBOTH);
   end;
   ttntCCODE:
   begin
    while assigned(TreeNode.Left) do begin
     TranslateCode(TreeNode.Left);
     TreeNode.Left:=TreeNode.Left.Left;
    end;
    FProcCode.AddLn('');
   end;
   ttntCBLOCK:
   begin
    FProcCode.Add(HugeStringToAnsiString(TreeNode.StringData));
   end;
   ttntPASCALBLOCK:
   begin
     TranslateCode(TreeNode.Right);
   end;
   ttntCLOCATION:
   begin
     FProcCode.Add(HugeStringToAnsiString(TreeNode.StringData));
   end;
   ttntTEMPOBJECT:
   begin
     FProcCode.Add('tempObject');
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
    if TreeNode.WithLevel>=0 then begin
     FProcCode.Add(GetSymbolName(TreeNode.Symbol,'withLevel'+IntToStr(TreeNode.WithLevel)+'->'));
    end else if assigned(TreeNode.Symbol^.OwnerObjectClass) and assigned(TreeNode.Symbol^.OwnerType) then begin
     FProcCode.Add(GetSymbolName(TreeNode.Symbol,'instanceData->', not NoTypecasting));
    end else begin
     FProcCode.Add(GetSymbolName(TreeNode.Symbol));
    end;
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
         TranslateCode(TreeNode.Left, NoTypecasting);
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
         TranslateCode(TreeNode.Left, NoTypecasting);
         FProcCode.Add('))');
        end;
       end else begin
        TranslateCode(TreeNode.Left, NoTypecasting);
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
        TranslateCode(TreeNode.Left, True);
        FProcCode.Add(')))');
       end else begin
        FProcCode.Add('((void*)(');
        TranslateCode(TreeNode.Left, True);
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
        SubTreeNode := TreeNode.Left;
        while Assigned(SubTreeNode) and (SubTreeNode.TreeNodeType in [ttntTYPECONV]) do begin
         SubTreeNode:=SubTreeNode.Left;
        end;
        TranslateCode(SubTreeNode, True);
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
    FProcCode.AddLn('{');
    FProcCode.IncTab;
    ProcessTypeOrName(TreeNode.Left.Return,FProcCode);
    FProcCode.Add('* withLevel'+IntToStr(FWithStackSize)+' = ');
    if TreeNode.Left.Return^.TypeDefinition<>ttdCLASS then begin
     FProcCode.Add('&');
    end;
    FProcCode.Add('(');
    TranslateCode(TreeNode.Left);
    FProcCode.AddLn(');');
    inc(FWithStackSize);
    if FWithStackSize>=length(FWithStack) then begin
     SetLength(FWithStack,RoundUpToPowerOfTwo(FWithStackSize+16));
    end;
    FWithStack[FWithStackSize-1]:=TreeNode.Left.Return;
    TranslateCode(TreeNode.Right);
    dec(FWithStackSize);
    FProcCode.DecTab;
    FProcCode.AddLn('}');
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
     for Level := FCodeLevel downto FBreakContinueLevel[FNestedBreakCount-1]+1 do begin
      if (Level>=0) and FLevelHasTryBlock[Level] then begin
       FProcCode.AddLn('pasExceptionPopJmpBuf();');
      end;
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
     for Level := FCodeLevel downto FBreakContinueLevel[FNestedBreakCount-1]+1 do begin
      if (Level>=0) and FLevelHasTryBlock[Level] then begin
       FProcCode.AddLn('pasExceptionPopJmpBuf();');
      end;
     end;
     FProcCode.AddLn('goto '+GetSymbolName(FSelf)+'_CONTINUELABEL'+IntToStr(FContinueLabelNeeded[FNestedBreakCount-1])+';');
    end;
   end;
   ttntHALT:begin
     FProcCode.Add('exit(');

     if Assigned(TreeNode.Left) then
       TranslateCode(TreeNode.Left)
     else
       FProcCode.Add('0');  

     FProcCode.AddLn(');');
   end;
   ttntEXIT:begin
    for Level := FCodeLevel downto 0 do begin
     if FLevelHasTryBlock[Level] then begin
      FProcCode.AddLn('pasExceptionPopJmpBuf();');
     end;
    end;
    FinalizeSymbolList(SymbolManager.CurrentList, FProcCode);
    if assigned(FProcSymbol.ReturnType) then begin
     FProcCode.AddLn('return '+GetSymbolName(FProcSymbol.ResultSymbol)+';',spacesBOTH);
    end else if (tpaConstructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
     FProcCode.AddLn('return instanceData;');
    end else if (tpaDestructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
     FProcCode.AddLn('if(instanceData->INTERNAL_FIELD_VMT == pasClassVMTMask((void*)&'+GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT)){');
     MethodSymbol:=FProcSymbol^.OwnerObjectClass.RecordTable.GetSymbol('FREEINSTANCE');
     FProcCode.IncTab;
     FProcCode.Add('(('+GetTypeName(MethodSymbol^.OwnerObjectClass)+'_VMT_'+IntToStr(MethodSymbol^.VirtualIndex)+')(');
     FProcCode.Add(GetSymbolName(FProcSymbol^.OwnerObjectClass^.Symbol)+'_VMT');
     FProcCode.Add('.virtualMethods['+IntToStr(MethodSymbol^.VirtualIndex)+']))');
     FProcCode.AddLn('((void*)instanceData);');
     FProcCode.DecTab;
     FProcCode.AddLn('}');
    end else if assigned(FProcSymbol) and (tpaConstructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
     FProcCode.AddLn('return 0;');
    end else begin
     FProcCode.AddLn('return;',spacesBOTH);
    end;
   end;
   ttntFAIL:begin
    if assigned(FProcSymbol) and (tpaConstructor in FProcSymbol^.ProcedureAttributes) and assigned(FProcSymbol^.OwnerObjectClass) and (FProcSymbol^.OwnerObjectClass^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
     FinalizeSymbolList(SymbolManager.CurrentList, FProcCode);
     case FProcSymbol^.OwnerObjectClass^.TypeDefinition of
      ttdOBJECT:begin
       FProcCode.AddLn('return 1;');
      end;
      else {ttdCLASS:}begin
       // TODO: implement me!
       Error.InternalError(201304050029001); // and kill this line then! :-)
      end;
     end;
    end else begin
     Error.InternalError(201304050029000);
    end;
   end;
   ttntLABEL:begin
    FProcCode.AddLn('LABEL_'+GetSymbolName(FSelf)+'_'+TreeNode.LabelName+':')
   end;
   ttntGOTO:begin
    FProcCode.Add('goto LABEL_'+GetSymbolName(FSelf)+'_'+TreeNode.LabelName+';');
   end;
   ttntTRY:begin
    FLevelHasTryBlock[FCodeLevel]:=true;
    TryBlockCounter:=FTryBlockCounter;
    inc(FTryBlockCounter);
    FProcCode.AddLn('{');
    FProcCode.IncTab;
    FProcCode.AddLn('pasExceptionStackJmpBufItem TRY_JMPBUF_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+';');
    Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'SYSTEM');
    if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
     Symbol:=Symbol^.SymbolList.GetSymbol(tpsIdentifier+'TOBJECT');
    end;
    FProcCode.AddLn(GetTypeName(Symbol^.TypeDefinition)+' TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' = NULL;');
    FProcCode.AddLn('int TRY_VALUE_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' = setjmp(TRY_JMPBUF_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+'.jmpBuf);');
    FProcCode.AddLn('if(!TRY_VALUE_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+'){');
    FProcCode.IncTab;
    FProcCode.AddLn('pasExceptionPushJmpBuf((void*)&TRY_JMPBUF_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+');');
    if assigned(TreeNode.Block) then begin
     TranslateCode(TreeNode.Block);
    end;
    FProcCode.AddLn('pasExceptionPopJmpBuf();');
    if assigned(TreeNode.FinallyTree) then begin
     FProcCode.AddLn('goto TRY_FINALLY_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+';');
    end;
    FProcCode.DecTab;
    FProcCode.AddLn('}else{');
    FProcCode.IncTab;
    if assigned(TreeNode.ExceptTree) and (TreeNode.ExceptTree.TreeNodeType=ttntTRYONELSE) then begin
     FProcCode.AddLn('TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' = pasExceptioneGetRaiseObject();');
     FProcCode.AddLn('void* TRY_OBJECT_VMT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' = TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' ? TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+'->INTERNAL_FIELD_VMT : NULL;');
    end;
    FProcCode.AddLn('pasExceptionPopJmpBuf();');
    if assigned(TreeNode.ExceptTree) then begin
     case TreeNode.ExceptTree.TreeNodeType of
      ttntTRYONELSE:begin
       if assigned(TreeNode.ExceptTree) then begin
        SubTreeNode:=TreeNode.ExceptTree;
        while assigned(SubTreeNode) do begin
         if assigned(SubTreeNode.Symbol) then begin
          FProcCode.AddLn('if(TRY_OBJECT_VMT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+' == pasClassVMTMask((void*)&'+GetTypeName(SubTreeNode.CheckType)+'_VMT)){');
          FProcCode.IncTab;
          Symbol:=SubTreeNode.Symbol;
          Symbol^.IntValue:=FTrySymbolCounter;
          inc(FTrySymbolCounter);
          FProcCode.AddLn(GetTypeName(SubTreeNode.CheckType)+' TRY_EXCEPTION_'+GetSymbolName(FSelf)+'_'+IntToStr(Symbol^.IntValue)+' = (void*)TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+';');
          TranslateCode(SubTreeNode.Left);
          SubTreeNode:=SubTreeNode.Right;
          FProcCode.DecTab;
          if assigned(SubTreeNode) and assigned(SubTreeNode.Left) then begin
           FProcCode.Add('}else ');
          end else begin
           FProcCode.AddLn('}');
          end;
         end else if assigned(SubTreeNode.Left) then begin
          FProcCode.AddLn('{');
          FProcCode.IncTab;
          TranslateCode(SubTreeNode.Left);
          FProcCode.DecTab;
          FProcCode.AddLn('}');
          break;
         end else begin
          break;
         end;
        end;
       end;
       if assigned(TreeNode.ElseTree) and assigned(TreeNode.ElseTree.Right) then begin
        TranslateCode(TreeNode.ElseTree.Right);
       end;
      end;
      else begin
       TranslateCode(TreeNode.ElseTree);
      end;
     end;
    end;
    Symbol:=SymbolManager.GetSymbol('TOBJECT');
    if assigned(Symbol) then begin
     MethodSymbol:=Symbol^.TypeDefinition^.RecordTable.GetSymbol('FREE');
     if assigned(MethodSymbol) then begin
      FProcCode.AddLn('if(TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+'){');
      FProcCode.IncTab;
      FProcCode.AddLn(GetSymbolName(MethodSymbol)+'((void*)TRY_OBJECT_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+');');
      FProcCode.DecTab;
      FProcCode.AddLn('}');
     end;
    end;
    if assigned(TreeNode.FinallyTree) then begin
     FProcCode.AddLn('TRY_FINALLY_'+GetSymbolName(FSelf)+'_'+IntToStr(TryBlockCounter)+':');
     TranslateCode(TreeNode.FinallyTree);
    end;
    FProcCode.DecTab;
    FProcCode.AddLn('}');
    FProcCode.DecTab;
    FProcCode.AddLn('}');
    FLevelHasTryBlock[FCodeLevel]:=false;
   end;
   ttntTRYONELSE:begin
   end;
   ttntRAISE:begin
    if assigned(TreeNode.Left) then begin
     FProcCode.Add('pasExceptionRaise(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add(', ');
     if assigned(TreeNode.Right) then begin
      FProcCode.Add('(void*)((size_t)'+IntToStr(TreeNode.Right.ItemValue)+')');
     end else begin
      FProcCode.Add('NULL');
     end;
     FProcCode.AddLn(');');
    end else begin
     FProcCode.Add('pasExceptionReraise();');
    end;
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
             tstSigned8Bit: FProcCode.Add('pasWriteInt8(');
             tstSigned16Bit: FProcCode.Add('pasWriteInt16(');
             tstSigned32Bit: FProcCode.Add('pasWriteInt32(');
             tstSigned64Bit: FProcCode.Add('pasWriteInt64(');
             tstUnsigned8Bit: FProcCode.Add('pasWriteUInt8(');
             tstUnsigned16Bit: FProcCode.Add('pasWriteUInt16(');
             tstUnsigned32Bit: FProcCode.Add('pasWriteUInt32(');
             tstUnsigned64Bit: FProcCode.Add('pasWriteUInt64(');
             tstUnsignedChar,
             tstUnsignedWideChar,
             tstUnsignedHugeChar: FProcCode.Add('pasWriteChar(');
             tstFloat32Bit,
             tstFloat64Bit,
             tstFloat80Bit: FProcCode.Add('pasWriteFloat(');
            end;
           end;
           ttdBoolean: FProcCode.Add('pasWriteBool(');
           ttdShortString,
           ttdLongString: FProcCode.Add('pasWriteLongString((pasLongstring)');
           ttdFloat: FProcCode.Add('pasWriteFloat(');
           ttdPointer: FProcCode.Add('pasWritePChar(');
           else
            Error.InternalError(201303210020000);
          end
         else
          Error.InternalError(201303210030000);
         if (SubTreeNode.ReferenceParameter) then begin
          FProcCode.Add('((void*)(&(');
         end else if assigned(SubTreeNode.Left.Return) and (SubTreeNode.Left.Return^.TypeDefinition=ttdPointer) then begin
          FProcCode.Add('((void*)(');
         end;

         if Assigned(SubTreeNode.Left)and IsStringType(SubTreeNode.Left.Return, False) then begin
          TranslateStringCode(SubTreeNode.Left, @AnsistringType)
         end else begin
          TranslateCode(SubTreeNode.Left);
         end;

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
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Left.Return.PointerTo) and not assigned(TreeNode.Left.Right) then begin
        if TreeNode.Left.Left.Return.PointerTo.TypeDefinition^.TypeDefinition=ttdOBJECT then begin
         FProcCode.AddLn('{');
         FProcCode.IncTab;
         FProcCode.AddLn('void* tempObject = pasGetMem('+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
         FProcCode.AddLn('if(tempObject){');
         FProcCode.IncTab;
         FProcCode.AddLn('pasZeroMem(tempObject, '+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
         FProcCode.AddLn('(('+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'*)tempObject)->INTERNAL_FIELD_VMT = (void*)&'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_VMT;');
         if TreeNode.Left.Left.Return.PointerTo.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(TreeNode.Left.Left.Return.PointerTo.TypeDefinition) then begin
          FProcCode.AddLn('pasInitialize(tempObject, &'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_INITIALIZATION_TYPEINFO);');
         end;
         FProcCode.DecTab;
         FProcCode.AddLn('}');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(' = tempObject;');
         FProcCode.DecTab;
         FProcCode.AddLn('}');
        end else begin
         if TreeNode.Left.Left.Return.PointerTo.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(TreeNode.Left.Left.Return.PointerTo.TypeDefinition) then begin
          FProcCode.AddLn('{');
          FProcCode.IncTab;
          FProcCode.AddLn('void* tempPointer = pasGetMem('+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
          FProcCode.AddLn('pasInitialize(tempPointer, &'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_INITIALIZATION_TYPEINFO);');
          TranslateCode(TreeNode.Left.Left);
          FProcCode.AddLn(' = tempPointer;');
          FProcCode.DecTab;
          FProcCode.AddLn('}');
         end else begin
          TranslateCode(TreeNode.Left.Left);
          FProcCode.AddLn(' = pasGetMem('+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
         end;
        end;
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Left.Return.PointerTo) and not assigned(TreeNode.Left.Right.Right) then begin
        if TreeNode.Left.Left.Return.PointerTo.TypeDefinition^.TypeDefinition=ttdOBJECT then begin
         FProcCode.AddLn('{');
         FProcCode.IncTab;
         FProcCode.AddLn('void* tempObject = pasGetMem('+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
         FProcCode.AddLn('if(tempObject){');
         FProcCode.IncTab;
         FProcCode.AddLn('pasZeroMem(tempObject, '+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
         FProcCode.AddLn('(('+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'*)tempObject)->INTERNAL_FIELD_VMT = (void*)&'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_VMT;');
         if TreeNode.Left.Left.Return.PointerTo.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(TreeNode.Left.Left.Return.PointerTo.TypeDefinition) then begin
          FProcCode.AddLn('pasInitialize(tempObject, &'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_INITIALIZATION_TYPEINFO);');
         end;
         FProcCode.Add('if(');
         TranslateCode(TreeNode.Left.Right);
         FProcCode.AddLn('){');
         FProcCode.IncTab;
         FProcCode.AddLn('pasFreeMem(tempObject);');
         FProcCode.AddLn('tempObject = NULL;');
         FProcCode.DecTab;
         FProcCode.AddLn('}');
         FProcCode.DecTab;
         FProcCode.AddLn('}');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(' = tempObject;');
         FProcCode.DecTab;
         FProcCode.AddLn('}');
        end else begin
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(' = pasGetMem('+IntToStr(SymbolManager.GetSize(TreeNode.Left.Left.Return.PointerTo.TypeDefinition))+');');
        end;
       end else begin
        Error.InternalError(201304050457000);
       end;
      end;
      tipDISPOSE:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Left.Return.PointerTo) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('pasFreeMem(');
        TranslateCode(TreeNode.Left.Left);
        FProcCode.AddLn(');');
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Left.Return.PointerTo) and not assigned(TreeNode.Left.Right.Right) then begin
        if TreeNode.Left.Left.Return.PointerTo.TypeDefinition^.TypeDefinition=ttdOBJECT then begin
         FProcCode.AddLn('{');
         FProcCode.IncTab;
         FProcCode.Add('void* tempObject = &');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(';');
         TranslateCode(TreeNode.Left.Right);
         FProcCode.AddLn(';');
         if TreeNode.Left.Left.Return.PointerTo.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(TreeNode.Left.Left.Return.PointerTo.TypeDefinition) then begin
          FProcCode.AddLn('pasFinalize(tempObject, &'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_INITIALIZATION_TYPEINFO);');
         end;
         FProcCode.AddLn('pasFreeMem(tempObject);');
         FProcCode.DecTab;
         FProcCode.AddLn('}');
        end else begin
         if TreeNode.Left.Left.Return.PointerTo.TypeDefinition.InitializationTypeInfo and SymbolManager.TyppeDoNeedUseInitializationTypeInfo(TreeNode.Left.Left.Return.PointerTo.TypeDefinition) then begin
          FProcCode.AddLn('{');
          FProcCode.IncTab;
          FProcCode.Add('void* tempPointer = (void*)(');
          TranslateCode(TreeNode.Left.Left);
          FProcCode.AddLn(');');
          FProcCode.AddLn('pasFinalize(tempPointer, &'+GetTypeName(TreeNode.Left.Left.Return.PointerTo.TypeDefinition)+'_INITIALIZATION_TYPEINFO);');
          FProcCode.AddLn('pasFreeMem(tempPointer);');
          FProcCode.DecTab;
          FProcCode.AddLn('}');
         end else begin
          FProcCode.Add('pasFreeMem(');
          TranslateCode(TreeNode.Left.Left);
          FProcCode.AddLn(');');
         end;
        end;
       end else begin
        Error.InternalError(201304050529000);
       end;
      end;
      tipSETLENGTH:begin
       if Assigned(TreeNode.Left)and(Assigned(TreeNode.Left.Left)) then
       begin
        if (TreeNode.Left.Left.Return.TypeDefinition = ttdArray)and
           (TreeNode.Left.Left.Return.DynamicArray) then begin
         FProcCode.Add('pasSetLengthArray(&');
         TranslateCode(TreeNode.Left);
         FProcCode.Add(',');
         FProcCode.Add(IntToStr(GetTypeSize(TreeNode.Left.Left.Return.Definition)));
         FProcCode.Add(')');
        end;
       end;
      end;
      tipLENGTH:begin
       // TODO: Implement it!
       if assigned(TreeNode.Left) and Assigned(TreeNode.Left.Left) then
       begin
        if Assigned(TreeNode.Left.Left.Return) then
         case TreeNode.Left.Left.Return.TypeDefinition of
          ttdArray: begin
           if TreeNode.Left.Left.Return.DynamicArray then begin
            FProcCode.Add('pasLengthArray(');
            TranslateCode(TreeNode.Left.Left);
            FProcCode.Add(')');
           end;
          end;
          ttdLongString: begin
           FProcCode.Add('LengthLongstring(');
           TranslateStringCode(TreeNode.Left.Left, TreeNode.Left.Left.Return);
           FProcCode.Add(')');
          end;
          ttdShortString: begin
           if (TreeNode.Left.Left.TreeNodeType = ttntAdd) then
           begin
            FProcCode.Add('LengthLongstring(');
            TranslateStringCode(TreeNode.Left.Left, @AnsiStringType);
            FProcCode.Add(')');
           end else
           begin
            FProcCode.Add('((uint8_t*)(&');
            TranslateCode(TreeNode.Left.Left);
            FProcCode.Add('))[0]');
           end;
          end
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
      tipTYPEOF:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
        if (TreeNode.Left.Left.Return^.TypeDefinition=ttdOBJECT) and TreeNode.Left.Left.Return^.HasVirtualTable then begin
         FProcCode.Add('((void*)((');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.Add(').INTERNAL_FIELD_VMT))');
        end else begin
         Error.AddErrorCode(86);
        end;
       end else begin
        Error.InternalError(201304050127000);
       end;
      end;
      tipTYPEINFO:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
        FProcCode.Add('((void*)(&'+GetTypeName(TreeNode.Left.Left.Return)+'_INITIALIZATION_TYPEINFO))');
       end else begin
        Error.InternalError(201304111953000);
       end;
      end;
      tipINITIALIZE:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
        if TreeNode.Left.Left.Return^.InitializationTypeInfo then begin
         FProcCode.Add('(pasInitialize((void*)(&(');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(')), ((void*)(&'+GetTypeName(TreeNode.Left.Left.Return)+'_INITIALIZATION_TYPEINFO)));');
        end else begin
         if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
          Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
         end else begin
          Error.AbortCode(138,'???');
         end;
        end;
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Right.Left.Return) and not assigned(TreeNode.Left.Right.Right) then begin
        if TreeNode.Left.Left.Return^.InitializationTypeInfo then begin
         FProcCode.Add('(pasInitializeArray((void*)(&(');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.Add(')), ((void*)(&'+GetTypeName(TreeNode.Left.Left.Return)+'_INITIALIZATION_TYPEINFO)),',spacesRIGHT);
         TranslateCode(TreeNode.Left.Right.Left);
         FProcCode.AddLn(');');
        end else begin
         if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
          Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
         end else begin
          Error.AbortCode(138,'???');
         end;
        end;
       end else begin
        Error.InternalError(201304120332000);
       end;
      end;
      tipFINALIZE:begin
       if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and not assigned(TreeNode.Left.Right) then begin
        if TreeNode.Left.Left.Return^.InitializationTypeInfo then begin
         FProcCode.Add('(pasFinalize((void*)(&(');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.AddLn(')), ((void*)(&'+GetTypeName(TreeNode.Left.Left.Return)+'_INITIALIZATION_TYPEINFO)));');
        end else begin
         if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
          Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
         end else begin
          Error.AbortCode(138,'???');
         end;
        end;
       end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and assigned(TreeNode.Left.Right) and assigned(TreeNode.Left.Right.Left) and assigned(TreeNode.Left.Right.Left.Return) and not assigned(TreeNode.Left.Right.Right) then begin
        if TreeNode.Left.Left.Return^.InitializationTypeInfo then begin
         FProcCode.Add('(pasFinalizeArray((void*)(&(');
         TranslateCode(TreeNode.Left.Left);
         FProcCode.Add(')), ((void*)(&'+GetTypeName(TreeNode.Left.Left.Return)+'_INITIALIZATION_TYPEINFO)),',spacesRIGHT);
         TranslateCode(TreeNode.Left.Right.Left);
         FProcCode.AddLn(');');
        end else begin
         if assigned(TreeNode.Left.Left.Return^.Symbol) then begin
          Error.AbortCode(138,CorrectSymbolName(TreeNode.Left.Left.Return^.Symbol^.Name));
         end else begin
          Error.AbortCode(138,'???');
         end;
        end;
       end else begin
        Error.InternalError(201304120332001);
       end;
      end;
      else {tipNone:}begin
       InjectNullPointer:=false;
       if assigned(TreeNode.Symbol.OwnerModule) then begin
        if assigned(TreeNode.MethodSymbol) then begin
         if assigned(TreeNode.Right) then begin
          ObjectClassType:=TreeNode.Right.Return;
          if assigned(TreeNode.MethodSymbol) and (tpaConstructor in TreeNode.MethodSymbol^.ProcedureAttributes) and assigned(ObjectClassType) and (ObjectClassType^.TypeDefinition=ttdCLASS) then begin
           InjectNullPointer:=true;
          end;
          if assigned(TreeNode.MethodSymbol) and (tpaVirtual in TreeNode.MethodSymbol^.ProcedureAttributes) and (ObjectClassType^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
           if assigned(ObjectClassType) and (ObjectClassType^.TypeDefinition=ttdOBJECT) then begin
            // OBJECT
            FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_VMT_'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+')(((('+GetTypeName(ObjectClassType)+'*)&(');
            TranslateCode(TreeNode.Right);
            FProcCode.Add('))->INTERNAL_FIELD_VMT)->virtualMethods['+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+']))');
           end else begin
            // CLASS
            FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_VMT_'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+')(((pasClassVirtualMethodTable*)pasClassVMTUnmask(((('+GetTypeName(ObjectClassType)+')(');
            TranslateCode(TreeNode.Right);
            FProcCode.Add('))->INTERNAL_FIELD_VMT)))->virtualMethods['+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+']))');
           end;
          end else if assigned(TreeNode.MethodSymbol) and (tpaDynamic in TreeNode.MethodSymbol^.ProcedureAttributes) and (ObjectClassType^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
           if assigned(ObjectClassType) and (ObjectClassType^.TypeDefinition=ttdOBJECT) then begin
            // OBJECT
            FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_DMT_'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+'*)pasObjectDMTDispatch((void*)&(');
            TranslateCode(TreeNode.Right);
            FProcCode.Add('),'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+'))');
           end else begin
            // CLASS
            FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_DMT_'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+')pasClassDMTDispatch(((pasClassVirtualMethodTable*)pasClassVMTUnmask((('+GetTypeName(ObjectClassType)+')(');
            TranslateCode(TreeNode.Right);
            FProcCode.Add(')->INTERNAL_FIELD_VMT))),'+IntToStr(TreeNode.MethodSymbol^.VirtualIndex)+'))');
           end;
          end else begin
           if assigned(ObjectClassType) and (ObjectClassType^.TypeDefinition=ttdINTERFACE) then begin
            FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_VTABLE_NAMED_TYPE)(*((void**)(');
            TranslateCode(TreeNode.Right);
            FProcCode.Add(')))))->'+TreeNode.MethodSymbol^.Name);
           end else begin
            FProcCode.Add(GetSymbolName(TreeNode.MethodSymbol));
           end;
          end;
         end else begin
          FProcCode.Add(GetSymbolName(TreeNode.MethodSymbol));
         end;
        end else begin
         ObjectClassType:=TreeNode.Symbol^.OwnerObjectClass;
         if assigned(ObjectClassType) and (tpaVirtual in TreeNode.Symbol^.ProcedureAttributes) and (ObjectClassType^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
          if ObjectClassType^.TypeDefinition=ttdOBJECT then begin
           // OBJECT
           FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_VMT_'+IntToStr(TreeNode.Symbol^.VirtualIndex)+')(((('+GetTypeName(ObjectClassType)+'*)(');
           FProcCode.Add('instanceData');
           FProcCode.Add('))->INTERNAL_FIELD_VMT)->virtualMethods['+IntToStr(TreeNode.Symbol^.VirtualIndex)+']))');
          end else begin
           // CLASS
           FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_VMT_'+IntToStr(TreeNode.Symbol^.VirtualIndex)+')(((pasClassVirtualMethodTable*)pasClassVMTUnmask(((('+GetTypeName(ObjectClassType)+')(');
           FProcCode.Add('instanceData');
           FProcCode.Add('))->INTERNAL_FIELD_VMT)))->virtualMethods['+IntToStr(TreeNode.Symbol^.VirtualIndex)+']))');
          end;
         end else if assigned(ObjectClassType) and (tpaDynamic in TreeNode.Symbol^.ProcedureAttributes) and (ObjectClassType^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
          if ObjectClassType^.TypeDefinition=ttdOBJECT then begin
           // OBJECT
           FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_DMT_'+IntToStr(TreeNode.Symbol^.VirtualIndex)+'*)pasObjectDMTDispatch((void*)(');
           FProcCode.Add('instanceData');
           FProcCode.Add('),'+IntToStr(TreeNode.Symbol^.VirtualIndex)+'))');
          end else begin
           // CLASS
           FProcCode.Add('(('+GetTypeName(ObjectClassType)+'_DMT_'+IntToStr(TreeNode.Symbol^.VirtualIndex)+')pasClassDMTDispatch(((pasClassVirtualMethodTable*)pasClassVMTUnmask((('+GetTypeName(ObjectClassType)+')(');
           FProcCode.Add('instanceData');
           FProcCode.Add(')->INTERNAL_FIELD_VMT))),'+IntToStr(TreeNode.Symbol^.VirtualIndex)+'))');
          end;
         end else begin
          FProcCode.Add(GetSymbolName(TreeNode.Symbol));
         end;
        end;
       end else begin
        Error.InternalError(201304050609000);
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
         (({(TreeNode.Symbol.OwnerObjectClass=FProcSymbol.OwnerObjectClass) and}
          not assigned(TreeNode.MethodSymbol)) or assigned(TreeNode.InheritedType)) then begin
       if HaveParameters then
       begin
        FProcCode.Add(',',spacesRIGHT);
       end;
       if tpaClassProcedure in TreeNode.Symbol.ProcedureAttributes then begin
        if tpaClassProcedure in FProcSymbol.ProcedureAttributes then begin
         FProcCode.Add('(void*)classReference');
        end else begin
         FProcCode.Add('((void*)(instanceData->INTERNAL_FIELD_VMT))');
        end;
       end else begin
        FProcCode.Add('(void*)instanceData');
       end;
       HaveParameters:=true;
      end else if assigned(TreeNode.Right) then begin
       if HaveParameters then
       begin
        FProcCode.Add(',',spacesRIGHT);
       end;
       if TreeNode.Right.TreeNodeType=ttntTEMPOBJECT then begin
        // OBJECT
        TranslateCode(TreeNode.Right);
       end else if TreeNode.Right.Return^.TypeDefinition=ttdOBJECT then begin
        // OBJECT
        FProcCode.Add('((void*)&(');
        TranslateCode(TreeNode.Right);
        FProcCode.Add('))');
{      end else if (TreeNode.Right.Return^.TypeDefinition=ttdPOINTER) and assigned(TreeNode.Right.Return^.PointerTo) and assigned(TreeNode.Right.Return^.PointerTo^.TypeDefinition) and (TreeNode.Right.Return^.PointerTo^.TypeDefinition^.TypeDefinition=ttdOBJECT) then begin
        // OBJECT
        FProcCode.Add('(void*)(');
        TranslateCode(TreeNode.Right);
        FProcCode.Add(')');}
       end else begin
        // CLASS and INTERFACE
        if InjectNullPointer then begin
         FProcCode.Add('NULL');
        end else begin
         FProcCode.Add('(void*)(');
         TranslateCode(TreeNode.Right);
         FProcCode.Add(')');
        end;
       end;
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
        // CLASS and INTERFACE
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

      if Assigned(SubTreeNode.Left) and (not SubTreeNode.ReferenceParameter) and
         IsStringType(SubTreeNode.Left.Return) then
       TranslateStringCode(SubTreeNode.Left, SubTreeNode.Return)
      else
       TranslateCode(SubTreeNode.Left, SubTreeNode.ReferenceParameter);

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
    if (TreeNode.Left.Return.TypeDefinition = ttdArray)and
       (TreeNode.Left.Return.DynamicArray = True) then begin
     FProcCode.Add('*');
     FProcCode.Add('((' + GetTypeName(TreeNode.Left.Return.Definition)+'*)(');
     TranslateCode(TreeNode.Left);
     FProcCode.Add(' + ('+IntToStr(GetTypeSize(TreeNode.Left.Return.Definition))+'*(');
     TranslateCode(TreeNode.Right);
     FProcCode.Add('))))');
    end else begin
     TranslateCode(TreeNode.Left);
     FProcCode.Add('[');
     TranslateCode(TreeNode.Right);
     if(TreeNode.Left.Return.TypeDefinition = ttdLongString) then
       FProcCode.Add('-1');
     FProcCode.Add(']');
    end;
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
     if (TreeNode.Left.TreeNodeType = ttntPointer) then
     begin
      // pointer type field access in c is "myPointerTypeVar->myFieldEntry"
      TranslateCode(TreeNode.Left.Left, NoTypecasting);
      FProcCode.Add('->');
     end else if (TreeNode.Left.TreeNodeType = ttntIndex) and assigned(TreeNode.Left.Left) and assigned(TreeNode.Left.Left.Return) and TreeNode.Left.Left.Return.DynamicArray then begin
      FProcCode.FIgnoreNextToken := True;
      TranslateCode(TreeNode.Left, NoTypecasting);
      FProcCode.Add('->');
     end else if assigned(TreeNode.Left) and assigned(TreeNode.Left.Return) and (TreeNode.Left.Return.TypeDefinition=ttdCLASS) then begin
      TranslateCode(TreeNode.Left, NoTypecasting);
      FProcCode.Add('->');
     end else begin
       TranslateCode(TreeNode.Left, NoTypecasting);
       FProcCode.Add('.');
     end;
     if assigned(TreeNode.SymbolField) then begin
      FProcCode.Add(GetSymbolName(TreeNode.SymbolField,'',false));
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
       if (TreeNode.Value>=0) and (TreeNode.Value<=$7fffffff) then begin
        FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
       end else begin
        FProcCode.Add(IntToStr(TreeNode.Value)+'l',spacesBOTH);
       end;
      end;
      tstSigned64Bit:begin
       if (TreeNode.Value>=0) and (TreeNode.Value<=$7fffffff) then begin
        FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
       end else begin
        FProcCode.Add(IntToStr(TreeNode.Value)+'ll',spacesBOTH);
       end;
      end;
      tstUnsigned8Bit,tstUnsignedChar:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstUnsigned16Bit,tstUnsignedWideChar:begin
       FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
      end;
      tstUnsigned32Bit:begin
       if (TreeNode.Value>=0) and (TreeNode.Value<=$7fffffff) then begin
        FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
       end else begin
        FProcCode.Add(IntToStr(TreeNode.Value)+'ul',spacesBOTH);
       end;
      end;
      tstUnsigned64Bit:begin
       if (TreeNode.Value>=0) and (TreeNode.Value<=$7fffffff) then begin
        FProcCode.Add(IntToStr(TreeNode.Value),spacesBOTH);
       end else begin
        FProcCode.Add(IntToStr(TreeNode.Value)+'ull',spacesBOTH);
       end;
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
   else begin
    Error.InternalError(201302222201000);
   end;
  end;
  dec(FCodeLevel);
 end;
end;

procedure TCodeGenC.TranslateStringCode(TreeNode: TTreeNode;
  DesiredStringType: PType);
{ this function enforces a specific string type, used for stringvar assignments
  and string parameter passing. }
var DoConversion: Boolean;

begin
 if (not assigned(TreeNode.Return)) or
   ( (TreeNode.Return.TypeDefinition <> ttdLongString) and
     (TreeNode.Return.TypeDefinition <> ttdShortString) ) then
  Error.InternalError(20130406173842);

 if DesiredStringType.TypeDefinition = ttdShortString then
 begin
  case TreeNode.TreeNodeType of
   ttntVAR:begin
    if TreeNode.Return.TypeDefinition = ttdShortString then begin
     TranslateCode(TreeNode);
    end else
    begin
     FProcCode.Add('pasToShortstring'+IntToStr(DesiredStringType.Length)+'(');
     TranslateStringCode(TreeNode, @AnsistringType);
     FProcCode.Add(')');
    end;
   end;
   ttntAdd,ttntStringconst:begin
    FProcCode.Add('pasToShortstring'+IntToStr(DesiredStringType.Length)+'(');
    TranslateStringCode(TreeNode, @AnsistringType);
    FProcCode.Add(')');
   end;
   ttntTypeConv:
   begin
    //FProcCode.Add('pasToShortstring'+IntToStr(DesiredStringType.Length)+'(');
    TranslateStringCode(TreeNode.Left, DesiredStringtype);
    //FProcCode.Add(')');
   end;
   else begin
    Error.InternalError(20130408100827);
    Writeln(Cardinal(TreeNode.TreeNodeType));
   end;
  end;
 end else
 case TreeNode.TreeNodeType of
  // string concat
  ttntAdd:begin
   if assigned(TreeNode.Left) and assigned(TreeNode.Right) and
      IsStringType(TreeNode.Left.Return) and IsStringType(TreeNode.Right.Return) then begin
    FProcCode.Add('AddLongstring((pasLongstring)');
    TranslateStringCode(TreeNode.Left, DesiredStringType);
    FProcCode.Add(', (pasLongstring)');
    TranslateStringCode(TreeNode.Right, DesiredStringType);
    FProcCode.Add(')');
   end else begin
    Error.InternalError(20130406173827);
   end;
  end;
   ttntIndex:begin
    if TreeNode.Left.Return = DesiredStringType then
     TranslateCode(TreeNode) else
     TranslateCode(TreeNode);
   end;
  ttntTYPECONV:begin
   if assigned(TreeNode.Return) and assigned(TreeNode.Left.Return) and (TreeNode.Left.Return<>TreeNode.Return) then begin
   if (not IsStringType(TreeNode.Return)) or (not IsStringType(TreeNode.Left.Return)) then
     Error.InternalError(20130406173437);

    if (TreeNode.Left.TreeNodeType = ttntAdd)or(TreeNode.Left.TreeNodeType = ttntTYPECONV) then
     TranslateStringCode(TreeNode.Left, DesiredStringType)
    else
    // this is either clever or extremely stupid: we directly convert into the desired string type
    // or skip conversion if the types are the same
    if TreeNode.Left.Return = DesiredStringType then
     TranslateCode(TreeNode.Left)
    else if (TreeNode.Left.Return.TypeDefinition = ttdShortString) and
         (DesiredStringType.TypeDefinition = ttdLongString) then
    begin
     FProcCode.Add('ConvertShortstring('+ IntToStr(DesiredStringType^.LongStringCodePage)+',');
     case DesiredStringType^.LongStringType of
      tstUnsignedChar: FProcCode.Add('1');
      tstUnsignedWideChar: FProcCode.Add('2');
      tstUnsignedHugeChar: FProcCode.Add('4');
     end;
     FProcCode.Add(',&');
     TranslateCode(TreeNode.Left);
     FProcCode.Add(')');
    end else begin
     FProcCode.Add('ConvertLongstring('+ IntToStr(DesiredStringType^.LongStringCodePage)+',');
     case DesiredStringType^.LongStringType of
      tstUnsignedChar: FProcCode.Add('1');
      tstUnsignedWideChar: FProcCode.Add('2');
      tstUnsignedHugeChar: FProcCode.Add('4');
     end;
     FProcCode.Add(',');
     TranslateCode(TreeNode.Left);
     FProcCode.Add(')');
    end;
   end;
  end;

  ttntSTRINGConst, ttntVar, ttntCHARConst, ttntCall, ttntField:
  begin
   // for string consts, we could actually just convert them into the right string type

   if (DesiredStringType.TypeDefinition = ttdLongString)and
      (TreeNode.Return.TypeDefinition = ttdShortString)then begin
    DoConversion := True;
     FProcCode.Add('ConvertShortstring('+ IntToStr(DesiredStringType^.LongStringCodePage)+',');
     case DesiredStringType^.LongStringType of
      tstUnsignedChar: FProcCode.Add('1');
      tstUnsignedWideChar: FProcCode.Add('2');
      tstUnsignedHugeChar: FProcCode.Add('4');
     end;
     FProcCode.Add(',&');
   end else begin
    DoConversion :=(TreeNode.Return.LongStringType <> DesiredStringType^.LongStringType) or
                   (TreeNode.Return.LongStringCodePage <> DesiredStringType^.LongStringCodePage);

    if DoConversion then begin
     FProcCode.Add('ConvertLongstring('+ IntToStr(DesiredStringType^.LongStringCodePage)+',');
     case DesiredStringType^.LongStringType of
      tstUnsignedChar: FProcCode.Add('1');
      tstUnsignedWideChar: FProcCode.Add('2');
      tstUnsignedHugeChar: FProcCode.Add('4');
     end;
     FProcCode.Add(',');
    end;
   end;

   TranslateCode(TreeNode);

   if DoConversion then begin
    FProcCode.Add(')');
   end;
  end;
  else
  begin
   Error.InternalError(20130406143876);
   Writeln(Integer(TreeNode.TreeNodeType));
  end;
 end;
end;

procedure TCodeGenC.TranslateClass(Symbol: PSymbol; Target: TCodeWriter);
begin
 Target.AddLn('//class ' + Symbol.Name);
end;

procedure TCodeGenC.TranslateConstant(Symbol: PSymbol; Target: TCodeWriter);
begin
 if Target = FHeader then
 begin
  Target.Add('extern const ' + GetSymbolName(Symbol) + ';// = ' + ConvertConstSymbol(Symbol));
  FCode.AddLn('const ' + GetSymbolName(Symbol) + ' = ' + ConvertConstSymbol(Symbol)+';');
 end else
 Target.Add('const ' + GetSymbolName(Symbol) + ' = ' + ConvertConstSymbol(Symbol));
end;

procedure TCodeGenC.TranslateConstants;
var cons: PConstant;
begin
 cons := SymbolManager.ConstantList.First;
end;

procedure TCodeGenC.TranslateFunction(Symbol: PSymbol; Target: TCodeWriter);
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

   Target.Add('inline ');
   if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
    Target.Add('int',spacesRIGHT);
   end else begin
    ProcessTypeOrName(Symbol.ReturnType, Target);
   end;
   Target.Add(GetSymbolName(Symbol)+'(',spacesLEFT);
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
  ProcessFunctionType(Symbol, Symbol.ReturnType, Symbol.Parameter, 'PROC_'+Symbol.OverloadedName, Target);
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
   if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
    Target.AddLn(';');
    Target.Add('extern ',spacesRIGHT);
    ConvertFuncSymbol(Symbol, Target, true);
   end else if (tpaDestructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdCLASS) then begin
    Target.AddLn(';');
    Target.Add('extern ',spacesRIGHT);
    ConvertFuncSymbol(Symbol, Target, false, true);
   end;
   Symbol := Symbol.NextOverloaded;
   if Assigned(Symbol) then
     Target.AddLn(';');
 until not Assigned(Symbol);
end;

procedure TCodeGenC.TranslateObject(Symbol: PSymbol; Target: TCodeWriter);
begin
  Target.AddLn('//object ' + Symbol.Name);

end;

procedure TCodeGenC.TranslateProcedure(Symbol: PSymbol; Target: TCodeWriter);
begin
 TranslateFunction(Symbol, Target);
end;

procedure TCodeGenC.TranslateTemp(Symbol: PSymbol; Target: TCodeWriter);
begin
 Target.AddLn('//Reg ' + Symbol.Name);
end;

function UIntToCString(const Value: Cardinal): ansistring;
begin
  result := '\x'+IntToHex(Byte(Value shr 0), 2) +
            '\x'+IntToHex(Byte(Value shr 8), 2) +
            '\x'+IntToHex(Byte(Value shr 16), 2) +
            '\x'+IntToHex(Byte(Value shr 24), 2);
end;

function TCodeGenC.TranslateStringConstant(
  ConstantStr: THugeString; ATarget: TCodeWriter = nil): ansistring;
var AStr: ansistring;
begin
  result := GetSymbolName(FSelf) +'_STRING_CONST_'+INTTOSTR(FStringConstCount);
  Inc(FStringConstCount);

  AStr := HugeStringToAnsiString(ConstantStr);

  if assigned(ATarget) then begin
    ATarget.AddLn('static const char '+result+'_DATA['+IntToStr(Length(AStr)+17)+'] = "' + UIntToCString(65535)+UIntToCSTring(1)+
                  UIntToCString($FFFFFFFF)+UIntToCString(Length(AStr))+'" ' +
                  AnsiStringEscape(AStr+#0,True) + ';');
    ATarget.AddLn('void* '+result+' = (void*)(((void*)(&'+result+'_DATA))+16);');
  end else begin
    FProcCode.InsertAtMark('static const char '+result+'_DATA['+IntToStr(Length(AStr)+17)+'] = "' + UIntToCString(65535)+UIntToCSTring(1)
                         +UIntToCString($FFFFFFFF)+UIntToCString(Length(AStr))+'" ' +
                         AnsiStringEscape(AStr+#0,True) + ';');
    FProcCode.InsertAtMark('void* '+result+' = (void*)(((void*)(&'+result+'_DATA))+16);');
  end;
end;

function TCodeGenC.TranslateStringConstantDataOnly(
  ConstantStr: THugeString; ATarget: TCodeWriter): ansistring;
var AStr: ansistring;
begin
  result := GetSymbolName(FSelf) +'_STRING_CONST_'+INTTOSTR(FStringConstCount);
  Inc(FStringConstCount);
  AStr := HugeStringToAnsiString(ConstantStr);
  if length(AStr)=0 then begin
   result:='NULL';
  end else begin
   if assigned(ATarget) then begin
    ATarget.AddLn('static const char '+result+'_DATA['+IntToStr(Length(AStr)+17)+'] = "' + UIntToCString(65535)+UIntToCSTring(1)+
                  UIntToCString($FFFFFFFF)+UIntToCString(Length(AStr))+'" ' +
                  AnsiStringEscape(AStr+#0,True) + ';');
   end else begin
    FProcCode.InsertAtMark('static const char '+result+'_DATA['+IntToStr(Length(AStr)+17)+'] = "' + UIntToCString(65535)+UIntToCSTring(1)
                         +UIntToCString($FFFFFFFF)+UIntToCString(Length(AStr))+'" ' +
                         AnsiStringEscape(AStr+#0,True) + ';');
   end;
   result:='((void*)(((void*)(&'+result+'_DATA))+16))';
  end;
end;

procedure TCodeGenC.TranslateShortStringConstant(const Name:ansistring; const ConstantStr: ShortString; ATarget: TCodeWriter);
begin
  ATarget.AddLn('static const char '+Name+'['+IntToStr(Length(ConstantStr)+1)+'] = "\x' + IntToHex(Byte(length(ConstantStr)), 2) + '" ' + AnsiStringEscape(ConstantStr,True) + ';');
end;

procedure TCodeGenC.TranslateShortStringConstant(const ConstantStr: ShortString; ATarget: TCodeWriter);
begin
  ATarget.Add('"\x' + IntToHex(Byte(length(ConstantStr)), 2) + '" ' + AnsiStringEscape(ConstantStr,True));
end;

function TCodeGenC.GetShortStringConstant(const ConstantStr: ShortString): ansistring;
begin
  result := '"\x' + IntToHex(Byte(length(ConstantStr)), 2) + '" ' + AnsiStringEscape(ConstantStr,True);
end;

procedure TCodeGenC.TranslateMethodList(List: TSymbolList; ATarget: TCodeWriter = nil);
var sym: PSymbol;
    Symbol: PSymbol;
    Target: TCodeWriter;
    LastProcStruct:boolean;
begin
 sym := List.First;
 LastProcStruct:=false;

 while Assigned(sym) do
 begin
  if not (tsaMapped in Sym^.Attributes) then begin

   if Assigned(ATarget) then
    Target := ATarget
   else if (tsaPublicUnitSymbol in sym.Attributes) then
    Target := FHeader
   else
    Target := FCode;

   case Sym.SymbolType of
    tstProcedure,
    tstFunction:begin
     Symbol:=sym;
     repeat
      if assigned(Symbol.MethodSymbol) and (tsaMethodDefined in Symbol.MethodSymbol.Attributes) then begin
       Target.Add('extern ',spacesRIGHT);
       ConvertFuncSymbol(Symbol, Target);
       if Assigned(Symbol) then
         Target.AddLn(';');
      end;
      Symbol := Symbol.NextOverloaded;
     until not Assigned(Symbol);
    end;
   end;

   if Sym.SymbolType<>tstUnit then
     if LastProcStruct then
       FProcStruct.AddLn(';')
      else
       Target.AddLn(';');

  end;
  sym := sym.Next;
 end;

 dec(FDepth);
end;

procedure TCodeGenC.TranslateSymbolList(List: TSymbolList; IgnoreTypes: boolean; ATarget: TCodeWriter = nil);
var sym: PSymbol;
    Target: TCodeWriter;
    LastProcStruct:boolean;
begin
 inc(FDepth);

 sym := List.First;

 while Assigned(sym) do
 begin
  if not (tsaMapped in Sym^.Attributes) then begin

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
    tstType:begin

    end;
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

  end;
  sym := sym.Next;
 end;

 dec(FDepth);
end;

procedure TCodeGenC.ProcessTypedConstant(var Constant: PConstant; AType: PType;
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

  ttdShortString: Target.Add('{ "\x'+IntToHex(Length(Constant.ShortStringValue), 2)+'" '+AnsiStringEscape(Constant.ShortStringValue)+' }',spacesBOTH);
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

procedure TCodeGenC.ProcessTypeOrName(AType: PType; Target: TCodeWriter; OwnType: PType = nil);
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

procedure TCodeGenC.TranslateUnit(Symbol: PSymbol; Target: TCodeWriter);
begin
  Target.AddLn('//unit ' + Symbol.Name);
  Target.AddInclude(CorrectSymbolName(Symbol.Name)+'.h');
end;

procedure TCodeGenC.TranslateVariable(Symbol: PSymbol; Target: TCodeWriter);
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

procedure TCodeGenC.SaveToStreams(CodeStream, HeaderStream: TBeRoStream);
begin
  FCode.ExportStream(CodeStream);
  CodeStream.Append(FProcCode.FData);
  CodeStream.Append(FModuleCode.FData);
  FHeader.ExportStream(HeaderStream);
end;

procedure TCodeGenC.StartBreakContinuePart;
var i,j:longint;
begin
 i:=FNestedBreakCount;
 inc(FNestedBreakCount);
 if FNestedBreakCount > length(FBreakLabelNeeded) then begin
  j:=1;
  while j<=FNestedBreakCount do begin
   inc(j,j);
  end;
  SetLength(FBreakLabelNeeded, j);
  SetLength(FContinueLabelNeeded, j);
  SetLength(FBreakContinueLevel, j);
 end;
 FBreakLabelNeeded[i] := -1;
 FContinueLabelNeeded[i] := -1;
 FBreakContinueLevel[i] := FCodeLevel;
end;

procedure TCodeGenC.StopBreakPart;
begin
 if FBreakLabelNeeded[FNestedBreakCount-1] <> -1 then begin
  FProcCode.AddLn(';');
  FProcCode.AddLn(GetSymbolName(FSelf)+'_BREAKLABEL'+IntToStr(FBreakLabelNeeded[FNestedBreakCount-1])+': ;');
 end;
 dec(FNestedBreakCount);
end;

procedure TCodeGenC.StopContinuePart;
begin
 if FContinueLabelNeeded[FNestedBreakCount-1] <> -1 then begin
  FProcCode.AddLn(';');
  FProcCode.AddLn(GetSymbolName(FSelf)+'_CONTINUELABEL'+IntToStr(FContinueLabelNeeded[FNestedBreakCount-1])+': ;');
 end;
end;

procedure TCodeGenC.AddCode(const Input: string);
begin
  FCode.AddLn(Input);
end;

procedure TCodeGenC.AddHeader(const Input: string);
begin
  FHeader.AddLn(Input);
end;

function TCodeGenC.AnsiStringEscape(const Input: ansistring; Quotes: Boolean = True): ansistring;
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

function TCodeGenC.WideStringEscape(const Input: widestring): ansistring;
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

procedure TCodeGenC.TranslateModuleTypes(ModuleSymbol:PSymbol;Target,CodeTarget:TCodeWriter);
type PGUIDCastedBytes=^TGUIDCastedBytes;
     TGUIDCastedBytes=array[0..15] of byte;
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
     Type_,CurrentType:PType;
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
        CurrentType:=Type_;
        while assigned(CurrentType) do begin
         SymbolList:=CurrentType^.RecordTable;
         if assigned(SymbolList) then begin
          Symbol:=SymbolList.First;
          while assigned(Symbol) do begin
           case Symbol^.SymbolType of
            Symbols.tstVariable:begin
             if assigned(Symbol^.TypeDefinition) and not (((Symbol^.TypeDefinition.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.PointerTo)) and ((Symbol^.TypeDefinition^.PointerTo^.TypeDefinition=Type_) {or (Symbol^.TypeDefinition^.PointerTo^.TypeDefinition=CurrentType)})) then begin
              Index:=TypePointerList.IndexOf(Symbol^.TypeDefinition);
              if (Index>=0) and (DependencyItems[Counter].Dependencies.IndexOf(Index)<0) then begin
               DependencyItems[Counter].Dependencies.Add(Index);
              end;
             end;
            end;
           end;
           Symbol:=Symbol^.Next;
          end;
          if assigned(CurrentType^.ChildOf) then begin
           CurrentType:=CurrentType^.ChildOf^.TypeDefinition;
          end else begin
           break;
          end;
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
var i,j,k,Value,VirtualIndexCountOffset:longint;
    Type_,CurrentType:PType;
    TypeChain:TPointerList;
    Name:ansistring;
    SymbolList:TSymbolList;
    MethodList:TStringList;
    MethodTableList:TStringList;
    FieldTableList:TPointerList;
    ClassTableList:TPointerList;
    PropNameList:TStringList;
    ParamNameList:TStringList;
    TypeNameList:TStringList;
    Symbol,OtherSymbol:PSymbol;
    CurrentVariantLevelIndex:longint;
    VariantLevelVariants:array of integer;
    HasLast,HasDynamicMethods,DoRuntimeTypeInfo,DoGeneralTypeInfo:boolean;
    DumpedRTTIList:TStringList;
    ImplementedInterface:TImplementedInterface;
    GUIDCastedBytes:PGUIDCastedBytes;
    UnitNameString,NameString:ansistring;
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

  Symbol:=SymbolManager.GetSymbol(tpsIdentifier+'SYSTEM');
  if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
   Symbol:=Symbol^.SymbolList.GetSymbol(tpsIdentifier+'TOBJECT');
   VirtualIndexCountOffset:=Symbol^.TypeDefinition^.VirtualIndexCount;
  end else begin
   VirtualIndexCountOffset:=0;
  end;

  SortTypes;

  (*
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
    ttdInterface:begin
     Target.AddLn('typedef struct '+Name+'_VTABLE_NAMED_TYPE;');
    end;
   end;
  end;
  Target.AddLn(''); *)

  UnitNameString:=TranslateStringConstantDataOnly(UTF8ToHugeString(ModuleSymbol.OriginalCaseName),CodeTarget);
  Target.AddLn('extern static void* '+GetSymbolName(ModuleSymbol)+'_UNIT_NAME;');
  CodeTarget.AddLn('static void* '+GetSymbolName(ModuleSymbol)+'_UNIT_NAME = '+UnitNameString+';');

  DumpedRTTIList:=TStringList.Create;
  try
   Target.AddLn('// Type info definitions');
   for i:=0 to length(TypeItems)-1 do begin
    Type_:=TypeItems[i];
    Name:=GetTypeName(Type_);
    if (DumpedRTTIList.IndexOf(Name)<0) and not
       ((Type_^.TypeDefinition in [ttdCEXPRESSION]) or
        (Type_=SymbolManager.TypeEmpty) or
        ((length(Name)>0) and (Name[1]='$'))) then begin

     DumpedRTTIList.Add(Name);

     DoRuntimeTypeInfo:=Type_^.RuntimeTypeInfo and not Type_^.RuntimeTypeInfoDumped;
     Type_^.RuntimeTypeInfoDumped:=true;

     DoGeneralTypeInfo:=(Type_^.InitializationTypeInfo or not (Type_.TypeKind in [TypeKindUnknown,TypeKindRecord,TypeKindArray])) and not Type_^.GeneralTypeInfoDumped;
     Type_^.GeneralTypeInfoDumped:=true;

     if DoRuntimeTypeInfo or DoGeneralTypeInfo then begin
      if assigned(Type_.Symbol) then begin
       NameString:=TranslateStringConstantDataOnly(UTF8ToHugeString(Type_.Symbol.OriginalCaseName),CodeTarget);
      end else begin
       NameString:='NULL';
      end;
     end else begin
      NameString:='';
     end;

     if DoRuntimeTypeInfo then begin
      case Type_.TypeKind of
       TypeKindInteger,TypeKindAnsiChar,TypeKindEnumeration,TypeKindSet,TypeKindWideChar,TypeKindHugeChar:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;       
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('uint8_t ordType;');
        case Type_.TypeKind of
         TypeKindInteger,TypeKindAnsiChar,TypeKindEnumeration,TypeKindWideChar,TypeKindHugeChar:begin
          Target.AddLn('int64_t minValue;');
          Target.AddLn('int64_t maxValue;');
          case Type_.TypeKind of
           TypeKindEnumeration:begin
            Target.AddLn('void* baseType;');
            Target.AddLn('void* nameList;');
           end;
          end;
         end;
         TypeKindSet:begin
          Target.AddLn('void* compType;');
         end;
        end;
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        case Type_.TypeKind of
         TypeKindInteger:begin
          case Type_.SubRangeType of
           tstSigned8Bit:begin
            CodeTarget.AddLn(IntToStr(otSByte)+',');
           end;
           tstSigned16Bit:begin
            CodeTarget.AddLn(IntToStr(otSWord)+',');
           end;
           tstSigned32Bit:begin
            CodeTarget.AddLn(IntToStr(otSLong)+',');
           end;
           tstSigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otSQuad)+',');
           end;
           tstUnsigned8Bit,tstUnsignedChar:begin
            CodeTarget.AddLn(IntToStr(otUByte)+',');
           end;
           tstUnsigned16Bit,tstUnsignedWideChar:begin
            CodeTarget.AddLn(IntToStr(otUWord)+',');
           end;
           tstUnsigned32Bit,tstUnsignedHugeChar:begin
            CodeTarget.AddLn(IntToStr(otULong)+',');
           end;
           tstUnsigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otUQuad)+',');
           end;
          end;
          CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+'ll,');
          CodeTarget.AddLn(IntToStr(Type_^.UpperLimit)+'ll');
         end;
         TypeKindAnsiChar:begin
          CodeTarget.AddLn(IntToStr(otUByte)+',');
          CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+'ll,');
          CodeTarget.AddLn(IntToStr(Type_^.UpperLimit)+'ll');
         end;
         TypeKindEnumeration:begin
          case Type_.SubRangeType of
           tstSigned8Bit:begin
            CodeTarget.AddLn(IntToStr(otSByte)+',');
           end;
           tstSigned16Bit:begin
            CodeTarget.AddLn(IntToStr(otSWord)+',');
           end;
           tstSigned32Bit:begin
            CodeTarget.AddLn(IntToStr(otSLong)+',');
           end;
           tstSigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otSQuad)+',');
           end;
           tstUnsigned8Bit,tstUnsignedChar:begin
            CodeTarget.AddLn(IntToStr(otUByte)+',');
           end;
           tstUnsigned16Bit,tstUnsignedWideChar:begin
            CodeTarget.AddLn(IntToStr(otUWord)+',');
           end;
           tstUnsigned32Bit,tstUnsignedHugeChar:begin
            CodeTarget.AddLn(IntToStr(otULong)+',');
           end;
           tstUnsigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otUQuad)+',');
           end;
          end;
          CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+'ll,');
          CodeTarget.AddLn(IntToStr(Type_^.UpperLimit)+'ll,');
          if assigned(Type_^.Definition) and Type_^.Definition^.RuntimeTypeInfo then begin
           CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.Definition)+'_RUNTIME_TYPEINFO,');
          end else begin
           CodeTarget.AddLn('NULL,');
          end;
          CodeTarget.AddLn('NULL');
         end;
         TypeKindSet:begin
          case Type_.SubRangeType of
           tstSigned8Bit:begin
            CodeTarget.AddLn(IntToStr(otSByte)+',');
           end;
           tstSigned16Bit:begin
            CodeTarget.AddLn(IntToStr(otSWord)+',');
           end;
           tstSigned32Bit:begin
            CodeTarget.AddLn(IntToStr(otSLong)+',');
           end;
           tstSigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otSQuad)+',');
           end;
           tstUnsigned8Bit,tstUnsignedChar:begin
            CodeTarget.AddLn(IntToStr(otUByte)+',');
           end;
           tstUnsigned16Bit,tstUnsignedWideChar:begin
            CodeTarget.AddLn(IntToStr(otUWord)+',');
           end;
           tstUnsigned32Bit,tstUnsignedHugeChar:begin
            CodeTarget.AddLn(IntToStr(otULong)+',');
           end;
           tstUnsigned64Bit:begin
            CodeTarget.AddLn(IntToStr(otUQuad)+',');
           end;
          end;
          if assigned(Type_^.SetOf) and Type_^.SetOf^.RunTimeTypeInfo then begin
           CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.SetOf)+'_RUNTIME_TYPEINFO');
          end else begin
           CodeTarget.AddLn('NULL');
          end;
         end;
         TypeKindWideChar:begin
          CodeTarget.AddLn(IntToStr(otUWord)+',');
          CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+'ll,');
          CodeTarget.AddLn(IntToStr(Type_^.UpperLimit)+'ll');
         end;
         TypeKindHugeChar:begin
          CodeTarget.AddLn(IntToStr(otULong)+',');
          CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+'ll,');
          CodeTarget.AddLn(IntToStr(Type_^.UpperLimit)+'ll');
         end;
        end;
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindFloat:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('uint8_t floatType;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        case Type_^.TypeDefinition of
         ttdCurrency:begin
          CodeTarget.AddLn(IntToStr(ftCurr));
         end;
         else begin
          case Type_^.FloatType of
           tstFloat32Bit:begin
            CodeTarget.AddLn(IntToStr(ftSingle));
           end;
           tstFloat64Bit:begin
            CodeTarget.AddLn(IntToStr(ftDouble));
           end;
           tstFloat80Bit:begin
            CodeTarget.AddLn(IntToStr(ftExtended));
           end;
          end;
         end;
        end;
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindString:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('uint8_t maxLength;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        CodeTarget.AddLn(IntToStr(Type_^.Length));
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindClass:begin
        PropNameList:=TStringList.Create;
        try
         k:=0;
         if assigned(Type_.RecordTable) then begin
          Symbol:=Type_.RecordTable.First;
          while assigned(Symbol) do begin
           case Symbol^.SymbolType of
            Symbols.tstProperty:begin
             if tsaOOPPublished in Symbol^.Attributes then begin
              PropNameList.Add(TranslateStringConstantDataOnly(UTF8ToHugeString(Symbol^.OriginalCaseName),CodeTarget));
              inc(k);
             end;
            end;
           end;
           Symbol:=Symbol^.Next;
          end;
         end;
         Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
         Target.IncTab;
         Target.AddLn('uint8_t kind;');
         Target.AddLn('void* name;');
         Target.AddLn('void* classType;');
         Target.AddLn('void* parentInfo;');
         Target.AddLn('uint32_t propCount;');
         Target.AddLn('void* unitName;');
         if k>0 then begin
          Target.AddLn('pasPropInfo props['+IntToStr(k)+'];');
         end;
         Target.DecTab;
         Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
         Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
         CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
         CodeTarget.IncTab;
         CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
         CodeTarget.AddLn(NameString+',');
         CodeTarget.AddLn('(void*)pasClassVMTMask((void*)&'+Name+'_VMT),');
         if assigned(Type_^.ChildOf) and Type_^.ChildOf^.TypeDefinition^.RuntimeTypeInfo then begin
          CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.ChildOf^.TypeDefinition)+'_RUNTIME_TYPEINFO,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         CodeTarget.AddLn('0,');
         if assigned(Type_.RecordTable) then begin
          j:=0;
          CodeTarget.AddLn(UnitNameString+',');
          Symbol:=Type_.RecordTable.First;
          while assigned(Symbol) do begin
           case Symbol^.SymbolType of
            Symbols.tstProperty:begin
             if tsaOOPPublished in Symbol^.Attributes then begin
              dec(k);
              CodeTarget.AddLn('{');
              CodeTarget.IncTab;
              if assigned(Symbol.PropertyType) and Symbol.PropertyType^.RuntimeTypeInfo then begin
               CodeTarget.AddLn('(void*)&'+GetTypeName(Symbol.PropertyType)+'_RUNTIME_TYPEINFO,');
              end else begin
               CodeTarget.AddLn('NULL,');
              end;
              if assigned(Symbol.PropertyRead) then begin
               CodeTarget.AddLn('(void*)&'+GetSymbolName(Symbol.PropertyRead)+',');
              end else begin
               CodeTarget.AddLn('NULL,');
              end;
              if assigned(Symbol.PropertyWrite) then begin
               CodeTarget.AddLn('(void*)&'+GetSymbolName(Symbol.PropertyWrite)+',');
              end else begin
               CodeTarget.AddLn('NULL,');
              end;
              if assigned(Symbol.PropertyStored) and (Symbol.PropertyStored.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction]) then begin
               CodeTarget.AddLn('(void*)&'+GetSymbolName(Symbol.PropertyStored)+',');
              end else begin
               CodeTarget.AddLn('NULL,');
              end;
              if assigned(Symbol.PropertyIndex) and (Symbol.PropertyIndex.SymbolType=Symbols.tstConstant) and (Symbol.PropertyIndex.ConstantType=tctOrdinal) then begin
               CodeTarget.AddLn(IntToStr(Symbol.PropertyIndex.IntValue)+',');
              end else begin
               CodeTarget.AddLn('NULL,');
              end;
              if assigned(Symbol.PropertyDefault) and (Symbol.PropertyDefault.SymbolType=Symbols.tstConstant) and (Symbol.PropertyDefault.ConstantType=tctOrdinal) then begin
               CodeTarget.AddLn(IntToStr(Symbol.PropertyDefault.IntValue)+',');
              end else begin
               CodeTarget.AddLn('-2147483648,');
              end;
              CodeTarget.AddLn(IntToStr(j)+',');
              CodeTarget.AddLn(PropNameList[j]);
              inc(j);
              CodeTarget.DecTab;
              if k>0 then begin
               CodeTarget.AddLn('},');
              end else begin
               CodeTarget.AddLn('}');
              end;
             end;
            end;
           end;
           Symbol:=Symbol^.Next;
          end;
         end else begin
          CodeTarget.AddLn(UnitNameString);
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
         CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
        finally
         PropNameList.Free;
        end;
       end;
       TypeKindMethod:begin
        ParamNameList:=TStringList.Create;
        TypeNameList:=TStringList.Create;
        try
         k:=0;
         if assigned(Type_.Parameter) then begin
          Symbol:=Type_.Parameter.First;
          while assigned(Symbol) do begin
           if not (tsaHiddenParameter in Symbol^.Attributes) then begin
            ParamNameList.Add(TranslateStringConstantDataOnly(UTF8ToHugeString(Symbol^.OriginalCaseName),CodeTarget));
            TypeNameList.Add(TranslateStringConstantDataOnly(UTF8ToHugeString(GetOriginalTypeName(Symbol^.TypeDefinition)),CodeTarget));
            inc(k);
           end;
           Symbol:=Symbol^.Next;
          end;
         end;
         Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
         Target.IncTab;
         Target.AddLn('uint8_t kind;');
         Target.AddLn('void* name;');
         Target.AddLn('uint8_t methodKind;');
         Target.AddLn('uint8_t paramCount;');
         if k>0 then begin
          Target.AddLn('pasParamInfo params['+IntToStr(k)+'];');
         end;
         Target.AddLn('char* resultType;');
         Target.DecTab;
         Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
         Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
         CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
         CodeTarget.IncTab;
         CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
         CodeTarget.AddLn(NameString+',');
         if assigned(Type_.ReturnType) then begin
          if tpaClass in Type_^.ProcedureAttributes then begin
           Target.AddLn(IntToStr(mkClassFunction)+',');
          end else begin
           Target.AddLn(IntToStr(mkFunction)+',');
          end;
         end else begin
          if tpaConstructor in Type_^.ProcedureAttributes then begin
           Target.AddLn(IntToStr(mkConstructor)+',');
          end else if tpaDestructor in Type_^.ProcedureAttributes then begin
           Target.AddLn(IntToStr(mkDestructor)+',');
          end else begin
           if tpaClass in Type_^.ProcedureAttributes then begin
            Target.AddLn(IntToStr(mkClassProcedure)+',');
           end else begin
            Target.AddLn(IntToStr(mkProcedure)+',');
           end;
          end;
         end;
         Target.AddLn(IntToStr(k)+',');
         if (k>0) and assigned(Type_.Parameter) then begin
          j:=0;
          Symbol:=Type_.Parameter.First;
          while assigned(Symbol) do begin
           if not (tsaHiddenParameter in Symbol^.Attributes) then begin
            dec(k);
            CodeTarget.AddLn('{');
            CodeTarget.IncTab;
            Value:=0;
            if Symbol^.VariableType=tvtParameterVariable then begin
             Value:=Value or pfVar;
            end;
            if Symbol^.VariableType=tvtParameterConstant then begin
             Value:=Value or pfConst;
            end;
            if assigned(Symbol^.TypeDefinition) and (Symbol^.TypeDefinition^.TypeDefinition=ttdArray) then begin
             Value:=Value or pfArray;
            end;
            if assigned(Symbol^.TypeDefinition) and (Symbol^.TypeDefinition^.TypeDefinition=ttdPointer) then begin
             Value:=Value or pfAddress;
            end;
            if IsSymbolReference(Symbol) then begin
             Value:=Value or pfReference;
            end;
            if Symbol^.VariableType=tvtParameterResult then begin
             Value:=Value or pfOut;
            end;
            CodeTarget.AddLn(IntToStr(Value)+',');
            CodeTarget.AddLn(ParamNameList[j]+',');
            CodeTarget.AddLn(TypeNameList[j]);
            inc(j);
            CodeTarget.DecTab;
            if k>0 then begin
             CodeTarget.AddLn('},');
            end else begin
             CodeTarget.AddLn('}');
            end;
           end;
           Symbol:=Symbol^.Next;
          end;
         end;
         if assigned(Type_^.ReturnType) then begin
          TranslateShortStringConstant(GetOriginalTypeName(Type_^.ReturnType),CodeTarget);
          Target.AddLn('');
         end else begin
          Target.AddLn('NULL');
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
         CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
        finally
         ParamNameList.Free;
         TypeNameList.Free;
        end;
       end;
       TypeKindInterface:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('void* intfParent;');
        Target.AddLn('uint8_t intfFlags;');
        Target.AddLn('uint8_t intfGUID[16];');
        Target.AddLn('void* intfUnitName;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        if assigned(Type_^.ChildOf) and Type_^.ChildOf^.TypeDefinition^.RuntimeTypeInfo then begin
         CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.ChildOf^.TypeDefinition)+'_RUNTIME_TYPEINFO,');
        end else begin
         CodeTarget.AddLn('NULL,');
        end;
        j:=0;
        GUIDCastedBytes:=pointer(@Type_.GUID);
        for k:=0 to 15 do begin
         if GUIDCastedBytes^[k]<>0 then begin
          j:=j or ifHasGuid;
          break;
         end;
        end;
 {      ifDispInterface=1 shl 1;
        ifDispatch=1 shl 2;}
        CodeTarget.AddLn(IntToStr(j)+',');
        for k:=0 to 15 do begin
         j:=GUIDCastedBytes^[k];
         CodeTarget.AddLn(IntToStr(j)+',');
        end;
        CodeTarget.AddLn(UnitNameString);
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindInt64,TypeKindUInt64:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('int64_t minValue;');
        Target.AddLn('int64_t maxValue;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        CodeTarget.AddLn(IntToStr(Type_^.LowerLimit)+',');
        CodeTarget.AddLn(IntToStr(Type_^.UpperLimit));
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindArray,TypeKindDynArray:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('uint32_t elSize;');
        Target.AddLn('void* elType;');
        Target.AddLn('void* dynUnitName;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        if Type_^.DynamicArray then begin
         CodeTarget.AddLn(IntToStr(TypeKindDynArray)+',');
        end else begin
         CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        end;
        CodeTarget.AddLn(NameString+',');
        if assigned(Type_^.Definition) then begin
         CodeTarget.AddLn(IntToStr(SymbolManager.GetSize(Type_^.Definition))+',');
         if Type_^.Definition^.RuntimeTypeInfo then begin
          CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.Definition)+'_RUNTIME_TYPEINFO,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
        end else begin
         CodeTarget.AddLn('0,');
         CodeTarget.AddLn('NULL,');
        end;
        CodeTarget.AddLn(UnitNameString);
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       TypeKindPointer:begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.AddLn('void* pointerType;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString+',');
        if assigned(Type_^.PointerTo) and Type_^.PointerTo^.TypeDefinition^.RuntimeTypeInfo then begin
         CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.PointerTo^.TypeDefinition)+'_RUNTIME_TYPEINFO,');
        end else begin
         CodeTarget.AddLn('NULL,');
        end;
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
       else {TypeKindUnknown,TypeKindLString,TypeKindWString,TypeKindHString,TypeKindVariant:}begin
        Target.AddLn('typedef struct '+Name+'_RUNTIME_TYPEINFO_TYPE {');
        Target.IncTab;
        Target.AddLn('uint8_t kind;');
        Target.AddLn('void* name;');
        Target.DecTab;
        Target.AddLn('} '+Name+'_RUNTIME_TYPEINFO_TYPE;');
        Target.AddLn('typedef '+Name+'_RUNTIME_TYPEINFO_TYPE* '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE;');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO={');
        CodeTarget.IncTab;
        CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
        CodeTarget.AddLn(NameString);
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER = (void*)(&'+Name+'_RUNTIME_TYPEINFO);');
       end;
      end;

      Target.AddLn('extern '+Name+'_RUNTIME_TYPEINFO_TYPE '+Name+'_RUNTIME_TYPEINFO;');
      Target.AddLn('extern '+Name+'_RUNTIME_TYPEINFO_POINTER_TYPE '+Name+'_RUNTIME_TYPEINFO_POINTER;');
     end;

     if DoGeneralTypeInfo then begin

      Target.AddLn('extern pasTypeInfo '+Name+'_INITIALIZATION_TYPEINFO;');
      Target.AddLn('extern pasTypeInfoPointer '+Name+'_INITIALIZATION_TYPEINFO_POINTER;');
      case Type_.TypeDefinition of
       ttdRecord,ttdObject,ttdClass:begin
        k:=0;
        TypeChain:=TPointerList.Create;
        try
         CurrentType:=Type_;
         while assigned(CurrentType) do begin
          TypeChain.Add(CurrentType);
          if assigned(CurrentType^.ChildOf) then begin
           CurrentType:=CurrentType^.ChildOf^.TypeDefinition;
          end else begin
           break;
          end;
         end;            
         for j:=TypeChain.Count-1 downto 0 do begin
          CurrentType:=TypeChain.Items[j];
          if assigned(CurrentType^.RecordTable) then begin
           Symbol:=CurrentType^.RecordTable.First;
           while assigned(Symbol) do begin
            case Symbol^.SymbolType of
             tstVariable:begin
              if not (tsaInternalField in Symbol^.Attributes) then begin
               inc(k);
              end;
             end;
            end;
            Symbol:=Symbol^.Next;
           end;
          end;
         end;
         Target.AddLn('typedef struct '+Name+'_FIELDTABLE_TYPE {');
         Target.IncTab;
         Target.AddLn('pasFieldTableStripped fieldTable;');
         Target.AddLn('pasFieldInfo fields['+IntToStr(k)+'];');
         Target.DecTab;
         Target.AddLn('} '+Name+'_FIELDTABLE_TYPE;');
         CodeTarget.AddLn(Name+'_FIELDTABLE_TYPE '+Name+'_FIELDTABLE={');
         CodeTarget.IncTab;
         CodeTarget.AddLn('{');
         CodeTarget.IncTab;
         CodeTarget.AddLn('0,');
         CodeTarget.AddLn(IntToStr(SymbolManager.GetSize(Type_))+',');
         CodeTarget.AddLn(IntToStr(k));
         CodeTarget.DecTab;
         CodeTarget.AddLn('},');
         CodeTarget.AddLn('{');
         CodeTarget.IncTab;
         for j:=TypeChain.Count-1 downto 0 do begin
          CurrentType:=TypeChain.Items[j];
          if assigned(CurrentType^.RecordTable) then begin
           Symbol:=CurrentType^.RecordTable.First;
           while assigned(Symbol) do begin
            case Symbol^.SymbolType of
             tstVariable:begin
              if not (tsaInternalField in Symbol^.Attributes) then begin
               CodeTarget.AddLn('{');
               CodeTarget.IncTab;
               if assigned(Symbol^.TypeDefinition) and Symbol^.TypeDefinition^.InitializationTypeInfo then begin
                CodeTarget.AddLn('(void*)&'+GetTypeName(Symbol^.TypeDefinition)+'_INITIALIZATION_TYPEINFO_POINTER,');
               end else begin
                CodeTarget.AddLn('(void*)&pasTypeInfoUnknownPointer,');
               end;
               CodeTarget.AddLn(IntToStr(Symbol^.Offset));
               CodeTarget.DecTab;
               if k>1 then begin
                CodeTarget.AddLn('},');
               end else begin
                CodeTarget.AddLn('}');
               end;
               dec(k);
              end;
             end;
            end;
            Symbol:=Symbol^.Next;
           end;
          end;
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('}');
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
        finally
         TypeChain.Free;
        end;
       end;
       ttdArray:begin
        Target.AddLn('typedef struct {');
        Target.IncTab;
        Target.AddLn('pasFieldTableStripped fieldTable;');
        Target.AddLn('pasFieldInfo fields[1];');
        Target.DecTab;
        Target.AddLn('} '+Name+'_FIELDTABLE_TYPE;');
        CodeTarget.AddLn(Name+'_FIELDTABLE_TYPE '+Name+'_FIELDTABLE={');
        CodeTarget.IncTab;
        CodeTarget.AddLn('{');
        CodeTarget.IncTab;
        CodeTarget.AddLn('0,');
        CodeTarget.AddLn(IntToStr(SymbolManager.GetSize(Type_))+',');
        CodeTarget.AddLn(IntToStr((Type_^.UpperLimit-Type_^.LowerLimit)-1));
        CodeTarget.DecTab;
        CodeTarget.AddLn('},');
        CodeTarget.AddLn('{');
        CodeTarget.IncTab;
        CodeTarget.AddLn('{');
        CodeTarget.IncTab;
        if assigned(Type_^.Definition) and Type_^.Definition^.InitializationTypeInfo then begin
         CodeTarget.AddLn('(void*)&'+GetTypeName(Type_^.Definition)+'_INITIALIZATION_TYPEINFO_POINTER,');
        end else begin
         CodeTarget.AddLn('(void*)&pasTypeInfoUnknownPointer,');
        end;
        CodeTarget.AddLn(IntToStr(0));
        CodeTarget.DecTab;
        CodeTarget.AddLn('}');
        CodeTarget.DecTab;
        CodeTarget.AddLn('}');
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
       end;
      end;
      CodeTarget.AddLn('pasTypeInfo '+Name+'_INITIALIZATION_TYPEINFO={');
      CodeTarget.IncTab;
      if (Type_^.TypeKind=TypeKindArray) and Type_^.DynamicArray then begin
       CodeTarget.AddLn(IntToStr(TypeKindDynArray)+',');
      end else begin
       CodeTarget.AddLn(IntToStr(Type_^.TypeKind)+',');
      end;
      if Type_^.TypeDefinition=ttdLongString then begin
       CodeTarget.AddLn(IntToStr(Type_^.LongStringCodePage)+',');
      end else begin
       CodeTarget.AddLn('0,');
      end;
      if (Type_^.TypeDefinition=ttdOBJECT) and Type_^.HasVirtualTable then begin
       CodeTarget.AddLn('(void*)&'+GetTypeName(Type_)+'_VMT,');
      end else begin
       CodeTarget.AddLn('NULL,');
      end;
      CodeTarget.AddLn(NameString+',');
      case Type_.TypeDefinition of
       ttdRecord,ttdObject,ttdClass,ttdArray:begin
        CodeTarget.AddLn('(void*)&'+Name+'_FIELDTABLE');
       end;
       else begin
        CodeTarget.AddLn('NULL');
       end;
      end;
      CodeTarget.DecTab;
      CodeTarget.AddLn('};');
      CodeTarget.AddLn('pasTypeInfoPointer '+Name+'_INITIALIZATION_TYPEINFO_POINTER = &'+Name+'_INITIALIZATION_TYPEINFO;');

     end;
    end;
   end;
   Target.AddLn('');
  finally
   DumpedRTTIList.Free;
  end;

  FVariantPrefix:=false;

  Target.AddLn('// Real definitions');
  for i:=0 to length(TypeItems)-1 do begin
   Type_:=TypeItems[i];
   Name:=GetTypeName(Type_);
   case Type_.TypeDefinition of
    ttdEmpty:begin
     if Type_<>SymbolManager.TypeEmpty then begin
      Target.AddLn('typedef void* '+Name+';');
     end;
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
      Type_^.Dumped:=true;
      Target.AddLn('typedef pasDynArray '+Name+';');
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
     TypeChain:=TPointerList.Create;
     try
      CurrentType:=Type_;
      while assigned(CurrentType) do begin
       TypeChain.Add(CurrentType);
       if assigned(CurrentType^.ChildOf) then begin
        CurrentType:=CurrentType^.ChildOf^.TypeDefinition;
       end else begin
        break;
       end;
      end;
      for j:=TypeChain.Count-1 downto 0 do begin
       CurrentType:=TypeChain.Items[j];
       SymbolList:=CurrentType^.RecordTable;
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
            SetLength(VariantLevelVariants,RoundUpToPowerOfTwo(CurrentVariantLevelIndex));
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
           Target.AddLn('} '+GetTypeName(Type_)+'_'+'L'+IntToStr(CurrentVariantLevelIndex)+'V'+IntToStr(VariantLevelVariants[CurrentVariantLevelIndex-1])+';');
          end;
          Symbols.tstVariable:begin
           if tsaObjectVMT in Symbol^.Attributes then begin
            Target.AddLn('pasObjectVirtualMethodTable* '+GetSymbolName(Symbol)+';');
           end else if tsaClassVMT in Symbol^.Attributes then begin
            Target.AddLn('pasClassVirtualMethodTable* '+GetSymbolName(Symbol)+';');
           end else if tsaClassInterfaceVTable in Symbol^.Attributes then begin
            Target.AddLn('void* '+GetSymbolName(Symbol)+';');
//          Target.AddLn(GetTypeName(Symbol^.TypeDefinition)+'_VTABLE_NAMED_TYPE* '+GetSymbolName(Symbol)+';');
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
            if assigned(Symbol^.TypeDefinition) and ((Symbol^.TypeDefinition^.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.PointerTo)) and (Symbol^.TypeDefinition^.PointerTo^.TypeDefinition=Symbol^.TypeDefinition) then begin
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
      end;
     finally
      TypeChain.Free;
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
     Target.AddLn('typedef pasShortstring'+IntToStr(Type_^.Length)+' '+Name+';');
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
      end else if assigned(Symbol^.TypeDefinition) and Assigned(Symbol^.TypeDefinition^.Definition)and((Symbol^.TypeDefinition^.TypeDefinition=ttdPointer) and assigned(Symbol^.TypeDefinition^.Definition^.PointerTo)) and (Symbol^.TypeDefinition^.Definition^.PointerTo^.TypeDefinition=Symbol^.TypeDefinition) then begin
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
     Target.Add('typedef ',spacesRIGHT);
     ProcessTypeOrName(Type_^.ReturnType, Target, Type_);
     Target.Add('(*');
     Target.Add(Name);
     Target.Add(')(');
     if assigned(Type_.Parameter) then
     begin
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
     end;
     Target.Add(')',spacesRIGHT);
     Target.AddLn(';');
    end;
    ttdClassRef:begin
     Target.AddLn('typedef pasClassVirtualMethodTable* '+Name+';');
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

  Target.AddLn('// VMT/DMT definitions');
  for i:=0 to length(TypeItems)-1 do begin
   Type_:=TypeItems[i];
   Name:=GetTypeName(Type_);
   case Type_.TypeDefinition of
    ttdInterface:begin
     MethodTableList:=TStringList.Create;
     TypeChain:=TPointerList.Create;
     try
      CurrentType:=Type_;
      while assigned(CurrentType) do begin
       TypeChain.Add(CurrentType);
       if assigned(CurrentType^.ChildOf) then begin
        CurrentType:=CurrentType^.ChildOf^.TypeDefinition;
       end else begin
        break;
       end;
      end;
      for j:=TypeChain.Count-1 downto 0 do begin
       CurrentType:=TypeChain.Items[j];
       if assigned(CurrentType^.RecordTable) then begin
        Symbol:=CurrentType^.RecordTable.First;
        while assigned(Symbol) do begin
         case Symbol^.SymbolType of
          tstFunction,tstProcedure:begin
           MethodTableList.AddObject(Symbol^.Name,Symbol);
          end;
         end;
         Symbol:=Symbol^.Next;
        end;
       end;
      end;
      Target.AddLn('typedef struct '+Name+'_VTABLE_NAMED_TYPE {');
      Target.IncTab;
      for j:=0 to MethodTableList.Count-1 do begin
       Symbol:=pointer(MethodTableList.Objects[j]);
       ProcessFunctionType(Symbol,Symbol^.ReturnType,Symbol^.Parameter,Symbol^.Name,Target,true);
       Target.AddLn(';');
      end;
      Target.DecTab;
      Target.AddLn('} '+Name+'_VTABLE_NAMED_TYPE;');
      Target.AddLn('typedef struct '+Name+'_VTABLE_INDEXED_TYPE {');
      Target.IncTab;
      Target.AddLn('void* methods['+IntToStr(MethodTableList.Count)+'];');
      Target.DecTab;
      Target.AddLn('} '+Name+'_VTABLE_INDEXED_TYPE;');
      Target.AddLn('typedef union '+Name+'_VTABLE_TYPE {');
      Target.IncTab;
      Target.AddLn(Name+'_VTABLE_NAMED_TYPE namedMethods;');
      Target.AddLn(Name+'_VTABLE_INDEXED_TYPE indexedMethods;');
      Target.DecTab;
      Target.AddLn('} '+Name+'_VTABLE_TYPE;');
     finally
      TypeChain.Free;
      MethodTableList.Free;
     end;
    end;
    ttdObject,ttdClass:begin
     MethodTableList:=TStringList.Create;
     FieldTableList:=TPointerList.Create;
     ClassTableList:=TPointerList.Create;
     try
      SymbolList:=Type_^.RecordTable;
      if assigned(SymbolList) then begin
       if Type_^.HasVirtualTable then begin
        CodeTarget.AddLn('');
        HasDynamicMethods:=false;

        MethodList:=TStringList.Create;
        try
         Symbol:=Type_^.RecordTable.First;
         while assigned(Symbol) do begin
          case Symbol^.SymbolType of
           Symbols.tstProcedure,Symbols.tstFunction:begin
            if tpaDynamic in Symbol^.ProcedureAttributes then begin
             HasDynamicMethods:=true;
             if (tpaAbstract in Symbol^.ProcedureAttributes) and not (tsaMethodDefined in Symbol^.Attributes) then begin
              MethodList.Add('{'+IntToStr(Symbol^.VirtualIndex)+',NULL},');
             end else begin
              MethodList.Add('{'+IntToStr(Symbol^.VirtualIndex)+',(void*)&'+GetSymbolName(Symbol)+'},');
             end;
            end else if tpaVirtual in Symbol^.ProcedureAttributes then begin
            end else if (Type_.TypeDefinition=ttdCLASS) and (tsaOOPPublished in Symbol^.Attributes) then begin
             MethodTableList.Add('{(void*)&'+GetSymbolName(Symbol)+',' + GetShortStringConstant(Symbol^.OriginalCaseName) + '},');
            end;
           end;
           Symbols.tstProperty:begin
            if (Type_.TypeDefinition=ttdCLASS) and (tsaOOPPublished in Symbol^.Attributes) then begin
             if Symbol^.TypeDefinition^.TypeDefinition=ttdCLASS then begin
              j:=ClassTableList.IndexOf(Symbol^.TypeDefinition);
              if j<0 then begin
               ClassTableList.Add(Symbol^.TypeDefinition);
              end;
              FieldTableList.Add(Symbol);
             end else begin
              Error.InternalError(201304121425000);
             end;
            end;
           end;
          end;
          Symbol:=Symbol^.Next;
         end;
         if HasDynamicMethods then begin
          if Type_.TypeDefinition=ttdObject then begin
           CodeTarget.AddLn('pasObjectDynamicMethodTableItem '+Name+'_DMT['+IntToStr(MethodList.Count+1)+']={');
          end else begin
           CodeTarget.AddLn('pasClassDynamicMethodTableItem '+Name+'_DMT['+IntToStr(MethodList.Count+1)+']={');
          end;
          CodeTarget.IncTab;
          for j:=0 to MethodList.Count-1 do begin
           CodeTarget.AddLn(MethodList[j]);
          end;
          CodeTarget.AddLn('{-1,NULL}');
          CodeTarget.DecTab;
          CodeTarget.AddLn('};');
          CodeTarget.AddLn('');
         end;
        finally
         MethodList.Free;
        end;

{       if (Type_.TypeDefinition=ttdCLASS) and assigned(Type_.Symbol) then begin
         TranslateShortStringConstant(Name+'_CLASSNAME',Type_.Symbol.OriginalCaseName,CodeTarget);
         CodeTarget.AddLn('');
        end;{}

        if (Type_^.TypeDefinition=ttdCLASS) and (length(Type_^.ImplementedInterfaces)>0) then begin
         Target.AddLn('typedef struct {');
         Target.IncTab;
         Target.AddLn('uint32_t entryCount;');
         Target.AddLn('pasInterfaceEntry entries['+IntToStr(length(Type_^.ImplementedInterfaces))+'];');
         Target.DecTab;
         Target.AddLn('} '+Name+'_INTERFACE_TABLE_TYPE;');
         Target.AddLn('extern '+Name+'_INTERFACE_TABLE_TYPE '+Name+'_INTERFACE_TABLE;');
         CodeTarget.AddLn(Name+'_INTERFACE_TABLE_TYPE '+Name+'_INTERFACE_TABLE = {');
         CodeTarget.IncTab;
         CodeTarget.AddLn(IntToStr(length(Type_^.ImplementedInterfaces))+',');
         for j:=0 to length(Type_^.ImplementedInterfaces)-1 do begin
          ImplementedInterface:=Type_^.ImplementedInterfaces[j];
          CodeTarget.AddLn('{');
          CodeTarget.IncTab;
          if assigned(ImplementedInterface.InterfaceTypeSymbol) then begin
           GUIDCastedBytes:=pointer(@ImplementedInterface.InterfaceTypeSymbol^.TypeDefinition.GUID);
           CodeTarget.AddLn(IntToStr(GUIDCastedBytes^[0])+','+IntToStr(GUIDCastedBytes^[1])+','+IntToStr(GUIDCastedBytes^[2])+','+IntToStr(GUIDCastedBytes^[3])+','+
                            IntToStr(GUIDCastedBytes^[4])+','+IntToStr(GUIDCastedBytes^[5])+','+IntToStr(GUIDCastedBytes^[6])+','+IntToStr(GUIDCastedBytes^[7])+','+
                            IntToStr(GUIDCastedBytes^[8])+','+IntToStr(GUIDCastedBytes^[9])+','+IntToStr(GUIDCastedBytes^[10])+','+IntToStr(GUIDCastedBytes^[11])+','+
                            IntToStr(GUIDCastedBytes^[12])+','+IntToStr(GUIDCastedBytes^[13])+','+IntToStr(GUIDCastedBytes^[14])+','+IntToStr(GUIDCastedBytes^[15])+',');
          end else begin
           CodeTarget.AddLn('0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,');
          end;
          CodeTarget.AddLn('NULL,');
          CodeTarget.AddLn(IntToStr(ImplementedInterface.IType)+',');
          case ImplementedInterface.IType of
           iiitSTANDARD:begin
            CodeTarget.AddLn(IntToStr(ImplementedInterface.InternalClassVTableFieldOffset)+',');
            CodeTarget.AddLn('NULL');
           end;
           iiitFIELDVALUE:begin
            CodeTarget.AddLn(IntToStr(ImplementedInterface.InternalClassVTableFieldOffset)+',');
            CodeTarget.AddLn('NULL');
           end;
           iiitVIRTUALMETHOD:begin
            CodeTarget.AddLn(IntToStr(ImplementedInterface.InternalClassVTableField^.VirtualIndex-VirtualIndexCountOffset)+'*sizeof(void*),');
            CodeTarget.AddLn('NULL');
           end;
           iiitSTATICMETHOD:begin
            CodeTarget.AddLn('0,');
            CodeTarget.AddLn('&'+GetSymbolName(ImplementedInterface.InternalClassVTableField));
           end;
           else begin
            CodeTarget.AddLn('0,');
            CodeTarget.AddLn('NULL');
           end;
          end;
          CodeTarget.DecTab;
          if (j+1)<length(Type_^.ImplementedInterfaces) then begin
           CodeTarget.AddLn('},');
          end else begin
           CodeTarget.AddLn('}');
          end;
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
        end;

        if (Type_^.TypeDefinition=ttdCLASS) and (MethodTableList.Count>0) then begin
         Target.AddLn('typedef struct {');
         Target.IncTab;
         Target.AddLn('size_t count;');
         Target.AddLn('pasClassMethodTableItem methods['+IntToStr(MethodTableList.Count)+'];');
         Target.DecTab;
         Target.AddLn('} '+Name+'_METHOD_TABLE_TYPE;');
         Target.AddLn('extern '+Name+'_METHOD_TABLE_TYPE '+Name+'_METHOD_TABLE;');
         CodeTarget.AddLn(Name+'_METHOD_TABLE_TYPE '+Name+'_METHOD_TABLE = {');
         CodeTarget.IncTab;
         CodeTarget.AddLn(IntToStr(MethodTableList.Count)+',');
         for j:=0 to MethodTableList.Count-1 do begin
          CodeTarget.AddLn(MethodTableList[j]);
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
        end;

        if (Type_^.TypeDefinition=ttdCLASS) and ((FieldTableList.Count>0) or (ClassTableList.Count>0)) then begin
         Target.AddLn('typedef struct {');
         Target.IncTab;
         Target.AddLn('size_t count;');
         Target.AddLn('void* classTable;');
         Target.AddLn('pasClassFieldTableItem fields['+IntToStr(FieldTableList.Count)+'];');
         Target.DecTab;
         Target.AddLn('} '+Name+'_FIELLD_TABLE_TYPE;');
         Target.AddLn('extern '+Name+'_METHOD_TABLE_TYPE '+Name+'_METHOD_TABLE;');
         CodeTarget.AddLn(Name+'_METHOD_TABLE_TYPE '+Name+'_METHOD_TABLE = {');
         CodeTarget.IncTab;
         CodeTarget.AddLn(IntToStr(FieldTableList.Count)+',');
         CodeTarget.AddLn('NULL,');
         for j:=0 to FieldTableList.Count-1 do begin
          Symbol:=FieldTableList[j];
          CodeTarget.AddLn('{');
          CodeTarget.IncTab;
          CodeTarget.AddLn(IntToStr(Symbol^.Offset)+',');
          CodeTarget.AddLn(IntToStr(ClassTableList.IndexOf(Symbol^.TypeDefinition))+',');
          CodeTarget.AddLn(GetShortStringConstant(Symbol^.OriginalCaseName));
          CodeTarget.DecTab;
          if j<(FieldTableList.Count-1) then begin
           CodeTarget.AddLn('},');
          end else begin
           CodeTarget.AddLn('}');
          end;
         end;
         CodeTarget.DecTab;
         CodeTarget.AddLn('};');
        end;

        Target.AddLn('typedef struct {');
        Target.IncTab;
        if Type_.TypeDefinition=ttdObject then begin
         Target.AddLn('pasObjectVirtualMethodTableStripped VMT;');
        end else begin
         Target.AddLn('pasClassVirtualMethodTableStripped VMT;');
        end;
        Target.AddLn('void* virtualMethods['+IntToStr(Type_^.VirtualIndexCount)+'];');
        Target.DecTab;
        Target.AddLn('} '+Name+'_VMT_TYPE;');
        Target.AddLn('extern '+Name+'_VMT_TYPE '+Name+'_VMT;');
        Target.AddLn('extern '+Name+'_VMT_TYPE* '+Name+'_VMT_POINTER;');
        CodeTarget.AddLn(Name+'_VMT_TYPE '+Name+'_VMT={');
        CodeTarget.IncTab;
        CodeTarget.AddLn('{');
        CodeTarget.IncTab;
        if Type_.TypeDefinition=ttdObject then begin
         CodeTarget.AddLn(IntToStr(Type_^.RecordSize)+',');
         if HasDynamicMethods then begin
          CodeTarget.AddLn('(void*)&'+Name+'_DMT,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         if assigned(Type_^.ChildOf) then begin
          CodeTarget.AddLn('(void*)&'+GetSymbolName(Type_^.ChildOf)+'_VMT');
         end else begin
          CodeTarget.AddLn('NULL');
         end;
        end else begin
         // void* vmtSelfPtr;
         CodeTarget.AddLn('(void*)&'+Name+'_VMT,');
         // void* vmtIntfTable;
         if (Type_^.TypeDefinition=ttdCLASS) and (length(Type_^.ImplementedInterfaces)>0) then begin
          CodeTarget.AddLn('(void*)&'+Name+'_INTERFACE_TABLE,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         // void* vmtAutoTable;
         CodeTarget.AddLn('NULL,');
         // void* vmtInitTable;
         CodeTarget.AddLn('(void*)&'+Name+'_INITIALIZATION_TYPEINFO,');
         // void* vmtTypeInfo;
         CodeTarget.AddLn('(void*)&'+Name+'_RUNTIME_TYPEINFO,');
         // void* vmtFieldTable;
         if (FieldTableList.Count>0) or (ClassTableList.Count>0) then begin
          CodeTarget.AddLn('(void*)&'+Name+'_FIELD_TABLE,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         // void* vmtMethodTable;
         if MethodTableList.Count>0 then begin
          CodeTarget.AddLn('(void*)&'+Name+'_METHOD_TABLE,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         // void* vmtDynamicTable;
         if HasDynamicMethods then begin
          CodeTarget.AddLn('(void*)&'+Name+'_DMT,');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         // void* vmtClassName;
//        CodeTarget.AddLn('(void*)&'+Name+'_CLASSNAME,');
         if assigned(Type_^.Symbol) then begin
          CodeTarget.AddLn(GetShortStringConstant(Type_^.Symbol^.OriginalCaseName)+',');
         end else begin
          CodeTarget.AddLn('NULL,');
         end;
         // size_t vmtInstanceSize;
         CodeTarget.AddLn(IntToStr(Type_^.RecordSize)+',');
         // void* vmtParent;
         if assigned(Type_^.ChildOf) then begin
          CodeTarget.AddLn('pasClassVMTMask((void*)&'+GetSymbolName(Type_^.ChildOf)+'_VMT)');
         end else begin
          CodeTarget.AddLn('NULL');
         end;
        end;
        CodeTarget.DecTab;
        CodeTarget.AddLn('},');
        CodeTarget.AddLn('{');
        CodeTarget.IncTab;
        HasLast:=false;
        MethodList:=TStringList.Create;
        try
         TypeChain:=TPointerList.Create;
         try
          for j:=0 to Type_^.VirtualIndexCount-1 do begin
           MethodList.Add('NULL');
          end;
          CurrentType:=Type_;
          while assigned(CurrentType) do begin
           TypeChain.Add(CurrentType);
           if assigned(CurrentType^.ChildOf) then begin
            CurrentType:=CurrentType^.ChildOf^.TypeDefinition;
           end else begin
            break;
           end;
          end;
          for j:=TypeChain.Count-1 downto 0 do begin
           CurrentType:=TypeChain.Items[j];
           if assigned(CurrentType^.RecordTable) then begin
            Symbol:=CurrentType^.RecordTable.First;
            while assigned(Symbol) do begin
             case Symbol^.SymbolType of
              Symbols.tstProcedure,Symbols.tstFunction:begin
               if tpaVirtual in Symbol^.ProcedureAttributes then begin
                if (tpaAbstract in Symbol^.ProcedureAttributes) and not (tsaMethodDefined in Symbol^.Attributes) then begin
                 MethodList[Symbol^.VirtualIndex]:='NULL';
                end else begin
                 MethodList[Symbol^.VirtualIndex]:='(void*)&'+GetSymbolName(Symbol);
                end;
               end;
              end;
             end;
             Symbol:=Symbol^.Next;
            end;
           end;
          end;
          for j:=0 to MethodList.Count-1 do begin
           if HasLast then begin
            CodeTarget.AddLn(',');
           end;
           HasLast:=true;
           CodeTarget.Add(MethodList[j]);
          end;
         finally
          TypeChain.Free;
         end;
        finally
         MethodList.Free;
        end;
        if HasLast then begin
         CodeTarget.AddLn('');
        end;
        CodeTarget.DecTab;
        CodeTarget.AddLn('}');
        CodeTarget.DecTab;
        CodeTarget.AddLn('};');
        CodeTarget.AddLn(Name+'_VMT_TYPE* '+Name+'_VMT_POINTER = &'+Name+'_VMT;');
       end;

       Symbol:=SymbolList.First;
       while assigned(Symbol) do begin
        case Symbol^.SymbolType of
         Symbols.tstProcedure,Symbols.tstFunction:begin
          if tpaVirtual in Symbol^.ProcedureAttributes then begin
           Target.Add('typedef',spacesRIGHT);
           if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
            Target.Add('int',spacesRIGHT);
           end else begin
            ProcessTypeOrName(Symbol^.ReturnType, Target, nil);
           end;
           Target.Add('(*',spacesLEFT);
           Target.Add(Name+'_VMT_'+IntToStr(Symbol^.VirtualIndex));
           Target.Add(')(');
           Target.Add(GetSymbolName(Symbol^.OwnerObjectClass^.Symbol)+'*');
           if assigned(Symbol.Parameter) then
           begin
             OtherSymbol:=Symbol.Parameter.First;
             while Assigned(OtherSymbol) do
             begin
              Target.Add(',',spacesRIGHT);
              ProcessTypeOrName(OtherSymbol.TypeDefinition, Target, nil);
              if IsSymbolReference(OtherSymbol) then begin
               Target.Add('*',spacesRIGHT);
              end;
              OtherSymbol := OtherSymbol.Next;
             end;
           end;
           Target.Add(')',spacesRIGHT);
           Target.AddLn(';');
          end else if tpaDynamic in Symbol^.ProcedureAttributes then begin
           Target.Add('typedef',spacesRIGHT);
           if (tpaConstructor in Symbol^.ProcedureAttributes) and assigned(Symbol^.OwnerObjectClass) and (Symbol^.OwnerObjectClass^.TypeDefinition=ttdOBJECT) then begin
            Target.Add('int',spacesRIGHT);
           end else begin
            ProcessTypeOrName(Symbol^.ReturnType, Target, nil);
           end;
           Target.Add('*',spacesRIGHT);
           Target.Add(Name+'_DMT_'+IntToStr(Symbol^.VirtualIndex));
           Target.Add('(');
           Target.Add(GetSymbolName(Symbol^.OwnerObjectClass^.Symbol)+'*');
           if assigned(Symbol.Parameter) then
           begin
             OtherSymbol:=Symbol.Parameter.First;
             while Assigned(OtherSymbol) do
             begin
              Target.Add(',',spacesRIGHT);
              ProcessTypeOrName(OtherSymbol.TypeDefinition, Target, nil);
              if IsSymbolReference(OtherSymbol) then begin
               Target.Add('*',spacesRIGHT);
              end;
              OtherSymbol := OtherSymbol.Next;
             end;
           end;
           Target.Add(')',spacesRIGHT);
           Target.AddLn(';');
          end;
         end;
        end;
        Symbol:=Symbol^.Next;
       end;

      end;

     finally
      ClassTableList.Free;
      FieldTableList.Free;
      MethodTableList.Free;
     end;
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
 if FIgnoreNextToken then
 begin
  FIgnoreNextToken := False;
  Exit;
 end;

 if ((Spaces and spacesLEFT)<>0) and ((length(FCurrentLine)>0) and not (FCurrentLine[length(FCurrentLine)] in [#0..#32,'(','[','{'])) then
   FCurrentLine := FCurrentLine + ' ';
(*if (length(s)>0) and (s[1] in [',',';',')',']','}']) then begin
  FCurrentLine := trimright(FCurrentLine);
 end;*)
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

procedure TCodeWriter.AddLn(s: ansistring; Spaces: longint = spacesNONE);
var i: Integer;
begin
 if (s = ';')and ((FCurrentLine = '') or ((length(FCurrentLine)>0) and (FCurrentLine[length(FCurrentLine)] in [#10,#13]))) then begin
  //s:='';
  exit;
 end;
  //Exit;

 if ((Spaces and spacesLEFT)<>0) and ((length(FCurrentLine)>0) and not (FCurrentLine[length(FCurrentLine)] in [#0..#32,'(','[','{'])) then
   FCurrentLine := FCurrentLine + ' ';

 if (length(s)>0) and (s[1] in [',',';',')',']','}']) then begin
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

initialization
 AnsistringType.TypeDefinition:=ttdLongString;
 AnsistringType.LongStringType:=tstUnsignedChar;
 AnsistringType.LongStringCodePage:=65535;
 AnsistringType.LongStringReferenceCounted:=True;
end.
