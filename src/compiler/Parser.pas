unit Parser;
{$i Compiler.inc}

interface

uses SysUtils,BeRoStream,Globals,Symbols,Scanner,Error,Tree,OptimizerHighLevel,
     BeRoGUIDTools,IntegerList,UnitManager,PointerList,Code,CodegenC,
     HugeString;

type TParser=class
      private
       Options:TOptions;
       Error:TError;
       SymbolManager:TSymbolManager;
       TreeManager:TTreeManager;
       GlobalSwitches:PGlobalSwitches;
       LocalSwitches:PLocalSwitches;
       OptimizerHighLevel:TOptimizerHighLevel;
       UnitManager:TUnitManager;
       CodeGenerator:TCode;
       WithStack:TPointerList;
       ACompiler:pointer;
       ParameterNumber:longint;
       UnitLevel:longint;
       procedure CheckDefaultParameters(FirstNeededDefaultSymbol:PSymbol;var Parameters:TTreeNode);
       procedure CheckParameters(Symbol:PSymbol;SymbolParameters:TSymbolList;var Parameters:TTreeNode);
       procedure SearchOverloadedSymbolAndCheckParameters(var Symbol:PSymbol;var Parameters:TTreeNode);
       procedure FinishCheckSymbols(ParentSymbol:PSymbol;List:TSymbolList);
       procedure FinishCheckMethods(ParentSymbol:PSymbol);
       procedure AddDefaultSymbols;
       procedure GetDefaultTypes;
       procedure ParseRecordField(var RecordType:PType;IsRecord:boolean=true;SymbolAttributes:TSymbolAttributes=[];UntilDirectives:TScannerTokens=[];CaseOfLevel:longint=0;CaseOfVariant:longint=0;VariantPrefix:ansistring='';IgnoreExistingOnParent:boolean=false);
       procedure ParsePropertyField(var RecordType:PType);
       procedure ParseParameterList(var Symbol:PSymbol;var SymbolParameter:TSymbolList;Bracket,AllowParameterConstant:boolean;var ParameterSuffix:ansistring);
       function ParseObjectDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
       function ParseClassDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
       function ParseInterfaceDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
       function ParseTypeDefinition(const Name:ansistring;AllowOpenArray:boolean=false):PType;
       function ParseTypedConstantDeclaration(var ConstantType:PType;ID:longword;IsPacked:boolean;Dummy:boolean=false):PConstant;
       procedure ParseDefaultParameterTypedConstantDeclaration(var ConstantType:PType;ParentSymbol:PSymbol);
       function ParseProcedureVariable(var Symbol:PSymbol):PType;
       procedure ParseProcedureType(var Symbol:PSymbol;RealDefinition:boolean);
       procedure CheckProcedureType(var Symbol:PSymbol);
       function ParseProcedure(ParseHeader:boolean;ProcedureAttributes:TProcedureAttributes):PSymbol;
       function ParsePortabilityDirectives:TPortabilityDirectives;
      public
       Scanner:TScanner;
       ModuleName,OriginalModuleName,FileName,ObjectClassName:ansistring;
       MustHaveParens,MakeSymbolsPublic,IsSystemUnit,IsInExceptionHandler:boolean;
       ModuleSymbol,CurrentMethod,CurrentProcedureFunction:PSymbol;
       CurrentObjectClass:PType;
       CurrentParseObjectClass:PType;
       DoBreak:boolean;
       ForEachVarCounter:longint;
       DefaultParameterCounter:longint;
       constructor Create(TheInputStream:TBeRoStream;TheFileName:ansistring;TheError:TError;TheSymbolManager:TSymbolManager;TheUnitManager:TUnitManager;TheCompiler:pointer;TheUnitLevel:longint;TheOptions:TOptions;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;
       procedure Parse;
       function ParseCallParameter:TTreeNode;
       function ParseNewDisposeParameter(IsNew:boolean):TTreeNode;
       function ParseTypeInfoParameter:TTreeNode;
       function HasPropertyNode(TreeNode:TTreeNode):boolean;
       function FinalizePropertyNodes(TreeNode,AssignTreeNode:TTreeNode;IsWrite:boolean):TTreeNode;
       function ParseFactor:TTreeNode;
       function ParseTerm:TTreeNode;
       function ParseSimpleExpression:TTreeNode;
       function ParseBooleanExpression:TTreeNode;
       function ParseExpression(AllowAssignment:boolean):TTreeNode;
       function ParseFORStatement:TTreeNode;
       function ParseWHILEStatement:TTreeNode;
       function ParseREPEATStatement:TTreeNode;
       function ParseTRYStatement:TTreeNode;
       function ParseIFStatement:TTreeNode;
       function ParseBREAKStatement:TTreeNode;
       function ParseCONTINUEStatement:TTreeNode;
       function ParseEXITStatement:TTreeNode;
       function ParseHALTStatement:TTreeNode;
       function ParseFAILStatement:TTreeNode;
       function ParseRAISEStatement:TTreeNode;
       function ParseGOTOStatement:TTreeNode;
       function ParseCASEStatement:TTreeNode;
       function ParseWITHStatement:TTreeNode;
       procedure ParseProgram(WithProgramToken:boolean);
       procedure ParsePackage;
       procedure ParseLibrary;
       procedure ParseUnit;
       procedure ParseUSESStatement(AfterImplementation,AllowIN,IsContains:boolean);
       procedure ParseHeadBlock(ParseHeader,IsGlobal:boolean);
       function ParseMainBlock:TTreeNode;
       function ParseStatement(AllowExpression:boolean):TTreeNode;
       function ParseBlockStatement(EndTokens:TScannerTokens):TTreeNode;
       function ParseBEGINBlockStatement:TTreeNode;
       function ParseCCodeStatement:TTreeNode;
       function ParseASMBlockStatement:TTreeNode;
       procedure ParseRESOURCESTRINGDeclartion;
       procedure ParseCONSTDeclartion;
       procedure ParseVARDeclartion;
       procedure ParseTYPEDeclartion;
       procedure ParseLABELDeclartion;
       procedure ParseEXPORTS;
     end;

implementation

uses BeRoUtils,TypeCheck;

constructor TParser.Create(TheInputStream:TBeRoStream;TheFileName:ansistring;TheError:TError;TheSymbolManager:TSymbolManager;TheUnitManager:TUnitManager;TheCompiler:pointer;TheUnitLevel:longint;TheOptions:TOptions;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
begin
 inherited Create;
 Options:=TheOptions;
 FileName:=TheFileName;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 GlobalSwitches:=TheGlobalSwitches;
 LocalSwitches:=TheLocalSwitches;
 TreeManager:=TTreeManager.Create(Error,SymbolManager,Options);
 CodeGenerator:=TCodeGenC.Create(Error,SymbolManager,TreeManager,Options,TheLocalSwitches);
 ACompiler:=TheCompiler;
 Scanner:=TScanner.Create(TheInputStream,TheFileName,Error,SymbolManager,Options,TheGlobalSwitches,TheLocalSwitches);
 OptimizerHighLevel:=TOptimizerHighLevel.Create(Error,SymbolManager,TreeManager,Options,TheLocalSwitches);
 Error.LocalSwitches:=TheLocalSwitches;
 UnitManager:=TheUnitManager;
 WithStack:=TPointerList.Create;
 MustHaveParens:=false;
 MakeSymbolsPublic:=true;
 IsSystemUnit:=false;
 IsInExceptionHandler:=false;
 CurrentMethod:=nil;
 CurrentProcedureFunction:=nil;
 CurrentObjectClass:=nil;
 CurrentParseObjectClass:=nil;
 ObjectClassName:='';
 ParameterNumber:=0;
 UnitLevel:=TheUnitLevel;
 ForEachVarCounter:=0;
 DefaultParameterCounter:=0;
end;

destructor TParser.Destroy;
begin
 ObjectClassName:='';
 WithStack.Free;
 Scanner.Free;
 OptimizerHighLevel.Free;
 CodeGenerator.Free;
 TreeManager.Free;
 inherited Destroy;
end;

procedure TParser.FinishCheckSymbols(ParentSymbol:PSymbol;List:TSymbolList);
var Symbol:PSymbol;
begin
 if not (assigned(ParentSymbol) and assigned(List)) then begin
  Error.InternalError(200605220413000);
  exit;
 end;

 Symbol:=List.First;
 while assigned(Symbol) do begin
  case Symbol^.SymbolType of
   Symbols.tstVariable:begin
    if not (tsaPublicUnitSymbol in Symbol^.Attributes) then begin
     if not Symbol^.DeclarationUsed then begin
      Error.AddHintCode(169,CorrectSymbolName(Symbol^.Name),CorrectSymbolName(ParentSymbol^.Name));
     end;
    end;
   end;
  end;
  Symbol:=Symbol^.Next;
 end;

end;

procedure TParser.FinishCheckMethods(ParentSymbol:PSymbol);
var Symbol:PSymbol;
    AType:PType;
    i:longint;
begin
 if Error.DoAbort then begin
  exit;
 end;
 for i:=0 to ModuleSymbol^.TypePointerList.Count-1 do begin
  AType:=ModuleSymbol^.TypePointerList[i];
  if assigned(AType) and (AType^.TypeDefinition in [ttdOBJECT,ttdCLASS]) and assigned(AType^.RecordTable) then begin
   Symbol:=AType^.RecordTable.First;
   while assigned(Symbol) do begin
    if (Symbol^.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction]) and (Symbol^.MethodOfType=AType) and not (tpaAbstract in Symbol^.ProcedureAttributes) then begin
     if not (tsaMethodDefined in Symbol^.Attributes) then begin
      if assigned(AType^.Symbol) then begin
       Error.AddErrorCode(66,CorrectSymbolName(AType^.Symbol^.Name)+'.'+CorrectSymbolName(Symbol^.Name));
      end else begin
       Error.AddErrorCode(66,CorrectSymbolName(Symbol^.Name));
      end;
     end;
    end;
    Symbol:=Symbol^.Next;
   end;
  end;
 end;
end;

procedure TParser.AddDefaultSymbols;
var Symbol,LastSymbol:PSymbol;
    AType,BooleanType,LongIntType,LongWordType,Int64Type,QWordType:PType;
    //Constant:PConstant;
begin
 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'POINTER';
 Symbol^.OriginalCaseName:='Pointer';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdPointer;
 AType^.PointerTo:=nil;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'BOOLEAN';
 Symbol^.OriginalCaseName:='Boolean';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindEnumeration;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 BooleanType:=AType;
 AType^.TypeDefinition:=ttdBoolean;
 AType^.SubRangeType:=Symbols.tstUnsigned8Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=1;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'BYTEBOOL';
 Symbol^.OriginalCaseName:='ByteBool';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindEnumeration;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdBoolean;
 AType^.SubRangeType:=Symbols.tstUnsigned8Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WORDBOOL';
 Symbol^.OriginalCaseName:='WordBool';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindEnumeration;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdBoolean;
 AType^.SubRangeType:=Symbols.tstUnsigned16Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'LONGBOOL';
 Symbol^.OriginalCaseName:='LongBool';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindEnumeration;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdBoolean;
 AType^.SubRangeType:=Symbols.tstUnsigned32Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffffffff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'BOOL64';
 Symbol^.OriginalCaseName:='Bool64';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindEnumeration;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdBoolean;
 AType^.SubRangeType:=Symbols.tstUnsigned64Bit;
 AType^.LowerLimit:=low(int64);
 AType^.UpperLimit:=high(int64);
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'FALSE';
 Symbol^.OriginalCaseName:='False';
 Symbol^.SymbolType:=Symbols.tstConstant;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.ConstantTypeRecord:=BooleanType;
 Symbol^.ConstantType:=tctORDINAL;
 Symbol^.IntValue:=0;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'TRUE';
 Symbol^.OriginalCaseName:='True';
 Symbol^.SymbolType:=Symbols.tstConstant;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.ConstantTypeRecord:=BooleanType;
 Symbol^.ConstantType:=tctORDINAL;
 Symbol^.IntValue:=1;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'INT64';
 Symbol^.OriginalCaseName:='Int64';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInt64;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstSigned64Bit;
 AType^.LowerLimit:=low(int64);
 AType^.UpperLimit:=high(int64);
 Int64Type:=AType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'QWORD';
 Symbol^.OriginalCaseName:='QWord';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 Symbol^.TypeDefinition:=AType;
 AType^.TypeKind:=TypeKindInt64;
 AType^.NeedTypeInfo:=false;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsigned64Bit;
 AType^.LowerLimit:=int64(0);
 AType^.UpperLimit:=int64($ffffffffffffffff);
 QWordType:=AType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'UINT64';
 Symbol^.OriginalCaseName:='UInt64';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.TypeDefinition:=AType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'LONGWORD';
 Symbol^.OriginalCaseName:='LongWord';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsigned32Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffffffff;
 LongWordType:=AType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'CARDINAL';
 Symbol^.OriginalCaseName:='Cardinal';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsigned32Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffffffff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'LONGINT';
 Symbol^.OriginalCaseName:='LongInt';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstSigned32Bit;
 AType^.LowerLimit:=-2147483646;
 AType^.UpperLimit:=2147483647;
 Symbol^.TypeDefinition:=AType;
 LongIntType:=AType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'INTEGER';
 Symbol^.OriginalCaseName:='Integer';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstSigned32Bit;
 AType^.LowerLimit:=-2147483646;
 AType^.UpperLimit:=2147483647;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WORD';
 Symbol^.OriginalCaseName:='Word';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsigned16Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SMALLINT';
 Symbol^.OriginalCaseName:='SmallInt';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstSigned16Bit;
 AType^.LowerLimit:=-32768;
 AType^.UpperLimit:=32767;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'BYTE';
 Symbol^.OriginalCaseName:='Byte';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsigned8Bit;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SHORTINT';
 Symbol^.OriginalCaseName:='ShortInt';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindInteger;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstSigned8Bit;
 AType^.LowerLimit:=-128;
 AType^.UpperLimit:=127;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 case Options.TargetArchitecture of
  taX64,taX64WIN64:begin
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'PTRINT';
   Symbol^.OriginalCaseName:='PtrInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=Int64Type;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'PTRUINT';
   Symbol^.OriginalCaseName:='PtrUInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=QWordType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'NATIVEINT';
   Symbol^.OriginalCaseName:='NativeInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=Int64Type;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'NATIVEUINT';
   Symbol^.OriginalCaseName:='NativEUInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=QWordType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
  end;
  else {taARM,taARMEABI,taX86,taX86WIN32:}begin
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'PTRINT';
   Symbol^.OriginalCaseName:='PtrInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=LongIntType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'PTRUINT';
   Symbol^.OriginalCaseName:='PtrUInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=LongWordType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'NATIVEINT';
   Symbol^.OriginalCaseName:='NativeInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=LongIntType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=tpsIdentifier+'NATIVEUINT';
   Symbol^.OriginalCaseName:='NativeUInt';
   Symbol^.SymbolType:=Symbols.tstType;
   Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
   Symbol^.TypeDefinition:=LongWordType;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
  end;
 end;

 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindAnsiChar;
 AType^.NeedTypeInfo:=false;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsignedChar;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ff;

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'CHAR';
 Symbol^.OriginalCaseName:='Char';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'ANSICHAR';
 Symbol^.OriginalCaseName:='AnsiChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 LastSymbol:=Symbol;

 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 AType^.TypeDefinition:=ttdPointer;
 AType^.PointerTo:=LastSymbol;

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'PCHAR';
 Symbol^.OriginalCaseName:='PChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'PANSICHAR';
 Symbol^.OriginalCaseName:='PAnsiChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WIDECHAR';
 Symbol^.OriginalCaseName:='WideChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindWideChar;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsignedWideChar;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=$ffff;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 LastSymbol:=Symbol;

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'PWIDECHAR';
 Symbol^.OriginalCaseName:='PWideChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdPointer;
 AType^.PointerTo:=LastSymbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'HUGECHAR';
 Symbol^.OriginalCaseName:='HugeChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindHugeChar;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdSubRange;
 AType^.SubRangeType:=Symbols.tstUnsignedHugeChar;
 AType^.LowerLimit:=0;
 AType^.UpperLimit:=int64($ffffffff);
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 LastSymbol:=Symbol;

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'PHUGECHAR';
 Symbol^.OriginalCaseName:='PHugeChar';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdPointer;
 AType^.PointerTo:=LastSymbol;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SINGLE';
 Symbol^.OriginalCaseName:='Single';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindFloat;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdFloat;
 AType^.FloatType:=Symbols.tstFloat32Bit;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'DOUBLE';
 Symbol^.OriginalCaseName:='Double';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindFloat;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdFloat;
 AType^.FloatType:=Symbols.tstFloat64Bit;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'REAL';
 Symbol^.OriginalCaseName:='Real';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindFloat;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdFloat;
 AType^.FloatType:=Symbols.tstFloat64Bit;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'EXTENDED';
 Symbol^.OriginalCaseName:='Extended';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindFloat;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdFloat;
 AType^.FloatType:=Symbols.tstFloat80Bit;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'TEXT';
 Symbol^.OriginalCaseName:='Text';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdFile;
 AType^.FileType:=tftText;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'ANSISTRING';
 Symbol^.OriginalCaseName:='AnsiString';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindLString;
 AType^.NeedTypeInfo:=true;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdLongString;
 AType^.LongStringType:=Symbols.tstUnsignedChar;
 AType^.LongStringCodePage:=$ffff;
 AType^.LongStringReferenceCounted:=true;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WIDESTRING';
 Symbol^.OriginalCaseName:='WideString';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindWString;
 AType^.NeedTypeInfo:=true;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdLongString;
 AType^.LongStringType:=Symbols.tstUnsignedWideChar;
 AType^.LongStringCodePage:=$ffff;
 AType^.LongStringReferenceCounted:=false;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'HUGESTRING';
 Symbol^.OriginalCaseName:='HugeString';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindHString;
 AType^.NeedTypeInfo:=true;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdLongString;
 AType^.LongStringType:=Symbols.tstUnsignedHugeChar;
 AType^.LongStringCodePage:=$ffff;
 AType^.LongStringReferenceCounted:=true;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'UNICODESTRING';
 Symbol^.OriginalCaseName:='UnicodeString';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUString;
 AType^.NeedTypeInfo:=true;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdLongString;
 AType^.LongStringType:=Symbols.tstUnsignedWideChar;
 AType^.LongStringCodePage:=$ffff;
 AType^.LongStringReferenceCounted:=true;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'$CEXPRESSION$';
 Symbol^.OriginalCaseName:='$CExpression$';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdCExpression;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'$EMPTY$';
 Symbol^.OriginalCaseName:='$Empty$';
 Symbol^.SymbolType:=Symbols.tstType;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 AType^.TypeKind:=TypeKindUnknown;
 AType^.NeedTypeInfo:=false;
 Symbol^.TypeDefinition:=AType;
 AType^.Symbol:=Symbol;
 AType^.TypeDefinition:=ttdEmpty;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WRITE';
 Symbol^.OriginalCaseName:='Write';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipWRITE;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'WRITELN';
 Symbol^.OriginalCaseName:='WriteLn';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipWRITELN;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'READ';
 Symbol^.OriginalCaseName:='Read';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipREAD;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'READLN';
 Symbol^.OriginalCaseName:='ReadLn';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipREADLN;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SIZEOF';
 Symbol^.OriginalCaseName:='SizeOf';
 Symbol^.SymbolType:=Symbols.tstFunction;
 Symbol^.InternalProcedure:=tipSIZEOF;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 Symbol^.ReturnType:=LongIntType;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'DEC';
 Symbol^.OriginalCaseName:='Dec';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipDEC;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'INC';
 Symbol^.OriginalCaseName:='Inc';
 Symbol^.SymbolType:=Symbols.tstProcedure;
 Symbol^.InternalProcedure:=tipINC;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SUCC';
 Symbol^.OriginalCaseName:='Succ';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipSUCC;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'PRED';
 Symbol^.OriginalCaseName:='Pred';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipPRED;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'ORD';
 Symbol^.OriginalCaseName:='Ord';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipORD;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'CHR';
 Symbol^.OriginalCaseName:='Chr';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipCHR;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'NEW';
 Symbol^.OriginalCaseName:='New';
 Symbol^.SymbolType:=Symbols.tstPROCEDURE;
 Symbol^.InternalProcedure:=tipNEW;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'DISPOSE';
 Symbol^.OriginalCaseName:='Dispose';
 Symbol^.SymbolType:=Symbols.tstPROCEDURE;
 Symbol^.InternalProcedure:=tipDISPOSE;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SETLENGTH';
 Symbol^.OriginalCaseName:='SetLength';
 Symbol^.SymbolType:=Symbols.tstPROCEDURE;
 Symbol^.InternalProcedure:=tipSETLENGTH;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'LENGTH';
 Symbol^.OriginalCaseName:='Length';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipLENGTH;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'ASSIGNED';
 Symbol^.OriginalCaseName:='Assigned';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipASSIGNED;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'TRUNC';
 Symbol^.OriginalCaseName:='Trunc';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipTRUNC;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'ROUND';
 Symbol^.OriginalCaseName:='Round';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipROUND;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SQR';
 Symbol^.OriginalCaseName:='Sqr';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipSQR;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'SQRT';
 Symbol^.OriginalCaseName:='Sqrt';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipSQR;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'TYPEOF';
 Symbol^.OriginalCaseName:='TypeOf';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipTYPEOF;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'TYPEINFO';
 Symbol^.OriginalCaseName:='TypeInfo';
 Symbol^.SymbolType:=Symbols.tstFUNCTION;
 Symbol^.InternalProcedure:=tipTYPEINFO;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'INITIALIZE';
 Symbol^.OriginalCaseName:='Initialize';
 Symbol^.SymbolType:=Symbols.tstPROCEDURE;
 Symbol^.InternalProcedure:=tipINITIALIZE;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.Name:=tpsIdentifier+'FINIALIZE';
 Symbol^.OriginalCaseName:='Finalize';
 Symbol^.SymbolType:=Symbols.tstPROCEDURE;
 Symbol^.InternalProcedure:=tipFINALIZE;
 Symbol^.Attributes:=[tsaPublic,tsaPublicUnitSymbol];
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);

end;

procedure TParser.GetDefaultTypes;
begin
 SymbolManager.GetDefaultTypes;
end;

procedure TParser.Parse;
var OldList:TSymbolList;
begin
 Scanner.Reset;
 Scanner.PreprocessorInstance.ModuleSymbol:=nil;
 Error.LocalSwitches:=LocalSwitches;
 OldList:=SymbolManager.CurrentList;
 Scanner.ReadNext;
 DoBreak:=false;
 Scanner.CheckForDirectives([tstLIBRARY,tstPACKAGE]);
 case Scanner.CurrentToken of
  tstPROGRAM:begin
   ParseProgram(true);
  end;
  tstLIBRARY:begin
   ParseLibrary;
  end;
  tstPACKAGE:begin
   ParsePackage;
  end;
  tstUNIT:begin
   ParseUnit;
  end;
  else begin
   ParseProgram(false);
  end;
 end;
 SymbolManager.CurrentList:=OldList;
end;

function TParser.ParseCallParameter:TTreeNode;
var NewTreeNode,LastTreeNode:TTreeNode;
    ParamaterList:TPointerList;
    i:longint;
begin
 result:=nil;
 if Scanner.CurrentToken<>tstRightParen then begin
  ParamaterList:=TPointerList.Create;
  try
   while not Scanner.IsEOFOrAbortError do begin
    NewTreeNode:=TreeManager.GenerateParameterNode(ParseExpression(false),nil);
    NewTreeNode.Colon:=false;
    ParamaterList.Add(NewTreeNode);
    if Scanner.CurrentToken=tstColon then begin
     Scanner.Match(tstColon);
     NewTreeNode:=TreeManager.GenerateParameterNode(ParseExpression(false),nil);
     NewTreeNode.Colon:=true;
     ParamaterList.Add(NewTreeNode);
     if Scanner.CurrentToken=tstColon then begin
      Scanner.Match(tstColon);
      NewTreeNode:=TreeManager.GenerateParameterNode(ParseExpression(false),nil);
      NewTreeNode.Colon:=true;
      ParamaterList.Add(NewTreeNode);
     end;
    end;
    if Scanner.CurrentToken=tstComma then begin
     Scanner.Match(tstComma);
    end else begin
     break;
    end;
   end;
   LastTreeNode:=nil;
   for i:=0 to ParamaterList.Count-1 do begin
    NewTreeNode:=ParamaterList[i];
    if i=0 then begin
     result:=NewTreeNode;
    end else if assigned(LastTreeNode) then begin
     LastTreeNode.Right:=NewTreeNode;
    end;
    LastTreeNode:=NewTreeNode;
   end;
  finally
   ParamaterList.Free;
  end;
 end;
end;

function TParser.ParseNewDisposeParameter(IsNew:boolean):TTreeNode;
var FirstTreeNode,NewTreeNode,LastTreeNode:TTreeNode;
    ObjectClassSymbolList:TSymbolList;
    AType:PType;
begin
 FirstTreeNode:=ParseExpression(false);
 NewTreeNode:=FirstTreeNode;
 LastTreeNode:=TreeManager.GenerateParameterNode(NewTreeNode,nil);
 if assigned(NewTreeNode.Return) and assigned(NewTreeNode.Return^.PointerTo) and assigned(NewTreeNode.Return^.PointerTo^.TypeDefinition) and (NewTreeNode.Return^.PointerTo^.TypeDefinition^.TypeDefinition=ttdOBJECT) then begin
  AType:=NewTreeNode.Return^.PointerTo^.TypeDefinition;
  ObjectClassSymbolList:=NewTreeNode.Return^.PointerTo^.TypeDefinition^.RecordTable;
  SymbolManager.PushSymbolList(ObjectClassSymbolList);
  if Scanner.CurrentToken<>tstRightParen then begin
   Scanner.Match(tstComma);
   NewTreeNode:=ParseExpression(false);
   if NewTreeNode.TreeNodeType=ttntCALL then begin
    NewTreeNode.Right:=TreeManager.GenerateTempObjectNode(AType);
    NewTreeNode.MethodSymbol:=NewTreeNode.Symbol;
    if assigned(NewTreeNode.MethodSymbol^.OwnerObjectClass) then begin
     if IsNew and SymbolManager.IsObjectClassAncestorType(NewTreeNode.MethodSymbol^.OwnerObjectClass,AType) and not (tpaConstructor in NewTreeNode.MethodSymbol^.ProcedureAttributes) then begin
      Error.AddErrorCode(82);
     end else if (not IsNew) and SymbolManager.IsObjectClassAncestorType(NewTreeNode.MethodSymbol^.OwnerObjectClass,AType) and not (tpaDestructor in NewTreeNode.MethodSymbol^.ProcedureAttributes) then begin
      Error.AddErrorCode(83);
     end;
    end else begin
     if IsNew then begin
      Error.AddErrorCode(82);
     end else begin
      Error.AddErrorCode(83);
     end;
    end;
   end else begin
    if IsNew then begin
     Error.AddErrorCode(82);
    end else begin
     Error.AddErrorCode(83);
    end;
   end;
   LastTreeNode.Right:=TreeManager.GenerateParameterNode(NewTreeNode,nil);
   SymbolManager.PopSymbolList(ObjectClassSymbolList);
  end;
 end;
 result:=LastTreeNode;
end;

function TParser.ParseTypeInfoParameter:TTreeNode;
var AType:PType;
begin
 result:=nil;
 AType:=ParseTypeDefinition('');
 if assigned(AType) then begin
  if AType^.NeedTypeInfo then begin
   result:=TreeManager.GenerateParameterNode(TreeManager.GenerateTypeInfoNode(AType),nil);
  end else begin
   if assigned(AType^.Symbol) then begin
    Error.AbortCode(138,CorrectSymbolName(AType^.Symbol^.Name));
   end else begin
    Error.AbortCode(138,'???');
   end;
  end;
 end else begin
  Error.AbortCode(115);
 end;
end;

procedure TParser.CheckDefaultParameters(FirstNeededDefaultSymbol:PSymbol;var Parameters:TTreeNode);
var TempSymbol,DefaultParameterSymbol:PSymbol;
    LastParameter,NewParameter:TTreeNode;
begin
 if assigned(FirstNeededDefaultSymbol) then begin
  TempSymbol:=FirstNeededDefaultSymbol;
  LastParameter:=Parameters;
  while assigned(LastParameter) and assigned(LastParameter.Right) do begin
   LastParameter:=LastParameter.Right;
  end;
  while assigned(TempSymbol) do begin
   DefaultParameterSymbol:=TempSymbol^.DefaultParameterSymbol;
   if assigned(DefaultParameterSymbol) then begin
    NewParameter:=nil;
    case DefaultParameterSymbol^.SymbolType of
     Symbols.tstConstant:begin
      case DefaultParameterSymbol^.ConstantType of
       tctOrdinal:begin
        NewParameter:=TreeManager.GenerateOrdConstNode(DefaultParameterSymbol^.IntValue,DefaultParameterSymbol^.ConstantTypeRecord);
       end;
       tctAnsiChar:begin
        NewParameter:=TreeManager.GenerateCharConstNode(DefaultParameterSymbol^.CharValue,SymbolManager.TypeChar);
       end;
       tctWideChar:begin
        NewParameter:=TreeManager.GenerateCharConstNode(DefaultParameterSymbol^.CharValue,SymbolManager.TypeWideChar);
       end;
       tctHugeChar:begin
        NewParameter:=TreeManager.GenerateCharConstNode(DefaultParameterSymbol^.CharValue,SymbolManager.TypeHugeChar);
       end;
       tctAnsiString:begin
        NewParameter:=TreeManager.GenerateStringConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypeAnsiString);
       end;
       tctPANSICHAR:begin
        NewParameter:=TreeManager.GeneratePCharConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypePAnsiChar);
       end;
       tctWideString:begin
        NewParameter:=TreeManager.GenerateStringConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypeWideString);
       end;
       tctHugeString:begin
        NewParameter:=TreeManager.GenerateStringConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypeHugeString);
       end;
       tctPWIDECHAR:begin
        NewParameter:=TreeManager.GeneratePCharConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypePWideChar);
       end;
       tctPHUGECHAR:begin
        NewParameter:=TreeManager.GeneratePCharConstNode(DefaultParameterSymbol^.StringValue,SymbolManager.TypePHugeChar);
       end;
       tctFloat:begin
        NewParameter:=TreeManager.GenerateFloatConstNode(DefaultParameterSymbol^.FloatValue,SymbolManager.TypeExtended);
       end;
       tctPointer:begin
        NewParameter:=TreeManager.GenerateLeftNode(ttntAddress,TreeManager.GenerateVarNode(DefaultParameterSymbol^.PointerTo));
       end;
       tctSet:begin
        NewParameter:=TreeManager.GenerateSetConstNode(DefaultParameterSymbol^.SetArray,DefaultParameterSymbol^.ConstantTypeRecord);
       end;
      end;
      if assigned(NewParameter) then begin
       NewParameter.Symbol:=DefaultParameterSymbol;
      end;
     end;
     else begin
      Error.InternalError(201303040708001);
      break;
     end;
    end;
    if assigned(NewParameter) then begin
     NewParameter:=TreeManager.GenerateParameterNode(NewParameter,nil);
     if assigned(LastParameter) then begin
      LastParameter.Right:=NewParameter;
     end else begin
      Parameters:=LastParameter;
     end;
     LastParameter:=NewParameter;
    end else begin
     Error.InternalError(201303040709000);
    end;
   end else begin
    Error.InternalError(201303040708000);
    break;
   end;
   TempSymbol:=TempSymbol^.Next;
  end;
 end;
end;

procedure TParser.CheckParameters(Symbol:PSymbol;SymbolParameters:TSymbolList;var Parameters:TTreeNode);
var ParameterImbalance:longint;
    TypeA,TypeB:PType;
    FirstNeededDefaultSymbol:PSymbol;
begin
 ParameterImbalance:=0;
 TypeA:=nil;
 TypeB:=nil;
 FirstNeededDefaultSymbol:=nil;
 case CompareCallParameters(Error,SymbolManager,SymbolParameters,Parameters,ParameterImbalance,TypeA,TypeB,FirstNeededDefaultSymbol) of
  tcteIncompatible:begin
   case ParameterImbalance of
    -1:begin
     Error.AbortCode(36,CorrectSymbolName(Symbol^.Name));
    end;
    1:begin
     Error.AbortCode(35,CorrectSymbolName(Symbol^.Name));
    end;
    else begin
     if (assigned(TypeA) and assigned(TypeB)) and (assigned(TypeA^.Symbol) and assigned(TypeB^.Symbol)) then begin
      Error.AbortCode(9,CorrectSymbolName(TypeA^.Symbol^.Name),CorrectSymbolName(TypeB^.Symbol^.Name));
     end else if assigned(TypeA) and assigned(TypeA^.Symbol) then begin
      Error.AbortCode(8,CorrectSymbolName(TypeA^.Symbol^.Name));
     end else if assigned(TypeB) and assigned(TypeB^.Symbol) then begin
      Error.AbortCode(8,CorrectSymbolName(TypeB^.Symbol^.Name));
     end else begin
      Error.AbortCode(7);
     end;
    end;
   end;
  end;
  else begin
   CheckDefaultParameters(FirstNeededDefaultSymbol,Parameters);
  end;
 end;
end;

procedure TParser.SearchOverloadedSymbolAndCheckParameters(var Symbol:PSymbol;var Parameters:TTreeNode);
var TempSymbol:PSymbol;
    Overloaded:boolean;
    ParameterImbalance:longint;
    TypeA,TypeB:PType;
    FirstNeededDefaultSymbol:PSymbol;
begin
 if Symbol^.InternalProcedure=tipNONE then begin
  TempSymbol:=Symbol;
  Overloaded:=false;
  ParameterImbalance:=0;
  TypeA:=nil;
  TypeB:=nil;
  FirstNeededDefaultSymbol:=nil;
  while assigned(TempSymbol) do begin
   case CompareCallParameters(Error,SymbolManager,TempSymbol^.Parameter,Parameters,ParameterImbalance,TypeA,TypeB,FirstNeededDefaultSymbol) of
    tcteIncompatible:begin
     Overloaded:=Overloaded or (assigned(TempSymbol^.NextOverloaded) or (tpaOVERLOAD in TempSymbol^.ProcedureAttributes));
     TempSymbol:=TempSymbol^.NextOverloaded;
    end;
    else begin
     break;
    end;
   end;
  end;
  if assigned(TempSymbol) then begin
   Symbol:=TempSymbol;
   CheckDefaultParameters(FirstNeededDefaultSymbol,Parameters);
  end else begin
   if Overloaded then begin
    Error.AbortCode(265,CorrectSymbolName(Symbol^.Name));
   end else begin
    case ParameterImbalance of
     -1:begin
      Error.AbortCode(36,CorrectSymbolName(Symbol^.Name));
     end;
     1:begin
      Error.AbortCode(35,CorrectSymbolName(Symbol^.Name));
     end;
     else begin
      if (assigned(TypeA) and assigned(TypeB)) and (assigned(TypeA^.Symbol) and assigned(TypeB^.Symbol)) then begin
       Error.AbortCode(9,CorrectSymbolName(TypeA^.Symbol^.Name),CorrectSymbolName(TypeB^.Symbol^.Name));
      end else if assigned(TypeA) and assigned(TypeA^.Symbol) then begin
       Error.AbortCode(8,CorrectSymbolName(TypeA^.Symbol^.Name));
      end else if assigned(TypeB) and assigned(TypeB^.Symbol) then begin
       Error.AbortCode(8,CorrectSymbolName(TypeB^.Symbol^.Name));
      end else begin
       Error.AbortCode(7);
      end;
     end;
    end;
   end;
  end;
 end;
end;

function TParser.HasPropertyNode(TreeNode:TTreeNode):boolean;
begin
 if assigned(TreeNode) then begin
  case TreeNode.TreeNodeType of
   ttntProperty:begin
    if not TreeNode.PropertyReadyForToOptimize then begin
     result:=true;
     exit;
    end;
   end;
  end;
  result:=HasPropertyNode(TreeNode.Left) or HasPropertyNode(TreeNode.Right);
 end else begin
  result:=false;
 end;
end;

function TParser.FinalizePropertyNodes(TreeNode,AssignTreeNode:TTreeNode;IsWrite:boolean):TTreeNode;
var Node,LastNode:TTreeNode;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 if assigned(TreeNode) then begin
  case TreeNode.TreeNodeType of
   ttntProperty:begin
    if not TreeNode.PropertyReadyForToOptimize then begin
     TreeNode.PropertyReadyForToOptimize:=true;
     TreeNode.PropertyIsToWrite:=IsWrite;
     if IsWrite then begin
      if assigned(AssignTreeNode) then begin
       LastNode:=nil;
       Node:=TreeNode.Left;
       while assigned(Node) and (Node.TreeNodeType=ttntParameter) do begin
        LastNode:=Node;
        Node:=Node.Right;
       end;
       Node:=TreeManager.GenerateParameterNode(AssignTreeNode,nil);
       if assigned(LastNode) then begin
        LastNode.Right:=Node;
       end else begin
        TreeNode.Left:=Node;
       end;
       case CompareTypesExt(Error,SymbolManager,Node.Left.Return,TreeNode.Return,ttntEmpty,ConvertType,ProcType,[tctoCHECKOPERATOR,tctoALLOWVARIANT]) of
        tcteIncompatible:begin
         Error.AddErrorCode(7);
        end;
       end;
      end else begin
       Error.InternalError(201306052222000);
      end;
      IsWrite:=false;
     end;
    end;
   end;
  end;
  FinalizePropertyNodes(TreeNode.Left,AssignTreeNode,IsWrite);
  FinalizePropertyNodes(TreeNode.Right,AssignTreeNode,IsWrite);
  FinalizePropertyNodes(TreeNode.Block,AssignTreeNode,IsWrite);
  FinalizePropertyNodes(TreeNode.ElseTree,AssignTreeNode,IsWrite);
  FinalizePropertyNodes(TreeNode.ExceptTree,AssignTreeNode,IsWrite);
  FinalizePropertyNodes(TreeNode.FinallyTree,AssignTreeNode,IsWrite);
 end;
 result:=TreeNode;
end;

function TParser.ParseFactor:TTreeNode;
var NewTreeNode,FirstTreeNode,LastTreeNode,LastParameterNode,ParameterNode:TTreeNode;
    ToHandleSymbol,CanHaveQualifiers,Overloaded,OK:boolean;
    AType,FieldType,ClassType,ProcType:PType;
    MethodSymbol,FieldSymbol,Symbol,TempSymbol,PropertySymbol,ParameterSymbol:PSymbol;
    FieldName,Name:ansistring;
    SetConstant:TSetArray;
    CounterValue,LowValue,HighValue:byte;
    Value:int64;
    ParameterImbalance,Counter,WhichWithLevel:longint;
    ConvertType:TConvertType;
begin
 NewTreeNode:=nil;
 AType:=nil;
 PropertySymbol:=nil;
 ToHandleSymbol:=false;
 CanHaveQualifiers:=false;
 case Scanner.CurrentToken of
  tstIdentifier:begin
   Error.Push;
   Name:=Scanner.ReadIdentifier(nil);
   WhichWithLevel:=-1;
   Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass,@WhichWithLevel);
   if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
    Error.Pop;
    Error.Push;
    Scanner.Match(tstPeriod);
    Name:=Scanner.ReadIdentifier(nil);
    Symbol:=Symbol^.SymbolList.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
   end;
   ToHandleSymbol:=true;
  end;
  tstINHERITED:begin
   Scanner.Match(tstINHERITED);
   if Scanner.CurrentToken=tstIdentifier then begin
    Name:=Scanner.ReadIdentifier(nil);
   end else if assigned(CurrentMethod) then begin
    Name:=CurrentMethod^.Name;
   end else begin
    Error.InternalError(200605181033000);
   end;
   Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
   if assigned(CurrentMethod) then begin
    AType:=CurrentMethod^.TypeDefinition;
    if assigned(AType^.ChildOf) then begin
     AType:=AType^.ChildOf^.TypeDefinition;
     MethodSymbol:=nil;
     while assigned(AType) and not Scanner.IsEOFOrAbortError do begin
      MethodSymbol:=AType^.RecordTable.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
      if assigned(MethodSymbol) then begin
       if MethodSymbol^.SymbolType in [Symbols.tstFunction,Symbols.tstProcedure] then begin
        NewTreeNode:=TreeManager.GenerateMethodCallNode(Symbol,MethodSymbol,nil,AType);
        if (Scanner.CurrentToken=tstLeftParen) or MustHaveParens then begin
         Scanner.Match(tstLeftParen);
         NewTreeNode.Left:=ParseCallParameter;
         SearchOverloadedSymbolAndCheckParameters(NewTreeNode.MethodSymbol,NewTreeNode.Left);
         Scanner.Match(tstRightParen);
        end else begin
         NewTreeNode.Left:=nil;
        end;
       end else begin
        Symbol:=MethodSymbol;
        ToHandleSymbol:=true;
        Error.Push;
       end;
       break;
      end else begin
       if assigned(AType^.ChildOf) and assigned(AType^.ChildOf^.TypeDefinition) then begin
        AType:=AType^.ChildOf^.TypeDefinition;
       end else begin
        Error.AbortCode(141,CorrectSymbolName(Name));
        break;
       end;
      end;
     end;
     if not assigned(MethodSymbol) then begin
      Error.AbortCode(506);
     end;
    end else begin
     Error.AbortCode(506);
    end;
   end;
  end;
  tstValue:begin
   Value:=Scanner.ReadNumber;
   if (Value>=-128) and (Value<=127) then begin
    AType:=SymbolManager.TypeShortint;
   end else if (Value>=0) and (Value<=255) then begin
    AType:=SymbolManager.TypeByte;
   end else if (Value>=-32768) and (Value<=32767) then begin
    AType:=SymbolManager.TypeSmallint;
   end else if (Value>=0) and (Value<=65535) then begin
    AType:=SymbolManager.TypeWord;
   end else if (Value>=low(longint)) and (Value<=high(longint)) then begin
    AType:=SymbolManager.TypeLongint;
   end else if (Value>=low(longword)) and (Value<=high(longword)) then begin
    AType:=SymbolManager.TypeLongword;
   end else begin
    AType:=SymbolManager.TypeInt64;
   end;
   NewTreeNode:=TreeManager.GenerateOrdConstNode(Value,AType);
  end;
  tstFloatValue:begin
   NewTreeNode:=TreeManager.GenerateFloatConstNode(Scanner.ReadFloat,SymbolManager.TypeExtended);
  end;
  tstStringValue:begin
   Value:=0;
   for Counter:=0 to length(Scanner.CurrentString)-1 do begin
    if Value<Scanner.CurrentString[Counter] then begin
     Value:=Scanner.CurrentString[Counter];
    end;
   end;
   if Value>$ffff then begin
    NewTreeNode:=TreeManager.GenerateStringConstNode(Scanner.CurrentString,SymbolManager.TypeHugeString);
   end else if Value>$ff then begin
    NewTreeNode:=TreeManager.GenerateStringConstNode(Scanner.CurrentString,SymbolManager.TypeWideString);
   end else begin
    NewTreeNode:=TreeManager.GenerateStringConstNode(Scanner.CurrentString,SymbolManager.TypeAnsiString);
   end;
   Scanner.Match(tstStringValue);
  end;
  tstCharValue:begin
   if Scanner.CurrentCharValue>$ffff then begin
    NewTreeNode:=TreeManager.GenerateCharConstNode(Scanner.CurrentCharValue,SymbolManager.TypeHugeChar);
   end else if Scanner.CurrentCharValue>$ff then begin
    NewTreeNode:=TreeManager.GenerateCharConstNode(Scanner.CurrentCharValue,SymbolManager.TypeWideChar);
   end else begin
    NewTreeNode:=TreeManager.GenerateCharConstNode(Scanner.CurrentCharValue,SymbolManager.TypeChar);
   end;
   Scanner.Match(tstCharValue);
  end;
  tstSTRING:begin
   Scanner.Match(tstLeftBracket);
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Scanner.Match(tstRightBracket);
  end;
  tstAT:begin
   Scanner.Match(tstAt);
   NewTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
   NewTreeNode:=TreeManager.GenerateLeftNode(ttntAddress,NewTreeNode);
  end;
  tstLeftParen:begin
   Scanner.Match(tstLeftParen);
   NewTreeNode:=ParseExpression(false);
   Scanner.Match(tstRightParen);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   AType:=NewTreeNode.Return;
   if assigned(AType) then begin
    CanHaveQualifiers:=true;
   end;
  end;
  tstLeftBracket:begin
   Scanner.Match(tstLeftBracket);
   FillChar(SetConstant,SizeOf(TSetArray),#0);
   if Scanner.CurrentToken<>tstRightBracket then begin
    while not Scanner.IsEOFOrAbortError do begin
     LastTreeNode:=nil;
     NewTreeNode:=ParseExpression(false);
     OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
     OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
     OptimizerHighLevel.OptimizeTree(NewTreeNode);
     AType:=NewTreeNode.Return;
     case NewTreeNode.TreeNodeType of
      ttntCharConst:begin
       Value:=ord(NewTreeNode.CharValue);
       if Value in [0..255] then begin
        SetConstant[Value shr 3]:=SetConstant[Value shr 3] or (1 shl (Value and 7));
       end else begin
        Error.AbortCode(28);
       end;
       NewTreeNode.Destroy;
      end;
      ttntORDConst:begin
       Value:=NewTreeNode.Value;
       if Value in [0..255] then begin
        SetConstant[Value shr 3]:=SetConstant[Value shr 3] or (1 shl (Value and 7));
       end else begin
        Error.AbortCode(28);
       end;
       NewTreeNode.Destroy;
      end;
      ttntSubRange:begin
       if NewTreeNode.Left.TreeNodeType=ttntCharConst then begin
        LowValue:=ord(NewTreeNode.Left.CharValue);
       end else begin
        LowValue:=NewTreeNode.Left.Value;
       end;
       if NewTreeNode.Right.TreeNodeType=ttntCharConst then begin
        HighValue:=ord(NewTreeNode.Right.CharValue);
       end else begin
        HighValue:=NewTreeNode.Right.Value;
       end;
       for CounterValue:=LowValue to HighValue do begin
        if CounterValue in [0..255] then begin
         SetConstant[CounterValue shr 3]:=SetConstant[CounterValue shr 3] or (1 shl (CounterValue and 7));
        end else begin
         Error.AbortCode(28);
         break;
        end;
       end;
       NewTreeNode.Destroy;
      end;
     end;
     if Scanner.CurrentToken=tstComma then begin
      Scanner.Match(tstComma);
     end else begin
      break;
     end;
    end;
   end;
   Scanner.Match(tstRightBracket);
   NewTreeNode:=TreeManager.GenerateLeftNode(ttntSETCONST,LastTreeNode);
   NewTreeNode.SetData:=SetConstant;
   NewTreeNode.Return:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   NewTreeNode.Return.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   NewTreeNode.Return.TypeDefinition:=ttdSet;
   NewTreeNode.Return.TypeKind:=TypeKindSet;
   NewTreeNode.Return.SetOf:=AType;
  end;
  tstPlus:begin
   Scanner.Match(tstPlus);
   NewTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
  end;
  tstMinus:begin
   Scanner.Match(tstMinus);
   NewTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
   NewTreeNode:=TreeManager.GenerateLeftNode(ttntMinus,NewTreeNode);
  end;
  tstNot:begin
   Scanner.Match(tstNot);
   NewTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
   NewTreeNode:=TreeManager.GenerateLeftNode(ttntNot,NewTreeNode);
  end;
  tstNIL:begin
   Scanner.Match(tstNIL);
   NewTreeNode:=TreeManager.GenerateNilNode(SymbolManager.TypePointer);
   NewTreeNode:=FinalizePropertyNodes(NewTreeNode,nil,false);
  end;
  tstCEXPR:begin
   Scanner.Match(tstCEXPR);
   NewTreeNode:=nil;
   FirstTreeNode:=nil;
   LastTreeNode:=nil;
   while not Scanner.IsEOFOrAbortError do begin
    case Scanner.CurrentToken of
     tstCBLOCK:begin
      NewTreeNode:=TreeManager.GenerateCBlockNode(Scanner.CurrentString);
      Scanner.ReadNext;
     end;
     tstCEND:begin
      Scanner.ReadNext;
      break;
     end;
     tstCSKIP:begin
      Scanner.ReadNext;
     end;
     else begin
      if Scanner.CurrentMode=smPASCALEXPRESSION then begin
       NewTreeNode:=TreeManager.GeneratePascalBlockNode(ParseExpression(false));
      end else begin
       NewTreeNode:=TreeManager.GeneratePascalBlockNode(ParseStatement(false));
       if Scanner.CurrentToken=tstSeparator then begin
        Scanner.ReadNext;
       end;
      end;
     end;
    end;
    if assigned(LastTreeNode) then begin
     LastTreeNode.Left:=NewTreeNode;
    end else begin
     FirstTreeNode:=NewTreeNode;
    end;
    LastTreeNode:=NewTreeNode;
   end;
   NewTreeNode:=TreeManager.GenerateCExpressionNode(FirstTreeNode);
   NewTreeNode:=FinalizePropertyNodes(NewTreeNode,nil,false);
  end;
  else begin
   Error.AbortCode(504);
  end;
 end;
 while true do begin
  if ToHandleSymbol then begin
   ToHandleSymbol:=false;
   if assigned(Symbol) then begin
    AType:=Symbol^.TypeDefinition;
    if (WhichWithLevel<0) and assigned(Symbol^.OwnerObjectClass) and
       ((assigned(CurrentProcedureFunction) and assigned(CurrentProcedureFunction^.OwnerObjectClass)) and
         (tpaClassProcedure in CurrentProcedureFunction^.ProcedureAttributes)) and
        not (((Symbol^.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction]) and (tpaClassProcedure in Symbol^.ProcedureAttributes)) or
             not (Symbol^.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction])) then begin
     Error.AddErrorCode(128,CorrectSymbolName(Symbol .Name));
    end;
    case Symbol^.SymbolType of
     Symbols.tstVariable:begin
      NewTreeNode:=TreeManager.GenerateVarNode(Symbol);
      if (WhichWithLevel>=0) and (WhichWithLevel<WithStack.Count) then begin
       NewTreeNode.WithType:=WithStack.Items[WhichWithLevel];
       NewTreeNode.WithLevel:=WhichWithLevel;
      end;
     end;
     Symbols.tstProperty:begin
      PropertySymbol:=Symbol;
      Symbol:=nil;
     end;
     Symbols.tstType:begin
      if Scanner.CurrentToken=tstLeftParen then begin
       Scanner.Match(tstLeftParen);
       NewTreeNode:=ParseExpression(false);
       Scanner.Match(tstRightParen);
       NewTreeNode:=TreeManager.GenerateTypeConvNode(NewTreeNode,AType,true);
      end else begin
       NewTreeNode:=TreeManager.GenerateTypeNode(AType);
      end;
     end;
     Symbols.tstConstant:begin
      case Symbol^.ConstantType of
       tctOrdinal:begin
        NewTreeNode:=TreeManager.GenerateOrdConstNode(Symbol^.IntValue,Symbol^.ConstantTypeRecord);
       end;
       tctAnsiChar:begin
        NewTreeNode:=TreeManager.GenerateCharConstNode(Symbol^.CharValue,SymbolManager.TypeChar);
       end;
       tctWideChar:begin
        NewTreeNode:=TreeManager.GenerateCharConstNode(Symbol^.CharValue,SymbolManager.TypeWideChar);
       end;
       tctHugeChar:begin
        NewTreeNode:=TreeManager.GenerateCharConstNode(Symbol^.CharValue,SymbolManager.TypeHugeChar);
       end;
       tctAnsiString:begin
        NewTreeNode:=TreeManager.GenerateStringConstNode(Symbol^.StringValue,SymbolManager.TypeAnsiString);
       end;
       tctPANSICHAR:begin
        NewTreeNode:=TreeManager.GeneratePCharConstNode(Symbol^.StringValue,SymbolManager.TypePAnsiChar);
       end;
       tctWideString:begin
        NewTreeNode:=TreeManager.GenerateStringConstNode(Symbol^.StringValue,SymbolManager.TypeWideString);
       end;
       tctHugeString:begin
        NewTreeNode:=TreeManager.GenerateStringConstNode(Symbol^.StringValue,SymbolManager.TypeHugeString);
       end;
       tctPWIDECHAR:begin
        NewTreeNode:=TreeManager.GeneratePCharConstNode(Symbol^.StringValue,SymbolManager.TypePWideChar);
       end;
       tctPHUGECHAR:begin
        NewTreeNode:=TreeManager.GeneratePCharConstNode(Symbol^.StringValue,SymbolManager.TypePHugeChar);
       end;
       tctFloat:begin
        NewTreeNode:=TreeManager.GenerateFloatConstNode(Symbol^.FloatValue,SymbolManager.TypeExtended);
       end;
       tctPointer:begin
        NewTreeNode:=TreeManager.GenerateLeftNode(ttntAddress,TreeManager.GenerateVarNode(Symbol^.PointerTo));
       end;
       tctSet:begin
        NewTreeNode:=TreeManager.GenerateSetConstNode(Symbol^.SetArray,Symbol^.ConstantTypeRecord);
       end;
      end;
      NewTreeNode.Symbol:=Symbol;
     end;
     Symbols.tstProcedure,Symbols.tstFunction:begin
      NewTreeNode:=TreeManager.GenerateCallNode(Symbol);
      if (Scanner.CurrentToken=tstLeftParen) or MustHaveParens then begin
       Scanner.Match(tstLeftParen);
       case Symbol^.InternalProcedure of
        tipNEW,tipDISPOSE:begin
         NewTreeNode.Left:=ParseNewDisposeParameter(Symbol^.InternalProcedure=tipNew);
        end;
        tipTYPEINFO:begin
         NewTreeNode.Left:=ParseTypeInfoParameter;
        end;
        else begin
         NewTreeNode.Left:=ParseCallParameter;
         SearchOverloadedSymbolAndCheckParameters(NewTreeNode.Symbol,NewTreeNode.Left);
        end;
       end;
       Scanner.Match(tstRightParen);
      end else begin
       NewTreeNode.Left:=nil;
      end;
     end;
     else begin
      Error.AbortCode(504);
     end;
    end;
    CanHaveQualifiers:=true;
   end else begin
    Error.AbortCode(2,CorrectSymbolName(Name));
   end;
   Error.Pop;
  end;
  if assigned(PropertySymbol) then begin
   if assigned(PropertySymbol^.PropertyType) then begin
    NewTreeNode:=TreeManager.GeneratePropertyNode(Symbol,PropertySymbol,nil,NewTreeNode,FieldSymbol^.PropertyType);
    AType:=FieldSymbol^.PropertyType;
    if assigned(PropertySymbol^.PropertyIndex) then begin
     NewTreeNode.Left:=TreeManager.GenerateParameterNode(TreeManager.GenerateOrdConstNode(PropertySymbol^.PropertyIndex^.IntValue,PropertySymbol^.PropertyIndex^.ConstantTypeRecord),nil);
    end;
    CanHaveQualifiers:=true;
   end else begin
    Error.InternalError(201306050146000);
   end;
  end;
  if CanHaveQualifiers then begin
   while CanHaveQualifiers and not Scanner.IsEOFOrAbortError do begin
    case Scanner.CurrentToken of
     tstPointer:begin
      Scanner.Match(tstPointer);
      if AType^.TypeDefinition=ttdPOINTER then begin
       if assigned(AType^.PointerTo) then begin
        AType:=AType^.PointerTo^.TypeDefinition;
       end else begin
        if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntAddress) and assigned(NewTreeNode.Left) then begin
         AType:=NewTreeNode.Left.Return;
        end else begin
         AType:=SymbolManager.TypeEmpty;
        end;
       end;
       NewTreeNode:=TreeManager.GeneratePointerNode(NewTreeNode,AType);
      end else begin
       Error.AbortCode(504);
      end;
     end;
     tstLeftParen:begin
      if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntVAR) and (NewTreeNode.Symbol^.TypeDefinition^.TypeDefinition=ttdProcedure) then begin
       NewTreeNode.TreeNodeType:=ttntCALL;
       Scanner.Match(tstLeftParen);
       NewTreeNode.Left:=ParseCallParameter;
       CheckParameters(NewTreeNode.Symbol,NewTreeNode.Symbol^.TypeDefinition^.Parameter,NewTreeNode.Left);
       Scanner.Match(tstRightParen);
       AType:=NewTreeNode.Symbol^.ReturnType;
      end else begin
       Error.AbortCode(504);
      end;
     end;
     tstLeftBracket:begin
      Scanner.Match(tstLeftBracket);
      if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType<>ttntProperty) and assigned(AType) and (AType^.TypeDefinition=ttdCLASS) and
         not assigned(PropertySymbol) then begin
       ClassType:=AType;
       while assigned(ClassType) and not assigned(PropertySymbol) do begin
        if assigned(ClassType^.RecordTable) then begin
         TempSymbol:=ClassType^.RecordTable.First;
         while assigned(TempSymbol) do begin
          case TempSymbol^.SymbolType of
           Symbols.tstProperty:begin
            if TempSymbol.PropertyDefaultArray then begin
             PropertySymbol:=TempSymbol;
             break;
            end;
           end;
          end;
          TempSymbol:=TempSymbol^.Next;
         end;
        end;
        if assigned(ClassType^.ChildOf) then begin
         ClassType:=ClassType^.ChildOf^.TypeDefinition;
        end else begin
         break;
        end;
       end;
       if assigned(PropertySymbol) then begin
        if assigned(PropertySymbol^.PropertyType) then begin
         NewTreeNode:=TreeManager.GeneratePropertyNode(Symbol,PropertySymbol,nil,NewTreeNode,FieldSymbol^.PropertyType);
         AType:=FieldSymbol^.PropertyType;
         if assigned(PropertySymbol^.PropertyIndex) then begin
          NewTreeNode.Left:=TreeManager.GenerateParameterNode(TreeManager.GenerateOrdConstNode(PropertySymbol^.PropertyIndex^.IntValue,PropertySymbol^.PropertyIndex^.ConstantTypeRecord),nil);
         end;
        end else begin
         Error.InternalError(201306060037000);
        end;
       end else begin
        Error.AbortCode(154);
       end;
      end;
      if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntProperty) and assigned(PropertySymbol) and
         assigned(PropertySymbol.PropertyParameter) and assigned(PropertySymbol.PropertyParameter.First) then begin
       ParameterSymbol:=PropertySymbol.PropertyParameter.First;
       LastParameterNode:=NewTreeNode.Left;
       while assigned(LastParameterNode) and assigned(LastParameterNode.Right) do begin
        LastParameterNode:=LastParameterNode.Right;
       end;
       while assigned(ParameterSymbol) and not Scanner.IsEOFOrAbortError do begin
        ParameterNode:=TreeManager.GenerateParameterNode(ParseExpression(false),nil);
        case CompareTypesExt(Error,SymbolManager,ParameterNode.Left.Return,ParameterSymbol^.TypeDefinition,ttntEmpty,ConvertType,ProcType,[tctoCHECKOPERATOR,tctoALLOWVARIANT]) of
         tcteIncompatible:begin
          Error.AddErrorCode(7);
         end;
        end;
        if assigned(LastParameterNode) then begin
         LastParameterNode.Right:=ParameterNode;
        end else begin
         NewTreeNode.Left:=ParameterNode;
        end;
        LastParameterNode:=ParameterNode;
        ParameterSymbol:=ParameterSymbol^.Next;
        if Scanner.CurrentToken=tstComma then begin
         Scanner.Match(tstComma);
        end else begin
         break;
        end;
       end;
       if assigned(ParameterSymbol) then begin
        Error.AbortCode(36);
       end;
      end;
      while not Scanner.IsEOFOrAbortError do begin
       case AType^.TypeDefinition of
        ttdShortString,ttdLongString:begin
         LastTreeNode:=ParseExpression(false);
         OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
         OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
         OptimizerHighLevel.OptimizeTree(LastTreeNode);
         NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntIndex,NewTreeNode,LastTreeNode);
         AType^.Definition:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
         AType^.Definition^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
         AType^.Range:=AType^.Definition;
         AType:=AType^.Definition;
         AType^.TypeDefinition:=ttdSubRange;
         AType^.SubRangeType:=tstUnsignedChar;
         AType^.TypeKind:=TypeKindAnsiChar;
         AType^.NeedTypeInfo:=false;
         AType^.LowerLimit:=0;
         AType^.UpperLimit:=255;
         NewTreeNode.Return:=AType;
        end;
        ttdArray:begin
         LastTreeNode:=ParseExpression(false);
         OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
         OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
         OptimizerHighLevel.OptimizeTree(LastTreeNode);
         NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntIndex,NewTreeNode,LastTreeNode);
         AType:=AType^.Definition;
         NewTreeNode.Return:=AType;
        end;
        ttdPointer:begin
         if assigned(AType^.PointerTo) then begin
          AType:=AType^.PointerTo^.TypeDefinition;
         end else begin
          if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntAddress) and assigned(NewTreeNode.Left) then begin
           AType:=NewTreeNode.Left.Return;
          end else begin
           AType:=SymbolManager.TypeEmpty;
          end;
         end;
         NewTreeNode:=TreeManager.GeneratePointerNode(NewTreeNode,AType);
         PropertySymbol:=nil;
         continue;
        end;
        else begin
         Error.AbortCode(501);
        end;
       end;
       if Scanner.CurrentToken=tstComma then begin
        Scanner.Match(tstComma);
       end else begin
        break;
       end;
      end;
      Scanner.Match(tstRightBracket);
     end;
     tstPeriod:begin
      Scanner.Match(tstPeriod);
      while assigned(AType) and (AType^.TypeDefinition=ttdPOINTER) do begin
       if assigned(AType^.PointerTo) then begin
        AType:=AType^.PointerTo^.TypeDefinition;
       end else begin
        if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntAddress) and assigned(NewTreeNode.Left) then begin
         AType:=NewTreeNode.Left.Return;
        end else begin
         AType:=SymbolManager.TypeEmpty;
        end;
       end;
       NewTreeNode:=TreeManager.GeneratePointerNode(NewTreeNode,AType);
      end;
      case AType^.TypeDefinition of
       ttdRecord,ttdObject,ttdClass,ttdInterface:begin
        FieldName:=Scanner.ReadIdentifier(nil);
 //     Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
        if assigned(AType) then begin
         FieldSymbol:=AType^.RecordTable.GetSymbol(FieldName,ModuleSymbol,CurrentObjectClass,true);
         if assigned(FieldSymbol) then begin
          FieldType:=FieldSymbol^.TypeDefinition;
          case FieldSymbol^.SymbolType of
           Symbols.tstVariable:begin
            case AType^.TypeDefinition of
             ttdRecord,ttdObject,ttdClass:begin
              NewTreeNode:=TreeManager.GenerateFieldNode(Symbol,FieldSymbol,NewTreeNode);
              AType:=FieldSymbol^.TypeDefinition;
             end;
             ttdInterface:begin
              Error.AbortCode(221);
             end;
             else begin
              Error.InternalError(200605181020000);
             end;
            end;
           end;
           Symbols.tstFunction,Symbols.tstProcedure:begin
            case AType^.TypeDefinition of
             ttdObject,ttdClass,ttdInterface:begin       
              NewTreeNode:=TreeManager.GenerateMethodCallNode(Symbol,FieldSymbol,NewTreeNode,nil);
              if (Scanner.CurrentToken=tstLeftParen) or MustHaveParens then begin
               Scanner.Match(tstLeftParen);
               NewTreeNode.Left:=ParseCallParameter;
               SearchOverloadedSymbolAndCheckParameters(NewTreeNode.MethodSymbol,NewTreeNode.Left);
               AType:=NewTreeNode.MethodSymbol^.ReturnType;
               Scanner.Match(tstRightParen);
              end else begin
               NewTreeNode.Left:=nil;
              end;
             end;
             else begin
              Error.InternalError(200605181020001);
             end;
            end;
           end;
           Symbols.tstProperty:begin
            PropertySymbol:=FieldSymbol;
            Symbol:=FieldSymbol;
            ToHandleSymbol:=false;
            CanHaveQualifiers:=false;
            continue;
           end;
          end;
         end else begin
          Error.AbortCode(174);
          AType:=nil;
         end;
        end else begin
         Error.AbortCode(503);
         AType:=nil;
        end;
       end;
       else begin
        if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntVAR) and (NewTreeNode.Symbol^.TypeDefinition^.TypeDefinition=ttdProcedure) then begin
         NewTreeNode.TreeNodeType:=ttntCALL;
         if (Scanner.CurrentToken=tstLeftParen) or MustHaveParens then begin
          Scanner.Match(tstLeftParen);
          NewTreeNode.Left:=ParseCallParameter;
          CheckParameters(NewTreeNode.Symbol,NewTreeNode.Symbol^.TypeDefinition^.Parameter,NewTreeNode.Left);
          Scanner.Match(tstRightParen);
          AType:=NewTreeNode.Symbol^.ReturnType;
         end else begin
          NewTreeNode.Left:=nil;
         end;
        end else begin
         break;
        end;
       end;
      end;
     end;
     else begin
      break;
     end;
    end;
   end;
  end;
  PropertySymbol:=nil;
  if ToHandleSymbol then begin
  end;
  break;
 end;
 if not assigned(NewTreeNode) then begin
  NewTreeNode:=TreeManager.GenerateEmptyNode;
 end;
 OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
 OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
 OptimizerHighLevel.OptimizeTree(NewTreeNode);
 result:=NewTreeNode;
end;

function TParser.ParseTerm:TTreeNode;
var NewTreeNode,LastTreeNode:TTreeNode;
    AType:PType;
    Symbol:PSymbol;
    Name:ansistring;
begin
 NewTreeNode:=ParseFactor;
 while not Scanner.IsEOFOrAbortError do begin
  case Scanner.CurrentToken of
   tstMul:begin
    Scanner.Match(tstMul);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntMul,NewTreeNode,LastTreeNode);
   end;
   tstSlash:begin
    Scanner.Match(tstSlash);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntSlash,NewTreeNode,LastTreeNode);
   end;
   tstDIV:begin
    Scanner.Match(tstDIV);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntDiv,NewTreeNode,LastTreeNode);
   end;
   tstMOD:begin
    Scanner.Match(tstMOD);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntMod,NewTreeNode,LastTreeNode);
   end;
   tstAND:begin
    Scanner.Match(tstAND);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntAnd,NewTreeNode,LastTreeNode);
   end;
   tstSHL:begin
    Scanner.Match(tstSHL);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntShl,NewTreeNode,LastTreeNode);
   end;
   tstSHR:begin
    Scanner.Match(tstSHR);
    LastTreeNode:=FinalizePropertyNodes(ParseFactor,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntShr,NewTreeNode,LastTreeNode);
   end;
   tstAS:begin
    Scanner.Match(tstAS);
    Name:=Scanner.ReadIdentifier(nil);
    Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
    if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
     Scanner.Match(tstPeriod);
     Name:=Scanner.ReadIdentifier(nil);
     Symbol:=Symbol^.SymbolList.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
    end;
    if assigned(Symbol) then begin
     AType:=Symbol^.TypeDefinition;
     if Symbol^.SymbolType=Symbols.tstType then begin
      NewTreeNode:=TreeManager.GenerateTypeConvNode(NewTreeNode,AType,true);
     end else begin
      Error.AbortCode(115);
     end;
    end else begin
     Error.AbortCode(115);
    end;
   end;
   else begin
    break;
   end;
  end;
  if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Left.Return:=NewTreeNode.Right.Return;
  end else if assigned(NewTreeNode.Right.Return) and (NewTreeNode.Right.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Right.Return:=NewTreeNode.Left.Return;
  end;
  OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
  OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
  OptimizerHighLevel.OptimizeTree(NewTreeNode);
 end;
 result:=NewTreeNode;
end;

function TParser.ParseSimpleExpression:TTreeNode;
var NewTreeNode,LastTreeNode:TTreeNode;
begin
 NewTreeNode:=ParseTerm;
 while not Scanner.IsEOFOrAbortError do begin
  case Scanner.CurrentToken of
   tstPlus:begin
    Scanner.Match(tstPlus);
    LastTreeNode:=FinalizePropertyNodes(ParseTerm,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntAdd,NewTreeNode,LastTreeNode);
   end;
   tstMinus:begin
    Scanner.Match(tstMinus);
    LastTreeNode:=FinalizePropertyNodes(ParseTerm,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntSub,NewTreeNode,LastTreeNode);
   end;
   tstOR:begin
    Scanner.Match(tstOR);
    LastTreeNode:=FinalizePropertyNodes(ParseTerm,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntOr,NewTreeNode,LastTreeNode);
   end;
   tstXOR:begin
    Scanner.Match(tstXOR);
    LastTreeNode:=FinalizePropertyNodes(ParseTerm,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntXor,NewTreeNode,LastTreeNode);
   end;
   else begin
    break;
   end;
  end;
  if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Left.Return:=NewTreeNode.Right.Return;
  end else if assigned(NewTreeNode.Right.Return) and (NewTreeNode.Right.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Right.Return:=NewTreeNode.Left.Return;
  end;
  OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
  OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
  OptimizerHighLevel.OptimizeTree(NewTreeNode);
 end;
 result:=NewTreeNode;
end;

function TParser.ParseBooleanExpression:TTreeNode;
var NewTreeNode,LastTreeNode:TTreeNode;
    AType:PType;
    Symbol:PSymbol;
    Name:ansistring;
begin
 NewTreeNode:=ParseSimpleExpression;
 while not Scanner.IsEOFOrAbortError do begin
  case Scanner.CurrentToken of
   tstLess:begin
    Scanner.Match(tstLess);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntLess,NewTreeNode,LastTreeNode);
   end;
   tstLessOrEqual:begin
    Scanner.Match(tstLessOrEqual);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntLessOrEqual,NewTreeNode,LastTreeNode);
   end;
   tstGreater:begin
    Scanner.Match(tstGreater);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntGreater,NewTreeNode,LastTreeNode);
   end;
   tstGreaterOrEqual:begin
    Scanner.Match(tstGreaterOrEqual);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntGreaterOrEqual,NewTreeNode,LastTreeNode);
   end;
   tstEqual:begin
    Scanner.Match(tstEqual);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntEqual,NewTreeNode,LastTreeNode);
   end;
   tstNotEqual:begin
    Scanner.Match(tstNotEqual);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntNotEqual,NewTreeNode,LastTreeNode);
   end;
   tstIN:begin
    Scanner.Match(tstIN);
    LastTreeNode:=FinalizePropertyNodes(ParseSimpleExpression,nil,false);
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntIN,NewTreeNode,LastTreeNode);
   end;
   tstIS:begin
    Scanner.Match(tstIS);
    Name:=Scanner.ReadIdentifier(nil);
    Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
    if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
     Scanner.Match(tstPeriod);
     Name:=Scanner.ReadIdentifier(nil);
     Symbol:=Symbol^.SymbolList.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
    end;
    if assigned(Symbol) then begin
     AType:=Symbol^.TypeDefinition;
     if Symbol^.SymbolType=Symbols.tstType then begin
      NewTreeNode:=TreeManager.GenerateTypeCheckNode(NewTreeNode,AType);
     end else begin
      Error.AbortCode(115);
     end;
    end else begin
     Error.AbortCode(115);
    end;
   end;
   else begin
    break;
   end;
  end;
  if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Left.Return:=NewTreeNode.Right.Return;
  end else if assigned(NewTreeNode.Right.Return) and (NewTreeNode.Right.Return.TypeDefinition=ttdCEXPRESSION) then begin
   NewTreeNode.Right.Return:=NewTreeNode.Left.Return;
  end;
  OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
  OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
  OptimizerHighLevel.OptimizeTree(NewTreeNode);
 end;
 result:=NewTreeNode;
end; 

function TParser.ParseExpression(AllowAssignment:boolean):TTreeNode;
var NewTreeNode,LastTreeNode:TTreeNode;
begin
 NewTreeNode:=ParseBooleanExpression;
 OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
 OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
 OptimizerHighLevel.OptimizeTree(NewTreeNode);
 case Scanner.CurrentToken of
  tstDoublePeriod:begin
   Scanner.Match(tstDoublePeriod);
   LastTreeNode:=FinalizePropertyNodes(ParseBooleanExpression,nil,false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(LastTreeNode);
   if (NewTreeNode.TreeNodeType in [ttntORDConst,ttntCharConst]) and
      (NewTreeNode.TreeNodeType=LastTreeNode.TreeNodeType) then begin
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntSubRange,NewTreeNode,LastTreeNode);
   end else begin
    NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntFloat,NewTreeNode,LastTreeNode);
   end;
   if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.Return.TypeDefinition=ttdCEXPRESSION) then begin
    NewTreeNode.Left.Return:=NewTreeNode.Right.Return;
   end else if assigned(NewTreeNode.Right.Return) and (NewTreeNode.Right.Return.TypeDefinition=ttdCEXPRESSION) then begin
    NewTreeNode.Right.Return:=NewTreeNode.Left.Return;
   end;
  end;
  tstAssign:begin
   Scanner.Match(tstAssign);
   if AllowAssignment then begin
    LastTreeNode:=FinalizePropertyNodes(ParseExpression(false),nil,false);
    if HasPropertyNode(NewTreeNode) then begin
     NewTreeNode:=FinalizePropertyNodes(NewTreeNode,LastTreeNode,true);
    end else begin                                                  
     NewTreeNode:=TreeManager.GenerateLeftRightNode(ttntAssign,FinalizePropertyNodes(NewTreeNode,nil,false),LastTreeNode);
    end;
    if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.Return.TypeDefinition=ttdCEXPRESSION) then begin
     NewTreeNode.Left.Return:=NewTreeNode.Right.Return;
    end else if assigned(NewTreeNode.Right.Return) and (NewTreeNode.Right.Return.TypeDefinition=ttdCEXPRESSION) then begin
     NewTreeNode.Right.Return:=NewTreeNode.Left.Return;
    end;
    if assigned(NewTreeNode.Left.Return) and (NewTreeNode.Left.TreeNodeType=ttntVAR) and assigned(NewTreeNode.Left.Symbol) and (NewTreeNode.Left.Symbol.SymbolType=Symbols.tstVariable) and NewTreeNode.Left.Symbol.TypedTrueConstant and (NewTreeNode.Left.Symbol.TypedConstantReadOnly or not LocalSwitches^.WriteableConst) then begin
     Error.AddErrorCode(65);
    end else begin
     OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
     OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
     OptimizerHighLevel.OptimizeTree(NewTreeNode);
    end;
   end else begin
    Error.AddErrorCode(65);
   end;
  end;
 end;
 if not assigned(NewTreeNode) then begin
  NewTreeNode:=TreeManager.GenerateEmptyNode;
 end;
 result:=FinalizePropertyNodes(NewTreeNode,nil,false);
end;

function TParser.ParseFORStatement:TTreeNode;
var FromValue,ToValue,FromVar,ToVar,Block:TTreeNode;
    IsDownTo,First:boolean;
    Name:ansistring;
    ToSymbol,CounterSymbol:PSymbol;
    SrcType,SrcTypeEx,CastType:PType;
    Size,SizeEx:longint;
begin
 Scanner.Match(tstFOR);
 if Error.DoAbort then begin
  Error.AbortCode(37);
  result:=nil;
  exit;
 end;
 if Scanner.CurrentToken=tstIdentifier then begin
  Name:=Scanner.ReadIdentifier(nil);
  ToSymbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
  if not assigned(ToSymbol) then begin
   Error.AbortCode(37);
   result:=nil;
   exit;
  end;
 end else begin
  Scanner.Match(tstIdentifier);
  result:=nil;
  exit;
 end;
 if ToSymbol^.SymbolType<>Symbols.tstVariable then begin
  Error.AbortCode(37);
  result:=nil;
  exit;
 end;
 if not SymbolManager.CurrentList.ContainsSymbol(ToSymbol) then begin
  Error.AbortCode(32);
  result:=nil;
  exit;
 end;
 if tsaFORControlVariable in ToSymbol^.Attributes then begin
  Error.AbortCode(84,CorrectSymbolName(ToSymbol^.Name));
  result:=nil;
  exit;
 end;
 case Scanner.CurrentToken of
  tstIN:begin
   Scanner.Match(tstIN);
   ToVar:=TreeManager.GenerateVarNode(ToSymbol);
   FromVar:=FinalizePropertyNodes(ParseFactor,nil,false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(ToVar);
   OptimizerHighLevel.OptimizeTree(FromVar);
   if Error.DoAbort then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   if not (FromVar.TreeNodeType in [ttntVAR,ttntINDEX,ttntTYPECONV]) then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   Scanner.Match(tstDO);
   if Error.DoAbort then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   if Scanner.CurrentToken<>tstSeparator then begin
    ToSymbol^.Attributes:=ToSymbol^.Attributes+[tsaFORControlVariable];
    Block:=ParseStatement(false);
    ToSymbol^.Attributes:=ToSymbol^.Attributes-[tsaFORControlVariable];
   end else begin
    Block:=nil;
   end;
   SrcType:=FromVar.Return;
   if not assigned(SrcType) then begin
    Error.AbortCode(7);
    result:=nil;
    exit;
   end;
   if SrcType^.TypeDefinition in [ttdArray,ttdShortString] then begin
    First:=true;
    SrcTypeEx:=SrcType;
    Size:=0;
    while assigned(SrcTypeEx) and assigned(SrcTypeEx^.Definition) and
          (SrcTypeEx^.TypeDefinition=ttdArray) do begin
     if not assigned(SrcTypeEx^.Range) then begin
      Error.InternalError(200605232347000);
      result:=nil;
      exit;
     end;
     if SrcTypeEx^.Range^.TypeDefinition=ttdSubRange then begin
      SizeEx:=(SrcTypeEx^.Range^.UpperLimit-SrcTypeEx^.Range^.LowerLimit)+1;
      if First then begin
       Size:=SizeEx;
       First:=false;
      end else begin
       Size:=Size*SizeEx;
      end;
     end else begin
      Error.InternalError(200605232348000);
      result:=nil;
      exit;
     end;
     SrcTypeEx:=SrcTypeEx^.Definition;
    end;
    if not AreTypesCompatible(Error,SymbolManager,SrcTypeEx,ToVar.Return) then begin
     Error.AbortCode(7);
     result:=nil;
     exit;
    end;
    if Size<=0 then begin
     Error.InternalError(200605240138000);
     result:=nil;
     exit;
    end;
    if SrcType.OpenArray or SrcType.DynamicArray then begin
     CastType:=SymbolManager.NewType(ModuleSymbol);
     CastType^:=SrcType^;
     CastType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    end else begin
     CastType:=SymbolManager.NewType(ModuleSymbol);
     CastType^.TypeDefinition:=ttdArray;
     CastType^.Range:=SymbolManager.TypeLongword;
     CastType^.Definition:=SrcTypeEx;
     CastType^.OpenArray:=true;
     CastType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
     CastType^.TypeKind:=TypeKindArray;
     CastType^.NeedTypeInfo:=true;
    end;
{   CastType^.Range^.TypeDefinition:=ttdSubRange;
    CastType^.Range^.SubRangeType:=tstSigned32Bit;
    CastType^.Range^.LowerLimit:=0;
    if SrcType.OpenArray OR SrcType.DynamicArray then begin
     CastType^.Range^.UpperLimit:=$ffffffff;
    end elee begin
     CastType^.Range^.UpperLimit:=Size-1;
    end;}
    CounterSymbol:=SymbolManager.NewSymbol(ModuleSymbol);
    CounterSymbol^.Name:='FOREACH_CONTROL_'+INTTOSTR(ForEachVarCounter);
    inc(ForEachVarCounter);
    CounterSymbol^.OverloadedName:=ModuleName+tpsOverload+CounterSymbol^.Name;
    CounterSymbol^.SymbolType:=Symbols.tstVariable;
    CounterSymbol^.TypeDefinition:=SymbolManager.TypeLongint;
    HashSymbol(CounterSymbol);
    if SrcType.OpenArray or SrcType.DynamicArray then begin
     FromValue:=TreeManager.GenerateOrdConstNode(0,CounterSymbol^.TypeDefinition);
     ToValue:=TreeManager.GenerateOrdConstNode(MAXINT,CounterSymbol^.TypeDefinition);
    end else begin
     FromValue:=TreeManager.GenerateOrdConstNode(0,CounterSymbol^.TypeDefinition);
     ToValue:=TreeManager.GenerateOrdConstNode(Size-1,CounterSymbol^.TypeDefinition);
    end;
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    FromVar:=TreeManager.GenerateLeftNode(ttntAddress,FromVar);
    FromVar:=TreeManager.GenerateTypeConvNode(FromVar,SymbolManager.TypePointer,true);
    FromVar:=TreeManager.GeneratePointerNode(FromVar,CastType);
    FromVar:=TreeManager.GenerateTypeConvNode(FromVar,CastType,true);
    FromVar:=TreeManager.GenerateLeftRightNode(ttntIndex,FromVar,TreeManager.GenerateVarNode(CounterSymbol));
    FromVar:=TreeManager.GenerateTypeConvNode(FromVar,SrcTypeEx,true);
    OptimizerHighLevel.OptimizeTree(FromVar);
    Block:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,Block);
    Block:=TreeManager.GenerateLeftRightNode(ttntStatement,Block,TreeManager.GenerateLeftRightNode(ttntAssign,ToVar,FromVar));
    Block:=TreeManager.GenerateLeftNode(ttntBlock,Block);
    result:=TreeManager.GenerateForNode(TreeManager.GenerateLeftRightNode(ttntAssign,TreeManager.GenerateVarNode(CounterSymbol),FromValue),ToValue,Block,false);
   end else if SrcType^.TypeDefinition in [ttdLongString] then begin
    result:=nil;
   end else if SrcType^.TypeDefinition in [ttdSet] then begin
    result:=nil;
   end else begin
    Error.AbortCode(7);
    result:=nil;
    exit;
{   if not AreTypesCompatible(Error,SymbolManager,SrcType,ToValue.Return) then begin
     Error.AbortCode(7);
     result:=nil;
     exit;
    end;
    result:=nil;}
   end;
  end;
  tstAssign:begin
   Scanner.Match(tstAssign);
   if not assigned(ToSymbol^.TypeDefinition) then begin
    Error.AbortCode(33);
    result:=nil;
    exit;
   end;
   if (ToSymbol^.TypeDefinition.TypeDefinition<>ttdSubRange) or not
      (ToSymbol^.TypeDefinition.SubRangeType in [tstSigned8Bit,tstSigned16Bit,
                                                 tstSigned32Bit,tstSigned64Bit,
                                                 tstUnsigned8Bit,tstUnsigned16Bit,
                                                 tstUnsigned32Bit,tstUnsigned64Bit]) then begin
    Error.AbortCode(33);
    result:=nil;
    exit;
   end;
   FromValue:=TreeManager.GenerateLeftRightNode(ttntAssign,TreeManager.GenerateVarNode(ToSymbol),FinalizePropertyNodes(ParseExpression(false),nil,false));
   IsDownTo:=Scanner.CurrentToken=tstDownto;
   if IsDownTo then begin
    Scanner.Match(tstDOWNTO);
   end else begin
    Scanner.Match(tstTO);
   end;
   if Error.DoAbort then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   ToValue:=FinalizePropertyNodes(ParseExpression(false),nil,false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(ToValue);
   if Error.DoAbort then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   Scanner.Match(tstDO);
   if Error.DoAbort then begin
    Error.AbortCode(37);
    result:=nil;
    exit;
   end;
   if Scanner.CurrentToken<>tstSeparator then begin
    ToSymbol^.Attributes:=ToSymbol^.Attributes+[tsaFORControlVariable];
    Block:=ParseStatement(false);
    ToSymbol^.Attributes:=ToSymbol^.Attributes-[tsaFORControlVariable];
   end else begin
    Block:=nil;
   end;
   result:=TreeManager.GenerateForNode(FromValue,ToValue,Block,IsDownTo);
  end;
  else begin
   Scanner.Match(tstAssign);
   result:=nil;
  end;
 end;
end;

function TParser.ParseWHILEStatement:TTreeNode;
var Block,BooleanExpression:TTreeNode;
begin
 Scanner.Match(tstWHILE);
 BooleanExpression:=ParseExpression(false);
 Scanner.Match(tstDO);
 if Scanner.CurrentToken<>tstSeparator then begin
  Block:=ParseStatement(false);
 end else begin
  Block:=nil;
 end;
 result:=TreeManager.GenerateWhileNode(BooleanExpression,Block);
end;

function TParser.ParseREPEATStatement:TTreeNode;
var NewStatementTree,LastTreeNode,NewTreeNode,Block,BooleanExpression:TTreeNode;
begin
 LastTreeNode:=nil;
 NewTreeNode:=nil;
 Scanner.Match(tstREPEAT);
 while (Scanner.CurrentToken<>tstUNTIL) and not Scanner.IsEOFOrAbortError do begin
  NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
  if assigned(LastTreeNode) then begin
   NewTreeNode.Left:=NewStatementTree;
   NewTreeNode:=NewTreeNode.Left;
  end else begin
   NewTreeNode:=NewStatementTree;
   LastTreeNode:=NewTreeNode;
  end;
  if Scanner.CurrentToken=tstUNTIL then begin
   break;
  end;
  if Scanner.CurrentToken=tstSeparator then begin
   Scanner.Match(tstSeparator);
  end;
 end;
 Block:=TreeManager.GenerateLeftNode(ttntBlock,LastTreeNode);
 Scanner.Match(tstUNTIL);
 BooleanExpression:=ParseExpression(false);
 result:=TreeManager.GenerateRepeatNode(BooleanExpression,Block);
end;

function TParser.ParseTRYStatement:TTreeNode;
var OnElseTree,NewStatementTree,LastTreeNode,NewTreeNode,Block,ExceptTree,FinallyTree:TTreeNode;
    AType:PType;
    Symbol:PSymbol;
    SymbolName:ansistring;
    ExceptionSymbolList:TSymbolList;
    OldIsInExceptionHandler:boolean;
begin
 OldIsInExceptionHandler:=IsInExceptionHandler;
 IsInExceptionHandler:=false;
 LastTreeNode:=nil;
 NewTreeNode:=nil;
 ExceptTree:=nil;
 FinallyTree:=nil;
 Scanner.Match(tstTRY);
 while not ((Scanner.CurrentToken in [tstEXCEPT,tstFINALLY]) or Scanner.IsEOFOrAbortError) do begin
  NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
  if assigned(LastTreeNode) then begin
   NewTreeNode.Left:=NewStatementTree;
   NewTreeNode:=NewTreeNode.Left;
  end else begin
   NewTreeNode:=NewStatementTree;
   LastTreeNode:=NewTreeNode;
  end;
  if Scanner.CurrentToken in [tstEXCEPT,tstFINALLY] then begin
   break;
  end;
  if Scanner.CurrentToken=tstSeparator then begin
   Scanner.Match(tstSeparator);
  end;
 end;
 Block:=TreeManager.GenerateLeftNode(ttntBlock,LastTreeNode);
 IsInExceptionHandler:=true;
 case Scanner.CurrentToken of
  tstEXCEPT:begin
   Scanner.CheckForDirectives([tstON]);
   Scanner.Match(tstEXCEPT);
   OnElseTree:=nil;
   while not Scanner.IsEOFOrAbortError do begin
    Scanner.CheckForDirectives([tstON]);
    case Scanner.CurrentToken of
     tstON:begin
      Scanner.Match(tstON);
      if Scanner.CurrentToken=tstIdentifier then begin
       AType:=nil;
       SymbolName:='';
       Symbol:=SymbolManager.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
       if (assigned(Symbol) and (Symbol^.SymbolType<>Symbols.tstType)) or not assigned(Symbol) then begin
        SymbolName:=Scanner.CurrentIdentifier;
        Scanner.Match(tstIdentifier);
        Scanner.Match(tstCOLON);
       end;
       Symbol:=SymbolManager.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
       if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
        Scanner.Match(tstIdentifier);
        Scanner.Match(tstPeriod);
        Symbol:=Symbol^.SymbolList.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
       end;
       if (assigned(Symbol) and (Symbol^.SymbolType<>Symbols.tstType)) or not assigned(Symbol) then begin
        Error.AbortCode(115);
       end else begin
        Scanner.Match(tstIdentifier);
        if assigned(Symbol) then begin
         AType:=Symbol^.TypeDefinition;
        end;
       end;
      end else begin
       Scanner.Match(tstIdentifier);
      end;
      Scanner.Match(tstDO);
      if length(SymbolName)>0 then begin
       ExceptionSymbolList:=TSymbolList.Create(SymbolManager);
       Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
       Symbol^.Name:=tpsIdentifier+SymbolName;
       Symbol^.SymbolType:=Symbols.tstVariable;
       Symbol^.Attributes:=[tsaPublic,tsaTemporaryExceptionVariable];
       Symbol^.TypeDefinition:=AType;
       ExceptionSymbolList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
       SymbolManager.PushSymbolList(ExceptionSymbolList);
       NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
       SymbolManager.PopSymbolList(ExceptionSymbolList);
       ExceptionSymbolList.RemoveSymbol(Symbol);
       FreeAndNil(ExceptionSymbolList);
      end else begin
       Symbol:=nil;
       NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
      end;
      NewStatementTree:=TreeManager.GenerateTryOnElseNode(Symbol,AType,NewStatementTree,nil);
      if assigned(OnElseTree) then begin
       OnElseTree.Right:=NewStatementTree;
      end;
      OnElseTree:=NewStatementTree;
     end;
     tstELSE:begin
      Scanner.Match(tstELSE);
      break;
     end;
     else begin
      break;
     end;
    end;
   end;
   LastTreeNode:=nil;
   NewTreeNode:=nil;
   while not ((Scanner.CurrentToken=tstEND) or Scanner.IsEOFOrAbortError) do begin
    NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
    if assigned(LastTreeNode) then begin
     NewTreeNode.Left:=NewStatementTree;
     NewTreeNode:=NewTreeNode.Left;
    end else begin
     NewTreeNode:=NewStatementTree;
     LastTreeNode:=NewTreeNode;
    end;
    if Scanner.CurrentToken=tstEND then begin
     break;
    end;
    if Scanner.CurrentToken=tstSeparator then begin
     Scanner.Match(tstSeparator);
    end;
   end;
   ExceptTree:=TreeManager.GenerateLeftNode(ttntBlock,LastTreeNode);
   if assigned(OnElseTree) then begin
    OnElseTree.Right:=ExceptTree;
    ExceptTree:=OnElseTree;
   end;
  end;
  tstFINALLY:begin
   Scanner.Match(tstFINALLY);
   LastTreeNode:=nil;
   NewTreeNode:=nil;
   while not ((Scanner.CurrentToken=tstEND) or Scanner.IsEOFOrAbortError) do begin
    NewStatementTree:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
    if assigned(LastTreeNode) then begin
     NewTreeNode.Left:=NewStatementTree;
     NewTreeNode:=NewTreeNode.Left;
    end else begin
     NewTreeNode:=NewStatementTree;
     LastTreeNode:=NewTreeNode;
    end;
    if Scanner.CurrentToken=tstEND then begin
     break;
    end;
    if Scanner.CurrentToken=tstSeparator then begin
     Scanner.Match(tstSeparator);
    end;
   end;
   FinallyTree:=TreeManager.GenerateLeftNode(ttntBlock,LastTreeNode);
  end;
  else begin
   Scanner.Match(tstFINALLY);
  end;
 end;
 Scanner.Match(tstEND);
 result:=TreeManager.GenerateTryNode(Block,ExceptTree,FinallyTree);
 IsInExceptionHandler:=OldIsInExceptionHandler;
end;

function TParser.ParseIFStatement:TTreeNode;
var BooleanExpression,Block,ElseBlock:TTreeNode;
begin
 Scanner.Match(tstIF);
 BooleanExpression:=ParseExpression(false);
 Scanner.Match(tstTHEN);
 if Scanner.CurrentToken=tstELSE then begin
  Block:=nil;
 end else begin
  Block:=ParseStatement(false);
 end;
 if Scanner.CurrentToken=tstELSE then begin
  Scanner.Match(tstELSE);
  ElseBlock:=ParseStatement(false);
 end else begin
  ElseBlock:=nil;
 end;
 result:=TreeManager.GenerateIfNode(BooleanExpression,Block,ElseBlock);
end;

function TParser.ParseBREAKStatement:TTreeNode;
begin
 Scanner.Match(tstBREAK);
 result:=TreeManager.GenerateBreakNode;
end;

function TParser.ParseCONTINUEStatement:TTreeNode;
begin
 Scanner.Match(tstCONTINUE);
 result:=TreeManager.GenerateContinueNode;
end;

function TParser.ParseEXITStatement:TTreeNode;
begin
 Scanner.Match(tstEXIT);
 result:=TreeManager.GenerateExitNode;
end;

function TParser.ParseHALTStatement:TTreeNode;
begin
 Scanner.Match(tstHALT);
 result:=TreeManager.GenerateHaltNode;
 if Scanner.CurrentToken=tstLeftParen then begin
  Scanner.Match(tstLeftParen);
  result.Left:=ParseExpression(false);
  Scanner.Match(tstRightParen);
 end else begin
  result.Left:=nil;
 end;
end;

function TParser.ParseFAILStatement:TTreeNode;
begin
 if assigned(CurrentProcedureFunction) and not (tpaCONSTRUCTOR in CurrentProcedureFunction^.ProcedureAttributes) then begin 
  Error.AddErrorCode(81);
 end;
 Scanner.Match(tstFAIL);
 result:=TreeManager.GenerateFAILNode;
end;

function TParser.ParseRAISEStatement:TTreeNode;
var Left,Right:TTreeNode;
    AType:PType;
    NewName:ansistring;
    Symbol,ASymbol,NewSymbol:PSymbol;
begin
 Scanner.Match(tstRAISE);
 if Scanner.CurrentToken=tstSeparator then begin
  if not IsInExceptionHandler then begin
   Error.AddErrorCode(150);
  end;
  result:=TreeManager.GenerateRaiseNode(nil,nil);
 end else begin
  Left:=ParseExpression(false);
  if (not assigned(Left)) or (Left.TreeNodeType<>ttntCall) then begin
   Error.AbortCode(20);
  end else if not assigned(Left.MethodSymbol) then begin
   Error.AbortCode(20);
//end else if (not assigned(Left.Symbol)) or (Left.Symbol^.SymbolType<>Symbols.tstVariable) or (Left.Symbol^.TypeDefinition.TypeDefinition<>ttdCLASS) then begin
  end else if (not assigned(Left.Symbol)) or (Left.Symbol^.SymbolType<>Symbols.tstType) then begin
   Error.AbortCode(20);
  end else begin
   Symbol:=Left.Symbol;
   ASymbol:=Symbol;
   AType:=Symbol^.TypeDefinition;
   if AType^.TypeDefinition=ttdClass then begin
    while assigned(AType^.ChildOf) do begin
     ASymbol:=AType^.ChildOf;
     AType:=AType^.ChildOf^.TypeDefinition;
     if AType^.TypeDefinition<>ttdClass then begin
      Error.AbortCode(20);
      break;
     end;
    end;
    if assigned(ASymbol) then begin
     NewName:=tpsIdentifier+'SYSUTILS';
     NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
     if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
      NewName:=tpsIdentifier+'EXCEPTION';
      NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
     end;
     if not assigned(NewSymbol) then begin
      NewName:=tpsIdentifier+'SYSTEM';
      NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
      if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
       NewName:=tpsIdentifier+'EXCEPTION';
       NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
      end;
     end;
     if not assigned(NewSymbol) then begin
      Error.AbortCode(20);
     end else if (NewSymbol^.SymbolType<>Symbols.tstType) or (NewSymbol^.TypeDefinition^.TypeDefinition<>ttdClass) then begin
      Error.AbortCode(20);
     end else begin
{     if AType<>NewSymbol^.TypeDefinition then begin
       Error.AbortCode(9,CorrectSymbolName(ASymbol^.Name),CorrectSymbolName(NewSymbol^.Name));
      end;}
     end;
    end else begin
     Error.AbortCode(20);
    end;
   end else begin
    Error.AbortCode(20);
   end;
  end;
  Scanner.CheckForDirectives([tstAT_]);
  if Scanner.CurrentToken=tstAT_ then begin
   Scanner.Match(tstAT_);
   Right:=ParseExpression(false);
   if (not assigned(Right)) or (Right.TreeNodeType<>ttntAddress) then begin
    Error.AbortCode(113); //527
   end;
  end else begin
   Right:=nil;
  end;
  result:=TreeManager.GenerateRaiseNode(Left,Right);
 end;
end;

function TParser.ParseGOTOStatement:TTreeNode;
var Symbol:PSymbol;
    Name:ansistring;
begin
 result:=nil;
 Scanner.Match(tstGOTO);
 Error.Push;
 if Scanner.MaybeLabel(Scanner.CurrentToken) then begin
  Name:=Scanner.ReadLabel;
  if length(Scanner.ProcedureName)=0 then begin
   Symbol:=SymbolManager.GetSymbol(Name,ModuleSymbol,CurrentObjectClass);
  end else begin
   Symbol:=SymbolManager.GetSymbol(Scanner.ProcedureName+Name,ModuleSymbol,CurrentObjectClass);
  end;
  if assigned(Symbol) then begin
   result:=TreeManager.GenerateGotoNode(Symbol);
  end else begin
   Error.AbortCode(98,CorrectSymbolName(Name));
  end;
  Scanner.Match(Scanner.CurrentToken);
 end else begin
  Error.AbortCode(31);
 end;
 Error.Pop;
end;

function TParser.ParseCASEStatement:TTreeNode;
var CaseExpression,CaseValueExpression,CaseBlock,CaseElse:TTreeNode;
begin
 Scanner.Match(tstCASE);
 CaseExpression:=ParseExpression(false);
 OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
 OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
 OptimizerHighLevel.OptimizeTree(CaseExpression);
 Scanner.Match(tstOF);
 CaseBlock:=nil;
 CaseElse:=nil;
 repeat
  CaseValueExpression:=nil;
  while not Scanner.IsEOFOrAbortError do begin
   CaseValueExpression:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(CaseValueExpression);
   if Scanner.CurrentToken=tstComma then begin
    Scanner.Match(tstComma);
   end else begin
    break;
   end;
   CaseBlock:=TreeManager.GenerateCaseBlockNode(CaseBlock,CaseValueExpression,nil);
  end;
  Scanner.Match(tstColon);
  CaseBlock:=TreeManager.GenerateCaseBlockNode(CaseBlock,CaseValueExpression,ParseStatement(false));
  if not (Scanner.CurrentToken in [tstELSE,tstEND]) then begin
   Scanner.Match(tstSeparator);
  end;
 until (Scanner.CurrentToken in [tstELSE,tstEND]) or Scanner.IsEOFOrAbortError;
 if Scanner.CurrentToken=tstELSE then begin
  Scanner.Match(tstELSE);
  CaseElse:=ParseStatement(false);
  if Scanner.CurrentToken=tstSeparator then begin
   Scanner.Match(tstSeparator);
  end;
 end;
 Scanner.Match(tstEnd);
 result:=TreeManager.GenerateCaseNode(CaseExpression,CaseBlock,CaseElse);
end;

function TParser.ParseWITHStatement:TTreeNode;
var Block,NewTreeNode:TTreeNode;
    WithTable:TSymbolList;
begin
 Scanner.Match(tstWITH);
 NewTreeNode:=ParseExpression(false);
 OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
 OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
 OptimizerHighLevel.OptimizeTree(NewTreeNode);
 if NewTreeNode.Return^.TypeDefinition in [ttdRECORD,ttdCLASS,ttdOBJECT] then begin
  WithStack.Add(NewTreeNode.Return);
  try
   WithTable:=NewTreeNode.Return^.RecordTable;
   SymbolManager.PushSymbolList(WithTable,WithStack.Count-1);
   Scanner.Match(tstDO);
   if Scanner.CurrentToken<>tstSeparator then begin
    Block:=ParseStatement(false);
   end else begin
    Block:=nil;
   end;
   SymbolManager.PopSymbolList(WithTable);
   result:=TreeManager.GenerateWithNode(NewTreeNode,Block);
  finally
   WithStack.Delete(WithStack.Count-1);
  end;
 end else begin
  result:=nil;
  Error.AbortCode(17);
 end;
end;

procedure TParser.ParseProgram(WithProgramToken:boolean);
var Symbol:PSymbol;
    CodeTree:TTreeNode;
    OldList:TSymbolList;
    OldIsSystemUnit:boolean;
    CodeFileStream,HeaderFileStream:TBeRoFileStream;
begin
 OldIsSystemUnit:=IsSystemUnit;
 IsSystemUnit:=false;
 if WithProgramToken then begin
  Scanner.Match(tstPROGRAM);
  if Scanner.CurrentToken=tstIdentifier then begin
   ModuleName:=Scanner.ReadIdentifier(@OriginalModuleName);
   if Scanner.CurrentToken=tstLeftParen then begin
    Scanner.Match(tstLeftParen);
    if Scanner.CurrentToken=tstIdentifier then begin
     Scanner.Match(tstIdentifier);
     if Scanner.CurrentToken=tstComma then begin
      while (Scanner.CurrentToken<>tstRightParen) and not Scanner.IsEOFOrAbortError do begin
       Scanner.Match(tstComma);
       Scanner.Match(tstIdentifier);
      end;
     end else if Scanner.CurrentToken<>tstRightParen then begin
      Scanner.Match(tstComma);
     end;
    end;
    Scanner.Match(tstRightParen);
   end;
   Scanner.Match(tstSeparator);
  end else begin
   Scanner.Match(tstIdentifier);
  end;
 end else begin
  ModuleName:='AProgram';
 end;
 Symbol:=SymbolManager.NewSymbol(nil,nil,MakeSymbolsPublic);
 Symbol^.SymbolType:=Symbols.tstUNIT;
 Symbol^.UnitKind:=tukPROGRAM;
 Symbol^.Name:=ModuleName;
 Symbol^.OriginalCaseName:=OriginalModuleName;
 Symbol^.OriginalName:=ChangeFileExt(ExtractFileName(FileName),'');
 Symbol^.OriginalFileName:=FileName;
 Symbol^.SymbolPointerList:=TPointerList.Create;
 Symbol^.SymbolPointerList.Add(Symbol);
 Symbol^.TypePointerList:=TPointerList.Create;
 Symbol^.SymbolTableStream:=TBeRoStream.Create;
 Symbol^.OwnerModule:=Symbol;
 Symbol^.Compiler:=ACompiler;
 Symbol^.Parser:=self;
 ModuleSymbol:=Symbol;
 Scanner.PreprocessorInstance.ModuleSymbol:=ModuleSymbol;
 OldList:=SymbolManager.CurrentList;
 Symbol^.SymbolList:=TSymbolList.Create(SymbolManager);
 SymbolManager.CurrentList:=Symbol^.SymbolList;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 SymbolManager.PushSymbolList(Symbol^.SymbolList);
 MakeSymbolsPublic:=true;
 if not Scanner.IsEOFOrAbortError then begin
  UnitManager.Load('SYSTEM','',false,true,ModuleSymbol,UnitLevel);
  Error.LocalSwitches:=LocalSwitches;
  GetDefaultTypes;
  if Scanner.CurrentToken=tstUSES then begin
   ParseUSESStatement(true,true,false);
  end;
  MakeSymbolsPublic:=true;
  ParseHeadBlock(false,true);
  FinishCheckMethods(Symbol);
  CodeTree:=ParseMainBlock;
  FinishCheckSymbols(Symbol,Symbol^.SymbolList);
  if assigned(CodeTree) then begin
   if not Error.Errors then begin
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(CodeTree);
    TreeManager.Dump(CodeTree,CorrectSymbolName(ModuleSymbol^.Name));
    CodeGenerator.GenerateProgram(ModuleSymbol,CodeTree);
    CodeFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.c'));
    try
     HeaderFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.h'));
     try
      CodeGenerator.SaveToStreams(CodeFileStream,HeaderFileStream);
     finally
      HeaderFileStream.Free;
     end;
    finally
     CodeFileStream.Free;
    end;
   end;
   TreeManager.Clear;
  end;
 end;
 IsSystemUnit:=OldIsSystemUnit;
 SymbolManager.CurrentList:=OldList;
end;

procedure TParser.ParsePackage;
var Symbol:PSymbol;
    CodeTree:TTreeNode;
    OldList:TSymbolList;
    OldIsSystemUnit:boolean;
    CodeFileStream,HeaderFileStream:TBeRoFileStream;
begin
 OldIsSystemUnit:=IsSystemUnit;
 IsSystemUnit:=false;
 Scanner.Match(tstPACKAGE);
 if Scanner.CurrentToken=tstIdentifier then begin
  ModuleName:=Scanner.ReadIdentifier(@OriginalModuleName);
  Scanner.Match(tstSeparator);
 end else begin
  Scanner.Match(tstIdentifier);
 end;
 Symbol:=SymbolManager.NewSymbol(nil,nil,MakeSymbolsPublic);
 Symbol^.SymbolType:=Symbols.tstUNIT;
 Symbol^.UnitKind:=tukPACKAGE;
 Symbol^.Name:=ModuleName;
 Symbol^.OriginalCaseName:=OriginalModuleName;
 Symbol^.OriginalName:=ChangeFileExt(ExtractFileName(FileName),'');
 Symbol^.OriginalFileName:=FileName;
 Symbol^.SymbolPointerList:=TPointerList.Create;
 Symbol^.SymbolPointerList.Add(Symbol);
 Symbol^.TypePointerList:=TPointerList.Create;
 Symbol^.SymbolTableStream:=TBeRoStream.Create;
 Symbol^.OwnerModule:=Symbol;
 Symbol^.Compiler:=ACompiler;
 Symbol^.Parser:=self;
 ModuleSymbol:=Symbol;
 Scanner.PreprocessorInstance.ModuleSymbol:=ModuleSymbol;
 OldList:=SymbolManager.CurrentList;
 Symbol^.SymbolList:=TSymbolList.Create(SymbolManager);
 SymbolManager.CurrentList:=Symbol^.SymbolList;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 SymbolManager.PushSymbolList(Symbol^.SymbolList);
 MakeSymbolsPublic:=true;
 if not Scanner.IsEOFOrAbortError then begin
  UnitManager.Load('SYSTEM','',false,true,ModuleSymbol,UnitLevel);
  Error.LocalSwitches:=LocalSwitches;
  GetDefaultTypes;
  if Scanner.CurrentToken=tstREQUIRES then begin
   Scanner.Match(tstREQUIRES);
   if DoBreak then begin
    exit;
   end;
   while not Scanner.IsEOFOrAbortError do begin
    if Scanner.CurrentToken=tstIdentifier then begin
     // TODO: Complete this!
     Scanner.Match(tstIdentifier);
     if Error.DoAbort then begin
      break;
     end;
     if DoBreak then begin
      exit;
     end;
    end else begin
     Scanner.Match(tstIdentifier);
    end;
    if Scanner.CurrentToken=tstComma then begin
     Scanner.Match(tstComma);
    end else begin
     break;
    end;
   end;
   if DoBreak then begin
    exit;
   end;
   Scanner.Match(tstSeparator);
  end;
  if Scanner.CurrentToken=tstCONTAINS then begin
   ParseUSESStatement(true,true,true);
  end;
  Scanner.Match(tstEND);
  FinishCheckMethods(Symbol);
  FinishCheckSymbols(Symbol,Symbol^.SymbolList);
  if assigned(CodeTree) then begin
   if not Error.Errors then begin
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(CodeTree);
    TreeManager.Dump(CodeTree,CorrectSymbolName(ModuleSymbol^.Name));
    CodeGenerator.GeneratePackage(ModuleSymbol);
    CodeFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.c'));
    try
     HeaderFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.h'));
     try
      CodeGenerator.SaveToStreams(CodeFileStream,HeaderFileStream);
     finally
      HeaderFileStream.Free;
     end;
    finally
     CodeFileStream.Free;
    end;
   end;
   TreeManager.Clear;
  end;
 end;
 IsSystemUnit:=OldIsSystemUnit;
 SymbolManager.CurrentList:=OldList;
end;

procedure TParser.ParseLibrary;
var Symbol:PSymbol;
    CodeTree:TTreeNode;
    OldList:TSymbolList;
    OldIsSystemUnit:boolean;
    CodeFileStream,HeaderFileStream:TBeRoFileStream;
begin
 OldIsSystemUnit:=IsSystemUnit;
 IsSystemUnit:=false;
 Scanner.Match(tstLIBRARY);
 if Scanner.CurrentToken=tstIdentifier then begin
  ModuleName:=Scanner.ReadIdentifier(@OriginalModuleName);
  Scanner.Match(tstSeparator);
 end else begin
  Scanner.Match(tstIdentifier);
 end;
 Symbol:=SymbolManager.NewSymbol(nil,nil,MakeSymbolsPublic);
 Symbol^.SymbolType:=Symbols.tstUNIT;
 Symbol^.UnitKind:=tukLIBRARY;
 Symbol^.Name:=ModuleName;
 Symbol^.OriginalCaseName:=OriginalModuleName;
 Symbol^.OriginalName:=ChangeFileExt(ExtractFileName(FileName),'');
 Symbol^.OriginalFileName:=FileName;
 Symbol^.SymbolPointerList:=TPointerList.Create;
 Symbol^.SymbolPointerList.Add(Symbol);
 Symbol^.TypePointerList:=TPointerList.Create;
 Symbol^.SymbolTableStream:=TBeRoStream.Create;
 Symbol^.OwnerModule:=Symbol;
 Symbol^.Compiler:=ACompiler;
 Symbol^.Parser:=self;
 ModuleSymbol:=Symbol;
 Scanner.PreprocessorInstance.ModuleSymbol:=ModuleSymbol;
 OldList:=SymbolManager.CurrentList;
 Symbol^.SymbolList:=TSymbolList.Create(SymbolManager);
 SymbolManager.CurrentList:=Symbol^.SymbolList;
 SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 SymbolManager.PushSymbolList(Symbol^.SymbolList);
 MakeSymbolsPublic:=true;
 if not Scanner.IsEOFOrAbortError then begin
  UnitManager.Load('SYSTEM','',false,true,ModuleSymbol,UnitLevel);
  Error.LocalSwitches:=LocalSwitches;
  GetDefaultTypes;
  if Scanner.CurrentToken=tstUSES then begin
   ParseUSESStatement(true,true,false);
  end;
  ParseHeadBlock(false,true);
  FinishCheckMethods(Symbol);
  if Scanner.CurrentToken=tstBEGIN then begin
   CodeTree:=ParseMainBlock;
  end else begin
   Scanner.Match(tstEND);
  end;
  FinishCheckSymbols(Symbol,Symbol^.SymbolList);
  if assigned(CodeTree) then begin
   if not Error.Errors then begin
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(CodeTree);
    TreeManager.Dump(CodeTree,CorrectSymbolName(ModuleSymbol^.Name));
    CodeGenerator.GenerateLibrary(ModuleSymbol,CodeTree);
    CodeFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.c'));
    try
     HeaderFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.h'));
     try
      CodeGenerator.SaveToStreams(CodeFileStream,HeaderFileStream);
     finally
      HeaderFileStream.Free;
     end;
    finally
     CodeFileStream.Free;
    end;
   end;
   TreeManager.Clear;
  end;
 end;
 IsSystemUnit:=OldIsSystemUnit;
 SymbolManager.CurrentList:=OldList;
end;

procedure TParser.ParseUnit;
var Symbol:PSymbol;
    InitializationCodeTree,FinalizationCodeTree:TTreeNode;
    OldList:TSymbolList;
    OldModuleName:ansistring;
    OldIsSystemUnit:boolean;
    CodeFileStream,HeaderFileStream:TBeRoFileStream;
begin
 Scanner.Match(tstUNIT);
 if Scanner.CurrentToken=tstIdentifier then begin
  DoBreak:=false;

  OldModuleName:=ModuleName;
  OldIsSystemUnit:=IsSystemUnit;

  ModuleName:=Scanner.ReadIdentifier(@OriginalModuleName);

  if CorrectSymbolName(ModuleName)<>UPPERCASE(ChangeFileExt(ExtractFileName(Scanner.FileName),'')) then begin
   Error.AbortCode(89,CorrectSymbolName(ModuleName),UPPERCASE(ExtractFileName(Scanner.FileName)));
   exit;
  end;

  OldList:=SymbolManager.CurrentList;

  Symbol:=SymbolManager.GetSymbol(ModuleName);
  if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
   Symbol^.Parser:=self;
   SymbolManager.CurrentList:=Symbol^.SymbolList;
   ModuleSymbol:=Symbol;
   Scanner.PreprocessorInstance.ModuleSymbol:=ModuleSymbol;
  end else begin
   Symbol:=SymbolManager.NewSymbol(nil,nil,MakeSymbolsPublic);
   Symbol^.SymbolType:=Symbols.tstUNIT;
   Symbol^.UnitKind:=tukUNIT;
   Symbol^.SymbolPointerList:=TPointerList.Create;
   Symbol^.SymbolPointerList.Add(Symbol);
   Symbol^.TypePointerList:=TPointerList.Create;
   Symbol^.SymbolTableStream:=TBeRoStream.Create;
   Symbol^.Name:=ModuleName;
   Symbol^.OriginalCaseName:=OriginalModuleName;
   Symbol^.OriginalName:=ChangeFileExt(ExtractFileName(FileName),'');
   Symbol^.OriginalFileName:=FileName;
   Symbol^.OwnerModule:=Symbol;
   Symbol^.Compiler:=ACompiler;
   Symbol^.Parser:=self;
   HashSymbol(Symbol);
   ModuleSymbol:=Symbol;
   Scanner.PreprocessorInstance.ModuleSymbol:=ModuleSymbol;
   Symbol^.SymbolList:=TSymbolList.Create(SymbolManager);
   SymbolManager.CurrentList:=Symbol^.SymbolList;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
   SymbolManager.PushSymbolList(Symbol^.SymbolList);
  end;

  Symbol^.IsUnitCompiled:=true;
  Symbol^.IsCompileInterface:=true;
  Symbol^.IsInterfaceReady:=false;

  Symbol^.PortabilityDirectives:=ParsePortabilityDirectives;
  
  Scanner.Match(tstSeparator);
  Scanner.Match(tstINTERFACE);
  MakeSymbolsPublic:=true;
  if ModuleName=tpsIdentifier+'SYSTEM' then begin
   AddDefaultSymbols;
   IsSystemUnit:=true;
  end else begin
   UnitManager.Load('SYSTEM','',false,true,ModuleSymbol,UnitLevel);
   Error.LocalSwitches:=LocalSwitches;
   IsSystemUnit:=false;
  end;

  GetDefaultTypes;

  if Scanner.CurrentToken=tstUSES then begin
   ParseUSESStatement(false,false,false);
  end;

  if DoBreak then begin
   ModuleName:=OldModuleName;
   IsSystemUnit:=OldIsSystemUnit;

   SymbolManager.CurrentList:=OldList;
   exit;
  end;

  Error.LocalSwitches:=LocalSwitches;

  ParseHeadBlock(true,true);
  MakeSymbolsPublic:=false;
  Scanner.Match(tstIMPLEMENTATION);

  if IsSystemUnit then begin
   GetDefaultTypes;
  end;

  if not (Error.Errors or Error.DoAbort) then begin

   UnitManager.SaveUnitSymbolTable(Symbol^.SymbolList,Symbol);

   Symbol^.IsCompileInterface:=false;
   Symbol^.IsInterfaceReady:=true;

   if Scanner.CurrentToken=tstUSES then begin
    ParseUSESStatement(true,false,false);
    if DoBreak then begin
     exit;
    end;
   end;

   Error.LocalSwitches:=LocalSwitches;

   ParseHeadBlock(false,true);

   FinishCheckMethods(Symbol);

   InitializationCodeTree:=nil;
   FinalizationCodeTree:=nil;
   if Scanner.CurrentToken=tstBEGIN then begin
    InitializationCodeTree:=ParseMainBlock;
   end else begin
    if Scanner.CurrentToken=tstINITIALIZATION then begin
     Scanner.Match(tstINITIALIZATION);
     InitializationCodeTree:=ParseBlockStatement([tstEND,tstFINALIZATION]);
    end;
    if Scanner.CurrentToken=tstFINALIZATION then begin
     Scanner.Match(tstFINALIZATION);
     FinalizationCodeTree:=ParseBlockStatement([tstEND]);
    end;
    Scanner.Match(tstEND);
   end;

   FinishCheckSymbols(Symbol,Symbol^.SymbolList);

   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;

   if assigned(InitializationCodeTree) then begin
    OptimizerHighLevel.OptimizeTree(InitializationCodeTree);
   end;
   if assigned(FinalizationCodeTree) then begin
    OptimizerHighLevel.OptimizeTree(FinalizationCodeTree);
   end;
   TreeManager.Dump(InitializationCodeTree,CorrectSymbolName(ModuleSymbol^.Name+'_INITIALIZATION'));
   TreeManager.Dump(FinalizationCodeTree,CorrectSymbolName(ModuleSymbol^.Name+'_FINALIZATION'));
   if not (Error.Errors or Error.DoAbort) then begin
    CodeGenerator.GenerateUnit(Symbol,InitializationCodeTree,FinalizationCodeTree);
   end;
   if assigned(InitializationCodeTree) then begin
    InitializationCodeTree.Destroy;
   end;
   if assigned(FinalizationCodeTree) then begin
    FinalizationCodeTree.Destroy;
   end;

   if not (Error.Errors or Error.DoAbort) then begin
    UnitManager.SaveUnitFile(ChangeFileExt(FileName,tfeUnit),Symbol^.SymbolList,Symbol,ModuleName);
    CodeFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.c'));
    try
     HeaderFileStream:=TBeRoFileStream.CreateNew(ChangeFileExt(ExtractFilename(ModuleSymbol^.OriginalFileName),'.h'));
     try
      CodeGenerator.SaveToStreams(CodeFileStream,HeaderFileStream);
     finally
      HeaderFileStream.Free;
     end;
    finally
     CodeFileStream.Free;
    end;
   end;
   
  end;

  ModuleName:=OldModuleName;
  IsSystemUnit:=OldIsSystemUnit;

  SymbolManager.CurrentList:=OldList;
 end else begin
  Scanner.Match(tstIdentifier);
 end;
end;

procedure TParser.ParseUSESStatement(AfterImplementation,AllowIN,IsContains:boolean);
var AUnitName,ASrcPath:ansistring;
begin
 if DoBreak then begin
  exit;
 end;
 if IsContains then begin
  Scanner.Match(tstCONTAINS);
 end else begin
  Scanner.Match(tstUSES);
 end;
 while not Scanner.IsEOFOrAbortError do begin
  if Scanner.CurrentToken=tstIdentifier then begin
   ASrcPath:='';
   AUnitName:=Scanner.CurrentIdentifier;
   Scanner.Match(tstIdentifier);
   if Error.DoAbort then begin
    break;
   end;
   if AllowIN then begin
    if Scanner.CurrentToken=tstIN then begin
     Scanner.Match(tstIN);
     if Scanner.CurrentToken=tstStringValue then begin
      ASrcPath:=HugeStringToWideString(Scanner.CurrentString);
     end;
     Scanner.Match(tstStringValue);
    end;
    if Error.DoAbort then begin
     break;
    end;
   end;
   UnitManager.Load(AUnitName,ASrcPath,AfterImplementation,true,ModuleSymbol,UnitLevel);
   Error.LocalSwitches:=LocalSwitches;
   if DoBreak then begin
    exit;
   end;
   if Error.DoAbort then begin
    break;
   end;
  end else begin
   Scanner.Match(tstIdentifier);
  end;
  if Scanner.CurrentToken=tstComma then begin
   Scanner.Match(tstComma);
  end else begin
   break;
  end;
 end;
 if DoBreak then begin
  exit;
 end;
 Scanner.Match(tstSeparator);
end;

procedure TParser.ParseEXPORTS;
var AIdentifier,AIdentifierEx,AName:ansistring;
    AIndex:longint;
    NewTreeNode:TTreeNode;
    Symbol:PSymbol;
    SymbolExport:PSymbolExport;
begin
 if DoBreak then begin
  exit;
 end;
 Scanner.Match(tstEXPORTS);
 while not Scanner.IsEOFOrAbortError do begin
  if Scanner.CurrentToken=tstIdentifier then begin
   AIdentifier:=Scanner.CurrentIdentifier;
   AIdentifierEx:=Scanner.ReadIdentifier(nil);
   AName:='';
   AIndex:=-1;
   Scanner.Match(tstIdentifier);
   if Error.DoAbort then begin
    break;
   end;
   Symbol:=SymbolManager.GetSymbol(AIdentifierEx,ModuleSymbol,CurrentObjectClass);
   if not (assigned(Symbol) and (Symbol^.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction])) then begin
    Error.AbortCode(126);
   end;
   Scanner.CheckForDirectives([tstNAME,tstINDEX]);
   case Scanner.CurrentToken of
    tstNAME:begin
     Scanner.Match(tstNAME);
     if Scanner.CurrentToken=tstStringValue then begin
      AName:=HugeStringToWideString(Scanner.CurrentString);
     end else begin
      Scanner.Match(tstStringValue);
     end;
    end;
    tstINDEX:begin
     Scanner.Match(tstINDEX);
     if Scanner.CurrentToken=tstStringValue then begin
      NewTreeNode:=ParseExpression(false);
      try
       OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
       OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
       OptimizerHighLevel.OptimizeTree(NewTreeNode);
       if NewTreeNode.TreeNodeType=ttntORDConst then begin
        AIndex:=NewTreeNode.Value;
       end else begin
        Error.AbortCode(526);
       end;
      finally
       NewTreeNode.Free;
      end;
     end else begin
      Scanner.Match(tstStringValue);
     end;
    end;
   end;
   if DoBreak then begin
    exit;
   end;
   if Error.DoAbort then begin
    break;
   end;
   New(SymbolExport);
   FillChar(SymbolExport^,SizeOf(TSymbolExport),#0);
   if assigned(Symbol^.OwnerModule) then begin
    SymbolExport^.ModuleName:=Symbol^.OwnerModule^.Name;
   end else begin
    SymbolExport^.ModuleName:='';
   end;
   SymbolExport^.SymbolName:=Symbol^.Name;
   SymbolExport^.SymbolOverloadedName:=Symbol^.OverloadedName;
   SymbolExport^.Name:=AName;
   SymbolExport^.Index:=AIndex;
   SymbolExport^.Next:=ModuleSymbol^.SymbolExports;
   ModuleSymbol^.SymbolExports:=SymbolExport;
  end else begin
   Scanner.Match(tstIdentifier);
  end;
  if Scanner.CurrentToken=tstComma then begin
   Scanner.Match(tstComma);
  end else begin
   break;
  end;
 end;
 if DoBreak then begin
  exit;
 end;
 Scanner.Match(tstSeparator);
end;

procedure TParser.ParseHeadBlock(ParseHeader,IsGlobal:boolean);
begin
 while not Scanner.IsEOFOrAbortError do begin
  case Scanner.CurrentToken of
   tstCCODE:begin
    Scanner.ReadNext;
    while not Scanner.IsEOFOrAbortError do begin
     case Scanner.CurrentToken of
      tstCBLOCK:begin
       if ParseHeader then begin
        TCodeGenC(CodeGenerator).AddHeader(HugeStringToAnsiString(Scanner.CurrentString));
       end else begin
        TCodeGenC(CodeGenerator).AddCode(HugeStringToAnsiString(Scanner.CurrentString));
       end;
       Scanner.ReadNext;
      end;
      tstCEND:begin
       Scanner.ReadNext;
       break;
      end;
      tstCSKIP:begin
       Scanner.ReadNext;
      end;
      else begin
       Error.InternalError(201304050301000);
       Scanner.ReadNext;
       break;
      end;
     end;
    end;
   end;
   tstRESOURCESTRING:begin
    ParseRESOURCESTRINGDeclartion;
   end;
   tstCONST:begin
    ParseCONSTDeclartion;
   end;
   tstVAR,tstTHREADVAR:begin
    ParseVARDeclartion;
   end;
   tstTYPE:begin
    ParseTYPEDeclartion;
   end;
   tstLABEL:begin
    ParseLABELDeclartion;
   end;
   tstCLASS,tstCONSTRUCTOR,tstDESTRUCTOR,tstPROCEDURE,tstFUNCTION:begin
    ParseProcedure(ParseHeader,[]);
   end;
   tstEXPORTS:begin
    if IsGlobal then begin
     ParseEXPORTS;
    end else begin
     Error.AddErrorCode(201);
    end;
   end;
   else begin
    break;
   end;
  end;
 end;
end;

function TParser.ParseMainBlock:TTreeNode;
begin
 result:=ParseBEGINBlockStatement;
end;

function TParser.ParseStatement(AllowExpression:boolean):TTreeNode;
var Symbol:PSymbol;
begin
 Scanner.CheckForDirectives([tstFAIL]);
 case Scanner.CurrentToken of
  tstCCODE:begin
   result:=ParseCCodeStatement;
  end;
  tstBEGIN:begin
   result:=ParseBEGINBlockStatement;
  end;
  tstFOR:begin
   result:=ParseFORStatement;
  end;
  tstWHILE:begin
   result:=ParseWHILEStatement;
  end;
  tstREPEAT:begin
   result:=ParseREPEATStatement;
  end;
  tstTRY:begin
   result:=ParseTRYStatement;
  end;
  tstIF:begin
   result:=ParseIFStatement;
  end;
  tstCASE:begin
   result:=ParseCASEStatement;
  end;
  tstWITH:begin
   result:=ParseWITHStatement;
  end;
  tstBREAK:begin
   result:=ParseBREAKStatement;
  end;
  tstCONTINUE:begin
   result:=ParseCONTINUEStatement;
  end;
  tstEXIT:begin
   result:=ParseEXITStatement;
  end;
  tstHALT:begin
   result:=ParseHALTStatement;
  end;
  tstFAIL:begin
   result:=ParseFAILStatement;
  end;
  tstRAISE:begin
   result:=ParseRAISEStatement;
  end;
  tstGOTO:begin
   result:=ParseGOTOStatement;
  end;
  tstSeparator:begin
   result:=nil;
  end;
  tstValue,tstIdentifier,tstINHERITED:begin
   if Scanner.MaybeLabel(Scanner.CurrentToken) then begin
    Error.Push;
    if length(Scanner.ProcedureName)=0 then begin
     Symbol:=SymbolManager.GetSymbol(Scanner.ReadLabelEx,ModuleSymbol,CurrentObjectClass);
    end else begin
     Symbol:=SymbolManager.GetSymbol(Scanner.ProcedureName+Scanner.ReadLabelEx,ModuleSymbol,CurrentObjectClass);
    end;
    if assigned(Symbol) then begin
     if Symbol^.SymbolType=Symbols.tstLabel then begin
      Scanner.Match(Scanner.CurrentToken);
      Scanner.Match(tstColon);
      if tsaVarDmp in Symbol^.Attributes then begin
       Error.AbortCode(74,CorrectSymbolName(Symbol^.Name));
      end else begin
       result:=TreeManager.GenerateLabelNode(Symbol);
       Symbol^.Attributes:=Symbol^.Attributes+[tsaVarDmp];
       Error.Pop;
       exit;
      end;
     end;
    end;
    Error.Pop;
   end;
   if Scanner.CurrentToken in [tstIdentifier,tstINHERITED] then begin
    result:=ParseExpression(true);
    if assigned(result) then begin
     if not AllowExpression then begin
      if (not GlobalSwitches^.ExtendedSyntax) and (result.TreeNodeType=ttntCALL) and assigned(result.Symbol) and assigned(result.Symbol^.ReturnType) then begin
       if Error.Errors then begin
        Error.DoAbort:=true;
       end else begin
        Error.AbortCode(504);
       end;
      end else if not (result.TreeNodeType in [ttntAssign,ttntCALL,ttntASM]) then begin
       if Error.Errors then begin
        Error.DoAbort:=true;
       end else begin
        Error.AbortCode(504);
       end;
      end;
     end;
    end else begin
     Error.AbortCode(507);
    end;
   end else begin
    Error.AbortCode(507);
    result:=nil;
   end;
  end;
  else begin
   if AllowExpression then begin
    result:=ParseExpression(true);
    if not assigned(result) then begin
     result:=nil;
    end;
   end else begin
    Error.AbortCode(507);
    result:=nil;
   end;
  end;
 end;
 OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
 OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
 OptimizerHighLevel.OptimizeTree(result);
end;

function TParser.ParseBlockStatement(EndTokens:TScannerTokens):TTreeNode;
var LastTreeNode,NewTreeNode,NewStatementNode:TTreeNode;
begin
 LastTreeNode:=nil;
 NewTreeNode:=nil;
 while (not (Scanner.CurrentToken in EndTokens)) and not Scanner.IsEOFOrAbortError do begin
  NewStatementNode:=TreeManager.GenerateLeftRightNode(ttntStatement,nil,ParseStatement(false));
  if assigned(LastTreeNode) then begin
   NewTreeNode.Left:=NewStatementNode;
   NewTreeNode:=NewTreeNode.Left;
  end else begin
   NewTreeNode:=NewStatementNode;
   LastTreeNode:=NewTreeNode;
  end;
  if Scanner.CurrentToken in EndTokens then begin
   break;
  end;
  case Scanner.CurrentToken of
   tstSEPARATOR:begin
    Scanner.Match(tstSEPARATOR);
   end;
  end;
 end;
 result:=TreeManager.GenerateLeftNode(ttntBlock,LastTreeNode);
end;

function TParser.ParseBEGINBlockStatement:TTreeNode;
begin
 if Scanner.CurrentToken=tstBEGIN then begin
  Scanner.Match(tstBEGIN);
  result:=ParseBlockStatement([tstEND]);
  Scanner.Match(tstEND);
 end else begin
  Scanner.Match(tstBEGIN);
  result:=nil;
 end
end;

function TParser.ParseCCodeStatement:TTreeNode;
var NewTreeNode,FirstTreeNode,LastTreeNode:TTreeNode;
begin
 if Scanner.CurrentToken=tstCCODE then begin
  Scanner.Match(tstCCODE);
  NewTreeNode:=nil;
  FirstTreeNode:=nil;
  LastTreeNode:=nil;
  while not Scanner.IsEOFOrAbortError do begin
   case Scanner.CurrentToken of
    tstCBLOCK:begin
     NewTreeNode:=TreeManager.GenerateCBlockNode(Scanner.CurrentString);
     Scanner.ReadNext;
    end;
    tstCEND:begin
     Scanner.ReadNext;
     break;
    end;
    tstCSKIP:begin
     Scanner.ReadNext;
    end;
    else begin
     if Scanner.CurrentMode=smPASCALEXPRESSION then begin
      NewTreeNode:=TreeManager.GeneratePascalBlockNode(ParseExpression(false));
     end else begin
      NewTreeNode:=TreeManager.GeneratePascalBlockNode(ParseStatement(false));
      if Scanner.CurrentToken=tstSeparator then begin
       Scanner.ReadNext;
      end;
     end;
    end;
   end;
   if assigned(LastTreeNode) then begin
    LastTreeNode.Left:=NewTreeNode;
   end else begin
    FirstTreeNode:=NewTreeNode;
   end;
   LastTreeNode:=NewTreeNode;
  end;
  result:=TreeManager.GenerateCCodeNode(FirstTreeNode);
 end else begin
  result:=nil;
  Scanner.Match(tstCCODE);
 end;
end;

function TParser.ParseASMBlockStatement:TTreeNode;
begin
 result:=nil;
end;

procedure TParser.ParseRecordField(var RecordType:PType;IsRecord:boolean=true;SymbolAttributes:TSymbolAttributes=[];UntilDirectives:TScannerTokens=[];CaseOfLevel:longint=0;CaseOfVariant:longint=0;VariantPrefix:ansistring='';IgnoreExistingOnParent:boolean=false);
var AType:PType;
    StartSymbol,Symbol,LastSymbol,NextSymbol:PSymbol;
    TypeName,VariantStr:ansistring;
    NewTreeNode:TTreeNode;
    PortabilityDirectives:TPortabilityDirectives;
begin
 Scanner.CheckForDirectives(UntilDirectives);
 if Scanner.CurrentToken=tstIdentifier then begin
  repeat
   StartSymbol:=nil;
   LastSymbol:=nil;
   repeat
    Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    if not assigned(StartSymbol) then begin
     StartSymbol:=Symbol;
    end;
    if assigned(LastSymbol) then begin
     LastSymbol^.Next:=Symbol;
    end;
    LastSymbol:=Symbol;
    Symbol^.Name:=Scanner.ReadIdentifier(@Symbol^.OriginalCaseName);
    HashSymbol(Symbol);
    if Scanner.CurrentToken=tstCOMMA then begin
     Scanner.Match(tstCOMMA);
    end else begin
     break;
    end;
   until (Scanner.CurrentToken<>tstIdentifier) or Scanner.IsEOFOrAbortError;
   Scanner.Match(tstCOLON);
   AType:=ParseTypeDefinition('');
   PortabilityDirectives:=ParsePortabilityDirectives;
   Symbol:=StartSymbol;
   while assigned(Symbol) do begin
    NextSymbol:=Symbol^.Next;
    Symbol^.Next:=nil;
    Symbol^.SymbolType:=Symbols.tstVARIABLE;
    Symbol^.TypeDefinition:=AType;
    Symbol^.Offset:=0;
    Symbol^.VariantPrefix:=VariantPrefix;
    Symbol^.VariableType:=SymbolManager.VariableType;
    Symbol^.TypedConstant:=false;
    Symbol^.TypedTrueConstant:=false;
    Symbol^.TypedConstantReadOnly:=false;
    Symbol^.CaseOfLevel:=CaseOfLevel;
    Symbol^.CaseOfVariant:=CaseOfVariant;
    Symbol^.Attributes:=SymbolAttributes+[tsaField];
    if MakeSymbolsPublic then begin
     Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
    end;
    Symbol^.PortabilityDirectives:=PortabilityDirectives;
    Symbol^.OwnerType:=RecordType;
    RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass,false,not IgnoreExistingOnParent);
    Symbol:=NextSymbol;
   end;
   if Scanner.CurrentToken=tstSEPARATOR then begin
    Scanner.Match(tstSEPARATOR);
   end else begin
    break;
   end;
   Scanner.CheckForDirectives(UntilDirectives);
  until (Scanner.CurrentToken<>tstIdentifier) or Scanner.IsEOFOrAbortError;
 end;
 if (Scanner.CurrentToken=tstCASE) and IsRecord then begin
  Scanner.Match(tstCASE);
  TypeName:=tpsIdentifier+Scanner.CurrentIdentifier;
  Symbol:=SymbolManager.GetSymbol(TypeName,ModuleSymbol,CurrentObjectClass);
  if assigned(Symbol) then begin
   if (Symbol^.SymbolType<>Symbols.tstType) and ((assigned(Symbol^.TypeDefinition) and (Symbol^.TypeDefinition^.TypeDefinition<>ttdEnumerated) and (Symbol^.TypeDefinition^.TypeDefinition<>ttdSubRange) and (Symbol^.TypeDefinition^.TypeDefinition<>ttdCurrency)) or not assigned(Symbol^.TypeDefinition)) then begin
    Error.AbortCode(0);
   end;
   Scanner.Match(tstIdentifier);
  end else begin
   if Scanner.CurrentToken<>tstIdentifier then begin
    Error.AbortCode(508);
   end;
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=Scanner.ReadIdentifier(@Symbol^.OriginalCaseName);
   Scanner.Match(tstCOLON);
   AType:=ParseTypeDefinition(TypeName);
   Symbol^.SymbolType:=Symbols.tstVariable;
   Symbol^.TypeDefinition:=AType;
   Symbol^.VariantPrefix:=VariantPrefix;
   Symbol^.Offset:=0;
   Symbol^.VariableType:=SymbolManager.VariableType;
   Symbol^.TypedConstant:=false;
   Symbol^.TypedTrueConstant:=false;
   Symbol^.TypedConstantReadOnly:=false;
   Symbol^.CaseOfLevel:=CaseOfLevel;
   Symbol^.CaseOfVariant:=CaseOfVariant;
   Symbol^.Attributes:=SymbolAttributes+[tsaField];
   if MakeSymbolsPublic then begin
    Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
   end;
   Symbol^.OwnerType:=RecordType;
   RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
  end;
  Scanner.Match(tstOF);
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  Symbol^.Name:='';
  Symbol^.SymbolType:=Symbols.tstCaseVariantLevelPush;
  Symbol^.OwnerType:=RecordType;
  RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
  inc(CaseOfLevel);
  while not Scanner.IsEOFOrAbortError do begin
   while not Scanner.IsEOFOrAbortError do begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    if not (NewTreeNode.TreeNodeType in [ttntORDConst,ttntCHARConst]) then begin
     Error.AbortCode(25);
    end;
    NewTreeNode.Destroy;
    if Scanner.CurrentToken=tstCOMMA then begin
     Scanner.Match(tstCOMMA);
    end else begin
     break;
    end;
   end;
   Scanner.Match(tstCOLON);
   Scanner.Match(tstLeftParen);
   inc(CaseOfVariant);
   if Scanner.CurrentToken<>tstRightParen then begin
    VariantStr:='L'+InttoStr(CaseOfLevel)+'V'+InttoStr(CaseOfVariant);
    Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    Symbol^.Name:='';
    Symbol^.SymbolType:=Symbols.tstCaseVariantPush;
    Symbol^.OwnerType:=RecordType;
    RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
    ParseRecordField(RecordType,IsRecord,SymbolAttributes,[],CaseOfLevel,CaseOfVariant,VariantPrefix+VariantStr+'.',IgnoreExistingOnParent);
    Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    Symbol^.Name:='';
    Symbol^.SymbolType:=Symbols.tstCaseVariantPop;
    Symbol^.OwnerType:=RecordType;
    RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
   end;
   Scanner.Match(tstRightParen);
   if Scanner.CurrentToken=tstSEPARATOR then begin
    Scanner.Match(tstSEPARATOR);
   end else begin
    break;
   end;
   if Scanner.CurrentToken in [tstEND,tstRightParen] then begin
    break;
   end;
  end;
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  Symbol^.Name:='';
  Symbol^.SymbolType:=Symbols.tstCaseVariantLevelPop;
  Symbol^.OwnerType:=RecordType;
  RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
 end;
end;

procedure TParser.ParsePropertyField(var RecordType:PType);
var SymbolName,TypeName,TempName,OriginalCaseName:ansistring;
    NewTreeNode:TTreeNode;
    Symbol,ParentSymbol,TempSymbol,PropertySymbol,SymbolA,SymbolB:PSymbol;
    PortabilityDirectives:TPortabilityDirectives;
    NoDefault,HasParameter:boolean;
    ParameterSuffix:ansistring;
    ClassType:PType;
begin
 Scanner.Match(tstPROPERTY);
 OriginalCaseName:='';
 SymbolName:=Scanner.ReadIdentifier(@OriginalCaseName);
 if Error.DoAbort then begin
  exit;
 end;
 ParentSymbol:=RecordType^.RecordTable.GetSymbol(SymbolName,ModuleSymbol,CurrentObjectClass,true);
 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.OriginalCaseName:=OriginalCaseName;
 Symbol^.Name:=SymbolName;
 Symbol^.SymbolType:=Symbols.tstProperty;
 if assigned(ParentSymbol) and ((ParentSymbol^.SymbolType<>Symbols.tstProperty) or not assigned(ParentSymbol^.OwnerObjectClass)) then begin
  Error.AbortCode(152,CorrectSymbolName(SymbolName));
  ParentSymbol:=nil;
 end;
 if assigned(ParentSymbol) then begin
  Symbol^.PropertyType:=ParentSymbol^.PropertyType;
  Symbol^.PropertyParameter:=ParentSymbol^.PropertyParameter;
  Symbol^.PropertyRead:=ParentSymbol^.PropertyRead;
  Symbol^.PropertyWrite:=ParentSymbol^.PropertyWrite;
  Symbol^.PropertyStored:=ParentSymbol^.PropertyStored;
  Symbol^.PropertyImplements:=ParentSymbol^.PropertyImplements;
  Symbol^.PropertyDefault:=ParentSymbol^.PropertyDefault;
  Symbol^.PropertyDefaultArray:=ParentSymbol^.PropertyDefaultArray;
  Symbol^.PropertyNoDefault:=ParentSymbol^.PropertyNoDefault;
  Symbol^.PropertyIndex:=ParentSymbol^.PropertyIndex;
  PortabilityDirectives:=ParentSymbol^.PortabilityDirectives;
 end else begin
  Symbol^.PropertyType:=nil;
  Symbol^.PropertyParameter:=nil;
  Symbol^.PropertyRead:=nil;
  Symbol^.PropertyWrite:=nil;
  Symbol^.PropertyStored:=nil;
  Symbol^.PropertyImplements:=nil;
  Symbol^.PropertyDefault:=nil;
  Symbol^.PropertyDefaultArray:=false;
  Symbol^.PropertyNoDefault:=true;
  Symbol^.PropertyIndex:=nil;
  PortabilityDirectives:=[];
 end;
 HasParameter:=false;
 if Scanner.CurrentToken=tstLeftBracket then begin
  HasParameter:=true;
  Symbol^.PropertyType:=nil;
  ParameterSuffix:='';
  ParseParameterList(Symbol,Symbol^.PropertyParameter,true,false,ParameterSuffix);
  SymbolManager.PopSymbolList(Symbol^.PropertyParameter);
  Symbol^.PropertyRead:=nil;
  Symbol^.PropertyWrite:=nil;
  Symbol^.PropertyStored:=nil;
  Symbol^.PropertyImplements:=nil;
  Symbol^.PropertyDefault:=nil;
  Symbol^.PropertyDefaultArray:=false;
  Symbol^.PropertyNoDefault:=true;
  Symbol^.PropertyIndex:=nil;
  PortabilityDirectives:=[];
  ParentSymbol:=nil;
 end;
 TypeName:='';
 TempName:='';
 NoDefault:=false;
 if Scanner.CurrentToken<>tstSEPARATOR then begin
  Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
  if Scanner.CurrentToken=tstCOLON then begin
   ParentSymbol:=nil;
   Symbol^.PropertyType:=nil;
   if not HasParameter then begin
    Symbol^.PropertyParameter:=nil;
   end;
   Symbol^.PropertyRead:=nil;
   Symbol^.PropertyWrite:=nil;
   Symbol^.PropertyStored:=nil;
   Symbol^.PropertyImplements:=nil;
   Symbol^.PropertyDefault:=nil;
   Symbol^.PropertyDefaultArray:=false;
   Symbol^.PropertyNoDefault:=true;
   Symbol^.PropertyIndex:=nil;
   PortabilityDirectives:=[];
   Scanner.Match(tstCOLON);
   TypeName:=Scanner.ReadIdentifier(nil);
   Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
   TempSymbol:=SymbolManager.GetSymbol(TypeName,ModuleSymbol,CurrentObjectClass);
   if (not assigned(TempSymbol)) or (TempSymbol^.SymbolType<>Symbols.tstType) then begin
    Error.AbortCode(115);
   end else begin
    Symbol^.PropertyType:=TempSymbol^.TypeDefinition;
   end;
  end;
  while (Scanner.CurrentToken in [tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]) and not Scanner.IsEOFOrAbortError do begin
   case Scanner.CurrentToken of
    tstREAD:begin
     Scanner.Match(tstREAD);
     TempName:=Scanner.ReadIdentifier(nil);
     Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
     TempSymbol:=RecordType^.RecordTable.GetSymbol(TempName,ModuleSymbol,CurrentObjectClass,true);
     if (not (assigned(TempSymbol) and assigned(TempSymbol^.OwnerObjectClass))) or not (TempSymbol^.SymbolType in [Symbols.tstVariable,Symbols.tstFunction]) then begin
      Error.AbortCode(246);
     end else begin
      case TempSymbol^.SymbolType of
       Symbols.tstVariable:begin
        if not EqualTypes(Error,SymbolManager,Symbol^.PropertyType,TempSymbol^.TypeDefinition) then begin
         Error.AbortCode(7);
        end else begin
         Symbol^.PropertyRead:=TempSymbol;
        end;
       end;
       Symbols.tstFunction:begin
        if not EqualTypes(Error,SymbolManager,Symbol^.PropertyType,TempSymbol^.ReturnType) then begin
         Error.AbortCode(7);
        end else begin
         if assigned(Symbol^.PropertyParameter) and assigned(Symbol^.PropertyParameter.First) then begin
          SymbolA:=Symbol^.PropertyParameter.First;
         end else begin
          SymbolA:=nil;
         end;
         if assigned(TempSymbol^.Parameter) and assigned(TempSymbol^.Parameter.First) then begin
          SymbolB:=TempSymbol^.Parameter.First;
         end else begin
          SymbolB:=nil;
         end;
         if assigned(Symbol^.PropertyIndex) and
            ((assigned(SymbolB) and not SymbolManager.CompatibleTypes(Symbol^.PropertyIndex^.TypeDefinition,SymbolB^.TypeDefinition)) or
             not assigned(SymbolB)) then begin
          Error.AbortCode(7);
         end else begin
          if assigned(Symbol^.PropertyIndex) and assigned(SymbolB) then begin
           SymbolB:=SymbolB^.Next;
          end;
          while assigned(SymbolA) and assigned(SymbolB) do begin
           if not SymbolManager.CompatibleTypes(SymbolA^.TypeDefinition,SymbolB^.TypeDefinition) then begin
            break;
           end;
           SymbolA:=SymbolA^.Next;
           SymbolB:=SymbolB^.Next;
          end;
          if assigned(SymbolA) or assigned(SymbolB) then begin
           Error.AbortCode(7);
          end;
         end;
         Symbol^.PropertyRead:=TempSymbol;
        end;
       end;
       else begin
        Error.AbortCode(7);
       end;
      end;
     end;
    end;
    tstWRITE:begin
     Scanner.Match(tstWRITE);
     TempName:=Scanner.ReadIdentifier(nil);
     Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
     TempSymbol:=RecordType^.RecordTable.GetSymbol(TempName,ModuleSymbol,CurrentObjectClass,true);
     if (not (assigned(TempSymbol) and assigned(TempSymbol^.OwnerObjectClass))) or not (TempSymbol^.SymbolType in [Symbols.tstVariable,Symbols.tstProcedure]) then begin
      Error.AbortCode(246);
     end else begin
      case TempSymbol^.SymbolType of
       Symbols.tstVariable:begin
        if not EqualTypes(Error,SymbolManager,Symbol^.PropertyType,TempSymbol^.TypeDefinition) then begin
         Error.AbortCode(7);
        end else begin
         Symbol^.PropertyWrite:=TempSymbol;
        end;
       end;
       Symbols.tstProcedure:begin
        if assigned(Symbol^.PropertyParameter) and assigned(Symbol^.PropertyParameter.First) then begin
         SymbolA:=Symbol^.PropertyParameter.First;
        end else begin
         SymbolA:=nil;
        end;
        if assigned(TempSymbol^.Parameter) and assigned(TempSymbol^.Parameter.First) then begin
         SymbolB:=TempSymbol^.Parameter.First;
        end else begin
         SymbolB:=nil;
        end;
        if assigned(Symbol^.PropertyIndex) and
           ((assigned(SymbolB) and not SymbolManager.CompatibleTypes(Symbol^.PropertyIndex^.TypeDefinition,SymbolB^.TypeDefinition)) or
            not assigned(SymbolB)) then begin
         Error.AbortCode(7);
        end else begin
         if assigned(Symbol^.PropertyIndex) and assigned(SymbolB) then begin
          SymbolB:=SymbolB^.Next;
         end;
         while assigned(SymbolA) and assigned(SymbolB) do begin
          if not SymbolManager.CompatibleTypes(SymbolA^.TypeDefinition,SymbolB^.TypeDefinition) then begin
           break;
          end;
          SymbolA:=SymbolA^.Next;
          SymbolB:=SymbolB^.Next;
         end;
         if assigned(SymbolA) or not assigned(SymbolB) then begin
          Error.AbortCode(7);
         end else begin
          if not EqualTypes(Error,SymbolManager,Symbol^.PropertyType,SymbolB^.TypeDefinition) then begin
           SymbolB:=SymbolB^.Next;
           if assigned(SymbolB) then begin
            Error.AbortCode(7);
           end;
          end else begin
           Error.AbortCode(7);
          end;
         end;
        end;
        Symbol^.PropertyWrite:=TempSymbol;
       end;
       else begin
        Error.AbortCode(7);
       end;
      end;
     end;
    end;
    tstSTORED:begin
     Scanner.Match(tstSTORED);
     Scanner.AllowedDirectives:=Scanner.AllowedDirectives+[tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX];
     TempName:=Scanner.ReadIdentifier(nil);
     Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
     TempSymbol:=RecordType^.RecordTable.GetSymbol(TempName,ModuleSymbol,CurrentObjectClass,true);
     if assigned(TempSymbol) and assigned(TempSymbol^.OwnerObjectClass) then begin
      case TempSymbol^.SymbolType of
       Symbols.tstVariable:begin
        if assigned(TempSymbol^.TypeDefinition) and (TempSymbol^.TypeDefinition^.TypeDefinition=ttdBoolean) then begin
         Symbol^.PropertyStored:=TempSymbol;
        end else begin
         Error.AbortCode(7);
        end;
       end;
       Symbols.tstFunction:begin
        if assigned(TempSymbol^.ReturnType) and (TempSymbol^.ReturnType^.TypeDefinition=ttdBoolean) then begin
         if assigned(TempSymbol^.Parameter) and assigned(TempSymbol^.Parameter.First) then begin
          Error.AbortCode(7);
         end else begin
          Symbol^.PropertyStored:=TempSymbol;
         end;
        end else begin
         Error.AbortCode(7);
        end;
       end;
       else begin
        Error.AbortCode(7);
       end;
      end;
     end else begin
      TempSymbol:=SymbolManager.GetSymbol(TempName,ModuleSymbol,CurrentObjectClass);
      if assigned(TempSymbol) and (TempSymbol^.SymbolType=Symbols.tstConstant) and
         assigned(TempSymbol^.ConstantTypeRecord) and (TempSymbol^.ConstantTypeRecord^.TypeDefinition=ttdBoolean) then begin
       Symbol^.PropertyStored:=TempSymbol;
      end else begin
       Error.AbortCode(7);
      end;
     end;
    end;
    tstDEFAULT:begin
     Symbol^.PropertyNoDefault:=false;
     Symbol^.PropertyDefault:=nil;
     Symbol^.PropertyDefaultArray:=false;
     Scanner.Match(tstDEFAULT);
     if Scanner.CurrentToken in [tstIdentifier,tstValue,tstStringValue,tstCharValue] then begin
      if (assigned(Symbol^.PropertyType) and (Symbol^.PropertyType^.TypeDefinition<>ttdArray)) and not assigned(Symbol^.PropertyParameter) then begin
       Scanner.AllowedDirectives:=Scanner.AllowedDirectives+[tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX];
       NewTreeNode:=ParseExpression(false);
       Scanner.AllowedDirectives:=Scanner.AllowedDirectives-[tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX];
       OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
       OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
       OptimizerHighLevel.OptimizeTree(NewTreeNode);
       if SymbolManager.CompatibleTypes(Symbol^.PropertyType,NewTreeNode.Return) then begin
        case NewTreeNode.TreeNodeType of
         ttntORDConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          TempSymbol^.ConstantType:=tctOrdinal;
          TempSymbol^.IntValue:=NewTreeNode.Value;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         ttntCHARConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          if NewTreeNode.Return.SubRangeType=tstUnsignedHugeChar then begin
           TempSymbol^.ConstantType:=tctHugeChar;
           TempSymbol^.CharValue:=NewTreeNode.Value and longword($ffffffff);
          end else if NewTreeNode.Return.SubRangeType=tstUnsignedWideChar then begin
           TempSymbol^.ConstantType:=tctWideChar;
           TempSymbol^.CharValue:=NewTreeNode.Value and $ffff;
          end else begin
           TempSymbol^.ConstantType:=tctAnsiChar;
           TempSymbol^.CharValue:=NewTreeNode.Value and $ff;
          end;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         ttntSTRINGConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          if NewTreeNode.Return.SubRangeType=tstUnsignedHugeChar then begin
           TempSymbol^.ConstantType:=tctHugeString;
           TempSymbol^.StringValue:=NewTreeNode.StringData;
          end else if NewTreeNode.Return.SubRangeType=tstUnsignedWideChar then begin
           TempSymbol^.ConstantType:=tctWideString;
           TempSymbol^.StringValue:=NewTreeNode.StringData;
          end else begin
           TempSymbol^.ConstantType:=tctAnsiString;
           TempSymbol^.StringValue:=NewTreeNode.StringData;
          end;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         ttntFloatConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          TempSymbol^.ConstantType:=tctFloat;
          TempSymbol^.FloatValue:=NewTreeNode.FloatValue;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         ttntSETConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          TempSymbol^.ConstantType:=tctSet;
          TempSymbol^.SetArray:=NewTreeNode.SetData;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         ttntPCHARConst:begin
          TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
          TempSymbol^.Name:='#';
          TempSymbol^.SymbolType:=tstConstant;
          TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
          if assigned(NewTreeNode.Return) and (NewTreeNode.Return^.TypeDefinition=ttdPointer) and
             assigned(NewTreeNode.Return^.PointerTo) and (NewTreeNode.Return^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
             (NewTreeNode.Return^.PointerTo^.TypeDefinition^.SubRangeType=tstUnsignedHugeChar) then begin
           TempSymbol^.ConstantType:=tctHugeString;
          end else if assigned(NewTreeNode.Return) and (NewTreeNode.Return^.TypeDefinition=ttdPointer) and
                      assigned(NewTreeNode.Return^.PointerTo) and (NewTreeNode.Return^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                      (NewTreeNode.Return^.PointerTo^.TypeDefinition^.SubRangeType=tstUnsignedWideChar) then begin
           TempSymbol^.ConstantType:=tctWideString;
          end else begin
           TempSymbol^.ConstantType:=tctAnsiString;
          end;
          TempSymbol^.StringValue:=NewTreeNode.StringData;
          Symbol^.PropertyDefault:=TempSymbol;
         end;
         else begin
          Error.AddErrorCode(151);
         end;
        end;
       end else begin
        Error.AddErrorCode(151);
       end;
       NewTreeNode.Destroy;
      end else begin
       Error.AbortCode(7);
      end;
     end else begin
      if (assigned(Symbol^.PropertyType) and (Symbol^.PropertyType^.TypeDefinition=ttdArray)) or assigned(Symbol^.PropertyParameter) then begin
       PropertySymbol:=nil;
       ClassType:=RecordType;
       while assigned(ClassType) and not assigned(PropertySymbol) do begin
        if assigned(ClassType^.RecordTable) then begin
         TempSymbol:=ClassType^.RecordTable.First;
         while assigned(TempSymbol) do begin
          case TempSymbol^.SymbolType of
           Symbols.tstProperty:begin
            if TempSymbol.PropertyDefaultArray then begin
             PropertySymbol:=TempSymbol;
             break;
            end;
           end;
          end;
          TempSymbol:=TempSymbol^.Next;
         end;
        end;
        if assigned(ClassType^.ChildOf) then begin
         ClassType:=ClassType^.ChildOf^.TypeDefinition;
        end else begin
         break;
        end;
       end;
       if assigned(PropertySymbol) then begin
        Error.AddErrorCode(135);
       end else begin
        Symbol^.PropertyDefaultArray:=true;
       end;
      end else begin
       Error.AddErrorCode(136);
      end;
     end;
    end;
    tstNODEFAULT:begin
     Scanner.Match(tstNODEFAULT);
     NoDefault:=true;
     Symbol^.PropertyNoDefault:=true;
     Symbol^.PropertyDefault:=nil;
     Symbol^.PropertyDefaultArray:=false;
    end;
    tstIMPLEMENTS:begin
     Scanner.Match(tstIMPLEMENTS);
     if (assigned(Symbol^.PropertyType) and (Symbol^.PropertyType^.TypeDefinition=ttdArray)) or assigned(Symbol^.PropertyParameter) then begin
      Error.AbortCode(7);
     end else begin
      if not assigned(Symbol^.PropertyImplements) then begin
       Symbol^.PropertyImplements:=TSymbolList.Create(SymbolManager);
      end;
      while not Scanner.IsEOFOrAbortError do begin
       TempName:=Scanner.ReadIdentifier(nil);
       TempSymbol:=SymbolManager.GetSymbol(TempName,ModuleSymbol,CurrentObjectClass);
       if assigned(TempSymbol) and (TempSymbol^.SymbolType=Symbols.tstType) and
          assigned(TempSymbol^.TypeDefinition) and (TempSymbol^.TypeDefinition^.TypeDefinition in [ttdCLASS,ttdINTERFACE]) then begin
        Symbol^.PropertyImplements.AddSymbol(TempSymbol,ModuleSymbol,CurrentObjectClass,false,false);
       end else begin
        Error.AbortCode(7);
       end;
       Scanner.CheckForDirectives([tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX]);
       if Scanner.CurrentToken=tstComma then begin
        Scanner.Match(tstComma);
       end else begin
        break;
       end;
      end;
     end;
    end;
    tstINDEX:begin
     Symbol^.PropertyIndex:=nil;
     Scanner.Match(tstINDEX);
     Scanner.AllowedDirectives:=Scanner.AllowedDirectives+[tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX];
     NewTreeNode:=ParseExpression(false);
     Scanner.AllowedDirectives:=Scanner.AllowedDirectives-[tstREAD,tstWRITE,tstSTORED,tstDEFAULT,tstNODEFAULT,tstIMPLEMENTS,tstINDEX];
     OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
     OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
     OptimizerHighLevel.OptimizeTree(NewTreeNode);
     if assigned(NewTreeNode) then begin
      case NewTreeNode.TreeNodeType of
       ttntORDConst:begin
        TempSymbol:=SymbolManager.NewSymbol(ModuleSymbol,nil);
        TempSymbol^.Name:='#';
        TempSymbol^.SymbolType:=tstConstant;
        TempSymbol^.ConstantTypeRecord:=NewTreeNode.Return;
        TempSymbol^.ConstantType:=tctOrdinal;
        TempSymbol^.IntValue:=NewTreeNode.Value;
        Symbol^.PropertyIndex:=TempSymbol;
       end;
       else begin
        Error.AbortCode(0);
       end;
      end;
      FreeAndNil(NewTreeNode);
     end else begin
      Error.AbortCode(0);
     end;
    end;
   end;
  end;
  PortabilityDirectives:=ParsePortabilityDirectives;
  if PortabilityDirectives<>[] then begin
  end;
  if NoDefault then begin
  end;
 end;
 Scanner.Match(tstSEPARATOR);
 Symbol^.PortabilityDirectives:=PortabilityDirectives;
 Symbol^.OwnerType:=RecordType;
 RecordType^.RecordTable.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass,false,false);
end;

function TParser.ParseProcedureVariable(var Symbol:PSymbol):PType;
var OldList:TSymbolList;
begin
 OldList:=SymbolManager.CurrentList;
 result:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 result^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 result^.TypeKind:=TypeKindUnknown;
 result^.NeedTypeInfo:=false;
 result^.TypeDefinition:=ttdProcedure;
 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol^.ParameterSuffix:='';
 ParseParameterList(Symbol,Symbol^.Parameter,false,true,Symbol^.ParameterSuffix);
 SymbolManager.PopSymbolList(Symbol^.Parameter);
 result^.Parameter:=Symbol^.Parameter;
 SymbolManager.CurrentList:=OldList;
end;

function TParser.ParseObjectDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
var Symbol,{NewSymbol,}Parent,TestSymbol:PSymbol;
    OldList:TSymbolList;
    OldVariableType:TVariableType;
    ParentName,OldObjectClassName:ansistring;
    SymbolAttributes:TSymbolAttributes;
    OldCurrentObjectClass,OldCurrentParseObjectClass,CurrentObject:PType;
    NewTreeNode:TTreeNode;
    OK:boolean;
begin
 if assigned(CurrentProcedureFunction) then begin
  Error.AbortCode(62);
 end;
 Scanner.Match(tstOBJECT);
 OldList:=SymbolManager.CurrentList;
 Parent:=nil;
 if Scanner.CurrentToken=tstLeftParen then begin
  Scanner.Match(tstLeftParen);
  ParentName:=Scanner.ReadIdentifier(nil);
  Parent:=SymbolManager.GetSymbol(ParentName,ModuleSymbol,CurrentObjectClass);
  if assigned(Parent) and (Parent^.SymbolType=Symbols.tstUnit) then begin
   Scanner.Match(tstPeriod);
   ParentName:=Scanner.ReadIdentifier(nil);
   Parent:=Parent^.SymbolList.GetSymbol(ParentName,ModuleSymbol,CurrentObjectClass);
  end;
  if not assigned(Parent) then begin
   Error.AbortCode(18);
  end else if (Parent^.SymbolType<>Symbols.tstType) and (Parent^.TypeDefinition^.TypeDefinition<>ttdObject) then begin
   Error.AbortCode(18);
  end;
  Scanner.Match(tstRightParen);
 end;
 result:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 result^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 result^.TypeKind:=TypeKindRecord;
 result^.NeedTypeInfo:=false;
 result^.TypeDefinition:=ttdObject;
 result^.RecordAlignment:=0;
 result^.RecordPacked:=IsPacked or (LocalSwitches^.Alignment=1);
 result^.ChildOf:=Parent;
 OldCurrentObjectClass:=CurrentObjectClass;
 OldCurrentParseObjectClass:=CurrentParseObjectClass;
 CurrentObjectClass:=result;
 CurrentParseObjectClass:=result;
 result^.HasVirtualTable:=false;
 result^.RecordTable:=TSymbolList.Create(SymbolManager);
 SymbolManager.CurrentList:=result^.RecordTable;
 OldVariableType:=SymbolManager.VariableType;
 SymbolManager.VariableType:=tvtObjectField;
 SymbolAttributes:=[tsaOOPPublic];
 result^.VirtualIndexCount:=0;
 CurrentObject:=nil;
 if assigned(result^.ChildOf) then begin
  CurrentObject:=result^.ChildOf^.TypeDefinition;
  result^.RecordPacked:=CurrentObject^.RecordPacked;
  result^.RecordSize:=CurrentObject^.RecordSize;
  result^.RecordAlignment:=CurrentObject^.RecordAlignment;
  result^.VirtualIndexCount:=CurrentObject^.VirtualIndexCount;            
{ Symbol:=CurrentObject^.RecordTable.First;
  while assigned(Symbol) do begin
   if Symbol^.SymbolType=Symbols.tstVariable then begin
    NewSymbol:=SymbolManager.NewSymbol(ModuleSymbol,result);
    NewSymbol^.Name:=Symbol^.Name;
    if assigned(Symbol^.InheritedFrom) then begin
     NewSymbol^.InheritedFrom:=Symbol^.InheritedFrom;
    end else begin
     NewSymbol^.InheritedFrom:=Symbol;
    end;
    NewSymbol^.Attributes:=Symbol^.Attributes+[tsaInherited];
    NewSymbol^.SymbolType:=Symbol^.SymbolType;
    NewSymbol^.TypeDefinition:=Symbol^.TypeDefinition;
    NewSymbol^.Offset:=Symbol^.Offset;
    NewSymbol^.TypedConstant:=Symbol^.TypedConstant;
    NewSymbol^.TypedTrueConstant:=Symbol^.TypedTrueConstant;
    NewSymbol^.TypedConstantReadOnly:=Symbol^.TypedConstantReadOnly;
    NewSymbol^.OwnerType:=RecordType;
    result^.RecordTable.AddSymbol(NewSymbol,ModuleSymbol,result,false);
   end;
   Symbol:=Symbol^.Next;
  end;}
 end;
 if assigned(result^.ChildOf) and assigned(result^.ChildOf^.TypeDefinition) then begin
  result^.RecordTable.ChildOf:=result^.ChildOf^.TypeDefinition^.RecordTable;
 end;
 OldObjectClassName:=ObjectClassName;
 ObjectClassName:=ObjectName;
 while not Scanner.IsEOFOrAbortError do begin
  Scanner.CheckForDirectives([tstSTRICT,tstPRIVATE,tstPUBLIC,tstPROTECTED,tstPUBLISHED]);
  case Scanner.CurrentToken of
   tstSTRICT:begin
    Scanner.Match(tstSTRICT);
    Scanner.Match(tstPRIVATE);
    SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPStrictPrivate];
   end;
   tstPRIVATE:begin
    Scanner.Match(tstPRIVATE);
    SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPrivate];
   end;
   tstPROTECTED:begin
    Scanner.Match(tstPROTECTED);
    SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPProtected];
   end;
   tstPUBLIC:begin
    Scanner.Match(tstPUBLIC);
    SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPublic];
   end;
   tstPUBLISHED:begin
    Scanner.Match(tstPUBLISHED);
    SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPublished];
   end;
   tstIdentifier:begin
    ParseRecordField(result,false,SymbolAttributes,[tstSTRICT,tstPRIVATE,tstPUBLIC,tstPROTECTED,tstPUBLISHED],0,0,'',false);
   end;
   tstPROPERTY:begin
    ParsePropertyField(result);
   end;
   tstCONSTRUCTOR,tstDESTRUCTOR,tstFUNCTION,tstPROCEDURE:begin
    Symbol:=ParseProcedure(true,[tpaObject]);
    if Error.DoAbort then begin
     exit;
    end;
    Symbol^.MethodOfType:=result;
    Symbol^.Attributes:=(Symbol^.Attributes-OOPSymbolAttribute)+SymbolAttributes;
    Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL]);
    while Scanner.CurrentToken in [tstVIRTUAL,tstABSTRACT] do begin
     case Scanner.CurrentToken of
      tstVIRTUAL:begin
       Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaVirtual];
       Scanner.Match(tstVIRTUAL);
       if Scanner.CurrentToken<>tstSEPARATOR then begin
        NewTreeNode:=ParseExpression(false);
        OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
        OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
        OptimizerHighLevel.OptimizeTree(NewTreeNode);
        if assigned(NewTreeNode) and (NewTreeNode.TreeNodeType=ttntORDConst) then begin
         Symbol^.ProcedureAttributes:=(Symbol^.ProcedureAttributes-[tpaVirtual])+[tpaDynamic];
         Symbol^.VirtualIndex:=NewTreeNode.Value;
        end else begin
         Error.AbortCode(526);
        end;
       end else begin
        if assigned(CurrentObject) then begin
         TestSymbol:=CurrentObject^.RecordTable.GetSymbol(Symbol^.Name,ModuleSymbol,CurrentObjectClass,true);
         if assigned(TestSymbol) then begin
          OK:=false;
          TestSymbol:=SymbolManager.SearchProcedureSymbol(Symbol,TestSymbol,OK);
          if not OK then begin
           Error.AbortCode(265,CorrectSymbolName(Symbol^.Name));
          end else begin
           Symbol^.VirtualIndex:=TestSymbol^.VirtualIndex;
          end;
         end else begin
          Error.AbortCode(265,CorrectSymbolName(Symbol^.Name));
         end;
        end else begin
         Symbol^.VirtualIndex:=result^.VirtualIndexCount;
         inc(result^.VirtualIndexCount);
        end;
       end;
       Scanner.Match(tstSEPARATOR);
       result^.HasVirtualTable:=true;
      end;
      tstABSTRACT:begin
       Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaAbstract];
       Scanner.Match(tstABSTRACT);
       Scanner.Match(tstSEPARATOR);
      end;
     end;
     Symbol^.PortabilityDirectives:=Symbol^.PortabilityDirectives+ParsePortabilityDirectives;
     Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL]);
    end;
   end;
   tstEND:begin
    break;
   end;
   else begin
    Scanner.Illegal(Scanner.CurrentToken);
   end;
  end;
 end;
 Scanner.Match(tstEND);
 ObjectClassName:=OldObjectClassName;

 if not result^.HasVirtualTable then begin
  CurrentObject:=result;
  while assigned(CurrentObject) and not result^.HasVirtualTable do begin
   result^.HasVirtualTable:=CurrentObject^.HasVirtualTable;
   if result^.HasVirtualTable then begin
    break;
   end else begin
    Symbol:=CurrentObject^.RecordTable.First;
    while assigned(Symbol) do begin
     if (Symbol^.SymbolType in [Symbols.tstProcedure,Symbols.tstFunction]) and
        (tpaVirtual in Symbol^.ProcedureAttributes) then begin
      result^.HasVirtualTable:=true;
      break;
     end;
     Symbol:=Symbol^.Next;
    end;
    if assigned(CurrentObject^.ChildOf) then begin
     CurrentObject:=CurrentObject^.ChildOf^.TypeDefinition;
    end else begin
     break;
    end;
   end;
  end;
 end;
 if result^.HasVirtualTable then begin
  if assigned(result^.ChildOf) then begin
   if not result^.ChildOf^.TypeDefinition^.HasVirtualTable then begin
    Error.AbortCode(90,CorrectSymbolName(result^.ChildOf^.Name));
   end;
  end else begin
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,result);
   Symbol^.Name:='VMT';
   Symbol^.Attributes:=Symbol^.Attributes+[tsaField,tsaInternalField,tsaObjectVMT];
   Symbol^.SymbolType:=Symbols.tstVariable;
   Symbol^.TypeDefinition:=SymbolManager.TypePointer;
   Symbol^.Offset:=0;
   Symbol^.TypedConstant:=false;
   Symbol^.TypedTrueConstant:=false;
   Symbol^.TypedConstantReadOnly:=false;
   Symbol^.OwnerType:=result;
   result^.RecordTable.AddSymbol(Symbol,ModuleSymbol,result,true);
  end;
 end;

 SymbolManager.AlignRecord(result,LocalSwitches^.Alignment);

 result^.PortabilityDirectives:=ParsePortabilityDirectives;

 result^.NeedTypeInfo:=SymbolManager.TypeDoNeedTypeInfo(result);

 SymbolManager.VariableType:=OldVariableType;
 SymbolManager.CurrentList:=OldList;

 CurrentObjectClass:=OldCurrentObjectClass;
 CurrentParseObjectClass:=OldCurrentParseObjectClass;
end;

function TParser.ParseClassDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
var Symbol,Parent,NewSymbol,ForwardClass,ParentSymbol,TempSymbol:PSymbol;
    OldList:TSymbolList;
    OldVariableType:TVariableType;
    I,J:longint;
    NewName,ParentName,OldObjectClassName:ansistring;
    SymbolAttributes:TSymbolAttributes;
    NewTreeNode:TTreeNode;
    InterfaceSymbols:array of PSymbol;
    OldCurrentObjectClass,OldCurrentParseObjectClass,ClassOfType,ClassType:PType;
    IsClassOf,IsForward:boolean;
begin
 if assigned(CurrentProcedureFunction) then begin
  Error.AbortCode(62);
 end;
 InterfaceSymbols:=nil;
 Scanner.Match(tstCLASS);
 OldList:=SymbolManager.CurrentList;
 Parent:=nil;
 IsForward:=Scanner.CurrentToken=tstSEPARATOR;
 IsClassOf:=false;
 ForwardClass:=nil;
 if not IsForward then begin
  NewSymbol:=SymbolManager.GetSymbol(ObjectName,ModuleSymbol,CurrentObjectClass);
  if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstType) and (NewSymbol^.TypeDefinition^.TypeDefinition=ttdClass) and
     NewSymbol^.TypeDefinition^.ForwardClass then begin
   ForwardClass:=NewSymbol;
  end;
  if Scanner.CurrentToken=tstOF then begin
// IsClassOf:=TRUE;
   Scanner.Match(tstOF);
   NewName:=Scanner.ReadIdentifier(nil);
   NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
   if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
    Scanner.Match(tstPeriod);
    NewName:=Scanner.ReadIdentifier(nil);
    NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
   end;
   if not assigned(NewSymbol) then begin
    Error.AbortCode(20);
   end else if (NewSymbol^.SymbolType<>Symbols.tstType) or (NewSymbol^.TypeDefinition^.TypeDefinition<>ttdClass) then begin
    Error.AbortCode(20);
   end else begin
    ParentName:=NewName;
    Parent:=NewSymbol;
   end;
   if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstType) and (NewSymbol^.TypeDefinition^.TypeDefinition=ttdClass) and assigned(NewSymbol^.TypeDefinition^.ClassOfType) then begin
    result:=NewSymbol^.TypeDefinition^.ClassOfType;
   end else begin
    result:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    result^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    result^.TypeKind:=TypeKindUnknown;
    result^.TypeDefinition:=ttdClassRef;
    result^.ClassOf:=Parent;
   end;
   exit;
  end else if Scanner.CurrentToken=tstLeftParen then begin
   Scanner.Match(tstLeftParen);
   I:=0;
   while not Scanner.IsEOFOrAbortError do begin
    NewName:=Scanner.ReadIdentifier(nil);
    NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
    if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
     Scanner.Match(tstPeriod);
     NewName:=Scanner.ReadIdentifier(nil);
     NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
    end;
    if not assigned(NewSymbol) then begin
     if I=0 then begin
      Error.AbortCode(510);
     end else begin
      Error.AbortCode(217);
     end;
    end else begin
     if (I=0) and (NewSymbol^.SymbolType=Symbols.tstType) and (NewSymbol^.TypeDefinition^.TypeDefinition=ttdClass) then begin
      ParentName:=NewName;
      Parent:=NewSymbol;
     end else if (NewSymbol^.SymbolType=Symbols.tstType) and (NewSymbol^.TypeDefinition^.TypeDefinition=ttdInterface) then begin
      J:=length(InterfaceSymbols);
      SetLength(InterfaceSymbols,J+1);
      InterfaceSymbols[J]:=NewSymbol;
     end else begin
      if I=0 then begin
       Error.AbortCode(510);
      end else begin
       Error.AbortCode(217);
      end;
     end;
    end;
    inc(I);
    if Scanner.CurrentToken=tstComma then begin
     Scanner.Match(tstComma);
    end else begin
     break;
    end;
   end;
   Scanner.Match(tstRightParen);
  end;
 end;
 if assigned(ForwardClass) then begin
  result:=ForwardClass^.TypeDefinition;
  result^.WasForwardedClass:=true;
  result^.NeedTypeInfo:=false;
 end else begin
  result:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  result^.TypeKind:=TypeKindClass;
  result^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
  result^.WasForwardedClass:=false;
  result^.NeedTypeInfo:=false;
 end;
 if not IsForward then begin
  if IsSystemUnit then begin
   if not assigned(Parent) then begin
    NewName:=tpsIdentifier+'TOBJECT';
    NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
    if not assigned(NewSymbol) then begin
     Error.AbortCode(20);
    end else if (NewSymbol^.SymbolType<>Symbols.tstType) or (NewSymbol^.TypeDefinition^.TypeDefinition<>ttdClass) then begin
     Error.AbortCode(20);
    end else begin
     ParentName:=NewName;
     Parent:=NewSymbol;
    end;
   end;
  end else begin
   if not assigned(Parent) then begin
    NewName:=tpsIdentifier+'SYSTEM';
    NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
    if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
     NewName:=tpsIdentifier+'TOBJECT';
     NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
    end;
    if not assigned(NewSymbol) then begin
     Error.AbortCode(20);
    end else if (NewSymbol^.SymbolType<>Symbols.tstType) or (NewSymbol^.TypeDefinition^.TypeDefinition<>ttdClass) then begin
     Error.AbortCode(20);
    end else begin
     ParentName:=NewName;
     Parent:=NewSymbol;
    end;
   end;
  end;
 end;
 result^.TypeDefinition:=ttdClass;
 result^.HasVirtualTable:=true;
 if result^.Symbol=Parent then begin
  Parent:=nil;
 end;
 result^.ChildOf:=Parent;
 result^.ForwardClass:=IsForward;
 result^.RecordAlignment:=0;
 result^.RecordPacked:=IsPacked or (LocalSwitches^.Alignment=1);
 if not assigned(result^.ClassOfType) then begin
  ClassOfType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  ClassOfType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
  ClassOfType^.TypeKind:=TypeKindUnknown;
  ClassOfType^.TypeDefinition:=ttdClassRef;
  ClassOfType^.ClassOf:=nil;
  result^.ClassOfType:=ClassOfType;
 end;
 if IsForward then begin
  result^.VirtualIndexCount:=0;
  SetLength(InterfaceSymbols,0);
  exit;
 end;
 SetLength(result^.InterfaceChildOf,length(InterfaceSymbols));
 for I:=0 to length(InterfaceSymbols)-1 do begin
  result^.InterfaceChildOf[I]:=InterfaceSymbols[I];
 end;
 SetLength(InterfaceSymbols,0);

 OldCurrentObjectClass:=CurrentObjectClass;
 OldCurrentParseObjectClass:=CurrentParseObjectClass;
 CurrentObjectClass:=result;
 CurrentParseObjectClass:=result;

 if assigned(result^.ChildOf) then begin
  result^.VirtualIndexCount:=result^.ChildOf^.TypeDefinition^.VirtualIndexCount;
  result^.DynamicIndexCount:=result^.ChildOf^.TypeDefinition^.DynamicIndexCount;
 end else begin
  result^.VirtualIndexCount:=0;
  result^.DynamicIndexCount:=0;
 end;

 result^.RecordTable:=TSymbolList.Create(SymbolManager);
 if assigned(result^.ChildOf) and assigned(result^.ChildOf^.TypeDefinition) then begin
  result^.RecordTable.ChildOf:=result^.ChildOf^.TypeDefinition^.RecordTable;
 end;
 SymbolManager.CurrentList:=result^.RecordTable;
 OldVariableType:=SymbolManager.VariableType;
 SymbolManager.VariableType:=tvtClassField;
 SymbolAttributes:=[tsaOOPPublic];
 OldObjectClassName:=ObjectClassName;
 ObjectClassName:=ObjectName;
 if not IsClassOf then begin
  while not Scanner.IsEOFOrAbortError do begin
   Scanner.CheckForDirectives([tstSTRICT,tstPRIVATE,tstPUBLIC,tstPROTECTED,tstPUBLISHED]);
   case Scanner.CurrentToken of
    tstSTRICT:begin
     Scanner.Match(tstSTRICT);
     Scanner.CheckForDirectives([tstSTRICT,tstPRIVATE,tstPUBLIC,tstPROTECTED,tstPUBLISHED]);
     Scanner.Match(tstPRIVATE);
     SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPStrictPrivate];
    end;
    tstPRIVATE:begin
     Scanner.Match(tstPRIVATE);
     SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPrivate];
    end;
    tstPROTECTED:begin
     Scanner.Match(tstPROTECTED);
     SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPProtected];
    end;
    tstPUBLIC:begin
     Scanner.Match(tstPUBLIC);
     SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPublic];
    end;
    tstPUBLISHED:begin
     Scanner.Match(tstPUBLISHED);
     SymbolAttributes:=(SymbolAttributes-OOPSymbolAttribute)+[tsaOOPPublished];
    end;
    tstIdentifier:begin
     ParseRecordField(result,false,SymbolAttributes,[tstSTRICT,tstPRIVATE,tstPUBLIC,tstPROTECTED,tstPUBLISHED],0,0,'',true);
    end;
    tstPROPERTY:begin
     ParsePropertyField(result);
    end;
    tstCLASS,tstCONSTRUCTOR,tstDESTRUCTOR,tstFUNCTION,tstPROCEDURE:begin
     Symbol:=ParseProcedure(true,[tpaClass]);
     if Error.DoAbort then begin
      exit;
     end;
     Symbol^.MethodOfType:=result;
     Symbol^.Attributes:=(Symbol^.Attributes-OOPSymbolAttribute)+SymbolAttributes;
     Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL,tstDYNAMIC,tstOVERRIDE,tstMESSAGE]);
     while Scanner.CurrentToken in [tstVIRTUAL,tstABSTRACT,tstDYNAMIC,tstOVERRIDE,tstMESSAGE] do begin
      case Scanner.CurrentToken of
       tstVIRTUAL:begin
        Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaVirtual];
        Scanner.Match(tstVIRTUAL);
        Scanner.Match(tstSEPARATOR);
        Symbol^.VirtualIndex:=result^.VirtualIndexCount;
        inc(result^.VirtualIndexCount);
       end;
       tstABSTRACT:begin
        Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaAbstract];
        Scanner.Match(tstABSTRACT);
        Scanner.Match(tstSEPARATOR);
       end;
       tstDYNAMIC:begin
        Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaDynamic];
        Scanner.Match(tstDYNAMIC);
        Scanner.Match(tstSEPARATOR);
        Symbol^.DynamicIndex:=result^.DynamicIndexCount;
        inc(result^.DynamicIndexCount);
       end;
       tstOVERRIDE:begin
        Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaOverride];
        Scanner.Match(tstOVERRIDE);
        Scanner.Match(tstSEPARATOR);
        ClassType:=result;
        ParentSymbol:=nil;
        while assigned(ClassType) and not assigned(ParentSymbol) do begin
         if assigned(ClassType^.RecordTable) then begin
          TempSymbol:=ClassType^.RecordTable.First;
          while assigned(TempSymbol) do begin
           case TempSymbol^.SymbolType of
            Symbols.tstFunction,Symbols.tstProcedure:begin
             if (([tpaVirtual,tpaDynamic]*TempSymbol^.ProcedureAttributes)<>[]) and
                (not (tpaOverride in TempSymbol^.ProcedureAttributes)) and
                (Symbol^.Name=TempSymbol^.Name) and
                (CompareProcToProc(Error,SymbolManager,TempSymbol,Symbol)=tcteEqual) then begin
              ParentSymbol:=TempSymbol;
              break;
             end;
            end;
           end;
           TempSymbol:=TempSymbol^.Next;
          end;
         end;
         if assigned(ClassType^.ChildOf) then begin
          ClassType:=ClassType^.ChildOf^.TypeDefinition;
         end else begin
          break;
         end;
        end;
        if assigned(ParentSymbol) then begin
         Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+([tpaVirtual,tpaDynamic]*TempSymbol^.ProcedureAttributes);
         Symbol^.VirtualIndex:=ParentSymbol^.VirtualIndex;
         Symbol^.DynamicIndex:=ParentSymbol^.DynamicIndex;
        end else begin
         Error.AddErrorCode(141,CorrectSymbolName(Symbol^.Name));
        end;
       end;
       tstREINTRODUCE:begin
        Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaReintroduce];
        Scanner.Match(tstREINTRODUCE);
        Scanner.Match(tstSEPARATOR);
       end;                                       
       tstMESSAGE:begin
        Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaMessage];
        Scanner.Match(tstMESSAGE);
        NewTreeNode:=ParseExpression(false);
        OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
        OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
        OptimizerHighLevel.OptimizeTree(NewTreeNode);
        if NewTreeNode.TreeNodeType in [ttntORDConst,ttntCHARConst] then begin
         Symbol^.MessageCode:=NewTreeNode.Value;
        end else begin
         Error.AbortCode(25);
        end;
        NewTreeNode.Destroy;
        Scanner.Match(tstSEPARATOR);
       end;
      end;
      Symbol^.PortabilityDirectives:=Symbol^.PortabilityDirectives+ParsePortabilityDirectives;
      Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL,tstDYNAMIC,tstOVERRIDE,tstMESSAGE]);
      if ([tpaVirtual,tpaDynamic,tpaReintroduce]*Symbol^.ProcedureAttributes)=[] then begin
       ClassType:=result;
       ParentSymbol:=nil;
       while assigned(ClassType) and not assigned(ParentSymbol) do begin
        if assigned(ClassType^.RecordTable) then begin
         TempSymbol:=ClassType^.RecordTable.First;
         while assigned(TempSymbol) do begin
          case TempSymbol^.SymbolType of
           Symbols.tstFunction,Symbols.tstProcedure:begin
            if (([tpaVirtual,tpaDynamic]*TempSymbol^.ProcedureAttributes)<>[]) and
               (not (tpaOverride in TempSymbol^.ProcedureAttributes)) and
               (Symbol^.Name=TempSymbol^.Name) and
               (CompareProcToProc(Error,SymbolManager,TempSymbol,Symbol)=tcteEqual) then begin
             ParentSymbol:=TempSymbol;
             break;
            end;
           end;
          end;
          TempSymbol:=TempSymbol^.Next;
         end;
        end;
        if assigned(ClassType^.ChildOf) then begin
         ClassType:=ClassType^.ChildOf^.TypeDefinition;
        end else begin
         break;
        end;
       end;
       if assigned(ParentSymbol) then begin
        if assigned(ParentSymbol^.OwnerObjectClass) then begin
         Error.AddWarningCode(168,CorrectSymbolName(Symbol^.Name),CorrectSymbolName(ParentSymbol^.OwnerObjectClass.Symbol^.Name));
        end else begin
         Error.AddWarningCode(168,CorrectSymbolName(Symbol^.Name),'???');
        end;
       end;
      end;
     end;
    end;
    tstEND:begin
     break;
    end;
    else begin
     Scanner.Illegal(Scanner.CurrentToken);
    end;
   end;
  end;
  Scanner.Match(tstEND);
 end;
 ObjectClassName:=OldObjectClassName;

 result^.HasVirtualTable:=true;

 if assigned(result^.ChildOf) then begin
  if not result^.ChildOf^.TypeDefinition^.HasVirtualTable then begin
   Error.AbortCode(90,CorrectSymbolName(result^.ChildOf^.Name));
  end;
 end else begin
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,result);
  Symbol^.Name:='VMT';
  Symbol^.Attributes:=Symbol^.Attributes+[tsaField,tsaInternalField,tsaClassVMT];
  Symbol^.SymbolType:=Symbols.tstVariable;
  Symbol^.TypeDefinition:=SymbolManager.TypePointer;
  Symbol^.Offset:=0;
  Symbol^.TypedConstant:=false;
  Symbol^.TypedTrueConstant:=false;
  Symbol^.TypedConstantReadOnly:=false;
  Symbol^.OwnerType:=result;
  result^.RecordTable.AddSymbol(Symbol,ModuleSymbol,result,true);
 end;

 SymbolManager.AlignRecord(result,LocalSwitches^.Alignment);

 result^.PortabilityDirectives:=ParsePortabilityDirectives;

 result^.NeedTypeInfo:=SymbolManager.TypeDoNeedTypeInfo(result);

 SymbolManager.VariableType:=OldVariableType;
 SymbolManager.CurrentList:=OldList;

 CurrentObjectClass:=OldCurrentObjectClass;
 CurrentParseObjectClass:=OldCurrentParseObjectClass;
end;

function TParser.ParseInterfaceDeclaration(ObjectName:ansistring;IsPacked:boolean):PType;
var Symbol,NewSymbol:PSymbol;
    OldList:TSymbolList;
    OldVariableType:TVariableType;
    I,J:longint;
    NewName:ansistring;
    SymbolAttributes:TSymbolAttributes;
    InterfaceSymbols:array of PSymbol;
//  NewTreeNode:TTreeNode;
begin
 if assigned(CurrentProcedureFunction) then begin
  Error.AbortCode(62);
 end;
 Scanner.Match(Scanner.CurrentToken);
 InterfaceSymbols:=nil;
 OldList:=SymbolManager.CurrentList;
 if Scanner.CurrentToken=tstLeftParen then begin
  Scanner.Match(tstLeftParen);
  while not Scanner.IsEOFOrAbortError do begin
   NewName:=Scanner.ReadIdentifier(nil);
   NewSymbol:=SymbolManager.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
   if assigned(NewSymbol) and (NewSymbol^.SymbolType=Symbols.tstUnit) then begin
    Scanner.Match(tstPeriod);
    NewName:=Scanner.ReadIdentifier(nil);
    NewSymbol:=NewSymbol^.SymbolList.GetSymbol(NewName,ModuleSymbol,CurrentObjectClass);
   end;
   if not assigned(NewSymbol) then begin
    Error.AbortCode(217);
   end else begin
    if (NewSymbol^.SymbolType=Symbols.tstType) and (NewSymbol^.TypeDefinition^.TypeDefinition=ttdInterface) then begin
     J:=length(InterfaceSymbols);
     SetLength(InterfaceSymbols,J+1);
     InterfaceSymbols[J]:=NewSymbol;
    end else begin
     Error.AbortCode(217);
    end;
   end;
   if Scanner.CurrentToken=tstComma then begin
    Scanner.Match(tstComma);
   end else begin
    break;
   end;
  end;
  Scanner.Match(tstRightParen);
 end;
 result:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 result^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
 result^.TypeKind:=TypeKindInterface;
 result^.NeedTypeInfo:=false;
 result^.TypeDefinition:=ttdInterface;
 result^.RecordAlignment:=0;
 result^.RecordPacked:=IsPacked or (LocalSwitches^.Alignment=1);
 result^.ChildOf:=nil;
 SetLength(result^.InterfaceChildOf,length(InterfaceSymbols));
 result^.VirtualIndexCount:=0;
 result^.DynamicIndexCount:=0;
 for I:=0 to length(InterfaceSymbols)-1 do begin
  result^.InterfaceChildOf[I]:=InterfaceSymbols[I];
  if assigned(InterfaceSymbols[I]) then begin
   inc(result^.VirtualIndexCount,InterfaceSymbols[I]^.TypeDefinition^.VirtualIndexCount);
   inc(result^.DynamicIndexCount,InterfaceSymbols[I]^.TypeDefinition^.DynamicIndexCount);
  end;
 end;
 SetLength(InterfaceSymbols,0);

 FillChar(result^.GUID,SizeOf(TGUID),#0);
 if Scanner.CurrentToken=tstLeftBracket then begin
  Scanner.Match(tstLeftBracket);
  if Scanner.CurrentToken=tstStringValue then begin
   result^.GUID:=StringToGUID(HugeStringToAnsiString(Scanner.CurrentString));
  end;
  Scanner.Match(tstStringValue);
  Scanner.Match(tstRightBracket);
 end;
 result^.RecordTable:=TSymbolList.Create(SymbolManager);
 SymbolManager.CurrentList:=result^.RecordTable;
 OldVariableType:=SymbolManager.VariableType;
 SymbolManager.VariableType:=tvtInterfaceField;

 SymbolAttributes:=[tsaOOPPublic];
 while not Scanner.IsEOFOrAbortError do begin
  case Scanner.CurrentToken of
   tstIdentifier:begin
    Error.AbortCode(221);
   end;
   tstPROPERTY:begin
    ParsePropertyField(result);
   end;
   tstFUNCTION,tstPROCEDURE:begin
    Symbol:=ParseProcedure(true,[tpaInterface]);
    if Error.DoAbort then begin
     exit;
    end;
    Symbol^.MethodOfType:=result;
    Symbol^.Attributes:=(Symbol^.Attributes-OOPSymbolAttribute)+SymbolAttributes;
    Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL,tstDYNAMIC,tstOVERRIDE]);
    while Scanner.CurrentToken in [tstVIRTUAL,tstABSTRACT,tstDYNAMIC,tstOVERRIDE] do begin
     case Scanner.CurrentToken of
      tstVIRTUAL:begin
       Scanner.Match(tstVIRTUAL);
       Scanner.Match(tstSEPARATOR);
      end;
      tstABSTRACT:begin
       Scanner.Match(tstABSTRACT);
       Scanner.Match(tstSEPARATOR);
      end;
      tstDYNAMIC:begin
       Scanner.Match(tstDYNAMIC);
       Scanner.Match(tstSEPARATOR);
      end;
      tstOVERRIDE:begin
       Scanner.Match(tstOVERRIDE);
       Scanner.Match(tstSEPARATOR);
      end;
     end;
    end;
    Symbol^.PortabilityDirectives:=Symbol^.PortabilityDirectives+ParsePortabilityDirectives;
    Scanner.CheckForDirectives([tstABSTRACT,tstVIRTUAL,tstDYNAMIC,tstOVERRIDE]);
    Symbol^.VirtualIndex:=result^.VirtualIndexCount;
    inc(result^.VirtualIndexCount);
   end;
   tstEND:begin
    break;
   end;
   else begin
    Scanner.Illegal(Scanner.CurrentToken);
   end;
  end;
 end;
 Scanner.Match(tstEND);

 result^.PortabilityDirectives:=ParsePortabilityDirectives;

 result^.NeedTypeInfo:=SymbolManager.TypeDoNeedTypeInfo(result);

 SymbolManager.VariableType:=OldVariableType;
 SymbolManager.CurrentList:=OldList;
end;

procedure TParser.ParseParameterList(var Symbol:PSymbol;var SymbolParameter:TSymbolList;Bracket,AllowParameterConstant:boolean;var ParameterSuffix:ansistring);
var ParameterSymbol,StartSymbol,LastSymbol,NextSymbol,CurrentSymbol:PSymbol;
    AType:PType;
    VariableType:TVariableType;    
    TypeName:ansistring;
begin
 SymbolParameter:=TSymbolList.Create(SymbolManager);
 if Bracket then begin
  Scanner.Match(tstLeftBracket);
  if Scanner.CurrentToken=tstRightBracket then begin
   Scanner.Match(tstRightBracket);
   exit;
  end;
 end else begin
  Scanner.Match(tstLeftParen);
  if Scanner.CurrentToken=tstRightParen then begin
   Scanner.Match(tstRightParen);
   SymbolManager.PushSymbolList(SymbolParameter);
   exit;
  end;
 end;
 repeat
  case Scanner.CurrentToken of
   tstVAR:begin
    Scanner.Match(tstVAR);
    VariableType:=tvtParameterVariable;
   end;
   tstOUT:begin
    Scanner.Match(tstOUT);
    VariableType:=tvtParameterResult;
   end;
   tstCONST:begin
    Scanner.Match(tstCONST);
    VariableType:=tvtParameterConstant;
   end;
   else begin
    VariableType:=tvtParameterValue;
   end;
  end;

  StartSymbol:=nil;
  LastSymbol:=nil;
  repeat
   CurrentSymbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   if not assigned(StartSymbol) then begin
    StartSymbol:=CurrentSymbol;
   end;
   if assigned(LastSymbol) then begin
    LastSymbol^.Next:=CurrentSymbol;
   end;
   LastSymbol:=CurrentSymbol;
   CurrentSymbol^.Name:=Scanner.ReadIdentifier(@CurrentSymbol^.OriginalCaseName);
   CurrentSymbol^.OverloadedName:=ModuleName+tpsParameter+CurrentSymbol^.Name+tpsParameter+INTTOSTR(ParameterNumber);
   HashSymbol(CurrentSymbol);
   inc(ParameterNumber);
   if Scanner.CurrentToken=tstCOMMA then begin
    Scanner.Match(tstCOMMA);
   end else begin
    break;
   end;
  until (Scanner.CurrentToken<>tstIdentifier) or Scanner.IsEOFOrAbortError;

  if Scanner.CurrentToken=tstColon then begin
   Scanner.Match(tstColon);
   TypeName:=Scanner.CurrentIdentifier;
   Scanner.CheckForDirectives([tstOPENSTRING]);
   Scanner.AllowedDirectives:=Scanner.AllowedDirectives+[tstOPENSTRING];
   AType:=ParseTypeDefinition('',true);
   Scanner.AllowedDirectives:=Scanner.AllowedDirectives-[tstOPENSTRING];
   if not assigned(AType) then begin
    Error.InternalError(200605181006002);
    exit;
   end;
   TypeName:=AType.OwnerModule.Name+'_'+TypeName;
  end else begin
   AType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   AType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   AType^.TypeKind:=TypeKindUnknown;
   AType^.NeedTypeInfo:=false;
   TypeName:='EMPTY';
// TypeName:=tpsOverload;
   AType^.TypeDefinition:=ttdEmpty;
  end;

  if (Scanner.CurrentToken=tstEQUAL) and AllowParameterConstant then begin
   Scanner.Match(tstEQUAL);
   if assigned(StartSymbol) then begin
    if assigned(StartSymbol.Next) then begin
     Error.AbortCode(35);
    end else begin
     StartSymbol^.Attributes:=StartSymbol^.Attributes+[tsaParameterWithDefault];
     ParseDefaultParameterTypedConstantDeclaration(AType,StartSymbol);
    end;
   end else begin
    Error.AbortCode(511);
    exit;
   end;
  end;

  CurrentSymbol:=StartSymbol;
  while assigned(CurrentSymbol) do begin
   NextSymbol:=CurrentSymbol^.Next;
   ParameterSymbol:=CurrentSymbol;
   ParameterSymbol^.Next:=nil;
   ParameterSymbol^.SymbolType:=Symbols.tstVariable;
   ParameterSymbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
   ParameterSymbol^.VariableType:=VariableType;
   ParameterSymbol^.LocalProcSymbol:=CurrentProcedureFunction;
   ParameterSymbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
   ParameterSymbol^.AbsoluteReference:=false;
   ParameterSymbol^.Alias:=nil;
   ParameterSymbol^.Offset:=0;
   ParameterSymbol^.TypeDefinition:=AType;
   ParameterSymbol^.TypedConstant:=false;
   ParameterSymbol^.TypedTrueConstant:=false;
   ParameterSymbol^.TypedConstantReadOnly:=false;
   if MakeSymbolsPublic then begin
    ParameterSymbol^.Attributes:=ParameterSymbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
   end;
   SymbolParameter.AddSymbol(ParameterSymbol,ModuleSymbol,CurrentObjectClass);
   if not ((VariableType in [tvtParameterVariable,tvtParameterConstant,tvtParameterResult]) or assigned(ParameterSymbol^.TypeDefinition)) then begin
    if Error.Errors then begin
     Error.DoAbort:=true;
    end else begin
     Error.InternalError(200605181006003);
    end;
    exit;
   end;
   ParameterSuffix:=ParameterSuffix+'_PARAMETER_'+TypeName;
   CurrentSymbol:=NextSymbol;
  end;
  if Scanner.CurrentToken=tstSEPARATOR then begin
   Scanner.Match(tstSEPARATOR);
  end else begin
   break;
  end;
 until Scanner.IsEOFOrAbortError;
 SymbolManager.PushSymbolList(SymbolParameter);
 if Bracket then begin
  Scanner.Match(tstRightBracket);
 end else begin
  Scanner.Match(tstRightParen);
 end;
 HashSymbol(Symbol);
end;

procedure TParser.ParseProcedureType(var Symbol:PSymbol;RealDefinition:boolean);
begin
 if assigned(Symbol) then begin
  if (ProcedureCallingConventionAttributesEx*Symbol^.ProcedureAttributes)=[] then begin
   Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaRegister];
  end;
  Scanner.CheckForDirectives([tstASSEMBLER,tstCDECL,tstEXPORT,tstEXTERNAL,tstFAR,tstFASTCALL,tstINTERRUPT,tstNEAR,tstOVERLOAD,tstPASCAL,tstREGISTER,tstSAFECALL,tstSTDCALL,tstLOCAL]);
  while (Scanner.CurrentToken in [tstASSEMBLER,tstCDECL,tstEXPORT,tstEXTERNAL,tstFAR,tstFASTCALL,tstINTERRUPT,tstNEAR,tstOVERLOAD,tstPASCAL,tstREGISTER,tstSAFECALL,tstSTDCALL,tstLOCAL]) and not Scanner.IsEOFOrAbortError do begin
   case Scanner.CurrentToken of
    tstEXPORT,tstASSEMBLER:begin
     if RealDefinition then begin
      Scanner.Match(Scanner.CurrentToken);
      Scanner.Match(tstSeparator);
     end else begin
      Scanner.Illegal(Scanner.CurrentToken);
     end;
    end;
    tstFAR,tstNEAR:begin
     Scanner.Match(Scanner.CurrentToken);
     Scanner.Match(tstSeparator);
    end;
    tstOVERLOAD:begin
     if RealDefinition then begin
      Scanner.Match(tstOVERLOAD);
      Scanner.Match(tstSeparator);
      Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaOverload];
     end else begin
      Scanner.Illegal(Scanner.CurrentToken);
     end;
    end;
    tstINTERRUPT:begin
     Scanner.Match(tstINTERRUPT);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaInterrupt];
    end;
    tstINLINE:begin
     Scanner.Match(tstINLINE);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaInline];
    end;
    tstREGISTER:begin
     Scanner.Match(tstREGISTER);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaRegister,tpaCALLCONV];
    end;
    tstSTDCALL:begin
     Scanner.Match(tstSTDCALL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaSTDCALL,tpaCALLCONV];
    end;
    tstPASCAL:begin
     Scanner.Match(tstPASCAL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaPASCAL,tpaCALLCONV];
    end;
    tstCDECL:begin
     Scanner.Match(tstCDECL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaCDECL,tpaCALLCONV];
    end;
    tstSAFECALL:begin
     Scanner.Match(tstSAFECALL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaSAFECALL,tpaCALLCONV];
    end;
    tstFASTCALL:begin
     Scanner.Match(tstFASTCALL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=(Symbol.ProcedureAttributes-ProcedureCallingConventionAttributes)+[tpaFASTCALL,tpaCALLCONV];
    end;
    tstLOCAL:begin
     Scanner.Match(tstLOCAL);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaLOCAL];
    end;
    tstVARARGS:begin
     Scanner.Match(tstVARARGS);
     Scanner.Match(tstSeparator);
     Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaVARARGS];
    end;
    tstEXTERNAL:begin
     Scanner.Match(tstEXTERNAL);
     if RealDefinition then begin
      if Scanner.CurrentToken=tstStringValue then begin
       Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaExternal];
       Symbol^.Attributes:=Symbol.Attributes+[tsaExtern,tsaUsed];
       Symbol^.LibraryName:=HugeStringToAnsiString(Scanner.CurrentString);
       HashSymbol(Symbol);
       Scanner.Match(tstStringValue);
       Scanner.CheckForDirectives([tstNAME]);
       if Scanner.CurrentToken=tstNAME then begin
        Scanner.Match(tstNAME);
        if Scanner.CurrentToken=tstStringValue then begin
         Symbol^.ExternalName:=HugeStringToAnsiString(Scanner.CurrentString);
         HashSymbol(Symbol);
        end;
        Scanner.Match(tstStringValue);
       end;
      end else begin
       Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaUnderscore];
      end;
      Scanner.Match(tstSeparator);
     end else begin
      Scanner.Illegal(Scanner.CurrentToken);
     end;
    end;
    tstStringValue:begin
     if RealDefinition then begin
      Symbol^.OverloadedName:=HugeStringToAnsiString(Scanner.CurrentString);
      HashSymbol(Symbol);
      Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes-[tpaUnderscore];
     end else begin
      Scanner.Illegal(Scanner.CurrentToken);
     end;
     Scanner.Match(tstStringValue);
     Scanner.Match(tstSeparator);
    end;
    else begin
     break;
    end;
   end;
   Scanner.CheckForDirectives([tstASSEMBLER,tstCDECL,tstEXPORT,tstEXTERNAL,tstFAR,tstFASTCALL,tstINTERRUPT,tstNEAR,tstOVERLOAD,tstPASCAL,tstREGISTER,tstSAFECALL,tstSTDCALL]);
  end;
  Symbol^.PortabilityDirectives:=ParsePortabilityDirectives;
 end;
end;

procedure TParser.CheckProcedureType(var Symbol:PSymbol);
{var RegisterCount:longint;
    ParameterSymbol:PSymbol;}
begin
{if assigned(Symbol) then begin
  if (tpaRegister in Symbol^.ProcedureAttributes) and not (tpaInline in Symbol^.ProcedureAttributes) then begin
   RegisterCount:=0;
   if assigned(Symbol^.Parameter) then begin
    ParameterSymbol:=Symbol^.Parameter^.First;
    while assigned(ParameterSymbol) and (RegisterCount<3) do begin
     ParameterSymbol^.VariableType:=tvtTemporaryVariable;
     dec(Symbol^.ParameterSize,4);
     inc(RegisterCount);
     ParameterSymbol:=ParameterSymbol^.Next;
    end;
   end;
  end;
 end;}
end;

function TParser.ParseProcedure(ParseHeader:boolean;ProcedureAttributes:TProcedureAttributes):PSymbol;
var Parent:PSymbol;
    ObjectClassSymbolList:TSymbolList;
    OldObjectName,OldName,Name,ParameterSuffix,MethodName,OriginalCaseName,s:ansistring;
    Method,MethodSymbol,SearchSymbol,Symbol,SymbolA,SymbolB,ResultSymbol,SelfSymbol,
    OldCurrentMethod,OldCurrentProcedureFunction:PSymbol;
    OldCurrentObjectClass:PType;
    AType:PType;
    BlockNode:TTreeNode;
    OldVariableType:TVariableType;
    OldList,OldProcList,LocalList:TSymbolList;
    OldAssemblerMode,ForwardProc,OK:boolean;
    i:longint;
begin
 result:=nil;

 if SymbolManager.LexicalScopeLevel=0 then begin
  SymbolManager.LexicalScopeLevelCount:=0;
 end;

 inc(SymbolManager.LexicalScopeLevel);

 if SymbolManager.LexicalScopeLevelCount<SymbolManager.LexicalScopeLevel then begin
  SymbolManager.LexicalScopeLevelCount:=SymbolManager.LexicalScopeLevel;
 end;

 OldVariableType:=SymbolManager.VariableType;

 Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
 Symbol.ProcedureAttributes:=Symbol.ProcedureAttributes+ProcedureAttributes;
 if MakeSymbolsPublic then begin
  Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
 end;
 Symbol^.LexicalScopeLevel:=SymbolManager.LexicalScopeLevel-1;
 Symbol^.ProcedureLevel:=SymbolManager.LexicalScopeLevel;

 if Scanner.CurrentToken=tstCLASS then begin
  Scanner.Match(tstCLASS);
  Symbol.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaClassProcedure];
 end;

 case Scanner.CurrentToken of
  tstFUNCTION:begin
   Scanner.Match(tstFUNCTION);
   Symbol^.SymbolType:=Symbols.tstFunction;
  end;
  tstPROCEDURE:begin
   Scanner.Match(tstPROCEDURE);
   Symbol^.SymbolType:=Symbols.tstProcedure;
  end;
  tstCONSTRUCTOR:begin
   Scanner.Match(tstCONSTRUCTOR);
   Symbol^.SymbolType:=Symbols.tstProcedure;
   Symbol.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaConstructor];
  end;
  tstDESTRUCTOR:begin
   Scanner.Match(tstDESTRUCTOR);
   Symbol^.SymbolType:=Symbols.tstProcedure;
   Symbol.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaDestructor];
  end;
  else begin
   Error.AbortCode(512);
  end;
 end;

 if Error.DoAbort then begin
  exit;
 end;

 OldCurrentObjectClass:=CurrentObjectClass;
 
 OldName:=Scanner.ProcedureName;
 OriginalCaseName:='';
 Name:=Scanner.ReadIdentifier(@OriginalCaseName);
 if length(Scanner.ProcedureName)>0 then begin
  Scanner.ProcedureName:=Scanner.ProcedureName+'_NESTED_'+Name;
 end else begin
  Scanner.ProcedureName:=Name;
 end;

 if (Scanner.CurrentToken=tstPeriod) and not ParseHeader then begin
  Scanner.Match(tstPeriod);
  MethodSymbol:=SymbolManager.GetSymbol(Scanner.ProcedureName,ModuleSymbol,CurrentObjectClass);
  if not assigned(MethodSymbol) then begin
   Error.AbortCode(19);
   exit;
  end else if not assigned(MethodSymbol^.TypeDefinition) then begin
   Error.AbortCode(19);
   exit;
  end else if not (MethodSymbol^.TypeDefinition^.TypeDefinition in [ttdObject,ttdClass]) then begin
   Error.AbortCode(19);
   exit;
  end;
  ObjectClassSymbolList:=MethodSymbol^.TypeDefinition^.RecordTable;
  SymbolManager.PushSymbolList(ObjectClassSymbolList);
  MethodName:=Scanner.ReadIdentifier(@OriginalCaseName);
  Method:=ObjectClassSymbolList.GetSymbol(MethodName,ModuleSymbol,CurrentObjectClass);
  if not assigned(Method) then begin
   Error.AbortCode(101);
   exit;
  end;
 // Method^.OverloadedName:=Scanner.ProcedureName+'_'+Method^.OverloadedName;
  HashSymbol(Method);
{ Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+(Method^.ProcedureAttributes*[tpaVirtual,tpaDynamic]);
  Symbol^.VirtualIndex:=Method^.VirtualIndex;}
 end else begin
  MethodSymbol:=nil;
  Method:=nil;
  ObjectClassSymbolList:=nil;
 end;
 Symbol^.ProcedureName:=Scanner.ProcedureName;
 Symbol^.Name:=Name;
 Symbol^.OriginalCaseName:=OriginalCaseName;
 HashSymbol(Symbol);

 if Scanner.CurrentToken=tstLeftParen then begin
  Symbol^.ParameterSuffix:='';
  ParseParameterList(Symbol,Symbol^.Parameter,false,true,Symbol^.ParameterSuffix);
 end;
 if not (assigned(Symbol^.Parameter) and assigned(Symbol^.Parameter.First)) then begin
  Symbol^.Parameter:=nil;
  Symbol^.OverloadedName:=Scanner.ProcedureName;
  HashSymbol(Symbol);
 end;
 if Symbol^.SymbolType=Symbols.tstFunction then begin
  Scanner.Match(tstColon);
  ParameterSuffix:=Scanner.CurrentIdentifier;
  Symbol^.ReturnType:=ParseTypeDefinition('');
  if not assigned(Symbol^.ReturnType) then begin
   Error.InternalError(200605181006004);
   exit;
  end;
  Symbol^.ParameterSuffix:=Symbol^.ParameterSuffix+'_RESULT_'+Symbol^.ReturnType^.OwnerModule^.Name+'_'+ParameterSuffix;
 end else begin
  Symbol^.ReturnType:=nil;
 end;

 if assigned(Method) then begin
  OK:=false;
  s:=Method^.Name;
  Method:=SymbolManager.SearchProcedureSymbol(Symbol,Method,OK);
  if (not (OK and assigned(Method))) or (Method^.OwnerObjectClass<>MethodSymbol^.TypeDefinition) then begin
   Error.AbortCode(265,CorrectSymbolName(s));
   exit;
  end;
  Method^.Attributes:=Method^.Attributes+[tsaMethodDefined];
  HashSymbol(Method);
 end;

 if assigned(Method) then begin
  CurrentObjectClass:=Method^.OwnerObjectClass;
  Symbol^.MethodSymbol:=Method;
  Method^.MethodSymbol:=Symbol;
  Symbol^.Attributes:=Symbol^.Attributes+[tsaMethod];
  Scanner.ProcedureName:=Scanner.ProcedureName+'_'+Method^.Name;
  Symbol^.ProcedureName:=Scanner.ProcedureName;
  Symbol^.Name:=Method^.Name;
  Symbol^.OriginalCaseName:=Method^.OriginalCaseName;
  HashSymbol(Symbol);
 end else if length(ObjectClassName)>0 then begin
  Scanner.ProcedureName:=ObjectClassName+'_'+Symbol^.Name;
  Symbol^.ProcedureName:=Scanner.ProcedureName;
 end;

 Symbol^.OverloadedName:=Scanner.ProcedureName+Symbol^.ParameterSuffix;
 HashSymbol(Symbol);

 if Scanner.CurrentToken=tstSeparator then begin
  Scanner.Match(tstSeparator);
 end;
 if Error.DoAbort then begin
  exit;
 end;

 ParseProcedureType(Symbol,true);
 if Error.DoAbort then begin
  exit;
 end;

 if assigned(CurrentParseObjectClass) then begin
  SearchSymbol:=CurrentParseObjectClass.RecordTable.GetSymbol(Scanner.ProcedureName,ModuleSymbol,CurrentObjectClass,true);
  if assigned(SearchSymbol) then begin
   OK:=false;
   SearchSymbol:=SymbolManager.SearchProcedureSymbol(Symbol,SearchSymbol,OK);
   if not OK then begin
    if assigned(SearchSymbol) and (tpaOverload in Symbol^.ProcedureAttributes) then begin
     SearchSymbol^.NextOverloaded:=Symbol;
     Symbol^.NextOverloaded:=nil;
    end else begin
     Error.AbortCode(265,CorrectSymbolName(Symbol^.Name));
    end;
   end;
  end;
  Symbol^.NextOverloaded:=nil;
  dec(SymbolManager.LexicalScopeLevel);
  Symbol^.OwnerObjectClass:=CurrentParseObjectClass;
  Symbol^.OwnerType:=CurrentParseObjectClass;
  SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass,false,false);
  inc(SymbolManager.LexicalScopeLevel);
 end else begin
   if assigned(SymbolManager.CurrentList) then begin
   SearchSymbol:=SymbolManager.CurrentList.GetSymbol(Scanner.ProcedureName,ModuleSymbol,CurrentObjectClass);
  end else begin
   SearchSymbol:=nil;
  end;
  if (not assigned(SearchSymbol)) or
     (SearchSymbol^.LexicalScopeLevel<>(SymbolManager.LexicalScopeLevel-1)) or
     ((SearchSymbol^.OwnerModule<>ModuleSymbol) and not (tpaOverload in Symbol^.ProcedureAttributes)) then begin
   Symbol^.NextOverloaded:=nil;
   dec(SymbolManager.LexicalScopeLevel);
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
   inc(SymbolManager.LexicalScopeLevel);
  end else begin
   case SearchSymbol^.SymbolType of
    Symbols.tstType:begin
     if assigned(SearchSymbol^.TypeDefinition) and not (SearchSymbol^.TypeDefinition^.TypeDefinition in [ttdObject,ttdClass]) then begin
      Error.AbortCode(3,CorrectSymbolName(SearchSymbol^.Name));
     end;
    end;
    Symbols.tstProcedure,Symbols.tstFunction:begin
     if (([tpaForward,tpaOverload]*SearchSymbol^.ProcedureAttributes)=[tpaForward]) and not
        ((tpaOverload in Symbol^.ProcedureAttributes) or
         (tpaOverload in SearchSymbol^.ProcedureAttributes)) then begin
      ForwardProc:=true; //assigned(SearchSymbol^.Parameter) and not assigned(Symbol^.Parameter);
     end else begin
      ForwardProc:=false;
     end;
     if not ForwardProc then begin
      while ((SearchSymbol^.OverloadedNameHash<>Symbol^.OverloadedNameHash) or (SearchSymbol^.OverloadedName<>Symbol^.OverloadedName)) and assigned(SearchSymbol^.NextOverloaded) do begin
       SearchSymbol:=SearchSymbol^.NextOverloaded;
      end;
     end;
     if ((SearchSymbol^.OverloadedNameHash=Symbol^.OverloadedNameHash) and
         (SearchSymbol^.OverloadedName=Symbol^.OverloadedName)) or
         ForwardProc then begin
      if tpaForward in SearchSymbol^.ProcedureAttributes then begin
       SearchSymbol^.ForwardSymbol:=Symbol;
       Symbol^.ForwardSymbol:=SearchSymbol;
       if tpaCALLCONV in Symbol^.ProcedureAttributes then begin
        if (ProcedureCallingConventionAttributes*Symbol^.ProcedureAttributes)<>(ProcedureCallingConventionAttributes*SearchSymbol^.ProcedureAttributes) then begin
         Error.AbortCode(513,CorrectSymbolName(Symbol^.Name));
        end;
       end else if (tpaCALLCONV in Symbol^.ProcedureAttributes) and not (tpaCALLCONV in SearchSymbol^.ProcedureAttributes) then begin
        SearchSymbol^.ProcedureAttributes:=(SearchSymbol^.ProcedureAttributes-ProcedureCallingConventionAttributesEx)+(Symbol^.ProcedureAttributes*ProcedureCallingConventionAttributesEx);
        CheckProcedureType(SearchSymbol);
       end else begin
        if (ProcedureCallingConventionAttributesEx*SearchSymbol^.ProcedureAttributes)<>[] then begin
         Symbol^.ProcedureAttributes:=(Symbol^.ProcedureAttributes-ProcedureCallingConventionAttributesEx)+(SearchSymbol^.ProcedureAttributes*ProcedureCallingConventionAttributesEx);
        end;
       end;
      end;
      if (assigned(SearchSymbol^.Parameter) and not assigned(Symbol^.Parameter)) and not
         ((tpaOverload in Symbol^.ProcedureAttributes) or
          (tpaOverload in SearchSymbol^.ProcedureAttributes)) then begin
       SymbolManager.PopSymbolList(Symbol^.Parameter);
       Symbol^.Parameter:=SearchSymbol^.Parameter;
       Symbol:=SearchSymbol;
       if assigned(Symbol^.Parameter) then begin
        SymbolManager.PushSymbolList(Symbol^.Parameter);
       end;
      end else begin
       if not ((tpaOverload in Symbol^.ProcedureAttributes) or
               (tpaOverload in SearchSymbol^.ProcedureAttributes)) then begin
        if assigned(Symbol^.Parameter) and not assigned(SearchSymbol^.Parameter) then begin
         Error.AbortCode(514,CorrectSymbolName(Symbol^.Name));
        end else if assigned(Symbol^.Parameter) and assigned(SearchSymbol^.Parameter) then begin
         SymbolA:=Symbol^.Parameter.First;
         SymbolB:=SearchSymbol^.Parameter.First;
         while assigned(SymbolA) and assigned(SymbolB) do begin
          if (SymbolA^.Name<>SymbolB^.Name) or (SymbolA^.SymbolType<>SymbolB^.SymbolType) or
             ((SymbolA^.SymbolType in [Symbols.tstVariable]) and
              ((SymbolA^.VariableType<>SymbolB^.VariableType) or
               (((SymbolA^.VariableType=SymbolB^.VariableType) and (SymbolA^.VariableType=tvtParameterValue)) and
                (SymbolA^.TypeDefinition<>SymbolB^.TypeDefinition)) or
               ((assigned(SymbolA^.TypeDefinition) and assigned(SymbolB^.TypeDefinition)) and
                not SymbolManager.SameTypes(SymbolA^.TypeDefinition,SymbolB^.TypeDefinition)
               )
              )
             ) then begin
           Error.AbortCode(514,CorrectSymbolName(Symbol^.Name));
           break;
          end;
          SymbolA:=SymbolA^.Next;
          SymbolB:=SymbolB^.Next;
         end;
         if assigned(SymbolA)<>assigned(SymbolB) then begin
          Error.AbortCode(514,CorrectSymbolName(Symbol^.Name));
         end;
         if (Symbol^.OverloadedNameHash<>SearchSymbol^.OverloadedNameHash) or
            (Symbol^.OverloadedName<>SearchSymbol^.OverloadedName) then begin
          Error.AbortCode(514,CorrectSymbolName(Symbol^.Name));
         end;
        end;
       end;
       SymbolManager.PopSymbolList(Symbol^.Parameter);
       FreeAndNil(Symbol^.Parameter);
       Symbol:=SearchSymbol;
       if assigned(Symbol^.Parameter) then begin
        SymbolManager.PushSymbolList(Symbol^.Parameter);
       end;
      end;
     end else begin
      if tpaOverload in Symbol.ProcedureAttributes then begin
       SearchSymbol^.NextOverloaded:=Symbol;
       Symbol^.NextOverloaded:=nil;
      end else begin
       Error.AbortCode(3,CorrectSymbolName(Symbol^.Name));
      end;
     end;
    end;
    else begin
     Error.AbortCode(3,CorrectSymbolName(Symbol^.Name));
    end;
   end;
  end;
 end;
 if Error.DoAbort then begin
  exit;
 end;

{if (tpaEXTERNAL in Symbol^.ProcedureAttributes) or not ParseHeader then begin
  CheckProcedureType(Symbol);
 end;{}

 if tpaCALLCONV in Symbol^.ProcedureAttributes then begin
  CheckProcedureType(Symbol);
 end;

 if tpaEXTERNAL in Symbol^.ProcedureAttributes then begin
{ Symbol^.OverloadedName:=Symbol^.OverloadedName+'_S_'+INTTOSTR(Symbol^.ParameterSize);
  HashSymbol(Symbol);}
 end;

 if Error.DoAbort then begin
  exit;
 end;

 if ParseHeader then begin
  Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaForward];
 end else begin
  Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes-[tpaForward];
 end;

 OldCurrentMethod:=CurrentMethod;
 OldCurrentProcedureFunction:=CurrentProcedureFunction;
 CurrentMethod:=Method;
 CurrentProcedureFunction:=Symbol;

 if assigned(Symbol^.Parameter) then begin
  SymbolA:=Symbol^.Parameter.First;
  while assigned(SymbolA) do begin
   if SymbolA^.SymbolType=Symbols.tstVariable then begin
    SymbolA^.LocalProcSymbol:=CurrentProcedureFunction;
   end;
   SymbolA:=SymbolA^.Next;
  end;
 end;

 if tpaForward in Symbol.ProcedureAttributes then begin
  Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
 end else begin
  OldList:=SymbolManager.CurrentList;
  LocalList:=TSymbolList.Create(SymbolManager);
  SymbolManager.PushSymbolList(LocalList);
  SymbolManager.CurrentList:=LocalList;
  LocalList.Nested:=SymbolManager.ProcList;
  LocalList.ProcSymbol:=Symbol;
  OldProcList:=SymbolManager.ProcList;
  SymbolManager.ProcList:=LocalList;

  if not (ParseHeader or (([tpaExternal]*Symbol^.ProcedureAttributes)<>[])) then begin
   if SymbolManager.LexicalScopeLevel<=1 then begin
    CodeGenerator.BeginRootNestedProc;
   end;
   if not ((tpaUnderscore in Symbol^.ProcedureAttributes) or (tpaExternal in Symbol^.ProcedureAttributes)) then begin
    SymbolManager.VariableType:=tvtLocal;
    if assigned(Symbol^.ReturnType) then begin
     ResultSymbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     ResultSymbol^.Name:='RESULT';
     if not GlobalSwitches^.ExtendedSyntax then begin
      ResultSymbol^.Attributes:=ResultSymbol^.Attributes+[tsaHidden];
     end;
     ResultSymbol^.OverloadedName:=ResultSymbol^.Name;
     HashSymbol(ResultSymbol);
     ResultSymbol^.SymbolType:=Symbols.tstVariable;
     ResultSymbol^.TypeDefinition:=Symbol^.ReturnType;
     ResultSymbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
     ResultSymbol^.VariableType:=tvtResult;
     ResultSymbol^.LocalProcSymbol:=CurrentProcedureFunction;
     ResultSymbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
     ResultSymbol^.Alias:=nil;
     ResultSymbol^.AbsoluteReference:=false;
     ResultSymbol^.TypedConstant:=false;
     ResultSymbol^.TypedTrueConstant:=false;
     ResultSymbol^.TypedConstantReadOnly:=false;
     ResultSymbol^.PortabilityDirectives:=[];
     Symbol^.ResultSymbol:=ResultSymbol;
     SymbolManager.CurrentList.AddSymbol(ResultSymbol,ModuleSymbol,CurrentObjectClass);
    end else begin
     Symbol^.ResultSymbol:=nil;
    end;
    if assigned(Symbol^.OwnerObjectClass) then begin
     SelfSymbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     SelfSymbol^.Name:='SELF';
     SelfSymbol^.OverloadedName:=SelfSymbol^.Name;
     SelfSymbol^.Attributes:=SelfSymbol^.Attributes+[tsaMapped];
     HashSymbol(SelfSymbol);
     SelfSymbol^.DeclarationUsed:=true;
     SelfSymbol^.SymbolType:=Symbols.tstVariable;
     SelfSymbol^.TypeDefinition:=Symbol^.OwnerObjectClass;
     SelfSymbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
     case Symbol^.OwnerObjectClass^.TypeDefinition of
      ttdOBJECT:begin
       SelfSymbol^.VariableType:=tvtObjectInstanceSelf;
       SelfSymbol^.TypeDefinition:=Symbol^.OwnerObjectClass;
      end;
      ttdCLASS:begin
       if tpaClassProcedure in Symbol^.ProcedureAttributes then begin
        SelfSymbol^.VariableType:=tvtClassSelf;
        SelfSymbol^.TypeDefinition:=Symbol^.OwnerObjectClass^.ClassOfType;
       end else begin
        SelfSymbol^.VariableType:=tvtClassInstanceSelf;
        SelfSymbol^.TypeDefinition:=Symbol^.OwnerObjectClass;
       end;
      end;
      else begin
       Error.InternalError(201304052355000);
      end;
     end;
     SelfSymbol^.LocalProcSymbol:=CurrentProcedureFunction;
     SelfSymbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
     SelfSymbol^.Alias:=nil;
     SelfSymbol^.AbsoluteReference:=false;
     SelfSymbol^.TypedConstant:=false;
     SelfSymbol^.TypedTrueConstant:=false;
     SelfSymbol^.TypedConstantReadOnly:=false;
     SelfSymbol^.PortabilityDirectives:=[];
     Symbol^.SelfSymbol:=SelfSymbol;
     SymbolManager.CurrentList.AddSymbol(SelfSymbol,ModuleSymbol,CurrentObjectClass);
    end else begin
     Symbol^.SelfSymbol:=nil;
    end;
    ParseHeadBlock(false,false);
    case Scanner.CurrentToken of
     tstASM:begin
      Symbol^.ProcedureAttributes:=Symbol.ProcedureAttributes+[tpaAssembler];
      if tpaInline in Symbol^.ProcedureAttributes then begin
       AType:=Symbol^.ReturnType;
       Symbol^.ReturnType:=nil;
       BlockNode:=ParseASMBlockStatement;
{      Symbol^.InlineFirst:=BlockNode^.AsmBlock.first;
       Symbol^.InlineRear:=BlockNode^.AsmBlock.Eear;}
       Symbol^.ReturnType:=AType;
      end else begin
       BlockNode:=ParseASMBlockStatement;
      end;
      OldAssemblerMode:=SymbolManager.AssemblerMode;
      SymbolManager.AssemblerMode:=true;
      OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
      OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
      OptimizerHighLevel.OptimizeTree(BlockNode);
      SymbolManager.AssemblerMode:=OldAssemblerMode;
     end;
     tstBEGIN:begin
      BlockNode:=ParseMAINBlock;
      OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
      OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
      OptimizerHighLevel.OptimizeTree(BlockNode);
     end;
     else begin
      Scanner.Match(tstBEGIN);
      exit;
     end;
    end;

    FinishCheckSymbols(Symbol,LocalList);
   end;

   OldObjectName:=Symbol^.OverloadedName;
   if assigned(Method) then begin
    Symbol^.OverloadedName:=Method^.OverloadedName;
    HashSymbol(Symbol);
   end;
   TreeManager.Dump(BlockNode,CorrectSymbolName(Symbol^.Name));
   if not Error.Errors then begin
    Symbol^.LexicalScopeLevelCount:=SymbolManager.LexicalScopeLevelCount;
    CodeGenerator.GenerateProc(Symbol,BlockNode);
   end;
   Symbol^.OverloadedName:=OldObjectName;
   HashSymbol(Symbol);
   Scanner.Match(tstSeparator);
   if SymbolManager.LexicalScopeLevel<=1 then begin
    CodeGenerator.EndRootNestedProc;
   end;
  end;

  SymbolManager.ProcList:=OldProcList;
  SymbolManager.PopSymbolList(LocalList);
  FreeAndNil(LocalList);
  SymbolManager.CurrentList:=OldList;
 end;

 SymbolManager.PopSymbolList(Symbol^.Parameter);

 Scanner.ProcedureName:=OldName;

 dec(SymbolManager.LexicalScopeLevel);

 if SymbolManager.LexicalScopeLevel=0 then begin
  SymbolManager.LexicalScopeLevelCount:=0;
 end;

 SymbolManager.VariableType:=OldVariableType;

 if assigned(Method) and not ParseHeader then begin
  SymbolManager.PopSymbolList(ObjectClassSymbolList);
 end;

 CurrentMethod:=OldCurrentMethod;
 CurrentObjectClass:=OldCurrentObjectClass;
 CurrentProcedureFunction:=OldCurrentProcedureFunction;

 result:=Symbol;
end;

function TParser.ParseTypeDefinition(const Name:ansistring;AllowOpenArray:boolean=false):PType;
var NewTreeNode:TTreeNode;
    RecordSize,Value,LowerValue,HigherValue:longint;
    StartType,CurrentType:PType;
    Symbol:PSymbol;
    IsPacked,IsUnique,CanBeConstant,First:boolean;
    LowerValue64Bit,HigherValue64Bit:int64;
    LowerFloatValue,HigherFloatValue:extended;
    Token,OldToken:TScannerToken;
    AType:PType;
    IntegerList:TIntegerList;
begin
 OldToken:=Scanner.CurrentToken;
 IsPacked:=false;
 IsUnique:=false;
 CanBeConstant:=false;
 while Scanner.CurrentToken in [tstPACKED,tstTYPE] do begin
  case Scanner.CurrentToken of
   tstPACKED:begin
    Scanner.Match(tstPACKED);
    IsPacked:=true;
   end;
   tstTYPE:begin
    Scanner.Match(tstTYPE);
    IsUnique:=true;
   end;
  end;
 end;
 CurrentType:=nil;
 case Scanner.CurrentToken of
  tstIdentifier:begin
   Symbol:=SymbolManager.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
   if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstConstant) then begin
    CanBeConstant:=true;
   end else begin
    if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
     Scanner.Match(tstIdentifier);
     Scanner.Match(tstPeriod);
     Symbol:=Symbol^.SymbolList.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
    end; 
    if (assigned(Symbol) and (Symbol^.SymbolType<>Symbols.tstType)) or not assigned(Symbol) then begin
     CurrentType:=SymbolManager.TypePointer;
     Error.AbortCode(115);
    end else begin
     Scanner.Match(tstIdentifier);
     if assigned(Symbol) then begin
      if IsUnique then begin
       CurrentType:=SymbolManager.CloneType(Symbol^.TypeDefinition,ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
       CurrentType^.Unique:=true;
      end else begin
       CurrentType:=Symbol^.TypeDefinition;
      end;
     end;
    end;
   end;
  end;
  tstLeftParen:begin
   Scanner.Match(tstLeftParen);
   IntegerList:=TIntegerList.Create;
   try
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeDefinition:=ttdEnumerated;
    CurrentType^.TypeKind:=TypeKindEnumeration;
    CurrentType^.NeedTypeInfo:=true;
    StartType:=CurrentType;
    Value:=0;
    First:=true;
    LowerValue:=0;
    HigherValue:=0;
    while not Scanner.IsEOFOrAbortError do begin
     Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     Symbol^.Name:=Scanner.ReadIdentifier(@Symbol^.OriginalCaseName);
     HashSymbol(Symbol);
     Symbol^.SymbolType:=Symbols.tstConstant;
     Symbol^.ConstantType:=tctOrdinal;
     if Scanner.CurrentToken=tstEqual then begin
      Scanner.Match(tstEqual);
      NewTreeNode:=ParseExpression(false);
      OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
      OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
      OptimizerHighLevel.OptimizeTree(NewTreeNode);
      if NewTreeNode.TreeNodeType=ttntORDCONST then begin
       Value:=NewTreeNode.Value;
      end else begin
       Error.AbortCode(0);
      end;
      NewTreeNode.Destroy;
     end;
     if First then begin
      First:=false;
      LowerValue:=Value;
      HigherValue:=Value;
     end;
     if IntegerList.Find(Value)<0 then begin
      if LowerValue>Value then begin
       LowerValue:=Value;
      end;
      if HigherValue<Value then begin
       HigherValue:=Value;
      end;
      IntegerList.Add(Value);
      CurrentType^.Number:=Value;
      Symbol^.IntValue:=Value;
     end else begin
      Error.AbortCode(515);
     end;
     if MakeSymbolsPublic then begin
      Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
     end;
     SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
     if Scanner.CurrentToken=tstComma then begin
      Scanner.Match(tstComma);
     end else begin
      break;
     end;
     inc(Value);
     CurrentType^.Definition:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
     CurrentType:=CurrentType^.Definition;
     CurrentType^.TypeDefinition:=ttdEnumerated;
     CurrentType^.TypeKind:=TypeKindEnumeration;
     CurrentType^.NeedTypeInfo:=true;
    end;
    CurrentType:=StartType;
    CurrentType.LowerLimit:=LowerValue;
    CurrentType.UpperLimit:=HigherValue;
   finally
    IntegerList.Free;
   end;
   Scanner.Match(tstRightParen);
  end;
  tstSTRING,tstSHORTSTRING,tstLONGSTRING,tstANSISTRING,tstWIDESTRING,tstHUGESTRING,tstUNICODESTRING,tstOPENSTRING:begin
   Token:=Scanner.CurrentToken;
   Scanner.Match(Scanner.CurrentToken);
   if (Token in [tstSTRING,tstSHORTSTRING]) and (Scanner.CurrentToken=tstLeftBracket) then begin
    Value:=255;
    Scanner.Match(tstLeftBracket);
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    if NewTreeNode.TreeNodeType<>ttntORDCONST then begin
     Error.AbortCode(0);
    end else if (NewTreeNode.Value<1) or (NewTreeNode.Value>255) then begin
     Error.AbortCode(119);
     Value:=255;
    end else begin
     Value:=NewTreeNode.Value;
    end;
    NewTreeNode.Destroy;
    Scanner.Match(tstRightBracket);
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdShortString;
    CurrentType^.Length:=Value;
    CurrentType^.OpenString:=false;
   end else if Token=tstOPENSTRING then begin
    if LocalSwitches^.OpenStrings and not LocalSwitches^.LongStrings then begin
     Value:=255;
     CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
     CurrentType^.NeedTypeInfo:=true;
     CurrentType^.TypeKind:=TypeKindString;
     CurrentType^.TypeDefinition:=ttdShortString;
     CurrentType^.Length:=Value;
     CurrentType^.OpenString:=true;
    end else begin
     if LocalSwitches^.LongStrings then begin
      if LocalSwitches^.UnicodeStrings then begin
       CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
       CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
       CurrentType^.TypeKind:=TypeKindWString;
       CurrentType^.NeedTypeInfo:=true;
       CurrentType^.TypeDefinition:=ttdLongString;
       CurrentType^.LongStringType:=Symbols.tstUnsignedWideChar;
       CurrentType^.LongStringCodePage:=$ffff;
       CurrentType^.LongStringReferenceCounted:=true;
      end else begin
       CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
       CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
       CurrentType^.TypeKind:=TypeKindLString;
       CurrentType^.NeedTypeInfo:=true;
       CurrentType^.TypeDefinition:=ttdLongString;
       CurrentType^.LongStringType:=Symbols.tstUnsignedChar;
       CurrentType^.LongStringCodePage:=$ffff;
       CurrentType^.LongStringReferenceCounted:=true;
      end;
     end else begin
      Value:=255;
      CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
      CurrentType^.TypeKind:=TypeKindString;
      CurrentType^.NeedTypeInfo:=true;
      CurrentType^.TypeDefinition:=ttdShortString;
      CurrentType^.Length:=Value;
      CurrentType^.OpenString:=false;
     end;
    end;
   end else if Token=tstSHORTSTRING then begin
    Value:=255;
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdShortString;
    CurrentType^.Length:=Value;
    CurrentType^.OpenString:=false;
   end else if Token=tstWIDESTRING then begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindWString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdLongString;
    CurrentType^.LongStringType:=Symbols.tstUnsignedWideChar;
    CurrentType^.LongStringCodePage:=$ffff;
    CurrentType^.LongStringReferenceCounted:=false;
   end else if Token=tstUNICODESTRING then begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindUString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdLongString;
    CurrentType^.LongStringType:=Symbols.tstUnsignedWideChar;
    CurrentType^.LongStringCodePage:=$ffff;
    CurrentType^.LongStringReferenceCounted:=true;
   end else if Token=tstHUGESTRING then begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindHString;
    CurrentType^.TypeDefinition:=ttdLongString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.LongStringCodePage:=$ffff;
    CurrentType^.LongStringType:=Symbols.tstUnsignedHugeChar;
    CurrentType^.LongStringReferenceCounted:=true;
   end else if Token=tstANSISTRING then begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindLString;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdLongString;
    CurrentType^.LongStringType:=Symbols.tstUnsignedChar;
    CurrentType^.LongStringCodePage:=$ffff;
    CurrentType^.LongStringReferenceCounted:=true;
    if Scanner.CurrentToken=tstLeftParen then begin
     Scanner.Match(tstLeftParen);
     if Scanner.CurrentToken<>tstRightParen then begin
      NewTreeNode:=ParseExpression(false);
      try
       OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
       OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
       OptimizerHighLevel.OptimizeTree(NewTreeNode);
       if NewTreeNode.TreeNodeType=ttntORDConst then begin
        CurrentType^.LongStringCodePage:=NewTreeNode.Value;
       end else begin                  
        Error.AbortCode(526);
       end;
      finally
       NewTreeNode.Free;
      end;
     end;
     Scanner.Match(tstRightParen);
    end;
   end else begin
    if LocalSwitches^.LongStrings then begin
     if LocalSwitches^.UnicodeStrings then begin
      CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
      CurrentType^.TypeKind:=TypeKindWString;
      CurrentType^.NeedTypeInfo:=true;
      CurrentType^.TypeDefinition:=ttdLongString;
      CurrentType^.LongStringType:=Symbols.tstUnsignedWideChar;
      CurrentType^.LongStringCodePage:=$ffff;
      CurrentType^.LongStringReferenceCounted:=true;
     end else begin
      CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
      CurrentType^.TypeKind:=TypeKindLString;
      CurrentType^.NeedTypeInfo:=true;
      CurrentType^.TypeDefinition:=ttdLongString;
      CurrentType^.LongStringType:=Symbols.tstUnsignedChar;
      CurrentType^.LongStringCodePage:=$ffff;
      CurrentType^.LongStringReferenceCounted:=true;
     end;
    end else begin
     Value:=255;
     CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
     CurrentType^.TypeKind:=TypeKindString;
     CurrentType^.NeedTypeInfo:=true;
     CurrentType^.TypeDefinition:=ttdShortString;
     CurrentType^.Length:=Value;
    end;
   end;
  end;
  tstARRAY:begin
   Scanner.Match(tstARRAY);
   if Scanner.CurrentToken=tstLeftBracket then begin
    Scanner.Match(tstLeftBracket);
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindArray;
    CurrentType^.TypeDefinition:=ttdArray;
    StartType:=CurrentType;
    while not Scanner.IsEOFOrAbortError do begin
     CurrentType^.Range:=ParseTypeDefinition('');
     if CurrentType^.Range^.TypeDefinition=ttdEnumerated then begin
      AType:=CurrentType^.Range;
      CurrentType^.Range:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      CurrentType^.Range^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
      CurrentType^.TypeKind:=TypeKindEnumeration;
      CurrentType^.Range^.TypeDefinition:=ttdSubRange;
      CurrentType^.Range^.SubRangeType:=tstSigned32Bit;
      CurrentType^.Range^.LowerLimit:=AType^.LowerLimit;
      CurrentType^.Range^.UpperLimit:=AType^.UpperLimit;
      break;
     end;
     if not SymbolManager.IsOrdinal(CurrentType^.Range) then begin
      Error.AbortCode(0);
     end;
     if Scanner.CurrentToken=tstComma then begin
      Scanner.Match(tstComma);
     end else begin
      break;
     end;
     CurrentType^.Definition:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
     CurrentType^.Definition^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
     CurrentType:=CurrentType^.Definition;
     CurrentType^.TypeKind:=TypeKindArray;
     CurrentType^.TypeDefinition:=ttdArray;
    end;
    Scanner.Match(tstRightBracket);
   end else begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindArray;
    CurrentType^.TypeDefinition:=ttdArray;
    CurrentType^.Range:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.Range^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.Range^.TypeDefinition:=ttdSubRange;
    CurrentType^.OpenArray:=AllowOpenArray;
    CurrentType^.DynamicArray:=not AllowOpenArray;
    StartType:=CurrentType;
   end;
   Scanner.Match(tstOF);
   CurrentType^.Definition:=ParseTypeDefinition('');
   CurrentType^.PortabilityDirectives:=ParsePortabilityDirectives;
   CurrentType:=StartType;
   while assigned(CurrentType) do begin
    CurrentType^.NeedTypeInfo:=SymbolManager.TypeDoNeedTypeInfo(CurrentType);
    if CurrentType^.TypeDefinition=ttdArray then begin
     CurrentType:=CurrentType^.Definition;
    end else begin
     break;
    end;
   end;
   CurrentType:=StartType;
  end;
  tstFILE:begin
   Scanner.Match(tstFILE);
   CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   CurrentType^.TypeKind:=TypeKindUnknown;
   CurrentType^.NeedTypeInfo:=false;
   CurrentType^.TypeDefinition:=ttdFile;
   if Scanner.CurrentToken=tstOF then begin
    Scanner.Match(tstOF);
    CurrentType^.FileTypeRecord:=ParseTypeDefinition('');
    CurrentType^.FileType:=tftTyped;
   end else begin
    CurrentType^.FileType:=tftUntyped;
   end;
   CurrentType^.PortabilityDirectives:=ParsePortabilityDirectives;
  end;
  tstPointer:begin
   Scanner.Match(tstPointer);
   CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   CurrentType^.TypeKind:=TypeKindUnknown;
   CurrentType^.NeedTypeInfo:=false;
   CurrentType^.TypeDefinition:=ttdPointer;
   CurrentType^.PointerTo:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol:=SymbolManager.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
   if assigned(Symbol) and (Symbol^.SymbolType=Symbols.tstUnit) then begin
    Scanner.Match(tstIdentifier);
    Scanner.Match(tstPeriod);
    Symbol:=Symbol^.SymbolList.GetSymbol(tpsIdentifier+Scanner.CurrentIdentifier,ModuleSymbol,CurrentObjectClass);
   end;
   if assigned(Symbol) then begin
    Scanner.Match(tstIdentifier);
    CurrentType^.PointerTo:=Symbol;
   end else begin
    case Scanner.CurrentToken of
     tstIdentifier:begin
      Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      Symbol^.Name:=Scanner.ReadIdentifier(@Symbol^.OriginalCaseName);
      Symbol^.SymbolType:=Symbols.tstType;
      Symbol^.VariableType:=SymbolManager.VariableType;
      Symbol^.TypeDefinition:=nil;
      Symbol^.ForwardType:=true;
      SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      CurrentType^.PointerTo:=Symbol;
     end;
     else begin
      Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
      Symbol^.Name:=tpsDummyPointerType+Name;
      Symbol^.SymbolType:=Symbols.tstType;
      Symbol^.VariableType:=SymbolManager.VariableType;
      Symbol^.TypeDefinition:=ParseTypeDefinition('');
      Symbol^.ForwardType:=false;
      SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      CurrentType^.PointerTo:=Symbol;
     end;
    end;
   end;
   CurrentType^.PortabilityDirectives:=ParsePortabilityDirectives;
  end;
  tstSET:begin
   Scanner.Match(tstSET);
   Scanner.Match(tstOF);
   CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   CurrentType^.TypeKind:=TypeKindSet;
   CurrentType^.NeedTypeInfo:=true;
   CurrentType^.TypeDefinition:=ttdSet;
   CurrentType^.SetOf:=ParseTypeDefinition('');
   case CurrentType^.SetOf^.TypeDefinition of
    ttdEnumerated,ttdSubRange:begin // ttdEnumerated needed ??? !!! TO FIX !!!
     LowerValue:=CurrentType^.setof^.LowerLimit;
     HigherValue:=CurrentType^.setof^.UpperLimit;
     if (LowerValue>=0) and (HigherValue<=255) then begin
      Value:=HigherValue-LowerValue;
      CurrentType^.SetSize:=(Value+7) shr 3;
     end else begin
      Error.AbortCode(28); // <- ('Set base type out of range'); !!! TO FIX !!!
     end;
    end;
   end;
   CurrentType^.PortabilityDirectives:=ParsePortabilityDirectives;
  end;
  tstRECORD:begin
   Scanner.Match(tstRECORD);
   CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
   CurrentType^.TypeKind:=TypeKindRecord;
   CurrentType^.TypeDefinition:=ttdRECORD;
   CurrentType^.RecordAlignment:=0;
   CurrentType^.RecordPacked:=IsPacked or (LocalSwitches^.Alignment=1);
   CurrentType^.RecordTable:=TSymbolList.Create(SymbolManager);
   if Scanner.CurrentToken<>tstEND then begin
    ParseRecordField(CurrentType,true,[],[],0,0,'',false);
   end;
   SymbolManager.AlignRecord(CurrentType,LocalSwitches^.Alignment);
{  if SymbolManager.GetSize(CurrentType)=0 then begin
    Error.AbortCode(516);
   end;}
   Scanner.Match(tstEND);
   CurrentType^.PortabilityDirectives:=ParsePortabilityDirectives;
   CurrentType^.NeedTypeInfo:=SymbolManager.TypeDoNeedTypeInfo(CurrentType);
  end;
  tstOBJECT:begin
   CurrentType:=ParseObjectDeclaration(Name,IsPacked);
  end;
  tstCLASS:begin
   CurrentType:=ParseClassDeclaration(Name,IsPacked);
  end;
  tstINTERFACE,tstDISPINTERFACE:begin
   CurrentType:=ParseInterfaceDeclaration(Name,IsPacked);
  end;
  tstPROCEDURE,tstFUNCTION:begin
   Scanner.Match(Scanner.CurrentToken);
   CurrentType:=ParseProcedureVariable(Symbol);
   if OldToken=tstFUNCTION then begin
//  CurrentType:=ParseProcedureVariable(Symbol);
    Scanner.Match(tstCOLON);
    if assigned(Symbol) then begin
     Symbol^.ReturnType:=ParseTypeDefinition('');
    end;
   end;
   if Scanner.CurrentToken=tstOF then begin
    Scanner.Match(tstOF);
    Scanner.Match(tstOBJECT);
    Scanner.Match(tstSeparator);
    Symbol^.ProcedureAttributes:=Symbol^.ProcedureAttributes+[tpaClass];
    CurrentType^.AddressOnly:=false;
   end else begin
    CurrentType^.AddressOnly:=true;
   end;
   ParseProcedureType(Symbol,false);
   CurrentType^.ProcedureAttributes:=CurrentType^.ProcedureAttributes+Symbol^.ProcedureAttributes;
   CurrentType^.PortabilityDirectives:=CurrentType^.PortabilityDirectives+Symbol^.PortabilityDirectives;
   CheckProcedureType(Symbol);
   if (Scanner.CurrentToken<>tstSeparator) and (Scanner.LastToken=tstSeparator) then begin
    Scanner.UseNextToken:=true;
    Scanner.NextToken:=Scanner.CurrentToken;
    Scanner.CurrentToken:=Scanner.LastToken;
   end;
   SymbolManager.DestroySymbol(Symbol);
  end;
  else begin
   CanBeConstant:=true;
  end;
 end;
 if CanBeConstant then begin
  NewTreeNode:=ParseExpression(false);
  OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
  OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
  OptimizerHighLevel.OptimizeTree(NewTreeNode);
  case NewTreeNode.TreeNodeType of
   ttntSubRange:begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindInteger;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdSubRange;
    CurrentType^.LowerLimit:=NewTreeNode.Left.Value;
    CurrentType^.UpperLimit:=NewTreeNode.Right.Value;
    LowerValue64Bit:=CurrentType^.LowerLimit;
    HigherValue64Bit:=CurrentType^.UpperLimit;
    if LowerValue64Bit>HigherValue64Bit then begin
     Error.AbortCode(10);
    end else if (NewTreeNode.Left.TreeNodeType=ttntCharConst) and (NewTreeNode.Right.TreeNodeType=ttntCharConst) then begin
     CurrentType^.SubRangeType:=Symbols.tstUnsignedChar;
     CurrentType^.LowerLimit:=ord(NewTreeNode.Left.CharValue);
     CurrentType^.UpperLimit:=ord(NewTreeNode.Right.CharValue);
    end else begin
     if (LowerValue64Bit>=0) and (HigherValue64Bit<=255) then begin
      CurrentType^.SubRangeType:=Symbols.tstUnsigned8Bit;
     end else if (LowerValue64Bit>=-128) and (HigherValue64Bit<=127) then begin
      CurrentType^.SubRangeType:=Symbols.tstSigned8Bit;
     end else if (LowerValue64Bit>=0) and (HigherValue64Bit<=65535) then begin
      CurrentType^.SubRangeType:=Symbols.tstUnsigned16Bit;
     end else if (LowerValue64Bit>=-32768) and (HigherValue64Bit<=32767) then begin
      CurrentType^.SubRangeType:=Symbols.tstSigned16Bit;
     end else if (LowerValue64Bit>=0) and (HigherValue64Bit<=$ffffffff) then begin
      CurrentType^.SubRangeType:=Symbols.tstUnsigned32Bit;
     end else if (LowerValue64Bit>=low(longint)) and (HigherValue64Bit<=high(longint)) then begin
      CurrentType^.SubRangeType:=Symbols.tstSigned32Bit;
     end else begin
      CurrentType^.SubRangeType:=Symbols.tstSigned64Bit;
     end;
    end;
   end;
   ttntFloat:begin
    CurrentType:=SymbolManager.NewType(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
    CurrentType^.RuntimeTypeInfo:=LocalSwitches^.TypeInfo;
    CurrentType^.TypeKind:=TypeKindFloat;
    CurrentType^.NeedTypeInfo:=true;
    CurrentType^.TypeDefinition:=ttdFloat;
    LowerFloatValue:=NewTreeNode.Left.FloatValue;
    HigherFloatValue:=NewTreeNode.Right.FloatValue;
    if (LowerFloatValue>=1.5e-45) and (HigherFloatValue<=3.4e38) then begin
     CurrentType^.FloatType:=Symbols.tstFloat32Bit;
    end else if (LowerFloatValue>=5.0e-324) and (HigherFloatValue<=1.7e308) then begin
     CurrentType^.FloatType:=Symbols.tstFloat64Bit;
    end else begin
     CurrentType^.FloatType:=Symbols.tstFloat80Bit;
    end;
    CurrentType^.FloatLowerLimit:=LowerFloatValue;
    CurrentType^.FloatUpperLimit:=HigherFloatValue;
   end;
   else begin
    Error.AbortCode(114); // <-- 'Error in expression' !!! TO FIX !!!
   end;
  end;
 end;
 result:=CurrentType;
 if not assigned(result) then begin
  result:=SymbolManager.TypePointer;
 end;
end;

function TParser.ParseTypedConstantDeclaration(var ConstantType:PType;ID:longword;IsPacked:boolean;Dummy:boolean=false):PConstant;
var NewTreeNode:TTreeNode;
    Symbol,RecordSymbol:PSymbol;
    Constant:PConstant;
    Field:ansistring;
    Counter:longint;
    Found:boolean;
begin
 result:=nil;
 case ConstantType^.TypeDefinition of
  ttdPointer:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctPOINTER;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.PointerTo:=nil;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctPOINTER;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    if (ConstantType^.TypeDefinition=ttdPointer) and
       ((NewTreeNode.TreeNodeType=ttntNIL) or
        ((NewTreeNode.TreeNodeType=ttntORDConst) and (NewTreeNode.Value=0))) then begin
     if (NewTreeNode.TreeNodeType=ttntORDConst) and (NewTreeNode.Value=0) then begin
      Error.AddWarningCode(252);
     end;
     Constant^.PointerTo:=nil;
    end else if (NewTreeNode.TreeNodeType=ttntAddress) and
                assigned(NewTreeNode.Left) and
                (NewTreeNode.Left.TreeNodeType=ttntVAR) and
                assigned(NewTreeNode.Left.Symbol) and
                (NewTreeNode.Left.Symbol^.SymbolType in [Symbols.tstVARIABLE,Symbols.tstFUNCTION,Symbols.tstPROCEDURE]) then begin
     if NewTreeNode.Left.Symbol^.SymbolType=Symbols.tstVARIABLE then begin
      if NewTreeNode.Left.Symbol^.VariableLevel<>0 then begin
       Error.AbortCode(517);
      end;
     end;
     if not EqualTypes(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
      Error.AbortCode(7);
     end;
     Constant^.PointerTo:=NewTreeNode.Left.Symbol;
    end else if (NewTreeNode.TreeNodeType=ttntAddress) and
                assigned(NewTreeNode.Left) and
                (NewTreeNode.Left.TreeNodeType=ttntCALL) and
                assigned(NewTreeNode.Left.Symbol) and
                (NewTreeNode.Left.Symbol^.SymbolType in [Symbols.tstVARIABLE,Symbols.tstPROCEDURE]) then begin
     if NewTreeNode.Left.Symbol^.SymbolType=Symbols.tstVARIABLE then begin
      if NewTreeNode.Left.Symbol.VariableLevel<>0 then begin
       Error.AbortCode(518);
      end;
     end;
     if not EqualTypes(Error,SymbolManager,NewTreeNode.Left.Return,ConstantType) then begin
      Error.AbortCode(7);
     end;
     Constant^.PointerTo:=NewTreeNode.Left.Symbol;
    end else if assigned(ConstantType^.PointerTo) and
                (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
                assigned(ConstantType^.PointerTo^.TypeDefinition) and
                (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedChar) then begin
     if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
      Constant^.ConstantType:=tctAnsiString;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=NewTreeNode.StringData;
     end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
      Constant^.ConstantType:=tctPANSICHAR;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
     end else begin
      Error.AbortCode(519);
      Constant^.PointerTo:=nil;
     end;
    end else if assigned(ConstantType^.PointerTo) and
                (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
                assigned(ConstantType^.PointerTo^.TypeDefinition) and
                (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedWideChar) then begin
     if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
      Constant^.ConstantType:=tctWIDESTRING;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=NewTreeNode.StringData;
     end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
      Constant^.ConstantType:=tctPWIDECHAR;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
     end else begin
      Error.AbortCode(519);
      Constant^.PointerTo:=nil;
     end;
    end else if assigned(ConstantType^.PointerTo) and
                (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
                assigned(ConstantType^.PointerTo^.TypeDefinition) and
                (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
     if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
      Constant^.ConstantType:=tctHUGESTRING;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=NewTreeNode.StringData;
     end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
      Constant^.ConstantType:=tctPHUGECHAR;
      Constant^.Size:=SymbolManager.GetSize(ConstantType);
      Constant^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
     end else begin
      Error.AbortCode(519);
      Constant^.PointerTo:=nil;
     end;
    end else if EqualTypes(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
     Constant^.PointerTo:=NewTreeNode.Symbol;
    end else begin
     Error.AbortCode(519);
     Constant^.PointerTo:=nil;
    end;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end;
  end;
  ttdBoolean:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.IntValue:=0;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    if (NewTreeNode.TreeNodeType=ttntORDConst) and
       assigned(NewTreeNode.Return) and
       (NewTreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
     Constant^.IntValue:=NewTreeNode.Value;
    end else begin
     Error.AbortCode(11);
     Constant^.IntValue:=0;
    end;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
    NewTreeNode.Destroy;
   end;
  end;
  ttdEnumerated:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.IntValue:=0;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
     Constant^.IntValue:=NewTreeNode.Value;
    end else begin
     Error.AbortCode(7);
     Constant^.IntValue:=0;
    end;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
    NewTreeNode.Destroy;
   end;
  end;
  ttdSubRange:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    if ConstantType^.SubRangeType=Symbols.tstUnsignedChar then begin
     Constant^.ConstantType:=tctAnsiChar;
     Constant^.CharValue:=0;
    end else if ConstantType^.SubRangeType=Symbols.tstUnsignedWideChar then begin
     Constant^.ConstantType:=tctWideChar;
     Constant^.CharValue:=0;
    end else if ConstantType^.SubRangeType=Symbols.tstUnsignedHugeChar then begin
     Constant^.ConstantType:=tctHugeChar;
     Constant^.CharValue:=0;
    end;
    Constant^.IntValue:=0;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) and
       (NewTreeNode.TreeNodeType in [ttntCHARCONST,ttntORDCONST]) then begin
     if NewTreeNode.TreeNodeType=ttntCHARCONST then begin
      if ConstantType^.SubRangeType=Symbols.tstUnsignedChar then begin
       NewTreeNode.Value:=Constant^.CharValue and $ff;
      end else if ConstantType^.SubRangeType=Symbols.tstUnsignedWideChar then begin
       NewTreeNode.Value:=Constant^.CharValue and $ffff;
      end else if ConstantType^.SubRangeType=Symbols.tstUnsignedHugeChar then begin
       NewTreeNode.Value:=Constant^.CharValue;
      end;
     end;
     Constant^.IntValue:=NewTreeNode.Value;
     if ConstantType^.SubRangeType=Symbols.tstUnsignedChar then begin
      Constant^.ConstantType:=tctAnsiChar;
      Constant^.CharValue:=Constant^.IntValue and $ff;
     end else if ConstantType^.SubRangeType=Symbols.tstUnsignedWideChar then begin
      Constant^.ConstantType:=tctWideChar;
      Constant^.CharValue:=Constant^.IntValue and $ffff;
     end else if ConstantType^.SubRangeType=Symbols.tstUnsignedHugeChar then begin
      Constant^.ConstantType:=tctHugeChar;
      Constant^.CharValue:=Constant^.IntValue and longword($ffffffff);
     end;
    end else begin
     Error.AbortCode(7);
     Constant^.IntValue:=0;
    end;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
    NewTreeNode.Destroy;
   end;
  end;
  ttdCurrency:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.IntValue:=0;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctOrdinal;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
     Constant^.IntValue:=NewTreeNode.Value;
    end else begin
     Error.AbortCode(7);
     Constant^.IntValue:=0;
    end;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
    NewTreeNode.Destroy;
   end;
  end;
  ttdFloat:begin
   if Dummy then begin
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctFloat;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    Constant^.FloatValue:=0;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
   end else begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctFloat;
    Constant^.Size:=SymbolManager.GetSize(ConstantType);
    if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) and
       (NewTreeNode.TreeNodeType in [ttntFloatConst,ttntORDConst]) then begin
     if NewTreeNode.TreeNodeType=ttntFloatConst then begin
      Constant^.FloatValue:=NewTreeNode.FloatValue;
     end else if NewTreeNode.TreeNodeType=ttntORDConst then begin
      Constant^.FloatValue:=NewTreeNode.Value;
     end;
    end else begin
     Error.AbortCode(520);
    end;
    Constant^.ID:=ID;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=IsPacked;
    result:=Constant;
    SymbolManager.AddConstant(Constant);
    NewTreeNode.Destroy;
   end;
  end;
  ttdShortString,ttdLongString:begin
   if not Dummy then begin
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
   end;
   Constant:=SymbolManager.NewConstant(ModuleSymbol);
   Constant^.Size:=SymbolManager.GetSize(ConstantType);
   case ConstantType^.TypeDefinition of
    ttdShortString:begin
     Constant^.ConstantType:=tctShortString;
     if Dummy then begin
      Constant^.ShortStringValue:='';
     end else begin
      case NewTreeNode.TreeNodeType of
       ttntCHARCONST:begin
        Constant^.ShortStringValue:=AnsiChar(byte(NewTreeNode.CharValue and $ff));
       end;
       ttntSTRINGCONST:begin
        Constant^.ShortStringValue:=HugeStringToAnsiString(NewTreeNode.StringData);
       end;
       else begin
        Error.AbortCode(524);
       end;
      end;
     end;
    end;
    ttdLongString:begin
     if ConstantType^.LongStringType=tstUnsignedHugeChar then begin
      Constant^.ConstantType:=tctHugeString;
     end else if ConstantType^.LongStringType=tstUnsignedWideChar then begin
      Constant^.ConstantType:=tctWideString;
     end else begin
      Constant^.ConstantType:=tctAnsiString;
     end;
     if Dummy then begin
      Constant^.StringValue:=nil;
     end else begin
      case NewTreeNode.TreeNodeType of
       ttntCHARCONST:begin
        Constant^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
       end;
       ttntSTRINGCONST:begin
        Constant^.StringValue:=NewTreeNode.StringData;
       end;
       else begin
        Error.AbortCode(524);
       end;
      end;
     end;
    end;
   end;
   Constant^.ID:=ID;
   Constant^.Dumped:=false;
   Constant^.IsPacked:=IsPacked;
   result:=Constant;
   SymbolManager.AddConstant(Constant);
   if not Dummy then begin
    NewTreeNode.Destroy;
   end;
  end;
  ttdArray:begin
   if not Dummy then begin
    Scanner.Match(tstLeftParen);
   end;
   if ConstantType^.DynamicArray then begin
    Error.AbortCode(521);
   end else if ConstantType^.OpenArray then begin
    Error.AbortCode(522);
   end else begin
    for Counter:=ConstantType^.Range^.LowerLimit to ConstantType^.Range^.UpperLimit-1 do begin
     Constant:=ParseTypedConstantDeclaration(ConstantType^.Definition,ID,true,Dummy);
     if not assigned(result) then begin
      result:=Constant;
     end;
     if not Dummy then begin
      Scanner.Match(tstComma);
     end;
    end;
    Constant:=ParseTypedConstantDeclaration(ConstantType^.Definition,ID,true,Dummy);
    if not assigned(result) then begin
     result:=Constant;
    end;
   end;
   if not Dummy then begin
    Scanner.Match(tstRightParen);
   end;
  end;
  ttdRecord:begin
   RecordSymbol:=ConstantType^.RecordTable.First;
   if not Dummy then begin
    Scanner.Match(tstLeftParen);
    while (Scanner.CurrentToken<>tstRightParen) and not Scanner.IsEOFOrAbortError do begin
     if Scanner.CurrentToken<>tstIdentifier then begin
      Error.AbortCode(124);
      break;
     end;
     Field:=Scanner.ReadIdentifier(nil);
     Scanner.Match(tstColon);                                                              
     Symbol:=ConstantType^.RecordTable.GetSymbol(Field,ModuleSymbol,CurrentObjectClass,true);
     if assigned(Symbol) then begin
      Found:=false;
      while assigned(RecordSymbol) do begin
       if RecordSymbol=Symbol then begin
        RecordSymbol:=RecordSymbol^.Next;
        Found:=true;
        break;
       end;
       Constant:=ParseTypedConstantDeclaration(RecordSymbol^.TypeDefinition,ID,ConstantType^.RecordPacked,true);
       if not assigned(result) then begin
        result:=Constant;
       end;
       RecordSymbol:=RecordSymbol^.Next;
      end;
      if not Found then begin
       Error.AbortCode(87);
      end;
      Constant:=ParseTypedConstantDeclaration(Symbol^.TypeDefinition,ID,ConstantType^.RecordPacked);
      if not assigned(result) then begin
       result:=Constant;
      end;
      if (ConstantType^.TypeDefinition in [ttdRecord,ttdObject,ttdClass,ttdInterface]) and (ConstantType^.RecordAlignment<>1) then begin
       Constant:=SymbolManager.NewConstant(ModuleSymbol);
       Constant^.ConstantType:=tctAlign;
       Constant^.Size:=SymbolManager.GetSize(ConstantType);
       Constant^.IntValue:=0;
       Constant^.ID:=ID;
       Constant^.Dumped:=false;
       Constant^.IsPacked:=ConstantType^.RecordPacked;
       SymbolManager.AddConstant(Constant);
      end;
      if Scanner.CurrentToken=tstSeparator then begin
       Scanner.Match(tstSeparator);
      end else begin
       break;
      end;
     end else begin
      Error.AbortCode(523,CorrectSymbolName(Field));
     end;
    end;
    Scanner.Match(tstRightParen);
   end;
   while assigned(RecordSymbol) do begin
    Constant:=ParseTypedConstantDeclaration(RecordSymbol^.TypeDefinition,ID,ConstantType^.RecordPacked,true);
    if not assigned(result) then begin
     result:=Constant;
    end;
    RecordSymbol:=RecordSymbol^.Next;
   end;
  end;
  else begin
   Error.AbortCode(524);
  end;
 end;
end;

procedure TParser.ParseDefaultParameterTypedConstantDeclaration(var ConstantType:PType;ParentSymbol:PSymbol);
var NewTreeNode:TTreeNode;
    Symbol:PSymbol;
begin
 Symbol:=nil;
 case ConstantType^.TypeDefinition of
  ttdPointer:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctPOINTER;
   if (ConstantType^.TypeDefinition=ttdPointer) and
      ((NewTreeNode.TreeNodeType=ttntNIL) or
       ((NewTreeNode.TreeNodeType=ttntORDConst) and (NewTreeNode.Value=0))) then begin
    if (NewTreeNode.TreeNodeType=ttntORDConst) and (NewTreeNode.Value=0) then begin
     Error.AddWarningCode(252);
    end;
    Symbol^.PointerTo:=nil;
   end else if (NewTreeNode.TreeNodeType=ttntAddress) and
               assigned(NewTreeNode.Left) and
               (NewTreeNode.Left.TreeNodeType=ttntVAR) and
               assigned(NewTreeNode.Left.Symbol) and
               (NewTreeNode.Left.Symbol^.SymbolType=Symbols.tstVARIABLE) then begin
    if NewTreeNode.Left.Symbol.VariableLevel<>0 then begin
     Error.AbortCode(517);
    end;
    if not EqualTypes(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
     Error.AbortCode(7);
    end;
    Symbol^.PointerTo:=NewTreeNode.Left.Symbol;
   end else if (NewTreeNode.TreeNodeType=ttntAddress) and
               assigned(NewTreeNode.Left) and
               (NewTreeNode.Left.TreeNodeType=ttntCALL) and
               assigned(NewTreeNode.Left.Symbol) and
               (NewTreeNode.Left.Symbol^.SymbolType in [Symbols.tstVARIABLE,Symbols.tstPROCEDURE]) then begin
    if NewTreeNode.Left.Symbol.VariableLevel<>0 then begin
     Error.AbortCode(518);
    end;
    if not EqualTypes(Error,SymbolManager,NewTreeNode.Left.Return,ConstantType) then begin
     Error.AbortCode(7);
    end;
    Symbol^.PointerTo:=NewTreeNode.Left.Symbol;
   end else if assigned(ConstantType^.PointerTo) and
               (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
               assigned(ConstantType^.PointerTo^.TypeDefinition) and
               (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
               (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedChar) then begin
    if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
     Symbol^.ConstantType:=tctPANSICHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePAnsiChar;
     Symbol^.StringValue:=NewTreeNode.StringData;
    end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
     Symbol^.ConstantType:=tctPANSICHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePAnsiChar;
     Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue)
    end else begin
     Error.AbortCode(519);
     Symbol^.PointerTo:=nil;
    end;
   end else if assigned(ConstantType^.PointerTo) and
               (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
               assigned(ConstantType^.PointerTo^.TypeDefinition) and
               (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
               (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedWideChar) then begin
    if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
     Symbol^.ConstantType:=tctPWIDECHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePWideChar;
     Symbol^.StringValue:=NewTreeNode.StringData;
    end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
     Symbol^.ConstantType:=tctPWIDECHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePWideChar;
     Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
    end else begin
     Error.AbortCode(519);
     Symbol^.PointerTo:=nil;
    end;
   end else if assigned(ConstantType^.PointerTo) and
               (ConstantType^.PointerTo^.SymbolType=Symbols.tstType) and
               assigned(ConstantType^.PointerTo^.TypeDefinition) and
               (ConstantType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
               (ConstantType^.PointerTo^.TypeDefinition^.SubRangeType=Symbols.tstUnsignedHugeChar) then begin
    if NewTreeNode.TreeNodeType=ttntSTRINGConst then begin
     Symbol^.ConstantType:=tctPHUGECHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePHugeChar;
     Symbol^.StringValue:=NewTreeNode.StringData;
    end else if NewTreeNode.TreeNodeType=ttntCHARConst then begin
     Symbol^.ConstantType:=tctPHUGECHAR;
     Symbol^.ConstantTypeRecord:=SymbolManager.TypePHugeChar;
     Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
    end else begin
     Error.AbortCode(519);
     Symbol^.PointerTo:=nil;
    end;
   end else begin
    Error.AbortCode(519);
    Symbol^.PointerTo:=nil;
   end;
   NewTreeNode.Destroy;
  end;
  ttdBoolean:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctOrdinal;
   if (NewTreeNode.TreeNodeType=ttntORDConst) and
      assigned(NewTreeNode.Return) and
      (NewTreeNode.Return^.TypeDefinition=ttdBOOLEAN) then begin
    Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
    Symbol^.IntValue:=NewTreeNode.Value;
   end else begin
    Error.AbortCode(11);
    Symbol^.IntValue:=0;
   end;
   NewTreeNode.Destroy;
  end;
  ttdEnumerated:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctOrdinal;
   if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
    Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
    Symbol^.IntValue:=NewTreeNode.Value;
   end else begin
    Error.AbortCode(7);
    Symbol^.IntValue:=0;
   end;
   NewTreeNode.Destroy;
  end;
  ttdSubRange:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctOrdinal;
   if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) and
      (NewTreeNode.TreeNodeType in [ttntCHARCONST,ttntORDCONST]) then begin
    Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
    if NewTreeNode.TreeNodeType=ttntCHARCONST then begin
     if ConstantType^.SubRangeType=Symbols.tstUnsignedChar then begin
      NewTreeNode.Value:=Symbol^.CharValue and $ff;
     end else if ConstantType^.SubRangeType=Symbols.tstUnsignedWideChar then begin
      NewTreeNode.Value:=Symbol^.CharValue and $ffff;
     end else if ConstantType^.SubRangeType=Symbols.tstUnsignedHugeChar then begin
      NewTreeNode.Value:=Symbol^.CharValue and longword($ffffffff);
     end;
    end;
    Symbol^.IntValue:=NewTreeNode.Value;
    if ConstantType^.SubRangeType=Symbols.tstUnsignedChar then begin
     Symbol^.ConstantType:=tctAnsiChar;
     Symbol^.CharValue:=Symbol^.IntValue and $ff;
    end else if ConstantType^.SubRangeType=Symbols.tstUnsignedWideChar then begin
     Symbol^.ConstantType:=tctWideChar;
     Symbol^.CharValue:=Symbol^.IntValue and $ffff;
    end else if ConstantType^.SubRangeType=Symbols.tstUnsignedHugeChar then begin
     Symbol^.ConstantType:=tctHugeChar;
     Symbol^.CharValue:=Symbol^.IntValue and longword($ffffffff);
    end;
   end else begin
    Error.AbortCode(7);
    Symbol^.IntValue:=0;
   end;
   NewTreeNode.Destroy;
  end;
  ttdCurrency:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctOrdinal;
   if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) then begin
    Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
    Symbol^.IntValue:=NewTreeNode.Value;
   end else begin
    Error.AbortCode(7);
    Symbol^.IntValue:=0;
   end;
   NewTreeNode.Destroy;
  end;
  ttdFloat:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
   Symbol^.ConstantType:=tctFloat;
   if EqualTypesEx(Error,SymbolManager,NewTreeNode.Return,ConstantType) and
      (NewTreeNode.TreeNodeType in [ttntFloatConst,ttntORDConst]) then begin
    Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
    if NewTreeNode.TreeNodeType=ttntFloatConst then begin
     Symbol^.FloatValue:=NewTreeNode.FloatValue;
    end else if NewTreeNode.TreeNodeType=ttntORDConst then begin
     Symbol^.FloatValue:=NewTreeNode.Value;
    end;
   end else begin
    Error.AbortCode(520);
   end;
   NewTreeNode.Destroy;
  end;
  ttdShortString,ttdLongString:begin
   NewTreeNode:=ParseExpression(false);
   OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
   OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
   OptimizerHighLevel.OptimizeTree(NewTreeNode);
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
   Symbol^.SymbolType:=Symbols.tstConstant;
{  from plang:

   if ConstantType^.DynamicArray then begin
    Symbol^.ConstantType:=tctAnsiString;
    case NewTreeNode.TreeNodeType of
     ttntCHARCONST:Symbol^.StringValue:=NewTreeNode.CharValue;
     else Symbol^.StringValue:=NewTreeNode.StringData;
    end;
   end else begin
    Symbol^.ConstantType:=tctShortString;
    case NewTreeNode.TreeNodeType of
     ttntCHARCONST:Symbol^.ShortStringValue:=NewTreeNode.CharValue;
     else Symbol^.ShortStringValue:=NewTreeNode.StringData;
    end;
   end;
}
   case ConstantType^.TypeDefinition of
    ttdShortString:begin
     Symbol^.ConstantType:=tctShortString;
     Symbol^.ConstantTypeRecord:=ConstantType;
     case NewTreeNode.TreeNodeType of
      ttntCHARCONST:begin
       Symbol^.ShortStringValue:=AnsiChar(NewTreeNode.CharValue);
      end;
      ttntSTRINGCONST:begin
       Symbol^.ShortStringValue:=HugeStringToAnsiString(NewTreeNode.StringData);
      end;
      else begin
       Error.AbortCode(524);
      end;
     end;
    end;
    ttdLongString:begin
     if ConstantType^.LongStringType=tstUnsignedHugeChar then begin
      Symbol^.ConstantType:=tctHugeString;
      Symbol^.ConstantTypeRecord:=ConstantType;
      case NewTreeNode.TreeNodeType of
       ttntCHARCONST:begin
        Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue);
       end;
       ttntSTRINGCONST:begin
        Symbol^.StringValue:=NewTreeNode.StringData;
       end;
       else begin
        Error.AbortCode(524);
       end;
      end;
     end else if ConstantType^.LongStringType=tstUnsignedWideChar then begin
      Symbol^.ConstantType:=tctWideString;
      Symbol^.ConstantTypeRecord:=ConstantType;
      case NewTreeNode.TreeNodeType of
       ttntCHARCONST:begin
        Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue and $ffff);
       end;
       ttntSTRINGCONST:begin
        Symbol^.StringValue:=NewTreeNode.StringData;
       end;
       else begin
        Error.AbortCode(524);
       end;
      end;
     end else begin
      Symbol^.ConstantType:=tctAnsiString;
      Symbol^.ConstantTypeRecord:=ConstantType;
      case NewTreeNode.TreeNodeType of
       ttntCHARCONST:begin
        Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.CharValue and $ff);
       end;
       ttntSTRINGCONST:begin
        Symbol^.StringValue:=NewTreeNode.StringData;
       end;
       else begin
        Error.AbortCode(524);
       end;
      end;
     end;
    end;
   end;
   NewTreeNode.Destroy;
  end;
  ttdSet:begin
   NewTreeNode:=ParseExpression(false);
   case NewTreeNode.TreeNodeType of
    ttntSETCONST:begin
     OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
     OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
     OptimizerHighLevel.OptimizeTree(NewTreeNode);
     Symbol:=SymbolManager.NewSymbol(ModuleSymbol);
     Symbol^.SymbolType:=Symbols.tstConstant;
     Symbol^.ConstantType:=tctSet;
     Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
     Symbol^.SetArray:=NewTreeNode.SetData;
     if not SymbolManager.CompatibleTypes(NewTreeNode.Return,ConstantType) then begin
      Error.AbortCode(7);
     end;
     NewTreeNode.Destroy;
    end;
    else begin
     Error.AbortCode(524);
    end;
   end;
  end;
  else begin
   Error.AbortCode(524);
  end;
 end;
 if assigned(Symbol) then begin
  Symbol^.Name:='DEFAULTPARAMETER_SYMBOL_'+INTTOSTR(DefaultParameterCounter);
  HashSymbol(Symbol);
  inc(DefaultParameterCounter);
  ParentSymbol^.DefaultParameterSymbol:=Symbol;
 end else begin
  Error.AbortCode(524);
 end;
end;

procedure TParser.ParseRESOURCESTRINGDeclartion;
var NewTreeNode:TTreeNode;
    Name,OriginalCaseName:ansistring;
//  ConstantType:PType;
    Symbol:PSymbol;
begin
 Scanner.Match(tstRESOURCESTRING);
 repeat
  OriginalCaseName:='';
  Name:=Scanner.ReadIdentifier(@OriginalCaseName);
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  Symbol^.OriginalCaseName:=OriginalCaseName;
  Symbol^.Name:=Name;
  HashSymbol(Symbol);
  if MakeSymbolsPublic then begin
   Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
  end;
  Symbol^.VariableType:=SymbolManager.VariableType;
  case Scanner.CurrentToken of
   tstEqual:begin
    Scanner.Match(tstEqual);
    Symbol^.SymbolType:=Symbols.tstConstant;
    Symbol^.ConstantTypeRecord:=nil;
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    while not Scanner.IsEOFOrAbortError do begin
     case NewTreeNode.TreeNodeType of
      ttntCharConst:begin
       Symbol^.StringValue:=HugeStringConcat(nil,NewTreeNode.Value);
       Symbol^.ConstantType:=tctHugeString;
       Symbol^.ConstantTypeRecord:=SymbolManager.TypeHugeString;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;
      ttntStringConst:begin
       Symbol^.StringValue:=NewTreeNode.StringData;
       Symbol^.ConstantType:=tctHugeString;
       Symbol^.ConstantTypeRecord:=SymbolManager.TypeHugeString;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;
{     ttntTYPE:BEGIN
       Symbol^.ConstantTypeRecord:=NewTreeNode.ConvType;
       while NewTreeNode.TreeNodeType=ttntTYPE do begin
        NewTreeNode:=NewTreeNode.Left;
       end;
       continue;
      end;}
      else begin
       Error.AbortCode(504);
      end;
     end;
     break;
    end;
    NewTreeNode.Destroy;
    Scanner.Match(tstSeparator);
   end;
   else Scanner.Match(tstEqual);
  end;
 until Scanner.CurrentToken<>tstIdentifier;
end;

procedure TParser.ParseCONSTDeclartion;
var NewTreeNode:TTreeNode;
    Name,OriginalCaseName:ansistring;
    ConstantType:PType;
    Constant:PConstant;
    Symbol:PSymbol;
begin
 Scanner.Match(tstCONST);
 repeat
  OriginalCaseName:='';
  Name:=Scanner.ReadIdentifier(@OriginalCaseName);
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  Symbol^.OriginalCaseName:=OriginalCaseName;
  Symbol^.Name:=Name;
  HashSymbol(Symbol);
  if MakeSymbolsPublic then begin
   Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
  end;
  Symbol^.VariableType:=SymbolManager.VariableType;
  case Scanner.CurrentToken of
   tstEqual:begin
    Scanner.Match(tstEqual);
    Symbol^.SymbolType:=Symbols.tstConstant;
    Symbol^.ConstantTypeRecord:=nil;
    NewTreeNode:=ParseExpression(false);
    OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
    OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
    OptimizerHighLevel.OptimizeTree(NewTreeNode);
    while not Scanner.IsEOFOrAbortError do begin
     case NewTreeNode.TreeNodeType of
      ttntOrdConst:begin
       Symbol^.IntValue:=NewTreeNode.Value;
       Symbol^.ConstantType:=tctOrdinal;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;
      ttntCharConst:begin
       Symbol^.IntValue:=word(NewTreeNode.CharValue);
       Symbol^.ConstantType:=tctOrdinal;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;
      ttntStringConst:begin
       Symbol^.StringValue:=NewTreeNode.StringData;
       Symbol^.ConstantType:=tctAnsiString;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;
      ttntFloatConst:begin
       Symbol^.FloatValue:=NewTreeNode.FloatValue;
       Symbol^.ConstantType:=tctFloat;
       SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
      end;    
      ttntTYPE,ttntTYPECONV:begin
       Symbol^.ConstantTypeRecord:=NewTreeNode.Return;
       while assigned(NewTreeNode) and
             (NewTreeNode.TreeNodeType in [ttntTYPE,ttntTYPECONV]) do begin
        NewTreeNode:=NewTreeNode.Left;
       end;
       continue;
      end;
      else begin
       Error.AbortCode(504);
      end;
     end;
     break;
    end;
    NewTreeNode.Destroy;
    Symbol^.PortabilityDirectives:=ParsePortabilityDirectives;
    Scanner.Match(tstSeparator);
   end;
   tstColon:begin
    Scanner.Match(tstColon);
    Symbol^.SymbolType:=Symbols.tstVariable;
    Symbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
    Symbol^.VariableType:=SymbolManager.VariableType;
    Symbol^.LocalProcSymbol:=CurrentProcedureFunction;
    Symbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
    Symbol^.AbsoluteReference:=false;
    Symbol^.Alias:=nil;
    ConstantType:=ParseTypeDefinition('');
    Scanner.Match(tstEqual);
{   if length(ModuleName)=0 then begin
     Symbol^.OverloadedName:=Scanner.ProcedureName+tpsOverload+Symbol^.Name;
    end else begin
     Symbol^.OverloadedName:=ModuleName+Scanner.ProcedureName+tpsOverload+Symbol^.Name;
    end;}
    Symbol^.OverloadedName:=Symbol^.Name;
    HashSymbol(Symbol);
    inc(SymbolManager.ConstantIDCounter);
    Symbol^.Constant:=ParseTypedConstantDeclaration(ConstantType,SymbolManager.ConstantIDCounter,false);
    Symbol^.TypeDefinition:=ConstantType;
    Symbol^.TypedConstant:=true;
    Symbol^.TypedTrueConstant:=true;
    Symbol^.TypedConstantReadOnly:=not LocalSwitches^.WriteableConst;
    Symbol^.PortabilityDirectives:=ParsePortabilityDirectives;
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctStop;
    Constant^.Size:=0;
    Constant^.IntValue:=0;
    Constant^.ID:=SymbolManager.ConstantIDCounter;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=false;
    SymbolManager.AddConstant(Constant);
    SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
    Scanner.Match(tstSeparator);
   end;
   else begin
    Scanner.Match(tstEqual);
   end;
  end;
 until Scanner.CurrentToken<>tstIdentifier;
end;

procedure TParser.ParseVARDeclartion;
type TAbsoluteType=(tatNone,tatMemoryConstant,tatVariableOverlay);
var Symbol,AbsoluteSymbol,StartSymbol,LastSymbol,CurrentSymbol,NextSymbol:PSymbol;
    AType:PType;
    AbsoluteAddress:int64;
    AbsoluteType:TAbsoluteType;
    NewTreeNode:TTreeNode;
    ExternalVariable:boolean;
    ConstantType:PType;
    Constant:PConstant;
    PortabilityDirectives:TPortabilityDirectives;
begin
 Scanner.Match(Scanner.CurrentToken);
 repeat

  StartSymbol:=nil;
  LastSymbol:=nil;
  repeat
   CurrentSymbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   CurrentSymbol^.DeclarationUsed:=false;
   if not assigned(StartSymbol) then begin
    StartSymbol:=CurrentSymbol;
   end;
   if assigned(LastSymbol) then begin
    LastSymbol^.Next:=CurrentSymbol;
   end;
   LastSymbol:=CurrentSymbol;
   CurrentSymbol^.Name:=Scanner.ReadIdentifier(@CurrentSymbol^.OriginalCaseName);
   HashSymbol(CurrentSymbol);
   if Scanner.CurrentToken=tstCOMMA then begin
    Scanner.Match(tstCOMMA);
   end else begin
    break;
   end;
  until (Scanner.CurrentToken<>tstIdentifier) or Scanner.IsEOFOrAbortError;

  Scanner.Match(tstCOLON);

  if assigned(StartSymbol) then begin
   AType:=ParseTypeDefinition(StartSymbol^.Name);
  end else begin
   AType:=ParseTypeDefinition('');
  end;

  AbsoluteSymbol:=nil;
  AbsoluteAddress:=0;

  AbsoluteType:=tatNone;
  Scanner.CheckForDirectives([tstABSOLUTE]);
  if Scanner.CurrentToken=tstABSOLUTE then begin
   Scanner.Match(tstABSOLUTE);
   case Scanner.CurrentToken of
    tstIdentifier:begin
     AbsoluteType:=tatVariableOverlay;
     AbsoluteSymbol:=SymbolManager.GetSymbol(Scanner.ProcedureName+Scanner.ReadIdentifier(nil),ModuleSymbol,CurrentObjectClass);
     if assigned(AbsoluteSymbol) and (AbsoluteSymbol^.SymbolType=Symbols.tstUnit) then begin
      Scanner.Match(tstIdentifier);
      Scanner.Match(tstPeriod);
      AbsoluteSymbol:=AbsoluteSymbol^.SymbolList.GetSymbol(Scanner.ReadIdentifier(nil),ModuleSymbol,CurrentObjectClass);
     end;
     if not assigned(AbsoluteSymbol) then begin
      Error.AbortCode(525);
     end;
    end;
    tstValue:begin
     AbsoluteType:=tatMemoryConstant;
     NewTreeNode:=ParseExpression(false);
     OptimizerHighLevel.ModuleSymbol:=ModuleSymbol;
     OptimizerHighLevel.CurrentObjectClass:=CurrentObjectClass;
     OptimizerHighLevel.OptimizeTree(NewTreeNode);
     if NewTreeNode.TreeNodeType=ttntORDConst then begin
      AbsoluteAddress:=NewTreeNode.Value;
     end else begin
      Error.AbortCode(526);
     end;
     NewTreeNode.Destroy;
    end;
    else Error.AbortCode(527);
   end;
  end else if Scanner.CurrentToken=tstEqual then begin
   Scanner.Match(tstEqual);
   if assigned(StartSymbol^.Next) then begin
    Error.InternalError(200605181007000);
   end else begin
    Symbol:=StartSymbol;
    Symbol^.SymbolType:=Symbols.tstVariable;
    Symbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
    Symbol^.VariableType:=SymbolManager.VariableType;
    Symbol^.LocalProcSymbol:=CurrentProcedureFunction;
    Symbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
    Symbol^.AbsoluteReference:=false;
    Symbol^.Alias:=nil;
    ConstantType:=AType;
    if length(ModuleName)=0 then begin
     Symbol^.OverloadedName:=Scanner.ProcedureName+tpsOverload+Symbol^.Name;
    end else begin
     Symbol^.OverloadedName:=ModuleName+Scanner.ProcedureName+tpsOverload+Symbol^.Name;
    end;
    HashSymbol(Symbol);
    inc(SymbolManager.ConstantIDCounter);
    Symbol^.Constant:=ParseTypedConstantDeclaration(ConstantType,SymbolManager.ConstantIDCounter,false);
    Symbol^.TypeDefinition:=ConstantType;
    Symbol^.TypedConstant:=true;
    Symbol^.TypedTrueConstant:=false;
    Symbol^.TypedConstantReadOnly:=false;
    Constant:=SymbolManager.NewConstant(ModuleSymbol);
    Constant^.ConstantType:=tctStop;
    Constant^.Size:=0;
    Constant^.IntValue:=0;
    Constant^.ID:=SymbolManager.ConstantIDCounter;
    Constant^.Dumped:=false;
    Constant^.IsPacked:=false;
    SymbolManager.AddConstant(Constant);
    SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
    Scanner.Match(tstSeparator);
    if (Scanner.CurrentToken=tstIdentifier) and not Scanner.IsEOFOrAbortError then begin
     continue;
    end;
    exit;
   end;
  end;
  PortabilityDirectives:=ParsePortabilityDirectives;
  Scanner.Match(tstSeparator);
  ExternalVariable:=false;

  if Scanner.CurrentToken=tstEXTERNAL then begin
   ExternalVariable:=true;
   Scanner.Match(tstEXTERNAL);
   Scanner.Match(tstSeparator);
  end;

  Symbol:=StartSymbol;
  while assigned(Symbol) do begin
   NextSymbol:=Symbol^.Next;
   Symbol^.Next:=nil;
{  if (length(ModuleName)<>0) and (SymbolManager.LexicalScopeLevel=0) then begin
    Symbol^.OverloadedName:=ModuleName+tpsOverload+Symbol^.Name;
   end else begin
    Symbol^.OverloadedName:=Symbol^.Name;
   end;}
   Symbol^.OverloadedName:=Symbol^.Name;
   HashSymbol(Symbol);
   if MakeSymbolsPublic and not ExternalVariable then begin
    Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
   end;
   if ExternalVariable then begin
    Symbol^.Attributes:=Symbol^.Attributes+[tsaVarExt];
   end;
   Symbol^.SymbolType:=Symbols.tstVariable;
   Symbol^.TypeDefinition:=AType;
   Symbol^.VariableLevel:=SymbolManager.LexicalScopeLevel;
   Symbol^.VariableType:=SymbolManager.VariableType;
   Symbol^.LocalProcSymbol:=CurrentProcedureFunction;
   Symbol^.LocalProcSymbolAccessedFromHigherNestedProc:=false;
   Symbol^.Alias:=nil;
   Symbol^.AbsoluteReference:=false;
   Symbol^.TypedConstant:=false;
   Symbol^.TypedTrueConstant:=false;
   Symbol^.TypedConstantReadOnly:=false;
   Symbol^.PortabilityDirectives:=PortabilityDirectives;
   case AbsoluteType of
    tatVariableOverlay:begin
     if assigned(AbsoluteSymbol) then begin
      Symbol^.VariableLevel:=AbsoluteSymbol^.VariableLevel;
      Symbol^.LocalProcSymbol:=AbsoluteSymbol^.LocalProcSymbol;
      Symbol^.LocalProcSymbolAccessedFromHigherNestedProc:=AbsoluteSymbol^.LocalProcSymbolAccessedFromHigherNestedProc;
      Symbol^.Offset:=AbsoluteSymbol^.Offset;
      Symbol^.Attributes:=Symbol^.Attributes+[tsaVarDmp];
      Symbol^.Alias:=AbsoluteSymbol;
     end else begin
      Error.InternalError(200605181007001);
     end;
    end;
    tatMemoryConstant:begin
     Symbol^.Offset:=AbsoluteAddress;
     Symbol^.AbsoluteReference:=true;
    end;
    else begin
     Symbol^.Offset:=0;
    end;
   end;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
   Symbol:=NextSymbol;
  end;
 until (Scanner.CurrentToken<>tstIdentifier) or Scanner.IsEOFOrAbortError;
end;

procedure TParser.ParseTYPEDeclartion;
var OriginalCaseName,Name:ansistring;
    ForwardedSymbol,Symbol:PSymbol;
    IsForwared:boolean;
begin
 Scanner.Match(tstTYPE);
 while not Scanner.IsEOFOrAbortError do begin
  OriginalCaseName:='';
  Name:=Scanner.ReadIdentifier(@OriginalCaseName);
  Scanner.Match(tstEQUAL);
  Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
  Symbol^.Name:=Name;
  Symbol^.OriginalCaseName:=OriginalCaseName;
  HashSymbol(Symbol);
  if MakeSymbolsPublic then begin
   Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
  end;
  Symbol^.SymbolType:=Symbols.tstType;
  Symbol^.TypeDefinition:=ParseTypeDefinition(Name);
  if assigned(Symbol^.TypeDefinition) then begin
   if not assigned(Symbol^.TypeDefinition^.Symbol) then begin
    Symbol^.TypeDefinition^.Symbol:=Symbol;
   end;
   if (Symbol^.TypeDefinition^.TypeDefinition=ttdCLASS) and assigned(Symbol^.TypeDefinition^.ClassOfType) and not assigned(Symbol^.TypeDefinition^.ClassOfType^.ClassOf) then begin
    Symbol^.TypeDefinition^.ClassOfType^.ClassOf:=Symbol;
   end;
  end;
  IsForwared:=false;
  ForwardedSymbol:=SymbolManager.GetSymbol(Symbol^.Name,ModuleSymbol,CurrentObjectClass);
  if assigned(ForwardedSymbol) then begin
   if ForwardedSymbol^.ForwardType then begin
    if assigned(ForwardedSymbol^.TypeDefinition) then begin
     ForwardedSymbol^.TypeDefinition^.Symbol:=nil;
    end;
    ForwardedSymbol^.TypeDefinition:=Symbol^.TypeDefinition;
    ForwardedSymbol^.TypeDefinition^.Symbol:=ForwardedSymbol;
    ForwardedSymbol^.ForwardType:=false;
    ForwardedSymbol^.Attributes:=ForwardedSymbol^.Attributes+Symbol^.Attributes;
    Symbol^.TypeDefinition:=nil;
    SymbolManager.DestroySymbol(Symbol);
    IsForwared:=true;
   end;
   if not IsForwared then begin
    if assigned(ForwardedSymbol) and (ForwardedSymbol^.SymbolType=Symbols.tstType) and
       (ForwardedSymbol^.TypeDefinition^.TypeDefinition=ttdClass) and
       ForwardedSymbol^.TypeDefinition^.WasForwardedClass then begin
     if assigned(Symbol^.TypeDefinition) then begin
      Symbol^.TypeDefinition^.Symbol:=nil;
     end;
     ForwardedSymbol^.TypeDefinition^.WasForwardedClass:=false;
     ForwardedSymbol^.TypeDefinition^.Symbol:=ForwardedSymbol;
     ForwardedSymbol^.Attributes:=ForwardedSymbol^.Attributes+Symbol^.Attributes;
     if (ForwardedSymbol^.TypeDefinition^.TypeDefinition=ttdCLASS) and assigned(ForwardedSymbol^.TypeDefinition^.ClassOfType) then begin
      ForwardedSymbol^.TypeDefinition^.ClassOfType^.ClassOf:=ForwardedSymbol;
     end;
     SymbolManager.DestroySymbol(Symbol);
     IsForwared:=true;
    end;
   end;
  end;
  if not IsForwared then begin
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
  end;
  if assigned(Symbol) then begin
   Symbol^.PortabilityDirectives:=ParsePortabilityDirectives;
  end;
  Scanner.Match(tstSeparator);
  if Scanner.CurrentToken<>tstIdentifier then begin
   break;
  end;
 end;
 ForwardedSymbol:=SymbolManager.CurrentList.First;
 while assigned(ForwardedSymbol) do begin
  if (ForwardedSymbol^.SymbolType=Symbols.tstType) and ForwardedSymbol^.ForwardType then begin
   Error.AbortCode(528,CorrectSymbolName(ForwardedSymbol^.Name));
  end else if (ForwardedSymbol^.SymbolType=Symbols.tstType) and (ForwardedSymbol^.TypeDefinition^.TypeDefinition=ttdClass) and
              (ForwardedSymbol^.TypeDefinition^.WasForwardedClass or ForwardedSymbol^.TypeDefinition^.ForwardClass) then begin
   Error.AbortCode(529,CorrectSymbolName(ForwardedSymbol^.Name));
  end;
  ForwardedSymbol:=ForwardedSymbol^.Next;
 end;
end;

procedure TParser.ParseLABELDeclartion;
var Symbol:PSymbol;
begin
 Scanner.Match(tstLABEL);
 repeat
  repeat
   Symbol:=SymbolManager.NewSymbol(ModuleSymbol,CurrentObjectClass,MakeSymbolsPublic);
   Symbol^.Name:=Scanner.ProcedureName+Scanner.ReadLabel;
   HashSymbol(Symbol);
   if MakeSymbolsPublic then begin
    Symbol^.Attributes:=Symbol^.Attributes+[tsaPublic,tsaPublicUnitSymbol];
   end;
   Symbol^.SymbolType:=Symbols.tstLabel;
   SymbolManager.CurrentList.AddSymbol(Symbol,ModuleSymbol,CurrentObjectClass);
   if Scanner.CurrentToken=tstComma then begin
    Scanner.Match(tstComma);
   end else begin
    break;
   end;
  until (Scanner.CurrentToken=tstSeparator) or Scanner.IsEOFOrAbortError;
  Scanner.Match(tstSeparator);
 until Scanner.IsEOFOrAbortError or not Scanner.MaybeLabel(Scanner.CurrentToken);
end;

function TParser.ParsePortabilityDirectives:TPortabilityDirectives;
begin
 result:=[];
 while not Scanner.IsEOFOrAbortError do begin
  Scanner.CheckForDirectives([tstPLATFORM,tstDEPRECATED,tstLIBRARY]);
  case Scanner.CurrentToken of
   tstPLATFORM:begin
    Scanner.Match(tstPLATFORM);
    result:=result+[tpdPLATFORM];
   end;
   tstDEPRECATED:begin
    Scanner.Match(tstDEPRECATED);
    result:=result+[tpdDEPRECATED];
   end;
   tstLIBRARY:begin
    Scanner.Match(tstLIBRARY);
    result:=result+[tpdLIBRARY];
   end;
   else begin
    break;
   end;
  end;
  Scanner.Match(tstSeparator);
 end;
end;

end.
