unit Symbols;
{$i Compiler.inc}

interface

uses SysUtils,BeRoStringHashMap,Globals,Error,BeRoGUIDTools,PointerList,StringList,CRC32,
     BeRoStream,HugeString;

const TypeKindUnknown=0;
      TypeKindInteger=1;
      TypeKindAnsiChar=2;
      TypeKindEnumeration=3;
      TypeKindFloat=4;
      TypeKindString=5;
      TypeKindSet=6;
      TypeKindClass=7;
      TypeKindMethod=8;
      TypeKindWideChar=9;
      TypeKindLString=10;
      TypeKindWString=11;
      TypeKindVariant=12;
      TypeKindArray=13;
      TypeKindRecord=14;
      TypeKindInterface=15;
      TypeKindInt64=16;
      TypeKindDynArray=17;
      TypeKindHugeChar=18;
      TypeKindUString=19;
      TypeKindHString=20;

type TSymbolAttribute=(tsaPublic,tsaExtern,tsaVarDmp,tsaVarExt,tsaUsed,
                       tsaOOPStrictPrivate,tsaOOPPrivate,tsaOOPProtected,
                       tsaOOPPublic,tsaOOPPublished,tsaPublicUnitSymbol,
                       tsaMethod,tsaFORControlVariable,tsaParameterWithDefault,
                       tsaHiddenParameter,tsaParameterSelf,tsaParameterResult,
                       tsaTemporaryExceptionVariable,tsaField,tsaInternalField,
                       tsaInherited,tsaInheritedInClass,tsaObjectVMT,tsaClassVMT,
                       tsaHidden,tsaInternalHidden,tsaMethodDefined,tsaMapped);
     TSymbolAttributes=set of TSymbolAttribute;

     TPortabilityDirective=(tpdPLATFORM,tpdDEPRECATED,tpdLIBRARY);
     TPortabilityDirectives=set of TPortabilityDirective;

     TProcedureAttribute=(tpaAssembler,tpaRegister,tpaForward,tpaInline,
                          tpaUnderscore,tpaConstructor,tpaDestructor,
                          tpaVirtual,tpaDynamic,tpaAbstract,tpaOverride,
                          tpaObject,tpaClass,tpaClassProcedure,
                          tpaInterrupt,tpaSTDCALL,tpaPASCAL,tpaCDECL,
                          tpaSAFECALL,tpaFASTCALL,tpaCALLCONV,tpaExternal,
                          tpaMessage,tpaInterface,tpaOverload,tpaReintroduce,
                          tpaLOCAL,tpaVARARGS);
     TProcedureAttributes=set of TProcedureAttribute;

     TInternalProcedure=(tipNone,tipWRITE,tipWRITELN,tipREAD,tipREADLN,
                         tipSIZEOF,tipDEC,tipINC,tipSUCC,tipPRED,tipORD,
                         tipCHR,tipNEW,tipDISPOSE,tipSETLENGTH,
                         tipLENGTH,tipASSIGNED,tipTRUNC,tipROUND,
                         tipSQR,tipSQRT,tipTYPEOF,tipTYPEINFO,
                         tipINITIALIZE,tipFINALIZE);

     TStandardType=(tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                    tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                    tstUnsigned32Bit,tstUnsigned64Bit,
                    tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar,
                    tstFloat32Bit,tstFloat64Bit,tstFloat80Bit);

     TStringType=(tstSHORTSTRING,tstLONGSTRING,tstWIDESTRING,tstHUGESTRING);

     TSymbolType=(tstLabel,tstConstant,tstVariable,tstType,tstProperty,
                  tstProcedure,tstFunction,tstUnit,tstTemp,
                  tstCaseVariantLevelPush,tstCaseVariantLevelPop,
                  tstCaseVariantPush,tstCaseVariantPop);

///  TVisibilityType=(tvtPrivate,tvtProtected,tvtPublic,tvtPublished);

     TConstantType=(tctOrdinal,
                    tctAnsiChar,tctWideChar,tctHugeChar,
                    tctAnsiString,tctWideString,tctHugeString,
                    tctShortString,tctDataString,
                    tctPAnsiChar,tctPWideChar,tctPHugeChar,
                    tctFloat,
                    tctSet,
                    tctVMT,tctVMTCLASS,
                    tctPOINTER,
                    tctAlign,
                    tctStop);

     TVariableType=(tvtGlobal,tvtLocal,tvtResult,
                    tvtObjectInstanceSelf,tvtClassInstanceSelf,tvtClassSelf,
                    tvtParameterVariable,tvtParameterResult,
                    tvtParameterConstant,tvtParameterValue,
                    tvtObjectField,tvtClassField,
                    tvtInterfaceField,
                    tvtTemporaryVariable);

     TFileType=(tftText,tftUntyped,tftTyped);

     TTypeDefinition=(ttdEmpty,ttdEnumerated,ttdBoolean,ttdSubRange,
                      ttdCurrency,ttdVariant,ttdArray,ttdRecord,ttdShortString,
                      ttdLongString,ttdFile,ttdPointer,ttdSet,ttdProcedure,
                      ttdObject,ttdClass,ttdClassRef,ttdInterface,ttdFloat,
                      ttdCExpression);

     PUnit=^TUnit;
     PSymbol=^TSymbol;
     PConstant=^TConstant;
     PConstantList=^TConstantList;
     PSymbolListStack=^TSymbolListStack;

     TUnitKind=(tukUNIT,tukPROGRAM,tukLIBRARY,tukPACKAGE);

     TSymbolManager=class;

     TSymbolList=class;

     PType=^TType;
     TType=record
      Previous,Next:PType;

      OwnerModule:PSymbol;
      OwnerObjectClass:PType;

      Symbol:PSymbol;

      ID:longword;

      // ttdInterface
      InterfaceChildOf:array of PSymbol;

      Number,LowerLimit,UpperLimit:int64;

      Unique:boolean;

      PortabilityDirectives:TPortabilityDirectives;

      TypeKind:longint;

      NeedTypeInfo:boolean;

      RuntimeTypeInfo:boolean;

      Dumped:boolean;

      case TypeDefinition:TTypeDefinition of
       ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency:(
        SubRangeType:TStandardType;
       );
       ttdArray,ttdShortString:(
        Length:int64;
        Range:PType;
        Definition:PType;
        OpenArray:boolean;
        DynamicArray:boolean;
        VariantArray:boolean;
        ConstructorArray:boolean;
        ArrayOfConst:boolean;
        OpenString:boolean;
       );
       ttdLongString:(
        LongStringType:TStandardType;
        LongStringCodePage:longword;
        LongStringReferenceCounted:boolean;
       );
       ttdRecord,ttdObject,ttdClass,ttdInterface:(
        ForwardClass:boolean;
        WasForwardedClass:boolean;
        HasVirtualTable:boolean;
        RecordAlignment:longint;
        RecordSize:longint;
        RecordPacked:boolean;
        RecordTable:TSymbolList;
        ChildOf:PSymbol;
        ClassOfType:PType;
      //InterfaceChildOf:array of PSymbol;
        GUID:TGUID;
        VirtualIndexCount:longint;
        DynamicIndexCount:longint;
       );
       ttdClassRef:(
        ClassOf:PSymbol;
       );
       ttdFile:(
        FileType:TFileType;
        FileTypeRecord:PType;
       );
       ttdPointer:(
        PointerTo:PSymbol;
        PointerFar:boolean;
       );
       ttdSet:(
        SetOf:PType;
        SetSize:byte;
       );
       ttdFloat:(
        FloatLowerLimit,FloatUpperLimit:extended;
        FloatType:TStandardType;
       );
       ttdProcedure:(
        ProcedureAttributes:TProcedureAttributes;
        MethodPointer:boolean;
        AddressOnly:boolean;
        Parameter:TSymbolList;
        ReturnType:PType;
       );
       ttdCExpression:(
       );
     end;

     TUnit=record
      Previous,Next:PUnit;
      Symbol:PSymbol;
      AfterImplementation,LocalDefined:boolean;
     end;

     PSymbolUnitObject=^TSymbolUnitObject;
     TSymbolUnitObject=record
      Previous,Next:PSymbolUnitObject;
      Stream:TBeRoStream;
     end;

     PSymbolUnitObjectList=^TSymbolUnitObjectList;
     TSymbolUnitObjectList=record
      First,Last:PSymbolUnitObject;
     end;

     PSymbolUnitResource=^TSymbolUnitResource;
     TSymbolUnitResource=record
      Previous,Next:PSymbolUnitResource;
      Stream:TBeRoStream;
     end;

     PSymbolUnitResourceList=^TSymbolUnitResourceList;
     TSymbolUnitResourceList=record
      First,Last:PSymbolUnitResource;
     end;

     PSymbolExport=^TSymbolExport;
     TSymbolExport=record
      Next:PSymbolExport;
      ModuleName:ansistring;
      SymbolName:ansistring;
      SymbolOverloadedName:ansistring;
      Name:ansistring;
      Index:longint;
     end;

     TSymbol=record
      Previous,Next:PSymbol;
      PreviousWithEqualName,NextWithEqualName:PSymbol;
      Name,ProcedureName,ParameterSuffix,OverloadedName,LibraryName,
      ExternalName,OriginalName,OriginalFileName,OriginalCaseName:ansistring;
      Attributes:TSymbolAttributes;
      PortabilityDirectives:TPortabilityDirectives;

      NameHash:longword;
      OverloadedNameHash:longword;

      OwnerModule:PSymbol;
      OwnerObjectClass:PType;
      OwnerType:PType;

      ID:longword;

      // tstConstant
      StringValue:THugeString;

      // tstVariable
      VariantPrefix:ansistring;

      LexicalScopeLevel:longint;
      LexicalScopeLevelCount:longint;

      case SymbolType:TSymbolType of
       tstConstant:(
        ConstantTypeRecord:PType;
        case ConstantType:TConstantType of
         tctOrdinal:(
          IntValue:int64;
         );
         tctAnsiChar,
         tctWideChar,
         tctHugeChar:(
          CharValue:THugeChar;
         );
         tctFloat:(
          FloatValue:extended;
         );
         tctShortString:(
          ShortStringValue:shortstring;
         );
         tctPointer:(
          PointerTo:PSymbol;
         );
         tctSet:(
          SetArray:TSetArray;
         );
       );
       tstTemp,tstType,tstVariable:(
        TypeDefinition:PType;
        Offset:longint;
        VariableLevel:longint;
        VariableType:TVariableType;
        LocalProcSymbol:PSymbol;
        LocalProcSymbolAccessedFromHigherNestedProc:boolean;
        AbsoluteReference:boolean;
        Alias:PSymbol;
        TypedConstant:boolean;
        TypedTrueConstant:boolean;
        TypedConstantReadOnly:boolean;
        CaseOfLevel:longint;
        CaseOfVariant:longint;
        Constant:PConstant;
        ForwardType:boolean;
        DeclarationUsed:boolean;
        DefaultParameterSymbol:PSymbol;
        InheritedFrom:PSymbol;
       );
       tstProperty:(
        PropertyType:PType;
        PropertyParameter:TSymbolList;
        PropertyRead:PSymbol;
        PropertyWrite:PSymbol;
        PropertyStored:PSymbol;
        PropertyImplements:TSymbolList;
        PropertyDefault:PSymbol;
        PropertyDefaultArray:boolean;
        PropertyNoDefault:boolean;
       );
       tstProcedure,tstFunction:(
        MethodOfType:PType;
        NextOverloaded:PSymbol;
        Parameter:TSymbolList;
        ResultSymbol:PSymbol;
        SelfSymbol:PSymbol;
        ReturnType:PType;
        ProcedureLevel:longint;
        ProcedureAttributes:TProcedureAttributes;
        InlineFirst:pointer;
        InlineRear:pointer;
        InternalProcedure:TInternalProcedure;
        VirtualIndex:longint;
        DynamicIndex:longint;
        MessageCode:longint;
        ForwardSymbol:PSymbol;
        MethodSymbol:PSymbol;
       );
       tstUnit:(
        UnitCheckSum:longword;
        UnitSize:longint;
        UnitIndex:longint;
        SymbolList:TSymbolList;
        Loaded:boolean;
        FirstUnit,LastUnit:PUnit;
        IsCompileInterface:boolean;
        IsInterfaceReady:boolean;
        IsUnitCompiled:boolean;
        UnitKind:TUnitKind;

        Compiler:pointer;
        Parser:pointer;

        ObjectList:TSymbolUnitObjectList;
        ResourceList:TSymbolUnitResourceList;

        SymbolPointerList:TPointerList;
        TypePointerList:TPointerList;
        SymbolTableStream:TBeRoStream;

        SymbolExports:PSymbolExport;

        CountSymbols:longint;
        CountTypes:longint;
        CountConstants:longint;

        SymbolIDCounter:longword;
        TypeIDCounter:longword;

       );
     end;

     TConstant=record
      Previous,Next:PConstant;
      OwnerModule:PSymbol;
      Dumped:boolean;
      StringDumped:boolean;
      IsPacked:boolean;
      Size:longint;
      IDNr:longint;
      ID:longword;
      StringValue:THugeString;
      case ConstantType:TConstantType of
        tctOrdinal:(IntValue:longint);
        tctAnsiChar,
        tctWideChar,
        tctHugeChar:(CharValue:THugeChar);
        tctVMT:(VMTData:pointer);
        tctVMTCLASS:(VMTClassData:pointer);
        tctFloat:(FloatValue:extended);
        tctShortString:(
         ShortStringValue:shortstring;
        );
        tctPointer:(
         PointerTo:PSymbol;
        );
        tctSet:(
         SetArray:TSetArray;
        );
        tctStop:(
        );
     end;

     TConstantList=record
      First,Last:PConstant;
     end;

     TSymbolListStack=record
      Previous,Next:PSymbolListStack;
      List:TSymbolList;
      WithLevel:ptrint;
     end;

     TSymbolList=class
      public
       SymbolManager:TSymbolManager;
       First:PSymbol;
       Last:PSymbol;
       ProcSymbol:PSymbol;
       ChildOf:TSymbolList;
       Nested:TSymbolList;
       StringHashMap:TBeRoStringHashMap;
       constructor Create(TheSymbolManager:TSymbolManager);
       destructor Destroy; override;
       procedure UnlistSymbol(var Symbol:PSymbol);
       procedure DeleteSymbol(var Symbol:PSymbol);
       procedure RemoveLastSymbol;
       function GetSymbol(const Name:ansistring;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;Childs:boolean=true):PSymbol;
       function ContainsSymbol(Symbol:PSymbol;Childs:boolean=true):boolean;
       procedure AddSymbol(Symbol:PSymbol;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;AtBegin:boolean=false;Childs:boolean=true);
     end;

     TTypeList=class
      public
       SymbolManager:TSymbolManager;
       First:PType;
       Last:PType;
       constructor Create(TheSymbolManager:TSymbolManager);
       destructor Destroy; override;
       procedure Clear;
       procedure DeleteType(var AType:PType);
     end;

     TSymbolManager=class
      private
       Options:POptions;
       Error:TError;
      public
       FirstSymbolListStack:PSymbolListStack;
       LastSymbolListStack:PSymbolListStack;
       TypeList:TTypeList;
       UnitList:TStringList;
       CurrentList:TSymbolList;
       GlobalList:TSymbolList;
       ProcList:TSymbolList;
       ConstantList:TConstantList;
       ConstantCount:longint;
       LabelCount:longint;
       VariableType:TVariableType;
       StackSize:longint;
       LexicalScopeLevel:longint;
       LexicalScopeLevelCount:longint;
       AssemblerMode:boolean;
       IDCode:longword;
       AnsiStrings:boolean;
       TypeChar,TypeWideChar,TypeHugeChar,
       TypePAnsiChar,TypePWideChar,TypePHugeChar,
       TypeAnsiString,TypeWideString,TypeUnicodeString,TypeHugeString,
       TypeSingle,TypeDouble,TypeExtended,
       TypePointer,TypeInt64,TypeLongint,TypeLongword,TypeSmallint,TypeWord,TypeShortint,
       TypeByte,TypeBoolean,TypeByteBool,TypeWordBool,TypeLongBool,TypeBool64,
       TypeTOBJECT,TypeTGUID,TypeCExpression,TypeEmpty:PType;
       GlobalSwitches:PGlobalSwitches;
       ConstantIDCounter:longword;
       constructor Create(TheError:TError;TheOptions:POptions;TheGlobalSwitches:PGlobalSwitches);
       destructor Destroy; override;
       procedure Clear;
       procedure GetDefaultTypes;
       function IsOrdinal(TypeToTest:PType):boolean;
       function GetSize(AType:PType):longint;
       function GetAlignment(AType:PType):longint;
       function GetType(AType:PType):TTypeDefinition;
       procedure AddUnit(CurrentModuleSymbol,UnitSymbol:PSymbol;AfterImplementation,LocalDefined:boolean);
       function NewSymbol(ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PSymbol;
       function NewType(ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PType;
       procedure AddUnitObjectToSymbol(Symbol:PSymbol;Stream:TBeRoStream);
       procedure AddUnitResourceToSymbol(Symbol:PSymbol;Stream:TBeRoStream);
       procedure DestroySymbolUnits(Symbol:PSymbol);
       procedure DestroySymbol(var Symbol:PSymbol);
       function IsObjectClassAncestor(Symbol:PSymbol;ObjectClassType:PType):boolean;
       function IsObjectClassAncestorType(ToCheck,ObjectClassType:PType):boolean;
       function SameTypes(A,B:PType):boolean;
       function CompatibleTypes(A,B:PType):boolean;
       function CompatibleEqualTypes(A,B:PType):boolean;
       function GetSymbol(const Name:ansistring;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;WhichWithLevel:pointer=nil):PSymbol;
       function CheckUnits(ModuleSymbol:PSymbol;ObjectClassType:PType;Symbol:PSymbol):boolean;
       function GetOverloadedProcedure(Start:PSymbol;Name:ansistring):PSymbol;
       procedure PushSymbolList(SymbolList:TSymbolList;WithLevel:longint=-1);
       procedure PopSymbolList(SymbolList:TSymbolList);
       procedure PopLastSymbolList;
       procedure FixSymbolListChilds;
       function NewConstant(ModuleSymbol:PSymbol):PConstant;
       procedure AddConstant(AConstant:PConstant);
       function CloneType(FromType:PType;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PType;
       function CloneSymbol(FromSymbol:PSymbol;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PSymbol;
       function AreConstSymbolEqual(SymbolA,SymbolB:PSymbol):boolean;
       procedure AlignRecord(RecordType:PType;DefaultAlignment:longint);
       function SearchProcedureSymbol(Symbol,FirstSymbol:PSymbol;var OK:boolean):PSymbol;
       function TypeDoNeedTypeInfo(AType:PType):boolean;
       function TypeDoNeedInitialization(AType:PType):boolean;
     end;

const ProcedureCallingConventionAttributes:TProcedureAttributes=[tpaSTDCALL,tpaPASCAL,tpaCDECL,tpaSAFECALL,tpaFASTCALL,tpaRegister];
      ProcedureCallingConventionAttributesEx:TProcedureAttributes=[tpaSTDCALL,tpaPASCAL,tpaCDECL,tpaSAFECALL,tpaFASTCALL,tpaRegister,tpaCALLCONV];
      OOPSymbolAttribute:TSymbolAttributes=[tsaOOPStrictPrivate,tsaOOPPrivate,tsaOOPProtected,tsaOOPPublic,tsaOOPPublished];

function CorrectSymbolName(S:ansistring):ansistring;
procedure HashSymbol(var Symbol:PSymbol);
function IsSymbolReference(Symbol:PSymbol):boolean;

implementation

uses BeRoUtils,TypeCheck;

function CorrectSymbolName(S:ansistring):ansistring;
begin
 result:=S;
{if (length(result)>0) and (result[1]=tpsIdentifier) then begin
  Delete(result,1,1);
 end;}
end;

procedure HashSymbol(var Symbol:PSymbol);
begin
 if assigned(Symbol) then begin
  Symbol^.NameHash:=HashString(Symbol^.Name);
  Symbol^.OverloadedNameHash:=HashString(Symbol^.OverloadedName);
 end;
end;

function IsSymbolReference(Symbol:PSymbol):boolean;
begin
 result:=(Symbol^.VariableType in [tvtParameterVariable{VAR},tvtParameterResult{OUT}]) or
         ((Symbol^.VariableType=tvtParameterConstant{CONST}) and
          ((Symbol^.TypeDefinition^.TypeDefinition=ttdEmpty) or not
           (Symbol^.TypeDefinition^.TypeDefinition in [ttdEnumerated,ttdBoolean,ttdSubRange,ttdCurrency,ttdFloat,ttdLongstring,ttdShortstring])));
end;

constructor TSymbolList.Create(TheSymbolManager:TSymbolManager);
begin
 inherited Create;
 SymbolManager:=TheSymbolManager;
 First:=nil;
 Last:=nil;
 ProcSymbol:=nil;
 ChildOf:=nil;
 Nested:=nil;
 StringHashMap:=TBeRoStringHashMap.Create;
end;

destructor TSymbolList.Destroy;
begin
 while assigned(First) do begin
  RemoveLastSymbol;
 end;
 First:=nil;
 Last:=nil;
 StringHashMap.Free;
 inherited Destroy;
end;

procedure TSymbolList.UnlistSymbol(var Symbol:PSymbol);
var StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 if assigned(Symbol) then begin
  StringHashMapEntity:=StringHashMap.Get(Symbol^.Name);
  if assigned(StringHashMapEntity) then begin
   if StringHashMapEntity^.Value=TBeRoStringHashMapValue(pointer(Symbol)) then begin
    if assigned(Symbol^.PreviousWithEqualName) then begin
     StringHashMapEntity^.Value:=TBeRoStringHashMapValue(pointer(Symbol^.PreviousWithEqualName));
    end else begin
     StringHashMap.Delete(Symbol^.Name);
    end;
   end;
  end;
 end;
end;

procedure TSymbolList.DeleteSymbol(var Symbol:PSymbol);
begin
 if assigned(Symbol) then begin
  if assigned(Symbol^.Next) then begin
   UnlistSymbol(Symbol);
   if First=Symbol then begin
    First:=First^.Next;
    First^.Previous:=nil;
   end else begin
    if assigned(Symbol^.Previous) and assigned(Symbol^.Next) then begin
     Symbol^.Previous^.Next:=Symbol^.Next;
     Symbol^.Next^.Previous:=Symbol^.Previous;
    end else begin
     SymbolManager.Error.InternalError(200605180933000);
    end;
   end;
   SymbolManager.DestroySymbol(Symbol);
  end else begin
   RemoveLastSymbol;
  end;
 end;
end;

procedure TSymbolList.RemoveLastSymbol;
begin
 if assigned(First) and assigned(Last) then begin
  if assigned(First^.Next) then begin
   Last:=Last^.Previous;
   UnlistSymbol(Last);
   SymbolManager.DestroySymbol(Last^.Next);
   Last^.Next:=nil;
  end else begin
   UnlistSymbol(First);
   SymbolManager.DestroySymbol(First);
   First:=nil;
   Last:=nil;
  end;
 end;
end;

function TSymbolList.GetSymbol(const Name:ansistring;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;Childs:boolean=true):PSymbol;
var NextOverloadedSymbol,Symbol:PSymbol;
    StringHashMapEntity:PBeRoStringHashMapEntity;
    Found,IsVisible:boolean;
    NameHash:longword;
begin
 result:=nil;
 NameHash:=HashString(Name);
 Symbol:=Last;
 if assigned(Symbol) then begin

  Found:=false;

  // Search first in the hash map
  StringHashMapEntity:=StringHashMap.Get(Name);
  if assigned(StringHashMapEntity) then begin
   Symbol:=PSymbol(pointer(StringHashMapEntity^.Value));
   Found:=assigned(Symbol) and (Symbol^.NameHash=NameHash);
  end;

  // Turn into the check loop
  while true do begin

   // If a symbol is found, check if this symbol is visible
   if Found then begin
    if not assigned(Symbol) then begin
     if Childs and assigned(ChildOf) then begin
      result:=ChildOf.GetSymbol(Name,ModuleSymbol,ObjectClassType,Childs);
     end else begin
      result:=nil;
     end;
     exit;
    end;

    // General symbol visible check
    if (Symbol^.OwnerModule=ModuleSymbol) then begin
     IsVisible:=true;
    end else begin
     IsVisible:=false;

     // Check if the symbol is defined as public unit symbol or an unit
     if {(Symbol^.SymbolType=tstUnit) AND} not assigned(ModuleSymbol) then begin
      // for TUnitManager, TOptimizerHighLevel, TOptimizerLowLevel, TOptimizerDataFlow
      IsVisible:=true;
     end else if ((tsaPublicUnitSymbol in Symbol^.Attributes) or (Symbol^.SymbolType=tstUnit)) and assigned(Symbol^.OwnerModule) and assigned(ModuleSymbol) then begin
      // Unit-wise symbol visible chck
      IsVisible:=SymbolManager.CheckUnits(ModuleSymbol,ObjectClassType,Symbol);
     end;

    end;

    if IsVisible then begin
     // Yeah this symbol is for other units/modules visible

     // Check for OOP visibly
     if (Symbol^.Attributes*OOPSymbolAttribute)<>[] then begin
      if tsaOOPStrictPrivate in Symbol^.Attributes then begin
       IsVisible:=(Symbol^.OwnerModule=ModuleSymbol) and SymbolManager.IsObjectClassAncestor(Symbol,ObjectClassType);
      end else if tsaOOPPrivate in Symbol^.Attributes then begin
       IsVisible:=Symbol^.OwnerModule=ModuleSymbol;
      end else if tsaOOPProtected in Symbol^.Attributes then begin
       IsVisible:=(Symbol^.OwnerModule=ModuleSymbol) or SymbolManager.isObjectClassAncestor(Symbol,ObjectClassType);
      end;
     end;

    end;

    if IsVisible then begin
     break;
    end else begin
     if assigned(Symbol^.PreviousWithEqualName) then begin
      Symbol:=Symbol^.PreviousWithEqualName;
     end else begin
      Symbol:=Symbol^.Previous;
     end;
    end;

   end;

   // Search again, but with hashs and normal strings now
   while assigned(Symbol) and
         ((Symbol^.NameHash<>NameHash) or (Symbol^.Name<>Name)) and (Symbol<>First) do begin
    Symbol:=Symbol^.Previous;
   end;

   // Check for an found
   Found:=assigned(Symbol) and ((Symbol^.NameHash=NameHash) and (Symbol^.Name=Name));
   if not Found then begin
    // If nothing found -> leave while check loop
    break;
   end;
  end;

  if Found then begin
   Symbol^.Attributes:=Symbol^.Attributes+[tsaUsed];
   if Symbol^.SymbolType in [tstProcedure,tstFunction] then begin
    NextOverloadedSymbol:=Symbol^.NextOverloaded;
    while assigned(NextOverloadedSymbol) do begin
     NextOverloadedSymbol^.Attributes:=NextOverloadedSymbol^.Attributes+[tsaUsed];
     NextOverloadedSymbol:=NextOverloadedSymbol^.NextOverloaded;
    end;
   end;
   result:=Symbol;
  end;
 end;
 if Childs and assigned(ChildOf) and not assigned(result) then begin
  result:=ChildOf.GetSymbol(Name,ModuleSymbol,ObjectClassType,Childs);
 end;
end;

function TSymbolList.ContainsSymbol(Symbol:PSymbol;Childs:boolean=true):boolean;
var ListSymbol:PSymbol;
begin
 result:=false;
 ListSymbol:=First;
 while assigned(ListSymbol) do begin
  if ListSymbol=Symbol then begin
   result:=true;
   exit;
  end;
  ListSymbol:=ListSymbol^.Next;
 end;
 if Childs and assigned(ChildOf) then begin
  result:=ChildOf.ContainsSymbol(Symbol);
 end;
end;

procedure TSymbolList.AddSymbol(Symbol:PSymbol;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;AtBegin:boolean=false;Childs:boolean=true);
var TestSymbol,OurNewSymbol:PSymbol;
    StringHashMapEntity:PBeRoStringHashMapEntity;
begin
 HashSymbol(Symbol);
 if ([tsaInternalField,tsaHidden]*Symbol^.Attributes)<>[] then begin
  TestSymbol:=nil;
 end else begin
  TestSymbol:=GetSymbol(Symbol^.Name,ModuleSymbol,ObjectClassType,Childs);
 end;
 if (not (Symbol^.SymbolType in [Symbols.tstCaseVariantLevelPush,Symbols.tstCaseVariantLevelPop,
                                 Symbols.tstCaseVariantPush,Symbols.tstCaseVariantPop])) and
    (assigned(TestSymbol) and
     (TestSymbol^.LexicalScopeLevel=SymbolManager.LexicalScopeLevel)) and
     (assigned(TestSymbol^.OwnerObjectClass)=assigned(Symbol^.OwnerObjectClass)) then begin
  SymbolManager.Error.AddErrorCode(3,CorrectSymbolName(Symbol^.Name));
 end else begin
  if Symbol^.OwnerModule<>ModuleSymbol then begin
   SymbolManager.Error.InternalError(200605180932000);
  end;
  if assigned(ObjectClassType) then begin
   Symbol^.OwnerObjectClass:=ObjectClassType;
  end;
  OurNewSymbol:=Symbol;
  OurNewSymbol^.LexicalScopeLevel:=SymbolManager.LexicalScopeLevel;
  if AtBegin then begin
   OurNewSymbol^.Next:=First;
   OurNewSymbol^.Previous:=nil;
  end else begin
   OurNewSymbol^.Next:=nil;
   OurNewSymbol^.Previous:=Last;
  end;
  if ([tsaInternalField,tsaHidden]*Symbol^.Attributes)=[] then begin
   StringHashMapEntity:=StringHashMap.Get(Symbol^.Name);
   if assigned(StringHashMapEntity) then begin
    TestSymbol:=PSymbol(pointer(StringHashMapEntity^.Value));
    if assigned(TestSymbol) then begin
     Symbol^.PreviousWithEqualName:=TestSymbol;
     TestSymbol^.NextWithEqualName:=Symbol;
    end;
   end else begin
    StringHashMapEntity:=StringHashMap.Get(Symbol^.Name,true);
   end;
   StringHashMapEntity^.Value:=TBeRoStringHashMapValue(pointer(OurNewSymbol));
  end;
  if AtBegin then begin
   if assigned(First) then begin
    First.Previous:=OurNewSymbol;
   end else begin
    Last:=OurNewSymbol;
   end;
   First:=OurNewSymbol;
  end else begin
   if assigned(Last) then begin
    Last.Next:=OurNewSymbol;
   end else begin
    First:=OurNewSymbol;
   end;
   Last:=OurNewSymbol;
  end;
 end;
end;

constructor TTypeList.Create(TheSymbolManager:TSymbolManager);
begin
 inherited Create;
 SymbolManager:=TheSymbolManager;
 First:=nil;
 Last:=nil;
end;

destructor TTypeList.Destroy;
begin
 Clear;
 inherited Destroy;
end;

procedure TTypeList.Clear;
var AType:PType;
begin
 while assigned(Last) do begin
  AType:=Last;
  DeleteType(AType);
 end;
 First:=nil;
 Last:=nil;
end;

procedure TTypeList.DeleteType(var AType:PType);
begin
 if assigned(AType) then begin
  if assigned(AType^.OwnerModule) and assigned(AType^.OwnerModule^.TypePointerList) then begin
   AType^.OwnerModule^.TypePointerList.Remove(AType);
  end;
  setlength(AType^.InterfaceChildOf,0);
  if First=AType then begin
   First:=AType^.Next;
  end;
  if Last=AType then begin
   Last:=AType^.Previous;
  end;
  if assigned(AType^.Next) then begin
   AType^.Next:=AType^.Previous;
  end;
  if assigned(AType^.Previous) then begin
   AType^.Previous:=AType^.Next;
  end;
  Dispose(AType);
  AType:=nil;
 end;
end;

constructor TSymbolManager.Create(TheError:TError;TheOptions:POptions;TheGlobalSwitches:PGlobalSwitches);
begin
 inherited Create;
 Options:=TheOptions;
 Error:=TheError;
 GlobalSwitches:=TheGlobalSwitches;
 FirstSymbolListStack:=nil;
 LastSymbolListStack:=nil;
 CurrentList:=nil;
 GlobalList:=nil;
 ProcList:=nil;
 TypeList:=TTypeList.Create(self);
 FillChar(ConstantList,SizeOf(TConstantList),#0);
 ConstantCount:=0;
 LabelCount:=0;
 VariableType:=tvtGlobal;
 StackSize:=0;
 LexicalScopeLevel:=0;
 LexicalScopeLevelCount:=0;
 AssemblerMode:=false;
 AnsiStrings:=true;
 IDCode:=0;
 UnitList:=TStringList.Create;
 Clear;
end;

destructor TSymbolManager.Destroy;
begin
 Clear;
 FreeAndNil(GlobalList);
 FreeAndNil(TypeList);
 FreeAndNil(UnitList);
 inherited Destroy;
end;

procedure TSymbolManager.Clear;
begin
 ConstantIDCounter:=0;
 while assigned(ConstantList.First) do begin
  if assigned(ConstantList.First) and assigned(ConstantList.Last) then begin
   if assigned(ConstantList.First^.Next) then begin
    ConstantList.Last:=ConstantList.Last^.Previous;
    ConstantList.Last^.Next^.StringValue:=nil;
    Dispose(ConstantList.Last^.Next);
    ConstantList.Last^.Next:=nil;
   end else begin
    ConstantList.First.StringValue:=nil;
    Dispose(ConstantList.First);
    ConstantList.First:=nil;
    ConstantList.Last:=nil;
   end;
  end;
 end;
 FillChar(ConstantList,SizeOf(TConstantList),#0);
 FreeAndNil(GlobalList);
 while assigned(FirstSymbolListStack) do begin
  PopLastSymbolList;
 end;
 TypeList.Clear;
 FirstSymbolListStack:=nil;
 LastSymbolListStack:=nil;
 CurrentList:=nil;
 GlobalList:=nil;
 ProcList:=nil;
 UnitList.Clear;
 GlobalList:=TSymbolList.Create(self);
 PushSymbolList(GlobalList);
 CurrentList:=GlobalList;
end;

procedure TSymbolManager.GetDefaultTypes;
var Symbol:PSymbol;
begin
 Symbol:=GetSymbol(tpsIdentifier+'CHAR');
 if assigned(Symbol) then begin
  TypeChar:=Symbol^.TypeDefinition;
 end else begin
  TypeChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'WIDECHAR');
 if assigned(Symbol) then begin
  TypeWideChar:=Symbol^.TypeDefinition;
 end else begin
  TypeWideChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'HUGECHAR');
 if assigned(Symbol) then begin
  TypeHugeChar:=Symbol^.TypeDefinition;
 end else begin
  TypeHugeChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'PANSICHAR');
 if assigned(Symbol) then begin
  TypePAnsiChar:=Symbol^.TypeDefinition;
 end else begin
  TypePAnsiChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'PWIDECHAR');
 if assigned(Symbol) then begin
  TypePWideChar:=Symbol^.TypeDefinition;
 end else begin
  TypePWideChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'PHUGECHAR');
 if assigned(Symbol) then begin
  TypePHugeChar:=Symbol^.TypeDefinition;
 end else begin
  TypePHugeChar:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'ANSISTRING');
 if assigned(Symbol) then begin
  TypeAnsiString:=Symbol^.TypeDefinition;
 end else begin
  TypeAnsiString:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'WIDESTRING');
 if assigned(Symbol) then begin
  TypeWideString:=Symbol^.TypeDefinition;
 end else begin
  TypeWideString:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'UNICODESTRING');
 if assigned(Symbol) then begin
  TypeUnicodeString:=Symbol^.TypeDefinition;
 end else begin
  TypeUnicodeString:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'HUGESTRING');
 if assigned(Symbol) then begin
  TypeHugeString:=Symbol^.TypeDefinition;
 end else begin
  TypeHugeString:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'SINGLE');
 if assigned(Symbol) then begin
  TypeSingle:=Symbol^.TypeDefinition;
 end else begin
  TypeSingle:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'DOUBLE');
 if assigned(Symbol) then begin
  TypeDouble:=Symbol^.TypeDefinition;
 end else begin
  TypeDouble:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'EXTENDED');
 if assigned(Symbol) then begin
  TypeExtended:=Symbol^.TypeDefinition;
 end else begin
  TypeExtended:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'INT64');
 if assigned(Symbol) then begin
  TypeInt64:=Symbol^.TypeDefinition;
 end else begin
  TypeInt64:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'POINTER');
 if assigned(Symbol) then begin
  TypePointer:=Symbol^.TypeDefinition;
 end else begin
  TypePointer:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'LONGINT');
 if assigned(Symbol) then begin
  TypeLongint:=Symbol^.TypeDefinition;
 end else begin
  TypeLongint:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'LONGWORD');
 if assigned(Symbol) then begin
  TypeLongword:=Symbol^.TypeDefinition;
 end else begin
  TypeLongword:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'SMALLINT');
 if assigned(Symbol) then begin
  TypeSmallint:=Symbol^.TypeDefinition;
 end else begin
  TypeSmallint:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'WORD');
 if assigned(Symbol) then begin
  TypeWord:=Symbol^.TypeDefinition;
 end else begin
  TypeWord:=nil;
 end;                                                
 Symbol:=GetSymbol(tpsIdentifier+'SHORTINT');
 if assigned(Symbol) then begin
  TypeShortint:=Symbol^.TypeDefinition;
 end else begin
  TypeShortint:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'BYTE');
 if assigned(Symbol) then begin
  TypeByte:=Symbol^.TypeDefinition;
 end else begin
  TypeByte:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'BOOLEAN');
 if assigned(Symbol) then begin
  TypeBoolean:=Symbol^.TypeDefinition;
 end else begin
  TypeBoolean:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'BYTEBOOL');
 if assigned(Symbol) then begin
  TypeByteBool:=Symbol^.TypeDefinition;
 end else begin
  TypeByteBool:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'WORDBOOL');
 if assigned(Symbol) then begin
  TypeWordBool:=Symbol^.TypeDefinition;
 end else begin
  TypeWordBool:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'LONGBOOL');
 if assigned(Symbol) then begin
  TypeLongBool:=Symbol^.TypeDefinition;
 end else begin
  TypeLongBool:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'BOOL64');
 if assigned(Symbol) then begin
  TypeBool64:=Symbol^.TypeDefinition;
 end else begin
  TypeBool64:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'TOBJECT');
 if assigned(Symbol) then begin
  TypeTOBJECT:=Symbol^.TypeDefinition;
 end else begin
  TypeTOBJECT:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'TGUID');
 if assigned(Symbol) then begin
  TypeTGUID:=Symbol^.TypeDefinition;
 end else begin
  TypeTGUID:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'$CEXPRESSION$');
 if assigned(Symbol) then begin
  TypeCExpression:=Symbol^.TypeDefinition;
 end else begin
  TypeCExpression:=nil;
 end;
 Symbol:=GetSymbol(tpsIdentifier+'$EMPTY$');
 if assigned(Symbol) then begin
  TypeEmpty:=Symbol^.TypeDefinition;
 end else begin
  TypeEmpty:=nil;
 end;
end;

function TSymbolManager.IsOrdinal(TypeToTest:PType):boolean;
begin
 result:=TypeToTest^.TypeDefinition in [ttdBoolean,ttdSubRange,ttdCurrency,ttdEnumerated,ttdPointer];
end;

function TSymbolManager.GetSize(AType:PType):longint;
begin
 if assigned(AType) then begin
  case AType^.TypeDefinition of
   ttdVariant:begin
    result:=0;
   end;
   ttdEmpty,ttdEnumerated,ttdProcedure,ttdPointer,ttdLongString,ttdClass,
   ttdInterface:begin
    case Options^.TargetArchitecture of
     taX64,taX64WIN64:begin
      result:=8;
     end;
     else {taARM,taARMEABI,taX86,taX86WIN32,taEMSCRIPTEN:}begin
      result:=4;
     end;
    end;
   end;
   ttdBoolean,ttdSubRange,ttdCurrency:begin
    case AType^.SubRangeType of
     tstSigned8Bit,tstUnsigned8Bit,tstUnsignedChar:begin
      result:=1;
     end;
     tstSigned16Bit,tstUnsigned16Bit:begin
      result:=2;
     end;
     tstSigned32Bit,tstUnsigned32Bit,tstFloat32Bit:begin
      result:=4;
     end;
     tstSigned64Bit,tstUnsigned64Bit,tstFloat64Bit:begin
      result:=8;
     end;
     tstFloat80Bit:begin
      case Options^.TargetArchitecture of
       taX86,taX86WIN32:begin
        result:=12;
       end;
       taX64,taX64WIN64:begin
        result:=16;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdShortString:begin
    result:=1+AType^.Length;
   end;
   ttdArray:begin
    if AType^.Range^.TypeDefinition=ttdSubRange then begin
     result:=((AType^.Range^.UpperLimit-AType^.Range^.LowerLimit)+1)*GetSize(AType^.Definition);
    end else begin
     result:=0;
    end;
   end;
   ttdRecord,ttdObject:begin
    result:=AType^.RecordSize;
   end;
   ttdFloat:begin
    case AType^.FloatType of
     tstSigned8Bit,tstUnsigned8Bit,tstUnsignedChar:begin
      result:=1;
     end;
     tstSigned16Bit,tstUnsigned16Bit:begin
      result:=2;
     end;
     tstSigned32Bit,tstUnsigned32Bit,tstFloat32Bit:begin
      result:=4;
     end;
     tstSigned64Bit,tstUnsigned64Bit,tstFloat64Bit:begin
      result:=8;
     end;
     tstFloat80Bit:begin
      case Options^.TargetArchitecture of
       taX86,taX86WIN32:begin
        result:=12;
       end;
       taX64,taX64WIN64:begin
        result:=16;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdFile:begin
    case AType^.FileType of
     tftText:begin
      result:=512;
     end;
     tftUntyped:begin
      result:=256;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdSet:begin
    result:=AType^.SetSize;
   end;
   else begin
    result:=0;
    Error.InternalError(200605180923000);
   end;
  end;
 end else begin
  result:=0;
  Error.InternalError(200605180923001);
 end;
end;

function TSymbolManager.GetAlignment(AType:PType):longint;
begin
 if assigned(AType) then begin
  case AType^.TypeDefinition of
   ttdVariant:begin
    result:=0;
   end;
   ttdEmpty,ttdEnumerated,ttdProcedure,ttdPointer,ttdLongString,ttdClass,
   ttdInterface:begin
    case Options^.TargetArchitecture of
     taX64,taX64WIN64:begin
      result:=8;
     end;
     else {taARM,taARMEABI,taX86,taX86WIN32,taEMSCRIPTEN:}begin
      result:=4;
     end;
    end;
   end;
   ttdBoolean,ttdSubRange,ttdCurrency:begin
    case AType^.SubRangeType of
     tstSigned8Bit,tstUnsigned8Bit,tstUnsignedChar:begin
      result:=1;
     end;
     tstSigned16Bit,tstUnsigned16Bit:begin
      result:=2;
     end;
     tstSigned32Bit,tstUnsigned32Bit,tstFloat32Bit:begin
      result:=4;
     end;
     tstSigned64Bit,tstUnsigned64Bit,tstFloat64Bit:begin
      case Options^.TargetArchitecture of
       taARMEABI,taX86WIN32:begin
        result:=8;
       end;
       taARM,taX86,taEMSCRIPTEN:begin
        result:=4;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     tstFloat80Bit:begin
      case Options^.TargetArchitecture of
       taX86,taX86WIN32:begin
        result:=4;
       end;
       taX64,taX64WIN64:begin
        result:=8;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdShortString:begin
    result:=1;
   end;               
   ttdArray:begin
    if AType^.Range^.TypeDefinition=ttdSubRange then begin
     result:=GetAlignment(AType^.Definition);
    end else begin
     result:=0;
    end;
   end;
   ttdRecord,ttdObject:begin
    result:=AType^.RecordSize;
   end;
   ttdFloat:begin
    case AType^.FloatType of
     tstSigned8Bit,tstUnsigned8Bit,tstUnsignedChar:begin
      result:=1;
     end;
     tstSigned16Bit,tstUnsigned16Bit:begin
      result:=2;
     end;
     tstSigned32Bit,tstUnsigned32Bit,tstFloat32Bit:begin
      result:=4;
     end;
     tstSigned64Bit,tstUnsigned64Bit,tstFloat64Bit:begin
      case Options^.TargetArchitecture of
       taARMEABI,taX86WIN32:begin
        result:=8;
       end;
       taARM,taX86,taEMSCRIPTEN:begin
        result:=4;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     tstFloat80Bit:begin
      case Options^.TargetArchitecture of
       taX86,taX86WIN32:begin
        result:=4;
       end;
       taX64,taX64WIN64:begin
        result:=8;
       end;
       else begin
        result:=8;
       end;
      end;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdFile:begin
    case AType^.FileType of
     tftText:begin
      case Options^.TargetArchitecture of
       taX64,taX64WIN64:begin
        result:=8;
       end;
       else {taARM,taARMEABI,taX86,taX86WIN32,taEMSCRIPTEN:}begin
        result:=4;
       end;
      end;
     end;
     tftUntyped:begin
      case Options^.TargetArchitecture of
       taX64,taX64WIN64:begin
        result:=8;
       end;
       else {taARM,taARMEABI,taX86,taX86WIN32,taEMSCRIPTEN:}begin
        result:=4;
       end;
      end;
     end;
     else begin
      result:=0;
     end;
    end;
   end;
   ttdSet:begin
    result:=1;
   end;
   else begin
    result:=0;
    Error.InternalError(200605180923000);
   end;
  end;
 end else begin
  result:=0;
  Error.InternalError(200605180923001);
 end;
end;

function TSymbolManager.GetType(AType:PType):TTypeDefinition;
begin
 if assigned(AType) then begin
  case AType^.TypeDefinition of
   ttdEnumerated:begin
    result:=ttdEnumerated;
   end;
   ttdBoolean:begin
    result:=ttdBoolean;
   end;
   ttdSubrange:begin
    result:=ttdSubrange;
   end;
   ttdCurrency:begin
    result:=ttdCurrency;
   end;
   ttdArray:begin
    result:=GetType(AType^.Definition);
   end;
   ttdRecord:begin
    result:=ttdRecord;
   end;
   ttdFloat:begin
    result:=ttdFloat;
   end;
   ttdFile:begin
    result:=ttdFile;
   end;
   ttdPointer:begin
    result:=ttdPointer;
   end;
   else begin
    result:=ttdEmpty;
    Error.InternalError(200605180924000);
   end;
  end;
 end else begin
  result:=ttdEmpty;
  Error.InternalError(200605180924001);
 end;
end;

procedure TSymbolManager.AddUnit(CurrentModuleSymbol,UnitSymbol:PSymbol;AfterImplementation,LocalDefined:boolean);
var AUnit:PUnit;
begin
 if assigned(CurrentModuleSymbol) and assigned(UnitSymbol) then begin
  AUnit:=CurrentModuleSymbol^.FirstUnit;
  while assigned(AUnit) do begin
   if AUnit^.Symbol=UnitSymbol then begin
//  Error.Abort('Unit redeclared "'+CorrectSymbolName(UnitSymbol^.Name)+'"');
    exit;
   end;
   AUnit:=AUnit^.Next;
  end;
  New(AUnit);
  FillChar(AUnit^,SizeOf(TUnit),#0);
  AUnit^.Symbol:=UnitSymbol;
  AUnit^.AfterImplementation:=AfterImplementation;
  AUnit^.LocalDefined:=LocalDefined;
  if assigned(CurrentModuleSymbol^.LastUnit) then begin
   CurrentModuleSymbol^.LastUnit^.Next:=AUnit;
   AUnit^.Previous:=CurrentModuleSymbol^.LastUnit;
   CurrentModuleSymbol^.LastUnit:=AUnit;
  end else begin
   CurrentModuleSymbol^.FirstUnit:=AUnit;
   CurrentModuleSymbol^.LastUnit:=AUnit;
  end;
  if UnitList.IndexOfObject(UnitSymbol)<0 then begin
   UnitList.AddObject(UnitSymbol^.Name,UnitSymbol);
  end;
 end;
end;

function TSymbolManager.NewSymbol(ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PSymbol;
begin
 New(result);
 FillChar(result^,SizeOf(TSymbol),#0);
 result^.OwnerModule:=ModuleSymbol;
 result^.OwnerObjectClass:=ObjectClassType;
 if assigned(ModuleSymbol) then begin
  inc(ModuleSymbol^.SymbolIDCounter);
  result^.ID:=ModuleSymbol^.SymbolIDCounter;
 end;
 if assigned(ModuleSymbol) and assigned(ModuleSymbol^.SymbolPointerList) then begin
  ModuleSymbol^.SymbolPointerList.Add(result);
 end;
end;

function TSymbolManager.NewType(ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PType;
begin
 New(result);
 FillChar(result^,SizeOf(TType),#0);
 result^.TypeKind:=0;
 result^.NeedTypeInfo:=false;
 result^.RuntimeTypeInfo:=false;
 result^.Dumped:=false;
 result^.OwnerModule:=ModuleSymbol;
 result^.OwnerObjectClass:=ObjectClassType;
 if assigned(TypeList.Last) then begin
  TypeList.Last^.Next:=result;
  result^.Previous:=TypeList.Last;
 end else begin
  TypeList.First:=result;
 end;
 TypeList.Last:=result;
 if assigned(ModuleSymbol) then begin
  inc(ModuleSymbol^.TypeIDCounter);
  result^.ID:=ModuleSymbol^.TypeIDCounter;
 end;
 if assigned(ModuleSymbol) and assigned(ModuleSymbol^.TypePointerList) then begin
  ModuleSymbol^.TypePointerList.Add(result);
 end;
end;

procedure TSymbolManager.AddUnitObjectToSymbol(Symbol:PSymbol;Stream:TBeRoStream);
var UnitObject:PSymbolUnitObject;
begin
 if assigned(Symbol) and assigned(Stream) then begin
  New(UnitObject);
  FillChar(UnitObject^,SizeOf(TSymbolUnitObject),#0);
  UnitObject.Stream:=TBeRoStream.Create;
  UnitObject.Stream.Assign(Stream);
  if assigned(Symbol^.ObjectList.Last) then begin
   Symbol^.ObjectList.Last^.Next:=UnitObject;
   Symbol^.ObjectList.Last:=UnitObject;
  end else begin
   Symbol^.ObjectList.First:=UnitObject;
   Symbol^.ObjectList.Last:=UnitObject;
  end;
 end;
end;

procedure TSymbolManager.AddUnitResourceToSymbol(Symbol:PSymbol;Stream:TBeRoStream);
var UnitResource:PSymbolUnitResource;
begin
 if assigned(Symbol) and assigned(Stream) then begin
  New(UnitResource);
  FillChar(UnitResource^,SizeOf(TSymbolUnitResource),#0);
  UnitResource.Stream:=TBeRoStream.Create;
  UnitResource.Stream.Assign(Stream);
  if assigned(Symbol^.ResourceList.Last) then begin
   Symbol^.ResourceList.Last^.Next:=UnitResource;
   Symbol^.ResourceList.Last:=UnitResource;
  end else begin
   Symbol^.ResourceList.First:=UnitResource;
   Symbol^.ResourceList.Last:=UnitResource;
  end;
 end;
end;

procedure TSymbolManager.DestroySymbolUnits(Symbol:PSymbol);
var AUnit,NextUnit:PUnit;
begin
 if assigned(Symbol) and (Symbol^.SymbolType=tstUnit) then begin
  AUnit:=Symbol^.FirstUnit;
  while assigned(AUnit) do begin
   NextUnit:=AUnit^.Next;
   Dispose(AUnit);
   AUnit:=NextUnit;
  end;
  Symbol^.FirstUnit:=nil;
  Symbol^.LastUnit:=nil;
 end;
end;

procedure TSymbolManager.DestroySymbol(var Symbol:PSymbol);
var UnitObject,NextUnitObject:PSymbolUnitObject;
    UnitResource,NextUnitResource:PSymbolUnitResource;
    SymbolExport,NextSymbolExport:PSymbolExport;
begin
 if assigned(Symbol) then begin
  if assigned(Symbol^.PreviousWithEqualName) then begin
   Symbol^.PreviousWithEqualName^.NextWithEqualName:=Symbol^.NextWithEqualName;
  end;
  if assigned(Symbol^.NextWithEqualName) then begin
   Symbol^.NextWithEqualName^.PreviousWithEqualName:=Symbol^.PreviousWithEqualName;
  end;
  if assigned(Symbol^.OwnerModule) and assigned(Symbol^.OwnerModule^.SymbolPointerList) then begin
   Symbol^.OwnerModule^.SymbolPointerList.Remove(Symbol);
  end;
  Symbol^.Name:='';
  Symbol^.ProcedureName:='';
  Symbol^.ParameterSuffix:='';
  Symbol^.OverloadedName:='';
  Symbol^.LibraryName:='';
  Symbol^.ExternalName:='';
  Symbol^.OriginalName:='';
  Symbol^.OriginalFileName:='';
  Symbol^.OriginalCaseName:='';
  Symbol^.StringValue:=nil;
  if Symbol^.SymbolType=tstUnit then begin
   DestroySymbolUnits(Symbol);
   UnitObject:=Symbol^.ObjectList.First;
   while assigned(UnitObject) do begin
    NextUnitObject:=UnitObject^.Next;
    if assigned(UnitObject^.Stream) then begin
     UnitObject^.Stream.Destroy;
     UnitObject^.Stream:=nil;
    end;
    Dispose(UnitObject);
    UnitObject:=NextUnitObject;
   end;
   UnitResource:=Symbol^.ResourceList.First;
   while assigned(UnitResource) do begin
    NextUnitResource:=UnitResource^.Next;
    if assigned(UnitResource^.Stream) then begin
     UnitResource^.Stream.Destroy;
     UnitResource^.Stream:=nil;
    end;
    Dispose(UnitResource);
    UnitResource:=NextUnitResource;
   end;
   if assigned(Symbol^.SymbolPointerList) then begin
    Symbol^.SymbolPointerList.Destroy;
   end;
   if assigned(Symbol^.TypePointerList) then begin
    Symbol^.TypePointerList.Destroy;
   end;
   if assigned(Symbol^.SymbolTableStream) then begin
    Symbol^.SymbolTableStream.Destroy;
   end;
   if assigned(Symbol^.SymbolExports) then begin
    SymbolExport:=Symbol^.SymbolExports;
    Symbol^.SymbolExports:=nil;
    while assigned(SymbolExport) do begin
     NextSymbolExport:=SymbolExport^.Next;
     SymbolExport^.ModuleName:='';
     SymbolExport^.SymbolName:='';
     SymbolExport^.SymbolOverloadedName:='';
     SymbolExport^.Name:='';
     Dispose(SymbolExport);
     SymbolExport:=NextSymbolExport;
    end;
   end;
  end;
  Dispose(Symbol);
  Symbol:=nil;
 end;
end;

function TSymbolManager.IsObjectClassAncestor(Symbol:PSymbol;ObjectClassType:PType):boolean;
var ASymbol:PSymbol;
    AType:PType;
begin
 result:=false;
 if assigned(Symbol) then begin
  AType:=ObjectClassType;
  while assigned(AType) do begin
   if AType^.ChildOf=Symbol then begin
    result:=true;
    exit;
   end else begin
    ASymbol:=AType^.ChildOf;
    if assigned(ASymbol) and (ASymbol^.SymbolType=tstType) and assigned(ASymbol^.TypeDefinition) and (ASymbol^.TypeDefinition^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
     AType:=ASymbol^.TypeDefinition;
    end else begin
     exit;
    end;
   end;
  end;
 end;
end;

function TSymbolManager.IsObjectClassAncestorType(ToCheck,ObjectClassType:PType):boolean;
var ASymbol:PSymbol;
    AType:PType;
begin
 result:=false;
 if assigned(ToCheck) then begin
  AType:=ObjectClassType;
  while assigned(AType) do begin
   if assigned(AType^.ChildOf) and (AType^.ChildOf^.TypeDefinition=ToCheck) then begin
    result:=true;
    exit;
   end else begin
    ASymbol:=AType^.ChildOf;
    if assigned(ASymbol) and (ASymbol^.SymbolType=tstType) and assigned(ASymbol^.TypeDefinition) and (ASymbol^.TypeDefinition^.TypeDefinition in [ttdOBJECT,ttdCLASS]) then begin
     AType:=ASymbol^.TypeDefinition;
    end else begin
     exit;
    end;
   end;
  end;
 end;
end;

function TSymbolManager.SameTypes(A,B:PType):boolean;
begin
 result:=TypeCheck.EqualTypes(Error,self,A,B);
end;

function TSymbolManager.CompatibleTypes(A,B:PType):boolean;
begin
 result:=TypeCheck.AreTypesCompatible(Error,self,A,B);
end;

function TSymbolManager.CompatibleEqualTypes(A,B:PType):boolean;
begin
 result:=TypeCheck.AreTypesEqualCompatible(Error,self,A,B);
end;

function TSymbolManager.GetSymbol(const Name:ansistring;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;WhichWithLevel:pointer=nil):PSymbol;
var List:PSymbolListStack;
begin
 List:=LastSymbolListStack;
 if assigned(WhichWithLevel) then begin
  longint(WhichWithLevel^):=-1;
 end;
 result:=nil;
 while assigned(List) do begin
  result:=List^.List.GetSymbol(Name,ModuleSymbol,ObjectClassType,true);
  if assigned(result) then begin
   if assigned(WhichWithLevel) then begin
    longint(WhichWithLevel^):=List.WithLevel;
   end;
   break;
  end;
  List:=List^.Previous;
 end;
end;

function TSymbolManager.CheckUnits(ModuleSymbol:PSymbol;ObjectClassType:PType;Symbol:PSymbol):boolean;
var AUnit:PUnit;
begin
 result:=false;
 if assigned(ModuleSymbol) and assigned(Symbol) then begin
  AUnit:=ModuleSymbol^.FirstUnit;
  while assigned(AUnit) do begin
   if AUnit^.Symbol=Symbol^.OwnerModule then begin
    result:=true;
    exit;
   end;
   AUnit:=AUnit^.Next;
  end;
  if assigned(Symbol^.OwnerObjectClass) and assigned(ObjectClassType) and assigned(ObjectClassType^.ChildOf) and assigned(ObjectClassType^.ChildOf^.OwnerModule) then begin
   // Check parent object/class unit too
   result:=CheckUnits(ObjectClassType^.ChildOf^.OwnerModule,ObjectClassType^.ChildOf^.TypeDefinition,Symbol);
  end;
 end;
end;

function TSymbolManager.GetOverloadedProcedure(Start:PSymbol;Name:ansistring):PSymbol;
var Hash:longword;
begin
 result:=Start;
 Hash:=HashString(Name);
 while assigned(result) and ((result^.OverloadedNameHash<>Hash) or (result^.OverloadedName<>Name)) do begin
  result:=result^.NextOverloaded;
 end;
 if (result^.OverloadedNameHash<>Hash) or (result^.OverloadedName<>Name) then begin
  result:=nil;
 end;
end;

procedure TSymbolManager.PushSymbolList(SymbolList:TSymbolList;WithLevel:longint=-1);
var SymbolListStack:PSymbolListStack;
begin
 GetMem(SymbolListStack,SizeOf(TSymbolListStack));
 FillChar(SymbolListStack^,SizeOf(TSymbolListStack),#0);
 SymbolListStack^.List:=SymbolList;
 SymbolListStack^.WithLevel:=WithLevel;
 if assigned(LastSymbolListStack) then begin
  LastSymbolListStack^.Next:=SymbolListStack;
  SymbolListStack^.Previous:=LastSymbolListStack;
 end else begin
  FirstSymbolListStack:=SymbolListStack;
 end;
 LastSymbolListStack:=SymbolListStack;
end;

procedure TSymbolManager.PopSymbolList(SymbolList:TSymbolList);
var SymbolListStack,UntilSymbolListStack,PreviousSymbolListStack:PSymbolListStack;
begin
 if assigned(SymbolList) then begin
  SymbolListStack:=LastSymbolListStack;
  while assigned(SymbolListStack) do begin
   if SymbolListStack^.List=SymbolList then begin
    UntilSymbolListStack:=SymbolListStack;
    SymbolListStack:=LastSymbolListStack;
    while assigned(SymbolListStack) do begin
     PreviousSymbolListStack:=SymbolListStack^.Previous;
     if assigned(SymbolListStack^.Previous) then begin
      SymbolListStack^.Previous^.Next:=SymbolListStack^.Next;
     end else if FirstSymbolListStack=SymbolListStack then begin
      FirstSymbolListStack:=SymbolListStack^.Next;
     end;
     if assigned(SymbolListStack^.Next) then begin
      SymbolListStack^.Next^.Previous:=SymbolListStack^.Previous;
     end else if LastSymbolListStack=SymbolListStack then begin
      LastSymbolListStack:=SymbolListStack^.Previous;
     end;
     FreeMem(SymbolListStack);
     if UntilSymbolListStack=SymbolListStack then begin
      break;
     end;
     SymbolListStack:=PreviousSymbolListStack;
    end;
    exit;
   end;
   SymbolListStack:=SymbolListStack^.Previous;
  end;
  Error.InternalError(200605180933002);
 end;
end;

procedure TSymbolManager.PopLastSymbolList;
var SymbolListStack:PSymbolListStack;
begin
 SymbolListStack:=LastSymbolListStack;
 if assigned(SymbolListStack) then begin
  if assigned(SymbolListStack^.Previous) then begin
   SymbolListStack^.Previous^.Next:=SymbolListStack^.Next;
  end else if FirstSymbolListStack=SymbolListStack then begin
   FirstSymbolListStack:=SymbolListStack^.Next;
  end;
  if assigned(SymbolListStack^.Next) then begin
   SymbolListStack^.Next^.Previous:=SymbolListStack^.Previous;
  end else if LastSymbolListStack=SymbolListStack then begin
   LastSymbolListStack:=SymbolListStack^.Previous;
  end;
  FreeMem(SymbolListStack);
 end;
end;

procedure TSymbolManager.FixSymbolListChilds;
var AType:PType;
begin
 AType:=TypeList.First;
 while assigned(AType) do begin
  if (AType^.TypeDefinition in [ttdRecord,ttdObject,ttdClass,ttdInterface]) and assigned(AType^.ChildOf) and assigned(AType^.RecordTable) then begin
   AType^.RecordTable.ChildOf:=AType^.ChildOf^.TypeDefinition^.RecordTable;
  end;
  AType:=AType^.Next;
 end;
end;

function TSymbolManager.NewConstant(ModuleSymbol:PSymbol):PConstant;
begin
 New(result);
 FillChar(result^,SizeOf(TConstant),#0);
 result^.OwnerModule:=ModuleSymbol;
end;

procedure TSymbolManager.AddConstant(AConstant:PConstant);
var AnotherConstant,YetAnotherConstant:PConstant;
begin
 AnotherConstant:=nil;
 if AConstant^.ConstantType in [tctAnsiString,tctWideString,tctHugeString] then begin
  YetAnotherConstant:=ConstantList.Last;
  while assigned(AnotherConstant) do begin
   if (AnotherConstant^.ConstantType=AConstant^.ConstantType) and
      (HugeStringCompare(AnotherConstant^.StringValue,AConstant^.StringValue)=0) then begin
    AnotherConstant:=YetAnotherConstant;
    break;
   end;
   YetAnotherConstant:=YetAnotherConstant^.Previous;
  end;
 end;
 if assigned(AnotherConstant) then begin
  AConstant.StringValue:=AnotherConstant^.StringValue;
 end;
 AConstant^.Next:=nil;
 AConstant^.Previous:=ConstantList.Last;
 if assigned(ConstantList.Last) then begin
  ConstantList.Last^.Next:=AConstant;
 end else begin
  ConstantList.First:=AConstant;
 end;
 ConstantList.Last:=AConstant;
end;

function TSymbolManager.CloneType(FromType:PType;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PType;
var ID:longword;
begin
 result:=NewType(ModuleSymbol,ObjectClassType,IsPublic);
 ID:=result^.ID;
 result^:=FromType^;
 result^.ID:=ID;
 result^.OwnerModule:=ModuleSymbol;
 result^.OwnerObjectClass:=ObjectClassType;
 case result^.TypeDefinition of
  ttdArray,ttdShortString:begin
{  if assigned(result^.Range) then begin
    result^.Range:=CloneType(result^.Range,ModuleSymbol,ObjectClassType,IsPublic);
   END;
   if assigned(result^.Definition) then begin
    result^.Definition:=CloneType(result^.Definition,ModuleSymbol,ObjectClassType,IsPublic);
   end;}
  end;
 end;
end;

function TSymbolManager.CloneSymbol(FromSymbol:PSymbol;ModuleSymbol:PSymbol=nil;ObjectClassType:PType=nil;IsPublic:boolean=false):PSymbol;
begin
 result:=NewSymbol(ModuleSymbol,ObjectClassType,IsPublic);
 result^:=FromSymbol^;
 result^.Previous:=nil;
 result^.Next:=nil;
 result^.PreviousWithEqualName:=nil;
 result^.NextWithEqualName:=nil;
end;

function TSymbolManager.AreConstSymbolEqual(SymbolA,SymbolB:PSymbol):boolean;
var Counter:byte;
begin
 result:=false;
 if ((SymbolA^.SymbolType<>tstConstant) or
     (SymbolB^.SymbolType<>tstConstant)) or
    (SymbolA^.ConstantType<>SymbolB^.ConstantType) then begin
  exit;
 end;
 case SymbolA^.ConstantType of
  tctOrdinal:begin
   result:=SymbolA^.IntValue=SymbolB^.IntValue;
  end;
  tctAnsiChar:begin
   result:=(SymbolA^.CharValue and $ff)=(SymbolB^.CharValue and $ff);
  end;
  tctWideChar:begin
   result:=(SymbolA^.CharValue and $ffff)=(SymbolB^.CharValue and $ffff);
  end;
  tctHugeChar:begin
   result:=SymbolA^.CharValue=SymbolB^.CharValue;
  end;
  tctFloat:begin
   result:=SymbolA^.FloatValue=SymbolB^.FloatValue;
  end;
  tctAnsiString:begin
   result:=HugeStringToAnsiString(SymbolA^.StringValue)=HugeStringToAnsiString(SymbolB^.StringValue);
  end;
  tctWideString:begin
   result:=HugeStringToWideString(SymbolA^.StringValue)=HugeStringToWideString(SymbolB^.StringValue);
  end;
  tctHugeString:begin
   result:=HugeStringCompare(SymbolA^.StringValue,SymbolB^.StringValue)=0;
  end;
  tctShortString:begin
   result:=SymbolA^.ShortStringValue=SymbolB^.ShortStringValue;
  end;
  tctDataString:begin
   result:=HugeStringToAnsiString(SymbolA^.StringValue)=HugeStringToAnsiString(SymbolB^.StringValue);
  end;
  tctPointer:begin
   result:=SymbolA^.PointerTo=SymbolB^.PointerTo;
  end;
  tctSet:begin
   result:=true;
   for Counter:=low(TSetArray) to high(TSetArray) do begin
    if SymbolA^.SetArray[Counter]<>SymbolB^.SetArray[Counter] then begin
     result:=false;
     break;
    end;
   end;
  end;
 end;
end;

procedure TSymbolManager.AlignRecord(RecordType:PType;DefaultAlignment:longint);
var TypeSize,Alignment,Allocated:longint;
    SizeStack:array of longint;
    AlignmentStack:array of longint;
    StackPointer:longint;
 procedure ProcessSymbol(Symbol:PSymbol;Store:boolean);
 var AType:PType;
 begin
  case Symbol^.SymbolType of
   Symbols.tstVariable:begin
    AType:=Symbol^.TypeDefinition;
    TypeSize:=GetSize(AType);
    Alignment:=GetAlignment(AType);
    if (DefaultAlignment=1) or RecordType^.RecordPacked then begin
     Alignment:=1;
    end else if (DefaultAlignment>=2) and (Alignment<DefaultAlignment) then begin
     Alignment:=DefaultAlignment;
    end;
    if AlignmentStack[StackPointer]<Alignment then begin
     AlignmentStack[StackPointer]:=Alignment;
    end;
    if RecordType^.RecordAlignment<Alignment then begin
     RecordType^.RecordAlignment:=Alignment;
    end;
    if (Alignment>1) and ((SizeStack[StackPointer] and (Alignment-1))<>0) then begin
     SizeStack[StackPointer]:=(SizeStack[StackPointer]+(Alignment-1)) and not (Alignment-1);
    end;
    if Store then begin
     Symbol^.Offset:=SizeStack[StackPointer];
    end;
    inc(SizeStack[StackPointer],TypeSize);
    if RecordType^.RecordSize<SizeStack[StackPointer] then begin
     RecordType^.RecordSize:=SizeStack[StackPointer];
    end;
   end;
   tstCaseVariantLevelPush:begin
    if (AlignmentStack[StackPointer]>1) and ((SizeStack[StackPointer] and (AlignmentStack[StackPointer]-1))<>0) then begin
     SizeStack[StackPointer]:=(SizeStack[StackPointer]+(AlignmentStack[StackPointer]-1)) and not (AlignmentStack[StackPointer]-1);
    end;
    inc(StackPointer);
    while (StackPointer+1)>=Allocated do begin
     inc(Allocated,Allocated);
     SetLength(SizeStack,Allocated);
     SetLength(AlignmentStack,Allocated);
    end;
    SizeStack[StackPointer]:=SizeStack[StackPointer-1];
    AlignmentStack[StackPointer]:=AlignmentStack[StackPointer-1];
   end;
   tstCaseVariantLevelPop:begin
    dec(StackPointer);
   end;
   tstCaseVariantPush:begin
    SizeStack[StackPointer]:=SizeStack[StackPointer-1];
    AlignmentStack[StackPointer]:=AlignmentStack[StackPointer-1];
   end;
   tstCaseVariantPop:begin
   end;
  end;
 end;
var Symbol:PSymbol;
    AType:PType;
    Count,Counter:longint;
    ChildOfList:array of PType;
begin
 SizeStack:=nil;
 AlignmentStack:=nil;
 ChildOfList:=nil;
 try
  if assigned(RecordType^.RecordTable) then begin
   Allocated:=16;
   SetLength(SizeStack,Allocated);
   SetLength(AlignmentStack,Allocated);
   StackPointer:=0;
   SizeStack[StackPointer]:=0;
   AlignmentStack[StackPointer]:=0;
   RecordType^.RecordAlignment:=0;
   RecordType^.RecordSize:=0;
   if assigned(RecordType^.ChildOf) then begin
    Count:=0;
    AType:=RecordType^.ChildOf^.TypeDefinition;
    while assigned(AType) do begin
     inc(Count);
     if assigned(AType^.ChildOf) then begin
      AType:=AType^.ChildOf^.TypeDefinition;
     end else begin
      break;
     end;
    end;
    SetLength(ChildOfList,Count);
    Counter:=Count;
    AType:=RecordType^.ChildOf^.TypeDefinition;
    while assigned(AType) do begin
     dec(Counter);
     ChildOfList[Counter]:=AType;
     if assigned(AType^.ChildOf) then begin
      AType:=AType^.ChildOf^.TypeDefinition;
     end else begin
      break;
     end;
    end;
    for Counter:=0 to Count-1 do begin
     Symbol:=ChildOfList[Counter]^.RecordTable.First;
     while assigned(Symbol) do begin
      ProcessSymbol(Symbol,false);
      Symbol:=Symbol^.Next;
     end;
    end;
    SetLength(ChildOfList,0);
   end;
   Symbol:=RecordType^.RecordTable.First;
   while assigned(Symbol) do begin
    ProcessSymbol(Symbol,true);
    Symbol:=Symbol^.Next;
   end;
   if ((RecordType^.RecordAlignment>1) and ((RecordType^.RecordSize and (RecordType^.RecordAlignment-1))<>0)) and not RecordType^.RecordPacked then begin
    RecordType^.RecordSize:=(RecordType^.RecordSize+(RecordType^.RecordAlignment-1)) and not (RecordType^.RecordAlignment-1);
   end;
  end;
 finally
  SetLength(SizeStack,0);
  SetLength(AlignmentStack,0);
  SetLength(ChildOfList,0);
 end;
end;

function TSymbolManager.SearchProcedureSymbol(Symbol,FirstSymbol:PSymbol;var OK:boolean):PSymbol;
begin
 result:=FirstSymbol;
 OK:=true;
 while assigned(result) do begin
  case CompareParameters(Error,self,Symbol^.Parameter,result^.Parameter,tcptNONE,[tcpoCOMPAREDEFAULTVALUE]) of
   tcteExact,tcteEqual:begin
    if assigned(Symbol^.ReturnType)<>assigned(result^.ReturnType) then begin
     OK:=false;
    end else if assigned(Symbol^.ReturnType) and assigned(result^.ReturnType) then begin
     OK:=EqualTypes(Error,self,Symbol^.ReturnType,result^.ReturnType);
    end else begin
     OK:=true;
    end;
   end;
   else begin
    OK:=false;
   end;
  end;
  if OK then begin
   break;
  end else begin
   result:=result^.NextOverloaded;
  end;
 end;
 OK:=OK and assigned(result);
end;

function TSymbolManager.TypeDoNeedTypeInfo(AType:PType):boolean;
var Symbol:PSymbol;
begin
 result:=false;
 if assigned(AType) then begin
  case AType^.TypeDefinition of
   ttdEmpty:begin
   end;
   ttdEnumerated:begin
    result:=true;
   end;
   ttdBoolean:begin
    result:=true;
   end;
   ttdSubRange:begin
    result:=true;
   end;
   ttdCurrency:begin
    result:=true;
   end;
   ttdVariant:begin
    result:=true;
   end;
   ttdArray:begin
    result:=AType^.DynamicArray or (assigned(AType) and TypeDoNeedTypeInfo(AType^.Definition));
   end;
   ttdRecord:begin
    result:=false;
    if assigned(AType^.RecordTable) then begin
     Symbol:=AType^.RecordTable.First;
     while assigned(Symbol) do begin
      case Symbol^.SymbolType of
       tstVariable:begin
        if assigned(Symbol^.TypeDefinition) and
           (Symbol^.TypeDefinition^.TypeDefinition in [ttdVariant,ttdArray,ttdRecord,ttdLongString,ttdObject,ttdClass,ttdInterface]) and
           TypeDoNeedTypeInfo(Symbol^.TypeDefinition) then begin
         result:=true;
        end;
       end;
      end;
      Symbol:=Symbol^.Next;
     end;
    end;
   end;
   ttdShortString:begin
    result:=true;
   end;
   ttdLongString:begin
    result:=true;
   end;
   ttdFile:begin
    result:=false;
   end;
   ttdPointer:begin
    result:=false;
   end;
   ttdSet:begin
    result:=true;
   end;
   ttdProcedure:begin
    result:=false;
   end;
   ttdObject:begin
    result:=AType^.HasVirtualTable;
    if assigned(AType^.RecordTable) and not result then begin
     Symbol:=AType^.RecordTable.First;
     while assigned(Symbol) do begin
      case Symbol^.SymbolType of
       tstVariable:begin
        if assigned(Symbol^.TypeDefinition) and
           (Symbol^.TypeDefinition^.TypeDefinition in [ttdVariant,ttdArray,ttdRecord,ttdLongString,ttdObject,ttdClass,ttdInterface]) and
           TypeDoNeedTypeInfo(Symbol^.TypeDefinition) then begin
         result:=true;
        end;
       end;
      end;
      Symbol:=Symbol^.Next;
     end;
    end;
   end;
   ttdClass:begin
    result:=true;
   end;
   ttdClassRef:begin
    result:=false;
   end;
   ttdInterface:begin
    result:=true;
   end;
   ttdFloat:begin
    result:=true;
   end;
   ttdCExpression:begin
    result:=false;
   end;
  end;
 end;
end;

function TSymbolManager.TypeDoNeedInitialization(AType:PType):boolean;
var Symbol:PSymbol;
begin
 result:=false;
 if assigned(AType) then begin
  case AType^.TypeDefinition of
   ttdEmpty:begin
   end;
   ttdEnumerated:begin
    result:=false;
   end;
   ttdBoolean:begin
    result:=false;
   end;
   ttdSubRange:begin
    result:=false;
   end;
   ttdCurrency:begin
    result:=false;
   end;
   ttdVariant:begin
    result:=false;
   end;
   ttdArray:begin
    result:=AType^.DynamicArray or (assigned(AType) and TypeDoNeedInitialization(AType^.Definition));
   end;
   ttdRecord:begin
    result:=false;
    if assigned(AType^.RecordTable) then begin
     Symbol:=AType^.RecordTable.First;
     while assigned(Symbol) do begin
      case Symbol^.SymbolType of
       tstVariable:begin
        if assigned(Symbol^.TypeDefinition) and
           (Symbol^.TypeDefinition^.TypeDefinition in [ttdVariant,ttdArray,ttdRecord,ttdLongString,ttdObject,ttdClass,ttdInterface]) and
           TypeDoNeedInitialization(Symbol^.TypeDefinition) then begin
         result:=true;
        end;
       end;
      end;
      Symbol:=Symbol^.Next;
     end;
    end;
   end;
   ttdShortString:begin
    result:=false;
   end;
   ttdLongString:begin
    result:=true;
   end;
   ttdFile:begin
    result:=false;
   end;
   ttdPointer:begin
    result:=false;
   end;
   ttdSet:begin
    result:=false;
   end;
   ttdProcedure:begin
    result:=false;
   end;
   ttdObject:begin
    result:=AType^.HasVirtualTable;
    if assigned(AType^.RecordTable) and not result then begin
     Symbol:=AType^.RecordTable.First;
     while assigned(Symbol) do begin
      case Symbol^.SymbolType of
       tstVariable:begin
        if assigned(Symbol^.TypeDefinition) and
           (Symbol^.TypeDefinition^.TypeDefinition in [ttdVariant,ttdArray,ttdRecord,ttdLongString,ttdObject,ttdClass,ttdInterface]) and
           TypeDoNeedInitialization(Symbol^.TypeDefinition) then begin
         result:=true;
        end;
       end;
      end;
      Symbol:=Symbol^.Next;
     end;
    end;
   end;
   ttdClass:begin
    result:=false;
   end;
   ttdClassRef:begin
    result:=false;
   end;
   ttdInterface:begin
    result:=true;
   end;
   ttdFloat:begin
    result:=false;
   end;
   ttdCExpression:begin
    result:=false;
   end;
  end;
 end;
end;

end.

