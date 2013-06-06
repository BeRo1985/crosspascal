unit TypeCheck;
{$i Compiler.inc}

interface

uses Symbols,Tree,Error;

type TCompareParametersType=(tcptNONE,tcptVALUEEQUALCONST,tcptALL,tcptPROCVAR);
     TCompareParametersOption=(tcpoALLOWDEFAULTS,tcpoIGNOREHIDDEN,tcpoALLOWCONVERT,tcpoCOMPAREDEFAULTVALUE);
     TCompareParametersOptions=set of TCompareParametersOption;

     TCompareTypesOption=(tctoINTERNAL,tctoEXPLICIT,tctoCHECKOPERATOR,tctoALLOWVARIANT);
     TCompareTypesOptions=set of TCompareTypesOption;

     TCompareTypesEqual=(
      tcteIncompatible,
      tcteConvertOperator,
      tcteConvertWithPossibleLossOfData,
      tcteConvertWithLessPreferedConversion,
      tcteConvertCompatible,
      tcteEqual,
      tcteExact);

     TConvertType=(
      tctNone,
      tctEqual,
      tctNotPossible,
      tctStringToString,
      tctCharToString,
      tctCharToCharArray,
      tctPCharToString,
      tctPCharToPChar,
      tctCCharToPChar,
      tctCStringToPChar,
      tctCStringToInt,
      tctAnsiStringToPChar,
      tctStringToCharArray,
      tctCharArrayToString,
      tctArrayToPointer,
      tctPointerToArray,
      tctIntegerToInteger,
      tctIntegerToBoolean,
      tctBooleanToBoolean,
      tctBooleanToInteger,
      tctFloatToFloat,
      tctIntegerToFloat,
      tctFloatToCurrency,
      tctProcToProcVar,
      tctArrayConstructorToSet,
      tctLoadSmallSet,
      tctCOrdToPointer,
      tctInterfaceToString,
      tctInterfaceToGUID,
      tctClassToInterface,
      tctCharToChar,
      tctNormalToSmallSet,
      tctDynArrayToOpenArray,
      tctPWideCharToString,
      tctPHugeCharToString,
      tctVariantToDynArray,
      tctDynArrayToVariant,
      tctVariantToEnum,
      tctEnumToVariant,
      tctInterfaceToVariant,
      tctVariantToInterface,
      tctArrayToDynArray);

function CompareTypesExt(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType;FromTreeNodeType:TTreeNodeType;var ConvertType:TConvertType;var ProcType:PType;CompareTypesOptions:TCompareTypesOptions):TCompareTypesEqual;
function EqualTypes(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
function EqualTypesEx(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
function AreTypesCompatible(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
function AreTypesEqualCompatible(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
function AreTypesSubEqual(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
function CompareParameters(Error:TError;SymbolManager:TSymbolManager;ParametersA,ParametersB:TSymbolList;ParametersType:TCompareParametersType;Options:TCompareParametersOptions):TCompareTypesEqual;
function CompareProcToProcVar(Error:TError;SymbolManager:TSymbolManager;ProcFrom,ProcTo:PType):TCompareTypesEqual;
function CompareCallParameters(Error:TError;SymbolManager:TSymbolManager;SymbolParameters:TSymbolList;CallParameters:TTreeNode;var ParameterImbalance:longint;var TypeA,TypeB:PType;var FirstNeededDefaultSymbol:PSymbol):TCompareTypesEqual;

implementation

function CompareTypesExt(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType;FromTreeNodeType:TTreeNodeType;var ConvertType:TConvertType;var ProcType:PType;CompareTypesOptions:TCompareTypesOptions):TCompareTypesEqual;
var SubEqualType:TCompareTypesEqual;
    SubConvertType:TConvertType;
    SubProcType:PType;
    ClassType:PType;
    Counter:longint;
begin
 result:=tcteIncompatible;
 ConvertType:=tctNotPossible;
 if not (assigned(FromType) and assigned(ToType)) then begin
  exit;
 end;
 if FromType=ToType then begin
  result:=tcteExact;
  ConvertType:=tctEqual;
  exit;
 end;
 if (FromType^.TypeDefinition=ttdCEXPRESSION) or (ToType^.TypeDefinition=ttdCEXPRESSION) then begin
  result:=tcteExact;
  ConvertType:=tctEqual;
  exit;
 end;
 case ToType^.TypeDefinition of

  // ------------

  ttdCurrency:begin
   case FromType^.TypeDefinition of
    ttdSubRange:begin
     if FromType^.SubRangeType in [tstFloat32Bit,tstFloat64Bit,tstFloat80Bit] then begin
      if ord(FromType^.SubRangeType)<ord(ToType^.SubRangeType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithLessPreferedConversion;
      end;
      ConvertType:=tctFloatToCurrency;
     end else if (FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                             tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                             tstUnsigned32Bit,tstUnsigned64Bit]) then begin
      if ord(FromType^.SubRangeType)<ord(ToType^.SubRangeType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithLessPreferedConversion;
      end;
      ConvertType:=tctEqual;
     end;
    end;
   end;
  end;

  // ------------

  ttdSubRange:begin
   case FromType^.TypeDefinition of
    ttdShortString,ttdLongString:begin
     if ToType^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar] then begin
      ConvertType:=tctCharToString;
      result:=tcteConvertCompatible;
     end;
    end;
    ttdSubRange:begin
     if ToType^.SubRangeType in [tstFloat32Bit,tstFloat64Bit,tstFloat80Bit] then begin
      if FromType^.SubRangeType in [tstFloat32Bit,tstFloat64Bit,tstFloat80Bit] then begin
       if FromType^.SubRangeType=ToType^.SubRangeType then begin
        result:=tcteEqual;
       end else begin
        if (FromTreeNodeType=ttntFloatConst) or not (tctoEXPLICIT in CompareTypesOptions) then begin
         if ord(FromType^.SubRangeType)<ord(ToType^.SubRangeType) then begin
          result:=tcteConvertCompatible;
         end else begin
          result:=tcteConvertWithLessPreferedConversion;
         end;
         ConvertType:=tctFloatToFloat;
        end;
       end;
      end else if FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                             tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                             tstUnsigned32Bit,tstUnsigned64Bit] then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctIntegerToFloat;
      end;
     end else if FromType^.SubRangeType=ToType^.SubRangeType then begin
      if (FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                     tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                     tstUnsigned32Bit,tstUnsigned64Bit,tstUnsignedChar,
                                     tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
       if (FromType^.LowerLimit=ToType^.LowerLimit) and
          (FromType^.UpperLimit=ToType^.UpperLimit) then begin
        result:=tcteEqual;
       end else begin
        result:=tcteConvertCompatible;
        ConvertType:=tctIntegerToInteger;
       end;
      end else begin
       Error.AbortCode(10000);
      end;
     end else begin
      case ToType^.SubRangeType of
       tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,tstSigned64Bit,
       tstUnsigned8Bit,tstUnsigned16Bit,tstUnsigned32Bit,tstUnsigned64Bit:begin
        case FromType^.SubRangeType of
         tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,tstSigned64Bit,
         tstUnsigned8Bit,tstUnsigned16Bit,tstUnsigned32Bit,tstUnsigned64Bit:begin
          if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
           result:=tcteConvertCompatible;
          end else begin
           result:=tcteConvertWithPossibleLossOfData;
          end;
          ConvertType:=tctIntegerToInteger;
         end;
         tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar:begin
          if tctoEXPLICIT in CompareTypesOptions then begin
           if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
            result:=tcteConvertCompatible;
           end else begin
            result:=tcteConvertWithPossibleLossOfData;
           end;
           ConvertType:=tctIntegerToInteger;
          end;
         end;
        end;
       end;
       tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar:begin
        case FromType^.SubRangeType of
         tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,tstSigned64Bit,
         tstUnsigned8Bit,tstUnsigned16Bit,tstUnsigned32Bit,tstUnsigned64Bit:begin
          if tctoEXPLICIT in CompareTypesOptions then begin
           if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
            result:=tcteConvertCompatible;
           end else begin
            result:=tcteConvertWithPossibleLossOfData;
           end;
           ConvertType:=tctIntegerToInteger;
          end;
         end;
         tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar:begin
          if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
           result:=tcteConvertCompatible;
          end else begin
           result:=tcteConvertWithPossibleLossOfData;
          end;
          ConvertType:=tctCharToChar;
         end;
        end;
       end;
      end;
     end;
    end;
    ttdBoolean:begin
     if (ToType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                  tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                  tstUnsigned32Bit,tstUnsigned64Bit]) and
        (tctoEXPLICIT in CompareTypesOptions) then begin
      if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithPossibleLossOfData;
      end;
      ConvertType:=tctBooleanToInteger;
     end;
    end;
    ttdEnumerated:begin
     if tctoEXPLICIT in CompareTypesOptions then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToInteger;
     end;
    end;
    ttdClassRef,ttdPointer,ttdProcedure:begin
     if tctoEXPLICIT in CompareTypesOptions then begin
      result:=tcteConvertCompatible;
      if FromTreeNodeType=ttntNIL then begin
       ConvertType:=tctEqual;
      end else begin
       ConvertType:=tctIntegerToInteger;
      end;
     end;
    end;
    ttdFloat:begin
     if ToType^.SubRangeType in [tstFloat32Bit,tstFloat64Bit,tstFloat80Bit] then begin
      if ord(FromType^.SubRangeType)<ord(ToType^.SubRangeType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithLessPreferedConversion;
      end;
      ConvertType:=tctFloatToFloat;
     end else if ToType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                          tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                          tstUnsigned32Bit,tstUnsigned64Bit] then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToFloat;
     end;
    end;
   end;
  end;


  // ------------

  ttdBoolean:begin
   case FromType^.TypeDefinition of
    ttdBoolean:begin
     if FromType^.UpperLimit=ToType^.UpperLimit then begin
      result:=tcteEqual;
      ConvertType:=tctEqual;
     end else begin
      result:=tcteConvertCompatible;
      ConvertType:=tctBooleanToBoolean;
     end;
    end;
    ttdSubRange,ttdCurrency:begin
     if (ToType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                  tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                  tstUnsigned32Bit,tstUnsigned64Bit]) and
        (tctoEXPLICIT in CompareTypesOptions) then begin
      if SymbolManager.GetSize(FromType)<=SymbolManager.GetSize(ToType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithPossibleLossOfData;
      end;
      ConvertType:=tctIntegerToBoolean;
     end;
    end;
   end;
  end;

  // ------------

  ttdShortString,ttdLongString:begin
   case FromType^.TypeDefinition of
    ttdShortString,ttdLongString:begin
     if FromTreeNodeType=ttntSTRINGConst then begin
      if (FromType^.TypeDefinition=ToType^.TypeDefinition) and
         ((FromType^.TypeDefinition=ttdShortString) or
          (FromType^.LongStringType=ToType^.LongStringType)) then begin
       result:=tcteEqual;
      end else begin
       ConvertType:=tctStringToString;
       if (FromType^.TypeDefinition=ttdLongString) and
          (ToType^.TypeDefinition=ttdLongString) and
          (FromType^.LongStringType=tstUnsignedHugeChar) then begin
        result:=tcteConvertWithPossibleLossOfData;
       end else if (FromType^.TypeDefinition=ttdLongString) and
                    (ToType^.TypeDefinition=ttdLongString) and
                    (ToType^.LongStringType=tstUnsignedHugeChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else if (FromType^.TypeDefinition=ttdLongString) and
          (ToType^.TypeDefinition=ttdLongString) and
          (FromType^.LongStringType=tstUnsignedWideChar) then begin
        result:=tcteConvertWithPossibleLossOfData;
       end else if (FromType^.TypeDefinition=ttdLongString) and
                    (ToType^.TypeDefinition=ttdLongString) and
                    (ToType^.LongStringType=tstUnsignedWideChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else if (FromType^.TypeDefinition=ttdLongString) and
                   (ToType^.TypeDefinition=ttdShortString) then begin
        result:=tcteConvertWithPossibleLossOfData;
       end else if (FromType^.TypeDefinition=ttdShortString) and
                   (ToType^.TypeDefinition=ttdLongString) then begin
        result:=tcteConvertCompatible;
       end else begin
        result:=tcteEqual;
       end;
      end;
     end else if (FromType^.TypeDefinition=ToType^.TypeDefinition) and
                 (FromType^.LongStringType=ToType^.LongStringType) and
                 ((FromType^.TypeDefinition<>ttdShortString) or
                  (FromType^.length=ToType^.length)) then begin
      result:=tcteEqual;
     end else begin
      ConvertType:=tctStringToString;
      if FromType^.TypeDefinition=ttdShortString then begin
       if ToType^.TypeDefinition=ttdShortString then begin
        result:=tcteConvertCompatible;
       end else if (ToType^.TypeDefinition=ttdLongString) and
                   (ToType^.LongStringType=tstUnsignedChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end else if FromType^.LongStringType=tstUnsignedHugeChar then begin
       if (ToType^.TypeDefinition=ttdLongString) and
          (ToType^.LongStringType=tstUnsignedHugeChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end else if FromType^.LongStringType=tstUnsignedWideChar then begin
       if (ToType^.TypeDefinition=ttdLongString) and
          (ToType^.LongStringType=tstUnsignedWideChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end else if FromType^.LongStringType=tstUnsignedChar then begin
       if (ToType^.TypeDefinition=ttdLongString) and
          (ToType^.LongStringType=tstUnsignedChar) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end;
     end;
    end;
    ttdSubRange:begin
     if FromType^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar] then begin
      ConvertType:=tctCharToString;
      result:=tcteConvertCompatible;
     end;
    end;
    ttdArray:begin
     if SymbolManager.GlobalSwitches^.ExtendedSyntax then begin
      if assigned(FromType^.Definition) and
         (FromType^.Definition^.TypeDefinition=ttdSubRange) and
         (FromType^.Definition^.SubRangeType=tstUnsignedChar) then begin
       if FromTreeNodeType=ttntSTRINGConst then begin
        ConvertType:=tctStringToString;
        if (ToType^.TypeDefinition=ttdShortString) and not SymbolManager.AnsiStrings then begin
         result:=tcteEqual;
        end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedChar) and not SymbolManager.AnsiStrings then begin
         result:=tcteEqual;
        end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedWideChar) then begin
         result:=tcteConvertWithPossibleLossOfData;
        end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedHugeChar) then begin
         result:=tcteConvertWithPossibleLossOfData;
        end else begin
         result:=tcteConvertCompatible;
        end;
       end else begin
        ConvertType:=tctCharArrayToString;
        if FromType^.DynamicArray then begin
         if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedChar) then begin
          result:=tcteConvertCompatible;
         end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedWideChar) then begin
          result:=tcteConvertWithPossibleLossOfData;
         end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedHugeChar) then begin
          result:=tcteConvertWithPossibleLossOfData;
         end else begin
          result:=tcteConvertWithLessPreferedConversion;
         end;
        end else begin
         if ToType^.TypeDefinition=ttdShortString then begin
          if FromType^.length<=255 then begin
           result:=tcteConvertCompatible;
          end;
         end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedChar) then begin
          if FromType^.length<=255 then begin
           result:=tcteConvertWithLessPreferedConversion;
          end else begin
           result:=tcteConvertCompatible;
          end;
         end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedWideChar) then begin
          result:=tcteConvertWithPossibleLossOfData;
         end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedHugeChar) then begin
          result:=tcteConvertWithPossibleLossOfData;
         end else begin
          result:=tcteConvertWithLessPreferedConversion;
         end;
        end;
       end;
      end else if assigned(FromType^.Definition) and
                  (FromType^.Definition^.TypeDefinition=ttdSubRange) and
                  (FromType^.Definition^.SubRangeType=tstUnsignedWideChar) then begin
       ConvertType:=tctCharArrayToString;
       if (ToType^.TypeDefinition=ttdLongString) and
          (ToType^.LongStringType=tstUnsignedHugeChar) then begin
        result:=tcteConvertWithPossibleLossOfData;
       end else if (ToType^.TypeDefinition=ttdLongString) and
                   (ToType^.LongStringType=tstUnsignedWideChar) then begin
        result:=tcteConvertCompatible;
       end else if (ToType^.TypeDefinition=ttdShortString) and
                   (FromType^.LowerLimit>(255*sizeof(widechar))) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end else if assigned(FromType^.Definition) and
                  (FromType^.Definition^.TypeDefinition=ttdSubRange) and
                  (FromType^.Definition^.SubRangeType=tstUnsignedHugeChar) then begin
       ConvertType:=tctCharArrayToString;
       if (ToType^.TypeDefinition=ttdLongString) and
          (ToType^.LongStringType=tstUnsignedHugeChar) then begin
        result:=tcteConvertCompatible;
       end else if (ToType^.TypeDefinition=ttdLongString) and
                   (ToType^.LongStringType=tstUnsignedWideChar) then begin
        result:=tcteConvertWithPossibleLossOfData;
       end else if (ToType^.TypeDefinition=ttdShortString) and
                   (FromType^.LowerLimit>(255*sizeof(widechar))) then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertWithPossibleLossOfData;
       end;
      end;
     end;
    end;
    ttdPointer:begin
     if assigned(FromType^.PointerTo) and
        (FromType^.PointerTo^.SymbolType in [tstTemp,tstType,tstVariable]) and
        assigned(FromType^.PointerTo^.TypeDefinition) and
        (FromType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
        (FromType^.PointerTo^.TypeDefinition^.SubRangeType=tstUnsignedChar) then begin
      ConvertType:=tctPCharToString;
      if (ToType^.TypeDefinition=ttdShortString) and not SymbolManager.AnsiStrings then begin
       result:=tcteConvertCompatible;
      end else if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedChar) and not SymbolManager.AnsiStrings then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithLessPreferedConversion;
      end;
     end else if assigned(FromType^.PointerTo) and
                 (FromType^.PointerTo^.SymbolType in [tstTemp,tstType,tstVariable]) and
                 assigned(FromType^.PointerTo^.TypeDefinition) and
                 (FromType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                 (FromType^.PointerTo^.TypeDefinition^.SubRangeType=tstUnsignedWideChar) then begin
      ConvertType:=tctPWideCharToString;
      if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedWideChar) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithPossibleLossOfData;
      end;
     end else if assigned(FromType^.PointerTo) and
                 (FromType^.PointerTo^.SymbolType in [tstTemp,tstType,tstVariable]) and
                 assigned(FromType^.PointerTo^.TypeDefinition) and
                 (FromType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
                 (FromType^.PointerTo^.TypeDefinition^.SubRangeType=tstUnsignedHugeChar) then begin
      ConvertType:=tctPHugeCharToString;
      if (ToType^.TypeDefinition=ttdLongString) and (ToType^.LongStringType=tstUnsignedHugeChar) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithPossibleLossOfData;
      end;
     end;
    end;
   end;
  end;

  // ------------

  ttdFloat:begin
   case FromType^.TypeDefinition of
    ttdFloat:begin
     if SymbolManager.GetSize(FromType)<SymbolManager.GetSize(ToType) then begin
      result:=tcteConvertCompatible;
     end else begin
      result:=tcteConvertWithLessPreferedConversion;
     end;
     ConvertType:=tctFloatToFloat;
    end;
    ttdSubRange:begin
     if FromType^.SubRangeType in [tstFloat32Bit,tstFloat64Bit,tstFloat80Bit] then begin
      if ord(FromType^.SubRangeType)<ord(ToType^.SubRangeType) then begin
       result:=tcteConvertCompatible;
      end else begin
       result:=tcteConvertWithLessPreferedConversion;
      end;
      ConvertType:=tctFloatToFloat;
     end else if FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                            tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                            tstUnsigned32Bit,tstUnsigned64Bit] then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToFloat;
     end;
    end;
   end;
  end;

  // ------------

  ttdEnumerated:begin
   case FromType^.TypeDefinition of
    ttdEnumerated:begin
     if tctoEXPLICIT in CompareTypesOptions then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToInteger;
     end else begin
      if FromType^.SubRangeType=FromType^.SubRangeType then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctIntegerToInteger;
      end else if (FromTreeNodeType=ttntORDConst) and
                  (FromType^.LowerLimit=ToType^.LowerLimit) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctIntegerToInteger;
      end;
     end;
    end;
    ttdSubrange:begin
     if tctoEXPLICIT in CompareTypesOptions then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToInteger;
     end;
    end;
    ttdVariant:begin
     if tctoEXPLICIT in CompareTypesOptions then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctVariantToEnum;
     end;
    end;
    ttdPointer:begin
     if (tctoEXPLICIT in CompareTypesOptions) and
        (result=tcteIncompatible) then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctVariantToEnum;
     end;
    end;
   end;
  end;

  // ------------

  ttdArray:begin
   if ToType^.OpenArray and
      EqualTypes(Error,SymbolManager,FromType,ToType^.Definition) then begin
    result:=tcteConvertCompatible;
    ConvertType:=tctEqual;
   end else begin
    case FromType^.TypeDefinition of
     ttdArray:begin
      if ToType^.DynamicArray then begin
       if EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) then begin
        if FromType^.DynamicArray then begin
         result:=tcteEqual;
        end else if (FromType^.LowerLimit=0) and not
                    (FromType^.OpenArray or FromType^.DynamicArray or
                     FromType^.VariantArray or FromType^.ConstructorArray or
                     FromType^.ArrayOfConst) then begin // Ext
         result:=tcteConvertWithLessPreferedConversion;
         ConvertType:=tctArrayToDynArray;
        end;
       end;
      end else if ToType^.OpenArray then begin
       if FromType^.ConstructorArray then begin
        if assigned(FromType^.Definition) and
           (FromType^.Definition^.TypeDefinition=ttdEmpty) then begin
         result:=tcteConvertCompatible;
         ConvertType:=tctEqual;
        end else begin
         SubEqualType:=CompareTypesExt(Error,SymbolManager,FromType^.Definition,ToType^.Definition,ttntArrayConstructor,SubConvertType,SubProcType,[tctoCHECKOPERATOR]);
         if SubEqualType>=tcteEqual then begin
          result:=tcteConvertCompatible;
          ConvertType:=tctEqual;
         end else if SubEqualType>=tcteIncompatible then begin
          result:=tcteConvertWithLessPreferedConversion;
          ConvertType:=SubConvertType;
         end;
        end;
       end else if FromType^.DynamicArray then begin
        if EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) then begin
         result:=tcteConvertWithLessPreferedConversion;
         ConvertType:=tctDynArrayToOpenArray;
        end;
       end else if EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) and not
                   (FromType^.DynamicArray or FromType^.OpenArray) then begin
        result:=tcteEqual;
       end;
      end else if ToType^.ArrayOfConst then begin
       if FromType^.ConstructorArray or FromType^.ArrayOfConst then begin
        result:=tcteEqual;
       end else if EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) then begin
        result:=tcteConvertCompatible;
        ConvertType:=tctEqual;
       end;
      end else if (FromTreeNodeType=ttntSTRINGCONST) and
                  assigned(FromType^.Definition) and
                  (FromType^.Definition^.TypeDefinition=ttdSubRange) and
                  (FromType^.Definition^.SubRangeType=tstUnsignedChar) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctStringToCharArray;
      end else if FromType^.OpenArray and
               EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) then begin
       result:=tcteEqual;
      end else if EqualTypes(Error,SymbolManager,FromType^.Definition,ToType^.Definition) and
                  assigned(FromType^.Range) and assigned(ToType^.Range) and
                  (FromType^.Range^.TypeDefinition=ttdSubRange) and
                  (ToType^.Range^.TypeDefinition=ttdSubRange) and
                  (FromType^.Range^.LowerLimit=ToType^.Range^.LowerLimit) and
                  (FromType^.Range^.UpperLimit=ToType^.Range^.UpperLimit) and not
                  ((FromType^.OpenArray or FromType^.DynamicArray) or
                   (ToType^.OpenArray or ToType^.DynamicArray)) then begin
       result:=tcteEqual;
      end;
     end;
     ttdPointer:begin
      if (ToType^.DynamicArray) and
         ((FromTreeNodeType=ttntNIL) or not assigned(FromType^.PointerTo)) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctEqual;
      end else if assigned(ToType^.Range) and
                 (ToType^.Range^.TypeDefinition=ttdSubRange) and
                 (ToType^.Range^.LowerLimit=0) and not
                 (ToType^.OpenArray or ToType^.DynamicArray) and
                 assigned(FromType^.PointerTo) and
                 (FromType^.PointerTo^.SymbolType in [tstTemp,tstVariable,tstType]) and
                 EqualTypes(Error,SymbolManager,FromType^.PointerTo^.TypeDefinition,ToType^.Definition) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctPointerToArray;
      end;
     end;
     ttdShortString,ttdLongString:begin
      if assigned(ToType^.Definition) and
         (ToType^.Definition^.TypeDefinition=ttdSubRange) and
         (ToType^.Definition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and not
         (FromType^.OpenArray or FromType^.DynamicArray or
          FromType^.VariantArray or FromType^.ConstructorArray or
          FromType^.ArrayOfConst) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctStringToCharArray;
      end;
     end;
     ttdSubRange:begin
      if assigned(ToType^.Definition) and
         (ToType^.Definition^.TypeDefinition=ttdSubRange) and
         (ToType^.Definition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
         (FromType^.TypeDefinition=ttdSubRange) and
         (FromType^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
         (ToType^.Definition^.SubRangeType=FromType^.SubRangeType) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctCharToCharArray;
      end;
     end;
     ttdRecord:begin
      if ToType^.ArrayOfConst and
         EqualTypes(Error,SymbolManager,FromType,ToType^.Definition) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctEqual;
      end;
     end;
     ttdVariant:begin
      if ToType^.DynamicArray then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctVariantToDynArray;
      end;
     end;
    end;
   end;
  end;

  // ------------

  ttdVariant:begin
   if tctoALLOWVARIANT in CompareTypesOptions then begin
    case FromType^.TypeDefinition of
     ttdEnumerated:begin
      result:=tcteConvertCompatible;
      ConvertType:=tctEnumToVariant;
     end;
     ttdArray:begin
      if FromType^.DynamicArray then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctDynArrayToVariant;
      end;
     end;
     ttdInterface:begin
      result:=tcteConvertCompatible;
      ConvertType:=tctInterfaceToVariant;
     end;
    end;
   end;
  end;

  // ------------

  ttdPointer:begin
   case FromType^.TypeDefinition of
    ttdShortString,ttdLongString:begin
     if (FromTreeNodeType in [ttntArrayConstructor,ttntSTRINGConst]) and
        (ToType^.TypeDefinition=ttdSubRange) and
        (ToType^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
      result:=tcteConvertWithLessPreferedConversion;
      ConvertType:=tctCStringToPChar;
     end else if tctoEXPLICIT in CompareTypesOptions then begin
      if assigned(ToType^.PointerTo) and
         (ToType^.PointerTo^.SymbolType=Symbols.tstType) and
         assigned(ToType^.PointerTo^.TypeDefinition) and
         (ToType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
         (ToType^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
         (FromType^.TypeDefinition=ttdLongString) and
         (FromType^.LongStringType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
         (FromType^.LongStringType=ToType^.PointerTo^.TypeDefinition^.SubRangeType) then begin
       result:=tcteConvertCompatible;
       ConvertType:=tctAnsiStringToPChar;
      end;
     end;
    end;
    ttdSubRange:begin
     if assigned(ToType^.PointerTo) and
        (ToType^.PointerTo^.SymbolType=Symbols.tstType) and
        assigned(ToType^.PointerTo^.TypeDefinition) and
        (ToType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
        (ToType^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
        (FromType^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) and
        (FromType^.SubRangeType=ToType^.PointerTo^.TypeDefinition^.SubRangeType) then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctCCharToPChar;
     end else if (FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,
                                             tstSigned32Bit,tstSigned64Bit,
                                             tstUnsigned8Bit,tstUnsigned16Bit,
                                             tstUnsigned32Bit,tstUnsigned64Bit]) then begin

      result:=tcteConvertCompatible;
      ConvertType:=tctCOrdToPointer;
     end;
     if (result=tcteIncompatible) and ((tctoEXPLICIT in CompareTypesOptions) or
        (tctoINTERNAL in CompareTypesOptions)) then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctIntegerToInteger;
     end;
    end;
    ttdArray:begin
     if (FromTreeNodeType in [ttntArrayConstructor,ttntSTRINGConst]) and
        assigned(ToType^.PointerTo) and
        (ToType^.PointerTo^.SymbolType=Symbols.tstType) and
        assigned(ToType^.PointerTo^.TypeDefinition) and
        (ToType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
        (ToType^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctCStringToPChar;
     end else begin
      if ((FromType^.LowerLimit=0) or FromType^.OpenArray) and
         assigned(ToType^.PointerTo) and
         EqualTypes(Error,SymbolManager,FromType,ToType^.PointerTo^.TypeDefinition) then begin
       if FromTreeNodeType=ttntSTRINGConst then begin
        result:=tcteConvertWithLessPreferedConversion;
       end else begin
        result:=tcteConvertCompatible;
       end;
       ConvertType:=tctArrayToPointer;
      end else if FromType^.DynamicArray then begin
       result:=tcteEqual;
      end;
     end;
    end;
    ttdPointer:begin
     if FromType^.PointerFar<>ToType^.PointerFar then begin
      result:=tcteIncompatible;
     end else if assigned(ToType^.PointerTo) and
                 ToType^.PointerTo^.ForwardType then begin
      if FromType^.PointerTo=ToType^.PointerTo then begin
       result:=tcteEqual;
      end;
     end else if EqualTypes(Error,SymbolManager,FromType,ToType^.Definition) then begin
      result:=tcteEqual;
     end else if assigned(FromType^.PointerTo) and
                 assigned(ToType^.PointerTo) and
                 assigned(FromType^.PointerTo^.TypeDefinition) and
                 assigned(ToType^.PointerTo^.TypeDefinition) and
                 (FromType^.PointerTo^.TypeDefinition^.TypeDefinition in [ttdObject,ttdClass]) and
                 (FromType^.PointerTo^.TypeDefinition^.TypeDefinition=ToType^.PointerTo^.TypeDefinition^.TypeDefinition) and
                 SymbolManager.IsObjectClassAncestorType(ToType^.PointerTo^.TypeDefinition,FromType^.PointerTo^.TypeDefinition) then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctEqual;
     end else if not assigned(ToType^.PointerTo) then begin
      if assigned(FromType^.PointerTo) and
         (FromType^.PointerTo^.SymbolType=Symbols.tstType) and
         assigned(FromType^.PointerTo^.TypeDefinition) and
         (FromType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
         (FromType^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
       result:=tcteConvertWithLessPreferedConversion;
      end else begin
       result:=tcteConvertCompatible;
      end;
      ConvertType:=tctEqual;
     end else if not assigned(FromType^.PointerTo) then begin
      ConvertType:=tctEqual;
      if assigned(ToType^.PointerTo) and
         (ToType^.PointerTo^.SymbolType=Symbols.tstType) and
         assigned(ToType^.PointerTo^.TypeDefinition) and
         (ToType^.PointerTo^.TypeDefinition^.TypeDefinition=ttdSubRange) and
         (ToType^.PointerTo^.TypeDefinition^.SubRangeType in [tstUnsignedWideChar,tstUnsignedHugeChar]) then begin
       result:=tcteConvertWithLessPreferedConversion;
      end else begin
       result:=tcteConvertCompatible;
      end;
     end else if assigned(FromType^.PointerTo) and
                 assigned(ToType^.PointerTo) and
                 assigned(FromType^.PointerTo^.TypeDefinition) and
                 assigned(ToType^.PointerTo^.TypeDefinition) and
                 EqualTypes(Error,SymbolManager,FromType^.PointerTo^.TypeDefinition,ToType^.PointerTo^.TypeDefinition) then begin
      result:=tcteEqual;
     end else begin
      // !!!
      result:=tcteConvertCompatible;
     end;
    end;
    ttdProcedure:begin
     if FromType^.AddressOnly then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctEqual;
     end;
    end;
    ttdClassRef,ttdClass,ttdObject,ttdInterface:begin
     if not assigned(ToType^.PointerTo) then begin
      result:=tcteConvertWithLessPreferedConversion;
      ConvertType:=tctEqual;
     end;
    end;
   end;
  end;

  // ------------

  ttdSet:begin
   case FromType^.TypeDefinition of
    ttdSet:begin
     if assigned(FromType^.SetOf) and assigned(ToType^.SetOf) then begin
      if AreTypesSubEqual(Error,SymbolManager,FromType,ToType^.Definition) then begin
       result:=tcteEqual;
      end;
     end else begin
      result:=tcteEqual;
     end;
    end;
    ttdArray:begin
     if FromType^.ConstructorArray then begin
      result:=tcteConvertCompatible;
      ConvertType:=tctArrayConstructorToSet;
     end;
    end;
   end;
  end;

  // ------------

  ttdProcedure:begin
   case FromType^.TypeDefinition of
    ttdProcedure:begin
     result:=CompareProcToProcVar(Error,SymbolManager,FromType,ToType);
     if result<>tcteIncompatible then begin
      ConvertType:=tctEqual;
     end;
    end;
    ttdPointer:begin
     if FromTreeNodeType=ttntNIL then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end else if ToType^.AddressOnly and not assigned(FromType^.PointerTo) then begin
     ConvertType:=tctEqual;
     result:=tcteConvertCompatible;
    end;
   end;
  end;

  // ------------

  ttdObject:begin
   case FromType^.TypeDefinition of
    ttdObject:begin
     if SymbolManager.IsObjectClassAncestorType(ToType,FromType) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdClass:begin
   case FromType^.TypeDefinition of
    ttdClass:begin
     if SymbolManager.IsObjectClassAncestorType(ToType,FromType) or
        SymbolManager.IsObjectClassAncestorType(FromType,ToType) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end;
    ttdPointer:begin
     if FromTreeNodeType=ttntNIL then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end else if not assigned(FromType^.PointerTo) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertWithLessPreferedConversion;
     end;
    end;
    ttdSubRange:begin
     if (ToType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                  tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                  tstUnsigned32Bit,tstUnsigned64Bit]) and
        (tctoEXPLICIT in CompareTypesOptions) then begin
      ConvertType:=tctIntegerToInteger;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdInterface:begin
   case FromType^.TypeDefinition of
    ttdInterface:begin
     if SymbolManager.IsObjectClassAncestorType(ToType,FromType) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end;
    ttdClass:begin
     ClassType:=FromType;
     while assigned(ClassType) do begin
      for Counter:=0 to length(ClassType^.InterfaceChildOf)-1 do begin
       if assigned(ClassType^.InterfaceChildOf[Counter]) and
          ((ToType=ClassType^.InterfaceChildOf[Counter]^.TypeDefinition) {OR
           SymbolManager.IsObjectClassAncestorType(ToType,ClassType^.InterfaceChildOf[Counter]^.TypeDefinition)}) then begin
        ConvertType:=tctClassToInterface;
        result:=tcteConvertWithLessPreferedConversion;
        break;
       end;
      end;
      if (ConvertType=tctClassToInterface) or not
         assigned(ClassType^.ChildOf) then break;
      ClassType:=ClassType^.ChildOf^.TypeDefinition;
     end;
    end;
    ttdPointer:begin
     if FromTreeNodeType=ttntNIL then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end else if not assigned(FromType^.PointerTo) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertWithLessPreferedConversion;
     end;
    end;
    ttdVariant:begin
     ConvertType:=tctVariantToInterface;
     result:=tcteConvertWithLessPreferedConversion;
    end;
    ttdSubRange:begin
     if (ToType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,
                                  tstSigned64Bit,tstUnsigned8Bit,tstUnsigned16Bit,
                                  tstUnsigned32Bit,tstUnsigned64Bit]) and
        (tctoEXPLICIT in CompareTypesOptions) then begin
      ConvertType:=tctIntegerToInteger;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdClassRef:begin
   case FromType^.TypeDefinition of
    ttdClassRef:begin
     if assigned(FromType^.ClassOf) and
        assigned(FromType^.ClassOf^.TypeDefinition) and
        assigned(ToType^.ClassOf) and
        assigned(ToType^.ClassOf^.TypeDefinition) then begin
      if EqualTypes(Error,SymbolManager,FromType^.ClassOf^.TypeDefinition,ToType^.ClassOf^.TypeDefinition) then begin
       ConvertType:=tctEqual;
       result:=tcteEqual;
      end else if (tctoEXPLICIT in CompareTypesOptions) or
                  SymbolManager.IsObjectClassAncestorType(ToType^.ClassOf^.TypeDefinition,FromType^.ClassOf^.TypeDefinition) then begin
       ConvertType:=tctEqual;
       result:=tcteConvertCompatible;
      end;
     end;
    end;
    ttdPointer:begin
     if FromTreeNodeType=ttntNIL then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdFile:begin
   case FromType^.TypeDefinition of
    ttdFile:begin
     if ToType^.FileType=FromType^.FileType then begin
      if (assigned(ToType^.FileTypeRecord) and assigned(FromType^.FileTypeRecord)) and
         EqualTypes(Error,SymbolManager,ToType^.FileTypeRecord,FromType^.FileTypeRecord) then begin
       ConvertType:=tctEqual;
       result:=tcteEqual;
      end else if not (assigned(ToType^.FileTypeRecord) or
                       assigned(FromType^.FileTypeRecord)) then begin
       ConvertType:=tctEqual;
       result:=tcteEqual;
      end else if ((ToType^.FileType=tftTyped) and
                   (FromType^.FileType=tftTyped)) and
                  ((ToType^.FileTypeRecord=SymbolManager.TypePOINTER) or
                   (FromType^.FileTypeRecord=SymbolManager.TypePOINTER)) then begin
       ConvertType:=tctEqual;
       result:=tcteEqual;
      end;
     end else if ((ToType^.FileType=tftTyped) and (FromType^.FileType=tftUntyped)) or
                 ((ToType^.FileType=tftUntyped) and (FromType^.FileType=tftTyped)) then begin
      ConvertType:=tctEqual;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdRecord:begin
   case FromType^.TypeDefinition of
    ttdInterface:begin
     if ToType=SymbolManager.TypeTGUID then begin
      ConvertType:=tctInterfaceToGUID;
      result:=tcteConvertCompatible;
     end;
    end;
   end;
  end;

  // ------------

  ttdEmpty:begin // Formal
   case FromType^.TypeDefinition of
    ttdEmpty:begin
     ConvertType:=tctEqual;
     result:=tcteEqual;
    end;
    else begin
     ConvertType:=tctEqual;
     result:=tcteConvertCompatible;
    end;
   end;
  end;

 end;

 if result=tcteIncompatible then begin
  if (tctoALLOWVARIANT in CompareTypesOptions) and
     (FromType^.TypeDefinition=ttdVariant) and
     (ToType^.TypeDefinition=ttdVariant) then begin
   ConvertType:=tctEqual;
   result:=tcteConvertOperator;
  end;
 end;

 if (result=tcteEqual) and (ConvertType=tctNotPossible) then begin
  result:=tcteEqual;
 end;
end;

function EqualTypes(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
var ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=CompareTypesExt(Error,SymbolManager,FromType,ToType,ttntEMPTY,ConvertType,ProcType,[])>=tcteEQUAL;
end;

function EqualTypesEx(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
var ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=CompareTypesExt(Error,SymbolManager,FromType,ToType,ttntEMPTY,ConvertType,ProcType,[tctoEXPLICIT])>tcteIncompatible;
end;

function AreTypesCompatible(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
var ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=CompareTypesExt(Error,SymbolManager,FromType,ToType,ttntEMPTY,ConvertType,ProcType,[])>tcteIncompatible;
end;

function AreTypesEqualCompatible(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
var ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=CompareTypesExt(Error,SymbolManager,FromType,ToType,ttntEMPTY,ConvertType,ProcType,[])>=tcteEqual;
end;

function AreTypesSubEqual(Error:TError;SymbolManager:TSymbolManager;FromType,ToType:PType):boolean;
begin
 result:=false;
 if assigned(FromType) and assigned(ToType) then begin
  if (FromType^.TypeDefinition=ttdSubRange) and
     (ToType^.TypeDefinition=ttdSubRange) then begin
   case ToType^.SubRangeType of
    tstSigned8Bit,tstSigned16Bit,tstSigned32Bit,tstSigned64Bit,
    tstUnsigned8Bit,tstUnsigned16Bit,tstUnsigned32Bit,tstUnsigned64Bit:begin
     result:=FromType^.SubRangeType in [tstSigned8Bit,tstSigned16Bit,
                                        tstSigned32Bit,tstSigned64Bit,
                                        tstUnsigned8Bit,tstUnsigned16Bit,
                                        tstUnsigned32Bit,tstUnsigned64Bit];
    end;
    tstUnsignedChar,tstUnsignedWideChar,tstUnsignedHugeChar:begin
     result:=ToType^.SubRangeType=FromType^.SubRangeType;
    end;
   end;
  end else if (FromType^.TypeDefinition=ttdBoolean) and
              (ToType^.TypeDefinition=ttdBoolean) then begin
   result:=true;
  end else if (FromType^.TypeDefinition=ttdEnumerated) and
              (ToType^.TypeDefinition=ttdEnumerated) then begin
   result:=(ToType^.SubRangeType=FromType^.SubRangeType) and
           (ToType^.LowerLimit=FromType^.LowerLimit) and
           (ToType^.UpperLimit=FromType^.UpperLimit);
  end;
 end;
end;

function CompareParameters(Error:TError;SymbolManager:TSymbolManager;ParametersA,ParametersB:TSymbolList;ParametersType:TCompareParametersType;Options:TCompareParametersOptions):TCompareTypesEqual;
var TypeOptions:TCompareTypesOptions;
    SymbolA,SymbolB:PSymbol;
    Equal,WorstEqual:TCompareTypesEqual;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=tcteIncompatible;
 TypeOptions:=[tctoCHECKOPERATOR,tctoALLOWVARIANT];
 if assigned(ParametersA) then begin
  SymbolA:=ParametersA.First;
 end else begin
  SymbolA:=nil;
 end;
 if assigned(ParametersB) then begin
  SymbolB:=ParametersB.First;
 end else begin
  SymbolB:=nil;
 end;
 if tcpoIGNOREHIDDEN in Options then begin
  while assigned(SymbolA) and (tsaHiddenParameter in SymbolA^.Attributes) do begin
   SymbolA:=SymbolA^.Next;
  end;
  while assigned(SymbolB) and (tsaHiddenParameter in SymbolB^.Attributes) do begin
   SymbolB:=SymbolB^.Next;
  end;
 end;
{if not (assigned(SymbolA) and assigned(SymbolB)) then begin
  if SymbolA=SymbolB then begin
   result:=tcteEqual;
  end;
  exit;
 erd;}
 WorstEqual:=high(TCompareTypesEqual);
 while assigned(SymbolA) and assigned(SymbolB) do begin

  if (assigned(SymbolA^.TypeDefinition) and assigned(SymbolB^.TypeDefinition)) and
     ((SymbolA^.TypeDefinition^.Unique) or (SymbolB^.TypeDefinition^.Unique)) and
     (SymbolA^.TypeDefinition<>SymbolB^.TypeDefinition) then begin
   exit;
  end;

  if (tsaHiddenParameter in SymbolA^.Attributes) or
     (tsaHiddenParameter in SymbolB^.Attributes) then begin
   if (tsaHiddenParameter in SymbolA^.Attributes)<>(tsaHiddenParameter in SymbolB^.Attributes) then begin
    exit;
   end;
   Equal:=tcteEqual;
   if (not (tsaParameterSelf in SymbolA^.Attributes)) and
      (not (tsaParameterSelf in SymbolB^.Attributes)) then begin
    if SymbolA^.VariableType<>SymbolB^.VariableType then begin
     exit;
    end;
    Equal:=CompareTypesExt(Error,SymbolManager,SymbolA^.TypeDefinition,SymbolB^.TypeDefinition,ttntEmpty,ConvertType,ProcType,TypeOptions);
   end;
  end else begin
   case ParametersType of
    tcptVALUEEQUALCONST:begin
     if (SymbolA^.VariableType<>SymbolB^.VariableType) and
        ((SymbolA^.VariableType in [tvtParameterVariable,tvtParameterResult]) or
         (SymbolB^.VariableType in [tvtParameterVariable,tvtParameterResult])) then begin
      exit;
     end;
     Equal:=CompareTypesExt(Error,SymbolManager,SymbolA^.TypeDefinition,SymbolB^.TypeDefinition,ttntEmpty,ConvertType,ProcType,TypeOptions);
    end;
    tcptALL:begin
     if SymbolA^.VariableType<>SymbolB^.VariableType then begin
      exit;
     end;
     Equal:=CompareTypesExt(Error,SymbolManager,SymbolA^.TypeDefinition,SymbolB^.TypeDefinition,ttntEmpty,ConvertType,ProcType,TypeOptions);
    end;
    tcptPROCVAR:begin
     if SymbolA^.VariableType<>SymbolB^.VariableType then begin
      exit;
     end;
     Equal:=CompareTypesExt(Error,SymbolManager,SymbolA^.TypeDefinition,SymbolB^.TypeDefinition,ttntEmpty,ConvertType,ProcType,TypeOptions);
     if Equal<tcteEqual then begin
      Equal:=tcteIncompatible;
     end;
    end;
    else begin
     Equal:=CompareTypesExt(Error,SymbolManager,SymbolA^.TypeDefinition,SymbolB^.TypeDefinition,ttntEmpty,ConvertType,ProcType,TypeOptions);
    end;
   end;
  end;

  if Equal=tcteIncompatible then begin
   exit;
  end;
  if Equal<WorstEqual then begin
   WorstEqual:=Equal;
  end;

  if tcpoCOMPAREDEFAULTVALUE in Options then begin
   if (assigned(SymbolA^.DefaultParameterSymbol) and assigned(SymbolB^.DefaultParameterSymbol)) and not
      SymbolManager.AreConstSymbolEqual(SymbolA^.DefaultParameterSymbol,SymbolB^.DefaultParameterSymbol) then begin
    exit;
   end;
  end;

  SymbolA:=SymbolA^.Next;
  SymbolB:=SymbolB^.Next;
  if tcpoIGNOREHIDDEN in Options then begin
   while assigned(SymbolA) and (tsaHiddenParameter in SymbolA^.Attributes) do begin
    SymbolA:=SymbolA^.Next;
   end;
   while assigned(SymbolB) and (tsaHiddenParameter in SymbolB^.Attributes) do begin
    SymbolB:=SymbolB^.Next;
   end;
  end;
 end;
 if not (assigned(SymbolA) or assigned(SymbolB)) then begin
  result:=WorstEqual;
 end else if (tcpoALLOWDEFAULTS in Options) and
             (assigned(SymbolA) and (tsaParameterWithDefault in SymbolA^.Attributes) or
              assigned(SymbolB) and (tsaParameterWithDefault in SymbolB^.Attributes)) then begin
  result:=WorstEqual;
 end;
end;

function CompareProcToProcVar(Error:TError;SymbolManager:TSymbolManager;ProcFrom,ProcTo:PType):TCompareTypesEqual;
var ProcedureAttributes:TProcedureAttributes;
    HasReturnTypes:boolean;
begin
 result:=tcteIncompatible;
 if (assigned(ProcFrom) and assigned(ProcTo)) and not
    ((ProcFrom^.MethodPointer xor ProcTo^.MethodPointer) or
     (ProcFrom^.AddressOnly xor ProcTo^.AddressOnly)) then begin
  ProcedureAttributes:=[tpaInterrupt,tpaSTDCALL,tpaPASCAL,tpaCDECL,tpaSAFECALL,tpaFASTCALL,tpaREGISTER];
  HasReturnTypes:=assigned(ProcFrom^.ReturnType) and assigned(ProcTo^.ReturnType);
  if ((ProcFrom^.ProcedureAttributes*ProcedureAttributes)=(ProcTo^.ProcedureAttributes*ProcedureAttributes)) and
     ((HasReturnTypes and EqualTypes(Error,SymbolManager,ProcFrom^.ReturnType,ProcTo^.ReturnType)) or not
      HasReturnTypes) then begin
   result:=CompareParameters(Error,SymbolManager,ProcFrom^.Parameter,ProcTo^.Parameter,tcptPROCVAR,[]);
   if result=tcteExact then begin
    result:=tcteEqual;
   end;
  end;
 end;
end;

function CompareCallParameters(Error:TError;SymbolManager:TSymbolManager;SymbolParameters:TSymbolList;CallParameters:TTreeNode;var ParameterImbalance:longint;var TypeA,TypeB:PType;var FirstNeededDefaultSymbol:PSymbol):TCompareTypesEqual;
var SymbolParameter:PSymbol;
    CallParameter:TTreeNode;
    Equal:TCompareTypesEqual;
    ConvertType:TConvertType;
    ProcType:PType;
begin
 result:=high(TCompareTypesEqual);
 ParameterImbalance:=0;
 TypeA:=nil;
 TypeB:=nil;
 FirstNeededDefaultSymbol:=nil;
 if assigned(SymbolParameters) then begin
  SymbolParameter:=SymbolParameters.First;
 end else begin
  SymbolParameter:=nil;
 end;
 CallParameter:=CallParameters;
 while assigned(SymbolParameter) and assigned(CallParameter) do begin
  while assigned(SymbolParameter) and (tsaHiddenParameter in SymbolParameter^.Attributes) do begin
   SymbolParameter:=SymbolParameter^.Next;
  end;
  while assigned(CallParameter) and not ((CallParameter.TreeNodeType=ttntPARAMETER) and assigned(CallParameter.Left) and not CallParameter.Left.Colon) do begin
   CallParameter:=CallParameter.Right;
  end;
  if assigned(SymbolParameter) and assigned(CallParameter) then begin
   Equal:=CompareTypesExt(Error,SymbolManager,CallParameter.Left.Return,SymbolParameter^.TypeDefinition,ttntEmpty,ConvertType,ProcType,[tctoCHECKOPERATOR,tctoALLOWVARIANT]);
   if Equal<result then begin
    result:=Equal;
   end;
   if result=tcteIncompatible then begin
    TypeA:=CallParameter.Left.Return;
    TypeB:=SymbolParameter^.TypeDefinition;
    exit;
   end;
   SymbolParameter:=SymbolParameter^.Next;
   CallParameter:=CallParameter.Right;
  end else begin
   break;
  end;
 end;
 if assigned(SymbolParameter) and
    (assigned(SymbolParameter^.DefaultParameterSymbol) and
     (tsaParameterWithDefault in SymbolParameter^.Attributes)) and not
    assigned(CallParameter) then begin
  FirstNeededDefaultSymbol:=SymbolParameter;
 end else begin
  if assigned(SymbolParameter) or assigned(CallParameter) then begin
   if assigned(SymbolParameter) then begin
    ParameterImbalance:=-1;
   end else if assigned(CallParameter) then begin
    ParameterImbalance:=1;
   end;
   result:=tcteIncompatible;
  end;
 end;
end;

end.
