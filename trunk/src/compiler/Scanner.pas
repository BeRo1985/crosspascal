unit Scanner;
{$i Compiler.inc}

interface

uses Math,BeRoStringToDouble,BeRoStream,BeRoStringTree,Globals,Error,Symbols,Preprocessor,
     HugeString,UnicodeUtils,CodePages;

type TScannerToken=(tstNone,
                    tstPeriod,tstComma,tstPlus,tstMinus,tstMul,tstSlash,
                    tstLeftParen,tstRightParen,tstSeparator,tstAssign,
                    tstEqual,tstGreater,tstLess,tstGreaterOrEqual,
                    tstLessOrEqual,tstNotEqual,tstColon,tstDoublePeriod,
                    tstLeftBracket,tstRightBracket,tstAt,tstPointer,
                    tstStringValue,tstValue,tstFloatValue,tstCharValue,
                    tstCCODE,tstCEXPR,tstCBLOCK,tstCEND,tstCSKIP,
                    tstPROGRAM,
                    tstLABEL,tstGOTO,tstVAR,
                    tstBEGIN,tstEND,tstAND,tstOR,tstXOR,tstNOT,tstSHL,
                    tstSHR,tstDIV,tstMOD,tstIN,tstAS,tstIS,tstIF,tstTHEN,
                    tstELSE,tstWHILE,tstREPEAT,tstUNTIL,tstFOR,tstTO,
                    tstDOWNTO,tstDO,tstNIL,tstTYPE,tstPROCEDURE,
                    tstFUNCTION,tstCONST,tstRECORD,tstFORWARD,tstEXTERNAL,
                    tstNAME,tstUNIT,tstINTERFACE,tstIMPLEMENTATION,tstUSES,
                    tstASSEMBLER,tstASM,tstCLASS,tstOBJECT,tstPUBLIC,tstPRIVATE,
                    tstPROTECTED,tstPUBLISHED,tstCONSTRUCTOR,tstDESTRUCTOR,
                    tstOVERRIDE,tstOVERLOAD,tstVIRTUAL,tstDYNAMIC,
                    tstABSTRACT,tstPROPERTY,tstINDEX,tstREAD,tstWRITE,
                    tstINHERITED,tstSTRING,tstBREAK,tstCONTINUE,tstEXIT,tstHALT,
                    tstFAIL,tstCASE,tstOF,tstWITH,tstARRAY,tstFILE,tstSET,
                    tstFAR,tstNEAR,tstINTERRUPT,tstOUT,tstINLINE,tstREGISTER,
                    tstSTDCALL,tstPASCAL,tstCDECL,tstSAFECALL,tstFASTCALL,
                    tstEXPORT,tstSHORTSTRING,tstWIDESTRING,tstLONGSTRING,
                    tstANSISTRING,tstMESSAGE,tstINITIALIZATION,
                    tstFINALIZATION,tstSTORED,tstDEFAULT,tstNODEFAULT,
                    tstIMPLEMENTS,tstRESOURCESTRING,tstSTRICT,tstTHREADVAR,
                    tstDISPINTERFACE,tstPACKED,tstTRY,tstEXCEPT,
                    tstFINALLY,tstRAISE,tstAUTOMATED,tstDEPRECATED,
                    tstDISPID,tstLIBRARY,tstON,tstPLATFORM,tstREINTRODUCE,
                    tstABSOLUTE,tstLOCAL,tstVARARGS,tstPACKAGE,tstCONTAINS,
                    tstREQUIRES,tstEXPORTS,tstOPENSTRING,tstHUGESTRING,
                    tstUNICODESTRING,tstAT_,
                    tstIdentifier);

     TScannerTokens=set of TScannerToken;

     TScannerEncoding=(tseLATIN1,tseUTF8,tseUTF16LE,tseUTF16BE,tseUTF32LE,tseUTF32BE);

     TScannerFileStackItem=record
      FileName:ansistring;
      InputStream:TBeRoStream;
      Line:longint;
      Column:longint;
      Position:longint;
      TheChar:THugeChar;
      Encoding:TScannerEncoding;
     end;

     TScannerFileStack=array of TScannerFileStackItem;

     TScannerMode=(smPASCAL,smPASCALCODE,smPASCALEXPRESSION,smC);

     TScanner=class
      private
       Options:TOptions;
       Error:TError;
       SymbolManager:TSymbolManager;
       LocalSwitches:PLocalSwitches;
       KeywordStringTree:TBeRoStringTree;
       Preprocessor:TPreprocessor;
       LastChar:THugeChar;
       CurrentChar:THugeChar;
       Encoding:TScannerEncoding;
       procedure NewKeyword(Keyword:ansistring;Token:TScannerToken);
       procedure CheckEncoding;
       function ReadWordLE:word;
       function ReadWordBE:word;
       function ReadDWordLE:longword;
       function ReadDWordBE:longword;
       function ReadChar:THugeChar;
       function ReadPascalString:THugeString;
      public
       FileStack:TScannerFileStack;
       InputStream:TBeRoStream;
       FileNames:array of ansistring;
       FileName:ansistring;
       CurrentFileName:ansistring;
       CurrentLine:longint;
       CurrentColumn:longint;
       LastLine:longint;
       LastColumn:longint;
       LastPosition:longint;
       ToInjectToken,CurrentToken,CurrentDirectiveToken,LastToken,NextToken:TScannerToken;
       CurrentString:THugeString;
       CurrentIdentifier:ansistring;
       CurrentOriginalIdentifier:ansistring;
       CurrentValue:int64;
       CurrentFloatValue:extended;
       CurrentCharValue:THugeChar;
       ProcedureName:ansistring;
       AllowedDirectives:TScannerTokens;
       UseNextToken:boolean;
       ModeStack:array of TScannerMode;
       ModeStackPointer:longint;
       constructor Create(TheInputStream:TBeRoStream;TheFileName:ansistring;TheError:TError;TheSymbolManager:TSymbolManager;TheOptions:TOptions;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;
       procedure Reset;
       procedure FileStackPush;
       procedure FileStackPop;
       procedure FileStackOpenInclude(IncludeFileName:ansistring);
       function IsEOF:boolean;
       function IsEOFOrAbortError:boolean;
       function ReadNext:boolean;
       function ReadIdentifier(OriginalIdentifier:pointer):ansistring;
       function ReadNumber:int64;
       function ReadFloat:extended;
       function ReadLabel:ansistring;
       function ReadLabelEx:ansistring;
       function Match(Token:TScannerToken):boolean;
       procedure Illegal(Token:TScannerToken);
       function CurrentMode:TScannerMode;
       function MaybeLabel(Token:TScannerToken):boolean;
       function IsLabel(Token:TScannerToken):boolean;
       procedure CheckForDirectives(AllowedDirectives:TScannerTokens);
       property PreprocessorInstance:TPreprocessor read Preprocessor;
     end;

const Directives:TScannerTokens=[tstAT_,tstABSOLUTE,tstABSTRACT,tstASSEMBLER,tstAT,tstAUTOMATED,
                                 tstCDECL,tstCONTAINS,tstDEFAULT,tstDEPRECATED,tstDISPID,
                                 tstDYNAMIC,tstEXPORT,tstFAR,tstIMPLEMENTS,tstINDEX,
                                 tstLOCAL,tstNEAR,tstNODEFAULT,tstON,tstOPENSTRING,
                                 tstOVERLOAD,tstOVERRIDE,tstPASCAL,tstPLATFORM,tstPRIVATE,
                                 tstPROTECTED,tstPUBLIC,tstPUBLISHED,tstREAD,tstREGISTER,
                                 tstREINTRODUCE,tstSAFECALL,tstSTDCALL,tstSTRICT,
                                 tstSTORED,tstVARARGS,tstVIRTUAL,tstWRITE,
                                 tstFAIL,tstNAME,tstMESSAGE];

implementation

uses BeRoUtils;

const TokenNames:array[TScannerToken] of ansistring=('',
       '.',',','+','-','*','/','(',')',';',':=','=','>','<','>=','<=','<>',
       ':','..','[',']','@','^','''','numberic value','float value',
       'char value','c code','c expression','c block','c end','c skip',
       'PROGRAM','LABEL','GOTO','VAR','BEGIN',
       'END','AND','OR','XOR','NOT','SHL','SHR','DIV','MOD','IN','AS',
       'IS','IF','THEN','ELSE','WHILE','REPEAT','UNTIL','FOR','TO',
       'DOWNTO','DO','NIL','TYPE','PROCEDURE','FUNCTION','CONST',
       'RECORD','FORWARD','EXTERNAL','NAME','UNIT','INTERFACE',
       'IMPLEMENTATION','USES','ASSEMBLER','ASM','CLASS','OBJECT',
       'PUBLIC','PRIVATE','PROTECTED','PUBLISHED','CONSTRUCTOR',
       'DESTRUCTOR','OVERRIDE','OVERLOAD','VIRTUAL','DYNAMIC','ABSTRACT',
       'PROPERTY','INDEX','READ','WRITE','INHERITED','STRING','BREAK',
       'CONTINUE','EXIT','HALT','FAIL','CASE','OF','WITH','ARRAY','FILE','SET',
       'FAR','NEAR','INTERRUPT','OUT','INLINE','REGISTER','STDCALL',
       'PASCAL','CDECL','SAFECALL','FASTCALL','EXPORT','SHORTSTRING',
       'WIDESTRING','LONGSTRING','ANSISTRING','MESSAGE','INITIALIZATION',
       'FINALIZATION','STORED','DEFAULT','NODEFAULT','IMPLEMENTS',
       'RESOURCESTRING','STRICT','THREADVAR','DISPINTERFACE','PACKED',
       'TRY','EXCEPT','FINALLY','RAISE','AUTOMATED','DEPRECATED',
       'DISPID','LIBRARY','ON','PLATFORM','REINTRODUCE','ABSOLUTE',
       'LOCAL','VARARGS','PACKAGE','CONTAINS','REQUIRES','EXPORTS',
       'OPENSTRING','HUGESTRING','UNICODESTRING','AT',
       'identifier');
                                                                               
      MaxToken=tstIdentifier;

{$ifdef HAS_TYPE_EXTENDED}
function StringToFloat(const StringValue:ansistring;OK:pointer=nil):extended;
const MaxExponentOfTen=4932;
      MaxExponentOfTenMinusTwo=MaxExponentOfTen-2;
      PowersOfTen:array[0..12] of extended=(1e1,1e2,1e4,1e8,1e16,1e32,1e64,1e128,1e256,1e512,1e1024,1e2048,1e4096);
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmEXTENDED;
var i:longint;
    Negative,HasDigits:boolean;
    Mantissa:{$ifdef HasQWORD}qword{$else}extended{$endif};
    MantissaSize,FractionalDigits,FractionalExponent,ExponentSign,Exponent:longint;
    FloatValue,ExponentFactor:extended;
    MantissaExponent,PartExponent,Index:longint;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
begin
 if assigned(OK) then begin
  longbool(OK^):=false;
 end;
 result:=NaN;
 i:=1;
 while (i<=length(StringValue)) and (StringValue[i] in [#0..#32]) do begin
  inc(i);
 end;
 if (i<=length(StringValue)) and ((StringValue[i]='-') or (StringValue[i]='+')) then begin
  Negative:=StringValue[i]='-';
  inc(i);
 end else begin
  Negative:=false;
 end;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   result:=-Infinity;
  end else begin
   result:=Infinity;
  end;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  result:=Nan;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else begin
  FractionalDigits:=0;
  FractionalExponent:=0;
  MantissaSize:=0;
  Mantissa:=0;
  HasDigits:=false;
  ExponentSign:=1;
  Exponent:=0;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
   if MantissaSize<20 then begin
    Mantissa:=(Mantissa*10)+(byte(ansichar(StringValue[i]))-ord('0'));
   end;
   inc(MantissaSize);
   HasDigits:=true;
   inc(i);
  end;
  if (i<=length(StringValue)) and (StringValue[i]='.') then begin
   inc(i);
   if (MantissaSize=0) and (Mantissa=0) then begin
    while (i<=length(StringValue)) and (StringValue[i]='0') do begin
     inc(FractionalExponent);
     HasDigits:=true;
     inc(i);
    end;
   end;
   while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
    if MantissaSize<20 then begin
     Mantissa:=(Mantissa*10)+(byte(ansichar(StringValue[i]))-ord('0'));
     inc(MantissaSize);
     inc(FractionalDigits);
    end;
    HasDigits:=true;
    inc(i);
   end;
  end;
  if HasDigits then begin
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     if StringValue[i]='-' then begin
      ExponentSign:=-1;
     end;
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     Exponent:=(Exponent*10)+longint(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if HasDigits then begin

    NewFPURoundingMode:=rmNearest;

    OldFPUExceptionMask:=GetExceptionMask;
    OldFPURoundingMode:=GetRoundMode;
    OldFPUPrecisionMode:=GetPrecisionMode;
    try
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(DtoAFPUExceptionMask);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(NewFPURoundingMode);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(DtoAFPUPrecisionMode);
     end;                                     

     FloatValue:=Mantissa;

     MantissaExponent:=FractionalDigits+FractionalExponent;
     if MantissaSize>20 then begin
      dec(MantissaExponent,MantissaSize-20);
     end;

     if ExponentSign>0 then begin
      dec(Exponent,MantissaExponent);
     end else begin
      inc(Exponent,MantissaExponent);
     end;
     if Exponent<0 then begin
      ExponentSign:=-ExponentSign;
      Exponent:=-Exponent;
     end;

     while Exponent>0 do begin
      PartExponent:=Exponent;
      if PartExponent>MaxExponentOfTenMinusTwo then begin
       PartExponent:=MaxExponentOfTenMinusTwo;
      end;
      dec(Exponent,PartExponent);

      Index:=0;
      ExponentFactor:=1;
      while (PartExponent<>0) and (Index<length(PowersOfTen)) do begin
       if (PartExponent and 1)<>0 then begin
        ExponentFactor:=ExponentFactor*PowersOfTen[Index];
       end;
       inc(Index);
       PartExponent:=PartExponent shr 1;
      end;

      if ExponentSign>0 then begin
       FloatValue:=FloatValue*ExponentFactor;
      end else begin
       FloatValue:=FloatValue/ExponentFactor;
      end;
     end;

     // Nice-to-have-TODO: Implement here on-bit-level ULP correction code

     if Negative then begin
      result:=-FloatValue;
     end else begin
      result:=FloatValue;
     end;
     
     if assigned(OK) then begin
      longbool(OK^):=true;
     end;
    finally
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(OldFPUExceptionMask);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(OldFPURoundingMode);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(OldFPUPrecisionMode);
     end;
    end;
   end;
  end;
 end;
end;
{$else}
function StringToFloat(const StringValue:ansistring;OK:pointer=nil):double;
begin
 result:=BeRoConvertStringToDouble(const StringValue,bstd_ROUND_TO_NEAREST,OK);
end;
{$endif}

constructor TScanner.Create(TheInputStream:TBeRoStream;TheFileName:ansistring;TheError:TError;TheSymbolManager:TSymbolManager;TheOptions:TOptions;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
var Token:TScannerToken;
begin
 inherited Create;
 Options:=TheOptions;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 LocalSwitches:=TheLocalSwitches;
 KeywordStringTree:=TBeRoStringTree.Create;
 InputStream:=TheInputStream;
 Preprocessor:=TPreprocessor.Create(Error,SymbolManager,FileStackOpenInclude,TheGlobalSwitches,TheLocalSwitches);
 FileStack:=nil;
 FileNames:=nil;
 setlength(FileNames,1);
 FileNames[0]:=TheFileName;
 FileName:=TheFileName;
 CurrentFileName:=TheFileName;
 CurrentLine:=1;
 CurrentColumn:=0;
 LastLine:=1;
 LastColumn:=0;
 CurrentString:=nil;
 Encoding:=tseLATIN1;
 for Token:=tstPROGRAM to tstABSOLUTE do begin
  NewKeyword(TokenNames[Token],Token);
 end;
 Error.SetFilePosition(CurrentFileName,CurrentLine,CurrentColumn);
 ProcedureName:='';
 LastChar:=0;
 LastToken:=tstNone;
 NextToken:=tstNone;
 ToInjectToken:=tstNone;
 CurrentToken:=tstNone;
 CurrentDirectiveToken:=tstNone;
 UseNextToken:=false;
 AllowedDirectives:=[];
 ModeStack:=nil;
 SetLength(ModeStack,4096);
 ModeStack[0]:=smPASCAL;
 ModeStackPointer:=0;
 Reset;
end;

destructor TScanner.Destroy;
var Counter:longint;
begin
 for Counter:=1 to length(FileStack)-1 do begin
  if assigned(FileStack[Counter].InputStream) then begin
   FileStack[Counter].InputStream.Destroy;
   FileStack[Counter].InputStream:=nil;
  end;
 end;
 SetLength(ModeStack,0);
 setlength(FileStack,0);
 setlength(FileNames,0);
 KeywordStringTree.Destroy;
 Preprocessor.Destroy;
 inherited Destroy;
end;

procedure TScanner.NewKeyword(Keyword:ansistring;Token:TScannerToken);
begin
 KeywordStringTree.Add(Keyword,ord(Token));
end;

procedure TScanner.Reset;
begin
 Preprocessor.Reset;
end;

procedure TScanner.FileStackPush;
var index:longint;
begin
 index:=length(FileStack);
 setlength(FileStack,index+1);
 FileStack[index].FileName:=CurrentFileName;
 FileStack[index].InputStream:=InputStream;
 FileStack[index].Line:=LastLine;
 FileStack[index].Column:=LastColumn;
 FileStack[index].Position:=LastPosition;
 FileStack[index].TheChar:=LastChar;
 FileStack[index].Encoding:=Encoding;
end;

procedure TScanner.FileStackPop;
var index:longint;
begin
 if length(FileStack)>0 then begin
  if assigned(InputStream) then begin
   InputStream.Destroy;
   InputStream:=nil;
  end;
  index:=length(FileStack)-1;
  CurrentFileName:=FileStack[index].FileName;
  InputStream:=FileStack[index].InputStream;
  CurrentLine:=FileStack[index].Line;
  CurrentColumn:=FileStack[index].Column;
  InputStream.Seek(FileStack[index].Position);
  CurrentChar:=FileStack[index].TheChar;
  Encoding:=FileStack[index].Encoding;
  setlength(FileStack,index);
 end;
end;

procedure TScanner.FileStackOpenInclude(IncludeFileName:ansistring);
var FullFileName: ansistring;
begin
 FileStackPush;
 CurrentFileName:=IncludeFileName;
 FullFileName:=Options.FindInclude(IncludeFilename);
 if FullFileName='' then
 begin
  Error.AddErrorCode(40, IncludeFileName);
  InputStream:=TBeRoFileStream.Create(IncludeFileName); // just so we return a valid class
  end else begin
  InputStream:=TBeRoFileStream.Create(FullFileName);
 end;
 CurrentLine:=1;
 CurrentColumn:=0;
 Encoding:=tseLATIN1;
 ReadChar;
end;

function TScanner.IsEOF:boolean;
var Counter:longint;
begin
 result:=InputStream.Position>=InputStream.Size;
 for Counter:=0 to length(FileStack)-1 do begin
  if assigned(FileStack[Counter].InputStream) then begin
   result:=result and (FileStack[Counter].InputStream.Position>=FileStack[Counter].InputStream.Size);
  end;
 end;
end;

function TScanner.IsEOFOrAbortError:boolean;
begin
 result:=IsEOF or Error.DoAbort;
end;

procedure TScanner.CheckEncoding;
var BOM:array[0..3] of byte;
begin
 Encoding:=tseLATIN1;
 BOM[0]:=$11;
 BOM[1]:=$22;
 BOM[2]:=$33;
 BOM[3]:=$44;
 if InputStream.Read(BOM,sizeof(BOM))>=(2*sizeof(byte)) then begin
  if (BOM[0]=$ff) and (BOM[1]=$fe) then begin
   if (BOM[2]=$00) and (BOM[3]=$00) then begin
    Encoding:=tseUTF32LE;
   end else begin
    Encoding:=tseUTF16LE;
    InputStream.Seek(2);
   end;
  end else if (BOM[0]=$00) and (BOM[1]=$00) and (BOM[2]=$fe) and (BOM[3]=$ff) then begin
   Encoding:=tseUTF32BE;
  end else if (BOM[0]=$fe) and (BOM[1]=$ff) then begin
   Encoding:=tseUTF16BE;
   InputStream.Seek(2);
  end else if (BOM[0]=$ef) and (BOM[1]=$bb) and (BOM[2]=$bd) then begin
   Encoding:=tseUTF8;
   InputStream.Seek(3);
  end else begin         // UTF-1 F7 64 4C 
   InputStream.Seek(0);
  end;
 end;
end;

function TScanner.ReadWordLE:word;
var Bytes:array[0..1] of byte;
begin
 InputStream.Read(Bytes,sizeof(Bytes));
 result:=Bytes[0] or (Bytes[1] shl 8);
end;

function TScanner.ReadWordBE:word;
var Bytes:array[0..1] of byte;
begin
 InputStream.Read(Bytes,sizeof(Bytes));
 result:=(Bytes[0] shl 8) or Bytes[1];
end;

function TScanner.ReadDWordLE:longword;
var Bytes:array[0..3] of byte;
begin
 InputStream.Read(Bytes,sizeof(Bytes));
 result:=Bytes[0] or (Bytes[1] shl 8) or (Bytes[2] shl 16) or (Bytes[3] shl 24);
end;

function TScanner.ReadDWordBE:longword;
var Bytes:array[0..3] of byte;
begin
 InputStream.Read(Bytes,sizeof(Bytes));
 result:=(Bytes[0] shl 24) or (Bytes[1] shl 16) or (Bytes[2] shl 8) or Bytes[3];
end;

function TScanner.ReadChar:THugeChar;
var c:ansichar;
    Value,CharClass,State:longword;
    CharSetSubCodePages:PCharSetSubCodePages;
    CharSetSubSubCodePages:PCharSetSubSubCodePages;
    CharSetCodePage:PCharSetCodePage;
begin
 if InputStream.Position>=InputStream.Size then begin
  FileStackPop;
 end;
 if InputStream.Position=0 then begin
  CheckEncoding;
 end;
 LastLine:=CurrentLine;
 LastColumn:=CurrentColumn;
 LastPosition:=InputStream.Position;
 if (CurrentChar=13) or ((CurrentChar=10) and (LastChar<>13)) then begin
  inc(CurrentLine);
  Error.SetFilePosition(CurrentFileName,CurrentLine,CurrentColumn);
  CurrentColumn:=0;
 end else if not (CurrentChar in [13,10]) then begin
  inc(CurrentColumn);
  Error.SetFilePositionColumn(CurrentColumn);
 end;
 if IsEOFOrAbortError then begin
  result:=0;
 end else begin
  case Encoding of
   tseUTF8:begin
    result:=0;
    State:=usmcACCEPT;
    while true do begin
     if InputStream.Position>=InputStream.Size then begin
      FileStackPop;
     end;
     if IsEOFOrAbortError then begin
      break;
     end;
     InputStream.Read(c,sizeof(ansichar));
     Value:=byte(ansichar(c));
     CharClass:=UTF8DFACharClasses[ansichar(Value)];
     if State=usmcACCEPT then begin
      result:=Value and ($ff shr CharClass);
     end else begin
      result:=(result shl 6) or (Value and $3f);
     end;
     State:=UTF8DFATransitions[State+CharClass];
     if State<=usmcERROR then begin
      break;
     end;
    end;
   end;
   tseUTF16LE:begin
    result:=ReadWordLE;
    if (result>=$d800) and (result<=$dbff) then begin
     Value:=ReadWordLE;
     if (Value>=$dc00) and (Value<=$dfff) then begin
      result:=(((result and $3ff) shl 10) or (Value and $3ff))+$10000;
     end else begin
      result:=$fffd;
     end;
    end else if (result>=$d7ff) and (result<=$e000) then begin
     result:=$fffd;
    end;
   end;
   tseUTF16BE:begin
    result:=ReadWordBE;
    if (result>=$d800) and (result<=$dbff) then begin
     Value:=ReadWordBE;
     if (Value>=$dc00) and (Value<=$dfff) then begin
      result:=(((result and $3ff) shl 10) or (Value and $3ff))+$10000;
     end else begin
      result:=$fffd;
     end;
    end else if (result>=$d7ff) and (result<=$e000) then begin
     result:=$fffd;
    end;
   end;
   tseUTF32LE:begin
    result:=ReadDWordLE;
   end;
   tseUTF32BE:begin
    result:=ReadDWordBE;
   end;
   else begin
    if LocalSwitches^.CodePage=65001 then begin
     result:=0;
     State:=usmcACCEPT;
     while true do begin
      if InputStream.Position>=InputStream.Size then begin
       FileStackPop;
      end;
      if IsEOFOrAbortError then begin
       break;
      end;
      InputStream.Read(c,sizeof(ansichar));
      Value:=byte(ansichar(c));
      CharClass:=UTF8DFACharClasses[ansichar(Value)];
      if State=usmcACCEPT then begin
       result:=Value and ($ff shr CharClass);
      end else begin
       result:=(result shl 6) or (Value and $3f);
      end;
      State:=UTF8DFATransitions[State+CharClass];
      if State<=usmcERROR then begin
       break;
      end;
     end;
    end else begin
     InputStream.Read(c,sizeof(ansichar));
     result:=byte(ansichar(c));
     if (LocalSwitches^.CodePage<>28591) and (LocalSwitches^.CodePage>=0) and (LocalSwitches^.CodePage<=65535) then begin
      CharSetSubCodePages:=CodePages.CharSetCodePages[LocalSwitches^.CodePage shr 8];
      if assigned(CharSetSubCodePages) then begin
       CharSetSubSubCodePages:=CharSetSubCodePages[(LocalSwitches^.CodePage shr 4) and $f];
       if assigned(CharSetSubSubCodePages) then begin
        CharSetCodePage:=CharSetSubSubCodePages[LocalSwitches^.CodePage and $f];
        if assigned(CharSetCodePage) then begin
         result:=CharSetCodePage[result and $ff];
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 LastChar:=CurrentChar;
 CurrentChar:=result;
end;

function TScanner.ReadPascalString:THugeString;
var Value,Count,Len:longint;
    AreWeInString:boolean;
begin
 Len:=0;
 result:=nil;
 AreWeInString:=false;
 while not IsEOFOrAbortError do begin
  if AreWeInString then begin
   if CurrentChar=ord('''') then begin
    ReadChar;
    if CurrentChar=ord('''') then begin
     HugeStringConcatChar(result,CurrentChar,Len);
     ReadChar;
    end else begin
     AreWeInString:=false;
    end;
   end else if CurrentChar=13 then begin
    Error.AbortCode(53);
   end else begin
    HugeStringConcatChar(result,CurrentChar,Len);
    ReadChar;
   end;
  end else begin
   if CurrentChar=ord('''') then begin
    ReadChar;
    AreWeInString:=true;
   end else if CurrentChar=ord('^') then begin
    ReadChar;
    HugeStringConcatChar(result,CurrentChar xor $40,Len);
    ReadChar;
   end else if CurrentChar=ord('#') then begin
    ReadChar;
    Count:=0;
    Value:=0;
    if CurrentChar=ord('$') then begin
     ReadChar;
     while not IsEOFOrAbortError do begin
      case CurrentChar of
       ord('0')..ord('9'):begin
        Value:=(Value shl 4) or (byte(CurrentChar)-byte(ansichar('0')));
       end;
       ord('a')..ord('f'):begin
        Value:=(Value shl 4) or ((byte(CurrentChar)-byte(ansichar('a')))+$a);
       end;
       ord('A')..ord('F'):begin
        Value:=(Value shl 4) or ((byte(CurrentChar)-byte(ansichar('A')))+$a);
       end;
       else begin
        break;
       end;
      end;
      ReadChar;
      inc(Count);
     end;
    end else begin
     while not IsEOFOrAbortError do begin
      case CurrentChar of
       ord('0')..ord('9'):begin
        Value:=(Value*10)+(byte(CurrentChar)-byte(ansichar('0')));
       end;
       else begin
        break;
       end;
      end;
      ReadChar;
      inc(Count);
     end;
    end;
    if Count>0 then begin
     HugeStringConcatChar(result,Value,Len);
    end;
   end else begin
    break;
   end;
  end;
 end;
 if AreWeInString then begin
  Error.AbortCode(53);
 end;
 SetLength(result,Len);
end;

function TScanner.ReadNext:boolean;
var CurrentReadNextString,CurrentReadNextStringUpCase:ansistring;
    Comment:THugeString;
    LastChar:THugeChar;
    Link:longword;
    OldPos,Count,Len:longint;
    InString:boolean;
    OK:longbool;
begin
 if ToInjectToken<>tstNone then begin
  result:=true;
  CurrentToken:=ToInjectToken;
  ToInjectToken:=tstNone;
  exit;
 end;
 if UseNextToken then begin
  result:=true;
  CurrentToken:=NextToken;
  UseNextToken:=false;
  NextToken:=tstNone;
  LastToken:=tstNone;
  exit;
 end;
 if InputStream.Position=0 then begin
  ReadChar;
 end;
 case ModeStack[ModeStackPointer] of
  smPASCAL,smPASCALCODE,smPASCALEXPRESSION:begin
   while (CurrentChar<=32) and not IsEOFOrAbortError do begin
    ReadChar;
   end;
   result:=not IsEOFOrAbortError;
   LastToken:=CurrentToken;
   CurrentToken:=tstNone;
   CurrentDirectiveToken:=tstNone;
   if result then begin
    while not IsEOFOrAbortError do begin
     while (CurrentChar<=32) and not IsEOFOrAbortError do begin
      ReadChar;
     end;
     if CurrentChar<256 then begin
      case CurrentChar of
       ord('('):begin
        ReadChar;
        if CurrentChar=ord('.') then begin
         ReadChar;
         CurrentToken:=tstLeftBracket;
         exit;
        end else if CurrentChar=ord('*') then begin
         ReadChar;
         LastChar:=0;
         if CurrentChar=ord('$') then begin
          ReadChar;
          Comment:=nil;
          Len:=0;
          InString:=false;
          while not ((((CurrentChar=ord(')')) and (LastChar=ord('*'))) and not InString) or IsEOFOrAbortError) do begin
           HugeStringConcatChar(Comment,CurrentChar,Len);
           if CurrentChar=ord('''') then begin
            InString:=not InString;
           end;
           LastChar:=CurrentChar;
           ReadChar;
          end;
          SetLength(Comment,Len);
          if length(Comment)>0 then begin
           Comment:=COPY(Comment,0,length(Comment)-1);
          end;
          ReadChar;
          Preprocessor.ProcessComment(Comment);
         end else begin
          while not (((CurrentChar=ord(')')) and (LastChar=ord('*'))) or IsEOFOrAbortError) do begin
           LastChar:=CurrentChar;
           ReadChar;
          end;
          ReadChar;
         end;
         continue;
        end else if not Preprocessor.SkippingFalseIf then begin
         CurrentToken:=tstLeftParen;
         exit;
        end;
       end;
       ord('{'):begin
        ReadChar;
        if CurrentChar=ord('$') then begin
         ReadChar;
         InString:=false;
         Comment:=nil;
         Len:=0;
         while not (((CurrentChar=ord('}')) and not InString) or IsEOFOrAbortError) do begin
          HugeStringConcatChar(Comment,CurrentChar,Len);
          if CurrentChar=ord('''') then begin
           InString:=not InString;
          end;
          ReadChar;
         end;
         ReadChar;
         SetLength(Comment,Len);
         Preprocessor.ProcessComment(Comment);
        end else begin
         while (CurrentChar<>ord('}')) and not IsEOFOrAbortError do begin
          ReadChar;
         end;
         ReadChar;
        end;
        continue;
       end;
       ord('/'):begin
        ReadChar;
        if CurrentChar=ord('/') then begin
         ReadChar;
         if CurrentChar=ord('$') then begin
          ReadChar;
          Comment:=nil;
          Len:=0;
          while not ((CurrentChar in [13,10]) or IsEOFOrAbortError) do begin
           HugeStringConcatChar(Comment,CurrentChar,Len);
           ReadChar;
          end;
          SetLength(Comment,Len);
          Preprocessor.ProcessComment(Comment);
         end else begin
          while not ((CurrentChar in [13,10]) or IsEOFOrAbortError) do begin
           ReadChar;
          end;
         end;
         continue;
        end else if not Preprocessor.SkippingFalseIf then begin
         CurrentToken:=tstSlash;
         exit;
        end;
       end;
       else begin
        if Preprocessor.SkippingFalseIf and not IsEOFOrAbortError then begin
         ReadChar;
        end;
       end;
      end;
     end else begin
      if Preprocessor.SkippingFalseIf and not IsEOFOrAbortError then begin
       ReadChar;
      end;
     end;
     if Preprocessor.SkippingFalseIf and not IsEOFOrAbortError then begin
      continue;
     end;
     break;
    end;
    if IsEOFOrAbortError then begin
     result:=false;
     exit;
    end;
    if CurrentChar<256 then begin
     case CurrentChar of
      ord('a')..ord('z'),ord('A')..ord('Z'):begin
   //  CurrentReadNextString:=ReadString(['a'..'z','A'..'Z','0'..'9','_']);
       CurrentReadNextString:='';
       SetLength(CurrentReadNextString,16);
       Len:=0;
       while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
        case ansichar(byte(CurrentChar)) of
         'A'..'Z','a'..'z','0'..'9','_':begin
          if (Len+1)>=length(CurrentReadNextString) then begin
           SetLength(CurrentReadNextString,RoundUpToPowerOfTwo(Len+1));
          end;
          CurrentReadNextString[Len+1]:=ansichar(byte(CurrentChar));
          inc(Len);
          ReadChar;
         end;
  {      '_':begin
          if (Len+3)>=length(CurrentReadNextString) then begin
           SetLength(CurrentReadNextString,RoundUpToPowerOfTwo(Len+3));
          end;
          CurrentReadNextString[Len+1]:='_';
          CurrentReadNextString[Len+2]:='U';
          CurrentReadNextString[Len+3]:='_';
          inc(Len,3);
          ReadChar;
         end;}
         else begin
          break;
         end;
        end;
       end;
       SetLength(CurrentReadNextString,Len);
       CurrentReadNextStringUpCase:=UpperCase(CurrentReadNextString);
       if KeywordStringTree.Find(CurrentReadNextStringUpCase,Link) then begin
        CurrentToken:=TScannerToken(Link);
        if (CurrentToken in Directives) and not (CurrentToken in AllowedDirectives) then begin
         CurrentDirectiveToken:=CurrentToken;
         CurrentToken:=tstNone;
        end;
       end;
       if CurrentToken=tstNone then begin
        CurrentToken:=tstIdentifier;
        CurrentIdentifier:=CurrentReadNextStringUpCase;
        CurrentOriginalIdentifier:=CurrentReadNextString;
       end;
      end;
      ord('0')..ord('9'):begin
       CurrentReadNextString:='';
       Count:=0;
       CurrentValue:=0;
       while not IsEOFOrAbortError do begin
        case CurrentChar of
         ord('0')..ord('9'):begin
          CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
          CurrentValue:=(CurrentValue*10)+(CurrentChar-byte(ansichar('0')));
         end;
         else begin
          break;
         end;
        end;
        ReadChar;
        inc(Count);
       end;
       OldPos:=InputStream.Position;
       if CurrentChar=ord('.') then begin
        ReadChar;
        if CurrentChar in [ord('0')..ord('9')] then begin
         CurrentReadNextString:=CurrentReadNextString+'.';
         while not IsEOFOrAbortError do begin
          case CurrentChar of
           ord('0')..ord('9'):begin
            CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
           end;
           else begin
            break;
           end;
          end;
          ReadChar;
         end;
         if CurrentChar in [ord('e'),ord('E')] then begin
          ReadChar;
          CurrentReadNextString:=CurrentReadNextString+'E';
          if CurrentChar in [ord('-'),ord('+')] then begin
           CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
           ReadChar;
          end;
          while not IsEOFOrAbortError do begin
           case CurrentChar of
            ord('0')..ord('9'):begin
             CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
            end;
            else begin
             break;
            end;
           end;
           ReadChar;
          end;
         end;
         OK:=false;
         CurrentFloatValue:=StringToFloat(CurrentReadNextString,@OK);
         if OK then begin
          CurrentToken:=tstFloatValue;
         end;
        end else begin
         InputStream.Seek(OldPos);
         CurrentChar:=ord('.');
         if Count>0 then begin
          CurrentToken:=tstValue;
         end;
        end;
       end else begin
        if CurrentChar in [ord('e'),ord('E')] then begin
         ReadChar;
         CurrentReadNextString:=CurrentReadNextString+'E';
         if CurrentChar in [ord('-'),ord('+')] then begin
          CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
          ReadChar;
         end;
         while not IsEOFOrAbortError do begin
          case CurrentChar of
           ord('0')..ord('9'):begin
            CurrentReadNextString:=CurrentReadNextString+ansichar(byte(CurrentChar));
           end;
           else begin
            break;
           end;
          end;
          ReadChar;
         end;
         OK:=false;
         CurrentFloatValue:=StringToFloat(CurrentReadNextString,@OK);
         if OK then begin
          CurrentToken:=tstFloatValue;
         end;
        end else begin
         if Count>0 then begin
          CurrentToken:=tstValue;
         end;
        end;
       end;
      end;
      ord('$'):begin
       ReadChar;
       Count:=0;
       CurrentValue:=0;
       while not IsEOFOrAbortError do begin
        case CurrentChar of
         ord('0')..ord('9'):begin
          CurrentValue:=(CurrentValue shl 4) or (CurrentChar-byte(ansichar('0')));
         end;
         ord('a')..ord('f'):begin
          CurrentValue:=(CurrentValue shl 4) or ((CurrentChar-byte(ansichar('a')))+$a);
         end;
         ord('A')..ord('F'):begin
          CurrentValue:=(CurrentValue shl 4) or ((CurrentChar-byte(ansichar('A')))+$a);
         end;
         else begin
          break;
         end;
        end;
        ReadChar;
        inc(Count);
       end;
       if Count>0 then begin
        CurrentToken:=tstValue;
       end;
      end;
      ord('#'),ord(''''):begin
       CurrentString:=ReadPascalString;
       if length(CurrentString)=1 then begin
        CurrentCharValue:=CurrentString[0];
        CurrentToken:=tstCharValue;
       end else begin
        CurrentToken:=tstStringValue;
       end;
      end;
      ord('='):begin
       ReadChar;
       CurrentToken:=tstEqual;
      end;
      ord(')'):begin
       ReadChar;
       CurrentToken:=tstRightParen;
      end;
      ord('['):begin
       ReadChar;
       if CurrentChar=ord('[') then begin
        ReadChar;
        if CurrentChar=ord('[') then begin
         ReadChar;
         CurrentToken:=tstCCODE;
         inc(ModeStackPointer);
         if ModeStackPointer>=length(ModeStack) then begin
          SetLength(ModeStack,ModeStackPointer+4096);
         end;
         ModeStack[ModeStackPointer]:=smC;
        end;
       end else begin
        CurrentToken:=tstLeftBracket;
       end;
      end;
      ord(']'):begin
       ReadChar;
       if CurrentChar=ord(']') then begin
        ReadChar;
        if CurrentChar=ord(']') then begin
         ReadChar;
         if ModeStackPointer>0 then begin
          CurrentToken:=tstCSKIP;
          dec(ModeStackPointer);
         end else begin
          Error.InternalError(201304050322000);
         end;
        end;
       end else begin
        CurrentToken:=tstRightBracket;
       end;
      end;
      ord('*'):begin
       ReadChar;
       CurrentToken:=tstMul;
      end;
      ord('+'):begin
       ReadChar;
       CurrentToken:=tstPlus;
      end;
      ord('-'):begin
       ReadChar;
       CurrentToken:=tstMinus;
      end;
      ord(';'):begin
       ReadChar;
       CurrentToken:=tstSeparator;
      end;
      ord('@'):begin
       ReadChar;
       CurrentToken:=tstAt;
      end;
      ord('^'):begin
       ReadChar;
       CurrentToken:=tstPointer;
      end;
      ord(':'):begin
       ReadChar;
       if CurrentChar=ord('=') then begin
        ReadChar;
        CurrentToken:=tstAssign;
       end else begin
        CurrentToken:=tstColon;
       end;
      end;
      ord('.'):begin
       ReadChar;
       if CurrentChar=ord('.') then begin
        ReadChar;
        CurrentToken:=tstDoublePeriod;
       end else if CurrentChar=ord(')') then begin
        ReadChar;
        CurrentToken:=tstRightBracket;
       end else begin
        CurrentToken:=tstPeriod;
       end;
      end;
      ord('>'):begin
       ReadChar;
       if CurrentChar=ord('>') then begin
        ReadChar;
        if CurrentChar=ord('>') then begin
         ReadChar;
         if ModeStackPointer>0 then begin
          CurrentToken:=tstCSKIP;
          dec(ModeStackPointer);
         end else begin
          Error.InternalError(201304050322000);
         end;
        end;
       end else if CurrentChar=ord('=') then begin
        ReadChar;
        CurrentToken:=tstGreaterOrEqual;
       end else begin
        CurrentToken:=tstGreater;
       end;
      end;
      ord('<'):begin
       ReadChar;
       if CurrentChar=ord('=') then begin
        ReadChar;
        CurrentToken:=tstLessOrEqual;
       end else if CurrentChar=ord('>') then begin
        ReadChar;
        CurrentToken:=tstNotEqual;
       end else if CurrentChar=ord('<') then begin
        ReadChar;
        if CurrentChar=ord('<') then begin
         ReadChar;
         CurrentToken:=tstCEXPR;
         inc(ModeStackPointer);
         if ModeStackPointer>=length(ModeStack) then begin
          SetLength(ModeStack,ModeStackPointer+4096);
         end;
         ModeStack[ModeStackPointer]:=smC;
        end;
       end else begin
        CurrentToken:=tstLess;
       end;
      end;
      ord('('):begin
       ReadChar;
       if CurrentChar=ord('*') then begin
        ReadChar;
        LastChar:=0;
        while not (((CurrentChar=ord(')')) and (LastChar=ord('*'))) or IsEOFOrAbortError) do begin
         LastChar:=CurrentChar;
         ReadChar;
        end;
        ReadChar;
        result:=ReadNext;
       end else begin
        CurrentToken:=tstLeftParen;
       end;
      end;
      ord('{'):begin
       while (CurrentChar<>ord('}')) and not IsEOFOrAbortError do begin
        ReadChar;
       end;
       ReadChar;
       result:=ReadNext;
      end;
      ord('/'):begin
       ReadChar;
       if CurrentChar=ord('/') then begin
        while not ((CurrentChar in [ord(13),ord(10)]) or IsEOFOrAbortError) do begin
         ReadChar;
        end;
        result:=ReadNext;
       end else begin
        CurrentToken:=tstSlash;
       end;
      end;
      ord(','):begin
       ReadChar;
       CurrentToken:=tstComma;
      end;
     end;
    end;
   end;
   Comment:=nil;
  end;
  smC:begin
   result:=false;
   CurrentToken:=tstCBLOCK;
   CurrentString:=nil;
   Len:=0;
   while not IsEOFOrAbortError do begin
    case CurrentChar of
     ord(']'):begin
      ReadChar;
      if CurrentChar=ord(']') then begin
       ReadChar;
       if CurrentChar=ord(']') then begin
        ReadChar;
        if ModeStackPointer>0 then begin
         dec(ModeStackPointer);
        end else begin
         Error.InternalError(201304050322000);
        end;
        ToInjectToken:=tstCEND;
        break;
       end else begin
        HugeStringConcatChar(CurrentString,ord(']'),Len);
        HugeStringConcatChar(CurrentString,ord(']'),Len);
       end;
      end else begin
       HugeStringConcatChar(CurrentString,ord(']'),Len);
      end;
     end;
     ord('<'):begin
      ReadChar;
      if CurrentChar=ord('<') then begin
       ReadChar;
       if CurrentChar=ord('<') then begin
        ReadChar;
        inc(ModeStackPointer);
        if ModeStackPointer>=length(ModeStack) then begin
         SetLength(ModeStack,ModeStackPointer+4096);
        end;
        ModeStack[ModeStackPointer]:=smPASCALEXPRESSION;
        break;
       end else begin
        HugeStringConcatChar(CurrentString,ord('<'),Len);
        HugeStringConcatChar(CurrentString,ord('<'),Len);
       end;
      end else begin
       HugeStringConcatChar(CurrentString,ord('<'),Len);
      end;
     end;
     ord('['):begin
      ReadChar;
      if CurrentChar=ord('[') then begin
       ReadChar;
       if CurrentChar=ord('[') then begin
        ReadChar;
        inc(ModeStackPointer);
        if ModeStackPointer>=length(ModeStack) then begin
         SetLength(ModeStack,ModeStackPointer+4096);
        end;
        ModeStack[ModeStackPointer]:=smPASCALCODE;
        break;
       end else begin
        HugeStringConcatChar(CurrentString,ord('['),Len);
        HugeStringConcatChar(CurrentString,ord('['),Len);
       end;
      end else begin
       HugeStringConcatChar(CurrentString,ord('['),Len);
      end;
     end;
     else begin
      HugeStringConcatChar(CurrentString,CurrentChar,Len);
      ReadChar;
     end;
    end;
   end;
   SetLength(CurrentString,Len);
  end;
  else begin
   result:=false;
   CurrentToken:=tstNone;
   ReadChar;
  end;
 end;
end;

function TScanner.ReadIdentifier(OriginalIdentifier:pointer):ansistring;
begin
 if CurrentToken=tstIdentifier then begin
  result:=tpsIdentifier+CurrentIdentifier;
  if assigned(OriginalIdentifier) then begin
   ansistring(OriginalIdentifier^):=CurrentOriginalIdentifier;
  end;
  ReadNext;
 end else begin
  if CurrentToken<=MaxToken then begin
   Error.AbortCode(29,TokenNames[tstIdentifier],TokenNames[CurrentToken]);
  end else begin
   Error.InternalError(200605180937000);
  end;
 end;
end;

function TScanner.ReadNumber:int64;
begin
 if CurrentToken=tstValue then begin
  result:=CurrentValue;
  ReadNext;
 end else begin
  result:=0;
  if CurrentToken<=MaxToken then begin
   Error.AbortCode(29,TokenNames[tstValue],TokenNames[CurrentToken]);
  end else begin
   Error.InternalError(200605180937001);
  end;
 end;
end;

function TScanner.ReadFloat:extended;
begin
 if CurrentToken=tstFloatValue then begin
  result:=CurrentFloatValue;
  ReadNext;
 end else begin
  result:=0;
  if CurrentToken<=MaxToken then begin
   Error.AbortCode(29,TokenNames[tstFloatValue],TokenNames[CurrentToken]);
  end else begin
   Error.InternalError(200605180937002);
  end;
 end;
end;

function TScanner.ReadLabel:ansistring;
begin
 if CurrentToken=tstIdentifier then begin
  result:=tpsIdentifier+CurrentIdentifier;
  ReadNext;
 end else if CurrentToken=tstValue then begin
  STR(CurrentValue,result);
  result:=tpsIdentifier+result;
  ReadNext;
 end else begin
  if CurrentToken<=MaxToken then begin
   Error.AbortCode(31);
  end else begin
   Error.InternalError(200605180937003);
  end;
 end;
end;

function TScanner.ReadLabelEx:ansistring;
begin
 if CurrentToken=tstIdentifier then begin
  result:=tpsIdentifier+CurrentIdentifier;
 end else if CurrentToken=tstValue then begin
  STR(CurrentValue,result);
  result:=tpsIdentifier+result;
 end else begin
  result:='';
 end;
end;

function TScanner.Match(Token:TScannerToken):boolean;
begin
 result:=CurrentToken=Token;
 if result then begin
  ReadNext;
 end else begin
  if CurrentToken<=MaxToken then begin
   Error.AbortCode(29,TokenNames[Token],TokenNames[CurrentToken]);
  end else begin
   Error.InternalError(200605180938000);
  end;
 end;
end;

procedure TScanner.Illegal(Token:TScannerToken);
begin
 if CurrentToken<=MaxToken then begin
  Error.AbortCode(509,TokenNames[Token]);
 end else begin
  Error.InternalError(200605181054000);
 end;
end;

function TScanner.CurrentMode:TScannerMode;
begin
 result:=ModeStack[ModeStackPointer];
end;

function TScanner.MaybeLabel(Token:TScannerToken):boolean;
begin
 result:=CurrentToken in [tstValue,tstIdentifier];
end;

function TScanner.IsLabel(Token:TScannerToken):boolean;
begin
 result:=MaybeLabel(CurrentToken) and (CurrentChar=ord(':'));
end;

procedure TScanner.CheckForDirectives(AllowedDirectives:TScannerTokens);
begin
 if CurrentDirectiveToken in AllowedDirectives then begin
  CurrentToken:=CurrentDirectiveToken;
 end;
end;

end.
