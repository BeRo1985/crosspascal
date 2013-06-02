unit Preprocessor;
{$i Compiler.inc}

interface

uses SysUtils,Math,BeRoUtils,Error,BeRoStringTree,BeRoStringHashMap,StringList,Symbols,Globals,
     HugeString;

type TPreprocessorDirectives=(tdNONE,tdDEFINE,tdELSE,tdELSEIF,tdENDIF,tdIF,
                              tdIFDEF,tdIFEND,tdIFNDEF,tdIFOPT,tdI,tdINCLUDE,
                              tdUNDEF,
                              tdA,
                              tdA1,
                              tdA2,
                              tdA4,
                              tdA8,
                              tdALIGN,
                              tdAPPTYPE,
                              tdC,
                              tdASSERTIONS,
                              tdB,
                              tdBOOLEVAL,
                              tdLIBPREFIX,
                              tdLIBSUBFIX,
                              tdLIBVERSION,
                              tdCODEPAGE,
                              tdD,
                              tdDEBUGINFO,
                              tdDENYPACKAGEUNIT,
                              tdDESCRIPTION,
                              tdDESIGNINFO,
                              tdE,
                              tdEXTENSION,
                              tdOBJEXPORTALL,
                              tdX,
                              tdEXTENDEDSYNTAX,
                              tdEXTERNALSYM,
                              tdHINTS,
                              tdHPPEMIT,
                              tdIMAGEBASE,
                              tdIMPLICITBUILD,
                              tdG,
                              tdIMPORTEDDATA,
                              tdIOCHECKS,
                              tdL,
                              tdLINK,
                              tdLOCALSYMBOLS,
                              tdH,
                              tdLONGSTRINGS,
                              tdM,
                              tdMINSTACKSIZE,
                              tdMAXSTACKSIZE,
                              tdZ,
                              tdZ1,
                              tdZ2,
                              tdZ4,
                              tdMINENUMSIZE,
                              tdP,
                              tdOPENSTRINGS,
                              tdO,
                              tdOPTIMIZATION,
                              tdQ,
                              tdOVERFLOWCHECKS,
                              tdSETPEFLAGS,
                              tdSETPEOPTFLAGS,
                              tdU,
                              tdSAFEDIVIDE,
                              tdNODEFINE,
                              tdNOINCLUDE,
                              tdR,
                              tdRANGECHECKS,
                              tdREALCOMPATIBILITY,
                              tdRESOURCE,
                              tdRUNONLY,
                              tdTYPEINFO,
                              tdY,
                              tdYD,
                              tdREFERENCEINFO,
                              tdDEFINITIONINFO,
                              tdT,
                              tdTYPEDADDRESS,
                              tdUNICODESTRINGS,
                              tdV,
                              tdVARSTRINGCHECKS,
                              tdWARNINGS,
                              tdWEAKPACKAGEUNIT,
                              tdW,
                              tdSTACKFRAMES,
                              tdJ,
                              tdWRITEABLECONST
                             );

     PPreprocessorStack=^TPreprocessorStack;
     TPreprocessorStack=record
      Next:PPreprocessorStack;
      ParentDirective:TPreprocessorDirectives;
      Directive:TPreprocessorDirectives;
      ParentCondition:boolean;
      Condition:boolean;
     end;

     TPreprocessorFileStackOpenIncludeProc=procedure(IncludeFileName:ansistring) of object;

     TPreprocessor=class
      private
       Error:TError;
       SymbolManager:TSymbolManager;
       DirectiveStringTree:TBeRoStringTree;
       DefineStringHashMap:TBeRoStringHashMap;
       DefineStringList:TStringList;
       Stack:PPreprocessorStack;
       StackFree:PPreprocessorStack;
       FileStackOpenInclude:TPreprocessorFileStackOpenIncludeProc;
       CurrentComment:THugeString;
       CurrentCommentPosition:longint;
       CurrentCommentEOF:boolean;
       CurrentChar:longword;
       function ReadChar:longword;
       function PeekChar(i:longint):longword;
      public
       ModuleSymbol:PSymbol;
       GlobalSwitches:PGlobalSwitches;
       LocalSwitches:PLocalSwitches;
       constructor Create(TheError:TError;TheSymbolManager:TSymbolManager;TheFileStackOpenInclude:TPreprocessorFileStackOpenIncludeProc;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
       destructor Destroy; override;
       procedure Reset;
       procedure AddDirective(Directive:TPreprocessorDirectives;Name:ansistring);
       function PushStack(Directive,ParentDirective:TPreprocessorDirectives;Condition,ParentCondition:boolean):PPreprocessorStack;
       function PopStack:boolean;
       function CurrentCondition:boolean;
       function SkippingFalseIf:boolean;
       function ReadPascalString:THugeString;
       function ProcessExpression:boolean;
       procedure ProcessComment(const Comment:THugeString);
       procedure AddDefine(const DefineString:ansistring);
       procedure RemoveDefine(const DefineString:ansistring);
     end;

implementation

uses Scanner;

constructor TPreprocessor.Create(TheError:TError;TheSymbolManager:TSymbolManager;TheFileStackOpenInclude:TPreprocessorFileStackOpenIncludeProc;TheGlobalSwitches:PGlobalSwitches;TheLocalSwitches:PLocalSwitches);
begin
 inherited Create;
 CurrentComment:=nil;
 Error:=TheError;
 SymbolManager:=TheSymbolManager;
 DirectiveStringTree:=TBeRoStringTree.Create;
 DefineStringHashMap:=TBeRoStringHashMap.Create;
 DefineStringList:=TStringList.Create;
 FileStackOpenInclude:=TheFileStackOpenInclude;
 GlobalSwitches:=TheGlobalSwitches;
 LocalSwitches:=TheLocalSwitches;
 AddDirective(tdDEFINE,'DEFINE');
 AddDirective(tdELSE,'ELSE');
 AddDirective(tdELSEIF,'ELSEIF');
 AddDirective(tdENDIF,'ENDIF');
 AddDirective(tdI,'I');
 AddDirective(tdIF,'IF');
 AddDirective(tdIFDEF,'IFDEF');
 AddDirective(tdIFEND,'IFEND');
 AddDirective(tdIFNDEF,'IFNDEF');
 AddDirective(tdIFOPT,'IFOPT');
 AddDirective(tdINCLUDE,'INCLUDE');
 AddDirective(tdUNDEF,'UNDEF');
 AddDirective(tdA,'A');
 AddDirective(tdA1,'A1');
 AddDirective(tdA2,'A2');
 AddDirective(tdA4,'A4');
 AddDirective(tdA8,'A8');
 AddDirective(tdALIGN,'ALIGN');
 AddDirective(tdAPPTYPE,'APPTYPE');
 AddDirective(tdC,'C');
 AddDirective(tdC,'ASSERTIONS');
 AddDirective(tdB,'B');
 AddDirective(tdBOOLEVAL,'BOOLEVAL');
 AddDirective(tdLIBPREFIX,'LIBPREFIX');
 AddDirective(tdLIBSUBFIX,'LIBSUBFIX');
 AddDirective(tdLIBVERSION,'LIBVERSION');
 AddDirective(tdCODEPAGE,'CODEPAGE');
 AddDirective(tdD,'D');
 AddDirective(tdDEBUGINFO,'DEBUGINFO');
 AddDirective(tdDENYPACKAGEUNIT,'DENYPACKAGEUNIT');
 AddDirective(tdDESCRIPTION,'DESCRIPTION');
 AddDirective(tdDESIGNINFO,'DESIGNINFO');
 AddDirective(tdE,'E');
 AddDirective(tdEXTENSION,'EXTENSION');
 AddDirective(tdOBJEXPORTALL,'OBJEXPORTALL');
 AddDirective(tdX,'X');
 AddDirective(tdEXTENDEDSYNTAX,'EXTENDEDSYNTAX');
 AddDirective(tdEXTERNALSYM,'EXTERNALSYM');
 AddDirective(tdHINTS,'HINTS');
 AddDirective(tdHPPEMIT,'HPPEMIT');
 AddDirective(tdIMAGEBASE,'IMAGEBASE');
 AddDirective(tdG,'G');
 AddDirective(tdIMPLICITBUILD,'IMPLICITBUILD');
 AddDirective(tdIOCHECKS,'IOCHECKS');
 AddDirective(tdL,'L');
 AddDirective(tdLINK,'LINK');
 AddDirective(tdLOCALSYMBOLS,'LOCALSYMBOLS');
 AddDirective(tdH,'H');
 AddDirective(tdLONGSTRINGS,'LONGSTRINGS');
 AddDirective(tdM,'M');
 AddDirective(tdMINSTACKSIZE,'MINSTACKSIZE');
 AddDirective(tdMAXSTACKSIZE,'MAXSTACKSIZE');
 AddDirective(tdZ,'Z');
 AddDirective(tdZ1,'Z1');
 AddDirective(tdZ2,'Z2');
 AddDirective(tdZ4,'Z4');
 AddDirective(tdMINENUMSIZE,'MINENUMSIZE');
 AddDirective(tdP,'P');
 AddDirective(tdOPENSTRINGS,'OPENSTRINGS');
 AddDirective(tdO,'O');
 AddDirective(tdOPTIMIZATION,'OPTIMIZATION');
 AddDirective(tdQ,'Q');
 AddDirective(tdOVERFLOWCHECKS,'OVERFLOWCHECKS');
 AddDirective(tdSETPEFLAGS,'SETPEFLAGS');
 AddDirective(tdSETPEOPTFLAGS,'SETPEOPTFLAGS');
 AddDirective(tdU,'U');
 AddDirective(tdSAFEDIVIDE,'SAFEDIVIDE');
 AddDirective(tdNODEFINE,'NODEFINE');
 AddDirective(tdNOINCLUDE,'NOINCLUDE');
 AddDirective(tdR,'R');
 AddDirective(tdRANGECHECKS,'RANGECHECKS');
 AddDirective(tdREALCOMPATIBILITY,'REALCOMPATIBILITY');
 AddDirective(tdRESOURCE,'RESOURCE');
 AddDirective(tdRUNONLY,'RUNONLY');
 AddDirective(tdTYPEINFO,'TYPEINFO');
 AddDirective(tdY,'Y');
 AddDirective(tdYD,'YD');
 AddDirective(tdREFERENCEINFO,'REFERENCEINFO');
 AddDirective(tdDEFINITIONINFO,'DEFINITIONINFO');
 AddDirective(tdT,'T');
 AddDirective(tdTYPEDADDRESS,'TYPEDADDRESS');
 AddDirective(tdUNICODESTRINGS,'UNICODESTRINGS');
 AddDirective(tdV,'V');
 AddDirective(tdVARSTRINGCHECKS,'VARSTRINGCHECKS');
 AddDirective(tdWARNINGS,'WARNINGS');
 AddDirective(tdWEAKPACKAGEUNIT,'WEAKPACKAGEUNIT');
 AddDirective(tdW,'W');
 AddDirective(tdSTACKFRAMES,'STACKFRAMES');
 AddDirective(tdJ,'J');
 AddDirective(tdWRITEABLECONST,'WRITEABLECONST');
 Stack:=nil;
 StackFree:=nil;
 ModuleSymbol:=nil;
 Reset;
end;

destructor TPreprocessor.Destroy;
begin
 Reset;
 DirectiveStringTree.Destroy;
 DefineStringHashMap.Destroy;
 DefineStringList.Destroy;
 inherited Destroy;
end;

procedure TPreprocessor.Reset;
var StackNext:PPreprocessorStack;
begin
 CurrentComment:=nil;
 DefineStringHashMap.Clear;
 DefineStringList.Clear;
 AddDefine('OBJPAS2C');
 AddDefine('CROSSPASCAL');
 AddDefine('VER100');
 AddDefine('CPUC');
 AddDefine('CONDITIONALEXPRESSIONS');
 while assigned(Stack) do begin
  StackNext:=Stack^.Next;
  Dispose(Stack);
  Stack:=StackNext;
 end;
 while assigned(StackFree) do begin
  StackNext:=StackFree^.Next;
  Dispose(StackFree);
  StackFree:=StackNext;
 end;
 Stack:=nil;
 StackFree:=nil;
end;

procedure TPreprocessor.AddDirective(Directive:TPreprocessorDirectives;Name:ansistring);
begin
 DirectiveStringTree.Add(Name,TBeRoStringTreeLink(ord(Directive)));
end;

function TPreprocessor.PushStack(Directive,ParentDirective:TPreprocessorDirectives;Condition,ParentCondition:boolean):PPreprocessorStack;
begin
 if assigned(StackFree) then begin
  result:=StackFree;
  StackFree:=StackFree^.Next;
 end else begin
  New(result);
 end;
 FillChar(result^,SizeOf(TPreprocessorStack),#0);
 result^.ParentDirective:=ParentDirective;
 result^.Directive:=Directive;
 result^.Condition:=Condition and CurrentCondition;
 result^.ParentCondition:=ParentCondition;
 result^.Next:=Stack;
 Stack:=result;
end;

function TPreprocessor.PopStack:boolean;
var StackNext:PPreprocessorStack;
begin
 result:=assigned(Stack);
 if result then begin
  StackNext:=Stack^.Next;
  Stack^.Next:=StackFree;
  StackFree:=Stack;
  Stack:=StackNext;
 end;
end;

function TPreprocessor.CurrentCondition:boolean;
begin
 if assigned(Stack) then begin
  result:=Stack^.Condition;
 end else begin
  result:=true;
 end;
end;

function TPreprocessor.SkippingFalseIf:boolean;
begin
 if assigned(Stack) then begin
  result:=not Stack^.Condition;
 end else begin
  result:=false;
 end;
end;

function TPreprocessor.ReadChar:longword;
begin
 if (CurrentCommentPosition>=0) and (CurrentCommentPosition<length(CurrentComment)) then begin
  result:=CurrentComment[CurrentCommentPosition];
  inc(CurrentCommentPosition);
 end else begin
  CurrentCommentEOF:=true;
  result:=0;
 end;
 CurrentChar:=result;
end;

function TPreprocessor.PeekChar(i:longint):longword;
begin
 if ((CurrentCommentPosition+i)>=0) and ((CurrentCommentPosition+i)<length(CurrentComment)) then begin
  result:=CurrentComment[CurrentCommentPosition+i];
 end else begin
  result:=0;
 end;
end;

function TPreprocessor.ReadPascalString:THugeString;
var Value,Count,Len:longint;
    AreWeInString:boolean;
begin
 result:=nil;
 Len:=0;
 AreWeInString:=false;
 while not (CurrentCommentEOF or Error.DoAbort or Error.Errors) do begin
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
     while CurrentChar>0 do begin
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
     while CurrentChar>0 do begin
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

function TPreprocessor.ProcessExpression:boolean;
type TValue=record
      T:ansichar;
      B:boolean;
      I:int64;
      F:extended;
      S:THugeString;
     end;

 procedure TermSyntaxError;
 begin
  Error.AbortCode(504);
 end;

 function ParseCompare:TValue; forward;

 procedure SkipSpaces;
 begin
  while (CurrentChar<=32) and not CurrentCommentEOF do begin
   ReadChar;
  end;
 end;

 function ParseIdent:TValue;
 var Ident,IdentEX:ansistring;
     Symbol:PSymbol;
 begin
  FillChar(result,SizeOf(TValue),#0);
  result.B:=false;
  result.T:='B';
  SkipSpaces;
  Ident:='';
  while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['A'..'Z','a'..'z','0'..'9','_']) and not CurrentCommentEOF do begin
   Ident:=Ident+UPCASE(ansichar(byte(CurrentChar)));
   ReadChar;
  end;
  SkipSpaces;
  if (Ident='DEFINED') or (Ident='UNDEFINED') then begin
   if CurrentChar=ord('(') then begin
    ReadChar;
    SkipSpaces;
    IdentEx:='';
    while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['A'..'Z','a'..'z','0'..'9','_']) and not CurrentCommentEOF do begin
     IdentEx:=IdentEx+UPCASE(ansichar(byte(CurrentChar)));
     ReadChar;
    end;
    SkipSpaces;
    if Ident='DEFINED' then begin
     result.B:=assigned(DefineStringHashMap.Get(IdentEx));
    end else begin
     result.B:=not assigned(DefineStringHashMap.Get(IdentEx));
    end;
    result.T:='B';
    if CurrentChar=ord(')') then begin
     ReadChar;
    end else begin
     Error.AbortCode(504);
    end;
   end else begin
    Error.AbortCode(504);
   end;
  end else if Ident='FALSE' then begin
   result.B:=false;
   result.T:='B';
  end else if Ident='TRUE' then begin
   result.B:=true;
   result.T:='B';
  end else if Ident='NOT' then begin
   result:=ParseCompare;
   case result.T of
    'B':begin
     result.B:=not result.B;
    end;
    'I':begin
     result.I:=not result.I;
    end;
    'F':begin
     result.F:=0;
    end;
    'S':begin
     result.S:=nil;
    end;
   end;
  end else begin
   Symbol:=SymbolManager.GetSymbol(tpsIdentifier+Ident,ModuleSymbol);
   if assigned(Symbol) then begin
    if Symbol^.SymbolType=tstConstant then begin
     case Symbol^.ConstantType of
      tctOrdinal:begin
       result.I:=Symbol^.IntValue;
       result.T:='I';
      end;
      tctAnsiChar:begin
       result.S:=nil;
       SetLength(result.S,1);
       result.S[0]:=Symbol^.CharValue;
       result.T:='S';
      end;
      tctWideChar:begin
       result.S:=nil;
       SetLength(result.S,1);
       result.S[0]:=Symbol^.CharValue;
       result.T:='S';
      end;
      tctHugeChar:begin
       result.S:=nil;
       SetLength(result.S,1);
       result.S[0]:=Symbol^.CharValue;
       result.T:='S';
      end;
      tctFloat:begin
       result.F:=Symbol^.FloatValue;
       result.T:='F';
      end;
      tctAnsiString:begin
       result.S:=Symbol^.StringValue;
       result.T:='S';
      end;
      tctWideString:begin
       result.S:=Symbol^.StringValue;
       result.T:='S';
      end;
      tctShortString:begin
       result.S:=AnsiStringToHugeString(Symbol^.ShortStringValue);
       result.T:='S';
      end;
      tctPointer:begin
       result.I:=longword(Symbol^.PointerTo);
       result.T:='I';
      end;
      else begin
       Error.AbortCode(114);
      end;
     end;
    end else begin
     Error.AbortCode(114);
    end;
   end else begin
    Error.AbortCode(504);
   end;
  end;
 end;

 function ReadString:TValue;
 begin
  FillChar(result,SizeOf(TValue),#0);
  result.T:='S';
  result.S:=ReadPascalString;
 end;

 function ParseNumber:TValue;
 var NumberString,Sign:ansistring;
     Dot,Hex:boolean;
     C:ansichar;
     Value,VALCode:longint;
     FPValue:single;
     FPValueCasted:longint absolute FPValue;
 begin
  FillChar(result,SizeOf(TValue),#0);
  result.T:='I';
  NumberString:='';
  Hex:=false;
  Dot:=false;
  SkipSpaces;
  if (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['-','+']) and not (CurrentCommentEOF or Error.DoAbort or Error.Errors) then begin
   Sign:=ansichar(byte(CurrentChar));
   ReadChar;
   SkipSpaces;
  end else begin
   Sign:='';
  end;
  while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['0'..'9','A'..'F','a'..'f','$','.']) and not (CurrentCommentEOF or Error.DoAbort or Error.Errors) do begin
   C:=ansichar(byte(CurrentChar));
   ReadChar;
   case C of
    '$':begin
     Hex:=true;
     NumberString:='$';
    end;
    '.':begin
     if Hex then begin
      Error.AbortCode(120);
      exit;
     end else if Dot then begin
      Error.AbortCode(54);
      exit;
     end;
     result.T:='F';
     Dot:=true;
     NumberString:=NumberString+'.';
    end;
    else begin
     NumberString:=NumberString+C;
    end;
   end;
  end;
  NumberString:=Sign+NumberString;
  SkipSpaces;
  if Dot then begin
   Val(NumberString,FPValue,VALCode);
   result.F:=FPValue;
  end else begin
   Val(NumberString,Value,VALCode);
   result.I:=Value;
   result.F:=Value;
  end;
 end;

 function ParseFactor:TValue;
 begin
  if (CurrentCommentEOF or Error.DoAbort or Error.Errors) then begin
   exit;
  end;
  SkipSpaces;
  if (CurrentChar<128) and not CurrentCommentEOF then begin
   case ansichar(byte(CurrentChar)) of
    'A'..'Z','a'..'z':begin
     result:=ParseIdent;
    end;
    '+','-','0'..'9','$':begin
     result:=ParseNumber;
     SkipSpaces;
    end;
    '''','#':begin
     SkipSpaces;
     result:=ReadString;
     SkipSpaces;
    end;
    '(':begin
     ReadChar;
     SkipSpaces;
     result:=ParseCompare;
     SkipSpaces;
     if CurrentChar=ord(')') then begin
      ReadChar;
      SkipSpaces;
     end else begin
      Error.AbortCode(504);
      exit;
     end;
    end;
    '!':begin
     ReadChar;
     result:=ParseCompare;
     SkipSpaces;
     case result.T of
      'B':begin
       result.B:=not result.B;
      end;
      'I':begin
       result.I:=not result.I;
      end;
      'F':begin
       result.F:=0;
      end;
      'S':begin
       result.S:=nil;
      end;
     end;
    end;
    else begin
     TermSyntaxError;
    end;
   end;
  end else begin
   TermSyntaxError;
  end;
 end;

 function ParseMul:TValue;
 var Operation:ansichar;

  procedure ParseOperation;
  begin
   SkipSpaces;
   Operation:=#0;
   if CurrentChar=ord('*') then begin
    Operation:='*';
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('*') then begin
    Operation:='/';
    ReadChar;
    SkipSpaces;
   end else if (CurrentChar=ord('<')) and (PeekChar(0)=ord('<')) then begin
    Operation:='<';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if (CurrentChar=ord('>')) and (PeekChar(0)=ord('>')) then begin
    Operation:='>';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('m')) or (CurrentChar=ord('M'))) and
               ((PeekChar(0)=ord('u')) or (PeekChar(0)=ord('U'))) and
               ((PeekChar(1)=ord('l')) or (PeekChar(1)=ord('L'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='*';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('d')) or (CurrentChar=ord('D'))) and
               ((PeekChar(0)=ord('i')) or (PeekChar(0)=ord('I'))) and
               ((PeekChar(1)=ord('v')) or (PeekChar(1)=ord('V'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='/';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('s')) or (CurrentChar=ord('S'))) and
               ((PeekChar(0)=ord('h')) or (PeekChar(0)=ord('H'))) and
               ((PeekChar(1)=ord('l')) or (PeekChar(1)=ord('L'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='<';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('s')) or (CurrentChar=ord('S'))) and
               ((PeekChar(0)=ord('h')) or (PeekChar(0)=ord('H'))) and
               ((PeekChar(1)=ord('r')) or (PeekChar(1)=ord('R'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='>';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end;
  end;

 var A,B:TValue;
     C:extended;
 begin
  A:=ParseFactor;
  if not (CurrentCommentEOF or Error.DoAbort or Error.Errors) then begin
   ParseOperation;
   while Operation<>#0 do begin
    B:=ParseFactor;
    if Operation='*' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.I:=A.I*B.I;
        end;
        'F':begin
         A.F:=A.I*B.F;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=A.F*B.I;
        end;
        'F':begin
         A.F:=A.F*B.F;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='/' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         if B.I<>0 then begin
          A.I:=A.I div B.I;
         end else begin
          A.I:=0;
         end;
        end;
        'F':begin
         if B.F<>0 then begin
          A.F:=A.I/B.F;
         end else begin
          A.F:=0;
         end;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         if B.I<>0 then begin
          A.F:=A.F/B.I;
         end else begin
          A.F:=0;
         end;
        end;
        'F':begin
         if B.F<>0 then begin
          A.F:=A.F/B.F;
         end else begin
          A.F:=0;
         end;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='<' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.I:=A.I shl B.I;
        end;
        'F':begin
         if B.F<>0 then begin
          A.F:=A.I*power(2,B.F);
         end else begin
          A.F:=0;
         end;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=A.F*(1 shl B.I);
        end;
        'F':begin
         A.F:=A.F*power(2,B.F);
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='>' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.I:=A.I shr B.I;
        end;
        'F':begin
         C:=power(2,B.F);
         if C<>0 then begin
          A.F:=A.I/C;
         end else begin
          A.F:=0;
         end;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         C:=power(2,B.I);
         if C<>0 then begin
          A.F:=A.F/C;
         end else begin
          A.F:=0;
         end;
        end;
        'F':begin
         C:=power(2,B.F);
         if C<>0 then begin
          A.F:=A.F/C;
         end else begin
          A.F:=0;
         end;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else begin
     Error.AbortCode(504);
    end;
    ParseOperation;
   end;
  end;
  result:=A;
 end;

 function ParseAdd:TValue;
 var Operation:ansichar;

  procedure ParseOperation;
  begin
   SkipSpaces;
   Operation:=#0;
   if CurrentChar=ord('+') then begin
    Operation:='+';
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('-') then begin
    Operation:='-';
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('^') then begin
    Operation:='^';
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('&') then begin
    Operation:='&';
    ReadChar;
    if CurrentChar=ord('&') then begin
     ReadChar;
    end;
    SkipSpaces;
   end else if CurrentChar=ord('|') then begin
    Operation:='|';
    ReadChar;
    if CurrentChar=ord('|') then begin
     ReadChar;
    end;
    SkipSpaces;
   end else if ((CurrentChar=ord('a')) or (CurrentChar=ord('A'))) and
               ((PeekChar(0)=ord('d')) or (PeekChar(0)=ord('D'))) and
               ((PeekChar(1)=ord('d')) or (PeekChar(1)=ord('D'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='+';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('s')) or (CurrentChar=ord('S'))) and
               ((PeekChar(0)=ord('u')) or (PeekChar(0)=ord('U'))) and
               ((PeekChar(1)=ord('b')) or (PeekChar(1)=ord('B'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='-';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('x')) or (CurrentChar=ord('X'))) and
               ((PeekChar(0)=ord('o')) or (PeekChar(0)=ord('O'))) and
               ((PeekChar(1)=ord('r')) or (PeekChar(1)=ord('R'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='^';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('a')) or (CurrentChar=ord('A'))) and
               ((PeekChar(0)=ord('n')) or (PeekChar(0)=ord('N'))) and
               ((PeekChar(1)=ord('d')) or (PeekChar(1)=ord('D'))) and
               ((PeekChar(2)>0) or (PeekChar(2)<=32)) then begin
    Operation:='&';
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('o')) or (CurrentChar=ord('O'))) and
               ((PeekChar(0)=ord('r')) or (PeekChar(0)=ord('R'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='|';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('a')) or (CurrentChar=ord('A'))) and
               ((PeekChar(0)=ord('p')) or (PeekChar(0)=ord('P'))) and
               ((PeekChar(1)=ord('p')) or (PeekChar(1)=ord('P'))) and
               ((PeekChar(2)=ord('e')) or (PeekChar(2)=ord('E'))) and
               ((PeekChar(3)=ord('n')) or (PeekChar(3)=ord('N'))) and
               ((PeekChar(4)=ord('d')) or (PeekChar(4)=ord('D'))) and
               ((PeekChar(5)>0) or (PeekChar(5)<=32)) then begin
    Operation:='#';
    ReadChar;
    ReadChar;
    ReadChar;
    ReadChar;
    ReadChar;
    ReadChar;
    SkipSpaces;
   end;
  end;

 var A,B:TValue;
 begin
  A:=ParseMul;
  if not (CurrentCommentEOF or Error.DoAbort or Error.Errors) then begin
   ParseOperation;
   while Operation<>#0 do begin
    B:=ParseMul;
    if Operation='+' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.I:=A.I+B.I;
        end;
        'F':begin
         A.F:=A.I+B.F;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=A.F+B.I;
        end;
        'F':begin
         A.F:=A.F+B.F;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.S:=HugeStringConcat(A.S,B.S);
        end;
        else begin
         A.S:=nil;
        end;
       end;
      end;
     end;
    end else if Operation='-' then begin
     case A.T of
      'B':begin
       A.B:=false;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.I:=A.I-B.I;
        end;
        'F':begin
         A.F:=A.I-B.F;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=A.F-B.I;
        end;
        'F':begin
         A.F:=A.F-B.F;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='^' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B xor B.B;
        end;
        'I':begin
         A.B:=A.B xor boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0) xor B.B;
         A.T:='B';
        end;
        'I':begin
         A.I:=A.I xor B.I;
        end;
        'F':begin
         A.F:=0;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=0;
        end;
        'F':begin
         A.F:=0;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='&' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B and B.B;
        end;
        'I':begin
         A.B:=A.B and boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0) and B.B;
         A.T:='B';
        end;
        'I':begin
         A.I:=A.I and B.I;
        end;
        'F':begin
         A.F:=0;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=0;
        end;
        'F':begin
         A.F:=0;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='|' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B or B.B;
        end;
        'I':begin
         A.B:=A.B or boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0) or B.B;
         A.T:='B';
        end;
        'I':begin
         A.I:=A.I or B.I;
        end;
        'F':begin
         A.F:=0;
         A.T:='F';
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=false;
        end;
        'I':begin
         A.F:=0;
        end;
        'F':begin
         A.F:=0;
        end;
        'S':begin
         A.S:=nil;
        end;
       end;
      end;
      'S':begin
       A.S:=nil;
      end;
     end;
    end else if Operation='#' then begin
     case A.T of
      'B':begin
       A.S:=nil;
       A.T:='S';
      end;
      'I':begin
       A.S:=nil;
       A.T:='S';
      end;
      'F':begin
       A.S:=nil;
       A.T:='S';
      end;
      'S':begin
       case B.T of
        'S':begin
         A.S:=HugeStringConcat(A.S,B.S);
        end;
        else begin
         A.S:=nil;
        end;
       end;
      end;
     end;
    end else begin
     Error.AbortCode(504);
    end;
    ParseOperation;
   end;
  end;
  result:=A;
 end;

 function ParseCompare:TValue;
 var Operation:string[2];

  procedure ParseOperation;
  begin
   SkipSpaces;
   Operation:='';
   if (CurrentChar=ord('>')) and (PeekChar(0)=ord('=')) then begin
    Operation:='>=';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if (CurrentChar=ord('<')) and (PeekChar(0)=ord('=')) then begin
    Operation:='<=';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('<')) and (PeekChar(0)=ord('>'))) or ((CurrentChar=ord('!')) and (PeekChar(0)=ord('='))) then begin
    Operation:='<>';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('=') then begin
    Operation:='=';
    ReadChar;
    if CurrentChar=ord('=') then begin
     ReadChar;
    end;
    SkipSpaces;
   end else if CurrentChar=ord('>') then begin
    Operation:='>';
    ReadChar;
    SkipSpaces;
   end else if CurrentChar=ord('<') then begin
    Operation:='<';
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('e')) or (CurrentChar=ord('E'))) and
               ((PeekChar(0)=ord('q')) or (PeekChar(0)=ord('Q'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='=';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('n')) or (CurrentChar=ord('N'))) and
               ((PeekChar(0)=ord('e')) or (PeekChar(0)=ord('E'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='<>';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('g')) or (CurrentChar=ord('G'))) and
               ((PeekChar(0)=ord('r')) or (PeekChar(0)=ord('R'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='>';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('g')) or (CurrentChar=ord('G'))) and
               ((PeekChar(0)=ord('e')) or (PeekChar(0)=ord('E'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='>=';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('l')) or (CurrentChar=ord('L'))) and
               ((PeekChar(0)=ord('s')) or (PeekChar(0)=ord('S'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='<';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('l')) or (CurrentChar=ord('L'))) and
               ((PeekChar(0)=ord('t')) or (PeekChar(0)=ord('T'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='<';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end else if ((CurrentChar=ord('l')) or (CurrentChar=ord('L'))) and
               ((PeekChar(0)=ord('e')) or (PeekChar(0)=ord('E'))) and
               ((PeekChar(1)>0) or (PeekChar(1)<=32)) then begin
    Operation:='<=';
    ReadChar;
    ReadChar;
    SkipSpaces;
   end;
  end;

 var A,B:TValue;
 begin
  A:=ParseAdd;
  if not (CurrentCommentEOF or Error.DoAbort or Error.Errors) then begin
   ParseOperation;
   while length(Operation)<>0 do begin
    B:=ParseAdd;
    if Operation='=' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B=B.B;
        end;
        'I':begin
         A.B:=A.B=boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)=0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else if Operation='<>' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B<>B.B;
        end;
        'I':begin
         A.B:=A.B<>boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)<>B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I<>B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I<>B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)<>B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F<>B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F<>B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)<>0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else if Operation='<' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B<B.B;
        end;
        'I':begin
         A.B:=A.B<boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)<B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I<B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I<B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)<B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F<B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F<B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)<0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else if Operation='>' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B>B.B;
        end;
        'I':begin
         A.B:=A.B>boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)>B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I>B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I>B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)>B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F>B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F>B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)>0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else if Operation='<=' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B<=B.B;
        end;
        'I':begin
         A.B:=A.B<=boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)<=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I<=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I<=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)<=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F<=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F<=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)<=0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else if Operation='>=' then begin
     case A.T of
      'B':begin
       case B.T of
        'B':begin
         A.B:=A.B>=B.B;
        end;
        'I':begin
         A.B:=A.B>=boolean(B.I);
        end;
        else begin
         A.B:=false;
        end;
       end;
      end;
      'I':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.I<>0)>=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.I>=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.I>=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'F':begin
       case B.T of
        'B':begin
         A.B:=boolean(A.F<>0)>=B.B;
         A.T:='B';
        end;
        'I':begin
         A.B:=A.F>=B.I;
         A.T:='B';
        end;
        'F':begin
         A.B:=A.F>=B.F;
         A.T:='B';
        end;
        'S':begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
      'S':begin
       case B.T of
        'S':begin
         A.B:=HugeStringCompare(A.S,B.S)>=0;
         A.T:='B';
        end;
        else begin
         A.B:=false;
         A.T:='B';
        end;
       end;
      end;
     end;
    end else begin
     Error.AbortCode(504);
    end;
    ParseOperation;
   end;
  end;
  result:=A;
 end;

begin
 if CurrentCommentEOF or (CurrentCommentPosition>=length(CurrentComment)) then begin
  TermSyntaxError;
  result:=false;
 end else begin
  result:=ParseCompare.B;
 end;
end;

procedure TPreprocessor.ProcessComment(const Comment:THugeString);
var DirectiveName,DefineName,IncludeFileName,Parameter:ansistring;
    Link:TBeRoStringTreeLink;
    LastDirective:TPreprocessorDirectives;
    Directive:TPreprocessorDirectives;
    Condition,ParentCondition:boolean;
    Value:int64;
    Index,Code:longint;
begin
 CurrentComment:=Comment;
 CurrentCommentPosition:=0;
 CurrentCommentEOF:=length(CurrentComment)=0;
 ReadChar;
 Condition:=false;                       
 while not ((CurrentChar=0) or CurrentCommentEOF or Error.DoAbort) do begin
  DirectiveName:='';
  while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
   DirectiveName:=DirectiveName+UpCase(ansichar(byte(CurrentChar)));
   ReadChar;
  end;
  while (CurrentChar>0) and (CurrentChar<=32) do begin
   ReadChar;
  end;
  if DirectiveStringTree.Find(DirectiveName,Link) then begin
   Directive:=TPreprocessorDirectives(Link);
   case Directive of
    tdIF:begin
     PushStack(tdIF,tdNONE,ProcessExpression,true);
     break;
    end;
    tdIFDEF:begin
     while (CurrentChar>0) and (CurrentChar<=32) do begin
      ReadChar;
     end;
     DefineName:='';
     while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
      DefineName:=DefineName+UpCase(ansichar(byte(CurrentChar)));
      ReadChar;
     end;
     PushStack(tdIFDEF,tdNONE,assigned(DefineStringHashMap.Get(DefineName)),true);
     break;
    end;
    tdIFEND:begin
     if assigned(Stack) then begin
      if Stack^.Directive in [tdIF,tdIFDEF,tdIFNDEF,tdIFOPT] then begin
       LastDirective:=Stack^.Directive;
      end else begin
       LastDirective:=Stack^.ParentDirective;
      end;
      if LastDirective<>tdIF then begin
       Error.AbortCode(59,DirectiveName);
      end else begin
       PopStack;
      end;
     end else begin
      Error.AbortCode(59,DirectiveName);
     end;
    end;
    tdIFNDEF:begin
     while (CurrentChar>0) and (CurrentChar<=32) do begin
      ReadChar;
     end;
     DefineName:='';
     while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
      DefineName:=DefineName+UpCase(ansichar(byte(CurrentChar)));
      ReadChar;
     end;
     PushStack(tdIFNDEF,tdNONE,assigned(DefineStringHashMap.Get(DefineName)),true);
     break;
    end;
    tdIFOPT:begin
     while (CurrentChar>0) and (CurrentChar<=32) do begin
      ReadChar;
     end;
     if (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z']) then begin
      case UpCase(ansichar(byte(CurrentChar))) of
       'A':begin
        Condition:=LocalSwitches^.Alignment<>1;
       end;
       'B':begin
        Condition:=LocalSwitches^.BoolEval;
       end;
       'C':begin
        Condition:=LocalSwitches^.Assertions;
       end;
       'D':begin
        Condition:=GlobalSwitches^.DebugInfo;
       end;
       'G':begin
        Condition:=LocalSwitches^.ImportedData;
       end;
       'L':begin
        Condition:=GlobalSwitches^.LocalSymbols;
       end;
       'H':begin
        Condition:=LocalSwitches^.LongStrings;
       end;
       'J':begin
        Condition:=LocalSwitches^.WriteableConst;
       end;
       'M':begin
        Condition:=LocalSwitches^.TypeInfo;
       end;
       'P':begin
        Condition:=LocalSwitches^.OpenStrings;
       end;
       'O':begin
        Condition:=LocalSwitches^.Optimization;
       end;
       'Q':begin
        Condition:=LocalSwitches^.OverflowChecks;
       end;
       'U':begin
        Condition:=LocalSwitches^.SafeDivide;
       end;
       'R':begin
        Condition:=LocalSwitches^.RangeChecks;
       end;
       'T':begin
        Condition:=GlobalSwitches^.TypedAddress;
       end;
       'V':begin
        Condition:=LocalSwitches^.VarStringChecks;
       end;
       'W':begin
        Condition:=LocalSwitches^.StackFrames;
       end;
       'X':begin
        Condition:=GlobalSwitches^.ExtendedSyntax;
       end;
       'Y':begin
        Condition:=GlobalSwitches^.ReferenceInfo and GlobalSwitches^.DefinitionInfo;
       end;
       'Z':begin
        Condition:=LocalSwitches^.MinEnumSize<>1;
       end;
       else begin
        Error.AbortCode(59,DirectiveName);
        break;
       end;
      end;
      ReadChar;
      if (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['+','-']) then begin
       if ansichar(byte(CurrentChar))='-' then begin
        Condition:=not Condition;
       end;
       ReadChar;
       PushStack(tdIFOPT,tdNONE,Condition,true);
      end else begin
       Error.AbortCode(59,DirectiveName);
      end;
     end else begin
      Error.AbortCode(59,DirectiveName);
     end;
     break;
    end;
    tdELSE:begin
     if assigned(Stack) then begin
      if Stack^.Directive in [tdIF,tdIFDEF,tdIFNDEF,tdIFOPT] then begin
       LastDirective:=Stack^.Directive;
      end else begin
       LastDirective:=Stack^.ParentDirective;
      end;
      ParentCondition:=Stack^.ParentCondition;
      if ParentCondition then begin
       Condition:=not Stack^.Condition;
       PopStack;
       PushStack(tdELSE,LastDirective,Condition,false);
      end else begin
       PopStack;
       PushStack(tdELSE,LastDirective,false,false);
      end;
     end else begin
      Error.AbortCode(59,DirectiveName);
     end;
    end;
    tdELSEIF:begin
     if assigned(Stack) then begin
      if Stack^.Directive in [tdIF,tdIFDEF,tdIFNDEF,tdIFOPT] then begin
       LastDirective:=Stack^.Directive;
      end else begin
       LastDirective:=Stack^.ParentDirective;
      end;
      ParentCondition:=Stack^.ParentCondition;
      if ParentCondition then begin
       PopStack;
       Condition:=ProcessExpression;
       PushStack(tdELSE,LastDirective,Condition,not Condition);
      end else begin
       PopStack;
       PushStack(tdELSEIF,LastDirective,false,false);
      end;
     end else begin
      Error.AbortCode(59,DirectiveName);
     end;
     break;
    end;
    tdENDIF:begin
     if assigned(Stack) then begin
      if Stack^.Directive in [tdIF,tdIFDEF,tdIFNDEF,tdIFOPT] then begin
       LastDirective:=Stack^.Directive;
      end else begin
       LastDirective:=Stack^.ParentDirective;
      end;
      if not (LastDirective in [tdIF,tdIFDEF,tdIFNDEF,tdIFOPT]) then begin
       Error.AbortCode(59,DirectiveName);
      end else begin
       PopStack;
      end;
     end else begin
      Error.AbortCode(59,DirectiveName);
     end;
    end;
    else begin
     if CurrentCondition then begin
      case Directive of
       tdDEFINE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        DefineName:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         DefineName:=DefineName+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        AddDefine(DefineName);
       end;
       tdUNDEF:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        DefineName:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         DefineName:=DefineName+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        RemoveDefine(DefineName);
       end;
       tdI,tdINCLUDE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        if (Directive=tdI) and ((CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['-','+'])) then begin
         LocalSwitches^.IOChecks:=ansichar(byte(CurrentChar))='+';
         ReadChar;
        end else begin
         if (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['''','#','^']) then begin
          IncludeFileName:=HugeStringToAnsiString(ReadPascalString);
         end else begin
          IncludeFileName:='';
          while (CurrentChar<128) and not (ansichar(byte(CurrentChar)) in [#0..#32,',']) do begin
           IncludeFileName:=IncludeFileName+ansichar(byte(CurrentChar));
           ReadChar;
          end;
         end;
         FileStackOpenInclude(IncludeFileName);
        end;
       end;
       tdA:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.Alignment:=1;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.Alignment:=-1;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdALIGN:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.Alignment:=1;
        end else if Parameter='ON' then begin
         LocalSwitches^.Alignment:=-1;
        end else begin
         if SysUtils.TryStrToInt64(Parameter,Value) then begin
          if Value<1 then begin
           Value:=-1;
          end;
          LocalSwitches^.Alignment:=Value;
         end else begin
          Error.AbortCode(59,DirectiveName);
         end;
        end;
       end;
       tdA1:begin
        LocalSwitches^.Alignment:=1;
       end;
       tdA2:begin
        LocalSwitches^.Alignment:=2;
       end;
       tdA4:begin
        LocalSwitches^.Alignment:=4;
       end;
       tdA8:begin
        LocalSwitches^.Alignment:=8;
       end;
       tdAPPTYPE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='CONSOLE' then begin
         GlobalSwitches^.AppType:=tatCONSOLE;
        end else if Parameter='GUI' then begin
         GlobalSwitches^.AppType:=tatGUI;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdC:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.Assertions:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.Assertions:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdASSERTIONS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.Assertions:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.Assertions:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdB:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.BoolEval:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.BoolEval:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdBOOLEVAL:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.BoolEval:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.BoolEval:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdLIBPREFIX:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        if CurrentChar=ord('''') then begin
         Parameter:=HugeStringToAnsiString(ReadPascalString);
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
        if length(Parameter)>0 then begin
         GlobalSwitches^.LibPrefix:=Parameter;
        end;
       end;
       tdLIBSUBFIX:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        if CurrentChar=ord('''') then begin
         Parameter:=HugeStringToAnsiString(ReadPascalString);
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
        if length(Parameter)>0 then begin
         GlobalSwitches^.LibSubfix:=Parameter;
        end;
       end;
       tdCODEPAGE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         LocalSwitches^.CodePage:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdLIBVERSION:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        if CurrentChar=ord('''') then begin
         Parameter:=HugeStringToAnsiString(ReadPascalString);
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
        if length(Parameter)>0 then begin
         GlobalSwitches^.LibVersion:=Parameter;
        end;
       end;
       tdD:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         GlobalSwitches^.DebugInfo:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         GlobalSwitches^.DebugInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdDEBUGINFO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.DebugInfo:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.DebugInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdDENYPACKAGEUNIT:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.DenyPackageUnit:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.DenyPackageUnit:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdDESCRIPTION:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        if CurrentChar=ord('''') then begin
         Parameter:=HugeStringToAnsiString(ReadPascalString);
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
        if length(Parameter)>0 then begin
         GlobalSwitches^.Description:=Parameter;
        end;
       end;
       tdDESIGNINFO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.DesignOnly:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.DesignOnly:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdE,tdEXTENSION:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        GlobalSwitches^.Extension:=Parameter;
       end;
       tdOBJEXPORTALL:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.ObjExportAll:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.ObjExportAll:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdX:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         GlobalSwitches^.ExtendedSyntax:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         GlobalSwitches^.ExtendedSyntax:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdEXTENDEDSYNTAX:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.ExtendedSyntax:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.ExtendedSyntax:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdEXTERNALSYM:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
       end;
       tdHINTS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.Hints:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.Hints:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdHPPEMIT:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        if CurrentChar=ord('''') then begin
         Parameter:=HugeStringToAnsiString(ReadPascalString);
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
        if length(Parameter)>0 then begin
        end;
       end;
       tdIMAGEBASE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'f','A'..'F','0'..'9','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         GlobalSwitches^.ImageBase:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdIMPLICITBUILD:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.ImplicitBuild:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.ImplicitBuild:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdG:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.ImportedData:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.ImportedData:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdIMPORTEDDATA:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.ImportedData:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.ImportedData:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdIOCHECKS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.IOChecks:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.IOChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdL:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         GlobalSwitches^.LocalSymbols:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         GlobalSwitches^.LocalSymbols:=true;
        end else begin
         Parameter:='';
         while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','.','/','\','-']) do begin
          Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
          ReadChar;
         end;
        end;
       end;
       tdLINK:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','.','/','\','-']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
       end;
       tdLOCALSYMBOLS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.LocalSymbols:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.LocalSymbols:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdH:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.LongStrings:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.LongStrings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdLONGSTRINGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.LongStrings:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.LongStrings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdM:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.TypeInfo:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.TypeInfo:=true;
        end else begin
         while (CurrentChar>0) and (CurrentChar<=32) do begin
          ReadChar;
         end;
         Parameter:='';
         while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
          Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
          ReadChar;
         end;
         Val(Parameter,Value,Code);
         if Code=0 then begin
          GlobalSwitches^.MinStackSize:=Value;
         end else begin
          Error.AbortCode(59,DirectiveName);
         end;
         while (CurrentChar>0) and (CurrentChar<=32) do begin
          ReadChar;
         end;
         if CurrentChar=ord(',') then begin
          ReadChar;
          while (CurrentChar>0) and (CurrentChar<=32) do begin
           ReadChar;
          end;
         end else begin
          Error.AbortCode(59,DirectiveName);
         end;
         while (CurrentChar>0) and (CurrentChar<=32) do begin
          ReadChar;
         end;
         Parameter:='';
         while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
          Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
          ReadChar;
         end;
         Val(Parameter,Value,Code);
         if Code=0 then begin
          GlobalSwitches^.MaxStackSize:=Value;
         end else begin
          Error.AbortCode(59,DirectiveName);
         end;
        end;
       end;
       tdMINSTACKSIZE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         GlobalSwitches^.MinStackSize:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdMAXSTACKSIZE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         GlobalSwitches^.MaxStackSize:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdZ:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.MinEnumSize:=1;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.MinEnumSize:=4;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdZ1:begin
        LocalSwitches^.MinEnumSize:=1;
       end;
       tdZ2:begin
        LocalSwitches^.MinEnumSize:=2;
       end;
       tdZ4:begin
        LocalSwitches^.MinEnumSize:=4;
       end;
       tdMINENUMSIZE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         LocalSwitches^.MinEnumSize:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;                                 
       tdP:Begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.OpenStrings:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.OpenStrings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdOPENSTRINGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.OpenStrings:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.OpenStrings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.Optimization:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.Optimization:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdOPTIMIZATION:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.Optimization:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.Optimization:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdQ:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.OverflowChecks:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.OverflowChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdOVERFLOWCHECKS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.OverflowChecks:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.OverflowChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdSETPEFLAGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         LocalSwitches^.SetPEFlags:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdSETPEOPTFLAGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_','$']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        Val(Parameter,Value,Code);
        if Code=0 then begin
         LocalSwitches^.SetPEOptFlags:=Value;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdU:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.SafeDivide:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.SafeDivide:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdSAFEDIVIDE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.SafeDivide:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.SafeDivide:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdNODEFINE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and not (ansichar(byte(CurrentChar)) in [#0..#32,',']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
       end;
       tdNOINCLUDE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and not (ansichar(byte(CurrentChar)) in [#0..#32,',']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
       end;
       tdR:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.RangeChecks:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.RangeChecks:=true;
        end else begin
        end;
       end;
       tdRANGECHECKS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.RangeChecks:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.RangeChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdREALCOMPATIBILITY:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.RealCompatibility:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.RealCompatibility:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdRESOURCE:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and not (ansichar(byte(CurrentChar)) in [#0..#32,',']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
       end;
       tdRUNONLY:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.RunOnly:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.RunOnly:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdTYPEINFO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.TypeInfo:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.TypeInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdY:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         GlobalSwitches^.ReferenceInfo:=false;
         GlobalSwitches^.DefinitionInfo:=true;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         GlobalSwitches^.ReferenceInfo:=true;
         GlobalSwitches^.DefinitionInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdYD:begin
        GlobalSwitches^.DefinitionInfo:=true;
       end;
       tdREFERENCEINFO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.ReferenceInfo:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.ReferenceInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdDEFINITIONINFO:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.DefinitionInfo:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.DefinitionInfo:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdT:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         GlobalSwitches^.TypedAddress:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         GlobalSwitches^.TypedAddress:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdTYPEDADDRESS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         GlobalSwitches^.TypedAddress:=false;
        end else if Parameter='ON' then begin
         GlobalSwitches^.TypedAddress:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdUNICODESTRINGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.UnicodeStrings:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.UnicodeStrings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdV:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.VarStringChecks:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.VarStringChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdVARSTRINGCHECKS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.VarStringChecks:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.VarStringChecks:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdWARNINGS:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.Warnings:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.Warnings:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdWEAKPACKAGEUNIT:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.WeakPackageUnit:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.WeakPackageUnit:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdW:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.StackFrames:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.StackFrames:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdSTACKFRAMES:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.StackFrames:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.StackFrames:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdJ:begin
        if CurrentChar=ord('-') then begin
         ReadChar;
         LocalSwitches^.WriteableConst:=false;
        end else if CurrentChar=ord('+') then begin
         ReadChar;
         LocalSwitches^.WriteableConst:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       tdWRITEABLECONST:begin
        while (CurrentChar>0) and (CurrentChar<=32) do begin
         ReadChar;
        end;
        Parameter:='';
        while (CurrentChar<128) and (ansichar(byte(CurrentChar)) in ['a'..'z','A'..'Z','0'..'9','_']) do begin
         Parameter:=Parameter+UpCase(ansichar(byte(CurrentChar)));
         ReadChar;
        end;
        if Parameter='OFF' then begin
         LocalSwitches^.WriteableConst:=false;
        end else if Parameter='ON' then begin
         LocalSwitches^.WriteableConst:=true;
        end else begin
         Error.AbortCode(59,DirectiveName);
        end;
       end;
       else begin
        Error.AbortCode(59,DirectiveName);
       end;
      end;
     end;
    end;
   end;
  end else begin
   Error.AbortCode(59,DirectiveName);
  end;
  while (CurrentChar>0) and (CurrentChar<=32) do begin
   ReadChar;
  end;
  while (CurrentChar>0) and (CurrentChar<>ord(',')) do begin
   ReadChar;
  end;
  if CurrentChar=ord(',') then begin
   ReadChar;
  end else begin
   break;
  end;
  while (CurrentChar>0) and (CurrentChar<=32) do begin
   ReadChar;
  end;
 end;
end;

procedure TPreprocessor.AddDefine(const DefineString:ansistring);
var ToWork,DefineName:ansistring;
    index:longint;
begin
 ToWork:=DefineString;
 DefineName:=UpperCase(Parse(ToWork,[#0..#32],true));
 if length(ToWork)>0 then begin
  index:=DefineStringList.Find('');
  if index<0 then begin
   index:=DefineStringList.Add(ToWork);
  end else begin
   DefineStringList[index]:=ToWork;
  end;
  DefineStringHashMap.Add(DefineName,index+1);
 end else begin
  DefineStringHashMap.Add(DefineName,0);
 end;
end;

procedure TPreprocessor.RemoveDefine(const DefineString:ansistring);
var ToWork,DefineName:ansistring;
    StringHashMapEntity:PBeRoStringHashMapEntity;
    Index:longint;
begin
 ToWork:=DefineString;
 DefineName:=UpperCase(Parse(ToWork,[#0..#32],true));
 StringHashMapEntity:=DefineStringHashMap.Get(DefineName);
 if assigned(StringHashMapEntity) then begin
  Index:=StringHashMapEntity.Value;
  if Index>0 then begin
   DefineStringList[Index-1]:=ToWork;
  end;
  DefineStringHashMap.Delete(DefineString);
 end;
end;

end.



