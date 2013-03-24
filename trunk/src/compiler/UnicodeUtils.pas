unit UnicodeUtils;
{$i Compiler.inc}

interface

uses Unicode;

const ustNOUTF8=0;
      ustPOSSIBLEUTF8=1;
      ustISUTF8=2;

      usmcACCEPT=0;
      usmcERROR=16;

type TUTF8Chars=array[ansichar] of byte;

     TUTF8Bytes=array[byte] of byte;

{$ifdef StrictUTF8}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,1,1,1,1,1,1,1,1,1,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$else}
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f
const UTF8CharSteps:TUTF8Chars=(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 0
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 1
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 2
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 3
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 4
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 5
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 6
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 7
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 8
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // 9
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // a
                                1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,  // b
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // c
                                2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,  // d
                                3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,  // e
                                4,4,4,4,4,4,4,4,5,5,5,5,6,6,1,1); // f
                              //0 1 2 3 4 5 6 7 8 9 a b c d e f

{$endif}

var UTF8DFACharClasses:TUTF8Chars;
    UTF8DFATransitions:TUTF8Bytes;

function UnicodeGetCategoryFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
function UnicodeGetScriptFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
function UnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function UnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function UnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
function UnicodeIsWord(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function UnicodeIsIDBegin(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function UnicodeIsIDPart(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function UnicodeIsWhiteSpace(c:longword):boolean; {$ifdef caninline}inline;{$endif}
function UnicodeToUpper(c:longword):longword; {$ifdef caninline}inline;{$endif}
function UnicodeToLower(c:longword):longword; {$ifdef caninline}inline;{$endif}
function UnicodeToTitle(c:longword):longword; {$ifdef caninline}inline;{$endif}

function UTF32CharToUTF8(CharValue:longword):ansistring;
function UTF32CharToUTF8Len(CharValue:longword):longint;
function IsUTF8(const s:ansistring):boolean;
function ValidateUTF8(const s:ansistring):boolean;
function GetUTF8(const s:ansistring):longint;
procedure UTF8SafeInc(const s:ansistring;var CodeUnit:longint);
procedure UTF8Inc(const s:ansistring;var CodeUnit:longint);
procedure UTF8Dec(const s:ansistring;var CodeUnit:longint);
procedure UTF8Delete(var s:ansistring;CodeUnit:longint);
function UTF8Length(const s:ansistring):longint;
function UTF8LengthEx(const s:ansistring):longint;
function UTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
function UTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
function UTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
function UTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
function UTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
function UTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
function UTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
function UTF8GetCharLen(const s:ansistring;i:longint):longword;
function UTF8Pos(const FindStr,InStr:ansistring):longint;
function UTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
function UTF8Uppercase(const Str:ansistring):ansistring;
function UTF8Lowercase(const Str:ansistring):ansistring;
function UTF8Trim(const Str:ansistring):ansistring;
function UTF8Correct(const Str:ansistring):ansistring;
function UTF8FromLatin1(const Str:ansistring):ansistring;
function UTF8LevenshteinDistance(const s,t:ansistring):longint;
function UTF8DamerauLevenshteinDistance(const s,t:ansistring):longint;
function StringLength(const s:ansistring):longint;

function UTF8ToUTF16(const s:ansistring):widestring;
function UTF16ToUTF8(const s:widestring):ansistring;

implementation

function UnicodeGetCategoryFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr UnicodeCategoryArrayBlockBits;
  result:=UnicodeCategoryArrayBlockData[UnicodeCategoryArrayIndexBlockData[UnicodeCategoryArrayIndexIndexData[Index shr UnicodeCategoryArrayIndexBlockBits],Index and UnicodeCategoryArrayIndexBlockMask],c and UnicodeCategoryArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetScriptFromTable(c:longword):longword; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr UnicodeScriptArrayBlockBits;
  result:=UnicodeScriptArrayBlockData[UnicodeScriptArrayIndexBlockData[UnicodeScriptArrayIndexIndexData[Index shr UnicodeScriptArrayIndexBlockBits],Index and UnicodeScriptArrayIndexBlockMask],c and UnicodeScriptArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetUpperCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr UnicodeUpperCaseDeltaArrayBlockBits;
  result:=UnicodeUpperCaseDeltaArrayBlockData[UnicodeUpperCaseDeltaArrayIndexBlockData[UnicodeUpperCaseDeltaArrayIndexIndexData[Index shr UnicodeUpperCaseDeltaArrayIndexBlockBits],Index and UnicodeUpperCaseDeltaArrayIndexBlockMask],c and UnicodeUpperCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetLowerCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr UnicodeLowerCaseDeltaArrayBlockBits;
  result:=UnicodeLowerCaseDeltaArrayBlockData[UnicodeLowerCaseDeltaArrayIndexBlockData[UnicodeLowerCaseDeltaArrayIndexIndexData[Index shr UnicodeLowerCaseDeltaArrayIndexBlockBits],Index and UnicodeLowerCaseDeltaArrayIndexBlockMask],c and UnicodeLowerCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeGetTitleCaseDeltaFromTable(c:longword):longint; {$ifdef caninline}inline;{$endif}
var Index:longword;
begin
 if c<=$10ffff then begin
  Index:=c shr UnicodeTitleCaseDeltaArrayBlockBits;
  result:=UnicodeTitleCaseDeltaArrayBlockData[UnicodeTitleCaseDeltaArrayIndexBlockData[UnicodeTitleCaseDeltaArrayIndexIndexData[Index shr UnicodeTitleCaseDeltaArrayIndexBlockBits],Index and UnicodeTitleCaseDeltaArrayIndexBlockMask],c and UnicodeTitleCaseDeltaArrayBlockMask];
 end else begin
  result:=0;
 end;
end;

function UnicodeIsWord(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [UnicodeCategoryLu,UnicodeCategoryLl,UnicodeCategoryLt,UnicodeCategoryLm,UnicodeCategoryLo,UnicodeCategoryNd,UnicodeCategoryNl,UnicodeCategoryNo,UnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsIDBegin(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [UnicodeCategoryLu,UnicodeCategoryLl,UnicodeCategoryLt,UnicodeCategoryLm,UnicodeCategoryLo,UnicodeCategoryNl,UnicodeCategoryNo,UnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsIDPart(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(UnicodeGetCategoryFromTable(c) in [UnicodeCategoryLu,UnicodeCategoryLl,UnicodeCategoryLt,UnicodeCategoryLm,UnicodeCategoryLo,UnicodeCategoryNd,UnicodeCategoryNl,UnicodeCategoryNo,UnicodeCategoryPc]) or (c=ord('_'));
end;

function UnicodeIsWhiteSpace(c:longword):boolean; {$ifdef caninline}inline;{$endif}
begin
//result:=UnicodeGetCategoryFromTable(c) in [UnicodeCategoryZs,UnicodeCategoryZp,UnicodeCategoryZl];
 result:=((c>=$0009) and (c<=$000d)) or (c=$0020) or (c=$00a0) or (c=$1680) or (c=$180e) or ((c>=$2000) and (c<=$200b)) or (c=$2028) or (c=$2029) or (c=$202f) or (c=$205f) or (c=$3000) or (c=$feff) or (c=$fffe);
end;

function UnicodeToUpper(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetUpperCaseDeltaFromTable(c)));
end;

function UnicodeToLower(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetLowerCaseDeltaFromTable(c)));
end;

function UnicodeToTitle(c:longword):longword; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longint(longint(c)+UnicodeGetTitleCaseDeltaFromTable(c)));
end;

function UTF32CharToUTF8(CharValue:longword):ansistring;
var Data:array[0..{$ifdef strictutf8}3{$else}5{$endif}] of ansichar;
    ResultLen:longint;
begin
 if CharValue=0 then begin
  result:=#0;
 end else begin
  if CharValue<=$7f then begin
   Data[0]:=ansichar(byte(CharValue));
   ResultLen:=1;
  end else if CharValue<=$7ff then begin
   Data[0]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
   Data[1]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=2;
{$ifdef strictutf8}
  end else if CharValue<=$d7ff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$dfff then begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
{$endif}
  end else if CharValue<=$ffff then begin
   Data[0]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[2]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=3;
  end else if CharValue<=$1fffff then begin
   Data[0]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[3]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=4;
{$ifndef strictutf8}
  end else if CharValue<=$3ffffff then begin
   Data[0]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[4]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=5;
  end else if CharValue<=$7fffffff then begin
   Data[0]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
   Data[1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
   Data[2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
   Data[3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
   Data[4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
   Data[5]:=ansichar(byte($80 or (CharValue and $3f)));
   ResultLen:=6;
{$endif}
  end else begin
   Data[0]:=#$ef; // $fffd
   Data[1]:=#$bf;
   Data[2]:=#$bd;
   ResultLen:=3;
  end;
  SetString(result,pansichar(@Data[0]),ResultLen);
 end;
end;

function UTF32CharToUTF8Len(CharValue:longword):longint;
begin
 if CharValue<=$7f then begin
  result:=1;
 end else if CharValue<=$7ff then begin
  result:=2;
 end else if CharValue<=$ffff then begin
  result:=3;
 end else if CharValue<=$1fffff then begin
  result:=4;
{$ifndef strictutf8}
 end else if CharValue<=$3ffffff then begin
  result:=5;
 end else if CharValue<=$7fffffff then begin
  result:=6;
{$endif}
 end else begin
  result:=3;
 end;
end;

function IsUTF8(const s:ansistring):boolean;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(CodePoints);
   end;
   usmcERROR:begin
    result:=false;
    exit;
   end;
  end;
 end;
 result:=(State=usmcACCEPT) and (length(s)<>CodePoints);
end;

function ValidateUTF8(const s:ansistring):boolean;
var CodeUnit:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  if State=usmcERROR then begin
   result:=false;
   exit;
  end;
 end;
 result:=State=usmcACCEPT;
end;

function GetUTF8(const s:ansistring):longint;
var CodeUnit,CodePoints:longint;
    State:longword;
begin
 State:=usmcACCEPT;
 CodePoints:=0;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(CodePoints);
   end;
   usmcERROR:begin
    result:=ustNOUTF8;
    exit;
   end;
  end;
 end;
 if State=usmcACCEPT then begin
  if length(s)<>CodePoints then begin
   result:=ustISUTF8;
  end else begin
   result:=ustPOSSIBLEUTF8;
  end;
 end else begin
  result:=ustNOUTF8;
 end;
end;

procedure UTF8SafeInc(const s:ansistring;var CodeUnit:longint);
var Len:longint;
    StartCodeUnit,State:longword;
begin
 Len:=length(s);
 if CodeUnit>0 then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
   inc(CodeUnit);
   if State<=usmcERROR then begin
    break;
   end;
  end;
  if State<>usmcACCEPT then begin
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

procedure UTF8Inc(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  inc(CodeUnit,UTF8CharSteps[s[CodeUnit]]);
 end;
end;

procedure UTF8Dec(const s:ansistring;var CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=(length(s)+1)) then begin
  dec(CodeUnit);
  while CodeUnit>0 do begin
   if s[CodeUnit] in [#$80..#$bf] then begin
    dec(CodeUnit);
   end else begin
    break;
   end;
  end;
 end;
end;

procedure UTF8Delete(var s:ansistring;CodeUnit:longint);
begin
 if (CodeUnit>=1) and (CodeUnit<=length(s)) then begin
  Delete(s,CodeUnit,1);
  while ((CodeUnit>=1) and (CodeUnit<=length(s))) and (s[CodeUnit] in [#$80..#$bf]) do begin
   Delete(s,CodeUnit,1);
  end;
 end;
end;

function UTF8Length(const s:ansistring):longint;
{$ifdef cpu386} assembler; register;
asm
 test eax,eax
 jz @End
  push esi
   cld
   mov esi,eax
   mov ecx,dword ptr [esi-4]
   xor edx,edx
   jecxz @LoopEnd
    @Loop:
      lodsb
      shl al,1
      js @IsASCIICharOrUTF8Begin
      jc @IsUTF8Part
      @IsASCIICharOrUTF8Begin:
       inc edx
      @IsUTF8Part:
     dec ecx
    jnz @Loop
   @LoopEnd:
   mov eax,edx
  pop esi
 @End:
end;
{$else}
var CodeUnit:longint;
begin
 result:=0;
 for CodeUnit:=1 to length(s) do begin
  if (byte(s[CodeUnit]) and $c0)<>$80 then begin
   inc(result);
  end;
 end;
end;
{$endif}

function UTF8LengthEx(const s:ansistring):longint;
var State:longword;
    CodeUnit:longint;
begin
 result:=0;
 State:=usmcACCEPT;
 for CodeUnit:=1 to length(s) do begin
  State:=UTF8DFATransitions[State+UTF8DFACharClasses[s[CodeUnit]]];
  case State of
   usmcACCEPT:begin
    inc(result);
   end;
   usmcERROR:begin
    result:=0;
    exit;
   end;
  end;
 end;
 if State=usmcERROR then begin
  result:=0;
 end;
end;

function UTF8GetCodePoint(const s:ansistring;CodeUnit:longint):longint;
var CurrentCodeUnit,Len:longint;
begin
 if CodeUnit<1 then begin
  result:=-1;
 end else begin
  result:=0;
  CurrentCodeUnit:=1;
  Len:=length(s);
  while (CurrentCodeUnit<=Len) and (CurrentCodeUnit<>CodeUnit) do begin
   inc(result);
   inc(CurrentCodeUnit,UTF8CharSteps[s[CurrentCodeUnit]]);
  end;
 end;
end;

function UTF8GetCodeUnit(const s:ansistring;CodePoint:longint):longint;
var CurrentCodePoint,Len:longint;
begin
 if CodePoint<0 then begin
  result:=0;
 end else begin
  result:=1;
  CurrentCodePoint:=0;
  Len:=length(s);
  while (result<=Len) and (CurrentCodePoint<>CodePoint) do begin
   inc(CurrentCodePoint);
   inc(result,UTF8CharSteps[s[result]]);
  end;
 end;
end;

function UTF8CodeUnitGetChar(const s:ansistring;CodeUnit:longint):longword;
var Value,CharClass,State:longword;
begin
 result:=0;
 if (CodeUnit>0) and (CodeUnit<=length(s)) then begin
  State:=usmcACCEPT;
  for CodeUnit:=CodeUnit to length(s) do begin
   Value:=byte(ansichar(s[CodeUnit]));
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
  if State<>usmcACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharAndInc(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
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
  if State<>usmcACCEPT then begin
   result:=$fffd;
  end;
 end;
end;

function UTF8CodeUnitGetCharFallback(const s:ansistring;CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
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
  if State<>usmcACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
  end;
 end;
end;

function UTF8CodeUnitGetCharAndIncFallback(const s:ansistring;var CodeUnit:longint):longword;
var Len:longint;
    StartCodeUnit,Value,CharClass,State:longword;
begin
 result:=0;
 Len:=length(s);
 if (CodeUnit>0) and (CodeUnit<=Len) then begin
  StartCodeUnit:=CodeUnit;
  State:=usmcACCEPT;
  while CodeUnit<=Len do begin
   Value:=byte(ansichar(s[CodeUnit]));
   inc(CodeUnit);
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
  if State<>usmcACCEPT then begin
   result:=byte(ansichar(s[StartCodeUnit]));
   CodeUnit:=StartCodeUnit+1;
  end;
 end;
end;

function UTF8CodePointGetChar(const s:ansistring;CodePoint:longint;Fallback:boolean=false):longword;
begin
 result:=UTF8CodeUnitGetChar(s,UTF8GetCodeUnit(s,CodePoint));
end;

function UTF8GetCharLen(const s:ansistring;i:longint):longword;
begin
 if (i>0) and (i<=length(s)) then begin
  result:=UTF8CharSteps[s[i]];
 end else begin
  result:=0;
 end;
end;

function UTF8Pos(const FindStr,InStr:ansistring):longint;
var i,j,l:longint;
    ok:boolean;
begin
 result:=0;
 i:=1;
 while i<=length(InStr) do begin
  l:=i+length(FindStr)-1;
  if l>length(InStr) then begin
   exit;
  end;
  ok:=true;
  for j:=1 to length(FindStr) do begin
   if InStr[i+j-1]<>FindStr[j] then begin
    ok:=false;
    break;
   end;
  end;
  if ok then begin
   result:=i;
   exit;
  end;
  inc(i,UTF8CharSteps[InStr[i]]);
 end;
end;

function UTF8Copy(const Str:ansistring;Start,Len:longint):ansistring;
var CodeUnit:longint;
begin
 result:='';
 CodeUnit:=1;
 while (CodeUnit<=length(Str)) and (Start>0) do begin
  inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
  dec(Start);
 end;
 if Start=0 then begin
  Start:=CodeUnit;
  while (CodeUnit<=length(Str)) and (Len>0) do begin
   inc(CodeUnit,UTF8CharSteps[Str[CodeUnit]]);
   dec(Len);
  end;
  if Start<CodeUnit then begin
   result:=copy(Str,Start,CodeUnit-Start);
  end;
 end;
end;

function UTF8Uppercase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef strictutf8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=usmcACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=usmcACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=usmcERROR then begin
     break;
    end;
   end;
   if State<>usmcACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr UnicodeUpperCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+UnicodeUpperCaseDeltaArrayBlockData[UnicodeUpperCaseDeltaArrayIndexBlockData[UnicodeUpperCaseDeltaArrayIndexIndexData[Value shr UnicodeUpperCaseDeltaArrayIndexBlockBits],Value and UnicodeUpperCaseDeltaArrayIndexBlockMask],CharValue and UnicodeUpperCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef strictutf8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef strictutf8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8Lowercase(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 result:='';
 CodeUnit:=1;
 Len:=length(Str);
 if Len>0 then begin
  SetLength(result,Len*{$ifdef strictutf8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=usmcACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=usmcACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=usmcERROR then begin
     break;
    end;
   end;
   if State<>usmcACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$10ffff then begin
    Value:=CharValue shr UnicodeLowerCaseDeltaArrayBlockBits;
    CharValue:=longword(longint(longint(CharValue)+UnicodeLowerCaseDeltaArrayBlockData[UnicodeLowerCaseDeltaArrayIndexBlockData[UnicodeLowerCaseDeltaArrayIndexIndexData[Value shr UnicodeLowerCaseDeltaArrayIndexBlockBits],Value and UnicodeLowerCaseDeltaArrayIndexBlockMask],CharValue and UnicodeLowerCaseDeltaArrayBlockMask]));
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef strictutf8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef strictutf8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8Trim(const Str:ansistring):ansistring;
var i,j:longint;
begin
 i:=1;
 while UnicodeIsWhiteSpace(UTF8CodeUnitGetChar(Str,i)) do begin
  inc(i,UTF8CharSteps[Str[i]]);
 end;
 j:=length(Str)+1;
 UTF8Dec(Str,j);
 while UnicodeIsWhiteSpace(UTF8CodeUnitGetChar(Str,j)) do begin
  UTF8Dec(Str,j);
 end;
 if (j<=length(Str)) and (Str[j]>=#80) then begin
  inc(j,longint(UTF8GetCharLen(Str,j))-1);
 end;
 if i<=j then begin
  result:=copy(Str,i,(j-i)+1);
 end else begin
  result:='';
 end;
end;

function UTF8Correct(const Str:ansistring):ansistring;
var CodeUnit,Len,ResultLen:longint;
    StartCodeUnit,Value,CharClass,State,CharValue:longword;
    Data:pansichar;
begin
 if (length(Str)=0) or ValidateUTF8(Str) then begin
  result:=Str;
 end else begin
  result:='';
  CodeUnit:=1;
  Len:=length(Str);
  SetLength(result,Len*{$ifdef strictutf8}4{$else}6{$endif});
  Data:=@result[1];
  ResultLen:=0;
  while CodeUnit<=Len do begin
   StartCodeUnit:=CodeUnit;
   State:=usmcACCEPT;
   CharValue:=0;
   while CodeUnit<=Len do begin
    Value:=byte(ansichar(Str[CodeUnit]));
    inc(CodeUnit);
    CharClass:=UTF8DFACharClasses[ansichar(Value)];
    if State=usmcACCEPT then begin
     CharValue:=Value and ($ff shr CharClass);
    end else begin
     CharValue:=(CharValue shl 6) or (Value and $3f);
    end;
    State:=UTF8DFATransitions[State+CharClass];
    if State<=usmcERROR then begin
     break;
    end;
   end;
   if State<>usmcACCEPT then begin
    CharValue:=byte(ansichar(Str[StartCodeUnit]));
    CodeUnit:=StartCodeUnit+1;
   end;
   if CharValue<=$7f then begin
    Data[ResultLen]:=ansichar(byte(CharValue));
    inc(ResultLen);
   end else if CharValue<=$7ff then begin
    Data[ResultLen]:=ansichar(byte($c0 or ((CharValue shr 6) and $1f)));
    Data[ResultLen+1]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,2);
{$ifdef strictutf8}
   end else if CharValue<=$d7ff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$dfff then begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
{$endif}
   end else if CharValue<=$ffff then begin
    Data[ResultLen]:=ansichar(byte($e0 or ((CharValue shr 12) and $0f)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,3);
   end else if CharValue<=$1fffff then begin
    Data[ResultLen]:=ansichar(byte($f0 or ((CharValue shr 18) and $07)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,4);
{$ifndef strictutf8}
   end else if CharValue<=$3ffffff then begin
    Data[ResultLen]:=ansichar(byte($f8 or ((CharValue shr 24) and $03)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,5);            
   end else if CharValue<=$7fffffff then begin
    Data[ResultLen]:=ansichar(byte($fc or ((CharValue shr 30) and $01)));
    Data[ResultLen+1]:=ansichar(byte($80 or ((CharValue shr 24) and $3f)));
    Data[ResultLen+2]:=ansichar(byte($80 or ((CharValue shr 18) and $3f)));
    Data[ResultLen+3]:=ansichar(byte($80 or ((CharValue shr 12) and $3f)));
    Data[ResultLen+4]:=ansichar(byte($80 or ((CharValue shr 6) and $3f)));
    Data[ResultLen+5]:=ansichar(byte($80 or (CharValue and $3f)));
    inc(ResultLen,6);
{$endif}
   end else begin
    Data[ResultLen]:=#$ef; // $fffd
    Data[ResultLen+1]:=#$bf;
    Data[ResultLen+2]:=#$bd;
    inc(ResultLen,3);
   end;
  end;
  SetLength(result,ResultLen);
 end;
end;

function UTF8FromLatin1(const Str:ansistring):ansistring;
var CodeUnit:longint;
begin
 if ValidateUTF8(Str) then begin
  result:=Str;
 end else begin
  result:='';
  for CodeUnit:=1 to length(Str) do begin
   result:=result+UTF32CharToUTF8(byte(ansichar(Str[CodeUnit])));
  end;
 end;
end;

function UTF8LevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Deletion,Insertion,Substitution:longint;
    si,tj:longword;
begin
 n:=UTF8LengthEx(s);
 m:=UTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,oi)=UTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  UTF8Dec(s,ci);
  UTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,ci)=UTF8CodeUnitGetChar(t,cj)) do begin
   UTF8Dec(s,ci);
   UTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  for i:=1 to n do begin
   si:=UTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   for j:=1 to m do begin
    tj:=UTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Deletion:=d[i-1,j]+1;
     Insertion:=d[i,j-1]+1;
     Substitution:=d[i-1,j-1]+1;
     if Deletion<Insertion then begin
      if Deletion<Substitution then begin
       d[i,j]:=Deletion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end else begin
      if Insertion<Substitution then begin
       d[i,j]:=Insertion;
      end else begin
       d[i,j]:=Substitution;
      end;
     end;
    end else begin
     d[i,j]:=d[i-1,j-1];
    end;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function UTF8DamerauLevenshteinDistance(const s,t:ansistring):longint;
var d:array of array of longint;
    n,m,i,j,ci,cj,oi,oj,Cost,Deletion,Insertion,Substitution,Transposition,Value:longint;
    si,tj,lsi,ltj:longword;
begin
 n:=UTF8LengthEx(s);
 m:=UTF8LengthEx(t);
 oi:=1;
 oj:=1;
 while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,oi)=UTF8CodeUnitGetChar(t,oj)) do begin
  if (oi>0) and (oi<=length(s)) then begin
   inc(oi,UTF8CharSteps[s[oi]]);
  end else begin
   break;
  end;
  if (oj>0) and (oj<=length(t)) then begin
   inc(oj,UTF8CharSteps[t[oj]]);
  end else begin
   break;
  end;
  dec(n);
  dec(m);
 end;
 if ((n>0) and (m>0)) and (s[length(s)]=t[length(t)]) then begin
  ci:=length(s)+1;
  cj:=length(t)+1;
  UTF8Dec(s,ci);
  UTF8Dec(t,cj);
  while ((n>0) and (m>0)) and (UTF8CodeUnitGetChar(s,ci)=UTF8CodeUnitGetChar(t,cj)) do begin
   UTF8Dec(s,ci);
   UTF8Dec(t,cj);
   dec(n);
   dec(m);
  end;
 end;
 if n=0 then begin
  result:=m;
 end else if m=0 then begin
  result:=n;
 end else begin
  d:=nil;
  SetLength(d,n+1,m+1);
  for i:=0 to n do begin
   d[i,0]:=i;
  end;
  for j:=0 to m do begin
   d[0,j]:=j;
  end;
  ci:=oi;
  si:=0;
  for i:=1 to n do begin
   lsi:=si;
   si:=UTF8CodeUnitGetCharAndInc(s,ci);
   cj:=oj;
   tj:=0;
   for j:=1 to m do begin
    ltj:=tj;
    tj:=UTF8CodeUnitGetCharAndInc(t,cj);
    if si<>tj then begin
     Cost:=1;
    end else begin
     Cost:=0;
    end;
    Deletion:=d[i-1,j]+1;
    Insertion:=d[i,j-1]+1;
    Substitution:=d[i-1,j-1]+Cost;
    if Deletion<Insertion then begin
     if Deletion<Substitution then begin
      Value:=Deletion;
     end else begin
      Value:=Substitution;
     end;
    end else begin
     if Insertion<Substitution then begin
      Value:=Insertion;
     end else begin
      Value:=Substitution;
     end;
    end;
    if ((i>1) and (j>1)) and ((si=ltj) and (lsi=tj)) then begin
     Transposition:=d[i-2,j-2]+Cost;
     if Transposition<Value then begin
      Value:=Transposition;
     end;
    end;
    d[i,j]:=Value;
   end;
  end;
  result:=d[n,m];
  SetLength(d,0);
 end;
end;

function StringLength(const s:ansistring):longint;
begin
 if IsUTF8(s) then begin
  result:=UTF8Length(s);
 end else begin
  result:=length(s);
 end;
end;

function UTF8ToUTF16(const s:ansistring):widestring;
var i,j:longint;
    w:longword;
    b:byte;
begin
 result:='';
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
  end else if w<=$dfff then begin
   inc(j);
  end else if w<=$fffd then begin
   inc(j);
  end else if w<=$ffff then begin
   inc(j);
  end else if w<=$10ffff then begin
   inc(j,2);
  end else begin
   inc(j);
  end;
 end;
 SetLength(result,j);
 i:=1;
 j:=0;
 while i<=length(s) do begin
  b:=byte(s[i]);
  if (b and $80)=0 then begin
   w:=b;
   inc(i);
  end else if ((i+1)<=length(s)) and ((b and $e0)=$c0) and ((byte(s[i+1]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $1f) shl 6) or (byte(s[i+1]) and $3f);
   inc(i,2);
  end else if ((i+2)<=length(s)) and ((b and $f0)=$e0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $0f) shl 12) or ((byte(s[i+1]) and $3f) shl 6) or (byte(s[i+2]) and $3f);
   inc(i,3);
  end else if ((i+3)<=length(s)) and ((b and $f8)=$f0) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $07) shl 18) or ((byte(s[i+1]) and $3f) shl 12) or ((byte(s[i+2]) and $3f) shl 6) or (byte(s[i+3]) and $3f);
   inc(i,4);
{$ifndef strictutf8}
  end else if ((i+4)<=length(s)) and ((b and $fc)=$f8) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $03) shl 24) or ((byte(s[i+1]) and $3f) shl 18) or ((byte(s[i+2]) and $3f) shl 12) or ((byte(s[i+3]) and $3f) shl 6) or (byte(s[i+4]) and $3f);
   inc(i,5);
  end else if ((i+5)<=length(s)) and ((b and $fe)=$fc) and ((byte(s[i+1]) and $c0)=$80) and ((byte(s[i+2]) and $c0)=$80) and ((byte(s[i+3]) and $c0)=$80) and ((byte(s[i+4]) and $c0)=$80) and ((byte(s[i+5]) and $c0)=$80) then begin
   w:=((byte(s[i]) and $01) shl 30) or ((byte(s[i+1]) and $3f) shl 24) or ((byte(s[i+2]) and $3f) shl 18) or ((byte(s[i+3]) and $3f) shl 12) or ((byte(s[i+4]) and $3f) shl 6) or (byte(s[i+5]) and $3f);
   inc(i,6);
{$endif}
  end else begin
   w:=$fffd;
   inc(i);
  end;
  if w<=$d7ff then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$dfff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$fffd then begin
   inc(j);
   result[j]:=widechar(word(w));
  end else if w<=$ffff then begin
   inc(j);
   result[j]:=#$fffd;
  end else if w<=$10ffff then begin
   dec(w,$10000);
   inc(j);
   result[j]:=widechar(word((w shr 10) or $d800));
   inc(j);
   result[j]:=widechar(word((w and $3ff) or $dc00));
  end else begin
   inc(j);
   result[j]:=#$fffd;
  end;
 end;
end;

function UTF16ToUTF8(const s:widestring):ansistring;
var i,j:longint;
    w:word;
    u4c:longword;
begin
 result:='';
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   u4c:=(longword(longword(w and $3ff) shl 10) or longword(word(s[i+1]) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   inc(j);
  end else if u4c<=$7ff then begin
   inc(j,2);
  end else if u4c<=$ffff then begin
   inc(j,3);
  end else if u4c<=$1fffff then begin
   inc(j,4);
{$ifndef StrictUTF8}
  end else if u4c<=$3ffffff then begin
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   inc(j,6);
{$endif}
  end else begin
   inc(j,3);
  end;
 end;
 SetLength(result,j);
 j:=1;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   u4c:=w;
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   u4c:=(longword(longword(w and $3ff) shl 10) or longword(word(s[i+1]) and $3ff))+$10000;
   inc(i,2);
  end else begin
   u4c:=$fffd;
   inc(i);
  end;
  if u4c<=$7f then begin
   result[j]:=ansichar(byte(u4c));
   inc(j);
  end else if u4c<=$7ff then begin
   result[j]:=ansichar(byte($c0 or ((u4c shr 6) and $1f)));
   result[j+1]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,2);
  end else if u4c<=$ffff then begin
   result[j]:=ansichar(byte($e0 or ((u4c shr 12) and $0f)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end else if u4c<=$1fffff then begin
   result[j]:=ansichar(byte($f0 or ((u4c shr 18) and $07)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+3]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,4);
{$ifndef StrictUTF8}
  end else if u4c<=$3ffffff then begin
   result[j]:=ansichar(byte($f8 or ((u4c shr 24) and $03)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+4]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,5);
  end else if u4c<=$7fffffff then begin
   result[j]:=ansichar(byte($fc or ((u4c shr 30) and $01)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 24) and $3f)));
   result[j+2]:=ansichar(byte($80 or ((u4c shr 18) and $3f)));
   result[j+3]:=ansichar(byte($80 or ((u4c shr 12) and $3f)));
   result[j+4]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+5]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,6);
{$endif}
  end else begin
   u4c:=$fffd;
   result[j]:=ansichar(byte($e0 or (u4c shr 12)));
   result[j+1]:=ansichar(byte($80 or ((u4c shr 6) and $3f)));
   result[j+2]:=ansichar(byte($80 or (u4c and $3f)));
   inc(j,3);
  end;
 end;
end;

procedure InitializeUTF8DFA;
type TAnsicharset=set of ansichar;
{$ifdef StrictUTF8}
{ c0  8 11000000   | d0  2 11(010000) | e0 10 11100000   | f0 11 11110000
  c1  8 11000001   | d1  2 11(010001) | e1  3 111(00001) | f1  6 111100(01)
  c2  2 11(000010) | d2  2 11(010010) | e2  3 111(00010) | f2  6 111100(10)
  c3  2 11(000011) | d3  2 11(010011) | e3  3 111(00011) | f3  6 111100(11)
  c4  2 11(000100) | d4  2 11(010100) | e4  3 111(00100) | f4  5 11110(100)
  c5  2 11(000101) | d5  2 11(010101) | e5  3 111(00101) | f5  8 11110101
  c6  2 11(000110) | d6  2 11(010110) | e6  3 111(00110) | f6  8 11110110
  c7  2 11(000111) | d7  2 11(010111) | e7  3 111(00111) | f7  8 11110111
  c8  2 11(001000) | d8  2 11(011000) | e8  3 111(01000) | f8  8 11111000
  c9  2 11(001001) | d9  2 11(011001) | e9  3 111(01001) | f9  8 11111001
  ca  2 11(001010) | da  2 11(011010) | ea  3 111(01010) | fa  8 11111010
  cb  2 11(001011) | db  2 11(011011) | eb  3 111(01011) | fb  8 11111011
  cc  2 11(001100) | dc  2 11(011100) | ec  3 111(01100) | fc  8 11111100
  cd  2 11(001101) | dd  2 11(011101) | ed  4 1110(1101) | fd  8 11111101
  ce  2 11(001110) | de  2 11(011110) | ee  3 111(01110) | fe  8 11111110
  cf  2 11(001111) | df  2 11(011111) | ef  3 111(01111) | ff  8 11111111  }
const cc007F=$0;
      cc808F=$1;
      ccC2DF=$2;
      ccE1ECEEEF=$3;
      ccED=$4;
      ccF4=$5;
      ccF1F3=$6;
      ccA0BF=$7;
      ccC0C1F5FF=$8;
      cc909F=$9;
      ccE0=$a;
      ccF0=$b;
      tsBEGIN=0;
      tsERROR=1;
      tsSINGLETAIL=2;
      tsDOUBLETAIL=3;
      tsDOUBLETAILwithA0BFonly=4;
      tsDOUBLETAILwith809FFonly=5;
      tsTRIPLETAILwith90BFonly=6;
      tsTRIPLETAIL=7;
      tsTRIPLETAILwith808Fonly=8;
{$else}
const cc007F=$0;
      cc80BF=$1; // Tail
      ccC0DF=$3; // ($ff shr $03)=$1f
      ccE0EF=$4; // ($ff shr $04)=$0f
      ccF0F7=$5; // ($ff shr $05)=$07
      ccF8FB=$6; // ($ff shr $06)=$03
      ccFCFD=$7; // ($ff shr $07)=$01
      ccFEFF=$8; // ($ff shr $08)=$00
      tsBEGIN=0;
      tsERROR=1;
      tsSINGLETAIL=2;
      tsDOUBLETAIL=3;
      tsTRIPLETAIL=4;
      tsQUADTAIL=5;
      tsQUINTAIL=6;
{$endif}
      tsMUL=16;
 procedure AssignCharsetToCharClass(const Charset:TAnsicharset;CharClass:byte);
 var c:ansichar;
 begin
  for c:=low(ansichar) to high(ansichar) do begin
   if c in Charset then begin
    UTF8DFACharClasses[c]:=CharClass;
   end;
  end;
 end;
 procedure AddTranslation(FromState,AtCharClass,ToState:byte);
 begin
  UTF8DFATransitions[(FromState*tsMUL)+AtCharClass]:=ToState*tsMUL;
 end;
var i:longint;
begin
 FillChar(UTF8DFACharClasses,sizeof(TUTF8Chars),#0);
 FillChar(UTF8DFATransitions,sizeof(TUTF8Bytes),#0);
 begin
{$ifdef StrictUTF8}
  AssignCharsetToCharClass([#$00..#$7f],cc007F);
  AssignCharsetToCharClass([#$80..#$8f],cc808F);
  AssignCharsetToCharClass([#$90..#$9f],cc909F);
  AssignCharsetToCharClass([#$a0..#$bf],ccA0BF);
  AssignCharsetToCharClass([#$c0..#$c1],ccC0C1F5FF);
  AssignCharsetToCharClass([#$c2..#$df],ccC2DF);
  AssignCharsetToCharClass([#$e0],ccE0);
  AssignCharsetToCharClass([#$e1..#$ec,#$ee..#$ef],ccE1ECEEEF);
  AssignCharsetToCharClass([#$ed],ccED);
  AssignCharsetToCharClass([#$f0],ccF0);
  AssignCharsetToCharClass([#$f1..#$f3],ccF1F3);
  AssignCharsetToCharClass([#$f4],ccF4);
  AssignCharsetToCharClass([#$f5..#$ff],ccC0C1F5FF);
{$else}
  AssignCharsetToCharClass([#$00..#$7f],cc007F);
  AssignCharsetToCharClass([#$80..#$bf],cc80BF);
  AssignCharsetToCharClass([#$c0..#$df],ccC0DF);
  AssignCharsetToCharClass([#$e0..#$ef],ccE0EF);
  AssignCharsetToCharClass([#$f0..#$f7],ccF0F7);
  AssignCharsetToCharClass([#$f8..#$fb],ccF8FB);
  AssignCharsetToCharClass([#$fc..#$fd],ccFCFD);
  AssignCharsetToCharClass([#$fe..#$ff],ccFEFF);
{$endif}
 end;
 begin
  for i:=low(TUTF8Bytes) to high(TUTF8Bytes) do begin
   UTF8DFATransitions[i]:=tsERROR*tsMUL;
  end;
{$ifdef StrictUTF8}
  begin
   AddTranslation(tsBEGIN,cc007F,tsBEGIN);
   AddTranslation(tsBEGIN,cc808F,tsERROR);
   AddTranslation(tsBEGIN,cc909F,tsERROR);
   AddTranslation(tsBEGIN,ccA0BF,tsERROR);
   AddTranslation(tsBEGIN,ccC2DF,tsSINGLETAIL);
   AddTranslation(tsBEGIN,ccE0,tsDOUBLETAILwithA0BFonly);
   AddTranslation(tsBEGIN,ccE1ECEEEF,tsDOUBLETAIL);
   AddTranslation(tsBEGIN,ccED,tsDOUBLETAILwith809FFonly);
   AddTranslation(tsBEGIN,ccF0,tsTRIPLETAILwith90BFonly);
   AddTranslation(tsBEGIN,ccF1F3,tsTRIPLETAIL);
   AddTranslation(tsBEGIN,ccF4,tsTRIPLETAILwith808Fonly);
   AddTranslation(tsBEGIN,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsERROR,cc007F,tsERROR);
   AddTranslation(tsERROR,cc808F,tsERROR);
   AddTranslation(tsERROR,cc909F,tsERROR);
   AddTranslation(tsERROR,ccA0BF,tsERROR);
   AddTranslation(tsERROR,ccC2DF,tsERROR);
   AddTranslation(tsERROR,ccE0,tsERROR);
   AddTranslation(tsERROR,ccE1ECEEEF,tsERROR);
   AddTranslation(tsERROR,ccED,tsERROR);
   AddTranslation(tsERROR,ccF0,tsERROR);
   AddTranslation(tsERROR,ccF1F3,tsERROR);
   AddTranslation(tsERROR,ccF4,tsERROR);
   AddTranslation(tsERROR,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
   AddTranslation(tsSINGLETAIL,cc808F,tsBEGIN);
   AddTranslation(tsSINGLETAIL,cc909F,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccA0BF,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE0,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccED,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF0,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF4,tsERROR);
   AddTranslation(tsSINGLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAIL,cc808F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,cc909F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccA0BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccED,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc808F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,cc909F,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccA0BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccED,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAILwithA0BFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAILwith809FFonly,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,cc808F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwith809FFonly,cc909F,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccA0BF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccC2DF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccE0,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccED,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF0,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF1F3,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccF4,tsERROR);
   AddTranslation(tsDOUBLETAILwith809FFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAILwith90BFonly,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,cc808F,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,cc909F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccA0BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccED,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAILwith90BFonly,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAIL,cc808F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,cc909F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccA0BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccED,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccC0C1F5FF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAILwith808Fonly,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,cc808F,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAILwith808Fonly,cc909F,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccA0BF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccC2DF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccE0,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccE1ECEEEF,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccED,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF0,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF1F3,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccF4,tsERROR);
   AddTranslation(tsTRIPLETAILwith808Fonly,ccC0C1F5FF,tsERROR);
  end;
 end;
{$else}
  begin
   AddTranslation(tsBEGIN,cc007F,tsBEGIN);
   AddTranslation(tsBEGIN,cc80BF,tsERROR);
   AddTranslation(tsBEGIN,ccC0DF,tsSINGLETAIL);
   AddTranslation(tsBEGIN,ccE0EF,tsDOUBLETAIL);
   AddTranslation(tsBEGIN,ccF0F7,tsTRIPLETAIL);
   AddTranslation(tsBEGIN,ccF8FB,tsQUADTAIL);
   AddTranslation(tsBEGIN,ccFCFD,tsQUINTAIL);
   AddTranslation(tsBEGIN,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsERROR,cc007F,tsERROR);
   AddTranslation(tsERROR,cc80BF,tsERROR);
   AddTranslation(tsERROR,ccC0DF,tsERROR);
   AddTranslation(tsERROR,ccE0EF,tsERROR);
   AddTranslation(tsERROR,ccF0F7,tsERROR);
   AddTranslation(tsERROR,ccF8FB,tsERROR);
   AddTranslation(tsERROR,ccFCFD,tsERROR);
   AddTranslation(tsERROR,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsSINGLETAIL,cc007F,tsERROR);
   AddTranslation(tsSINGLETAIL,cc80BF,tsBEGIN);
   AddTranslation(tsSINGLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsSINGLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsSINGLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsSINGLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsDOUBLETAIL,cc007F,tsERROR);
   AddTranslation(tsDOUBLETAIL,cc80BF,tsSINGLETAIL);
   AddTranslation(tsDOUBLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsDOUBLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsTRIPLETAIL,cc007F,tsERROR);
   AddTranslation(tsTRIPLETAIL,cc80BF,tsDOUBLETAIL);
   AddTranslation(tsTRIPLETAIL,ccC0DF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccE0EF,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF0F7,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccF8FB,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccFCFD,tsERROR);
   AddTranslation(tsTRIPLETAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsQUADTAIL,cc007F,tsERROR);
   AddTranslation(tsQUADTAIL,cc80BF,tsTRIPLETAIL);
   AddTranslation(tsQUADTAIL,ccC0DF,tsERROR);
   AddTranslation(tsQUADTAIL,ccE0EF,tsERROR);
   AddTranslation(tsQUADTAIL,ccF0F7,tsERROR);
   AddTranslation(tsQUADTAIL,ccF8FB,tsERROR);
   AddTranslation(tsQUADTAIL,ccFCFD,tsERROR);
   AddTranslation(tsQUADTAIL,ccFEFF,tsERROR);
  end;
  begin
   AddTranslation(tsQUINTAIL,cc007F,tsERROR);
   AddTranslation(tsQUINTAIL,cc80BF,tsQUADTAIL);
   AddTranslation(tsQUINTAIL,ccC0DF,tsERROR);
   AddTranslation(tsQUINTAIL,ccE0EF,tsERROR);
   AddTranslation(tsQUINTAIL,ccF0F7,tsERROR);
   AddTranslation(tsQUINTAIL,ccF8FB,tsERROR);
   AddTranslation(tsQUINTAIL,ccFCFD,tsERROR);
   AddTranslation(tsQUINTAIL,ccFEFF,tsERROR);
  end;
 end;
{$endif}
end;

initialization
 InitializeUTF8DFA;
end.
