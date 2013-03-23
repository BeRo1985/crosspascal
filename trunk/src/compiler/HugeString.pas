unit HugeString; 
{$I Compiler.inc}

interface

type THugeChar=longword;

     THugeString=array of THugeChar;

function HugeStringCompare(const a,b:THugeString):longint;
function HugeStringConcatChar(var a:THugeString;const b:THugeChar;var Len:longint):longint;
function HugeStringConcat(const a,b:THugeString):THugeString; overload;
function HugeStringConcat(const a:THugeString;const b:THugeChar):THugeString; overload;
function HugeStringConcat(const a:THugeChar;const b:THugeString):THugeString; overload;
function HugeStringConcat(const a,b:THugeChar):THugeString; overload;
function HugeStringCopy(const Str:THugeString;Index,Len:longint):THugeString;
function HugeStringCharGet(const Str:THugeString;Index:longint):THugeChar;
procedure HugeStringCharSet(var Str:THugeString;Index:longint;c:THugeChar);
function UTF8ToHugeString(const Str:ansistring):THugeString;
function AnsiStringToHugeString(const Str:ansistring):THugeString;
function WideStringToHugeString(const Str:widestring):THugeString;
function HugeStringToUTF8(const Str:THugeString):ansistring;
function HugeStringToAnsiString(const Str:THugeString):ansistring;
function HugeStringToWideString(const Str:THugeString):widestring;

implementation

uses Globals,UnicodeUtils;

function HugeStringCompare(const a,b:THugeString):longint;
var i:longint;
begin
 result:=0;
 if a=b then begin
  exit;
 end;
 i:=0;
 while (i<length(a)) and (i<length(b)) do begin
  if a[i]<b[i] then begin
   result:=-1;
   exit;
  end else if a[i]>b[i] then begin
   result:=1;
   exit;
  end;
  inc(i);
 end;
 if length(a)<length(b) then begin
  result:=-1;
 end else if length(a)>length(b) then begin
  result:=1;
 end;
end;

function HugeStringConcatChar(var a:THugeString;const b:THugeChar;var Len:longint):longint;
var AllocLen:longint;
begin
 result:=Len;
 inc(Len);
 if Len>=length(a) then begin
  if Len<16 then begin
   AllocLen:=16;
  end else begin
   AllocLen:=RoundUpToPowerOfTwo(Len);
  end;
  SetLength(a,AllocLen);
 end;
 a[result]:=b;
end;

function HugeStringConcat(const a,b:THugeString):THugeString; overload;
begin
 if (length(a)>0) and (length(b)>0) then begin
  result:=nil;
  SetLength(result,length(a)+length(b));
  Move(a[0],result[0],length(a)*SizeOf(THugeChar));
  Move(b[0],result[length(a)],length(b)*SizeOf(THugeChar));
 end else if length(a)>0 then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function HugeStringConcat(const a:THugeString;const b:THugeChar):THugeString; overload;
begin
 result:=nil;
 if length(a)>0 then begin
  SetLength(result,length(a)+1);
  Move(a[0],result[0],length(a)*SizeOf(THugeChar));
  result[length(a)]:=b;
 end else begin
  SetLength(result,1);
  result[0]:=b;
 end;
end;

function HugeStringConcat(const a:THugeChar;const b:THugeString):THugeString; overload;
begin
 result:=nil;
 if length(b)>0 then begin
  SetLength(result,length(b)+1);
  result[0]:=a;
  Move(b[0],result[1],length(b)*SizeOf(THugeChar));
 end else begin
  SetLength(result,1);
  result[0]:=a;
 end;
end;

function HugeStringConcat(const a,b:THugeChar):THugeString; overload;
begin
 result:=nil;
 SetLength(result,2);
 result[0]:=a;
 result[1]:=b;
end;

function HugeStringCopy(const Str:THugeString;Index,Len:longint):THugeString;
begin
 if (Index>=1) and (Index<=length(Str)) then begin
  result:=copy(Str,Index-1,Len);
 end else begin
  result:=nil;
 end;
end;

function HugeStringCharGet(const Str:THugeString;Index:longint):THugeChar;
begin
 if (Index>=1) and (Index<=length(Str)) then begin
  result:=Str[Index-1];
 end else begin
  result:=0;
 end;
end;

procedure HugeStringCharSet(var Str:THugeString;Index:longint;c:THugeChar);
begin
 if (Index>=1) and (Index<=length(Str)) then begin
  Str[Index-1]:=c;
 end;
end;

function UTF8ToHugeString(const Str:ansistring):THugeString;
var i:longint;
begin
 result:=nil;
 i:=1;
 while i<=length(Str) do begin
  result:=HugeStringConcat(result,UTF8CodeUnitGetCharAndInc(Str,i));
 end;
end;

function AnsiStringToHugeString(const Str:ansistring):THugeString;
var i:longint;
begin
 result:=nil;
 SetLength(result,length(Str));
 for i:=1 to length(Str) do begin
  result[i-1]:=THugeChar(byte(ansichar(Str[i])));
 end;
end;

function WideStringToHugeString(const Str:widestring):THugeString;
var i:longint;
begin
 result:=nil;
 SetLength(result,length(Str));
 for i:=1 to length(Str) do begin
  result[i-1]:=THugeChar(word(widechar(Str[i])));
 end;
end;

function HugeStringToUTF8(const Str:THugeString):ansistring;
var i:longint;
begin
 result:='';
 for i:=1 to length(Str) do begin
  result:=result+UTF32CharToUTF8(Str[i-1]);
 end;
end;

function HugeStringToAnsiString(const Str:THugeString):ansistring;
var i:longint;
begin
 result:='';
 SetLength(result,length(Str));
 for i:=1 to length(Str) do begin
  result[i]:=ansichar(byte(THugeChar(Str[i-1])));
 end;
end;

function HugeStringToWideString(const Str:THugeString):widestring;
var i:longint;
begin
 result:='';
 SetLength(result,length(Str));
 for i:=1 to length(Str) do begin
  result[i]:=widechar(word(THugeChar(Str[i-1])));
 end;
end;

end.
