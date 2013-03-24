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
function UTF16ToHugeString(const s:widestring):THugeString;
function AnsiStringToHugeString(const Str:ansistring):THugeString;
function WideStringToHugeString(const Str:widestring):THugeString;
function HugeStringToUTF8(const Str:THugeString):ansistring;
function HugeStringToUTF16(const s:THugeString):widestring;
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
var i,j:longint;
begin
 result:=nil;
 i:=1;
 j:=0;
 while i<=length(Str) do begin
  UTF8SafeInc(Str,i);
  inc(j);
 end;           
 SetLength(Result,j);
 i:=1;
 j:=0;
 while i<=length(Str) do begin
  result[j]:=UTF8CodeUnitGetCharAndInc(Str,i);
  inc(j);
 end;
end;

function UTF16ToHugeString(const s:widestring):THugeString;
var i,j:longint;
    w:word;
begin
 result:=nil;
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   inc(j);
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   inc(j);
   inc(i,2);
  end else begin
   inc(j);
   inc(i);
  end;
 end;
 SetLength(result,j);
 j:=0;
 i:=1;
 while i<=length(s) do begin
  w:=word(s[i]);
  if (w<=$d7ff) or (w>=$e000) then begin
   result[j]:=w;
   inc(j);
   inc(i);
  end else if ((i+1)<=length(s)) and ((w>=$d800) and (w<=$dbff)) and ((word(s[i+1])>=$dc00) and (word(s[i+1])<=$dfff)) then begin
   result[j]:=(longword(longword(w and $3ff) shl 10) or longword(word(s[i+1]) and $3ff))+$10000;
   inc(j);
   inc(i,2);
  end else begin
   result[j]:=$fffd;
   inc(j);
   inc(i);
  end;
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
var i,j:longint;
    u4c:longword;
begin
 result:='';
 j:=0;
 for i:=0 to length(Str)-1 do begin
  u4c:=Str[i];
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
 for i:=0 to length(Str)-1 do begin
  u4c:=Str[i];
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

function HugeStringToUTF16(const s:THugeString):widestring;
var i,j:longint;
    w:longword;
begin
 result:='';
 j:=0;
 for i:=0 to length(s)-1 do begin
  w:=s[i];
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
 j:=0;
 for i:=0 to length(s)-1 do begin
  w:=s[i];
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
