unit BeRoUtils;
{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}

interface

{$IFDEF WIN32}
uses Windows;
{$ELSE}
uses SysUtils;
{$ENDIF}

type TCharSet=set of ansichar;
     TAlphabet=array['A'..'Z'] of ansichar;

     TFileName=ansistring;

     dword=longword;

     Int64Rec=packed record
      Lo,Hi:dword;
     end;

     LongRec=packed record
      Lo,Hi:word;
     end;

{$IFDEF WIN32}
     TSearchRec=record
      Time,Size,Attr:longint;
      name:TFileName;
      ExcludeAttr:longint;
      FindHandle:THandle;
      FindData:TWin32FindData;
     end;
{$ELSE}
     TSearchRec=SysUtils.TSearchRec;
{$ENDIF}

const Alphabet:TCharSet=['A'..'Z'];
      SmallCaps:TAlphabet=('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z');
      SentenceChars:TCharSet=['.',',',';','!','?'];
      SearchSentenceChars:TCharSet=['.','!','?'];
      SpaceChars:TCharSet=[#0..#32];

      DirSplashChar={$IFDEF unixversion}'/'{$ELSE}'\'{$ENDIF};
      DirSplashNotChar={$IFNDEF unixversion}'/'{$ELSE}'\'{$ENDIF};

      fmOpenRead=$0000;
      fmOpenWrite=$0001;
      fmOpenReadWrite=$0002;
      fmShareCompat=$0000;
      fmShareExclusive=$0010;
      fmShareDenyWrite=$0020;
      fmShareDenyRead=$0030;
      fmShareDenyNone=$0040;

      faReadOnly=$00000001;
      faHidden=$00000002;
      faSysFile=$00000004;
      faVolumeID=$00000008;
      faDirectory=$00000010;
      faArchive=$00000020;
      faAnyFile=$0000003f;

function Parse(var S:ansistring;C:TCharSet;Continuous:boolean=false):ansistring; overload;
function Parse(var S:ansistring;C:ansichar;Continuous:boolean=false):ansistring; overload;
function STRTOINT(S:ansistring):int64;
function INTTOSTR(I:int64):ansistring;
function STRTOFLOAT(S:ansistring):extended;
function FLOATTOSTR(F:extended):ansistring;
function StrLCopy(Dest:pansichar;const Source:pansichar;MaxLen:longword):pansichar; assembler;
function StrPCopy(Dest:pansichar;const Source:ansistring):pansichar;
function FindNext(var F:TSearchRec):longint;
procedure FindClose(var F:TSearchRec);
function FindFirst(const Path:ansistring;Attr:longint;var F:TSearchRec):longint;
function FILEEXISTS(S:ansistring):boolean;
function ExtractFileExt(S:ansistring):ansistring;
function ExtractFileName(S:ansistring):ansistring;
function ExtractFilePath(S:ansistring):ansistring;
function ChangeFileExt(S,E:ansistring):ansistring;
function TRIM(const S:ansistring):ansistring;
function TRIMLEFT(const S:ansistring):ansistring;
function TRIMRIGHT(const S:ansistring):ansistring;
function UPPERCASE(const S:ansistring):ansistring;
function LOWERCASE(const S:ansistring):ansistring;
function UPCASE(const C:ansichar):ansichar;
function LOCASE(const C:ansichar):ansichar;
function StringReplace(var S:ansistring;const FindStr,RepStr:ansistring):boolean;
function StringReplaceAll(var S:ansistring;const FindStr,RepStr:ansistring):boolean;
function MatchPattern(Input,Pattern:pansichar):boolean;
function StrToAddr(S:ansistring):longint;
function AddrToStr(Address:longint):ansistring;
procedure FastFillChar(var Dest;Count:longint;Value:ansichar);
function AreBytesEqual(const A,B;Count:longint):boolean;
function GetNoteDuration(NoteType,ShortestTime:longint;Dots:longint=0):longint;
function GCD(A,B:longint):longint;
function Swap16(const Value:word):word;
function Swap32(const Value:longword):longword; 
function SwapWordLittleEndian(Value:word):word; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
function SwapDWordLittleEndian(Value:longword):longword; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
function SwapWordBigEndian(Value:word):word; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
function SwapDWordBigEndian(Value:longword):longword; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
procedure SwapLittleEndianData16(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
procedure SwapLittleEndianData32(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
procedure SwapBigEndianData16(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
procedure SwapBigEndianData32(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
function DecToHex(Value:int64):ansistring;
function HexToDec(Value:ansistring):int64;
function ByteToHex(Value:byte):ansistring;
function HexToByte(Value:ansistring):byte;
function WordToHex(Value:word):ansistring;
function HexToWord(Value:ansistring):word;
function LongWordToHex(Value:longword):ansistring;
function HexToLongWord(Value:ansistring):longword;
function FABS(Value:single):single;

implementation

const HexNumbers:array[0..$f] of ansichar='0123456789ABCDEF';

function Parse(var S:ansistring;C:TCharSet;Continuous:boolean=false):ansistring; overload;
var Counter,index:longint;
begin
 index:=0;
 for Counter:=1 to length(S) do begin
  if S[Counter] in C then begin
   index:=Counter;
   break;
  end;
 end;
 if index<>0 then begin
  result:=COPY(S,1,index-1);
  DELETE(S,1,index);
 end else begin
  result:=S;
  S:='';
 end;
 if Continuous then begin
  while (length(S)>0) and (S[1] in C) do begin
   DELETE(S,1,1);
  end;
 end;
end;

function Parse(var S:ansistring;C:ansichar;Continuous:boolean=false):ansistring; overload;
var index:longint;
begin
 index:=POS(C,S);;
 if index<>0 then begin
  result:=COPY(S,1,index-1);
  DELETE(S,1,index);
 end else begin
  result:=S;
  S:='';
 end;
 if Continuous then while (length(S)>0) and (S[1]=C) do DELETE(S,1,1);
end;

function STRTOINT(S:ansistring):int64;
var Code:longint;
begin
 VAL(S,result,Code);
end;

function INTTOSTR(I:int64):ansistring;
begin
 STR(I,result);
end;

function STRTOFLOAT(S:ansistring):extended;
var Code:longint;
begin
 VAL(S,result,Code);
end;

function FLOATTOSTR(F:extended):ansistring;
begin
 if F=TRUNC(F) then begin
  result:=INTTOSTR(TRUNC(F));
 end else begin
  STR(F,result);
  result:=TRIM(result);
 end;
end;

function StrLCopy(Dest:pansichar;const Source:pansichar;MaxLen:longword):pansichar; assembler;
asm
 PUSH EDI
 PUSH ESI
 PUSH EBX
 MOV ESI,EAX
 MOV EDI,EDX
 MOV EBX,ECX
 xor AL,AL
 TEST ECX,ECX
 JZ @@1
 REPNE SCASB
 JNE @@1
 inc ECX
@@1:
 SUB EBX,ECX
 MOV EDI,ESI
 MOV ESI,EDX
 MOV EDX,EDI
 MOV ECX,EBX
 shr ECX,2
 REP MOVSD
 MOV ECX,EBX
 and ECX,3
 REP MOVSB
 STOSB
 MOV EAX,EDX
 POP EBX
 POP ESI
 POP EDI
end;

function StrPCopy(Dest:pansichar;const Source:ansistring):pansichar;
begin
 result:=StrLCopy(Dest,pansichar(Source),length(Source));
end;

{$IFDEF WIN32}
function FindMatchingFile(var F:TSearchRec):longint;
var LocalFileTime:TFileTime;
begin
 with F do begin
  while (FindData.dwFileAttributes and ExcludeAttr)<>0 do begin
   if not Windows.FindNextFile(FindHandle,FindData) then begin
    result:=GetLastError;
    exit;
   end;
  end;
  FileTimeToLocalFileTime(FindData.ftLastWriteTime,LocalFileTime);
  FileTimeToDosDateTime(LocalFileTime,LongRec(Time).Hi,LongRec(Time).Lo);
  Size:=FindData.nFileSizeLow;
  Attr:=FindData.dwFileAttributes;
  name:=FindData.cFileName;
 end;
 result:=0;
end;
{$ENDIF}

function FindNext(var F:TSearchRec):longint;
begin
{$IFDEF WIN32}
 if Windows.FindNextFile(F.FindHandle,F.FindData) then begin
  result:=FindMatchingFile(F);
 end else begin
  result:=GetLastError;
 end;
{$ELSE}
 result:=SysUtils.FindNext(F);
{$ENDIF}
end;

procedure FindClose(var F:TSearchRec);
begin
{$IFDEF WIN32}
 if F.FindHandle<>INVALID_HANDLE_VALUE then begin
  Windows.FindClose(F.FindHandle);
  F.FindHandle:=INVALID_HANDLE_VALUE;
 end;
{$ELSE}
 SysUtils.FindClose(F);
{$ENDIF}
end;

function FindFirst(const Path:ansistring;Attr:longint;var F:TSearchRec):longint;
{$IFDEF WIN32}
const faSpecial=faHidden or faSysFile or faVolumeID or faDirectory;
{$ENDIF}
begin
{$IFDEF WIN32}
 F.ExcludeAttr:=not Attr and faSpecial;
 F.FindHandle:=Windows.FindFirstFile(pchar(string(Path)),F.FindData);
 if F.FindHandle<>INVALID_HANDLE_VALUE then begin
  result:=FindMatchingFile(F);
  if result<>0 then FindClose(F);
 end else begin
  result:=GetLastError;
 end;
{$ELSE}
 result:=SysUtils.FindFirst(Path,Attr,F);
{$ENDIF}
end;

function FILEEXISTS(S:ansistring):boolean;
var F:file;
begin
 result:=false;
 ASSIGNFILE(F,S);
 {$I-}RESET(F,1);{$I+}
 if IOResult=0 then begin
  CLOSEFILE(F);
  result:=true;
 end;
end;

function ExtractFileExt(S:ansistring):ansistring;
var I,J,K:longint;
begin
 result:='';
 K:=0;
 J:=length(S);
 for I:=J downto 1 do if (S[I]='.') or (S[I]='\') or (S[I]='/') or (S[I]=':') then begin
  K:=I;
  break;
 end;
 if (K>0) and (S[K]='.') then result:=COPY(S,K,J-K+1);
end;

function ExtractFileName(S:ansistring):ansistring;
var I,J,K:longint;
begin
 result:=S;
 K:=0;
 J:=length(S);
 for I:=J downto 1 do if (S[I]='\') or (S[I]='/') or (S[I]=':') then begin
  K:=I;
  break;
 end;
 if K>0 then result:=COPY(S,K+1,J-K+1);
end;

function ExtractFilePath(S:ansistring):ansistring;
var I,J,K:longint;
begin
 result:=S;
 K:=0;
 J:=length(S);
 for I:=J downto 1 do if (S[I]='\') or (S[I]='/') or (S[I]=':') then begin
  K:=I;
  break;
 end;
 if K>0 then result:=COPY(S,1,K);
end;

function ChangeFileExt(S,E:ansistring):ansistring;
var I,J,K:longint;
begin
 K:=0;
 J:=length(S);
 for I:=J downto 1 do if (S[I]='.') or (S[I]='\') or (S[I]='/') or (S[I]=':') then begin
  K:=I;
  break;
 end;
 if (K>0) and (S[K]='.') then begin
  result:=COPY(S,1,K-1)+E;
 end else begin
  result:=S+E;
 end;
end;

function TRIM(const S:ansistring):ansistring;
var StartPosition,LengthCount:longint;
begin
 LengthCount:=length(S);
 if LengthCount>0 then begin
  while (LengthCount>0) and (S[LengthCount] in [#0..#32]) do dec(LengthCount);
  StartPosition:=1;
  while (StartPosition<=LengthCount) and (S[StartPosition] in [#0..#32]) do inc(StartPosition);
  result:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 end else begin
  result:='';
 end;
end;

function TRIMLEFT(const S:ansistring):ansistring;
var StartPosition,LengthCount:longint;
begin
 LengthCount:=length(S);
 if LengthCount>0 then begin
  StartPosition:=1;
  while (StartPosition<=LengthCount) and (S[StartPosition] in [#0..#32]) do inc(StartPosition);
  result:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 end else begin
  result:='';
 end;
end;

function TRIMRIGHT(const S:ansistring):ansistring;
var StartPosition,LengthCount:longint;
begin
 LengthCount:=length(S);
 if LengthCount>0 then begin
  while (LengthCount>0) and (S[LengthCount] in [#0..#32]) do dec(LengthCount);
  StartPosition:=1;
  result:=COPY(S,StartPosition,LengthCount-StartPosition+1);
 end else begin
  result:='';
 end;
end;

function UPPERCASE(const S:ansistring):ansistring;
var I,L:longint;
begin
 result:='';
 L:=length(S);
 I:=1;
 while I<=L do begin
  if S[I] in ['a'..'z'] then begin
   result:=result+ansichar(byte(S[I])-32);
  end else begin
   result:=result+S[I];
  end;
  inc(I);
 end;
end;

function LOWERCASE(const S:ansistring):ansistring;
var I,L:longint;
begin
 result:='';
 L:=length(S);
 I:=1;
 while I<=L do begin
  if S[I] in ['A'..'Z'] then begin
   result:=result+ansichar(byte(S[I])+32);
  end else begin
   result:=result+S[I];
  end;
  inc(I);
 end;
end;

function UPCASE(const C:ansichar):ansichar;
begin
 if C in ['a'..'z'] then begin
  result:=ansichar(byte(C)-32);
 end else begin
  result:=C;
 end;
end;

function LOCASE(const C:ansichar):ansichar;
begin
 if C in ['A'..'Z'] then begin
  result:=ansichar(byte(C)+32);
 end else begin
  result:=C;
 end;
end;

function StringReplace(var S:ansistring;const FindStr,RepStr:ansistring):boolean;
var index:longint;
begin
 result:=false;
 if POS(FindStr,RepStr)=0 then begin
  index:=POS(FindStr,S);
  if index<>0 then begin
   S:=COPY(S,1,index-1)+RepStr+COPY(S,index+length(FindStr),length(S));
   result:=true;
  end;
 end;
end;

function StringReplaceAll(var S:ansistring;const FindStr,RepStr:ansistring):boolean;
begin
 result:=false;
 while StringReplace(S,FindStr,RepStr) do begin
  result:=true;
 end;
end;

function MatchPattern(Input,Pattern:pansichar):boolean;
begin
 result:=true;
 while true do begin
  case Pattern[0] of
   #0:begin
    result:=Input[0]=#0;
    exit;
   end;
   '*':begin
    inc(Pattern);
    if Pattern[0]=#0 then begin
     result:=true;
     exit;
    end;
    while Input[0]<>#0 do begin
     if MatchPattern(Input,Pattern) then begin
      result:=true;
      exit;
     end;
     inc(Input);
    end;
   end;
   '?':begin
    if Input[0]=#0 then begin
     result:=false;
     exit;
    end;
    inc(Input);
    inc(Pattern);
   end;
   '[':begin
    if Pattern[1] in [#0,'[',']'] then begin
     result:=false;
     exit;
    end;
    if Pattern[1]='^' then begin
     inc(Pattern,2);
     result:=true;
     while Pattern[0]<>']' do begin
      if Pattern[1]='-' then begin
       if (Input[0]>=Pattern[0]) and (Input[0]<=Pattern[2]) then begin
        result:=false;
        break;
       end else begin
        inc(Pattern,3);
       end;
      end else begin
       if Input[0]=Pattern[0] then begin
        result:=false;
        break;
       end else begin
        inc(Pattern);
       end;
      end;
     end;
    end else begin
     inc(Pattern);
     result:=false;
     while Pattern[0]<>']' do begin
      if Pattern[1]='-' then begin
       if (Input[0]>=Pattern[0]) and (Input[0]<=Pattern[2]) then begin
        result:=true;
        break;
       end else begin
        inc(Pattern,3);
       end;
      end else begin
       if Input[0]=Pattern[0] then begin
        result:=true;
        break;
       end else begin
        inc(Pattern);
       end;
      end;
     end;
    end;
    if result then begin
     inc(Input);
     while not (Pattern[0] in [']',#0]) do inc(Pattern);
     if Pattern[0]=#0 then begin
      result:=false;
      exit;
     end else begin
      inc(Pattern);
     end;
    end else begin
     exit;
    end;
   end;
   else begin
    if Input[0]<>Pattern[0] then begin
     result:=false;
     break;
    end;
    inc(Input);
    inc(Pattern);
   end;
  end;
 end;
end;

function StrToAddr(S:ansistring):longint;
var R,I,P,C:longint;
    T:ansistring;
begin
 result:=0;
 R:=0;
 for I:=0 to 3 do begin
  P:=POS('.',S);
  if P=0 then P:=length(S)+1;
  if P<=1 then exit;
  T:=COPY(S,1,P-1);
  DELETE(S,1,P);
  VAL(T,P,C);
  if (C<>0) or (P<0) or (P>255) then exit;
  R:=R or P shl (I*8);
 end;
 result:=R;
end;

function AddrToStr(Address:longint):ansistring;
var R,S:ansistring;
    I:longint;
begin
 R:='';
 for I:=0 to 3 do begin
  STR(Address shr (I*8) and $ff,S);
  R:=R+S;
  if I<3 then R:=R+'.';
 end;
 result:=R;
end;

type pinteger=^longint;
     PIntegerArray=^TIntegerArray;
     TIntegerArray=array[0..($7fffffff div sizeof(longint))-1] of longint;

     pbyte=^byte;
     PByteArray=^TByteArray;
     TByteArray=array[0..($7fffffff div sizeof(byte))-1] of byte;

procedure FastFillChar(var Dest;Count:longint;Value:ansichar);
label P01,P02,P03,P04,P05,P06,P07,P08,P09,P10,P11,P12;
var I,J,K:longint;
    P:pointer;
begin
 if Count>0 then begin
  P:=@Dest;
  if Count>=12 then begin
   J:=byte(Value);
   J:=J or (J shl 8);
   J:=J or (J shl 16);
   pinteger(P)^:=J;
   pinteger(longint(P)+Count-4)^:=J;
   I:=Count shr 2;
   if Count>=256 then begin
    if Count<448 then begin
     PIntegerArray(P)[1]:=J;
     PIntegerArray(P)[2]:=J;
     PIntegerArray(P)[3]:=J;
     repeat
      dec(I,4);
      PIntegerArray(P)[I]:=J;
      PIntegerArray(P)[I+1]:=J;
      PIntegerArray(P)[I+2]:=J;
      PIntegerArray(P)[I+3]:=J;
     until I<4;
    end else begin
     I:=Count;
     K:=(longint(P) and 3)-4;
     dec(I,16);
     dec(pbyte(P),K);
     inc(I,K);
     inc(pbyte(P),I);
     PIntegerArray(P)[0] := J;
     PIntegerArray(P)[1] := J;
     PIntegerArray(P)[2] := J;
     PIntegerArray(P)[3] := J;
     repeat
      PIntegerArray(longint(P)-I)[0]:=J;
      PIntegerArray(longint(P)-I)[1]:=J;
      PIntegerArray(longint(P)-I)[2]:=J;
      PIntegerArray(longint(P)-I)[3]:=J;
      dec(I,16);
     until I<=0;
    end;
   end else begin
    repeat
     dec(I,2);
     PIntegerArray(P)[I]:=J;
     PIntegerArray(P)[I+1]:=J;
    until I<2;
   end;
  end else begin
   case Count of
    1:goto P01;
    2:goto P02;
    3:goto P03;
    4:goto P04;
    5:goto P05;
    6:goto P06;
    7:goto P07;
    8:goto P08;
    9:goto P09;
    10:goto P10;
    11:goto P11;
    12:goto P12;
   end;
   P12:PByteArray(P)[11]:=byte(Value);
   P11:PByteArray(P)[10]:=byte(Value);
   P10:PByteArray(P)[09]:=byte(Value);
   P09:PByteArray(P)[08]:=byte(Value);
   P08:PByteArray(P)[07]:=byte(Value);
   P07:PByteArray(P)[06]:=byte(Value);
   P06:PByteArray(P)[05]:=byte(Value);
   P05:PByteArray(P)[04]:=byte(Value);
   P04:PByteArray(P)[03]:=byte(Value);
   P03:PByteArray(P)[02]:=byte(Value);
   P02:PByteArray(P)[01]:=byte(Value);
   P01:PByteArray(P)[00]:=byte(Value);
  end;
 end;
end;

function AreBytesEqual(const A,B;Count:longint):boolean;
var FirstComparePointer,SecondComparePointer:pbyte;
    Counter:longint;
begin
 try
  result:=true;
  FirstComparePointer:=@A;
  SecondComparePointer:=@B;
  for Counter:=1 to Count do begin
   if FirstComparePointer^<>SecondComparePointer^ then begin
    result:=false;
    exit;
   end;
   inc(FirstComparePointer);
   inc(SecondComparePointer);
  end;
 except
  result:=false;
 end;
end;

function GetNoteDuration(NoteType,ShortestTime:longint;Dots:longint=0):longint;
var Duration,Extra,DotCouunter:longint;
begin
 Duration:=ShortestTime*(1 shl NoteType);
 if Dots<>0 then begin
  Extra:=Duration div 2;
  for DotCouunter:=1 to Dots do begin
   inc(Duration,Extra);
   Extra:=Extra div 2;
  end;
 end;
 result:=Duration;
end;

function GCD(A,B:longint):longint;
begin
 if A=0 then B:=A;
 if B=0 then A:=B;
 while A<>B do begin
  if A>B then dec(A,B);
  if B>A then dec(B,A);
 end;
 if A=0 then A:=1;
 result:=A;
end;

function Swap16(const Value:word):word;
begin
{$IFDEF CPU386}
 result:=((Value and $ff) shl 8) or ((Value and $ff00) shr 8);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

function Swap32(const Value:longword):longword;
begin
{$IFDEF CPU386}
 result:=((Value and $ff) shl 24) or (((Value and $ff00) shr 8) shl 16) or (((Value and $ff0000) shr 16) shl 8) or ((Value and $ff000000) shr 24);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

function SwapWordLittleEndian(Value:word):word; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
begin
{$IFDEF BIG_ENDIAN}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

function SwapDWordLittleEndian(Value:longword):longword; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
begin
{$IFDEF BIG_ENDIAN}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

function SwapWordBigEndian(Value:word):word; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
begin
{$IFDEF LITTLE_ENDIAN}
 result:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

function SwapDWordBigEndian(Value:longword):longword; {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
begin
{$IFDEF LITTLE_ENDIAN}
 result:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
         ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$ELSE}
 result:=Value;
{$ENDIF}
end;

procedure SwapLittleEndianData16(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
{$IFDEF BIG_ENDIAN}
var Value:word absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$ELSE}
begin
{$ENDIF}
end;

procedure SwapLittleEndianData32(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
{$IFDEF BIG_ENDIAN}
var Value:longword absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$ELSE}
begin
{$ENDIF}
end;

procedure SwapBigEndianData16(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
{$IFDEF LITTLE_ENDIAN}
var Value:word absolute Data;
begin
 Value:=((Value and $ff00) shr 8) or ((Value and $ff) shl 8);
{$ELSE}
begin
 result:=Value;
{$ENDIF}
end;

procedure SwapBigEndianData32(var Data); {$IFDEF FPC}{INLINE;}{$ELSE}register;{$ENDIF}
{$IFDEF LITTLE_ENDIAN}
var Value:longword absolute Data;
begin
 Value:=((Value and $ff000000) shr 24) or ((Value and $00ff0000) shr 8) or
        ((Value and $0000ff00) shl 8) or ((Value and $000000ff) shl 24);
{$ELSE}
begin
{$ENDIF}
end;

function DecToHex(Value:int64):ansistring;
begin
 result:='';
 while Value<>0 do begin
  result:=HexNumbers[Value and $f]+result;
  Value:=Value shr 4;
 end;
end;

function HexToDec(Value:ansistring):int64;
var Counter:longint;
    Nibble:byte;
begin
 result:=0;
 for Counter:=length(Value) downto 1 do begin
  Nibble:=byte(Value[Counter]);
  if (Nibble>=byte('0')) and (Nibble<=byte('9')) then begin
   Nibble:=Nibble-byte('0');
  end else if (Nibble>=byte('A')) and (Nibble<=byte('F')) then begin
   Nibble:=Nibble-byte('A')+$a;
  end else if (Nibble>=byte('a')) and (Nibble<=byte('f')) then begin
   Nibble:=Nibble-byte('a')+$a;
  end else begin
   Nibble:=0;
  end;
  result:=(result shl 4) or Nibble;
 end;
end;

function ByteToHex(Value:byte):ansistring;
begin
 result:='';
 while Value<>0 do begin
  result:=HexNumbers[Value and $f]+result;
  Value:=Value shr 4;
 end;
 while length(result)<2 do result:='0'+result;
end;

function HexToByte(Value:ansistring):byte;
var Counter:longint;
    Nibble:byte;
begin
 result:=0;
 for Counter:=length(Value) downto 1 do begin
  Nibble:=byte(Value[Counter]);
  if (Nibble>=byte('0')) and (Nibble<=byte('9')) then begin
   Nibble:=Nibble-byte('0');
  end else if (Nibble>=byte('A')) and (Nibble<=byte('F')) then begin
   Nibble:=Nibble-byte('A')+$a;
  end else if (Nibble>=byte('a')) and (Nibble<=byte('f')) then begin
   Nibble:=Nibble-byte('a')+$a;
  end else begin
   Nibble:=0;
  end;
  result:=(result shl 4) or Nibble;
 end;
end;

function WordToHex(Value:word):ansistring;
begin
 result:='';
 while Value<>0 do begin
  result:=HexNumbers[Value and $f]+result;
  Value:=Value shr 4;
 end;
 while length(result)<4 do result:='0'+result;
end;

function HexToWord(Value:ansistring):word;
var Counter:longint;
    Nibble:byte;
begin
 result:=0;
 for Counter:=length(Value) downto 1 do begin
  Nibble:=byte(Value[Counter]);
  if (Nibble>=byte('0')) and (Nibble<=byte('9')) then begin
   Nibble:=Nibble-byte('0');
  end else if (Nibble>=byte('A')) and (Nibble<=byte('F')) then begin
   Nibble:=Nibble-byte('A')+$a;
  end else if (Nibble>=byte('a')) and (Nibble<=byte('f')) then begin
   Nibble:=Nibble-byte('a')+$a;
  end else begin
   Nibble:=0;
  end;
  result:=(result shl 4) or Nibble;
 end;
end;

function LongWordToHex(Value:longword):ansistring; register;
begin
 result:='';
 while Value<>0 do begin
  result:=HexNumbers[Value and $f]+result;
  Value:=Value shr 4;
 end;
 while length(result)<8 do result:='0'+result;
end;

function HexToLongWord(Value:ansistring):longword; register;
var Counter:longint;
    Nibble:byte;
begin
 result:=0;
 for Counter:=length(Value) downto 1 do begin
  Nibble:=byte(Value[Counter]);
  if (Nibble>=byte('0')) and (Nibble<=byte('9')) then begin
   Nibble:=Nibble-byte('0');
  end else if (Nibble>=byte('A')) and (Nibble<=byte('F')) then begin
   Nibble:=Nibble-byte('A')+$a;
  end else if (Nibble>=byte('a')) and (Nibble<=byte('f')) then begin
   Nibble:=Nibble-byte('a')+$a;
  end else begin
   Nibble:=0;
  end;
  result:=(result shl 4) or Nibble;
 end;
end;

function FABS(Value:single):single;
var L:longword absolute Value;
begin
 L:=L and $7fffffff;
 result:=Value;
end;

end.

