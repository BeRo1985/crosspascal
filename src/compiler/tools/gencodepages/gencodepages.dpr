program gencodepages;
{$APPTYPE CONSOLE}
uses
  Windows,
  SysUtils,
  ActiveX,
  Registry,
  Classes;

type _cpinfoex=record
      MaxCharSize:UINT; { max length (bytes) of a char }
      DefaultChar:array[0..MAX_DEFAULTCHAR-1] of Byte; { default character }
      LeadByte:array[0..MAX_LEADBYTES-1] of Byte; { lead byte ranges }
      UnicodeDefaultChar:WCHAR;
      CodePage:UINT;
      CodePageName:array[0..MAX_PATH] of char;
     end;
     TCPInfoEx=_cpinfoex;
     CPINFOEX=_cpinfoex;

var s:ansistring;
    cp,i:longint;
    w:widestring;
    u,supported:array[word] of boolean;
    SubSubPages:array[word] of boolean;
    SubPages:array[word] of boolean;
    CodePageNames:array[word] of ansistring;
    t:text;

function SetThreadUILanguage(LangId:WORD):WORD; stdcall; external 'kernel32.dll' name 'SetThreadUILanguage';

function GetCPInfoEx(CodePage:UINT; dwFlags:DWORD; var lpCPInfoEx:TCPInfoEx):BOOL; stdcall; external 'kernel32.dll' name 'GetCPInfoExA';

function CpEnumProc(CodePage:PChar):Cardinal; stdcall;
var CpInfoEx:TCPInfoEx;
    Cp:cardinal;
begin
 Cp:=StrToIntDef(CodePage,0);
 if IsValidCodePage(Cp) then begin
  supported[cp]:=true;
  GetCPInfoEx(Cp,0,CpInfoEx);
  CodePageNames[cp]:=CpInfoEx.CodePageName;
 end;
 result:=1;
end;

function MakeLangID(P,S:Word):Word;
begin
 result:=(S shl 10) or P;
end;

function AnsiStringEscape(const Input:ansistring):ansistring;
var Counter:longint;
    c:ansichar;
begin
 result:='''';
 for Counter:=1 to length(Input) do begin
  C:=Input[Counter];
  case C of
   'A'..'Z','a'..'z',' ','0'..'9','_','-','(',')','[',']','{','}','!','?','&','$','\','/','=','+','"','%','#','*',':',';',',','.':begin
    result:=result+c;
   end;
   else begin
    result:=result+'''#$'+IntToHex(byte(C),2)+'''';
   end;
  end;
 end;
 result:=result+'''';
end;

begin
 SetThreadUILanguage(MAKELANGID(LANG_ENGLISH,SUBLANG_DEFAULT));
 FillChar(supported,sizeof(supported),#0);
 FillChar(CodePageNames,sizeof(CodePageNames),#0);
 FillChar(SubSubPages,sizeof(SubSubPages),#0);
 FillChar(SubPages,sizeof(SubPages),#0);
 CoInitialize(nil);
 EnumSystemCodePages(@CpEnumProc,CP_SUPPORTED);
 s:='';
 for i:=1 to 256 do begin
  s:=s+chr(i-1);
 end;
 assignfile(t,'CodePages.pas');
 rewrite(t);
 writeln(t,'unit CodePages;');
 writeln(t,'{$ifdef fpc}');
 writeln(t,' {$mode delphi}');
 writeln(t,'{$endif}');
 writeln(t,'interface');
 writeln(t,'type PCharSetCodePage=^TCharSetCodePage;');
 writeln(t,'     TCharSetCodePage=array[0..255] of word;');
 writeln(t,'     PCharSetSubSubCodePages=^TCharSetSubSubCodePages;');
 writeln(t,'     TCharSetSubSubCodePages=array[0..15] of PCharSetCodePage;');
 writeln(t,'     PCharSetSubCodePages=^TCharSetSubCodePages;');
 writeln(t,'     TCharSetSubCodePages=array[0..15] of PCharSetSubSubCodePages;');
 writeln(t,'     PCharSetCodePages=^TCharSetCodePages;');
 writeln(t,'     TCharSetCodePages=array[0..255] of PCharSetSubCodePages;');
 writeln(t,'     PCharSetSubSubCodePageNames=^TCharSetSubSubCodePageNames;');
 writeln(t,'     TCharSetSubSubCodePageNames=array[0..15] of ansistring;');
 writeln(t,'     PCharSetSubCodePageNames=^TCharSetSubCodePageNames;');
 writeln(t,'     TCharSetSubCodePageNames=array[0..15] of PCharSetSubSubCodePageNames;');
 writeln(t,'     PCharSetCodePageNames=^TCharSetCodePageNames;');
 writeln(t,'     TCharSetCodePageNames=array[0..255] of PCharSetSubCodePageNames;');
 SetLength(w,256*2);
 for cp:=0 to 65535 do begin
  u[cp]:=false;
  if (cp<>65001) and supported[cp] then begin
   SubSubPages[cp shr 4]:=true;
   SubPages[cp shr 8]:=true;
   i:=MultiByteToWideChar(cp,0,PAnsiChar(s),256,PWideChar(w),512);
   if i=256 then begin
    u[cp]:=true;
    write(t,'const CharSetCodePage',cp,':TCharSetCodePage=(');
    writeln(t);
    for i:=1 to 256 do begin
     write(t,'$',IntToHex(ord(w[i]),4));
     if i<>256 then begin
      write(t,',');
     end else begin
      write(t,');');
     end;
     if (i and 7)=0 then begin
      writeln(t);
     end;
    end;
   end;
  end;
 end;
 for i:=0 to (256*16)-1 do begin
  if SubSubPages[i] then begin
   writeln(t,'const CharSetSubSubCodePage',i,':TCharSetSubSubCodePages=(');
   for cp:=(i*16) to (i*16)+15 do begin
    if u[cp] then begin
     write(t,'@CharSetCodePage',cp);
    end else begin
     write(t,'nil');
    end;
    if cp<>((i*16)+15) then begin
     write(t,',');
    end;
    writeln(t);
   end;
   writeln(t,');');
  end;
 end;
 for i:=0 to 255 do begin
  if SubPages[i] then begin
   writeln(t,'const CharSetCodeSubPage',i,':TCharSetSubCodePages=(');
   for cp:=(i*16) to (i*16)+15 do begin
    if SubSubPages[cp] then begin
     write(t,'@CharSetSubSubCodePage',cp);
    end else begin
     write(t,'nil');
    end;
    if cp<>((i*16)+15) then begin
     write(t,',');
    end;
    writeln(t);
   end;
   writeln(t,');');
  end;
 end;
 writeln(t,'const CharSetCodePages:TCharSetCodePages=(');
 for i:=0 to 256-1 do begin
  if SubPages[i] then begin
   write(t,'@CharSetCodeSubPage',i);
  end else begin
   write(t,'nil');
  end;
  if i<>255 then begin
   write(t,',');
  end;
  writeln(t);
 end;
 writeln(t,');');
{writeln(t,'const CharSetCodePageNames:TCharSetCodePageNames=(');
 for cp:=0 to 65535 do begin
  if u[cp] then begin
   write(t,AnsiStringEscape(CodePageNames[cp]));
  end else begin
   write(t,'''''');
  end;
  if cp<>65535 then begin
   write(t,',');
  end;
  writeln(t);
 end;
 writeln(t,');');}
 for i:=0 to (256*16)-1 do begin
  if SubSubPages[i] then begin
   writeln(t,'const CharSetSubSubCodePageNames',i,':TCharSetSubSubCodePageNames=(');
   for cp:=(i*16) to (i*16)+15 do begin
    if u[cp] then begin
     write(t,AnsiStringEscape(CodePageNames[cp]));
    end else begin
     write(t,'''''');
    end;
    if cp<>((i*16)+15) then begin
     write(t,',');
    end;
    writeln(t);
   end;
   writeln(t,');');
  end;
 end;
 for i:=0 to 255 do begin
  if SubPages[i] then begin
   writeln(t,'const CharSetCodeSubPageNames',i,':TCharSetSubCodePageNames=(');
   for cp:=(i*16) to (i*16)+15 do begin
    if SubSubPages[cp] then begin
     write(t,'@CharSetSubSubCodePageNames',cp);
    end else begin
     write(t,'nil');
    end;
    if cp<>((i*16)+15) then begin
     write(t,',');
    end;
    writeln(t);
   end;
   writeln(t,');');
  end;
 end;
 writeln(t,'const CharSetCodePageNames:TCharSetCodePageNames=(');
 for i:=0 to 256-1 do begin
  if SubPages[i] then begin
   write(t,'@CharSetCodeSubPageNames',i);
  end else begin
   write(t,'nil');
  end;
  if i<>255 then begin
   write(t,',');
  end;
  writeln(t);
 end;
 writeln(t,');');
 writeln(t,'implementation');
 writeln(t,'end.');
 closefile(t);
end.
