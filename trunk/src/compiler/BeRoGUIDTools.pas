unit BeRoGUIDTools;
{$ifdef fpc}
 {$mode delphi}
 {$ifdef cpui386}
  {$define cpu386}
 {$endif}
 {$ifdef cpu386}
  {$asmmode intel}
 {$endif}
 {$ifdef cpuamd64}
  {$asmmode intel}
 {$endif}
 {$ifdef FPC_LITTLE_ENDIAN}
  {$define LITTLE_ENDIAN}
 {$else}
  {$ifdef FPC_BIG_ENDIAN}
   {$define BIG_ENDIAN}
  {$endif}
 {$endif}
 {-$pic off}
 {$define caninline}
 {$ifdef FPC_HAS_TYPE_EXTENDED}
  {$define HAS_TYPE_EXTENDED}
 {$else}
  {$undef HAS_TYPE_EXTENDED}
 {$endif}
 {$ifdef FPC_HAS_TYPE_DOUBLE}
  {$define HAS_TYPE_DOUBLE}
 {$else}
  {$undef HAS_TYPE_DOUBLE}
 {$endif}
 {$ifdef FPC_HAS_TYPE_SINGLE}
  {$define HAS_TYPE_SINGLE}
 {$else}
  {$undef HAS_TYPE_SINGLE}
 {$endif}
{$else}
 {$realcompatibility off}
 {$localsymbols on}
 {$define LITTLE_ENDIAN}
 {$ifndef cpu64}
  {$define cpu32}
 {$endif}
 {$define HAS_TYPE_EXTENDED}
 {$define HAS_TYPE_DOUBLE}
 {$define HAS_TYPE_SINGLE}
{$endif}
{$ifdef win32}
 {$define windows}
{$endif}
{$ifdef win64}
 {$define windows}
{$endif}
{$ifdef wince}
 {$define windows}
{$endif}
{$rangechecks off}
{$extendedsyntax on}
{$writeableconst on}
{$hints off}
{$booleval off}
{$typedaddress off}
{$stackframes off}
{$varstringchecks on}
{$typeinfo on}
{$overflowchecks off}
{$longstrings on}
{$openstrings on}

interface

function AreGUIDsEqual(const A,B:TGUID):boolean;
function StringToGUID(GUID:ansistring):TGUID;
function GUIDToString(const GUID:TGUID):ansistring;

implementation

function StringToHex(Hex:ansistring):longword;
var Counter:longint;
    Nibble:byte;
    C:ansichar;
begin
 result:=0;
 for Counter:=1 to length(Hex) do begin
  C:=Hex[Counter];
  case C of
   'A'..'F':begin
    Nibble:=(byte(C)-ord('A'))+$a;
   end;
   'a'..'f':begin
    Nibble:=(byte(C)-ord('a'))+$a;
   end;
   '0'..'9':begin
    Nibble:=(byte(C)-ord('0'));
   end;
   else begin
    Nibble:=0;
   end;
  end;
  result:=(result shl 4) or Nibble;
 end;
end;

function HexToString(Value:longword;MinSize:longint):ansistring;
var Nibble:byte;
begin
 result:='';
 while Value>0 do begin
  Nibble:=Value and $f;
  case Nibble of
   $0..$9:begin
    result:=ansichar(Nibble+ord('0'))+result;
   end;
   $a..$f:begin
    result:=ansichar((Nibble-$a)+ord('A'))+result;
   end;
  end;
  Value:=Value shr 4;
 end;
 while length(result)<MinSize do begin
  result:='0'+result;
 end;
end;

function AreGUIDsEqual(const A,B:TGUID):boolean;
begin
 result:=(A.D1=B.D1) and (A.D2=B.D2) and (A.D3=B.D3) and (A.D4[0]=B.D4[0]) and
         (A.D4[1]=B.D4[1]) and (A.D4[2]=B.D4[2]) and (A.D4[3]=B.D4[3]) and
         (A.D4[4]=B.D4[4]) and (A.D4[5]=B.D4[5]) and (A.D4[6]=B.D4[6]) and
         (A.D4[7]=B.D4[7]);
end;

function StringToGUID(GUID:ansistring):TGUID;
begin
 if (length(GUID)=38) and (GUID[1]='{') and (GUID[10]='-') and
    (GUID[15]='-') and (GUID[20]='-') and (GUID[25]='-') and
    (GUID[38]='}') then begin
  result.D1:=StringToHex(copy(GUID,2,8));
  result.D2:=StringToHex(copy(GUID,11,4));
  result.D3:=StringToHex(copy(GUID,16,4));
  result.D4[0]:=StringToHex(copy(GUID,21,2));
  result.D4[1]:=StringToHex(copy(GUID,23,2));
  result.D4[2]:=StringToHex(copy(GUID,26,2));
  result.D4[3]:=StringToHex(copy(GUID,28,2));
  result.D4[4]:=StringToHex(copy(GUID,30,2));
  result.D4[5]:=StringToHex(copy(GUID,32,2));
  result.D4[6]:=StringToHex(copy(GUID,34,2));
  result.D4[7]:=StringToHex(copy(GUID,36,2));
 end else begin
  FillChar(result,SizeOf(TGUID),#0);
 end;
end;

function GUIDToString(const GUID:TGUID):ansistring;
begin
 result:='{'+HexToString(GUID.D1,8)+'-'+HexToString(GUID.D2,4)+'-'+HexToString(GUID.D3,4)+'-'+HexToString(GUID.D4[0],2)+HexToString(GUID.D4[1],2)+'-'+HexToString(GUID.D4[2],2)+HexToString(GUID.D4[3],2)+HexToString(GUID.D4[4],2)+HexToString(GUID.D4[5],2)+HexToString(GUID.D4[6],2)+HexToString(GUID.D4[7],2)+'}';
end;

end.
