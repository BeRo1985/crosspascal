unit BeRoDoubleToString;
(**********************************************************
** BeRo Double To String Conversion Library               *
***********************************************************
**
** This file is part of the BeRo Double To String Conversion Library.
** Copyright (C) 2011-2012 by Benjamin Rosseaux
**
** The source code of the BeRo Double To String Conversion Library and helper tools are
** distributed under the Library GNU General Public License
** (see the file COPYING) with the following modification:
**
** As a special exception, the copyright holders of this library give you
** permission to link this library with independent modules to produce an
** executable, regardless of the license terms of these independent modules,
** and to copy and distribute the resulting executable under terms of your choice,
** provided that you also meet, for each linked independent module, the terms
** and conditions of the license of that module. An independent module is a module
** which is not derived from or based on this library. If you modify this
** library, you may extend this exception to your version of the library, but you are
** not obligated to do so. If you do not wish to do so, delete this exception
** statement from your version.
**
** If you didn't receive a copy of the file COPYING, contact:
**      Free Software Foundation
**      675 Mass Ave
**      Cambridge, MA  02139
**      USA
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
**
*)
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
{$ifndef HAS_TYPE_DOUBLE}
 {$error No double floating point precision}
{$endif}

interface

const bdtsmSTANDARD=0;
      bdtsmSTANDARDEXPONENTIAL=1;
      bdtsmFIXED=2;
      bdtsmEXPONENTIAL=3;
      bdtsmPRECISION=4;
      bdtsmRADIX=5;

function BeRoConvertDoubleToString(const AValue:double;Mode,RequestedDigits:longint):ansistring;

implementation

uses SysUtils,Math;

type PBeRoINT64=^TBeRoINT64;
     TBeRoINT64=int64;

{$ifdef fpc}
 {$undef OldDelphi}
{$else}
 {$ifdef conditionalexpressions}
  {$if CompilerVersion>=23.0}
   {$undef OldDelphi}
     qword=uint64;
     ptruint=NativeUInt;
     ptrint=NativeInt;
  {$else}
   {$define OldDelphi}
  {$ifend}
 {$else}
  {$define OldDelphi}
 {$endif}
{$endif}
{$ifdef OldDelphi}
     qword=int64;
{$ifdef cpu64}
     ptruint=qword;
     ptrint=int64;
{$else}
     ptruint=longword;
     ptrint=longint;
{$endif}
{$endif}

{$ifdef fpc}
     PBeRoQWORD=^TBeRoQWORD;
     TBeRoQWORD=qword;
{$endif}

     PBeRoDoubleHiLo=^TBeRoDoubleHiLo;
     TBeRoDoubleHiLo=packed record
{$ifdef BIG_ENDIAN}
      Hi,Lo:longword;
{$else}
      Lo,Hi:longword;
{$endif}
     end;

     PBeRoDoubleBytes=^TBeRoDoubleBytes;
     TBeRoDoubleBytes=array[0..sizeof(double)-1] of byte;

{$ifdef cpu64}
function BeRoIsNaN(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBeRoINT64(@AValue)^ and $7ff0000000000000)=$7ff0000000000000) and ((PBeRoINT64(@AValue)^ and $000fffffffffffff)<>$0000000000000000);
end;

function BeRoIsInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoINT64(@AValue)^ and $7fffffffffffffff)=$7ff0000000000000;
end;

function BeRoIsFinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoINT64(@AValue)^ and $7ff0000000000000)<>$7ff0000000000000;
end;

function BeRoIsPosInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PBeRoINT64(@AValue)^=int64($7ff0000000000000);
end;

function BeRoIsNegInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=qword(pointer(@AValue)^)=qword($fff0000000000000);
{$else}
 result:=PBeRoINT64(@AValue)^=int64($fff0000000000000);
{$endif}
end;

function BeRoIsPosZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=PBeRoINT64(@AValue)^=int64($0000000000000000);
end;

function BeRoIsNegZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=qword(pointer(@AValue)^)=qword($8000000000000000);
{$else}
 result:=PBeRoINT64(@AValue)^=int64($8000000000000000);
{$endif}
end;

function BeRoIsZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(qword(pointer(@AValue)^) and qword($7fffffffffffffff))=qword($0000000000000000);
{$else}
 result:=(PBeRoINT64(@AValue)^ and int64($7fffffffffffffff))=int64($0000000000000000);
{$endif}
end;

function BeRoIsNegative(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
{$ifdef fpc}
 result:=(qword(pointer(@AValue)^) and qword($8000000000000000))<>0;
{$else}
 result:=(PBeRoINT64(@AValue)^ shr 63)<>0;
{$endif}
end;
{$else}
{$ifdef TrickyNumberChecks}
function BeRoIsNaN(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
var l:longword;
begin
 l:=PBeRoDoubleHiLo(@AValue)^.Lo;
 result:=(longword($7ff00000-longword(longword(PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff) or ((l or (-l)) shr 31))) shr 31)<>0;
end;

function BeRoIsInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword((longword(PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff) xor $7ff00000) or PBeRoDoubleHiLo(@AValue)^.Lo)=0;
end;

function BeRoIsFinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(longword((PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff)-$7ff00000) shr 31)<>0;
end;

function BeRoIsPosInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBeRoDoubleHiLo(@AValue)^.Hi;
 result:=longword(((longword(h and $7fffffff) xor $7ff00000) or PBeRoDoubleHiLo(@AValue)^.Lo) or longword(h shr 31))=0;
end;

function BeRoIsNegInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBeRoDoubleHiLo(@AValue)^.Hi;
 result:=longword(((longword(h and $7fffffff) xor $7ff00000) or PBeRoDoubleHiLo(@AValue)^.Lo) or longword(longword(not h) shr 31))=0;
end;

function BeRoIsPosZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBeRoDoubleHiLo(@AValue)^.Hi;
 result:=longword(longword(longword(h and $7fffffff) or PBeRoDoubleHiLo(@AValue)^.Lo) or longword(h shr 31))=0;
end;

function BeRoIsNegZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
var h:longword;
begin
 h:=PBeRoDoubleHiLo(@AValue)^.Hi;
 result:=longword(longword(longword(h and $7fffffff) or PBeRoDoubleHiLo(@AValue)^.Lo) or longword(longword(not h) shr 31))=0;
end;

function BeRoIsZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(longword(PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff) or PBeRoDoubleHiLo(@AValue)^.Lo)=0;
end;

function BeRoIsNegative(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=longword(PBeRoDoubleHiLo(@AValue)^.Hi and longword($80000000))<>0;
end;
{$else}
function BeRoIsNaN(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBeRoDoubleHiLo(@AValue)^.Hi and $7ff00000)=$7ff00000) and (((PBeRoDoubleHiLo(@AValue)^.Hi and $000fffff) or PBeRoDoubleHiLo(@AValue)^.Lo)<>0);
end;

function BeRoIsInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff)=$7ff00000) and (PBeRoDoubleHiLo(@AValue)^.Lo=0);
end;

function BeRoIsFinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi and $7ff00000)<>$7ff00000;
end;

function BeRoIsPosInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi=$7ff00000) and (PBeRoDoubleHiLo(@AValue)^.Lo=0);
end;

function BeRoIsNegInfinite(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi=$fff00000) and (PBeRoDoubleHiLo(@AValue)^.Lo=0);
end;

function BeRoIsPosZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi or PBeRoDoubleHiLo(@AValue)^.Lo)=0;
end;

function BeRoIsNegZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi=$80000000) and (PBeRoDoubleHiLo(@AValue)^.Lo=0);
end;

function BeRoIsZero(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=((PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff) or PBeRoDoubleHiLo(@AValue)^.Lo)=0;
end;

function BeRoIsNegative(const AValue:double):boolean; {$ifdef caninline}inline;{$endif}
begin
 result:=(PBeRoDoubleHiLo(@AValue)^.Hi and $80000000)<>0;
end;
{$endif}
{$endif}

function BeRoDoubleAbsolute(const AValue:double):double; {$ifdef caninline}inline;{$endif}
begin
{$ifdef cpu64}
 PBeRoINT64(@result)^:=PBeRoINT64(@AValue)^ and $7fffffffffffffff;
{$else}
 PBeRoDoubleHiLo(@result)^.Hi:=PBeRoDoubleHiLo(@AValue)^.Hi and $7fffffff;
 PBeRoDoubleHiLo(@result)^.Lo:=PBeRoDoubleHiLo(@AValue)^.Lo;
{$endif}
end;

const BeRoDoubleToStringPowerOfTenTable:array[0..86,0..2] of int64=((int64($fa8fd5a0081c0288),-1220,-348),
                                                                    (int64($baaee17fa23ebf76),-1193,-340),
                                                                    (int64($8b16fb203055ac76),-1166,-332),
                                                                    (int64($cf42894a5dce35ea),-1140,-324),
                                                                    (int64($9a6bb0aa55653b2d),-1113,-316),
                                                                    (int64($e61acf033d1a45df),-1087,-308),
                                                                    (int64($ab70fe17c79ac6ca),-1060,-300),
                                                                    (int64($ff77b1fcbebcdc4f),-1034,-292),
                                                                    (int64($be5691ef416bd60c),-1007,-284),
                                                                    (int64($8dd01fad907ffc3c),-980,-276),
                                                                    (int64($d3515c2831559a83),-954,-268),
                                                                    (int64($9d71ac8fada6c9b5),-927,-260),
                                                                    (int64($ea9c227723ee8bcb),-901,-252),
                                                                    (int64($aecc49914078536d),-874,-244),
                                                                    (int64($823c12795db6ce57),-847,-236),
                                                                    (int64($c21094364dfb5637),-821,-228),
                                                                    (int64($9096ea6f3848984f),-794,-220),
                                                                    (int64($d77485cb25823ac7),-768,-212),
                                                                    (int64($a086cfcd97bf97f4),-741,-204),
                                                                    (int64($ef340a98172aace5),-715,-196),
                                                                    (int64($b23867fb2a35b28e),-688,-188),
                                                                    (int64($84c8d4dfd2c63f3b),-661,-180),
                                                                    (int64($c5dd44271ad3cdba),-635,-172),
                                                                    (int64($936b9fcebb25c996),-608,-164),
                                                                    (int64($dbac6c247d62a584),-582,-156),
                                                                    (int64($a3ab66580d5fdaf6),-555,-148),
                                                                    (int64($f3e2f893dec3f126),-529,-140),
                                                                    (int64($b5b5ada8aaff80b8),-502,-132),
                                                                    (int64($87625f056c7c4a8b),-475,-124),
                                                                    (int64($c9bcff6034c13053),-449,-116),
                                                                    (int64($964e858c91ba2655),-422,-108),
                                                                    (int64($dff9772470297ebd),-396,-100),
                                                                    (int64($a6dfbd9fb8e5b88f),-369,-92),
                                                                    (int64($f8a95fcf88747d94),-343,-84),
                                                                    (int64($b94470938fa89bcf),-316,-76),
                                                                    (int64($8a08f0f8bf0f156b),-289,-68),
                                                                    (int64($cdb02555653131b6),-263,-60),
                                                                    (int64($993fe2c6d07b7fac),-236,-52),
                                                                    (int64($e45c10c42a2b3b06),-210,-44),
                                                                    (int64($aa242499697392d3),-183,-36),
                                                                    (int64($fd87b5f28300ca0e),-157,-28),
                                                                    (int64($bce5086492111aeb),-130,-20),
                                                                    (int64($8cbccc096f5088cc),-103,-12),
                                                                    (int64($d1b71758e219652c),-77,-4),
                                                                    (int64($9c40000000000000),-50,4),
                                                                    (int64($e8d4a51000000000),-24,12),
                                                                    (int64($ad78ebc5ac620000),3,20),
                                                                    (int64($813f3978f8940984),30,28),
                                                                    (int64($c097ce7bc90715b3),56,36),
                                                                    (int64($8f7e32ce7bea5c70),83,44),
                                                                    (int64($d5d238a4abe98068),109,52),
                                                                    (int64($9f4f2726179a2245),136,60),
                                                                    (int64($ed63a231d4c4fb27),162,68),
                                                                    (int64($b0de65388cc8ada8),189,76),
                                                                    (int64($83c7088e1aab65db),216,84),
                                                                    (int64($c45d1df942711d9a),242,92),
                                                                    (int64($924d692ca61be758),269,100),
                                                                    (int64($da01ee641a708dea),295,108),
                                                                    (int64($a26da3999aef774a),322,116),
                                                                    (int64($f209787bb47d6b85),348,124),
                                                                    (int64($b454e4a179dd1877),375,132),
                                                                    (int64($865b86925b9bc5c2),402,140),
                                                                    (int64($c83553c5c8965d3d),428,148),
                                                                    (int64($952ab45cfa97a0b3),455,156),
                                                                    (int64($de469fbd99a05fe3),481,164),
                                                                    (int64($a59bc234db398c25),508,172),
                                                                    (int64($f6c69a72a3989f5c),534,180),
                                                                    (int64($b7dcbf5354e9bece),561,188),
                                                                    (int64($88fcf317f22241e2),588,196),
                                                                    (int64($cc20ce9bd35c78a5),614,204),
                                                                    (int64($98165af37b2153df),641,212),
                                                                    (int64($e2a0b5dc971f303a),667,220),
                                                                    (int64($a8d9d1535ce3b396),694,228),
                                                                    (int64($fb9b7cd9a4a7443c),720,236),
                                                                    (int64($bb764c4ca7a44410),747,244),
                                                                    (int64($8bab8eefb6409c1a),774,252),
                                                                    (int64($d01fef10a657842c),800,260),
                                                                    (int64($9b10a4e5e9913129),827,268),
                                                                    (int64($e7109bfba19c0c9d),853,276),
                                                                    (int64($ac2820d9623bf429),880,284),
                                                                    (int64($80444b5e7aa7cf85),907,292),
                                                                    (int64($bf21e44003acdd2d),933,300),
                                                                    (int64($8e679c2f5e44ff8f),960,308),
                                                                    (int64($d433179d9c8cb841),986,316),
                                                                    (int64($9e19db92b4e31ba9),1013,324),
                                                                    (int64($eb96bf6ebadf77d9),1039,332),
                                                                    (int64($af87023b9bf0ee6b),1066,340));

      BeRoDoubleToStringPowerOfTenBinaryExponentTable:array[-1220..(1066+27)-1] of byte=(0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                                                         1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,
                                                                                         2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                                                         2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,
                                                                                         3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                                                         3,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
                                                                                         4,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,
                                                                                         5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
                                                                                         5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,
                                                                                         6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
                                                                                         6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
                                                                                         7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,
                                                                                         8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,
                                                                                         8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,
                                                                                         9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,
                                                                                         9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,
                                                                                         10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,
                                                                                         11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
                                                                                         11,11,11,11,11,11,12,12,12,12,12,12,12,12,12,12,
                                                                                         12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,
                                                                                         13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
                                                                                         13,13,13,13,13,13,13,13,13,13,13,14,14,14,14,14,
                                                                                         14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,14,
                                                                                         14,14,14,14,14,14,15,15,15,15,15,15,15,15,15,15,
                                                                                         15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,15,
                                                                                         16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,16,
                                                                                         16,16,16,16,16,16,16,16,16,16,16,17,17,17,17,17,
                                                                                         17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,17,
                                                                                         17,17,17,17,17,18,18,18,18,18,18,18,18,18,18,18,
                                                                                         18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,
                                                                                         19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,19,
                                                                                         19,19,19,19,19,19,19,19,19,19,20,20,20,20,20,20,
                                                                                         20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                                                                                         20,20,20,20,20,21,21,21,21,21,21,21,21,21,21,21,
                                                                                         21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,21,
                                                                                         22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,22,
                                                                                         22,22,22,22,22,22,22,22,22,22,23,23,23,23,23,23,
                                                                                         23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,23,
                                                                                         23,23,23,23,23,24,24,24,24,24,24,24,24,24,24,24,
                                                                                         24,24,24,24,24,24,24,24,24,24,24,24,24,24,24,25,
                                                                                         25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,25,
                                                                                         25,25,25,25,25,25,25,25,25,25,26,26,26,26,26,26,
                                                                                         26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,26,
                                                                                         26,26,26,26,27,27,27,27,27,27,27,27,27,27,27,27,
                                                                                         27,27,27,27,27,27,27,27,27,27,27,27,27,27,27,28,
                                                                                         28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,28,
                                                                                         28,28,28,28,28,28,28,28,28,28,29,29,29,29,29,29,
                                                                                         29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,29,
                                                                                         29,29,29,29,30,30,30,30,30,30,30,30,30,30,30,30,
                                                                                         30,30,30,30,30,30,30,30,30,30,30,30,30,30,30,31,
                                                                                         31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,31,
                                                                                         31,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                         32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,32,
                                                                                         32,32,32,32,33,33,33,33,33,33,33,33,33,33,33,33,
                                                                                         33,33,33,33,33,33,33,33,33,33,33,33,33,33,34,34,
                                                                                         34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,34,
                                                                                         34,34,34,34,34,34,34,34,34,35,35,35,35,35,35,35,
                                                                                         35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,35,
                                                                                         35,35,35,35,36,36,36,36,36,36,36,36,36,36,36,36,
                                                                                         36,36,36,36,36,36,36,36,36,36,36,36,36,36,37,37,
                                                                                         37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,37,
                                                                                         37,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                         38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,
                                                                                         38,38,38,39,39,39,39,39,39,39,39,39,39,39,39,39,
                                                                                         39,39,39,39,39,39,39,39,39,39,39,39,39,39,40,40,
                                                                                         40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,
                                                                                         40,40,40,40,40,40,40,40,41,41,41,41,41,41,41,41,
                                                                                         41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,
                                                                                         41,41,41,42,42,42,42,42,42,42,42,42,42,42,42,42,
                                                                                         42,42,42,42,42,42,42,42,42,42,42,42,42,42,43,43,
                                                                                         43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,
                                                                                         43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,44,
                                                                                         44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,
                                                                                         44,44,44,45,45,45,45,45,45,45,45,45,45,45,45,45,
                                                                                         45,45,45,45,45,45,45,45,45,45,45,45,45,46,46,46,
                                                                                         46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
                                                                                         46,46,46,46,46,46,46,46,47,47,47,47,47,47,47,47,
                                                                                         47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,
                                                                                         47,47,47,48,48,48,48,48,48,48,48,48,48,48,48,48,
                                                                                         48,48,48,48,48,48,48,48,48,48,48,48,48,49,49,49,
                                                                                         49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,49,
                                                                                         49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,50,
                                                                                         50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,50,
                                                                                         50,50,51,51,51,51,51,51,51,51,51,51,51,51,51,51,
                                                                                         51,51,51,51,51,51,51,51,51,51,51,51,51,52,52,52,
                                                                                         52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,52,
                                                                                         52,52,52,52,52,52,52,53,53,53,53,53,53,53,53,53,
                                                                                         53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,53,
                                                                                         53,53,54,54,54,54,54,54,54,54,54,54,54,54,54,54,
                                                                                         54,54,54,54,54,54,54,54,54,54,54,54,54,55,55,55,
                                                                                         55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,55,
                                                                                         55,55,55,55,55,55,55,56,56,56,56,56,56,56,56,56,
                                                                                         56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,56,
                                                                                         56,56,57,57,57,57,57,57,57,57,57,57,57,57,57,57,
                                                                                         57,57,57,57,57,57,57,57,57,57,57,57,58,58,58,58,
                                                                                         58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,58,
                                                                                         58,58,58,58,58,58,58,59,59,59,59,59,59,59,59,59,
                                                                                         59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,59,
                                                                                         59,60,60,60,60,60,60,60,60,60,60,60,60,60,60,60,
                                                                                         60,60,60,60,60,60,60,60,60,60,60,60,61,61,61,61,
                                                                                         61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,61,
                                                                                         61,61,61,61,61,61,61,62,62,62,62,62,62,62,62,62,
                                                                                         62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,62,
                                                                                         62,63,63,63,63,63,63,63,63,63,63,63,63,63,63,63,
                                                                                         63,63,63,63,63,63,63,63,63,63,63,63,64,64,64,64,
                                                                                         64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,64,
                                                                                         64,64,64,64,64,64,65,65,65,65,65,65,65,65,65,65,
                                                                                         65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,65,
                                                                                         65,66,66,66,66,66,66,66,66,66,66,66,66,66,66,66,
                                                                                         66,66,66,66,66,66,66,66,66,66,66,67,67,67,67,67,
                                                                                         67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,67,
                                                                                         67,67,67,67,67,67,68,68,68,68,68,68,68,68,68,68,
                                                                                         68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,68,
                                                                                         68,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                                                                                         69,69,69,69,69,69,69,69,69,69,69,70,70,70,70,70,
                                                                                         70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,70,
                                                                                         70,70,70,70,70,70,71,71,71,71,71,71,71,71,71,71,
                                                                                         71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,71,
                                                                                         72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,72,
                                                                                         72,72,72,72,72,72,72,72,72,72,72,73,73,73,73,73,
                                                                                         73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,73,
                                                                                         73,73,73,73,73,74,74,74,74,74,74,74,74,74,74,74,
                                                                                         74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,74,
                                                                                         75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,75,
                                                                                         75,75,75,75,75,75,75,75,75,75,75,76,76,76,76,76,
                                                                                         76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,76,
                                                                                         76,76,76,76,76,77,77,77,77,77,77,77,77,77,77,77,
                                                                                         77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,77,
                                                                                         78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,78,
                                                                                         78,78,78,78,78,78,78,78,78,78,79,79,79,79,79,79,
                                                                                         79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,79,
                                                                                         79,79,79,79,79,80,80,80,80,80,80,80,80,80,80,80,
                                                                                         80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,
                                                                                         81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,81,
                                                                                         81,81,81,81,81,81,81,81,81,81,82,82,82,82,82,82,
                                                                                         82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,82,
                                                                                         82,82,82,82,82,83,83,83,83,83,83,83,83,83,83,83,
                                                                                         83,83,83,83,83,83,83,83,83,83,83,83,83,83,83,84,
                                                                                         84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,84,
                                                                                         84,84,84,84,84,84,84,84,84,84,85,85,85,85,85,85,
                                                                                         85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,85,
                                                                                         85,85,85,85,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                         86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                         86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,86,
                                                                                         86,86,86,86,86,86,86,86,86);

      BeRoDoubleToStringPowerOfTenDecimalExponentTable:array[-348..(340+8)-1] of byte=(0,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,
                                                                                       2,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,
                                                                                       4,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,
                                                                                       6,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,
                                                                                       8,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,
                                                                                       10,11,11,11,11,11,11,11,11,12,12,12,12,12,12,12,
                                                                                       12,13,13,13,13,13,13,13,13,14,14,14,14,14,14,14,
                                                                                       14,15,15,15,15,15,15,15,15,16,16,16,16,16,16,16,
                                                                                       16,17,17,17,17,17,17,17,17,18,18,18,18,18,18,18,
                                                                                       18,19,19,19,19,19,19,19,19,20,20,20,20,20,20,20,
                                                                                       20,21,21,21,21,21,21,21,21,22,22,22,22,22,22,22,
                                                                                       22,23,23,23,23,23,23,23,23,24,24,24,24,24,24,24,
                                                                                       24,25,25,25,25,25,25,25,25,26,26,26,26,26,26,26,
                                                                                       26,27,27,27,27,27,27,27,27,28,28,28,28,28,28,28,
                                                                                       28,29,29,29,29,29,29,29,29,30,30,30,30,30,30,30,
                                                                                       30,31,31,31,31,31,31,31,31,32,32,32,32,32,32,32,
                                                                                       32,33,33,33,33,33,33,33,33,34,34,34,34,34,34,34,
                                                                                       34,35,35,35,35,35,35,35,35,36,36,36,36,36,36,36,
                                                                                       36,37,37,37,37,37,37,37,37,38,38,38,38,38,38,38,
                                                                                       38,39,39,39,39,39,39,39,39,40,40,40,40,40,40,40,
                                                                                       40,41,41,41,41,41,41,41,41,42,42,42,42,42,42,42,
                                                                                       42,43,43,43,43,43,43,43,43,44,44,44,44,44,44,44,
                                                                                       44,45,45,45,45,45,45,45,45,46,46,46,46,46,46,46,
                                                                                       46,47,47,47,47,47,47,47,47,48,48,48,48,48,48,48,
                                                                                       48,49,49,49,49,49,49,49,49,50,50,50,50,50,50,50,
                                                                                       50,51,51,51,51,51,51,51,51,52,52,52,52,52,52,52,
                                                                                       52,53,53,53,53,53,53,53,53,54,54,54,54,54,54,54,
                                                                                       54,55,55,55,55,55,55,55,55,56,56,56,56,56,56,56,
                                                                                       56,57,57,57,57,57,57,57,57,58,58,58,58,58,58,58,
                                                                                       58,59,59,59,59,59,59,59,59,60,60,60,60,60,60,60,
                                                                                       60,61,61,61,61,61,61,61,61,62,62,62,62,62,62,62,
                                                                                       62,63,63,63,63,63,63,63,63,64,64,64,64,64,64,64,
                                                                                       64,65,65,65,65,65,65,65,65,66,66,66,66,66,66,66,
                                                                                       66,67,67,67,67,67,67,67,67,68,68,68,68,68,68,68,
                                                                                       68,69,69,69,69,69,69,69,69,70,70,70,70,70,70,70,
                                                                                       70,71,71,71,71,71,71,71,71,72,72,72,72,72,72,72,
                                                                                       72,73,73,73,73,73,73,73,73,74,74,74,74,74,74,74,
                                                                                       74,75,75,75,75,75,75,75,75,76,76,76,76,76,76,76,
                                                                                       76,77,77,77,77,77,77,77,77,78,78,78,78,78,78,78,
                                                                                       78,79,79,79,79,79,79,79,79,80,80,80,80,80,80,80,
                                                                                       80,81,81,81,81,81,81,81,81,82,82,82,82,82,82,82,
                                                                                       82,83,83,83,83,83,83,83,83,84,84,84,84,84,84,84,
                                                                                       84,85,85,85,85,85,85,85,85,86,86,86,86,86,86,86,
                                                                                       86,86,86,86,86,86,86,86);

      BeRoDoubleToStringEstimatePowerFactorTable:array[2..36] of int64=(4294967296, // round((ln(2)/ln(Radix))*4294967296.0);
                                                                        2709822658,
                                                                        2147483648,
                                                                        1849741732,
                                                                        1661520155,
                                                                        1529898219,
                                                                        1431655765,
                                                                        1354911329,
                                                                        1292913986,
                                                                        1241523975,
                                                                        1198050829,
                                                                        1160664035,
                                                                        1128071163,
                                                                        1099331346,
                                                                        1073741824,
                                                                        1050766077,
                                                                        1029986701,
                                                                        1011073584,
                                                                        993761859,
                                                                        977836272,
                                                                        963119891,
                                                                        949465783,
                                                                        936750801,
                                                                        924870866,
                                                                        913737342,
                                                                        903274219,
                                                                        893415894,
                                                                        884105413,
                                                                        875293062,
                                                                        866935226,
                                                                        858993459,
                                                                        851433729,
                                                                        844225782,
                                                                        837342623,
                                                                        830760078);

function BeRoConvertDoubleToString(const AValue:double;Mode,RequestedDigits:longint):ansistring;
const SignificantMantissaSize=64;
      MinimalTargetExponent=-60;
      MaximalTargetExponent=-32;
      mSHORTEST=0;
      mFIXED=1;
      mPRECISION=2;
      BigNumMaxSignificantMantissaBits=3584;
      BigitChunkSize=32;
      BigitDoubleChunkSize=64;
      BigitSize=28;
      BigitMask=(1 shl BigitSize)-1;
      BigNumCapacity=(BigNumMaxSignificantMantissaBits+(BigitSize-1)) div BigitSize;
type TDoubleValue=record
      SignificantMantissa:qword;
      Exponent:longint;
     end;
     TBigNumChunk=longword;
     TBigNumDoubleChunk=qword;
     TBigNum=record
      Bigits:array[0..BigNumCapacity] of TBigNumChunk;
      UsedDigits:longint;
      Exponent:longint;
     end;
 function QWordLessOrEqual(a,b:qword):boolean;
 begin
  result:=(a=b) or (((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff))));
 end;
 function QWordGreaterOrEqual(a,b:qword):boolean;
 begin
  result:=(a=b) or (((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff))));
 end;
 function QWordLess(a,b:qword):boolean;
 begin
  result:=((a shr 32)<(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)<(b and $ffffffff)));
 end;
 function QWordGreater(a,b:qword):boolean;
 begin
  result:=((a shr 32)>(b shr 32)) or (((a shr 32)=(b shr 32)) and ((a and $ffffffff)>(b and $ffffffff)));
 end;
 function DoubleValue(SignificantMantissa:qword=0;Exponent:longint=0):TDoubleValue;
 begin
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure SplitDouble(Value:double;var SignificantMantissa:qword;var Exponent:longint);
 var Casted:qword absolute Value;
 begin
  SignificantMantissa:=Casted and qword($000fffffffffffff);
  if (Casted and qword($7ff0000000000000))<>0 then begin
   inc(SignificantMantissa,qword($0010000000000000));
   Exponent:=((Casted and qword($7ff0000000000000)) shr 52)-($3ff+52);
  end else begin
   Exponent:=(-($3ff+52))+1;
  end;
 end;
 function DoubleValueGet(Value:double):TDoubleValue;
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value>0);
  SplitDouble(Value,SignificantMantissa,Exponent);
  while (SignificantMantissa and qword($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  SignificantMantissa:=SignificantMantissa shl (SignificantMantissaSize-53);
  dec(Exponent,SignificantMantissaSize-53);
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 procedure DoubleValueSubtract(var Left:TDoubleValue;const Right:TDoubleValue);
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  dec(Left.SignificantMantissa,Right.SignificantMantissa);
 end;
 function DoubleValueMinus(const Left,Right:TDoubleValue):TDoubleValue;
 begin
  Assert(Left.Exponent=Right.Exponent);
  Assert(QWordGreaterOrEqual(Left.SignificantMantissa,Right.SignificantMantissa));
  result.Exponent:=Left.Exponent;
  result.SignificantMantissa:=Left.SignificantMantissa-Right.SignificantMantissa;
 end;
 procedure DoubleValueMuliply(var Left:TDoubleValue;const Right:TDoubleValue);
 var a,b,c,d,ac,bc,ad,bd:qword;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  inc(Left.Exponent,Right.Exponent+64);
  Left.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(qword(((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(qword(1) shl 31)) shr 32);
 end;
 function DoubleValueMul(const Left,Right:TDoubleValue):TDoubleValue;
 var a,b,c,d,ac,bc,ad,bd:qword;
 begin
  a:=Left.SignificantMantissa shr 32;
  b:=Left.SignificantMantissa and $ffffffff;
  c:=Right.SignificantMantissa shr 32;
  d:=Right.SignificantMantissa and $ffffffff;
  ac:=a*c;
  bc:=b*c;
  ad:=a*d;
  bd:=b*d;
  result.Exponent:=Left.Exponent+(Right.Exponent+64);
  a:=((bd shr 32)+((ad and $ffffffff)+(bc and $ffffffff)))+(qword(1) shl 31);
  result.SignificantMantissa:=ac+(ad shr 32)+(bc shr 32)+(a shr 32);
 end;
 procedure DoubleValueNormalize(var Value:TDoubleValue);
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and qword($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and qword($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  Value.SignificantMantissa:=SignificantMantissa;
  Value.Exponent:=Exponent;
 end;
 function DoubleValueNorm(const Value:TDoubleValue):TDoubleValue;
 var SignificantMantissa:qword;
     Exponent:longint;
 begin
  Assert(Value.SignificantMantissa<>0);
  SignificantMantissa:=Value.SignificantMantissa;
  Exponent:=Value.Exponent;
  while (SignificantMantissa and qword($ffc0000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 10;
   dec(Exponent,10);
  end;
  while (SignificantMantissa and qword($8000000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result.SignificantMantissa:=SignificantMantissa;
  result.Exponent:=Exponent;
 end;
 function BigNumNew:TBigNum;
 begin
  FillChar(result,sizeof(TBigNum),#0);
 end;
 procedure BigNumZero(var BigNum:TBigNum);
 begin
  BigNum.UsedDigits:=0;
  BigNum.Exponent:=0;
 end;
 procedure BigNumEnsureCapacity(var BigNum:TBigNum;Size:longint);
 begin
 end;
 procedure BigNumClamp(var BigNum:TBigNum);
 begin
  while (BigNum.UsedDigits>0) and (BigNum.Bigits[BigNum.UsedDigits-1]=0) do begin
   dec(BigNum.UsedDigits);
  end;
  if BigNum.UsedDigits=0 then begin
   BigNum.Exponent:=0;
  end;
 end;
 function BigNumIsClamped(const BigNum:TBigNum):boolean;
 begin
  result:=(BigNum.UsedDigits=0) or (BigNum.Bigits[BigNum.UsedDigits-1]<>0);
 end;
 procedure BigNumAlign(var BigNum:TBigNum;const Other:TBigNum);
 var ZeroDigits,i:longint;
 begin
  if BigNum.Exponent>Other.Exponent then begin
   ZeroDigits:=BigNum.Exponent-Other.Exponent;
   BigNumEnsureCapacity(BigNum,Bignum.UsedDigits+ZeroDigits);
   for i:=BigNum.UsedDigits-1 downto 0 do begin
    BigNum.Bigits[i+ZeroDigits]:=BigNum.Bigits[i];
   end;
   for i:=0 to ZeroDigits-1 do begin
    BigNum.Bigits[i]:=0;
   end;
   inc(BigNum.UsedDigits,ZeroDigits);
   dec(BigNum.Exponent,ZeroDigits);
   Assert(BigNum.UsedDigits>=0);
   Assert(BigNum.Exponent>=0);
  end;
 end;
 procedure BigNumAssignUInt16(var BigNum:TBigNum;Value:word);
 begin
  Assert(BigitSize>=(sizeof(word)*8));
  BigNumZero(BigNum);
  if Value<>0 then begin
   BigNumEnsureCapacity(BigNum,1);
   BigNum.Bigits[0]:=Value;
   BigNum.UsedDigits:=1;
  end;
 end;
 procedure BigNumAssignUInt64(var BigNum:TBigNum;Value:qword);
 var i,j:longint;
 begin
  BigNumZero(BigNum);
  if Value<>0 then begin
   j:=(64 div BigitSize)+1;
   BigNumEnsureCapacity(BigNum,j);
   for i:=0 to j-1 do begin
    BigNum.Bigits[i]:=Value and BigitMask;
    Value:=Value shr BigitSize;
   end;
   BigNum.UsedDigits:=j;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumAssignBigNum(var BigNum:TBigNum;const Other:TBigNum);
 begin
  BigNum.Exponent:=Other.Exponent;
  BigNum.Bigits:=Other.Bigits;
  BigNum.UsedDigits:=Other.UsedDigits;
 end;
 procedure BigNumAddBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Carry,Sum:TBigNumChunk;
     BigitPos,i:longint;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  BigNumAlign(BigNum,Other);
  BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+Other.UsedDigits);
  BigitPos:=Other.Exponent-BigNum.Exponent;
  Assert(BigitPos>=0);
  Carry:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Sum:=BigNum.Bigits[BigitPos]+Other.Bigits[i]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  while Carry<>0 do begin
   Sum:=BigNum.Bigits[BigitPos]+Carry;
   BigNum.Bigits[BigitPos]:=Sum and BigitMask;
   Carry:=Sum shr BigitSize;
   inc(BigitPos);
  end;
  if BigNum.UsedDigits<BigitPos then begin
   BigNum.UsedDigits:=BigitPos;
  end;
  Assert(BigNumIsClamped(BigNum));
 end;
 procedure BigNumAddUInt64(var BigNum:TBigNum;const Value:qword);
 var Other:TBigNum;
 begin
  Other:=BigNumNew;
  BigNumAssignUInt64(Other,Value);
  BigNumAddBigNum(BigNum,Other);
 end;
 function BigNumBigitAt(const BigNum:TBigNum;Index:longint):TBigNumChunk;
 begin
  if (Index<BigNum.Exponent) or (Index>=(BigNum.UsedDigits+BigNum.Exponent)) then begin
   result:=0;
  end else begin
   result:=BigNum.Bigits[Index-BigNum.Exponent];
  end;
 end;
 function BigNumCompare(const a,b:TBigNum):longint;
 var la,lb,i,j:longint;
     ba,bb:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  if la<lb then begin
   result:=-1;
  end else if la>lb then begin
   result:=1;
  end else begin
   if a.Exponent<b.Exponent then begin
    j:=a.Exponent;
   end else begin
    j:=b.Exponent;
   end;
   result:=0;
   for i:=la-1 downto j do begin
    ba:=BigNumBigItAt(a,i);
    bb:=BigNumBigItAt(b,i);
    if ba<bb then begin
     result:=-1;
     break;
    end else if ba>bb then begin
     result:=1;
     break;
    end;
   end;
  end;
 end;
 function BigNumPlusCompare(const a,b,c:TBigNum):longint;
 var la,lb,lc,i,j:longint;
     ba,bb,bc,br,Sum:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(a));
  Assert(BigNumIsClamped(b));
  Assert(BigNumIsClamped(c));
  la:=a.UsedDigits+a.Exponent;
  lb:=b.UsedDigits+b.Exponent;
  lc:=c.UsedDigits+c.Exponent;
  if la<lb then begin
   result:=BigNumPlusCompare(b,a,c);
  end else begin
   if (la+1)<lc then begin
    result:=-1;
   end else if la>lc then begin
    result:=1;
   end else if (a.Exponent>=lb) and (la<lc) then begin
    result:=-1;
   end else begin
    if a.Exponent<b.Exponent then begin
     if a.Exponent<c.Exponent then begin
      j:=a.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end else begin
     if b.Exponent<c.Exponent then begin
      j:=b.Exponent;
     end else begin
      j:=c.Exponent;
     end;
    end;
    br:=0;
    for i:=lc-1 downto j do begin
     ba:=BigNumBigItAt(a,i);
     bb:=BigNumBigItAt(b,i);
     bc:=BigNumBigItAt(c,i);
     Sum:=ba+bb;
     if Sum>(bc+br) then begin
      result:=1;
      exit;
     end else begin
      br:=(bc+br)-Sum;
      if br>1 then begin
       result:=-1;
       exit;
      end;
      br:=br shl BigitSize;
     end;
    end;
    if br=0 then begin
     result:=0;
    end else begin
     result:=-1;
    end;
   end;
  end;
 end;
 procedure BigNumSubtractBigNum(var BigNum:TBigNum;const Other:TBigNum);
 var Borrow,Difference:TBigNumChunk;
     i,Offset:longint;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(BigNumCompare(Other,BigNum)<=0);
  BigNumAlign(BigNum,Other);
  Offset:=Other.Exponent-BigNum.Exponent;
  Borrow:=0;
  for i:=0 to Other.UsedDigits-1 do begin
   Assert((Borrow=0) or (Borrow=1));
   Difference:=(BigNum.Bigits[i+Offset]-Other.Bigits[i])-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
  end;
  i:=Other.UsedDigits;
  while Borrow<>0 do begin
   Difference:=BigNum.Bigits[i+Offset]-Borrow;
   BigNum.Bigits[i+Offset]:=Difference and BigitMask;
   Borrow:=Difference shr (BigitChunkSize-1);
   inc(i);
  end;
  BigNumClamp(BigNum);
 end;
 procedure BigNumBigitsShiftLeft(var BigNum:TBigNum;Shift:longint);
 var Carry,NextCarry:TBigNumChunk;
     i:longint;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  Carry:=0;
  for i:=0 to BigNum.UsedDigits-1 do begin
   NextCarry:=BigNum.Bigits[i] shr (BigitSize-Shift);
   BigNum.Bigits[i]:=((BigNum.Bigits[i] shl Shift)+Carry) and BigitMask;
   Carry:=NextCarry;
  end;
  if Carry<>0 then begin
   BigNum.Bigits[BigNum.UsedDigits]:=Carry;
   inc(BigNum.UsedDigits);
  end;
 end;
 procedure BigNumBigitsShiftRight(var BigNum:TBigNum;Shift:longint);
 var Carry,NextCarry:TBigNumChunk;
     i:longint;
 begin
  Assert(Shift<BigitSize);
  Assert(Shift>=0);
  if BigNum.UsedDigits>0 then begin
   Carry:=0;
   for i:=BigNum.UsedDigits-1 downto 1 do begin
    NextCarry:=BigNum.Bigits[i] shl (BigitSize-Shift);
    BigNum.Bigits[i]:=((BigNum.Bigits[i] shr Shift)+Carry) and BigitMask;
    Carry:=NextCarry;
   end;
   BigNum.Bigits[0]:=(BigNum.Bigits[0] shr Shift)+Carry;
  end;
  BigNumClamp(BigNum);
 end;
 procedure BignumSubtractTimes(var BigNum:TBigNum;const Other:TBigNum;Factor:longint);
 var i,ExponentDiff:longint;
     Borrow,Difference:TBigNumChunk;
     Product,Remove:TBigNumDoubleChunk;
 begin
  Assert(BigNum.Exponent<=Other.Exponent);
  if Factor<3 then begin
   for i:=1 to Factor do begin
    BigNumSubtractBignum(BigNum,Other);
   end;
  end else begin
   Borrow:=0;
   ExponentDiff:=Other.Exponent-BigNum.Exponent;
   for i:=0 to Other.UsedDigits-1 do begin
    Product:=TBigNumDoubleChunk(Factor)*Other.Bigits[i];
    Remove:=Borrow+Product;
    Difference:=BigNum.Bigits[i+ExponentDiff]-TBigNumChunk(Remove and BigitMask);
    BigNum.Bigits[i+ExponentDiff]:=Difference and BigitMask;
    Borrow:=TBigNumChunk((Difference shr (BigitChunkSize-1))+(Remove shr BigitSize));
   end;
   for i:=Other.UsedDigits+ExponentDiff to BigNum.UsedDigits-1 do begin
    if Borrow=0 then begin
     exit;
    end;
    Difference:=BigNum.Bigits[i]-Borrow;
    BigNum.Bigits[i]:=Difference and BigitMask;
    Borrow:=TBigNumChunk(Difference shr (BigitChunkSize-1));
   end;
   BigNumClamp(BigNum);
  end;
 end;
 procedure BigNumShiftLeft(var BigNum:TBigNum;Shift:longint);
 begin
  if BigNum.UsedDigits<>0 then begin
   inc(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
   BigNumBigitsShiftLeft(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumShiftRight(var BigNum:TBigNum;Shift:longint);
 begin
  if BigNum.UsedDigits<>0 then begin
   dec(BigNum.Exponent,Shift div BigitSize);
   BignumEnsureCapacity(BigNum,BigNum.UsedDigits);
   BigNumBigitsShiftRight(BigNum,Shift mod BigitSize);
  end;
 end;
 procedure BigNumMultiplyByUInt32(var BigNum:TBigNum;Factor:word);
 var Carry,Product:qword;
     i:longint;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   for i:=0 to BigNum.UsedDigits-1 do begin
    Product:=(Factor*BigNum.Bigits[i])+Carry;
    BigNum.Bigits[i]:=Product and BigitMask;
    Carry:=Product shr BigitSize;
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumMultiplyByUInt64(var BigNum:TBigNum;Factor:qword);
 var Carry,Low,High,ProductLow,ProductHigh,Tmp:qword;
     i:longint;
 begin
  if Factor=0 then begin
   BigNumZero(BigNum);
  end else if Factor<>1 then begin
   Assert(BigitSize<32);
   Carry:=0;
   Low:=Factor and $ffffffff;
   High:=Factor shr 32;
   for i:=0 to BigNum.UsedDigits-1 do begin
    ProductLow:=Low*BigNum.Bigits[i];
    ProductHigh:=High*BigNum.Bigits[i];
    Tmp:=(Carry and BigitMask)+ProductLow;
    BigNum.Bigits[i]:=Tmp and BigitMask;
    Carry:=(Carry shr BigitSize)+(Tmp shr BigitSize)+(ProductHigh shl (32-BigitSize));
   end;
   while Carry<>0 do begin
    BigNumEnsureCapacity(BigNum,BigNum.UsedDigits+1);
    BigNum.Bigits[BigNum.UsedDigits]:=Carry and BigitMask;
    inc(BigNum.UsedDigits);
    Carry:=Carry shr BigitSize;
   end;
  end;
 end;
 procedure BigNumSquare(var BigNum:TBigNum);
 var ProductLength,CopyOffset,i,BigitIndex1,BigitIndex2:longint;
     Accumulator:TBigNumDoubleChunk;
     Chunk1,Chunk2:TBigNumChunk;
 begin
  Assert(BigNumIsClamped(BigNum));
  ProductLength:=2*BigNum.UsedDigits;
  BigNumEnsureCapacity(BigNum,ProductLength);
  Assert(not ((1 shl (2*(BigItChunkSize-BigitSize)))<=BigNum.UsedDigits));
  Accumulator:=0;
  CopyOffset:=BigNum.UsedDigits;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigNum.Bigits[i+CopyOffset]:=BigNum.Bigits[i];
  end;
  for i:=0 to BigNum.UsedDigits-1 do begin
   BigitIndex1:=i;
   BigitIndex2:=0;
   while BigitIndex1>=0 do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  for i:=BigNum.UsedDigits-1 to ProductLength-1 do begin
   BigitIndex1:=BigNum.UsedDigits-1;
   BigitIndex2:=i-BigitIndex1;
   while BigitIndex2<BigNum.UsedDigits do begin
    Chunk1:=BigNum.Bigits[CopyOffset+BigitIndex1];
    Chunk2:=BigNum.Bigits[CopyOffset+BigitIndex2];
    inc(Accumulator,TBigNumDoubleChunk(Chunk1)*Chunk2);
    dec(BigitIndex1);
    inc(BigitIndex2);
   end;
   BigNum.Bigits[i]:=Accumulator and BigitMask;
   Accumulator:=Accumulator shr BigitSize;
  end;
  Assert(Accumulator=0);
  BigNum.UsedDigits:=ProductLength;
  inc(BigNum.Exponent,BigNum.Exponent);
  BigNumClamp(BigNum);
 end;
 procedure BigNumAssignPowerUInt16(var BigNum:TBigNum;Base:word;PowerExponent:longint);
 var Shifts,BitSize,TmpBase,FinalSize,Mask:longint;
     ThisValue:qword;
     DelayedMultipliciation:boolean;
 begin
  Assert(Base<>0);
  Assert(PowerExponent>=0);
  if PowerExponent=0 then begin
   BigNumAssignUInt16(BigNum,1);
  end else begin
   BigNumZero(BigNum);
   Shifts:=0;
   while (Base and 1)=0 do begin
    Base:=Base shr 1;
    inc(Shifts);
   end;
   BitSize:=0;
   TmpBase:=Base;
   while TmpBase<>0 do begin
    TmpBase:=TmpBase shr 1;
    inc(BitSize);
   end;
   FinalSize:=BitSize*PowerExponent;
   BigNumEnsureCapacity(BigNum,FinalSize);
   Mask:=1;
   while Mask<=PowerExponent do begin
    inc(Mask,Mask);
   end;
   Mask:=Mask shr 2;
   ThisValue:=Base;
   DelayedMultipliciation:=false;
   while (Mask<>0) and (ThisValue<=$ffffffff) do begin
    ThisValue:=ThisValue*ThisValue;
    if (PowerExponent and Mask)<>0 then begin
     if (ThisValue and not ((qword(1) shl (64-BitSize))-1))=0 then begin
      ThisValue:=ThisValue*Base;
     end else begin
      DelayedMultipliciation:=true;
     end;
    end;
    Mask:=Mask shr 1;
   end;
   BigNumAssignUInt64(BigNum,ThisValue);
   if DelayedMultipliciation then begin
    BigNumMultiplyByUInt32(BigNum,Base);
   end;
   while Mask<>0 do begin
    BigNumSquare(BigNum);
    if (PowerExponent and Mask)<>0 then begin
     BigNumMultiplyByUInt32(BigNum,Base);
    end;
    Mask:=Mask shr 1;
   end;
   BigNumShiftLeft(BigNum,Shifts*PowerExponent);
  end;
 end;
 function BigNumDivideModuloIntBigNum(var BigNum:TBigNum;const Other:TBigNum):word;
 var ThisBigit,OtherBigit:TBigNumChunk;
     Quotient,DivisionEstimate:longword;
 begin
  Assert(BigNumIsClamped(BigNum));
  Assert(BigNumIsClamped(Other));
  Assert(Other.UsedDigits>0);
  result:=0;
  if (BigNum.UsedDigits+BigNum.Exponent)>=(Other.UsedDigits+Other.Exponent) then begin
   BigNumAlign(BigNum,Other);
   while (BigNum.UsedDigits+BigNum.Exponent)>(Other.UsedDigits+Other.Exponent) do begin
    Assert(Other.Bigits[Other.UsedDigits-1]>=((1 shl BigitSize) div 16));
    inc(result,BigNum.Bigits[BigNum.UsedDigits-1]);
    BigNumSubtractTimes(BigNum,Other,BigNum.Bigits[BigNum.UsedDigits-1]);
   end;
   Assert((BigNum.UsedDigits+BigNum.Exponent)=(Other.UsedDigits+Other.Exponent));
   ThisBigit:=BigNum.Bigits[BigNum.UsedDigits-1];
   OtherBigit:=Other.Bigits[Other.UsedDigits-1];
   if Other.UsedDigits=1 then begin
    Quotient:=ThisBigit div OtherBigit;
    BigNum.Bigits[BigNum.UsedDigits-1]:=ThisBigit-(OtherBigit*Quotient);
    inc(result,Quotient);
    BigNumClamp(BigNum);
   end else begin
    DivisionEstimate:=ThisBigit div (OtherBigit+1);
    inc(result,DivisionEstimate);
    BigNumSubtractTimes(BigNum,Other,DivisionEstimate);
    if (OtherBigit*(DivisionEstimate+1))<=ThisBigit then begin
     while BigNumCompare(Other,BigNum)<=0 do begin
      BigNumSubtractBigNum(BigNum,Other);
      inc(result);
     end;
    end;
   end;
  end;
 end;
 function BigNumDivideModuloInt(var BigNum:TBigNum;Divisor:word):word;
 var q0,r0,q1,r1:qword;
     i:integer;
 begin
  Assert(BigNumIsClamped(BigNum));
  q0:=0;
  for i:=BigNum.UsedDigits-1 downto 1 do begin
   q1:=(BigNum.Bigits[i] div Divisor)+q0;
   r1:=((BigNum.Bigits[i] mod Divisor) shl 16)+(BigNum.Bigits[i-1] shr 16);
   q0:=((r1 div Divisor) shl 16);
   r0:=r1 mod Divisor;
   BigNum.Bigits[i]:=q1;
   BigNum.Bigits[i-1]:=(r0 shl 16)+(BigNum.Bigits[i-1] and $ffff);
  end;
  q1:=(BigNum.Bigits[0] div Divisor)+q0;
  r1:=BigNum.Bigits[0] mod Divisor;
  BigNum.Bigits[0]:=q1;
  result:=r1;
  BigNumClamp(BigNum);
 end;
 function NormalizedExponent(SignificantMantissa:qword;Exponent:longint):longint;
 begin
  Assert(SignificantMantissa<>0);
  while (SignificantMantissa and qword($0010000000000000))=0 do begin
   SignificantMantissa:=SignificantMantissa shl 1;
   dec(Exponent);
  end;
  result:=Exponent;
 end;
 function GetEstimatePower(Exponent:longint):longint;
 begin
  result:=longint(int64(((Exponent+52)*int64(1292913986))-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*0.30102999566398114)-(1e-10)));
 end;
 function GetEstimatePowerOf(Exponent,Radix:longint):longint;
 begin
  result:=longint(int64(((Exponent+52)*BeRoDoubleToStringEstimatePowerFactorTable[Radix])-$1000) shr 32)+1; // result:=System.Trunc(Math.Ceil(((Exponent+52)*(ln(2)/ln(Radix)))-(1e-10)));
 end;
 procedure GenerateShortestDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:boolean;var Buffer:ansistring;var Len:longint);
 var Digit,Compare:longint;
     InDeltaRoomMinus,InDeltaRoomPlus:boolean;
 begin
  Len:=0;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,10);
    BigNumMultiplyByUInt32(DeltaMinus,10);
    BigNumMultiplyByUInt32(DeltaPlus,10);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin
    end else if Compare>0 then begin
     Assert(Buffer[Len]<>'9');
     inc(Buffer[Len]);
    end else begin
     if ((ord(Buffer[Len])-ord('0')) and 1)<>0 then begin
      Assert(Buffer[Len]<>'9');
      inc(Buffer[Len]);
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(Buffer[Len]<>'9');
    inc(Buffer[Len]);
    exit;
   end;
  end;
 end;
 procedure GenerateCountedDigits(Count:longint;var DecimalPoint:longint;var Numerator,Denominator:TBigNum;var Buffer:ansistring;var Len:longint);
 var i,Digit:longint;
 begin
  Assert(Count>=0);
  for i:=1 to Count-1 do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=9));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
   BigNumMultiplyByUInt32(Numerator,10);
  end;
  Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
  if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
   inc(Digit);
  end;
  inc(Len);
  if Len>=length(Buffer) then begin
   SetLength(Buffer,Len*2);
  end;
  Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
  for i:=Len downto 2 do begin
   if ord(Buffer[i])<>(ord('0')+10) then begin
    break;
   end;
   Buffer[i]:='0';
   inc(Buffer[i-1]);
  end;
  if ord(Buffer[1])=(ord('0')+10) then begin
   Buffer[1]:='1';
   inc(DecimalPoint);
  end;
 end;
 procedure GenerateFixedDigits(RequestedDigits:longint;var DecimalPoint:longint;var Numerator,Denominator:TBigNum;var Buffer:ansistring;var Len:longint);
 begin
  if (-DecimalPoint)>RequestedDigits then begin
   DecimalPoint:=-RequestedDigits;
   Len:=0;
  end else if (-DecimalPoint)=RequestedDigits then begin
   Assert(DecimalPoint=(-RequestedDigits));
   BigNumMultiplyByUInt32(Denominator,10);
   if BigNumPlusCompare(Numerator,Numerator,Denominator)>=0 then begin
    Buffer:='1';
    Len:=1;
  end else begin
    Len:=0;
   end;
  end else begin
   GenerateCountedDigits(DecimalPoint+RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
  end;
 end;
 procedure FixupMultiplyBase(EstimatedPower:longint;IsEven:boolean;var DecimalPoint:longint;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 var InRange:boolean;
 begin
  if IsEven then begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
  end else begin
   InRange:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
  end;
  if InRange then begin
   DecimalPoint:=EstimatedPower+1;
  end else begin
   DecimalPoint:=EstimatedPower;
   BigNumMultiplyByUInt32(Numerator,Base);
   if BigNumCompare(DeltaMinus,DeltaPlus)=0 then begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumAssignBigNum(DeltaPlus,DeltaMinus);
   end else begin
    BigNumMultiplyByUInt32(DeltaMinus,Base);
    BigNumMultiplyByUInt32(DeltaPlus,Base);
   end;
  end;
 end;
 procedure InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  Assert(EstimatedPower>=0);

  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumShiftLeft(Numerator,Exponent);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumShiftLeft(DeltaPlus,Exponent);

   BigNumAssignUInt16(DeltaMinus,1);
   BigNumShiftLeft(DeltaMinus,Exponent);

   if (Casted and qword($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  BigNumAssignUInt64(Numerator,SignificantMantissa);
  BigNumAssignPowerUInt16(Denominator,Base,EstimatedPower);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);

   BigNumAssignUInt16(DeltaPlus,1);
   BigNumAssignUInt16(DeltaMinus,1);

   if (Casted and qword($000fffffffffffff))=0 then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  BigNumAssignPowerUInt16(Numerator,Base,-EstimatedPower);
  if NeedBoundaryDeltas then begin
   BigNumAssignBigNum(DeltaPlus,Numerator);
   BigNumAssignBigNum(DeltaMinus,Numerator);
  end;
  BigNumMultiplyByUInt64(Numerator,SignificantMantissa);

  BigNumAssignUInt16(Denominator,1);
  BigNumShiftLeft(Denominator,-Exponent);

  if NeedBoundaryDeltas then begin
   BigNumShiftLeft(Numerator,1);
   BigNumShiftLeft(Denominator,1);
   if ((Casted and qword($000fffffffffffff))=0) and ((Casted and qword($7ff0000000000000))<>qword($0010000000000000)) then begin
    BigNumShiftLeft(Numerator,1);
    BigNumShiftLeft(Denominator,1);
    BigNumShiftLeft(DeltaPlus,1);
   end;
  end;
 end;
 procedure InitialScaledStartValues(Casted,SignificantMantissa:qword;Exponent:longint;EstimatedPower:longint;NeedBoundaryDeltas:boolean;var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;Base:longint);
 begin
  if Exponent>=0 then begin
   InitialScaledStartValuesPositiveExponent(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else if EstimatedPower>=0 then begin
   InitialScaledStartValuesNegativeExponentPositivePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end else begin
   InitialScaledStartValuesNegativeExponentNegativePower(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Base);
  end;
 end;
 procedure DoubleToDecimal(Value:double;Mode,RequestedDigits:longint;var Buffer:ansistring;var Len,DecimalPoint:longint);
 var Casted:qword absolute Value;
     SignificantMantissa:qword;
     Exponent,EstimatedPower:longint;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:boolean;
 begin
  Assert(Value>0);
  Assert(BeRoIsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePower(NormalizedExponent(SignificantMantissa,Exponent));
  if (Mode=mFIXED) and (((-EstimatedPower)-1)>RequestedDigits) then begin
   Buffer:='';
   Len:=0;
   DecimalPoint:=-RequestedDigits;
  end else begin
   Assert(BigNumMaxSignificantMantissaBits>=(324*4));
   NeedBoundaryDeltas:=Mode=mSHORTEST;
   InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,10);
   case Mode of
    mSHORTEST:begin
     GenerateShortestDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len);
    end;
    mFIXED:begin
     GenerateFixedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
    else {mPRECISION:}begin
     GenerateCountedDigits(RequestedDigits,DecimalPoint,Numerator,Denominator,Buffer,Len);
    end;
   end;
  end;
 end;
 procedure GenerateRadixDigits(var Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;IsEven:boolean;var Buffer:ansistring;var Len:longint;Radix:longint);
 const Base36:array[0..36] of ansichar='0123456789abcdefghijklmnopqrstuvwxyz{';
 var Digit,Compare,MaxDigit:longint;
     InDeltaRoomMinus,InDeltaRoomPlus:boolean;
  function ValueOf(c:ansichar):longint;
  begin
   case c of
    '0'..'9':begin
     result:=ord(c)-ord('0');
    end;
    else begin
     result:=(ord(c)-ord('a'))+$a;
    end;
   end;
  end;
 begin
  Len:=0;
  MaxDigit:=Radix-1;
  while true do begin
   Digit:=BigNumDivideModuloIntBigNum(Numerator,Denominator);
   Assert((Digit>=0) and (Digit<=MaxDigit));
   inc(Len);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   Buffer[Len]:=Base36[Digit];
   BigNumClamp(Numerator);
   BigNumClamp(DeltaMinus);
   BigNumClamp(DeltaPlus);
   if IsEven then begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<=0;
   end else begin
    InDeltaRoomMinus:=BigNumCompare(Numerator,DeltaMinus)<0;
   end;
   if IsEven then begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>=0;
   end else begin
    InDeltaRoomPlus:=BigNumPlusCompare(Numerator,DeltaPlus,Denominator)>0;
   end;
   if (not InDeltaRoomMinus) and (not InDeltaRoomPlus) then begin
    BigNumMultiplyByUInt32(Numerator,Radix);
    BigNumMultiplyByUInt32(DeltaMinus,Radix);
    BigNumMultiplyByUInt32(DeltaPlus,Radix);
   end else if InDeltaRoomMinus and InDeltaRoomPlus then begin
    Compare:=BigNumPlusCompare(Numerator,Numerator,Denominator);
    if Compare<0 then begin       
    end else if Compare>0 then begin
     Assert(ValueOf(Buffer[Len])<>MaxDigit);
     Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    end else begin
     if (ValueOf(Buffer[Len]) and 1)<>0 then begin
      Assert(ValueOf(Buffer[Len])<>MaxDigit);
      Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
     end;
    end;
    exit;
   end else if InDeltaRoomMinus then begin
    exit;
   end else begin
    Assert(ValueOf(Buffer[Len])<>MaxDigit);
    Buffer[Len]:=Base36[ValueOf(Buffer[Len])+1];
    exit;
   end;
  end;
 end;
 procedure DoubleToRadix(Value:double;Radix:longint;var Buffer:ansistring;var Len,DecimalPoint:longint);
 var Casted:qword absolute Value;
     SignificantMantissa:qword;
     Exponent,EstimatedPower:longint;
     Numerator,Denominator,DeltaMinus,DeltaPlus:TBigNum;
     IsEven,NeedBoundaryDeltas:boolean;
 begin
  Assert(Value>0);
  Assert(BeRoIsFinite(Value));
  SplitDouble(Value,SignificantMantissa,Exponent);
  IsEven:=(SignificantMantissa and 1)=0;
  EstimatedPower:=GetEstimatePowerOf(NormalizedExponent(SignificantMantissa,Exponent),Radix);
  Assert(BigNumMaxSignificantMantissaBits>=(324*4));
  NeedBoundaryDeltas:=true;
  InitialScaledStartValues(Casted,SignificantMantissa,Exponent,EstimatedPower,NeedBoundaryDeltas,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  FixupMultiplyBase(EstimatedPower,IsEven,DecimalPoint,Numerator,Denominator,DeltaMinus,DeltaPlus,Radix);
  GenerateRadixDigits(Numerator,Denominator,DeltaMinus,DeltaPlus,IsEven,Buffer,Len,Radix);
 end;
 {$warnings off}
 procedure FastDoubleToRadix(v:double;Radix:longint;var Buffer:ansistring;var Len,DecimalPoint:longint);
 const Base36:array[0..35] of ansichar='0123456789abcdefghijklmnopqrstuvwxyz';
       DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
       DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
       DtoAFPURoundingMode:TFPURoundingMode=rmNEAREST;
 var IntPart,FracPart,Old,Epsilon:double;
     Digit,i,j:longint;
     TempBuffer:ansistring;
     OldFPUExceptionMask:TFPUExceptionMask;
     OldFPUPrecisionMode:TFPUPrecisionMode;
     OldFPURoundingMode:TFPURoundingMode;
     IntPart64:int64;
 begin
  if (Radix<2) or (Radix>36) then begin
   result:='';
  end else begin
   OldFPUExceptionMask:=GetExceptionMask;
   OldFPUPrecisionMode:=GetPrecisionMode;
   OldFPURoundingMode:=GetRoundMode;
   try
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(DtoAFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(DtoAFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(DtoAFPURoundingMode);
    end;
    try
     TempBuffer:='';
     IntPart:=System.Int(v);
     FracPart:=System.Frac(v);
     if IntPart=0 then begin
      result:='0';
     end else begin
      if IntPart<4294967295.0 then begin
       IntPart64:=trunc(IntPart);
       while IntPart64>0 do begin
        Digit:=IntPart64 mod Radix;
        Assert((Digit>=0) and (Digit<Radix));
        IntPart64:=IntPart64 div Radix;
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end else begin
       while IntPart>0 do begin
        Old:=IntPart;
        IntPart:=System.Int(IntPart/Radix);
        Digit:=trunc(Old-(IntPart*Radix));
        Assert((Digit>=0) and (Digit<Radix));
        inc(Len);
        if Len>=length(TempBuffer) then begin
         SetLength(TempBuffer,Len*2);
        end;
        TempBuffer[Len]:=Base36[Digit];
       end;
      end;
      SetLength(Buffer,Len);
      j:=1;
      for i:=Len downto 1 do begin
       Buffer[j]:=TempBuffer[i];
       inc(j);
      end;
     end;
     if FracPart<>0 then begin
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:='.';
      Epsilon:=0.001/Radix;
      while (FracPart>=Epsilon) and (Len<32) do begin
       FracPart:=FracPart*Radix;
       Digit:=trunc(FracPart);
       FracPart:=System.Frac(FracPart);
       Assert((Digit>=0) and (Digit<Radix));
       inc(Len);
       if Len>=length(Buffer) then begin
        SetLength(Buffer,Len*2);
       end;
       Buffer[Len]:=Base36[Digit];
      end;
     end;
    finally
     TempBuffer:='';
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>DtoAFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end;
 end;
 {$warnings on}
 function GetCachedPowerForBinaryExponentRange(MinExponent,MaxExponent:longint;var Power:TDoubleValue;var DecimalExponent:longint):boolean;
 var Index:longint;
 begin
  result:=false;
  if (low(BeRoDoubleToStringPowerOfTenBinaryExponentTable)<=MinExponent) and (MinExponent<=high(BeRoDoubleToStringPowerOfTenBinaryExponentTable)) then begin
   Index:=BeRoDoubleToStringPowerOfTenBinaryExponentTable[MinExponent];
   if ((Index>=0) and (Index<length(BeRoDoubleToStringPowerOfTenTable))) and ((MinExponent<=BeRoDoubleToStringPowerOfTenTable[Index,1]) and (BeRoDoubleToStringPowerOfTenTable[Index,1]<=MaxExponent)) then begin
    Power.SignificantMantissa:=BeRoDoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=BeRoDoubleToStringPowerOfTenTable[Index,1];
    DecimalExponent:=BeRoDoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function GetCachedPowerForDecimalExponent(RequestedExponent:longint;var Power:TDoubleValue;var FoundExponent:longint):boolean;
 var Index:longint;
 begin
  result:=false;
  if (low(BeRoDoubleToStringPowerOfTenDecimalExponentTable)<=RequestedExponent) and (RequestedExponent<=high(BeRoDoubleToStringPowerOfTenDecimalExponentTable)) then begin
   Index:=BeRoDoubleToStringPowerOfTenDecimalExponentTable[RequestedExponent];
   if (Index>=0) and (Index<length(BeRoDoubleToStringPowerOfTenTable)) then begin
    Power.SignificantMantissa:=BeRoDoubleToStringPowerOfTenTable[Index,0];
    Power.Exponent:=BeRoDoubleToStringPowerOfTenTable[Index,1];
    FoundExponent:=BeRoDoubleToStringPowerOfTenTable[Index,2];
    result:=true;
   end;
  end;
 end;
 function RoundWeed(var Buffer:ansistring;Len:longint;DistanceTooHighW,UnsafeInterval,Rest,TenCapacity,UnitValue:qword):boolean;
 var SmallDistance,BigDistance:qword;
 begin
  SmallDistance:=DistanceTooHighW-UnitValue;
  BigDistance:=DistanceTooHighW+UnitValue;
  Assert(QWordLessOrEqual(Rest,UnsafeInterval));
  while (QWordLess(Rest,SmallDistance) and (QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity))) and (QWordLess(Rest+TenCapacity,SmallDistance) or QWordGreaterOrEqual(SmallDistance-Rest,((Rest+TenCapacity)-SmallDistance))) do begin
   dec(Buffer[Len]);
   inc(Rest,TenCapacity);
  end;
  if ((QWordLess(Rest,BigDistance) and QWordGreaterOrEqual(UnsafeInterval-Rest,TenCapacity)) and (QWordLess(Rest+TenCapacity,BigDistance) or QWordGreater(BigDistance-Rest,((Rest+TenCapacity)-BigDistance)))) then begin
   result:=false;
  end else begin
   result:=(QWordLessOrEqual(2*UnitValue,Rest) and QWordLessOrEqual(Rest,UnsafeInterval-(4*UnitValue)));
  end;
 end;
 function RoundWeedCounted(var Buffer:ansistring;Len:longint;Rest,TenCapacity,UnitValue:qword;var Capacity:longint):boolean;
 var i:longint;
 begin
  Assert(QWordLess(Rest,TenCapacity));
  result:=false;
  if QWordGreater(TenCapacity-UnitValue,UnitValue) then begin
   result:=QWordGreater(TenCapacity-Rest,Rest) and QWordGreaterOrEqual(TenCapacity-(2*Rest),2*UnitValue);
   if not result then begin
    result:=QWordGreater(Rest,UnitValue) and QWordLessOrEqual(TenCapacity-(Rest-UnitValue),Rest-UnitValue);
    if result then begin
     inc(Buffer[Len]);
     for i:=Len downto 2 do begin
      if ord(Buffer[i])<>(ord('0')+10) then begin
       break;
      end;
      Buffer[i]:='0';
      inc(Buffer[i-1]);
     end;
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(Capacity);
    end;
   end;
  end;
 end;
 function BiggestPowerTen(Number:longword;NumberBits:longint;var Power:longword;var Exponent:longint):boolean;
 label c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11;
 begin
  result:=true;
  case NumberBits of
   30,31,32:begin
    c1:
    if 1000000000<=Number then begin
     Power:=1000000000;
     Exponent:=9;
    end else begin
     goto c2;
    end;
   end;
   27,28,29:begin
    c2:
    if 100000000<=Number then begin
     Power:=100000000;
     Exponent:=8;
    end else begin
     goto c3;
    end;
   end;
   24,25,26:begin
    c3:
    if 10000000<=Number then begin
     Power:=10000000;
     Exponent:=7;
    end else begin
     goto c4;
    end;
   end;
   20,21,22,23:begin
    c4:
    if 1000000<=Number then begin
     Power:=1000000;
     Exponent:=6;
    end else begin
     goto c5;
    end;
   end;
   17,18,19:begin
    c5:
    if 100000<=Number then begin
     Power:=100000;
     Exponent:=5;
    end else begin
     goto c6;
    end;
   end;
   14,15,16:begin
    c6:
    if 10000<=Number then begin
     Power:=10000;
     Exponent:=4;
    end else begin
     goto c7;
    end;
   end;
   10,11,12,13:begin
    c7:
    if 1000<=Number then begin
     Power:=1000;
     Exponent:=3;
    end else begin
     goto c8;
    end;
   end;
   7,8,9:begin
    c8:
    if 100<=Number then begin
     Power:=100;
     Exponent:=2;
    end else begin
     goto c9;
    end;
   end;
   4,5,6:begin
    c9:
    if 10<=Number then begin
     Power:=10;
     Exponent:=1;
    end else begin
     goto c10;
    end;
   end;
   1,2,3:begin
    c10:
    if 1<=Number then begin
     Power:=1;
     Exponent:=0;
    end else begin
     goto c11;
    end;
   end;
   0:begin
    c11:
    Power:=0;
    Exponent:=-1;
   end;
   else begin
    Power:=0;
    Exponent:=0;
    result:=false;
   end;
  end;
 end;
 function DigitGen(Low,w,High:TDoubleValue;var Buffer:ansistring;var Len,Capacity:longint):boolean;
 var UnitValue,Fractionals,Rest:qword;
     TooLow,TooHigh,UnsafeInterval,One:TDoubleValue;
     Integrals,Divisor,Digit:longword;
     DivisorExponent:longint;
 begin
  result:=false;
  if ((Low.Exponent=w.Exponent) and (w.Exponent=High.Exponent)) and (QWordLessOrEqual(Low.SignificantMantissa+1,High.SignificantMantissa-1) and
     ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent))) then begin
   UnitValue:=1;
   TooLow.SignificantMantissa:=Low.SignificantMantissa-UnitValue;
   TooLow.Exponent:=Low.Exponent;
   TooHigh.SignificantMantissa:=High.SignificantMantissa+UnitValue;
   TooHigh.Exponent:=High.Exponent;
   UnsafeInterval:=DoubleValueMinus(TooHigh,TooLow);
   One.SignificantMantissa:=qword(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=TooHigh.SignificantMantissa shr (-One.Exponent);
   Fractionals:=TooHigh.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
     dec(Capacity);
     Rest:=qword(qword(Integrals) shl (-One.Exponent))+Fractionals;
     if QWordLess(Rest,UnsafeInterval.SignificantMantissa) then begin
      result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa,UnsafeInterval.SignificantMantissa,Rest,qword(Divisor) shl (-One.Exponent),UnitValue);
      exit;
     end;
     Divisor:=Divisor div 10;
    end;
    if (One.Exponent>=-60) and (QWordLess(Fractionals,One.SignificantMantissa) and QWordGreaterOrEqual(qword($1999999999999999),One.SignificantMantissa)) then begin
     while true do begin
      Fractionals:=Fractionals*10;
      UnitValue:=UnitValue*10;
      UnsafeInterval.SignificantMantissa:=UnsafeInterval.SignificantMantissa*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
      if QWordLess(Fractionals,UnsafeInterval.SignificantMantissa) then begin
       result:=RoundWeed(Buffer,Len,DoubleValueMinus(TooHigh,w).SignificantMantissa*UnitValue,UnsafeInterval.SignificantMantissa,Fractionals,One.SignificantMantissa,UnitValue);
       exit;
      end;
     end;
    end;
   end;
  end;
 end;
 function DigitGenCounted(w:TDoubleValue;RequestedDigits:longint;var Buffer:ansistring;var Len,Capacity:longint):boolean;
 var wError,Fractionals,Rest:qword;
     One:TDoubleValue;
     Integrals,Divisor,Digit:longword;
     DivisorExponent:longint;
 begin
  result:=false;
  if ((MinimalTargetExponent<=w.Exponent) and (w.Exponent<=MaximalTargetExponent)) and ((MinimalTargetExponent>=-60) and (MaximalTargetExponent<=-32)) then begin
   wError:=1;
   One.SignificantMantissa:=qword(1) shl (-w.Exponent);
   One.Exponent:=w.Exponent;
   Integrals:=w.SignificantMantissa shr (-One.Exponent);
   Fractionals:=w.SignificantMantissa and (One.SignificantMantissa-1);
   Divisor:=0;
   DivisorExponent:=0;
   if BiggestPowerTen(Integrals,SignificantMantissaSize-(-One.Exponent),Divisor,DivisorExponent) then begin
    Capacity:=DivisorExponent+1;
    Len:=0;
    while Capacity>0 do begin
     Digit:=Integrals div Divisor;
     Integrals:=Integrals mod Divisor;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
     dec(RequestedDigits);
     dec(Capacity);
     if RequestedDigits=0 then begin
      break;
     end;
     Divisor:=Divisor div 10;
    end;
    if RequestedDigits=0 then begin
     Rest:=qword(qword(Integrals) shl (-One.Exponent))+Fractionals;
     result:=RoundWeedCounted(Buffer,Len,Rest,qword(Divisor) shl (-One.Exponent),wError,Capacity);
     exit;
    end;
    if ((One.Exponent>=-60) and QWordLess(Fractionals,One.SignificantMantissa)) and QWordGreaterOrEqual(qword($1999999999999999),One.SignificantMantissa) then begin
     while (RequestedDigits>0) and (Fractionals>wError) do begin
      Fractionals:=Fractionals*10;
      Digit:=Fractionals shr (-One.Exponent);
      inc(Len);
      if Len>=length(Buffer) then begin
       SetLength(Buffer,Len*2);
      end;
      Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
      dec(RequestedDigits);
      dec(Capacity);
      Fractionals:=Fractionals and (One.SignificantMantissa-1);
     end;
     if RequestedDigits=0 then begin
      result:=RoundWeedCounted(Buffer,Len,Fractionals,One.SignificantMantissa,wError,Capacity);
     end else begin
      result:=false;
     end;
    end;
   end;
  end;
 end;
 procedure NormalizedBoundaries(Value:double;var BoundaryMinus,BoundaryPlus:TDoubleValue);
 var v:TDoubleValue;
     SignificantMantissaIsZero:boolean;
 begin
  Assert(not BeRoIsNegative(Value));
  Assert(BeRoIsFinite(Value));
  SplitDouble(Value,v.SignificantMantissa,v.Exponent);
  SignificantMantissaIsZero:=v.SignificantMantissa=qword($0010000000000000);
  BoundaryPlus.SignificantMantissa:=(v.SignificantMantissa shl 1)+1;
  BoundaryPlus.Exponent:=v.Exponent-1;
  DoubleValueNormalize(BoundaryPlus);
  if SignificantMantissaIsZero and (v.Exponent<>((-($3ff+52))+1)) then begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 2)-1;
   BoundaryMinus.Exponent:=v.Exponent-2;
  end else begin
   BoundaryMinus.SignificantMantissa:=(v.SignificantMantissa shl 1)-1;
   BoundaryMinus.Exponent:=v.Exponent-1;
  end;
  BoundaryMinus.SignificantMantissa:=BoundaryMinus.SignificantMantissa shl (BoundaryMinus.Exponent-BoundaryPlus.Exponent);
  BoundaryMinus.Exponent:=BoundaryPlus.Exponent;
 end;
 function DoFastShortest(Value:double;var Buffer:ansistring;var Len,DecimalExponent:longint):boolean;
 var w,BoundaryMinus,BoundaryPlus,TenMK,ScaledW,ScaledBoundaryMinus,ScaledBoundaryPlus:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:longint;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  NormalizedBoundaries(Value,BoundaryMinus,BoundaryPlus);
  Assert(BoundaryPlus.Exponent=w.Exponent);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    if ScaledW.Exponent=(BoundaryPlus.Exponent+TenMK.Exponent+SignificantMantissaSize) then begin
     ScaledBoundaryMinus:=DoubleValueMul(BoundaryMinus,TenMK);
     ScaledBoundaryPlus:=DoubleValueMul(BoundaryPlus,TenMK);
     Capacity:=0;
     result:=DigitGen(ScaledBoundaryMinus,ScaledW,ScaledBoundaryPlus,Buffer,Len,Capacity);
     DecimalExponent:=Capacity-mK;
    end;
   end;
  end;
 end;
 function DoFastPrecision(Value:double;RequestedDigits:longint;var Buffer:ansistring;var Len,DecimalExponent:longint):boolean;
 var w,TenMK,ScaledW:TDoubleValue;
     mK,TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,Capacity:longint;
 begin
  result:=false;
  w:=DoubleValueGet(Value);
  TenMKMinimalBinaryExponent:=MinimalTargetExponent-(w.Exponent+SignificantMantissaSize);
  TenMKMaximalBinaryExponent:=MaximalTargetExponent-(w.Exponent+SignificantMantissaSize);
  if GetCachedPowerForBinaryExponentRange(TenMKMinimalBinaryExponent,TenMKMaximalBinaryExponent,TenMK,mK) then begin
   if (MinimalTargetExponent<=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) and (MaximalTargetExponent>=(w.Exponent+TenMK.Exponent+SignificantMantissaSize)) then begin
    ScaledW:=DoubleValueMul(w,TenMK);
    Capacity:=0;
    result:=DigitGenCounted(ScaledW,RequestedDigits,Buffer,Len,Capacity);
    DecimalExponent:=Capacity-mK;
   end;
  end;
 end;
 function DoFastFixed(Value:double;FracitionalCount:longint;var Buffer:ansistring;var Len,DecimalPoint:longint):boolean;
 const Five17=$b1a2bc2ec5; // 5^17
 type TInt128=record
       High,Low:qword;
      end;
  procedure Int128Mul(var a:TInt128;const Multiplicand:longword);
  var Accumulator:qword;
      Part:longword;
  begin
   Accumulator:=(a.Low and $ffffffff)*Multiplicand;
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.Low shr 32)*Multiplicand);
   a.Low:=(Accumulator shl 32)+Part;
   Accumulator:=(Accumulator shr 32)+((a.High and $ffffffff)*Multiplicand);
   Part:=Accumulator and $ffffffff;
   Accumulator:=(Accumulator shr 32)+((a.High shr 32)*Multiplicand);
   a.High:=(Accumulator shl 32)+Part;
   Assert((Accumulator shr 32)=0);
  end;
  procedure Int128Shift(var a:TInt128;const Shift:longint);
  begin
   Assert(((-64)<=Shift) and (Shift<=64));
   if Shift<>0 then begin
    if Shift=-64 then begin
     a.High:=a.Low;
     a.Low:=0;
    end else if Shift=64 then begin
     a.Low:=a.High;
     a.High:=0;
    end else if Shift<=0 then begin
     a.High:=(a.High shl (-Shift))+(a.Low shr (64+Shift));
     a.Low:=a.Low shl (-Shift);
    end else begin
     a.Low:=(a.Low shr Shift)+(a.High shl (64-Shift));
     a.High:=a.High shr Shift;
    end;
   end;
  end;
  function Int128DivModPowerOfTwo(var a:TInt128;const Power:longint):longint;
  begin
   if Power>=64 then begin
    result:=a.High shr (Power-64);
    dec(a.High,result shl (Power-64));
   end else begin
    result:=(a.Low shr Power)+(a.High shl (64-Power));
    a.High:=0;
    dec(a.Low,(a.Low shr Power) shl Power);
   end;
  end;
  function Int128IsZero(const a:TInt128):boolean;
  begin
   result:=(a.High=0) and (a.Low=0);
  end;
  function Int128BitAt(const a:TInt128;const Position:longint):boolean;
  begin
   if Position>=64 then begin
    result:=((a.High shr (Position-64)) and 1)<>0;
   end else begin
    result:=((a.LOw shr Position) and 1)<>0;
   end;
  end;
  procedure FillDigits32FixedLength(Number:longword;RequestedLength:longint;var Buffer:ansistring;var Len:longint);
  var i,l:longint;
  begin
   l:=Len;
   inc(Len,RequestedLength);
   if Len>=length(Buffer) then begin
    SetLength(Buffer,Len*2);
   end;
   for i:=RequestedLength downto 1 do begin
    Buffer[l+i]:=AnsiChar(byte(byte(AnsiChar('0'))+(Number mod 10)));
    Number:=Number div 10;
   end;
  end;
  procedure FillDigits32(Number:longword;var Buffer:ansistring;var Len:longint);
  var NumberLength,i,l:longint;
      OldNumber:longword;
  begin
   OldNumber:=Number;
   NumberLength:=0;
   while Number<>0 do begin
    Number:=Number div 10;
    inc(NumberLength);
   end;
   if NumberLength<>0 then begin
    l:=Len;
    inc(Len,NumberLength);
    if Len>=length(Buffer) then begin
     SetLength(Buffer,Len*2);
    end;
    Number:=OldNumber;
    for i:=NumberLength downto 1 do begin
     Buffer[l+i]:=AnsiChar(byte(byte(AnsiChar('0'))+(Number mod 10)));
     Number:=Number div 10;
    end;
   end;
  end;
  procedure FillDigits64FixedLength(Number:qword;RequestedLength:longint;var Buffer:ansistring;var Len:longint);
  var p0,p1,p2:longword;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   FillDigits32FixedLength(p0,3,Buffer,Len);
   FillDigits32FixedLength(p1,7,Buffer,Len);
   FillDigits32FixedLength(p2,7,Buffer,Len);
  end;
  procedure FillDigits64(Number:qword;var Buffer:ansistring;var Len:longint);
  var p0,p1,p2:longword;
  begin
   p2:=Number mod 10000000;
   Number:=Number div 10000000;
   p1:=Number mod 10000000;
   p0:=Number div 10000000;
   if p0<>0 then begin
    FillDigits32(p0,Buffer,Len);
    FillDigits32FixedLength(p1,7,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else if p1<>0 then begin
    FillDigits32(p1,Buffer,Len);
    FillDigits32FixedLength(p2,7,Buffer,Len);
   end else begin
    FillDigits32(p2,Buffer,Len);
   end;
  end;
  procedure RoundUp(var Buffer:ansistring;var Len,DecimalPoint:longint);
  var i:longint;
  begin
   if Len=0 then begin
    Buffer:='1';
    Len:=1;
    DecimalPoint:=1;
   end else begin
    inc(Buffer[Len]);
    for i:=Len downto 2 do begin
     if ord(Buffer[i])<>(ord('0')+10) then begin
      exit;
     end;
     Buffer[i]:='0';
     inc(Buffer[i-1]);
    end;
    if ord(Buffer[1])=(ord('0')+10) then begin
     Buffer[1]:='1';
     inc(DecimalPoint);
    end;
   end;
  end;
  procedure FillFractionals(Fractionals:qword;Exponent:longint;FractionalCount:longint;var Buffer:ansistring;var Len,DecimalPoint:longint);
  var Point,i,Digit:longint;
      Fractionals128:TInt128;
  begin
   Assert(((-128)<=Exponent) and (Exponent<=0));
   if (-Exponent)<=64 then begin
    Assert((Fractionals shr 56)=0);
    Point:=-Exponent;
    for i:=1 to FracitionalCount do begin
     Fractionals:=Fractionals*5;
     dec(Point);
     Digit:=Fractionals shr Point;
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
     dec(Fractionals,qword(Digit) shl Point);
    end;
    if ((Fractionals shr (Point-1)) and 1)<>0 then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end else begin
    Assert((64<(-Exponent)) and ((-Exponent)<=128));
    Fractionals128.High:=Fractionals;
    Fractionals128.Low:=0;
    Int128Shift(Fractionals128,(-Exponent)-64);
    Point:=128;
    for i:=1 to FracitionalCount do begin
     if Int128IsZero(Fractionals128) then begin
      break;
     end;
     Int128Mul(Fractionals128,5);
     dec(Point);
     Digit:=Int128DivModPowerOfTwo(Fractionals128,Point);
     inc(Len);
     if Len>=length(Buffer) then begin
      SetLength(Buffer,Len*2);
     end;
     Buffer[Len]:=AnsiChar(byte(byte(AnsiChar('0'))+Digit));
    end;
    if Int128BitAt(Fractionals128,Point-1) then begin
     RoundUp(Buffer,Len,DecimalPoint);
    end;
   end;
  end;
  procedure TrimZeros(var Buffer:ansistring;var Len,DecimalPoint:longint);
  var i:longint;
  begin
   while (Len>0) and (Buffer[Len]='0') do begin
    dec(Len);
   end;
   i:=0;
   while (i<Len) and (Buffer[i+1]='0') do begin
    inc(i);
   end;
   if i<>0 then begin
    Delete(Buffer,1,i);
    dec(Len,i);
    dec(DecimalPoint,i);
   end;
  end;
 var SignificantMantissa,Divisor,Dividend,Remainder,Integrals,Fractionals:qword;
     Exponent,DivisorPower:longint;
     Quotient:longword;
 begin
  result:=false;
  SplitDouble(Value,SignificantMantissa,Exponent);
  if (Exponent<=20) and (FracitionalCount<=20) then begin
   Len:=0;
   if (Exponent+53)>74 then begin
    Divisor:=Five17;
    DivisorPower:=17;
    Dividend:=SignificantMantissa;
    if Exponent>DivisorPower then begin
     Dividend:=Dividend shl (Exponent-DivisorPower);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl DivisorPower;
    end else begin
     Dividend:=Dividend shl (DivisorPower-Exponent);
     Quotient:=Dividend div Divisor;
     Remainder:=(Dividend mod Divisor) shl Exponent;
    end;
    FillDigits32(Quotient,Buffer,Len);
    FillDigits64FixedLength(Remainder,DivisorPower,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>=0 then begin
    SignificantMantissa:=SignificantMantissa shl Exponent;
    FillDigits64(SignificantMantissa,Buffer,Len);
    DecimalPoint:=Len;
   end else if Exponent>-53 then begin
    Integrals:=SignificantMantissa shr (-Exponent);
    Fractionals:=SignificantMantissa-(Integrals shl (-Exponent));
    if Integrals>$ffffffff then begin
     FillDigits64(Integrals,Buffer,Len);
    end else begin
     FillDigits32(Integrals,Buffer,Len);
    end;
    DecimalPoint:=Len;
    FillFractionals(Fractionals,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end else if Exponent<-128 then begin
    Assert(FracitionalCount>=20);
    Buffer:='';
    Len:=0;
    DecimalPoint:=-FracitionalCount;
   end else begin
    DecimalPoint:=0;
    FillFractionals(SignificantMantissa,Exponent,FracitionalCount,Buffer,Len,DecimalPoint);
   end;
   TrimZeros(Buffer,Len,DecimalPoint);
   SetLength(Buffer,Len);
   if Len=0 then begin
    DecimalPoint:=-FracitionalCount;
   end;
   result:=true;
  end;
 end;
var OK,Fast:boolean;
    Len,DecimalPoint,ZeroPrefixLength,ZeroPostfixLength,i:longint;
begin
 if BeRoIsNaN(AValue) then begin
  result:='NaN';
 end else if BeRoIsZero(AValue) then begin
  result:='0';
 end else if BeRoIsNegInfinite(AValue) then begin
  result:='-Infinity';
 end else if BeRoIsNegative(AValue) then begin
  result:='-'+BeRoConvertDoubleToString(BeRoDoubleAbsolute(AValue),Mode,RequestedDigits);
 end else if BeRoIsInfinite(AValue) then begin
  result:='Infinity';
 end else begin
  result:='0';
  if AValue<>0 then begin
   Len:=0;
   DecimalPoint:=0;
   OK:=false;
   Fast:=false;
   if ((Mode=bdtsmFIXED) and (AValue>=1e21)) or ((Mode=bdtsmRADIX) and (RequestedDigits=10)) then begin
    Mode:=bdtsmSTANDARD;
   end;
   case Mode of
    bdtsmSTANDARD,bdtsmSTANDARDEXPONENTIAL:begin
     OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
     inc(DecimalPoint,Len);
    end;
    bdtsmFIXED:begin
     OK:=DoFastFixed(AValue,RequestedDigits,result,Len,DecimalPoint);
    end;
    bdtsmEXPONENTIAL,bdtsmPRECISION:begin
     if RequestedDigits<=0 then begin
      OK:=DoFastShortest(AValue,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
      RequestedDigits:=Len-1;
     end else begin
      OK:=DoFastPrecision(AValue,RequestedDigits,result,Len,DecimalPoint);
      inc(DecimalPoint,Len);
     end;
     Assert((Len>0) and (Len<=(RequestedDigits+1)));
    end;
    bdtsmRADIX:begin
     if ((RequestedDigits>=2) and (RequestedDigits<=36)) and (BeRoIsFinite(AValue) and (AValue<4294967295.0) and (System.Int(AValue)=AValue)) then begin
      FastDoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
      Fast:=true;
      OK:=true;
     end;
    end;
   end;
   if not OK then begin
    case Mode of
     bdtsmSTANDARD,bdtsmSTANDARDEXPONENTIAL:begin
      DoubleToDecimal(AValue,mSHORTEST,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     bdtsmFIXED:begin
      DoubleToDecimal(AValue,mFIXED,RequestedDigits,result,Len,DecimalPoint);
      OK:=true;
     end;
     bdtsmEXPONENTIAL,bdtsmPRECISION:begin
      if RequestedDigits<=0 then begin
       DoubleToDecimal(AValue,mSHORTEST,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
       RequestedDigits:=Len-1;
      end else begin
       DoubleToDecimal(AValue,mPRECISION,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
      Assert((Len>0) and (Len<=(RequestedDigits+1)));
     end;
     bdtsmRADIX:begin
      if (RequestedDigits>=2) and (RequestedDigits<=36) then begin
       DoubleToRadix(AValue,RequestedDigits,result,Len,DecimalPoint);
       OK:=true;
      end;
     end;
    end;
   end;
   if OK then begin
    SetLength(result,Len);
    case Mode of
     bdtsmSTANDARD:begin
      if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
       SetLength(result,DecimalPoint);
       FillChar(result[Len+1],DecimalPoint-Len,'0');
      end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
       Insert('.',result,DecimalPoint+1);
      end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
       for i:=1 to -DecimalPoint do begin
        result:='0'+result;
       end;
       result:='0.'+result;
      end else begin
       if Len<>1 then begin
        Insert('.',result,2);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+AnsiString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+AnsiString(IntToStr(abs(DecimalPoint-1)));
       end;
      end;
     end;
     bdtsmSTANDARDEXPONENTIAL:begin
      if Len<>1 then begin
       Insert('.',result,2);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+AnsiString(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+AnsiString(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     bdtsmFIXED:begin
      ZeroPrefixLength:=0;
      ZeroPostfixLength:=0;
      if DecimalPoint<=0 then begin
       ZeroPrefixLength:=(-DecimalPoint)+1;
       DecimalPoint:=1;
      end;
      if (ZeroPrefixLength+Len)<(DecimalPoint+RequestedDigits) then begin
       ZeroPostfixLength:=((DecimalPoint+RequestedDigits)-Len)-ZeroPrefixLength;
      end;
      for i:=1 to ZeroPrefixLength do begin
       result:='0'+result;
      end;
      for i:=1 to ZeroPostfixLength do begin
       result:=result+'0';
      end;
      if (RequestedDigits>0) and (DecimalPoint>0) and (DecimalPoint<=length(result)) then begin
       Insert('.',result,DecimalPoint+1);
      end;
     end;
     bdtsmEXPONENTIAL:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if RequestedDigits<>1 then begin
       Insert('.',result,2);
       for i:=Len+1 to RequestedDigits do begin
        result:=result+'0';
       end;
      end else begin
       SetLength(result,1);
      end;
      if DecimalPoint>=0 then begin
       result:=result+'e+'+AnsiString(IntToStr(abs(DecimalPoint-1)));
      end else begin
       result:=result+'e-'+AnsiString(IntToStr(abs(DecimalPoint-1)));
      end;
     end;
     bdtsmPRECISION:begin
      if RequestedDigits<1 then begin
       RequestedDigits:=1;
      end;
      if (DecimalPoint<-6) or (DecimalPoint>=RequestedDigits) then begin
       if RequestedDigits<>1 then begin
        Insert('.',result,2);
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,1);
       end;
       if DecimalPoint>=0 then begin
        result:=result+'e+'+AnsiString(IntToStr(abs(DecimalPoint-1)));
       end else begin
        result:=result+'e-'+AnsiString(IntToStr(abs(DecimalPoint-1)));
       end;
      end else begin
       if DecimalPoint<=0 then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
        for i:=Len+1 to RequestedDigits do begin
         result:=result+'0';
        end;
       end else begin
        SetLength(result,RequestedDigits);
        for i:=Len+1 to RequestedDigits do begin
         result[i]:='0';
        end;
        if DecimalPoint<RequestedDigits then begin
         if Len<>1 then begin
          Insert('.',result,DecimalPoint+1);
         end;
        end;
       end;
      end;
     end;
     bdtsmRADIX:begin
      if not Fast then begin
       if (Len<=DecimalPoint) and (DecimalPoint<=21) then begin
        SetLength(result,DecimalPoint);
        FillChar(result[Len+1],DecimalPoint-Len,'0');
       end else if (0<DecimalPoint) and (DecimalPoint<=21) then begin
        Insert('.',result,DecimalPoint+1);
       end else if (DecimalPoint<=0) and (DecimalPoint>-6) then begin
        for i:=1 to -DecimalPoint do begin
         result:='0'+result;
        end;
        result:='0.'+result;
       end else begin
        if Len<>1 then begin
         Insert('.',result,2);
        end;
        if DecimalPoint>=0 then begin
         result:=result+'p+'+AnsiString(IntToStr(abs(DecimalPoint-1)));
        end else begin
         result:=result+'p-'+AnsiString(IntToStr(abs(DecimalPoint-1)));
        end;
       end;
       while (length(result)>1) and ((result[1]='0') and (result[2] in ['0'..'9','a'..'f'])) do begin
        Delete(result,1,1);
       end;
      end;
     end;
    end;
   end else begin
    result:='';
   end;
  end;
 end;
end;

initialization
finalization
end.
