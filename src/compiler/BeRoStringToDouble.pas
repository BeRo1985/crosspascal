unit BeRoStringToDouble;
(**********************************************************
** BeRo String To Double Conversion Library               *
***********************************************************
**
** This file is part of the BeRo String To Double Conversion Library.
** Copyright (C) 2011-2012 by Benjamin Rosseaux
**
** The source code of the BeRo String To Double Conversion Library and helper tools are
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
{$warnings off}

interface

uses Math;

const bstd_ROUND_TO_NEAREST=0;
      bstd_ROUND_TOWARD_ZERO=1;
      bstd_ROUND_UPWARD=2;
      bstd_ROUND_DOWNWARD=3;

function BeRoConvertStringToDoubleFast(const StringValue:ansistring;RoundingMode:integer=bstd_ROUND_TO_NEAREST;OK:pointer=nil):double;
function BeRoConvertStringToDouble(const StringValue:ansistring;RoundingMode:integer=bstd_ROUND_TO_NEAREST;OK:pointer=nil;Base:longint=-1):double;

implementation

function BeRoConvertStringToDoubleFast(const StringValue:ansistring;RoundingMode:integer=bstd_ROUND_TO_NEAREST;OK:pointer=nil):double;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case byte of
       0:(Value:double);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:longword);
     end;
const MaxExponentOfTen=308;
      MaxExponentOfTenMinusTwo=MaxExponentOfTen-2;
      PowersOfTen:array[0..8] of double=(1e1,1e2,1e4,1e8,1e16,1e32,1e64,1e128,1e256);
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
var i:integer;
    Negative,HasDigits:boolean;
    Mantissa:int64;
    MantissaSize,FractionalDigits,FractionalExponent,ExponentSign,Exponent:integer;
    ResultCasted:PDoubleCasted;
    FloatValue,ExponentFactor:double;
    MantissaExponent,PartExponent,Index:integer;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
begin
 if assigned(OK) then begin
  longbool(OK^):=false;
 end;
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
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
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
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
   if MantissaSize<18 then begin
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
    if MantissaSize<18 then begin
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

    case RoundingMode of
     bstd_ROUND_TO_NEAREST:begin
      NewFPURoundingMode:=rmNearest;
     end;
     bstd_ROUND_TOWARD_ZERO:begin
      NewFPURoundingMode:=rmTruncate;
     end;
     bstd_ROUND_UPWARD:begin
      NewFPURoundingMode:=rmUp;
     end;
     bstd_ROUND_DOWNWARD:begin
      NewFPURoundingMode:=rmDown;
     end;
     else begin
      NewFPURoundingMode:=rmNearest;
     end;
    end;

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
     if MantissaSize>18 then begin
      dec(MantissaExponent,MantissaSize-18);
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

     ResultCasted^.Value:=FloatValue;
     if Negative then begin
      ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
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

function BeRoConvertStringToDouble(const StringValue:ansistring;RoundingMode:integer=bstd_ROUND_TO_NEAREST;OK:pointer=nil;Base:longint=-1):double;
type PDoubleCasted=^TDoubleCasted;
     TDoubleCasted=packed record
      case byte of
       0:(Value:double);
       1:({$ifdef BIG_ENDIAN}Hi,Lo{$else}Lo,Hi{$endif}:longword);
       2:(Value64:int64);
     end;
const MantissaWords=12; //6; // 12
      MantissaDigits=52; //28; // 52
      WordTopBit=$8000;
      WordBits=16;
      WordBitShift=4;
      WordBitMask=WordBits-1;
      WordMask=$ffff;
      IEEEFormatBytes=8;
      IEEEFormatBits=IEEEFormatBytes shl 3;
      IEEEFormatExplicit=0;
      IEEEFormatExponent=11;
      IEEEFormatOneMask=WordTopBit shr ((IEEEFormatExponent+IEEEFormatExplicit) and WordBitMask);
      IEEEFormatOnePos=(IEEEFormatExponent+IEEEFormatExplicit) shr WordBitShift;
      IEEEFormatExpMax=1 shl (IEEEFormatExponent-1);
      Bit53=int64(int64(1) shl 53);
      InvBit53Mask=int64($ffe0000000000000);
      MaximumMultiplier=longword(longword($ffffffff) div 36);
      DtoAFPUExceptionMask:TFPUExceptionMask=[exInvalidOp,exDenormalized,exZeroDivide,exOverflow,exUnderflow,exPrecision];
      DtoAFPUPrecisionMode:TFPUPrecisionMode=pmDOUBLE;
      MaxFastPathDigits=16;
      TenPowers:array[0..18] of int64=
       (
        1,
        10,
        100,
        1000,
        10000,
        100000,
        1000000,
        10000000,
        100000000,
        1000000000,
        10000000000,
        100000000000,
        1000000000000,
        10000000000000,
        100000000000000,
        1000000000000000,
        10000000000000000,
        100000000000000000,
        1000000000000000000
       );
type PWords=^TWords;
     TWords=array[0..MantissaWords] of word;
     PTemp=^TTemp;
     TTemp=array[0..MantissaWords*2] of longword;
     PDigits=^TDigits;
     TDigits=array[0..MantissaDigits] of byte;
var MantissaPosition,Exponent,TenPower,TwoPower,ExtraTwos,Shift,i,DigitPos,StoredDigitPos,DigitPosBackwards,Digit,Overflow,OverflowBits,DroppedBits,DroppedBitsMask,MiddleValue,ExponentPower,ExponentValue:longint;
    Bit,Carry:word;
    Negative,ExponentNegative,HasDigits,Started,ZeroTail,Done:boolean;
    ResultCasted:PDoubleCasted;
    Temp:PTemp;
    Digits:PDigits;
    MantissaMultiplicator,Mantissa:PWords;
    Value:int64;
    c:ansichar;
    Part,Multiplier,NextMultiplier:longword;
    OldFPUExceptionMask:TFPUExceptionMask;
    OldFPURoundingMode,NewFPURoundingMode:TFPURoundingMode;
    OldFPUPrecisionMode:TFPUPrecisionMode;
 function MantissaMultiply(vTo,vFrom:PWords):longint;
 var i,j,k:longint;
     v:longword;
     t:PTemp;
 begin
  t:=Temp;
  FillChar(t^,sizeof(TTemp),#0);
  for i:=0 to MantissaWords-1 do begin
   for j:=0 to MantissaWords-1 do begin
    v:=longword(vTo^[i]+0)*longword(vFrom^[j]+0);
    k:=i+j;
    inc(t^[k],v shr WordBits);
    inc(t^[k+1],v and WordMask);
   end;
  end;
  for i:=high(TTemp) downto 1 do begin
   inc(t^[i-1],t^[i] shr WordBits);
   t^[i]:=t^[i] and WordMask;
  end;
  if (t^[0] and WordTopBit)<>0 then begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=t^[i] and WordMask;
   end;
   result:=0;
  end else begin
   for i:=0 to MantissaWords-1 do begin
    vTo^[i]:=(t^[i] shl 1)+word(ord((t^[i+1] and WordTopBit)<>0));
   end;
   result:=-1;
  end;
 end;
 procedure MantissaShiftRight(var Mantissa:TWords;Shift:longint);
 var Bits,Words,InvBits,Position:longint;
     Carry,Current:longword;
 begin
  Bits:=Shift and WordBitMask;
  Words:=Shift shr WordBitShift;
  InvBits:=WordBits-Bits;
  Position:=high(TWords);
  if Bits=0 then begin
   if Words<>0 then begin
    while Position>=Words do begin
     Mantissa[Position]:=Mantissa[Position-Words];
     dec(Position);
    end;
   end;
  end else begin
   if (high(TWords)-Words)>=0 then begin
    Carry:=Mantissa[high(TWords)-Words] shr Bits;
   end else begin
    Carry:=0;
   end;
   while Position>Words do begin
    Current:=Mantissa[Position-(Words+1)];
    Mantissa[Position]:=(Current shl InvBits) or Carry;
    Carry:=Current shr Bits;
    dec(Position);
   end;
   Mantissa[Position]:=Carry;
   dec(Position);
  end;
  while Position>=0 do begin
   Mantissa[Position]:=0;
   dec(Position);
  end;
 end;
 procedure MantissaSetBit(var Mantissa:TWords;i:longint);
 begin
  Mantissa[i shr WordBitShift]:=Mantissa[i shr WordBitShift] or (WordTopBit shr (i and WordBitMask));
 end;
 function MantissaTestBit(var Mantissa:TWords;i:longint):boolean;
 begin
  result:=(Mantissa[i shr WordBitShift] shr ((not i) and WordBitMask))<>0;
 end;
 function MantissaIsZero(var Mantissa:TWords):boolean;
 var i:longint;
 begin
  result:=true;
  for i:=low(TWords) to High(TWords) do begin
   if Mantissa[i]<>0 then begin
    result:=false;
    break;
   end;
  end;
 end;
 function MantissaRound(Negative:boolean;var Mantissa:TWords;BitPos:longint):boolean;
 var i,p:longint;
     Bit:longword;
  function RoundAbsDown:boolean;
  var j:longint;
  begin
   Mantissa[i]:=Mantissa[i] and not (Bit-1);
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   result:=false;
  end;
  function RoundAbsUp:boolean;
  var j:longint;
  begin
   Mantissa[i]:=(Mantissa[i] and not (Bit-1))+Bit;
   for j:=i+1 to high(TWords) do begin
    Mantissa[j]:=0;
   end;
   while (i>0) and (Mantissa[i]=0) do begin
    dec(i);
    inc(Mantissa[i]);
   end;
   result:=Mantissa[0]=0;
  end;
  function RoundTowardsInfinity:boolean;
  var j:longint;
      m:longword;
  begin
   m:=Mantissa[i] and ((Bit shl 1)-1);
   for j:=i+1 to high(TWords) do begin
    m:=m or Mantissa[j];
   end;
   if m<>0 then begin
    result:=RoundAbsUp;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
  function RoundNear:boolean;
  var j:longint;
      m:longword;
  begin
   if (Mantissa[i] and Bit)<>0 then begin
    Mantissa[i]:=Mantissa[i] and not Bit;
    m:=Mantissa[i] and ((Bit shl 1)-1);
    for j:=i+1 to high(TWords) do begin
     m:=m or Mantissa[j];
    end;
    Mantissa[i]:=Mantissa[i] or Bit;
    if m<>0 then begin
     result:=RoundAbsUp;
    end else begin
     if MantissaTestBit(Mantissa,BitPos-1) then begin
      result:=RoundAbsUp;
     end else begin
      result:=RoundAbsDown;
     end;
    end;
   end else begin
    result:=RoundAbsDown;
   end;
  end;
 begin
  i:=BitPos shr WordBitShift;
  p:=BitPos and WordBitMask;
  Bit:=WordTopBit shr p;
  case RoundingMode of
   bstd_ROUND_TO_NEAREST:begin
    result:=RoundNear;
   end;
   bstd_ROUND_TOWARD_ZERO:begin
    result:=RoundAbsDown;
   end;
   bstd_ROUND_UPWARD:begin
    if Negative then begin
     result:=RoundAbsDown;
    end else begin
     result:=RoundTowardsInfinity;
    end;
   end;
   bstd_ROUND_DOWNWARD:begin
    if Negative then begin
     result:=RoundTowardsInfinity;
    end else begin
     result:=RoundAbsDown;
    end;
   end;
   else begin
    result:=false;
   end;
  end;
 end;
 function CountLeadingZeros32(a:longword):longint;
 const CountLeadingZerosHigh:array[byte] of byte=(8,7,6,6,5,5,5,5,4,4,4,4,4,4,4,4,
                                                  3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
                                                  0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0);
 begin
  result:=0;
  if a<$10000 then begin
   inc(result,16);
   a:=a shl 16;
  end;
  if a<$1000000 then begin
   inc(result,8);
   a:=a shl 8;
  end;
  inc(result,CountLeadingZerosHigh[a shr 24]);
 end;
 function CountLeadingZeros64(a:int64):longint;
 begin
 if a<int64($100000000) then begin
   result:=32;
  end else begin
   result:=0;
   a:=a shr 32;
  end;
  inc(result,CountLeadingZeros32(a));
 end;
begin
 if assigned(OK) then begin
  longbool(OK^):=false;
 end;
 ResultCasted:=pointer(@result);
 ResultCasted^.Hi:=$7ff80000;
 ResultCasted^.Lo:=$00000000;
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
 HasDigits:=false;
 if ((i+7)<=length(StringValue)) and ((StringValue[i]='I') and (StringValue[i+1]='n') and (StringValue[i+2]='f') and (StringValue[i+3]='i') and (StringValue[i+4]='n') and (StringValue[i+5]='i') and (StringValue[i+6]='t') and (StringValue[i+7]='y')) then begin
  if Negative then begin
   ResultCasted^.Hi:=$fff00000;
   ResultCasted^.Lo:=$00000000;
  end else begin
   ResultCasted^.Hi:=$7ff00000;
   ResultCasted^.Lo:=$00000000;
  end;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if ((i+2)<=length(StringValue)) and ((StringValue[i]='N') and (StringValue[i+1]='a') and (StringValue[i+2]='N')) then begin
  ResultCasted^.Hi:=$7ff80000;
  ResultCasted^.Lo:=$00000000;
  if assigned(OK) then begin
   longbool(OK^):=true;
  end;
 end else if (Base in [2,4,8,16,32]) or ((not (Base in [2..36])) and ((((i+1)<=length(StringValue)) and ((StringValue[i]='0') and (StringValue[i+1] in ['b','o','x']))))) then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  case Base of
   2:begin
    Shift:=1;
   end;
   4:begin
    Shift:=2;
   end;
   8:begin
    Shift:=3;
   end;
   16:begin
    Shift:=4;
   end;
   32:begin
    Shift:=5;
   end;
   else begin
    inc(i);
    case StringValue[i] of
     'b':begin
      Shift:=1;
     end;
     'o':begin
      Shift:=3;
     end;
     else {'x':}begin
      Shift:=4;
     end;
    end;
    inc(i);
   end;
  end;
  TwoPower:=1 shl Shift;
  Value:=0;
  Exponent:=0;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   //q:=0;      
   ExponentPower:=1;
   Digit:=0;
   while i<=length(StringValue) do begin
    c:=StringValue[i];
    if c='.' then begin
     if ExponentPower>0 then begin
      ExponentPower:=0;
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       break;
      end;
      continue;
     end else begin
      break;
     end;
    end else if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
     Digit:=ord(c)-ord('0');
    end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('a'))+10;
    end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
     Digit:=(ord(c)-ord('A'))+10;
    end else begin
     break;
    end;
    inc(i);
    HasDigits:=true;
    if ExponentPower<=0 then begin
     dec(ExponentPower);
    end;
    Value:=(Value shl Shift) or Digit;
    Overflow:=longint(int64(Value shr 53));
    if Overflow<>0 then begin
     OverflowBits:=1;
     while Overflow>1 do begin
      inc(OverflowBits);
      Overflow:=Overflow shr 1;
     end;
     DroppedBitsMask:=(1 shl OverflowBits)-1;
     DroppedBits:=Value and DroppedBitsMask;
     Value:=Value shr OverflowBits;
     Exponent:=OverflowBits;
     ZeroTail:=true;
     while i<=length(StringValue) do begin
      c:=StringValue[i];
      if (c in ['0'..'9']) and (ord(c)<=(ord('0')+TwoPower)) then begin
       Digit:=ord(c)-ord('0');
      end else if (TwoPower>10) and ((c in ['a'..'z']) and (ord(c)<=((ord('a')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (TwoPower>10) and ((c in ['A'..'Z']) and (ord(c)<=((ord('A')+TwoPower)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       break;
      end;
      inc(i);
      if Digit<>0 then begin
       ZeroTail:=false;
      end;
      inc(Exponent,Shift);
     end;
     MiddleValue:=1 shl (OverflowBits-1);
     if DroppedBits>MiddleValue then begin
      inc(Value);
     end else if DroppedBits=MiddleValue then begin
      if ((Value and 1)<>0) or not ZeroTail then begin
       inc(Value);
      end;
     end;
     while (Value and Bit53)<>0 do begin
      Value:=Value shr 1;
      inc(Exponent);
     end;
     break;
    end;
   end;
   if ExponentPower>0 then begin
    ExponentPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+longint(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if ExponentNegative then begin
    dec(ExponentPower,ExponentValue);
   end else begin
    inc(ExponentPower,ExponentValue);
   end;
   inc(Exponent,Shift*ExponentPower);
   Shift:=CountLeadingZeros64(Value);                                     
   ExponentValue:=$432-((Shift-IEEEFormatExponent)-Exponent);
   if (((ExponentValue>$34) and (ExponentValue<$7fe)) and (Exponent<IEEEFormatExponent)) and (Value<Bit53) then begin
    dec(Shift,IEEEFormatExponent);
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((int64(ExponentValue) shl 52)+Value) and int64($7fffffffffffffff);
   end else begin
    New(Mantissa);
    try
     FillChar(Mantissa^,sizeof(TWords),#0);
     Value:=Value shl Shift;
     inc(Exponent,64-Shift);
     Mantissa^[0]:=(Value shr 48) and $ffff;
     Mantissa^[1]:=(Value shr 32) and $ffff;
     Mantissa^[2]:=(Value shr 16) and $ffff;
     Mantissa^[3]:=(Value shr 0) and $ffff; 
     if (Mantissa^[0] and WordTopBit)<>0 then begin
      dec(Exponent);
      if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
       inc(Exponent,IEEEFormatExpMax-1);
       MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
        MantissaShiftRight(Mantissa^,1);
        inc(Exponent);
       end;
       if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end;
      end else if Exponent>0 then begin
       ResultCasted^.Hi:=$7ff00000;
       ResultCasted^.Lo:=$00000000;
      end else begin
       Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
       MantissaShiftRight(Mantissa^,Shift);
       MantissaRound(Negative,Mantissa^,IEEEFormatBits);
       if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
        Exponent:=1;
        if IEEEFormatExplicit=0 then begin
         Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
        end;
        Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
        ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
        ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
       end else begin
        if MantissaIsZero(Mantissa^) then begin
         ResultCasted^.Hi:=$00000000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end;
      end;
     end else begin
      ResultCasted^.Hi:=$00000000;
      ResultCasted^.Lo:=$00000000;
     end;
    finally
     Dispose(Mantissa);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end;
 end else if Base in [2..9,11..36] then begin
  ResultCasted^.Hi:=$00000000;
  ResultCasted^.Lo:=$00000000;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  if i<=length(StringValue) then begin
   case RoundingMode of
    bstd_ROUND_TO_NEAREST:begin
     NewFPURoundingMode:=rmNearest;
    end;
    bstd_ROUND_TOWARD_ZERO:begin
     NewFPURoundingMode:=rmTruncate;
    end;
    bstd_ROUND_UPWARD:begin
     NewFPURoundingMode:=rmUp;
    end;
    bstd_ROUND_DOWNWARD:begin
     NewFPURoundingMode:=rmDown;
    end;
    else begin
     NewFPURoundingMode:=rmNearest;
    end;
   end;
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
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(NewFPURoundingMode);
    end;
    Part:=0;
    Multiplier:=1;
    Digit:=0;
    Done:=false;
    ExponentPower:=1;
    while not Done do begin
     while true do begin
      c:=StringValue[i];
      if c='.' then begin
       if ExponentPower>0 then begin
        ExponentPower:=0;
        inc(i);
        if i>length(StringValue) then begin
         Done:=true;
         break;
        end;
        continue;
       end else begin
        Done:=true;
        break;
       end;
      end else if ((c>='0') and (c<='9')) and (ord(c)<=(ord('0')+Base)) then begin
       Digit:=ord(c)-ord('0');
      end else if (Base>10) and (((c>='a') and (c<='z')) and (ord(c)<=((ord('a')+Base)-10))) then begin
       Digit:=(ord(c)-ord('a'))+10;
      end else if (Base>10) and (((c>='A') and (c<='Z')) and (ord(c)<=((ord('A')+Base)-10))) then begin
       Digit:=(ord(c)-ord('A'))+10;
      end else begin
       Done:=true;
       break;
      end;
      HasDigits:=true;
      NextMultiplier:=Multiplier*longword(Base);
      if NextMultiplier>MaximumMultiplier then begin
       break;
      end;
      if ExponentPower<=0 then begin
       dec(ExponentPower);
      end;
      Part:=(Part*longword(Base))+longword(Digit);
      Multiplier:=NextMultiplier;
      Assert(Multiplier>Part);
      inc(i);
      if i>length(StringValue) then begin
       Done:=true;
       break;
      end;
     end;
     ResultCasted^.Value:=(ResultCasted^.Value*Multiplier)+Part;
    end;
    if ExponentPower>0 then begin
     ExponentPower:=0;
    end;
    ExponentValue:=0;
    ExponentNegative:=false;
    if (i<=length(StringValue)) and (StringValue[i] in ['e','E','p','P']) then begin
     inc(i);
     if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
      ExponentNegative:=StringValue[i]='-';
      inc(i);
     end;
     HasDigits:=false;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      ExponentValue:=(ExponentValue*10)+longint(ord(StringValue[i])-ord('0'));
      HasDigits:=true;
      inc(i);
     end;
    end;
    if ExponentNegative then begin
     dec(ExponentPower,ExponentValue);
    end else begin
     inc(ExponentPower,ExponentValue);
    end;
    if ExponentPower<>0 then begin
     ResultCasted^.Value:=ResultCasted^.Value*power(Base,ExponentPower);
    end;
   finally
    if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
     SetExceptionMask(OldFPUExceptionMask);
    end;
    if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
     SetPrecisionMode(OldFPUPrecisionMode);
    end;
    if OldFPURoundingMode<>NewFPURoundingMode then begin
     SetRoundMode(OldFPURoundingMode);
    end;
   end;
  end else begin
   ResultCasted^.Hi:=$00000000;
   ResultCasted^.Lo:=$00000000;
  end;
  if Negative then begin
   ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
  end;
  if HasDigits then begin
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end;
 end else begin
  HasDigits:=false;
  Value:=0;
  StoredDigitPos:=i;
  DigitPos:=0;
  TenPower:=1;
  while (i<=length(StringValue)) and (StringValue[i]='0') do begin
   HasDigits:=true;
   inc(i);
  end;
  while i<=length(StringValue) do begin
   c:=StringValue[i];
   case c of
    '0'..'9':begin
     HasDigits:=true;
     Value:=(Value*10)+(ord(c)-ord('0'));
     inc(DigitPos);
     if (Value and InvBit53Mask)<>0 then begin
      HasDigits:=false;
      break;
     end;
     if TenPower<=0 then begin
      dec(TenPower);
      if TenPower>high(TenPowers) then begin
       HasDigits:=false;
       break;
      end;
     end;
    end;
    '.':begin
     if TenPower<=0 then begin
      HasDigits:=false;
      break;
     end else begin
      TenPower:=0;
     end;
    end;
    'e','E':begin
     break;
    end;
    else begin
     HasDigits:=false;
     break;
    end;
   end;
   inc(i);
  end;
  if HasDigits then begin
   if TenPower>0 then begin
    TenPower:=0;
   end;
   ExponentValue:=0;
   ExponentNegative:=false;
   if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
    inc(i);
    if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
     ExponentNegative:=StringValue[i]='-';
     inc(i);
    end;
    HasDigits:=false;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     ExponentValue:=(ExponentValue*10)+longint(ord(StringValue[i])-ord('0'));
     HasDigits:=true;
     inc(i);
    end;
   end;
   if Value=0 then begin
    TenPower:=0;
   end else begin
    if ExponentNegative then begin
     dec(TenPower,ExponentValue);
    end else begin
     inc(TenPower,ExponentValue);
    end;
    if TenPower<>0 then begin
     if TenPower>0 then begin
      if ((DigitPos+TenPower)>MaxFastPathDigits) or (TenPower>high(TenPowers)) then begin
       HasDigits:=false;
      end else begin
       Value:=Value*TenPowers[TenPower];
       TenPower:=0;
       if (Value and InvBit53Mask)<>0 then begin
        HasDigits:=false;
       end;
      end;
     end else begin
      if (-TenPower)>high(TenPowers) then begin
       HasDigits:=false;
      end else begin
       i:=-TenPower;
       while (i>0) and (TenPower<0) do begin
        if (Value mod TenPowers[i])=0 then begin
         Value:=Value div TenPowers[i];
         inc(TenPower,i);
        end else begin
         if (i and 1)=0 then begin
          i:=i shr 1;
         end else begin
          dec(i);
         end;
        end;
       end;
      end;
     end;
    end;
   end;
  end;
  if HasDigits then begin
   if Value=0 then begin
    ResultCasted^.Hi:=$00000000;
    ResultCasted^.Lo:=$00000000;
   end else begin
    Shift:=CountLeadingZeros64(Value)-11;
    if Shift>=0 then begin
     Value:=Value shl Shift;
    end else begin
     Value:=Value shr (-Shift);
    end;
    ResultCasted^.Value64:=((int64($432-Shift) shl 52)+Value) and int64($7fffffffffffffff);
   end;
   if TenPower<>0 then begin
    case RoundingMode of
     bstd_ROUND_TO_NEAREST:begin
      NewFPURoundingMode:=rmNearest;
     end;
     bstd_ROUND_TOWARD_ZERO:begin
      NewFPURoundingMode:=rmTruncate;
     end;
     bstd_ROUND_UPWARD:begin
      NewFPURoundingMode:=rmUp;
     end;
     bstd_ROUND_DOWNWARD:begin
      NewFPURoundingMode:=rmDown;
     end;
     else begin
      NewFPURoundingMode:=rmNearest;
     end;
    end;
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
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(NewFPURoundingMode);
     end;
     if TenPower>0 then begin
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value*TenPowers[TenPower];
      end;
     end else begin
      TenPower:=-TenPower;
      while TenPower>high(TenPowers) do begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[high(TenPowers)];
       dec(TenPower,high(TenPowers));
      end;
      if TenPower>0 then begin
       ResultCasted^.Value:=ResultCasted^.Value/TenPowers[TenPower];
      end;
     end;
    finally
     if OldFPUExceptionMask<>DtoAFPUExceptionMask then begin
      SetExceptionMask(OldFPUExceptionMask);
     end;
     if OldFPUPrecisionMode<>DtoAFPUPrecisionMode then begin
      SetPrecisionMode(OldFPUPrecisionMode);
     end;
     if OldFPURoundingMode<>NewFPURoundingMode then begin
      SetRoundMode(OldFPURoundingMode);
     end;
    end;
   end;
   if Negative then begin
    ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
   end;
   if assigned(OK) then begin
    longbool(OK^):=true;
   end;
  end else begin
   i:=StoredDigitPos;
   New(MantissaMultiplicator);
   New(Mantissa);
   New(Temp);
   New(Digits);
   try
    FillChar(Digits^,sizeof(TDigits),#0);

    DigitPos:=0;
    TenPower:=0;
    HasDigits:=false;
    Started:=false;
    ExponentNegative:=false;
    ExponentValue:=0;
    while (i<=length(StringValue)) and (StringValue[i]='0') do begin
     HasDigits:=true;
     inc(i);
    end;
    while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
     HasDigits:=true;
     Started:=true;
     if DigitPos<=high(TDigits) then begin
      Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
      inc(DigitPos);
     end;
     inc(TenPower);
     inc(i);
    end;
    if (i<=length(StringValue)) and (StringValue[i]='.') then begin
     inc(i);
     if not Started then begin
      while (i<=length(StringValue)) and (StringValue[i]='0') do begin
       HasDigits:=true;
       dec(TenPower);
       inc(i);
      end;
     end;
     while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
      HasDigits:=true;
      if DigitPos<=high(TDigits) then begin
       Digits^[DigitPos]:=ord(StringValue[i])-ord('0');
       inc(DigitPos);
      end;
      inc(i);
     end;
    end;
    if HasDigits then begin
     if (i<=length(StringValue)) and (StringValue[i] in ['e','E']) then begin
      inc(i);
      if (i<=length(StringValue)) and (StringValue[i] in ['+','-']) then begin
       ExponentNegative:=StringValue[i]='-';
       inc(i);
      end;
      HasDigits:=false;
      while (i<=length(StringValue)) and (StringValue[i] in ['0'..'9']) do begin
       ExponentValue:=(ExponentValue*10)+longint(ord(StringValue[i])-ord('0'));
       HasDigits:=true;
       inc(i);
      end;
     end;
     if HasDigits then begin
      if ExponentNegative then begin
       dec(TenPower,ExponentValue);
      end else begin
       inc(TenPower,ExponentValue);
      end;

      FillChar(Mantissa^,sizeof(TWords),#0);

      Bit:=WordTopBit;
      StoredDigitPos:=0;
      Started:=false;
      TwoPower:=0;
      MantissaPosition:=0;
      while MantissaPosition<MantissaWords do begin
       Carry:=0;  
       while (DigitPos>StoredDigitPos) and (Digits^[DigitPos-1]=0) do begin
        dec(DigitPos);
       end;
       if DigitPos<=StoredDigitPos then begin
        break;
       end;
       DigitPosBackwards:=DigitPos;
       while DigitPosBackwards>StoredDigitPos do begin
        dec(DigitPosBackwards);
        i:=(2*Digits^[DigitPosBackwards])+Carry;
        if i>=10 then begin
         dec(i,10);
         Carry:=1;
        end else begin
         Carry:=0;
        end;
        Digits^[DigitPosBackwards]:=i;
       end;
       if Carry<>0 then begin
        Mantissa^[MantissaPosition]:=Mantissa^[MantissaPosition] or Bit;
        Started:=true;
       end;
       if Started then begin
        if Bit=1 then begin
         Bit:=WordTopBit;
         inc(MantissaPosition);
        end else begin
         Bit:=Bit shr 1;
        end;
       end else begin
        dec(TwoPower);
       end;
      end;
      inc(TwoPower,TenPower);

      if TenPower<0 then begin
       for i:=0 to high(TWords)-1 do begin
        MantissaMultiplicator^[i]:=$cccc;
       end;
       MantissaMultiplicator^[high(TWords)]:=$cccd;
       ExtraTwos:=-2;
       TenPower:=-TenPower;
      end else if TenPower>0 then begin
       MantissaMultiplicator^[0]:=$a000;
       for i:=1 to high(TWords) do begin
        MantissaMultiplicator^[i]:=$0000;
       end;
       ExtraTwos:=3;
      end else begin
       ExtraTwos:=0;
      end;
      while TenPower<>0 do begin
       if (TenPower and 1)<>0 then begin
        inc(TwoPower,ExtraTwos+MantissaMultiply(Mantissa,MantissaMultiplicator));
       end;
       inc(ExtraTwos,ExtraTwos+MantissaMultiply(MantissaMultiplicator,MantissaMultiplicator));
       TenPower:=TenPower shr 1;
      end;

      Exponent:=TwoPower;
      if (Mantissa^[0] and WordTopBit)<>0 then begin
       dec(Exponent);

       if (Exponent>=(2-IEEEFormatExpMax)) and (Exponent<=IEEEFormatExpMax) then begin
        inc(Exponent,IEEEFormatExpMax-1);
        MantissaShiftRight(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if MantissaTestBit(Mantissa^,IEEEFormatExponent+IEEEFormatExplicit-1) then begin
         MantissaShiftRight(Mantissa^,1);
         inc(Exponent);
        end;
        if Exponent>=(IEEEFormatExpMax shl 1)-1 then begin
         ResultCasted^.Hi:=$7ff00000;
         ResultCasted^.Lo:=$00000000;
        end else begin
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end;
       end else if Exponent>0 then begin
        ResultCasted^.Hi:=$7ff00000;
        ResultCasted^.Lo:=$00000000;
       end else begin
        Shift:=IEEEFormatExplicit-(Exponent+(IEEEFormatExpMax-(2+IEEEFormatExponent)));
        MantissaShiftRight(Mantissa^,Shift);
        MantissaRound(Negative,Mantissa^,IEEEFormatBits);
        if (Mantissa^[IEEEFormatOnePos] and IEEEFormatOneMask)<>0 then begin
         Exponent:=1;
         if IEEEFormatExplicit=0 then begin
          Mantissa^[IEEEFormatOnePos]:=Mantissa^[IEEEFormatOnePos] and not IEEEFormatOneMask;
         end;
         Mantissa^[0]:=Mantissa^[0] or (Exponent shl (WordBitMask-IEEEFormatExponent));
         ResultCasted^.Hi:=(((Exponent shl 4) or (Mantissa^[0] and $f)) shl 16) or Mantissa^[1];
         ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
        end else begin
         if MantissaIsZero(Mantissa^) then begin
          ResultCasted^.Hi:=$00000000;
          ResultCasted^.Lo:=$00000000;
         end else begin
          ResultCasted^.Hi:=(Mantissa^[0] shl 16) or Mantissa^[1];
          ResultCasted^.Lo:=(Mantissa^[2] shl 16) or Mantissa^[3];
         end;
        end;
       end;
      end else begin
       ResultCasted^.Hi:=$00000000;
       ResultCasted^.Lo:=$00000000;
      end;
      if Negative then begin
       ResultCasted^.Hi:=ResultCasted^.Hi or $80000000;
      end;
      if assigned(OK) then begin
       longbool(OK^):=true;
      end;
     end;
    end;
   finally
    Dispose(MantissaMultiplicator);
    Dispose(Mantissa);
    Dispose(Temp);
    Dispose(Digits);
   end;
  end;
 end;
end;

end.
