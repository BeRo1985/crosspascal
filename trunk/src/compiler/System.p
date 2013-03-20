unit System;
{$c+}
{$j+}

interface                                                                               

const CompilerInfoString:pchar='OBJPAS2C';
     
      fmClosed=$D7B0;
      fmInput=$D7B1;
      fmOutput=$D7B2;
      fmInOut=$D7B3;

      faReadOnly=$01;
      faHidden=$02;
      faSysFile=$04;
      faVolumeID=$08;
      faDirectory=$10;
      faArchive=$20;
      faAnyFile=$3F;

      FileMode:integer=2;

      RandSeed:integer=0;

var Input,Output:text;

procedure Randomize;
function Random(Max:integer):integer; overload;
function Random:double; overload;

procedure FillChar(var Dest;Count:integer;Value:byte); overload;
procedure FillChar(var Dest;Count:integer;Value:ansichar); overload;

procedure GetMem(var p;Size:longint);

procedure FreeMem(var p); overload;
procedure FreeMem(var p;Size:longint); overload;

function Power(Base,Exponent:extended):extended;
function Exp(x:extended):extended;
function Ln(x:extended):extended;

function Min(a,b:longint):longint; overload;
function Min(a,b:int64):int64; overload;
function Min(a,b:extended):extended; overload;

function Max(a,b:longint):longint; overload;
function Max(a,b:int64):int64; overload;
function Max(a,b:extended):extended; overload;

implementation

procedure NextRandSeed;
begin
 RandSeed:=(RandSeed*$8088405)+1;
end;

procedure Randomize;
begin
 NextRandSeed;
end;

function Random(Max:integer):integer;
begin
 NextRandSeed;
 if Max>0 then begin
  result:=RandSeed mod Max;
 end else begin
  result:=0;
 end;
end;

function Random:double;
const DivFactor=(1.0/$10000)/$10000;
begin
 NextRandSeed;
 result:=RandSeed*DivFactor;
end;

procedure FillChar(var Dest;Count:integer;Value:byte);
begin
end;

procedure FillChar(var Dest;Count:integer;Value:ansichar);
begin
end;

procedure GetMem(var p;Size:longint);
begin
end;

procedure FreeMem(var p);
begin
end;

procedure FreeMem(var p;Size:longint);
begin
end;

function Power(Base,Exponent:extended):extended;
begin
end;

function Exp(x:extended):extended;
begin
end;

function Ln(x:extended):extended;
begin
end;

function Min(a,b:longint):longint;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Min(a,b:int64):int64;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Min(a,b:extended):extended;
begin
 if a<b then begin
  result:=a;
 end else begin
  result:=b;
 end;
end;

function Max(a,b:longint):longint;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

function Max(a,b:int64):int64;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

function Max(a,b:extended):extended;
begin
 if a<b then begin
  result:=b;
 end else begin
  result:=a;
 end;
end;

begin
end.