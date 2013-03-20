unit System;

interface

type TTest=record
      a:integer;
      b:integer;
      c:array[1..3] of byte
     end;

const CompilerInfoString:pchar='OBJPAS2CPP';

      Test:array[0..3] of integer=(0,1,2,6);

      Test2:TTest=(a:6;b:45;c:(8,7,9));
      
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
 if Max<>0 then begin
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

begin
end.