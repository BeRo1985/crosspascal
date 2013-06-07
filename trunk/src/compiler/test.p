program test;

{$IFDEF FPC}
{$APPTYPE console}
uses Windows;
{$ELSE}
function GetTickCount: Integer; external 'windows.h' name 'GetTickCount';
{$ENDIF}

type THandle = Cardinal;

var Handle: Cardinal;
    Dummy: Cardinal;
    SomeFloat: Single;
type PFoo = ^Foo;
     Foo = record
       Wurst: Byte;
     end;

const HelloWorld: PChar = 'Hallo Welt!\n'+#10;
      MyString: string = 'Hallo';
      MyString2: string = ' Stringwelt!';
	  
const fooSize = 19999;

var
    A, B: string;
    Temp: PChar;
	StartTime: Cardinal;
	
var Argh: array[0..fooSize] of Integer;

procedure BubbleSort;
var i,k: integer;
    j: Byte;
    B: Boolean;
begin
  for i:=0 to fooSize do
    Argh[i]:=Byte(i*119);
	
  k:=0;
  repeat
   b := True;
   for i:=0 to fooSize-1 do
    if Argh[i]>Argh[i+1] then
    begin
      b := False;
      j := Argh[i];
      Argh[i] := Argh[i+1];
      Argh[i+1] := j;
	  k := k + 1;
    end;
  until b;
  Writeln(k, ' iterations');
  k := 2;
  for i:=0 to 1 do
  case k of
    1: writeln(100);
	2: begin
	  writeln(111);
	  Break;
	  writeln(112);
	end;
	3,4: writeln(123);
	else Writeln('kotzen');
  end;
end;

procedure BubbleSort2;
var i,k: integer;
    j: Byte;
    B: Boolean;
    x,x2: Cardinal;
begin
  for i:=0 to fooSize do
    Argh[i]:=Byte(i*119);
	
  k:=0;
  x:=foosize - 1;
  repeat
   b := True;
   if x = 0 then
    Break;
	
   for i:=0 to x do
    if Argh[i]>Argh[i+1] then
    begin
      b := False;
      j := Argh[i];
      Argh[i] := Argh[i+1];
      Argh[i+1] := j;
	    k := k + 1;
      x2 := i;
	  Continue;
	  Break;
    end;
   x:=x2;
  until (b) or (x < 1);
  Writeln(k, ' iterations');
end;

procedure TestA;
 procedure TestB;
 begin
 {$IFDEF FPC}
 Writeln('Hey!');
 {$ELSE}
 // inline c
[[[
  printf("Hey!\n");
]]]
 {$ENDIF}
 end;
begin
 TestB;
end;


label blrgh;

var
  i: Integer;
  
begin
 Writeln('Hello World!');
 goto blrgh;
 
 blrgh:
 
 A:=MyString + Mystring2;
 B:=A;
 
 // bug:
 //B[3]:='X';
 
 WriteLn('"',A,'" has a length of ', Length(MyString + MyString2));

 StartTime := GetTickCount;
  BubbleSort; 
 Writeln(GetTickCount - StartTime, ' ms');
 
 StartTime := GetTickCount;
 BubbleSort2;
 Writeln(GetTickCount - StartTime, ' ms for bubblesort2');
 
 TestA;
end.
