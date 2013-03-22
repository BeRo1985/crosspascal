program test;

{$IFDEF VER150}
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
	  
const fooSize = 14999;

var Argh: array[0..fooSize] of Byte;
    A, B: string;
    Temp: PChar;
	StartTime: Cardinal;
	
procedure BubbleSort;
var i,k: integer;
    j: Byte;
    B: Boolean;
begin
  for i:=0 to fooSize do
    Argh[i]:=i*119;
	
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
end;

procedure TestA;
 procedure TestB;
 begin
 {$IFDEF VER150}
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

begin
 Writeln('Hello World!');
 
 B:=A;
 
 // bug:
 //B[3]:='X';
 
 A:=MyString + Mystring2;
 WriteLn('"',A,'" has a length of ', Length(MyString + MyString2));

 StartTime := GetTickCount;
 BubbleSort;
 Writeln(GetTickCount - StartTime, ' ms for bubblesort');
 
 TestA;
end.
