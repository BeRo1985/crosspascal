program test;

type THandle = Cardinal;

function GetTickCount: Integer; external 'windows.h' name 'GetTickCount';

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

var Argh: array[0..fooSize] of Byte;
    A, B: string;
    Temp: PChar;
	StartTime: Cardinal;
	
procedure BubbleSort;
var i,j,k: integer;
    B: Boolean;
begin
  for i:=0 to fooSize do
    Argh[i]:=i*119;
  repeat
   b := True;
   for i:=0 to fooSize-1 do
    if Argh[i]>Argh[i+1] then
    begin
      b := False;
      j := Argh[i];
      Argh[i] := Argh[i+1];
      Argh[i+1] := j;
    end;
  until b;
end;

procedure TestA;
 procedure TestB;
 begin
[[[
  printf("Hey!\n");
]]]
 end;
begin
 TestB;
end;

begin
 Writeln('Hello World!');
 
 A:=MyString + Mystring2;
 B:=A;
 WriteLn('"',A,'" has a length of ', Length(MyString + MyString2));

 StartTime := GetTickCount;
 BubbleSort;
 Writeln(GetTickCount - StartTime, ' ms for bubblesort');
 
 TestA;
end.
