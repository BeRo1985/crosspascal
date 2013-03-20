program test;

type THandle = Cardinal;

function GetStdHandle(nStdHandle: Cardinal): THandle; stdcall; external 'windows.h' name 'GetStdHandle';
function WriteConsole(Handle: THandle; Buffer: Pointer; CharsToWrite: Cardinal; CharsWritten: Pointer; Reserved: Pointer): Boolean; external 'windows.h' name 'WriteConsole';

var Handle: Cardinal;
    Dummy: Cardinal;
    SomeFloat: Single;

type PFoo = ^Foo;
     Foo = record
       Wurst: Byte;
     end;

const HelloWorld: PChar = 'Hallo Welt!\n'+#10;

begin
 [[[ int a = 1; ]]]  
 SomeFloat:=1;
 if SomeFloat < 1 then
 begin

 end;
 [[[
   printf("blblblblbl \n");
 ]]]
 Handle := GetStdHandle(Cardinal(-11));
 if Handle <> 0 then
  WriteConsole(Handle, HelloWorld, 14, @Dummy, nil);
 if <<< a >>> <> 0 then begin
  WriteConsole(Handle, HelloWorld, 14, @Dummy, nil);
 end;
end.
