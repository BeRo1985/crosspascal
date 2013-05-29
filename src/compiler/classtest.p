program classtest;
{$apptype console}

type TTestClass=class
      public
       a:longint;
       constructor Create;
       destructor Destroy;
     end;

constructor TTestClass.Create;
begin
 a:=123;
end;

destructor TTestClass.Destroy;
begin
end;

var TestClass:TTestClass;
begin
 TestClass:=TTestClass.Create;
 TestClass.Destroy;
end.
