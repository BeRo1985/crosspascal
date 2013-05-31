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
 inherited Create; 
 a:=123;
end;

destructor TTestClass.Destroy;
begin
 inherited Destroy;
end;

var TestClass:TTestClass;
begin
 TestClass:=TTestClass.Create;
 writeln(TestClass.a);
 TestClass.Destroy;
end.
