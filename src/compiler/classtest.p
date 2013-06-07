program classtest;
{$apptype console}

type TTestClass=class
      public
       a:longint;
       constructor Create;
       destructor Destroy; override;
      published
       procedure Test; virtual;
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

procedure TTestClass.Test;
begin
 a:=a*2;
end;

var TestClass:TTestClass;
begin
 TestClass:=TTestClass.Create;
 TestClass.Test;
 writeln(TestClass.a);
 TestClass.Destroy;
end.                                                              
