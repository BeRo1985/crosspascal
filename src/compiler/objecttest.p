program objecttest;

type TTestObject=object
      public
       a:longint;
       constructor Create;
       destructor Destroy;
       procedure Bla; virtual; abstract;
       procedure Bluh(b:longint); virtual;
     end;

constructor TTestObject.Create;
begin
 a:=4;
end;

destructor TTestObject.Destroy;
begin
end;

procedure TTestObject.Bluh(b:longint);
begin
 WriteLn(a+b);
end;

var TestObject:TTestObject;
begin
 TestObject.Create;
 TestObject.Bluh(4);
end.
