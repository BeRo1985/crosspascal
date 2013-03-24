program objecttest;

type TTestObject=object
      public
       a:longint;
       constructor Create;
       destructor Destroy;
       procedure Bla; virtual; abstract;
       procedure Bluh; virtual;
     end;

constructor TTestObject.Create;
begin
 a:=4;
end;

destructor TTestObject.Destroy;
begin
end;

procedure TTestObject.Bluh;
begin
 WriteLn(a);
end;

var TestObject:TTestObject;
begin
 TestObject.Create;
 TestObject.Bluh;
end.
