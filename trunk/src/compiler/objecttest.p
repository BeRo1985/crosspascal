program objecttest;

type TTestObject=object
      public
       a:longint;
       constructor Create;
       destructor Destroy;
       procedure Bla; virtual; abstract;
       procedure Bluh(b:longint); virtual;
     end;

     TTestObject2=object(TTestObject)
      public
       aa:longint;
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

procedure TTestObject2.Bluh(b:longint);
begin
 WriteLn((a+b)*aa);
end;

var TestObject:TTestObject;
    TestObject2:TTestObject2;
begin
 TestObject.Create;
 TestObject.Bluh(4);
 TestObject2.Create;
 TestObject2.aa:=TestObject2.a;
 TestObject2.Bluh(4);
end.
