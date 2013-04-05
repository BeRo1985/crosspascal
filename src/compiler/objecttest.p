program objecttest;

type PTestObject=^TTestObject;
     TTestObject=object
      public
       a:longint;
       constructor Create;
       destructor Destroy;
       procedure Bla; virtual; abstract;
       procedure Bluh(b:longint); virtual;
       procedure Blieh; virtual 42;
     end;

     PTestObject2=^TTestObject2;
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
 WriteLn(' TTestObject.Bluh: ',a+b);
end;

procedure TTestObject.Blieh;
begin
 WriteLn(' TTestObject.Blieh!');
end;

procedure TTestObject2.Bluh(b:longint);
begin
 WriteLn('TTestObject2.Bluh: ',(a+b)*aa);
 inherited Bluh(b);
end;

var TestObject:PTestObject;
    TestObject2:PTestObject2;
begin
 New(TestObject,Create);
 New(TestObject2,Create);
 TestObject^.Bluh(4);
 TestObject2^.aa:=TestObject2.a;
 TestObject2^.Bluh(4);
 if typeof(TestObject^)=typeof(TestObject2^) then begin
  WriteLn('Same object types');
 end else begin
  WriteLn('Different object types');
 end;
 TestObject^.Blieh;
 Dispose(TestObject,Destroy);
 Dispose(TestObject2,Destroy);
end.
