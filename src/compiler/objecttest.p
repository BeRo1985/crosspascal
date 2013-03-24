program objecttest;

type TTestObject=object
      public
       constructor Create;
       destructor Destroy;
       procedure Bla; virtual; abstract;
       procedure Bluh; virtual;
     end;

constructor TTestObject.Create;
begin
end;

destructor TTestObject.Destroy;
begin
end;

procedure TTestObject.Bluh;
begin
end;

begin
end.
