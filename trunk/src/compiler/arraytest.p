program arraytest;

type TSomeRecord = record
        a,b,c: Integer;
     end;

     TSomeTypedDynArray = array of TSomeRecord;
     TSomeOtherTypedDynArray = array of Integer;

     TSomeUntypedDynArray = Array of record
       d,e,f: Integer;
     end;

var a,g: TSomeTypedDynArray;
    b: TSomeOtherTypedDynArray;
    c: TSomeUntypedDynArray;
    d: TSomeRecord;
    i: Integer;
begin
  Setlength(a, 10);
  Setlength(b, 10);
  Setlength(c, 20);

  a[0] := d;
  d:=a[0];
  g:=a;
  b[5] := 10;
  a[0].b := 0;
  for i:=0 to 9 do
  Writeln(B[i]);
end;