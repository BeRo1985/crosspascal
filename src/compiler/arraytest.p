program arraytest;

type TSomeRecord = record
        a,b,c: Integer;
     end;

     TSomeTypedDynArray = array of TSomeRecord;
     TSomeOtherTypedDynArray = array of Integer;

     TSomeUntypedDynArray = Array of record
       d,e,f: Integer;
     end;

var a: TSomeTypedDynArray;
    b: TSomeOtherTypedDynArray;
    c: TSomeUntypedDynArray;
    d: TSomeRecord;
begin
  Setlength(a, 10);
  Setlength(c, 20);

  a[0] := d;
  d:=a[0];

//  a[0].a := 10;
end;