program withtest;
{$apptype console}

type TTestRecord=record
      a:longint;
     end;

var TestRecord:TTestRecord;
begin
 with TestRecord do begin
  a:=6;
 end;
 writeln(TestRecord.a);
end.
