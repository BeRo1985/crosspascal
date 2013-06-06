program consttest;

type TSomeRecord=record
      a,b,c:longint;
     end;

procedure pa(const r:TSomeRecord);
begin
 writeln(r.a);
 writeln(r.b);
 writeln(r.c);
end;

procedure pb(const r:TSomeRecord);
begin
 pa(r);
end;

var r:TSomeRecord;
begin
 r.a:=1;
 r.b:=2;
 r.c:=3;
 pb(r);
end;
