program preprocessortest;

{$define caseA}

procedure bla;
begin
{$ifdef caseA}
{$ifdef caseB}
  writeln('A+ B+');
{$else}
  writeln('A+ B-');
{$endif}
{$else}
{$ifdef caseB}
  writeln('A- B+');
{$else}
  writeln('A- B-');
{$endif}
{$endif}
end;

begin
 bla;
end.
