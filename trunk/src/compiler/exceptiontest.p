program exceptiontest;
{$apptype console}

begin
 try
  try
   writeln('Foo');
  except
   on e:Exception do begin
    if assigned(e) then begin
    end;
   end;
  end;
 finally
  writeln('Bar');
 end;
end.
