program exceptiontest;
{$apptype console}

begin
 try
  try
   writeln('Foo');
  except
   on e:Exception do begin
   end;
  end;
 finally
  writeln('Bar');
 end;
end.
