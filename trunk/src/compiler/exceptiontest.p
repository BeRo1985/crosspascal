program exceptiontest;
{$apptype console}

begin
 try
  try
   writeln('Foo');
   raise Exception.Create('Ich bin boese');
  except
   on e:Exception do begin
    if assigned(e) then begin
     writeln(e.Message);
    end else begin
     writeln(1);
    end;
   end;
  end;
 finally
  writeln('Bar');
 end;
end.
