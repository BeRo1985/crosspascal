unit DebugLogUtils;
{$i Compiler.inc}

interface

uses BeRoStream,BeRoUtils;

const DebugLogActive:boolean=true;
      DebugLogDumpTree:boolean=true;
      DebugLogDumpUnitManager:boolean=true;

procedure DebugLog(Line:ansistring);

implementation

var LogStream:TBeRoStream;

procedure DebugLog(Line:ansistring);
begin
 if not DebugLogActive then begin
  exit;
 end;
 if not assigned(LogStream) then begin
  LogStream:=TBeRoFileStream.CreateNew(ExtractFilePath(PARAMSTR(0))+'debug.log');
 end;
 LogStream.WriteLine(Line);
end;

initialization
 LogStream:=nil;
finalization
 if DebugLogActive then begin
  if assigned(LogStream) then begin
   LogStream.Destroy;
   LogStream:=nil;
  end;
 end;
end.
