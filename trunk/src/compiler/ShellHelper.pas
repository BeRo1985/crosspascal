unit ShellHelper;

interface

function GetDosOutput(const CommandLine: string): string;

implementation

uses
  Windows,
  SysUtils;

function GetDosOutput(const CommandLine: string): string;
var
  SA: TSecurityAttributes;
  SI: TStartupInfo;
  PI: TProcessInformation;
  StdOutPipeRead, StdOutPipeWrite: THandle;
  WasOK: Boolean;
  Buffer: array[0..255] of AnsiChar;
  BytesRead: Cardinal;
  WorkDir, Line: string;
begin
  with SA do
  begin
    nLength := SizeOf(SA);
    bInheritHandle := True;
    lpSecurityDescriptor := nil;
  end;
  {create pipe for standard output redirection}
  CreatePipe(StdOutPipeRead, {read handle}
    StdOutPipeWrite, {write handle}
    @SA, {security attributes}
    0 {number of bytes reserved for pipe - 0 default}
    );
  try
    {Make child process use StdOutPipeWrite as standard out, and
    make sure it does not show on screen}
    with SI do
    begin
      FillChar(SI, SizeOf(SI), 0);
      cb := SizeOf(SI);
      dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
      wShowWindow := SW_HIDE;
      hStdInput := GetStdHandle(STD_INPUT_HANDLE); {don't redirect stdinput}
      hStdOutput := StdOutPipeWrite;
      hStdError := StdOutPipeWrite;
    end;
    {launch the command line compiler
    WorkDir := 'C:\';}
    WorkDir := '';
    WasOK := CreateProcess(nil, PChar(CommandLine), nil, nil, True, 0, nil, nil, SI,
      PI);
//    Now that the handle has been inherited, close write to be safe.We don't
//      want to read or write to it accidentally}
      CloseHandle(StdOutPipeWrite);
    {if process could be created then handle its output}
    if not WasOK then
      raise Exception.Create('Could not execute command line!')
    else
    try
      {get all output until DOS app finishes}
      Line := '';
      repeat
        {read block of characters (might contain carriage returns and line feeds)}
        WasOK := ReadFile(StdOutPipeRead, Buffer, 255, BytesRead, nil);
        {has anything been read?}
        if BytesRead > 0 then
        begin
          {finish buffer to PChar}
          Buffer[BytesRead] := #0;
          {combine the buffer with the rest of the last run}
          Line := Line + Buffer;
        end;
      until
        not WasOK or (BytesRead = 0);
      {wait for console app to finish (should be already at this point)}
      WaitForSingleObject(PI.hProcess, INFINITE);
    finally
      {Close all remaining handles}
      CloseHandle(PI.hThread);
      CloseHandle(PI.hProcess);
    end;
  finally
    result := Line;
    CloseHandle(StdOutPipeRead);
  end;
end;


end.
