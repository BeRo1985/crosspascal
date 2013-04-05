unit Compiler;
{$i Compiler.inc}

interface

uses SysUtils,BeRoStream,BeRoUtils,Globals,Parser,Error,Symbols,Tree,OptimizerHighLevel,
     UnitManager,Code,ShellHelper,DebugLogUtils,libtcc;

type TCompiler=class
      private
      public
       Options:TOptions;
       Error:TError;
       SymbolManager:TSymbolManager;
       TreeManager:TTreeManager;
       UnitManager:TUnitManager;
       GlobalSwitches:TGlobalSwitches;
       LocalSwitches:TLocalSwitches;
       constructor Create;
       destructor Destroy; override;
       procedure Compile(TheInputStream:TBeRoStream;TheFileName:ansistring;UnitLevel:longint=0);
       procedure CompileFile(FileName:ansistring;UnitLevel:longint=0);
       function GetErrors:ansistring;
       procedure CompileMainFile(FileName:ansistring);
     end;

implementation

procedure libtccErrorFunc(opaque:pointer;msg:pansichar); cdecl;
var s,ss,sss:ansistring;
    i:longint;
begin
 if assigned(opaque) then begin
  s:=msg;
  try
   i:=pos(':',s);
   if i>0 then begin
    sss:='';
    ss:='['+copy(s,1,i-1)+']';
    delete(s,1,i);
    i:=pos(':',s);
    if i>0 then begin
     sss:='('+copy(s,1,i-1)+'):';
     delete(s,1,i);
    end;
    s:=ss+sss+s;
   end;
   if pos('error: ',s)>0 then begin
    StringReplaceAll(s,'error: ','');
    TCompiler(opaque).Error.ErrorList.Add(s);
    TCompiler(opaque).Error.Errors:=true;
   end else if pos('warning: ',s)>0 then begin
    StringReplaceAll(s,'warning: ','');
    TCompiler(opaque).Error.WarningList.Add(s);
    TCompiler(opaque).Error.Warnings:=true;
   end else if pos('hint: ',s)>0 then begin
    StringReplaceAll(s,'hint: ','');
    TCompiler(opaque).Error.HintList.Add(s);
    TCompiler(opaque).Error.Hints:=true;
   end else begin
    TCompiler(opaque).Error.ErrorList.Add(s);
    TCompiler(opaque).Error.Errors:=true;
   end;
  finally
   s:='';
  end;
 end;
end;

constructor TCompiler.Create;
begin
 inherited Create;
 FillChar(Options,SizeOf(TOptions),#0);
 Options.Bits:=32;
 GlobalSwitches:=DefaultGlobalSwitches;
 LocalSwitches:=DefaultLocalSwitches;
 Error:=TError.Create(@Options);
 Error.LocalSwitches:=@LocalSwitches;
 SymbolManager:=TSymbolManager.Create(Error,@Options,@GlobalSwitches);
 TreeManager:=TTreeManager.Create(Error,SymbolManager,@Options);
 UnitManager:=TUnitManager.Create(Error,SymbolManager,self,@Options,@GlobalSwitches);
end;

destructor TCompiler.Destroy;
begin
 UnitManager.Free;
 TreeManager.Free;
 SymbolManager.Free;
 Error.Free;
 inherited Destroy;
end;

procedure TCompiler.Compile(TheInputStream:TBeRoStream;TheFileName:ansistring;UnitLevel:longint=0);
var Parser:TParser;
    Name:ansistring;
    TCCState:PTCCState;
    OldLocalSwitches:PLocalSwitches;
begin
 New(OldLocalSwitches);
 OldLocalSwitches^:=LocalSwitches;
 LocalSwitches:=DefaultLocalSwitches;
 Error.LocalSwitches:=@LocalSwitches;
 try
  Parser:=TParser.Create(TheInputStream,TheFileName,Error,SymbolManager,UnitManager,self,UnitLevel,@Options,@GlobalSwitches,@LocalSwitches);
  Parser.Parse;
  if assigned(Parser.ModuleSymbol) then begin
   Name:=Parser.ModuleSymbol.OriginalFileName;
  end else begin
   Name:='UNKNOWN';
  end;
  Parser.Destroy;
  if libtccReady then begin
   if not Error.Errors then begin
    TCCState:=tcc_new;
    try
     tcc_set_error_func(TCCState,self,libtccErrorFunc);
  {  tcc_add_include_path(TCCState,pansichar(SysUtils.ExtractFilePath(ParamStr(0))));
     tcc_add_include_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'include'));
     tcc_add_include_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'include')+'sys'));
     tcc_add_include_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'include')+'sec_api'));
     tcc_add_include_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'include')+'winapi'));
     tcc_set_lib_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'lib'));}
     tcc_set_output_type(TCCState,TCC_OUTPUT_OBJ);
     tcc_set_options(TCCState,'-O3');
     if tcc_add_file(TCCState,pansichar(ChangeFileExt(Name,'.c')))>=0 then begin
      if tcc_output_file(TCCState,pansichar(ChangeFileExt(Name,'.o')))<0 then begin
       Error.AddErrorCode(10000);
      end;
     end else begin
      Error.AddErrorCode(10000);
     end;
    finally
     tcc_delete(TCCState);
    end;
   end;
  end else begin
   DebugLog('Compiling '+name+'.c: '+GetDosOutput(Options.TargetCompiler+' -c '+pansichar(ChangeFileExt(Name,'.c'))));
  end;
 finally
  Error.LocalSwitches:=@LocalSwitches;
  if UnitLevel<>0 then begin
   LocalSwitches:=OldLocalSwitches^;
  end;
  Dispose(OldLocalSwitches);
 end;
end;

procedure TCompiler.CompileFile(FileName:ansistring;UnitLevel:longint=0);
var FileStream:TBeRoFileStream;
    Stream:TBeRoStream;
begin
 if FileExists(FileName) then begin
  FileStream:=TBeRoFileStream.Create(FileName);
  Stream:=TBeRoStream.Create;
  Stream.Assign(FileStream);
  FileStream.Destroy;
  Stream.Seek(0);
  Compile(Stream,FileName,UnitLevel);
  Stream.Destroy;
 end else begin
  Error.AbortCode(40,FileName);
 end;
end;

function TCompiler.GetErrors:ansistring;
begin
 result:=Error.Text;
end;

procedure TCompiler.CompileMainFile(FileName:ansistring);
var s:ansistring;
    i:longint;
    TCCState:PTCCState;
begin
 CompileFile(FileName);
 if libtccReady then begin
  if not Error.Errors then begin
   TCCState:=tcc_new;
   try
    tcc_set_error_func(TCCState,self,libtccErrorFunc);
 {  tcc_set_lib_path(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'lib')));
    tcc_add_library(TCCState,pansichar(SysUtils.IncludeTrailingPathDelimiter(SysUtils.IncludeTrailingPathDelimiter(SysUtils.ExtractFilePath(ParamStr(0)))+'lib')+'libtcc1.a'));}
    tcc_set_output_type(TCCState,TCC_OUTPUT_EXE);
    tcc_set_options(TCCState,'-O3');
    s:=ChangeFileExt(ExtractFileName(FileName),'.o');
    if tcc_add_file(TCCState,pansichar(s))<0 then begin
     Error.AddErrorCode(10000);
    end else begin
     for i:=0 to SymbolManager.UnitList.Count-1 do begin
      s:=ChangeFileExt(PSymbol(SymbolManager.UnitList.Objects[i]).OriginalFileName,'.o');
      if tcc_add_file(TCCState,pansichar(s))<0 then begin
       Error.AddErrorCode(10000);
       break;
      end;
     end;
     if not Error.Errors then begin
      if tcc_output_file(TCCState,pansichar(ChangeFileExt(ExtractFileName(FileName),'.exe')))<0 then begin
       Error.AddErrorCode(10000);
      end;
     end;
    end;
   finally
    tcc_delete(TCCState);
   end;
  end;
 end else begin
  s:=ChangeFileExt(ExtractFileName(FileName),'');
  s:=s+'.o -o '+s+'.exe';
  for i:=0 to SymbolManager.UnitList.Count-1 do begin
   s:=SymbolManager.UnitList[i]+'.o '+s;
  end;

//  DebugLog(GetDosOutput(Options.TargetCompiler+' -c objpas2c.c'));
  s:=Options.TargetCompiler+' objpas2c.o '+s;
  DebugLog('Executing: '+s);
  DebugLog(GetDosOutput(s));
 end;
end;

end.
