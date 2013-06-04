PROGRAM objpas2c;  
{$APPTYPE CONSOLE}
uses
  BeRoStream in 'BeRoStream.pas',
  BeRoStringTree in 'BeRoStringTree.pas',
  DebugLogUtils in 'DebugLogUtils.pas',
  Tree in 'Tree.pas',
  Symbols in 'Symbols.pas',
  Code in 'Code.pas',
  Globals in 'Globals.pas',
  Compiler in 'Compiler.pas',
  Scanner in 'Scanner.pas',
  Parser in 'Parser.pas',
  Error in 'Error.pas',
  OptimizerHighLevel in 'OptimizerHighLevel.pas',
  Preprocessor in 'Preprocessor.pas',
  BeRoGUIDTools in 'BeRoGUIDTools.pas',
  PointerList in 'PointerList.pas',
  IntegerList in 'IntegerList.pas',
  List in 'List.pas',
  StringList in 'StringList.pas',
  UnitManager in 'UnitManager.pas',
  CRC32 in 'CRC32.pas',
  SortedStringList in 'SortedStringList.pas',
  FastStringList in 'FastStringList.pas',
  BeRoUtils in 'BeRoUtils.pas',
  TypeCheck in 'TypeCheck.pas',
  CodeGenC in 'CodeGenC.pas',
  ShellHelper in 'ShellHelper.pas',
  BeRoDoubleToString in 'BeRoDoubleToString.pas',
  BeRoStringToDouble in 'BeRoStringToDouble.pas',
  libtcc in 'libtcc.pas',
  BeRoStringHashMap in 'BeRoStringHashMap.pas',
  HugeString in 'HugeString.pas',
  UnicodeUtils in 'UnicodeUtils.pas',
  Unicode in 'Unicode.pas',
  CodePages in 'CodePages.pas';

procedure Compile(FileName:ansistring);
var ACompiler:TCompiler;
begin
 ACompiler:=TCompiler.Create;
 ACompiler.Options.TargetCompiler:='e:\tcc\tcc.exe'; //'e:\mingw\bin\gcc.exe'; //
 ACompiler.UnitManager.RebuildAll:=UPPERCASE(PARAMSTR(2))='-B';
 WriteLn('Compiling ',UPPERCASE(ChangeFileExt(ExtractFileName(FileName),'')));
 ACompiler.CompileMainFile(FileName);
 Write(ACompiler.GetErrors);
 ACompiler.Destroy;
end;

begin
 WriteLn('ObjPas2C 1.00 - Copyright (C) 2005-2013, BeRo & red');
 if ParamCount>0 then begin
  Compile(ParamStr(1));
 end;
end.
