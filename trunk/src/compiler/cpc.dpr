program cpc;
{$i Compiler.inc}
{$ifdef windows}
 {$apptype console}
{$endif}
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

procedure Compile;
var ACompiler:TCompiler;
begin
 ACompiler:=TCompiler.Create;
 // read own configuration
 ACompiler.Options.Load;
 ACompiler.UnitManager.RebuildAll:=ACompiler.Options.BuildAll;

 if ACompiler.Options.ShowVersion then begin
  Writeln('');
 end else if ACompiler.Options.ShowHelp or (ACompiler.Options.TargetFilename='') then
 begin
  Writeln;
  Writeln('Syntax: ',ChangeFileExt(ExtractFileName(Paramstr(0)),''), ' [options] filename [options]');
  Writeln;
  Writeln('  -B = Build all units                     -U<paths> = Unit directories');
  Writeln;
  Writeln('  -I<path> = Include directories           -CS<executable> = target compiler');
  Writeln;
  Writeln('  -CP<params> = target compiler parameters -D<syms> = define conditional symbol');
  Writeln;
  Writeln('  --HELP = this help                       ');
  Writeln;
 end else
 begin
  WriteLn('Compiling ',UPPERCASE(ChangeFileExt(ExtractFileName(ACompiler.Options.TargetFilename),'')));
  ACompiler.CompileMainFile(ACompiler.Options.TargetFilename);
  Write(ACompiler.GetErrors);
 end;
 ACompiler.Destroy;
end;

begin
 WriteLn('CrossPascal 1.00 - Copyright (C) 2005-2013, BeRo & red');
 Compile;
end.
