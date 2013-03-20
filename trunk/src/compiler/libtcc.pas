unit libtcc;
{$i Compiler.inc}

interface

uses SysUtils,{$ifdef Windows}Windows{$ifdef fpc},dynlibs{$endif}{$else}dynlibs{$endif};

const libtccReady:boolean={$ifdef staticlinked}true{$else}false{$endif};

      libtccLib={$ifdef windows}'libtcc.dll'{$else}'libtcc.so'{$endif};

      TCC_OUTPUT_MEMORY=0;     // output will be run in memory (default)
      TCC_OUTPUT_EXE=1;        // executable file
      TCC_OUTPUT_DLL=2;        // dynamic library
      TCC_OUTPUT_OBJ=3;        // object file
      TCC_OUTPUT_PREPROCESS=4; // only preprocess (used internally)

      TCC_RELOCATE_AUTO=pointer(1);

type PTCCState=pointer;

     TlibtccErrorFunc=procedure(opaque:pointer;msg:pansichar); cdecl;

{$ifdef staticlinked}
// create a new TCC compilation context
function tcc_new:PTCCState; cdecl; external libtccLib name 'tcc_new';

// free a TCC compilation context
procedure tcc_delete(s:PTCCState); cdecl; external libtccLib name 'tcc_delete';

// set CONFIG_TCCDIR at runtime
procedure tcc_set_lib_path(s:PTCCState;path:pansichar); cdecl; external libtccLib name 'tcc_set_lib_path';

// set error/warning display callback
procedure tcc_set_error_func(s:PTCCState;error_opaque:pointer;error_func:TlibtccErrorFunc); cdecl; external libtccLib name 'tcc_set_error_func';

// set options as from command line (multiple supported)
procedure tcc_set_options(s:PTCCState;str:pansichar); cdecl; external libtccLib name 'tcc_set_options';

// add include path
function tcc_add_include_path(s:PTCCState;pathname:pansichar):longint; cdecl; external libtccLib name 'tcc_add_include_path';

// add system include path
function tcc_add_sysinclude_path(s:PTCCState;pathname:pansichar):longint; cdecl; external libtccLib name 'tcc_add_sysinclude_path';

// define preprocessor symbol 'sym'. Can put optional value
procedure tcc_define_symbol(s:PTCCState;sym,value:pansichar); cdecl; external libtccLib name 'tcc_define_symbol';

// undefine preprocess symbol 'sym'
procedure tcc_undefine_symbol(s:PTCCState;sym:pansichar); cdecl; external libtccLib name 'tcc_undefine_symbol';

// add a file (C file, dll, object, library, ld script). Return -1 if error.
function tcc_add_file(s:PTCCState;filename:pansichar):longint; cdecl; external libtccLib name 'tcc_add_file';

// compile a string containing a C source. Return -1 if error
function tcc_compile_string(s:PTCCState;buf:pansichar):longint; cdecl; external libtccLib name 'tcc_compile_string';

// set output type. MUST BE CALLED before any compilation
function tcc_set_output_type(s:PTCCState;output_type:longint):longint; cdecl; external libtccLib name 'tcc_set_output_type';

// equivalent to -Lpath option
function tcc_add_library_path(s:PTCCState;pathname:pansichar):longint; cdecl; external libtccLib name 'tcc_add_library_path';

// the library name is the same as the argument of the '-l' option
function tcc_add_library(s:PTCCState;libraryname:pansichar):longint; cdecl; external libtccLib name 'tcc_add_library';

// add a symbol to the compiled program
function tcc_add_symbol(s:PTCCState;name:pansichar;val:pointer):longint; cdecl; external libtccLib name 'tcc_add_symbol';

// output an executable, library or object file. DO NOT call tcc_relocate() before.
function tcc_output_file(s:PTCCState;filename:pansichar):longint; cdecl; external libtccLib name 'tcc_output_file';

// link and run main() function and return its value. DO NOT calltcc_relocate() before.
function tcc_run(s:PTCCState;argc:longint;argv:ppansichar):longint; cdecl; external libtccLib name 'tcc_run';

// do all relocations (needed before using tcc_get_symbol())
function tcc_relocate(s:PTCCState;ptr:pointer):longint; cdecl; external libtccLib name 'tcc_relocate';
(* possible values for 'ptr':
   - TCC_RELOCATE_AUTO : Allocate and manage memory internally
   - NULL              : return required memory size for the step below
   - memory address    : copy code to memory passed by the caller
   returns -1 if error. *)

// return symbol value or NULL if not found *
function tcc_get_symbol(s:PTCCState;name:pansichar):pansichar; cdecl; external libtccLib name 'tcc_get_symbol';
{$else}

type Ttcc_new=function():PTCCState; cdecl;
     Ttcc_delete=procedure(s:PTCCState); cdecl;
     Ttcc_set_lib_path=procedure(s:PTCCState;path:pansichar); cdecl;
     Ttcc_set_error_func=procedure(s:PTCCState;error_opaque:pointer;error_func:TlibtccErrorFunc); cdecl;
     Ttcc_set_options=procedure(s:PTCCState;str:pansichar); cdecl;
     Ttcc_add_include_path=function(s:PTCCState;pathname:pansichar):longint; cdecl;
     Ttcc_add_sysinclude_path=function(s:PTCCState;pathname:pansichar):longint; cdecl;
     Ttcc_define_symbol=procedure(s:PTCCState;sym,value:pansichar); cdecl;
     Ttcc_undefine_symbol=procedure(s:PTCCState;sym:pansichar); cdecl;
     Ttcc_add_file=function(s:PTCCState;filename:pansichar):longint; cdecl;
     Ttcc_compile_string=function(s:PTCCState;buf:pansichar):longint; cdecl;
     Ttcc_set_output_type=function(s:PTCCState;output_type:longint):longint; cdecl;
     Ttcc_add_library_path=function(s:PTCCState;pathname:pansichar):longint; cdecl;
     Ttcc_add_library=function(s:PTCCState;libraryname:pansichar):longint; cdecl;
     Ttcc_add_symbol=function(s:PTCCState;name:pansichar;val:pointer):longint; cdecl;
     Ttcc_output_file=function(s:PTCCState;filename:pansichar):longint; cdecl;
     Ttcc_run=function(s:PTCCState;argc:longint;argv:ppansichar):longint; cdecl;
     Ttcc_relocate=function(s:PTCCState;ptr:pointer):longint; cdecl;
     Ttcc_get_symbol=function(s:PTCCState;name:pansichar):pansichar; cdecl;

var tcc_new:Ttcc_new=nil;
    tcc_delete:Ttcc_delete=nil;
    tcc_set_lib_path:Ttcc_set_lib_path=nil;
    tcc_set_error_func:Ttcc_set_error_func=nil;
    tcc_set_options:Ttcc_set_options=nil;
    tcc_add_include_path:Ttcc_add_include_path=nil;
    tcc_add_sysinclude_path:Ttcc_add_sysinclude_path=nil;
    tcc_define_symbol:Ttcc_define_symbol=nil;
    tcc_undefine_symbol:Ttcc_undefine_symbol=nil;
    tcc_add_file:Ttcc_add_file=nil;
    tcc_compile_string:Ttcc_compile_string=nil;
    tcc_set_output_type:Ttcc_set_output_type=nil;
    tcc_add_library_path:Ttcc_add_library_path=nil;
    tcc_add_library:Ttcc_add_library=nil;
    tcc_add_symbol:Ttcc_add_symbol=nil;
    tcc_output_file:Ttcc_output_file=nil;
    tcc_run:Ttcc_run=nil;
    tcc_relocate:Ttcc_relocate=nil;
    tcc_get_symbol:Ttcc_get_symbol=nil;
{$endif}

implementation

{$ifndef staticlinked}
var LibTCCHandle:{$ifdef fpc}TLibHandle{$else}THandle{$endif}=0;

procedure LoadTCCLibrary;
begin
 libtccReady:=false;
 LibTCCHandle:=LoadLibrary(libtccLib);
 if LibTCCHandle<>0 then begin
  tcc_new:=GetProcAddress(LibTCCHandle,'tcc_new');
  tcc_delete:=GetProcAddress(LibTCCHandle,'tcc_delete');
  tcc_set_lib_path:=GetProcAddress(LibTCCHandle,'tcc_set_lib_path');
  tcc_set_error_func:=GetProcAddress(LibTCCHandle,'tcc_set_error_func');
  tcc_set_options:=GetProcAddress(LibTCCHandle,'tcc_set_options');
  tcc_add_include_path:=GetProcAddress(LibTCCHandle,'tcc_add_include_path');
  tcc_add_sysinclude_path:=GetProcAddress(LibTCCHandle,'tcc_add_sysinclude_path');
  tcc_define_symbol:=GetProcAddress(LibTCCHandle,'tcc_define_symbol');
  tcc_undefine_symbol:=GetProcAddress(LibTCCHandle,'tcc_undefine_symbol');
  tcc_add_file:=GetProcAddress(LibTCCHandle,'tcc_add_file');
  tcc_compile_string:=GetProcAddress(LibTCCHandle,'tcc_compile_string');
  tcc_set_output_type:=GetProcAddress(LibTCCHandle,'tcc_set_output_type');
  tcc_add_library_path:=GetProcAddress(LibTCCHandle,'tcc_add_library_path');
  tcc_add_library:=GetProcAddress(LibTCCHandle,'tcc_add_library');
  tcc_add_symbol:=GetProcAddress(LibTCCHandle,'tcc_add_symbol');
  tcc_output_file:=GetProcAddress(LibTCCHandle,'tcc_output_file');
  tcc_run:=GetProcAddress(LibTCCHandle,'tcc_run');
  tcc_relocate:=GetProcAddress(LibTCCHandle,'tcc_relocate');
  tcc_get_symbol:=GetProcAddress(LibTCCHandle,'tcc_get_symbol');
  if assigned(tcc_new) and
     assigned(tcc_delete) and
     assigned(tcc_set_lib_path) and
     assigned(tcc_set_error_func) and
     assigned(tcc_set_options) and
     assigned(tcc_add_include_path) and
     assigned(tcc_add_sysinclude_path) and
     assigned(tcc_define_symbol) and
     assigned(tcc_undefine_symbol) and
     assigned(tcc_add_file) and
     assigned(tcc_compile_string) and
     assigned(tcc_set_output_type) and
     assigned(tcc_add_library_path) and
     assigned(tcc_add_library) and
     assigned(tcc_add_symbol) and
     assigned(tcc_output_file) and
     assigned(tcc_run) and
     assigned(tcc_relocate) and
     assigned(tcc_get_symbol) then begin
   libtccReady:=true;
  end;
 end;
end;

procedure UnloadTCCLibrary;
begin
 if LibTCCHandle<>0 then begin
  FreeLibrary(LibTCCHandle);
 end;
end;
{$endif}

initialization
{$ifndef staticlinked}
 LoadTCCLibrary;
{$endif}
finalization
{$ifndef staticlinked}
 UnloadTCCLibrary;
{$endif}
end.
