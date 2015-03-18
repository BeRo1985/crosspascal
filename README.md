# CrossPascal

## Abstract

`CrossPascal` aims to be a Delphi 7 compatible cross-platform [http://en.wikipedia.org/wiki/Source-to-source_compiler source-to-source compiler] (together with the new unicode string types from XE3 but where ansistring is still the default string type for to be still Delphi 7 compatible) which generates intermediate C code. Since C compilers are available on nearly every platform, this introduces a large variety of targets (native 32 bit targets for as minimum) for crosspascal to work with. 

`CrossPascal` is not a simple converter. It features a full parser building an abstract syntax tree, from which C code is generated. All unique language features compared to C (such as nested functions, Delphi RTTI, and much more) are translated accordingly and reimplemented in C code.

## Current Status 
_Last update: Juny 11th, 2013_

Here is a little overview of the current status, our main focus is the actual code generation right now. The parser itself was originally written by Benjamin Rosseaux in 2005-2006 and was used as a starting point for this project.

| *Task* | *Done* |
| :----: | :----: |
| Parser | 95% |
| General Code Generation | 65% |
| Nested Functions | 100% |
| Unit Handling | 100% |
| Strings | 80% |
| Objects (Turbo Pascal oldstyle) | 100% |
| Classes | 90% |
| Interfaces | 10% |
| Variants | 0% |
| Dynamic arrays | 75% |
| RTTI | 95% |
| Exception Handling | 80% |
| Codepage Handling | 0% |
| RTL | 0% |
