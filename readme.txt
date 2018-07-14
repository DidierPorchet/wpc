WPC
Pascal Compiler for Windows
Didier Porchet 2018

in development ...

IMPORTANT NOTE : CASE SENSITIVE

developed under Borland Delphi 6.0

Files:
  wpc.dpr
  compiler.pas
  cmessage.pas
  preproc.pas
  scanner.pas
  parser.pas
  codegen.pas
  outfasm.pas
  readme.txt

actually, the compiler recognize only a subset of pascal language and need
a lot of work to be really useable.

supported :
  source program compilation outputted as asm file (FASM format)
  modular development allowed by use of unit like Turbo Pascal or Delphi
  fundamentals types (integer, char, boolean, pointer)
  subrange, enumeration, array and records (with variants parts support)
  nested procedure up to 31 levels
  expressions
  statements (excepted for loop, case test and with selector)
  minimal RTL (write/writeln on std output, getmem, freemem, inc, dec)

unsupported :
  strings, real, set, files, procedural and object types
  external procedure
  initialization and finalization blocks in units
  preprocessor directives
  PE output

history :
  04/05/2018  skeleton
  12/07/2018  upload on GitHub

