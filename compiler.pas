(******************************************************************************
 *  PASCAL compiler                                                           *
 *  main interface                                                            *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit compiler;

interface

type
  tmsgproc = procedure (const filename, msg: string;
                        msgtype, line, index: integer; var abort: boolean);

procedure compilemem(const filename: string; src: pchar; msgproc: tmsgproc);
procedure compile(const filename: string; msgproc: tmsgproc);

implementation

uses
  sysutils, cmessage, preproc, scanner, parser, codegen, outfasm;

type
  ecompilerexception = class(exception);

var
  _msgproc: tmsgproc;
  errorcnt: integer;

(* emit messages to user application *)
procedure sendmsg(const filename, msg: string; msgtype, line, index: integer);
var
  doabort: boolean;
begin
  if (msgtype = 1) or (msgtype = 2) then inc(errorcnt);
  doabort := false;
  if assigned(_msgproc) then
    _msgproc(filename, msg, msgtype, line, index, doabort);
  if doabort or (msgtype = 1) then
    raise ecompilerexception.create('compilation aborted');
end;  (* sendmsg *)

(* compiler initialization *)
procedure init(const filename: string; src: pchar);
begin
  initoutfasm(changefileext(filename, '.asm'));
  initpreproc(filename, src);
  initscanner;
  initparser;
  initcodegen;
end;  (* init *)

(* compiler finalization *)
procedure done;
begin
  donecodegen;
  doneparser;
  donescanner;
  donepreproc;
  doneoutfasm;
end;  (* done *)

(* compile pascal file in memory *)
procedure compilemem(const filename: string; src: pchar; msgproc: tmsgproc);
begin
  sendmessage := sendmsg;
  errorcnt := 0;
  _msgproc := msgproc;
  try
    init(filename, src);
    try
      parse;               (* parse whole source *)
//      link;                (* link program *)
    except
      on ecompilerexception do ;
    end;
  finally
    done;
  end;
end;  (* compilemem *)

(* compile pascal file *)
procedure compile(const filename: string; msgproc: tmsgproc);
var
  doabort: boolean;
  fsize: integer;
  f: file of byte;
  p: pointer;
begin
  if fileexists(filename) then
  begin
    p := nil;
    assign(f, filename);
    reset(f);
    try
      fsize := filesize(f);
      getmem(p, fsize + 1);
      blockread(f, p^, fsize);
      pchar(p)[fsize] := #0;
      compilemem(filename, p, msgproc);
    finally
      if p <> nil then freemem(p);
      close(f);
    end;
  end else begin
    errorcnt := 1;
    if assigned(msgproc) then
      msgproc('', filename + ' not found', 1, -1, -1, doabort);
  end;
end;  (* compile *)

end.
