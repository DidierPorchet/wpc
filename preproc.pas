(******************************************************************************
 *  PASCAL compiler                                                           *
 *  preprocessor                                                              *
 *  provide source char stream                                                *
 *  unit loader                                                               *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit preproc;

interface

var
  ch: char;                                      (* current char *)
  chline, chindex: integer;                      (* current position *)
  chname: string;                                (* current file name *)

procedure loadchar;
function loadunit(const unitname: string): boolean;
procedure initpreproc(const filename: string; src: pchar);
procedure donepreproc;

implementation

uses
  sysutils, cmessage;

type
  pmodule = ^tmodule;
  tmodule = record
    link: pmodule;
    name: string;
    filename: string;
  end;

  psrcfile = ^tsrcfile;
  tsrcfile = record
    link: psrcfile;
    name: string;

    (* used to save/restore preprocessor state when file change occur *)
    ch: char;
    chline, chindex: integer;
    srcfile: pchar;
    eol: boolean;
    instring: boolean;
  end;

var
  srcfile: pchar;
  eol: boolean;
  instring: boolean;
  modules: pmodule;
  stack: psrcfile;

function peekchar: char;
begin
  result := srcfile^;
end;  (* peekchar *)

function getchar: char;
begin
  if eol then
  begin
    chindex := 1;
    inc(chline);
  end else inc(chindex);

  result := peekchar;
  if result <> #0 then
  begin
    inc(srcfile);
    eol := ch = #10
  end else eol := false
end;  (* getchar *)

procedure savestate;
begin
  stack^.ch := ch;
  stack^.chline := chline;
  stack^.chindex := chindex;
  stack^.srcfile := srcfile;
  stack^.eol := eol;
  stack^.instring := instring;
end;  (* savestate *)

procedure restorestate;
begin
  ch := stack^.ch;
  chline := stack^.chline;
  chindex := stack^.chindex;
  chname := stack^.name;
  srcfile := stack^.srcfile;
  eol := stack^.eol;
  instring := stack^.instring;
end;  (* restorestate *)

procedure includefile(const filename: string);
var
  f: file of byte;
  size: integer;
  buf: pointer;
  p: psrcfile;
begin
  assign(f, filename);
  reset(f);
  size := filesize(f);
  getmem(buf, size+1);
  blockread(f, buf^, size);
  pchar(buf)[size] := #0;
  close(f);

  new(p);
  p^.link := stack;
  p^.name := filename;
  p^.ch := ' ';
  p^.chline := 1;
  p^.chindex := 0;
  p^.srcfile := buf;
  p^.eol := false;
  p^.instring := false;

  savestate;
  stack := p;
  restorestate;
end;  (* includefile *)

procedure loadchar;
var
  lastch: char;
begin
  ch := getchar;
  if instring then instring := ch = ''''
  else
    case ch of
    '''' :
      instring := true;

    '(' :
      if peekchar = '*' then
      begin
        getchar;
        ch := getchar;
        repeat
          if ch = #0 then fatal('open comment');
          lastch := ch;
          ch := getchar
        until (lastch = '*') and (ch = ')');
        ch := ' '
      end;

    #0 :
      begin
        if stack^.link <> nil then
        begin
          stack := stack^.link;
          restorestate;
        end;
      end;
    end
end;  (* loadchar *)

function loadunit(const unitname: string): boolean;
var
  p: pmodule;
begin
  (* does nothing if unit already loaded *)
  p := modules;
  while p <> nil do
    if sametext(p^.name, unitname) then
    begin
      result := false;
      exit
    end else p := p^.link;

  new(p);
  p^.link := modules;
  modules := p;

  (* locate unit *)
  p^.name := unitname;
  p^.filename := unitname + '.pas';
  if not fileexists(p^.filename) then
    fatal(format('cannot locate used unit %s', [unitname]));

  (* load *)
  includefile(p^.filename);

  result := true;
end;  (* loadunit *)

procedure initpreproc(const filename: string; src: pchar);
begin
  modules := nil;
  chname := filename;
  srcfile := src;
  eol := false;
  chline := 1;
  chindex := 0;
  instring := false;
  new(stack);
  stack^.link := nil;
  stack^.name := filename;
  savestate;
  loadchar;
end;  (* initpreproc *)

procedure donepreproc;
begin
end;  (* donepreproc *)

end.
