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

  psymbol = ^tsymbol;
  tsymbol = record
    link: psymbol;
    name: pchar;
  end;

  plevel = ^tlevel;
  tlevel = record
    link: plevel;
    emit: boolean;
  end;

var
  srcfile: pchar;
  eol: boolean;
  instring: boolean;
  doinclude: boolean;
  modules: pmodule;
  stack: psrcfile;
  symbols: psymbol;
  levels: plevel;
  name: packed array[0..127] of char;
  namelen: integer;

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

procedure getname;
begin
  if not (ch in []) then fatal('identifier expected');
  while ch in ['a'..'z', 'A'..'Z', '0'..'9', '_'] do
  begin
    name[namelen] := ch;
    inc(namelen);
    ch := getchar;
  end;
  name[namelen] := #0;
end;  (* getname *)

procedure directive;

  procedure skipblank;
  begin
    while ch in [#1..' '] do ch := getchar;
  end;

  procedure handleinclude;
  begin
    skipblank;

    (* obtain file name *)
    namelen := 0;
    while ch in ['a'..'z', 'A'..'Z', '0'..'9', '.', '-', '_', ':', '(', ')', '\', '/', '~', '@'] do
    begin
      name[namelen] := ch;
      inc(namelen);
      ch := getchar;
    end;
    name[namelen] := #0;
    doinclude := true;

    if namelen = 0 then fatal('file name expected');
  end;  (* handleinclude *)

  procedure handledefine;
  var
    p: psymbol;
  begin
    skipblank;
    getname;
    new(p);
    p^.link := symbols;
    p^.name := strnew(@name);
    symbols := p;
  end;  (* handledefine *)

  procedure handleundef;
  var
    p, last: psymbol;
  begin
    skipblank;
    getname;
    p := symbols;
    last := nil;
    while p <> nil do
    begin
      if strcomp(@name, p^.name) = 0 then
      begin
        if p = symbols then symbols := symbols^.link
                       else last^.link := p^.link;
        exit;
      end else begin
        last := p;
        p := p^.link;
      end;
    end;
    warning(format('symbol %s undefined', [name]));
  end;  (* handleundef *)

  function isdefined: boolean;
  var
    p: psymbol;
  begin
    p := symbols;
    while p <> nil do
    begin
      if strcomp(@name, p^.name) = 0 then
      begin
        isdefined := true;
        exit;
      end;
    end;
    isdefined := false;
  end;  (* isdefined *)

  procedure handleifdef;
  var
    p: plevel;
  begin
    new(p);
    p^.link := levels;
    levels := p;
    p^.emit := p^.link^.emit and isdefined;
  end;  (* handleifdef *)

  procedure handleifndef;
  var
    p: plevel;
  begin
    new(p);
    p^.link := levels;
    levels := p;
    p^.emit := p^.link^.emit and not isdefined;
  end;  (* handleifndef *)

  procedure handleelse;
  begin
    if levels^.link = nil then fatal('''end'' directive without condition');
    levels^.emit := levels^.link^.emit and not levels^.emit;
  end;  (* handleelse *)

  procedure handleend;
  begin
    if levels^.link = nil then fatal('''end'' directive without condition');
    levels := levels^.link;
  end;  (* handleend *)

  procedure handleapptype;
  begin
  end;  (* handleapptype *)

begin (* directive *)
  skipblank;
  getname;
  if strcomp(@name, 'include') = 0 then handleinclude else
  if strcomp(@name, 'define') = 0 then handledefine else
  if strcomp(@name, 'undef') = 0 then handleundef else
  if strcomp(@name, 'ifdef') = 0 then handleifdef else
  if strcomp(@name, 'ifndef') = 0 then handleifndef else
  if strcomp(@name, 'else') = 0 then handleelse else
  if strcomp(@name, 'end') = 0 then handleend else
  if strcomp(@name, 'apptype') = 0 then handleapptype else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
//  if strcomp(@name, '') = 0 then else
  error(format('unknown directive %s', [name]));
end;  (* directive *)

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

    '/' :
      if peekchar = '/' then
        repeat
          ch := getchar;
        until ch in [#0, #10];

    '(' :
      if peekchar = '*' then
      begin
        getchar;
        ch := getchar;

        if ch = '$' then
        begin
          getchar;
          directive
        end;

        repeat
          if ch = #0 then fatal('open comment');
          lastch := ch;
          ch := getchar
        until (lastch = '*') and (ch = ')');
        ch := ' ';
        if doinclude then includefile(name);
      end;

    '{' :
      begin
        ch := getchar;

        if ch = '$' then
        begin
          getchar;
          directive
        end;

        while ch <> '}' do
        begin
          if ch = #0 then fatal('open comment');
          ch := getchar;
        end;
        ch := ' ';
        if doinclude then includefile(name);
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
  symbols := nil;
  new(levels);
  levels^.link := nil;
  levels^.emit := true;
  chname := filename;
  srcfile := src;
  eol := false;
  chline := 1;
  chindex := 0;
  instring := false;
  doinclude := false;
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
