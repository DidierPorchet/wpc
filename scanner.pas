(******************************************************************************
 *  PASCAL compiler                                                           *
 *  scanner                                                                   *
 *  provide token stream converting char stream                               *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit scanner;

interface

type
  ttoken = (tkeof, tkif, tkof, tkin, tkor, tkto, tkdo, tkvar, tkfor, tkand,
            tkend, tkdiv, tkmod, tknot, tkxor, tkshl, tkshr, tkset, tktype,
            tkcase, tkgoto, tkwith, tkthen, tkuses, tkunit, tkelse, tkexit, tkfile, tkbreak,
            tkconst, tklabel, tkarray, tkbegin, tkwhile, tkuntil, tkrecord,
            tkdownto, tkrepeat, tkpacked, tkobject, tkpublic, tkprivate,
            tkprogram, tkforward,
            tkfunction, tkcontinue, tkprocedure, tkident, tkintcst, tkrealcst,
            tkstar, tkslash, tkplus, tkminus, tkequal, tkcolon, tksemicolon,
            tkcomma, tkarrow, tkaroba, tklparent, tkrparent, tklbracket,
            tkrbracket, tkperiod, tknotequ, tkgreater, tklower, tkgreatequ,
            tklowequ, tkstrgcst, tkassign,
            tkinterface, tkimplementation, tkinitialization, tkfinalization);

const
  maxidlen = 64;
  maxstrglen = 256;

type
  alpha = packed array[0..maxidlen-1] of char;

var
  token: ttoken;
  id: alpha;
  idlen: integer;
  intcst: int64;
  realcst: extended;
  strgcst: packed array[0..maxstrglen-1] of char;
  strglen: integer;

procedure loadtoken;
procedure check(atoken: ttoken);
procedure expect(atoken: ttoken);
procedure initscanner;
procedure donescanner;

implementation

uses
  sysutils, cmessage, preproc;

const
  tokname: array[ttoken] of string =
    ('eof', 'if', 'of', 'in', 'or', 'to', 'do', 'var', 'for', 'and',
     'end', 'div', 'mod', 'not', 'xor', 'shl', 'shr', 'set', 'type', 'case',
     'goto', 'with', 'then', 'uses', 'unit', 'else', 'exit', 'file', 'break', 'const',
     'label', 'array', 'begin', 'while', 'until', 'record',
     'downto', 'repeat', 'packed', 'object', 'public', 'private', 'program',
     'forward',
     'function', 'continue', 'procedure', 'identifier', 'intcst', 'realcst',
     '*', '/', '+', '-', '=', ':', ';',
     ',', '^', '@', '(', ')', '[',
     ']', '.', '<>', '>', '<', '>=',
     '<=', 'strgcst', ':=',
     'interface', 'implementation', 'initialization', 'finalization');

  rwcnt = 50;

  rws: array[0..rwcnt-1] of pchar = (
    'if',
    'of',
    'in',
    'or',
    'to',
    'do',
    'var',
    'for',
    'and',
    'end',
    'div',
    'mod',
    'not',
    'xor',
    'shl',
    'shr',
    'set',
    'type',
    'case',
    'goto',
    'with',
    'then',
    'uses',
    'unit',
    'else',
    'exit',
    'file',
    'break',
    'const',
    'label',
    'array',
    'begin',
    'while',
    'until',
    'record',
    'downto',
    'repeat',
    'packed',
    'object',
    'public',
    'private',
    'program',
    'forward',
    'function',
    'continue',
    'procedure',
    'interface',
    'implementation',
    'initialization',
    'finalization'
  );

  rwt : array[0..rwcnt-1] of ttoken = (
    tkif,
    tkof,
    tkin,
    tkor,
    tkto,
    tkdo,
    tkvar,
    tkfor,
    tkand,
    tkend,
    tkdiv,
    tkmod,
    tknot,
    tkxor,
    tkshl,
    tkshr,
    tkset,
    tktype,
    tkcase,
    tkgoto,
    tkwith,
    tkthen,
    tkuses,
    tkunit,
    tkelse,
    tkexit,
    tkfile,
    tkbreak,
    tkconst,
    tklabel,
    tkarray,
    tkbegin,
    tkwhile,
    tkuntil,
    tkrecord,
    tkdownto,
    tkrepeat,
    tkpacked,
    tkobject,
    tkpublic,
    tkprivate,
    tkprogram,
    tkforward,
    tkfunction,
    tkcontinue,
    tkprocedure,
    tkinterface,
    tkimplementation,
    tkinitialization,
    tkfinalization
  );

  maxnumdigit = 64;

var
  numcst: packed array[0..maxnumdigit-1] of char;
  numlen: integer;

procedure readnumber;
begin
  case ch of
  '0'..'9' :
    begin
      repeat
        numcst[numlen] := ch;
        inc(numlen);
        loadchar;
      until not (ch in ['0'..'9'])
    end;
  '$' :
    begin
      loadchar;
      if ch in ['0'..'9', 'a'..'f', 'A'..'F'] then
      begin
        repeat
          numcst[numlen] := ch;
          inc(numlen);
          loadchar;
        until not (ch in [])
      end else begin
        error('hexadecimal number expected');
        numcst[numlen] := '0';
        inc(numlen);
      end
    end;
  '%' :
    begin
      loadchar;
      if ch in ['0'..'1'] then
      begin
        repeat
          numcst[numlen] := ch;
          inc(numlen);
          loadchar;
        until not (ch in [])
      end else begin
        error('binary number expected');
        numcst[numlen] := '0';
        inc(numlen);
      end
    end;
  '&' :
    begin
      loadchar;
      if ch in ['0'..'7'] then
      begin
        repeat
          numcst[numlen] := ch;
          inc(numlen);
          loadchar;
        until not (ch in [])
      end else begin
        error('octal number expected');
        numcst[numlen] := '0';
        inc(numlen);
      end
    end
  else
    error('number expected');
    numcst[numlen] := '0';
    inc(numlen);
  end;
end;

procedure convnumber;
var
  i, digit, base, fpart, epart, fdigit, fbase: integer;
  signed, negate: boolean;
  scale: extended;

  function convint: int64;
  begin
    result := 0;
    digit := 0;
    case numcst[i] of
    '0'..'9' :
      begin
        base := 10;
        while (i < numlen) and (numcst[i] in ['0'..'9']) do
        begin
          result := result * 10;
          inc(result, ord(numcst[i]) - ord('0'));
          inc(digit);
          inc(i)
        end
      end;

    '$' :
      begin
        base := 16;
        inc(i);
        while (i < numlen) and (numcst[i] in ['0'..'9', 'a'..'f', 'A'..'F']) do
        begin
          result := result shl 4;
          case numcst[i] of
          '0'..'9' : inc(result, ord(numcst[i]) - ord('0'));
          'a'..'f' : inc(result, ord(numcst[i]) - ord('a') + 10);
          'A'..'F' : inc(result, ord(numcst[i]) - ord('A') + 10)
          end;
          inc(digit);
          inc(i)
        end
      end;

    '%' :
      begin
        base := 2;
        inc(i);
        while (i < numlen) and ((numcst[i] = '0') or (numcst[i] = '1')) do
        begin
          result := result shl 1;
          if numcst[i] = '1' then inc(result);
          inc(digit);
          inc(i)
        end
      end;

    '&' :
      begin
        base := 8;
        inc(i);
        while (i < numlen) and (numcst[i] in ['0'..'7']) do
        begin
          result := result shl 3;
          inc(result, ord(numcst[i]) - ord('0'));
          inc(digit);
          inc(i)
        end
      end
    else
      error('number conversion error');
      base := 10;
      digit := 1;
    end;
  end;

begin
  i := 0;
  intcst := convint;
  if i < numlen then
  begin
    if numcst[i] = '.' then
    begin
      inc(i);
      fpart := convint;
      fdigit := digit;
      fbase := base;
    end else begin
      fpart := 0;
      fdigit := 1;
      fbase := 10;
    end;

    if (numcst[i] = 'e') or (numcst[i] = 'E') then
    begin
      inc(i);
      if (numcst[i] = '+') or (numcst[i] = '-') then
      begin
        signed := true;
        negate := numcst[i] = '-';
        inc(i)
      end else begin
        signed := false;
        negate := false;
      end;
      epart := convint;
    end else begin
      epart := 0;
      signed := false;
      negate := false;
    end;

    realcst := fpart;
    while fdigit <> 0 do
    begin
      realcst := realcst / fbase;
      dec(fdigit)
    end;
    realcst := intcst + realcst;

    scale := 1;
    if signed and negate then
    begin
      while epart <> 0 do
      begin
        scale := scale / 10;
        dec(epart)
      end
    end else begin
      while epart <> 0 do
      begin
        scale := scale * 10;
        dec(epart);
      end
    end
  end
end;

procedure loadtoken;
label 1, 2, 3;
var
  i: integer;
  done: boolean;
begin
1:
  while ch in [#1..' '] do loadchar;

  case ch of
  'a'..'z', 'A'..'Z', '_' :
    begin
      idlen := 0;
      repeat
        if idlen < maxidlen then id[idlen] := ch;
        inc(idlen);
        loadchar
      until not (ch in ['a'..'z', 'A'..'Z', '_', '0'..'9']);

      if idlen > maxidlen then
      begin
        idlen := maxidlen;
        warning(format('identifier ''%s'' truncated', [id]));
      end;
      id[idlen] := #0;

      for i := 0 to rwcnt-1 do
        if strcomp(@id, rws[i]) = 0 then
        begin
          token := rwt[i];
          exit
        end;

      token := tkident
    end;

  '0'..'9', '$' :
    begin
      numlen := 0;
      readnumber;
      token := tkintcst;

      if (ch = '.') or (ch = 'e') or (ch = 'E') then
      begin
        if ch = '.' then
        begin
          loadchar;
          if ch = '.' then
          begin
            ch := ':';
            goto 2
          end;
          if numlen < maxnumdigit then numcst[numlen] := '.';
          inc(numlen);
          readnumber
        end;
        token := tkrealcst;

        if (ch = 'e') or (ch = 'E') then
        begin
          if strglen < maxstrglen then numcst[numlen] := 'e';
          inc(numlen);
          loadchar;
          if (ch = '+') or (ch = '-') then
          begin
            if numlen < maxnumdigit then numcst[numlen] := ch;
            loadchar
          end;
          readnumber
        end;
      end;
    2:
      if numlen > maxnumdigit then
      begin
        error('numerical constant overflow');
        token := tkintcst;
        intcst := 0
      end else
        convnumber
    end;

  '''', '#' :
    begin
      strglen := 0;
      done := false;
      while not done do
      begin
        if ch = '#' then
        begin
          loadchar;
          numlen := 0;
          readnumber;
          convnumber;
          if (intcst < 0) or (intcst > 255) then
          begin
            error('numerical char constant out of range');
            if strglen < maxstrglen then strgcst[strglen] := ' '
          end else begin
            if strglen < maxstrglen then strgcst[strglen] := chr(intcst)
          end;
          inc(strglen)
        end else
          if ch = '''' then
          begin
            repeat
              loadchar;
              while ch <> '''' do
              begin
                if ch = #10 then
                begin
                  error('unterminated string');
                  goto 3
                end;
                if strglen < maxstrglen then strgcst[strglen] := ch;
                inc(strglen);
                loadchar
              end;
              loadchar;
              if ch = '''' then
              begin
                if strglen < maxstrglen then strgcst[strglen] := ch;
                inc(strglen);
              end;
            until ch <> ''''
          end else
            done := true;
      end;
    3:
      if strglen > maxstrglen then
      begin
        error('string overflow');
        strglen := maxstrglen;
      end;
      token := tkstrgcst
    end;

  ':' :
    begin
      loadchar;
      if ch = '=' then
      begin
        loadchar;
        token := tkassign;
      end else token := tkcolon;
    end;

  '.' :
    begin
      loadchar;
      if ch = '.' then
      begin
        loadchar;
        token := tkcolon
      end else token := tkperiod
    end;

  '>' :
    begin
      loadchar;
      if ch = '=' then
      begin
        loadchar;
        token := tkgreatequ
      end else token := tkgreater
    end;

  '<' :
    begin
      loadchar;
      if ch = '=' then
      begin
        loadchar;
        token := tklowequ
      end else
        if ch = '>' then
        begin
          loadchar;
          token := tknotequ
        end else token := tklower
    end;

  #0 :
    token := tkeof

  else
    case ch of
    '*' : token := tkstar;
    '/' : token := tkslash;
    '+' : token := tkplus;
    '-' : token := tkminus;
    '=' : token := tkequal;
    ';' : token := tksemicolon;
    ',' : token := tkcomma;
    '^' : token := tkarrow;
    '@' : token := tkaroba;
    '(' : token := tklparent;
    ')' : token := tkrparent;
    '[' : token := tklbracket;
    ']' : token := tkrbracket
    else
      error('illegal char');
      loadchar;
      goto 1
    end;
    loadchar
  end;
end;  // loadtoken

procedure check(atoken: ttoken);
begin
  if token <> atoken then
    fatal(format('''%s'' expected, but ''%s'' found', [tokname[atoken], tokname[token]]));
end;

procedure expect(atoken: ttoken);
begin
  check(atoken);
  loadtoken;
end;

procedure initscanner;
begin
  loadtoken
end;

procedure donescanner;
begin
end;

end.
