(******************************************************************************
 *  PASCAL compiler                                                           *
 *  inline compiler                                                           *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

program wpc;

{$APPTYPE CONSOLE}

uses
  SysUtils, compiler;

const
  msgtypestr: array[1..4] of string = ('fatal', 'error', 'warning', 'hint');

var
  errorcount: integer;

procedure compmsg(const filename, msg: string;
                  msgtype, line, index: integer;
                  var abort: boolean);
begin
  writeln(output, format('[%s] %s (%d,%d): %s', [msgtypestr[msgtype], filename,
          line, index, msg]));
  if (msgtype = 1) or (msgtype = 2) then inc(errorcount);
end;  (* compmsg *)

begin
  if paramcount = 1 then
  begin
    errorcount := 0;
    compile(paramstr(1), compmsg);
    if errorcount = 0 then writeln(output, 'successfull compilation');
  end else writeln(output, 'syntax : wpc srcfile');
  readln;
end.
