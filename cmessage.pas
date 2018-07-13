(******************************************************************************
 *  PASCAL compiler                                                           *
 *  message sender                                                            *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit cmessage;

interface

var
  sendmessage: procedure (const filename, msg: string;
                          msgtype, line, index: integer);

procedure fatal(const msg: string);
procedure error(const msg: string);
procedure warning(const msg: string);
procedure hint(const msg: string);

implementation

uses
  preproc;

procedure fatal(const msg: string);
begin
  sendmessage(chname, msg, 1, chline, chindex);
end;  (* fatal *)

procedure error(const msg: string);
begin
  sendmessage(chname, msg, 2, chline, chindex);
end;  (* error *)

procedure warning(const msg: string);
begin
  sendmessage(chname, msg, 3, chline, chindex);
end;  (* warning *)

procedure hint(const msg: string);
begin
  sendmessage(chname, msg, 4, chline, chindex);
end;  (* hint *)

end.
