(* test program *)
unit unt1;

interface

const
  hello = 'hello';

type
  pinteger = ^integer;

  rec = record
    a, b, c: integer;
    case d: integer of
    0 : (x: integer);
    1 : (y: integer)
  end;

var
  n: integer;
  
implementation

end.
