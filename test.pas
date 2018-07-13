(* test program *)
program test;

uses
  unt1;

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
  p, p2: pinteger;
  r: rec;
  pn: pointer;
  s1 : set of char;

begin
  p := nil;
  p := @n;
  p^ := 1000;
  inc(n, -1);
  if n <= 1000 then writeln(n);

  unt1.n := 455;
  writeln(unt1.n);

  r.a := 100;
  r.b := 200;
  r.c := 300;
  r.x := 400;
  r.y := 500;

  writeln(r.a);
  writeln(r.b);
  writeln(r.c);
  writeln(r.x);
  writeln(r.y);

  writeln(hello);
end.
