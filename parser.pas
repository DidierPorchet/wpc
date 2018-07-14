(******************************************************************************
 *  PASCAL compiler                                                           *
 *  parser                                                                    *
 *  interpret token stream converting it in code blocks and segments          *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit parser;

interface

procedure parse;
procedure initparser;
procedure doneparser;

implementation

uses
  sysutils, cmessage, preproc, scanner, codegen, outfasm;

const
  decltokens   = [tklabel, tkconst, tktype, tkvar, tkfunction, tkprocedure];
  stattokens   = [tkident, tkintcst, tkbegin, tkif, tkcase, tkfor, tkwhile,
                  tkrepeat, tkwith, tkgoto, tkexit, tkbreak, tkcontinue];
  selecttokens = [tkarrow, tklbracket, tkperiod];
  muloperators = [tkstar, tkslash, tkdiv, tkmod, tkshl, tkshr, tkand];
  addoperators = [tkplus, tkminus, tkor, tkxor];
  reloperators = [tkequal, tknotequ, tkgreater, tklower, tkgreatequ, tklowequ,
                  tkin];
  asgoperators = [tkassign];
  ordtyptokens = [tklparent, tkident, tkplus, tkminus, tkintcst, tkstrgcst];

  sysordindex          = 1;
  syschrindex          = 2;
  syspredindex         = 3;
  syssuccindex         = 4;
  syslowindex          = 5;
  syshighindex         = 6;
  syssizeofindex       = 7;
  sysincindex          = 8;
  sysdecindex          = 9;
  syswriteindex        = 10;
  syswritelnindex      = 11;
  sysreadindex         = 12;
  syseofindex          = 13;
  sysgetmemindex       = 14;
  sysfreememindex      = 15;

  syslblwrl            = 1;
  syslblwrc            = 2;
  syslblwri            = 3;
  syslblwrd            = 4;
  syslblrdc            = 5;
  syslbleof            = 6;
  syslblwra            = 7;
  syslblgtm            = 8;
  syslblfrm            = 9;

  maxtop               = 50;
  maxlevel             = 31;

type
  plabel = ^tlabel;
  pstruc = ^tstruc;
  pident = ^tident;

  tconstvalue = record
    case integer of
    0 : (ival: integer);
    1 : (rval: single);
    2 : (bval: boolean);
    3 : (sval: pchar);
  end;

  tlabel = record
    link: plabel;
    name: integer;
    ival: integer;
    defined: boolean;
  end;

  tcallconv = (ccpascal, ccstdcall, cccdecl, ccvarargs);

  tstruckind = (skordinal, sksubrange, skpointer, skreal, skstring, skset,
                skarray, skrecord, skfile, skfunc, skobject);

  tstruc = record
    size: integer;
    case kind: tstruckind of
    skordinal  : (declared: boolean; lconst: pident);
    sksubrange : (subtyp: pstruc; rmin, rmax: integer);
    skpointer  : (basetyp: pstruc);
    skreal     : ();
    skstring   : ();
    skset      : (settyp: pstruc);
    skarray    : (indextyp, elemtyp: pstruc);
    skrecord   : (fields: pident);
    skfile     : (filtyp: pstruc);
    skfunc     : (rettyp: pstruc; params: pident; callconv: tcallconv);
    skobject   : (pubfields, prvfields: pident);
  end;

  tidentkind = (ikconst, iktype, ikvar, ikfield, ikfunc, ikunit);

  tfunckind = (fksystem, fkmain, fkdeclared, fkexternal);

  tvaraccess = (vadirect, vaindirect, vaindexed);

  tident = record
    link: pident;
    next: pident;
    name: pchar;
    typ: pstruc;
    case kind: tidentkind of
    ikconst : (cval: tconstvalue);
    iktype  : ();
    ikvar   : (vaccess: tvaraccess; vaddr, vlev: integer);
    ikfunc  : (fkind: tfunckind; faddr: integer; fparams: pident;
               flocals: integer; fconv: tcallconv; forw: boolean);
    ikunit  : ()
  end;

  tattrkind = (akconst, akvar, akexpr);

  (* describe one data access path *)
  tattr = record
    typ: pstruc;                               (* value type *)
    case kind: tattrkind of
    akconst : (cval: tconstvalue);             (* constant (immediate value) *)
    akvar   : (vaccess: tvaraccess;            (* variable (where value is) *)
               vaddr, vlev: integer);
    akexpr  : ()                               (* value (already loaded) *)
  end;

  (* initialized data *)
  pidata = ^tidata;
  tidata = record
    link: pidata;
    offset: integer;                      (* offset relative in const section *)
    typ: pstruc;
    val: tconstvalue;
  end;

var
  nextlabel: integer;
  notifysearcherr: boolean;
  level: 0..maxlevel;
  top: 0..maxtop;
  display: array[0..maxtop] of record
    labels: plabel;
    idents: pident;
    kind: (dkblock, dkdecl, dkrecord)
  end;
  disx: integer;
  dataal: integer;
  gattr: tattr;
  intptr: pstruc;
  boolptr: pstruc;
  charptr: pstruc;
//  realptr: pstruc;
  nilptr: pstruc;
  frwtyp: pident;
  cstexpr: boolean;
  constants: pidata;
  cstsize: integer;
  frwall: boolean;                 (* used in unit interface *)

procedure genlabel(var lbl: integer);
begin
  inc(nextlabel);
  lbl := nextlabel;
end;  (* genlabel *)

(* search id in list *)
procedure searchlist(list: pident; var ident: pident);
var
  p: pident;
begin
  p := list;
  while p <> nil do
  begin
    if strcomp(p^.name, @id) = 0 then
    begin
      ident := p;
      exit;
    end else p := p^.link
  end;
  fatal(format('identifier %s undeclared', [id]));
  ident := nil;
end;  (* searchlist *)

(* search id at all scopes *)
procedure searchid(var ident: pident);
var
  p: pident;
  i: integer;
begin
  for i := top downto 0 do
  begin
    p := display[i].idents;
    while p <> nil do
    begin
      if strcomp(p^.name, @id) = 0 then
      begin
        ident := p;
        disx := i;
        exit
      end;
      p := p^.link
    end;
  end;
  if notifysearcherr then fatal(format('identifier %s undeclared', [id]));
  ident := nil;
end;  (* searchid *)

(* enter ident at current scope *)
procedure enterid(ident: pident);
var
  p: pident;
begin
  p := display[top].idents;
  while p <> nil do
  begin
    if strcomp(p^.name, ident^.name) = 0 then
    begin
      fatal(format('identifier ''%s'' redeclared', [ident^.name]));
      exit
    end;
    p := p^.link
  end;
  ident^.link := display[top].idents;
  display[top].idents := ident
end;  (* enterid *)

(* search a user defined label in current block scope *)
procedure searchlabel(var lbl: plabel);
var
  p: plabel;
  i: integer;
begin
  i := top;
  while display[i].kind <> dkblock do dec(i);
  p := display[i].labels;
  while p <> nil do
  begin
    if p^.name = intcst then
    begin
      lbl := p;
      exit
    end;
    p := p^.link;
  end;
  fatal(format('label ''%d'' undeclared', [integer(intcst)]));
end;  (* searchlabel *)

(* enter user defined label at current scope *)
procedure enterlabel(lbl: plabel);
var
  p: plabel;
begin
  p := display[top].labels;
  while p <> nil do
  begin
    if p^.name = lbl^.name then
    begin
      fatal(format('label ''%d'' redeclared', [lbl^.name]));
      exit
    end;
    p := p^.link;
  end;
  lbl^.link := display[top].labels;
  display[top].labels := lbl
end;  (* enterlabel *)

procedure parse;
var
  mainfunc, func: pident;
  p: pidata;

  done: boolean;
  ident: pident;
  lbl: plabel;
  exitlbl, breaklbl, nextlbl: integer;

  procedure block(funcid: pident); forward;


    procedure align(var offset: integer; typ: pstruc);
    begin
      inc(offset, typ^.size);
      while (offset mod dataal) <> 0 do inc(offset)
    end;  (* align *)

    procedure allocvar(var offset: integer; typ: pstruc);
    begin
      if level = 0 then
      begin               (* global *)
        offset := func^.flocals;
        align(func^.flocals, typ)
      end else begin      (* local *)
        align(func^.flocals, typ);
        offset := -(func^.flocals + (level * 4));
      end
    end;  (* allocvar *)

    function allocstrgcst(typ: pstruc; val: tconstvalue): integer;
    var
      p: pidata;
    begin
      new(p);
      p^.link := constants;
      constants := p;

      p^.typ := typ;
      p^.val := val;
      p^.offset := cstsize;
      inc(cstsize, typ^.size);

      allocstrgcst := p^.offset;
    end;  (* allocstrgcst *)

    (* check types compatibility *)
    function comptypes(typa, typb: pstruc): boolean;
    begin
      if typa = typb then comptypes := true
      else begin
        comptypes := false;
        if (typa^.kind = sksubrange) then comptypes := comptypes(typb, typa^.subtyp)
        else
          if (typb^.kind = sksubrange) then comptypes := comptypes(typa, typb^.subtyp)
          else
            if typa^.kind = skarray then
            begin
              if typb^.kind = skarray then
              begin
                if typa^.size = typb^.size then
                  if comptypes(typa^.indextyp, typb^.indextyp) then
                    comptypes := comptypes(typa^.elemtyp, typb^.elemtyp);
              end;
            end else
              if (typa^.kind = skpointer) and (typb^.kind = skpointer) then
              begin
                if (typa^.basetyp = nil) or (typb^.basetyp = nil) then comptypes := true
                else
                  comptypes := comptypes(typa^.basetyp, typb^.basetyp);
              end;
      end
    end;  (* comptypes *)

    (* return type range *)
    procedure getbounds(typ: pstruc; var lmin, lmax: integer);
    begin
      lmin := 0;
      lmax := 0;
      if typ^.kind = skordinal then
      begin
        if typ = intptr then
        begin
          lmin := low(integer);
          lmax := high(integer);
        end else
          if typ = charptr then lmax := 255
          else
            if typ = boolptr then lmax := 1
            else
              lmax := typ^.lconst.cval.ival;
      end else
        if typ^.kind = sksubrange then
        begin
          lmin := typ^.rmin;
          lmax := typ^.rmax;
        end;
    end;  (* getbounds *)

    (* check if string compatible type *)
    function isstring(typ: pstruc): boolean;
    begin
      isstring := false;
      if (typ^.kind = skarray) then
        isstring := typ^.elemtyp = charptr;
    end;  (* isstring *)

    (* load expression value *)
    procedure load;
    begin
      if gattr.kind = akconst then
      begin
        with gattr do
        begin
          if (typ^.kind = skordinal) or
             (typ^.kind = sksubrange) or
             (typ^.kind = skpointer) then
          begin
            case typ^.size of
            1    : genopimm(oppush, cval.ival, 1);
            2, 4 : genopimm(oppush, cval.ival, 4)
            else
              fatal('load ordinal const error')
            end;
          end else
            fatal('load const error')
        end
      end else
        if gattr.kind = akvar then
        begin
          with gattr do
          begin
            case vaccess of
            vadirect :
              begin
                if vlev = 0 then
                begin
                  if (typ^.kind in [skordinal, sksubrange, skpointer]) then
                  begin
                    case typ^.size of
                    1 : genopregmem(opmov, rgal, rgnone, rgnone, 1, 0, vaddr, vadata);
                    2 : genopregmem(opmov, rgax, rgnone, rgnone, 2, 0, vaddr, vadata);
                    4 : genopregmem(opmov, rgeax, rgnone, rgnone, 4, 0, vaddr, vadata);
                    end;
                    genopreg(oppush, rgeax)
                  end else
                    fatal('load var direct global error')
                end else
                  if vlev = level then
                  begin
                    if (typ^.kind in [skordinal, sksubrange, skpointer]) then
                    begin
                      case typ^.size of
                      1 : genopregmem(opmov, rgal, rgebp, rgnone, 1, 0, vaddr, vanone);
                      2 : genopregmem(opmov, rgax, rgebp, rgnone, 2, 0, vaddr, vanone);
                      4 : genopregmem(opmov, rgeax, rgebp, rgnone, 4, 0, vaddr, vanone);
                      end;
                      genopreg(oppush, rgeax)
                    end else
                      fatal('load var direct local error');
                  end else begin
                    if (typ^.kind in [skordinal, sksubrange, skpointer]) then
                    begin
                      genopreg(oppush, rgebp);
                      genopregmem(opmov, rgebp, rgebp, rgnone, 4, 0, -vlev * 4, vanone);
                      case typ^.size of
                      1 : genopregmem(opmov, rgal, rgebp, rgnone, 1, 0, vaddr, vanone);
                      2 : genopregmem(opmov, rgax, rgebp, rgnone, 2, 0, vaddr, vanone);
                      4 : genopregmem(opmov, rgeax, rgebp, rgnone, 4, 0, vaddr, vanone);
                      end;
                      genopreg(oppop, rgebp);
                      genopreg(oppush, rgeax)
                    end else
                      fatal('load var direct sub level error');
                  end
              end;

            vaindirect :
              begin
                if typ^.kind in [skordinal, sksubrange, skpointer] then
                begin
                  genopreg(oppop, rgebx);
                  case typ^.size of
                  1 : genopregmem(opmov, rgal, rgebx, rgnone, 1, 0, 0, vanone);
                  2 : genopregmem(opmov, rgax, rgebx, rgnone, 2, 0, 0, vanone);
                  4 : genopregmem(opmov, rgeax, rgebx, rgnone, 4, 0, 0, vanone);
                  end;
                  genopreg(oppush, rgeax);
                end else
                  fatal('load var indirect error')
              end;

            vaindexed :
              fatal('load var indexed error')

            end;
          end;
        end;
      gattr.kind := akexpr;
    end;  (* load *)

    // store expression value
    procedure store(attr: tattr);
    begin
      with attr do
      begin
        case vaccess of
        vadirect   :
          begin
            if vlev = 0 then
            begin
              if typ^.kind in [skordinal, sksubrange, skpointer] then
              begin
                genopreg(oppop, rgeax);
                case typ^.size of
                1 : genopmemreg(opmov, rgal, rgnone, rgnone, 1, 0, vaddr, vadata);
                2 : genopmemreg(opmov, rgax, rgnone, rgnone, 2, 0, vaddr, vadata);
                4 : genopmemreg(opmov, rgeax, rgnone, rgnone, 4, 0, vaddr, vadata);
                else
                  fatal('store direct global error');
                end;
              end else
                fatal('store error');
            end else
              if vlev = level then
              begin
                if (typ^.kind = skordinal) or
                   (typ^.kind = sksubrange) or
                   (typ^.kind = skpointer) then
                begin
                  genopreg(oppop, rgeax);
                  case typ^.size of
                  1 : genopmemreg(opmov, rgal, rgebp, rgnone, 1, 0, vaddr, vanone);
                  2 : genopmemreg(opmov, rgax, rgebp, rgnone, 2, 0, vaddr, vanone);
                  4 : genopmemreg(opmov, rgeax, rgebp, rgnone, 4, 0, vaddr, vanone);
                  else
                    fatal('store direct local error');
                  end;
                end else
                  fatal('store direct local error');
              end else begin
                if (typ^.kind = skordinal) or
                   (typ^.kind = sksubrange) or
                   (typ^.kind = skpointer) then
                begin
                  genopreg(oppop, rgeax);
                  genopreg(oppush, rgebp);
                  genopregmem(opmov, rgebp, rgebp, rgnone, 4, 0, -(vlev * 4), vanone);
                  case typ^.size of
                  1 : genopmemreg(opmov, rgal, rgebp, rgnone, 1, 0, vaddr, vanone);
                  2 : genopmemreg(opmov, rgax, rgebp, rgnone, 2, 0, vaddr, vanone);
                  4 : genopmemreg(opmov, rgeax, rgebp, rgnone, 4, 0, vaddr, vanone);
                  else
                    fatal('store direct local error');
                  end;
                  genopreg(oppop, rgebp);
                end else
                  fatal('store direct sub level error');
              end;
          end;
        else    (* indirect *)
          if vaddr <> 0 then fatal('store indirect vaddr <> 0');
          if (typ^.kind = skordinal) or
             (typ^.kind = sksubrange) or
             (typ^.kind = skpointer) then
          begin
            genopreg(oppop, rgeax);
            genopreg(oppop, rgebx);
            case typ^.size of
            1 : genopmemreg(opmov, rgal, rgebx, rgnone, 1, 0, 0, vanone);
            2 : genopmemreg(opmov, rgax, rgebx, rgnone, 2, 0, 0, vanone);
            4 : genopmemreg(opmov, rgeax, rgebx, rgnone, 4, 0, 0, vanone);
            else
              fatal('store indirect unknow ordinal size');
            end;
          end else
            fatal('store indirect error');
        end;
      end;
    end;  // store

    // load address
    procedure loadaddress;
    begin
      with gattr do
        if kind = akvar then
        begin
          if vaccess = vadirect then
          begin
            if vlev = 0 then
            begin (* vaddr indicate offset relative in data section *)
              genopregmem(oplea, rgebx, rgnone, rgnone, 4, 0, vaddr, vadata);
              genopreg(oppush, rgebx);
            end else
              if vlev = level then
              begin   (* vaddr indicate offset relative to stack frame *)
                genopregmem(oplea, rgebx, rgebp, rgnone, 4, 0, vaddr, vanone);
                genopreg(oppush, rgebx);
              end else begin
                fatal('load sub level address');
                genopreg(oppush, rgebp);
                genopregmem(opmov, rgebp, rgebp, rgnone, 4, 0, -vlev * 4, vanone);
                genopregmem(oplea, rgebx, rgebp, rgnone, 4, 0, vaddr, vanone);
                genopreg(oppop, rgebp);
                genopreg(oppush, rgebx);
              end;
            vaccess := vaindirect;
            vaddr := 0;
          end;
        end else
          if kind = akconst then
          begin
            if isstring(typ) then
            begin
              genopregmem(oplea, rgebx, rgnone, rgnone, 4, 0, allocstrgcst(typ, cval), vaconst);
              genopreg(oppush, rgebx);
            end;
          end else
            fatal('load address error')
    end;  // loadaddress

    procedure expression; forward;

    procedure selector(ident: pident);
    var
      field: pident;
      ltyp: pstruc;
      lattr: tattr;
      lmin, lmax: integer;
    begin
      gattr.typ := ident^.typ;
      gattr.kind := akvar;
      gattr.vaccess := ident^.vaccess;
      gattr.vaddr := ident^.vaddr;
      gattr.vlev := ident^.vlev;
      if gattr.vaccess = vaindirect then
      begin
        ltyp := gattr.typ;
        gattr.typ := nilptr;
        gattr.vaccess := vadirect;
        load;
        gattr.typ := ltyp;
        gattr.kind := akvar;
        gattr.vaccess := vaindirect;
        gattr.vaddr := 0;
      end;

      while token in selecttokens do
      begin
        case token of
        tkarrow :
          begin
            loadtoken;
            if gattr.typ^.kind <> skpointer then fatal('pointer type needed');
            load;
            gattr.typ := gattr.typ^.basetyp;
            gattr.kind := akvar;
            gattr.vaccess := vaindirect;
            gattr.vaddr := 0;
          end;

        tklbracket :
          begin
            loadtoken;
            if gattr.typ^.kind <> skarray then fatal('array type needed');
            loadaddress;
            repeat
              lattr := gattr;
              expression;
              if not comptypes(gattr.typ, lattr.typ^.indextyp) then
                fatal('type mismatch');
              load;
              genopreg(oppop, rgeax);
              if gattr.typ^.size = 1 then genopregreg(opmovzx, rgeax, rgal)
              else
                if gattr.typ^.size = 2 then genopregreg(opmovzx, rgeax, rgax);
              getbounds(lattr.typ^.indextyp, lmin, lmax);
              genopregimm(opadd, rgeax, -lmin, 4);
              genop(opcdq);
              genopregimm(opmov, rgecx, lattr.typ^.elemtyp^.size, 4);
              genopreg(opimul, rgecx);
              genopmemreg(opadd, rgeax, rgesp, rgnone, 4, 0, 0, vanone);
              gattr.typ := lattr.typ^.elemtyp;
              gattr.kind := akvar;
              gattr.vaccess := vaindirect;
              gattr.vaddr := 0;
              done := token <> tkcomma;
              if not done then loadtoken;
            until done;
            expect(tkrbracket);
          end;

        tkperiod :
          begin
            loadtoken;
            if gattr.typ^.kind <> skrecord then fatal('record type needed');
            check(tkident);
            searchlist(ident^.typ^.fields, field);
            if gattr.vaccess = vadirect then
            begin
              gattr.vaddr := gattr.vaddr + field^.vaddr;
            end else
              if gattr.vaccess = vaindirect then
              begin
                // loadaddress;
                genopreg(oppop, rgebx);
                genopregimm(opadd, rgebx, field^.vaddr, 4);
                genopreg(oppush, rgebx);
              end else
                fatal('indexed error in selector period');

            ident := field;
            gattr.typ := field^.typ;
            gattr.kind := akvar;
            loadtoken;
          end;
        end
      end
    end;  // selector

    procedure structure(var struc: pstruc);
    var
      ldataal, lmin, lmax: integer;

      procedure ordinaltype(var struc: pstruc);
      var
        ident, nxt: pident;
        ival, ltop: integer;
      begin
        if token = tklparent then
        begin           (* enumeration *)
          ltop := top;
          while display[top].kind <> dkblock do dec(top);
          loadtoken;
          new(struc);
          with struc^ do
          begin
            size := 1;
            kind := skordinal;
            declared := true;
          end;
          ival := 0;
          nxt := nil;
          repeat
            check(tkident);
            new(ident);
            with ident^ do
            begin
              next := nxt;
              name := strnew(@id);
              typ := struc;
              kind := ikconst;
              cval.ival := ival;
            end;
            nxt := ident;
            inc(ival);
            enterid(ident);
            loadtoken;
            done := token <> tkcomma;
            if not done then loadtoken
          until done;
          struc^.lconst := ident;
          expect(tkrparent);
          top := ltop;
        end else begin
          if token = tkident then
          begin
            searchid(ident);
            if ident^.kind = iktype then
            begin       (* alias *)
              loadtoken;
              struc := ident^.typ;
              exit
            end
          end;
          (* subrange *)
          new(struc);
          struc^.kind := sksubrange;
          expression;
          if gattr.typ^.kind <> skordinal then error('ordinal type required');
          struc^.subtyp := gattr.typ;
          struc^.size := gattr.typ^.size;
          struc^.rmin := gattr.cval.ival;
          expect(tkcolon);
          expression;
          if not comptypes(gattr.typ, struc^.subtyp) then error('type mismatch');
          struc^.rmax := gattr.cval.ival;
          if struc^.rmin > struc^.rmax then error('invalid range');
        end
      end;  (* ordinaltype *)

      procedure pointertype(var struc: pstruc);
      var
        ident: pident;
      begin
        check(tkident);
        new(struc);
        struc^.size := 4;
        struc^.kind := skpointer;
        notifysearcherr := false;
        searchid(ident);
        if ident = nil then
        begin
          new(ident);
          ident^.name := strnew(@id);
          ident^.typ := struc;
          ident^.kind := iktype;
          ident^.next := frwtyp;
          frwtyp := ident;
        end else struc^.basetyp := ident^.typ;
        notifysearcherr := true;
        loadtoken;
      end;  (* pointertype *)

      procedure arraytype(var struc: pstruc);
      var
        lmin, lmax: integer;
      begin
        new(struc);
        struc^.kind := skarray;
        structure(struc^.indextyp);
        if not ((struc^.indextyp^.kind = skordinal) or
                (struc^.indextyp^.kind = sksubrange)) then fatal('ordinal type required');
        if token = tkcomma then
        begin
          loadtoken;
          arraytype(struc^.elemtyp);
        end else begin
          expect(tkrbracket);
          expect(tkof);
          structure(struc^.elemtyp);
        end;
        getbounds(struc^.indextyp, lmin, lmax);
        struc^.size := ((lmax - lmin) + 1) * struc^.elemtyp^.size;
      end;  (* arraytype *)

      procedure fieldlist(var size: integer);
      var
        ident, nxt, last: pident;
        typ, ctyp: pstruc;
        lsize, msize: integer;
        done: boolean;
      begin
        (* fixed fields *)
        if token = tkident then
          repeat
            nxt := nil;
            repeat
              check(tkident);
              new(ident);
              ident^.next := nxt;
              ident^.name := strnew(@id);
              ident^.kind := ikfield;
              enterid(ident);
              nxt := ident;
              loadtoken;
              done := token <> tkcomma;
              if not done then loadtoken;
            until done;
            expect(tkcolon);
            structure(typ);

            last := nil;
            nxt := nil;
            while ident <> nil do
            begin
              last := ident;
              ident^.typ := typ;
              ident := ident^.next;
              last^.next := nxt;
              nxt := last;
            end;

            while last <> nil do
            begin
              last^.vaddr := size;
              align(size, last^.typ);
              last := last^.next;
            end;

            done := token <> tksemicolon;
            if not done then loadtoken;
          until done or (token = tkcase);

        (* variable fields *)
        if token = tkcase then
        begin
          (* case field *)
          loadtoken;
          check(tkident);
          new(ident);
          ident^.next := nil;
          ident^.name := strnew(@id);
          ident^.kind := ikfield;
          ident^.vaccess := vadirect;
          ident^.vaddr := size;
          loadtoken;
          expect(tkcolon);
          structure(ident^.typ);
          if not (ident^.typ^.kind in [skordinal, sksubrange]) then fatal('ordinal required');
          align(size, ident^.typ);
          expect(tkof);
          ctyp := ident^.typ;
          msize := size;

          repeat
            (* constant selector *)
            repeat
              expression;
              if not comptypes(ctyp, gattr.typ) then fatal('type mismatch');
              done := token <> tkcomma;
              if not done then loadtoken;
            until done;

            (* fields *)
            expect(tkcolon);
            expect(tklparent);
            lsize := size;
            fieldlist(lsize);
            if lsize > msize then msize := lsize;
            expect(tkrparent);

            done := token <> tksemicolon;
            if not done then loadtoken;
          until done;
          size := msize;
        end;
      end;  (* fieldlist *)

      procedure objecttype(var size: integer);
      var
        ident, nxt: pident;
        typ: pstruc;
        done: boolean;
      begin
        (* fields *)
        if token = tkident then
          repeat
            nxt := nil;
            repeat
              check(tkident);
              new(ident);
              ident^.next := nxt;
              ident^.name := strnew(@id);
              ident^.kind := ikfield;
              enterid(ident);
              nxt := ident;
              loadtoken;
              done := token <> tkcomma;
              if not done then loadtoken;
            until done;
            expect(tkcolon);
            structure(typ);
            while ident <> nil do
            begin
              ident^.typ := typ;
              ident^.vaccess := vadirect;
              ident^.vaddr := size;
              align(size, typ);
              ident := ident^.next;
            end;
            done := token <> tksemicolon;
            if not done then loadtoken;
          until done or (token in [tkfunction, tkprocedure]);

        (* methods *)
        if token in [tkfunction, tkprocedure] then
          repeat
            if not (token in [tkfunction, tkprocedure]) then
              fatal('method declaration expected');
            done := token <> tksemicolon;
            if not done then loadtoken;
          until done;
      end;  (* objecttype *)

    begin (* structure *)
      if token in ordtyptokens then ordinaltype(struc)
      else
        if token = tkarrow then
        begin
          loadtoken;
          pointertype(struc);
        end else begin
          ldataal := dataal;
          if token = tkpacked then
          begin
            loadtoken;
            dataal := 1;
          end;

          if token = tkarray then
          begin
            loadtoken;
            expect(tklbracket);
            arraytype(struc);
          end else
            if token = tkrecord then
            begin
              loadtoken;
              if top >= maxtop then fatal('display overflow');
              inc(top);
              display[top].labels := nil;
              display[top].idents := nil;
              display[top].kind := dkdecl;
              new(struc);
              struc^.size := 0;
              struc^.kind := skrecord;
              fieldlist(struc^.size);
              struc^.fields := display[top].idents;
              dec(top);
              expect(tkend);
            end else
              if token = tkset then
              begin
                loadtoken;
                expect(tkof);
                new(struc);
                struc^.kind := skset;
                structure(struc^.settyp);
                if not (struc^.settyp^.kind in [skordinal, sksubrange]) then
                  fatal('ordinal type required');
                getbounds(struc^.settyp, lmin, lmax);
                struc^.size := ((lmax - lmin) + 1) div 8;
                if ((lmax - lmin) + 1) mod 8 <> 0 then inc(struc^.size);
              end else
                if token = tkfile then
                begin
                  loadtoken;
                  new(struc);
                  struc^.size := 4;
                  struc^.kind := skfile;
                  if token = tkof then
                  begin
                    loadtoken;
                    structure(struc^.filtyp);
                  end else struc^.filtyp := nil;
                end else
                  if token in [tkfunction, tkprocedure] then
                  begin
                    fatal('not implemented yet');
                  end else
                    if token = tkobject then
                    begin
                      loadtoken;
                      if top >= maxtop then fatal('display overflow');
                      inc(top);
                      display[top].labels := nil;
                      display[top].idents := nil;
                      display[top].kind := dkdecl;
                      new(struc);
                      struc^.size := 0;
                      struc^.kind := skobject;
                      objecttype(struc^.size);
                      struc^.pubfields := display[top].idents;
                      dec(top);
                      expect(tkend);
                    end else
                      fatal('type declaration expected');

          dataal := ldataal;
        end
    end;  (* structure *)

    procedure labeldeclaration;
    var
      lbl: plabel;
      done: boolean;
    begin
      repeat
        check(tkintcst);
        new(lbl);
        with lbl^ do
        begin
          name := intcst;
          genlabel(ival);
          defined := false
        end;
        enterlabel(lbl);
        loadtoken;
        done := token <> tkcomma;
        if not done then loadtoken
      until done;
      expect(tksemicolon);
    end;  (* labeldeclaration *)

    procedure constdeclaration;
    var
      ident: pident;
    begin
      check(tkident);
      while token = tkident do
      begin
        new(ident);
        with ident^ do
        begin
          next := nil;
          name := strnew(@id);
          kind := ikconst;
        end;
        loadtoken;
        expect(tkequal);
        expression;
        ident^.typ := gattr.typ;
        ident^.cval := gattr.cval;
        enterid(ident);
        expect(tksemicolon)
      end
    end;  (* constdeclaration *)

    procedure typedeclaration;
    var
      ident, p, nxt: pident;
    begin
      check(tkident);
      while token = tkident do
      begin
        new(ident);
        with ident^ do
        begin
          next := nil;
          name := strnew(@id);
          kind := iktype
        end;
        loadtoken;
        expect(tkequal);
        structure(ident^.typ);
        enterid(ident);
        expect(tksemicolon);

        nxt := nil;
        p := frwtyp;
        while p <> nil do
        begin
          if strcomp(p^.name, ident^.name) = 0 then
          begin
            if p = frwtyp then frwtyp := p^.next
                          else nxt^.next := p^.next;
            p := nil;
          end else begin
            nxt := p;
            p := p^.next;
          end;
        end;
      end;

      while frwtyp <> nil do
      begin
        error(format('type %s not completely defined', [frwtyp^.name]));
        frwtyp := frwtyp^.next;
      end;
    end;  (* typedeclaration *)

    procedure vardeclaration;
    var
      ident, nxt: pident;
      struc: pstruc;
      done: boolean;
    begin
      check(tkident);
      while token = tkident do
      begin
        nxt := nil;
        repeat
          check(tkident);
          new(ident);
          with ident^ do
          begin
            next := nxt;
            name := strnew(@id);
            kind := ikvar;
            vaccess := vadirect;
            vlev := level;
          end;
          enterid(ident);
          nxt := ident;
          loadtoken;
          done := token <> tkcomma;
          if not done then loadtoken
        until done;
        expect(tkcolon);
        structure(struc);
        while ident <> nil do
        begin
          with ident^ do
          begin
            typ := struc;
            allocvar(vaddr, struc);
          end;
          ident := ident^.next
        end;
        expect(tksemicolon)
      end;

      while frwtyp <> nil do
      begin
        error(format('type %s not completely defined', [frwtyp^.name]));
        frwtyp := frwtyp^.next;
      end;
    end;  (* vardeclaration *)

    procedure funcdeclaration(isfunction: boolean);
    var
      funcid, parm: pident;
      forw: boolean;

      procedure parameterlist;
      var
        done: boolean;
        access: tvaraccess;
        ident, nxt, last, first: pident;
        struc: pstruc;
      begin
        if top >= maxtop then fatal('display overflow');
        inc(top);
        display[top].labels := nil;
        display[top].idents := nil;
        display[top].kind := dkdecl;

        first := nil;
        repeat
          if token = tkvar then
          begin
            loadtoken;
            access := vaindirect
          end else access := vadirect;
          nxt := nil;
          repeat
            check(tkident);
            new(ident);
            with ident^ do
            begin
              next := nxt;
              name := strnew(@id);
              typ := nil;
              kind := ikvar;
              vaccess := access;
              vlev := level + 1;
            end;
            enterid(ident);
            nxt := ident;
            loadtoken;
            done := token <> tkcomma;
            if not done then loadtoken
          until done;
          expect(tkcolon);
          structure(struc);
          last := ident;
          while ident <> nil do
          begin
            ident^.typ := struc;
            nxt := ident;
            ident := ident^.next;
          end;
          nxt^.next := first;
          first := last;
          done := token <> tksemicolon;
          if not done then loadtoken;
        until done;

        (* reverse order *)
        ident := first;
        nxt := nil;
        while ident <> nil do
        begin
          last := ident;
          ident := ident^.next;
          last^.next := nxt;
          nxt := last;
        end;

        funcid^.fparams := nxt;

        dec(top);
      end;  (* parameterlist *)

      procedure parameteroffset;
      var
        parm: pident;
        size, i: integer;
        parmsize: array of integer;
      begin
        parm := funcid^.fparams;
        size := 8;
        if funcid^.fconv <> ccpascal then
        begin
          while parm <> nil do
          begin
            parm^.vaddr := size;
            if parm^.vaccess = vadirect then align(size, parm^.typ)
                                        else align(size, nilptr);
            parm := parm^.next;
          end
        end else begin
          setlength(parmsize, 0);
          i := 0;
          while parm <> nil do
          begin
            setlength(parmsize, length(parmsize) + 1);
            if parm^.vaccess = vadirect then parmsize[i] := parm^.typ^.size
                                        else parmsize[i] := nilptr^.size;
            parm := parm^.next;
            inc(i);
          end;

          for i := 0 to high(parmsize) do
          begin
            inc(size, parmsize[i]);
            while (size mod dataal) <> 0 do inc(size);
          end;

          parm := funcid^.fparams;
          i := high(parmsize);
          while parm <> nil do
          begin
            dec(size, parmsize[i]);
            while (size mod dataal) <> 0 do dec(size);
            parm^.vaddr := size;
            parm := parm^.next;
            dec(i);
          end;
        end;
      end;  (* parameteroffset *)

    begin (* funcdeclaration *)
      check(tkident);

      notifysearcherr := false;
      searchid(funcid);
      notifysearcherr := true;
      if funcid <> nil then
      begin
        if frwall or (funcid^.kind <> ikfunc) or (funcid^.forw = false) then
          fatal(format('identifier ''%s'' redeclared', [id]));
        forw := true;
        funcid^.forw := false;
      end else begin
        new(funcid);
        with funcid^ do
        begin
          next := nil;
          name := strnew(@id);
          typ := nil;
          kind := ikfunc;
          fkind := fkdeclared;
          genlabel(faddr);
          fparams := nil;
          flocals := 0;
          fconv := ccpascal;
          forw := frwall;
        end;
        enterid(funcid);
        forw := false;
      end;
      loadtoken;

      if not forw then
      begin

        (* optional parameters *)
        if token = tklparent then
        begin
          loadtoken;
          if token <> tkrparent then
          begin
            parameterlist;
            expect(tkrparent);
          end else loadtoken;
        end;

        (* function's result type *)
        if isfunction then
        begin
          expect(tkcolon);
          structure(funcid^.typ);
        end;
      end;
      expect(tksemicolon);

      (* optional directive *)
      while (token = tkident) or (token = tkforward) do
      begin
        if token = tkforward then
        begin
          if forw then fatal('''forwrd'' used twice');
          funcid^.forw := true;
        end else begin
          if strcomp(@id, 'pascal') = 0 then funcid^.fconv := ccpascal else
          if strcomp(@id, 'stdcall') = 0 then funcid^.fconv := ccstdcall else
          if strcomp(@id, 'cdecl') = 0 then funcid^.fconv := cccdecl else
          if strcomp(@id, 'varargs') = 0 then funcid^.fconv := ccvarargs else
          if strcomp(@id, 'external') = 0 then
          begin
            funcid^.fkind := fkexternal;
          end else
            error(format('unknow directive %s', [id]));
        end;
        loadtoken;
        expect(tksemicolon);
      end;

      if (funcid^.fkind = fkdeclared) and (not funcid^.forw) then
      begin
        if level >= maxlevel then error('level overflow');
        inc(level);
        if top = maxtop then fatal('display overflow');
        inc(top);
        display[top].labels := nil;
        display[top].idents := nil;
        display[top].kind := dkblock;
        if funcid^.fparams <> nil then
        begin
          parameteroffset;
          parm := funcid^.fparams;
          while parm <> nil do
          begin
            enterid(parm);
            parm := parm^.next;
          end;
        end;
        block(funcid);
        expect(tksemicolon);
        dec(top);
        dec(level);
      end;
    end;  (* funcdeclaration *)

    procedure call(ident: pident);
    type
      pcodelist = ^tcodelist;
      tcodelist = record
        link: pcodelist;
        code: pcode;
      end;

    var
      parmsize: integer;
      codelist, nxt, last: pcodelist;

      procedure ordfunc;
      begin
        expect(tklparent);
        expression;
        if gattr.typ^.kind <> skordinal then fatal('ordinal type required');
        gattr.typ := intptr;
        expect(tkrparent)
      end;  // ordfunc

      procedure chrfunc;
      begin
        expect(tklparent);
        expression;
        if not comptypes(intptr, gattr.typ) then fatal('integer type required');
        gattr.typ := charptr;
        expect(tkrparent)
      end;  // chrfunc

      procedure predfunc;
      begin
        expect(tklparent);
        expression;
        if gattr.typ^.kind <> skordinal then fatal('ordinal type required');
        expect(tkrparent)
      end;  // predfunc

      procedure succfunc;
      begin
        expect(tklparent);
        expression;
        if gattr.typ^.kind <> skordinal then fatal('ordinal type required');
        expect(tkrparent)
      end;  // succfunc

      procedure lowfunc;
      var
        lmin, lmax: integer;
      begin
        expect(tklparent);
        expression;
        if gattr.typ^.kind in [skordinal, sksubrange] then getbounds(gattr.typ, lmin, lmax)
        else
          if gattr.typ^.kind = skarray then getbounds(gattr.typ^.indextyp, lmin, lmax)
          else
            fatal('ordinal or array required');
        expect(tkrparent);
        gattr.typ := intptr;
        gattr.kind := akconst;
        gattr.cval.ival := lmin;
      end;  // lowfunc

      procedure highfunc;
      var
        lmin, lmax: integer;
      begin
        expect(tklparent);
        expression;
        if gattr.typ^.kind in [skordinal, sksubrange] then getbounds(gattr.typ, lmin, lmax)
        else
          if gattr.typ^.kind = skarray then getbounds(gattr.typ^.indextyp, lmin, lmax)
          else
            fatal('ordinal or array required');
        expect(tkrparent);
        gattr.typ := intptr;
        gattr.kind := akconst;
        gattr.cval.ival := lmax;
      end;  // highfunc

      (* return size of a variable *)
      (* note : type not allowed, must be a variable *)
      procedure sizeoffunc;
      var lcode: pcode;
      begin
        expect(tklparent);

        (* preserve current code, discard expression, only interest of expression type *)
        lcode := code; code := nil;
        expression;
        code := lcode;

        if gattr.kind <> akvar then fatal('variable required');
        gattr.cval.ival := gattr.typ^.size;
        gattr.kind := akconst;
        gattr.typ := intptr;
        expect(tkrparent)
      end;  // sizeoffunc

      procedure incfunc;
      var
        lattr: tattr;
      begin
        expect(tklparent);
        expression;
        if not (gattr.typ^.kind in [skordinal, sksubrange, skpointer]) then
          fatal('ordinal or pointer required');
        loadaddress;
        lattr := gattr;
        if token = tkcomma then
        begin
          loadtoken;
          expression;
          if not comptypes(gattr.typ, intptr) then fatal('integer required');
          load;
          genopreg(oppop, rgeax);
          genopreg(oppop, rgebx);
          genopmemreg(opadd, rgeax, rgebx, rgnone, 4, 0, 0, vanone);
        end else begin
          genopreg(oppop, rgebx);
          genopmemimm(opinc, rgebx, rgnone, 1, 1, 4, 0, 0, vanone);
        end;
        expect(tkrparent);
      end;  (* incfunc *)

      procedure decfunc;
      var
        lattr: tattr;
      begin
        expect(tklparent);
        expression;
        if not (gattr.typ^.kind in [skordinal, sksubrange, skpointer]) then
          fatal('ordinal or pointer required');
        loadaddress;
        lattr := gattr;
        if token = tkcomma then
        begin
          loadtoken;
          expression;
          if not comptypes(gattr.typ, intptr) then fatal('integer required');
          load;
          genopreg(oppop, rgeax);
          genopreg(oppop, rgebx);
          genopmemreg(opsub, rgeax, rgebx, rgnone, 4, 0, 0, vanone);
        end else begin
          genopreg(oppop, rgebx);
          genopmemimm(opdec, rgebx, rgnone, 1, 1, 4, 0, 0, vanone);
        end;
        expect(tkrparent);
      end;  (* decfunc *)

      procedure writefunc(index: integer);
      var
        done: boolean;
      begin
        if index = syswriteindex then check(tklparent);
        if token = tklparent then
        begin
          loadtoken;
          repeat
            expression;
            if gattr.typ = charptr then
            begin
              load;
              genopimm(opsyscall, syslblwrc, 4);
            end else
              if comptypes(gattr.typ, intptr) then
              begin
                load;
                genopimm(opsyscall, syslblwri, 4);
              end else
                if isstring(gattr.typ) then
                begin
                  loadaddress;
                  genopimm(oppush, gattr.typ^.size, 4);
                  genopimm(opsyscall, syslblwra, 4);
              end else
                fatal('not yet implemented');
            done := token <> tkcomma;
            if not done then loadtoken;
          until done;
          expect(tkrparent);
        end;
        if index = syswritelnindex then genopimm(opsyscall, syslblwrl, 4);
      end;  (* writefunc *)

      procedure readfunc;
      begin
        expect(tklparent);
        repeat
          expression;
          if gattr.kind <> akvar then fatal('variable expected');
          if comptypes(gattr.typ, charptr) then
          begin
            loadaddress;
            genopimm(opsyscall, syslblrdc, 4);
          end else fatal('read only implemented for char type');
          done := token <> tkcomma;
          if not done then loadtoken;
        until done;
        expect(tkrparent);
      end;  (* readfunc *)

      procedure eoffunc;
      begin
        if cstexpr then fatal('constant expression expected');
        genopimm(opsyscall, syslbleof, 4);
        genopreg(oppush, rgeax);
        gattr.typ := boolptr;
        gattr.kind := akexpr;
      end;  (* eoffunc *)

      procedure getmemfunc;
      var
        lattr: tattr;
      begin
        expect(tklparent);
        expression;
        if gattr.kind <> akvar then fatal('variable expected');
        if gattr.typ^.kind <> skpointer then fatal('pointer type required');
        lattr := gattr;
        expect(tkcomma);
        expression;
        if not comptypes(gattr.typ, intptr) then fatal('integer expected');
        load;
        genopimm(opsyscall, syslblgtm, 4);
        genopreg(oppush, rgeax);
        gattr.kind := akexpr;
        store(lattr);
        expect(tkrparent);
      end;  (* getmemfunc *)

      procedure freememfunc;
      begin
        expect(tklparent);
        expression;
        if gattr.kind <> akvar then fatal('variable expected');
        if gattr.typ^.kind <> skpointer then fatal('pointer type required');
        load;
        genopimm(opsyscall, syslblfrm, 4);
        expect(tkrparent);
      end;  (* freememfunc *)

      procedure newfunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* newfunc *)

      procedure disposefunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* disposefunc *)

      procedure assignfunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* assignfunc *)

      procedure resetfunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* resetfunc *)

      procedure rewritefunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* rewritefunc *)

      procedure closefunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* closefunc *)

      procedure seekfunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* seekfunc *)

      procedure includefunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* includefunc *)

      procedure excludefunc;
      begin
        if cstexpr then fatal('constant expression expected');
      end;  (* excludefunc *)

      procedure parameterlist;
      var
        parm: pident;
        lcode: pcode;
        nxtcode: pcodelist;
      begin
        lcode := code;
        parmsize := 0;
        parm := ident^.fparams;
        expect(tklparent);
        repeat
          new(nxtcode);
          nxtcode^.link := codelist;
          codelist := nxtcode;
          code := nil;
          expression;
          if not comptypes(gattr.typ, parm^.typ) then error('type mismatch');
          if parm.vaccess = vaindirect then
          begin
            if gattr.kind <> akvar then fatal('variable required');
            loadaddress;
            align(parmsize, nilptr);
          end else begin
            load;
            align(parmsize, parm^.typ);
          end;
          codelist^.code := code;
          parm := parm^.next;
          if parm <> nil then expect(tkcomma);
        until parm = nil;
        expect(tkrparent);
        code := lcode;
      end;  (* parameterlist *)

      procedure varargslist;
      begin

      end;  (* varargslist *)

    begin // call
      case ident^.fkind of
      fksystem :
        begin
          case ident^.faddr of
          sysordindex      : ordfunc;
          syschrindex      : chrfunc;
          syspredindex     : predfunc;
          syssuccindex     : succfunc;
          syslowindex      : lowfunc;
          syshighindex     : highfunc;
          syssizeofindex   : sizeoffunc;
          sysincindex      : incfunc;
          sysdecindex      : decfunc;
          syswriteindex,
          syswritelnindex  : writefunc(ident^.faddr);
          sysreadindex     : readfunc;
          syseofindex      : eoffunc;
          sysgetmemindex   : getmemfunc;
          sysfreememindex  : freememfunc;
          end;
        end;

      fkmain :
        fatal('cannot call main function');

      fkdeclared,
      fkexternal :
        begin
          if cstexpr then fatal('constant expression expected');

          codelist := nil;

          (* obtain parameters *)
          if ident^.fconv = ccvarargs then varargslist
          else
            if ident^.fparams <> nil then parameterlist;

          (* reverse parameters list for pascal calling convention *)
          if ident^.fconv = ccpascal then
          begin
            nxt := nil;
            while codelist <> nil do
            begin
              last := codelist;
              codelist := codelist^.link;
              last^.link := nxt;
              nxt := last;
            end;
            codelist := nxt;
          end;

          (* push parameters *)
          while codelist <> nil do
          begin
            mergecode(codelist^.code);
            codelist := codelist^.link;
          end;

          (* declared -> direct call, external -> indirect call *)
          if ident^.fkind = fkdeclared then
            genopimm(opcall, ident^.faddr, 4)
          else
            genopmem(opcall, rgnone, rgnone, 4, 0, ident^.faddr, vaimport);

          (* c calling convention, caller must restore stack *)
          if ident^.fconv in [cccdecl, ccvarargs] then
            if parmsize <> 0 then genopregimm(opadd, rgesp, parmsize, 4);
        end;
      end;
    end;  (* call *)

    procedure statement;

      procedure labelstatement;
      var
        lbl: plabel;
      begin
        searchlabel(lbl);
        if lbl^.defined then error(format('label ''%d'' redefined', [lbl^.name]));
        lbl^.defined := true;
        putlabel(lbl^.ival);
        loadtoken;
        expect(tkcolon)
      end;  (* labelstatement *)

      procedure idstatement;
      var
        ident: pident;
        lattr: tattr;
      begin
        genlinenumber(chline);
        searchid(ident);

        if ident^.kind = ikunit then
        begin
          loadtoken;
          expect(tkperiod);
          check(tkident);
          searchlist(display[disx].idents, ident);
        end;

        loadtoken;
        case ident^.kind of
        ikvar :
          begin
            selector(ident);
            if not (gattr.typ^.kind in [skordinal, sksubrange, skpointer]) then
              loadaddress;
            lattr := gattr;
            expect(tkassign);
            expression;
            if not comptypes(gattr.typ, lattr.typ) then fatal('type mismatch');
            if lattr.typ^.kind in [skordinal, sksubrange, skpointer] then
            begin
              load;
              store(lattr);
            end else
              if lattr.typ^.kind = skarray then
              begin
                loadaddress;
                genopregimm(opmov, rgecx, lattr.typ^.size, 4);
                genopreg(oppop, rgesi);
                genopreg(oppop, rgedi);
                genop(opcld);
                genop(oprep);
                genop(opmovsb);
              end else
                fatal('assign to this type not implemented yet');
          end;

        ikfunc :
          begin
            if ident^.typ = nil then call(ident)
            else
              if ident = func then
              begin        (* assign to function's result *)
                expect(tkassign);
                expression;
                if not comptypes(gattr.typ, func^.typ) then error('type mismatch');
                load;
                lattr.typ := func^.typ;
                lattr.kind := akvar;
                lattr.vaccess := vadirect;
                lattr.vlev := level;
                lattr.vaddr := -(level * 4);
                store(lattr);
              end else
                fatal('cannot call function outside expression')
          end;
        else
          fatal('cannot assign to constant');
        end;
      end;  (* idstatement *)

      procedure compoundstatement;
      var
        done: boolean;
      begin
        repeat
          while token in stattokens do statement;
          done := token <> tksemicolon;
          if not done then loadtoken
        until done;
        expect(tkend)
      end;  (* compoundstatement *)

      procedure ifstatement;
      var
        lbl1, lbl2: integer;
      begin
        genlabel(lbl1);
        expression;
        load;
        if not comptypes(gattr.typ, boolptr) then fatal('boolean expression expected');
        genfalsejump(lbl1);
        expect(tkthen);
        statement;
        if token = tkelse then
        begin
          loadtoken;
          genlabel(lbl2);
          genjump(lbl2);
          putlabel(lbl1);
          statement;
          putlabel(lbl2);
        end else
          putlabel(lbl1);
      end;  (* ifstatement *)

      procedure casestatement;
      var
        casetyp: pstruc;
        done: boolean;
      begin
        expression;
        if not ((gattr.typ^.kind = skordinal) or
                (gattr.typ^.kind = sksubrange)) then fatal('ordinal type required');
        casetyp := gattr.typ;
        expect(tkof);
        cstexpr := true;
        repeat
          repeat
            expression;
            if not comptypes(casetyp, gattr.typ) then fatal('type mismatch');
            done := token <> tkcomma;
            if not done then loadtoken;
          until done;
          expect(tkcolon);
          statement;
          done := token <> tksemicolon;
          if not done then loadtoken;
        until done;
        if token = tkelse then
        begin
          statement;
        end;
        expect(tkend);
        cstexpr := false;
      end;  // casestatement

      procedure forstatement;
      begin
      end;  // forstatement

      procedure whilestatement;
      var
        lbl1, lbl2: integer;
      begin
        lbl1 := nextlbl; genlabel(nextlbl);
        lbl2 := breaklbl; genlabel(breaklbl);
        putlabel(nextlbl);
        expression;
        load;
        if not comptypes(gattr.typ, boolptr) then fatal('boolean expression expected');
        genfalsejump(breaklbl);
        expect(tkdo);
        statement;
        genjump(nextlbl);
        putlabel(breaklbl);
        nextlbl := lbl1;
        breaklbl := lbl2;
      end;  (* whilestatement *)

      procedure repeatstatement;
      var
        done: boolean;
        lbl1, lbl2: integer;
      begin
        lbl1 := nextlbl; genlabel(nextlbl);
        lbl2 := breaklbl; genlabel(breaklbl);
        putlabel(nextlbl);
        repeat
          while token in stattokens do statement;
          done := token <> tksemicolon;
          if not done then loadtoken
        until done;
        expect(tkuntil);
        expression;
        load;
        if not comptypes(gattr.typ, boolptr) then fatal('boolean expression expected');
        genfalsejump(nextlbl);
        putlabel(breaklbl);
        nextlbl := lbl1;
        breaklbl := lbl2;
      end;  (* repeatstatement *)

      procedure withstatement;
      var
        done: boolean;
      begin
        repeat
          check(tkident);
          loadtoken;
          done := token <> tkcomma;
          if not done then loadtoken
        until done;
        statement
      end;  // withstatement

      procedure gotostatement;
      var
        lbl: plabel;
      begin
        genlinenumber(chline);
        check(tkintcst);
        searchlabel(lbl);
        genjump(lbl^.ival);
        loadtoken
      end;  (* gotostatement *)

      procedure exitstatement;
      begin
        genopimm(opjmp, exitlbl, 4);
        loadtoken
      end;  (* exitstatement *)

      procedure breakstatement;
      begin
        if breaklbl <> 0 then genopimm(opjmp, breaklbl, 4)
        else error('''break'' cannot be used outside loop statement');
        loadtoken
      end;  (* breakstatement *)

      procedure continuestatement;
      begin
        if nextlbl <> 0 then genopimm(opjmp, nextlbl, 4)
        else error('''continue'' cannot be used outside loop statement');
        loadtoken
      end;  (* continuestatement *)

    begin (* statement *)
      case token of
      tkintcst   : labelstatement;
      tkident    : idstatement;
      tkbegin    : begin loadtoken; compoundstatement end;
      tkif       : begin loadtoken; ifstatement end;
      tkcase     : begin loadtoken; casestatement end;
      tkfor      : begin loadtoken; forstatement end;
      tkwhile    : begin loadtoken; whilestatement end;
      tkrepeat   : begin loadtoken; repeatstatement end;
      tkwith     : begin loadtoken; withstatement end;
      tkgoto     : begin loadtoken; gotostatement end;
      tkexit     : exitstatement;
      tkbreak    : breakstatement;
      tkcontinue : continuestatement
      end
    end;  (* statement *)

    (* solve constant expression, compile variable expression *)
    procedure expression;
    var
      lop: ttoken;
      lattr: tattr;
      lmin, lmax: integer;

      procedure expr2;
      var
        lop: ttoken;
        lattr: tattr;

        procedure expr1;
        var
          lop: ttoken;
          lattr: tattr;

          procedure factor;
          var
            ident: pident;
            ltyp: pstruc;
          begin
            case token of
            tklparent :
              begin
                loadtoken;
                expression;
                expect(tkrparent)
              end;

            tklbracket :
              fatal('set not supported');

            tknot :
              begin
                loadtoken;
                factor;
                if comptypes(gattr.typ, intptr) then
                begin
                  if gattr.kind = akconst then
                  begin
                    gattr.cval.ival := not gattr.cval.ival;
                  end else begin
                    load;
                    genopreg(oppop, rgeax);
                    genopreg(opnot, rgeax);
                    genopreg(oppush, rgeax);
                  end;
                end else
                  if comptypes(gattr.typ, boolptr) then
                  begin
                    if gattr.kind = akconst then
                    begin
                      gattr.cval.bval := not gattr.cval.bval;
                    end else begin
                      load;
                      genopreg(oppop, rgeax);
                      genopregimm(opcmp, rgal, 0, 1);
                      genopreg(opsetz, rgal);
                      genopreg(oppush, rgeax);
                    end;
                  end else
                    fatal('type mismatch');
              end;

            tkplus :
              begin
                loadtoken;
                factor;
                if comptypes(gattr.typ, intptr) then fatal('type mismatch');
              end;

            tkminus :
              begin
                loadtoken;
                factor;
                if comptypes(gattr.typ, intptr) then
                begin
                  if gattr.kind = akconst then
                  begin
                    gattr.cval.ival := -gattr.cval.ival
                  end else begin
                    load;
                    genopreg(oppop, rgeax);
                    genopreg(opneg, rgeax);
                    genopreg(oppush, rgeax);
                  end;
                end else
                  fatal('type mismatch');
              end;

            tkaroba :      (* return address of expression and typed pointer *)
              begin
                loadtoken;
                expression;
                if gattr.kind <> akvar then fatal('variable expected');
                loadaddress;
                new(ltyp);
                ltyp^.size := 4;
                ltyp^.kind := skpointer;
                ltyp^.basetyp := gattr.typ;
                gattr.typ := ltyp;
                gattr.kind := akexpr;
              end;

            tkintcst :
              begin
                gattr.typ := intptr;
                gattr.kind := akconst;
                gattr.cval.ival := intcst;
                loadtoken;
              end;

            tkrealcst :
              fatal('real not supported');

            tkstrgcst :
              begin
                if strglen = 1 then
                begin
                  gattr.typ := charptr;
                  gattr.kind := akconst;
                  gattr.cval.ival := ord(strgcst[0]);
                end else begin
                  new(gattr.typ);
                  with gattr.typ^ do
                  begin
                    size := strglen;
                    kind := skarray;
                    new(indextyp);
                    with indextyp^ do
                    begin
                      size := 4;
                      kind := sksubrange;
                      subtyp := intptr;
                      rmin := 0;
                      rmax := strglen-1;
                    end;
                    elemtyp := charptr;
                  end;
                  gattr.kind := akconst;
                  gattr.cval.sval := strnew(@strgcst);
                end;
                loadtoken;
              end;

            tkident :
              begin
                searchid(ident);

                if ident^.kind = ikunit then
                begin
                  loadtoken;
                  expect(tkperiod);
                  check(tkident);
                  searchlist(display[disx].idents, ident);
                end;

                loadtoken;
                case ident^.kind of
                ikconst :
                  begin
                    gattr.typ := ident^.typ;
                    gattr.kind := akconst;
                    gattr.cval := ident^.cval;
                  end;

                iktype  :
                  fatal('typecasting not implemented');

                ikvar   :
                  begin
                    if cstexpr then fatal('constant expression expected');
                    selector(ident);
                  end;

                ikfunc  :
                  begin
                    if ident^.typ = nil then
                      fatal('cannot call procedure in expression');
                    call(ident);
                    if ident^.fkind = fkdeclared then
                    begin
                      genopreg(oppush, rgeax);
                      gattr.typ := ident^.typ;
                      gattr.kind := akexpr;
                    end;
                  end
                end;
              end;
            else
              fatal('expression expected')
            end;
          end;  (* factor *)

        begin (* expr1 *)
          factor;
          while token in muloperators do
          begin
            if not cstexpr then load;
            lop := token;
            if lop in [tkstar, tkdiv, tkmod, tkshl, tkshr] then
            begin
              if not comptypes(gattr.typ, intptr) then fatal('type mismatch');
            end else begin
              if not comptypes(gattr.typ, intptr) then
                if not comptypes(gattr.typ, boolptr) then fatal('type mismatch');
            end;
            lattr := gattr;
            loadtoken;
            factor;
            if cstexpr then
            begin
              if comptypes(gattr.typ, intptr) then
              begin
                case lop of
                tkstar : gattr.cval.ival := lattr.cval.ival * gattr.cval.ival;
                tkdiv  : gattr.cval.ival := lattr.cval.ival div gattr.cval.ival;
                tkmod  : gattr.cval.ival := lattr.cval.ival mod gattr.cval.ival;
                tkshl  : gattr.cval.ival := lattr.cval.ival shl gattr.cval.ival;
                tkshr  : gattr.cval.ival := lattr.cval.ival shr gattr.cval.ival;
                tkand  : gattr.cval.ival := lattr.cval.ival and gattr.cval.ival;
                end;
              end else
                if comptypes(gattr.typ, boolptr) then
                begin
                  gattr.cval.bval := lattr.cval.bval and gattr.cval.bval;
                end;
            end else begin
              if comptypes(gattr.typ, intptr) then
              begin
                load;
                genopreg(oppop, rgecx);
                genopreg(oppop, rgeax);
                if lop in [tkstar, tkdiv, tkmod] then genop(opcdq);
                case lop of
                tkstar : genopreg(opimul, rgecx);
                tkdiv  : genopreg(opidiv, rgecx);
                tkmod  : genopreg(opidiv, rgecx);
                tkshl  : genopregreg(opsal, rgeax, rgcl);
                tkshr  : genopregreg(opsar, rgeax, rgcl);
                tkand  : genopregreg(opand, rgeax, rgecx);
                end;
                if lop = tkmod then genopreg(oppush, rgedx)
                               else genopreg(oppush, rgeax);
              end else
                if comptypes(gattr.typ, boolptr) then
                begin
                  load;
                  genopreg(oppop, rgedx);
                  genopreg(oppop, rgeax);
                  genopregreg(opand, rgal, rgdl);
                  genopreg(oppush, rgeax);
                end;
            end;
          end;
        end;  (* expr1 *)

      begin (* expr2 *)
        expr1;
        while token in addoperators do
        begin
          if not cstexpr then load;
          lop := token;
          if lop in [tkplus, tkminus] then
          begin
            if not comptypes(gattr.typ, intptr) then fatal('type mismatch');
          end else begin
            if not comptypes(gattr.typ, intptr) then
              if not comptypes(gattr.typ, boolptr) then fatal('type mismatch');
          end;
          lattr := gattr;
          loadtoken;
          expr1;
          if not comptypes(gattr.typ, lattr.typ) then fatal('type mismatch');
          if cstexpr then
          begin
            if comptypes(gattr.typ, intptr) then
            begin
              case lop of
              tkplus  : gattr.cval.ival := lattr.cval.ival + gattr.cval.ival;
              tkminus : gattr.cval.ival := lattr.cval.ival - gattr.cval.ival;
              tkor    : gattr.cval.ival := lattr.cval.ival or gattr.cval.ival;
              tkxor   : gattr.cval.ival := lattr.cval.ival xor gattr.cval.ival;
              end;
            end else
              if comptypes(gattr.typ, boolptr) then
              begin
                case lop of
                tkor  : gattr.cval.bval := lattr.cval.bval or gattr.cval.bval;
                tkxor : gattr.cval.bval := lattr.cval.bval xor gattr.cval.bval;
                end;
              end;
          end else begin
            if comptypes(gattr.typ, intptr) then
            begin
              load;
              genopreg(oppop, rgedx);
              genopreg(oppop, rgeax);
              case lop of
              tkplus  : genopregreg(opadd, rgeax, rgedx);
              tkminus : genopregreg(opsub, rgeax, rgedx);
              tkor    : genopregreg(opor, rgeax, rgedx);
              tkxor   : genopregreg(opxor, rgeax, rgedx);
              end;
              genopreg(oppush, rgeax);
            end else
              load;
              if comptypes(gattr.typ, boolptr) then
              begin
                genopreg(oppop, rgedx);
                genopreg(oppop, rgeax);
                case lop of
                tkor  : genopregreg(opor, rgal, rgdl);
                tkxor : genopregreg(opxor, rgal, rgdl);
                end;
                genopreg(oppush, rgeax);
              end;
          end;
        end;
      end;  (* expr2 *)

    begin (* expression *)
      expr2;
      if token in reloperators then
      begin
        if not cstexpr then
          if gattr.typ^.kind in [skordinal, sksubrange, skpointer] then load
          else
            loadaddress;
        lattr := gattr;
        lop := token;
        loadtoken;
        expr2;
        if lop = tkin then
        begin
          if not cstexpr then
          begin
            if not (gattr.typ^.kind = skset) then fatal('set type required');
            if not comptypes(gattr.typ^.settyp, lattr.typ) then fatal('type mismatch');

            (*
                bit offset (in bit stream) = ival - lmin
                byte offset = bit offset div 8
                bit pos = bit offset mod 8
            *)

            getbounds(gattr.typ^.settyp, lmin, lmax);
            loadaddress;
            genopreg(oppop, rgebx);
            genopreg(oppop, rgeax);
            genopregimm(opsub, rgeax, lmin, gattr.typ^.settyp^.size);
            genopregreg(opmov, rgedx, rgeax);
            genopregimm(opand, rgedx, 3, 4);
            genopregimm(opshr, rgeax, 3, 4);
            genopregreg(opadd, rgebx, rgeax);
            genopregmem(opmov, rgal, rgebx, rgnone, 1, 0, 0, vanone);
            genopregreg(opbt, rgeax, rgedx);
            genopreg(opsetc, rgal);
            genopreg(oppush, rgeax);
          end else fatal('out of sense in constant expression');
        end else begin
          if not comptypes(gattr.typ, lattr.typ) then fatal('type mismatch');
          if cstexpr then fatal('out of sense in constant expression')
          else begin
            if comptypes(gattr.typ, intptr) then
            begin
              load;
              genopreg(oppop, rgedx);
              genopreg(oppop, rgeax);
              genopregreg(opcmp, rgeax, rgedx);
              case lop of
                tkequal    : genopreg(opsete, rgal);
                tknotequ   : genopreg(opsetne, rgal);
                tklower    : genopreg(opsetl, rgal);
                tklowequ   : genopreg(opsetle, rgal);
                tkgreater  : genopreg(opsetg, rgal);
                tkgreatequ : genopreg(opsetge, rgal);
              end;
              genopreg(oppush, rgeax);
            end else
              if comptypes(gattr.typ, boolptr) then
              begin
                load;
                genopreg(oppop, rgedx);
                genopreg(oppop, rgeax);
                genopregreg(opcmp, rgal, rgdl);
                case lop of
                  tkequal  : genopreg(opsete, rgal);
                  tknotequ : genopreg(opsetne, rgal);
                else
                  fatal('operator not applicable');
                end;
                genopreg(oppush, rgeax);
              end else
                if comptypes(gattr.typ, charptr) then
                begin
                  load;
                  genopreg(oppop, rgedx);
                  genopreg(oppop, rgeax);
                  genopregreg(opcmp, rgal, rgdl);
                  case lop of
                    tkequal    : genopreg(opsete, rgal);
                    tknotequ   : genopreg(opsetne, rgal);
                    tklower    : genopreg(opsetb, rgal);
                    tklowequ   : genopreg(opsetbe, rgal);
                    tkgreater  : genopreg(opseta, rgal);
                    tkgreatequ : genopreg(opsetae, rgal);
                  end;
                  genopreg(oppush, rgeax);
                end else
                  if gattr.typ^.kind in [skordinal, sksubrange] then
                  begin
                    load;
                    genopreg(oppop, rgedx);
                    genopreg(oppop, rgeax);
                    case gattr.typ^.size of
                      1 : genopregreg(opcmp, rgal, rgdl);
                      2 : genopregreg(opcmp, rgax, rgdx);
                      4 : genopregreg(opcmp, rgeax, rgedx);
                    end;
                    case lop of
                      tkequal    : genopreg(opsete, rgal);
                      tknotequ   : genopreg(opsetne, rgal);
                      tklower    : genopreg(opsetl, rgal);
                      tklowequ   : genopreg(opsetle, rgal);
                      tkgreater  : genopreg(opsetg, rgal);
                      tkgreatequ : genopreg(opsetge, rgal);
                    end;
                    genopreg(oppush, rgeax);
                  end else
                    if gattr.typ^.kind = skpointer then
                    begin
                      load;
                      genopreg(oppop, rgedx);
                      genopreg(oppop, rgeax);
                      genopregreg(opcmp, rgeax, rgedx);
                      case lop of
                        tkequal    : genopreg(opsete, rgal);
                        tknotequ   : genopreg(opsetne, rgal);
                        tklower    : genopreg(opsetb, rgal);
                        tklowequ   : genopreg(opsetbe, rgal);
                        tkgreater  : genopreg(opseta, rgal);
                        tkgreatequ : genopreg(opsetae, rgal);
                      end;
                      genopreg(oppush, rgeax);
                    end else
                      if lattr.typ^.kind = skarray then
                      begin
                        loadaddress;
                        genopregimm(opmov, rgecx, lattr.typ^.size, 4);
                        genopreg(oppop, rgesi);
                        genopreg(oppop, rgedi);
                        genop(opcld);
                        case lop of
                          tkequal  : genop(oprepe);
                          tknotequ : genop(oprepne);
                        else
                          fatal('operator not applicable');
                        end;
                        genop(opcmpsb);
                        case lop of
                          tkequal  : genopreg(opsete, rgal);
                          tknotequ : genopreg(opsetne, rgal);
                        end;
                        genopreg(oppush, rgeax);
                        gattr.kind := akexpr;
                      end else
                        fatal('not yet implemented');
          end;
        end;
        gattr.kind := akexpr;
        gattr.typ := boolptr;
      end;
    end;  (* expression *)

    function getparmsize: integer;
    var
      parm: pident;
      size: integer;
    begin
      parm := func^.fparams;
      size := 0;
      while parm <> nil do
      begin
        align(size, parm^.typ);
        parm := parm^.next;
      end;
      getparmsize := size;
    end;  (* getparmsize *)

  procedure block(funcid: pident);
  var
    lfunc: pident;
  begin (* block *)
    lfunc := func;
    func := funcid;

    (* reserve local storage to function's result *)
    if func^.typ <> nil then
    begin
      new(ident);
      with ident^ do
      begin
        next := nil;
        name := 'result          ';
        typ := func^.typ;
        kind := ikvar;
        vaccess := vadirect;
        vlev := level;
        allocvar(vaddr, typ);
      end;
      enterid(ident);
    end;

    (* declarations part *)
    cstexpr := true;
    while token in decltokens do
    begin
      case token of
      tklabel     : begin loadtoken; labeldeclaration end;
      tkconst     : begin loadtoken; constdeclaration end;
      tktype      : begin loadtoken; typedeclaration end;
      tkvar       : begin loadtoken; vardeclaration end;
      tkfunction  : begin loadtoken; funcdeclaration(true) end;
      tkprocedure : begin loadtoken; funcdeclaration(false) end;
      end
    end;

    (* generate function's entry *)
    emitcomment('; -- ' + func^.name);
    putlabel(func^.faddr);
    if func^.fkind = fkdeclared then genfuncentry(func^.flocals, level);
    genlabel(exitlbl);       (* every all blocks have an exit point *)
    breaklbl := 0;
    nextlbl := 0;

    (* statements part *)
    cstexpr := false;
    expect(tkbegin);
    repeat
      while token in stattokens do statement;
      done := token <> tksemicolon;
      if not done then loadtoken
    until done;
    expect(tkend);

    (* generate function's return *)
    putlabel(exitlbl);
    if func^.fkind = fkdeclared then
    begin
      if func^.typ <> nil then genopregmem(opmov, rgeax, rgebp, rgnone, 4, 0, 0, vanone);
      genop(opleave);
    end;
    if (func^.fparams = nil) or (func^.fconv in [cccdecl, ccvarargs]) then genop(opret)
    else begin
      genopimm(opret, getparmsize, 4);
    end;

    optimize;
    emitcode; freecode;

    (* check labels definition *)
    lbl := display[top].labels;
    while lbl <> nil do
    begin
      if not lbl^.defined then
        error(format('label ''%d'' undefined', [lbl^.name]));
      lbl := lbl^.link;
    end;

    (* check forward functions definition *)
    ident := display[top].idents;
    while ident <> nil do
    begin
      if ident^.kind = ikfunc then
        if ident^.fkind = fkdeclared then
          if ident^.forw then
            error(format('function ''%s'' undefined', [ident^.name]));
      ident := ident^.link;
    end;

    func := lfunc;
  end;  (* block *)

  procedure usessection;
  var
    publics: pident;
    done: boolean;

    procedure parseunit;
    var
      ident: pident;
    begin
      expect(tkunit);
      expect(tkident);
      new(ident);
      ident^.next := nil;
      ident^.name := strnew(@id);
      ident^.typ := nil;
      ident^.kind := ikunit;
      expect(tksemicolon);

      expect(tkinterface);

      if token = tkuses then begin loadtoken; usessection end;

      if top >= maxtop then fatal('display overflow');
      inc(top);
      display[top].labels := nil;
      display[top].idents := nil;
      display[top].kind := dkblock;
      enterid(ident);

      frwall := true;
      cstexpr := true;
      while token in decltokens do
      begin
        case token of
        tklabel     : fatal('label not allowed in interface section');
        tkconst     : begin loadtoken; constdeclaration end;
        tktype      : begin loadtoken; typedeclaration end;
        tkvar       : begin loadtoken; vardeclaration end;
        tkfunction  : begin loadtoken; funcdeclaration(true) end;
        tkprocedure : begin loadtoken; funcdeclaration(false) end;
        end
      end;
      frwall := false;

      publics := display[top].idents;

      expect(tkimplementation);

      if token = tkuses then begin loadtoken; usessection end;

      while token in decltokens do
      begin
        case token of
        tklabel     : fatal('label not allowed in implementation section');
        tkconst     : begin loadtoken; constdeclaration end;
        tktype      : begin loadtoken; typedeclaration end;
        tkvar       : begin loadtoken; vardeclaration end;
        tkfunction  : begin loadtoken; funcdeclaration(true) end;
        tkprocedure : begin loadtoken; funcdeclaration(false) end;
        end
      end;

      (* check forward functions definition *)
      ident := display[top].idents;
      while ident <> nil do
      begin
        if ident^.kind = ikfunc then
          if ident^.fkind = fkdeclared then
            if ident^.forw then
              error(format('function ''%s'' undefined', [ident^.name]));
        ident := ident^.link;
      end;

      cstexpr := false;

      if token = tkinitialization then
      begin
      end;

      if token = tkfinalization then
      begin
      end;

      expect(tkend);
      expect(tkperiod);

      display[top].idents := publics;
    end;  (* parseunit *)

  begin
    repeat
      check(tkident);
      if loadunit(pchar(@id)) then
      begin
        loadtoken;
        parseunit;
      end;
      done := token <> tkcomma;
      if not done then loadtoken;
    until done;
    expect(tksemicolon);
  end;  (* usessection *)

  (* put constant in order of allocation *)
  procedure revertconst;
  var
    p, nxt, last: pidata;
  begin
    last := nil;
    nxt := nil;
    p := constants;
    while p <> nil do
    begin
      last := p;
      p := p^.link;
      last^.link := nxt;
      nxt := last;
    end;
    constants := last;
  end;  (* revertconst *)

begin
  emithead;
  expect(tkprogram);
  expect(tkident);
  new(mainfunc);
  with mainfunc^ do
  begin
    next := nil;
    name := strnew(@id);
    typ := nil;
    kind := ikfunc;
    fkind := fkmain;
    genlabel(faddr);
    fparams := nil;
    flocals := 0;
    fconv := ccpascal;
    forw := false;
  end;
  func := mainfunc;
  expect(tksemicolon);

  if token = tkuses then begin loadtoken; usessection end;

  if top >= maxtop then fatal('display overflow');
  inc(top);
  display[top].labels := nil;
  display[top].idents := nil;
  display[top].kind := dkblock;
  enterid(mainfunc);
  block(mainfunc);
  expect(tkperiod);
  emitimport;
  if cstsize <> 0 then
  begin
    emitconsthead;
    revertconst;
    p := constants;
    while p <> nil do
    begin
      emitconst(p^.val.sval, p^.typ^.size);
      p := p^.link;
    end;
  end;
  emitvar(mainfunc^.flocals);
  emitreloc;
  if token <> tkeof then warning('text after end ignored')
end;  (* parse *)

procedure initparser;

  procedure initstdtypes;
  begin
    new(intptr);
    with intptr^ do
    begin
      size := 4;
      kind := skordinal;
      declared := false;
    end;

    new(charptr);
    with charptr^ do
    begin
      size := 1;
      kind := skordinal;
      declared := false;
    end;

    new(boolptr);
    with boolptr^ do
    begin
      size := 1;
      kind := skordinal;
      declared := true;
    end;

    new(nilptr);
    with nilptr^ do
    begin
      size := 4;
      kind := skpointer;
      basetyp := nil;
    end;
  end;  // initstdtypes

  procedure initstdidents;
  var
    ident, nxt: pident;
  begin
    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('integer');
      typ := intptr;
      kind := iktype;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('char');
      typ := charptr;
      kind := iktype;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('boolean');
      typ := boolptr;
      kind := iktype;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('pointer');
      typ := nilptr;
      kind := iktype;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('false');
      typ := boolptr;
      kind := ikconst;
      cval.ival := 0;
    end;
    nxt := ident;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nxt;
      name := strnew('true');
      typ := boolptr;
      kind := ikconst;
      cval.ival := 1;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('nil');
      typ := nilptr;
      kind := ikconst;
      cval.ival := 0;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('ord');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysordindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('chr');
      typ := charptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syschrindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('pred');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syspredindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('succ');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syssuccindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('low');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syslowindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('high');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syshighindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('sizeof');
      typ := intptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syssizeofindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('inc');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysincindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('dec');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysdecindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('write');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syswriteindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('writeln');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syswritelnindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('read');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysreadindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('eof');
      typ := boolptr;
      kind := ikfunc;
      fkind := fksystem;
      faddr := syseofindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('getmem');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysgetmemindex;
    end;
    enterid(ident);

    new(ident);
    with ident^ do
    begin
      next := nil;
      name := strnew('freemem');
      typ := nil;
      kind := ikfunc;
      fkind := fksystem;
      faddr := sysfreememindex;
    end;
    enterid(ident);
  end;  // initstdidents

begin
  nextlabel := 0;
  dataal := 4;
  notifysearcherr := true;
  constants := nil;
  cstsize := 0;
  frwtyp := nil;
  frwall := false;
  level := 0;
  top := 0;
  display[0].labels := nil;
  display[0].idents := nil;
  display[0].kind := dkblock;
  initstdtypes;
  initstdidents;
end;

procedure doneparser;
begin
end;

end.
