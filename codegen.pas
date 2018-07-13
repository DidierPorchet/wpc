(******************************************************************************
 *  PASCAL compiler                                                           *
 *  code generator                                                            *
 *  x86 code generation and optimization                                      *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit codegen;

interface

type
  topcode = (
    (* i386 opcodes *)
    opaaa, opaad, opaam, opaas, opadc, opadd, opand, oparpl, opbound, opbsf,
    opbsr, opbt, opbtr, opbts, opcall, opcbw, opcwde, opclc, opcld, opcli,
    opclts, opcmc, opcmp, opcmpsb, opcmpsw, opcmpsd, opcwd, opcdq, opdaa, opdas, opdec, opdiv,
    openter, ophlt, opidiv, opimul, opin, opinc, opins, opint, opinto, opiret,
    opiretd, opja, opjae, opjb, opjbe, opjc, opjcxz, opjecxz, opje, opjg,
    opjge, opjl, opjle, opjna, opjnae, opjnb, opjnbe, opjnc, opjne, opjng,
    opjnge, opjnl, opjnle, opjno, opjnp, opjns, opjnz, opjo, opjp, opjpe,
    opjpo, opjs, opjz, opjmp, oplahf, oplar, oplea, opleave, oplgdt, oplidt,
    oplss, oplds, oples, oplfs, oplgs, oplldt, oplmsw, oplock, oplods, oploop,
    oploope, oploopz, oploopne, oploopnz, oplsl, opltr, opmov, opmovsb, opmovsw, opmovsd, opmovsx,
    opmovzx, opmul, opneg, opnop, opnot, opor, opout, opouts, oppop, oppopa,
    oppopad, oppopf, oppopfd, oppush, oppusha, oppushad, oppushf, oppushfd,
    oprcl, oprcr, oprol, opror, oprep, oprepe, oprepz, oprepne, oprepnz, opret,
    opsahf, opsal, opsar, opshl, opshr, opsbb, opscas, opseta, opsetae, opsetb,
    opsetbe, opsetc, opsete, opsetg, opsetge, opsetl, opsetle, opsetna,
    opsetnae, opsetnb, opsetnbe, opsetnc, opsetne, opsetng, opsetnge, opsetnl,
    opsetnle, opsetno, opsetnp, opsetns, opsetnz, opseto, opsetp, opsetpe,
    opsetpo, opsets, opsetz, opsgdt, opsidt, opshld, opshrd, opsldt, opsmsw,
    opstc, opstd, opsti, opstos, opstr, opsub, optest, opverr, opverw, opwait,
    opxchg, opxlat, opxor,

    (* i387 opcodes *)

    (* additional opcodes *)
    oplabel, opline, opsyscall
  );

  (* operand kind *)
  toperandkind = (oknone, okimmediate, okregister, okmemory);

  (* registers *)
  tregister = (rgnone,
               rgal,  rgbl,  rgcl,  rgdl,
               rgah,  rgbh,  rgch,  rgdh,
               rgax,  rgbx,  rgcx,  rgdx,  rgsi,  rgdi,  rgbp,  rgsp,
               rgeax, rgebx, rgecx, rgedx, rgesi, rgedi, rgebp, rgesp);

  tvirtualaddress = (vanone, vadata, vaconst, vacode);

  tmemoperand = record
    vaddr: tvirtualaddress;
    base: tregister;
    index: tregister;
    offset: integer;
    scale: integer;
  end;

  toperand = record
    size: integer;     (* byte, word, dword *)
    case kind: toperandkind of
    okimmediate : (ival: integer);     (* immediate, displacement *)
    okregister  : (reg: tregister);    (* direct register *)
    okmemory    : (mem: tmemoperand);  (* memory operand *)
  end;

  pcode = ^tcode;
  tcode = record
    prev, next: pcode;
    opcode: topcode;
    op1, op2: toperand;
  end;

var
  code: pcode;

procedure putlabel(lbl: integer);
procedure genjump(lbl: integer);
procedure genfalsejump(lbl: integer);
procedure gentruejump(lbl: integer);
procedure genfuncentry(locals, level: integer);
procedure genfuncreturn(params: integer);
procedure genlinenumber(line: integer);
procedure optimize;

procedure genop(opc: topcode);
procedure genopimm(opc: topcode; imm, siz: integer);
procedure genopreg(opc: topcode; reg: tregister);
procedure genopmem(opc: topcode; base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
procedure genopregreg(opc: topcode; reg1, reg2: tregister);
procedure genopregimm(opc: topcode; reg: tregister; imm, siz: integer);
procedure genopregmem(opc: topcode; reg, base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
procedure genopmemreg(opc: topcode; reg, base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
procedure genopmemimm(opc: topcode; base, index: tregister; imm, siz, size, scale, offset: integer; vofs: tvirtualaddress);
procedure deletelastopcode;

procedure freecode;

procedure initcodegen;
procedure donecodegen;

implementation

uses
  sysutils, cglobals, cmessage;

type
  pmemblock = ^tmemblock;
  tmemblock = record
    link: pmemblock;
    base: pointer;
    freeptr: pointer;
    freesiz: cardinal;
  end;

var
  heap: pmemblock;

procedure allocblock;
var
  p: pmemblock;
begin
  p := heap;
  new(heap);
  with heap^ do
  begin
    link := p;
    getmem(base, 64*1024);
    freeptr := base;
    freesiz := 64*1024;
  end;
end;  (* allocblock *)

function alloc(size: cardinal): pointer;
begin
  if size > heap^.freesiz then allocblock;
  with heap^ do
  begin
    result := heap^.freeptr;
    inc(cardinal(freeptr), size);
    dec(freesiz, size);
  end;
end;  (* alloc *)

function sizefromreg(reg: tregister): integer;
begin
  if reg in [rgal..rgdh] then result := 1
  else
    if reg in [rgax..rgsp] then result := 2
    else
      result := 4
end;  (* sizefromreg *)

procedure entercode(acode: pcode);
begin
  acode^.next := nil;
  if code = nil then acode^.prev := nil
  else begin
    while code^.next <> nil do code := code^.next;
    code^.next := acode;
    acode^.prev := code;
  end;
  code := acode;
end;  (* entercode *)

procedure genfuncentry(locals, level: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := openter;
    op1.kind := okimmediate;
    op1.ival := locals;
    op2.kind := okimmediate;
    op2.ival := level;
  end;
  entercode(acode);
end;  (* genfuncentry *)

procedure genfuncreturn(params: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  acode^.opcode := opleave;
  entercode(acode);

  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opret;
    op1.kind := okimmediate;
    op1.ival := params;
  end;
  entercode(acode);
end;  (* genfuncreturn *)

procedure genlinenumber(line: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  acode^.opcode := opline;
  acode^.op1.ival := line;
  entercode(acode);
end;  (* genlinenumber *)

procedure genop(opc: topcode);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.kind := oknone;
    op2.kind := oknone;
  end;
  entercode(acode);
end;  (* genop *)

procedure genopimm(opc: topcode; imm, siz: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := siz;
    op1.kind := okimmediate;
    op1.ival := imm;
    op2.kind := oknone;
  end;
  entercode(acode);
end;  (* genopimm *)

procedure genopreg(opc: topcode; reg: tregister);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := sizefromreg(reg);
    op1.kind := okregister;
    op1.reg := reg;
    op2.kind := oknone;
  end;
  entercode(acode);
end;  (* genopreg *)

procedure genopmem(opc: topcode; base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := size;
    op1.kind := okmemory;
    op1.mem.vaddr := vofs;
    op1.mem.base := base;
    op1.mem.index := index;
    op1.mem.offset := offset;
    op1.mem.scale := scale;
    op2.kind := oknone;
  end;
  entercode(acode);
end;  (* genopmem *)

procedure genopregreg(opc: topcode; reg1, reg2: tregister);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := sizefromreg(reg1);
    op1.kind := okregister;
    op1.reg := reg1;
    op2.size := sizefromreg(reg2);
    op2.kind := okregister;
    op2.reg := reg2;
  end;
  entercode(acode);
end;  (* genopregreg *)

procedure genopregimm(opc: topcode; reg: tregister; imm, siz: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := sizefromreg(reg);
    op1.kind := okregister;
    op1.reg := reg;
    op2.size := siz;
    op2.kind := okimmediate;
    op2.ival := imm;
  end;
  entercode(acode);
end;  (* genopregimm *)

procedure genopregmem(opc: topcode; reg, base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := sizefromreg(reg);
    op1.kind := okregister;
    op1.reg := reg;
    op2.size := size;
    op2.kind := okmemory;
    op2.mem.vaddr := vofs;
    op2.mem.base := base;
    op2.mem.index := index;
    op2.mem.offset := offset;
    op2.mem.scale := scale;
  end;
  entercode(acode);
end;  (* genopregmem *)

procedure genopmemreg(opc: topcode; reg, base, index: tregister; size, scale, offset: integer; vofs: tvirtualaddress);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := size;
    op1.kind := okmemory;
    op1.mem.vaddr := vofs;
    op1.mem.base := base;
    op1.mem.index := index;
    op1.mem.offset := offset;
    op1.mem.scale := scale;
    op2.size := sizefromreg(reg);
    op2.kind := okregister;
    op2.reg := reg;
  end;
  entercode(acode);
end;  (* genopmemreg *)

procedure genopmemimm(opc: topcode; base, index: tregister; imm, siz, size, scale, offset: integer; vofs: tvirtualaddress);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opc;
    op1.size := size;
    op1.kind := okmemory;
    op1.mem.vaddr := vofs;
    op1.mem.base := base;
    op1.mem.index := index;
    op1.mem.offset := offset;
    op1.mem.scale := scale;
    op2.size := siz;
    op2.kind := okimmediate;
    op2.ival := imm;
  end;
  entercode(acode);
end;  (* genopmemimm *)

procedure putlabel(lbl: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := oplabel;
    op1.ival := lbl;
  end;
  entercode(acode)
end;  (* putlabel *)

procedure genjump(lbl: integer);
var
  acode: pcode;
begin
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opjmp;
    op1.kind := oknone;
    op1.ival := lbl;
  end;
  entercode(acode)
end;  (* genjump *)

procedure genfalsejump(lbl: integer);
var
  acode: pcode;
begin
  genopreg(oppop, rgeax);
  genopregreg(opor, rgal, rgal);
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opjz;
    op1.kind := oknone;
    op1.ival := lbl;
  end;
  entercode(acode)
end;  (* genfalsejump *)

procedure gentruejump(lbl: integer);
var
  acode: pcode;
begin
  genopreg(oppop, rgeax);
  genopregreg(opor, rgal, rgal);
  acode := alloc(sizeof(acode^));
  with acode^ do
  begin
    opcode := opjnz;
    op1.kind := oknone;
    op1.ival := lbl;
  end;
  entercode(acode)
end;  (* gentruejump *)

(* simply suppress last opcode *)
procedure deletelastopcode;
begin
  if code <> nil then
  begin
    code := code^.prev;
    if code <> nil then code^.next := nil;
  end;
end;  (* deletelastopcode *)

(* -- optimizations --------------------------------------------------------- *)

(*
    simplification of code allow suppressing in most cases 50% or more of
    instructions in final output. the basic rule is respect program meaning
    and can simply be achieved by voluntary break algorithm when code jump
    or recept from jump

*)

(* suppress push/pop sequence *)
procedure opt1;
begin
  if code = nil then exit;
  while code^.prev <> nil do code := code^.prev;
  repeat
    code := code^.next;
    if code^.opcode = oppop then
    begin
      if code^.prev <> nil then
        if code^.prev^.opcode = oppush then
        begin
          code^.opcode := opmov;
          code^.op2 := code^.prev^.op1;
          code^.prev := code^.prev^.prev;
          code^.prev^.next := code;

          if (code^.op1.kind = okregister) and (code^.op2.kind = okregister) and
             (code^.op1.reg = code^.op2.reg) then
          begin
            code := code^.prev;
            code^.next := code^.next^.next;
            code^.next^.prev := code;
          end;
        end;
    end;
  until code^.next = nil;
end;  (* opt1 *)

(* treat to suppress use of registers loaded with immediate value
   used in next source operand *)
procedure opt2;
var
  temp, codea: pcode;
  done: boolean;
begin
  if code = nil then exit;
  temp := code;
  while temp^.prev <> nil do temp := temp^.prev;
  while temp^.next <> nil do
  begin
    with temp^ do
    begin
      if opcode = opmov then
      begin
        if (op1.kind = okregister) and (op2.kind = okimmediate) then
        begin
          codea := temp;

          if codea = nil then exit;

          done := false;
          repeat
            codea := codea^.next;
            if (codea^.op1.kind = okregister) and
               (codea^.op1.reg = temp^.op1.reg) then done := true;

            if codea^.opcode in [oplabel, opline, opjmp, opja..opjz, opsyscall, opcall, opleave] then done := true;

            if not done and (codea^.op2.kind = okregister) then
              if codea^.op2.reg = temp^.op1.reg then
              begin
                codea^.op2 := temp^.op2;
                temp^.prev^.next := temp^.next;
                temp^.next^.prev := temp^.prev;
                if temp = code then code := temp^.next;
                done := true;
              end;
          until done;
        end;
      end;
    end;

    temp := temp^.next;
  end;
end;  (* opt2 *)

(* treat to suppress use of registers loaded with immediate values *)
procedure opt3;
var
  temp, codea, codeb: pcode;
  done: boolean;
begin
  if code = nil then exit;
  temp := code;
  while temp^.prev <> nil do temp := temp^.prev;
  while temp <> nil do
  begin
    if (temp^.opcode = opmov) and
       (temp^.op1.kind = okregister) and
       (temp^.op2.kind = okimmediate) then
    begin
      codea := temp;
      codeb := temp^.next;
      done := false;
      repeat
        if codeb = nil then exit;
        if codeb^.opcode in [oplabel, opline, opjmp, opja..opjz, opsyscall, opcall, opleave, opret] then done := true
        else begin
          if (codeb^.op1.kind = okregister) and (codeb^.op1.reg = codea^.op1.reg) then
          begin
            case codeb^.opcode of
            opneg :
              begin
                codeb^.opcode := opmov;
                codeb^.op1.ival := -codea^.op2.ival;
                codea^.prev^.next := codea^.next;
                codea^.next^.prev := codea^.prev;
              end;

            opnot :
              begin
                codeb^.opcode := opmov;
                codeb^.op1.ival := not codea^.op2.ival;
                codea^.prev^.next := codea^.next;
                codea^.next^.prev := codea^.prev;
              end;

            opadd :
              begin
                if codeb^.op2.kind = okimmediate then
                begin
                  codeb^.opcode := opmov;
                  codeb^.op2.ival := codea^.op2.ival + codeb^.op2.ival;
                  codea^.prev^.next := codea^.next;
                  codea^.next^.prev := codea^.prev;
                end;
              end;

            opsub :
              begin
                if codeb^.op2.kind = okimmediate then
                begin
                  codeb^.opcode := opmov;
                  codeb^.op2.ival := codea^.op2.ival - codeb^.op2.ival;
                  codea^.prev^.next := codea^.next;
                  codea^.next^.prev := codea^.prev;
                end;
              end;

            opand :
              begin
                if codeb^.op2.kind = okimmediate then
                begin
                  codeb^.opcode := opmov;
                  codeb^.op2.ival := codea^.op2.ival and codeb^.op2.ival;
                  codea^.prev^.next := codea^.next;
                  codea^.next^.prev := codea^.prev;
                end;
              end;

            opor :
              begin
                if codeb^.op2.kind = okimmediate then
                begin
                  codeb^.opcode := opmov;
                  codeb^.op2.ival := codea^.op2.ival or codeb^.op2.ival;
                  codea^.prev^.next := codea^.next;
                  codea^.next^.prev := codea^.prev;
                end;
              end;

            opxor :
              begin
                if codeb^.op2.kind = okimmediate then
                begin
                  codeb^.opcode := opmov;
                  codeb^.op2.ival := codea^.op2.ival xor codeb^.op2.ival;
                  codea^.prev^.next := codea^.next;
                  codea^.next^.prev := codea^.prev;
                end;
              end;

            end;
            done := true;
          end;
        end;
        if not done then codeb := codeb^.next;
      until done;
    end;

    temp := temp^.next;
  end;
end;  (* opt3 *)

(* replace sequence setxx al - or al,al - jz lbl by jnxx lbl *)
procedure opt4;
label 1;
var
  codea, codeb: pcode;
begin
  while code^.prev <> nil do code := code^.prev;

  codea := code;
  while codea <> nil do
  begin
    if codea^.opcode = opjz then
      if codea^.prev <> nil then
        if (codea^.prev^.opcode = opor) and
           (codea^.prev^.op1.kind = okregister) and
           (codea^.prev^.op2.kind = okregister) and
           (codea^.prev^.op1.reg = codea^.prev^.op2.reg) then
          if codea^.prev^.prev <> nil then
          begin
            codeb := codea^.prev^.prev;
            case codeb^.opcode of
            opseta   : codea^.opcode := opjna;
            opsetae  : codea^.opcode := opjnae;
            opsetb   : codea^.opcode := opjnb;
            opsetbe  : codea^.opcode := opjnbe;
            opsetc   : codea^.opcode := opjnc;
            opsete   : codea^.opcode := opjne;
            opsetg   : codea^.opcode := opjng;
            opsetge  : codea^.opcode := opjnge;
            opsetl   : codea^.opcode := opjnl;
            opsetle  : codea^.opcode := opjnle;
            opsetna  : codea^.opcode := opja;
            opsetnae : codea^.opcode := opjae;
            opsetnb  : codea^.opcode := opjb;
            opsetnbe : codea^.opcode := opjbe;
            opsetnc  : codea^.opcode := opjc;
            opsetne  : codea^.opcode := opje;
            opsetng  : codea^.opcode := opjg;
            opsetnge : codea^.opcode := opjge;
            opsetnl  : codea^.opcode := opjl;
            opsetnle : codea^.opcode := opjle;
            opsetno  : codea^.opcode := opjo;
            opsetnp  : codea^.opcode := opjp;
            opsetns  : codea^.opcode := opjs;
            opsetnz  : codea^.opcode := opjz;
            opseto   : codea^.opcode := opjno;
            opsetp   : codea^.opcode := opjnp;
//            opsetpe  : codea^.opcode := opjnpe;
//            opsetpo  : codea^.opcode := opjnpo;
            opsets   : codea^.opcode := opjns;
            opsetz   : codea^.opcode := opjnz;
            else
              goto 1
            end;

            codea^.prev := codeb^.prev;
            if codeb^.prev <> nil then codeb^.prev^.next := codea;
            code := codea;
          end;
  1:
    codea := codea^.next;
  end;
end;

(* suppress (replace with inc, dec) add and sub with immediate value 0, 1, -1 *)
procedure opt5;
var
  codea: pcode;
begin
  while code^.prev <> nil do code := code^.prev;

  codea := code;
  while codea <> nil do
  begin
    if (codea^.opcode = opadd) or (codea^.opcode = opsub) then
    begin
      if codea^.op2.kind = okimmediate then
        if codea^.op2.ival = 0 then
        begin
          if codea^.prev <> nil then codea^.prev^.next := codea^.next;
          if codea^.next <> nil then codea^.next^.prev := codea^.prev;
          if code = codea then
            if codea^.prev <> nil then code := codea^.prev
            else
              code := codea^.next;
        end else
          if codea^.op2.ival = 1 then
          begin
            if codea^.opcode = opadd then codea^.opcode := opinc
                                     else codea^.opcode := opdec;
          end else
            if codea^.op2.ival = -1 then
            begin
              if codea^.opcode = opadd then codea^.opcode := opdec
                                       else codea^.opcode := opinc;
            end;                           
    end;

    codea := codea^.next;
  end;
end;  (* opt5 *)

(* convert sequence mov reg,mem - push reg to push mem (must be 32 bits) *)
procedure opt6;
var
  codea, codeb: pcode;
begin
  while code^.prev <> nil do code := code^.prev;

  codea := code;
  while codea <> nil do
  begin
    if (codea^.opcode = oppush) and (codea^.op1.kind = okregister) then
      if codea^.prev <> nil then
      begin
        codeb := codea^.prev;
        if (codeb^.opcode = opmov) and (codeb^.op1.kind = okregister) then
          if codeb^.op1.reg = codea^.op1.reg then
            if (codeb^.op2.kind = okmemory) and (codeb^.op2.size = 4) then
            begin
              codea^.op1 := codeb^.op2;
              codea^.prev := codeb^.prev;
              if codea^.prev <> nil then codea^.prev^.next := codea;
              if code = codeb then code := codea;
            end;
      end;

    codea := codea^.next;
  end;
end;  (* opt6 *)

(* suppress mov reg,reg *)
procedure opt7;
var
  codea, codeb: pcode;
begin
  while code^.prev <> nil do code := code^.prev;

  codea := code;
  while codea <> nil do
  begin
    if (codea^.opcode = opmov) and (codea^.op1.kind = okregister) and
       (codea^.op2.kind = okregister) then
    begin
      codeb := codea^.next;
      if codeb <> nil then
      begin
        if (codeb^.op2.kind = okregister) and (codeb^.op2.reg = codea^.op1.reg) then
        begin
          codeb^.op2.reg := codea^.op2.reg;
          codeb^.prev := codea^.prev;
          if codea^.prev <> nil then codea^.prev^.next := codeb;
          if code = codea then code := codeb;
        end;
      end;
    end;
    codea := codea^.next;
  end;
end;  (* opt7 *)

(* optimize current code *)
procedure optimize;
begin
  opt1;
  opt2; opt1;
  opt3; opt2; opt1;
  opt4; opt5; opt6; opt7;
end;  (* optimize *)

procedure freecode;
var
  p: pmemblock;
begin
  while heap^.link <> nil do
  begin
    freemem(heap^.base);
    p := heap^.link;
    dispose(heap);
    heap := p;
  end;
  heap^.freeptr := heap^.base;
  heap^.freesiz := 64*1024;
  code := nil;
end;  (* freecode *)

procedure initcodegen;
begin
  heap := nil;
  allocblock;
  code := nil;
end;  (* initcodegen *)

procedure donecodegen;
var
  p: pmemblock;
begin
  while heap <> nil do
  begin
    freemem(heap^.base);
    p := heap^.link;
    dispose(heap);
    heap := p;
  end;
end;  (* donecodegen *)

end.
