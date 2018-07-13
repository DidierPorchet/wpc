(******************************************************************************
 *  PASCAL compiler                                                           *
 *  asm writer (FASM format)                                                  *
 *  Didier Porchet, 2018                                                      *
 ******************************************************************************)

unit outfasm;

interface

procedure emitcode;
procedure emitcomment(const comment: string);
procedure emithead;
procedure emitimport;
procedure emitconsthead;
procedure emitconst(data: pointer; size: integer);
procedure emitvar(datasize: integer);
procedure emitreloc;
procedure initoutfasm(const filename: string);
procedure doneoutfasm;

implementation

uses
  sysutils, cmessage, parser, codegen;

var
  outfile: text;

const
  regstr : array[tregister] of string = (
               '',
               'al',  'bl',  'cl',  'dl',
               'ah',  'bh',  'ch',  'dh',
               'ax',  'bx',  'cx',  'dx',  'si',  'di',  'bp',  'sp',
               'eax', 'ebx', 'ecx', 'edx', 'esi', 'edi', 'ebp', 'esp'
  );

  syslabels : array[1..9] of string = (
               'writel',
               'writec',
               'writei',
               'writed',
               'readc',
               'eof',
               'writebuf',
               'getmem',
               'freemem'
  );

procedure emitcode;

  function memopstr(op: toperand): string;
  var
    first: boolean;

    function signstr(value: integer): string;
    begin
      if value < 0 then result := ' - ' else result := ' + ';
    end;  (* signstr *)

  begin
    first := true;

    (* size prefix *)
    case op.size of
    1 : result := 'byte [';
    2 : result := 'word [';
    4 : result := 'dword [';
    end;

    (* base register *)
    if op.mem.base <> rgnone then
    begin
      result := result + regstr[op.mem.base];
      first := false;
    end;

    (* index register *)
    if op.mem.index <> rgnone then
    begin
      if not first then result := result + ' + ';
      result := result + regstr[op.mem.index];
      first := false;
    end;

    (* scale factor *)
    if op.mem.scale <> 0 then
    begin
      if not first then result := result + ' * ';
      result := result + inttostr(op.mem.scale);
      first := false;
    end;

    (* virtual address prefix *)
    case op.mem.vaddr of
    vadata  :
      begin
        if not first then result := result + ' + ';
        result := result + 'gdata';
        first := false;
      end;

    vaconst :
      begin
        if not first then result := result + ' + ';
        result := result + 'cdata';
        first := false;
      end;

    vacode  :
      begin
        if not first then result := result + ' + ';
        result := result + 'l';
        first := false;
      end;
    end;

    (* offset *)
    if op.mem.offset <> 0 then
    begin
      if (not first) and (op.mem.vaddr <> vacode) then result := result + signstr(op.mem.offset);
      result := result + inttostr(abs(op.mem.offset));
    end;

    result := result + ']';
  end;

  function operandstr(op: toperand): string;
  begin
    case op.kind of
    okimmediate : result := inttostr(op.ival);
    okregister  : result := regstr[op.reg];
    okmemory    : result := memopstr(op);
    end;
  end;

begin
  if code <> nil then
  begin
    while code^.prev <> nil do code := code^.prev;
    while code <> nil do
    begin
      with code^ do
      begin
        case opcode of
        oplabel   : writeln(outfile, 'l', op1.ival, ':');
        opline    : writeln(outfile, ';  ', op1.ival);
        opjmp     : writeln(outfile, '':20, 'jmp l', op1.ival);
        opsyscall : writeln(outfile, '':20, 'call ', syslabels[op1.ival]);
        opcall    : writeln(outfile, '':20, 'call l', op1.ival);
        opret     : if op1.kind = oknone then writeln(outfile, '':20, 'ret') else writeln(outfile, '':20, 'ret ', op1.ival);
        openter   : writeln(outfile, '':20, 'enter ', op1.ival, ',', op2.ival);
        opleave   : writeln(outfile, '':20, 'leave');
        oppush    : writeln(outfile, '':20, 'push ', operandstr(op1));
        oppop     : writeln(outfile, '':20, 'pop ', operandstr(op1));
        opnot     : writeln(outfile, '':20, 'not ', operandstr(op1));
        opneg     : writeln(outfile, '':20, 'neg ', operandstr(op1));
        opinc     : writeln(outfile, '':20, 'inc ', operandstr(op1));
        opdec     : writeln(outfile, '':20, 'dec ', operandstr(op1));
        opor      : writeln(outfile, '':20, 'or ', operandstr(op1), ',', operandstr(op2));
        opxor     : writeln(outfile, '':20, 'xor ', operandstr(op1), ',', operandstr(op2));
        opand     : writeln(outfile, '':20, 'and ', operandstr(op1), ',', operandstr(op2));
        opadd     : writeln(outfile, '':20, 'add ', operandstr(op1), ',', operandstr(op2));
        opsub     : writeln(outfile, '':20, 'sub ', operandstr(op1), ',', operandstr(op2));
        opcmp     : writeln(outfile, '':20, 'cmp ', operandstr(op1), ',', operandstr(op2));
        opmov     : writeln(outfile, '':20, 'mov ', operandstr(op1), ',', operandstr(op2));
        opmovsx   : writeln(outfile, '':20, 'movsx ', operandstr(op1), ',', operandstr(op2));
        opmovzx   : writeln(outfile, '':20, 'movzx ', operandstr(op1), ',', operandstr(op2));
        oplea     : writeln(outfile, '':20, 'lea ', operandstr(op1), ',', operandstr(op2));
        opimul    : writeln(outfile, '':20, 'imul ', operandstr(op1));
        opidiv    : writeln(outfile, '':20, 'idiv ', operandstr(op1));
        opsal     : writeln(outfile, '':20, 'sal ', operandstr(op1), ',', operandstr(op2));
        opsar     : writeln(outfile, '':20, 'sar ', operandstr(op1), ',', operandstr(op2));
        opshl     : writeln(outfile, '':20, 'shl ', operandstr(op1), ',', operandstr(op2));
        opshr     : writeln(outfile, '':20, 'shr ', operandstr(op1), ',', operandstr(op2));
        opcbw     : writeln(outfile, '':20, 'cbw');
        opcwde    : writeln(outfile, '':20, 'cwde');
        opcdq     : writeln(outfile, '':20, 'cdq');
        opja      : writeln(outfile, '':20, 'ja l', op1.ival);
        opjae     : writeln(outfile, '':20, 'jae l', op1.ival);
        opjb      : writeln(outfile, '':20, 'jb l', op1.ival);
        opjbe     : writeln(outfile, '':20, 'jbe l', op1.ival);
        opjc      : writeln(outfile, '':20, 'jc l', op1.ival);
        opjcxz    : writeln(outfile, '':20, 'jcxz l', op1.ival);
        opjecxz   : writeln(outfile, '':20, 'jecxz l', op1.ival);
        opje      : writeln(outfile, '':20, 'je l', op1.ival);
        opjz      : writeln(outfile, '':20, 'jz l', op1.ival);
        opjg      : writeln(outfile, '':20, 'jg l', op1.ival);
        opjge     : writeln(outfile, '':20, 'jge l', op1.ival);
        opjl      : writeln(outfile, '':20, 'jl l', op1.ival);
        opjle     : writeln(outfile, '':20, 'jle l', op1.ival);
        opjna     : writeln(outfile, '':20, 'jna l', op1.ival);
        opjnae    : writeln(outfile, '':20, 'jnae l', op1.ival);
        opjnb     : writeln(outfile, '':20, 'jnb l', op1.ival);
        opjnbe    : writeln(outfile, '':20, 'jnbe l', op1.ival);
        opjnc     : writeln(outfile, '':20, 'jnc l', op1.ival);
        opjne     : writeln(outfile, '':20, 'jne l', op1.ival);
        opjng     : writeln(outfile, '':20, 'jng l', op1.ival);
        opjnge    : writeln(outfile, '':20, 'jnge l', op1.ival);
        opjnl     : writeln(outfile, '':20, 'jnl l', op1.ival);
        opjnle    : writeln(outfile, '':20, 'jnle l', op1.ival);
        opjno     : writeln(outfile, '':20, 'jno l', op1.ival);
        opjnp     : writeln(outfile, '':20, 'jnp l', op1.ival);
        opjns     : writeln(outfile, '':20, 'jns l', op1.ival);
        opjnz     : writeln(outfile, '':20, 'jnz l', op1.ival);
        opjo      : writeln(outfile, '':20, 'jo l', op1.ival);
        opjp      : writeln(outfile, '':20, 'jp l', op1.ival);
        opjpe     : writeln(outfile, '':20, 'jpe l', op1.ival);
        opjpo     : writeln(outfile, '':20, 'jpo l', op1.ival);
        opjs      : writeln(outfile, '':20, 'js l', op1.ival);
        opseta    : writeln(outfile, '':20, 'seta ', operandstr(op1));
        opsetae   : writeln(outfile, '':20, 'setae ', operandstr(op1));
        opsetb    : writeln(outfile, '':20, 'setb ', operandstr(op1));
        opsetbe   : writeln(outfile, '':20, 'setbe ', operandstr(op1));
        opsetc    : writeln(outfile, '':20, 'setc ', operandstr(op1));
        opsete    : writeln(outfile, '':20, 'sete ', operandstr(op1));
        opsetg    : writeln(outfile, '':20, 'setg ', operandstr(op1));
        opsetge   : writeln(outfile, '':20, 'setge ', operandstr(op1));
        opsetl    : writeln(outfile, '':20, 'setl ', operandstr(op1));
        opsetle   : writeln(outfile, '':20, 'setle ', operandstr(op1));
        opsetna   : writeln(outfile, '':20, 'setna ', operandstr(op1));
        opsetnae  : writeln(outfile, '':20, 'setnae ', operandstr(op1));
        opsetnb   : writeln(outfile, '':20, 'setnb ', operandstr(op1));
        opsetnbe  : writeln(outfile, '':20, 'setnbe ', operandstr(op1));
        opsetnc   : writeln(outfile, '':20, 'setnc ', operandstr(op1));
        opsetne   : writeln(outfile, '':20, 'setne ', operandstr(op1));
        opsetng   : writeln(outfile, '':20, 'setng ', operandstr(op1));
        opsetnge  : writeln(outfile, '':20, 'setnge ', operandstr(op1));
        opsetnl   : writeln(outfile, '':20, 'setnl ', operandstr(op1));
        opsetnle  : writeln(outfile, '':20, 'setnle ', operandstr(op1));
        opsetno   : writeln(outfile, '':20, 'setno ', operandstr(op1));
        opsetnp   : writeln(outfile, '':20, 'setnp ', operandstr(op1));
        opsetns   : writeln(outfile, '':20, 'setns ', operandstr(op1));
        opsetnz   : writeln(outfile, '':20, 'setnz ', operandstr(op1));
        opseto    : writeln(outfile, '':20, 'seto ', operandstr(op1));
        opsetp    : writeln(outfile, '':20, 'setp ', operandstr(op1));
        opsetpe   : writeln(outfile, '':20, 'setpe ', operandstr(op1));
        opsetpo   : writeln(outfile, '':20, 'setpo ', operandstr(op1));
        opsets    : writeln(outfile, '':20, 'sets ', operandstr(op1));
        opsetz    : writeln(outfile, '':20, 'setz ', operandstr(op1));
        opcld     : writeln(outfile, '':20, 'cld');
        oprep     : writeln(outfile, '':20, 'rep');
        oprepe    : writeln(outfile, '':20, 'repe');
        oprepne   : writeln(outfile, '':20, 'repne');
        opmovsb   : writeln(outfile, '':20, 'movsb');
        opcmpsb   : writeln(outfile, '':20, 'cmpsb');
        opbt      : writeln(outfile, '':20, 'bt ', operandstr(op1),',', operandstr(op2));
        else
          fatal('unknow opcode');
        end
      end;
      code := code^.next;
    end
  end
end;  (* emitcode *)

procedure emitcomment;
begin
  writeln(outfile, comment);
end;  (* emitcomment *)

(* write in outfile compliant header and run time library *)
procedure emithead;
begin
  writeln(outfile, 'format PE console');
  writeln(outfile, 'entry start');
  writeln(outfile);
  writeln(outfile, 'section ''.text'' code readable executable');
  writeln(outfile);
  writeln(outfile, '; == run time library ==========================================================');
  writeln(outfile);
  writeln(outfile, 'start:',    '':14, 'push -11');
  writeln(outfile,              '':20, 'call [GetStdHandle]');
  writeln(outfile,              '':20, 'mov [conout], eax');
  writeln(outfile,              '':20, 'push dword -10');
  writeln(outfile,              '':20, 'call [GetStdHandle]');
  writeln(outfile,              '':20, 'mov [conin], eax');
  writeln(outfile,              '':20, 'mov [rdeof],0');
  writeln(outfile,              '':20, 'call [GetProcessHeap]');
  writeln(outfile,              '':20, 'mov [hheap], eax'); 
  writeln(outfile,              '':20, 'jmp main');
  writeln(outfile, 'writebuf:', '':11, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'push 0');
  writeln(outfile,              '':20, 'push wrcnt');
  writeln(outfile,              '':20, 'push dword [ebp + 8]');
  writeln(outfile,              '':20, 'push dword [ebp + 12]');
  writeln(outfile,              '':20, 'push [conout]');
  writeln(outfile,              '':20, 'call [WriteFile]');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 8');
  writeln(outfile, 'writel:',   '':13, 'mov [wrbuf],13');
  writeln(outfile,              '':20, 'mov [wrbuf + 1],10');
  writeln(outfile,              '':20, 'push wrbuf');
  writeln(outfile,              '':20, 'push 2');
  writeln(outfile,              '':20, 'call writebuf');
  writeln(outfile,              '':20, 'ret');
  writeln(outfile, 'writec:',   '':13, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'mov al,[ebp + 8]');
  writeln(outfile,              '':20, 'mov [wrbuf],al');
  writeln(outfile,              '':20, 'push wrbuf');
  writeln(outfile,              '':20, 'push 1');
  writeln(outfile,              '':20, 'call writebuf');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'writei:',   '':13, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'mov eax,[ebp + 8]');
  writeln(outfile,              '':20, 'cmp eax,0');
  writeln(outfile,              '':20, 'jne .l1');
  writeln(outfile,              '':20, 'push ''0''');
  writeln(outfile,              '':20, 'call writec');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, ' .l1:',     '':15, 'cmp eax,0');
  writeln(outfile,              '':20, 'jg .l2');
  writeln(outfile,              '':20, 'push ''-''');
  writeln(outfile,              '':20, 'call writec');
  writeln(outfile,              '':20, 'mov eax,[ebp + 8]');
  writeln(outfile,              '':20, 'neg eax');
  writeln(outfile, ' .l2:',     '':15, 'xor ecx,ecx');
  writeln(outfile,              '':20, 'mov edi,wrbuf + 63');
  writeln(outfile,              '':20, 'mov ebx,10');
  writeln(outfile, ' .l3:',     '':15, 'cmp eax,0');
  writeln(outfile,              '':20, 'je .l4');
  writeln(outfile,              '':20, 'xor edx,edx');
  writeln(outfile,              '':20, 'idiv ebx');
  writeln(outfile,              '':20, 'add dl,''0''');
  writeln(outfile,              '':20, 'mov [edi],dl');
  writeln(outfile,              '':20, 'inc ecx');
  writeln(outfile,              '':20, 'dec edi');
  writeln(outfile,              '':20, 'jmp .l3');
  writeln(outfile, ' .l4:',     '':15, 'inc edi');
  writeln(outfile,              '':20, 'push edi');
  writeln(outfile,              '':20, 'push ecx');
  writeln(outfile,              '':20, 'call writebuf');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'writed:',   '':13, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'mov eax,[ebp + 8]');
  writeln(outfile,              '':20, 'cmp eax,0');
  writeln(outfile,              '':20, 'jne .l5');
  writeln(outfile,              '':20, 'push ''0''');
  writeln(outfile,              '':20, 'call writec');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, ' .l5:',     '':15, 'xor ecx,ecx');
  writeln(outfile,              '':20, 'mov edi,wrbuf + 63');
  writeln(outfile,              '':20, 'mov ebx,10');
  writeln(outfile, ' .l6:',     '':15, 'cmp eax,0');
  writeln(outfile,              '':20, 'je .l7');
  writeln(outfile,              '':20, 'xor edx,edx');
  writeln(outfile,              '':20, 'div ebx');
  writeln(outfile,              '':20, 'add dl,''0''');
  writeln(outfile,              '':20, 'mov [edi],dl');
  writeln(outfile,              '':20, 'inc ecx');
  writeln(outfile,              '':20, 'dec edi');
  writeln(outfile,              '':20, 'jmp .l6');
  writeln(outfile, ' .l7:',     '':15, 'inc edi');
  writeln(outfile,              '':20, 'push edi');
  writeln(outfile,              '':20, 'push ecx');
  writeln(outfile,              '':20, 'call writebuf');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'readc:',    '':14, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'push 0');
  writeln(outfile,              '':20, 'push rdcnt');
  writeln(outfile,              '':20, 'push 1');
  writeln(outfile,              '':20, 'push dword [ebp + 8]');
  writeln(outfile,              '':20, 'push [conin]');
  writeln(outfile,              '':20, 'call [ReadFile]');
  writeln(outfile,              '':20, 'cmp [rdcnt],0');
  writeln(outfile,              '':20, 'setz [rdeof]');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'eof:',      '':16, 'mov al, [rdeof]');
  writeln(outfile,              '':20, 'ret');
  writeln(outfile, 'getmem:',   '':13, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'push dword [ebp + 8]');
  writeln(outfile,              '':20, 'push 0');
  writeln(outfile,              '':20, 'push [hheap]');
  writeln(outfile,              '':20, 'call [HeapAlloc]');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'freemem:',  '':12, 'push ebp');
  writeln(outfile,              '':20, 'mov ebp,esp');
  writeln(outfile,              '':20, 'push dword [ebp + 8]');
  writeln(outfile,              '':20, 'push 0');
  writeln(outfile,              '':20, 'push [hheap]');
  writeln(outfile,              '':20, 'call [HeapFree]');
  writeln(outfile,              '':20, 'pop ebp');
  writeln(outfile,              '':20, 'ret 4');
  writeln(outfile, 'halt:',     '':15, 'mov eax,[esp+4]');
  writeln(outfile,              '':20, 'push eax');
  writeln(outfile,              '':20, 'call [ExitProcess]');
  writeln(outfile, 'main:',     '':15, 'call l1');
  writeln(outfile,              '':20, 'push 0');
  writeln(outfile,              '':20, 'call halt');
  writeln(outfile);
  writeln(outfile, '; == code ======================================================================');
  writeln(outfile);
end;  (* emithead *)

(* write in outfile import section *)
procedure emitimport;
begin
  writeln(outfile);
  writeln(outfile, '; == import ====================================================================');
  writeln(outfile);
  writeln(outfile, 'section ''.idata'' import data readable writeable');
  writeln(outfile);
  writeln(outfile, '  dd 0,0,0,RVA kernel_name,RVA kernel_table');
  writeln(outfile, '  dd 0,0,0,0,0');
  writeln(outfile);
  writeln(outfile, '  kernel_table:');
  writeln(outfile, '    ExitProcess dd RVA _ExitProcess');
  writeln(outfile, '    GetStdHandle dd RVA _GetStdHandle');
  writeln(outfile, '    WriteFile dd RVA _WriteFile');
  writeln(outfile, '    ReadFile dd RVA _ReadFile');
  writeln(outfile, '    GetProcessHeap dd RVA _GetProcessHeap');
  writeln(outfile, '    HeapAlloc dd RVA _HeapAlloc');
  writeln(outfile, '    HeapReAlloc dd RVA _HeapReAlloc');
  writeln(outfile, '    HeapFree dd RVA _HeapFree');
  writeln(outfile, '    dd 0');
  writeln(outfile);
  writeln(outfile, '  kernel_name db ''KERNEL32.DLL'',0');
  writeln(outfile);
  writeln(outfile, '  _ExitProcess dw 0');
  writeln(outfile, '    db ''ExitProcess'',0');
  writeln(outfile, '  _GetStdHandle dw 0');
  writeln(outfile, '    db ''GetStdHandle'',0');
  writeln(outfile, '  _WriteFile dw 0');
  writeln(outfile, '    db ''WriteFile'',0');
  writeln(outfile, '  _ReadFile dw 0');
  writeln(outfile, '    db ''ReadFile'',0');
  writeln(outfile, '  _GetProcessHeap dw 0');
  writeln(outfile, '    db ''GetProcessHeap'',0');
  writeln(outfile, '  _HeapAlloc dw 0');
  writeln(outfile, '    db ''HeapAlloc'',0');
  writeln(outfile, '  _HeapReAlloc dw 0');
  writeln(outfile, '    db ''HeapReAlloc'',0');
  writeln(outfile, '  _HeapFree dw 0');
  writeln(outfile, '    db ''HeapFree'',0');
end;  (* emitimport *)

(* write in outfile const section *)
procedure emitconsthead;
begin
  writeln(outfile);
  writeln(outfile, '; == const =====================================================================');
  writeln(outfile);
  writeln(outfile, 'section ''.data'' data readable writeable');
  writeln(outfile);
  writeln(outfile, '  cdata:');
end;  (* emitconsthead *)

(* write in outfile one initialized data *)
procedure emitconst(data: pointer; size: integer);
var
  p: pbyte;
begin
  write(outfile, '            db ');
  p := data;
  while size <> 0 do
  begin
    write(outfile, p^);
    inc(p);
    dec(size);
    if size <> 0 then write(outfile, ',');
  end;
  writeln(outfile);
end;  (* emitconst *)

(* write in outfile data section and import section *)
procedure emitvar(datasize: integer);
begin
  writeln(outfile);
  writeln(outfile, '; == var =======================================================================');
  writeln(outfile);
  writeln(outfile, 'section ''.bss'' data readable writeable');
  writeln(outfile);
  writeln(outfile, '  conin     dd ?');
  writeln(outfile, '  conout    dd ?');
  writeln(outfile, '  wrcnt     dd ?');
  writeln(outfile, '  wrbuf     db 256 dup ?');
  writeln(outfile, '  rdcnt     dd ?');
  writeln(outfile, '  rdbuf     db 256 dup ?');
  writeln(outfile, '  rdeof     db ?,?,?,?');
  writeln(outfile, '  hheap     dd ?');
  if datasize <> 0 then
  begin
    writeln(outfile, '  gdata:');
    writeln(outfile, '            db ', datasize, ' dup ?');
  end;
end;  (* emitvar *)

procedure emitreloc;
begin
  writeln(outfile);
  writeln(outfile, 'section ''.reloc'' fixups data readable discardable');
end;  (* emitreloc *)

procedure initoutfasm(const filename: string);
begin
  assign(outfile, filename);
  rewrite(outfile);
end;  (* initoutfasm *)

procedure doneoutfasm;
begin
  close(outfile);
end;  (* doneoutfasm *)

end.
