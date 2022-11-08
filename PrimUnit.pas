unit PrimUnit;

interface

  function  _wswap(var s, d: word): word;  
  function  _incw(var acc: word; w: word): boolean;
  function  _deek(a: word): word;
  procedure _doke(a: word; d: word);
  function  _RelAdr(cell: word; nib: byte): word;
  function  _drop: word;
  procedure _dup(data: word);
  function  _pop: word;
  procedure _push(adr: word);
  procedure jump;
  procedure _if;
  procedure ifm;
  procedure rstp;
  procedure rldp;
  procedure xr;
  procedure xa;
  procedure push;
  procedure j;
  procedure dup;
  procedure pop;
  procedure nand;
  procedure add2div;
  procedure PMul;
  procedure SDiv;
  function  FindPrim(wrd: string): shortint;

implementation

uses
  TypeUnit,
  ConstUnit,
  VarUnit
  ;

  {
  function  FindPrim(wrd: string): shortint;
  var
    i: OpCodes;
  begin
    for i := nandOp downto jumpOp do
      if opNames[i] = wrd then  begin
          FindPrim := shortint(i);
          exit;
      end;
    FindPrim := -1;
  end;
  }

  {$asmMode intel}
  {
  function  _incw(var acc: word; w: word): boolean; begin
  asm
    mov eax,acc ;
    mov dx,w
    add [eax],dx
    mov al,0
    rcl al,1
  end; end;

  function _wswap(var s, d: word): word;  begin asm
    mov   edx,d
    mov   ecx,s
    mov   ax,[ecx]
    xchg  ax,[edx]
    xchg  ax,[ecx]
  end; end;


  function  _drop: word;
  begin
    _drop := _wswap(shiftreg[1], shiftreg[0]);
    shiftreg[0] := dstack[dsp];
    inc(dsp);
  end;

  procedure  _dup(data: word);
  begin
    dec(dsp);
    dstack[dsp] := _wswap(shiftreg[0], shiftreg[1]);
    shiftreg[1] :=  data;
  end;

  function  _pop: word;
  begin
    _pop := rtop;
    rtop := rstack[rsp];
    inc(rsp);
  end;

  procedure  _push(adr: word);
  begin
    dec(rsp);
    rstack[rsp] := rtop;
    rtop := adr;
  end;
  }

{
; (JUMP (; (IF (IF-   CONTROLS

  function    _RelAdr(cell: word; nib: byte): word;
  const
    wAnd:  word4 = (0,$e,$fe, $ffe);
    wOr:   word4 = (0,$fff0,$ff00, $f000);
    wTest: word4 = (0,$8,$80, $800);
  begin
    if (wtest[nib] and cell) = 0
      then begin _RelAdr := cell and wand[nibNum];
        exit;
      end;
    _RelAdr := cell or wor[nibNum];
  end;

  procedure  jump;
  begin
    if nibNum = 0 then exit;
    pc := pc + _RelAdr(pcWord, nibNum);
  end;

  procedure  _if;
  begin
    if _drop <> 0 then exit;
    jump;
  end;

  procedure  ifm;
  begin
    dec(shiftreg[1]);
    if shortint(shiftreg[1]) < 0 then exit;
    jump;
  end;

{
; !R+ @R+ xR XA       TRANSFER
}
  procedure  rstp;  begin  _doke(rtop, _drop); inc(rtop,2) end;

  procedure  rldp;  begin   _dup(_Deek(rtop)); inc(rtop,2) end;

  procedure  xr;  begin  _wswap(rtop, shiftreg[1]);  end;

  procedure  xa;  begin  _wswap(rtop, areg);  end;

{
; push pop J DUP      STACK
}
  procedure  push;   begin    _push(_drop);  end;

  procedure  j;      begin  _dup(rstack[rsp]);  end;

  procedure  dup;    begin  _dup(shiftreg[1]);  end;

  procedure  pop;    begin   _dup(_pop);  end;

{
; NAND +2/ +* -/      MATH & LOGIC
}
  procedure  nand;
  begin
    shiftreg[0] := not (shiftreg[1] and shiftreg[0]);
    _drop;
  end;

  procedure  add2div;
  var fa: boolean;
  begin
    fa := _incw(shiftreg[0], shiftreg[1]);
    shiftreg[1]  := shiftreg[0] shr 1;
    if fa then inc(shiftreg[1],$8000);
  end;

  procedure  PMul;
  var fa: boolean;
  begin  fa := odd(shiftreg[0]);
    if fa then  fa := _incw(shiftreg[1], areg);
    longint(shiftreg) := longint(shiftreg) shr 1;
    if fa then inc(shiftreg[1],$8000);
  end;

  procedure  SDiv;
  begin    longint(shiftreg) := longint(shiftreg)  shl 1;
    if shiftreg[1] >= areg then begin      _incw(shiftreg[1], succ(not areg));
      inc(shiftreg[0]);
    end;
  end;
  }

end.
