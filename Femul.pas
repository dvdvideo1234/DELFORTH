unit Femul;

interface

  procedure l4emul(adr: word);

implementation
uses
  SysUtils;

type
  chipstack = array[byte] of word;
  byte2 = array[0..1] of byte;
  byte4 = array[0..3] of byte;
  wordp = ^word;
  word2 = array[0..1] of word;
  word4 = array[0..3] of word;
  nibble = 0..15;
  ProcArr= array[nibble]  of procedure;

const
  min2 = $fffe;
  wAnd:  word4 = (0,$e,$fe, $ffe);
  wOr:   word4 = (0,$fff0,$ff00, $f000);
  wTest: word4 = (0,$8,$80, $800);
  
var
  pc, pcWord, rtop, areg: word;
  dnext, dtop, wtemp: word; {, here, dict}
  ldtop: longint;
  tempw2: word2;
  rsp, dsp, nibNum: byte;
  f, xflag:  boolean;
  rstack, dstack: chipstack;
  adrSpc: array[word] of byte ;

  procedure bye;  begin   halt;  end;

  procedure byeEmul;  begin   xflag := true;  end;

  function _deek(a: word): word;
  begin    _deek := adrSpc[a] + adrSpc[a+1] shl 8;  end;

  procedure _doke(a: word; d: word);
  begin    adrSpc[a]   := d;    adrSpc[a+1] := d shr 8;  end;

  function  _drop: word;
  begin
    _drop := dtop;
    dtop  := dnext;
    dnext := dstack[dsp];
    inc(dsp);
  end;

  procedure  _dup(data: word);
  begin
    dec(dsp);
    dstack[dsp] :=  dnext;
    dnext       :=  dtop;
    dtop        :=  data;
  end;

  procedure emit;  begin  write(char(_drop));  end;

  procedure key;  var c: char;  begin read(c); _dup(byte(c));  end;

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

{
; (JUMP (; (IF (IF-   CONTROLS
}
  procedure  RET;
  begin
    if  nibNum = 3 then begin
      if byte2(pcWord)[1] = $4c then begin  {escape codes 128}
      end;
    end;
    pc := _pop and min2;
  end;

  procedure  jump;
  begin
    if nibNum = 0 then exit;
    if (wtest[nibNum] and pcWord) = 0
      then pcWord := pcWord and wand[nibNum]
      else pcWord := pcWord or wor[nibNum];
    pc := pc + pcWord;
  end;

  procedure  _if;
  begin
    if _drop <> 0 then exit;
    jump;
  end;

  procedure  ifm;
  begin
    dec(dtop);
    if shortint(dtop) < 0 then exit;
    jump;
  end;

{
; !R+ @R+ xR XA       TRANSFER
}
  procedure  rstp;  begin  _doke(rtop, _drop); inc(rtop,2) end;

  procedure  rldp;  begin   _dup(_Deek(rtop)); inc(rtop,2) end;

  procedure  xr;
  begin  wtemp := rtop;     rtop := dtop; dtop := wtemp;
  end;

  procedure  xa;
  begin  wtemp := rtop;     rtop := areg; areg := wtemp;
  end;

{
; push pop J DUP      STACK
}
  procedure  push;   begin    _push(_drop);  end;

  procedure  j;      begin  _dup(rstack[rsp]);  end;

  procedure  dup;    begin  _dup(dtop);  end;

  procedure  pop;    begin   _dup(_pop);  end;

{
; NAND +2/ +* -/      MATH & LOGIC
}
  procedure  nand;
  begin
    dnext := not (dtop and dnext);
    _drop;
  end;

  procedure  add2div;
  begin
    ldtop := dtop + dnext;
    dnext := ldtop;
    dtop := (ldtop div 2);
  end;

  procedure  PMul;
  begin   ldtop := dtop;
    if odd(dnext)
      then   inc(ldtop, areg);
    f := odd(ldtop shr 16);
    ldtop := ((ldtop shl 16) or dnext) shr 1;
    dnext := ldtop;
    dtop := ((ldtop shr 16) or word(ord(f) shl 15));
  end;

  procedure  SDiv;
  begin
    f := dtop >= areg;
    if f
      then   dec(dtop, areg);
    ldtop := ((dtop shl 16) or  dnext) shl 1;
    dtop := (ldtop shr 16) ;
    dnext := ldtop;
    if f
      then inc(dnext);
  end;

  const
    OpArr: procArr =
      (
      jump,  xr,    push,  SDiv,
      ret,   xa,    pop,   PMul,
      _if,   DUP,   rstp,  nand,
      ifm,   J,     rldp,  add2div
      );



  procedure l4emul(adr: word);
  label nextCell;
  begin
    pc := adr and min2;  xflag := false;
  repeat   nextCell: pcWord :=  _Deek(pc); inc(pc,2);
    if not odd(pcWord) then begin  {nesting}
      _push(pc); pc :=  pcWord;    {goto nextCell;}
      continue;
    end;
    tempw2[0] := pcWord and min2;
    nibNum := 3; {unpack cell to opcodes}
    repeat tempw2[1] := 0;
      longint(tempw2) := longint(tempw2) shl 4;
      OpArr[tempw2[1]];  {execute opcode}
      if tempw2[1] and 3 = 0    then goto  nextCell;
      dec(nibNum);
    until nibNum > 3;
  until xflag;
  end;
  

end.
