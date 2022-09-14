unit Femul;

interface

  procedure l4emul(adr: word);

implementation
uses
  SysUtils;

type
  chipstack = array[byte] of word;
  byte2 = array[0..1] of byte;
  word4 = array[0..3] of word;
  nibble = 0..15;

const
  min2 = $fffe;
  wand:  word4 = (0,$e,$fe, $ffe);
  wor:   word4 = (0,$fff0,$ff00, $f000);
  wtest: word4 = (0,$8,$80, $800);
var
  pc, pcWord, wdata, areg: word;
  rsp, dsp, nibNum: byte;
  n02, n13: byte2;
  {n3, n2, n1, n0: nibble;}
  rstack, dstack: chipstack;
  adrSpc: array[0..32767] of word ;

  function  _drop: word;
  begin
    _drop := dstack[dsp];
    inc(dsp);
  end;

  procedure  _dup(data: word);
  begin
    dec(dsp);
    dstack[dsp] := data;
  end;

  function  _pop: word;
  begin
    _pop := rstack[rsp];
    inc(rsp);
  end;

  procedure  _push(adr: word);
  begin
    dec(rsp);
    rstack[rsp] := adr;
  end;

  function  wFetch(var adr: word): word ;
  begin
    wFetch := adrspc[adr shr 1];
    inc(adr,2);
  end;

  procedure  wStore(var adr: word; what: word);
  begin
    adrspc[adr shr 1] := what ;
    inc(adr,2);
  end;

  procedure  _fetch;
  var temp: word;
  begin
    pcWord :=  wFetch(pc);
    n02 := byte2(pcWord and $f0f);
    pcWord := pcWord and min2; 
    temp := pcWord shr 4;
    n13 := byte2(temp and $f0f);
  end;

{
; (JUMP (; (IF (IF-   CONTROLS
}
  procedure  fRET;
  begin
    _pop;
    pc := wdata and min2;
  end;

  procedure  fjump;
  begin
    if nibNum = 0 then exit;
    if (wtest[nibNum] and pcWord) = 0
      then begin pc := pc + (pcWord and wand[nibNum]); exit; end;
    pcWord := pcWord or wor[nibNum];
    if pcWord <> min2 then begin  pc := pc + pcWord; exit; end;
  end;

  procedure  fif;
  begin  _drop;
    if wdata <> 0 then exit;
    fjump;
  end;

  procedure  fifm;
  begin
    dec(dstack[dsp]);
    if shortint(dstack[dsp]) < 0 then exit;
    fjump;
  end;

{
; !R+ @R+ xR XA       TRANSFER
}
  procedure  frstp;
  begin   wStore(rstack[rsp], _drop);  end;

  procedure  frldp;
  begin   _dup( wFetch(rstack[rsp]));  end;

  procedure  fxr;
  begin
    wdata := rstack[rsp];
    rstack[rsp] := dstack[dsp];
    dstack[dsp] := wdata;
  end;

  procedure  fxa;
  begin
    wdata := rstack[rsp];
    rstack[rsp] := areg;
    areg := wdata;
  end;

{
; push pop J DUP      STACK
}
  procedure  fpush;   begin    _push(_drop);  end;

  procedure  fj;      begin  _dup(rstack[byte(rsp+1)]);  end;

  procedure  fdup;    begin  _dup(dstack[byte(dsp)]);  end;

  procedure  fpop;    begin   _dup(_pop);  end;

{
; NAND +2/ +* -/      MATH & LOGIC
}
  procedure  fnand;
  begin  wdata := _drop;
    dstack[dsp] := not (wdata and dstack[dsp]);
  end;

  procedure  fadd2div;
  var temp: longint;
  begin  wdata := _drop;
    temp := wdata + dstack[dsp];
    dstack[dsp] := temp;
    _dup(temp div 2);
  end;

  procedure  fPMul;
  var temp: longint;  f: boolean;
  begin   temp := _drop;
    if odd(dstack[dsp]) then   inc(temp, areg);
    f := odd(temp shr 16);
    temp := ((temp shl 16) + dstack[dsp]) shr 1;
    dstack[dsp] := temp;
    _dup((temp shr 16) or word(ord(f) shl 15));
  end;

  procedure  fSDiv;
  var temp: longint;  f: boolean;
  begin  wdata := _drop;   f := wdata >= areg;
    if f then   dec(wdata, areg);
    temp := ((wdata shl 16) + dstack[dsp]) shl 1;
    wdata := (temp shr 16) ;
    dstack[dsp] := temp;
    if f then inc(dstack[dsp]);
    _dup(wdata);
  end;

  procedure l4emul(adr: word);
  begin
    pc := adr and min2;
  end;
  

end.
