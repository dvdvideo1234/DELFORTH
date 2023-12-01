unit BaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  nibble = 4;   {bits}
  NibNum = 16;
  NibMask= nibNum-1;
  wTest: array[0..nibble-1] of word = (0, $8,$80, $800);

type
  str3  = string[ 3];
  str5  = string[ 5];
  str7  = string[ 7];
  str15 = string[15];
  str39 = string[39];
  pstr  = ^shortstring;
  pword = ^word;
  pbyte = ^byte;

  function  getc(var where: pchar): char;
  procedure putc(what: char; var where: pchar);
  function  ascPack(s, d: pchar; l: longint): longint;
  function  ascUnPack(s, d: pchar; l: longint): longint;
  function  strPack(s: shortstring): shortstring;
  function  strUnPack(s: shortstring): shortstring;

  function  strToNum(s: shortstring): longint;
  procedure stradd(var sd: shortstring; ss: shortstring; del: char = ' ');
  function  cutStr(var s: shortstring; del: char = ' '): shortstring;
  function  NToHex(n: dword; chars: byte): str15;
  function  LongToHex(n: dword): str15;
  function  NumberToStr(n: longint; chars: byte = 14; base: byte = 10): str39;
  function  byteTohex(n: byte): str3;
  function  NToStr(n: longint): str15;
  function  byteTobin(n: byte): str15;
  function  DigToChar(n: byte): char;
  function  WordTohex(n: word): str7;

  function  LongRel(inw, Mask: longint): longint;
  function  divmod(var w: dword; d: dword): dword;
  function  Avg(a, b: Longint): Longint;
  function  even(what: longint): longint;
  function  ClrBit(d: dword; bit: byte): dword;
  function  GetBit(d: dword; bit: byte): boolean;
  function  SetBit(d: dword; bit: byte): dword;
  function  Pow2(Pow: byte): cardinal;
  function  ROLD(d: dword; ind: byte): dword;
  function  RORD(d: dword; ind: byte): dword;

  function  rcRw(var d: word; carry: boolean = false): boolean;
  function  rcLw(var d: word; carry: boolean = false): boolean;
  function  incw(var acc: word; w: word): boolean;

  var ssz: shortstring;

implementation

  {$asmMode intel}
  function getc(var where: pchar): char; assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    mov    ecx,[eax]
    mov    cl,[ecx]
    inc    dword ptr [eax]
    xchg   eax,ecx
  end;

  procedure putc(what: char; var where: pchar);assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
     mov    ecx,[edx]
     mov    [ecx],al
     inc    dword ptr [edx]
  end;

  function even(what: longint): longint;assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
     and    al,$fe
  end;

  function  DigToChar(n: byte): char;  assembler; nostackframe;
  label noInc;
  asm   // if n > 9 then inc(n, 7);   result := char(n+byte('0'));
     cmp   al,10
     jb    noInc
     add   al,7
  noInc:
     add   al,'0'
  end;

  function ascPack(s, d: pchar; l: longint): longint;
  var oldd: pchar; ch : char; c: byte absolute ch;
    procedure getc1; begin ch := getc(s); dec(l); end;
  begin  oldd := d;
    while l > 0 do begin  getc1;
      case ch of
        '"': if l > 0  then begin getc1; putc(ch,d);  end;
        '^': if l > 0  then begin getc1; c := c and 31; putc(ch,d); end;
        '~': if oldd <> d then  byte((d-1)^) := byte((d-1)^) xor $80;
        '_': begin ch := ' '; putc(ch,d); end;
      else putc(ch,d);;
      end;
    end;
    result := d - oldd;
  end;

  function strPack(s: shortstring): shortstring;
  var c: byte;
  begin
    c := ascPack(@s[1], @result[1], length(s));
    result[0] := char(c);
  end;

  function strUnPack(s: shortstring): shortstring;
  var RA: array[0..799] of char;
    c: word;
  begin
    c := ascUnPack(@s[1], @RA[1], length(s));
    ra[0] := char(c);
    result := pstr(@RA)^;
  end;

  function ascUnPack(s, d: pchar; l: longint): longint;
  var oldd: pchar; ch : char; c: byte absolute ch;  hibit: boolean;
  begin  oldd := d;
    for l := l downto 1 do begin  ch := s^; inc(s);
      hibit := c > 127; c :=  c and 127;
      case ch of
        #0..#31: begin putc('^',d);  c := c or ord('@'); end;
        '~','"','^','_':  putc('"',d);
        ' ': ch := '_';
      end;
      putc(ch,d);
      if hibit then putc('~',d);
    end;
    result := d - oldd;
  end;

  procedure stradd(var sd: shortstring; ss: shortstring; del: char = ' ');
  begin
    if sd = ''
      then sd := ss
      else sd := sd + del + ss;
  end;

  function strToNum(s: shortstring): longint;
  var err: word;
  begin
    val(s,result,err);
  end;

  function cutStr(var s: shortstring; del: char): shortstring;
  var p: integer;
  begin
    p := pos(del,s);
    if p = 0 then begin
      result := s;
      s := '';
      exit;
    end;
    result := copy(s,1,p-1);
    delete(s,1,p);
  end;

  function  byteTobin(n: byte): str15;
  begin
    result := NumberToStr(n or 256, 8, 2);
  end;

  function  incw(var acc: word; w: word): boolean;assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    add [eax],dx
    mov al,0
    rcl al,1
  end;

  function  XCHGw(WHAT: WORD; var d: word): word; assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    xchg  ax,[edx]
  end;

{function  RelAdr(inw: word; Mask: word): smallint;  mask power of 2
begin result := 0; if Mask < 2 then exit;
  result := (inw and pred(Mask)) - (inw and Mask);
end; }

function  LongRel(inw, Mask: longint): longint;  assembler; nostackframe;
label nodec;
asm
    shr   edx,1
    shl   edx,1
    mov   ecx,edx
    jecxz  nodec
    dec   edx
    and   ecx,eax
nodec:
    and   eax,edx
    sub   eax,ecx
end;

  function  rcRw(var d: word; carry: boolean): boolean;assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    xchg  eax,edx
    shr   al,1
    rcr   word ptr [edx],1
    rcl   al,1
  end;

  function  rcLw(var d: word; carry: boolean = false): boolean;assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    xchg  eax,edx
    shr   al,1
    rcl   word ptr [edx],1
    rcl   al,1
  end;

  function Avg(a, b: Longint): Longint; assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
    add   eax,edx
    rcr   eax,1
  end;

{
function Avg(a, b: Longint): Longint;
begin
  if a < b then
    Result := a + ((b - a) shr 1)
  else
    Result := b + ((a - b) shr 1);
end;
}
  function  NToStr(n: longint): str15;
  begin result := '';
    if n < 0 then begin n := -n; result := '-'; end;
    result := result + NumberToStr(n,14,10);
  end;

  function  NumberToStr(n: longint; chars, base: byte): str39;
  var dw: dword absolute n;   i: pchar;
  begin  i := @result[39];
    if base > 63 then base := 64 else if base < 2 then base := 2;
    repeat
      i^ := digToChar(divmod(dw, base));
      dec(i);
      dec(chars);
    until (n=0) or (chars=0);
    i^ := char(@result[39]-i);
    result := pstr(i)^;
  end;

  function  byteToHex(n: byte): str3;
  begin
    result := digtochar(n shr 4) + digtochar(n and 15);
  end;

  function  WordToHex(n: word): str7;
  begin
    result := byteToHex(wordRec(n).Hi) + byteToHex(wordRec(n).Lo);
  end;

  function  LongToHex(n: dword): str15;
  begin
    result := WordToHex(LongRec(n).Hi) + WordToHex(LongRec(n).Lo);
  end;

  function  NToHex(n: dword; chars: byte): str15;
    begin
      if (chars in [1..7])
        then  result := NumberToStr(n or $80000000, chars, 16)
        else  result := LongToHex(n);
    end;

  function divmod(var w: dword; d: dword): dword; assembler; nostackframe;
  asm
     push eax
     xor  ecx,ecx
     xchg ecx,edx
     mov  eax,[eax]
     div  ecx
     pop  ecx
     mov  [ecx],eax
     xchg eax,edx
  end;

  function  ClrBit(d: dword; bit: byte): dword; assembler; nostackframe;
  asm
    mov cl,dl
    mov dl,1    // value of bit zero
    and cl,31   // no more then 31 bit
    shl edx,cl  // create mask to edx
    mov ecx,edx // copy mask   to ecx
    and ecx,eax // one bit isolate in ecx
    xor eax,ecx // and zeroed in eax
  end;

  function  GetBit(d: dword; bit: byte): boolean; assembler; nostackframe;
  label stayz;
  asm
    call ClrBit
    mov al,0
    jecxz stayz
    inc  eax
  stayz:
  end;

  function  SetBit(d: dword; bit: byte): dword; assembler; nostackframe;
  asm
    call ClrBit
    or   eax,edx
  end;

  function  Pow2(Pow: byte): cardinal; assembler; nostackframe;
  asm
    xchg eax,edx
    call ClrBit
    xchg eax,edx
  end;

  function  ROLD(d: dword; ind: byte): dword; assembler; nostackframe;
  label stayz;
  asm
    call ClrBit
    dec edx     // 0..ind-1 mask
    and edx,eax
    xor eax,edx
    shl edx,1
    jecxz stayz
    inc  edx
  stayz:
    or  eax,edx
  end;

  function  RORD(d: dword; ind: byte): dword;  assembler; nostackframe;
  label stayz;
  asm
    call ClrBit
    push edx
    dec edx     // 0..ind-1 mask
    and edx,eax
    xor eax,edx
    or  edx,ecx
    pop ecx
    shr edx,1
    jnc stayz
    or  edx,ecx
  stayz:
    or  eax,edx
  end;

end.


