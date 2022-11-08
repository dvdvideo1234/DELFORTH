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

  procedure putc(what: char; var where: pchar);
  function  ascPack(s, d: pchar; l: longint): longint;
  function  ascUnPack(s, d: pchar; l: longint): longint;
  function  strToNum(s: shortstring): longint;
  procedure stradd(var sd: shortstring; ss: shortstring; del: char = ' ');
  function  cutStr(var s: shortstring; del: char): shortstring;
  function  LongRel(inw, Mask: longint): longint;
  function  NToHex(n: dword; chars: byte): str15;
  function  LongToHex(n: dword): str15;
  function  NumberToStr(n: longint; chars: byte = 14; base: byte = 10): str39;
  function  byteTohex(n: byte): str3;
  function  NToStr(n: longint): str15;
  function  byteTobin(n: byte): str15;
  function  DigToChar(n: byte): char;
  function  WordTohex(n: word): str7;
  function  divmod(var w: dword; d: dword): dword;
  function  Avg(a, b: Longint): Longint;

  function  rcRw(var d: word; carry: boolean = false): boolean;
  function  rcLw(var d: word; carry: boolean = false): boolean;
  function  incw(var acc: word; w: word): boolean;

  var ssz: shortstring;

implementation

  {$asmMode intel}
  procedure putc(what: char; var where: pchar);assembler; nostackframe;
  {$ifdef SYSTEMINLINE}inline;{$endif}
  asm
     mov    [edx],al
     inc    dword ptr [edx]
  end;

  function  DigToChar(n: byte): char;  assembler; nostackframe;
  label noInc;
  asm  // if n > 9 then inc(n, 7);   result := char(n+byte('0'));
     cmp   al,10
     jl    noInc
     add   al,7
  noInc:
     add   al,'0'
  end;

  function ascPack(s, d: pchar; l: longint): longint;
  var oldd: pchar; ch : char; c: byte absolute ch;
    procedure getc; begin ch := s^; inc(s); dec(l); end;
  begin  oldd := d;
    while l > 0 do begin  getc;
      case ch of
        '"': if l > 0  then begin getc; putc(ch,d);  end;
        '^': if l > 0  then begin getc; c := c and 31; putc(ch,d);; end;
        '~': if oldd <> d then  byte((d-1)^) := byte((d-1)^) xor $80;
        '_': begin ch := ' '; putc(ch,d);; end;
      else putc(ch,d);;
      end;
    end;
    result := d - oldd;
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
    jcxz  nodec
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

function  NToHex(n: dword; chars: byte): str15;
  begin
    if (chars in [1..7])
      then  result := NumberToStr(n or $80000000, chars, 16)
      else  result := LongToHex(n);
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

  function  ROLD(d: dword; ind: byte): dword; assembler; nostackframe;
  label stayz;
  asm
    and dl,31
    mov cl,dl
    mov dl,1
    shl edx,cl
    mov ecx,edx
    and ecx,eax // one bit isolate
    xor eax,ecx // and zeroed
    dec edx    // mask
    and edx,eax
    xor eax,edx
    shl edx,1
    jecxz stayz
    inc  edx
  stayz:
    or  eax,edx
  end;

{function  ROLD(d: dword; ind: byte): dword;
label stayz;
begin asm
  xor edx,edx
  inc edx
  mov cl,ind
  mov eax,d
  and cl,31
  shl edx,cl

  mov ecx,edx
  and ecx,eax // one bit isolate
  xor eax,ecx // and zeroed

  dec edx    // mask

  and edx,eax
  xor eax,edx
  shl edx,1
  jecxz stayz
  inc  edx
stayz:
  or  eax,edx
  mov Result,eax
end; end;
}
  function  RORD(d: dword; ind: byte): dword;
  label stayz;
  begin asm
    xor edx,edx
    inc edx
    mov cl,ind
    mov eax,d
    and cl,31
    shl edx,cl

    mov ecx,edx  // one bit isolate

    dec edx    // mask
    or  edx,ecx

    and edx,eax
    xor eax,edx
    shr edx,1
    jnc stayz
    or  edx,ecx
  stayz:
    or  eax,edx
    mov Result,eax
  end; end;

end.


