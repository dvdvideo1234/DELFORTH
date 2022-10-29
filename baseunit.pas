unit BaseUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const  wTest: array[1..3] of word = ($8,$80, $800);

type
  str3  = string[ 3];
  str5  = string[ 5];
  str7  = string[ 7];
  str15 = string[15];
  str39 = string[39];
  pstr  = ^shortstring;
  pword = ^word;
  pbyte = ^byte;

  function  RelAdr(inw: word; nib: byte): word;
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

  function  drcRw(var s, d: word; cr: boolean = false): boolean;
  function  drcLw(var s, d: word; cr: boolean = false): boolean;
  function  incw(var acc: word; w: word): boolean;

implementation

  function  RelAdr(inw: word; nib: byte): word;
  begin result := 0; if nib = 0 then exit;
    result := (inw and pred(wtest[nib])) - (inw and wtest[nib]);
  end;

  function  byteTobin(n: byte): str15;
  begin
    result := NumberToStr(n or 256, 8, 2);
  end;

  {$asmMode intel}
  function  incw(var acc: word; w: word): boolean;assembler; nostackframe;
  asm
    add [eax],dx
    mov al,0
    rcl al,1
  end;

  function  drcRw(var s,d:word; cr: boolean): boolean;assembler; nostackframe;
  asm
    xchg  ecx,eax
    shr   al,1
    rcr   word ptr [ecx],1
    rcr   word ptr [edx],1
    rcl   al,1
  end;

  function  drcLw(var s,d: word; cr:boolean): boolean;assembler; nostackframe;
  asm
    xchg  ecx,eax
    shr   al,1
    rcl   word ptr [ecx],1
    rcl   word ptr [edx],1
    rcl   al,1
  end;

  function Avg(a, b: Longint): Longint; assembler; nostackframe;
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

  function  DigToChar(n: byte): char;
  begin
    if n > 10 then inc(n, 7);
    result := char(n+byte('0'));
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

  function divmod(var w: dword; d: dword): dword;
  begin
    asm
       xor edx,edx
       mov ecx,w
       mov eax,[ecx]
       div d
       mov [ecx],eax
       mov Result,edx
    end;
  end;

  function  ROLD(d: dword; ind: byte): dword;
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

function  swapw(var s, d: word): word;  begin asm
  mov   edx,d
  mov   ecx,s
  mov   ax,[ecx]
  xchg  ax,[edx]
  xchg  ax,[ecx]
end; end;


