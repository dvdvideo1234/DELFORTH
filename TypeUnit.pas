unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

const
  nibble = 4;   {bits}
  NibNum = 16;
  min2 = $fffe; {mask without little indian bit}
  extendedOps = 8;
  //realRet = byte(retOp) shl nibble;

type
  OpCodes = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp
      );

  pword = ^word;
  pstr  = ^shortstring;
  stkAry= array[byte] of word;


  TProcType = procedure() of object; // Method type
  FuncType = function(): pword of object;


  tWordStack = object
    top, next : word;
    sp        : byte;
    stackWords: stkAry;

    Procedure Push(w: word);
    function  pop: word;
  end;

  tAbstractMemoryPointer = object
    private
      FMemory: Pointer;
      function  getItem(index: word): Pointer;
    protected
      procedure SetPointer(memPtr: Pointer);
    public
      property mem[index: word]: Pointer read getItem;
    end;

  TwordPointer = object(tAbstractMemoryPointer)
    ptr: word;
    Procedure store(adr, w: word);
    Procedure storeb(adr: word; b: byte);
    function  fetch(adr: word): word;
    function  fetchb(adr: word): byte;
  end;

  TNibblesPointer = object(TwordPointer)
    last : word;
    shift: dword;
    nib: byte;
  end;

  TpcReg = object(TNibblesPointer)
    function  getNibble: byte;
    function  nextWord: boolean;
  end;

  TDumpReg = object(TpcReg)
    procedure  dumpWords(n: word);
  end;

  TDictReg = object(TwordPointer)
    procedure Words;
    procedure putbyte(b: byte);
    procedure putWord(w: word);
    function  FindWord(where: word; wrd: shortstring): word;
    procedure defWord(code: word; name: shortstring);
  end;

  TParsReg = object(TwordPointer)
    nameAdr: word;   // last name parsed
    // ptr - address of text buffer
    maxbuf: word;    // max chars in buf
    etib: word;   // end of buffer
    ltib: word;   // len of buffer - remainder
    procedure Pars(del: byte = 32);
    function  ACCEPT(where, len: word): word;
    procedure readLine;
  end;

  TCompReg = object(TNibblesPointer)
    Procedure PutNibble(n: byte);
    Procedure PutRelAdr(rl: word);
    procedure bcomp(b: byte);
    procedure wcomp(w: word);
  end;

  t4thMemory = object(tAbstractMemoryPointer)
  private
    fsize: Longint;
  public
    p  : TpcReg;
    t  : TDictReg;
    h  : TCompReg;
    s  : TParsReg;
    d  : TDumpReg;


    procedure initPc(adr: word);
    function  LastName: shortstring;

    constructor create(memSize: LongInt);
    destructor Done;

    property pc  : word read p.ptr  write initPc;
    property dict: word read t.ptr  write t.ptr;
    property here: word read h.ptr  write h.ptr;
    property tib:  word read s.ptr;
    property size: Longint read fsize;
    property name: shortstring read LastName;

  end;


implementation

uses
  crt;

const
  opNames : array[0..NibNum-1] of str5 = (
    '(JMP', 'XR',  'PUSH', '-/',
    '(;',   'XA',  'POP',  '+*',
    '(IF',  'DUP', '!R+',  '+2/',
    '(IF-', 'J',   '@R+',  'NAND'
    );


  {t4thMemory}
  constructor t4thMemory.create(memSize: LongInt);
  begin
    fmemory := GetMem(memSize);
    fSize := memSize;
    p.SetPointer(fmemory);
    t.SetPointer(fmemory);
    h.SetPointer(fmemory);
    s.SetPointer(fmemory);
  end;


  destructor t4thMemory.Done;
  begin
    freeMem(fmemory,fsize);
  end;

  procedure t4thMemory.initPc(adr: word);
  begin
    p.ptr := adr and min2;
    p.nib:= 0;
  end;

  function  t4thMemory.LastName: shortstring;
  begin
    result := pstr(mem[s.nameAdr])^;
  end;


  {tAbstractMemoryPointer}
  procedure tAbstractMemoryPointer.SetPointer(MemPtr: Pointer);
  begin
    FMemory := MemPtr;
  end;

  //{$asmMode intel}
  function  tAbstractMemoryPointer.getItem(index: word): Pointer;
  begin
    result := FMemory  + index;
  end;


  {TCompReg}
  procedure TCompReg.bcomp(b: byte);
  begin
    pbyte(mem[ptr])^ := b;
    inc(ptr);
  end;

  procedure TCompReg.wcomp(w: word);
  begin
    pword(mem[ptr])^ := w;
    inc(ptr,2);
  end;

  Procedure TCompReg.PutNibble(n: byte);
  begin

  end;

  Procedure TCompReg.PutRelAdr(rl: word);
  begin

  end;

  {TDictReg}
  procedure TDictReg.defWord(code: word; name: shortstring);
    var  i: integer;
  begin
    for i := length(name) downto 0 do putbyte(byte(name[i]));
    putWord(code);
  end;

  procedure TDictReg.putWord(w: word);
  begin
    dec(ptr,2);
    pword(mem[ptr])^ := w;
  end;

  procedure TDictReg.Words;
  var t, l, c: word;
  begin t := ptr; c := 0;
    writeln;
    repeat l := fetchb(t+2); if l = 0 then exit;
      if c+l+5 > 78 then begin writeln; c := 0; end;
      write(wordToHex(fetch(t)),' ');  inc(t,2);
      write(pstr(mem[t])^);            t := t + l + 1;
      c := c + l + 5;
      if c = 78 then begin writeln; c := 0; end
      else begin l := 13 - c mod 13;
        inc(c,l); write(stringOfChar(' ',l));
      end;
    until false;
  end;

  procedure TDictReg.putbyte(b: byte);
  begin
    dec(ptr);
    pbyte(mem[ptr])^ := b;
  end;

  function  TDictReg.FindWord(where: word; wrd: shortstring): word;
  type pSearch = ^searchRec;
    searchRec = record w: word; s: shortstring; end;
  var p: pSearch;
  begin result := where;
    repeat
      p := pSearch(mem[result]);
      with p^ do
         if s = '' then exit(0)
         else if wrd = s then exit
         else result := result + 3 + byte(s[0]);
    until false ;
  end;

  {tWordStack}
  Procedure tWordStack.Push(w: word);
  begin
    dec(sp);
    stackWords[sp] := next;
    next := top;
    top := w;
  end;

  function  tWordStack.pop: word;
  begin
    result := top;
    top := next;
    next:= stackWords[sp];
    inc(sp);
  end;


  {TwordPointer}
  Procedure TwordPointer.store(adr, w: word);
  begin
    pword(mem[adr])^ := w;
  end;

  Procedure TwordPointer.storeb(adr: word; b: byte);
  begin
    pbyte(mem[adr])^ := b;
  end;

  function  TwordPointer.fetch(adr: word): word;
  begin
    result := pword(mem[adr])^;
  end;

  function  TwordPointer.fetchb(adr: word): byte;
  begin
    result := pbyte(mem[adr])^;
  end;


  {TpcReg}
  function  TpcReg.nextWord: boolean;
  begin
    last := fetch(ptr);
    result := odd(last);
    inc(ptr,2);
    if result then begin
      dec(last);
      shift := last;
      nib := nibble;
    end;
  end;

  function  TpcReg.getNibble: byte;
  begin
    shift := shift shl nibble;
    dec(nib);
    result := LongRec(shift).hi;
    LongRec(shift).hi := 0;
  end;

    procedure TParsReg.Pars(del: byte = 32);
    var c: byte;              //sa - store address
      sa, fa, start: word;    //fa - fetch address
    begin
      fa := ltib+etib;
      while fa < etib do begin    // skip delimiters if any
        c := fetchb(fa);
        if c<>del then break;
        inc(fa);
      end;
      start := fa;
      sa := nameAdr+1;
      while fa < etib do begin   // gets chars into namebuffer
        c := fetchb(fa);
        if c=del then break;
        storeb(sa, c);
        inc(sa);
        inc(fa);
      end;
      storeb(sa, byte('`'));
      storeb(nameAdr, fa-start);
      ltib := fa - etib;
    end;

    function  TParsReg.ACCEPT(where, len: word): word;
    var ind: word;  ch: char; c: byte absolute ch;
    begin  ind   := 0;
      repeat ch := readkey;
        case ch of
        #8: if len > 0 then begin  write(#8#32#8); dec(len); end;
        #9: ch := ' ';
        #13: break;
        else if ch >= ' ' then begin write(ch);
          pbyte(mem[where+ind])^ := c; inc(ind); end;
        end;
      until ind = len;
      result  := ind;
    end;

    procedure TParsReg.readLine;
    begin
      ltib := - ACCEPT(ptr, maxbuf);
      etib := ptr - ltib;
    end;

    procedure  TdumpReg.dumpWords(n: word);
    begin
      while n <> 0 do begin

      end;
    end;

end.

'0','1','2','3','0','1','2','i','0','2','2','4','5','5','0','1','2','6','2','3','0','1','i','2','i','2', // 65..90
 a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z

function possetex (const c:TSysCharSet;const s : ansistring;count:Integer ):Integer;
var i,j:Integer;
begin
  if pchar(pointer(s))=nil then exit(0);
  i:=length(s);
  j:=count;
  if j>i then  exit(0);
  while (j<=i) and (not (s[j] in c)) do inc(j);
  if (j>i) then  j:=0;      // not found.
  result:=j;
end;

