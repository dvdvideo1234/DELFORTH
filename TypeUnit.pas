unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  nibble = 4;   {bits}
  min2 = $fffe; {mask without little indian bit}

type
  pword = ^word;
  {ttstr = tstrings;
  tfilestreem = tstrings;
  TBytesStream }
  pstr  = ^shortstring;
  str5  = string[5];
  stkAry= array[byte] of word;
  bary = array[word] of byte;
  ppb = ppbyte;

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
      function  getItem(index: word): byte;
      procedure putItem(index: word; b: byte);
    protected
      procedure SetPointer(Ptr: Pointer);
    public
      property mem[index: word]: byte read getItem write putItem;
    end;

  TwordPointer = object(tAbstractMemoryPointer)
    ptr: word;
    Procedure store(adr, w: word);
    function  fetch(adr: word): word;
  end;

  TNibblesPointer = object(TwordPointer)
    last : word;
    shift: dword;
    nib: byte;
  end;

  TpcReg = object(TNibblesPointer)
    function  getNibble: byte;
    function  RelAdr: word;
    function  nextWord: boolean;
  end;

  TDictReg = object(TwordPointer)
    procedure pstbyte(b: byte);
    procedure pstWord(w: word);
    function  FindWord(where: word; wrd: shortstring): word;
    procedure defWord(code: word; name: shortstring);
  end;

  TCompReg = object(TNibblesPointer)
    Procedure PutNibble(n: byte);
    Procedure PutRelAdr(rl: word);
    procedure bcomp(b: byte);
    procedure wcomp(w: word);
  end;

  t4thMemory = class
    p  : TpcReg;
    t  : TDictReg;
    h  : TCompReg;
    fMemory : TBytesStream;

    procedure initPc(adr: word);

    constructor create(memSize: LongInt);
    destructor Done;

    property pc  : word read p.ptr write initPc;
    property dict: word read t.ptr  write t.ptr;
    property here: word read h.ptr  write h.ptr;
    property mem: TBytesStream read fMemory;

  end;


implementation

  {t4thMemory}
  constructor t4thMemory.create(memSize: LongInt);
  begin
    fMemory := TBytesStream.Create;
    fMemory.SetSize(memSize);
    fMemory.Clear;

    p.SetPointer(fMemory.Memory);
    t.SetPointer(fMemory.Memory);
    h.SetPointer(fMemory.Memory);
  end;

  destructor t4thMemory.Done;
  begin
    fMemory.Destroy;
  end;

  procedure t4thMemory.initPc(adr: word);
  begin
    p.ptr := adr and min2;
    p.nib:= 0;
  end;


  {tAbstractMemoryPointer}
  procedure tAbstractMemoryPointer.SetPointer(Ptr: Pointer);
  begin
    FMemory := ptr;
  end;

  function  tAbstractMemoryPointer.getItem(index: word): byte;
  begin
    //FMemory := ptr;
  end;

  procedure tAbstractMemoryPointer.putItem(index: word; b: byte);
  begin
    //FMemory := ptr;
  end;


  {TCompReg}
  procedure TCompReg.bcomp(b: byte);
  begin
    TBytesStream(memory).bytes[ptr] := b;
    inc(ptr);
  end;

  procedure TCompReg.wcomp(w: word);
  begin
    bcomp(wordrec(w).Lo);
    bcomp(wordrec(w).Hi);
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
    for i := length(name) downto 0 do pstbyte(byte(name[i]));
    pstWord(code);
  end;

  procedure TDictReg.pstWord(w: word);
  begin
    pstbyte(wordRec(w).Hi);
    pstbyte(wordRec(w).Lo);
  end;

  procedure TDictReg.pstbyte(b: byte);
  begin
    dec(ptr);
    TBytesStream(memory).bytes[ptr] := b;
  end;

  function  TDictReg.FindWord(where: word; wrd: shortstring): word;
  type pSearch = ^searchRec;
    searchRec = record w: word; s: shortstring; end;
  begin result := where;
    repeat
      with pSearch(@TBytesStream(memory).bytes[result])^ do
         if s = '' then begin
           result := 0;
           exit;
         end else
           if wrd = s then exit
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
    pword(@TBytesStream(memory).bytes[adr])^ := w;
  end;

  function  TwordPointer.fetch(adr: word): word;
  begin
    result := pword(@TBytesStream(memory).bytes[adr])^;
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
    shift := shift shl 4;
    dec(nib);
    result := LongRec(shift).hi;
    LongRec(shift).hi := 0;
  end;

  function  TpcReg.RelAdr: word;
  const  wTest: array[1..3] of word = ($8,$80, $800);
  begin result := 0; if nib = 0 then exit;
    result := (last and pred(wtest[nib])) - (last and wtest[nib]);
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

