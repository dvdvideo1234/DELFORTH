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
  pstr  = ^shortstring;
  str5  = string[5];
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


  tMemory = class
    pcReg : word;
    lastw : word;
    fshift: dword;
    nibNum: byte;
    tReg  : word;
    hReg  : word;
    bytes : array of byte;

    function  getNibble: byte;
    function  RelAdr: word;

    Procedure store(adr, w: word);
    function  fetch(adr: word): word;
    Procedure nextWord;
    procedure stp(b: byte);
    procedure bcomp(b: byte);
    procedure wcomp(w: word);
    function  FindWord(where: word; wrd: shortstring): word;
    procedure defWord(code: word; name: shortstring);

    procedure initPc(adr: word);
    constructor create(memSize: word);
    destructor Done;

    property pc  : word read pcReg write initPc;
    property dict: word read tReg  write tReg;
    property here: word read hReg  write hReg;

  end;


implementation

  procedure tMemory.bcomp(b: byte);
  begin
    bytes[hReg] := b;
    inc(hReg);
  end;

  procedure tMemory.wcomp(w: word);
  begin
    bcomp(wordrec(w).Lo);
    bcomp(wordrec(w).Hi);
  end;

  procedure tMemory.defWord(code: word; name: shortstring);
    var  i: integer;
  begin
    for i := length(name) downto 0 do stp(byte(name[i]));
    stp(wordRec(code).Hi);
    stp(wordRec(code).Lo);
  end;

  procedure tMemory.stp(b: byte);
  begin
    dec(tReg);
    bytes[tReg] := b;
  end;

  function  tMemory.FindWord(where: word; wrd: shortstring): word;
  type pSearch = ^searchRec;
    searchRec = record w: word; s: shortstring; end;
  begin result := where;
    repeat
      with pSearch(@bytes[result])^ do
         if s = '' then begin
           result := 0;
           exit;
         end else
           if wrd = s then exit
           else result := result + 3 + byte(s[0]);
    until false ;
  end;

  procedure tMemory.initPc(adr: word);
  begin
    pc := adr and min2;
    nibNum:= 0;
  end;

  constructor tMemory.create(memSize: word);
  begin
    if memSize < 400 then memSize := memSize;
    setLength(bytes,memSize);
  end;

  destructor tMemory.Done;
  begin
    bytes := nil;
  end;

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

  {ByteMemory}

  Procedure tMemory.store(adr, w: word);
  begin
    pword(@bytes[adr])^ := w;
  end;

  function  tMemory.fetch(adr: word): word;
  begin
    result := pword(@bytes[adr])^;
  end;

  Procedure tMemory.nextWord;
  begin
    lastw := fetch(pc);
    inc(pcReg,2);
  end;

  {tnibles}
  function  tMemory.getNibble: byte;
  begin
    LongRec(fshift).hi := 0;
    fshift := fshift shl 4;
    dec(nibNum);
    result := LongRec(fshift).hi;
  end;

  function  tMemory.RelAdr: word;
  type word4 = array[1..3] of word;
  const  wTest: word4 = ($8,$80, $800);
  begin result := 0; if nibNum = 0 then exit;
    result := (lastw and pred(wtest[nibNum])) - (lastw and wtest[nibNum]);
  end;

end.

{
  Procedure tRegister.incr;
  begin
    inc(fRegister);
  end;

  Procedure tRegister.decr;
  begin
    dec(fRegister);
  end;

  function  tRegister.adrs: pointer;
  begin
    result :=addr(fRegister);
  end;

  Procedure tRegister.add(value: integer);
  begin
    inc(fRegister, Value);
  end;
}

{
tRegister = object
  fRegister: word;
  Procedure incr;
  Procedure decr;
  Procedure add(value: integer);
  function  adrs: pointer;
end;

tByteRegister = object
  fRegister: word;
  Procedure incr;
  Procedure decr;
  Procedure add(value: integer);
  function  adrs: pointer;
end;

tAbstractMemory = class
  private
    FMemory: Pointer;
    FSize  : PtrInt;
  protected
    Function GetSize : LongInt; virtual; Abstract;
    procedure SetPointer(Ptr: Pointer; ASize: PtrInt); virtual; Abstract;
  public
    property Memory: Pointer read FMemory;
  end;
}


