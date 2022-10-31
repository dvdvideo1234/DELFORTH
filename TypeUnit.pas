unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

type
  pword = ^word;
  pstr  = ^shortstring;
  stkAry= array[byte] of word;
  TProcType = procedure() of object; // Method type
  OpArray=  array of TProcType;

  pSearch = ^searchRec;
  searchRec = record w: word; s: shortstring; end;


  tWordStack = object
    top, next : word;
    sp        : byte;
    stackWords: stkAry;

    Procedure Push(w: word);
    function  pop: word;
    Procedure ShowTop3;
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
    function  RelAdr: word;
  end;

  TpcReg = object(TNibblesPointer)
    function  getNibble: byte;
    function  nextWord: boolean;
  end;

  TDumpReg = object(TpcReg)
    function  dumpWord: boolean;
  end;

  TDictReg = object(TwordPointer)
    function  alloc(w: word): word;
    procedure Words;
    procedure putbyte(b: byte);
    procedure putWord(w: word);
    function  FindWord(where: word; wrd: shortstring): word;
    function  FindW(where, what: word): word;
    procedure defWord(code: word; name: shortstring);
  end;

  TParsReg = object(TwordPointer)
    nameAdr: word;   // last name parsed
    // ptr - address of text buffer
    maxbuf: word;    // max chars in buf
    strbuf: word;   //: shortstring
    buflen: word;
    etib: word;   // end of buffer
    ltib: word;   // len of buffer - remainder
    procedure Pars(del: byte = 32);
    function  ACCEPT(where, len: word): word;
    procedure readLine;
    procedure setstr(s: shortstring);
  end;

  TCompReg = object(TNibblesPointer)
    procedure wcomp(w: word);
    Procedure PutNibble(n: byte);
    function  PreparedForPut(ra: smallint):byte;
    Procedure FlushNumber(ra: smallint);
    Procedure PutRelAdr({nibble}nb: byte; {destination}ra: smallint);
    Procedure PutNumber(ra: smallint);
    procedure bcomp(b: byte);
    procedure PutString(s: shortstring);
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

    opArr : OpArray;

    Dstk : tWordStack;
    Rstk : tWordStack;

    SEARCH_EXEC: TProcType;
    xflag : boolean;
    xdbg  : boolean;

    procedure SEARCH_EXEC_INT;
    procedure SEARCH_EXEC_COMP;
    procedure semicolon;
    procedure Colon;
    procedure initCpu;
    procedure GetNumber;
    procedure words;
    function  found: word;
    procedure exec(dea: word);
    procedure error(msg: shortstring);
    procedure warning(msg: shortstring);
    procedure EvalStr(st:  shortstring);  {text evaluator}
    procedure Eval(buf, len: word);  {text evaluator}
    procedure doCall(adr: word);
    procedure execute;
    procedure doer(opind: byte);

    procedure comma;
    procedure readLine;
    procedure brk;
    procedure NewItem;
    procedure Parsing;
    procedure emit;
    procedure key;
    procedure key_pressed;
    procedure tron;
    procedure troff;

    destructor Done;
    constructor create(memSize: longint);
    procedure initPc(adr: word);
    function  LastName: shortstring;
    procedure dumpWords(n: word);

    property Off: boolean read xflag write xflag;
    property Debug: boolean read xdbg write xdbg;
    property pc  : word read p.ptr  write initPc;
    property dict: word read t.ptr  write t.ptr;
    property here: word read h.ptr  write h.ptr;
    property tib:  word read s.ptr;
    property size: Longint read fsize;
    property name: shortstring read LastName;
    property da: word read d.ptr write d.ptr;

  end;

var
  cpu: t4thMemory;

implementation

uses
  crt;

const
  opNames : array[0..NibMask] of str5 = (
    '(JMP', 'XR',  'PUSH', '-/',
    '(;',   'XA',  'POP',  '+*',
    '(IF',  'DUP', '!R+',  '+2/',
    '(IF-', 'J',   '@R+',  'NAND'
    );

const
  min2 = $fffe; {mask without little indian bit}
  //extendedOps = 8;

type
  OpCode = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp
      );


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

  procedure TCompReg.PutString(s: shortstring);
  var i: integer;
  begin
    for i := 0 to length(s) do  bcomp(ord(s[i]));
  end;

  procedure TCompReg.wcomp(w: word);
  begin
    pword(mem[ptr])^ := w;
    inc(ptr,2);
  end;

  Procedure TCompReg.PutNibble(n: byte);  {plus spetial codes}
  begin
    if opcode(n) in [jumpOp..NandOp] then begin
      if nib = 0 then begin last := n; nib := 3; exit; end;
        dec(nib);
        last := last shl nibble;
        if (nib <> 0) or (not  odd(n)) then last := last or n;
        if nib = 0 then begin
          wcomp(succ(last));
          if odd(n) then PutNibble(n);
        end;
      exit;
    end;
    n := (n and 7) shl 1;  {spetial codes 16..23  becames 0..14}
    if nib = 1 then PutNibble(0);
    PutNibble(byte(retOp));
    FlushNumber(n - (n and 8));
  end;

  function TCompReg.PreparedForPut(ra: smallint):byte;
  var newnib: byte;
  begin
    newnib := fitIn12b(ra);
    if newnib = 4 then error(reRangeError);
    if (newnib > nib+1) and (nib <> 0) then
      while nib <> 0 do PutNibble(0);
    result := nib;
  end;

  Procedure TCompReg.PutNumber(ra: smallint);
  begin
    if ra < 0 then dec(ra,4) else inc(ra,4);
    ra := ra shl 1;
    preparedForPut(ra);
    PutNibble(byte(RetOp));
    FlushNumber(ra);
  end;

  Procedure TCompReg.FlushNumber(ra: smallint);
  begin
    while nib <> 0 do  PutNibble((ra shr ((nib-1) shl 2)) and 15);
  end;

  Procedure TCompReg.PutRelAdr({nibble}nb: byte; {destination}ra: smallint);
  var newnib: byte;
  begin
    newnib := nib;
    if preparedForPut(ra) <> newnib then begin
      dec(ra,2);
      if fitIn12b(ra) = 4 then error(reRangeError);
    end;
    PutNibble(nb);
    FlushNumber(ra);
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

  function  TDictReg.alloc(w: word): word;
  begin
    dec(ptr,w);
    result := ptr;
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

  function  TDictReg.FindW(where, what: word): word;
  var p: pSearch;
  begin result := where;
    repeat p := pSearch(mem[result]);
      with p^ do
        if s = '' then exit(0)
        else if w = what then exit
        else result := result + 3 + byte(s[0]);
    until false ;
  end;

  function  TDictReg.FindWord(where: word; wrd: shortstring): word;
  var p: pSearch;
  begin result := where;
    repeat p := pSearch(mem[result]);
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

  Procedure tWordStack.ShowTop3;
  begin
    write(wordtohex(stackWords[sp]), ' ');
    write(wordtohex(next), ' ');
    write(wordtohex(top), ' ');
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

  {TParsReg}
    procedure TParsReg.Pars(del: byte = ord(' '));
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

    procedure TParsReg.setstr(s: shortstring);
    var ind: word;  c: byte ;
    begin  ind   := strbuf+1;
      for c := 1 to length(s) do begin
        pchar(mem[ind])^ := s[c];
        if s[c] in [#0..#8,#10..#31,#127..#255] then else inc(ind);
      end;
      pbyte(mem[strbuf])^ := pred(ind)- strbuf;
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
      buflen := ACCEPT(ptr, maxbuf);
    end;

    {TDumpReg}
    function  TDumpReg.dumpWord: boolean;
    var  nibl: byte; where: word; nibname: str7;
    begin
      write(#13#10,wordtohex(ptr),'  ',wordtohex(fetch(ptr)),' ');
      result := nextword; if not result then exit;
      repeat  nibl := self.getNibble;
        nibname := opnames[nibl];
        if (nibl and 3) = 0 then begin
          where := reladr;
          if where = 0 then begin
            case opCode(nibl) of
            jumpOp: nibname := 'NOP';
            RetOp: ;
            IfOP:  nibname := 'DROP';
            IfmOp: nibname := '1-';
            end;
          end else begin
            case opCode(nibl) of
            RetOp:
              case smalLint(where) and 7 of
                    0: nibname := 'UNDEF;'; // UNAVAILABLE
                    1: nibname := 'BRK;';   // СТОП
                    2: nibname := 'KEY;';
                    3: nibname := 'EMIT;';
                    4: nibname := 'GET#;';
                    5: nibname := 'KEY?;';
                    6: nibname := 'TRON;';
                    7: nibname := 'TROFF;';
              else
                begin
                  if smalLint(where) < 0 then inc(where,4) else dec(where,4);
                  nibname := inttostr(smalLint(where));
                end;
              end;
            else  nibname := nibname + ' ' + wordtohex(ptr + where);
            end;
          END;
          nib := 0;
        end;
        write(nibname,' ');
      until nib = 0;
    end;

    {TNibblesPointer}
    function  TNibblesPointer.RelAdr: word;
    begin
      result := BaseUnit.RelAdr(last, wTest[nib]);
    end;

    {t4thMemory}
    procedure t4thMemory.dumpWords(n: word);
    var  where: word;
    begin
      FOR n := n downto 1 do begin
        where := t.findw(t.ptr, d.ptr);
        if where <> 0 then write(#13#10,pSearch(mem[where])^.s, ':');
        if not d.dumpWord then  begin
          where := t.findw(t.ptr, d.last);
          if where <> 0 then write(pSearch(mem[where])^.s);
        end;
      end;
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

    procedure t4thMemory.doer(opind: byte);
    begin
      OpArr[opind]; {unpack & execute opcode}
    end;

    procedure t4thMemory.execute;
    begin
      if p.nib = 0 then
        if not p.nextWord then begin  { get next cell }
          rstk.push(pc); pc :=  p.last;  {nesting}
          exit;
        end;
      doer(p.getNibble); {unpack & execute opcode}
    end;

    procedure t4thMemory.doCall(adr: word);
    begin
      InitPc(adr);   off := false;
      repeat
        execute;
      until off;
    end;

    procedure t4thMemory.comma;
    begin
      h.wcomp(dstk.pop);
    end;

    function t4thMemory.found: word;
    begin
      result := t.FindWord(dict, name);
    end;

    procedure t4thMemory.readLine;
    begin
      s.readLine;
    end;

    procedure t4thMemory.words;
    begin
      t.Words;
    end;

    procedure t4thMemory.NewItem;
    begin
      Parsing;
      if name = '' then error('Empty WOrd Name');
      if found <> 0 then warning('duplicates');
      t.defWord(dstk.pop, name);
    end;

    procedure t4thMemory.Parsing;
    begin
      s.Pars();
    end;

    procedure t4thMemory.Eval(buf, len: word);  {text evaluator}
    var oldLtib, oldEtib: word;
    begin
      oldltib := s.ltib;
      oldetib := s.etib;
      s.ltib    := -len;
      s.etib    := buf + len;
      repeat
        Parsing;
        if name <> '' then SEARCH_EXEC;
      until name = '';
      s.ltib := oldltib;
      s.etib := oldetib;
    end;

    procedure t4thMemory.EvalStr(st:  shortstring);  {text evaluator}
    begin
      s.setstr(st);
      eval(s.strbuf+1,length(st));
    end;

    procedure t4thMemory.error(msg: shortstring);
    begin
      initCpu;
      writeln;
      writeln(name,':',msg);
      writeln;
    end;

    procedure t4thMemory.initCpu;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_INT;
      off := true;
      s.ltib := 0;   {buffer became empty}
    end;


    procedure t4thMemory.warning(msg: shortstring);
    begin write('[',name,']',msg,' ');   end;

    procedure t4thMemory.SEARCH_EXEC_INT;
    var dea: word;
    begin
      dea := found;
      if dea = 0 then error('Not found');
      exec(dea);
    end;

    procedure t4thMemory.SEARCH_EXEC_COMP;
    var dea: word;
    begin
      inc(pchar(mem[s.nameAdr])^);
      dea := found;
      dec(pchar(mem[s.nameAdr])^);
      if dea <> 0 then exec(dea)
      else begin
        dea := found;
        if dea = 0 then error('Not found');
        h.wcomp(dea);
      end;
    end;

    procedure t4thMemory.Colon;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_COMP;
    end;

    procedure t4thMemory.semicolon;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_INT;

    end;

    procedure t4thMemory.GetNumber;
    var n, err: word;
    begin
      val(name,n,err);
      if err <>0 then error('Not a number');
      dstk.Push(n);
    end;

    procedure t4thMemory.exec(dea: word);
    begin
      dstk.Push(dea);
      doCall($202); {execute word and return}
    end;

    procedure t4thMemory.brk;  begin   xflag := true;  end;

    procedure t4thMemory.emit;  begin  write(char(dstk.pop));  end;

    procedure t4thMemory.key;  var c: char;
    begin c := readkey; dstk.Push(byte(c));  end;

    procedure t4thMemory.key_pressed; begin dstk.Push(byte(keypressed));  end;
    procedure t4thMemory.tron; begin Debug:=true;    end;
    procedure t4thMemory.troff; begin Debug:=false;  end;

    constructor t4thMemory.create(memSize: LongInt); {memSize even}
    const maxbufchars = 128;
    begin
      Debug:=false;
      initCpu;

      setlength( OpArr,24);

      OpArr[17] := @brk;
      OpArr[18] := @key;
      OpArr[19] := @emit;
      OpArr[20] := @Colon;
      OpArr[21] := @semicolon;
      OpArr[22] := @tron;
      OpArr[23] := @troff;

      //defopx(@key, 'KEY');              defopx(@emit, 'EMIT');
      //defopx(@NewItem, ':=');           defopx(@Parsing, 'WORD');

      fmemory := GetMem(memSize);
      fSize := memSize;

      p.SetPointer(fmemory);

      t.SetPointer(fmemory);
      dict := size;     // t
      t.alloc(2);

      h.SetPointer(fmemory);
      here := 16;      // h
      h.nib:= 0;

      s.SetPointer(fmemory);
      s.ptr      := t.alloc(maxbufchars);    // tib
      s.nameAdr  := t.alloc(64);
      s.strbuf   := t.alloc(256);
      t.defWord(0, '');  //h.wcomp($e401);        // literal

      s.maxbuf   := maxbufchars;
      d.SetPointer(fmemory);

      {jumpOp,   xrOp,     pushOp,   SDivOp,     0 1 2 3
      retOp,    xaOp,     popOp,    PMulOp,      4 5 6 7
      ifOp,     DUPOp,    rstpOp,   a2dOp,       8 9 a b
      ifmOp,    JOp,      rldpOp,   nandOp       c d e f }

      t.defWord(here, '(2#');  h.wcomp($e001);        // literal
      t.defWord(here, '(#');  h.wcomp($e401);        // literal
      t.defWord(here, 'IXEC');  h.wcomp($D001);
      t.defWord(here, 'EXECUTE');  h.wcomp($2401);
      t.defWord(here, 'EX');  h.wcomp($6125);
      t.defWord(here, 'SWAP');  h.wcomp($2165);
      t.defWord(here, 'DROP');  h.wcomp($8001);   h.wcomp($4001);
      t.defWord(here, '(CON');  h.wcomp($E001);
      t.defWord(here, 'EXIT');  h.wcomp($6801);   h.wcomp($4001);

    end;

    destructor t4thMemory.Done;
    begin
      freeMem(fmemory,fsize);
      OpArr := nil;
    end;

initialization

  cpu.Create( $10000);

finalization

  cpu.Done;

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

