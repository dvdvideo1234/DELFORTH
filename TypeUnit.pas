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
  end;

  TpcReg = object(TNibblesPointer)
    function  RelAdr: word;
    function  getNibble: byte;
    function  nextWord: boolean;
  end;

  TDumpReg = object(TpcReg)
    cntc: word;
    procedure dot(c: char);
    procedure dot(b: byte);
    procedure dot(w: word);
    procedure dot(s: shortstring);
    procedure cr;
  end;

  TDictReg = object(TwordPointer)
    function  tick(wrd: ShortString): word;
    function  alloc(w: word): word;
    procedure Words;
    procedure putbyte(b: byte);
    procedure putWord(w: word);
    function  FindWord(where: word; wrd: shortstring): word;
    function  FindW(what: word): word;
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
    procedure align;
    function  HasAligned(ra: smallint): boolean;
    procedure wcomp(w: word);
    Procedure PutNibble(n: byte);
    Procedure PutRelAdr({nibble}nb: byte; {destination}ra: smallint);
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
    //procedure GetNumber;
    procedure words;
    function  found: word;
    procedure exec(dea: word);
    //procedure error(msg: shortstring);
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
    function  LastName: shortstring;  {from parsing}
    function  dumpWord: shortstring;
    procedure dumpWords(n: word);
    procedure wcomp(st:  shortstring);
    procedure strcomp(s3: shortstring);

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
  opNames : array[0..NibMask+9] of str5 = (
    '(JMP',  'XR',   'PUSH', '-/',
    '(;',    'XA',   'POP',  '+*',
    '(IF',   'DUP',  '!R+',  '+2/',
    '(IF-',  'J',    '@R+',  'NAND',
    '(TR0;', '(TR1;','(BRK;','(ESC;',
    '(NOP',  '(DROP','(1-',  ':',
    '(;#'
    );

const
  min2 = $fffe; {mask without little indian bit}
  //extendedOps = 8;

type
  OpCode = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp,
    TR0x,     TR1x,     BRKx,     ESCx,
    nopx,     dropx,    onemx,    colx,
    numx
    );

  function searchByName(opname: shortstring): shortint;
  var i: integer;
  begin
    for i := ord(numx) downto ord(jumpOp) do
      if opname = opnames[i] then exit(i);
    result := -1;
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
  procedure TCompReg.align;
  begin
    while nib <> 0 do
      PutNibble(0);
  end;

  function  TCompReg.HasAligned(ra: smallint): boolean;
  var newnib: byte;  wnib: byte;
  begin
    wnib := nib;
    result := false;
    if wnib = 0 then wnib := 4;
    if ra <> 0 then begin
      if (ra < -2048) or (ra > 2047)
        then error(reRangeError);
      if nib <> 0 then begin
        newnib := 1;
        while (ra < -wTest[newnib]) or (ra >= wTest[newnib]) do
          inc(newnib);
        inc(newnib);
        if (newnib > wnib) then  begin
          result :=  true;
          align;
        end;
      end;
    end;
  end;

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

  Procedure TCompReg.PutNibble(n: byte);
  begin
    n := n and NibMask;
    if (nib = 0) then nib := 4;
    dec(nib);
    last := last shl nibble;
    if nib <> 0 then last := last or n
    else if odd(n) then  begin
      wcomp(succ(last)); last := n; nib := 3;
    end else begin
      last := last or n; wcomp(succ(last));
    end;
  end;

  Procedure TCompReg.PutRelAdr({nibble}nb: byte; {destination}ra: smallint);
  begin
    HasAligned(ra);
    PutNibble(nb);
    while nib <> 0 do begin
      PutNibble(ra shr ((nib-1) shl 2));
    end;
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

  function  TDictReg.tick(wrd: ShortString): word;
  begin
    result := FindWord(ptr, wrd);
    if result = 0 then exit;
    result := fetch(result);
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

  function  TDictReg.FindW(what: word): word;
  var p: pSearch;
  begin result := ptr;
    repeat p := pSearch(mem[result]);
      with p^ do
        if s = '' then exit(0)
        else if w = what then exit
        else result := result + 3 + byte(s[0]);
    until false ;
  end;

  function  TDictReg.FindWord(where: word; wrd: ShortString): word;
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

  function  TpcReg.RelAdr: word;
  begin
    //if nib = 0 then exit(0);
    //result := (last and pred(wTest[nib])) - (last and (wTest[nib]));
    result := LongRel(last, wTest[nib]);
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
    procedure TDumpReg.dot(c: char);
    begin
      write(c);
      inc(cntc);
    end;

    procedure TDumpReg.dot(b: byte);
    begin
      dot(bytetohex(b));
    end;

    procedure TDumpReg.dot(w: word);
    begin
      dot(wordtohex(w));
    end;

    procedure TDumpReg.dot(s: shortstring);
    begin
      inc(cntc, length(s));
      write(s);
    end;

    procedure TDumpReg.cr;
    begin
      dot(#13#10);
      cntc := 0;
    end;


    {t4thMemory}
    function  t4thMemory.dumpWord: shortstring;
    var  nibl: byte; where: smallint; nibname: str39;
      wadr: word;
    begin   //write('{ ',wordtohex(fetch(ptr)),'} ');
      result := '';
      if not d.nextword then exit;
      with d do
      repeat
        nibl := getNibble;
        nibname := opnames[nibl];
        if (nibl and 3) = 0 then begin
          where := reladr;
          if where = 0 then begin
            case opCode(nibl) of
            jumpOp: nibname := '(NOP';
            //RetOp: ;
            IfOP:  nibname := '(DROP';
            IfmOp: nibname := '(1-';
            end;
          end else if opCode(nibl) <>  RetOp then begin  // branches
            wadr := t.FindW(ptr+where);
            if wadr <> 0
              then nibname := nibname + ' ' + pSearch(mem[wadr])^.s
              else nibname := nibname + ' { ' + wordtohex(ptr+where)+'}';
          end else begin
            where := where div 2;
            case where of
            -1: nibname := '(ESC;';
            -2: nibname := '(BRK;';
            -3: nibname := '(TR1;';
            -4: nibname := '(TR0;';
            ELSE  BEGIN
              if where<0  then  INC(where,5);
              nibname := '(;# ' + ntostr(where);
              END;
            end;
          end;
          nib := 0;
        end;
        result := result + nibname + ' ';
      until nib = 0;
    end;

    procedure t4thMemory.dumpWords(n: word);
    var  where: word;  ST: str39;
    begin d.cr; D.dot('{ '+wordtohex(d.ptr) + '} ');
      FOR n := n downto 1 do begin
        where := t.findw(d.ptr);
        if where <> 0 then begin
          d.dot(': '+pSearch(mem[where])^.s + '  ');
        end;
        st := dumpWord;
        if st = '' then  begin
          where := t.findw(d.last);
          if where <> 0
            then st := pSearch(mem[where])^.s
            else st := '{ '+wordtohex(d.last) + '}';
        end;
        d.dot(st);
        if d.cntc >= 96 // tabulate
          then BEGIN d.cr; D.dot('{ '+wordtohex(d.ptr) + '} '); END
          else begin
            where := 22 - (d.cntc - 8) mod 22;
            st := stringofchar(' ',where);
            d.dot(st);
          end;
      end;
    end;

    procedure t4thMemory.wcomp(st:  shortstring);
    var  where: word;
    begin  WHERE :=  T.FindWord(T.ptr,ST);
      if where = 0 then error(reNone);
      h.wcomp(t.fetch(where));
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
      if name = '' then error(reNone);
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

    {procedure t4thMemory.error(msg: shortstring);
    begin
      initCpu;
      writeln;
      writeln(name,':',msg);
      writeln;
    end;}

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
      if dea = 0 then error(reNone); //'Not found');
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
        if dea = 0 then error(reNone); //'Not found');
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

    {procedure t4thMemory.GetNumber;
    var n, err: word;
    begin
      val(name,n,err);
      if err <>0 then error(reNone); //'Not a number');
      dstk.Push(n);
    end;}

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

    procedure t4thMemory.strcomp(s3: shortstring);
    var s1, s2: shortstring;  fnd: integer;   num: word;
    begin
      repeat
        s1 := cutstr(s3,' ');        // get
        if s1 <> ''  then begin

          fnd := SearchByName(s1);  // search  opcode
          if fnd < 0 then begin
            num := t.tick(s1);      // search  name
            if num = 0 then begin  error(reNone); end;
            h.align;
            h.wcomp(num);
            continue;
          end;

          if fnd < nibNum then begin  // compile
            case OpCode(fnd) of
              retOp: h.PutRelAdr(4,0);
              jumpOp, ifOp, ifmOp: begin
                s2 := cutstr(s3,' ');
                if s2 <> '' then begin
                  num := t.tick(s2);
                  if num <> 0 then begin
                    h.HasAligned(num-here-2);
                    h.PutRelAdr(fnd,num-here-2);
                  end else begin
                    num := strtonum(s2);
                    h.PutRelAdr(fnd,num);
                  end;
                end else h.PutRelAdr(fnd,0);
              end
            else h.PutNibble(fnd);
            end;
          end else if OpCode(fnd) = numx then begin
            s2 := cutstr(s3,' ');
            fnd := strtonum(s2);
            if fnd <= 0 then dec(fnd,5);
            fnd := fnd shl 1 ;
            h.PutRelAdr(4,fnd);
          end else begin
            num := 0;
            case OpCode(fnd) of
              nopx:  fnd := 0;
              dropx: fnd := 8;
              onemx: fnd := 12;
              colx:  begin
                  s2 := cutstr(s3,' ');
                  if s2 = '' then error(reNone);
                  h.align;
                  t.defWord(here, s2);
                  num := 1;
                end;
            else begin  // TR0x, TR1x, BRKx, ESCx:
                fnd := (fnd-20) * 2;
                h.PutRelAdr(4,fnd);
                num := 1;
              end;
            end;
            if num = 0 then  h.PutRelAdr(fnd,0);
          end;
        end;

      until s3='';
      h.align;
    end;

    constructor t4thMemory.create(memSize: LongInt); {memSize even}
    const maxbufchars = 128;
      VAR test: integer;
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
      t.defWord(0, '');

      s.maxbuf   := maxbufchars;
      d.SetPointer(fmemory);

      strcomp(': (# @R+ (;  : 1- (1- (; : 1+ DUP NAND (1- : NOT DUP NAND (;');
      strcomp(': IXEC J     : EXECUTE PUSH (; : XTRON PUSH (TR1;  ');
      strcomp(': TRON (TR1; : TRACE XTRON     : TROFF (TR0;');
      strcomp(': (ESC (ESC; : 1STEP EXECUTE   : BRK (BRK;');
      strcomp(': AND NAND DUP NAND (; : EX POP XR PUSH (;');
      strcomp(': |SWAP EX : SWAP PUSH XR POP (;');
      strcomp(': - (1- DUP NAND : + +2/ (DROP  (;');
      strcomp(': |DROP EX : DROP (DROP : NOP (;');
      strcomp(': @ PUSH : (@ @R+ : EXIT POP (DROP (;');
      strcomp(': (SET @R+ (DROP : (! !R+ (JMP EXIT : A! PUSH XA (NOP (JMP EXIT');
      strcomp(': ! PUSH (JMP (! : (VAR3 @R+ @R+ NAND (DROP : (VAR POP (;');
      strcomp(': (| @R+ XR PUSH (;');
      strcomp(': A@ XA POP DUP PUSH XA (;');
      strcomp(': 0 (@ (NOP');
      strcomp(': -1 (@ NAND NAND NAND @R+');
      strcomp(': U* |DROP'); T.defWord(HERE+6,'`(8*');
      strcomp(': UM* A! 0 `(8* +* +* +* +*  +* +* +* +* (;');
      T.defWord(HERE+8,'`(8/');
      strcomp(': U/ |DROP : UM/MOD A! 0 : (U/ `(8/ -/ -/ -/ -/ -/ -/ -/ -/ (;');
      strcomp(': (BE (;# 0 (ESC');
      strcomp(': (BK (;# 1 (ESC');

      strcomp(': EMIT! (# (BE (SET (@ : EMIT (BE (;');
      strcomp(': EMIT! (# (BK (SET (@ : EMIT (BK (;');

      strcomp(': TEST');
      for test := 15 downto 0 do h.PutNibble(test);

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

