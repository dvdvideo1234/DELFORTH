unit TypeUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnit;

const
  opNames : array[0..NibMask+9] of str5 = (
    '(JMP',  'XR',   'PUSH', '-/',
    '(;',    'XA',   'POP',  '+*',
    '(IF',   'DUP',  '!R+',  '+2/',
    '(IF-',  'J',    '@R+',  'NAND',
    '(TR0;', '(TR1;','(BRK;','(ESC;',
    '(NOP',  '(DROP','(1-',  ':',
    '(NUM'
    );

const
  min2 = $fffe; {mask without little indian bit}
  //extendedOps = 8;

type
  tOpCode = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp,
    TR0x,     TR1x,     BRKx,     ESCx,
    nopx,     dropx,    onemx,    colx,
    numx
    );

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
    function  ShowTop3: shortstring;
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
    //procedure dot(c: char);
    procedure align(offset, edge: integer);
    function  wstr(w: word): shortstring;
    procedure dot(w: word);
    procedure dot(s: shortstring);
    procedure cr;
  end;

  TDictReg = object(TwordPointer)
    function  tick(wrd: ShortString): word;
    function  alloc(w: word): word;
    procedure putByte(b: byte);
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
    function  found: word;
    procedure exec(dea: word);
    //procedure error(msg: shortstring);
    procedure warning(msg: shortstring);
    procedure EvalStr(st:  shortstring);  {text evaluator}
    procedure Eval(buf, len: word);   {text evaluator}

    procedure comma;
    procedure readLine;
    procedure NewItem;
    procedure Parsing;
    procedure emit;
    procedure key;
    procedure key_pressed;

    procedure brk;
    procedure tron;
    procedure troff;
    procedure EscX;

    destructor Done;
    constructor create(memSize: longint);
    procedure initPc(adr: word);
    function  LastName: shortstring;  {from parsing}
    function  dumpWord: shortstring;
    procedure dumpWords(n: word);
    procedure wcomp(st:  shortstring);
    procedure strcomp(s3: shortstring);
    procedure Words;
    procedure doCall(adr: word);
    procedure execute;
    procedure doer(opind: byte);
    procedure GetHere;

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

  function searchByName(opname: shortstring): shortint;
//var cpu: t4thMemory;

implementation

uses
  crt;

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

  function  tWordStack.ShowTop3: shortstring;
  begin
    result := '';
    strAdd(result,bytetohex(self.sp));
    strAdd(result,wordtohex(stackWords[sp]));
    strAdd(result,wordtohex(next));
    strAdd(result,wordtohex(top));
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
    result := LongRel(last , wTest[nib]);
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
    procedure TDumpReg.align(offset, edge: integer);
    begin
      dot( stringofchar(' ',edge - (cntc - offset) mod edge));
    end;

    procedure TDumpReg.dot(w: word);
    begin
      dot(wstr(w));
    end;

    function  TDumpReg.wstr(w: word): shortstring;
    begin
      result := '{ ' + wordtohex(w) + '}';
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
            case tOpCode(nibl) of
            jumpOp: nibname := '(NOP';
            //RetOp: ;
            IfOP:  nibname := '(DROP';
            IfmOp: nibname := '(1-';
            end;
          end else if tOpCode(nibl) <>  RetOp then begin  // branches
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
              nibname := '(NUM ' + ntostr(where);
              END;
            end;
          end;
          nib := 0;
        end;
        result := result + nibname + ' ';
      until nib = 0;
      result := trim(result);
    end;

    procedure t4thMemory.Words;
    var pnt, l: word;
    begin pnt := t.ptr; d.cr;
      with d do
      repeat l := fetchb(pnt+2); if l = 0 then exit;
        if cntc+l+5 > 117 then cr;
        dot(wordToHex(fetch(pnt))+' ');  inc(pnt,2);
        dot(pstr(mem[pnt])^);            inc(pnt, l + 1);
        if cntc = 117 then cr  else align(0,13);
      until false;
    end;

    procedure t4thMemory.dumpWords(n: word);
    var  where: word;  ST, res: shortstring;
    begin d.cr; st := '';
      FOR n := n downto 1 do with d do begin
        if cntc = 0 then res := wstr(ptr) else res := '';
        if st <> '' then begin
          stradd(res, st);
          dot(res);
          align(8,22);
          res := '';
        end;
        where := t.findw(ptr);
        if where <> 0 then strAdd(res,': '+pSearch(mem[where])^.s);
        st := dumpWord;
        if st = '' then  begin
          where := t.findw(last);
          if where <> 0
            then st := pSearch(mem[where])^.s
            else st := wstr(last);
        end;
        strAdd(res, st);
        if  cntc + length(res) > 118 then begin
          cr; st := res;
        end else begin
          dot(res);
          st := '';
          if d.cntc >= 96 then d.cr else align(8,22);
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

    procedure t4thMemory.emit;  begin  write(char(dstk.pop));  end;

    procedure t4thMemory.key;   begin dstk.Push(byte(readkey));  end;

    procedure t4thMemory.key_pressed; begin dstk.Push(byte(keypressed));  end;

    procedure t4thMemory.troff; begin Debug:=false;  end;
    procedure t4thMemory.tron;  begin Debug:=true;    end;
    procedure t4thMemory.brk;   begin {rstk.Push(pc);}  xflag := true;  end;
    procedure t4thMemory.EscX;  begin doer((dstk.pop and 31) + 20); end;
    procedure t4thMemory.GetHere;  begin Dstk.Push(Here); end;

    procedure t4thMemory.doer(opind: byte);
    var lastd: word;
    begin
      if debug then begin
        lastd := d.ptr;
        d.ptr := p.ptr-2;
        if p.nib = 3 then  self.dumpWords(1);
        d.cr;
        d.dot(' RS: ' + rstk.ShowTop3 + '  DS: ' +  dstk.ShowTop3);
        d.dot('  OP: ' + bytetohex(opind)+' ');
        if opind <20 then  d.dot(opNames[opind]);
        d.ptr:=lastd;
        readkey;
      end;
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
            case tOpCode(fnd) of
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
          end else if tOpCode(fnd) = numx then begin
            s2 := cutstr(s3,' ');
            fnd := strtonum(s2);
            if fnd <= 0 then dec(fnd,5);
            fnd := fnd shl 1 ;
            h.PutRelAdr(4,fnd);
          end else begin
            num := 0;
            case tOpCode(fnd) of
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
    begin
      Debug:=false;
      initCpu;

      setlength( OpArr,24);
      OpArr[ 0] := @brk;
      OpArr[ 1] := @brk;
      OpArr[ 2] := @brk;
      OpArr[ 3] := @brk;
      OpArr[ 4] := @brk;
      OpArr[ 5] := @brk;
      OpArr[ 6] := @brk;
      OpArr[ 7] := @brk;
      OpArr[ 8] := @brk;
      OpArr[ 9] := @brk;
      OpArr[10] := @brk;
      OpArr[11] := @brk;
      OpArr[12] := @brk;
      OpArr[13] := @brk;
      OpArr[14] := @brk;
      OpArr[15] := @brk;

      {extended codes}
      OpArr[16] := @troff;
      OpArr[17] := @tron;
      OpArr[18] := @brk;
      OpArr[19] := @EscX;

      {escape codes}
      OpArr[20] := @GetHere;
      OpArr[21] := @key;
      OpArr[22] := @semicolon;
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

      {BASE control & COMPILER PRIMITIVES}
      strcomp(': (BR# @R+ XR (DROP (; : (#| @R+ XR PUSH (; : (# @R+ (;');
      strcomp(': (@& @R+ POP (DROP : AND NAND DUP NAND (;');
      strcomp(': I& J (JMP AND : SGN& (@&'); h.wcomp($8000);
      strcomp(': (-IF# DUP SGN& : (BR#? (IF (BR# : SKIP @R+ (DROP (;');
      strcomp(': (VAR3 @R+ @R+ NAND (DROP : (VAR POP (;');
      strcomp(': IXEC J PUSH (; : EXECUTE PUSH (; : EX POP XR PUSH (;' );
      strcomp(': |DROP EX : DROP (DROP : NOP (; ');
      strcomp(': 2-| (1- : 1-| (1- : XEP| XR EXECUTE POP (; ');
      strcomp(': TIMES PUSH XR : TIMES| PUSH (JMP 4  PUSH J'); {ITERATORS}
      strcomp('EXECUTE POP (IF- -6 : .EXIT POP : 2DROP NAND (DROP (;');
      strcomp(': (@+ @R+ +2/ (JMP .EXIT');
      strcomp(': STR| POP |DROP  PUSH (JMP TIMES|');

      {Fetch - Store}
      strcomp(': A@ XA POP DUP PUSH XA (; : A! PUSH XA POP (DROP (;');
      strcomp(': (FS XR PUSH J PUSH @R+ POP (DROP EX (JMP 2');
      strcomp(': (S1 @R+ (DROP : (! !R+  POP (DROP (;');
      strcomp(': (DZ0 POP XR (; : (F1 @R+ (DROP : (@ @R+ POP (DROP (; ');
      strcomp(': (DZ1 POP XR (@+');   h.wcomp(2);
      strcomp(': (DZ2 POP XR (@+');   h.wcomp(4);

      {BASE STACK & MATH  OPS}
      strcomp(': DEC (FS : 1- (1- (; : NEG (1- : NOT DUP NAND (;');
      strcomp(': INC (FS : 1+ (@+'); h.wcomp(1);
      strcomp(': (?#+ DUP : (#+ @R+ (JMP 4 : (?#- DUP @R+ ');
      strcomp(': - NEG : + +2/ (DROP  (; : 2* DUP (JMP +');
      strcomp(': OR PUSH DUP I& - POP (JMP +');
      strcomp(': 2/ (NUM 0 : AVG  +2/ : NIP PUSH (DROP POP (;');
      strcomp(': |SWAP EX : SWAP PUSH XR POP (; ');

      {BASE MEM OPS}
      strcomp(': C@I J : C@ PUSH @R+ POP (DROP : W>B (@&');   h.wcomp(255);
      strcomp(': W>HB (@&');   h.wcomp($FF00);
      strcomp(': STR- |SWAP : @- 2-| : @I J : @ PUSH (JMP (@');
      strcomp(': C! PUSH W>B @I W>HB +2/ (DROP (JMP (! ');
      strcomp(': !- 2-| : !I J : ! PUSH (JMP (! ');
      strcomp(': STR |SWAP : @+ PUSH @R+ POP (; : !+ PUSH !R+ POP (; ');
      strcomp(': CSTR PUSH @R+ POP (1- PUSH (NUM 255 NAND  DUP NAND XR POP (;');


      {BASE HOST OPS}
      strcomp(': XTRON PUSH (TR1;  : TRON (TR1; ');
      strcomp(': TRACE XTRON     : TROFF (TR0; : (BK (NUM 1 (ESC;');
      strcomp(': (BE (NUM 0 : (ESC (ESC; : 1STEP EXECUTE : BRK (BRK;');


      {CONSTANTS}
      strcomp(': 0 (@'); h.wcomp(0);
      //strcomp(': -1 (@'); h.wcomp(WORD(-1));
      //strcomp(': BL (@'); h.wcomp(32);
      // strcomp(': 1 (@'); h.wcomp(1);

      {BASE MUL/DIV OPS}
      strcomp(': U* |DROP'); T.defWord(HERE+6,'`(8*');
      strcomp(': UM* A! 0 `(8* +* +* +* +*  +* +* +* +* (;');
      T.defWord(HERE+8,'`(8/');
      strcomp(': U/ |DROP : UM/MOD A! 0 : (U/ `(8/ -/ -/ -/ -/ -/ -/ -/ -/ (;');

      {BASE IO}
      strcomp(': EMIT! (# (BE (S1 (@ : EMIT (BE ');
      T.defWord(HERE+6,'CNTC');   strcomp('CNTC 1+ (S1 (@  (NOP');
      strcomp(': CR (NUM 13 EMIT (NUM 10 EMIT (NUM 0 (JMP -18');
      strcomp(': KEY!  (# (BK (S1 (@ : KEY (BK (;');


      strcomp(': TEST'); // for test := 15 downto 0 do h.PutNibble(test);

    end;

    destructor t4thMemory.Done;
    begin
      freeMem(fmemory,fsize);
      OpArr := nil;
    end;

initialization

  //cpu.Create( $10000);

finalization

  //cpu.Done;

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

