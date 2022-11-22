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
    '(TR0;', '(TR1;','(BRK','(ESC;',
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

  //tBaseOp =  jumpOp..nandOp;
  tbaseSet= set of tOpCode;

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

  {TParsReg = object(TwordPointer)
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
  end;}

  TCompReg = object(TpcReg)
    flastColon: word;
    fbranchForward: word;
    fbranchBackward: word;
    procedure align;
    function  CanPut(nb: byte; ra: smallint): boolean;
    function  FindNibleIn(adrs: word; tbs: tbaseSet): byte;
    //function  PreparedFor(ra: smallint): boolean;
    procedure wcomp(w: word);
    Procedure PutNibble(n: byte);
    Procedure PutRel({nibble}nb: byte; {destination}ra: smallint);
    Procedure PutNum({destination}ra: smallint);
    Procedure PutAdrs(nb: byte; ra: smallint);
    procedure bcomp(b: byte);
    procedure PutString(s: shortstring);
  end;

  t4thMemory = object(tAbstractMemoryPointer)
  private
    fsize: Longint;
  public
    areg : word;

    p  : TpcReg;
    t  : TDictReg;
    h  : TCompReg;
    //s  : TParsReg;
    d  : TDumpReg;

    opArr : OpArray;

    Dstk : tWordStack;
    Rstk : tWordStack;
    Fstk : tWordStack; // forward branch stack

    xflag : boolean;
    xdbg  : boolean;

    procedure jump;            procedure _if;
    procedure ifm;             procedure rstp;
    procedure rldp;            procedure xr;
    procedure xa;              procedure push;
    procedure j;               procedure dup;
    procedure pop;             procedure nand;
    procedure add2div;         procedure PMul;
    procedure SDiv;            procedure RET;

    procedure comma;
    procedure emit;
    procedure key;
    procedure key_pressed;
    procedure clrscr;
    procedure setXY;
    procedure GetXY;

    procedure brk;
    procedure tron;
    procedure troff;
    procedure EscX;

    destructor Done;
    constructor create(memSize: longint);
    procedure initPc(adr: word);
    procedure exec(dea: word);

    //function  LastName: shortstring;  {from parsing}
    function  dumpWord: shortstring;
    procedure dumpWords(n: word);
    procedure strcomp(s3: shortstring);
    procedure Words;
    procedure doCall(adr: word);
    procedure execute;
    procedure doer(opind: byte);

    property Off: boolean read xflag write xflag;
    property Debug: boolean read xdbg write xdbg;
    property pc  : word read p.ptr  write initPc;
    property dict: word read t.ptr  write t.ptr;
    property here: word read h.ptr  write h.ptr;
    property size: Longint read fsize;
    property da: word read d.ptr write d.ptr;
    property lcolon: word read h.flastColon write h.flastColon;
    property Fwd: word read h.fbranchForward write h.fbranchForward;
    property Bwd: word read h.fbranchBackward write h.fbranchBackward;

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
    //while nib <> 0 do  PutNibble(0);
    NIB := 0;
  end;

  function  TCompReg.CanPut(nb: byte; ra: smallint): boolean;
  var wnib: byte;  n: shortint absolute nb;
  begin
    if odd(ra)
      then error(reInvalidOp);
    if (ra < -wTest[3]) or (ra >= wTest[3])
      then error(reRangeError);
    if (n <= 0) then exit(false);
    IF ((nb=1) and (ra=0)) then exit(TRUE);
    wnib := 0;
    repeat
      inc(wnib);
    until (ra >= -wTest[wnib]) and (ra < wTest[wnib]);
    result := nb > wnib;
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

  function  TCompReg.FindNibleIn(adrs: word; tbs: tbaseSet): byte;
  var op: tOpCode ;
  begin
    result:=0;
    shift := fetch(adrs);
    if not odd(shift) then exit;
    dec(shift);
    result :=  4;
    repeat op := tOpCode(getNibble);
      //WRITELN('OPW=',ORD(OP), ' ADRS=', adrs, ' RES=',RESULT); READKEY;
      if (op in tbs)  then exit;
      dec(result);
    until  result = 0;
  end;

  Procedure TCompReg.PutNibble(n: byte);
  begin
    n := n and NibMask;
    if (nib = 0) or ((nib = 1) and odd(n)) then begin
      nib := 4;
      wcomp(1);
    end;
    dec(nib);
    store(ptr-2,fetch(ptr-2) or (n shl (nib shl 2)));
  end;

  Procedure TCompReg.PutAdrs(nb: byte; ra: smallint);
  begin
    PutNibble(nb);
    while nib <> 0 do
          PutNibble(ra shr ((nib-1) shl 2));
  end;

  Procedure TCompReg.PutRel({nibble}nb: byte; {destination}ra: smallint);
  begin
    IF nib<>0 then if not canPut(nib,ra) then begin
      nib := 0;
      dec(ra,2);
      canPut(3,ra);
    end;
    PutAdrs(nb,ra);
  end;

  Procedure TCompReg.PutNum({destination}ra: smallint);
  begin
    if ra <= 0 then dec(ra,5);
    ra := ra shl 1 ;
    IF nib<>0 then if (not canPut(nib,ra)) then nib := 0;
    PutAdrs(ord(retOp),ra);
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

  {TParsReg
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
    end;}

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

    {
    ; (JUMP (; (IF (IF-   CONTROLS
       mandatory on exit  -->   nibNum:= 0;
    }

    procedure  t4thMemory.jump;
    begin
        //if p.nib = 0 then exit;
        pc := pc + p.RelAdr;
    end;

    procedure  t4thMemory._if;
    begin
      if dstk.pop = 0 then jump;
      p.nib:= 0;
    end;

    procedure  t4thMemory.ifm;
    begin
      dec(dstk.top);
      if smallint(dstk.top) >= 0 then jump;
      p.nib:= 0;
    end;

    procedure  t4thMemory.RET;    {escape codes 128}
    var anum: smallint;
    begin
      with p do
      if shift<> 0 then begin
        anum := RelAdr;
        anum := anum div 2;
        if (anum+4) in [0..3] then doer(20 + anum) {execute Extended opcode}
        else  begin
          if anum <0  then  INC(anum,5);
          dstk.Push(anum);
          p.nib:= 0;  {-1019..1023}
          exit;
        end;
      end;
      pc := rstk.pop;    {end return}
    end;

  {
  ; !R+ @R+ xR XA       TRANSFER
  }
    procedure  t4thMemory.rstp;
    begin
      p.store(rstk.top, dstk.pop);
      inc(rstk.top,2)
    end;

    procedure  t4thMemory.rldp;
    begin
      dstk.Push(p.fetch(rstk.top));
      inc(rstk.top,2)
    end;

    procedure  t4thMemory.xr;
    var temp: word;
    begin
      temp := rstk.top;
      rstk.top :=  dstk.top;
      dstk.top := temp;
    end;

    procedure  t4thMemory.xa;
    var temp: word;
    begin
      temp := rstk.top;
      rstk.top := areg;
      areg := temp;
    end;

  {
   push pop J DUP      STACK
  }
    procedure  t4thMemory.push;
    begin
      rstk.Push(dstk.pop);
    end;

    procedure  t4thMemory.j;
    begin
      dstk.Push(rstk.next);
    end;

    procedure  t4thMemory.dup;
    begin
      dstk.Push(dstk.top);
    end;

    procedure  t4thMemory.pop;
    begin
      dstk.Push(rstk.pop);
    end;

  {
  ; NAND +2/ +* -/      MATH & LOGIC
  }
    procedure  t4thMemory.nand;
    begin
      with dstk do top := not (pop and top);
    end;

    procedure  t4thMemory.add2div;
    var res: word;
    begin
      with dstk do begin
        res := avg(top, next);
        inc(next,top);
        top := res;
      end;
    end;

   procedure  t4thMemory.PMul;
   begin
     with dstk do
       if odd(next)
         then rcRw(next,rcrw(top,incw(top,areg)))
         else rcRw(next,rcrw(top));
   end;

    procedure  t4thMemory.SDiv;
    begin
      with dstk do begin
        rclw(top,rclw(next));
        if top >= areg then begin
          dec(top,areg);
          inc(next);
        end;
      end;
    end;

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
            jumpOp: nibname := '(>';
            //RetOp: ;
            IfOP:  nibname := '(.';
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
            -2: nibname := '(BRK';
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

    procedure t4thMemory.initPc(adr: word);
    begin
      p.ptr := adr and min2;
      p.nib:= 0;
    end;

    procedure t4thMemory.comma;
    begin
      H.align;
      h.wcomp(dstk.pop);
    end;

    procedure t4thMemory.exec(dea: word);
    begin
      dstk.Push(dea);
      doCall($202); {execute word and return}
    end;

    procedure t4thMemory.emit;  begin  write(char(dstk.pop));  end;

    procedure t4thMemory.key;   begin dstk.Push(byte(readkey));  end;

    procedure t4thMemory.key_pressed; begin dstk.Push(byte(keypressed));  end;
    procedure t4thMemory.clrscr; begin crt.ClrScr;  end;
    procedure t4thMemory.setXY;
    var x, y: word; begin y := dstk.pop; x := dstk.pop; crt.GotoXY(x, y);  end;

    procedure t4thMemory.GetXY;
    begin dstk.push(crt.WhereX); dstk.push(crt.Wherey); end;

    procedure t4thMemory.troff; begin Debug:=false;  end;
    procedure t4thMemory.tron;  begin Debug:=true;    end;
    procedure t4thMemory.brk;   begin rstk.Push(pc);  xflag := true;  end;
    procedure t4thMemory.EscX;  begin doer((dstk.pop and 31) + 20); end;

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
              retOp: h.PutAdrs(4,0);
              jumpOp, ifOp, ifmOp: begin
                s2 := cutstr(s3,' ');
                if s2 <> '' then begin
                  num := t.tick(s2);
                  if num <> 0 then begin
                    h.PutRel(fnd,num-here);
                  end else begin
                    num := strtonum(s2);
                    h.PutAdrs(fnd,num);
                  end;
                end else h.PutRel(fnd,0);
              end
            else h.PutNibble(fnd);
            end;
          end else if tOpCode(fnd) = numx then begin
            s2 := cutstr(s3,' ');
            fnd := strtonum(s2);
            h.PutnUM(fnd);
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
                h.PutAdrs(4,fnd);
                num := 1;
              end;
            end;
            if num = 0 then  h.PutAdrs(fnd,0);
          end;
        end;

      until s3='';
      h.align;
    end;

    constructor t4thMemory.create(memSize: LongInt); {memSize even}
    const maxbufchars = 128;
    begin
      lcolon := 0;
      fmemory := GetMem(memSize);
      fSize := memSize;
      FILLCHAR(FMEMORY^, FSIZE, #0);

      d.SetPointer(fmemory);      p.SetPointer(fmemory);
      t.SetPointer(fmemory);      h.SetPointer(fmemory);

      dict := size; t.alloc(2);   t.defWord(0, ''); // t

      here := 16;   h.nib:= 0;       // h

      setlength( OpArr,26);

      OpArr[ 0] := @jump;         OpArr[ 1] := @xr;
      OpArr[ 2] := @push;         OpArr[ 3] := @SDiv;
      OpArr[ 4] := @ret;          OpArr[ 5] := @xa;
      OpArr[ 6] := @pop;          OpArr[ 7] := @PMul;
      OpArr[ 8] := @_if;          OpArr[ 9] := @DUP;
      OpArr[10] := @rstp;         OpArr[11] := @add2div;
      OpArr[12] := @ifm;          OpArr[13] := @j;
      OpArr[14] := @rldp;         OpArr[15] := @nand;

      {extended codes}
      OpArr[16] := @troff;        OpArr[17] := @tron;
      OpArr[18] := @brk;          OpArr[19] := @EscX;

      {escape codes}
      OpArr[20] := @emit;         OpArr[21] := @key;
      OpArr[22] := @key_pressed;  OpArr[23] := @clrScr;
      OpArr[24] := @getXY;        OpArr[25] := @SETxy;

      {s.SetPointer(fmemory);
      //s.maxbuf   := maxbufchars;
      s.ptr      := t.alloc(maxbufchars);    // tib
      s.nameAdr  := t.alloc(64);
      s.strbuf   := t.alloc(256);}

      //{$include handComp.inc}

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


