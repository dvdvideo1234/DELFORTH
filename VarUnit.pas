unit VarUnit;

interface
uses
  Classes, SysUtils
  ,BaseUnit
  ,TypeUnit;

type
  nodeType = ( ntUndef, ntWord, ntPrimitive, ntConst, ntVar,
            ntValue, ntQuan, ntDefer, ntVector, ntVQuan);

  E4thError = class(Exception);

  pWordListNode = ^tWordListNode;
  tWordListNode = object
    aLink: pWordListNode;
    Act: TProcType;
    aName: string;
  end;

  tWordSearch = object
    fRoot: pWordListNode;
    function Search(s: string): pWordListNode;
    procedure AddNode(actor: TProcType; name: string);
    procedure Done;
  end;

  t4thCPU = object (t4thMemory)
    fHost: tWordSearch;
    fCompiler: tWordSearch;
    fLineCnt: longint;

    SEARCH_EXEC: TProcType;
    Fname:  shortstring;
    Ftib:   shortstring;

    {COMPILER STUFF}
    procedure jumpComp;     procedure xrComp;       procedure pushComp;
    procedure SDivComp;     procedure retComp;      procedure xaComp;
    procedure popComp;      procedure PMulComp;     procedure ifComp;
    procedure DUPComp;      procedure rstpComp;     procedure a2dComp;
    procedure ifmComp;      procedure JComp;        procedure rldpComp;
    procedure nandComp;     procedure TR0Comp;      procedure TR1Comp;
    procedure BRKComp;      procedure EscComp;      procedure SEARCH_EXEC_COMP;
    procedure semicolon;    procedure semisemi;     procedure Colon;
    procedure DoBegin;      procedure DoUntil;      procedure DoIf;
    procedure DoIfm;        procedure DoNextM;      procedure DoAhead;
    procedure DoTHEN;       procedure DoTWICE;      procedure DoCall;
    procedure DoAgain;      procedure ToLabel;      procedure DoTo;
    procedure DoAt;

    {INTERPRETTER STUFF}
    procedure BYE;          procedure Include;      procedure SEARCH_EXEC_INT;
    procedure dot;          procedure Hdot;         PROCEDURE GetDICT;
    procedure page;         procedure WDUMP;        procedure DoTick;
    procedure SetInterp;    procedure SetCompile;   procedure GetIndent;
    PROCEDURE GetHERE;      procedure REM;          PROCEDURE INCL;
    procedure DoCONST;      procedure DoValue;

    {COMMON STUFF}
    procedure NewItem;      procedure Parsing;
    procedure fSystem;      procedure wcomp(st:  shortstring);
    procedure initCpu;      procedure error(msg: shortstring);
    function  found: word;  procedure warning(msg: shortstring);
    procedure EvalStr;      procedure IncludeText(nam: shortstring);
    PROCEDURE CR;           procedure InitBaseOperations;
    procedure NotFound;     procedure MarkForward(o: tOpCode);
    procedure ACCEPT;       procedure ReleaseBackward(o: tOpCode);
    procedure GetNumber;    procedure CompNum;
    procedure MakeWord;
    procedure DoAdd;
    procedure DoSub;


      //procedure Eval(buf, len: word);   {text evaluator}
    //procedure readLine;


    property tib:  shortstring read ftib write ftib;
    property LastW: shortstring read Fname write Fname;

  end;

//var  cpu : t4thcpu;


implementation

uses
  crt;

    function t4thCPU.found: word;
    begin
      result := t.FindWord(t.ptr, lastw);
      if result <> 0 then result := t.fetch(result);
    end;

    procedure t4thCPU.wcomp(st:  shortstring);
    var  where: word;
    begin  WHERE :=  T.FindWord(T.ptr,ST);
      if where = 0 then error('"'+ST+'" Not exist');
      h.wcomp(t.fetch(where));
    end;

    procedure t4thCPU.NotFound;
    begin
      error('Not found');
    end;

    procedure t4thCPU.initCpu;
    begin
      off := true;
      Debug:=false;
      Fwd := 0;
      Bwd := 0;
      SEARCH_EXEC := @SEARCH_EXEC_INT;
      REM;
      fLineCnt := 0;
    end;

    {procedure t4thCPU.readLine; begin readLn(ftib); end;}

    procedure t4thCPU.GetIndent;
    begin
      Parsing;
      if lastw = '' then NotFound;
    end;

    procedure t4thCPU.NewItem;
    begin
      GetIndent;
      if found <> 0 then warning('duplicates');
    end;

    procedure t4thCPU.GetNumber;
    var n: LONGINT;
      err: word;
    begin
      val(LastW,n,err);
      if err <> 0  then NotFound;
      dstk.Push(n);
    end;

    procedure t4thCPU.DoTick;
    var dea: Word;
    begin
      GetIndent;
      dea := found;
      if dea = 0 then NotFound;
      dstk.Push(dea);
    end;

    procedure t4thCPU.Parsing;
    begin
      ftib := TrimLeft(ftib);
      lastw := cutstr(ftib,' ');
      //WRITE(LASTW,' '); IF (LASTW = ';') OR (LASTW = '(;') THEN CR;
    end;

    procedure t4thCPU.semisemi;
    begin
      retComp;
      if (Fwd or Bwd) = 0 then SetInterp;
    end;

    procedure t4thCPU.warning(msg: shortstring);
    begin write('[',lastW,']',msg,' ');   end;

    procedure t4thCPU.SEARCH_EXEC_INT;
    var dea: pWordListNode;
    begin  //write(lastw,' ');  //IF LASTW = ';' THEN WRITELN;
      dea := fHost.Search(lastw);
      if dea <> nil then BEGIN dea^.Act; EXIT; END;
      getNumber;
    end;

    procedure t4thCPU.SEARCH_EXEC_COMP;
    var dea: pWordListNode; W: integer;
    begin
      dea := fcompiler.Search(lastw);
      if dea <> nil then begin
        dea^.Act; exit; end;
      w := found;
      if w <> 0 then begin h.align; h.wcomp(w); exit; end;
      getNumber;
      compNum;
    end;

    procedure t4thCPU.CompNum;
    var W: integer;
    begin
      w := dstk.pop;
      if (w >= -1019) and (w <= 1023)
        then h.Putnum(w)
        else begin
          h.align;
          wcomp('(#');
          h.wcomp(w)
        end;
      lcolon := HERE;
    end;

    {text evaluator
    procedure t4thCPU.Eval(buf, len: word);
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
    end;}

    procedure t4thCPU.error(msg: shortstring);
    begin
      CR;
      write('"'+lastw+'" :',msg);
      if fLineCnt <> 0 then write(' at line '+ntostr(fLineCnt));
      initCpu;
      raise  E4thError.Create('');
    end;


    {tWordSearch}
    function tWordSearch.Search(s: string): pWordListNode;
    begin
      result := fRoot;
      while result <> nil do with result^ do begin
        if aName = s then exit;
        result := aLink;
      end;
    end;

    procedure tWordSearch.AddNode(actor: TProcType; name: string);
    var newp: pWordListNode;
    begin
      Pointer(newP) := new(PWordListNode);
      with NewP^ do begin
        aName:=Name;
        Act:=Actor;
        aLink:=Froot;
        fRoot:= newp;
      end;
    end;

    procedure tWordSearch.Done;
    var newp: pWordListNode;
    begin
      while self.fRoot <> nil do begin
        newp := froot^.aLink;
        dispose(froot);
        froot := newp;
      end;
    end;

    procedure t4thCPU.IncludeText(nam: shortstring);
    var incfile: text;   FLAG: boolean;  LineCnt: longint;
    begin   LineCnt := 0;
      assign(incfile,nam);
      IF NOT FileExists(nam) then error('file not found');
      reset(incfile);
      try
        while not eof(incfile) do begin   flag := true;
          readln(incfile,Ftib);   inc(LineCnt); FLineCnt := LineCnt;
          EvalStr;                        flag := false;
        end;
      finally
        close(incfile);
        if flag then BEGIN CR;
          write(nam + ' is closed at line ',ntostr(LineCnt));
        END;
      end;
    end;

    procedure t4thCPU.fSystem;
    begin
      repeat CR; write('>> ');        ACCEPT; WRITE(' ');
        try
          evalStr; WRITE(' Ok');
        Except
          on E: EInOutError do writeln('InOut '+E.Message);
          on E: E4thError do fLineCnt := 0; //  writeln('E4thError');
          //on e: reRunError do writeln('file not found');
          else  writeln('Unhandled Error');
        end;
      until false;
    end;

    procedure t4thCPU.EvalStr;  {text evaluator}
    begin
      repeat
        Parsing;
        if lastw <> '' then SEARCH_EXEC;
      until lastw = '';
    end;

    procedure t4thCPU.ACCEPT;
    var ind, len: word;  ch: char; c: byte absolute ch;
    begin  ind   := 0; LEN := 255;
      repeat ch := readkey;
        case ch of
        #8: if ind > 0 then begin  write(#8#32#8); dec(ind); end;
        #9: ch := ' ';
        #13: break;
        else if ch >= ' ' then begin write(ch); inc(ind);
          ftib[ind] := ch; end;
        end;
      until ind = len;
      c := ind;
      ftib[0]  := ch;
    end;

    procedure t4thCPU.Colon;
    begin
      GetIndent;
      SetCompile;
      h.align;
      lcolon := here;
      t.defWord(here, lastw);
    end;

    procedure t4thCPU.DoTHEN;  // ReleaseForward
    var opw, aptr: word;  num: smallint;
    begin
      with h do begin
        align;
        lcolon := here;
        IF fWD = 0 THEN ERROR('No Forward Mark');
        fWD := fWD - 1;
        aptr := Fstk.pop-2;  // for call only
        if fetch(aptr) = min2 then begin store(aptr,here); exit; end;
        opw := FindNibleIn(aptr, [jumpOp, ifOp, ifmOp]);
        DEC(OPW);
        num :=  HERE - (aptr+2);
        if (num < -wTest[opw]) or (num >= wTest[opw])
          then error('Can''t fix at '+wordtohex(aptr));
        num := num and pred(wTest[OPW]);
        store(aptr,fetch(aptr) or num);
      end;
    end;


    procedure t4thCPU.semicolon;
    var opw, aptr: word;   num: smallint;
    begin semisemi;
      WITH H DO BEGIN
        num := h.FindNibleIn(here-2,[retOp]);
        aptr:= here-4;
        opw := h.fetch(aptr);
        if (aptr < lcolon) or (num <> 4)  or odd(opw)
          then  exit;  // no optimization is possible
        HERE := aptr;
        DEC(APTR,2);
        if (aptr >= lcolon) and odd(fetch(aptr))
          then NIB := h.FindNibleIn(aptr,[jumpOp]);
        PutRel(ord(jumpOp),opw-here); // trying to insert jump
        lcolon := here;
      end;
    end;


    procedure t4thCPU.MarkForward(o: tOpCode);
    begin
      IF H.nib <> 0 then begin
         if not h.canPut(h.nib,126) then h.nib := 0;
      end;
      h.PutAdrs(ORD(o),0);
      Fstk.Push(here);
      Fwd := Fwd + 1;
      lcolon := here;
    end;

    procedure t4thCPU.ReleaseBackward(o: tOpCode);
    var num: word;
    begin IF BWD = 0 THEN ERROR('No Backward Mark');
      BWD := BWD - 1;
      num := dstk.pop;
      h.PutRel(ORD(o),num-here);
      lcolon := here;
    end;

    procedure t4thCPU.DoCONST;
    begin
      GetIndent;
      h.align;
      t.defWord(here, lastw);
      wcomp('(@');
      h.wcomp(dstk.pop);
      lcolon := here;
    end;

    procedure t4thCPU.MakeWord;
    begin
      GetIndent;
      h.align;
      t.defWord(dstk.pop, lastw);
    end;

    procedure t4thCPU.SetCompile; begin SEARCH_EXEC := @SEARCH_EXEC_COMP; end;
    PROCEDURE t4thCPU.INCL;  BEGIN INCLUDETEXT('TEST.INC'); END;
    procedure t4thCPU.DoAdd; begin with dstk do push(pop+pop); end;
    procedure t4thCPU.DoSub; var q: word; begin q := dstk.pop;
      dstk.top:= dstk.top - q;
    end;

    procedure t4thCPU.jumpComp;  begin h.PutAdrs(ORD(jumpOp),0); end;
    procedure t4thCPU.xrComp;   begin h.PutNibble(ORD(xrOp)); end;
    procedure t4thCPU.pushComp; begin h.PutNibble(ORD(PUSHOp)); end;
    procedure t4thCPU.SDivComp; begin h.PutNibble(ORD(SDIVOp)); end;
    procedure t4thCPU.retComp;   begin h.PutAdrs(ORD(RETOp),0);  end;
    procedure t4thCPU.xaComp;   begin h.PutNibble(ORD(xAOp)); end;
    procedure t4thCPU.popComp;  begin h.PutNibble(ORD(POPOp)); end;
    procedure t4thCPU.PMulComp; begin h.PutNibble(ORD(PMULOp)); end;
    procedure t4thCPU.ifComp;    begin h.PutAdrs(ORD(IFOp),0);  end;
    procedure t4thCPU.DUPComp;  begin h.PutNibble(ORD(DUPOp)); end;
    procedure t4thCPU.rstpComp; begin h.PutNibble(ORD(rstpOp)); end;
    procedure t4thCPU.a2dComp;  begin h.PutNibble(ORD(A2DOp)); end;
    procedure t4thCPU.ifmComp;   begin h.PutAdrs(ORD(IFMOp),0); end;
    procedure t4thCPU.JComp;    begin h.PutNibble(ORD(jOp)); end;
    procedure t4thCPU.rldpComp; begin h.PutNibble(ORD(rldpOp)); end;
    procedure t4thCPU.nandComp; begin h.PutNibble(ORD(nandOp)); end;
    procedure t4thCPU.TR0Comp;   begin strcomp('(TR0;'); end;
    procedure t4thCPU.TR1Comp;  begin strcomp('(TR1;'); end;
    procedure t4thCPU.BRKComp;  begin strcomp('(BRK'); end;
    procedure t4thCPU.EscComp;  begin strcomp('(ESC;'); end;

    procedure t4thCPU.SetInterp;
              begin h.align; SEARCH_EXEC := @SEARCH_EXEC_INT; end;
    procedure t4thCPU.REM;   begin  Ftib := '';  Fname:='';  end;
    procedure t4thCPU.DoTWICE;  begin h.align; h.wcomp(here+2); end;
    procedure t4thCPU.DoBegin;   // MarkBack
              begin  h.align; dstk.Push(here); bwd := bwd + 1;    end;
    procedure t4thCPU.DoCall;
         begin  h.align; h.wcomp(MIN2); fstk.Push(here);  Fwd := Fwd + 1; end;
    procedure t4thCPU.DoUntil;  begin  ReleaseBackward(IFOp); end;
    procedure t4thCPU.DoAgain;  begin  ReleaseBackward(jumpOp); end;
    procedure t4thCPU.DoNextM;  begin  ReleaseBackward(IFmOp); end;
    procedure t4thCPU.ToLabel;  begin  doBegin; dstk.pop; DoTick; end;
    procedure t4thCPU.DoIf;     begin  MarkForward(ifOp); end;
    procedure t4thCPU.DoIfm;    begin  MarkForward(ifmOp); end;
    procedure t4thCPU.DoAhead;  begin  MarkForward(jumpOp); end;
    procedure t4thCPU.DoTo;  BEGIN doTick; dec(dstk.top,2); comma;  END;
    procedure t4thCPU.DoAt;  BEGIN doTick; dec(dstk.top,4); comma;  END;

    procedure t4thCPU.BYE; begin HALT; end;
    procedure t4thCPU.Include; begin  GetIndent; IncludeText(lastw); end;
    procedure t4thCPU.page;  begin  clrScr; end;
    procedure t4thCPU.dot; begin write(dstk.pop,' '); end;
    procedure t4thCPU.Hdot;  begin write(wordtohex(dstk.pop),' '); end;
    procedure t4thCPU.WDUMP;  VAR L: word;
      begin  l := dstk.pop; d.ptr:=dstk.pop AND MIN2;  dumpwords(l); end;
    PROCEDURE t4thCPU.GetHERE;    begin dstk.push(HERE); end;
    PROCEDURE t4thCPU.GetDICT;    begin dstk.push(DICT); end;
    PROCEDURE t4thCPU.CR;    begin d.cr; end;
    procedure t4thCPU.DoValue;   BEGIN  wcomp('(S1'); DoConst;  END;



    procedure t4thCPU.InitBaseOperations;
    begin
      initCPU;

      with fHost do begin
        AddNode(@bye,'BYE');                   AddNode(@Include,'INCLUDE');
        AddNode(@HDOT,'H.');                   AddNode(@dot,'.');
        AddNode(@page,'PAGE');                 AddNode(@GETXY,'@XY');
        AddNode(@WDUMP,'WDUMP');               AddNode(@WORDS,'WORDS');
        AddNode(@COMMA,',');                   AddNode(@DOTICK,'''');
        AddNode(@GetHERE,'HERE');              AddNode(@GetDICT,'DICT');
        AddNode(@SetCompile,'>,');             AddNode(@COLON,':');
        AddNode(@EMIT,'EMIT');                 AddNode(@CR,'CR');
        AddNode(@REM,'\');                     AddNode(@INCL,'TEST');
        AddNode(@DoCONST,'CONST');             AddNode(@DoVALUE,'VALUE');
        AddNode(@MakeWord,'=:');               AddNode(@DoaDD,'+');
        AddNode(@dOsUB,'-');
      end;

      with fCompiler do begin
        AddNode(@jumpComp,opNames[0]);         AddNode(@xrComp,opNames[1]);
        AddNode(@pushComp,opNames[2]);         AddNode(@SDivComp,opNames[3]);
        AddNode(@retComp,opNames[4]);          AddNode(@xaComp,opNames[5]);
        AddNode(@popComp,opNames[6]);          AddNode(@PMulComp,opNames[7]);
        AddNode(@ifComp,opNames[8]);           AddNode(@DUPComp,opNames[9]);
        AddNode(@rstpComp,opNames[10]);        AddNode(@a2dComp,opNames[11]);
        AddNode(@ifmComp,opNames[12]);         AddNode(@JComp,opNames[13]);
        AddNode(@rldpComp,opNames[14]);        AddNode(@nandComp,opNames[15]);
        AddNode(@TR0Comp,opNames[16]);         AddNode(@TR1Comp,opNames[17]);
        AddNode(@BRKComp,opNames[18]);         AddNode(@EscComp,opNames[19]);

        AddNode(@SetInterp,',<');              AddNode(@COLON,':');
        AddNode(@REM,'\');                     AddNode(@SEMICOLON,';');
        AddNode(@ToLabel,'<''>');              AddNode(@SEMISEMI,';;');
        AddNode(@dOTo,'TO');                   AddNode(@dOAt,'AT');

        AddNode(@DoBegin,'BEGIN');             AddNode(@DoUntil,'UNTIL');
        AddNode(@DoNextM,'NEXT-');             AddNode(@DoAGAIN,'AGAIN');
        AddNode(@DoIf,'IF');                   AddNode(@DoIfm,'IF-');
        AddNode(@DoAhead,'AHEAD');             AddNode(@DoTHEN,'THEN');
        AddNode(@DoCall,'CALL');               AddNode(@doTWICE,'TWICE');
      end;
    end;



initialization

  //cpu.Create( $10000);
  //cpu.InitBaseOperations;

finalization

  //cpu.Done;

end.


