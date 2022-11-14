unit VarUnit;

interface
uses
  Classes, SysUtils
  ,BaseUnit
  ,TypeUnit;

type
  nodeType = ( ntUndef, ntWord, ntPrimitive, ntConst, ntVar,
            ntValue, ntQuan, ntDefer, ntVector, ntVQuan);

  pUnresolved = ^tUnresolved;
  tUnresolved = record
    aLink: pUnresolved;
    aWord: cardinal; {address}
  end;

  pWordListNode = ^tWordListNode;
  tWordListNode = object
    aLink: pWordListNode;
    aWord: cardinal; {address}
    aName: string;
    //Actor: TProcType;
    aTimes: cardinal; {Used}
    atype: nodeType;
    Procedure perform;
    Procedure compile;
    procedure Resolve;
  end;

  tWordSearch = object
    fRoot: pWordListNode;
    function Search(s: string): pWordListNode;
    function Search(w: cardinal): pWordListNode;
    procedure AddNode(wadr: word; name: string; typ: nodetype = ntUndef);
  end;

  t4thCPU = object (t4thMemory)
    areg : word;

    fTarget: tWordSearch;
    fHost: tWordSearch;
    fCompiler: tWordSearch;

    SEARCH_EXEC: TProcType;
    Fname:  shortstring;
    Ftib:   shortstring;
    fnum: longint;

    procedure jump;            procedure _if;
    procedure ifm;             procedure rstp;
    procedure rldp;            procedure xr;
    procedure xa;              procedure push;
    procedure j;               procedure dup;
    procedure pop;             procedure nand;
    procedure add2div;         procedure PMul;
    procedure SDiv;            procedure RET;

    procedure InitBaseOperations;
    procedure IncludeText(nam: string);
    procedure StrEval(st: SHortstring);

    procedure SEARCH_EXEC_INT;
    procedure SEARCH_EXEC_COMP;
    procedure semicolon;
    procedure Colon;
    function  foundW: word;
    procedure error(msg: shortstring);
    procedure warning(msg: shortstring);
    procedure EvalStr(st:  shortstring);  {text evaluator}
    procedure Eval(buf, len: word);   {text evaluator}
    procedure readLine;
    procedure NewItem;
    procedure Parsing;
    function  GetNumber: boolean;

    property tib:  shortstring read ftib write ftib;
    property LastW: shortstring read Fname write Fname;
    property num: longint read Fnum write Fnum;

  end;

//var  cpu : t4thcpu;


implementation

uses
  crt;

    procedure t4thCPU.readLine;
    begin
      readLn(ftib);
    end;

    procedure t4thCPU.warning(msg: shortstring);
    begin write('[',lastW,']',msg,' ');   end;

    procedure t4thCPU.SEARCH_EXEC_INT;
    var dea: pWordListNode;
    begin
      dea := fHost.Search(lastw);
      if dea = nil then error('Not found');
      dea^.perform;
    end;

    procedure t4thCPU.SEARCH_EXEC_COMP;
    var dea: pWordListNode;
    begin
      dea := fcompiler.Search(lastw);
      if dea <> nil then begin
        dea^.perform; exit; end;
      dea := fHost.Search(lastw);
      if dea <> nil then begin
        dea^.compile; exit; end;
      if not getNumber then begin
        error('Not a number');  exit; end;
      if (num >= -1019) and (num <= 1023)
        then begin
          if num <= 0 then dec(fnum,5);
            num := num shl 1 ;
            h.PutRelAdr(ord(retOp),num);
        end
        else begin
          h.align;
          h.wcomp(t.FindWord(t.ptr,'(#'));
          h.wcomp(num);
        end;
    end;

    procedure t4thCPU.Colon;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_COMP;
    end;

    procedure t4thCPU.semicolon;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_INT;

    end;

    function t4thCPU.GetNumber: boolean;
    var n, err: word;
    begin
      val(LastW,n,err);
      result := err = 0;
      if not result  then error('Not a number') else dstk.Push(n);
    end;

    procedure t4thCPU.NewItem;
    begin
      Parsing;
      if name = '' then error(reNone);
      if found <> 0 then warning('duplicates');
      t.defWord(dstk.pop, name);
    end;

    procedure t4thCPU.Parsing;
    begin
      s.Pars();
    end;

    procedure t4thCPU.Eval(buf, len: word);  {text evaluator}
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

    procedure t4thCPU.EvalStr(st:  shortstring);  {text evaluator}
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

    function t4thCPU.found: word;
    begin
      result := t.FindWord(dict, name);
    end;



    {
    ; (JUMP (; (IF (IF-   CONTROLS
       mandatory on exit  -->   nibNum:= 0;
    }

    procedure  t4thCPU.jump;
    begin
        //if p.nib = 0 then exit;
        pc := pc + p.RelAdr;
    end;

    procedure  t4thCPU._if;
    begin
      if dstk.pop = 0 then jump;
      p.nib:= 0;
    end;

    procedure  t4thCPU.ifm;
    begin
      dec(dstk.top);
      if smallint(dstk.top) >= 0 then jump;
      p.nib:= 0;
    end;

    procedure  t4thCPU.RET;    {escape codes 128}
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
    procedure  t4thCPU.rstp;
    begin
      p.store(rstk.top, dstk.pop);
      inc(rstk.top,2)
    end;

    procedure  t4thCPU.rldp;
    begin
      dstk.Push(p.fetch(rstk.top));
      inc(rstk.top,2)
    end;

    procedure  t4thCPU.xr;
    var temp: word;
    begin
      temp := rstk.top;
      rstk.top :=  dstk.top;
      dstk.top := temp;
    end;

    procedure  t4thCPU.xa;
    var temp: word;
    begin
      temp := rstk.top;
      rstk.top := areg;
      areg := temp;
    end;

  {
   push pop J DUP      STACK
  }
    procedure  t4thCPU.push;
    begin
      rstk.Push(dstk.pop);
    end;

    procedure  t4thCPU.j;
    begin
      dstk.Push(rstk.next);
    end;

    procedure  t4thCPU.dup;
    begin
      dstk.Push(dstk.top);
    end;

    procedure  t4thCPU.pop;
    begin
      dstk.Push(rstk.pop);
    end;

  {
  ; NAND +2/ +* -/      MATH & LOGIC
  }
    procedure  t4thCPU.nand;
    begin
      with dstk do top := not (pop and top);
    end;

    procedure  t4thCPU.add2div;
    var res: word;
    begin
      with dstk do begin
        res := avg(top, next);
        inc(next,top);
        top := res;
      end;
    end;

   procedure  t4thCPU.PMul;
   begin
     with dstk do
       if odd(next)
         then rcRw(next,rcrw(top,incw(top,areg)))
         else rcRw(next,rcrw(top));
   end;

    procedure  t4thCPU.SDiv;
    begin
      with dstk do begin
        rclw(top,rclw(next));
        if top >= areg then begin
          dec(top,areg);
          inc(next);
        end;
      end;
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

    function tWordSearch.Search(w: cardinal): pWordListNode;
    begin
      result := fRoot;
      while result <> nil do with result^ do begin
        if aWord = w then exit;
        result := aLink;
      end;
    end;

    procedure tWordSearch.AddNode(wadr: word; name: string; typ: nodetype);
    var newp: pWordListNode;
    begin
      Pointer(newP) := new(PWordListNode);
      with NewP^ do begin
        aName:=Name;
        aWord:=wadr;
        aTimes:=0;
        //Actor:=anActor;
        atype:=typ;
        aLink:=Froot;
        fRoot:= newp;
      end;
    end;

    {tWordListNode}
    Procedure tWordListNode.Compile;
    begin

    end;

    procedure tWordListNode.Resolve;
    begin

    end;

    procedure t4thCPU.IncludeText(nam: string);
    var incfile: text; aLine: shortstring;
    begin
      assign(incfile,nam);
      reset(incfile);
      while not eof(incfile) do begin
        readln(aline);
        StrEval(aline);
      end;
      close(incfile);
    end;

    procedure t4thCPU.StrEval(st: SHortstring);
    begin
    end;

    procedure t4thCPU.InitBaseOperations;
    begin
      SEARCH_EXEC := @SEARCH_EXEC_INT;
      {
      '(JMP',  'XR',   'PUSH', '-/',
      '(;',    'XA',   'POP',  '+*',
      '(IF',   'DUP',  '!R+',  '+2/',
      '(IF-',  'J',    '@R+',  'NAND',
      '(TR0;', '(TR1;','(BRK;','(ESC;',

      OpArr[ 0] := @jump; OpArr[ 1] := @xr;   OpArr[ 2] := @push; OpArr[ 3] := @SDiv;
      OpArr[ 4] := @ret;  OpArr[ 5] := @xa;   OpArr[ 6] := @pop;  OpArr[ 7] := @PMul;
      OpArr[ 8] := @_if;  OpArr[ 9] := @DUP;  OpArr[10] := @rstp; OpArr[11] := @add2div;
      OpArr[12] := @ifm;  OpArr[13] := @j;    OpArr[14] := @rldp; OpArr[15] := @nand;

      OpArr[16] := @troff;OpArr[17] := @tron; OpArr[19] := @brk;   OpArr[19] := @EscX;
      }

      {extendedOps
         defopx(@brk, 'BRK');              defopx(@comma, ',');
         defopx(@key, 'KEY');              defopx(@emit, 'EMIT');
         defopx(@readLine, 'READLINE');
         defopx(@NewItem, ':=');           defopx(@Parsing, 'WORD');
         }
         //defopx(@find, 'FIND');
         //procedure NewItem;     procedure Parsing;
         //procedure words;
         //defopx(@bye, 'BYE');
         //defopx(@commab, 'C,');
         //defopx(@key_pressed, 'key?');
         //defopx(@nvld, 'NVLD');

    end;


initialization

  //cpu.Create( $10000);
  //cpu.InitBaseOperations;

finalization

  //cpu.Done;

end.


