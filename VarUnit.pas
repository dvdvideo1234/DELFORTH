unit VarUnit;

interface
uses
  Classes, SysUtils
  ,BaseUnit
  ,TypeUnit;

type
  //objP = tobject;
  OpArray=  array of TProcType;

  t4thCPU = object (t4thMemory)
    areg : word;
    xflag : boolean;
    xdbg  : boolean;
    opArr : OpArray;

    Dstk : tWordStack;
    Rstk : tWordStack;

    SEARCH_EXEC: TProcType;

    procedure jump;            procedure _if;
    procedure ifm;             procedure rstp;
    procedure rldp;            procedure xr;
    procedure xa;              procedure push;
    procedure j;               procedure dup;
    procedure pop;             procedure nand;
    procedure add2div;         procedure PMul;
    procedure SDiv;            procedure RET;

    procedure comma;
    procedure readLine;
    procedure brk;
    procedure NewItem;
    procedure Parsing;
    procedure emit;
    procedure key;

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
    //function  existed(msg: shortstring): word;
    procedure Eval_1;

    procedure emul4(adr: word);
    procedure execute;

    destructor Done;
    constructor create(memSize: longint);
    property Off: boolean read xflag write xflag;
    property Debug: boolean read xdbg write xdbg;

  end;

var
  cpu : t4thcpu;


implementation

uses
  crt;

  {
  ; (JUMP (; (IF (IF-   CONTROLS
     mandatory on exit  -->   nibNum:= 0;
  }

    procedure  t4thCPU.jump;
    begin
        if p.nib = 0 then exit;
        pc := pc + RelAdr(p.last, p.nib);
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
    var anum: word;
    begin
      if p.shift<> 0 then begin
        anum := pred(pred(wTest[p.nib]) and (p.last  shr 1));
        if anum < 8 then OpArr[anum or 16]  {execute Extended opcode}
        else begin                 {max real value of numbers}
          dstk.Push(anum-9);
          p.nib:= 0;  {0..118, 0..2038}
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
  ; push pop J DUP      STACK
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
    var temp: word;
    begin
      with dstk do begin
        temp := pop;
        top := not (temp and top);
      end;
    end;

    procedure  t4thCPU.add2div;
    var res: word;
    begin
      with dstk do begin
        res := ord(incw(next,top));
        top := next;
        DrcRw(res, top);
      end;
    end;

   procedure  t4thCPU.PMul;
   var fa: boolean;
   begin
     with dstk do begin
        fa := odd(next);
        if fa then fa := incw(top,areg);
        DrcRw(top,next,fa);
     end;
   end;

    procedure  t4thCPU.SDiv;
    begin
      with dstk do begin
        drclw(next,top);
        if top >= areg then begin
          dec(top,areg);
          inc(next);
        end;
      end;
    end;

    //procedure t4thCPU.bye;  begin   halt;  end;

    procedure t4thCPU.brk;  begin   xflag := true;  end;

    procedure t4thCPU.emit;  begin  write(char(dstk.pop));  end;

    procedure t4thCPU.key;  var c: char;
    begin c := readkey; dstk.Push(byte(c));  end;

    //procedure t4thCPU.key_pressed; begin dstk.Push(byte(keypressed));  end;

  //procedure t4thCPU.nvld;begin writeln;writeln('Invalid subcode');readln;end;


    {     defopx(@brk, 'BRK');              defopx(@bye, 'BYE');
     defopx(@key, 'KEY');              defopx(@emit, 'EMIT');
     defopx(@find, 'FIND');            defopx(@ACCEPT, 'ACCEPT');}


    constructor t4thCPU.create(memSize: longint);
       var ind: integer;
       procedure addWord(code: TProcType);
       begin
         opArr[ind] := code;
         inc(ind);
       end;

       procedure defop(code: TProcType);
       begin
         t.defWord(ind, opNames[ind]);
         addWord(code);
       end;

       procedure defopx(code: TProcType; name: string);
       begin
         t.defWord(here, name);
         dstk.push((succ(ind-16) shl 1)+realRet);
         comma;
         addWord(code);
       end;

   begin
     inherited create(memSize);
     setlength( OpArr,24);
     dict := size;     // t

     here := 256;      // h

     s.nameAdr  :=16;
     s.ptr      := 80;    // tib
     s.maxbuf   := here - s.ptr;

     initCpu;

     t.defWord(0,'');      // end of vocabulary

     ind  := 0;
     defop(@jump);     defop(@xr);     defop(@push);     defop(@SDiv);
     defop(@ret);      defop(@xa);     defop(@PMul);     defop(@PMul);
     defop(@_if);      defop(@DUP);    defop(@rstp);     defop(@add2div);
     defop(@ifm);      defop(@j);      defop(@rldp);     defop(@nand);

     {extendedOps   }
     defopx(@brk, 'BRK');              defopx(@comma, ',');
     defopx(@key, 'KEY');              defopx(@emit, 'EMIT');
     defopx(@readLine, 'READLINE');
     defopx(@NewItem, ':=');           defopx(@Parsing, 'WORD');

     //defopx(@find, 'FIND');
     //procedure NewItem;     procedure Parsing;
     //procedure words;
     //defopx(@bye, 'BYE');
     //defopx(@commab, 'C,');
     //defopx(@key_pressed, 'key?');
     //defopx(@nvld, 'NVLD');

   end;

   destructor t4thCPU.Done;
   begin
     OpArr := nil;
     inherited done;
   end;

   procedure t4thCPU.execute;
   begin
     if p.nib = 0 then
       if not p.nextWord then begin  { get next cell }
         rstk.push(pc); pc :=  p.last;  {nesting}
         exit;
       end;
     OpArr[p.getNibble]; {unpack & execute opcode}
   end;

   procedure t4thCPU.emul4(adr: word);
   begin
     InitPc(adr);   off := false;
     repeat
       execute;
     until off;
   end;

   procedure t4thCPU.comma;
   begin
     h.wcomp(dstk.pop);
   end;

   //procedure t4thCPU.commab; begin h.bcomp(dstk.pop); end;

   function t4thCPU.found: word;
   begin
     result := t.FindWord(dict, name);
   end;

   procedure t4thCPU.readLine;
   begin
     s.readLine;
   end;

   procedure t4thCPU.words;
   begin
     t.Words;
   end;

   procedure t4thCPU.NewItem;
   begin
     Parsing;
     if name = '' then error('Empty WOrd Name');
     if found <> 0 then warning('duplicates');
     t.defWord(dstk.pop, name);
   end;

   procedure t4thCPU.Parsing;
   begin
     s.Pars();
   end;

   procedure t4thCPU.Eval_1;
   begin
     Parsing;
     if name = '' then exit;
     SEARCH_EXEC;
       //readLine
   end;

   procedure t4thCPU.error(msg: shortstring);
   begin
     initCpu;
     writeln;
     writeln(name,':',msg);
     writeln;
   end;

   procedure t4thCPU.initCpu;
   begin
     SEARCH_EXEC := @SEARCH_EXEC_INT;
     off := true;
     Debug:=false;
     s.ltib := 0;   {buffer became empty}
   end;


   procedure t4thCPU.warning(msg: shortstring);
   begin write('[',name,']',msg,' ');   end;

   procedure t4thCPU.SEARCH_EXEC_INT;
   var dea: word;
   begin
     dea := found;
     if dea = 0 then error('Not found');
     exec(dea);
   end;

   procedure t4thCPU.SEARCH_EXEC_COMP;
   begin
   end;

   procedure t4thCPU.Colon;
   begin
     SEARCH_EXEC := @SEARCH_EXEC_COMP;
   end;

   procedure t4thCPU.semicolon;
   begin
     SEARCH_EXEC := @SEARCH_EXEC_INT;

   end;

   procedure t4thCPU.GetNumber;
   var n, err: word;
   begin
     val(name,n,err);
     if err <>0 then error('Not a number');
     dstk.Push(n);
   end;

   procedure t4thCPU.exec(dea: word);
   begin
     dstk.Push(dea);
     emul4($202); {execute word and return}
   end;


initialization

  cpu.Create( $ffff+1);

finalization

  cpu.Done;

end.


