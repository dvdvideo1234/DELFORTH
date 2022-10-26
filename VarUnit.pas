unit VarUnit;

interface
uses
  Classes, SysUtils
  ,BaseUnit
  ,TypeUnit;

type
  OpCodes = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp
      );

const
  extendedOps = 4;
  realRet = byte(retOp) shl nibble;
  opNames : array[OpCodes] of str5 = (
    '(JMP', 'XR',  'PUSH', '-/',
    '(;',   'XA',  'POP',  '+*',
    '(IF',  'DUP', '!R+',  '+2/',
    '(IF-', 'J',   '@R+',  'NAND'
    );

type
  objP = tobject;
  OpArray=  array of TProcType;

  t4thCPU = class (t4thMemory)
    areg : word;
    xflag : boolean;
    xdbg  : boolean;
    opArr : OpArray;

    Dstk : tWordStack;
    Rstk : tWordStack;

    procedure jump;            procedure _if;
    procedure ifm;             procedure rstp;
    procedure rldp;            procedure xr;
    procedure xa;              procedure push;
    procedure j;               procedure dup;
    procedure pop;             procedure nand;
    procedure add2div;         procedure PMul;
    procedure SDiv;            procedure RET;

    procedure comma;           procedure commab;
    procedure bye;             procedure brk;
    procedure emit;            procedure key;
    procedure nvld;            procedure key_pressed;
    procedure find;            procedure ACCEPT;

    procedure emul4(adr: word);
    procedure execute;

    destructor Done;
    constructor create(memSize: longint);

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
    begin
      if  (p.nib = 3) and  (p.last <> realRet) then begin
        p.last := pred((p.last xor realRet) shr 1);
        if  p.last > extendedOps then begin  {short literal -1018..1024}
          dstk.Push(p.last-1025);            {literals does not return}
          p.nib:= 0;
          exit;
        end else OpArr[p.last+15];  {execute Extended opcode}
      end;                         {end return}
      pc := rstk.pop;
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

    procedure t4thCPU.bye;  begin   halt;  end;

    procedure t4thCPU.brk;  begin   xflag := true;  end;

    procedure t4thCPU.emit;  begin  write(char(dstk.pop));  end;

    procedure t4thCPU.key;  var c: char;
    begin c := readkey; dstk.Push(byte(c));  end;

    procedure t4thCPU.key_pressed;
    begin dstk.Push(byte(keypressed));  end;

    procedure t4thCPU.nvld;
    begin
      writeln;
      writeln('Invalid subcode');
      readln;
    end;


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
         t.defWord(ind, opNames[OpCodes(ind)]);
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
     setlength( OpArr,26);
     t.ptr := high(mem.Bytes);
     t.defWord(0,'');      // end of vocabulary

     ind  := 0;
     here := 256;

     defop(@jump);     defop(@xr);     defop(@push);     defop(@SDiv);
     defop(@ret);      defop(@xa);     defop(@PMul);     defop(@PMul);
     defop(@_if);      defop(@DUP);    defop(@rstp);     defop(@add2div);
     defop(@ifm);      defop(@j);      defop(@rldp);     defop(@nand);

     {extendedOps   }
     defopx(@comma, ',');              defopx(@commab, 'C,');
     defopx(@brk, 'BRK');              defopx(@bye, 'BYE');
     defopx(@key, 'KEY');              defopx(@emit, 'EMIT');
     defopx(@find, 'FIND');            defopx(@ACCEPT, 'ACCEPT');
     defopx(@key_pressed, 'key?');     defopx(@nvld, 'NVLD');

   end;

   destructor t4thCPU.Done;
   begin
     OpArr := nil;
     inherited destroy;
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
     InitPc(adr);
     xflag := false;
     repeat
       execute;
     until xflag;
   end;

   procedure t4thCPU.comma;
   begin
     h.wcomp(dstk.pop);
   end;

   procedure t4thCPU.commab;
   begin
     h.bcomp(dstk.pop);
   end;

   procedure t4thCPU.find;
   var what, where: word;
   begin
     where := dstk.top;       what  := dstk.next;
     where := t.FindWord(where, pstr(@TBytesStream(mem).bytes[where])^);
     if where <> 0 then dstk.next := where;
     dstk.top := where;
   end;

   procedure t4thCPU.ACCEPT;
   var len, ind, where: word;  ch: char; c: byte absolute ch;
   begin
     where := dstk.next;     len   := dstk.top;     ind   := 0;
     repeat ch := readkey;
       case ch of
       #8: if len > 0 then begin  write(#8#32#8); dec(len); end;
       #9: ch := ' ';
       #13: break;
       else if ch >= ' ' then begin write(ch);
         TBytesStream(mem).bytes[where+ind] := c; inc(ind); end;
       end;
     until ind = len;
     dstk.top  := ind;
   end;

initialization

  cpu := t4thcpu.Create( $ffff);

finalization

  cpu.Done;

end.


