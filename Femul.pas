unit Femul;

interface

  procedure  RET;   
  procedure l4emul(adr: word);

implementation

uses
  SysUtils,
  TypeUnit,
  ConstUnit,
  VarUnit,
  PrimUnit
  ;

  procedure bye;  begin   halt;  end;

  procedure StopEmul;  begin   xflag := true;  end;

  procedure emit;  begin  write(char(_drop));  end;

  procedure key;  var c: char;  begin read(c); _dup(byte(c));  end;

  procedure nvld;
  begin
    writeln;
    writeln('Invalid subcode');
    readln;
  end;

  const
    ExtendedOps : ProcArr = (
      bye,  StopEmul, emit, key,  nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,

      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld,
      nvld, nvld, nvld, nvld, nvld, nvld, nvld, nvld
    );

  procedure  RET;
  begin
    if  nibNum = 3 then begin
      if byte2(pcWord)[1] = $4c then begin  {escape codes 128}
        ExtendedOps[(pcWord shr 1) and 127];
        exit;
      end;
    end;
    pc := _pop and min2;
  end;

  const
    OpArr: OpArray =
      (
      jump,  xr,    push,  SDiv,
      ret,   xa,    pop,   PMul,
      _if,   DUP,   rstp,  add2div,
      ifm,   J,     rldp,  nand
      );



  procedure l4emul(adr: word);
  label nextCell;
  var shifter: word2;
  begin
    pc := adr and min2;  xflag := false;
  repeat   nextCell: pcWord :=  _Deek(pc); inc(pc,2);
    if not odd(pcWord) then begin  {nesting}
      _push(pc); pc :=  pcWord;    {goto nextCell;}
      continue;
    end;
    shifter[0] := pcWord and min2;
    nibNum := 3; {unpack cell to opcodes}
    repeat shifter[1] := 0;
      longint(shifter) := longint(shifter) shl 4;
      OpArr[OpCodes(shifter[1])];  {execute opcode}
      if shifter[1] and 3 = 0    then goto  nextCell;
      dec(nibNum);
    until nibNum > 3;
  until xflag;
  end;
  

end.
