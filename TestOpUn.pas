unit TestOpUn;

interface

  procedure TestAllOps;
  procedure Testjump;
  procedure Test_if;
  procedure Testifm;
  procedure Testrstp;
  procedure Testrldp;
  procedure Testxr;
  procedure Testxa;
  procedure Testpush;
  procedure Testj;
  procedure Testdup;
  procedure Testpop;
  procedure Testnand;
  procedure Testadd2div;
  procedure TestPMul;
  procedure TestSDiv;
  procedure TestFindPrim;
  procedure TestRet;


  implementation

uses
  SysUtils,
  TypeUnit,
  ConstUnit,
  VarUnit,
  PrimUnit,
  Femul
  ;

var
  oldrsp, olddsp: word;
  opCode: OpCodes;

  procedure getStack(op: OpCodes);
  begin
    oldrsp  := rsp;
    olddsp  := dsp;
    opCode  := op;
  end;

  procedure TestOp(f: boolean);
  begin
    if f then
      writeln('Test ',opNames[opCode],' ',nibNum,' ',shiftreg[1]);
  end;

  procedure TestStack;
  begin
   if olddsp <> dsp
     then writeln('TestDsp ',opNames[opCode]);
   if oldrsp <> rsp
     then writeln('TestRsp ',opNames[opCode]);
  end;

  procedure Testpush;
  begin  getStack(Pushop);    _dup($5a5a); push;
    TestOp(_pop <> $5a5a);
    TestStack;
  end;

  procedure Testdup;
  begin  getStack(dupop);  _dup($5a5a); dup;
    TestOp(_drop <> _drop);
    TestStack;
  end;

  procedure Testpop;
  begin   getStack(poPop);     _push($5a5a); pop;
    TestOp(_drop <> $5a5a);
    TestStack;
  end;


  procedure TestRet;
  begin

  end;

  procedure Testjump;
  begin    getStack(jumpOp);
    pc := $100; pcword := $246; nibNum := 3; jump;
    TestOp(pc<>$346);
    pc := $100;  nibNum := 2; jump;
    TestOp(pc<>$146);
    pc := $100;  nibNum := 1; jump;
    TestOp(pc<>$106);
    pc := $100;  nibNum := 0; jump;
    TestOp(pc<>$100);

    pc := $246; pcword := $f00; nibNum := 3; jump;
    TestOp(pc<>$146);
    pc := $246; pcword := $ff0; nibNum := 2; jump;
    TestOp(pc<>$236);
    pc := $246; pcword := $ffe; nibNum := 1; jump;
    TestOp(pc<>$244);
    pc := $246; pcword := $100; nibNum := 0; jump;
    TestOp(pc<>$246);
    TestStack;
  end;

  procedure Test_if;
  begin    getStack(ifOp);
    _dup(0); pc := $100; pcword := $246; nibNum := 3; _if;
    TestOp(pc<>$346);
    _dup(0); pc := $100;  nibNum := 2; _if;
    TestOp(pc<>$146);
    _dup(0); pc := $100;  nibNum := 1; _if;
    TestOp(pc<>$106);
    _dup(0); pc := $100;  nibNum := 0; _if;
    TestOp(pc<>$100);

    _dup(1); pc := $246; pcword := $246; nibNum := 3; _if;
    TestOp(pc<>$246);
    _dup(1); pc := $246; pcword := $246; nibNum := 2; _if;
    TestOp(pc<>$246);
    _dup(1); pc := $246; pcword := $246; nibNum := 1; _if;
    TestOp(pc<>$246);
    _dup(1); pc := $246; pcword := $246; nibNum := 0; _if;
    TestOp(pc<>$246);
    TestStack;
  end;

  procedure Testifm;
  begin    getStack(ifmOp);
    _dup(3);
    pc := $100; pcword := $246; nibNum := 3; ifm;
    TestOp((pc<>$346) or(shiftreg[1]<>2));
    pc := $100;  nibNum := 2; ifm;
    TestOp((pc<>$146) or(shiftreg[1]<>1));
    pc := $100;  nibNum := 1; ifm;
    TestOp((pc<>$106) or(shiftreg[1]<>0));
    pc := $100;  nibNum := 0; ifm;
    TestOp((pc<>$100) or(shiftreg[1]<>$ffff));

    pc := $246; pcword := $246; nibNum := 3; ifm;
    TestOp(pc<>$246);
    pc := $246; pcword := $246; nibNum := 2; ifm;
    TestOp(pc<>$246);
    pc := $246; pcword := $246; nibNum := 1; ifm;
    TestOp(pc<>$246);
    pc := $246; pcword := $246; nibNum := 0; ifm;
    TestOp(pc<>$246);
    _drop;
    TestStack;
  end;

  procedure Testrstp;
  const adr = 100;
  begin   getStack(rstpOp);  _dup($5a5a);   _push(adr); rstp;
    TestOp((_deek(adr) <> $5a5a) or (rtop <> adr+2));
    _pop;
    TestStack;
  end;

  procedure Testrldp;
  const adr = 100;
  begin   getStack(rldpOp);  _doke(adr,$5a5a);   _push(adr); rldp;
    TestOp((_drop <> $5a5a) or (rtop <> adr+2));
    _pop;
    TestStack;
  end;

  procedure Testxr;
  begin   getStack(xrOp); _dup( $5a5a); _push(1234);  xr;
    TestOp((shiftreg[1] <> 1234) or (rtop <> $5a5a));
    _pop; _drop;
    TestStack;
  end;

  procedure Testxa;
  begin   getStack(xaOp); areg := $5a5a; _push(1234);  xa;
    TestOp((areg <> 1234) or (rtop <> $5a5a));
    _pop;
    TestStack;
  end;

  procedure Testj;
  begin   getStack(jOp);    _push(1234);    _push( $5a5a);   j;
    TestOp((shiftreg[1] <> 1234) or (rtop <> $5a5a) );
    _pop; _drop; _pop;
    TestStack;
  end;

  procedure Testnand;
  var temp: word;
  begin   getStack(nandOp);    _dup(4);    _dup( $6);
    temp := not (shiftreg[1] and shiftreg[0]);   nand  ;
    TestOp(shiftreg[1] <> temp);
    _drop;
    TestStack;
  end;

  procedure Testadd2div;
  var temp, temp2: word; fa: boolean;
  begin   getStack(a2dOp);    _dup($fff4); _dup($16);
    temp := shiftreg[0]; fa := _incw(temp,shiftreg[1]);
    temp2:= temp  shr 1;     if fa then inc(temp2,$8000);  add2div;
    TestOp((shiftreg[0] <> temp) or (shiftreg[1] <> temp2));
    
    shiftreg[1] := ($fff4); shiftreg[0] := ( $6);
    temp := shiftreg[0]; fa := _incw(temp,shiftreg[1]);
    temp2:= temp  shr 1;     if fa then inc(temp2,$8000);  add2div;
    TestOp((shiftreg[0] <> temp) or (shiftreg[1] <> temp2));
    _drop;    _drop;
    TestStack;
  end;

  procedure TestPMul;          {dnext}
  var shifter: word2;  fa: boolean;
  begin   getStack(PMulOp);  _dup($fff1);  _dup( 0); areg := $ff16;
    longint(shifter) := longint(shiftreg);
    fa := odd(shifter[0]); if fa then fa := _incw(shifter[1], areg);
    longint(shifter)   := longint(shifter)  shr 1;
    if fa then inc(shifter[1],$8000);
    PMul;
    TestOp(longint(shifter) <> longint(shiftreg));
    PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;
    shifter[0] := $fff1;
    shifter[1] := $ff16;
    TestOp((shifter[1] * shifter[0]) <> longint(shiftreg));
    _drop;    _drop;
    TestStack;
  end;

  procedure TestSDiv;
  var shifter: word2;   fa: boolean;
  begin getStack(SDivOp);    _dup($fff4); _dup($b); areg := ( $16);
    longint(shifter) := longint(shiftreg)   shl 1;
    if shifter[1] >= areg then begin  _incw(shifter[1], (not areg) + 1);
      inc(shifter[0]);
    end;
    SDiv;    TestOp(longint(shifter) <> longint(shiftreg));
    SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;
    longint(shifter) := $bfff4 div areg;    shifter[1] := $bfff4 mod areg;
    TestOp(longint(shifter) <> longint(shiftreg));
    _drop;    _drop;
    TestStack;
  end;

  procedure TestFindPrim;  var i: OpCodes;
  begin    getStack(SDivOp); nibNum := 250;
    for i := jumpOp to nandOp do
      TestOp(FindPrim(opNames[i]) <> shortint(i));
    TestOp(FindPrim('Op') >= 0);
    TestStack;
  end;

  procedure TestAllOps;
  begin
   Testjump;
   Test_if;
   Testifm;
   Testrstp;
   Testrldp;
   Testxr;
   Testxa;
   Testpush;
   Testj;
   Testdup;
   Testpop;
   Testnand;
   Testadd2div;
   TestPMul;
   TestSDiv;
   TestFindPrim;
   TestRet;
  end;


end.
