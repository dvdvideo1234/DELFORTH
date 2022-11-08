unit TestOpUn;

interface
uses
   VarUnit
  ,TypeUnit
   ;

  type
    tTestCPU = object (t4thCPU)
      oldrsp, olddsp: word;
      opCode: tOpCode;

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

      procedure getStack(op: topcode);
      procedure TestOp(f: boolean);
      procedure TestStack;
      procedure mini(apc, alast, anib: word);
      function  shiftreg: longint;

    end;

var cpu: tTestCPU;

  implementation

uses
  SysUtils
  ,BaseUnit
  ;

  procedure tTestCPU.mini(apc, alast, anib: word);
  begin
    pc := apc;
    p.last := alast;
    p.nib := anib;
  end;

  function  tTestCPU.shiftreg: longint;
  begin
    with longrec(result) do begin
      hi := dstk.top;
      lo := dstk.next;
    end;
  end;

  procedure tTestCPU.getStack(op: topcode);
  begin
    oldrsp  := rstk.sp;
    olddsp  := dstk.sp;
    opCode  := op;
  end;

  procedure tTestCPU.TestOp(f: boolean);
  begin
    if f then
      writeln('Test ',opNames[ord(opCode)],' ',p.nib,' ',p.shift);
  end;

  procedure tTestCPU.TestStack;
  begin
   if olddsp <> dstk.sp
     then writeln('TestDsp ',opNames[ord(opCode)]);
   if oldrsp <> rstk.sp
     then writeln('TestRsp ',opNames[ord(opCode)]);
  end;

  procedure tTestCPU.Testpush;
  begin
    getStack(Pushop);
    with rstk do begin
      stackWords[sp-1] := 0;
      next := 1;
      top := 2;
      Push($5a5a);
      TestOp((stackWords[sp] <> 1) or
             (next<> 2) or
             (top <> $5a5a));
      inc(sp);
    end;
    TestStack;
  end;

  procedure tTestCPU.Testdup;  begin  Testpush;   end;

  procedure tTestCPU.Testpop;
  var res: word;
  begin   getStack(poPop);
    with rstk do begin
      stackWords[sp+1] := $5a5;
      stackWords[sp] := 0;
      next := 1;
      top := 2;
      res := pop;
      TestOp((stackWords[sp] <> $5a5) or
             (next<> 0) or
             (res<> 2) or
             (top <> 1));
      dec(sp);
    end;
    TestStack;
  end;


  procedure tTestCPU.TestRet;
  begin

  end;

  procedure tTestCPU.Testjump;
  begin
    getStack(jumpOp);
    mini($100, $246, 3); jump;    TestOp((pc<>$346) or (p.nib<>0));
    mini($100, $246, 2); jump;    TestOp((pc<>$146) or (p.nib<>0));
    mini($100, $246, 1); jump;    TestOp((pc<>$106) or (p.nib<>0));
    mini($100, $246, 0); jump;    TestOp((pc<>$100) or (p.nib<>0));

    mini($246, $f00, 3); jump;   TestOp(pc<>$146);
    mini($246, $ff0, 2); jump;   TestOp(pc<>$236);
    mini($246, $ffe, 1); jump;   TestOp(pc<>$244);
    mini($246, $100, 0); jump;   TestOp(pc<>$246);
    TestStack;
  end;

  procedure tTestCPU.Test_if;
  begin    getStack(ifOp);
    dstk.Push(0); mini($100, $246, 3); _if;   TestOp((pc<>$346) or (p.nib<>0));
    dstk.Push(0); mini($100, $246, 2); _if;   TestOp((pc<>$146) or (p.nib<>0));
    dstk.Push(0); mini($100, $246, 1); _if;   TestOp((pc<>$106) or (p.nib<>0));
    dstk.Push(0); mini($100, $246, 0); _if;   TestOp((pc<>$100) or (p.nib<>0));

    dstk.Push(1); mini($246, $246, 3); _if;   TestOp((pc<>$246) or (p.nib<>0));
    dstk.Push(1); mini($246, $246, 2); _if;   TestOp((pc<>$246) or (p.nib<>0));
    dstk.Push(1); mini($246, $246, 1); _if;   TestOp((pc<>$246) or (p.nib<>0));
    dstk.Push(1); mini($246, $246, 0); _if;   TestOp((pc<>$246) or (p.nib<>0));
    TestStack;
  end;

  procedure tTestCPU.Testifm;
  begin    getStack(ifmOp);
    dstk.Push(3);
    mini($100, $246, 3); ifm;    TestOp((pc<>$346) or (dstk.top<>2));
    mini($100, $246, 2); ifm;    TestOp((pc<>$146) or (dstk.top<>1));
    mini($100, $246, 1); ifm;    TestOp((pc<>$106) or (dstk.top<>0));
    mini($100, $246, 0); ifm;    TestOp((pc<>$100) or (dstk.top<>$ffff));

    mini($246, $246, 3); ifm;   TestOp((pc<>$246) or (dstk.top<>$fffe));
    mini($246, $246, 2); ifm;   TestOp((pc<>$246) or (dstk.top<>$fffd));
    mini($246, $246, 1); ifm;   TestOp((pc<>$246) or (dstk.top<>$fffc));
    mini($246, $246, 0); ifm;   TestOp((pc<>$246) or (dstk.top<>$fffb));
    dstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testrstp;
  const adr = $8000;
  begin   getStack(rstpOp);
   dstk.Push($5a5a);   rstk.Push(adr); p.store(adr,0); rstp;
    TestOp((p.fetch(adr) <> $5a5a) or (rstk.top <> adr+2));
    rstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testrldp;
  const adr = $8000;
  begin   getStack(rldpOp); p.store(adr,$5a5a);  rstk.Push(adr); rldp;
    TestOp((dstk.pop <> $5a5a) or (rstk.top <> adr+2));
    rstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testxr;
  begin   getStack(xrOp); dstk.Push( $5a5a); rstk.Push(1234);  xr;
    TestOp((dstk.top <> 1234) or (rstk.top <> $5a5a));
    dstk.pop;
    rstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testxa;
  begin   getStack(xaOp); areg := $5a5a; rstk.Push(1234);  xa;
    TestOp((areg <> 1234) or (rstk.top <> $5a5a));
    rstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testj;
  begin   getStack(jOp);    rstk.Push(1234);    rstk.Push( $5a5a);   j;
    TestOp((dstk.top <> 1234) or (rstk.top <> $5a5a) );
    rstk.pop;
    rstk.pop;
    dstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testnand;
  var temp: word;
  begin   getStack(nandOp);    dstk.Push(4);    dstk.Push( $6);
    temp := not (dstk.top and dstk.next);   nand  ;
    TestOp(dstk.top <> temp);
    dstk.pop;
    TestStack;
  end;

  procedure tTestCPU.Testadd2div;
  var temp, temp2: word; fa: boolean;
  begin   getStack(a2dOp);    dstk.Push($fff4); dstk.Push($16);
    temp := dstk.next; fa := incw(temp,dstk.top);
    temp2:= temp  shr 1;     if fa then inc(temp2,$8000);
    add2div;
    TestOp((dstk.next <> temp) or (dstk.top <> temp2));
    
    dstk.top := ($fff4); dstk.next := ( $6);
    temp := dstk.next; fa := incw(temp,dstk.top);
    temp2:= temp  shr 1;     if fa then inc(temp2,$8000);
    add2div;
    TestOp((dstk.next <> temp) or (dstk.top <> temp2));
    dstk.pop;
    dstk.pop;
    TestStack;
  end;

  procedure tTestCPU.TestPMul;          {dnext}
  var shifter: longint;  fa: boolean;
  begin   getStack(PMulOp);
    dstk.Push($fff1);  dstk.Push( 0); areg := $ff16;
    with longrec(shifter) do begin
       hi := dstk.top;
       lo := dstk.next;
       fa := odd(shifter); if fa then fa := incw(hi, areg);
       shifter   := shifter  shr 1;
       if fa then inc(hi,$8000);
       PMul;
       TestOp(shifter <> shiftreg);
       PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;PMul;
       cardinal(shifter) := $ff16fff1;
       TestOp(longint(lo * hi) <> shiftreg);
    end;
    dstk.pop;
    dstk.pop;
    TestStack;
  end;

  procedure tTestCPU.TestSDiv;
  var shifter: longint;
  begin getStack(SDivOp);    dstk.Push($fff4); dstk.Push($b); areg := ( $16);
    with longrec(shifter) do begin
      hi := dstk.top;
      lo := dstk.next;
      shifter := shiftreg   shl 1;
      if hi >= areg then begin
        incw(hi, (not areg) + 1);
        inc(lo);
      end;
       SDiv;    TestOp(shifter <> shiftreg);
       SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;SDiv;
       lo := $bfff4 div areg;    hi := $bfff4 mod areg;
    end;
    TestOp(shifter <> shiftreg);
    dstk.pop;
    dstk.pop;
    TestStack;
  end;
   //function searchByName(opname: shortstring): shortint;

  procedure tTestCPU.TestFindPrim;
  var i: integer;
  begin    getStack(SDivOp); //nibNum := 250;
    for i := 0 to ord(numx) do
      TestOp(searchByName(opNames[i]) <> shortint(i));
    TestOp(searchByName('Op') >= 0);
    TestStack;
  end;

  procedure tTestCPU.TestAllOps;
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

initialization

  cpu.Create( $10000);
  cpu.InitBaseOperations;

finalization

  cpu.Done;

end.
