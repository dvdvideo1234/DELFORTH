program emul4th;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, BaseUnit, TypeUnit, VarUnit, TestOpUn,CRT
  ;

begin
   //writeln(%111);
   //avg(12345,4321));
   //cpu.words;
  with cpu do begin
    da := 16;
    //dumpWords((t.tick('TEST')-da) div 2); //writeln;
    //cpu.InitBaseOperations;
    //words; writeln;
    debug := true;
    //doCall(16);
    {
    if p.nib <> 0 then execute;
    if p.nib <> 0 then execute;
    if p.nib <> 0 then execute;
    writeln(da);
    }

  end;

  //writeln(byte(readkey));
  readkey;
end.

