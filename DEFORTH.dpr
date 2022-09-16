program DEFORTH;  {$APPTYPE CONSOLE}

uses
  SysUtils,
  Femul in 'Femul.pas',
  TypeUnit in 'TypeUnit.pas',
  ConstUnit in 'ConstUnit.pas',
  VarUnit in 'VarUnit.pas',
  PrimUnit in 'PrimUnit.pas',
  TestOpUn in 'TestOpUn.pas';

begin
  { TODO -oUser -cConsole Main : Insert code here }
  TestAllOps;
  WRIteln(1);
  readln;
end.
