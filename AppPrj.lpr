program AppPrj;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, AppUnit, BaseUnit
  { you can add units after this }
  ;

begin
  ReadList(slr,'E:\Install\Backup\Hitachi_320\links\tem-settion-tabs.txt');
  PackList(slr, slw);
  ReadList(slr,'E:\Install\Backup\Hitachi_320\links\tem-settion-tabs2.txt');
  PackList(slr, slw);
  ReadList(slr,'E:\Install\Backup\Hitachi_320\links\tem-settion-tabs3.txt');
  PackList(slr, slw);
  WriteList(slw,'E:\Install\Backup\Hitachi_320\links\tem-set.txt');
end.

