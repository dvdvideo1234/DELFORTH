unit TypeUnit;

interface

type
  //nibble = 0..15;
  chipstack = array[byte] of word;
  byte2 = array[0..1] of byte;
  byte4 = array[0..3] of byte;
  wordp = ^word;
  word2 = array[0..1] of word;
  word4 = array[0..3] of word;
  str5  = string[5];
  OpCodes = (
    jumpOp,   xrOp,     pushOp,   SDivOp,
    retOp,    xaOp,     popOp,    PMulOp,
    ifOp,     DUPOp,    rstpOp,   a2dOp,
    ifmOp,    JOp,      rldpOp,   nandOp
      );

  atrOps= array[OpCodes] of str5;
  ProcArr= array[0..127]  of procedure;
  PrArPtr= ^ProcArr;
  OpArray=  array[OpCodes]  of procedure;


implementation

end.
