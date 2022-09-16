unit VarUnit;

interface
uses
  TypeUnit;

var
  pc, pcWord, rtop, areg: word;
  shiftreg: word2;
  rsp, dsp, nibNum: byte;
  xflag:  boolean;
  rstack, dstack: chipstack;
  adrSpc: array[word] of byte ;

implementation

end.
