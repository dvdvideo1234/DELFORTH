unit ConstUnit;

interface
uses
  TypeUnit;

const
  min2 = $fffe;

  opNames : atrOps = (
    '(JMP', 'XR',  'PUSH', '-/',
    '(;',   'XA',  'POP',  '+*',
    '(IF',  'DUP', '!R+',  '+2/',
    '(IF-', 'J',   '@R+',  'NAND'
    );

implementation

end.
