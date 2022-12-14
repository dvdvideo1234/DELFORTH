      {BASE control & COMPILER PRIMITIVES}
      t.defWord(0, 'ZERO');
      t.defWord($FF00, 'HIBYTE');
      strcomp(': (BR# @R+ XR (DROP (; : (#| @R+ XR PUSH (; : (# @R+ (;');
      strcomp(': (@& @R+ POP (DROP : AND NAND DUP NAND (;');
      strcomp(': I& J (JMP AND : ?SGN& DUP : SGN& (@&'); h.wcomp($8000);
      strcomp(': (-IF# ?SGN& : (BR#0 (IF (BR# : SKIP @R+ (DROP (;');
      strcomp(': (VAR3 @R+ @R+ NAND (DROP : (VAR POP (;');
      strcomp(': IXEC J PUSH (; : EXECUTE PUSH (; : EX POP XR PUSH (;' );
      strcomp(': |DROP EX : DROP (DROP : NOP (; ');
      strcomp(': 2-| (1- : 1-| (1- : XEP| XR EXECUTE POP (; ');
      strcomp(': TIMES PUSH XR : TIMES| PUSH (JMP 4  PUSH J'); {ITERATORS}
      strcomp('EXECUTE POP (IF- -6 : .EXIT (DROP : EXIT POP (DROP (;');
      strcomp(': (@+ @R+ +2/ (JMP .EXIT');
      strcomp(': STR| POP |DROP  PUSH (JMP TIMES|');

      {Fetch - Store}
      strcomp(': A@ XA POP DUP PUSH XA (; : A! PUSH XA (JMP EXIT');
      strcomp(': (FS XR PUSH J PUSH @R+ POP (DROP EX (JMP 2');
      strcomp(': (S1 @R+ (DROP : (! !R+ (JMP EXIT : 2DROP PUSH (JMP .EXIT');
      strcomp(': (DZ0 POP XR (; : (F1 @R+ (DROP : (@ @R+ (JMP EXIT');
      strcomp(': (DZ1 POP XR : 2+ (@+');   h.wcomp(2);
      strcomp(': (DZ2 POP XR : 4+ (@+');   h.wcomp(4);

      {BASE STACK & MATH  OPS}
      strcomp(': 2- (1- : 1- (1- (; : DEC (FS (JMP 1-');
      strcomp(': INC (FS : 1+ DUP NAND  : NEG (1- : NOT DUP NAND (;');
      strcomp(': ?1+ (JMP 1+ : (?#+ DUP : (#+ @R+ (JMP 4 : (?#- DUP @R+ ');
      strcomp(': - NEG : + +2/ (DROP  (; : 2* DUP (JMP +');
      strcomp(': OR PUSH DUP I& - POP (JMP +');
      strcomp(': 2/ (NUM 0 : AVG  +2/ : NIP PUSH (DROP POP (;');
      strcomp(': |SWAP EX : SWAP PUSH XR POP (; ');

      {BASE MEM OPS}
      strcomp(': CSTR |SWAP  ?1+ XEP| (JMP 2');
      strcomp(': C@I J : C@ PUSH @R+ POP (DROP : W>B (@&');   h.wcomp(255);
      strcomp(': W>HB (@&');   h.wcomp($FF00);
      strcomp(': STR- |SWAP : @- 2-| : @I J : @ PUSH (JMP (@');
      strcomp(': C! PUSH W>B @I W>HB +2/ (DROP (JMP (! ');
      strcomp(': !- 2-| : !I J : ! PUSH  (JMP (! ');
      strcomp(': STR |SWAP : @+ PUSH @R+ POP (; : !+ PUSH !R+ POP (; ');


      {BASE HOST OPS}
      strcomp(': XTRON PUSH (TR1;  : TRON (TR1; ');
      strcomp(': TRACE XTRON     : TROFF (TR0; : (BK (NUM 1 (ESC;');
      strcomp(': (BE (NUM 0 : (ESC (ESC; : 1STEP EXECUTE : BRK (BRK');


      {CONSTANTS}
      //strcomp(': 0 (@'); h.wcomp(0);
      //strcomp(': -1 (@'); h.wcomp(WORD(-1));
      //strcomp(': BL (@'); h.wcomp(32);
      // strcomp(': 1 (@'); h.wcomp(1);

      {BASE MUL/DIV OPS}
      strcomp(': U* |DROP'); T.defWord(HERE+6,'`(8*');
      strcomp(': UM* A! (NUM 0 `(8* +* +* +* +*  +* +* +* +* (;');
      T.defWord(HERE+8,'`(8/');
      strcomp(': U/ |DROP : UM/MOD A! (NUM 0 : (U/ `(8/ -/ -/ -/ -/ -/ -/ -/ -/ (;');

      {BASE IO}
      strcomp(': STDEMT! (# (BE : EMIT! (S1 : EMIT@ (@ : EMIT (BE ');
      T.defWord(HERE+6,'CNTC');   strcomp('CNTC 1+ (S1 (@  (NOP');
      strcomp(': CR (NUM 13 EMIT (NUM 10 EMIT (NUM 0 (JMP -18');
      strcomp(': STDKEY!  (# (BK : KEY! (S1 : KEY@ (@ : KEY (BK (;');


      //strcomp(': TEST'); // for test := 15 downto 0 do h.PutNibble(test);

