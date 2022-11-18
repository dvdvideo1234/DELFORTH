: (BR# @R+ XR (IF (; : (#| @R+ XR PUSH (; : (# @R+ ;    \ (DROP == (IF
: (@& @R+ POP (IF : AND NAND DUP NAND ;
: I& J  AND ; : ?SGN& DUP : SGN& (@& ,< $8000 ,
: (-IF# ?SGN& : (BR#? <'> (BR# UNTIL : SKIP @R+ (IF ; \ (IF (BR#
: (VAR3 @R+ @R+ NAND (IF : (VAR POP ;
: IXEC J PUSH (; : EXECUTE PUSH (; : EX POP XR PUSH ;
: |DROP EX : DROP (IF : NOP ;
: 2-| (IF- : 1-| (IF- : XEP| XR EXECUTE POP ;
: TIMES PUSH XR : TIMES| PUSH AHEAD BEGIN  PUSH J
EXECUTE THEN POP NEXT- : .EXIT POP : 2DROP NAND (IF ;
: (@+ @R+ +2/  .EXIT ;
: STR| POP |DROP  PUSH TIMES| ;
: A@ XA POP DUP PUSH XA (; : A! PUSH XA POP (IF ;   \
: (FS XR PUSH J PUSH @R+ POP (IF EX AHEAD
: (S1 @R+ (IF : (! THEN !R+  POP (IF ;
: (DZ0 POP XR (; : (F1 @R+ (IF : (@ @R+ POP (IF ;
: (DZ1 POP XR (@+ ,<  2 ,
: (DZ2 POP XR (@+ ,<  4 ,
: DEC (FS : 1- (IF- (; : NEG (IF- : NOT DUP NAND ;
: INC (FS : 1+ (@+ ,<  1 ,
: (?#+ DUP : (#+ @R+ AHEAD : (?#- DUP @R+
: - NEG : + THEN +2/ (IF  (; : 2* DUP  + ;
: OR PUSH DUP I& - POP  + ;
: 2/  0 : AVG  +2/ : NIP PUSH (IF POP (;
: |SWAP EX : SWAP PUSH XR POP ;
: C@I J : C@ PUSH @R+ POP (IF : W>B (@& ,<  255 ,
: W>HB (@& ,<  $FF00 ,
: STR- |SWAP : @- 2-| : @I J : @ PUSH (@ ;
: C! PUSH W>B @I W>HB +2/ (IF  (! ;
: !- 2-| : !I J : ! PUSH (! ;
: STR |SWAP : @+ PUSH @R+ POP (; : !+ PUSH !R+ POP ;
: CSTR PUSH @R+ POP (IF- PUSH 255 NAND  DUP NAND XR POP ;
: XTRON PUSH (TR1;  : TRON (TR1;
: TRACE XTRON     : TROFF (TR0; : (BK  1 (ESC;
: (BE  0 : (ESC (ESC; ; : 1STEP EXECUTE : BRK (BRK
: 0 (@ ,<  0 ,
: U* |DROP
: UM* A! 0 TWICE +* +* +* +*  +* +* +* +* ;
: U/ |DROP : UM/MOD A! 0 : (U/ TWICE -/ -/ -/ -/ -/ -/ -/ -/ ;
: EMIT! (# (BE (S1 (@ : EMIT (BE  CALL 1+ (S1 THEN ,<  0 CONST CNTC
: CR  13 EMIT  10 EMIT  0 TO  CNTC ;
: KEY!  (# (BK (S1 (@ : KEY (BK (;



