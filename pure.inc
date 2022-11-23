$8000 =: SGNMSK#  2 =: TWO#  4 =: FOUR#  $FF00 =: HIMASK#  0 =: ZERO#
: (@& @R+ POP (IF          : AND NAND DUP NAND ;          : I& J  AND ;
: (# @R+ ;     : ?SGN& DUP : SGN& (@& ,< $8000 ,
: EX POP XR    : EXECUTE PUSH (;
: 2-| (IF-     : 1-| (IF-   : XEP| XR EXECUTE POP ;
: STR PUSH @R+ AHEAD        : OVER PUSH DUP AHEAD
: |SWAP EX : SWAP PUSH : XPOP THEN THEN XR POP ;
: (BR# @R+ XR (IF ;   : (-IF# ?SGN& : (BR#? <'> (BR# UNTIL : SKIP @R+ (IF ;
: (VAR3 @R+ @R+ NAND (IF    : (VAR POP ;
: |DROP EX                  : DROP (IF        : NOP ;     : 2DROP NAND DROP ;
: TIMES PUSH XR
: TIMES| PUSH AHEAD BEGIN  PUSH J EXECUTE  THEN POP NEXT-
: .EXIT (IF    : EXIT POP (IF ;
: (@+ @R+ +2/  .EXIT ;      : A! PUSH XA EXIT ;
: A@ XA POP DUP PUSH XA (;  : STR| POP |DROP  PUSH TIMES| ;

: (FS XR PUSH J PUSH @R+ POP (IF EX AHEAD  : (S1 @R+ (IF THEN : (! !R+  EXIT ;
: (DZ0 POP XR (; : (F1 @R+ (IF : (@ @R+ EXIT ;
: (DZ1 POP XR : 2+ (@+ ,<  2 , : (DZ2 POP XR : 4+ (@+ ,<  4 ,
: 2- (IF- : 1- (IF- ; : DEC (FS 1- ;
: INC (FS : 1+ DUP NAND : NEG (IF- : NOT DUP NAND ; : ?1+ DUP 1+ ;

: (#+ @R+ AHEAD : - NEG THEN : + +2/ (IF  ; : 2* DUP  + ;
: OR PUSH DUP I& - POP  + ; : 2/  0 : AVG  +2/ : NIP PUSH (IF POP (;
: CSTR |SWAP : C@+ ?1+ XEP| : C@ PUSH @R+ POP (IF : W>B (@& ,<  255 ,
: W>HB (@& ,<  $FF00 , : !- 2-| : !I J : ! PUSH (! ;
: STR- |SWAP : @- 2-| : @I J : @ PUSH (@ ;
: C!+ ?1+ XEP| : C! PUSH W>B @I W>HB +2/ (IF  (! ;
: !+ PUSH !R+ POP ;
: XTRON PUSH (TR1;  : TRON (TR1;
: TRACE XTRON     : TROFF (TR0; : (BK  1 (ESC;
: (BE  0 : (ESC (ESC; ; : 1STEP EXECUTE : BRK (BRK
: U* |DROP : UM* A! 0 TWICE +* +* +* +*  +* +* +* +* ;
: U/ |DROP : UM/MOD A! 0 : (U/ TWICE -/ -/ -/ -/ -/ -/ -/ -/ ;
: STDEMT! (# (BE : EMIT! (S1 : EMIT@ (@
: EMIT (BE  CALL 1+ : CNTC! (S1 THEN ,<
0 CONST CNTC
: CR  13 EMIT  10 EMIT  0  TO CNTC ;
: STDKEY!  (# (BK : KEY! (S1 : KEY@ (@ : KEY (BK ;;

\ INCLUDE GAGA    : IXEC J PUSH (;  : (#| @R+ XR PUSH ;


