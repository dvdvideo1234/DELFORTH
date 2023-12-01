$8000 =: SGNMSK#  2 =: TWO#  4 =: FOUR#  $FF00 =: HIMASK#
0     =: ZERO#    1 =: ONE#

: (# @R+ ; : IXEC J  : EXECUTE PUSH (;
: 2-| (IF-     : 1-| (IF-   : XEP| XR EXECUTE POP ;
: EX POP XR PUSH (;  : (#| @R+ XR PUSH ;
: 2/  0 : AVG  +2/ : NIP PUSH (IF POP (;
: STR PUSH @R+ AHEAD        : OVER PUSH DUP AHEAD
: |SWAP EX : SWAP PUSH  THEN THEN XR POP ;
: NEG (IF- : NOT DUP NAND ;
: I& J AHEAD : (#& @R+ AHEAD : (@& @R+ RDROP THEN THEN : AND NAND DUP NAND ;
: ?SGN& DUP : SGN& (@& SGNMSK#  : ?ODD DUP : ODD (@& ONE#
: (BR# BEGIN @R+ XR (IF ;   : (-IF# ?SGN&  : (BR#? UNTIL : SKIP RSKIP ;
: ?1+ DUP  : 1+ 1 AHEAD
: (#+ @R+ AHEAD : OVER- OVER : - NEG THEN THEN : + +2/ (IF  ; : 2* DUP  + ;
: (WARY 2* : (BARY POP  + ;
: |DROP EX            : DROP (IF  : NOP ;  : |2DROP EX   : 2DROP NAND DROP ;
: STR| POP |DROP  PUSH AHEAD
: TIMES PUSH XR THEN FOR PUSH J EXECUTE : `1 NEXT : .EXIT (IF : EXIT RDROP ;
: (@+ @R+ +2/  .EXIT ;      : A! PUSH XA EXIT ;  : A@ XA POP DUP PUSH XA (;
: U* |DROP : UM* A! 0 TWICE +* +* +* +*  +* +* +* +* ;
: U/ |DROP : UM/MOD A! 0 : (U/ TWICE -/ -/ -/ -/ -/ -/ -/ -/ ;
: (F1 RSKIP : (@ @R+ EXIT ;
: (FS XR PUSH J PUSH @R+ RDROP EX AHEAD
: (P1 POP XR : (S1 RSKIP : (! THEN !R+  EXIT ;
: STR- |SWAP : @- 2-| : @I J : @ PUSH (@ ;
: 2- (IF- : 1- (IF- ;

: XTRON PUSH (TR1;  : TRON (TR1;
: TRACE XTRON     : TROFF (TR0;
: 1STEP EXECUTE : BRK (BRK
: (@ESC @R+ RDROP : (ESC (ESC;

: (BE  (@ESC ZERO#
: (BK  (@ESC ONE#
: (BK? (@ESC  ,< ' TWO# ,
: (ATXY (@ESC ,< 3 ,
: (CLS (@ESC ,< ' FOUR#  ,
: (FOP (@ESC  ,< 5 ,
: (FCLS (@ESC ,< 6 ,
: (FRD  (@ESC ,< 7 ,
: (FWR  (@ESC ,< 8 ,
: (FPOS (@ESC ,< 9 ,

: (DZ2 POP XR : 4+ 4 + ;
: (DZ1 POP XR : 2+ 2 + ;
: (DZ0 POP XR ;
: (VAR2 @R+ @R+ NAND (IF    : (VAR POP ;

: XOR! (FS : XOR PUSH DUP PUSH DUP NAND J NAND POP POP AHEAD
: OR! (FS : OR PUSH DUP NAND POP THEN DUP NAND NAND ;
: +! (FS + ;
: AND! (FS AND ;
: DEC (FS 1- ;
: INC (FS 1+ ;


\ IO  &  VARS

: STDEMT! (# (BE ,<
' (BE VALUE EMIT@  HERE 2 - =: EMIT >, CALL 1+ (S1 THEN ,<
' ZERO# CONST CNTC
: STDKEY!  (# (BK ,<
' (BK VALUE KEY@   HERE 2 - =: KEY >, ;;

: H| CALL EX (S1 THEN ,< 0 CONST H
: T| CALL EX (S1 THEN ,< 0 CONST T
\  0 POINT DOER

: CSTR |SWAP : C@+ ?1+ XEP| : C@ PUSH @R+ RDROP : W>B (@& ,<  255 ,
: W>HB (@& ,<  ' HIMASK# ,
: ", CSTR : (", DUP CALL STR|  THEN
: C, H|
: C!+ ?1+ XEP| : C! PUSH W>B @I W>HB +2/ (IF  (! ;
: CSTR- |SWAP 1-| : C@I J C@ ;
: C!- 1-| : C!I J C! ;
: !- 2-| : !I J : ! PUSH (! ;
: , H|
: !+ PUSH !R+ POP ;





