$8000 =: SGNMSK#  2 =: TWO#  4 =: FOUR#  $FF00 =: HIMASK#
0     =: ZERO#    1 =: ONE#

: EX POP XR    : EXECUTE PUSH (; : (# @R+ ;
: 2-| (IF-     : 1-| (IF-   : XEP| XR EXECUTE POP ;
: XTRON PUSH (TR1;  : TRON (TR1;
: TRACE XTRON     : TROFF (TR0;
: 1STEP EXECUTE : BRK (BRK
: (@ESC @R+ RDROP : (ESC (ESC;
: TIMES PUSH XR : TIMES| FOR PUSH J EXECUTE : `1 NEXT : .EXIT (IF : EXIT RDROP ;
: (@+ @R+ +2/  .EXIT ;      : A! PUSH XA EXIT ;
: I& J AHEAD : (#& @R+ AHEAD : (@& @R+ RDROP THEN THEN : AND NAND DUP NAND ;
: ?SGN& DUP : SGN& (@& SGNMSK#  : ?ODD DUP : ODD (@& ONE#
: (BR# BEGIN @R+ XR (IF ;   : (-IF# ?SGN& : (BR#? UNTIL : SKIP RSKIP ;
: (VAR2 @R+ @R+ NAND (IF    : (VAR POP ;
: 3DROP?; DUP IF
: |DROP EX                  : DROP (IF        : NOP ;    : 2DROP NAND DROP ;
   THEN 2DROP .EXIT ;
: A@ XA POP DUP PUSH XA (;  : STR| POP |DROP  PUSH TIMES| ;
: STR PUSH @R+ AHEAD        : OVER PUSH DUP AHEAD
: |SWAP EX : SWAP PUSH : XPOP THEN THEN XR POP ;
: NEG (IF- : NOT DUP NAND ; : ?1+ DUP  : 1+ 1 AHEAD
: (#+ @R+ AHEAD : OVER- OVER : - NEG THEN THEN : + +2/ (IF  ; : 2* DUP  + ;
: (DZ2 POP XR : 4+ 4 + ;
: (DZ1 POP XR : 2+ 2 + ;
: (DZ0 POP XR ;
: OR PUSH DUP I& -
: (BARY POP  + ;  : (WARY 2* (BARY ;
: 2/  0 : AVG  +2/ : NIP PUSH (IF POP (;
: (BE  (@ESC ZERO#
: (BK  (@ESC ONE#
: 2- (IF- : 1- (IF- ;


: (F1 RSKIP : (@ @R+ EXIT ;
: (FS XR PUSH J PUSH @R+ RDROP EX AHEAD
: (P1 POP XR : (S1 RSKIP : (! THEN !R+  EXIT ;
: DEC (FS 1- ;
: INC (FS 1+ ;

: CSTR |SWAP : C@+ ?1+ XEP| : C@ PUSH @R+ RDROP : W>B (@& ,<  255 ,
: W>HB (@& ,<  ' HIMASK# ,
: STR- |SWAP : @- 2-| : @I J : @ PUSH (@ ;
: C!+ ?1+ XEP| : C! PUSH W>B @I W>HB +2/ (IF  (! ;
: STDEMT! (# (BE ,<
' (BE VALUE EMIT@  HERE 2 - =: EMIT >, CALL 1+ (S1 THEN ,<
' ZERO# CONST CNTC
: STDKEY!  (# (BK ,<
' (BK VALUE KEY@   HERE 2 - =: KEY >, ;;

\ INCLUDE GAGA
\ : CR  13 EMIT  10 EMIT  0  TO CNTC ;
\ : U* |DROP : UM* A! 0 TWICE +* +* +* +*  +* +* +* +* ;
\ : U/ |DROP : UM/MOD A! 0 : (U/ TWICE -/ -/ -/ -/ -/ -/ -/ -/ ;
\ : (#| @R+ XR PUSH ;
\ : !+ PUSH !R+ POP ;
\ : IXEC J PUSH ;
\ : (BK? (@ESC ,< ' TWO# ,
\ : (ATXY (@ESC ,< 3 ,
\ : (FOP (@ESC ,< 3 ,
\ : (FCLS (@ESC ,< ' FOUR# ,
\ : (FRD  (@ESC ,< 5 ,
\ : (FWR  (@ESC ,< 6 ,
\ : (FPOS (@ESC ,< 7 ,
\ \ : (FMK  (@ESC ,< 8 ,



