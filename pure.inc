
\ INCLUDE GAGA
\ : CR  13 EMIT  10 EMIT  0  TO CNTC ;
: (#| @R+ XR PUSH ;
: !+ PUSH !R+ POP ;
: IXEC J PUSH ;
: (BK? (@ESC ,< ' TWO# ,
: (ATXY (@ESC ,< 3 ,
: (FOP (@ESC ,< 3 ,
: (FCLS (@ESC ,< ' FOUR# ,
: (FRD  (@ESC ,< 5 ,
: (FWR  (@ESC ,< 6 ,
: (FPOS (@ESC ,< 7 ,
\ : (FMK  (@ESC ,< 8 ,
: CSTR- |SWAP 1-| : C@I J C@ ;
: C!- 1-| : C!I J C! ;
: CMOVE 3DROP?; ?ODD IF PUSH PUSH CSTR POP C!+ POP THEN 2/
: MOVE PUSH  A! XR AHEAD BEGIN @R+ XA !R+ XA THEN NEXT- .EXIT ;
: CMOVE> 3DROP?; ?ODD IF PUSH PUSH CSTR- POP C!- POP THEN 2/
 STR| PUSH STR- POP
: !- 2-| : !I J : ! PUSH (! ;
: "ALIGN CSTR +   : 2ALIGN ?ODD + ;
: ("SKP POP POP DUP "ALIGN PUSH PUSH XR ;
: (". ("SKP  : ". CSTR  : TYPE STR| CSTR EMIT ;
: (" ("SKP  ;;
: CR .""^M^J" ;

: U* |DROP : UM* A! 0 TWICE +* +* +* +*  +* +* +* +* ;
: U/ |DROP : UM/MOD A! 0 : (U/ TWICE -/ -/ -/ -/ -/ -/ -/ -/ ;

