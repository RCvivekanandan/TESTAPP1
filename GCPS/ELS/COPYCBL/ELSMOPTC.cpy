000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSMOPTC                                        *00030000
000400*    DATE:       17-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*                EDWARD G. LISS                                  *00060000
000700*    FUNCTION:   LIST OF SELECTION CODES AND KEYWORD VALID FOR   *00070000
000800*                THE CURRENT MENU.                               *00080000
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 17-SEP-1986 RJL       CREATED                            *00160000
001700*                   EGL                                          *00170000
001710* 01.01 10-OCT-1986 RJL       ADDED INFORMATION TO INDICATE      *00171000
001800*                             USABLE LENGTH OF MSO-OPT-SEL AND   *00180000
001810*                             WHETHER VALUE IS ALPHABETIC OR     *00181000
001820*                             NUMERIC.                           *00182000
001821* 01.02 20-OCT-1986 RJL       ADD GROUP LEVEL FOR FIXED PORTION  *00182100
001830*                             OF RECORD.                         *00183000
001831* 01.03 23-OCT-1986 RJL       CORRECT TYPOGRAPHICAL ERROR.       *00183100
      * 01.04 21-SEP-1986 LET       CHANGES REQUESTED BY ELG:          *00183200
      *                             1.  ADD MSO MINIMUM AND MAXIMUM    *00183300
      *                                 NUMBERS CHOICES.               *00183400
001840*                                                                *00184000
001900******************************************************************00190000
002000                                                                  00200000
002100 01  MSO-MENU-SELECTION-VALUES.                                   00210000
002110     02 MSO-MENU-OPTS-HEADER.                                     00211000
002200        03 MSO-NBR-MENU-OPTS     PICTURE S9(04)          COMP.    00220000
002201        03 MSO-OPT-LEN           PICTURE S9(04)          COMP.    00220100
002210        03 MSO-OPT-TYP           PICTURE  X(01).                  00221000
002220           88 MSO-OPT-TYP-AN     VALUE 'X'.                       00222000
002230           88 MSO-OPT-TYP-NUM    VALUE '9'.                       00223000
              03 MSO-MIN-CHOICES       PICTURE S9(04)          COMP.    00225000
              03 MSO-MAX-CHOICES       PICTURE S9(04)          COMP.    00226000
                 88 MSO-MAX-CHOICES-VALUE      VALUE +30.               00227000
002300     02 MSO-MENU-OPTS.                                            00230000
002400        03 MSO-MENU-OPT          OCCURS 1 TO 750 TIMES            00240000
002500                                 DEPENDING ON MSO-NBR-MENU-OPTS   00250000
002600                                 INDEXED BY MSO-IDX.              00260000
002700           04 MSO-OPT-SEL        PICTURE  X(05).                  00270000
002710           04 FILLER             REDEFINES MSO-OPT-SEL.           00271000
002720              05 MSO-OPT-NUM-1   PICTURE  9(01).                  00272000
002730              05 FILLER          PICTURE  X(04).                  00273000
002740           04 FILLER             REDEFINES MSO-OPT-SEL.           00274000
002750              05 MSO-OPT-NUM-2   PICTURE  9(02).                  00275000
002760              05 FILLER          PICTURE  X(03).                  00276000
002770           04 FILLER             REDEFINES MSO-OPT-SEL.           00277000
002780              05 MSO-OPT-NUM-3   PICTURE  9(03).                  00278000
002790              05 FILLER          PICTURE  X(02).                  00279000
002791           04 FILLER             REDEFINES MSO-OPT-SEL.           00279100
002792              05 MSO-OPT-NUM-4   PICTURE  9(04).                  00279200
002793              05 FILLER          PICTURE  X(01).                  00279300
002794           04 FILLER             REDEFINES MSO-OPT-SEL.           00279400
002795              05 MSO-OPT-NUM-5   PICTURE  9(05).                  00279500
002800           04 MSO-OPT-KWD        PICTURE  X(16).                  00280000
