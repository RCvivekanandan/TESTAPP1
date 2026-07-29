000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSSLNRC                                        *00030003
000400*    DATE:       07-SEP-1993                                     *00040003
000500*    AUTHOR:     ANNE KEFFER-KING                                *00050003
000600*    FUNCTION:   CONTROL BLOCKS FOR HOLDING SLOT NUMBERS         *00060003
000700*                REQUIRED OR RE-REQUIRED BY THE SELECTION        *00070003
      *                PROCESS.                                         00080003
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 07-SEP-1991 AKK       CREATED.  THIS FIRST PASS WILL     *00160003
      *                             INCLUDE THE GVL SLOT NUMBER AND    *00170003
      *                             THE GCCP TABULAR SLOT NUMBERS WITH *00180003
      *                             ALLOWANCE FOR UP TO A TOTAL OF 60  *00190003
      *                             TABULAR ID S AND SLOT NUMBER PAIRS.*00200003
005920*                                                                *00390901
006000******************************************************************00391000
006100                                                                  00400000
006200 01  SLN-SELECTION-SLOT-NUMBERS.                                  00410003
006300      05 SLN-CLDR-SLOT-NMBRS.                                     00411004
009300         10  SLN-CLDR-ID      PIC X(06).                          00412107
9400           10  SLN-CLDR-SLOT-NO PIC S9(07) COMP-3.                  00413007
                                                                        00415004
            05 SLN-WRK-FL-CRTD-SW   PIC X(01).                          00416007
               88  SLN-GVL-WRK-FL-CRTD     VALUE 'Y'.                   00417007
               88  SLN-GVL-WRK-FL-NOT-CRTD VALUE 'N' LOW-VALUES.        00418008
                                                                        00419007
            05 FILLER               PIC X(19).                          00419109
006300      05 SLN-GVL-SLOT-NMBRS.                                      00420003
009300         10  SLN-GVLF-ID      PIC X(06).                          00700007
9400           10  SLN-GVLF-SLOT-NO PIC S9(07) COMP-3.                  00710007
                                                                        00720007
009300         10  SLN-GVLG-ID      PIC X(06).                          00721007
9400           10  SLN-GVLG-SLOT-NO PIC S9(07) COMP-3.                  00740007
                                                                        00750007
009300         10  SLN-GVLH-ID      PIC X(06).                          00751007
9400           10  SLN-GVLH-SLOT-NO PIC S9(07) COMP-3.                  00770007
                                                                        00780007
009300         10  SLN-GVLP-ID      PIC X(06).                          00781007
9400           10  SLN-GVLP-SLOT-NO PIC S9(07) COMP-3.                  00800007
                                                                        00810007
009300         10  SLN-GVLQ-ID      PIC X(06).                          00811007
9400           10  SLN-GVLQ-SLOT-NO PIC S9(07) COMP-3.                  00830007
                                                                        00840007
009300         10  SLN-GVLR-ID      PIC X(06).                          00841007
9400           10  SLN-GVLR-SLOT-NO PIC S9(07) COMP-3.                  00860007
            05 SLN-GVL-TABLE REDEFINES SLN-GVL-SLOT-NMBRS               00870010
                  OCCURS 6 TIMES                                        00871010
                      INDEXED BY SLN-IDX.                               00872010
               10  SLN-GVLX-ID      PIC X(06).                          00873010
9400           10  SLN-GVLX-SLOT-NO PIC S9(07) COMP-3.                  00874010
                                                                        00880004
006300      05 SLN-GCCP-SLOT-NMBRS.                                     00911004
009300         10  SLN-GCCP-ID      PIC X(06).                          00912007
9400           10  SLN-GCCP-SLOT-NO PIC S9(07) COMP-3.                  00913007
006300      05 FILLER               PIC X(520).                         00920005
