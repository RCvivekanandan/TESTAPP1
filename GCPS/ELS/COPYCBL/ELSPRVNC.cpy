000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSPRVNC                                        *00030000
000400*    DATE:       12-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   PROVISION LIST FOR COVERAGE DETERMINATION AND   *00060000
000700*                PAYMENT LEVEL GROUPING SUBPROGRAMS.             *00070000
000800*                                                                *00080000
000900******************************************************************00090000
001000*                                                                *00100000
001100*                      MAINTENANCE HISTORY                       *00110000
001200*                                                                *00120000
001300*  MOD     DATE     BY  DRPT                ACTION               *00130000
001400* ----- ----------- --- ----- ---------------------------------- *00140000
001500* 01.00 12-SEP-1986 RJL       CREATED                            *00150000
001510* 01.01 29-SEP-1986 RJL       CORRECTED MISSING PERIOD.          *00151000
001600* 01.02 21-APR-1988 LET       AS REQUESTED BY REB: CHANGED FILLER*00152000
001600*                             TO AN INDICATOR THAT SPECIFIES     *00152100
001600*                             IF TO SEARCH FURTHER ON ECF PVE.   *00152200
001600*                                                                *00152300
001700******************************************************************00153000
001800                                                                  00154000
001900 01  PVN-BENEFIT-PROVISION-LIST.                                  00155000
002000     02 PVN-FIXED-PART.                                           00156000
002100        03 PVN-NBR-BEN-PROVN     PICTURE S9(04)          COMP.    00157000
002200        03 PVN-COVERAGE-IND      PICTURE  X(01).                  00158000
002300           88 PVN-COVG-FULL      VALUE 'F'.                       00159000
002400           88 PVN-COVG-PART      VALUE 'P'.                       00160000
002500           88 PVN-COVG-NONE      VALUE 'N'.                       00170000
002600        03 PVN-PROCESSING-ECF-IND                                 00180000
002600                                 PICTURE  X(01).                  00181000
002600           88 FIND-ECF-PROV-ON-PVE        VALUE 'E'.              00182000
002700     02 PVN-BEN-PROVN-TABLE.                                      00190000
002800        03 PVN-BEN-PROVN-TBL     OCCURS 1 TO 50 TIMES             00200000
002900                                 DEPENDING ON PVN-NBR-BEN-PROVN   00210000
003000                                 INDEXED BY PVN-BEN-PROVN-IDX.    00220000
003100           04 PVN-BEN-PROVN-ID.                                   00230000
003200              05 PVN-BEN-ID      PICTURE  X(05).                  00240000
003300              05 PVN-BEN-FMT     PICTURE  X(01).                  00250000
003400           04 PVN-COVG-SAME-AS   PICTURE S9(04)          COMP.    00260000
003500           04 PVN-SLOT-NBR-BAS   PICTURE S9(07)          COMP-3.  00270000
003600           04 PVN-SLOT-NBR-SUP   PICTURE S9(07)          COMP-3.  00280000
