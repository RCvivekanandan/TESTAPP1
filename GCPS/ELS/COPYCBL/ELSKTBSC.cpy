000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSKTBSC                                        *00030000
000400*    DATE:       16-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   TABLE OF SECTIONS FOR A GIVEN GROUP             *00060000
000700*                                                                *00070000
000800******************************************************************00080000
000900*                                                                *00090000
001000*                      MAINTENANCE HISTORY                       *00100000
001100*                                                                *00110000
001200*  MOD     DATE     BY  DRPT                ACTION               *00120000
001300* ----- ----------- --- ----- ---------------------------------- *00130000
001400* 01.00 16-SEP-1986 RJL       CREATED                            *00140000
001410* 01.01 16-SEP-1986 RJL       CORRECTED SIZE OF SECTION NUMBER.  *00141001
001420* 01.02 17-NOV-1986 RJL       ADDED TABLE FULL CONDITION.        *00142002
001420* 01.03 07-AUG-1998 AKK       ADDED CHANGES FOR DATES TO         *00143003
001500*                             HANDLE CENTURY.                    *00150003
001600******************************************************************00160000
001700                                                                  00170000
001800 01  KTS-SECTIONS-KEY-TABLE.                                      00180000
001900     02 KTS-NBR-KEYS             PICTURE S9(04)          COMP.    00190000
001910        88 KTS-TBL-FULL          VALUE +750.                      00191002
002000     02 KTS-KEY-TBL              OCCURS 1 TO 750 TIMES            00200000
002100                                 DEPENDING ON KTS-NBR-KEYS        00210000
002200                                 INDEXED BY KTS-IDX.              00220000
002300        03 KTS-SECTION-NUMBER.                                    00230003
002300           05 KTS-SECTN-NO-1        PICTURE  X.                   00240003
002300           05 KTS-SECTN-NO          PICTURE  X(04).               00250003
002300        03 KTS-PKG-CODE             PICTURE X(03).                00260004
