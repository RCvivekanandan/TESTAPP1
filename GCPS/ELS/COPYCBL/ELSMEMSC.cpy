000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSMEMSC                                        *00030000
000400*    DATE:       20-OCT-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*    FUNCTION:   CONTAINS THE INFORMATION RETURNED BY THE        *00060000
000700*                MEMBERSHIP INTERFACE PROGRAM.                   *00070000
000900*                                                                *00080000
001000******************************************************************00090000
001100*                                                                *00100000
001200*                      MAINTENANCE HISTORY                       *00110000
001300*                                                                *00120000
001400*  MOD     DATE     BY  DRPT                ACTION               *00130000
001500* ----- ----------- --- ----- ---------------------------------- *00140000
001600* 01.00 20-OCT-1986 RJL       CREATED                            *00150000
001610* 01.01 28-OCT-1986 RJL       ADD INDEX FOR TABLE.               *00160000
001610* 01.02 23-MAY-1988 LET       AS REQUESTED BY REB:  ADDED        *00161000
001700*                               MSI-GRP-SUB-EFF-DT               *00170000
001700*                               MSI-GRP-SUB-TERM-DT              *00170100
001610* 01.03 07-AUG-1997 AKK       ADDED EXPANDED GRP/SECTION         *00170201
001700*                             AND DATE TO HANDLE YEAR 2000       *00171001
001700*                                                                *00172001
001800******************************************************************00180000
001900                                                                  00190000
002000 01  MSI-MEMBERSHIP-INTERFACE.                                    00200000
002010     02 MSI-MBR-INFO.                                             00201000
002020        03 MSI-NBR-MBR-SECTNS    PICTURE S9(04) COMP.             00202000
002500        03 MSI-PREV-GROUP-NUMBER.                                 00203001
002500           05 MSI-PREV-GRP-1-2-3    PICTURE  X(03).               00203101
002500           05 MSI-PREV-GRP-NBR      PICTURE  X(06).               00203201
002600        03 MSI-PREV-MEMBR-NBR    PICTURE  X(12).                  00204001
002700        03 MSI-NEXT-GROUP-NUMBER.                                 00205001
002700           05 MSI-NEXT-GRP-NBR-1-2-3    PICTURE  X(03).           00205101
002700           05 MSI-NEXT-GRP-NBR          PICTURE  X(06).           00205201
002800        03 MSI-NEXT-MEMBR-NBR    PICTURE  X(12).                  00206001
002700        03 MSI-GRP-SUB-EFFECTIVE-DATE.                            00206101
002700           05 MSI-GRP-SUB-EFF-DT-CC PICTURE X.                    00206201
002700           05 MSI-GRP-SUB-EFF-DT    PICTURE S9(05) COMP-3.        00206401
              03 MSI-GRP-SUB-EFF-DATE-CC      REDEFINES                 00206505
                    MSI-GRP-SUB-EFFECTIVE-DATE PICTURE S9(07) COMP-3.   00206601
002800        03 MSI-GRP-SUB-TERMINATION-DATE.                          00206701
002700           05 MSI-GRP-SUB-TERM-DT-CC PICTURE X.                   00206801
002700           05 MSI-GRP-SUB-TERM-DT    PICTURE S9(05) COMP-3.       00206901
              03 MSI-GRP-SUB-TERM-DATE-CENTURY REDEFINES                00207001
                  MSI-GRP-SUB-TERMINATION-DATE                          00207102
                                           PICTURE S9(07) COMP-3.       00207202
002900        03 MSI-MEMBER-NAME.                                       00207301
002910           04 MSI-FIRST-NAME     PICTURE  X(10).                  00208001
002920           04 MSI-MIDDLE-INITIAL PICTURE  X(01).                  00209001
002930           04 MSI-LAST-NAME      PICTURE  X(20).                  00210001
003000     02 MSI-MBR-SECTN-TABLE.                                      00220001
003100        03 MSI-MBR-SECN-TBL      OCCURS 1 TO 20 TIMES             00230000
003200                                 DEPENDING ON MSI-NBR-MBR-SECTNS  00240000
003210                                 INDEXED BY MSI-IDX.              00250000
003300           04 MSI-EFFECTIVE-DATE.                                 00260001
003300              06 MSI-EFF-DT-CC      PICTURE X.                    00261001
003300              06 MSI-EFF-DT         PICTURE S9(05) COMP-3.        00262001
003300           04 MSI-EFF-DATE-CENTURY REDEFINES MSI-EFFECTIVE-DATE   00263001
                                          PICTURE S9(07) COMP-3.        00264001
003400           04 MSI-TERMINATION-DATE.                               00270001
003400              06 MSI-TERM-DT-CC     PICTURE X.                    00271001
003400              06 MSI-TERM-DT        PICTURE S9(05) COMP-3.        00272001
                 04 MSI-TERMIN-DATE-CC REDEFINES MSI-TERMINATION-DATE   00273001
                                          PICTURE S9(07) COMP-3.        00274001
003500           04 MSI-MEMBER-SECTION.                                 00280001
003500              06 MSI-MBR-SECTN-CC   PICTURE  X(01).               00290001
003500              06 MSI-MBR-SECTN      PICTURE  X(04).               00300001
003500           04 MSI-PKG-CODE          PICTURE  X(03).               00310003
