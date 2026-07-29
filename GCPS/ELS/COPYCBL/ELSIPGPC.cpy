000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSIPGPC                                     *00050000
000600*    Date Created:  16-Aug-1992                                  *00060000
000700*    Author:        Richard J. Luketich                          *00070000
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       Contains slot numbers of, pointers to, and confidence    *00100000
001100*       factors for #IPGP (procedure) tabulars. Confidence       *00110000
001200*       factors from this table are used to calculate confidence *00120000
001300*       factors for accumulators in order to determine the       *00130000
001400*       applicability of an accumulator to a particular          *00140000
001500*       situation, e.g., the \
001600*                                                                *00160000
001700******************************************************************00170000
001800*                                                                *00180000
001900*                       Maintenance History                      *00190000
002000*                                                                *00200000
002100*  Mod     Date      By               Action/Reason              *00210000
002200* ----- ----------- ---- --------------------------------------- *00220000
002300* 01.00 16-Aug-1992 RJL1 Created.                                *00230000
002400*                                                                *00240000
002500******************************************************************00250000
002600                                                                  00260000
002700 01  IPGP-INTERNAL-TABS-TABLE.                                    00270000
002800     02 IPGP-TBL-CNT             PICTURE S9(4) COMP.              00280000
002900        88 IPGP-TBL-FULL         VALUE +150.                      00290000
003000     02 IPGP-CF-CALC-MODE        PICTURE  X(01).                  00300000
003100        88 IPGP-CF-CALC-MTCH     VALUE 'M'.                       00310000
003200        88 IPGP-CF-CALC-OV       VALUE 'O'.                       00320000
003300        88 IPGP-CF-CALC-WT-MTCH  VALUE 'W'.                       00330000
003400     02 IPGP-CF-CALC-RET-CD      PICTURE  X(01).                  00340000
003500        88 IPGP-CF-CALC-OK       VALUE 'Y'.                       00350000
003600        88 IPGP-CF-CALC-FAIL     VALUE 'N'.                       00360000
003700     02 IPGP-INTERNAL-TABS       OCCURS 1 TO 150 TIMES            00370000
003800                                 DEPENDING ON IPGP-TBL-CNT        00380000
003900                                 INDEXED BY IPGP-IDX              00390000
004000                                            IPGP-MAX-IDX.         00400003
004100         03 IPGP-SLOT-NUMBER                PICTURE S9(7) COMP-3. 00410000
004200         03 IPGP-TABULAR-PTR                POINTER.              00420000
004300         03 IPGP-CONFIDENCE-FACTORS.                              00430000
004400             04 IPGP-CF-INST                COMP-1.               00440000
004500             04 IPGP-CF-PROF                COMP-1.               00450000
004600             04 IPGP-CF-OV                  COMP-1.               00460000
004700             04 IPGP-CF-LIST-MTCH           COMP-1.               00470000
004800                                                                  00480000
