000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSIPGTC                                     *00050000
000600*    Date Created:  16-Aug-1992                                  *00060000
000700*    Author:        Richard J. Luketich                          *00070000
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       Contains slot numbers of, pointers to, and confidence    *00100000
001100*       factors for #IPGT (provider type) tabulars. Confidence   *00110000
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
002300* 01.00 01-JUL-1988 NAC  Created                                 *00230000
002400* 01.01 16-AUG-1992 RJL  Added confidence factors for list and   *00240000
002500*                        single value matches, increased maximum *00250000
002600*                        number of occurrences to 150.           *00260000
002700*                                                                *00270000
002800******************************************************************00280000
002900                                                                  00290000
003000 01  IPGT-INTERNAL-TABS-TABLE.                                    00300000
003100     02 IPGT-TBL-CNT             PICTURE S9(4) COMP.              00310000
003200        88 IPGT-TBL-FULL         VALUE +150.                      00320000
003300     02 IPGT-CF-CALC-MODE        PICTURE  X(01).                  00330000
003400        88 IPGT-CF-CALC-MTCH     VALUE 'M'.                       00340000
003500        88 IPGT-CF-CALC-OV       VALUE 'O'.                       00350000
003600        88 IPGT-CF-CALC-WT-MTCH  VALUE 'W'.                       00360000
003700     02 IPGT-CF-CALC-RET-CD      PICTURE  X(01).                  00370000
003800        88 IPGT-CF-CALC-OK       VALUE 'Y'.                       00380000
003900        88 IPGT-CF-CALC-FAIL     VALUE 'N'.                       00390000
004000     02 IPGT-INTERNAL-TABS       OCCURS 1 TO 150 TIMES            00400000
004100                                 DEPENDING ON IPGT-TBL-CNT        00410000
004200                                 INDEXED BY IPGT-IDX              00420000
004300                                            IPGT-MAX-IDX          00430000
004400                                            IPGT-X-IDX.           00440000
004500        03 IPGT-SLOT-NUMBER                 PICTURE S9(7) COMP-3. 00450000
004600        03 IPGT-TABULAR-PTR                 POINTER.              00460000
004700        03 IPGT-CONFIDENCE-FACTORS.                               00470000
004800           04 IPGT-CF-INST                  COMP-1.               00480000
004900           04 IPGT-CF-PROF                  COMP-1.               00490000
005000           04 IPGT-CF-PLAN                  COMP-1.               00500000
005100           04 IPGT-CF-NON-PLAN              COMP-1.               00510000
005200           04 IPGT-CF-OV                    COMP-1.               00520000
005300           04 IPGT-CF-LIST-MTCH             COMP-1.               00530000
005400                                                                  00540000
