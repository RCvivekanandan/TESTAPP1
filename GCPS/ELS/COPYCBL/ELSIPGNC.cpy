000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSIPGNC                                     *00050000
000600*    Date Created:  01-Jul-1988                                  *00060000
000700*    Author:        Nina A. Cervantes                            *00070000
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       Contains slot numbers of, pointers to, and confidence    *00100000
001100*       factors for #IPGN (provider number) tabulars. Confidence *00110000
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
002300* 01.00 01-Jul-1988 NAC  Created                                 *00230000
002400* 01.01 13-Aug-1992 RJL  Added confidence factors for single and *00240000
002500*                        list value matches, increased maximum   *00250000
002600*                        number of occurrences to 150.           *00260000
002700*                                                                *00270000
002800******************************************************************00280000
002900                                                                  00290000
003000 01  IPGN-INTERNAL-TABS-TABLE.                                    00300000
003100     02 IPGN-TBL-CNT             PICTURE S9(4) COMP.              00310000
003200        88 IPGN-TBL-FULL         VALUE +150.                      00320000
003300     02 IPGN-CF-CALC-MODE        PICTURE  X(01).                  00330000
003400        88 IPGN-CF-CALC-MTCH     VALUE 'M'.                       00340000
003500        88 IPGN-CF-CALC-OV       VALUE 'O'.                       00350000
003600        88 IPGN-CF-CALC-WT-MTCH  VALUE 'W'.                       00360000
003700     02 IPGN-CF-CALC-RET-CD      PICTURE  X(01).                  00370000
003800        88 IPGN-CF-CALC-OK       VALUE 'Y'.                       00380000
003900        88 IPGN-CF-CALC-FAIL     VALUE 'N'.                       00390000
004000     02 IPGN-INTERNAL-TABS       OCCURS 1 TO 150 TIMES            00400000
004100                                 DEPENDING ON IPGN-TBL-CNT        00410000
004200                                 INDEXED BY IPGN-IDX              00420000
004300                                            IPGN-MAX-IDX          00430000
004400                                            IPGN-X-IDX.           00440000
004500        03 IPGN-SLOT-NUMBER                 PICTURE S9(7) COMP-3. 00450000
004600        03 IPGN-TABULAR-PTR                 POINTER.              00460000
004700        03 IPGN-CONFIDENCE-FACTORS.                               00470000
004800           04 IPGN-CF-OV                    COMP-1.               00480000
004900           04 IPGN-CF-LIST-MTCH             COMP-1.               00490000
005000                                                                  00500000
