000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSIBGRC                                     *00050000
000600*    Date Created:  01-Jul-1992                                  *00060000
000700*    Author:        Nina A. Cervantes                            *00070000
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       Contains slot numbers of, pointers to, and confidence    *00100000
001100*       factors for #IBGR (benefit provision) tabulars.          *00110000
001200*       Confidence factors from this table are used to calculate *00120000
001300*       confidence factors for accumulators in order to          *00130000
001400*       determine the applicability of an accumulator to a       *00140000
001500*       particular situation, e.g., the \
001600*                                                                *00160000
001700******************************************************************00170000
001800*                                                                *00180000
001900*                       Maintenance History                      *00190000
002000*                                                                *00200000
002100*  Mod     Date      By               Action/Reason              *00210000
002200* ----- ----------- ---- --------------------------------------- *00220000
002300* 01.00 01-Jul-1988 NAC  Created.                                *00230000
002400* 01.01 26-Aug-1988 AKK  Increased maximum number of occurrences *00240000
002500*                        to 40.                                  *00250000
002600* 02.00 07-Sep-1988 NAC  Increased maximum number of occurrences *00260000
002700*                        to 150.                                 *00270000
002800* 02.01 20-Sep-1989 RJL  Removed second occurrence of confidence *00280000
002900*                        factors (not used).                     *00290000
003000* 02.02 13-Aug-1992 RJL1 Added new confidence factors for list   *00300000
003100*                        and single value match.                 *00310000
003200*                                                                *00320000
003300******************************************************************00330000
003400                                                                  00340000
003500 01  IBGR-INTERNAL-TABS-TABLE.                                    00350000
003600     02 IBGR-TBL-CNT             PICTURE S9(04) COMP.             00360000
003700        88 IBGR-TBL-FULL         VALUE +150.                      00370000
003800     02 IBGR-CF-CALC-MODE        PICTURE  X(01).                  00380000
003900        88 IBGR-CF-CALC-MTCH     VALUE 'M'.                       00390000
004000        88 IBGR-CF-CALC-OV       VALUE 'O'.                       00400000
004100        88 IBGR-CF-CALC-WT-MTCH  VALUE 'W'.                       00410000
004200     02 IBGR-CF-CALC-RET-CD      PICTURE  X(01).                  00420000
004300        88 IBGR-CF-CALC-OK       VALUE 'Y'.                       00430000
004400        88 IBGR-CF-CALC-FAIL     VALUE 'N'.                       00440000
004500     02 IBGR-INTERNAL-TABS       OCCURS 1 TO 150 TIMES            00450000
004600                                 DEPENDING ON IBGR-TBL-CNT        00460000
004700                                 INDEXED BY IBGR-IDX              00470000
004800                                            IBGR-MAX-IDX          00480000
004900                                            IBGR-X-IDX.           00490000
005000         03 IBGR-SLOT-NUMBER                PICTURE S9(07) COMP-3.00500000
005100         03 IBGR-TABULAR-PTR                POINTER.              00510000
005200         03 IBGR-CONFIDENCE-FACTORS.                              00520000
005300            04 IBGR-CF-INST                 COMP-1.               00530000
005400            04 IBGR-CF-PROF                 COMP-1.               00540000
005500            04 IBGR-CF-IP                   COMP-1.               00550000
005600            04 IBGR-CF-IP-BOTH              COMP-1.               00560000
005700            04 IBGR-CF-OP                   COMP-1.               00570000
005800            04 IBGR-CF-OP-BOTH              COMP-1.               00580000
005900            04 IBGR-CF-OV                   COMP-1.               00590000
006000            04 IBGR-CF-LIST-MTCH            COMP-1.               00600000
006100                                                                  00610000
