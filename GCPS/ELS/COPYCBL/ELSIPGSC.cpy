000100******************************************************************00010000
000200*                         COPYLIB Member                         *00020000
000300******************************************************************00030000
000400*                                                                *00040000
000500*    Member Name:   ELSIPGSC                                     *00050001
000600*    Date Created:  17-Aug-2000                                  *00060001
000700*    Author:        Anne Keffer King                             *00070001
000800*                                                                *00080000
000900*    Function:                                                   *00090000
001000*       Contains slot numbers of, pointers to, and confidence    *00100000
001100*       factors for #IPGS (provider spec) tabulars. Confidence   *00110001
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
002300* 01.00 17-aug-2000 akk  Created. REMOVED IPGS-CF-INST AS IPGS   *00230002
002700*                         IS PROFESSIONAL ONLY.                  *00270002
002800******************************************************************00280000
002900                                                                  00290000
003000 01  IPGS-INTERNAL-TABS-TABLE.                                    00300001
003100     02 IPGS-TBL-CNT             PICTURE S9(4) COMP.              00310001
003200        88 IPGS-TBL-FULL         VALUE +150.                      00320001
003300     02 IPGT-CF-CALC-MODE        PICTURE  X(01).                  00330000
003400        88 IPGS-CF-CALC-MTCH     VALUE 'M'.                       00340001
003500        88 IPGS-CF-CALC-OV       VALUE 'O'.                       00350001
003600        88 IPGS-CF-CALC-WT-MTCH  VALUE 'W'.                       00360001
003700     02 IPGS-CF-CALC-RET-CD      PICTURE  X(01).                  00370001
003800        88 IPGS-CF-CALC-OK       VALUE 'Y'.                       00380001
003900        88 IPGS-CF-CALC-FAIL     VALUE 'N'.                       00390001
004000     02 IPGS-INTERNAL-TABS       OCCURS 1 TO 150 TIMES            00400001
004100                                 DEPENDING ON IPGS-TBL-CNT        00410001
004200                                 INDEXED BY IPGS-IDX              00420001
004300                                            IPGS-MAX-IDX          00430001
004400                                            IPGS-X-IDX.           00440001
004500        03 IPGS-SLOT-NUMBER                 PICTURE S9(7) COMP-3. 00450001
004600        03 IPGS-TABULAR-PTR                 POINTER.              00460001
004700        03 IPGS-CONFIDENCE-FACTORS.                               00470001
004900           04 IPGS-CF-PROF                  COMP-1.               00490001
005000           04 IPGS-CF-PLAN                  COMP-1.               00500001
005100           04 IPGS-CF-NON-PLAN              COMP-1.               00510001
005200           04 IPGS-CF-OV                    COMP-1.               00520001
005300           04 IPGS-CF-LIST-MTCH             COMP-1.               00530001
005400                                                                  00540000
