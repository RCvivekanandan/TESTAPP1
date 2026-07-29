000100******************************************************************00010001
000200*                                                                *00020001
000300*    COPYBOOK:   ELSKTBCC                                        *00030001
000400*    DATE:       16-SEP-1986                                      00040001
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050001
000600*    FUNCTION:   CONTRACT KEYS FOR GIVEN GROUP/SECTION           *00060001
000700*                                                                *00070001
000800******************************************************************00080001
000900*                                                                *00090001
001000*                      MAINTENANCE HISTORY                       *00100001
001100*                                                                *00110001
001200*  MOD     DATE     BY  DRPT                ACTION               *00120001
001300* ----- ----------- --- ----- ---------------------------------- *00130001
001400* 01.00 16-SEP-1986 RJL       CREATED                            *00140001
001500* 01.01 28-OCT-1986 RJL       ADD TERMINATION DATE.              *00150001
001600* 01.02 01-NOV-1986 RJL       RESTRUCTURED SELECTION INDICATORS  *00160001
001700*                             FOR MORE GENERIC USE.              *00170001
001800* 01.03 17-NOV-1986 RJL       ADDED TABLE FULL CONDITION.        *00180001
001900*                                                                *00190001
002000* 01.04 14-OCT-1988 LET       ADDED NEW 88 LEVEL FOR KTC-IND     *00200001
002100*                             AS REQUESTED BY JPB.               *00210001
002200*                                                                *00220001
002300* 01.05 11-NOV-1988 LET       CORRECT POTENTIAL ERROR FOUND BY   *00230001
002400*                             EGL.                               *00240001
002401*                                                                *00240101
002410* 01.06 27-SEP-1991 RJL       EXPAND FAM-REL-LVL'S TO 2 BYTES.   *00241001
002500*                                                                *00250001
002510* 01.07 07-AUG-1997 AKK       UPDATE DATES TO ACCOMODATE YEAR    *00251003
002520*                             2000.                              *00252002
002530*                                                                *00253002
002600******************************************************************00260001
002700                                                                  00270001
002800 01  KTC-GCCONTR-KEY-TABLE.                                       00280001
002900     02 KTC-NBR-KEYS             PICTURE S9(04)          COMP.    00290001
003000        88 KTC-TBL-FULL          VALUE +500.                      00300001
003100     02 KTC-KEY-TBL              OCCURS 1 TO 500 TIMES            00310001
003200                                 DEPENDING ON KTC-NBR-KEYS        00320001
003300                                 INDEXED BY KTC-IDX.              00330001
003400        03 KTC-PLAN-CODE         PICTURE  X(03).                  00340005
003400        03 KTC-PKG-CODE          PICTURE  X(03).                  00341005
003400        03 KTC-L-O-B             PICTURE  X(01).                  00342005
003500        03 KTC-PROVDR-CONTROL    PICTURE  X(02).                  00350001
003600        03 KTC-FAM-REL-LVL       PICTURE  X(02).                  00360001
003700        03 KTC-EFFECTIVE-DATE.                                    00370002
003710           05 KTC-EFF-DT-CC      PICTURE X.                       00371002
003720           05 KTC-EFF-DT         PICTURE S9(05) COMP-3.           00372002
003730        03 KTC-EFF-DT-CENTURY REDEFINES KTC-EFFECTIVE-DATE        00373002
003740                                 PICTURE S9(07) COMP-3.           00374003
003750        03 KTC-TERMINATION-DATE.                                  00375002
003800           05 KTC-TERMN-DT-CC  PICTURE X.                         00380002
003810           05 KTC-TERMN-DT     PICTURE S9(05)    COMP-3.          00381002
003820        03 KTC-TERM-DT-CENTURY REDEFINES KTC-TERMINATION-DATE     00382002
003830                                 PICTURE S9(07) COMP-3.           00383003
003900        03 KTC-SEL-IND-LST.                                       00390001
004000           04 KTC-INST-BAS-IND   PICTURE  X(01).                  00400001
004100              88 KTC-INST-BAS-SEL              VALUE 'S'.         00410001
004200              88 KTC-INST-BAS-REJ              VALUE 'R' 'X'.     00420001
004300              88 KTC-INST-BAS-EXC              VALUE 'X'.         00430001
004400           04 KTC-INST-SUP-IND   PICTURE  X(01).                  00440001
004500              88 KTC-INST-SUP-SEL              VALUE 'S'.         00450001
004600              88 KTC-INST-SUP-REJ              VALUE 'R' 'X'.     00460001
004700              88 KTC-INST-SUP-EXC              VALUE 'X'.         00470001
004800           04 KTC-PROF-BAS-IND   PICTURE  X(01).                  00480001
004900              88 KTC-PROF-BAS-SEL              VALUE 'S'.         00490001
005000              88 KTC-PROF-BAS-REJ              VALUE 'R' 'X'.     00500001
005100              88 KTC-PROF-BAS-EXC              VALUE 'X'.         00510001
005200           04 KTC-PROF-SUP-IND   PICTURE  X(01).                  00520001
005300              88 KTC-PROF-SUP-SEL              VALUE 'S'.         00530001
005400              88 KTC-PROF-SUP-REJ              VALUE 'R' 'X'.     00540001
005500              88 KTC-PROF-SUP-EXC              VALUE 'X'.         00550001
005600        03 KTC-SEL-IND-TBL       REDEFINES KTC-SEL-IND-LST.       00560001
005700           04 KTC-SEL-IND        PICTURE  X(01)                   00570001
005800                                 OCCURS 4 TIMES                   00580001
005900                                 INDEXED BY KTC-SEL-IDX.          00590001
006000              88 KTC-SEL                       VALUE 'S'.         00600001
006100              88 KTC-REJ                       VALUE 'R' 'X'.     00610001
006200              88 KTC-EXC                       VALUE 'X'.         00620001
