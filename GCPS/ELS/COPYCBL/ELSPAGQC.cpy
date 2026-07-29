000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSPAGQC                                        *00030000
000400*    DATE:       11-JUN-1987                                     *00040000
000500*    AUTHOR:     EDWARD G. LISS                                  *00050000
000600*    FUNCTION:   INQUIRY OUTPUT QUEUE FILE RECORD LAYOUT.        *00060000
000700*                                                                *00070000
000800******************************************************************00080000
000900*                                                                *00090000
001000*                      MAINTENANCE HISTORY                       *00100000
001100*                                                                *00110000
001200*  MOD     DATE     BY  DRPT                ACTION               *00120000
001300* ----- ----------- --- ----- ---------------------------------- *00130000
001400* 01.00 11-JUN-1987 EGL       RECREATED AND UPDATED              *00140000
001500*                                                                *00150000
001600******************************************************************00160000
001700                                                                  00170000
001800 01  PQ-PAGE-QUEUE.                                               00180000
001900                                                                  00190000
002000     02 PQ-FIXED-PART.                                            00200000
002100        03 PQ-PAGE-NO            PICTURE S9(03)          COMP-3.  00210000
002200        03 PQ-DELETE-CODE-FLAG   PICTURE  X(01).                  00220000
002300           88 PQ-MORE-RECORDS    VALUE 'M'.                       00230000
002400           88 PQ-LAST-RECORD     VALUE 'L'.                       00240000
002500        03 PQ-HEADER-LINE-COUNT  PICTURE S9(04)   COMP.           00250000
002510        03 PQ-OCCURRENCE-COUNT   PICTURE S9(04)   COMP.           00260000
002600                                                                  00270000
002700     02 PQ-VARIABLE-TABLE.                                        00280000
002800        03 PQ-PAGE-LINE          PICTURE  X(79)                   00290000
002900                                 OCCURS 1 TO 23 TIMES             00300000
003000                                 DEPENDING ON PQ-OCCURRENCE-COUNT.00310000
