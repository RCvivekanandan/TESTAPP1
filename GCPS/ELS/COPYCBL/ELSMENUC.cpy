000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSMENUC                                        *00030000
000400*    DATE:       17-SEP-1986                                     *00040000
000500*    AUTHOR:     RICHARD J. LUKETICH                             *00050000
000600*                EDWARD G. LISS                                  *00060000
000700*    FUNCTION:   DESCRITIONS OF EACH OF THE VALID SELECTIONS FOR *00070000
000800*                THE CURRENT MENU.                               *00080000
000900*                                                                *00090000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 17-SEP-1986 RJL       CREATED                            *00160000
001700*                   EGL                                          *00170000
001710* 01.01 17-SEP-1986 RJL       CORRECTED NAME TOO LONG.           *00171001
001800*                                                                *00180000
001900******************************************************************00190000
002000                                                                  00200000
002100 01  MSD-MENU-ITEM-DESCRIPTIONS.                                  00210001
002200     02 MSD-NBR-DESCR-LINES      PICTURE S9(04)          COMP.    00220000
002300     02 MSD-DESCR-LINES.                                          00230000
002400        03 MSD-DESCR-LINE        PICTURE  X(79)                   00240000
002500                                 OCCURS 1 TO 3 TIMES              00250000
002600                                 DEPENDING ON MSD-NBR-DESCR-LINES 00260000
002700                                 INDEXED BY MSD-IDX.              00270000
