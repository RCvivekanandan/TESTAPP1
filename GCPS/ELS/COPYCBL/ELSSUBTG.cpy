000100******************************************************************00010000
000200*                                                                *00020000
000300*    COPYBOOK:   ELSSUBTG                                        *00030000
000400*    DATE:       28-OCT-1986                                     *00040000
000500*    AUTHOR:     JOHN T. CURIN,  KEANE, INC.                     *00050000
000600*    FUNCTION:   GENERIC COUNTERS AND TABLE AREA FOR THE         *00060000
000610*                SUB-TOPIC PROGRAM                               *00061000
000700*                                                                *00070000
001000******************************************************************00100000
001100*                                                                *00110000
001200*                      MAINTENANCE HISTORY                       *00120000
001300*                                                                *00130000
001400*  MOD     DATE     BY  DRPT                ACTION               *00140000
001500* ----- ----------- --- ----- ---------------------------------- *00150000
001600* 01.00 28-OCT-1986 JTC       CREATED                            *00160000
001700*                                                                *00170000
001700* 01.01 30-OCT-1986 JTC       CORRECTED OCCURS CLAUSE FROM       *00170100
001700*                             UPON TO ON                         *00170200
001700*                                                                *00170300
001700* 01.02 17-FEB-1987 JTC       EXPENDED TOPIC NAME TO PIC X(60)   *00170400
001700*                             UPON TO ON                         *00170500
001700*                                                                *00171000
001800******************************************************************00180000
011100                                                                  00181000
011200 01  GEN-TABLE-COUNTS.                                            00182000
011300     05  GEN-NUMBER-CODES    PIC S9(4) COMP.                      00183000
011400     05  GEN-NUMBER-HEADINGS PIC S9(4) COMP.                      00184000
           05  GEN-MENU-LINE-COUNT PIC S9(4) COMP.                      00185000
001900                                                                  00190000
002820 01  GEN-TOPIC-HEAD.                                              00282000
           05  GEN-TOPIC-HEADING-LINE                                   00282100
002830                     OCCURS 1 TO 15 TIMES                         00283000
                           DEPENDING ON GEN-NUMBER-HEADINGS             00283100
002840                     INDEXED BY GEN-HEAD-INDEX                    00284000
002850                         PIC X(78).                               00285000
002900                                                                  00290000
010600 01  GEN-TOPIC-TABLE.                                             01060000
           05  GEN-TOPIC-INFO                                           01061000
010700                     OCCURS 1 TO 100 TIMES                        01070000
                           DEPENDING ON GEN-NUMBER-CODES                01071000
010800                     INDEXED BY GEN-TOPIC-INDEX.                  01080000
010900         10  GEN-VALID-SELECT PIC XX.                             01090000
011000         10  GEN-TOPIC-NAME   PIC X(76).                          01100001
               10  GEN-KEYWORD-REDF REDEFINES GEN-TOPIC-NAME.           01100101
                   15  FILLER           PIC X(60).                      01100201
011010             15  GEN-KEYWORD      PIC X(16).                      01101001
011500                                                                  01150000
