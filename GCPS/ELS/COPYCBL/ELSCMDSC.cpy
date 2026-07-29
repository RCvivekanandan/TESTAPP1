000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSCMDSC                                        *        
000400*    DATE:       15-SEP-1986                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   CONTAINS THE DESCRIPTION OF A CODE VALUE AS     *        
000700*                FOUND IN THE CODES MANUAL BY THE CODES MANUAL   *        
000800*                INTERFACE PROGRAM                               *        
000900*                                                                *        
001000******************************************************************        
001100*                                                                *        
001200*                      MAINTENANCE HISTORY                       *        
001300*                                                                *        
001400*  MOD     DATE     BY  DRPT                ACTION               *        
001500* ----- ----------- --- ----- ---------------------------------- *        
001600* 01.00 15-SEP-1986 RJL       CREATED                            *        
001700* 01.01 30-NOV-1993 RJL       REVISED FOR CONSISTENCY.           *        
001800*                                                                *        
001900******************************************************************        
002000                                                                          
002100                                                                          
002200 01  CMF-DESCR.                                                           
002300     02 CMF-NBR-DESCR-LINES               PIC S9(04)       COMP.          
002400     02 CMF-DESCR-LINE                    PIC  X(79)                      
002500           OCCURS 1 TO 500 TIMES                                          
002600           DEPENDING ON CMF-NBR-DESCR-LINES                               
002700           INDEXED BY CMF-DESCR-IDX                                       
002800                      CMF-IDX                                             
002900                      CMF-MAX-IDX.                                        
