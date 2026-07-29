000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSCMIFC                                        *        
000400*    DATE:       10-SEP-1986                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   INTERFACE CONTROL BLOCK FOR CODES MANUAL        *        
000700*                TRANSLATION AND CONDITION BITS TRANSLATION      *        
000800*                ROUTINES USED BY ELS ENGLISH CONTRACT INQUIRY   *        
000900*                                                                *        
001000******************************************************************        
001100*                                                                *        
001200*                      MAINTENANCE HISTORY                       *        
001300*                                                                *        
001400*  MOD     DATE     BY  DRPT                ACTION               *        
001500* ----- ----------- --- ----- ---------------------------------- *        
001600* 01.00 10-SEP-1986 RJL       CREATED                            *        
001700* 02.01 30-NOV-1993 RJL       REVISED FOR CONSISTENCY            *        
001800*                                                                *        
001900******************************************************************        
002000                                                                          
002100 01  CMF-CODES-MANUAL-INTERFACE.                                          
002200     02 CMF-KEY-VALUE.                                                    
002300        03 CMF-RECORD-PREFIX              PIC  X(08).                     
002400        03 CMF-ELEMENT-SYSTEM-NAME        PIC  X(30).                     
002500        03 CMF-CODE-VALUE                 PIC  X(10).                     
002600     02 CMF-CONDITION-BITS                PIC  X(30).                     
002700     02 CMF-RETURN-CODE                   PIC S9(04)       COMP.          
002800        88 CMF-RC-OK                      VALUE ZERO.                     
