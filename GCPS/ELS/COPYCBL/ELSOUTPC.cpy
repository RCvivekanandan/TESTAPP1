000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSOUTPC                                        *        
000400*    DATE:       22-SEP-1986                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   INTERFACE CONTROL BLOCK FOR OUTPUT PAGE BUILDER *        
000700*                                                                *        
000800******************************************************************        
000900*                                                                *        
001000*                      MAINTENANCE HISTORY                       *        
001100*                                                                *        
001200*  MOD     DATE     BY  DRPT                ACTION               *        
001300* ----- ----------- --- ----- ---------------------------------- *        
001400* 01.00 22-SEP-1986 RJL       CREATED                            *        
001500* 01.01 17-FEB-1988 NAC       TO ACCOMODATE A MAXIMUM OF FIVE    *        
001600*                             HEADER LINES; CREATE A MASK LINE   *        
001700*                             WHICH ELUOUTPT WILL USE TO PAD OUT *        
001800*                             INCOMPLETE BLOCKS OF OUTPUT; CREATE*        
001900*                             A TRAILER AREA.                    *        
002000* 01.02 01-DEC-1993 RJL       ADDED -MAX-IDX ENTRIES.            *        
002100*                             REFORMATTED FOR CONSISTENCY        *        
002200*                                                                *        
002300******************************************************************        
002400                                                                          
002500 01  COF-OUTPUT-INTERFACE.                                                
002600     02 COF-FUNCTION                      PIC  X(01).                     
002700        88 COF-CONTINUE                   VALUE SPACE.                    
002800        88 COF-NEW-PAGE                   VALUE 'P'.                      
002900        88 COF-END                        VALUE 'E'.                      
003000     02 COF-RETURN-CODE                   PIC S9(04)       COMP.          
003100        88 COF-OK                         VALUE ZERO.                     
003200        88 COF-INVL-REQ                   VALUE +1.                       
003300     02 COF-MASK-LINE                     PIC  X(79).                     
003400        88  COF-DEFAULT-MASK              VALUE SPACES.                   
003500     02 COF-NBR-HDR-LINES                 PIC S9(04)       COMP.          
003600     02 COF-NBR-DTL-LINES                 PIC S9(04)       COMP.          
003700     02 COF-NBR-TRL-LINES                 PIC S9(04)       COMP.          
003800     02 COF-HDR.                                                          
003900        03 COF-HDR-LINE                   PIC  X(79)                      
004000              OCCURS 5 TIMES                                              
004100              INDEXED BY COF-HDR-IDX                                      
004200                         COF-HDR-MAX-IDX.                                 
004300     02 COF-DTL.                                                          
004400        03 COF-DTL-LINE                   PIC  X(79)                      
004500              OCCURS 20 TIMES                                             
004600              INDEXED BY COF-DTL-IDX                                      
004700                         COF-DTL-MAX-IDX.                                 
004800     02 COF-TRL.                                                          
004900        03 COF-TRL-LINE                   PIC  X(79)                      
005000              OCCURS 5 TIMES                                              
005100              INDEXED BY COF-TRL-IDX                                      
005200                         COF-TRL-MAX-IDX.                                 
