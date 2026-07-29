000100******************************************************************        
000200*                                                                *        
000300*    COPYBOOK:   ELSTCWAC                                        *        
000400*    DATE:       22-SEP-1986                                     *        
000500*    AUTHOR:     RICHARD J. LUKETICH                             *        
000600*    FUNCTION:   WORK AREAS FOR TEXT COMPRESSION ROUTINES        *        
000700*                                                                *        
000800******************************************************************        
000900*                                                                *        
001000*                      MAINTENANCE HISTORY                       *        
001100*                                                                *        
001200*  MOD     DATE     BY  DRPT                ACTION               *        
001300* ----- ----------- --- ----- ---------------------------------- *        
001400* 01.00 22-SEP-1986 RJL       CREATED                            *        
001500* 01.01 20-JUL-1987 EGL       ADDED TCAR-FROM-LINE-LAST-DIGIT    *        
001600* 01.02 01-DEC-1993 RJL       ADDED INDEXES. REFORMATTED FOR     *        
001700*                             CONSISTENCY.                       *        
001800*                                                                *        
001900******************************************************************        
002000                                                                          
002100 01  TCAR-COMPRESSION-WORK-AREA.                                          
002200     02 TCAR-TEXT-COMPRESSION-AREA.                                       
002300        03 TCAR-FROM-SUB                  PIC S9(04)       COMP.          
002400        03 TCAR-FROM-SUB-PLUS-1           PIC S9(04)       COMP.          
002500        03 TCAR-TO-SUB                    PIC S9(04)       COMP.          
002600        03 TCAR-FROM-LENGTH               PIC S9(04)       COMP.          
002700        03 TCAR-AREA-LENGTH               PIC S9(04)       COMP.          
002800                                                                          
002900        03 TCAR-FROM-AREA                 PIC  X(1580).                   
003000        03 TCAR-FROM-DIGIT                                                
003110           REDEFINES TCAR-FROM-AREA       PIC  X(01)                      
003200              OCCURS 1580 TIMES                                           
003300              INDEXED BY TCAR-FROM-AREA-IDX                               
003400                         TCAR-FROM-AREA-MAX-IDX.                          
003500        03 TCAR-FROM-LINE                                                 
003610           REDEFINES TCAR-FROM-AREA       PIC  X(79)                      
003700              OCCURS 20 TIMES                                             
003800              INDEXED BY TCAR-FROM-LINE-IDX                               
003900                         TCAR-FROM-LINE-MAX-IDX.                          
004000        03    REDEFINES TCAR-FROM-AREA                                    
004100              OCCURS 20 TIMES.                                            
004200           04                             PIC  X(78).                     
004300           04  TCAR-FROM-LINE-LAST-DIGIT  PIC  X(01).                     
004400                                                                          
004500        03 TCAR-TO-AREA                   PIC  X(1580).                   
004600        03 TCAR-TO-DIGIT                                                  
004700           REDEFINES TCAR-TO-AREA         PIC  X(01)                      
004800              OCCURS 1580 TIMES                                           
004900              INDEXED BY TCAR-TO-AREA-IDX                                 
005000                         TCAR-TO-AREA-MAX-IDX.                            
005100        03 TCAR-TO-LINE                                                   
005200           REDEFINES TCAR-TO-AREA         PIC  X(79)                      
005300              OCCURS 20 TIMES                                             
005400              INDEXED BY TCAR-TO-LINE-IDX                                 
005500                         TCAR-TO-LINE-MAX-IDX.                            
005600     02 TCAR-TEXT-UNSTRING-AREA.                                          
005700                                                                          
005800        03 TCAR-OUTPUT-FIELD-COUNT        PIC S9(04)       COMP.          
005900        03 TCAR-OUTPUT-FIELDS-USED        PIC S9(04)       COMP.          
006000        03 TCAR-OUTPUT-FIELD-1-LEN        PIC S9(04)       COMP.          
006100        03 TCAR-OUTPUT-FIELD-2-LEN        PIC S9(04)       COMP.          
006200        03 TCAR-OUTPUT-FIELD-3-LEN        PIC S9(04)       COMP.          
006300        03 TCAR-OUTPUT-FIELD-4-LEN        PIC S9(04)       COMP.          
006400        03 TCAR-OUTPUT-FIELD-5-LEN        PIC S9(04)       COMP.          
006500        03 TCAR-OUTPUT-FIELD-6-LEN        PIC S9(04)       COMP.          
006600        03 TCAR-OUTPUT-FIELD-7-LEN        PIC S9(04)       COMP.          
006700        03 TCAR-OUTPUT-FIELD-8-LEN        PIC S9(04)       COMP.          
006800        03 TCAR-OUTPUT-FIELD-9-LEN        PIC S9(04)       COMP.          
006900        03 TCAR-OUTPUT-FIELD-10-LEN       PIC S9(04)       COMP.          
007000        03 TCAR-OUTPUT-FIELD-11-LEN       PIC S9(04)       COMP.          
007100        03 TCAR-OUTPUT-FIELD-12-LEN       PIC S9(04)       COMP.          
007200        03 TCAR-OUTPUT-FIELD-13-LEN       PIC S9(04)       COMP.          
007300        03 TCAR-OUTPUT-FIELD-14-LEN       PIC S9(04)       COMP.          
007400        03 TCAR-OUTPUT-FIELD-15-LEN       PIC S9(04)       COMP.          
007500        03 TCAR-OUTPUT-FIELD-16-LEN       PIC S9(04)       COMP.          
007600        03 TCAR-OUTPUT-FIELD-17-LEN       PIC S9(04)       COMP.          
007700        03 TCAR-OUTPUT-FIELD-18-LEN       PIC S9(04)       COMP.          
007800        03 TCAR-OUTPUT-FIELD-19-LEN       PIC S9(04)       COMP.          
007900        03 TCAR-OUTPUT-FIELD-20-LEN       PIC S9(04)       COMP.          
008000                                                                          
008100*          TCAR-L  LENGTH OF INPUT TEXT                                   
008200*          TCAR-A  START OF WORD  (INPUT)                                 
008300*          TCAR-B  END OF WORD    (INPUT)                                 
008400*          TCAR-C  LENGTH OF WORD (INPUT)                                 
008500*          TCAR-OA CURRENT POSITION IN OUTPUT FIELD                       
008600*          TCAR-OR DIGITS REMAINING TO BE FILLED IN OUTPUT FIELD          
008700*          TCAR-X  NUMBER OF FIELDS TO UNSTRING TO                        
008800                                                                          
008900        03 TCAR-L                         PIC S9(04)       COMP.          
009000        03 TCAR-A                         PIC S9(04)       COMP.          
009100        03 TCAR-B                         PIC S9(04)       COMP.          
009200        03 TCAR-C                         PIC S9(04)       COMP.          
009300        03 TCAR-OA                        PIC S9(04)       COMP.          
009400        03 TCAR-OR                        PIC S9(04)       COMP.          
009500        03 TCAR-X                         PIC S9(04)       COMP.          
009600                                                                          
009700        03 TCAR-OUTPUT-FIELD-TABLE                                        
009800              OCCURS 20 TIMES                                             
009900              INDEXED BY TCAR-OPF-IDX                                     
010000                         TCAR-OPF-MAX-IDX                                 
010100                         TCAR-OPF-USED-IDX.                               
010200           04 TCAR-OPF-LENGTH             PIC S9(04)       COMP.          
010300           04 TCAR-OPF-DATA               PIC  X(80).                     
010400           04 TCAR-OPF-DIGIT                                              
010500              REDEFINES TCAR-OPF-DATA     PIC  X(01)                      
010600                 OCCURS 80 TIMES                                          
010700                 INDEXED BY TCAR-OPF-DIGIT-IDX                            
010800                            TCAR-OPF-DIGIT-MAX-IDX.                       
