000100******************************************************************        
000200*     ELCDIOP4 IS THE ENGLISH LANGUAGE I/O MODULE PARM AREA      *        
000200*     FOR ELIOPGM.  THIS IS FOR USE WITH CODE VALUE.             *        
000400*     IT CONTAINS THE FOLLOWING:                                 *        
000500*           1. PARM FIELDS PASSED TO AND FROM THE I/O MODULE     *        
000600*           2. RECORD AREA ADDRESS POINTER                       *        
000700******************************************************************        
000800*                                                                *        
000900*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
001000*       *-*         U P D A T E   H I S T O R Y         *-*      *        
001100*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
001200*                                                                *        
001300**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
001400*                                                                *        
001500*    XXXX    03/05/86  JTC  ORIGINAL MODULE                      *        
001510*    0001    03/08/86  DES  ADDED ALTERNATE INDEX DDNAME FOR     *        
001520*                           ACCESS                               *        
001700*    0002    03/10/86  NAC  ADDED ELCIO-READ-PREVIOUS            *        
001900*                                                                *        
001600*                                                                *        
001700******************************************************************        
001800     05  ELCIO-PARM-AREA4.                                                
001900*                                                                         
002000         10  ELCIO-FILE-DDNAME4       PIC X(8).                           
002100*                                                                         
002200         10  ELCIO-VSAM-KEY4          PIC X(255).                         
002300*                                     MAX VSAM KEY                        
002400*                                                                         
002500         10  ELCIO-FILE-ACCESS-CODE4  PIC X(3).                           
002600*                  READ TYPE OPERATIONS                                   
002700             88  ELCIO-READ4               VALUE 'RD '.                   
002800             88  ELCIO-START-BROWSE4       VALUE 'SB '.                   
002900             88  ELCIO-START-BROWSE-GETPREV4 VALUE 'SBP'.                 
003000             88  ELCIO-GENERIC-START-BROWSE4 VALUE 'GB '.                 
003100             88  ELCIO-GENERIC-ST-BROWSE-GETP4                            
003200                                           VALUE 'GBP'.                   
003300             88  ELCIO-READ-NEXT4          VALUE 'RN '.                   
                   88  ELCIO-READ-PREVIOUS4      VALUE 'RP '.            NAC0002
003400             88  ELCIO-END-BROWSE4         VALUE 'EB '.                   
003500*                  UPDATE TYPE OPERATIONS                                 
003600             88  ELCIO-READ-UPDATE4        VALUE 'RU '.                   
003700             88  ELCIO-ADD-RECORD4         VALUE 'WU '.                   
003800*                                          (REWRITE)                      
003900             88  ELCIO-DEL-AFTER-RDUPDTE4  VALUE 'DLU'.                   
004000             88  ELCIO-UNLOCK-READ-UPDATE4 VALUE 'ULK'.                   
004100*                  WRITE/DELETE TYPE OPERATIONS                           
004200             88  ELCIO-DELETE4             VALUE 'DL '.                   
004300             88  ELCIO-WRITE-RECORD4       VALUE 'WR '.                   
004400*                                          (ADD A RECORD)                 
004500             88  ELCIO-WRITE4              VALUE 'WDP'.                   
004600*                                     WRITE (ADD A RECORD BUT             
004700*                                     IF DUPLICATE EXITS THEN             
004800*                                     RETURN RECORD FOUND FOR             
004900*                                     TASK TO DO UPDATE                   
005000*                                                                         
005100         10  ELCIO-CIO-QUAL4          PIC X(3).                           
005200*                                     BLANK = NULL                        
005300*                                     EQ  = EQUAL TO                      
005400*                                     GTE = GREATER THAN OR EQUAL         
005500*                                                                         
005600         10  ELCIO-BROWSE-KEYLEN4     PIC 9(4) COMP SYNC.                 
005700*                                     GENERIC BROWSE KEY LENGTH           
005800*                            ##NOTE## LENGTH IS REQUIRED ONLY             
005900*                                     WHEN STARTING A GENERIC             
006000*                                     BROWSE.                             
006100*                                                                         
006200         10  ELCIO-MAX-REC-LEN4       PIC 9(4) COMP SYNC.                 
006300*                                     THIS FIELD CONTAINS THE MAX         
006400*                                     RECORD LENGTH FOR THIS              
006500*                                     FILE ACCESS.  IT IS FILLED          
006600*                                     BY WHOMEVER OBTAINS THE             
006700*                                     GETMAIN AREA FOR THE RECORD.        
006800*                                                                         
006900         10  ELCIO-RECORD-LEN4        PIC 9(4) COMP SYNC.                 
007000*                                     RECORD LENGTH                       
007100*                            ##NOTE## RECORD LENGTH IS                    
007200*                                     REQUIRED INPUT FROM                 
007300*                                     CALLER WHEN: 'WR' 'WU'              
007400*                                     OR 'WDP' FUNCTIONS ARE              
007500*                                     ADDING OR UPDATING A                
007600*                                     RECORD.                             
007700*                                     FOR 'RD' 'RU' 'SB' 'RN'             
008100*                                     'GB' AND 'RP' FUNCTIONS THE  NAC0002
007900*                                     ACTUAL LENGTH OF THE                
008000*                                     RECORD READ WILL BE                 
008100*                                     RETURNED TO THE CALLER.             
008200*                                                                         
008300         10  ELCIO-STORAGE4           PIC X.                              
008400             88  ELCIO-REC-ADDR-AVAIL4 VALUE 'M'.                         
008500*                                     MOVE MODE.                          
008600*                                     RECORD AREA ADDRESS IS              
008700*                                     AVAILABLE FROM CALLER AND           
008800*                                     CAN BE FOUND IN HGIORECA            
008900             88  ELCIO-REC-VIA-GETMAIN4 VALUE 'P'.                        
009000*                                     POINTER MODE.                       
009100*                                     HGAIOPGM WILL OBTAIN RECORD         
009200*                                     AREA AND RETURN ADDRESS             
009300*                                     AFTER READ, READ FOR                
009400*                                     UPDATE, OR ANY BROWSE               
009500*                                                                         
009600         10  ELCIO-GETMAIN-IND4       PIC X.                              
009700*                INTERNALLY USED GETMAIN FLAGS.                           
009800         10  ELCIO-RETURN-CODE4       PIC X(2).                           
009900             88  ELCIO-GOOD-RETURN4        VALUE '00'.                    
010000             88  ELCIO-REC-NOT-FOUND4      VALUE '01'.                    
010100             88  ELCIO-EOF-BROWSE4         VALUE '03'.                    
010200             88  ELCIO-DUP-SECKEY-AIX4     VALUE '10'.                    
010300*                FUNCTION COMPLETED NORMALLY, HOWEVER, VSAM               
010400*                AIX LOGIC INDICATES ADDITIONAL RECORDS                   
010500*                WITH THE SAVE SECONDARY KEY ARE AVAILABLE.               
010600             88  ELCIO-DUP-KEY-ADD4        VALUE '12'.                    
010700*                DUPLICATE KEY FOUND ON THE FILE DURING ADD               
010800*                BUT CALLER WANTS THE DUPE RECORD PASSED                  
010900*                BACK FOR UPDATE PROCESSING.                              
011000*                                                                         
011100         10  ELCIO-ERROR-MESSAGE4     PIC X(16).                          
011200*                                                                         
011300         10  ELCIO-WORK-BROWSE-KEY4   PIC X(255).                         
011400*                                     WORK BROWSE KEY TO COVER            
011500*                                     CICS CONSIDERATIONS                 
011600*                                     FOR GENERIC BROWSES                 
011700*                                                                         
011800         10  ELCIO-REC-AREA-ADDRESS4  USAGE IS POINTER  SYNC.             
011900*                                     ADDRESS OF WHERE RECORD IS          
012000*                                     LOCATED.......                      
012100*                                     IF ELCIO-STORAGE = 'M'              
012200*                                     AND IT IS A READ TYPE               
012300*                                     OPERATION THIS FIELD MUST           
012400*                                     BE FILLED.                          
012500*                                                                         
012600         10  ELCIO-REC-AREA-DUP-ADDR4 USAGE IS POINTER   SYNC.            
012700*                                     ADDRESS OF WHERE RECORD IS          
012800*                                     LOCATED IF FOUND ON R/U             
012900*                                     DURING WDP ADD PROCESSING.          
013000*                                                                         
013100         10  ELCIO-REC-LENGTH4        PIC 9(4) COMP SYNC.                 
013200*                                     LENGTH OF RECORD DURING WDP         
013300         10  ELCIO-BRW-REQ-ID4        PIC 9(4) COMP SYNC.                 
013400*                                     BROWSE REQUIST ID                   
013500*                                     FOR MULTIPLE BROWSES                
013600*                                                                         
013700         10  ELCIO-ALT-INDEX-FILE-DDNAME4    PIC X(8).                    
013800*                                     USED WHEN ACCESSING A FILE          
013900*                                     THROUGH ALTERNATE INDEX             
