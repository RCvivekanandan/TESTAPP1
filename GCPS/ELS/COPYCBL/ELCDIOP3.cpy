000100******************************************************************        
000200*     ELCDIOP3 IS THE ENGLISH LANGUAGE I/O MODULE PARM AREA      *        
000200*     FOR ELIOPGM.  THIS IS FOR USE WHEN THE PROGRAM ACCESSES    *        
000400*     A THIRD FILE.                                              *        
000500*     IT CONTAINS THE FOLLOWING:                                 *        
000600*           1. PARM FIELDS PASSED TO AND FROM THE I/O MODULE     *        
000700*           2. RECORD AREA ADDRESS POINTER                       *        
000800******************************************************************        
000900*                                                                *        
001000*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
001100*       *-*         U P D A T E   H I S T O R Y         *-*      *        
001200*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
001300*                                                                *        
001400**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
001500*                                                                *        
001600*    XXXX    03/05/86  JTC  ORIGINAL MODULE                      *        
001610*    0001    03/08/86  DES  ADDED ALTERNATE INDEX DDNAME FOR     *        
001620*                           ACCESS                               *        
001700*    0002    03/10/86  NAC  ADDED ELCIO-READ-PREVIOUS            *        
001900*                                                                *        
001700*                                                                *        
001800******************************************************************        
001900     05  ELCIO-PARM-AREA3.                                                
002000*                                                                         
002100         10  ELCIO-FILE-DDNAME3       PIC X(8).                           
002200*                                                                         
002300         10  ELCIO-VSAM-KEY3          PIC X(255).                         
002400*                                     MAX VSAM KEY                        
002500*                                                                         
002600         10  ELCIO-FILE-ACCESS-CODE3  PIC X(3).                           
002700*                  READ TYPE OPERATIONS                                   
002800             88  ELCIO-READ3               VALUE 'RD '.                   
002900             88  ELCIO-START-BROWSE3       VALUE 'SB '.                   
003000             88  ELCIO-START-BROWSE-GETPREV3 VALUE 'SBP'.                 
003100             88  ELCIO-GENERIC-START-BROWSE3 VALUE 'GB '.                 
003200             88  ELCIO-GENERIC-ST-BROWSE-GETP3                            
003300                                           VALUE 'GBP'.                   
003400             88  ELCIO-READ-NEXT3          VALUE 'RN '.                   
                   88  ELCIO-READ-PREVIOUS3      VALUE 'RP '.            NAC0002
003500             88  ELCIO-END-BROWSE3         VALUE 'EB '.                   
003600*                  UPDATE TYPE OPERATIONS                                 
003700             88  ELCIO-READ-UPDATE3        VALUE 'RU '.                   
003800             88  ELCIO-ADD-RECORD3         VALUE 'WU '.                   
003900*                                          (REWRITE)                      
004000             88  ELCIO-DEL-AFTER-RDUPDTE3  VALUE 'DLU'.                   
004100             88  ELCIO-UNLOCK-READ-UPDATE3 VALUE 'ULK'.                   
004200*                  WRITE/DELETE TYPE OPERATIONS                           
004300             88  ELCIO-DELETE3             VALUE 'DL '.                   
004400             88  ELCIO-WRITE-RECORD3       VALUE 'WR '.                   
004500*                                          (ADD A RECORD)                 
004600             88  ELCIO-WRITE3              VALUE 'WDP'.                   
004700*                                     WRITE (ADD A RECORD BUT             
004800*                                     IF DUPLICATE EXITS THEN             
004900*                                     RETURN RECORD FOUND FOR             
005000*                                     TASK TO DO UPDATE                   
005100*                                                                         
005200         10  ELCIO-CIO-QUAL3          PIC X(3).                           
005300*                                     BLANK = NULL                        
005400*                                     EQ  = EQUAL TO                      
005500*                                     GTE = GREATER THAN OR EQUAL         
005600*                                                                         
005700         10  ELCIO-BROWSE-KEYLEN3     PIC 9(4) COMP SYNC.                 
005800*                                     GENERIC BROWSE KEY LENGTH           
005900*                            ##NOTE## LENGTH IS REQUIRED ONLY             
006000*                                     WHEN STARTING A GENERIC             
006100*                                     BROWSE.                             
006200*                                                                         
006300         10  ELCIO-MAX-REC-LEN3       PIC 9(4) COMP SYNC.                 
006400*                                     THIS FIELD CONTAINS THE MAX         
006500*                                     RECORD LENGTH FOR THIS              
006600*                                     FILE ACCESS.  IT IS FILLED          
006700*                                     BY WHOMEVER OBTAINS THE             
006800*                                     GETMAIN AREA FOR THE RECORD.        
006900*                                                                         
007000         10  ELCIO-RECORD-LEN3        PIC 9(4) COMP SYNC.                 
007100*                                     RECORD LENGTH                       
007200*                            ##NOTE## RECORD LENGTH IS                    
007300*                                     REQUIRED INPUT FROM                 
007400*                                     CALLER WHEN: 'WR' 'WU'              
007500*                                     OR 'WDP' FUNCTIONS ARE              
007600*                                     ADDING OR UPDATING A                
007700*                                     RECORD.                             
007800*                                     FOR 'RD' 'RU' 'SB' 'RN'             
008100*                                     'GB' AND 'RP' FUNCTIONS THE  NAC0002
008000*                                     ACTUAL LENGTH OF THE                
008100*                                     RECORD READ WILL BE                 
008200*                                     RETURNED TO THE CALLER.             
008300*                                                                         
008400         10  ELCIO-STORAGE3           PIC X.                              
008500             88  ELCIO-REC-ADDR-AVAIL3 VALUE 'M'.                         
008600*                                     MOVE MODE.                          
008700*                                     RECORD AREA ADDRESS IS              
008800*                                     AVAILABLE FROM CALLER AND           
008900*                                     CAN BE FOUND IN HGIORECA            
009000             88  ELCIO-REC-VIA-GETMAIN3 VALUE 'P'.                        
009100*                                     POINTER MODE.                       
009200*                                     HGAIOPGM WILL OBTAIN RECORD         
009300*                                     AREA AND RETURN ADDRESS             
009400*                                     AFTER READ, READ FOR                
009500*                                     UPDATE, OR ANY BROWSE               
009600*                                                                         
009700         10  ELCIO-GETMAIN-IND3       PIC X.                              
009800*                INTERNALLY USED GETMAIN FLAGS.                           
009900         10  ELCIO-RETURN-CODE3       PIC X(2).                           
010000             88  ELCIO-GOOD-RETURN3        VALUE '00'.                    
010100             88  ELCIO-REC-NOT-FOUND3      VALUE '01'.                    
010200             88  ELCIO-EOF-BROWSE3         VALUE '03'.                    
010300             88  ELCIO-DUP-SECKEY-AIX3     VALUE '10'.                    
010400*                FUNCTION COMPLETED NORMALLY, HOWEVER, VSAM               
010500*                AIX LOGIC INDICATES ADDITIONAL RECORDS                   
010600*                WITH THE SAVE SECONDARY KEY ARE AVAILABLE.               
010700             88  ELCIO-DUP-KEY-ADD3        VALUE '12'.                    
010800*                DUPLICATE KEY FOUND ON THE FILE DURING ADD               
010900*                BUT CALLER WANTS THE DUPE RECORD PASSED                  
011000*                BACK FOR UPDATE PROCESSING.                              
011100*                                                                         
011200         10  ELCIO-ERROR-MESSAGE3     PIC X(16).                          
011300*                                                                         
011400         10  ELCIO-WORK-BROWSE-KEY3   PIC X(255).                         
011500*                                     WORK BROWSE KEY TO COVER            
011600*                                     CICS CONSIDERATIONS                 
011700*                                     FOR GENERIC BROWSES                 
011800*                                                                         
011900         10  ELCIO-REC-AREA-ADDRESS3  USAGE IS POINTER  SYNC.             
012000*                                     ADDRESS OF WHERE RECORD IS          
012100*                                     LOCATED.......                      
012200*                                     IF ELCIO-STORAGE = 'M'              
012300*                                     AND IT IS A READ TYPE               
012400*                                     OPERATION THIS FIELD MUST           
012500*                                     BE FILLED.                          
012600*                                                                         
012700         10  ELCIO-REC-AREA-DUP-ADDR3 USAGE IS POINTER  SYNC.             
012800*                                     ADDRESS OF WHERE RECORD IS          
012900*                                     LOCATED IF FOUND ON R/U             
013000*                                     DURING WDP ADD PROCESSING.          
013100*                                                                         
013200         10  ELCIO-REC-LENGTH3        PIC 9(4) COMP SYNC.                 
013300*                                     LENGTH OF RECORD DURING WDP         
013400         10  ELCIO-BRW-REQ-ID3        PIC 9(4) COMP SYNC.                 
013500*                                     BROWSE REQUIST ID                   
013600*                                     FOR MULTIPLE BROWSES                
013700*                                                                         
013800         10  ELCIO-ALT-INDEX-FILE-DDNAME3    PIC X(8).                    
013900*                                     USED WHEN ACCESSING A FILE          
014000*                                     THROUGH ALTERNATE INDEX             
