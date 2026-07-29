000100******************************************************************        
000200*     ELCDIOPM IS THE ENGLISH LANGUAGE I/O MODULE PARM AREA      *        
000300*     FOR ELIOPGM.  IT CONTAINS THE FOLLOWING:                   *        
000400*           1. PARM FIELDS PASSED TO AND FROM THE I/O MODULE     *        
000500*           2. RECORD AREA ADDRESS POINTER                       *        
000600******************************************************************        
000700*                                                                *        
000800*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
000900*       *-*         U P D A T E   H I S T O R Y         *-*      *        
001000*       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
001100*                                                                *        
001200**-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
001300*                                                                *        
001400*    XXXX    03/04/86  AMJ  ORIGINAL MODULE                      *        
001500*    0001    03/05/86  JTC  CHANGE ELIO TO ELCIO ON ALL DATA     *        
001600*                           NAMES                                *        
001700*    0002    03/08/86  DES  ADDED ALTERNATE INDEX DDNAME FOR     *        
001800*                           ACCESS                               *        
001700*    0003    03/10/86  NAC  ADDED ELCIO-READ-PREVIOUS            *        
001700*    0004    03/19/86  DES  ADDED ELCIO-MASS-DELETE              *        
001900*                                                                *        
002000******************************************************************        
002100     05  ELCIO-PARM-AREA.                                                 
002200*                                                                         
002300         10  ELCIO-FILE-DDNAME        PIC X(8).                           
002400*                                                                         
002500         10  ELCIO-VSAM-KEY           PIC X(255).                         
002600*                                     MAX VSAM KEY                        
002700*                                                                         
002800         10  ELCIO-FILE-ACCESS-CODE   PIC X(3).                           
002900*                  READ TYPE OPERATIONS                                   
003000             88  ELCIO-READ                VALUE 'RD '.                   
003100             88  ELCIO-START-BROWSE        VALUE 'SB '.                   
003200             88  ELCIO-START-BROWSE-GETPREV VALUE 'SBP'.                  
003300             88  ELCIO-GENERIC-START-BROWSE VALUE 'GB '.                  
003400             88  ELCIO-GENERIC-ST-BROWSE-GETP                             
003500                                           VALUE 'GBP'.                   
003600             88  ELCIO-READ-NEXT           VALUE 'RN '.                   
                   88  ELCIO-READ-PREVIOUS       VALUE 'RP '.            NAC0003
003700             88  ELCIO-END-BROWSE          VALUE 'EB '.                   
003800*                  UPDATE TYPE OPERATIONS                                 
003900             88  ELCIO-READ-UPDATE         VALUE 'RU '.                   
004000             88  ELCIO-ADD-RECORD          VALUE 'WU '.                   
004100*                                          (REWRITE)                      
004200             88  ELCIO-DEL-AFTER-RDUPDTE   VALUE 'DLU'.                   
004300             88  ELCIO-UNLOCK-READ-UPDATE  VALUE 'ULK'.                   
004400*                  WRITE/DELETE TYPE OPERATIONS                           
004500             88  ELCIO-DELETE              VALUE 'DL '.                   
004500             88  ELCIO-MASS-DELETE         VALUE 'DLM'.                   
004600             88  ELCIO-WRITE-RECORD        VALUE 'WR '.                   
004700*                                          (ADD A RECORD)                 
004800             88  ELCIO-WRITE               VALUE 'WDP'.                   
004900*                                     WRITE (ADD A RECORD BUT             
005000*                                     IF DUPLICATE EXITS THEN             
005100*                                     RETURN RECORD FOUND FOR             
005200*                                     TASK TO DO UPDATE                   
005300*                                                                         
005400         10  ELCIO-CIO-QUAL           PIC X(3).                           
005500*                                     BLANK = NULL                        
005600*                                     EQ  = EQUAL TO                      
005700*                                     GTE = GREATER THAN OR EQUAL         
005800*                                                                         
005900         10  ELCIO-BROWSE-KEYLEN      PIC 9(4) COMP SYNC.                 
006000*                                     GENERIC BROWSE KEY LENGTH           
006100*                            ##NOTE## LENGTH IS REQUIRED ONLY             
006200*                                     WHEN STARTING A GENERIC             
006300*                                     BROWSE.                             
006400*                                                                         
006500         10  ELCIO-MAX-REC-LEN        PIC 9(4) COMP SYNC.                 
006600*                                     THIS FIELD CONTAINS THE MAX         
006700*                                     RECORD LENGTH FOR THIS              
006800*                                     FILE ACCESS.  IT IS FILLED          
006900*                                     BY WHOMEVER OBTAINS THE             
007000*                                     GETMAIN AREA FOR THE RECORD.        
007100*                                                                         
007200         10  ELCIO-RECORD-LEN         PIC 9(4) COMP SYNC.                 
007300*                                     RECORD LENGTH                       
007400*                            ##NOTE## RECORD LENGTH IS                    
007500*                                     REQUIRED INPUT FROM                 
007600*                                     CALLER WHEN: 'WR' 'WU'              
007700*                                     OR 'WDP' FUNCTIONS ARE              
007800*                                     ADDING OR UPDATING A                
007900*                                     RECORD.                             
008000*                                     FOR 'RD' 'RU' 'SB' 'RN'             
008100*                                     'GB' AND 'RP' FUNCTIONS THE  NAC0003
008200*                                     ACTUAL LENGTH OF THE                
008300*                                     RECORD READ WILL BE                 
008400*                                     RETURNED TO THE CALLER.             
008500*                                                                         
008600         10  ELCIO-STORAGE            PIC X.                              
008700             88  ELCIO-REC-ADDR-AVAIL VALUE 'M'.                          
008800*                                     MOVE MODE.                          
008900*                                     RECORD AREA ADDRESS IS              
009000*                                     AVAILABLE FROM CALLER AND           
009100*                                     CAN BE FOUND IN HGIORECA            
009200             88  ELCIO-REC-VIA-GETMAIN VALUE 'P'.                         
009300*                                     POINTER MODE.                       
009400*                                     ELAIOPGM WILL OBTAIN RECORD         
009500*                                     AREA AND RETURN ADDRESS             
009600*                                     AFTER READ, READ FOR                
009700*                                     UPDATE, OR ANY BROWSE               
009800*                                                                         
009900         10  ELCIO-GETMAIN-IND        PIC X.                              
010000*                INTERNALLY USED GETMAIN FLAGS.                           
010100         10  ELCIO-RETURN-CODE        PIC X(2).                           
010200             88  ELCIO-GOOD-RETURN         VALUE '00'.                    
010300             88  ELCIO-REC-NOT-FOUND       VALUE '01'.                    
010400             88  ELCIO-EOF-BROWSE          VALUE '03'.                    
010500             88  ELCIO-DUP-SECKEY-AIX      VALUE '10'.                    
010600*                FUNCTION COMPLETED NORMALLY, HOWEVER, VSAM               
010700*                AIX LOGIC INDICATES ADDITIONAL RECORDS                   
010800*                WITH THE SAVE SECONDARY KEY ARE AVAILABLE.               
010900             88  ELCIO-DUP-KEY-ADD         VALUE '12'.                    
011000*                DUPLICATE KEY FOUND ON THE FILE DURING ADD               
011100*                BUT CALLER WANTS THE DUPE RECORD PASSED                  
011200*                BACK FOR UPDATE PROCESSING.                              
011300*                                                                         
011400         10  ELCIO-ERROR-MESSAGE      PIC X(16).                          
011500*                                                                         
011600         10  ELCIO-WORK-BROWSE-KEY    PIC X(255).                         
011700*                                     WORK BROWSE KEY TO COVER            
011800*                                     CICS CONSIDERATIONS                 
011900*                                     FOR GENERIC BROWSES                 
012000*                                                                         
012100         10  ELCIO-REC-AREA-ADDRESS   USAGE IS POINTER  SYNC.             
012200*                                     ADDRESS OF WHERE RECORD IS          
012300*                                     LOCATED.......                      
012400*                                     IF ELCIO-STORAGE = 'M'              
012500*                                     AND IT IS A READ TYPE               
012600*                                     OPERATION THIS FIELD MUST           
012700*                                     BE FILLED.                          
012800*                                                                         
012900         10  ELCIO-REC-AREA-DUP-ADDR  USAGE IS POINTER    SYNC.           
013000*                                     ADDRESS OF WHERE RECORD IS          
013100*                                     LOCATED IF FOUND ON R/U             
013200*                                     DURING WDP ADD PROCESSING.          
013300*                                                                         
013400         10  ELCIO-REC-LENGTH2        PIC 9(4) COMP SYNC.                 
013500*                                     LENGTH OF RECORD DURING WDP         
013600         10  ELCIO-BRW-REQ-ID         PIC 9(4) COMP SYNC.                 
013700*                                     BROWSE REQUIST ID                   
013800*                                     FOR MULTIPLE BROWSES                
013900*                                                                         
014000         10  ELCIO-ALT-INDEX-FILE-DDNAME  PIC X(8).                       
014100*                                     USED WHEN ACCESSING A FILE          
014200*                                     THROUGH ALTERNATE INDEX             
