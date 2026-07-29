      *****************************************************************         
      **************          N O T E         *************************         
      * LENGTH = 300                                                            
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
      * VS COBOL II MEMBERS ARE PREFIXED BY \
      *****************************************************************         
      *                                                                *        
      ******************************************************************        
      *             GENERIC CONTRACT PROCESSING SYSTEM (GCPS)          *        
      *                  COPYMEMBER:  G2IOPRM7                         *        
      *                                                                *        
      *   THIS IS THE 7TH VERSION OF THE COBOL GENERIC CONTRACT I/O             
      * MODULE PARAMETER AREA.  IT CONTAINS PARAMETER FIELDS PASSED TO          
      * AND FROM THE I/O MODULE.                                                
      *                                                                         
      *                                  LAST UPDATED: 06/15/84 - DS            
      *                                                06/27/84 - DS            
      *                                                07/05/84 - DS            
      *                                                11/21/84 - SB            
      *                                                01/28/85 - TR            
      *                                CREATE 7TH VERS 10/02/86 - JLA           
      *                                                01/15/87 - JLA           
      * CORRECT FIELD (GCIO7-DUP-KEY-ADDRESS) TO INCLUDE POINTER- FRY           
      * ADD OPERATOR-ID INDICATOR                      05/20/87 - FRY           
      * D11665    8-27-92   FRY    ADD GENERIC DELETE  'DG ' OPTION TO *        
      *                            FILE ACCESS CODE.                   *        
      * 15057    8-8-97     AB     CHANGE  GCIO7-FILE-KEY              *        
      *                                    GCIO7-BROWSE-FILE-KEY       *        
      *                                    FROM 64 TO 100              *        
      *                                    LENGTH FROM 228 TO 300      *        
      *                                                                *        
      *                                                                *        
      *****************************************************************         
      *                                                                         
      *                                                                         
           05  GCIO7-PARMS.                                                     
             10  GCIO7-FILE-ACCESS-CODE              PIC XXX.                   
      ***                                                                       
      ***      FILE ACCESS CODE FROM CALLING PGM TO I/O MODULE.                 
      ***        DG  = DELETE A GROUP OF RECORDS USING A GENERIC KEY            
      ***        DL  = DELETE                                                   
      ***        DLU = DELETE (AFTER READ FOR UPDATE)                           
      ***        EB  = END BROWSE                                               
      ***        GB  = GENERIC START BROWSE (FOLLOWED BY READNEXT)              
      ***        RD  = READ                                                     
      ***        RN  = READ NEXT                                                
      ***        RP  = READ PREVIOUS                                            
      ***        RU  = READ FOR UPDATE                                          
      ***        SB  = START BROWSE (FOLLOWED BY READNEXT)                      
      ***        SBP = START BROWSE PREVIOUS (FOLLOWED BY READPREV)             
      ***        SBO = START BROWSE ONLY                                        
      ***        UNL = RELEASE EXCLUSIVE FILE CONTROL                           
      ***        WR  = WRITE (ADD A RECORD)                                     
      ***        WU  = WRITE UPDATE (REWRITE)                                   
      ***        WDP = WRITE (ADD RECORD) BUT ON DUP KEY DO NOT ABEND           
      ***                                                                       
             10  GCIO7-FILE-DDNAME                   PIC X(8).                  
             10  GCIO7-FILE-KEY                      PIC X(100).                
             10  GCIO7-BROWSE-FILE-KEY               PIC X(100).                
             10  GCIO7-BROWSE-QUAL-CODE              PIC XXX.                   
             10  GCIO7-BROWSE-KEYLEN                 PIC 9(4) COMP.             
             10  GCIO7-RECORD-LENGTH                 PIC 9(4) COMP.             
      ***        ** NOTE **      THE RECORD LENGTH IS REQUIRED INPUT            
      ***           FROM CALLER ONLY WHEN USING THE ADD RECORD FUNCTION         
      ***           OR UPDATING A RECORD WITH A CHANGED LENGTH.                 
             10  GCIO7-RETURN-CODE                   PIC X.                     
               88  GCIO7-GOOD-RETURN                     VALUE '0'.             
               88  GCIO7-RECORD-NOT-FOUND                VALUE '1'.             
               88  GCIO7-DUPLICATE-KEY                   VALUE '2'.             
               88  GCIO7-END-OF-FILE                     VALUE '3'.             
             10  GCIO7-MESSAGE-RETURNED              PIC X(16).                 
             10  GCIO7-IO-AREA-TO-USE                PIC X.                     
               88  GCIO7-NULL-VALUE                      VALUE ' '.             
               88  GCIO7-REC-AREA1                       VALUE '1'.             
               88  GCIO7-REC-AREA2                       VALUE '2'.             
             10  GCIO7-DUP-KEY-COMP                  PIC 9(8) COMP SYNC.        
             10  GCIO7-DUP-KEY-ADDRESS        REDEFINES                         
                 GCIO7-DUP-KEY-COMP                  POINTER.                   
             10  GCIO7-DUP-KEY-LENGTH                PIC 9(4) COMP.             
             10  GCIO7-OPER-ID-IND                   PIC X.                     
               88  GCIO7-OPER-ID-IND-ON                  VALUE '1'.             
               88  GCIO7-OPER-ID-IND-OFF                 VALUE '0'.             
             10  FILLER                              PIC X(57).                 
