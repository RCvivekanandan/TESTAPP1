      *****************************************************************         
      **************          N O T E         *************************         
      * LENGTH = 100                                                            
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
      * VS COBOL II MEMBERS ARE PREFIXED BY \
      *****************************************************************         
      *                                                                *        
      ******************************************************************        
      *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *        
      *             COPYMEMBER:  G2IOPRMB                              *        
      *                                                                *        
      *   THIS IS THE 10TH VERSION OF THE COBOL GENERIC CONTRACT I/O            
      * MODULE PARAMETER AREA.  IT CONTAINS PARAMETER FIELDS PASSED TO          
      * AND FROM THE I/O MODULE.                                                
      *                                                                         
      *                                  LAST UPDATED: 06/15/84 - DS            
      *                                                06/27/84 - DS            
      *                                                07/05/84 - DS            
      *                                                11/21/84 - SB            
      *                                                01/28/85 - TR            
      *                                                01/15/87 - JLA           
      * CORRECT FIELD (GCIOB-DUP-KEY-ADDRESS) TO INCLUDE POINTER- FRY           
      * ADD OPERATOR-ID INDICATOR                      05/20/87 - FRY           
      *                               CREATE 11TH VERS 09/22/87 - JLA           
      * D11665    8-27-92   FRY    ADD GENERIC DELETE  'DG ' OPTION TO *        
      *                            FILE ACCESS CODE.                   *        
      * 15057    8-8-97     AB     CHANGE  GCIOB-FILE-KEY              *        
      *                                    GCIOB-BROWSE-FILE-KEY       *        
      *                                    FROM 64 TO 100              *        
      *                                    LENGTH FROM 228 TO 300      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
      *                                                                         
      *                                                                         
           05  GCIOB-PARMS.                                                     
             10  GCIOB-FILE-ACCESS-CODE              PIC XXX.                   
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
             10  GCIOB-FILE-DDNAME                   PIC X(8).                  
             10  GCIOB-FILE-KEY                      PIC X(100).                
             10  GCIOB-BROWSE-FILE-KEY               PIC X(100).                
             10  GCIOB-BROWSE-QUAL-CODE              PIC XXX.                   
             10  GCIOB-BROWSE-KEYLEN                 PIC 9(4) COMP.             
             10  GCIOB-RECORD-LENGTH                 PIC 9(4) COMP.             
      ***        ** NOTE **      THE RECORD LENGTH IS REQUIRED INPUT            
      ***           FROM CALLER ONLY WHEN USING THE ADD RECORD FUNCTION         
      ***           OR UPDATING A RECORD WITH A CHANGED LENGTH.                 
             10  GCIOB-RETURN-CODE                   PIC X.                     
               88  GCIOB-GOOD-RETURN                     VALUE '0'.             
               88  GCIOB-RECORD-NOT-FOUND                VALUE '1'.             
               88  GCIOB-DUPLICATE-KEY                   VALUE '2'.             
               88  GCIOB-END-OF-FILE                     VALUE '3'.             
             10  GCIOB-MESSAGE-RETURNED              PIC X(16).                 
             10  GCIOB-IO-AREA-TO-USE                PIC X.                     
               88  GCIOB-NULL-VALUE                      VALUE ' '.             
               88  GCIOB-REC-AREA1                       VALUE '1'.             
               88  GCIOB-REC-AREA2                       VALUE '2'.             
             10  GCIOB-DUP-KEY-COMP                  PIC 9(8) COMP SYNC.        
             10  GCIOB-DUP-KEY-ADDRESS        REDEFINES                         
                 GCIOB-DUP-KEY-COMP                  POINTER.                   
             10  GCIOB-DUP-KEY-LENGTH                PIC 9(4) COMP.             
             10  GCIOB-OPER-ID-IND                   PIC X.                     
               88  GCIOB-OPER-ID-IND-ON                  VALUE '1'.             
               88  GCIOB-OPER-ID-IND-OFF                 VALUE '0'.             
             10  FILLER                              PIC X(57).                 
