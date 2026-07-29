      *****************************************************************         
      **************          N O T E         *************************         
      * LENGTH = 300                                                            
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
      * VS COBOL II MEMBERS ARE PREFIXED BY \
      *****************************************************************         
      *                                                                *        
      ******************************************************************        
      *          GENERIC CONTRACT PROCESSING SYSTEM (GCPS)             *        
      *              COPYMEMBER:  G2IOPRMF                             *        
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
      * CORRECT FIELD (GCIOF-DUP-KEY-ADDRESS) TO INCLUDE POINTER- FRY           
      * ADD OPERATOR-ID INDICATOR                      05/20/87 - FRY           
      *                               CREATE 14TH VERS 09/22/87 - JLA           
      * D11665    8-27-92   FRY    ADD GENERIC DELETE  'DG ' OPTION TO *        
      *                            FILE ACCESS CODE.                   *        
      * 15057    8-8-97     AB     CHANGE  GCIOF-FILE-KEY              *        
      *                                    GCIOF-BROWSE-FILE-KEY       *        
      *                                    FROM 64 TO 100              *        
      *                                    LENGTH FROM 228 TO 300      *        
      *                                                                *        
      *                                                                *        
      *****************************************************************         
      *                                                                         
      *                                                                         
           05  GCIOF-PARMS.                                                     
             10  GCIOF-FILE-ACCESS-CODE              PIC XXX.                   
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
             10  GCIOF-FILE-DDNAME                   PIC X(8).                  
             10  GCIOF-FILE-KEY                      PIC X(100).                
             10  GCIOF-BROWSE-FILE-KEY               PIC X(100).                
             10  GCIOF-BROWSE-QUAL-CODE              PIC XXX.                   
             10  GCIOF-BROWSE-KEYLEN                 PIC 9(4) COMP.             
             10  GCIOF-RECORD-LENGTH                 PIC 9(4) COMP.             
      ***        ** NOTE **      THE RECORD LENGTH IS REQUIRED INPUT            
      ***           FROM CALLER ONLY WHEN USING THE ADD RECORD FUNCTION         
      ***           OR UPDATING A RECORD WITH A CHANGED LENGTH.                 
             10  GCIOF-RETURN-CODE                   PIC X.                     
               88  GCIOF-GOOD-RETURN                     VALUE '0'.             
               88  GCIOF-RECORD-NOT-FOUND                VALUE '1'.             
               88  GCIOF-DUPLICATE-KEY                   VALUE '2'.             
               88  GCIOF-END-OF-FILE                     VALUE '3'.             
             10  GCIOF-MESSAGE-RETURNED              PIC X(16).                 
             10  GCIOF-IO-AREA-TO-USE                PIC X.                     
               88  GCIOF-NULL-VALUE                      VALUE ' '.             
               88  GCIOF-REC-AREA1                       VALUE '1'.             
               88  GCIOF-REC-AREA2                       VALUE '2'.             
             10  GCIOF-DUP-KEY-COMP                  PIC 9(8) COMP SYNC.        
             10  GCIOF-DUP-KEY-ADDRESS        REDEFINES                         
                 GCIOF-DUP-KEY-COMP                  POINTER.                   
             10  GCIOF-DUP-KEY-LENGTH                PIC 9(4) COMP.             
             10  GCIOF-OPER-ID-IND                   PIC X.                     
               88  GCIOF-OPER-ID-IND-ON                  VALUE '1'.             
               88  GCIOF-OPER-ID-IND-OFF                 VALUE '0'.             
             10  FILLER                              PIC X(57).                 
