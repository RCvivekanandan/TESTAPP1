      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSOARRC                                        *        
      *    DATE:       05-JUL-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   COPYBOOK IS USED WITHIN THE OVERALL ACCUM       *        
      *                DETERMINATION SUBSYSTEM FOR ELIQ.  CONTAINS     *        
      *                THE ATTRIBUTE SELECTION PARAMETER LIST PER      *        
      *                ACCUM GIVEN BY SSD.  THIS LIST WILL THEN BE     *        
      *                USED TO BUILD ELSRRBLC COPYBOOK.                *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 05-JUL-1988 NAC       CREATED                            *        
      * 01.01 15-SEP-1988 NAC       CORRECTED OCCURS CLAUSE.           *        
      * 01.02 20-SEP-1988 NAC       DECREASED PIC FROM 62 TO 16 BYTES. *        
      ******************************************************************        
       01  WS-ABM-CMM-RR-LIST.                                                  
           03  WS-ABM-CMM-TBL-CNT         PIC S9(4) COMP   VALUE +0040.         
           03  WS-ABM-CMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNNYNN'.                     
           03  FILLER  REDEFINES  WS-ABM-CMM-FILLER.                            
               05  WS-ABM-CMM-ENTRY  OCCURS  40 TIMES  PIC X(16).               
                                                                                
       01  WS-ABM-BC-RR-LIST.                                                   
           03  WS-ABM-BC-TBL-CNT          PIC S9(4) COMP   VALUE +0017.         
           03  WS-ABM-BC-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ABM-BC-FILLER.                             
               05  WS-ABM-BC-ENTRY  OCCURS  17 TIMES PIC X(16).                 
                                                                                
       01  WS-ABM-BS-RR-LIST.                                                   
           03  WS-ABM-BS-TBL-CNT          PIC S9(4) COMP   VALUE +0017.         
           03  WS-ABM-BS-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ABM-BS-FILLER.                             
               05  WS-ABM-BS-ENTRY  OCCURS  17 TIMES PIC X(16).                 
                                                                                
       01  WS-ABM-SMM-RR-LIST.                                                  
           03  WS-ABM-SMM-TBL-CNT         PIC S9(4) COMP   VALUE +0012.         
           03  WS-ABM-SMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0BNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYNYNN'.                     
           03  FILLER  REDEFINES  WS-ABM-SMM-FILLER.                            
               05  WS-ABM-SMM-ENTRY OCCURS  12 TIMES PIC X(16).                 
                                                                                
       01  WS-ACL-CMM-RR-LIST.                                                  
           03  WS-ACL-CMM-TBL-CNT         PIC S9(4) COMP   VALUE +0040.         
           03  WS-ACL-CMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNNYNN'.                     
           03  FILLER  REDEFINES  WS-ACL-CMM-FILLER.                            
               05  WS-ACL-CMM-ENTRY OCCURS  40 TIMES PIC X(16).                 
                                                                                
       01  WS-ACL-BC-RR-LIST.                                                   
           03  WS-ACL-BC-TBL-CNT          PIC S9(4) COMP   VALUE +0018.         
           03  WS-ACL-BC-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ACL-BC-FILLER.                             
               05  WS-ACL-BC-ENTRY OCCURS   18 TIMES PIC X(16).                 
                                                                                
       01  WS-ACL-BS-RR-LIST.                                                   
           03  WS-ACL-BS-TBL-CNT          PIC S9(4) COMP   VALUE +0018.         
           03  WS-ACL-BS-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0PNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ACL-BS-FILLER.                             
               05  WS-ACL-BS-ENTRY OCCURS   18 TIMES PIC X(16).                 
                                                                                
       01  WS-ACL-SMM-RR-LIST.                                                  
           03  WS-ACL-SMM-TBL-CNT         PIC S9(4) COMP   VALUE +0009.         
           03  WS-ACL-SMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0BNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYNYNN'.                     
           03  FILLER  REDEFINES  WS-ACL-SMM-FILLER.                            
               05  WS-ACL-SMM-ENTRY OCCURS  09 TIMES PIC X(16).                 
                                                                                
       01  WS-ADL-CMM-RR-LIST.                                                  
           03  WS-ADL-CMM-TBL-CNT         PIC S9(4) COMP   VALUE +0054.         
           03  WS-ADL-CMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNNYNN'.                     
           03  FILLER  REDEFINES  WS-ADL-CMM-FILLER.                            
               05  WS-ADL-CMM-ENTRY OCCURS  54 TIMES PIC X(16).                 
                                                                                
       01  WS-ADL-BC-RR-LIST.                                                   
           03  WS-ADL-BC-TBL-CNT          PIC S9(4) COMP   VALUE +0028.         
           03  WS-ADL-BC-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0INYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNYNYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ADL-BC-FILLER.                             
               05  WS-ADL-BC-ENTRY OCCURS   28 TIMES PIC X(16).                 
                                                                                
       01  WS-ADL-BS-RR-LIST.                                                   
           03  WS-ADL-BS-TBL-CNT          PIC S9(4) COMP   VALUE +0028.         
           03  WS-ADL-BS-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0INYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0LYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0LYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0INYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0IYNNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ANYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0AYNNYYNNYNN'.                     
           03  FILLER  REDEFINES  WS-ADL-BS-FILLER.                             
               05  WS-ADL-BS-ENTRY OCCURS   28 TIMES PIC X(16).                 
                                                                                
       01  WS-ADL-SMM-RR-LIST.                                                  
           03  WS-ADL-SMM-TBL-CNT         PIC S9(4) COMP   VALUE +0018.         
           03  WS-ADL-SMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CYNYYNYYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0CYNYYNYYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CNYYYNYNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0CYNYYNYNYNN'.                     
           03  FILLER  REDEFINES  WS-ADL-SMM-FILLER.                            
               05  WS-ADL-SMM-ENTRY OCCURS  18 TIMES PIC X(16).                 
                                                                                
       01  WS-AOL-CMM-RR-LIST.                                                  
           03  WS-AOL-CMM-TBL-CNT         PIC S9(4) COMP   VALUE +0054.         
           03  WS-AOL-CMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYNNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYNNNYNN'.                     
           03  FILLER  REDEFINES  WS-AOL-CMM-FILLER.                            
               05  WS-AOL-CMM-ENTRY OCCURS  54 TIMES PIC X(16).                 
                                                                                
       01  WS-AOL-BC-RR-LIST.                                                   
           03  WS-AOL-BC-TBL-CNT          PIC S9(4) COMP   VALUE +0018.         
           03  WS-AOL-BC-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYYNYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNYNYNNYNN'.                     
           03  FILLER  REDEFINES  WS-AOL-BC-FILLER.                             
               05  WS-AOL-BC-ENTRY OCCURS   18 TIMES PIC X(16).                 
                                                                                
       01  WS-AOL-BS-RR-LIST.                                                   
           03  WS-AOL-BS-TBL-CNT          PIC S9(4) COMP   VALUE +0018.         
           03  WS-AOL-BS-FILLER.                                                
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BNYNYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0BYNNYYNNYNN'.                     
           03  FILLER  REDEFINES  WS-AOL-BS-FILLER.                             
               05  WS-AOL-BS-ENTRY OCCURS   18 TIMES PIC X(16).                 
                                                                                
       01  WS-AOL-SMM-RR-LIST.                                                  
           03  WS-AOL-SMM-TBL-CNT         PIC S9(4) COMP   VALUE +0012.         
           03  WS-AOL-SMM-FILLER.                                               
               05  FILLER   PIC X(16) VALUE '0DNYYYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYYNYYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYYNYNNN'.                     
               05  FILLER   PIC X(16) VALUE '0DNYYYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0DYNYYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0ENYYYYNNYNN'.                     
               05  FILLER   PIC X(16) VALUE '0EYNYYYNNYNN'.                     
           03  FILLER  REDEFINES  WS-AOL-SMM-FILLER.                            
               05  WS-AOL-SMM-ENTRY OCCURS  12 TIMES PIC X(16).                 
                                                                                
