           05  MEMBERSHIP-GC-INTERFACE-REC.                                     
      ******************************************************************        
      ***                     G2MBRINC                                          
      ***    GENERIC CONTRACT INTERFACE RECORD IS USED TO PASS KEY              
      ***  VALUES, REQUEST AND RETURN CODES, AND AN ADDRESS POINTER             
      ***  BETWEEN THE GENERIC CONTRACT INTERFACE APPLICATION                   
      ***  DEVELOPMENT PROGRAM AND THE MEMBERSHIP PROGRAMS.                     
      ***                                                                       
      ***   VS COBOL 2 VERSION OF GCMBRINC                                      
      ******************************************************************        
      **************          N O T E         *************************         
      *                                                                         
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS.  ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.              
      *****************************************************************         
      *** * * * * * * +-------------------------+ * * * * * * * * * * **        
      *** * * * * * * |   U P D A T E   L O G   | * * * * * * * * * * **        
      *** * * * * * * +-------------------------+ * * * * * * * * * * **        
      ***-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------        
      *                                                                         
      *  XXXXX    11/10/87    DES   INITIAL INSTALLATION                        
      *                                                                         
      ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****        
      ******************************************************************        
      ***                                                                       
             10  GCI-REQUEST-CODE              PIC XX.                          
               88  GCI-DATES-COVERAGE-TABLE-REQ    VALUE 'D0'.                  
               88  GCI-GROUP-SPECIFIC              VALUE 'GI'.                  
                                                                                
             10  GCI-RETURN-CODE               PIC XX.                          
               88  GCI-INVALID-REQUEST             VALUE '40'.                  
               88  GCI-REQUEST-SUCCESSFUL          VALUE '00'.                  
               88  GCI-RECORD-NOT-FOUND            VALUE '10'.                  
               88  GCI-DATES-NOT-COVERED           VALUE '05'.                  
      ***                                                                       
      ***   B E G I N N I N G   O F   K E Y   A R E A                           
             10  GCI-GROUP-NO-9                PIC X(9).                        
             10  FILLER    REDEFINES    GCI-GROUP-NO-9.                         
               15  GCI-GROUP-NO-PREF           PIC XXX.                         
               15  GCI-GROUP-NO-6              PIC X(6).                        
             10  GCI-SECTION-NO-5              PIC X(5).                        
             10  FILLER    REDEFINES    GCI-SECTION-NO-5.                       
               15  GCI-SECTION-NO-PREF         PIC X.                           
               15  GCI-SECTION-NO-4            PIC X(4).                        
             10  GCI-LOB                       PIC X.                           
             10  GCI-FAM-REL-LVL               PIC X.                           
             10  GCI-PROV-CTL                  PIC XX.                          
             10  GCI-EFFECTIVE-DATE            PIC S9(5)  COMP-3.               
             10  FILLER                        PIC X(3).                        
                                                                                
      *** THIS BLOCK OF CODE IS USED BY THE GET DATES ROUTINE TO                
      *** FIND A LIST OF GROUP SPECIFIC AND CONTRACT RECORDS THAT               
      *** COVER A PERIOD OF TIME FOR AN INDIVIDUAL CLAIM.  THE                  
      *** LAST FIELD IN THIS BLOCK IS PATIENT-AGE.                              
             10  GCI-FAM-REL-LVL-MEDICARE      PIC X.                           
             10  GCI-FAM-REL-LVL-M-S-D         PIC X.                           
             10  GCI-FAM-REL-LVL-LO-HI         PIC X.                           
                                                                                
             10  GCI-PROV-CTL-PLAN-BASIC       PIC X.                           
             10  GCI-PROV-CTL-PPO-BASIC        PIC X.                           
             10  GCI-PROV-CTL-EMPL-BASIC       PIC X.                           
             10  GCI-PROV-CTL-PLAN-MM          PIC X.                           
             10  GCI-PROV-CTL-PPO-MM           PIC X.                           
             10  GCI-PROV-CTL-EMPL-MM          PIC X.                           
                                                                                
             10  GCI-START-OR-NEXT-SVC-DT      PIC S9(5)  COMP-3.               
             10  GCI-END-SVC-DT                PIC S9(5)  COMP-3.               
             10  GCI-PATIENT-AGE               PIC S9(3)  COMP-3.               
                                                                                
             10  GCI-DEPENDENT-MAX-AGE         PIC S9(3)  COMP-3.               
             10  GCI-STUDENT-MAX-AGE           PIC S9(3)  COMP-3.               
                                                                                
             10    FILLER                      PIC X(7).                        
      ***                                                                       
      ***    B E G I N N I N G   O F   R E C O R D   P N T R S                  
      ***                                                                       
      ***    POINTER TO THE RECORD REQUESTED BY THE CALLING PROGRAM             
      ***    LENGTH OF THE ABOVE RECORD, THIS FIELD MAY BE READ BUT DO          
      ***    NOT MODIFY IT AS THIS MAY CAUSE A STORAGE MGT. SYSTEM ABEND        
             10  GCI-GEN-REQUEST-REC-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI-GEN-REQUEST-REC-ADDR      REDEFINES                        
                 GCI-GEN-REQUEST-REC-COMP      POINTER.                         
             10  GCI-GEN-REQUEST-REC-LEN       PIC S9(4)  COMP  SYNC.           
             10    FILLER                      PIC X(2).                        
             10  GCI-INTERFACE-PARMS-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI-INTERFACE-PARMS-ADDR      REDEFINES                        
                 GCI-INTERFACE-PARMS-COMP      POINTER.                         
      ***    E N D   O F   R E C O R D   P N T R S                              
      ***                                                                       
      ***   F I L L E R   F O R   P O S S I B L E   F U T U R E   U S E         
             10  FILLER                        PIC X(196).                      
      ***                                                                       
      ***   LENGTH OF THIS INTERFACE AREA IS 264 BYTES LONG.                    
      ***                                                                       
