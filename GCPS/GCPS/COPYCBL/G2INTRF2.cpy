           05  GENERIC-CONTRACT-INTERFACE-RC2.                                  
      ******************************************************************        
      ******************************************************************        
      ***                     G2INTRF2                                          
      ***    GENERIC CONTRACT INTERFACE RECORD IS USED TO                       
      ***  PASS KEY VALUES, REQUEST AND RETURN CODES, AND                       
      ***  ADDRESS POINTERS BETWEEN THE GENERIC CONTRACT                        
      ***  INTERFACE CONTROLLER PROGRAM AND ANY CALLING PROGRAM.                
      ***                                                                       
      ***  NOTE:  THERE IS A FULL DESCRIPTION OF THE FIELDS IN THIS             
      ***         COMMUNICATIONS AREA STARTING ON PAGE 2.                       
      ***                                                                       
      ***   VS COBOL 2 VERSION OF GCINTRFC                                      
      ***                                                                       
      ******************************************************************        
      *****************************************************************         
      **************          N O T E         *************************         
      *                                                                         
      *   THIS COPYLIB MEMBER IS USED IN BOTH OS COBOL AND VS COBOL II          
      * PROGRAMS. ANY MODIFICATIONS MUST BE MADE TO BOTH MEMBERS.               
      * VS COBOL II MEMBERS ARE PREFIXED BY \
      *****************************************************************         
      *****************************************************************         
      *****************************************************************         
      ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****        
      *** * * * * * * +-------------------------+ * * * * * * * * * * **        
      *** * * * * * * |   U P D A T E   L O G   | * * * * * * * * * * **        
      *** * * * * * * +-------------------------+ * * * * * * * * * * **        
      ***-LOG#-* *--DATE--* *-WHO-* *--------DESCRIPTION----------------        
      *                                                                         
      *  XXXXX    11/11/87    DES   GENERATED FROM G2INTRFC                     
      *                                                                         
      ***** * * * * * * * * * * * * * * * * * * * * * * * * * * * * ****        
      ******************************************************************        
      ***                                                                       
             10  GCI2-REQUEST-CODE             PIC XX.                          
             10  GCI2-APPLICATION-ID           PIC X(4).                        
             10  GCI2-RETURN-CODE              PIC XX.                          
      ***                                                                       
      ***   B E G I N N I N G   O F   K E Y   A R E A                           
             10  GCI2-GROUP-NO-9               PIC X(9).                        
             10  FILLER    REDEFINES    GCI2-GROUP-NO-9.                        
               15  GCI2-GROUP-NO-PREF          PIC XXX.                         
               15  GCI2-GROUP-NO-6             PIC X(6).                        
             10  GCI2-SECTION-NO-5             PIC X(5).                        
             10  FILLER    REDEFINES    GCI2-SECTION-NO-5.                      
               15  GCI2-SECTION-NO-PREF        PIC X.                           
               15  GCI2-SECTION-NO-4           PIC X(4).                        
             10  GCI2-LOB                      PIC X.                           
             10  GCI2-FAM-REL-LVL              PIC X.                           
             10  GCI2-PROV-CTL                 PIC XX.                          
             10  GCI2-EFFECTIVE-DATE           PIC S9(5)  COMP-3.               
             10  GCI2-BENEFIT-ID.                                               
               15  GCI2-BENEFIT-PVSN-ID        PIC X(5).                        
               15  GCI2-BENEFIT-PVSN-TYPE      PIC X.                           
             10  GCI2-GROUP-TABULAR-ID         PIC X(6).                        
             10  GCI2-CONTRACT-TABULAR-ID      PIC X(6).                        
             10  GCI2-BEN-PVSN-TABULAR-ID      PIC X(6).                        
             10  GCI2-TABULARS-TABULAR-ID      PIC X(6).                        
             10  GCI2-TABULARS-TABULAR-SLOT-NO PIC S9(7)  COMP-3.               
      ***   THE FOLLOWING KEY FIELDS ARE SET ASIDE FOR SPECIALIZED              
      ***   ROUTINE USAGE.  THEY ARE NOT USED BY THE GENERALIZED RTNS.          
                                                                                
             10  GCI2-BENEFIT-OR-TABULAR-ID.                                    
                 15  GCI2-BENEFIT-IDENTIFIER   PIC X(6).                        
                 15  FILLER     REDEFINES    GCI2-BENEFIT-IDENTIFIER.           
                     20  GCI2-TABULAR-ID       PIC X(5).                        
                     20  FILLER                PIC X(1).                        
                 15  GCI2-SLOT-NO              PIC S9(7)  COMP-3.               
                                                                                
             10  GCI2-CROSS-REF-TAB-REC-TYPE   PIC X.                           
             10  GCI2-INTERFACE-WORK-IND       PIC X.                           
             10  FILLER                        PIC X(2).                        
                                                                                
             10  GCI2-FAM-REL-LVL-MEDICARE     PIC X.                           
             10  GCI2-FAM-REL-LVL-M-S-D        PIC X.                           
             10  GCI2-FAM-REL-LVL-LO-HI        PIC X.                           
                                                                                
             10  GCI2-PROV-CTL-PLAN-BASIC      PIC X.                           
             10  GCI2-PROV-CTL-PPO-BASIC       PIC X.                           
             10  GCI2-PROV-CTL-EMPL-BASIC      PIC X.                           
             10  GCI2-PROV-CTL-PLAN-MM         PIC X.                           
             10  GCI2-PROV-CTL-PPO-MM          PIC X.                           
             10  GCI2-PROV-CTL-EMPL-MM         PIC X.                           
                                                                                
             10  GCI2-START-OR-NEXT-SVC-DT     PIC S9(5)  COMP-3.               
             10  GCI2-END-SVC-DT               PIC S9(5)  COMP-3.               
             10  GCI2-PATIENT-AGE              PIC S9(3)  COMP-3.               
                                                                                
             10    FILLER                      PIC X(6).                        
      ***    E N D   O F   K E Y   A R E A                                      
      ***                                                                       
      ***                                                                       
      ***    B E G I N N I N G   O F   R E C O R D   P N T R S                  
             10  GCI2-GROUP-SPEC-REC-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI2-GROUP-SPEC-REC-ADDR      REDEFINES                        
                 GCI2-GROUP-SPEC-REC-COMP      POINTER.                         
             10  GCI2-CONTRACT-REC-COMP        PIC S9(8)  COMP  SYNC.           
             10  GCI2-CONTRACT-REC-ADDR        REDEFINES                        
                 GCI2-CONTRACT-REC-COMP        POINTER.                         
             10  GCI2-BENEFIT-PVSN-REC-COMP    PIC S9(8)  COMP  SYNC.           
             10  GCI2-BENEFIT-PVSN-REC-ADDR    REDEFINES                        
                 GCI2-BENEFIT-PVSN-REC-COMP    POINTER.                         
             10  GCI2-BENEFIT-PVSN-SAVE-1      REDEFINES                        
                 GCI2-BENEFIT-PVSN-REC-COMP    POINTER.                         
             10  GCI2-BENEFIT-PVSN-2-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI2-BENEFIT-PVSN-SAVE-2      REDEFINES                        
                 GCI2-BENEFIT-PVSN-2-COMP      POINTER.                         
             10  GCI2-BENEFIT-PVSN-3-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI2-BENEFIT-PVSN-SAVE-3      REDEFINES                        
                 GCI2-BENEFIT-PVSN-3-COMP      POINTER.                         
             10  GCI2-BENEFIT-PVSN-W-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI2-BENEFIT-PVSN-SAVE-W      REDEFINES                        
                 GCI2-BENEFIT-PVSN-W-COMP      POINTER.                         
             10    FILLER                      PIC X(4).                        
             10  GCI2-GROUPS-TAB-REC-COMP      PIC S9(8)  COMP  SYNC.           
             10  GCI2-GROUPS-TAB-REC-ADDR      REDEFINES                        
                 GCI2-GROUPS-TAB-REC-COMP      POINTER.                         
             10  GCI2-CONTRACTS-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
             10  GCI2-CONTRACTS-TAB-REC-ADDR   REDEFINES                        
                 GCI2-CONTRACTS-TAB-REC-COMP   POINTER.                         
             10  GCI2-BEN-PVSNS-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
             10  GCI2-BEN-PVSNS-TAB-REC-ADDR   REDEFINES                        
                 GCI2-BEN-PVSNS-TAB-REC-COMP   POINTER.                         
             10  GCI2-TABULARS-TAB-REC-COMP    PIC S9(8)  COMP  SYNC.           
             10  GCI2-TABULARS-TAB-REC-ADDR    REDEFINES                        
                 GCI2-TABULARS-TAB-REC-COMP    POINTER.                         
             10  GCI2-ACCUMULATOR-TABLE-COMP   PIC S9(8)  COMP  SYNC.           
             10  GCI2-ACCUMULATOR-TABLE-ADDR   REDEFINES                        
                 GCI2-ACCUMULATOR-TABLE-COMP   POINTER.                         
             10  GCI2-DATES-REC-TAB-COMP       PIC S9(8)  COMP  SYNC.           
             10  GCI2-DATES-REC-TAB-ADDR       REDEFINES                        
                 GCI2-DATES-REC-TAB-COMP       POINTER.                         
             10  GCI2-CROSS-REF-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
             10  GCI2-CROSS-REF-TAB-REC-ADDR   REDEFINES                        
                 GCI2-CROSS-REF-TAB-REC-COMP   POINTER.                         
             10  GCI2-INTERFACE-WORKAREA1-COMP PIC S9(8)  COMP  SYNC.           
             10  GCI2-INTERFACE-WORK-AREA-1    REDEFINES                        
                 GCI2-INTERFACE-WORKAREA1-COMP POINTER.                         
             10  GCI2-INTERFACE-WORKAREA2-COMP PIC S9(8)  COMP  SYNC.           
             10  GCI2-INTERFACE-WORK-AREA-2    REDEFINES                        
                 GCI2-INTERFACE-WORKAREA2-COMP POINTER.                         
             10  GCI2-INTERFACE-WORKAREA3-COMP PIC S9(8)  COMP  SYNC.           
             10  GCI2-INTERFACE-WORK-AREA-3    REDEFINES                        
                 GCI2-INTERFACE-WORKAREA3-COMP POINTER.                         
             10  FILLER                        PIC X(12).                       
      ***    E N D   O F   R E C O R D   P N T R S                              
      ***                                                                       
      ***                                                                       
             10  FILLER                        PIC X(200).                      
      ***                                                                       
      ***   LENGTH OF THIS INTERFACE AREA IS 380 BYTES LONG.                    
      ***                                                                       
           05  FILLER   REDEFINES    GENERIC-CONTRACT-INTERFACE-RC2.            
      ***                                                                       
      ***                                                                       
             10  FILLER                        PIC XX.                          
      *      10  GCI2-REQUEST-CODE             PIC XX.                          
               88  GCI2-GROUP-SPECIFIC             VALUE 'G0'.                  
               88  GCI2-CONTRACT                   VALUE 'C0'.                  
               88  GCI2-BENEFIT-PVSN-1             VALUE 'B1'.                  
               88  GCI2-BENEFIT-PVSN-2             VALUE 'B2'.                  
               88  GCI2-BENEFIT-PVSN-3             VALUE 'B3'.                  
               88  GCI2-BENEFIT-PVSN-W             VALUE 'BW'.                  
               88  GCI2-GROUP-SPEC-TAB             VALUE 'TG'.                  
               88  GCI2-CONTRACT-TAB               VALUE 'TC'.                  
               88  GCI2-BENEFIT-PVSN-TAB           VALUE 'TB'.                  
               88  GCI2-CROSS-REF-TAB              VALUE 'TX'.                  
               88  GCI2-CROSS-REF-NEXT-TAB         VALUE 'TN'.                  
               88  GCI2-GROUP-ALL-LEV-INTERNAL-TB  VALUE 'IG'.                  
               88  GCI2-CONTR-ALL-LEV-INTERNAL-TB  VALUE 'IC'.                  
               88  GCI2-BEN-P-ALL-LEV-INTERNAL-TB  VALUE 'IB'.                  
               88  GCI2-DATES-COVERAGE-TABLE-REQ   VALUE 'D0'.                  
               88  GCI2-GROUP-SPEC-TAB-FUNC-TO-AR  VALUE 'FG'.                  
               88  GCI2-CONTRACT-TAB-FUNC-TO-ARG   VALUE 'FC'.                  
               88  GCI2-BEN-PVSN-W-TAB-FUNC-TO-AR  VALUE 'FW'.                  
               88  GCI2-BEN-PVSN-1-TAB-FUNC-TO-AR  VALUE 'F1'.                  
               88  GCI2-BEN-PVSN-2-TAB-FUNC-TO-AR  VALUE 'F2'.                  
               88  GCI2-BEN-PVSN-3-TAB-FUNC-TO-AR  VALUE 'F3'.                  
               88  GCI2-GROUP-SPEC-TAB-STATUS      VALUE 'SG'.                  
               88  GCI2-CONTRACT-TAB-STATUS        VALUE 'SC'.                  
               88  GCI2-BEN-PVSN-W-TAB-STATUS      VALUE 'SW'.                  
               88  GCI2-BEN-PVSN-1-TAB-STATUS      VALUE 'S1'.                  
               88  GCI2-BEN-PVSN-2-TAB-STATUS      VALUE 'S2'.                  
               88  GCI2-BEN-PVSN-3-TAB-STATUS      VALUE 'S3'.                  
               88  GCI2-ALL-LVL-ACCUM-TAB-STATUS   VALUE 'SA'.                  
               88  GCI2-GROUP-SPEC-SYS-LVL-TAB     VALUE 'XG'.                  
               88  GCI2-CONTRACT-SYS-LVL-TAB       VALUE 'XC'.                  
               88  GCI2-BEN-PVSN-W-SYS-LVL-TAB     VALUE 'XW'.                  
               88  GCI2-BEN-PVSN-1-SYS-LVL-TAB     VALUE 'X1'.                  
               88  GCI2-BEN-PVSN-2-SYS-LVL-TAB     VALUE 'X2'.                  
               88  GCI2-BEN-PVSN-3-SYS-LVL-TAB     VALUE 'X3'.                  
               88  GCI2-GROUP-SPEC-ALL-LVL-ACCUM   VALUE 'AG'.                  
               88  GCI2-CONTRACT-ALL-LVL-ACCUM     VALUE 'AC'.                  
               88  GCI2-GRP-N-CONT-ALL-LVL-ACCUMS  VALUE 'AB'.                  
               88  GCI2-ALL-N-BENW-ALL-LVL-ACCUMS  VALUE 'AW'.                  
               88  GCI2-ALL-N-BEN1-ALL-LVL-ACCUMS  VALUE 'AX'.                  
               88  GCI2-ALL-N-BEN2-ALL-LVL-ACCUMS  VALUE 'AY'.                  
               88  GCI2-ALL-N-BEN3-ALL-LVL-ACCUMS  VALUE 'AZ'.                  
               88  GCI2-BEN-PVSN-W-ALL-LVL-ACCUM   VALUE 'A0'.                  
               88  GCI2-BEN-PVSN-1-ALL-LVL-ACCUM   VALUE 'A1'.                  
               88  GCI2-BEN-PVSN-2-ALL-LVL-ACCUM   VALUE 'A2'.                  
               88  GCI2-BEN-PVSN-3-ALL-LVL-ACCUM   VALUE 'A3'.                  
               88  GCI2-GROUP-SPECIFIC-TYPE-REQ    VALUES 'G0',  'TG',          
                       'IG',  'FG',  'SG',  'SA',  'XG',  'AG',  'AB',          
                       'AW',  'AX',  'AY',  'AZ'.                               
               88  GCI2-CONTRACT-TYPE-REQ          VALUES 'C0',  'B1',          
                       'B2',  'B3',  'BW',  'TC',  'TB',  'IC',  'IB',          
                       'FC',  'FW',  'F1',  'F2',  'F3',  'SC',  'SW',          
                       'S1',  'S2',  'S3',  'SA',  'XC',  'XW',  'X1',          
                       'X2',  'X3',  'AC',  'AB',  'AW',  'AX',  'AY',          
                       'AZ',  'A0',  'A1',  'A2',  'A3'.                        
               88  GCI2-BENEFIT-PVSN-TYPE-REQ      VALUES 'B1',                 
                       'B2',  'B3',  'BW',  'TB',  'IB',  'FW',  'F1',          
                       'F2',  'F3',  'SW',  'S1',  'S2',  'S3',  'XW',          
                       'X1',  'X2',  'X3',  'AW',  'AX',  'AY',  'AZ',          
                       'A0',  'A1',  'A2',  'A3'.                               
               88  GCI2-VALID-REQUEST-CODE         VALUES 'G0',  'C0',          
                       'B1',  'B2',  'B3',  'BW',  'TG',  'TC',  'TB',          
                       'TX',  'TN',  'IG',  'IC',  'IB',  'D0',  'FG',          
                       'FC',  'FW',  'F1',  'F2',  'F3',  'SG',  'SW',          
                       'S1',  'S2',  'S3',  'SA',  'XG',  'XC',  'XW',          
                       'X1',  'X2',  'X3',  'AG',  'AC',  'AB',  'AW',          
                       'AX',  'AY',  'AZ',  'A0',  'A1',  'A2',  'A3'.          
                                                                                
             10  FILLER                        PIC X(4).                        
      *      10  GCI2-APPLICATION-ID           PIC X(4).                        
      *      IDENTIFIES THE APPLICATION MAKING THE REQUEST FOR ACCESS           
      *      SO THAT A SUBROUTINE OF THE INTERFACE CAN DETERMINE WHAT           
      *      VIEW OF THE DATA THE APPLICATION IS READY TO GET.                  
      *                                                                         
             10  FILLER                        PIC XX.                          
      *      10  GCI2-RETURN-CODE              PIC XX.                          
               88  GCI2-INVALID-REQUEST            VALUE '40'.                  
               88  GCI2-REQUEST-SUCCESSFUL         VALUE '00'.                  
               88  GCI2-RECORD-NOT-FOUND           VALUE '10'.                  
               88  GCI2-RECORD-MARKED--UNCODED     VALUE '1Z'.                  
               88  GCI2-DATES-NOT-COVERED          VALUE '05'.                  
               88  GCI2-TAB-NOT-ON-FILE-BUT-IN-RC  VALUES '50',  '1D'.          
               88  GCI2-NO-SYS-TAB-BUT-IN-REC      VALUE '1D'.                  
               88  GCI2-FIELD-NOT-ON-TAB           VALUES '20',  '2I',          
                                                          '2E',  '0C'.          
               88  GCI2-NO-FIELD-BUT-REC-INCLUDED  VALUE '2I'.                  
               88  GCI2-NO-FIELD-BUT-REC-EXCLUDED  VALUE '2E'.                  
               88  GCI2-NO-FIELD-ON-SYS-LEV-TAB    VALUE '0C'.                  
               88  GCI2-FIELD-ON-TAB--INCLUDED     VALUE '0I'.                  
               88  GCI2-FIELD-ON-TAB--EXCLUDED     VALUE '0E'.                  
               88  GCI2-FOUND-TAB-BUT-NOT-PROC     VALUE '0F'.                  
               88  GCI2-FOUND-TAB-PROC-N-PROVIDER  VALUE '0G'.                  
               88  GCI2-FOUND-TAB-N-PROCEDURE      VALUES '0A',  '0B'.          
               88  GCI2-FOUND-TAB-N-MORE-AVAILABL  VALUES '0M'.                 
               88  GCI2-CORP-LIST-OVRD--INCLUDED   VALUES 'NI',  'II'.          
               88  GCI2-CORP-LIST-OVRD--EXCLUDED   VALUES 'NE',  'IE'.          
               88  GCI2-CORP-LIST-OVRD--NO-FIELD   VALUES 'NN',  'IN'.          
                                                                                
      ***                                                                       
      ***   B E G I N N I N G   O F   K E Y   A R E A                           
      ***                                                                       
      ***                                                                       
             10  FILLER                        PIC X(21).                       
      ***    PRIMARY KEY FIELDS FOR G.C. SYSTEM                                 
      ***    10  GCI2-GROUP-NO-9               PIC X(9).                        
      ***    10  GCI2-SECTION-NO-5             PIC X(5).                        
      ***    10  GCI2-LOB                      PIC X.                           
      ***    10  GCI2-FAM-REL-LVL              PIC X.                           
      ***    10  GCI2-PROV-CTL                 PIC XX.                          
      ***    10  GCI2-EFFECTIVE-DATE           PIC S9(5)  COMP-3.               
      ***                                                                       
      ***  BENEFIT PVSN KEY.  ID MUST BE ON CONTRACT, REMAINDER OF              
      ***  THE KEY IS GOTTEN FROM THE CONTRACT.                                 
             10  FILLER                        PIC X(6).                        
      ***    10  GCI2-BENEFIT-ID               PIC X(6).                        
      ***                                                                       
      ***  TABULAR IDS MUST ALSO BE FOUND ON THEIR APPROPRIATE                  
      ***  RECORDS, WHICH THEN PROVIDES THE REST OF THE KEY                     
             10  FILLER                        PIC X(18).                       
      ***    10  GCI2-GROUP-TABULAR-ID         PIC X(6).                        
      ***    10  GCI2-CONTRACT-TABULAR-ID      PIC X(6).                        
      ***    10  GCI2-BEN-PVSN-TABULAR-ID      PIC X(6).                        
      ***  INTERNAL TABULARS MUST BE SUPPLIED WITH BOTH TABULAR ID              
      ***  AND THE SLOT NUMBER SINCE WE CAN HAVE MULTIPLE OCCURS OF             
      ***  THE PARTICULAR TABULAR ID ON THE SAME ACCUMULATOR TABULAR            
      ***  RECORD.                                                              
             10  FILLER                        PIC X(10).                       
      ***    10  GCI2-TABULARS-TABULAR-ID      PIC X(6).                        
      ***    10  GCI2-TABULARS-TABULAR-SLOT-NO PIC S9(7)  COMP-3.               
      ***                                                                       
      ***   THE FOLLOWING KEY FIELDS ARE SET ASIDE FOR SPECIALIZED              
      ***   ROUTINE USAGE.  THEY ARE NOT USED BY THE GENERALIZED RTNS.          
      ***                                                                       
      ***    10  GCI2-BENEFIT-OR-TABULAR-ID.                                    
             10  FILLER                        PIC X(10).                       
      ***        15  GCI2-BENEFIT-IDENTIFIER   PIC X(6).                        
      ***        15  FILLER     REDEFINES    GCI2-BENEFIT-ID.                   
      ***            20  GCI2-TABULAR-ID       PIC X(5).                        
      ***            20  FILLER                PIC X(1).                        
      ***        15  GCI2-SLOT-NO              PIC S9(7)  COMP-3.               
      ***                                                                       
                                                                                
             10  FILLER                        PIC X(4).                        
      ***    10  GCI2-CROSS-REF-TAB-REC-TYPE   PIC X.                           
      ***    10  GCI2-INTERFACE-WORK-IND       PIC X.                           
      ***    10  FILLER                        PIC X(2).                        
                                                                                
      ***************************************************************           
      *** THIS BLOCK OF CODE IS USED BY THE GET DATES ROUTINE TO    *           
      *** FIND A LIST OF GROUP SPECIFIC AND CONTRACT RECORDS THAT   *           
      *** COVER A PERIOD OF TIME FOR AN INDIVIDUAL CLAIM.           *           
      ***                                                           *           
             10  FILLER                        PIC X(17).                       
      ***    10  GCI2-FAM-REL-LVL-MEDICARE     PIC X.               *           
      ***    10  GCI2-FAM-REL-LVL-M-S-D        PIC X.               *           
      ***    10  GCI2-FAM-REL-LVL-LO-HI        PIC X.               *           
      ***                                                           *           
      ***    10  GCI2-PROV-CTL-PLAN-BASIC      PIC X.               *           
      ***    10  GCI2-PROV-CTL-PPO-BASIC       PIC X.               *           
      ***    10  GCI2-PROV-CTL-EMPL-BASIC      PIC X.               *           
      ***    10  GCI2-PROV-CTL-PLAN-MM         PIC X.               *           
      ***    10  GCI2-PROV-CTL-PPO-MM          PIC X.               *           
      ***    10  GCI2-PROV-CTL-EMPL-MM         PIC X.               *           
      ***                                                           *           
      ***    10  GCI2-START-OR-NEXT-SVC-DT     PIC S9(5)  COMP-3.   *           
      ***    10  GCI2-END-SVC-DT               PIC S9(5)  COMP-3.   *           
      ***    10  GCI2-PATIENT-AGE              PIC S9(3)  COMP-3.   *           
      ***************************************************************           
      ***                                                                       
      *** THE FOLLOWING FILLER IS USED TO PROVIDE SPACE FOR ADDITIONS           
             10    FILLER                      PIC X(6).                        
      ***    E N D   O F   K E Y   A R E A                                      
      ***                                                                       
      ***                                                                       
      ***    B E G I N N I N G   O F   R E C O R D   P N T R S                  
      ***    10  GCI2-GROUP-SPEC-REC-COMP      PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-GROUP-SPEC-REC-ADDR      REDEFINES                        
      ***        GCI2-GROUP-SPEC-REC-COMP      POINTER.                         
      ***    10  GCI2-CONTRACT-REC-COMP        PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-CONTRACT-REC-ADDR        REDEFINES                        
      ***        GCI2-CONTRACT-REC-COMP        POINTER.                         
             10    FILLER                      PIC X(8).                        
      ***                                                                       
      ***  ADDRESS PNTRS TO THE BENEFIT PVSN RECORD FOR ABOVE CONTRACT          
      ***    10  GCI2-BENEFIT-PVSN-REC-COMP    PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-BENEFIT-PVSN-REC-ADDR    REDEFINES                        
      ***        GCI2-BENEFIT-PVSN-REC-COMP    POINTER.                         
      ***    10  GCI2-BENEFIT-PVSN-SAVE-1      REDEFINES                        
      ***        GCI2-BENEFIT-PVSN-REC-COMP    POINTER.                         
      ***    10  GCI2-BENEFIT-PVSN-2-COMP      PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-BENEFIT-PVSN-SAVE-2      REDEFINES                        
      ***        GCI2-BENEFIT-PVSN-2-COMP      POINTER.                         
      ***    10  GCI2-BENEFIT-PVSN-3-COMP      PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-BENEFIT-PVSN-SAVE-3      REDEFINES                        
      ***        GCI2-BENEFIT-PVSN-3-COMP      POINTER.                         
      ***    10  GCI2-BENEFIT-PVSN-W-COMP      PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-BENEFIT-PVSN-SAVE-W      REDEFINES                        
      ***        GCI2-BENEFIT-PVSN-W-COMP      POINTER.                         
             10    FILLER                      PIC X(16).                       
      ***                                                                       
             10    FILLER                      PIC X(4).                        
      ***                                                                       
      ***  ADDRESS PNTRS TO THE TABULAR RECORDS                                 
      ***    10  GCI2-GROUPS-TAB-REC-COMP      PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-GROUPS-TAB-REC-ADDR      REDEFINES                        
      ***        GCI2-GROUPS-TAB-REC-COMP      POINTER.                         
      ***    10  GCI2-CONTRACTS-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-CONTRACTS-TAB-REC-ADDR   REDEFINES                        
      ***        GCI2-CONTRACTS-TAB-REC-COMP   POINTER.                         
      ***    10  GCI2-BEN-PVSNS-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-BEN-PVSNS-TAB-REC-ADDR   REDEFINES                        
      ***        GCI2-BEN-PVSNS-TAB-REC-COMP   POINTER.                         
      ***    10  GCI2-TABULARS-TAB-REC-COMP    PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-TABULARS-TAB-REC-ADDR    REDEFINES                        
      ***        GCI2-TABULARS-TAB-REC-COMP    POINTER.                         
             10  FILLER                        PIC X(16).                       
      ***                                                                       
      ***  ADDRESS PNTRS TO TWO TABLES BUILT BY SPECIALIZED RTNES               
      ***    10  GCI2-ACCUMULATOR-TABLE-COMP   PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-ACCUMULATOR-TABLE-ADDR   REDEFINES                        
      ***        GCI2-ACCUMULATOR-TABLE-COMP   POINTER.                         
      ***    10  GCI2-DATES-REC-TAB-COMP       PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-DATES-REC-TAB-ADDR       REDEFINES                        
      ***        GCI2-DATES-REC-TAB-COMP       POINTER.                         
             10  FILLER                        PIC X(08).                       
      ***                                                                       
      ***  ADDRESS PNTRS TO THE CROSS REFERENCE TABULAR RECORD                  
      ***    10  GCI2-CROSS-REF-TAB-REC-COMP   PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-CROSS-REF-TAB-REC-ADDR   REDEFINES                        
      ***        GCI2-CROSS-REF-TAB-REC-COMP   POINTER.                         
             10  FILLER                        PIC X(04).                       
      ***                                                                       
      ***  ADDRESS PNTRS AREAS USED BY THE INTERFACE                            
      ***    10  GCI2-INTERFACE-WORKAREA1-COMP PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-INTERFACE-WORK-AREA-1    REDEFINES                        
      ***        GCI2-INTERFACE-WORKAREA1-COMP POINTER.                         
      ***    10  GCI2-INTERFACE-WORKAREA2-COMP PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-INTERFACE-WORK-AREA-2    REDEFINES                        
      ***        GCI2-INTERFACE-WORKAREA2-COMP POINTER.                         
      ***    10  GCI2-INTERFACE-WORKAREA3-COMP PIC S9(8)  COMP  SYNC.           
      ***    10  GCI2-INTERFACE-WORK-AREA-3    REDEFINES                        
      ***        GCI2-INTERFACE-WORKAREA3-COMP POINTER.                         
             10  FILLER                        PIC X(12).                       
      ***    E N D   O F   R E C O R D   P N T R S                              
      ***                                                                       
      ***                                                                       
             10  FILLER                        PIC X(12).                       
      ***                                                                       
      ***    A R E A   F O R   R E S P O N S E   O F                            
      ***       S P E C I A L I Z E D   R O U T I N E S                         
      ***                                                                       
             10  GCI2-TABULAR-SPECIFIC-AREA    PIC X(200).                      
      ***    E N D   O F   S P E C I A L I Z E D   A R E A                      
      ***                                                                       
