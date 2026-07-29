      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSRRBLC                                        *        
      *    DATE:       21-JUN-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   COPYBOOK IS USED WITHIN THE OVERALL ACCUM       *        
      *                DETERMINATION SUBSYSTEM FOR ELIQ.  CONTAINS     *        
      *                THE ATTRIBUTE SELECTION PARAMETERS THAT ARE TO  *        
      *                BE CONSIDERED  WHEN RANKING THE CONFIDENCE      *        
      *                FACTOR FOR EACH ACCUMULATOR TABLE.              *        
      *                THIS TABLE IS BUILT OFF ELSOARRC COPYBOOK.      *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 21-JUN-1988 NAC       CREATED                            *        
      * 01.01 31-AUG-1988 NAC       INCREASE NUMBER OF OCCURS TO 100.  *        
      * 01.02 01-SEP-1988 NAC       SHORTENED DATA NAMES OVER 30 CHARS.*        
      * 01.03 15-SEP-1988 NAC       CHANGED PICTURE FOR RRBL-HIGH-CF-  *        
      *                             ENTRY.                             *        
      ******************************************************************        
       01  RRBL-RANK-REQ-BLOCK-LIST.                                            
           03  RRBL-ACCUMULATOR-TYPE             PIC X(06).                     
               88  RRBL-ABM                           VALUE '#ABM  '.           
               88  RRBL-ACL                           VALUE '#ACL  '.           
               88  RRBL-ADL                           VALUE '#ADL  '.           
               88  RRBL-AOL                           VALUE '#AOL  '.           
           03  RRBL-TBL-CNT                      PIC S9(4) COMP.                
           03  RRBL-RANK-REQ-BLOCK OCCURS 1 TO 100 TIMES                        
                                          DEPENDING ON RRBL-TBL-CNT             
                                          INDEXED BY RRBL-X-IDX.                
               05  RRBL-ATTR-SELECTION-PARMS.                                   
                   07  RRBL-BENEFIT-PERIOD       PIC XX.                        
                   07  RRBL-FAMILY               PIC X.                         
                       88  RRBL-REGARD-FAMILY              VALUE 'Y'.           
                   07  RRBL-INDIVIDUAL           PIC X.                         
                       88  RRBL-REGARD-INDIVIDUAL          VALUE 'Y'.           
                   07  RRBL-INSTITUTIONAL        PIC X.                         
                       88  RRBL-REGARD-INSTITUTIONAL       VALUE 'Y'.           
                   07  RRBL-PROFESSIONAL         PIC X.                         
                       88  RRBL-REGARD-PROFESSIONAL        VALUE 'Y'.           
                   07  RRBL-BASIC                PIC X.                         
                       88  RRBL-REGARD-BASIC               VALUE 'Y'.           
                   07  RRBL-SUPPLEMENTAL         PIC X.                         
                       88  RRBL-REGARD-SUPPLEMENTAL        VALUE 'Y'.           
                   07  RRBL-INPATIENT            PIC X.                         
                       88  RRBL-REGARD-INPATIENT           VALUE 'Y'.           
                   07  RRBL-OUTPATIENT           PIC X.                         
                       88  RRBL-REGARD-OUTPATIENT          VALUE 'Y'.           
                   07  RRBL-PLAN                 PIC X.                         
                       88  RRBL-REGARD-PLAN                VALUE 'Y'.           
                   07  RRBL-NON-PLAN             PIC X.                         
                       88  RRBL-REGARD-NON-PLAN            VALUE 'Y'.           
                   07  FILLER                    PIC X(04).                     
               05  RRBL-HIGH-CF-ENTRY            PIC S9(04)  COMP.              
               05  RRBL-CF-ENTRIES   OCCURS 150 TIMES                           
                                     INDEXED BY RRBL-Y-IDX                      
                                     COMP-1.                                    
