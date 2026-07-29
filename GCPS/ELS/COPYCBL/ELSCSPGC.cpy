      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSPGC                                        *        
      *    DATE:       14-JAN-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   BENEFIT PROVISION GROUP TABLE FOR CONTRACT      *        
      *                SUMMARY.                                        *        
      *                                                                *        
      *                CSPG-TBL-CNT CONTAINS THE NUMBER OF PROVISION   *        
      *                GROUPS PER SUBTOPIC.  EACH GROUP CONTAINS       *        
      *                PROVISIONS THAT ARE TO BE CONSIDERED FOR        *        
      *                GROUPING BY COVERAGE/PAYMENT LEVEL.             *        
      *                THIS TABLE CONTAINS INFORMATION INDICATING      *        
      *                WHETHER A PROVISION GROUP SPLITS INSTITUTIONAL  *        
      *                OR PROFESSIONAL.                                *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 14-JAN-1988 NAC       CREATED                            *        
      * 01.00 16-MAR-1988 AKK       CHANGED GROUP-COVERED-VALUE TO 'Y' *        
      *                             AND GROUP-NOT-COVERED-VALUE TO 'N' *        
      ******************************************************************        
       01  CSPG-PROVISION-GROUP-TABLE.                                          
           03  CSPG-FIXED-PORTION.                                              
               05  CSPG-SUBTOPIC              PICTURE X(03).                    
               05  CSPG-TBL-CNT               PICTURE S9(04) COMP.              
                   88  CSPG-TBL-FULL                 VALUE +0050.               
           03  CSPG-PROVISION-GROUP-ENTRY     OCCURS 1 TO 50 TIMES              
                                              DEPENDING ON CSPG-TBL-CNT         
                                              INDEXED BY CSPG-IDX.              
               05  CSPG-GROUP-COVERAGE-IND    PICTURE X(01).                    
                   88  CSPG-GROUP-COVERED                 VALUE 'Y'.            
                   88  CSPG-GROUP-NOT-COVERED             VALUE 'N'.            
               05  CSPG-INST-SPLIT-IND        PICTURE X(01).                    
                   88  CSPG-NO-INSTITUTIONAL-SPLIT        VALUE 'N'.            
                   88  CSPG-INSTITUTIONAL-SPLIT           VALUE 'Y'.            
               05  CSPG-PROF-SPLIT-IND        PICTURE X(01).                    
                   88  CSPG-NO-PROFESSIONAL-SPLIT         VALUE 'N'.            
                   88  CSPG-PROFESSIONAL-SPLIT            VALUE 'Y'.            
               05  CSPG-PROVIDER-CLASS        PICTURE X(01).                    
                   88  CSPG-INSTITUTIONAL                 VALUE 'I'.            
                   88  CSPG-PROFESSIONAL                  VALUE 'P'.            
                   88  CSPG-BOTH                          VALUE 'B'.            
