      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSPTC                                        *        
      *    DATE:       23-DEC-1987                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   CONTAINS THE REQUEST, BENEFIT PROVISION AND     *        
      *                PROVISION GROUP POINTERS FOR CONTRACT SUMMARY   *        
      *                PROVISION-BASED SUBTOPICS.                      *        
      *                                                                *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 23-DEC-1987 NAC       CREATED                            *        
      * 01.01 08-FEB-1988 REB       ADDED PROCESS SWITCH FOR SUBTOPICS *        
      * 01.02 26-FEB-1988 NAC       ADDED TABLE MAX COUNT              *        
      ******************************************************************        
       01  CSPT-POINTER-LIST.                                                   
           05  CSPT-TBL-CNT                    PIC S9(04)  COMP.                
               88  CSPT-TBL-MAX                          VALUE +0005.           
           05  CSPT-PTR-TABLE-AREA.                                             
               10  CSPT-IHS-PTRS.                                               
                   15  CSPT-IHS-REQ-LIST-PTR   POINTER.                         
                   15  CSPT-IHS-BP-TBL-PTR     POINTER.                         
                   15  CSPT-IHS-PROV-GRP-PTR   POINTER.                         
                   15  CSPT-IHS-PROCESS-SW     PIC X(01).                       
                       88  COMPLETED-IHS                 VALUE 'C'.             
                       88  PROCESS-IHS                   VALUE 'P'.             
               10  CSPT-IPS-PTRS.                                               
                   15  CSPT-IPS-REQ-LIST-PTR   POINTER.                         
                   15  CSPT-IPS-BP-TBL-PTR     POINTER.                         
                   15  CSPT-IPS-PROV-GRP-PTR   POINTER.                         
                   15  CSPT-IPS-PROCESS-SW     PIC X(01).                       
                       88  COMPLETED-IPS                 VALUE 'C'.             
                       88  PROCESS-IPS                   VALUE 'P'.             
               10  CSPT-OPS-PTRS.                                               
                   15  CSPT-OPS-REQ-LIST-PTR   POINTER.                         
                   15  CSPT-OPS-BP-TBL-PTR     POINTER.                         
                   15  CSPT-OPS-PROV-GRP-PTR   POINTER.                         
                   15  CSPT-OPS-PROCESS-SW     PIC X(01).                       
                       88  COMPLETED-OPS                 VALUE 'C'.             
                       88  PROCESS-OPS                   VALUE 'P'.             
               10  CSPT-OBS-PTRS.                                               
                   15  CSPT-OBS-REQ-LIST-PTR   POINTER.                         
                   15  CSPT-OBS-BP-TBL-PTR     POINTER.                         
                   15  CSPT-OBS-PROV-GRP-PTR   POINTER.                         
                   15  CSPT-OBS-PROCESS-SW     PIC X(01).                       
                       88  COMPLETED-OBS                 VALUE 'C'.             
                       88  PROCESS-OBS                   VALUE 'P'.             
               10  CSPT-PSY-PTRS.                                               
                   15  CSPT-PSY-REQ-LIST-PTR   POINTER.                         
                   15  CSPT-PSY-BP-TBL-PTR     POINTER.                         
                   15  CSPT-PSY-PROV-GRP-PTR   POINTER.                         
                   15  CSPT-PSY-PROCESS-SW     PIC X(01).                       
                       88  COMPLETED-PSY                 VALUE 'C'.             
                       88  PROCESS-PSY                   VALUE 'P'.             
           05  CSPT-PTR-TABLE   REDEFINES CSPT-PTR-TABLE-AREA                   
                                          OCCURS 5 TIMES                        
                                          INDEXED BY CSPT-IDX.                  
                                                                                
               10  CSPT-REQ-LIST-PTR           POINTER.                         
               10  CSPT-BP-TBL-PTR             POINTER.                         
               10  CSPT-PROV-GRP-PTR           POINTER.                         
               10  CSPT-SUBTOPIC-PROCESS-SW    PIC X(01).                       
                   88  COMPLETED-SUBTOPIC                VALUE 'C'.             
                   88  PROCESS-SUBTOPIC                  VALUE 'P'.             
