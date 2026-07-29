      ******************************************************************        
      *                                                                *        
      *              COPYBOOK MEMBER NAME:  DIMCOMMC                   *        
      *             DETAIL-INVENTORY-MESSAGE-COMMAREA                  *        
      *                                                                *        
      *        COMMUNICATIONS AREA USED FOR PATHING THRU THE           *        
      *                    INVENTORY LOG SYSTEM.                       *        
      *                                                                *        
      *             FIXED LENGTH RECORD -  151 BYTES                   *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      ******************************************************************        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *        
      * -*-*-*-*-*    M A I N T E N A N C E    L O G    *-*-*-*-*-*-*- *        
      *- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *        
      *                                                                *        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *   D236     12/01/89  FRY    CREATED.                           *        
      *   LVL#2    09/21/90  WGC    ADD USER ID NUMBER                 *        
      *                                                                *        
      *   LVL#3    06/04/98  AKK    EXPANDE DATES FIELDS FOR YR2K.     *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
           05  DETAIL-INVENTORY-MESSAGE-COMM.                                   
               10  DIM-PATHING-INFORMATION.                                     
                   15  DIM-LAST-CICS-TRANS               PIC  X(04).            
                       88  DIM-MENU                        VALUE 'GFIL'.        
                       88  DIM-ADD-DETAIL-INVENTORY        VALUE 'GFAA'.        
                       88  DIM-CHG-DETAIL-INVENTORY        VALUE 'GFAA'.        
                       88  DIM-ADD-DETAIL-MESSAGE          VALUE 'GFCA'.        
                       88  DIM-CHG-DETAIL-MESSAGE          VALUE 'GFCA'.        
                       88  DIM-DETAIL-INVENTORY-INQ        VALUE 'GFAB'.        
                       88  DIM-INVENTORY-SELECT-LOG        VALUE 'GFBA'.        
                       88  DIM-INVENTORY-SUMMARY-INQ       VALUE 'GFBB'.        
                       88  DIM-DETAIL-MESSAGE-INQ          VALUE 'GFCB'.        
                       88  DIM-MESSAGE-SUMMARY-INQ         VALUE 'GFDA'.        
                   15  DIM-KEY.                                                 
                       20  DIM-GROUP-NO.                                        
                           25  FILLER                    PIC  X(03).            
                           25  DIM-GROUP-NUM             PIC  X(06).            
                       20  DIM-SEC-NO.                                          
                           25  FILLER                    PIC  X(01).            
                           25  DIM-SEC-NUM               PIC  X(04).            
                       20  DIM-ITERATION                 PIC  X(03).            
                   15  DIM-ALT-KEY.                                             
                       20  DIM-ALT-STATUS                PIC  X(01).            
                       20  DIM-ALT-ANALYST               PIC  X(03).            
                       20  DIM-ALT-CODER                 PIC  X(03).            
                       20  DIM-ALT-GROUP-NO.                                    
                           25  FILLER                    PIC  X(03).            
                           25  DIM-ALT-GROUP-NUM         PIC  X(06).            
                       20  DIM-ALT-SEC-NO.                                      
                           25  FILLER                    PIC  X(01).            
                           25  DIM-ALT-SEC-NUM           PIC  X(04).            
                   15  DIM-OPTION                        PIC  X(01).            
                       88  DIM-DETAIL-INVENTORY-ADD        VALUE '1'.           
                       88  DIM-DETAIL-INVENTORY-CHG        VALUE '2'.           
                       88  DIM-DETAIL-MESSAGE-ADD          VALUE '3'.           
                       88  DIM-DETAIL-MESSAGE-CHG          VALUE '4'.           
                       88  DIM-INQ-DETAIL-INVENTORY        VALUE 'A'.           
                       88  DIM-INQ-INVENTORY-SELECT-LOG    VALUE 'B'.           
                       88  DIM-INQ-INVENTORY-SUMMARY       VALUE 'C'.           
                       88  DIM-INQ-DETAIL-MESSAGE          VALUE 'D'.           
                       88  DIM-INQ-MESSAGE-SUMMARY         VALUE 'E'.           
                   15  DIM-LAST-OPTION                    PIC  X(01).           
                       88  DIM-LAST-DETAIL-INVENTORY-ADD   VALUE '1'.           
                       88  DIM-LAST-DETAIL-INVENTORY-CHG   VALUE '2'.           
                       88  DIM-LAST-DETAIL-MESSAGE-ADD     VALUE '3'.           
                       88  DIM-LAST-DETAIL-MESSAGE-CHG     VALUE '4'.           
                       88  DIM-LAST-INQ-DETAIL-INVENTORY   VALUE 'A'.           
                       88  DIM-LAST-INQ-INVENT-SELECT-LOG  VALUE 'B'.           
                       88  DIM-LAST-INQ-INVENTORY-SUMMARY  VALUE 'C'.           
                       88  DIM-LAST-INQ-DETAIL-MESSAGE     VALUE 'D'.           
                       88  DIM-LAST-INQ-MESSAGE-SUMMARY    VALUE 'E'.           
                   15  DIM-EFFECTIVE-DATE     COMP-3     PIC S9(09).            
                   15  DIM-RETURN-CODE                   PIC  X(02).            
                       88  DIM-GOOD-RETURN                 VALUE '00'.          
                       88  DIM-ERROR                       VALUE '01'.          
                       88  DIM-ABEND                       VALUE '02'.          
                   15  DIM-USER-ID-NUMBER.                                      
                       20  DIM-USER-FIRST-DIGIT          PIC  X(01).            
                       20  DIM-USER-ID                   PIC  X(05).            
                       20  DIM-USER-FILLER               PIC  X(02).            
                   15  FILLER                            PIC  X(92).            
