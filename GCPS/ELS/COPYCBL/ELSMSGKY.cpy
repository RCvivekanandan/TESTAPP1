      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSMSGKY                                        *        
      *    DATE:       22-JUN-1988                                     *        
      *    AUTHOR:     RICK E. BARILEAU                                *        
      *    FUNCTION:   SPECIAL MESSAGE KEY RECORD LAYOUT               *        
      *                                                                *        
      *                THIS COPY MEMBER WILL BE USED TO LOAD THE       *        
      *                APPROPIATE KEY INFORMATION THAT A MESSAGE       *        
      *                APPLIES TO. IT WILL STORE THE POINTER(S)        *        
      *                OF THE MESSAGES WITH THE KEYS TO KNOW WHERE     *        
      *                TO LOCATE THESE MESSAGES ON THE MESSAGE FILE.   *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 22-JUN-1988 REB       CREATED                            *        
      *                                                                *        
      * 01.01 07-AUG-1997 AKK       ADDED SUPPORT FOR EXPANDED         *        
      *                             GRP/SECT NUMBER AND YEAR 2000      *        
      *                                                                *        
      ******************************************************************        
       01  SPECIAL-MESSAGE-KEY-AREA.                                            
           05  PRIMARY-MESSAGE-KEYS.                                            
               10  MESSAGE-RECORD-TYPE-IND     PIC  X(01).                      
               10  MESSAGE-PLAN-CODE           PIC  X(03).                      
               10  MESSAGE-GRP-NUM.                                             
                   15  MESSAGE-GRP-NUM-1-2-3   PIC  X(03).                      
                   15  MESSAGE-GROUP-NUMBER    PIC  X(06).                      
               10  MESSAGE-SECT-NUM.                                            
                   15  MESSAGE-SECT-NUM-1      PIC  X.                          
                   15  MESSAGE-SECTION-NUMBER  PIC  X(04).                      
               10  MESSAGE-PKG-CODE            PIC  X(03).                      
               10  MESSAGE-LINE-OF-BUSINESS    PIC  X(01).                      
               10  MESSAGE-PROVIDER-CONTROL    PIC  X(02).                      
               10  MESSAGE-FAMILY-RELATION     PIC  X(01).                      
               10  MESSAGE-EFF-DT.                                              
                   15  MESSAGE-EFF-DT-CC       PIC  X.                          
                   15  MESSAGE-EFFECTIVE-DATE  PIC S9(05)   COMP-3.             
               10  MESSAGE-EFF-DT-CENTURY REDEFINES                             
                     MESSAGE-EFF-DT            PIC S9(07)   COMP-3.             
               10  MESSAGE-ACCUMULATOR-ID      PIC  X(06).                      
               10  MESSAGE-BENEFIT-PROV-ID     PIC  X(06).                      
               10  MESSAGE-INTERNAL-TAB-ID     PIC  X(06).                      
               10  FILLER                      PIC  X(20).                      
           05  MESSAGE-TERM-DT.                                                 
               15  MESSAGE-TERM-DT-CC       PIC  X.                             
               15  MESSAGE-TERMINATION-DATE PIC S9(05)   COMP-3.                
           05 MESSAGE-TERM-DT-CENTURY REDEFINES                                 
                MESSAGE-TERM-DT             PIC S9(07)   COMP-3.                
           05  MESSAGE-POINTER-COUNT           PIC S9(04)   COMP.               
               88  MESSAGE-POINTER-LIMIT                    VALUE +0020.        
           05  SPECIAL-MESSAGE-POINTER-AREA.                                    
               10  MESSAGE-POINTER             PIC  X(05)                       
                                               OCCURS  20 TIMES                 
                                               INDEXED BY MSG-PTR-IDX.          
