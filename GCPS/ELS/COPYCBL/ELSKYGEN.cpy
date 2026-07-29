      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSKYGEN                                        *        
      *    DATE:       11-JUL-1988                                     *        
      *    AUTHOR:     RICK E. BARILEAU                                *        
      *    FUNCTION:   KEY GENERATOR RECORD LAYOUT                     *        
      *                                                                *        
      *                THIS COPY MEMBER WILL BE USED TO RETAIN THE     *        
      *                LAST NUMBER USED FOR A UNIQUE MESSAGE.          *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 11-JUL-1988 REB       CREATED                            *        
      * 01.01 29-JUL-1988 REB       ADDED FIELD FOR LAST COMMON MSG.   *        
      ******************************************************************        
       01  KEY-GENERATOR-AREA.                                                  
           05  GENERATOR-KEY                   PIC  X(56).                      
           05  FILLER                          PIC  X(01)  VALUE 'X'.           
           05  GENERATED-UNIQUE-NUMBER         PIC  9(04).                      
           05  FILLER                          PIC  X(95).                      
           05  FILLER                          PIC  X(01)  VALUE 'C'.           
           05  LAST-COMMON-MESSAGE-USED        PIC  9(04).                      
