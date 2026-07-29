      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSMSGR2                                        *        
      *    DATE:       22-JUN-1988                                     *        
      *    AUTHOR:     RICK E. BARILEAU                                *        
      *    FUNCTION:   SPECIAL MESSAGE RECORD LAYOUT                   *        
      *                                                                *        
      *                THIS COPY MEMBER WILL LOAD A SPECIAL            *        
      *                MESSAGE AND IDENTIFY IT WITH A UNIQUE KEY.      *        
      *                IT WILL ALSO KEEP TRACK OF HOW MANY TIMES       *        
      *                A PARTICULAR MESSAGE IS REFERRED TO.            *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 22-JUN-1988 REB       CREATED                            *        
      * 01.01 05-OCT-1988 LET       REFORMATTED AND ADDED AN INDICATOR *        
      *                             THAT INFORMS IF THE MESSAGE WAS    *        
      *                             PREVIOUSLY RETRIEVED.              *        
      *                                                                *        
      * 01.02 11-OCT-1988 LET       ADDED A LEVEL IN FRONT OF TEXT AREA*        
      *                                                                *00230000
      * 01.03 25-OCT-1988 LET       ADDED A 10 BYTE FILLER FOR FUTURE. *00240000
      ******************************************************************        
       01  SPECIAL-MESSAGE2-RECORD.                                             
           05  MESSAGE2-KEY.                                                    
               10  MESSAGE2-TYPE               PIC  X(01).                      
                   88  MESSAGE2-COMMON                    VALUE 'C'.            
                   88  MESSAGE2-UNIQUE                    VALUE 'X'.            
               10  MESSAGE2-NUMBER             PIC  X(04).                      
           05  MESSAGE2-RETRIEVED-IND           PIC  X(01).                     
               88  MSG2-RETRIEVED                         VALUE 'R'.            
               88  MSG2-NOT-RETRIEVED                     VALUE ' '.            
           05  FILLER                          PIC  X(10) VALUE SPACES.         
           05  MESSAGE2-REFERENCE-COUNT        PIC S9(04)   COMP.               
           05  MESSAGE2-LINE-COUNT             PIC S9(03)   COMP.               
               88  MAXIMUM-MESSAGE2-LINES                   VALUE +015.         
           05  MESSAGE2-TEXT-AREA.                                              
               10  MESSAGE2-LINE-ENTRY         PIC  X(72)                       
                                               OCCURS 1 TO 15 TIMES             
                                               DEPENDING ON                     
                                               MESSAGE2-LINE-COUNT              
                                               INDEXED BY MSG2-LINE-IDX.        
