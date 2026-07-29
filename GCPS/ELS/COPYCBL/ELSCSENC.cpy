      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSENC                                        *        
      *    DATE:       05-JAN-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   CONTAINS SENTENCE STRUCTURES FOR CONTRACT       *        
      *                SUMMARY.                                        *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 05-JAN-1988 NAC       CREATED                            *        
      * 01.01 04-FEB-1988 REB       CHANGED TABLE TO ONE-DIMENSIONAL   *        
      * 01.01 09-FEB-1988 NAC       INCREASED NUMBER OF OCCURENCES     *        
      * 01.02 23-MAR-1988 REB       INCREASED OCCURENCES UP TO 15.     *        
      ******************************************************************        
       01  CSEN-SENTENCE-TABLE.                                                 
           03  CSEN-NBR-SENTENCES    PICTURE S9(04) COMP.                       
           03  CSEN-SENTENCES                                                   
                                     PICTURE  X(46)                             
                                     OCCURS 1 TO 15 TIMES                       
                                     DEPENDING ON CSEN-NBR-SENTENCES            
                                     INDEXED BY CSEN-Y-IDX.                     
