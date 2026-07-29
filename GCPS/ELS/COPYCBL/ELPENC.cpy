      ******************************************************************        
      *         ENGLISH NAME CROSS REFERENCE FOR DATA ELEMENTS         *        
      *                   ENGLISH LANGUAGE PROTOTYPE                   *        
      *                                                                *        
      *                         LENGTH=87                              *        
      ******************************************************************        
      *                                                                *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *       *-*         U P D A T E   H I S T O R Y         *-*      *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *                                                                *        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *    XXXX    02/11/86  AMJ  ORIGINAL MODULE                      *        
      *                                                                *        
      ******************************************************************        
           05  EN-ENGLISH-ELMT-NAME.                                            
               10  EN-KEY.                                                      
                   15  EN-RECORD-PREFIX     PIC X(8).                           
                   15  EN-ELEMENT-NAME      PIC X(75).                          
               10  EN-ELEMENT-NBR       PIC S999V99    COMP-3.                  
               10  EN-DELETE-ELEMENT-FLAG   PIC X.                              
                   88  EN-DELETE            VALUE 'D'.                          
