      ******************************************************************        
      *          COBOL NAME CROSS REFERENCE FOR DATA ELEMENTS          *        
      *                   ENGLISH LANGUAGE PROTOTYPE                   *        
      *                                                                *        
      *                         LENGTH=42                              *        
      ******************************************************************        
      *                                                                *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *       *-*         U P D A T E   H I S T O R Y         *-*      *        
      *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *        
      *                                                                *        
      **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*        
      *                                                                *        
      *    XXXX    02/11/86  AMJ  ORIGINAL MODULE                      *        
      *    0001    02/20/86  RJL  CORRECTED ORDER OF FIELDS            *        
      *                                                                *        
      ******************************************************************        
           05  CN-COBOL-ELMT-NAME.                                              
               10  CN-KEY.                                                      
                   15  CN-RECORD-PREFIX     PIC X(8).                           
                   15  CN-COBOL-NAME        PIC X(30).                          
               10  CN-ELEMENT-NBR           PIC S999V99    COMP-3.              
               10  CN-DELETE-ELEMENT-FLAG   PIC X.                              
                   88  CN-DELETE            VALUE 'D'.                          
