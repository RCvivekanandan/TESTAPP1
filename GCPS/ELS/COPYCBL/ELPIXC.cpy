      ******************************************************************        
      ** THIS FORMAT DESCRIBES AN ENTRY WHICH WILL BECOME A PRINTED   **        
      ** LINE IN THE INDEX TO THE ELP CODES MANUAL.                   **        
      ** IT IS USED IN ELP006 AND ELP011.                             **        
      ******************************************************************        
                                                                                
           05  ENTRY-DATA.                                                      
               10  ENTRY-TAB.                                                   
                   15  ENTRY-CHAR         OCCURS 75 TIMES                       
                                          INDEXED BY ENTRY-IX1                  
                                          PIC X.                                
               10  ENTRY-PREFIX           PIC X(8).                             
               10  ENTRY-ELEMENT.                                               
                   15  ENTRY-ELEMENT-NBR  PIC 999V99.                           
                                                                                
