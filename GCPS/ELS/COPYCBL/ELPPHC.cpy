      ******************************************************************        
      ** THIS TABLE LISTS ALL OF THE TWO-WORD PHRASES THAT SHOULD BE  **        
      ** TREATED AS ONE WORD IN THE INDEX TO THE ELP CODES MANUAL.    **        
      ** IT IS USED IN ELP006 AND ELP011.                             **        
      ******************************************************************        
                                                                                
       01  PHRASE-TABLE.                                                        
           05  PHRASES.                                                         
               10  FILLER       PIC X(20) VALUE 'BLUE CROSS          '.         
               10  FILLER       PIC X(20) VALUE 'BLUE SHIELD         '.         
               10  FILLER       PIC X(20) VALUE 'CARRY OVER          '.         
               10  FILLER       PIC X(20) VALUE 'MAJOR MEDICAL       '.         
               10  FILLER       PIC X(20) VALUE 'PER DAY             '.         
               10  FILLER       PIC X(20) VALUE 'PER DIEM            '.         
               10  FILLER       PIC X(20) VALUE 'PER HOUR            '.         
               10  FILLER       PIC X(20) VALUE 'PER MILE            '.         
               10  FILLER       PIC X(20) VALUE 'SPILL OVER          '.         
               10  FILLER       PIC X(20) VALUE 'WAITING PERIOD      '.         
           05  PHRASES-RDF REDEFINES PHRASES.                                   
               10  PHRASE                 OCCURS 10 TIMES                       
                                          ASCENDING KEY IS PHRASE-KEY           
                                          INDEXED BY PHRASE-IX1.                
                   15  PHRASE-KEY         PIC X(20).                            
                                                                                
