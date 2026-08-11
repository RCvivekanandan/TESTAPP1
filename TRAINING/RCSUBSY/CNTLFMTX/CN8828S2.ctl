*------------------------------------------------------------*                  
 COPY,                                                                          
   INFILE(DD01), OUTFILE(DD01O),                                                
   SELRECIF(256,NE,C'H'),                                                       
   AND(256,NE,C'T')                                                             
