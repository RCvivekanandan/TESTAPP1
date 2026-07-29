COPY,                                                                           
   INFILE(DD01),                                                                
   OUTFILE(DD01O),                                                              
   SELRECIF=(1,EQ,C'621'),                                                      
     AND(58,NE,C'C'),                                                           
     AND(21,EQ,C'POS'),                                                         
   OUTLIM(0)                                                                    
