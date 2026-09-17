*------------------------------------------------------------*                  
*        ADVANTAGE CA-FILE MASTER PLUS 4.1 (BASE VERSION)    *                  
*   BATCH FM2TOFM3 COMMAND CONVERTER (01/13/2004 14:59:09.3) *                  
*------------------------------------------------------------*                  
 COPY,                                                                          
   INFILE(DD01), OUTFILE(DD01O),                                                
   SELRECIF(256,NE,C'H'),                                                       
   AND(256,NE,C'T')                                                             
