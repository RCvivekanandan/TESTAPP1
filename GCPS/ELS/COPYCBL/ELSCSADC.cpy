      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSADC                                        *        
      *    DATE:       22-SEP-1989                                     *        
      *    AUTHOR:     GEO E. MOORE                                    *        
      *    FUNCTION:   CONTAINS #ACL COINSURANCE ASCEND/DESEND INFO    *        
      *                FOR CONTRACT SUMMARY.                           *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 22-SEP-1989 GEM       CREATED                            *        
      * 02.00 01-JUL-1992 BAK       CHANGE KEY-COND TO 30 BYTES        *        
      ******************************************************************        
         01 CSAD-ACL-TABLE.                                                     
            03 CSAD-ACL-NBR-ENTRIES   PIC S9(04) COMP.                          
            03 CSAD-ACL-KEY           OCCURS 50 TIMES INDEXED BY                
                                      CSAD-ACL-IDX, CSAD-PREV-IDX.              
                                                                                
               05 CSAD-ACL-KEY-FIELDS.                                          
                  07 CSAD-ACL-KEY-A-D-IND     PIC X(01).                        
                  07 CSAD-ACL-KEY-TYPE        PIC X(02).                        
                  07 CSAD-ACL-KEY-MAN-IND     PIC X(01).                        
                  07 CSAD-ACL-KEY-F-R-I       PIC X(01).                        
                  07 CSAD-ACL-KEY-L-O-B       PIC X(01).                        
                  07 CSAD-ACL-KEY-INT-DES     PIC X(09).                        
                  07 CSAD-ACL-KEY-SER-GRP     PIC X(02).                        
                  07 CSAD-ACL-KEY-TRT-GRP     PIC X(02).                        
                  07 CSAD-ACL-KEY-COND        PIC X(30).                        
                  07 CSAD-ACL-KEY-COST        PIC X(02).                        
                  07 CSAD-ACL-KEY-COPAY       PIC X(01).                        
                                                                                
             05 CSAD-TBL-AREA.                                                  
               07 CSAD-ENTRIES-USED   PIC S9(04) COMP.                          
               07 CSAD-TBL-ENTRIES    OCCURS 29 TIMES INDEXED BY                
                                      CSAD-ENTRY-IDX, CSAD-NEW-IDX.             
                                                                                
                 09 CSAD-ACL-BISCEND-IND PIC X(01).                             
                 09 CSAD-ACL-PCT-LVL     PIC S9(3) VALUE '0' COMP-3.            
                 09 CSAD-ACL-VAL-LMT     PIC S9(7)V99 VALUE '0' COMP-3.         
