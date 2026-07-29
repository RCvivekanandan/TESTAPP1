      ******************************************************************        
      *                                                                *        
      *    COPYBOOK:   ELSCSACC                                        *        
      *    DATE:       14-JAN-1988                                     *        
      *    AUTHOR:     NINA A. CERVANTES                               *        
      *    FUNCTION:   CONTAINS THE POINTERS TO THE ACCUMULATOR TABLES *        
      *                WHICH WERE FOUND AT GROUP, CONTRACT(S) AND      *        
      *                BENEFIT PROVISION LEVELS AND THE RANKING REQUEST*        
      *                BLOCKS USED TO DETERMINE THE OVERALL ACCUM.     *        
      *                                                                *        
      *                CSAC-X-IDX REPRESENT ABM, ACL, ADL AND AOL, IN  *        
      *                GIVEN ORDER.                                    *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 01.00 14-JAN-1988 NAC       CREATED                            *        
      * 02.00 08-AUG-1988 NAC       ELIMINATE THE GROUP AND CONTRACT   *        
      *                             SLOTS AND POINTERS.                *        
      * 03.00 06-JUL-2000 JP        ADDED ACP POINTER.                 *        
      *                                                                *        
      ******************************************************************        
       01  CSAC-ACCUMULATOR-TABLE.                                              
           03  CSAC-ACCUM-AREA.                                                 
               05  CSAC-ABM-PTRS.                                               
                   07  CSAC-ABM-GC-TBL-PTR           POINTER.                   
                   07  CSAC-ABM-BP-TBL-PTR           POINTER.                   
                   07  CSAC-ABM-RR-PTR               POINTER.                   
               05  CSAC-ACL-PTRS.                                               
                   07  CSAC-ACL-GC-TBL-PTR           POINTER.                   
                   07  CSAC-ACL-BP-TBL-PTR           POINTER.                   
                   07  CSAC-ACL-RR-PTR               POINTER.                   
               05  CSAC-ADL-PTRS.                                               
                   07  CSAC-ADL-GC-TBL-PTR           POINTER.                   
                   07  CSAC-ADL-BP-TBL-PTR           POINTER.                   
                   07  CSAC-ADL-RR-PTR               POINTER.                   
               05  CSAC-AOL-PTRS.                                               
                   07  CSAC-AOL-GC-TBL-PTR           POINTER.                   
                   07  CSAC-AOL-BP-TBL-PTR           POINTER.                   
                   07  CSAC-AOL-RR-PTR               POINTER.                   
               05  CSAC-ACP-PTRS.                                               
                   07  CSAC-ACP-GC-TBL-PTR           POINTER.                   
                   07  CSAC-ACP-BP-TBL-PTR           POINTER.                   
                   07  CSAC-ACP-RR-PTR               POINTER.                   
           03  CSAC-ACCUM-TYPE-ENTRY      REDEFINES CSAC-ACCUM-AREA             
                                          OCCURS 5 TIMES                        
                                          INDEXED BY CSAC-X-IDX.                
               05  CSAC-GC-TBL-PTR                   POINTER.                   
               05  CSAC-BP-TBL-PTR                   POINTER.                   
               05  CSAC-RR-PTR                       POINTER.                   
