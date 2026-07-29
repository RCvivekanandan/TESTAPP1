      ******************************************************************        
      *    COPYBOOK:   ELSPMCBP                                        *        
      *    AUTHOR:     ANNE KEFFER-KING.                               *        
      *    DATE:       21-AUG-1992                                     *        
      *    FUNCTION:   OVERLAY OF LIST OF BENEFIT PROVISIONS USED BY   *        
      *                THE NOTICE OF ADMISSION/ ELIGIBILITY SUMMARY    *        
      *                                                                *        
      ******************************************************************        
      *                      MAINTENANCE HISTORY                       *        
      *                                                                *        
      *  MOD     DATE     BY  DRPT                ACTION               *        
      * ----- ----------- --- ----- ---------------------------------- *        
      * 1.00  21-AUG-1992 AKK       CREATED                            *        
      * 2.00  19-MAY-1993 BAK       CHANGE FORMAT TO SUPPORT MAJOR     *        
      *                             MEDICAL IDENTIFICATION.            *        
      ******************************************************************        
                                                                                
       01  BPLS-BNFT-PRVSN-LST-OVRLY.                                           
           02 BPLS-BP-TABLE.                                                    
              03 BPLS-TABLE-MAX        PIC S9(04) COMP.                         
              03 BPLS-BP-INFO-TAB      OCCURS 1 TO 99 TIMES                     
                                       DEPENDING ON BPLS-TABLE-MAX              
                                       INDEXED BY BPLS-INDEX                    
                                                  BPLS-MAX-INDEX.               
                 04 BPLS-BENPROV       PIC  X(06).                              
                 04 BPLS-DRVD-IND      PIC S9(04) COMP.                         
                 04 BPLS-DRVD-CHR      PIC  X(01).                              
                 04 BPLS-BP-SW         PIC  X(01).                              
                    88 BPLS-YES                         VALUE 'Y'.              
                    88 BPLS-NO                          VALUE 'N'.              
