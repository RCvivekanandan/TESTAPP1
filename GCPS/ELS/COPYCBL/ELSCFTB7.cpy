      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB7                                     *        
      *    Member Title:  Benefit Period Confidence Factors Table      *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of the confidence factors derived from   *        
      *       the benefit period in an accumulator tabular.            *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 03.00  9-Dec-1992 AUTO Generated from new data.                *        
      *       20-Nov-1996 DAU  ADDED CONFIDENCE FACTORS FOR AD AND AE  *        
      * 04.00 28-Jun-2000 akk  added cf benefit period yvonnne and I  *         
      *                        decided it was most related to vists (1.0        
      *                        occurance and treatment .90.            *        
      *                        this could change                       *        
      ******************************************************************        
                                                                                
       01  CFT7-CNFDNC-FCTRS.                                                   
           02 CFT7-NBR-ENTRS                   PIC S9(04)        COMP           
                                               VALUE   +37.                     
           02 CFT7-VLS.                                                         
                                                                                
              03 PIC  X(02)                VALUE 'AA'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'AB'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'AC'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'AD'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE 'AE'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
               03 PIC  X(02)                VALUE 'CA'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
               03 PIC  X(02)                VALUE 'CB'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
               03 PIC  X(02)                VALUE 'CC'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
               03 PIC  X(02)                VALUE 'CD'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
               03 PIC  X(02)                VALUE 'CE'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
               03 PIC  X(02)                VALUE 'CF'.                         
               03 PIC  X(02)                VALUE SPACES.                       
               03                   COMP-1  VALUE  1.000000E+00.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE  9.000000E-01.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
               03                   COMP-1  VALUE -1.000000E+00.                
                                                                                
              03 PIC  X(02)                VALUE '0A'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0B'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0C'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0D'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0E'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0F'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0G'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0H'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0I'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0J'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0K'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0L'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0M'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0N'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0P'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0Q'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0R'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0Z'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0S'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  2.000000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0T'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0U'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0V'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.500000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0W'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.500000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0X'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0Y'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '00'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
           02 CFT7-VL-TBL              REDEFINES CFT7-VLS.                      
              03 CFT7-TBL                      OCCURS    37 TIMES               
                                               INDEXED BY CFT7-IDX              
                                                          CFT7-MAX-IDX.         
                 04 CFT7-BNFT-PRD              PIC  X(02).                      
                 04                            PIC  X(02).                      
                 04 CFT7-CF-PR-VST                               COMP-1.        
                 04 CFT7-CF-PR-TRTMNT                            COMP-1.        
                 04 CFT7-CF-PR-OCRNC                             COMP-1.        
                 04 CFT7-CF-PR-CNFNMNT                           COMP-1.        
                 04 CFT7-CF-ANL                                  COMP-1.        
                 04 CFT7-CF-LFTM                                 COMP-1.        
                                                                               
