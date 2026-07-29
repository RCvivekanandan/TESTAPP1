      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB8                                     *        
      *    Member Title:  Line of Business Confidence Factors Table    *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of the confidence factors derived from   *        
      *       the line of business level indicator in an accumulator   *        
      *       tabular.                                                 *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 01.00  9-Dec-1992 AUTO Created from new data.                  *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01 CFT8-CNFDNC-FCTRS.                                                    
          02 CFT8-NBR-ENTRS                    PIC S9(04)        COMP           
                                               VALUE    +9.                     
          02 CFT8-VLS.                                                          
                                                                                
             03 PIC  X(01)                 VALUE '0'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '1'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '2'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '3'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '4'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '5'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '6'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '7'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE -1.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
                                                                                
             03 PIC  X(01)                 VALUE '8'.                           
             03 PIC  X(03)                 VALUE SPACES.                        
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
             03                    COMP-1  VALUE  0.000000E+00.                 
                                                                                
          02 CFT8-VL-TBL               REDEFINES CFT8-VLS.                      
             03 CFT8-TBL                       OCCURS     9 TIMES               
                                               INDEXED BY CFT8-IDX              
                                                          CFT8-MAX-IDX.         
                04 CFT8-LOB                    PIC  X(01).                      
                04                             PIC  X(03).                      
                04 CFT8-CF-INST-BAS                              COMP-1.        
                04 CFT8-CF-INST-SUP                              COMP-1.        
                04 CFT8-CF-PROF-BAS                              COMP-1.        
                04 CFT8-CF-PROF-SUP                              COMP-1.        
                                                                               
