      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB5                                     *        
      *    Member Title:  Value Qualifier Confidence Factors Table     *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of the confidence factors derived from   *        
      *       the internal descriptor in an accumulator tabular.       *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 03.00  2-Dec-1992 AUTO Generated from new data.                *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  CFT6.                                                                
           02 CFT6-NBR-ENTRS                   PIC S9(04)      COMP             
                                               VALUE   13.                      
           02 CFT6-NBR-TBL-ENTRIES     REDEFINES CFT6-NBR-ENTRS                 
                                               PIC S9(04)      COMP.            
                                                                                
           02 CFT6-VALUES.                                                      
                                                                                
              03 PIC  X(01)                VALUE 'A'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE 'B'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE 'C'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '0'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '1'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '2'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.500000E-01.                 
                                                                                
              03 PIC  X(01)                VALUE '3'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.500000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
                                                                                
              03 PIC  X(01)                VALUE '4'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '5'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
                                                                                
              03 PIC  X(01)                VALUE '6'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '7'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  8.000000E-01.                 
              03                   COMP-1  VALUE  8.000000E-01.                 
                                                                                
              03 PIC  X(01)                VALUE '8'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE '9'.                           
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
           02 CFT6-TABLE               REDEFINES CFT6-VALUES.                   
              03 CFT6-TBL                      OCCURS   13 TIMES                
                                               INDEXED BY CFT6-IDX              
                                                          CFT6-MAX-IDX.         
                 04 CFT6-VALQL                 PIC  X(01).                      
                 04                            PIC  X(03).                      
                 04 CFT6-CF-VALQL-INST                         COMP-1.          
                 04 CFT6-CF-VALQL-PROF                         COMP-1.          
