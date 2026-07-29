      ******************************************************************        
      *                        COPYLIB Member                          *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB3                                     *        
      *    Member Title:  Place of Treatment Confidence Factors Table  *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of the confidence factors derived from   *        
      *       the place of treatment eligibility indicator in an       *        
      *       accumulator tabular.                                     *        
      *                                                                *        
      ******************************************************************        
      *                                                  m             *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 03.00  2-Dec-1992 AUTO Generated from new data.                *        
      * 03.01 15-Dec-1992  bak corrected pot label.                    *        
      * 04.00 25-may-1999  akk added pots 10, 11, 12, 13.              *        
      * 05.00 02-mar-2000  akk changed 0s outpatient cf to             *        
      *                        5.705882E-01 this is an experiment.     *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  CFT3.                                                                
           02 CFT3-NBR-ENTRS                   PIC S9(04)        COMP           
                                               VALUE   35.                      
           02 CFT3-NBR-TBL-ENTRIES     REDEFINES CFT3-NBR-ENTRS                 
                                               PIC S9(04)        COMP.          
                                                                                
           02 CFT3-VALUES.                                                      
                                                                                
              03 PIC  X(02)                VALUE '0A'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0B'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  4.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0C'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  4.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0D'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  4.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0E'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  3.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0F'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  3.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0G'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  3.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0H'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  6.470588E-01.                 
              03                   COMP-1  VALUE  4.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0I'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0J'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.529412E-01.                 
              03                   COMP-1  VALUE  2.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0K'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.529412E-01.                 
              03                   COMP-1  VALUE  2.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0L'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.529412E-01.                 
              03                   COMP-1  VALUE  2.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0M'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  3.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0N'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  6.470588E-01.                 
              03                   COMP-1  VALUE  4.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0P'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  5.600000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0Q'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  6.800000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0R'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '0S'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  5.705882E-01.                 
              03                   COMP-1  VALUE  6.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0T'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  5.600000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0U'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  8.800000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '0V'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  4.444444E-01.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '00'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE '01'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  8.800000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '02'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '03'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  2.941176E-01.                 
              03                   COMP-1  VALUE  5.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '04'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  5.600000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '05'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  6.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '06'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  4.705882E-01.                 
              03                   COMP-1  VALUE  6.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '07'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.941176E-01.                 
              03                   COMP-1  VALUE  2.000000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '08'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '09'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '10'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  8.235294E-01.                 
              03                   COMP-1  VALUE  8.800000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '11'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.529412E-01.                 
              03                   COMP-1  VALUE  2.400000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '12'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
              03 PIC  X(02)                VALUE '13'.                          
              03 PIC  X(02)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.764706E-01.                 
              03                   COMP-1  VALUE  1.200000E-01.                 
                                                                                
           02 CFT3-TABLE               REDEFINES CFT3-VALUES.                   
              03 CFT3-TBL                  OCCURS   35 TIMES                    
                                           INDEXED BY CFT3-IDX                  
                                                      CFT3-MAX-IDX.             
                 04 CFT3-POT                   PIC  X(02).                      
                 04                            PIC  X(02).                      
                 04 CFT3-CF-POT-INPT                             COMP-1.        
                 04 CFT3-CF-POT-OUTPT                            COMP-1.        
                 04 CFT3-CF-POT-OV                               COMP-1.        
                                                                                
