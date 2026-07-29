      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB2                                     *        
      *    Member Title:  Provider Type Confidence Factors Table       *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of the confidence factors derived from   *        
      *       provider types stored in provider type list tabulars.    *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 03.00  2-Dec-1992 AUTO Generated from new data.                *        
      *                                                                *        
      * 04.00 23-jun-2000 manually generated.  Added BY, B0,           *        
      *                   B1, B5, B6, TA, TB, TC, TD, TE,              *        
      *                   TF, TH, TI, TJ, TU, T1, T2, T3, T4,          *        
      *                   T5, T6, T7, T8, T9.  the T's are being       *        
      *                   added as zero values until I get the         *        
      *                   real values from TX.                         *        
      *                   I added the 'B's' based on my own analysis   R        
      *                   TU and T5 on valid value table I added them  *        
      *       26-jun-00   as professional with all 0's                 *        
      ******************************************************************        
                                                                                
       01  CFT2.                                                                
           02 CFT2-NBR-ENTRS                   PIC S9(04)        COMP           
                                               VALUE   85.                      
           02 CFT2-NBR-TBL-ENTRIES     REDEFINES CFT2-NBR-ENTRS                 
                                               PIC S9(04)        COMP.          
                                                                                
           02 CFT2-VALUES.                                                      
                                                                                
              03 PIC  X(02)                VALUE 'AA'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AB'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AC'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AD'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AE'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AF'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.801802E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.900990E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AG'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AH'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AI'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AJ'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AK'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AL'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AM'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AN'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AP'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AQ'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AR'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'AS'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AT'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.603604E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.980198E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AU'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AV'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AW'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AX'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AY'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.603604E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.980198E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'AZ'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A0'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.603604E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.980198E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A1'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A2'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A3'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A4'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A5'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A6'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A7'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A8'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'A9'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.702703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'BY'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.603604E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.980198E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'B0'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'B1'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'B5'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'B6'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE 'TA'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TB'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TC'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TD'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TE'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TF'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TH'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TI'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TJ'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'TU'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T1'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T2'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T4'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T5'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T6'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T7'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T8'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'T9'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(02)                VALUE 'YZ'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.504505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE 'ZZ'.                          
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.009009E-03.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0A'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0B'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0C'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  5.084746E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0D'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  5.084746E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0E'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0F'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0G'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0H'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  2.197802E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  6.250000E-02.                 
              03                   COMP-1  VALUE  9.900990E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0I'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0J'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  1.098901E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.125000E-02.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0K'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  2.197802E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.389831E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.900990E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0L'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0M'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0N'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.562500E-01.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0O'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0P'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  8.474576E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0Q'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  5.494505E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.562500E-01.                 
              03                   COMP-1  VALUE  2.475248E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0R'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  1.098901E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  3.125000E-02.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0S'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  1.098901E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.694915E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  4.950495E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0T'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.375000E-02.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0U'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.375000E-02.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0V'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  2.197802E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  6.250000E-02.                 
              03                   COMP-1  VALUE  9.900990E-03.                 
                                                                                
              03 PIC  X(02)                VALUE '0W'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  4.395604E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.250000E-01.                 
              03                   COMP-1  VALUE  1.980198E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0X'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.375000E-02.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
              03 PIC  X(02)                VALUE '0Y'.                          
              03 PIC  X(01)                VALUE 'I'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  3.296703E-02.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  9.375000E-02.                 
              03                   COMP-1  VALUE  1.485149E-02.                 
                                                                                
           02 CFT2-TABLE               REDEFINES CFT2-VALUES.                   
              03 CFT2-TBL                      OCCURS   85 TIMES                
                                               INDEXED BY CFT2-IDX              
                                                          CFT2-MAX-IDX.         
                 04 CFT2-PT                    PIC  X(02).                      
                 04 CFT2-PT-INST-PROF          PIC  X(01).                      
                    88 CFT2-PT-INST            VALUE 'I'.                       
                    88 CFT2-PT-PROF            VALUE 'P'.                       
                 04 CFT2-PT-INCL-EXCL          PIC  X(01).                      
                    88 CFT2-PT-INCLUDE         VALUE 'I'.                       
                    88 CFT2-PT-EXCLUDE         VALUE 'E'.                       
                 04 CFT2-CF-PT-INST                           COMP-1.           
                 04 CFT2-CF-PT-PROF                           COMP-1.           
                 04 CFT2-CF-PT-PLAN                           COMP-1.           
                 04 CFT2-CF-PT-NONPLAN                        COMP-1.           
                 04 CFT2-CF-PT-OV                             COMP-1.           
                                                                                
