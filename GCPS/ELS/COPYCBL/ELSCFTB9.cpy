      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB9                                     *        
      *    Member Title:  Provider Specialty Confidenc Factor Table    *        
      *    Date Created:  17-Aug-2000                                  *        
      *    Author:        Anne Keffer King                             *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is a table of speciality codes that will be built   *        
      *       into a table to be used with confidence factors.  The    *        
      *       initial roll out will be used to support the #IPGS       *        
      *       tabular in ELIQ                                          *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 01.00 17-aug-2000 akk  created from ELSCFTB2 copylib.          *        
      *                                                                *        
      ******************************************************************        
                                                                                
       01  CFT9.                                                                
           02 CFT9-NBR-ENTRS                   PIC S9(04)        COMP           
                                               VALUE   97.                      
           02 CFT9-NBR-TBL-ENTRIES     REDEFINES CFT9-NBR-ENTRS                 
                                               PIC S9(04)        COMP.          
                                                                                
           02 CFT9-VALUES.                                                      
                                                                                
              03 PIC  X(03)                VALUE '000'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '001'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '002'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '003'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '004'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(03)                VALUE '005'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(03)                VALUE '006'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(01)                VALUE SPACE.                         
                                                                                
              03 PIC  X(03)                VALUE '007'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '008'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '009'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '010'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '011'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '012'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '013'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '014'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '015'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '016'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '017'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '018'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '019'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '020'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '021'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '022'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '023'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '024'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '025'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '026'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '027'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '028'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '029'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '030'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '031'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '032'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '033'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '034'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '035'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '036'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '037'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '038'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '040'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '041'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '042'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '043'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '044'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '045'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '046'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '047'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(03)                VALUE '048'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '049'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '050'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '051'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '052'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '053'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '054'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '055'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '056'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '057'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '059'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '060'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '061'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '062'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '063'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '064'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '065'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '066'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '067'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '068'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '069'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '070'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '071'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '072'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '073'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '074'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '075'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '076'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '077'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '078'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '079'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '080'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '081'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '082'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '083'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '084'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '085'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '086'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '087'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '088'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '089'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '090'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '091'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '092'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '093'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '094'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '095'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '096'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '097'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '098'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(03)                VALUE '999'.                         
              03 PIC  X(01)                VALUE 'P'.                           
              03 PIC  X(01)                VALUE SPACE.                         
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
           02 CFT9-TABLE               REDEFINES CFT9-VALUES.                   
              03 CFT9-TBL                      OCCURS   97 TIMES                
                                               INDEXED BY CFT9-IDX              
                                                          CFT9-MAX-IDX.         
                 04 CFT9-PT                    PIC  X(03).                      
                 04 CFT9-PT-INST-PROF          PIC  X(01).                      
                    88 CFT9-PT-INST            VALUE 'I'.                       
                    88 CFT9-PT-PROF            VALUE 'P'.                       
                 04 CFT9-PT-INCL-EXCL          PIC  X(01).                      
                    88 CFT9-PT-INCLUDE         VALUE 'I'.                       
                    88 CFT9-PT-EXCLUDE         VALUE 'E'.                       
                 04 CFT9-CF-PT-PROF                           COMP-1.           
                 04 CFT9-CF-PT-PLAN                           COMP-1.           
                 04 CFT9-CF-PT-NONPLAN                        COMP-1.           
                 04 CFT9-CF-PT-OV                             COMP-1.           
                                                                                
