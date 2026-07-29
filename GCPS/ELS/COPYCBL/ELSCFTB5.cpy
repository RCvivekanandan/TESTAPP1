      ******************************************************************        
      *                         COPYLIB Member                         *        
      ******************************************************************        
      *                                                                *        
      *    Member Name:   ELSCFTB5                                     *        
      *    Member Title:  Internal Descriptor Confidence Factors Table *        
      *    Date Created:  02-Dec-1992                                  *        
      *    Author:        Automated Procedure                          *        
      *                                                                *        
      *    Function:                                                   *        
      *       This is the table of the confidence factors derived from *        
      *       the internal descriptor in an accumulator table.         *        
      *                                                                *        
      ******************************************************************        
      *                                                                *        
      *                       Maintenance History                      *        
      *                                                                *        
      *  Mod     Date      By               Action/Reason              *        
      * ----- ----------- ---- --------------------------------------- *        
      * 01.00 02-Dec-1992 AUTO Created.                                *        
      * 01.01 02-Feb-1993 AUTO Revised values for blank internal       *        
      *                        descriptor field value.                 *        
      * 01.02 24-aug-1993 bak  corrected factors for dayreduce, and    *        
      *                        psychabuse and subsabuse                *        
      * 02.00 18-oct-1993 bak  add additional entries for emergency and*        
      *                        office visit support                    *        
      * 03.00 01-aug-1994 rgo  add 21 internal descriptors.            *        
      *                        For a list of these Internal Descriptors*        
      *                        and some documentation on Confidence    *        
      *                        factors and Internal Descriptors, see:  *        
      *                     G:\BlueChip\Coleman\Els\Pmci\Confidf1.doc  *        
      *                                                                *        
      * 04.00 26-may-1999 akk  add wellcagea, wellcagec                *        
      *                                                                *        
      * 04.00 27-may-1999 akk  add psychage                            *        
      *                                                                *        
      * 05.00 19-apr-2000 akk  add mammo, this may be an experiment    *        
      *                        for TX for Bluestorm.                   *        
      *                        also added chiro, off age,officeage,    *        
      *                        mammogram, rxdrugs, trnsplnt1-4,       *         
      *                        trnsplnt, vision, wellage,wellageb.              
      *                                                                         
      * 05.01 23-jun-2000 akk  added the following with all zereos     *        
      *                        til I hear from TX re: Bluestorm.       *        
      *                        accident, acp age, acupntur, aidsmx,    *        
      *                        chemnosmi, coinsin, coinsmax,          *         
      *                        coinsooa, coinsoon, coinsurnc,                   
      *                        copayage, deduct, deductin, deductmax,           
      *                        deductooa, deductoon, drugs, durmed,             
      *                        healthmax, hospmax, incoins, indeduct,           
      *                        invitro, lifeage, lifemax, lifetime,             
      *                        lowerelin, mennosmi, mentalhth,                  
      *                        mhcdnosmi, mntlabuse, mntnosmi,                  
      *                        no smi in, nosmi ooa, nosmi oon,                 
      *                        obgynexam, obgynvis, ooacoins,                   
      *                        ooadeduct, ooncoins, oondeduct,                  
      *                        physical, podiatry, prevage, prostate,           
      *                        psynosmi, replpros, sigmoid, tmjappmx,           
      *                        tmjmax,                                          
      *                        SET LOWERELIN TO SAME VALUES AS                  
      *                        LOWERLIN                                         
      * 07/06/00    AKK        ADDED ASTHMAKIT AND COINSHALB                    
      ******************************************************************        
                                                                                
       01  CFT5-CNFDNC-FCTRS.                                                   
           02 CFT5-NBR-ENTRS                   PIC S9(04)        COMP           
                                               VALUE  +219.                     
           02 CFT5-NBR-TBL-ENTRIES REDEFINES CFT5-NBR-ENTRS                     
                                               PIC S9(04)        COMP.          
           02 CFT5-VLS.                                                         
              03 PIC  X(09)                VALUE '         '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ABUSEMAX '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ACCIDENT '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ACP AGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ACUPNTUR '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ACUTECOIN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'AIDEVAL  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'AIDSMX   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'AMBULANCE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ANCILLARY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ASTHMAKIT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ATCPFARE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ATCPHOTEL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ATCPORGAN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ATCPTRAVL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BC PROCTO'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BCPROVIDR'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BENPERIOD'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BILENSMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BIRTHCNTR'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BLOOD    '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BS PROCTO'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'BSPROVIDR'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CARDREHAB'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHCDAYS  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHCVISITS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHEMNOSMI'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHEMOTHPY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHIRO    '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CHIROPRAT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSHALB'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSIN  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSMAX '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSOOA '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSOON '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COINSURNC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONSULT  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONTACACQ'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONTACDSP'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONTACMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONTAMACQ'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'CONTAMDSP'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'COPAYAGE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DAYREDUCE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DAYVISIT '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEDUCT   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEDUCTIN '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEDUCTMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEDUCTOOA'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEDUCTOON'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DEEPXRAY '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DENT AGE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DENTAL   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DENTALVIS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DENTALXRY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DIALYSIS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DIGNOSTIC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DRUGS    '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DURMED   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DXAGEINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'DXAGEPRO '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(09)                VALUE 'EARTEST  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ECFCOINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ECFDAYS  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ECFVISITS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'EMERGENCY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'EMERGSURG'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ERDIAGSUR'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'EYETEST  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FIRSTEAC '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FIRSTEMC '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FLUORIDE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FRAMEACQ '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FRAMEDISP'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FRAMES   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FRAMESMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'FULLSCALE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'GOLDFOIL '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HEALTHAGE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HEARADACQ'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HEARADISP'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HEARNGAID'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HOSPICE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'HOSPMAX  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ILLNESS  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'IMM AGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'INCOINS  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'INDEDUCT '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'INFERTILE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'INVITRO  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'IPMEDICAL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LABAGEINS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LABAGEPRO'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LABORATRY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LENSES   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LENTICMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LIFEAGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LIFEMAX  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
              03                   COMP-1  VALUE  0.000000E-00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LIFETIME '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LOWERELIN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'LOWERLIN'.                    
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MAMAGEALL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MAMAGEINS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MAMAGEPRO'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MAMMO    '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MAMMOGRAM'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MATERNITY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MEDBASMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MEDDEDMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MEDLIFEDA'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MENNOSMI '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MENTALHTH'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MHCDNOSMI'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MISC AGE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MNTLABUSE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MNTNOSMI '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'MUSCLEMAN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NEGBANK  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NO SMI IN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONACUTE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONDEDUCT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-00.                 
              03                   COMP-1  VALUE  7.000000E-00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONDIAG  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONEMERG '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONOPEX  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONPLAN  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NONPSYCH '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NOSMI OOA'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NOSMI OON'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'NURSING  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OBGYNEXAM'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OBGYNVIS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OCCUPTHPY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OFF AGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OFFICEAGE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OFFICEVIS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OOACOINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OOADEDUCT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OONCOINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OONDEDUCT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OP NONACC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OPACCIDNT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OPDIAG   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ORTHODONT'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OTHERDIAG'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'OTHERHEAL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PAPAGEINS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PAPAGEPRO'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PASTORAL '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(09)                VALUE 'PERDAYMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PERIOGUM '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PERIOVIS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PHYSICAL '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PHYSOTHPY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PODIATRY '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'POSBANK  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PRESDRUG '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PREVAGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PRISMMAX '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PROSTATE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PROVIDER '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PSYCABUSE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PSYCH    '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PSYCHAGE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  3.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PSYNOSMI '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PXAGEINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'PXAGEPRO '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'QUADSCALE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'RADIATION'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'REPLPROS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'REVSTERLZ'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'ROOMBOARD'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE  9.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'RXDRUGS  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SGLENSMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SIGMOID  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SPECANCIL'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SPECDIAGS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SPECPROCS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SPEECH   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'STERLIZE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SUBSABUSE'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE  9.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SUPERXRAY'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SUPPLEACC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SUPPLEMED'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'SURGERY  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TEETHCLEN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'THERAPY  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE  5.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TINTMAX  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TMJAPPMX '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TMJMAX   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRLENSMAX'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRNSPLNT1'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRNSPLNT2'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRNSPLNT3'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRNSPLNT4'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'TRSPLANT '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  0.000000E+00.                 
              03                   COMP-1  VALUE  7.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'UPPERELIN'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'VIS AGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'VISION   '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'VISIONPAR'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'VISIONPGM'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL LABC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL LABS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL PAPC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL PAPS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL PXC '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELL PXS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLAGE  '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLCAGE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLCAGEA'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLCAGEB'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLCAGEC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLCARE '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLDIAGC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLDIAGS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLXRAYC'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'WELLXRAYS'.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
                                                                                
              03 PIC  X(09)                VALUE 'XRAGEINS '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'XRAGEPRO '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
              03 PIC  X(09)                VALUE 'XRAY     '.                   
              03 PIC  X(03)                VALUE SPACES.                        
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE  1.000000E-01.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
              03                   COMP-1  VALUE -1.000000E+00.                 
                                                                                
                                                                                
           02 CFT5-VL-TBL              REDEFINES CFT5-VLS.                      
              03 CFT5-TBL                      OCCURS   219 TIMES               
                                               INDEXED BY CFT5-IDX              
                                                          CFT5-MAX-IDX.         
                 04 CFT5-INTD                  PIC  X(09).                      
                 04                            PIC  X(03).                      
                 04 CFT5-CF-INTD-INST                            COMP-1.        
                 04 CFT5-CF-INTD-PROF                            COMP-1.        
                 04 CFT5-CF-INTD-ATCP                            COMP-1.        
                 04 CFT5-CF-INTD-HSPC                            COMP-1.        
                 04 CFT5-CF-INTD-NRSNG                           COMP-1.        
                 04 CFT5-CF-INTD-PSYCH                           COMP-1.        
                 04 CFT5-CF-INTD-RM-BRD                          COMP-1.        
                 04 CFT5-CF-INTD-SB-ABS                          COMP-1.        
                 04 CFT5-CF-INTD-EMRGNC                          COMP-1.        
                 04 CFT5-CF-INTD-OFVST                           COMP-1.        
