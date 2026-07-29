      ******************************************************************0000000 
      *                                                                *0000000 
      *    COPYBOOK:   ELSTPTBC                                        *0000000 
      *    DATE:       14-OCT-1986                                     *0000000 
      *    AUTHOR:     EDWARD G. LISS                                  *0000000 
      *                RICHARD J. LUKETICH                             *0000000 
      *    FUNCTION:   TOPIC SELECTION TABLE AND SEARCH KEY DEFINITION *0000000 
      *                                                                *0000000 
      *                                                                *0000000 
      ******************************************************************0000000 
      *                                                                *0000000 
      *                      MAINTENANCE HISTORY                       *0000000 
      *                                                                *0000000 
      *  MOD     DATE     BY  DRPT                ACTION               *0000000 
      * ----- ----------- --- ----- ---------------------------------- *0000000 
      * 01.00 14-OCT-1986 EGL       CREATED                            *0000000 
      * 01.01 23-NOV-1987 EGL       ADDED THE STACK MENU SWITHES FOR   *0000000 
      *                             MODIFIER 1 AND 2.                  *0000000 
      * 01.02 12-JAN-1988 LET       RED ADDED CHANGES FOR A NEW TOPIC  *0000000 
      *                             PPO NETWORK.                       *0000000 
      * 01.03 25-JAN-1988 LET       REB ADDED CHANGES FOR A NEW TOPIC  *0000000 
      *                             CONTRACT SUMMARY.                  *0000000 
      * 01.04 08-MAR-1988 LET       REB ADDED CHANGES TO ALLOW THE 'B' *0000000 
      *                             OPTION FOR PROVIDER CLASS AND SER- *0000000 
      *                             VICE CLASS FOR 'CONTRACT SUMMARY'. *0000000 
      * 02.00 05-MAY-2000 AKK       ADDED BAE AND BAENET TOPIC       ' *0000000 
      *                                                                *0000000 
      * LAST GENERATED USING COPYGEN BY R360027                        *0000000 
      *                  ON 96/04/10 AT 10:58                          *0000000 
      *      FROM SOURCE TABLE R360043.ELS12059.TABLELIB(AAOTOPIC)     *0000000 
      *                                                                *0000000 
      *                                                                *0000000 
      ******************************************************************0000000 
       01  TOPIC-SELECTION-TABLE.                                       0000000 
           02 TST-NUMBER-OF-ITEMS      PICTURE S9(4) VALUE 00000144     0000000 
                                                     COMP SYNC.         0000000 
           02 TST-DEFINITION.                                           0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSTOPIC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ABUSE           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ABUSE           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTSUBST'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ADMIN           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ADMIN           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTGENRL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'AMB             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'AMB             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTAMBUL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ANES            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ANES            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTANEST'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ASSTS           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'P'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTASSTS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'BAENET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTBANET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'BEN             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTBEXNR'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CBLNET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCBNET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'Y'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSLCCP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'ATCP            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTATCP '.              0000000 
              03 FILLER.                                                        
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.              
                 04 FILLER PICTURE X(16) VALUE '                '.              
                 04 FILLER PICTURE X(01) VALUE '*'.                             
                 04 FILLER PICTURE X(01) VALUE ' '.                             
                 04 FILLER PICTURE X(16) VALUE 'BAE             '.              
                 04 FILLER PICTURE X(01) VALUE ' '.                             
                 04 FILLER PICTURE X(16) VALUE '                '.              
                 04 FILLER PICTURE X(01) VALUE ' '.                             
                 04 FILLER PICTURE X(08) VALUE 'ELTBAE  '.                      
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'CBL             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCBL  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'CPO             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCPO  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMH             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTSAM  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'HOSP            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTHOSP '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'IOB             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTIOB  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MASOP           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMASOP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MCN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMCN  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MEDNEC          '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMEDNC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MHSC            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMHSC '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MOND            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMONDC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MOPS            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMOPS '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'MSA             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMSA  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'PAN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPAN  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'PAR             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPAR  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'PAT             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPAT  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'POS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPOS  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'PPO             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPPO  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'REIMB           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTREIMB'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'RPO             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTRPO  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CCP             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'WEEK            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTWEEKN'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CHC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CHC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCOOHC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COB             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COB             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCOOBN'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COINS           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COINS           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTACL  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CON             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CON             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCONSL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COPAY           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'COPAY           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTACP  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CPONET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCPNET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CS              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'B'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE 'B'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'Y'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSCSMU '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'CS              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'B'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE 'B'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCSMRY'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DED             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DED             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTADL  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DME             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DME             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DME             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDUREQ'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMR             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMR             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDXEMR'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'IPS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'IPS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDXBIP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OPS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OPS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDXBOP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'RTN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'DX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'RTN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDXRTN'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ECF             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ECF             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTEXTCF'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'EMER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTEMERG'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'EXCL            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'EXCL            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTEXBEN'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'HEAR            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'HEAR            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'HEAR            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTHEARC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'MAX             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'MAX             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTABM  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'MEDI            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'MEDI            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTMEDCA'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'NURS            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'NURS            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'NURS            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTNURSE'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTOBSTS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OBREL           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OB              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'OBREL           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTOBREL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OCC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OCC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTOUTCC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OPX             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'OPX             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTAOL  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PANNET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPANET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'POD             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'POD             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPODIA'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PPONET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPPNET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'ORTHO           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'ORTHO           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'ORTHO           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTORTHO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'PROST           '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPROST'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PSYCH           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PSYCH           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PSYCH           '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPSYCH'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PVE             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PVE             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'Y'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSLPVE'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'PVE             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTPVE  '.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'ROOM            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTROOMB'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'RPONET          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'I'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTRPNET'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'RX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'RX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'RX              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDRUGS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSGVLM1'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'D               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTGVLPD'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'L               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTGVLPL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'N               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'Y'.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSGVLM2'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SPC             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE 'N               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTGVLPN'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'CONG            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'CONG            '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCONGS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'COS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'COS             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTCOSMS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'DEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'DEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'DEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTDENTS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'GEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'GEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'SURG            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE 'GEN             '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTSURGR'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'THER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '&               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSUBTP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'THER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'THER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'THER            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '*               '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTTHERP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VIS             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VIS             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VIS             '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTVISON'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VISITS          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VISITS          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSSRVLO'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'VISITS          '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTINPTS'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'WAIT            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'WAIT            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTWAITP'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'WC              '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE 'B'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTWORKC'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'WELL            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '&'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELSPRVCL'.              0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(16) VALUE 'WELL            '.      0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE '*'.                     0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(16) VALUE '                '.      0000000 
                 04 FILLER PICTURE X(01) VALUE ' '.                     0000000 
                 04 FILLER PICTURE X(08) VALUE 'ELTWELL '.              0000000 
           02 TST-TABLE                REDEFINES TST-DEFINITION         0000000 
                                       OCCURS 00000144 TIMES            0000000 
                                       INDEXED BY TST-FIRST-IDX         0000000 
                                                  TST-LAST-IDX.         0000000 
               03 TST-ARGUMENT-ITEMS.                                   0000000 
                  04 TST-TOPIC                PICTURE  X(16).           0000000 
                  04 TST-SUB-TOPIC            PICTURE  X(16).           0000000 
                  04 TST-PROVIDER-CLASS                                 0000000 
                                              PICTURE  X(01).           0000000 
                  04 TST-SERVICE-CLASS        PICTURE  X(01).           0000000 
                  04 TST-MODIFIER-1           PICTURE  X(16).           0000000 
                  04 TST-STACK-MODIFIER-1-SW  PICTURE  X(01).           0000000 
                     88  TST-STACK-MODIFIER-1          VALUE 'Y'.       0000000 
                  04 TST-MODIFIER-2           PICTURE  X(16).           0000000 
                  04 TST-STACK-MODIFIER-2-SW  PICTURE  X(01).           0000000 
                     88  TST-STACK-MODIFIER-2          VALUE 'Y'.       0000000 
               03 TST-FUNCTION-ITEMS.                                   0000000 
                  04 TST-SEL-PGM-NAME         PICTURE  X(08).           0000000 
