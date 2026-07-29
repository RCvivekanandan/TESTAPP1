      ******************************************************************0000000 
      *                                                                *0000000 
      *    COPYBOOK:   ELSTGTBC                                        *0000000 
      *    DATE:       17-JUN-1987                                     *0000000 
      *    AUTHOR:     EDWARD G. LISS                                  *0000000 
      *    FUNCTION:   TOPIC LEVEL STORAGE REQUIREMENTS TABLE          *0000000 
      *                                                                *0000000 
      ******************************************************************0000000 
      *                                                                *0000000 
      *                      MAINTENANCE HISTORY                       *0000000 
      *                                                                *0000000 
      *  MOD     DATE     BY  DRPT                ACTION               *0000000 
      * ----- ----------- --- ----- ---------------------------------- *0000000 
      * 01.00 17-JUN-1987 EGL       CREATED SKELETON                   *0000000 
      * 01.01 25-SEP-1987 EGL       ADDED BENEFIT PROVISION PTR LIST   *0000000 
      * 01.02 12-JAN-1988 LET       REB ADDED CHANGES FOR A NEW TOPIC  *0000000 
      *                             PPO NETWORK.                       *0000000 
      * 01.03 25-JAN-1988 LET       REB ADDED CHANGES FOR A NEW TOPIC  *0000000 
      * 01.0? 20-OCT-1998 AKK       ADDED ACP NEW TOPIC                *0000000 
      * 01.02 04-MAR-1999 AKK       ADDED BAE AND BAE NET TOPIC        *0000000 
      *                                                                *0000000 
      * LAST CREATED USING COPYGEN BY R360027                          *0000000 
      *                ON 96/02/22 AT 11:16                            *0000000 
      *      FROM SOURCE TABLE R360043.ELS59.COPYLIB(AANFILES)         *0000000 
      *                                                                *0000000 
      ******************************************************************0000000 
       01  STORAGE-ALLOCATION-TABLE.                                    0000000 
           02 SAT-NUMBER-OF-ITEMS      PICTURE S9(4) VALUE 00000077     0000000 
                                                     COMP SYNC.         0000000 
           02 SAT-DEFINITION.                                           0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTABM  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTACL  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTACP  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTADL  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTAMBUL'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTANEST'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTAOL  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTASSTS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTATCP '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTBAE  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTBANET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTBEXNR'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCBL  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCBNET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCONGS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCONSL'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCOOBN'.               0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCOOHC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCOSMS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCPNET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCPO  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTCSMRY'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDENTS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDRUGS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDUREQ'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDXBIP'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDXBOP'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDXEMR'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTDXRTN'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTEMERG'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTEXBEN'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTEXTCF'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTGENRL'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTGVLPD'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTGVLPL'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTGVLPN'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTHEARC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTHOSP '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTINPTS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTIOB  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMASOP'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMCN  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMEDCA'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMEDNC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMHSC '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMONDC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMOPS '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTMSA  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTNURSE'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTOBREL'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTOBSTS'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTORTHO'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTOUTCC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPAN  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPANET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPAR  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPAT  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPODIA'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPOS  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPPNET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPPO  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPROST'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPSYCH'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTPVE  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTREIMB'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTROOMB'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTRPNET'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTRPO  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTSAM  '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTSUBST'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTSURGR'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTTHERP'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTVISON'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTWAITP'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTWEEKN'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTWELL '.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
              03 FILLER.                                                0000000 
                 04 FILLER PICTURE X(8) VALUE 'ELTWORKC'.               0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE '*'.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
                 04 FILLER PICTURE X(1) VALUE ' '.                      0000000 
           02 SAT-TABLE                REDEFINES SAT-DEFINITION         0000000 
                                       OCCURS 00000077 TIMES            0000000 
                                       ASCENDING KEY SAT-TOPIC-PGM-NAME 0000000 
                                       INDEXED BY SAT-IDX.              0000000 
               03 SAT-ARGUMENT-ITEM.                                    0000000 
                  04 SAT-TOPIC-PGM-NAME    PICTURE  X(08).              0000000 
               03 SAT-FUNCTION-ITEMS.                                   0000000 
                  04 SAT-GROUP-SPEC-IND    PICTURE  X.                  0000000 
                     88  SAT-GROUP-SPEC-REQ         VALUE '*'.          0000000 
                     88  SAT-GROUP-SPEC-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-CONTRACT-IND      PICTURE  X.                  0000000 
                     88  SAT-CONTRACT-REQ           VALUE '*'.          0000000 
                     88  SAT-CONTRACT-REQ-REQ       VALUE ' '.          0000000 
                  04 SAT-BEN-PROV-IND      PICTURE  X.                  0000000 
                     88  SAT-BEN-PROV-REQ           VALUE '*'.          0000000 
                     88  SAT-GEN-PROV-NOT-REQ       VALUE ' '.          0000000 
                  04 SAT-TABULAR-IND       PICTURE  X.                  0000000 
                     88  SAT-TABULAR-REQ            VALUE '*'.          0000000 
                     88  SAT-TABULAR-NOT-REQ        VALUE ' '.          0000000 
                  04 SAT-PAY-LVL-GRP-IND   PICTURE  X.                  0000000 
                     88  SAT-PAY-LVL-GRP-REQ        VALUE '*'.          0000000 
                     88  SAT-PAY-LVL-GP-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-SUBR-PARM-IND     PICTURE  X.                  0000000 
                     88  SAT-SUBR-PARM-REQ          VALUE '*'.          0000000 
                     88  SAT-SUBR-PARM-NOT-REQ      VALUE ' '.          0000000 
                  04 SAT-FIELD-VAL-IND     PICTURE  X.                  0000000 
                     88  SAT-FIELD-VAL-REQ          VALUE '*'.          0000000 
                     88  SAT-FIELD-VAL-NOT-REQ      VALUE ' '.          0000000 
                  04 SAT-SYS-TABULAR-IND   PICTURE  X.                  0000000 
                     88  SAT-SYS-TABULAR-REQ        VALUE '*'.          0000000 
                     88  SAT-SYS-TABULAR-NOT-REQ    VALUE ' '.          0000000 
                  04 SAT-WORK-FILE1-IND    PICTURE  X.                  0000000 
                     88  SAT-WORK-FILE1-REQ         VALUE '*'.          0000000 
                     88  SAT-WORK-FILE1-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-WORK-FILE2-IND    PICTURE  X.                  0000000 
                     88  SAT-WORK-FILE2-REQ         VALUE '*'.          0000000 
                     88  SAT-WORK-FILE2-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-WORK-FILE3-IND    PICTURE  X.                  0000000 
                     88  SAT-WORK-FILE3-REQ         VALUE '*'.          0000000 
                     88  SAT-WORK-FILE3-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-WORK-FILE4-IND    PICTURE  X.                  0000000 
                     88  SAT-WORK-FILE4-REQ         VALUE '*'.          0000000 
                     88  SAT-WORK-FILE4-NOT-REQ     VALUE ' '.          0000000 
                  04 SAT-BEN-PROV-PTR-IND  PICTURE  X.                  0000000 
                     88  SAT-BEN-PROV-PTR-REQ       VALUE '*'.          0000000 
                     88  SAT-BEN-PROV-PTR-NOT-REQ   VALUE ' '.          0000000 
