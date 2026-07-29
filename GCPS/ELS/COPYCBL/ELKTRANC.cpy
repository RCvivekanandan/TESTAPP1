      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELKTRANC                                        *00030000
      *    DATE:       06-DEC-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   CONTAINS THE TRANSLATION OF A CODE VALUE AS     *00060000
      *                FOUND IN THE CODES MANUAL BY THE CODES MANUAL   *00070000
      *                INTERFACE PROGRAM ELKCMIF.                      *00080000
      *                                                                *00090000
      ******************************************************************00100000
      *                                                                *00110000
      *                      MAINTENANCE HISTORY                       *00120000
      *                                                                *00130000
      *  MOD     DATE     BY  DRPT                ACTION               *00140000
      * ----- ----------- --- ----- ---------------------------------- *00150000
      * 01.00 06-DEC-1989 EGL       CREATED                            *00160000
      *                                                                *00170000
      ******************************************************************00180000
                                                                        00190000
                                                                        00200000
       01  CMTR-TRANSLATION.                                            00210000
           05  CMTR-FIXED-PART.                                         00211001
               10  CMTR-INTERNAL-ID      PICTURE  X(8).                 00220002
               10  CMTR-NBR-TRANS-LINES  PICTURE S9(04)       COMP.     00230002
           05  CMTR-TRANS-LINE           PICTURE  X(79)                 00240001
                                         OCCURS 1 TO 400 TIMES          00250003
                                         DEPENDING ON                   00260001
                                            CMTR-NBR-TRANS-LINES        00261001
                                         INDEXED BY CMTR-TRANS-IDX.     00270001
