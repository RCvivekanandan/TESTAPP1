      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELKDESCC                                        *00030001
      *    DATE:       06-DEC-1989                                     *00040001
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   CONTAINS THE DESCRIPTION OF A FIELD AS          *00060001
      *                FOUND IN THE CODES MANUAL BY THE CODES MANUAL   *00070000
      *                INTERFACE PROGRAM ELKCMIF.                      *00080001
      *                                                                *00090000
      ******************************************************************00100000
      *                                                                *00110000
      *                      MAINTENANCE HISTORY                       *00120000
      *                                                                *00130000
      *  MOD     DATE     BY  DRPT                ACTION               *00140000
      * ----- ----------- --- ----- ---------------------------------- *00150000
      * 01.00 06-DEC-1989 EGL       CREATED                            *00160001
      *                                                                *00170000
      ******************************************************************00180000
                                                                        00190000
                                                                        00200000
       01  CMFD-FIELD-DESCRIPTION.                                      00210001
           05  CMFD-FIXED-PART.                                         00211002
               10  CMFD-INTERNAL-ID      PICTURE  X(8).                 00220002
               10  CMFD-NBR-DESCR-LINES  PICTURE S9(04)          COMP.  00230002
           05  CMFD-DESCR-LINE           PICTURE  X(79)                 00240002
                                         OCCURS 1 TO 12 TIMES           00250002
                                         DEPENDING ON                   00260002
                                            CMFD-NBR-DESCR-LINES        00261002
                                         INDEXED BY CMFD-DESCR-IDX.     00270002
