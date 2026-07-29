      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSNAPPC                                        *00030000
      *    DATE:       20-SEP-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING SNAP SHOT HEADER           *00060000
      *                PREFIX RECORD.  THIS RECORD CONTAINS            *00070001
      *                IDENTIFICATION INFORMATION AND THE SSCB.        *00080000
      *                                                                *00090000
      ******************************************************************00100000
      *                                                                *00110000
      *                      MAINTENANCE HISTORY                       *00120000
      *                                                                *00130000
      *  MOD     DATE     BY  DRPT                ACTION               *00140000
      * ----- ----------- --- ----- ---------------------------------- *00150000
      * 01.00 20-SEP-1989 EGL       CREATED                            *00160000
      *                                                                *00170000
      ******************************************************************00180000
                                                                        00190000
       01  SSP-SNAP-SHOT-PREFIX.                                        00200000
           05  SSP-FIXED-PORTION         PICTURE X(80).                 00210000
           05  FILLER    REDEFINES   SSP-FIXED-PORTION.                 00220000
               10  SSP-TASK-NUMBER       PICTURE S9(7) COMP-3.          00221002
               10  SSP-FACILITY          PICTURE X(4).                  00230000
               10  SSP-OPERATOR-INITIALS PICTURE X(3).                  00240000
               10  SSP-USER-ID           PICTURE X(8).                  00250000
               10  SSP-USER-DEPT-NO      PICTURE X(3).                  00260000
               10  FILLER                PICTURE X(58).                 00270002
           05  SSP-STATUS-BLOCK.                                        00280000
               10  SSP-STATUS-SIZE       PICTURE S9(4) COMP.            00290000
                   88  SSP-MAX-SIZE              VALUE 4000.            00300000
               10  SSP-STATUS-REC    OCCURS 1 TO 4000 TIMES             00310000
                                     DEPENDING ON SSP-STATUS-SIZE       00320001
                                     INDEXED BY  SSP-IDX                00330000
                                     PICTURE X.                         00340000
