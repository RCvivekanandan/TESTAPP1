      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSPRCBC                                        *00030000
      *    DATE:       25-APR-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING PRINT CONTROL BLOCK        *00060000
      *                                                                *00070000
      *                THIS CONTROL BLOCK IS PASSED TO AMONG THE       *00080000
      *                PRINT SUBROUTINES USED BY THE BATCH ABEND       *00090000
      *                PROCESSING SUBROUTINES.                         *00100000
      *                                                                *00110000
      ******************************************************************00120000
      *                                                                *00130000
      *                      MAINTENANCE HISTORY                       *00140000
      *                                                                *00150000
      *  MOD     DATE     BY  DRPT                ACTION               *00160000
      * ----- ----------- --- ----- ---------------------------------- *00170000
      * 01.00 25-APR-1989 EGL       CREATED                            *00180000
      *                                                                *00190000
      * 01.01 11-AUG-1997 AKK       ADDED SUPPORT FOR YEAR 2000 AND    *00201006
      *                             EXPANDED GRP/CONTRACT NUMBERS.     *00202006
      ******************************************************************00203006
                                                                        00210000
       01  PCB-PRINT-CONTROL-BLOCK.                                     00220000
           05  PCB-FUNCTION-CODE         PICTURE X.                     00230000
               88  PCB-OPEN-PRINTER              VALUE 'O'.             00240000
               88  PCB-PRINT-LINE                VALUE 'P'.             00250000
               88  PCB-CLOSE-PRINTER             VALUE 'C'.             00260000
           05  PCB-RETURN-CODE           PICTURE X.                     00270001
               88  PCB-OK                        VALUE 'O'.             00280001
               88  PCB-INVALID-REQUEST           VALUE 'I'.             00290001
               88  PCB-INVALID-DATA-FOUND        VALUE 'D'.             00300002
           05  PCB-PRINT-AREA.                                          00310000
               10  PCB-SPACING           PICTURE X.                     00320000
                   88  PCB-EJECT                 VALUE '1'.             00330000
                   88  PCB-SINGLE-SPACE          VALUE ' '.             00340000
                   88  PCB-DOUBLE-SPACE          VALUE '0'.             00350000
                   88  PCB-TRIPLE-SPACE          VALUE '-'.             00360000
                   88  PCB-VALID-SPACING         VALUE '1', ' ',        00370000
                                                       '0', '-'.        00380000
               10  PCB-PRINT-TEXT        PICTURE X(132).                00390003
           05  PCB-PRINT-DATE            PICTURE 9(06).                 00400011
           05  PCB-PAGE-HEADING-INFO.                                   00410000
               10  PCB-ABEND-CODE        PICTURE X(4).                  00420000
               10  PCB-ABEND-DT.                                        00430012
                   15 PCB-ABEND-DATE-CC  PICTURE X.                     00431012
                   15 PCB-ABEND-DATE     PICTURE S9(5) COMP-3.          00432010
               10  PCB-ABEND-DATE-CENTURY REDEFINES                     00433012
                       PCB-ABEND-DT      PICTURE S9(07) COMP-3.         00434012
               10  PCB-ABEND-TIME        PICTURE S9(7) COMP-3.          00440000
               10  PCB-CICS-APPL-ID      PICTURE X(8).                  00450000
               10  PCB-CICS-SYSTEM-ID    PICTURE X(4).                  00460000
               10  PCB-TERMINAL-ID       PICTURE X(4).                  00470000
               10  PCB-PLAN-CODE         PICTURE X(3).                  00480006
               10  PCB-GROUP-NUM.                                       00481009
                   15 PCB-GROUP-NUM-1-3  PICTURE X(3).                  00482006
                   15 PCB-GROUP-NUMBER   PICTURE X(6).                  00483009
               10  PCB-SECT-NUMBER.                                     00490009
                   15 PCB-SECT-NUM-1     PICTURE X(1).                  00491008
                   15 PCB-SECTION-NUM    PICTURE X(4).                  00492009
               10  PCB-PKG-CODE          PICTURE X(3).                  00493007
      ******************************************************************00500000
      *                                                                *00510000
      *        PCB-CONTROL-INFO IS FOR USE BY PRINT SUBROUTINE         *00520000
      *        ONLY.  PLEASE DO NOT ALTER ANY OF THE FIELDS AFTER      *00530000
      *        THE PRINTER IS OPEN.                                    *00540000
      *                                                                *00550000
      ******************************************************************00560000
           05  PCB-CONTROL-INFO.                                        00570000
               10  PCB-PAGE-NUMBER       PICTURE S9(5) COMP-3.          00580000
               10  PCB-CURRENT-LINE      PICTURE S9(3) COMP-3.          00590000
               10  PCB-MAX-LINES         PICTURE S9(3) COMP-3.          00600000
                   88  PCB-DEFAULT-MAX-LINES     VALUE +56.             00610004
