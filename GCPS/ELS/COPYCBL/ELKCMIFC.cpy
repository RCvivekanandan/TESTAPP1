      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELKCMIFC               COBOL II VERSION         *00030000
      *                ELKCMI1C               COBOL  I VERSION         *00040000
      *    DATE:       06-DEC-1989                                     *00050000
      *    AUTHOR:     EDWARD G LISS                                   *00060000
      *    FUNCTION:   COMMON AREA TO THE GENERIC CODES MANUAL         *00070000
      *                INTERFACE PROGRAM.                              *00080000
      *                                                                *00090000
      *                                                                *00100000
      ******************************************************************00110000
      *                                                                *00120000
      *                      MAINTENANCE HISTORY                       *00130000
      *                                                                *00140000
      *  MOD     DATE     BY  DRPT                ACTION               *00150000
      * ----- ----------- --- ----- ---------------------------------- *00160000
      * 01.00 06-DEC-1989 EGL       CREATED                            *00170000
      *                                                                *00180000
      ******************************************************************00190000
      *                                                                 00200000
      *                 DFHCOMMAREA  FOR ELKCMIF                        00210000
      *                                                                 00220000
           05  CMIF-ARG-TYPE             PIC X(1)   VALUE SPACE.        00230000
               88  CMIF-USE-SYSTEM-ARG              VALUE 'S'.          00240000
               88  CMIF-USE-ENGLISH-ARG             VALUE 'E'.          00250000
               88  CMIF-FREE-STORAGE                VALUE 'F'.          00260000
               88  CMIF-VALID-ARG                   VALUE 'S', 'E',     00270000
                                                          'F'.          00280000
                                                                        00290000
           05  CMIF-TRANS-REQ-TYPE       PIC X(1)   VALUE SPACE.        00300000
               88  CMIF-FULL-TRANS-REQ              VALUE 'T'.          00310000
               88  CMIF-SHORT-TRANS-REQ             VALUE 'S'.          00320000
               88  CMIF-GENERIC-TRANS-REQ           VALUE 'G'.          00330000
               88  CMIF-NO-TRANS-REQ                VALUE SPACE.        00340000
               88  CMIF-VALID-TRAN-REQ              VALUE 'T', 'S',     00350000
                                                          'G', SPACE.   00360000
                                                                        00370000
           05  CMIF-DESC-REQ-TYPE        PIC X(1)   VALUE SPACE.        00380000
               88  CMIF-CODE-DESC-REQ               VALUE 'C'.          00390000
               88  CMIF-FIELD-DESC-REQ              VALUE 'F'.          00400000
               88  CMIF-NO-DESC-REQ                 VALUE SPACE.        00410000
               88  CMIF-VALID-DESC-REQ              VALUE 'C', 'F',     00420000
                                                          SPACE.        00430000
                                                                        00440000
           05  CMIF-GENERIC-ARGUMENT     PIC X(100) VALUE SPACES.       00450000
           05  CMIF-SYSTEM-NAME-ARG    REDEFINES CMIF-GENERIC-ARGUMENT. 00460000
               10  CMIF-SYSTEM-PREFIX    PIC X(8).                      00470000
               10  CMIF-SYSTEM-NAME      PIC X(30).                     00480000
               10  FILLER                PIC X(62).                     00490000
           05  CMIF-ENGLISH-NAME-ARG   REDEFINES CMIF-GENERIC-ARGUMENT. 00500000
               10  CMIF-ENGLISH-PREFIX   PIC X(8).                      00510000
               10  CMIF-ENGLISH-NAME     PIC X(75).                     00520000
               10  FILLER                PIC X(17).                     00530000
                                                                        00540000
           05  CMIF-CODE-VALUE           PIC X(10)  VALUE SPACES.       00550000
                                                                        00560000
           05  CMIF-RETURN-CODE          PIC S9(4)  VALUE ZERO COMP.    00570000
               88  CMIF-OK                          VALUE ZERO.         00580000
               88  CMIF-INVALID-ARG                 VALUE 1.            00590000
               88  CMIF-ARGUMENT-NOT-FOUND          VALUE 2.            00600000
               88  CMIF-CODE-VALUE-NOT-FOUND        VALUE 3.            00610000
                                                                        00620000
           05  CMIF-GENERIC-DESC-AREA    PIC X(80)  VALUE SPACES.       00630000
           05  CMIF-CODE-DESC-AREA    REDEFINES CMIF-GENERIC-DESC-AREA. 00640000
               10  CMIF-CODE-DESC        PIC X(50).                     00650000
               10  FILLER                PIC X(30).                     00660000
                                                                        00670000
           05  CMIF-TRAN-PTR             POINTER    VALUE NULL.         00680000
           05  CMIF-DESC-PTR             POINTER    VALUE NULL.         00690000
           05  CMIF-WORK-AREA-PTR        POINTER    VALUE NULL.         00700000
