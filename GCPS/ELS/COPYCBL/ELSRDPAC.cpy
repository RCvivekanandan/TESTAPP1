      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSRDPAC                                        *00030000
      *    DATE:       26-APR-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING READ SUBROUTINE PARM LIST  *00060000
      *                                                                *00070000
      ******************************************************************00080000
      *                                                                *00090000
      *                      MAINTENANCE HISTORY                       *00100000
      *                                                                *00110000
      *  MOD     DATE     BY  DRPT                ACTION               *00120000
      * ----- ----------- --- ----- ---------------------------------- *00130000
      * 01.00 26-APR-1989 EGL       CREATED                            *00140000
      *                                                                *00150000
      ******************************************************************00160000
                                                                        00170000
       01  RDP-READ-PARAMETERS.                                         00180000
           02  RDP-FUNCTION-CODE       PICTURE X.                       00190000
               88  RDP-OPEN-FILE               VALUE 'O'.               00200000
               88  RDP-READ-FILE               VALUE 'R'.               00210000
               88  RDP-CLOSE-FILE              VALUE 'C'.               00220000
           02  RDP-RETURN-CODE         PICTURE X.                       00230000
               88  RDP-EOF                     VALUE 'Y', 'E'.          00240002
               88  RDP-CALL-OK                 VALUE 'N'.               00250000
               88  RDP-INVALID-DATA            VALUE 'I'.               00260000
               88  RDP-SHORT-RECORD            VALUE 'S'.               00270001
               88  RDP-UNEXPECTED-EOF          VALUE 'E'.               00280002
           02  RDP-RECORD.                                              00290000
             05  RDP-RECORD-TYPE       PICTURE X.                       00300000
                 88  RDP-DUMP-HEADER           VALUE '0'.               00310000
                 88  RDP-DATA-AREA             VALUE '1'.               00320000
                 88  RDP-I-O-ITEM              VALUE '2'.               00330000
                 88  RDP-T-S-ITEM              VALUE '3'.               00340000
             05  RDP-CICS-SYSTEM-ID    PICTURE X(4).                    00350000
             05  RDP-CICS-APPL-ID      PICTURE X(8).                    00360000
             05  RDP-ABEND-DATE        PICTURE S9(7) COMP-3.            00370000
             05  RDP-ABEND-TIME        PICTURE S9(7) COMP-3.            00380000
             05  RDP-TERMINAL-ID       PICTURE X(4).                    00390000
             05  RDP-ABEND-CODE        PICTURE X(4).                    00400000
             05  FILLER        REDEFINES RDP-ABEND-CODE.                00410000
                 10  RDP-ABEND-CODE-1  PICTURE XX.                      00420000
                     88  RDP-ELS-ABEND         VALUE 'EL'.              00430000
                 10  RDP-ABEND-CODE-2  PICTURE XX.                      00440000
             05  RDP-DDNAME            PICTURE X(8).                    00450000
             05  RDP-SORT-ID           PICTURE X.                       00460000
                 88  RDP-SORT-HEADER           VALUE '0'.               00470000
                 88  RDP-SORT-BLOCK            VALUE '1'.               00480000
                 88  RDP-SORT-RECORD           VALUE '2'.               00490000
                 88  RDP-SORT-TS-QUEUE         VALUE '3'.               00500000
                 88  RDP-SORT-TABLE-1          VALUE '4'.               00510000
                 88  RDP-SORT-TABLE-2          VALUE '5'.               00520000
                 88  RDP-SORT-TABLE-3          VALUE '6'.               00530000
             05  RDP-CICS-AREA-PTR     POINTER.                         00540000
             05  RDP-AREA-LENGTH       PICTURE S9(8) COMP.              00550000
             05  RDP-AREA              PICTURE X(65536).                00551004
             05  FILLER   REDEFINES   RDP-AREA.                         00560004
                 10  RDP-AREA-CHAR     OCCURS 65536 TIMES               00570003
                                       PICTURE X.                       00580000
