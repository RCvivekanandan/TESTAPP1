      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSNAPSC                                        *00030000
      *    DATE:       18-APR-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING SNAP SHOT RECORD           *00060000
      *                THIS RECORD HAS A DUPLICATE (ELSNAP2C)          *00070001
      *                WHICH HAS THE DATANAME PREFIX OF SSR2-.         *00080001
      *                ANY CHANGES IN THIS COPYBOOK SHOULD BE          *00090001
      *                REFLECTED IN ELSNAP2C.                          *00100001
      *                                                                *00110000
      ******************************************************************00120000
      *                                                                *00130000
      *                      MAINTENANCE HISTORY                       *00140000
      *                                                                *00150000
      *  MOD     DATE     BY  DRPT                ACTION               *00160000
      * ----- ----------- --- ----- ---------------------------------- *00170000
      * 01.00 18-APR-1989 EGL       CREATED                            *00180000
      *                                                                *00190000
      * 01.00 08-MAR-1990 AKK       ADDED SSR-RECORD-TYPES 4 AND 5     *00191004
      *                                                                *00192005
      * 01.00 25-APR-1990 AKK       ADDED SSR-RECORD-TYPE 6            *00193005
      ******************************************************************00200000
                                                                        00210000
       01  SSR-SNAP-SHOT-RECORD.                                        00220000
           05  SSR-FIXED-PORTION.                                       00230003
               10  SSR-RECORD-TYPE       PICTURE X.                     00240003
                   88  SSR-DUMP-HEADER           VALUE '0'.             00250003
                   88  SSR-DATA-AREA             VALUE '1'.             00260003
                   88  SSR-I-O-ITEM              VALUE '2'.             00270003
                   88  SSR-T-S-ITEM              VALUE '3'.             00280003
                   88  SSR-C-M-ELEMENT           VALUE '4'.             00281004
                   88  SSR-C-M-VALUE             VALUE '5'.             00282004
                   88  SSR-C-S-PPM               VALUE '6'.             00283005
                   88  SSR-VALID-RECORD-TYPE     VALUE '0' THRU '6'.    00290005
               10  SSR-CICS-SYSTEM-ID    PICTURE X(4).                  00300003
               10  SSR-CICS-APPL-ID      PICTURE X(8).                  00310003
               10  SSR-ABEND-DATE        PICTURE S9(7) COMP-3.          00320003
               10  SSR-ABEND-TIME        PICTURE S9(7) COMP-3.          00330003
               10  SSR-TERMINAL-ID       PICTURE X(4).                  00340003
               10  SSR-ABEND-CODE        PICTURE X(4).                  00350003
               10  FILLER        REDEFINES SSR-ABEND-CODE.              00360003
                   15  SSR-ABEND-CODE-1  PICTURE XX.                    00370003
                       88  SSR-ELS-ABEND         VALUE 'EL'.            00380003
                   15  SSR-ABEND-CODE-2  PICTURE XX.                    00390003
               10  SSR-DDNAME            PICTURE X(8).                  00400003
               10  SSR-SORT-ID           PICTURE X.                     00410003
                   88  SSR-SORT-HEADER           VALUE '0'.             00420003
                   88  SSR-SORT-BLOCK            VALUE '1'.             00430003
                   88  SSR-SORT-RECORD           VALUE '2'.             00440003
                   88  SSR-SORT-TS-QUEUE         VALUE '3'.             00450003
                   88  SSR-SORT-TABLE-1          VALUE '4'.             00460003
                   88  SSR-SORT-TABLE-2          VALUE '5'.             00470003
                   88  SSR-SORT-TABLE-3          VALUE '6'.             00480003
               10  SSR-SEQUENCE-NUM      PICTURE S9(4) COMP.            00490003
               10  SSR-AREA-PTR          POINTER.                       00500003
               10  SSR-AREA-LENGTH       PICTURE S9(8) COMP.            00510003
           05  SSR-SUB-AREA.                                            00520000
               10  SSR-SUB-SIZE          PICTURE S9(4) COMP.            00530003
                   88  SSR-MAX-SUB-SIZE      VALUE 4039.                00540000
               10  SSR-SUB-REC       OCCURS 1 TO 4039 TIMES             00550000
                                     DEPENDING ON SSR-SUB-SIZE          00560000
                                     INDEXED BY  SSR-IDX                00570000
                                     PICTURE X.                         00580000
