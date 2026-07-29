      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSNAP2C                                        *00030000
      *    DATE:       18-APR-1989                                     *00040000
      *    AUTHOR:     EDWARD G LISS                                   *00050000
      *    FUNCTION:   ELS ABEND PROCESSING SNAP SHOT RECORD           *00060000
      *                THIS RECORD IS A DUPLICATE OF ELSNAPSC          *00070000
      *                EXCEPT THAT THE DATANAME PREFIX IS SSR2-.       *00080000
      *                ANY CHANGES IN THIS COPYBOOK SHOULD BE          *00090000
      *                REFLECTED IN ELSNAPSC.                          *00100000
      *                                                                *00110000
      ******************************************************************00120000
      *                                                                *00130000
      *                      MAINTENANCE HISTORY                       *00140000
      *                                                                *00150000
      *  MOD     DATE     BY  DRPT                ACTION               *00160000
      * ----- ----------- --- ----- ---------------------------------- *00170000
      * 01.00 18-APR-1989 EGL       CREATED                            *00180000
      *                                                                *00190000
      * 01.01 08-MAR-1990 AKK       ADDED SSR2-RECORD-TYPES 4 AND 5    *00191005
      *                                                                *00192006
      * 01.02 24-MAY-1990 AKK       ADDED SSR2-RECORD-TYPES 6          *00193006
      ******************************************************************00200000
                                                                        00210000
      ****************** 01  SSR2-SNAP-SHOT-RECORD-2. ******************00220001
           05  SSR2-FIXED-PORTION.                                      00221003
               10  SSR2-RECORD-TYPE      PICTURE X.                     00230003
                   88  SSR2-DUMP-HEADER          VALUE '0'.             00240003
                   88  SSR2-DATA-AREA            VALUE '1'.             00250003
                   88  SSR2-I-O-ITEM             VALUE '2'.             00260003
                   88  SSR2-T-S-ITEM             VALUE '3'.             00270003
                   88  SSR2-C-M-ELEMENT          VALUE '4'.             00271005
                   88  SSR2-C-M-VALUE            VALUE '5'.             00272005
                   88  SSR2-C-S-PGM              VALUE '6'.             00273006
                   88  SSR2-VALID-RECORD-TYPE    VALUE '0' THRU '6'.    00280006
               10  SSR2-CICS-SYSTEM-ID   PICTURE X(4).                  00290003
               10  SSR2-CICS-APPL-ID     PICTURE X(8).                  00300003
               10  SSR2-ABEND-DATE       PICTURE S9(7) COMP-3.          00310003
               10  SSR2-ABEND-TIME       PICTURE S9(7) COMP-3.          00320003
               10  SSR2-TERMINAL-ID      PICTURE X(4).                  00330003
               10  SSR2-ABEND-CODE       PICTURE X(4).                  00340003
               10  FILLER        REDEFINES SSR2-ABEND-CODE.             00350003
                   15  SSR2-ABEND-CODE-1 PICTURE XX.                    00360004
                       88  SSR2-ELS-ABEND        VALUE 'EL'.            00370003
                   15  SSR2-ABEND-CODE-2 PICTURE XX.                    00380004
               10  SSR2-DDNAME           PICTURE X(8).                  00390003
               10  SSR2-SORT-ID          PICTURE X.                     00400003
                   88  SSR2-SORT-HEADER          VALUE '0'.             00410003
                   88  SSR2-SORT-BLOCK           VALUE '1'.             00420003
                   88  SSR2-SORT-RECORD          VALUE '2'.             00430003
                   88  SSR2-SORT-TS-QUEUE        VALUE '3'.             00440003
                   88  SSR2-SORT-TABLE-1         VALUE '4'.             00450003
                   88  SSR2-SORT-TABLE-2         VALUE '5'.             00460003
                   88  SSR2-SORT-TABLE-3         VALUE '6'.             00470003
               10  SSR2-SEQUENCE-NUM     PICTURE S9(4) COMP.            00480003
               10  SSR2-AREA-PTR         POINTER.                       00490003
               10  SSR2-AREA-LENGTH      PICTURE S9(8) COMP.            00500003
           05  SSR2-SUB-AREA.                                           00510000
               10  SSR2-SUB-SIZE     PICTURE S9(4) COMP.                00520000
                   88  SSR2-MAX-SUB-SIZE     VALUE 4039.                00530000
               10  SSR2-SUB-REC      OCCURS 1 TO 4039 TIMES             00540000
                                     DEPENDING ON SSR2-SUB-SIZE         00550000
                                     INDEXED BY  SSR2-IDX               00560000
                                     PICTURE X.                         00570000
