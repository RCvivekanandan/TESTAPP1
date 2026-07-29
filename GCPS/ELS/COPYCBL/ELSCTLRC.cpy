      ******************************************************************00010000
      *                                                                *00020000
      *    COPYBOOK:   ELSCTLRC                                        *00030000
      *    DATE:       23-JUN-1988                                     *00040000
      *    AUTHOR:     RICK E. BARILEAU                                *00050000
      *    FUNCTION:   CONTROL RECORD LAYOUT                           *00060000
      *                                                                *00070000
      *                THIS COPY MEMBER WILL BE USED TO LOAD THE       *00080000
      *                FIELDS PARSED OFF THE CONTROL RECORD. IT        *00090000
      *                WILL INDICATE ANY ERRORS DISCOVERED.            *00100000
      *                                                                *00110000
      ******************************************************************00120000
      *                                                                *00130000
      *                      MAINTENANCE HISTORY                       *00140000
      *                                                                *00150000
      *  MOD     DATE     BY  DRPT                ACTION               *00160000
      * ----- ----------- --- ----- ---------------------------------- *00170000
      * 01.00 23-JUN-1988 REB       CREATED                            *00180000
      * 01.01 29-JUL-1988 REB       REMOVED SOME 88 LEVELS FOR ERRORS. *00190000
      * 01.02 02-AUG-1988 REB       POINTER TABLE TO 20 OCCURENCES.    *00200000
      * 01.03 27-SEP-1988 LET       1) ADDED 2 NEW 88 LEVELS IN DATA   *00210000
      *                                ELEMENT CR-RECORD-TYPE:         *00220000
      *                                   PROCESS-MULT-GROUPS AND      *00230000
      *                                   PROCESS-MULT-SECTIONS        *00240000
      *                             2) ADDED 2 NEW DATA ELEMENTS TO    *00250000
      *                                ALLOW FOR A GROUP NUMBER RANGE  *00260000
      *                                AND ALSO A SECTION NUMBER RANGE *00270000
      *                                WITHIN A GROUP.                 *00280000
      *                                                                *00290000
      * 01.04 28-SEP-1988 LET       EXPANED ERROR LIST.                *00300000
      *                                                                *00310000
      * 01.05 11-OCT-1988 LET       ADDED NOT RETRIEVED ERROR.         *00320000
      *                                                                *00321001
      * 01.06 07-AUG-1997 AKK       ADDED PLAN CODE AND INCREASED      *00322004
      *                             GROUP AND CONTRACT SIZE.  ALSO     *00323001
      *                             CHANGED DATES TO ACCOMODATE        *00324001
      *                             YEAR 2000.                         *00325001
      *                                                                *00326004
      * 01.07 14-AUG-1997 AKK       ADDED PACKAGE CODE                 *00327004
      ******************************************************************00330000
       01  CONTROL-RECORD-SUMMARY.                                      00340000
           05  CR-TRANSACTION-TYPE             PIC  X(01).              00350000
               88  ADD-FUNCTION                           VALUE 'A'.    00360000
               88  CHANGE-FUNCTION                        VALUE 'C'.    00370000
               88  DELETE-FUNCTION                        VALUE 'D'.    00380000
               88  RETRIEVE-FUNCTION                      VALUE 'R'.    00390000
           05  CR-RECORD-TYPE                  PIC  X(01).              00400000
               88  PROCESS-CONTRACT                       VALUE 'C'.    00410000
               88  PROCESS-GROUP                          VALUE 'G'.    00420000
               88  PROCESS-MESSAGE                        VALUE 'M'.    00430000
               88  PROCESS-MULT-GROUPS                    VALUE 'N'.    00440000
               88  PROCESS-MULT-SECTIONS                  VALUE 'S'.    00450000
           05  CR-PLAN-CODE                    PIC  X(03).              00460001
           05  CR-GRP-NUM.                                              00461008
               10 CR-GRP-NUM-1-2-3             PIC  X(03).              00462001
               10 CR-GROUP-NUMBER              PIC  X(06).              00463008
           05  CR-SECT-NUM.                                             00470008
               10 CR-SECT-NUM-1                PIC  X(01).              00471001
               10 CR-SECTION-NUMBER            PIC  X(04).              00472008
           05  CR-PKG-CODE                     PIC  X(03).              00473005
           05  CR-LINE-OF-BUSINESS             PIC  X(01).              00480000
           05  CR-PROVIDER-CONTROL             PIC  X(02).              00490000
           05  CR-FAMILY-RELATION              PIC  X(01).              00500000
           05  CR-EFFECTIVE-DATE               PIC  X(08).              00510008
           05  CR-JUL-EFF-DT.                                           00520008
               10 CR-JUL-EFF-DT-1CC            PIC  X.                  00521007
               10 CR-JULIAN-EFFECTIVE-DATE     PIC S9(05) COMP-3.       00522008
           05  CR-JUL-EFF-CENTURY REDEFINES                             00523001
                 CR-JUL-EFF-DT                 PIC S9(07) COMP-3.       00524008
           05  CR-TERMINATION-DATE             PIC  X(08).              00530008
           05  CR-JUL-TERM-DT.                                          00540008
               10 CR-JUL-TERM-DT-CC            PIC  X.                  00541007
               10 CR-JULIAN-TERMINATION-DATE   PIC S9(05) COMP-3.       00542008
           05  CR-JUL-TERM-CENTURY REDEFINES                            00543001
                  CR-JUL-TERM-DT    PIC S9(07) COMP-3.                  00544008
           05  CR-ACCUMULATOR-ID               PIC  X(06).              00550000
           05  CR-BENEFIT-PROVISION-ID         PIC  X(06).              00560000
           05  CR-INTERNAL-TABULAR-ID          PIC  X(06).              00570000
           05  CR-TO-GRP-NUM-RANGE.                                     00580008
               10 CR-TO-GRP-NUM-1-2-3          PIC  X(03).              00581001
               10 CR-TO-GROUP-NUMBER-RANGE     PIC  X(06).              00582008
           05  CR-TO-SECT-NUM-RANGE.                                    00590008
               10 CR-TO-SECT-NUM-1             PIC  X(01).              00591001
               10 CR-TO-SECT-NUMBER-RANGE      PIC  X(04).              00592008
           05  CR-TO-PKG-CODE-RANGE            PIC  X(03).              00593006
           05  CR-NUMBER-OF-MESSAGES           PIC S9(04)  COMP.        00600000
           05  CR-MESSAGE-ENTRY                OCCURS 20 TIMES          00610000
                                               INDEXED BY CR-MSG-IDX.   00620000
               10  CR-MESSAGE-ACTION           PIC  X(01).              00630000
                   88  ADD-MESSAGE                        VALUE '+' ' '.00640000
                   88  DELETE-MESSAGE                     VALUE '-'.    00650000
               10  CR-MESSAGE-POINTER          PIC  X(05).              00660000
           05  CR-ERROR-INDICATOR              PIC  9(02).              00670000
               88  ERROR-FREE                             VALUE 00.     00680000
               88  TRANS-TYPE-INVALID                     VALUE 01.     00690000
               88  REC-TYPE-INVALID                       VALUE 02.     00700000
               88  GRP-NBR-INVALID                        VALUE 03.     00710000
               88  SCT-NBR-INVALID                        VALUE 04.     00720000
               88  FAM-REL-INVALID                        VALUE 05.     00730000
               88  LOB-IND-INVALID                        VALUE 06.     00740000
               88  PROV-CNTL-INVALID                      VALUE 07.     00750000
               88  EFF-DATE-INVALID                       VALUE 08.     00760000
               88  TERM-DATE-INVALID                      VALUE 09.     00770000
               88  DELIMITER-INVALID                      VALUE 10.     00780000
               88  GRP-KEY-INVALID                        VALUE 11.     00790000
               88  UNIQUE-IND-INVALID                     VALUE 12.     00800000
               88  MSG-TEXT-MISSING                       VALUE 13.     00810000
               88  MSG-PTR-INVALID                        VALUE 14.     00820000
               88  GRP-NOT-FND                            VALUE 15.     00830000
               88  DUP-GRP-KEY                            VALUE 16.     00840000
               88  GRP-NOT-BLUE-CHIP                      VALUE 17.     00850000
               88  GRP-1-NOT-BLUE-CHIP                    VALUE 18.     00860000
               88  GRP-2-NOT-BLUE-CHIP                    VALUE 19.     00870000
               88  GRP-SECT-1-NOT-BLUE-CHIP               VALUE 20.     00880000
               88  GRP-SECT-2-NOT-BLUE-CHIP               VALUE 21.     00890000
               88  KEY-NOT-BLUE-CHIP                      VALUE 22.     00900000
               88  NUMBER-SEQUENCE-INVALID                VALUE 23.     00910000
               88  TRANS-RECORD-COMBO-INVALID             VALUE 24.     00920000
               88  MESSAGE-MUST-BE-RETRIEVED              VALUE 25.     00930000
