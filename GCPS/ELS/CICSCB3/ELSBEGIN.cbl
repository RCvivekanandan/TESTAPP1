00001  IDENTIFICATION DIVISION.                                         09/03/03
00002                                                                   ELSBEGIN
00003  PROGRAM-ID.         ELSBEGIN.                                       LV002
00004                                                                   ELSBEGIN
00005  AUTHOR.             LUCY TORRES                                  ELSBEGIN
00006                                                                   ELSBEGIN
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELSBEGIN
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELSBEGIN
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELSBEGIN
00010                      233 N. MICHIGAN AVE                          ELSBEGIN
00011                      CHICAGO, ILLINOIS 60601                      ELSBEGIN
00012                                                                   ELSBEGIN
00013  DATE-WRITTEN.       11-04-1986.                                  ELSBEGIN
00014                                                                   ELSBEGIN
00015  DATE-COMPILED.                                                   ELSBEGIN
00016                                                                   ELSBEGIN
00017  SECURITY.           COPYRIGHT 1986,                              ELSBEGIN
00018                      HEALTH CARE SERVICE CORPORATION              ELSBEGIN
00019      SKIP3                                                        ELSBEGIN
00020  ENVIRONMENT DIVISION.                                            ELSBEGIN
00021                                                                   ELSBEGIN
00022  CONFIGURATION SECTION.                                           ELSBEGIN
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELSBEGIN
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELSBEGIN
00025  TITLE 'ELS OPENING SCREEN PROCESSOR                      '.      ELSBEGIN
00026 ****************************************************************  ELSBEGIN
00027 *                                                              *  ELSBEGIN
00028 *    PROGRAM:    ELSBEGIN                                      *  ELSBEGIN
00029 *    DATE:       04-NOV-1986                                   *  ELSBEGIN
00030 *    AUTHOR:     LUCY TORRES                                   *  ELSBEGIN
00031 *    FUNCTION:                                                 *  ELSBEGIN
00032 *                                                              *  ELSBEGIN
00033 ****************************************************************  ELSBEGIN
00034 *                                                              *  ELSBEGIN
00035 *                   MAINTENANCE HISTORY                        *  ELSBEGIN
00036 *                                                              *  ELSBEGIN
00037 *  MOD     DATE     BY  DRPT              ACTION               *  ELSBEGIN
00038 * ----- ----------- --- ---- --------------------------------- *  ELSBEGIN
00039 * 01.00 04-NOV-1986 LET      CREATED                           *  ELSBEGIN
00040 * 01.01 17-AUG-1987 EGL      ADDED GROUP SECURITY CHECK AT THE *  ELSBEGIN
00041 *                            REQUEST OF BLUE CHIP.             *  ELSBEGIN
00042 *                                                              *  ELSBEGIN
00043 * 01.02 15-OCT-1987 REB      ADDED A MESSAGE THAT IS SENT WHEN *  ELSBEGIN
00044 *                            A CONTRACT DOES NOT EXIST FOR THE *  ELSBEGIN
00045 *                            DATE SPECIFIED. THIS IS TO HANDLE *  ELSBEGIN
00046 *                            THE 'EL42' SITUATION.             *  ELSBEGIN
00047 *                                                              *  ELSBEGIN
00048 * 01.03 19-OCT-1987 REB      MOVING LOW-VALUES TO SSB-CMDLN-DATA  ELSBEGIN
00049 *                            AFTER GETTING THE INFO FROM THERE *  ELSBEGIN
00050 *                            SO MAINLINE DOESN'T GET CONFUSED, *  ELSBEGIN
00051 *                            AND PROCESS IT AS CMDLINE AGAIN.  *  ELSBEGIN
00052 *                                                              *  ELSBEGIN
00053 * 01.04 19-OCT-1987 REB      ADDED LOGIC TO JUSTIFY RIGHT ANY  *  ELSBEGIN
00054 *                            ALPHA GROUP/SECTION ENTERED.      *  ELSBEGIN
00055 *                                                              *  ELSBEGIN
00056 * 01.05 19-OCT-1987 REB      FIXED COMMAND LINE LOGIC TO HANDLE*  ELSBEGIN
00057 *                            JUSTIFICATION.                    *  ELSBEGIN
00058 *                                                              *  ELSBEGIN
00059 * 01.06 18-DEC-1987 AKK      ADDED LOGIC TO ALLOW SUBSCRIBER   *  ELSBEGIN
00060 *                            NUMBER TO BE ADDED WITHOUT LEAD-  *  ELSBEGIN
00061 *                            ING ZEROES.                       *  ELSBEGIN
00062 * 01.07 18-JUL-1988 EGL      ADDED NEW STORAGE MANAGEMENT      *  ELSBEGIN
00063 *                            ROUTINES.                         *  ELSBEGIN
00064 *                                                              *  ELSBEGIN
00065 * 01.08 31-OCT-1988 JPB      ADDED LOGIC TO DISPLAY A MESSAGE  *  ELSBEGIN
00066 *                            IF THE SECTION IS NOT VIEWABLE.   *  ELSBEGIN
00067 *                                                              *  ELSBEGIN
00068 * 01.09 01-MAY-1989 GEM      RESOLVED DISCREPANCY P7683 (THE DIS- ELSBEGIN
00069 *                            PLAYING OF ELIQ INFO WHENEVER THERE  ELSBEGIN
00070 *                            IS 1 SECTION AND THE INTER-RELATIONALELSBEGIN
00071 *                            CODE IS ZERO FILLED).                ELSBEGIN
00072 *                                                              *  ELSBEGIN
00073 * 01.10 20-JUN-1989 EGL      1.  DESTRUCTED PROGRAM.           *  ELSBEGIN
00074 *                            2.  CORRECTED ABOVE PROBLEM AGAIN *  ELSBEGIN
00075 *                            THE FIX MADE BY GEM COVERED MANY  *  ELSBEGIN
00076 *                            BUT NOT ALL CASES OF THE INTER-   *  ELSBEGIN
00077 *                            RELATIONAL CODE OF ZERO.          *  ELSBEGIN
00078 *                                                              *  ELSBEGIN
00079 * 01.11 06-SEP-1989 AKK      ADDED LOGIC TO DISPLAY MESSAGES   *  ELSBEGIN
00080 *                            ON BEGIN SCREEN                   *  ELSBEGIN
00081 *                                                              *  ELSBEGIN
00082 * 01.12 26-OCT-1989 AKK      CHANGED ELS CODE VALUE TO 01 AT   *  ELSBEGIN
00083 *                            CYCLE LEVEL AS AN EXCEPTION. THIS *  ELSBEGIN
00084 *                            WAS DONE PER RICH.                *  ELSBEGIN
00085 *                                                              *  ELSBEGIN
00086 * 01.13 15-MAR-1990 EGL      CHANGED COMMAND LINE PROCESSING   *  ELSBEGIN
00087 *                            SO IT DOES NOT CALL MEMBERSHIP.   *  ELSBEGIN
00088 *                            THIS AVOIDS AN ASRA IN MEMBERSHIP *  ELSBEGIN
00089 *                            SINCE THE DATES WERE NOT PRESENT  *  ELSBEGIN
00090 *                            AS WELL AS ELIMINATE A CALL TO    *  ELSBEGIN
00091 *                            MEMBERSHIP.                       *  ELSBEGIN
00092 *                                                              *  ELSBEGIN
00093 * 01.14 03-MAR-1997 AKK      ADDED SUPPORT FOR YR2000 AND      *  ELSBEGIN
00094 *                            TEXAS MERGER.                     *  ELSBEGIN
00095 *                                                              *  ELSBEGIN
00096 *       14-FEB-2000 JP       ADDED CODE TO RETURN TO PRIMARY   *  ELSBEGIN
00097 *                            SCREEN WITH ERROR MESSAGES WHEN   *  ELSBEGIN
00098 *                            SECTION NOT FOUND OR SECTION      *  ELSBEGIN
00099 *                            TABLE OVERFLOW.                   *  ELSBEGIN
00100 *                                                              *  ELSBEGIN
00101 * 01.15 03-JAN-2003 AKK      ADDED CODE TO INDICATES INVALID   *  ELSBEGIN
00102 *                            GROUP NUMBER WHEN ALL ZERO GROUP  *  ELSBEGIN
00103 *                            NUMBER KEYED IN.                  *  ELSBEGIN
00104 *                                                              *  ELSBEGIN
00105 *       12-AUG-2003 AKK      RGEN'D TO TEST ORFER OF COMPILE   *  ELSBEGIN
00106 ****************************************************************  ELSBEGIN
00107 * 10/03/97 SOME GROUP AND CONTRACT FIELDS STILL ACCESS THE     *  ELSBEGIN
00108 *          6-DIGIT GROUP NUMBER AND 4-DIGIT SECTION NUMBER.    *  ELSBEGIN
00109 *                                                              *  ELSBEGIN
00110 *                                                              *  ELSBEGIN
00111 ****************************************************************  ELSBEGIN
00112      EJECT                                                        ELSBEGIN
00113  DATA DIVISION.                                                   ELSBEGIN
00114  WORKING-STORAGE SECTION.                                         ELSBEGIN
00115  01  WS-BEGIN                    PIC  X(24) VALUE                 ELSBEGIN
00116          '** ELSBEGIN WS BEGINS **'.                              ELSBEGIN
00117 ****************************************************************  ELSBEGIN
00118 *   BMS ATTRIBUTES                                             *  ELSBEGIN
00119 ****************************************************************  ELSBEGIN
00120      COPY DFHBMSCA.                                               ELSBEGIN
00121 /***************************************************************  ELSBEGIN
00122 *                  ERROR MESSAGES                              *  ELSBEGIN
00123 ****************************************************************  ELSBEGIN
00124  01  WS-ENTER-GRP-NUM.                                            ELSBEGIN
00125      05  FILLER                  PIC  X(28) VALUE                 ELSBEGIN
00126              'GROUP NUMBER MUST BE ENTERED'.                      ELSBEGIN
00127      05  FILLER                  PIC  X(51) VALUE SPACES.         ELSBEGIN
00128                                                                   ELSBEGIN
00129  01  WS-ACCESS-DENIED            PIC  X(43) VALUE                 ELSBEGIN
00130      'YOU ARE NOT AUTHORIZED TO ACCESS THIS GROUP'.               ELSBEGIN
00131                                                                   ELSBEGIN
00132  01  WS-INVALID-GRP-NUM.                                          ELSBEGIN
00133      05  FILLER                  PIC  X(20) VALUE                 ELSBEGIN
00134              'INVALID GROUP NUMBER'.                              ELSBEGIN
00135      05  FILLER                  PIC  X(59) VALUE SPACES.         ELSBEGIN
00136                                                                   ELSBEGIN
00137  01  WS-INVALID-SEC-NUM.                                          ELSBEGIN
00138      05  FILLER                  PIC  X(50) VALUE                 ELSBEGIN
00139      'SECTION NUMBER FOR THIS GROUP/SUBSCRIBER NOT FOUND'.        ELSBEGIN
00140      05  FILLER                  PIC  X(29) VALUE SPACES.         ELSBEGIN
00141                                                                   ELSBEGIN
00142  01  WS-INVALID-SUB-NUM.                                          ELSBEGIN
00143      05  FILLER                  PIC  X(25) VALUE                 ELSBEGIN
00144              'INVALID SUBSCRIBER NUMBER'.                         ELSBEGIN
00145      05  FILLER                  PIC  X(54) VALUE SPACES.         ELSBEGIN
00146                                                                   ELSBEGIN
00147  01  WS-NOT-BLUE-CHIP.                                            ELSBEGIN
00148      05  FILLER                  PIC  X(38) VALUE                 ELSBEGIN
00149              'THIS GROUP NOT COVERED UNDER BLUE CHIP'.            ELSBEGIN
00150      05  FILLER                  PIC  X(24) VALUE                 ELSBEGIN
00151              ' FOR THE REQUESTED DATES'.                          ELSBEGIN
00152      05  FILLER                  PIC  X(17) VALUE SPACES.         ELSBEGIN
00153                                                                   ELSBEGIN
00154  01  WS-INVALID-DATE.                                             ELSBEGIN
00155      05  FILLER                  PIC  X(12) VALUE                 ELSBEGIN
00156              'INVALID DATE'.                                      ELSBEGIN
00157      05  FILLER                  PIC  X(67) VALUE SPACES.         ELSBEGIN
00158                                                                   ELSBEGIN
00159  01  WS-INVALID-DATE-RANGE.                                       ELSBEGIN
00160      05  FILLER                  PIC  X(18) VALUE                 ELSBEGIN
00161              'INVALID DATE RANGE'.                                ELSBEGIN
00162      05  FILLER                  PIC  X(61) VALUE SPACES.         ELSBEGIN
00163                                                                   ELSBEGIN
00164  01  WS-NO-CONTRACT-AT-THIS-DATE.                                 ELSBEGIN
00165      05  FILLER                  PIC  X(67) VALUE                 ELSBEGIN
00166      'NO EFFECTIVE GROUP OR CONTRACT RECORD FOUND FOR THE REQUESTEELSBEGIN
00167 -    'D DATES'.                                                   ELSBEGIN
00168      05  FILLER                  PIC  X(12) VALUE SPACES.         ELSBEGIN
00169                                                                   ELSBEGIN
00170  01  WS-SECTION-NOT-VIEWABLE.                                     ELSBEGIN
00171      05  FILLER                  PIC  X(44) VALUE                 ELSBEGIN
00172      'SECTION ENTERED IS NOT VIEWABLE AT THIS TIME'.              ELSBEGIN
00173      05  FILLER                  PIC  X(35) VALUE SPACES.         ELSBEGIN
00174                                                                   ELSBEGIN
00175  01  WS-SECTION-REQUIRED.                                         ELSBEGIN
00176      05  FILLER                  PIC  X(50) VALUE                 ELSBEGIN
00177      'GROUP CONTAINS TOO MANY SECTIONS - ENTER SECTION #'.        ELSBEGIN
00178      05  FILLER                  PIC  X(29) VALUE SPACES.         ELSBEGIN
00179                                                                   ELSBEGIN
00180  01  WS-CIA-ABCODE       PIC  X(04).                              ELSBEGIN
00181      88  WS-CIA-AB-INCR-TBL-SIZE      VALUE 'EL60'.               ELSBEGIN
00182                                                                   ELSBEGIN
00183  01  WS-CIA-RETURN-CODE  PIC S9(04)  COMP.                        ELSBEGIN
00184      88  WS-CIA-RC-OK                 VALUE +0000.                ELSBEGIN
00185      88  WS-CIA-RC-NO-DDNAME          VALUE +0001.                ELSBEGIN
00186      88  WS-CIA-RC-PTR-NULL           VALUE +0002.                ELSBEGIN
00187      88  WS-CIA-RC-STG-DUP-REL        VALUE +0101.                ELSBEGIN
00188      88  WS-CIA-RC-STG-DUP-REQ        VALUE +0102.                ELSBEGIN
00189      88  WS-CIA-RC-MEMB-NON-RETN      VALUE +0201.                ELSBEGIN
00190      88  WS-CIA-RC-MEMB-NOT-IN-GRP    VALUE +0202.                ELSBEGIN
00191      88  WS-CIA-RC-MEMB-TBL-OVFL      VALUE +0203.                ELSBEGIN
00192      88  WS-CIA-RC-MEMB-GRP-NOTFND    VALUE +0204.                ELSBEGIN
00193      88  WS-CIA-RC-MEMB-NO-SECTN-INFO VALUE +0205.                ELSBEGIN
00194      88  WS-CIA-RC-KTB-GRP-NOTFND     VALUE +0301.                ELSBEGIN
00195      88  WS-CIA-RC-KTB-SECTN-NOTFND   VALUE +0302.                ELSBEGIN
00196      88  WS-CIA-RC-CONDB-UNABLE       VALUE +0311.                ELSBEGIN
00197 /***************************************************************  ELSBEGIN
00198 *                SWITCHES, WORK AREAS                          *  ELSBEGIN
00199 ****************************************************************  ELSBEGIN
00200  01  SWITCHES.                                                    ELSBEGIN
00201      05  ERROR-SW                    PIC  X(01).                  ELSBEGIN
00202          88  NO-ERROR-FOUND                     VALUE '0'.        ELSBEGIN
00203          88  ERROR-FOUND                        VALUE '1'.        ELSBEGIN
00204                                                                   ELSBEGIN
00205      05  NON-BLANK-CHAR-SW           PIC  X(01).                  ELSBEGIN
00206          88  NON-BLANK-CHAR-FOUND               VALUE 'Y'.        ELSBEGIN
00207          88  NON-BLANK-CHAR-NOT-FOUND           VALUE 'N'.        ELSBEGIN
00208                                                                   ELSBEGIN
00209      05  EMBEDDED-BLANK-SW           PIC  X(01).                  ELSBEGIN
00210          88  EMBEDDED-BLANK-FOUND               VALUE 'Y'.        ELSBEGIN
00211          88  NO-EMBEDDED-BLANK                  VALUE 'N'.        ELSBEGIN
00212                                                                   ELSBEGIN
00213      05  ALL-ZERO-ENTRY-SW           PIC  X(01).                  ELSBEGIN
00214          88  ALL-ZERO-ENTRY-FOUND               VALUE 'Y'.        ELSBEGIN
00215          88  NO-ALL-ZERO-ENTRY                  VALUE 'N'.        ELSBEGIN
00216                                                                   ELSBEGIN
00217      05  WS-NUMERIC-SW               PIC  X(01).                  ELSBEGIN
00218          88  WS-NUMERIC-FIELD                   VALUE 'Y'.        ELSBEGIN
00219          88  WS-NOT-NUMERIC-FIELD               VALUE 'N'.        ELSBEGIN
00220                                                                   ELSBEGIN
00221      05  WS-SECT-NUM-SW              PIC  X(01).                  ELSBEGIN
00222          88  SECT-NUM-NOT-ENTERED               VALUE '0'.        ELSBEGIN
00223          88  SECT-NUM-ENTERED                   VALUE '1'.        ELSBEGIN
00224                                                                   ELSBEGIN
00225      05  WS-SUB-NUM-SW               PIC  X(01).                  ELSBEGIN
00226          88  SUB-NUM-NOT-ENTERED                VALUE '0'.        ELSBEGIN
00227          88  SUB-NUM-ENTERED                    VALUE '1'.        ELSBEGIN
00228                                                                   ELSBEGIN
00229      05  START-DATE-SW               PIC  X(01) VALUE 'N'.        ELSBEGIN
00230          88  START-DATE-IN                      VALUE 'Y'.        ELSBEGIN
00231                                                                   ELSBEGIN
00232      05  END-DATE-SW                 PIC  X(01) VALUE 'N'.        ELSBEGIN
00233          88  END-DATE-IN                        VALUE 'Y'.        ELSBEGIN
00234                                                                   ELSBEGIN
00235      05  WS-GETMAIN-SW               PIC  X(01) VALUE 'N'.        ELSBEGIN
00236          88  WS-DID-GETMAIN                     VALUE 'Y'.        ELSBEGIN
00237                                                                   ELSBEGIN
00238  01  WS-START-POSN               PIC S9(04) COMP SYNC.            ELSBEGIN
00239  01  WS-MAP-SUB                  PIC S9(04) COMP.                 ELSBEGIN
00240  01  STRING-START-PTR            PIC S9(04) COMP SYNC.            ELSBEGIN
00241  01  STRING-END-PTR              PIC S9(04) COMP SYNC.            ELSBEGIN
00242  01  INPUT-LEN                   PIC S9(04) COMP SYNC.            ELSBEGIN
00243  01  INPUTL                      PIC S9(04) COMP SYNC.            ELSBEGIN
00244                                                                   ELSBEGIN
00245  01  WS-INPUT-FIELD.                                              ELSBEGIN
00246      05  WS-INPUT-CHAR           OCCURS 12 TIMES                  ELSBEGIN
00247                                  INDEXED BY INPUT-IDX             ELSBEGIN
00248                                  PIC  X(01).                      ELSBEGIN
00249                                                                   ELSBEGIN
00250  01  WS-TEMP-INPUT               PIC  X(12).                      ELSBEGIN
00251  01  WS-TEMP-INPUT-RED  REDEFINES  WS-TEMP-INPUT.                 ELSBEGIN
00252      05  WS-TEMP-INPUT-CHAR      OCCURS 12 TIMES                  ELSBEGIN
00253                                  INDEXED BY TEMP-INPUT-IDX        ELSBEGIN
00254                                  PIC  X(01).                      ELSBEGIN
00255  01  WS-TEMP-GRP  REDEFINES  WS-TEMP-INPUT                        ELSBEGIN
00256                                  PIC  X(06).                      ELSBEGIN
00257  01  WS-TEMP-GRP-9  REDEFINES  WS-TEMP-INPUT                      ELSBEGIN
00258                                  PIC  X(09).                      ELSBEGIN
00259  01  WS-TEMP-SECT  REDEFINES  WS-TEMP-INPUT                       ELSBEGIN
00260                                  PIC  X(04).                      ELSBEGIN
00261  01  WS-TEMP-SECT-5  REDEFINES  WS-TEMP-INPUT                     ELSBEGIN
00262                                  PIC  X(05).                      ELSBEGIN
00263                                                                   ELSBEGIN
00264  01  WS-HOLD-GRP-9.                                               ELSBEGIN
00265      05  WS-HOLD-GRP-1-3         PIC X(03).                       ELSBEGIN
00266      05  WS-HOLD-GRP-6           PIC X(06).                       ELSBEGIN
00267                                                                   ELSBEGIN
00268  01  WS-HOLD-SECT-5.                                              ELSBEGIN
00269      05  WS-HOLD-SECT-1          PIC X(01).                       ELSBEGIN
00270      05  WS-HOLD-SECT-4          PIC X(04).                       ELSBEGIN
00271                                                                   ELSBEGIN
00272  01  WS-TEMP-GRP-NO.                                              ELSBEGIN
00273      05  WS-TEMP-GRP-CHAR        OCCURS 9 TIMES                   ELSBEGIN
00274                                  INDEXED BY TEMP-GRP-IDX          ELSBEGIN
00275                                  PIC  X(01).                      ELSBEGIN
00276                                                                   ELSBEGIN
00277  01  WS-GROUP-NO.                                                 ELSBEGIN
00278      05  WS-GROUP-CHAR           OCCURS 9 TIMES                   ELSBEGIN
00279                                  INDEXED BY GRP-IDX               ELSBEGIN
00280                                  PIC  X(01).                      ELSBEGIN
00281                                                                   ELSBEGIN
00282  01  WS-SECT-NO.                                                  ELSBEGIN
00283      05  WS-SECT-CHAR            OCCURS 5 TIMES                   ELSBEGIN
00284                                  INDEXED BY SECT-IDX              ELSBEGIN
00285                                  PIC  X(01).                      ELSBEGIN
00286                                                                   ELSBEGIN
00287  01  WS-SUBSC-NO.                                                 ELSBEGIN
00288      05  WS-SUBSC-CHAR           OCCURS 12 TIMES                  ELSBEGIN
00289                                  INDEXED BY SUBSC-IDX             ELSBEGIN
00290                                  PIC  X(01).                      ELSBEGIN
00291                                                                   ELSBEGIN
00292  01  WS-EIBDATE-AREA.                                             ELSBEGIN
00293      05  WS-EIBDATE.                                              ELSBEGIN
00294          10  WS-EIBDATE-CC.                                       ELSBEGIN
00295              15  WS-EIBDATE-1    PIC 9.                           ELSBEGIN
00296              15  WS-EIBDATE-2    PIC 9.                           ELSBEGIN
00297          10  WS-EIBDATE-DT       PIC 9(5).                        ELSBEGIN
00298      05  WS-EIBDATE-CEN REDEFINES                                 ELSBEGIN
00299          WS-EIBDATE              PIC 9(7).                        ELSBEGIN
00300                                                                   ELSBEGIN
00301  01  WS-DATE.                                                     ELSBEGIN
00302      05  WS-1-2                  PIC  9(02) VALUE ZEROS.          ELSBEGIN
00303      05  WS-YYDDD                PIC  9(05) VALUE ZEROS.          ELSBEGIN
00304                                                                   ELSBEGIN
00305  01  WS-DATE-OUT                 PIC  99/99/99.                   ELSBEGIN
00306                                                                   ELSBEGIN
00307  01  WS-TIME                     PIC  9(07) VALUE ZEROS.          ELSBEGIN
00308  01  WS-TIME-RED  REDEFINES  WS-TIME.                             ELSBEGIN
00309      05  WS-1                    PIC  9(01).                      ELSBEGIN
00310      05  WS-HH                   PIC  9(02).                      ELSBEGIN
00311      05  WS-MM                   PIC  9(02).                      ELSBEGIN
00312      05  WS-SS                   PIC  9(02).                      ELSBEGIN
00313                                                                   ELSBEGIN
00314  01  WS-TIME-OUT.                                                 ELSBEGIN
00315      05  WS-HH-OUT               PIC  9(02).                      ELSBEGIN
00316      05  FILLER                  PIC  X(01) VALUE ':'.            ELSBEGIN
00317      05  WS-MM-OUT               PIC  9(02).                      ELSBEGIN
00318      05  FILLER                  PIC  X(01) VALUE ':'.            ELSBEGIN
00319      05  WS-SS-OUT               PIC  9(02).                      ELSBEGIN
00320 /***************************************************************  ELSBEGIN
00321 *      P R O G R A M    C O N S T A N T S                      *  ELSBEGIN
00322 ****************************************************************  ELSBEGIN
00323  01  PROGRAM-CONSTANTS.                                           ELSBEGIN
00324      05  MSG-RECORD-PREFIX       PIC X(06)  VALUE                 ELSBEGIN
00325          '@ELS  '.                                                ELSBEGIN
00326                                                                   ELSBEGIN
00327      05  MSG-ELEMENT-SYSTEM-NAME PIC X(16)  VALUE                 ELSBEGIN
00328          'ELS-MESSAGE-AREA'.                                      ELSBEGIN
00329                                                                   ELSBEGIN
00330      05  MSG-CODE-VALUE          PIC X(02)  VALUE '01'.           ELSBEGIN
00331                                                                   ELSBEGIN
00332 *HEX VALUES                                                       ELSBEGIN
00333      COPY HEXCOBOL.                                               ELSBEGIN
00334                                                                   ELSBEGIN
00335 /***************************************************************  ELSBEGIN
00336 *      H G A D A T E S   P A R M   L I S T                     *  ELSBEGIN
00337 ****************************************************************  ELSBEGIN
00338  01  HGADATES-PARM-LIST.                                          ELSBEGIN
00339      COPY HGCDAT01.                                               ELSBEGIN
00340 /***************************************************************  ELSBEGIN
00341 *      M L D A T E S   P A R M   L I S T                     *    ELSBEGIN
00342 ****************************************************************  ELSBEGIN
00343      COPY MLDATE01.                                               ELSBEGIN
00344 /                                                                 ELSBEGIN
00345 ****************************************************************  ELSBEGIN
00346 *      G R O U P   S E C U R I T Y   C H E C K   P A R M       *  ELSBEGIN
00347 ****************************************************************  ELSBEGIN
00348  01  CSCOMSEC-PARM.                                               ELSBEGIN
00349      COPY COBSECDS.                                               ELSBEGIN
00350 /                                                                 ELSBEGIN
00351 ****************************************************************  ELSBEGIN
00352 *  M A P   C O B O L   S C R E E N   D S E C T                 *  ELSBEGIN
00353 ****************************************************************  ELSBEGIN
00354  01  WS-IO-MAP-AREA-01           PIC  X(24) VALUE                 ELSBEGIN
00355          '*WS MAP I/O AREA EL01 *'.                               ELSBEGIN
00356      COPY EL01SETC.                                               ELSBEGIN
00357  01  WS-IO-MAP-AREA-00           PIC  X(24) VALUE                 ELSBEGIN
00358          '*WS MAP I/O AREA EL00 *'.                               ELSBEGIN
00359 /                                                                 ELSBEGIN
00360      COPY EL00SETC.                                               ELSBEGIN
00361 /                                                                 ELSBEGIN
00362  LINKAGE SECTION.                                                 ELSBEGIN
00363  01  DFHCOMMAREA.                                                 ELSBEGIN
00364  COPY ELSCOMMC.                                                   ELSBEGIN
00365      EJECT                                                        ELSBEGIN
00366  COPY ELSCIA2C.                                                   ELSBEGIN
00367      EJECT                                                        ELSBEGIN
00368  COPY ELSCMDSC.                                                   ELSBEGIN
00369      EJECT                                                        ELSBEGIN
00370  COPY ELSCMIFC.                                                   ELSBEGIN
00371      EJECT                                                        ELSBEGIN
00372  COPY ELSSSCBC.                                                   ELSBEGIN
00373      EJECT                                                        ELSBEGIN
00374  COPY ELSMEMSC.                                                   ELSBEGIN
00375      EJECT                                                        ELSBEGIN
00376  COPY ELSKTBSC.                                                   ELSBEGIN
00377      EJECT                                                        ELSBEGIN
00378  PROCEDURE DIVISION.                                              ELSBEGIN
00379 ************************************************************      ELSBEGIN
00380 *                                                          *      ELSBEGIN
00381 *        MAIN SCREEN                                       *      ELSBEGIN
00382 *                                                          *      ELSBEGIN
00383 ************************************************************      ELSBEGIN
00384  MAIN-SCREEN.                                                     ELSBEGIN
00385      PERFORM MAIN-SCREEN-INITIALIZE.                              ELSBEGIN
00386      PERFORM MAIN-SCREEN-PROCESS.                                 ELSBEGIN
00387      PERFORM MAIN-SCREEN-TERMINATE.                               ELSBEGIN
00388                                                                   ELSBEGIN
00389                                                                   ELSBEGIN
00390 ************************************************************      ELSBEGIN
00391 *                                                          *      ELSBEGIN
00392 *        MAIN SCREEN.INITIALIZE                            *      ELSBEGIN
00393 *                                                          *      ELSBEGIN
00394 ************************************************************      ELSBEGIN
00395  MAIN-SCREEN-INITIALIZE.                                          ELSBEGIN
00396      PERFORM CHECK-COMMAREA-LENGTH.                               ELSBEGIN
00397      PERFORM ESTABLISH-ADDRESSING-TO-COMMON.                      ELSBEGIN
00398      PERFORM ESTABLISH-ADDRESSING-TO-SELECT.                      ELSBEGIN
00399      PERFORM ESTABLISH-ADDRESSING-TO-CODES.                       ELSBEGIN
00400      MOVE SPACES          TO ERRMSGO.                             ELSBEGIN
00401      SET NO-ERROR-FOUND   TO TRUE.                                ELSBEGIN
00402      PERFORM VARYING WS-MAP-SUB FROM 1 BY 1 UNTIL                 ELSBEGIN
00403         WS-MAP-SUB > 5                                            ELSBEGIN
00404         MOVE LOW-VALUES TO MSGO (WS-MAP-SUB)                      ELSBEGIN
00405      END-PERFORM.                                                 ELSBEGIN
00406      IF NOT SSB-REPROCESS (SSB-SELECTOR-STATE)                    ELSBEGIN
00407          PERFORM INITIALIZE-GROUP-SECTION-SUBSC.                  ELSBEGIN
00408                                                                   ELSBEGIN
00409                                                                   ELSBEGIN
00410 ************************************************************      ELSBEGIN
00411 *                                                          *      ELSBEGIN
00412 *        INITIALIZE GROUP SECTION SUBSCRIBER               *      ELSBEGIN
00413 *                                                          *      ELSBEGIN
00414 ************************************************************      ELSBEGIN
00415  INITIALIZE-GROUP-SECTION-SUBSC.                                  ELSBEGIN
00416      MOVE LOW-VALUES      TO SSB-GROUP-NUMBER                     ELSBEGIN
00417                              SSB-SECTN-NO                         ELSBEGIN
00418                              SSB-PLAN-CODE                        ELSBEGIN
00419                              SSB-PKG-CODE.                        ELSBEGIN
00420      MOVE LOW-VALUES      TO SSB-SUBSCRIBER-NBR.                  ELSBEGIN
00421                                                                   ELSBEGIN
00422                                                                   ELSBEGIN
00423 ************************************************************      ELSBEGIN
00424 *                                                          *      ELSBEGIN
00425 *        CHECK COMMAREA LENGTH                             *      ELSBEGIN
00426 *                                                          *      ELSBEGIN
00427 ************************************************************      ELSBEGIN
00428  CHECK-COMMAREA-LENGTH.                                           ELSBEGIN
00429      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELSBEGIN
00430          PERFORM SIGNAL-COMMAREA-LENGTH-ERROR.                    ELSBEGIN
00431                                                                   ELSBEGIN
00432                                                                   ELSBEGIN
00433 ************************************************************      ELSBEGIN
00434 *                                                          *      ELSBEGIN
00435 *        SIGNAL COMMAREA LENGTH ERROR                      *      ELSBEGIN
00436 *                                                          *      ELSBEGIN
00437 ************************************************************      ELSBEGIN
00438  SIGNAL-COMMAREA-LENGTH-ERROR.                                    ELSBEGIN
00439      EXEC CICS ABEND ABCODE ('EL01') END-EXEC.                    ELSBEGIN
00440      EJECT                                                        ELSBEGIN
00441 ************************************************************      ELSBEGIN
00442 *                                                          *      ELSBEGIN
00443 *        ESTABLISH ADDRESSING TO COMMON INTERFACE AREA     *      ELSBEGIN
00444 *                                                          *      ELSBEGIN
00445 ************************************************************      ELSBEGIN
00446  ESTABLISH-ADDRESSING-TO-COMMON.                                  ELSBEGIN
00447      CALL 'ELUINISM' USING DFHCOMMAREA                            ELSBEGIN
00448                 ADDRESS OF                                        ELSBEGIN
00449          CIA-ELS-COMMON-INTERFACE-AREA.                           ELSBEGIN
00450      IF ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA = NULL           ELSBEGIN
00451          PERFORM SIGNAL-CIA-ADDRESSING-ERROR.                     ELSBEGIN
00452                                                                   ELSBEGIN
00453                                                                   ELSBEGIN
00454 ************************************************************      ELSBEGIN
00455 *                                                          *      ELSBEGIN
00456 *        SIGNAL CIA ADDRESSING ERROR                       *      ELSBEGIN
00457 *                                                          *      ELSBEGIN
00458 ************************************************************      ELSBEGIN
00459  SIGNAL-CIA-ADDRESSING-ERROR.                                     ELSBEGIN
00460      EXEC CICS ABEND ABCODE ('EL02') END-EXEC.                    ELSBEGIN
00461                                                                   ELSBEGIN
00462                                                                   ELSBEGIN
00463 ************************************************************      ELSBEGIN
00464 *                                                          *      ELSBEGIN
00465 *        ESTABLISH ADDRESSING TO SELECTOR STATUS CONTROL BL*      ELSBEGIN
00466 *                                                          *      ELSBEGIN
00467 ************************************************************      ELSBEGIN
00468  ESTABLISH-ADDRESSING-TO-SELECT.                                  ELSBEGIN
00469      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELSBEGIN
00470      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSBEGIN
00471          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELSBEGIN
00472      IF NOT CIA-RC-OK                                             ELSBEGIN
00473          PERFORM SIGNAL-SSCB-ADDRESSING-ERROR.                    ELSBEGIN
00474                                                                   ELSBEGIN
00475                                                                   ELSBEGIN
00476 ************************************************************      ELSBEGIN
00477 *                                                          *      ELSBEGIN
00478 *        ESTABLISH ADDRESSING TO CODES MANUAL              *      ELSBEGIN
00479 *                                                          *      ELSBEGIN
00480 ************************************************************      ELSBEGIN
00481  ESTABLISH-ADDRESSING-TO-CODES.                                   ELSBEGIN
00482      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELSBEGIN
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSBEGIN
00484          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELSBEGIN
00485      IF NOT CIA-RC-OK                                             ELSBEGIN
00486          PERFORM DO-GETMAIN.                                      ELSBEGIN
00487      IF WS-DID-GETMAIN                                            ELSBEGIN
00488         SET CIA-ELSCMIF-DDN TO TRUE                               ELSBEGIN
00489         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELSBEGIN
00490             ADDRESS OF CMF-CODES-MANUAL-INTERFACE                 ELSBEGIN
00491         IF NOT CIA-RC-OK                                          ELSBEGIN
00492            EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.          ELSBEGIN
00493                                                                   ELSBEGIN
00494 ************************************************************      ELSBEGIN
00495 *                                                          *      ELSBEGIN
00496 *        SIGNAL SSCB ADDRESSING ERROR                      *      ELSBEGIN
00497 *                                                          *      ELSBEGIN
00498 ************************************************************      ELSBEGIN
00499  SIGNAL-SSCB-ADDRESSING-ERROR.                                    ELSBEGIN
00500      SET CIA-AB-PARM-MISSING TO TRUE.                             ELSBEGIN
00501      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELSBEGIN
00502      EJECT                                                        ELSBEGIN
00503 ************************************************************      ELSBEGIN
00504 *                                                          *      ELSBEGIN
00505 *        MAIN SCREEN.PROCESS                               *      ELSBEGIN
00506 *                                                          *      ELSBEGIN
00507 ************************************************************      ELSBEGIN
00508  MAIN-SCREEN-PROCESS.                                             ELSBEGIN
00509      IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                     ELSBEGIN
00510          PERFORM INITIAL-CALL                                     ELSBEGIN
00511      ELSE IF    SSB-PRIMARY-SCREEN-OUT (SSB-SELECTOR-STATE)       ELSBEGIN
00512                   OR SSB-SECONDARY-SCREEN-OUT                     ELSBEGIN
00513          (SSB-SELECTOR-STATE)                                     ELSBEGIN
00514          PERFORM PRIMARY-OR-SECONDARY-SCREEN                      ELSBEGIN
00515      ELSE IF SSB-CMDLN-INPUT (SSB-SELECTOR-STATE)                 ELSBEGIN
00516          PERFORM COMMAND-LINE-INPUT                               ELSBEGIN
00517      ELSE IF SSB-REPROCESS (SSB-SELECTOR-STATE)                   ELSBEGIN
00518          PERFORM NO-CONTRACT-FOUND-REESTABLISHX                   ELSBEGIN
00519      ELSE                                                         ELSBEGIN
00520          PERFORM SIGNAL-INVALID-SELECTOR-STATUS.                  ELSBEGIN
00521      EJECT                                                        ELSBEGIN
00522 ************************************************************      ELSBEGIN
00523 *                                                          *      ELSBEGIN
00524 *        NO CONTRACT FOUND REESTABLISH INFORMATION FOR MAP *      ELSBEGIN
00525 *                                                          *      ELSBEGIN
00526 ************************************************************      ELSBEGIN
00527  NO-CONTRACT-FOUND-REESTABLISHX.                                  ELSBEGIN
00528      MOVE SSB-GRP-NO           TO GROUPO.                         ELSBEGIN
00529      MOVE SSB-SECT-NO         TO SECTNO.                          ELSBEGIN
00530      MOVE SSB-SUBSCRIBER-NBR   TO MEMBERO.                        ELSBEGIN
00531      PERFORM REINSTATE-THE-SERVICE-FROM-DAT.                      ELSBEGIN
00532      PERFORM REINSTATE-THE-SERVICE-TO-DATEX.                      ELSBEGIN
00533      MOVE -1                          TO FROMDTL.                 ELSBEGIN
00534      MOVE WS-NO-CONTRACT-AT-THIS-DATE TO ERRMSGO.                 ELSBEGIN
00535      PERFORM SEND-ERROR-MESSAGE.                                  ELSBEGIN
00536                                                                   ELSBEGIN
00537                                                                   ELSBEGIN
00538 ************************************************************      ELSBEGIN
00539 *                                                          *      ELSBEGIN
00540 *        REINSTATE THE SERVICE FROM DATE TO MAP            *      ELSBEGIN
00541 *                                                          *      ELSBEGIN
00542 ************************************************************      ELSBEGIN
00543  REINSTATE-THE-SERVICE-FROM-DAT.                                  ELSBEGIN
00544      MOVE SSB-SRV-FROM-DATE    TO HGADATE-JULIAN1.                ELSBEGIN
00545      PERFORM CONVERT-DATE-FROM-JULIAN-TO-GR.                      ELSBEGIN
00546      MOVE HGADATE-DATE2        TO FROMDTO.                        ELSBEGIN
00547                                                                   ELSBEGIN
00548                                                                   ELSBEGIN
00549 ************************************************************      ELSBEGIN
00550 *                                                          *      ELSBEGIN
00551 *        REINSTATE THE SERVICE TO DATE TO MAP              *      ELSBEGIN
00552 *                                                          *      ELSBEGIN
00553 ************************************************************      ELSBEGIN
00554  REINSTATE-THE-SERVICE-TO-DATEX.                                  ELSBEGIN
00555      MOVE SSB-SRV-TO-DATE      TO HGADATE-JULIAN1.                ELSBEGIN
00556      PERFORM CONVERT-DATE-FROM-JULIAN-TO-GR.                      ELSBEGIN
00557      MOVE HGADATE-DATE2        TO TODTO.                          ELSBEGIN
00558                                                                   ELSBEGIN
00559                                                                   ELSBEGIN
00560 ************************************************************      ELSBEGIN
00561 *                                                          *      ELSBEGIN
00562 *        INITIAL CALL                                      *      ELSBEGIN
00563 *                                                          *      ELSBEGIN
00564 ************************************************************      ELSBEGIN
00565  INITIAL-CALL.                                                    ELSBEGIN
00566      MOVE LOW-VALUES  TO  EL00MAPO, EL01MAPO.                     ELSBEGIN
00567      MOVE -1          TO  GROUPL.                                 ELSBEGIN
00568      PERFORM SEND-PRIMARY-SCREEN.                                 ELSBEGIN
00569      EJECT                                                        ELSBEGIN
00570 ************************************************************      ELSBEGIN
00571 *                                                          *      ELSBEGIN
00572 *        PRIMARY OR SECONDARY SCREEN                       *      ELSBEGIN
00573 *                                                          *      ELSBEGIN
00574 ************************************************************      ELSBEGIN
00575  PRIMARY-OR-SECONDARY-SCREEN.                                     ELSBEGIN
00576      PERFORM RECEIVE-PRIMARY-SCREEN.                              ELSBEGIN
00577      PERFORM EXTRACT-INITIAL-INQUIRY-PARAME.                      ELSBEGIN
00578      IF    ERROR-FOUND                                            ELSBEGIN
00579          PERFORM SEND-ERROR-MESSAGE                               ELSBEGIN
00580      ELSE                                                         ELSBEGIN
00581          PERFORM CONTINUE-PROCESSING.                             ELSBEGIN
00582                                                                   ELSBEGIN
00583                                                                   ELSBEGIN
00584 ************************************************************      ELSBEGIN
00585 *                                                          *      ELSBEGIN
00586 *        RECEIVE PRIMARY SCREEN                            *      ELSBEGIN
00587 *                                                          *      ELSBEGIN
00588 ************************************************************      ELSBEGIN
00589  RECEIVE-PRIMARY-SCREEN.                                          ELSBEGIN
00590      EXEC CICS IGNORE CONDITION MAPFAIL END-EXEC.                 ELSBEGIN
00591      EXEC CICS RECEIVE MAP ('EL01MAP')                            ELSBEGIN
00592                        MAPSET ('EL01SET')                         ELSBEGIN
00593                        INTO (EL01MAPI)                            ELSBEGIN
00594                        END-EXEC.                                  ELSBEGIN
00595      EXEC CICS HANDLE CONDITION MAPFAIL END-EXEC.                 ELSBEGIN
00596      IF EIBRCODE IS NOT EQUAL TO LOW-VALUES                       ELSBEGIN
00597          PERFORM SIGNAL-TERMINAL-OR-MAPPING-ERR.                  ELSBEGIN
00598      EJECT                                                        ELSBEGIN
00599 ************************************************************      ELSBEGIN
00600 *                                                          *      ELSBEGIN
00601 *        SEND PRIMARY SCREEN                               *      ELSBEGIN
00602 *                                                          *      ELSBEGIN
00603 ************************************************************      ELSBEGIN
00604  SEND-PRIMARY-SCREEN.                                             ELSBEGIN
00605      MOVE 'ELIQ010'   TO  MAPID1O.                                ELSBEGIN
00606      PERFORM OBTAIN-CURRENT-DATE.                                 ELSBEGIN
00607      PERFORM OBTAIN-CURRENT-TIME.                                 ELSBEGIN
00608      PERFORM CHECK-FOR-ELS-MESSAGES.                              ELSBEGIN
00609      SET SSB-PRIMARY-SCREEN-OUT (SSB-SELECTOR-STATE) TO           ELSBEGIN
00610          TRUE.                                                    ELSBEGIN
00611      EXEC CICS SEND MAP ('EL01MAP')                               ELSBEGIN
00612                     MAPSET ('EL01SET')                            ELSBEGIN
00613                     FROM (EL01MAPO)                               ELSBEGIN
00614                     ERASE                                         ELSBEGIN
00615                     CURSOR                                        ELSBEGIN
00616                     END-EXEC.                                     ELSBEGIN
00617      EJECT                                                        ELSBEGIN
00618 ************************************************************      ELSBEGIN
00619 *                                                          *      ELSBEGIN
00620 *        SEND ERROR MESSAGE                                *      ELSBEGIN
00621 *                                                          *      ELSBEGIN
00622 ************************************************************      ELSBEGIN
00623  SEND-ERROR-MESSAGE.                                              ELSBEGIN
00624      PERFORM OBTAIN-CURRENT-DATE.                                 ELSBEGIN
00625      PERFORM OBTAIN-CURRENT-TIME.                                 ELSBEGIN
00626      MOVE 'ELIQ010'   TO  MAPID1O.                                ELSBEGIN
00627      MOVE DFHBMUNF TO GROUPA, SECTNA, MEMBERA, FROMDTA,           ELSBEGIN
00628          TODTA.                                                   ELSBEGIN
00629      IF SSB-CMDLN-INPUT (SSB-SELECTOR-STATE) OR                   ELSBEGIN
00630                  SSB-REPROCESS (SSB-SELECTOR-STATE)               ELSBEGIN
00631          PERFORM SEND-WITH-ERASE                                  ELSBEGIN
00632      ELSE                                                         ELSBEGIN
00633          PERFORM SEND-WITHOUT-ERASE.                              ELSBEGIN
00634      SET SSB-SECONDARY-SCREEN-OUT (SSB-SELECTOR-STATE) TO         ELSBEGIN
00635          TRUE.                                                    ELSBEGIN
00636      EXEC CICS SEND MAP ('EL01MAP')                               ELSBEGIN
00637                     MAPSET ('EL01SET')                            ELSBEGIN
00638                     CURSOR                                        ELSBEGIN
00639                     FROM (EL01MAPO)                               ELSBEGIN
00640                     END-EXEC.                                     ELSBEGIN
00641      EJECT                                                        ELSBEGIN
00642 ************************************************************      ELSBEGIN
00643 *                                                          *      ELSBEGIN
00644 *        SEND WITHOUT ERASE                                *      ELSBEGIN
00645 *                                                          *      ELSBEGIN
00646 ************************************************************      ELSBEGIN
00647  SEND-WITHOUT-ERASE.                                              ELSBEGIN
00648      EXEC CICS SEND MAP ('EL00MAP')                               ELSBEGIN
00649                     MAPSET ('EL00SET')                            ELSBEGIN
00650                     FROM (EL00MAPO)                               ELSBEGIN
00651                     END-EXEC.                                     ELSBEGIN
00652                                                                   ELSBEGIN
00653                                                                   ELSBEGIN
00654 ************************************************************      ELSBEGIN
00655 *                                                          *      ELSBEGIN
00656 *        SEND WITH ERASE                                   *      ELSBEGIN
00657 *                                                          *      ELSBEGIN
00658 ************************************************************      ELSBEGIN
00659  SEND-WITH-ERASE.                                                 ELSBEGIN
00660      EXEC CICS SEND MAP ('EL00MAP')                               ELSBEGIN
00661                     MAPSET ('EL00SET')                            ELSBEGIN
00662                     FROM (EL00MAPO)                               ELSBEGIN
00663                     ERASE                                         ELSBEGIN
00664                     END-EXEC.                                     ELSBEGIN
00665      EJECT                                                        ELSBEGIN
00666 ************************************************************      ELSBEGIN
00667 *                                                          *      ELSBEGIN
00668 *        EXTRACT INITIAL INQUIRY PARAMETERS                *      ELSBEGIN
00669 *                                                          *      ELSBEGIN
00670 ************************************************************      ELSBEGIN
00671  EXTRACT-INITIAL-INQUIRY-PARAME.                                  ELSBEGIN
00672      IF GROUPL > 0                                                ELSBEGIN
00673          PERFORM EXTRACT-GROUP-NUMBER-FROM-MAP                    ELSBEGIN
00674          PERFORM DETERMINE-PLAN-CODE                              ELSBEGIN
00675      ELSE                                                         ELSBEGIN
00676          PERFORM INDICATE-MISSING-GROUP-NUMBER.                   ELSBEGIN
00677      IF NO-ERROR-FOUND                                            ELSBEGIN
00678          PERFORM EXTRACT-REMAINING-INQUIRY-PARA.                  ELSBEGIN
00679                                                                   ELSBEGIN
00680                                                                   ELSBEGIN
00681 ************************************************************      ELSBEGIN
00682 *                                                          *      ELSBEGIN
00683 *        EXTRACT GROUP NUMBER FROM MAP                     *      ELSBEGIN
00684 *                                                          *      ELSBEGIN
00685 ************************************************************      ELSBEGIN
00686  EXTRACT-GROUP-NUMBER-FROM-MAP.                                   ELSBEGIN
00687      MOVE GROUPI  TO  WS-GROUP-NO, WS-INPUT-FIELD.                ELSBEGIN
00688      MOVE GROUPL  TO  INPUTL.                                     ELSBEGIN
00689      PERFORM INTERGATE-AND-FORMAT-GROUP-NUM.                      ELSBEGIN
00690      EJECT                                                        ELSBEGIN
00691 ************************************************************      ELSBEGIN
00692 *                                                          *      ELSBEGIN
00693 *        DETERMINE PLAN CODE                               *      ELSBEGIN
00694 * THIS PARA WILL EVENTUALLY POPULATE THE PLAN CODE.        *      ELSBEGIN
00695 ************************************************************      ELSBEGIN
00696  DETERMINE-PLAN-CODE.                                             ELSBEGIN
00697      MOVE ZEROES TO SSB-PLAN-CODE.                                ELSBEGIN
00698 *                   SSB-PKG-CODE.                                 ELSBEGIN
00699                                                                   ELSBEGIN
00700 ************************************************************      ELSBEGIN
00701 *                                                          *      ELSBEGIN
00702 *        CHECK FOR TRAILING BLANKS                         *      ELSBEGIN
00703 *                                                          *      ELSBEGIN
00704 ************************************************************      ELSBEGIN
00705  CHECK-FOR-TRAILING-BLANKS.                                       ELSBEGIN
00706      SET NON-BLANK-CHAR-NOT-FOUND  TO  TRUE.                      ELSBEGIN
00707      PERFORM FIND-NON-BLANK-CHARACTER                             ELSBEGIN
00708          VARYING INPUT-IDX FROM  INPUTL  BY  -1                   ELSBEGIN
00709                                   UNTIL INPUT-IDX = 0             ELSBEGIN
00710                                   OR                              ELSBEGIN
00711              NON-BLANK-CHAR-FOUND.                                ELSBEGIN
00712                                                                   ELSBEGIN
00713                                                                   ELSBEGIN
00714 ************************************************************      ELSBEGIN
00715 *                                                          *      ELSBEGIN
00716 *        CHECK FOR LEADING BLANKS                          *      ELSBEGIN
00717 *                                                          *      ELSBEGIN
00718 ************************************************************      ELSBEGIN
00719  CHECK-FOR-LEADING-BLANKS.                                        ELSBEGIN
00720      SET NON-BLANK-CHAR-NOT-FOUND  TO  TRUE.                      ELSBEGIN
00721      PERFORM FIND-NON-BLANK-CHARACTER                             ELSBEGIN
00722          VARYING INPUT-IDX FROM  1  BY  1                         ELSBEGIN
00723                                   UNTIL INPUT-IDX >               ELSBEGIN
00724              STRING-END-PTR                                       ELSBEGIN
00725                                   OR                              ELSBEGIN
00726              NON-BLANK-CHAR-FOUND.                                ELSBEGIN
00727      IF NO-ERROR-FOUND                                            ELSBEGIN
00728          PERFORM SET-START-STRING-POINTER.                        ELSBEGIN
00729      EJECT                                                        ELSBEGIN
00730 ************************************************************      ELSBEGIN
00731 *                                                          *      ELSBEGIN
00732 *        CHECK FOR EMBEDDED BLANKS                         *      ELSBEGIN
00733 *                                                          *      ELSBEGIN
00734 ************************************************************      ELSBEGIN
00735  CHECK-FOR-EMBEDDED-BLANKS.                                       ELSBEGIN
00736      IF WS-INPUT-CHAR (INPUT-IDX) = SPACE  OR                     ELSBEGIN
00737          LOW-VALUE                                                ELSBEGIN
00738          PERFORM INDICATE-AN-EMBEDDED-BLANK-FOU.                  ELSBEGIN
00739                                                                   ELSBEGIN
00740  CHECK-FOR-ALL-ZERO-ENTRIES.                                      ELSBEGIN
00741      IF WS-INPUT-CHAR (INPUT-IDX) = ZERO OR                       ELSBEGIN
00742          LOW-VALUE                                                ELSBEGIN
00743          PERFORM INDICATE-A-ZERO-ENTRY-FOUND                      ELSBEGIN
00744      ELSE                                                         ELSBEGIN
00745          SET NO-ALL-ZERO-ENTRY TO TRUE                            ELSBEGIN
00746      END-IF.                                                      ELSBEGIN
00747                                                                   ELSBEGIN
00748 ************************************************************      ELSBEGIN
00749 *                                                          *      ELSBEGIN
00750 *        FIND NON-BLANK CHARACTER                          *      ELSBEGIN
00751 *                                                          *      ELSBEGIN
00752 ************************************************************      ELSBEGIN
00753  FIND-NON-BLANK-CHARACTER.                                        ELSBEGIN
00754      IF WS-INPUT-CHAR (INPUT-IDX) NOT = SPACE AND                 ELSBEGIN
00755          LOW-VALUE                                                ELSBEGIN
00756          PERFORM INDICATE-A-NON-BLANK-CHARACTER.                  ELSBEGIN
00757                                                                   ELSBEGIN
00758                                                                   ELSBEGIN
00759 ************************************************************      ELSBEGIN
00760 *                                                          *      ELSBEGIN
00761 *        SET END STRING POINTER                            *      ELSBEGIN
00762 *                                                          *      ELSBEGIN
00763 ************************************************************      ELSBEGIN
00764  SET-END-STRING-POINTER.                                          ELSBEGIN
00765      SET INPUT-IDX       UP  BY  +1.                              ELSBEGIN
00766      SET STRING-END-PTR  TO  INPUT-IDX.                           ELSBEGIN
00767                                                                   ELSBEGIN
00768                                                                   ELSBEGIN
00769 ************************************************************      ELSBEGIN
00770 *                                                          *      ELSBEGIN
00771 *        SET START STRING POINTER                          *      ELSBEGIN
00772 *                                                          *      ELSBEGIN
00773 ************************************************************      ELSBEGIN
00774  SET-START-STRING-POINTER.                                        ELSBEGIN
00775      SET INPUT-IDX DOWN    BY  +1.                                ELSBEGIN
00776      SET STRING-START-PTR  TO  INPUT-IDX.                         ELSBEGIN
00777                                                                   ELSBEGIN
00778                                                                   ELSBEGIN
00779 ************************************************************      ELSBEGIN
00780 *                                                          *      ELSBEGIN
00781 ************************************************************      ELSBEGIN
00782 *        INDICATE AN ZERO IN AN ENTRY FOUND                *      ELSBEGIN
00783 *                                                          *      ELSBEGIN
00784 ************************************************************      ELSBEGIN
00785  INDICATE-A-ZERO-ENTRY-FOUND.                                     ELSBEGIN
00786      SET ALL-ZERO-ENTRY-FOUND  TO  TRUE.                          ELSBEGIN
00787                                                                   ELSBEGIN
00788 ************************************************************      ELSBEGIN
00789 *        INDICATE AN EMBEDDED BLANK FOUND                  *      ELSBEGIN
00790 *                                                          *      ELSBEGIN
00791 ************************************************************      ELSBEGIN
00792  INDICATE-AN-EMBEDDED-BLANK-FOU.                                  ELSBEGIN
00793      SET EMBEDDED-BLANK-FOUND  TO  TRUE.                          ELSBEGIN
00794                                                                   ELSBEGIN
00795                                                                   ELSBEGIN
00796 ************************************************************      ELSBEGIN
00797 *                                                          *      ELSBEGIN
00798 *        INDICATE A NON-BLANK CHARACTER FOUND              *      ELSBEGIN
00799 *                                                          *      ELSBEGIN
00800 ************************************************************      ELSBEGIN
00801  INDICATE-A-NON-BLANK-CHARACTER.                                  ELSBEGIN
00802      SET NON-BLANK-CHAR-FOUND  TO  TRUE.                          ELSBEGIN
00803      EJECT                                                        ELSBEGIN
00804 ************************************************************      ELSBEGIN
00805 *                                                          *      ELSBEGIN
00806 *        FORMAT GROUP NUMBER                               *      ELSBEGIN
00807 *                                                          *      ELSBEGIN
00808 ************************************************************      ELSBEGIN
00809  FORMAT-GROUP-NUMBER.                                             ELSBEGIN
00810      MOVE ZEROS  TO  WS-TEMP-INPUT.                               ELSBEGIN
00811      COMPUTE INPUT-LEN     = STRING-END-PTR - STRING-START-PTR    ELSBEGIN
00812          + 1.                                                     ELSBEGIN
00813      COMPUTE WS-START-POSN = LENGTH OF SSB-GROUP-NUMBER           ELSBEGIN
00814            - INPUT-LEN +  1.                                      ELSBEGIN
00815      SET TEMP-INPUT-IDX  TO  WS-START-POSN.                       ELSBEGIN
00816      PERFORM MOVE-CHARS-INDIVIDUALLY                              ELSBEGIN
00817          VARYING INPUT-IDX  FROM  STRING-START-PTR  BY  +1        ELSBEGIN
00818                                    UNTIL INPUT-IDX                ELSBEGIN
00819                                    >     STRING-END-PTR.          ELSBEGIN
00820      MOVE WS-TEMP-GRP-9  TO  SSB-GROUP-NUMBER.                    ELSBEGIN
00821                                                                   ELSBEGIN
00822                                                                   ELSBEGIN
00823 ************************************************************      ELSBEGIN
00824 *                                                          *      ELSBEGIN
00825 *        MOVE CHARS INDIVIDUALLY                           *      ELSBEGIN
00826 *                                                          *      ELSBEGIN
00827 ************************************************************      ELSBEGIN
00828  MOVE-CHARS-INDIVIDUALLY.                                         ELSBEGIN
00829      MOVE WS-INPUT-CHAR (INPUT-IDX)                               ELSBEGIN
00830                TO  WS-TEMP-INPUT-CHAR (TEMP-INPUT-IDX).           ELSBEGIN
00831      SET TEMP-INPUT-IDX  UP  BY  +1.                              ELSBEGIN
00832                                                                   ELSBEGIN
00833                                                                   ELSBEGIN
00834 ************************************************************      ELSBEGIN
00835 *                                                          *      ELSBEGIN
00836 *        MOVE FORMATTED GROUP NUMBER                       *      ELSBEGIN
00837 *                                                          *      ELSBEGIN
00838 ************************************************************      ELSBEGIN
00839  MOVE-FORMATTED-GROUP-NUMBER.                                     ELSBEGIN
00840      MOVE ZEROES TO SSB-GROUP-NUMBER.                             ELSBEGIN
00841      MOVE WS-TEMP-GRP-9  TO  SSB-GROUP-NUMBER                     ELSBEGIN
00842                              WS-HOLD-GRP-9.                       ELSBEGIN
00843      MOVE WS-HOLD-GRP-6 TO  GROUPO.                               ELSBEGIN
00844                                                                   ELSBEGIN
00845                                                                   ELSBEGIN
00846 ************************************************************      ELSBEGIN
00847 *                                                          *      ELSBEGIN
00848 *        INDICATE MISSING GROUP NUMBER                     *      ELSBEGIN
00849 *                                                          *      ELSBEGIN
00850 ************************************************************      ELSBEGIN
00851  INDICATE-MISSING-GROUP-NUMBER.                                   ELSBEGIN
00852      MOVE -1                TO  GROUPL.                           ELSBEGIN
00853      MOVE WS-ENTER-GRP-NUM  TO  ERRMSGO.                          ELSBEGIN
00854      SET ERROR-FOUND        TO  TRUE.                             ELSBEGIN
00855      MOVE LOW-VALUES        TO  SSB-GROUP-NUMBER.                 ELSBEGIN
00856      EJECT                                                        ELSBEGIN
00857 ************************************************************      ELSBEGIN
00858 *                                                          *      ELSBEGIN
00859 *        EXTRACT REMAINING INQUIRY PARAMETERS              *      ELSBEGIN
00860 *                                                          *      ELSBEGIN
00861 ************************************************************      ELSBEGIN
00862  EXTRACT-REMAINING-INQUIRY-PARA.                                  ELSBEGIN
00863      IF SECTNL > 0                                                ELSBEGIN
00864          PERFORM EXTRACT-SECTION-NUMBER-FROM-MA.                  ELSBEGIN
00865      IF MEMBERL > 0                                               ELSBEGIN
00866          PERFORM EXTRACT-SUBSCRIBER-NUMBER-FROM.                  ELSBEGIN
00867      IF FROMDTL > 0 OR TODTL > 0                                  ELSBEGIN
00868          PERFORM EXTRACT-INQUIRY-DATE-RANGE-FRO                   ELSBEGIN
00869      ELSE                                                         ELSBEGIN
00870          PERFORM USE-TODAYS-DATE-AS-DEFAULT-INQ.                  ELSBEGIN
00871                                                                   ELSBEGIN
00872                                                                   ELSBEGIN
00873 ************************************************************      ELSBEGIN
00874 *                                                          *      ELSBEGIN
00875 *        EXTRACT SECTION NUMBER FROM MAP                   *      ELSBEGIN
00876 *                                                          *      ELSBEGIN
00877 ************************************************************      ELSBEGIN
00878  EXTRACT-SECTION-NUMBER-FROM-MA.                                  ELSBEGIN
00879      MOVE SECTNI  TO  WS-SECT-NO, WS-INPUT-FIELD.                 ELSBEGIN
00880      MOVE SECTNL  TO  INPUTL.                                     ELSBEGIN
00881      MOVE ZEROS   TO  STRING-START-PTR, STRING-END-PTR.           ELSBEGIN
00882      MOVE ZEROS   TO  INPUT-LEN.                                  ELSBEGIN
00883      SET SECT-NUM-ENTERED  TO  TRUE.                              ELSBEGIN
00884      PERFORM CHECK-FOR-TRAILING-BLANKS.                           ELSBEGIN
00885      IF NON-BLANK-CHAR-FOUND                                      ELSBEGIN
00886          PERFORM SET-END-STRING-POINTER                           ELSBEGIN
00887      ELSE                                                         ELSBEGIN
00888          PERFORM SECTION-NUMBER-NOT-ENTERED.                      ELSBEGIN
00889      IF SECT-NUM-ENTERED                                          ELSBEGIN
00890          PERFORM CHECK-FOR-LEADING-BLANKS.                        ELSBEGIN
00891      IF SECT-NUM-ENTERED                                          ELSBEGIN
00892          PERFORM FORMAT-SECTION-NUMBER.                           ELSBEGIN
00893      IF SECT-NUM-ENTERED                                          ELSBEGIN
00894          PERFORM MOVE-FORMATTED-SECTION-NUMBER.                   ELSBEGIN
00895                                                                   ELSBEGIN
00896                                                                   ELSBEGIN
00897 ************************************************************      ELSBEGIN
00898 *                                                          *      ELSBEGIN
00899 *        SECTION NUMBER NOT ENTERED                        *      ELSBEGIN
00900 *                                                          *      ELSBEGIN
00901 ************************************************************      ELSBEGIN
00902  SECTION-NUMBER-NOT-ENTERED.                                      ELSBEGIN
00903      SET SECT-NUM-NOT-ENTERED  TO  TRUE.                          ELSBEGIN
00904      EJECT                                                        ELSBEGIN
00905 ************************************************************      ELSBEGIN
00906 *                                                          *      ELSBEGIN
00907 *        SUB NUMBER NOT ENTERED                            *      ELSBEGIN
00908 *                                                          *      ELSBEGIN
00909 ************************************************************      ELSBEGIN
00910  SUB-NUMBER-NOT-ENTERED.                                          ELSBEGIN
00911      SET SUB-NUM-NOT-ENTERED  TO  TRUE.                           ELSBEGIN
00912                                                                   ELSBEGIN
00913                                                                   ELSBEGIN
00914 ************************************************************      ELSBEGIN
00915 *                                                          *      ELSBEGIN
00916 *        FORMAT SECTION NUMBER                             *      ELSBEGIN
00917 *                                                          *      ELSBEGIN
00918 ************************************************************      ELSBEGIN
00919  FORMAT-SECTION-NUMBER.                                           ELSBEGIN
00920      MOVE ZEROS  TO  WS-TEMP-INPUT.                               ELSBEGIN
00921      COMPUTE INPUT-LEN     = STRING-END-PTR - STRING-START-PTR    ELSBEGIN
00922          + 1.                                                     ELSBEGIN
00923      COMPUTE WS-START-POSN = LENGTH OF SSB-SECTN-NO               ELSBEGIN
00924            - INPUT-LEN + 1.                                       ELSBEGIN
00925      SET TEMP-INPUT-IDX  TO  WS-START-POSN.                       ELSBEGIN
00926      PERFORM MOVE-CHARS-INDIVIDUALLY                              ELSBEGIN
00927          VARYING INPUT-IDX  FROM  STRING-START-PTR  BY  +1        ELSBEGIN
00928                                    UNTIL INPUT-IDX                ELSBEGIN
00929                                    >     STRING-END-PTR.          ELSBEGIN
00930      MOVE WS-TEMP-SECT-5  TO  SSB-SECTN-NO.                       ELSBEGIN
00931                                                                   ELSBEGIN
00932                                                                   ELSBEGIN
00933 ************************************************************      ELSBEGIN
00934 *                                                          *      ELSBEGIN
00935 *        FORMAT SUB NUMBER                                 *      ELSBEGIN
00936 *                                                          *      ELSBEGIN
00937 ************************************************************      ELSBEGIN
00938  FORMAT-SUB-NUMBER.                                               ELSBEGIN
00939      MOVE ZEROS  TO  WS-TEMP-INPUT.                               ELSBEGIN
00940      COMPUTE INPUT-LEN     = STRING-END-PTR - STRING-START-PTR    ELSBEGIN
00941          + 1.                                                     ELSBEGIN
00942      COMPUTE WS-START-POSN = LENGTH OF SSB-SUBSCRIBER-NBR  -      ELSBEGIN
00943          INPUT-LEN + 1.                                           ELSBEGIN
00944      SET TEMP-INPUT-IDX  TO  WS-START-POSN.                       ELSBEGIN
00945      PERFORM MOVE-CHARS-INDIVIDUALLY                              ELSBEGIN
00946          VARYING INPUT-IDX  FROM  STRING-START-PTR  BY  +1        ELSBEGIN
00947                                    UNTIL INPUT-IDX                ELSBEGIN
00948                                    >     STRING-END-PTR.          ELSBEGIN
00949      MOVE WS-TEMP-INPUT TO  SSB-SUBSCRIBER-NBR.                   ELSBEGIN
00950      EJECT                                                        ELSBEGIN
00951 ************************************************************      ELSBEGIN
00952 *                                                          *      ELSBEGIN
00953 *        MOVE FORMATTED SECTION NUMBER                     *      ELSBEGIN
00954 *                                                          *      ELSBEGIN
00955 ************************************************************      ELSBEGIN
00956  MOVE-FORMATTED-SECTION-NUMBER.                                   ELSBEGIN
00957      MOVE ZEROES TO SSB-SECTN-NO.                                 ELSBEGIN
00958      MOVE WS-TEMP-SECT-5  TO SSB-SECTN-NO                         ELSBEGIN
00959                              WS-HOLD-SECT-5.                      ELSBEGIN
00960      MOVE WS-HOLD-SECT-4  TO  SECTNO.                             ELSBEGIN
00961                                                                   ELSBEGIN
00962                                                                   ELSBEGIN
00963 ************************************************************      ELSBEGIN
00964 *                                                          *      ELSBEGIN
00965 *        MOVE FORMATTED SUB NUMBER                         *      ELSBEGIN
00966 *                                                          *      ELSBEGIN
00967 ************************************************************      ELSBEGIN
00968  MOVE-FORMATTED-SUB-NUMBER.                                       ELSBEGIN
00969      MOVE WS-TEMP-INPUT  TO  SSB-SUBSCRIBER-NBR,                  ELSBEGIN
00970          MEMBERO.                                                 ELSBEGIN
00971                                                                   ELSBEGIN
00972                                                                   ELSBEGIN
00973 ************************************************************      ELSBEGIN
00974 *                                                          *      ELSBEGIN
00975 *        EXTRACT SUBSCRIBER NUMBER FROM MAP                *      ELSBEGIN
00976 *                                                          *      ELSBEGIN
00977 ************************************************************      ELSBEGIN
00978  EXTRACT-SUBSCRIBER-NUMBER-FROM.                                  ELSBEGIN
00979      MOVE MEMBERI             TO  WS-SUBSC-NO, WS-INPUT-FIELD.    ELSBEGIN
00980      MOVE MEMBERL             TO  INPUTL.                         ELSBEGIN
00981      MOVE ZEROS   TO  STRING-START-PTR, STRING-END-PTR.           ELSBEGIN
00982      MOVE ZEROS   TO  INPUT-LEN.                                  ELSBEGIN
00983      SET SUB-NUM-ENTERED  TO  TRUE.                               ELSBEGIN
00984      PERFORM CHECK-FOR-TRAILING-BLANKS.                           ELSBEGIN
00985      IF NON-BLANK-CHAR-FOUND                                      ELSBEGIN
00986          PERFORM SET-END-STRING-POINTER                           ELSBEGIN
00987      ELSE                                                         ELSBEGIN
00988          PERFORM SUB-NUMBER-NOT-ENTERED.                          ELSBEGIN
00989      IF SUB-NUM-ENTERED                                           ELSBEGIN
00990          PERFORM CHECK-FOR-LEADING-BLANKS.                        ELSBEGIN
00991      PERFORM CHECK-FOR-EMBEDDED-BLANKS                            ELSBEGIN
00992          VARYING INPUT-IDX FROM STRING-START-PTR BY 1             ELSBEGIN
00993                  UNTIL INPUT-IDX > STRING-END-PTR                 ELSBEGIN
00994                  OR  ERROR-FOUND                                  ELSBEGIN
00995                  OR  EMBEDDED-BLANK-FOUND.                        ELSBEGIN
00996      IF EMBEDDED-BLANK-FOUND                                      ELSBEGIN
00997          PERFORM INDICATE-INVALID-SUB-NUMBER.                     ELSBEGIN
00998      IF SUB-NUM-ENTERED AND NO-ERROR-FOUND                        ELSBEGIN
00999          PERFORM FORMAT-SUB-NUMBER.                               ELSBEGIN
01000      IF SUB-NUM-ENTERED AND NO-ERROR-FOUND                        ELSBEGIN
01001          PERFORM MOVE-FORMATTED-SUB-NUMBER.                       ELSBEGIN
01002      EJECT                                                        ELSBEGIN
01003 ************************************************************      ELSBEGIN
01004 *                                                          *      ELSBEGIN
01005 *        EXTRACT INQUIRY DATE RANGE FROM MAP               *      ELSBEGIN
01006 *                                                          *      ELSBEGIN
01007 ************************************************************      ELSBEGIN
01008  EXTRACT-INQUIRY-DATE-RANGE-FRO.                                  ELSBEGIN
01009      IF FROMDTL > 0                                               ELSBEGIN
01010          PERFORM EXTRACT-FROM-SERVICE-DATE.                       ELSBEGIN
01011      IF     TODTL > 0                                             ELSBEGIN
01012         AND NO-ERROR-FOUND                                        ELSBEGIN
01013          PERFORM EXTRACT-TO-SERVICE-DATE.                         ELSBEGIN
01014      IF     START-DATE-IN                                         ELSBEGIN
01015         AND END-DATE-IN                                           ELSBEGIN
01016         AND NO-ERROR-FOUND                                        ELSBEGIN
01017          PERFORM VALIDATE-SERVICE-DATE-RANGE                      ELSBEGIN
01018      ELSE IF     START-DATE-IN                                    ELSBEGIN
01019         AND NO-ERROR-FOUND                                        ELSBEGIN
01020          PERFORM USE-FROM-SERVICE-DATE-ENTEREDX                   ELSBEGIN
01021      ELSE IF NO-ERROR-FOUND                                       ELSBEGIN
01022          PERFORM USE-TO-SERVICE-DATE-ENTERED-AS.                  ELSBEGIN
01023                                                                   ELSBEGIN
01024                                                                   ELSBEGIN
01025 ************************************************************      ELSBEGIN
01026 *                                                          *      ELSBEGIN
01027 *        EXTRACT FROM SERVICE DATE                         *      ELSBEGIN
01028 *                                                          *      ELSBEGIN
01029 ************************************************************      ELSBEGIN
01030  EXTRACT-FROM-SERVICE-DATE.                                       ELSBEGIN
01031      MOVE FROMDTI       TO  HGADATE-DATE1.                        ELSBEGIN
01032      SET START-DATE-IN  TO  TRUE.                                 ELSBEGIN
01033      PERFORM CONVERT-DATE-FROM-GREG-TO-JULI.                      ELSBEGIN
01034      IF HGADATE-RETURN NOT = '00'                                 ELSBEGIN
01035          PERFORM INDICATE-INVALID-FROM-SERVICEX                   ELSBEGIN
01036      ELSE                                                         ELSBEGIN
01037          PERFORM SAVE-VALID-FROM-SERVICE-DATE.                    ELSBEGIN
01038                                                                   ELSBEGIN
01039                                                                   ELSBEGIN
01040 ************************************************************      ELSBEGIN
01041 *                                                          *      ELSBEGIN
01042 *        INDICATE INVALID FROM SERVICE DATE                *      ELSBEGIN
01043 *                                                          *      ELSBEGIN
01044 ************************************************************      ELSBEGIN
01045  INDICATE-INVALID-FROM-SERVICEX.                                  ELSBEGIN
01046      MOVE WS-INVALID-DATE  TO  ERRMSGO.                           ELSBEGIN
01047      SET ERROR-FOUND       TO  TRUE.                              ELSBEGIN
01048      MOVE ZERO             TO  SSB-SRV-FROM-DATE.                 ELSBEGIN
01049      MOVE -1               TO  FROMDTL.                           ELSBEGIN
01050      EJECT                                                        ELSBEGIN
01051 ************************************************************      ELSBEGIN
01052 *                                                          *      ELSBEGIN
01053 *        SAVE VALID FROM SERVICE DATE                      *      ELSBEGIN
01054 *                                                          *      ELSBEGIN
01055 ************************************************************      ELSBEGIN
01056  SAVE-VALID-FROM-SERVICE-DATE.                                    ELSBEGIN
01057      MOVE HGADATE-JULIAN2  TO  SSB-SRV-FROM-DATE.                 ELSBEGIN
01058      IF HGADATE-JULIAN2 < +70000                                  ELSBEGIN
01059            MOVE HEX-20 TO SSB-SRV-FROM-DATE-CC                    ELSBEGIN
01060 *          MOVE 20 TO SSB-SRV-FROM-DATE-CC                        ELSBEGIN
01061      ELSE                                                         ELSBEGIN
01062 *          MOVE 19 TO SSB-SRV-FROM-DATE-CC                        ELSBEGIN
01063            MOVE HEX-19 TO SSB-SRV-FROM-DATE-CC                    ELSBEGIN
01064      END-IF.                                                      ELSBEGIN
01065                                                                   ELSBEGIN
01066                                                                   ELSBEGIN
01067 ************************************************************      ELSBEGIN
01068 *                                                          *      ELSBEGIN
01069 *        EXTRACT TO SERVICE DATE                           *      ELSBEGIN
01070 *                                                          *      ELSBEGIN
01071 ************************************************************      ELSBEGIN
01072  EXTRACT-TO-SERVICE-DATE.                                         ELSBEGIN
01073      MOVE TODTI       TO  HGADATE-DATE1.                          ELSBEGIN
01074      SET END-DATE-IN  TO  TRUE.                                   ELSBEGIN
01075      PERFORM CONVERT-DATE-FROM-GREG-TO-JULI.                      ELSBEGIN
01076      IF HGADATE-RETURN NOT = '00'                                 ELSBEGIN
01077          PERFORM INDICATE-INVALID-TO-SERVICE-DA                   ELSBEGIN
01078      ELSE                                                         ELSBEGIN
01079          PERFORM SAVE-VALID-TO-SERVICE-DATE.                      ELSBEGIN
01080                                                                   ELSBEGIN
01081                                                                   ELSBEGIN
01082 ************************************************************      ELSBEGIN
01083 *                                                          *      ELSBEGIN
01084 *        INDICATE INVALID TO SERVICE DATE                  *      ELSBEGIN
01085 *                                                          *      ELSBEGIN
01086 ************************************************************      ELSBEGIN
01087  INDICATE-INVALID-TO-SERVICE-DA.                                  ELSBEGIN
01088      MOVE WS-INVALID-DATE  TO  ERRMSGO.                           ELSBEGIN
01089      SET ERROR-FOUND       TO  TRUE.                              ELSBEGIN
01090      MOVE ZERO             TO  SSB-SERV-TO-DT.                    ELSBEGIN
01091      MOVE -1               TO  TODTL.                             ELSBEGIN
01092                                                                   ELSBEGIN
01093                                                                   ELSBEGIN
01094 ************************************************************      ELSBEGIN
01095 *                                                          *      ELSBEGIN
01096 *        SAVE VALID TO SERVICE DATE                        *      ELSBEGIN
01097 *                                                          *      ELSBEGIN
01098 ************************************************************      ELSBEGIN
01099  SAVE-VALID-TO-SERVICE-DATE.                                      ELSBEGIN
01100      MOVE HGADATE-JULIAN2  TO  SSB-SRV-TO-DATE.                   ELSBEGIN
01101      IF HGADATE-JULIAN2 < +70000                                  ELSBEGIN
01102 *          MOVE 20 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01103            MOVE HEX-20 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01104      ELSE                                                         ELSBEGIN
01105            MOVE HEX-19 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01106 *          MOVE 19 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01107      END-IF.                                                      ELSBEGIN
01108                                                                   ELSBEGIN
01109                                                                   ELSBEGIN
01110 ************************************************************      ELSBEGIN
01111 *                                                          *      ELSBEGIN
01112 *        VALIDATE SERVICE DATE RANGE                       *      ELSBEGIN
01113 *                                                          *      ELSBEGIN
01114 ************************************************************      ELSBEGIN
01115  VALIDATE-SERVICE-DATE-RANGE.                                     ELSBEGIN
01116      IF SSB-SERV-FROM-DT > SSB-SERV-TO-DT                         ELSBEGIN
01117          PERFORM INDICATE-INVALID-SERVICE-DATEX.                  ELSBEGIN
01118      EJECT                                                        ELSBEGIN
01119 ************************************************************      ELSBEGIN
01120 *                                                          *      ELSBEGIN
01121 *        INDICATE INVALID SERVICE DATE RANGE               *      ELSBEGIN
01122 *                                                          *      ELSBEGIN
01123 ************************************************************      ELSBEGIN
01124  INDICATE-INVALID-SERVICE-DATEX.                                  ELSBEGIN
01125      MOVE WS-INVALID-DATE-RANGE  TO  ERRMSGO.                     ELSBEGIN
01126      SET ERROR-FOUND             TO  TRUE.                        ELSBEGIN
01127      MOVE -1                     TO  FROMDTL.                     ELSBEGIN
01128                                                                   ELSBEGIN
01129                                                                   ELSBEGIN
01130 ************************************************************      ELSBEGIN
01131 *                                                          *      ELSBEGIN
01132 *        USE FROM SERVICE DATE ENTERED AS RANGE            *      ELSBEGIN
01133 *                                                          *      ELSBEGIN
01134 ************************************************************      ELSBEGIN
01135  USE-FROM-SERVICE-DATE-ENTEREDX.                                  ELSBEGIN
01136      MOVE SSB-SRV-FROM-DATE  TO  SSB-SRV-TO-DATE.                 ELSBEGIN
01137      IF SSB-SRV-TO-DATE    < +70000                               ELSBEGIN
01138            MOVE HEX-20 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01139 *          MOVE 20 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01140      ELSE                                                         ELSBEGIN
01141            MOVE HEX-19 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01142 *          MOVE 19 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01143      END-IF.                                                      ELSBEGIN
01144                                                                   ELSBEGIN
01145                                                                   ELSBEGIN
01146 ************************************************************      ELSBEGIN
01147 *                                                          *      ELSBEGIN
01148 *        USE TO SERVICE DATE ENTERED AS RANGE              *      ELSBEGIN
01149 *                                                          *      ELSBEGIN
01150 ************************************************************      ELSBEGIN
01151  USE-TO-SERVICE-DATE-ENTERED-AS.                                  ELSBEGIN
01152      MOVE SSB-SERV-TO-DT  TO  SSB-SERV-FROM-DT.                   ELSBEGIN
01153      IF SSB-SRV-TO-DATE    < +70000                               ELSBEGIN
01154 *          MOVE 20 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01155            MOVE HEX-20 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01156      ELSE                                                         ELSBEGIN
01157 *          MOVE 19 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01158            MOVE HEX-19 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01159      END-IF.                                                      ELSBEGIN
01160                                                                   ELSBEGIN
01161                                                                   ELSBEGIN
01162 ************************************************************      ELSBEGIN
01163 *                                                          *      ELSBEGIN
01164 *        USE TODAYS DATE AS DEFAULT INQUIRY RANGE          *      ELSBEGIN
01165 *                                                          *      ELSBEGIN
01166 ************************************************************      ELSBEGIN
01167  USE-TODAYS-DATE-AS-DEFAULT-INQ.                                  ELSBEGIN
01168      MOVE 'TDY'  TO  MLDATE-FUNC.                                 ELSBEGIN
01169      MOVE 'J'    TO  MLDATE-FORM1.                                ELSBEGIN
01170      EXEC CICS LINK PROGRAM ('MLDATEC')                           ELSBEGIN
01171                     COMMAREA (MLDATE01)                           ELSBEGIN
01172                     END-EXEC.                                     ELSBEGIN
01173      IF MLDATE-RETURN NOT = '00'                                  ELSBEGIN
01174          PERFORM SET-NULL-DATE.                                   ELSBEGIN
01175      MOVE MLDATE-JUL1  TO  SSB-SRV-FROM-DT-CEN,                   ELSBEGIN
01176                                SSB-SRV-TO-DT-CEN.                 ELSBEGIN
01177 *    MOVE 'TDY'  TO  HGADATE-FUNC.                                ELSBEGIN
01178 *    MOVE 'J'    TO  HGADATE-FORM1.                               ELSBEGIN
01179 *    EXEC CICS LINK PROGRAM ('HGADATES')                          ELSBEGIN
01180 *                   COMMAREA (HGADATES-PARM-LIST)                 ELSBEGIN
01181 *                   END-EXEC.                                     ELSBEGIN
01182 *    IF HGADATE-RETURN NOT = '00'                                 ELSBEGIN
01183 *        PERFORM SET-NULL-DATE.                                   ELSBEGIN
01184 *    MOVE HGADATE-JULIAN1  TO  SSB-SRV-FROM-DATE,                 ELSBEGIN
01185 *                              SSB-SRV-TO-DATE.                   ELSBEGIN
01186 *    IF SSB-SRV-TO-DATE    < +70000                               ELSBEGIN
01187 *          MOVE 20 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01188 *          MOVE HEX-20 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01189 *    ELSE                                                         ELSBEGIN
01190 *          MOVE 19 TO SSB-SRV-TO-DATE-CC                          ELSBEGIN
01191 *          MOVE HEX-19 TO SSB-SRV-TO-DATE-CC                      ELSBEGIN
01192 *    END-IF.                                                      ELSBEGIN
01193 *    IF SSB-SRV-FROM-DATE    < +70000                             ELSBEGIN
01194 *          MOVE HEX-20 TO SSB-SRV-FROM-DATE-CC                    ELSBEGIN
01195 *          MOVE 20 TO SSB-SRV-FROM-DATE-CC                        ELSBEGIN
01196 *    ELSE                                                         ELSBEGIN
01197 *          MOVE HEX-19 TO SSB-SRV-FROM-DATE-CC                    ELSBEGIN
01198 *          MOVE 19 TO SSB-SRV-FROM-DATE-CC                        ELSBEGIN
01199 *    END-IF.                                                      ELSBEGIN
01200      EJECT                                                        ELSBEGIN
01201 ************************************************************      ELSBEGIN
01202 *                                                          *      ELSBEGIN
01203 *        SET NULL DATE                                     *      ELSBEGIN
01204 *                                                          *      ELSBEGIN
01205 ************************************************************      ELSBEGIN
01206  SET-NULL-DATE.                                                   ELSBEGIN
01207      MOVE ZERO TO HGADATE-JULIAN1.                                ELSBEGIN
01208                                                                   ELSBEGIN
01209                                                                   ELSBEGIN
01210 ************************************************************      ELSBEGIN
01211 *                                                          *      ELSBEGIN
01212 *        CONTINUE PROCESSING                               *      ELSBEGIN
01213 *                                                          *      ELSBEGIN
01214 ************************************************************      ELSBEGIN
01215  CONTINUE-PROCESSING.                                             ELSBEGIN
01216      SET NO-ERROR-FOUND  TO  TRUE.                                ELSBEGIN
01217      IF SSB-SUBSCRIBER-NBR NOT = LOW-VALUES                       ELSBEGIN
01218               AND NO-ERROR-FOUND                                  ELSBEGIN
01219          PERFORM DETERMINE-SECTION-NUMBERS-FORX.                  ELSBEGIN
01220      IF NO-ERROR-FOUND                                            ELSBEGIN
01221          PERFORM DETERMINE-IF-GROUP-AND-SECTION.                  ELSBEGIN
01222      IF NO-ERROR-FOUND                                            ELSBEGIN
01223          PERFORM SET-SELECTOR-AS-COMPLETE.                        ELSBEGIN
01224      IF ERROR-FOUND                                               ELSBEGIN
01225          PERFORM SEND-ERROR-MESSAGE.                              ELSBEGIN
01226                                                                   ELSBEGIN
01227                                                                   ELSBEGIN
01228 ************************************************************      ELSBEGIN
01229 *                                                          *      ELSBEGIN
01230 *        DETERMINE SECTION NUMBERS FOR SUBSCRIBER          *      ELSBEGIN
01231 *                                                          *      ELSBEGIN
01232 ************************************************************      ELSBEGIN
01233  DETERMINE-SECTION-NUMBERS-FORX.                                  ELSBEGIN
01234      PERFORM OBTAIN-CANDIDATE-SECTION-NUMBE.                      ELSBEGIN
01235      IF CIA-RC-OK                                                 ELSBEGIN
01236          PERFORM DETERMINE-IF-ONLY-ONE-SECTIONX                   ELSBEGIN
01237      ELSE                                                         ELSBEGIN
01238          PERFORM DETERMINE-WHETHER-GROUP-NUMBER.                  ELSBEGIN
01239                                                                   ELSBEGIN
01240                                                                   ELSBEGIN
01241 ************************************************************      ELSBEGIN
01242 *                                                          *      ELSBEGIN
01243 *        OBTAIN CANDIDATE SECTION NUMBERS FROM CORPORATE ME*      ELSBEGIN
01244 *                                                          *      ELSBEGIN
01245 ************************************************************      ELSBEGIN
01246  OBTAIN-CANDIDATE-SECTION-NUMBE.                                  ELSBEGIN
01247      EXEC CICS LINK PROGRAM  ('ELUMEMBR')                         ELSBEGIN
01248                     COMMAREA (DFHCOMMAREA)                        ELSBEGIN
01249                     END-EXEC.                                     ELSBEGIN
01250      EJECT                                                        ELSBEGIN
01251 ************************************************************      ELSBEGIN
01252 *                                                          *      ELSBEGIN
01253 *        DETERMINE IF ONLY ONE SECTION APPLIES TO MEMBER   *      ELSBEGIN
01254 *                                                          *      ELSBEGIN
01255 ************************************************************      ELSBEGIN
01256  DETERMINE-IF-ONLY-ONE-SECTIONX.                                  ELSBEGIN
01257      SET CIA-ELSMEMS-DDN TO TRUE.                                 ELSBEGIN
01258      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSBEGIN
01259                 ADDRESS OF MSI-MEMBERSHIP-INTERFACE.              ELSBEGIN
01260      IF MSI-NBR-MBR-SECTNS = 1                                    ELSBEGIN
01261          PERFORM SELECT-ONLY-APPLICABLE-SECTION.                  ELSBEGIN
01262                                                                   ELSBEGIN
01263                                                                   ELSBEGIN
01264 ************************************************************      ELSBEGIN
01265 *                                                          *      ELSBEGIN
01266 *        DETERMINE IF NO SECTIONS APPLY TO MEMBER          *      ELSBEGIN
01267 *                                                          *      ELSBEGIN
01268 ************************************************************      ELSBEGIN
01269  DETERMINE-IF-NO-SECTIONS-APPLY.                                  ELSBEGIN
01270      SET SSB-NO-SUBSCRIBER-NBR TO TRUE.                           ELSBEGIN
01271                                                                   ELSBEGIN
01272                                                                   ELSBEGIN
01273 ************************************************************      ELSBEGIN
01274 *                                                          *      ELSBEGIN
01275 *        SELECT ONLY APPLICABLE SECTION FOR MEMBER         *      ELSBEGIN
01276 *                                                                 ELSBEGIN
01277 ************************************************************      ELSBEGIN
01278  SELECT-ONLY-APPLICABLE-SECTION.                                  ELSBEGIN
01279      SET SSB-DATA-DERIVED (SSB-SELECTOR-STATE) TO                 ELSBEGIN
01280          TRUE.                                                    ELSBEGIN
01281      MOVE MSI-MEMBER-SECTION (1)    TO  SSB-SECTN-NO.             ELSBEGIN
01282      MOVE MSI-EFF-DATE-CENTURY (1)  TO  SSB-SUB-SECTN-EFF-DT.     ELSBEGIN
01283      MOVE MSI-TERMIN-DATE-CC (1)    TO  SSB-SUB-SECTN-TERMN-DT.   ELSBEGIN
01284 *    MOVE MSI-MBR-SECTN (1)  TO  SSB-SECTN-NO.                    ELSBEGIN
01285 *    MOVE MSI-EFF-DT (1)     TO  SSB-SUB-SECTN-EFF-DT.            ELSBEGIN
01286 *    MOVE MSI-TERM-DT (1)    TO  SSB-SUB-SECTN-TERMN-DT.          ELSBEGIN
01287                                                                   ELSBEGIN
01288                                                                   ELSBEGIN
01289 ************************************************************      ELSBEGIN
01290 *                                                          *      ELSBEGIN
01291 *        DETERMINE WHETHER GROUP NUMBER OR SUBSCRIBER NUMBE*      ELSBEGIN
01292 *                                                          *      ELSBEGIN
01293 ************************************************************      ELSBEGIN
01294  DETERMINE-WHETHER-GROUP-NUMBER.                                  ELSBEGIN
01295      IF CIA-RC-MEMB-GRP-NOTFND                                    ELSBEGIN
01296          PERFORM INDICATE-INVALID-GROUP-NUMBER                    ELSBEGIN
01297      ELSE IF    CIA-RC-MEMB-NON-RETN                              ELSBEGIN
01298         OR CIA-RC-MEMB-NOT-IN-GRP                                 ELSBEGIN
01299         OR CIA-RC-MEMB-TBL-OVFL                                   ELSBEGIN
01300         OR CIA-RC-MEMB-NO-SECTN-INFO                              ELSBEGIN
01301          PERFORM INDICATE-SUBSCRIBER-NUMBER-NOT.                  ELSBEGIN
01302      EJECT                                                        ELSBEGIN
01303 ************************************************************      ELSBEGIN
01304 *                                                          *      ELSBEGIN
01305 *        INDICATE INVALID GROUP NUMBER                     *      ELSBEGIN
01306 *                                                          *      ELSBEGIN
01307 ************************************************************      ELSBEGIN
01308  INDICATE-INVALID-GROUP-NUMBER.                                   ELSBEGIN
01309      MOVE -1                  TO  GROUPL.                         ELSBEGIN
01310      SET ERROR-FOUND          TO  TRUE.                           ELSBEGIN
01311      MOVE WS-INVALID-GRP-NUM  TO  ERRMSGO.                        ELSBEGIN
01312                                                                   ELSBEGIN
01313                                                                   ELSBEGIN
01314 ************************************************************      ELSBEGIN
01315 *                                                          *      ELSBEGIN
01316 *        INDICATE INVALID SUB NUMBER                       *      ELSBEGIN
01317 *                                                          *      ELSBEGIN
01318 ************************************************************      ELSBEGIN
01319  INDICATE-INVALID-SUB-NUMBER.                                     ELSBEGIN
01320      MOVE -1                  TO  GROUPL.                         ELSBEGIN
01321      SET ERROR-FOUND          TO  TRUE.                           ELSBEGIN
01322      MOVE WS-INVALID-SUB-NUM  TO  ERRMSGO.                        ELSBEGIN
01323                                                                   ELSBEGIN
01324                                                                   ELSBEGIN
01325 ************************************************************      ELSBEGIN
01326 *                                                          *      ELSBEGIN
01327 *        INDICATE SUBSCRIBER NUMBER NOT USABLE             *      ELSBEGIN
01328 *                                                          *      ELSBEGIN
01329 ************************************************************      ELSBEGIN
01330  INDICATE-SUBSCRIBER-NUMBER-NOT.                                  ELSBEGIN
01331      MOVE LOW-VALUES  TO  SSB-SUBSCRIBER-NBR.                     ELSBEGIN
01332      EJECT                                                        ELSBEGIN
01333 ************************************************************      ELSBEGIN
01334 *                                                          *      ELSBEGIN
01335 *        DETERMINE IF GROUP AND SECTION ARE ON BLUE CHIP   *      ELSBEGIN
01336 *                                                          *      ELSBEGIN
01337 ************************************************************      ELSBEGIN
01338  DETERMINE-IF-GROUP-AND-SECTION.                                  ELSBEGIN
01339      PERFORM OBTAIN-KEY-TABLES-FROM-GENERIC.                      ELSBEGIN
01340      IF WS-CIA-RC-KTB-GRP-NOTFND                                  ELSBEGIN
01341          PERFORM INDICATE-GROUP-NOT-ON-BLUE-CHI                   ELSBEGIN
01342      ELSE IF WS-CIA-RC-KTB-SECTN-NOTFND                           ELSBEGIN
01343          PERFORM INDICATE-SECTION-NOT-ON-BLUE-C                   ELSBEGIN
01344      ELSE IF WS-CIA-AB-INCR-TBL-SIZE                              ELSBEGIN
01345          PERFORM INDICATE-SECT-TABLE-OVERFLOW                     ELSBEGIN
01346      ELSE                                                         ELSBEGIN
01347          IF KTS-NBR-KEYS = 1                                      ELSBEGIN
01348              PERFORM AUTO-SELECT-ONLY-SECTION.                    ELSBEGIN
01349                                                                   ELSBEGIN
01350  AUTO-SELECT-ONLY-SECTION.                                        ELSBEGIN
01351      MOVE KTS-SECTN-NO (1) TO SSB-SECT-NO  SECTNO.                ELSBEGIN
01352      IF WS-CIA-RC-MEMB-NO-SECTN-INFO                              ELSBEGIN
01353           PERFORM INDICATE-SECTION-IS-NOT-VIEWAB                  ELSBEGIN
01354      ELSE                                                         ELSBEGIN
01355           PERFORM OBTAIN-KEY-TABLES-FROM-GENERIC                  ELSBEGIN
01356           IF WS-CIA-RC-MEMB-NO-SECTN-INFO                         ELSBEGIN
01357               PERFORM INDICATE-SECTION-IS-NOT-VIEWAB              ELSBEGIN
01358           ELSE                                                    ELSBEGIN
01359               SET SSB-DATA-DERIVED (SSB-SELECTOR-STATE) TO TRUE.  ELSBEGIN
01360                                                                   ELSBEGIN
01361                                                                   ELSBEGIN
01362 ************************************************************      ELSBEGIN
01363 *                                                          *      ELSBEGIN
01364 *        OBTAIN KEY TABLES FROM GENERIC CONTRACT PROCESSING*      ELSBEGIN
01365 *                                                          *      ELSBEGIN
01366 ************************************************************      ELSBEGIN
01367  OBTAIN-KEY-TABLES-FROM-GENERIC.                                  ELSBEGIN
01368      EXEC CICS LINK PROGRAM  ('ELUKYTAB')                         ELSBEGIN
01369                     COMMAREA (DFHCOMMAREA)                        ELSBEGIN
01370                     END-EXEC.                                     ELSBEGIN
01371      SET CIA-ELSKTBS-DDN TO TRUE.                                 ELSBEGIN
01372      MOVE CIA-RETURN-CODE TO WS-CIA-RETURN-CODE.                  ELSBEGIN
01373      MOVE CIA-ABCODE      TO WS-CIA-ABCODE.                       ELSBEGIN
01374      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSBEGIN
01375          ADDRESS OF KTS-SECTIONS-KEY-TABLE.                       ELSBEGIN
01376                                                                   ELSBEGIN
01377                                                                   ELSBEGIN
01378 ************************************************************      ELSBEGIN
01379 *                                                          *      ELSBEGIN
01380 *        INDICATE GROUP NOT ON BLUE CHIP                   *      ELSBEGIN
01381 *                                                          *      ELSBEGIN
01382 ************************************************************      ELSBEGIN
01383  INDICATE-GROUP-NOT-ON-BLUE-CHI.                                  ELSBEGIN
01384      MOVE -1                TO  GROUPL.                           ELSBEGIN
01385      SET ERROR-FOUND        TO  TRUE.                             ELSBEGIN
01386      MOVE WS-NOT-BLUE-CHIP  TO  ERRMSGO.                          ELSBEGIN
01387      EJECT                                                        ELSBEGIN
01388 ************************************************************      ELSBEGIN
01389 *                                                          *      ELSBEGIN
01390 *        INDICATE SECTION IS NOT VIEWABLE                  *      ELSBEGIN
01391 *                                                          *      ELSBEGIN
01392 ************************************************************      ELSBEGIN
01393  INDICATE-SECTION-IS-NOT-VIEWAB.                                  ELSBEGIN
01394      MOVE -1                      TO  GROUPL.                     ELSBEGIN
01395      SET ERROR-FOUND              TO  TRUE.                       ELSBEGIN
01396      MOVE WS-SECTION-NOT-VIEWABLE TO  ERRMSGO.                    ELSBEGIN
01397                                                                   ELSBEGIN
01398                                                                   ELSBEGIN
01399 ************************************************************      ELSBEGIN
01400 *                                                          *      ELSBEGIN
01401 *        INDICATE SECTION NOT ON BLUE CHIP                 *      ELSBEGIN
01402 *                                                          *      ELSBEGIN
01403 ************************************************************      ELSBEGIN
01404  INDICATE-SECTION-NOT-ON-BLUE-C.                                  ELSBEGIN
01405      MOVE SSB-SECT-NO TO SECTNI.                                  ELSBEGIN
01406      MOVE LOW-VALUES  TO  SSB-SECTN-NO.                           ELSBEGIN
01407      MOVE -1                TO  SECTNL.                           ELSBEGIN
01408      SET ERROR-FOUND        TO  TRUE.                             ELSBEGIN
01409      MOVE WS-INVALID-SEC-NUM      TO  ERRMSGO.                    ELSBEGIN
01410      EJECT                                                        ELSBEGIN
01411                                                                   ELSBEGIN
01412 ************************************************************      ELSBEGIN
01413 *                                                          *      ELSBEGIN
01414 *    INDICATE SECTION TABLE OVERFLOW - SECTION # REQUIRED  *      ELSBEGIN
01415 *                                                          *      ELSBEGIN
01416 ************************************************************      ELSBEGIN
01417  INDICATE-SECT-TABLE-OVERFLOW.                                    ELSBEGIN
01418      MOVE -1                TO  SECTNL.                           ELSBEGIN
01419      SET ERROR-FOUND        TO  TRUE.                             ELSBEGIN
01420      MOVE WS-SECTION-REQUIRED     TO  ERRMSGO.                    ELSBEGIN
01421      EJECT                                                        ELSBEGIN
01422 ************************************************************      ELSBEGIN
01423 *                                                          *      ELSBEGIN
01424 *        COMMAND LINE INPUT                                *      ELSBEGIN
01425 *                                                          *      ELSBEGIN
01426 ************************************************************      ELSBEGIN
01427  COMMAND-LINE-INPUT.                                              ELSBEGIN
01428      MOVE LOW-VALUES  TO  EL00MAPO, EL01MAPO.                     ELSBEGIN
01429      PERFORM EXTRACT-GROUP-NUMBER-FROM-COMM.                      ELSBEGIN
01430      PERFORM MOVE-COMMAND-LINE-ENTRIES-TO-S.                      ELSBEGIN
01431      IF NO-ERROR-FOUND                                            ELSBEGIN
01432          PERFORM DETERMINE-IF-GROUP-AND-SECTION.                  ELSBEGIN
01433      IF NO-ERROR-FOUND                                            ELSBEGIN
01434          PERFORM OBTAIN-USER-DATES                                ELSBEGIN
01435      ELSE                                                         ELSBEGIN
01436          PERFORM SETUP-ERROR-SCREEN.                              ELSBEGIN
01437 ***************************************************               ELSBEGIN
01438 *** THIS MOVE IS DONE SO THAT MAINLINE DOES NOT ***               ELSBEGIN
01439 *** PROCESS AS COMMAND LINE INPUT AGAIN         ***               ELSBEGIN
01440 ***************************************************               ELSBEGIN
01441      MOVE LOW-VALUES TO SSB-CMDLN-DATA.                           ELSBEGIN
01442                                                                   ELSBEGIN
01443                                                                   ELSBEGIN
01444 ************************************************************      ELSBEGIN
01445 *                                                          *      ELSBEGIN
01446 *        EXTRACT GROUP NUMBER FROM COMMAND LINE            *      ELSBEGIN
01447 *                                                          *      ELSBEGIN
01448 ************************************************************      ELSBEGIN
01449  EXTRACT-GROUP-NUMBER-FROM-COMM.                                  ELSBEGIN
01450      MOVE SSB-CMDLN-GROUP-NUMBER  TO  WS-GROUP-NO,                ELSBEGIN
01451          WS-INPUT-FIELD.                                          ELSBEGIN
01452      MOVE LENGTH OF SSB-CMDLN-GROUP-NUMBER TO INPUTL.             ELSBEGIN
01453      PERFORM INTERGATE-AND-FORMAT-GROUP-NUM.                      ELSBEGIN
01454      EJECT                                                        ELSBEGIN
01455 ************************************************************      ELSBEGIN
01456 *                                                          *      ELSBEGIN
01457 *        INTERGATE AND FORMAT GROUP NUMBER                 *      ELSBEGIN
01458 *                                                          *      ELSBEGIN
01459 ************************************************************      ELSBEGIN
01460  INTERGATE-AND-FORMAT-GROUP-NUM.                                  ELSBEGIN
01461      MOVE ZEROS       TO  STRING-START-PTR, STRING-END-PTR.       ELSBEGIN
01462      MOVE ZEROS       TO  INPUT-LEN.                              ELSBEGIN
01463      PERFORM CHECK-FOR-TRAILING-BLANKS.                           ELSBEGIN
01464      IF NON-BLANK-CHAR-FOUND                                      ELSBEGIN
01465          PERFORM SET-END-STRING-POINTER                           ELSBEGIN
01466      ELSE                                                         ELSBEGIN
01467          PERFORM INDICATE-INVALID-GROUP-NUMBER.                   ELSBEGIN
01468      IF NO-ERROR-FOUND                                            ELSBEGIN
01469          PERFORM CHECK-FOR-LEADING-BLANKS.                        ELSBEGIN
01470      SET NO-EMBEDDED-BLANK  TO  TRUE.                             ELSBEGIN
01471      PERFORM CHECK-FOR-EMBEDDED-BLANKS                            ELSBEGIN
01472          VARYING INPUT-IDX FROM STRING-START-PTR BY 1             ELSBEGIN
01473                                   UNTIL INPUT-IDX >               ELSBEGIN
01474              STRING-END-PTR                                       ELSBEGIN
01475                                   OR    ERROR-FOUND               ELSBEGIN
01476                                   OR                              ELSBEGIN
01477              EMBEDDED-BLANK-FOUND.                                ELSBEGIN
01478      IF EMBEDDED-BLANK-FOUND                                      ELSBEGIN
01479          PERFORM INDICATE-INVALID-GROUP-NUMBER.                   ELSBEGIN
01480 *    SET NO-ALL-ZERO-ENTRY TO  TRUE.                              ELSBEGIN
01481      PERFORM CHECK-FOR-ALL-ZERO-ENTRIES                           ELSBEGIN
01482          VARYING INPUT-IDX FROM STRING-START-PTR BY 1             ELSBEGIN
01483                                   UNTIL INPUT-IDX >               ELSBEGIN
01484              STRING-END-PTR                                       ELSBEGIN
01485                                   OR    ERROR-FOUND               ELSBEGIN
01486                                   OR                              ELSBEGIN
01487              NO-ALL-ZERO-ENTRY.                                   ELSBEGIN
01488      IF ALL-ZERO-ENTRY-FOUND                                      ELSBEGIN
01489          PERFORM INDICATE-INVALID-GROUP-NUMBER.                   ELSBEGIN
01490      IF NO-ERROR-FOUND                                            ELSBEGIN
01491          PERFORM FORMAT-GROUP-NUMBER.                             ELSBEGIN
01492      IF NO-ERROR-FOUND                                            ELSBEGIN
01493          PERFORM MOVE-FORMATTED-GROUP-NUMBER.                     ELSBEGIN
01494                                                                   ELSBEGIN
01495                                                                   ELSBEGIN
01496 ************************************************************      ELSBEGIN
01497 *                                                          *      ELSBEGIN
01498 *        MOVE COMMAND LINE ENTRIES TO SELECTION FIELDS     *      ELSBEGIN
01499 *                                                          *      ELSBEGIN
01500 ************************************************************      ELSBEGIN
01501  MOVE-COMMAND-LINE-ENTRIES-TO-S.                                  ELSBEGIN
01502      MOVE SSB-CMDLN-SECTN-NO        TO  SSB-SECTN-NO.             ELSBEGIN
01503      MOVE SSB-CMDLN-SUBSCRIBER-NBR  TO                            ELSBEGIN
01504          SSB-SUBSCRIBER-NBR.                                      ELSBEGIN
01505      EJECT                                                        ELSBEGIN
01506 ************************************************************      ELSBEGIN
01507 *                                                          *      ELSBEGIN
01508 *        SETUP ERROR SCREEN                                *      ELSBEGIN
01509 *                                                          *      ELSBEGIN
01510 ************************************************************      ELSBEGIN
01511  SETUP-ERROR-SCREEN.                                              ELSBEGIN
01512      IF EMBEDDED-BLANK-FOUND                                      ELSBEGIN
01513          PERFORM USE-GROUP-NUMBER-ENTERED-ON-CO.                  ELSBEGIN
01514      MOVE SSB-CMDLN-SECTN       TO  SECTNO.                       ELSBEGIN
01515      MOVE SSB-CMDLN-SUBSCRIBER-NBR TO  MEMBERO.                   ELSBEGIN
01516      PERFORM SEND-ERROR-MESSAGE.                                  ELSBEGIN
01517                                                                   ELSBEGIN
01518                                                                   ELSBEGIN
01519 ************************************************************      ELSBEGIN
01520 *                                                          *      ELSBEGIN
01521 *        USE GROUP NUMBER ENTERED ON COMMAND LINE          *      ELSBEGIN
01522 *                                                          *      ELSBEGIN
01523 ************************************************************      ELSBEGIN
01524  USE-GROUP-NUMBER-ENTERED-ON-CO.                                  ELSBEGIN
01525      MOVE SSB-CMDLN-GRP-NO         TO  GROUPO.                    ELSBEGIN
01526                                                                   ELSBEGIN
01527                                                                   ELSBEGIN
01528 ************************************************************      ELSBEGIN
01529 *                                                          *      ELSBEGIN
01530 *        OBTAIN USER DATES                                 *      ELSBEGIN
01531 *                                                          *      ELSBEGIN
01532 ************************************************************      ELSBEGIN
01533  OBTAIN-USER-DATES.                                               ELSBEGIN
01534      MOVE SSB-CMDLN-SECTN       TO  SECTNO.                       ELSBEGIN
01535      MOVE SSB-CMDLN-SUBSCRIBER-NBR TO  MEMBERO.                   ELSBEGIN
01536      MOVE LOW-VALUES               TO  FROMDTO, TODTO.            ELSBEGIN
01537      MOVE DFHBMUNF TO GROUPA, SECTNA, MEMBERA, FROMDTA,           ELSBEGIN
01538          TODTA.                                                   ELSBEGIN
01539      MOVE -1                       TO  FROMDTL.                   ELSBEGIN
01540      PERFORM SEND-PRIMARY-SCREEN.                                 ELSBEGIN
01541                                                                   ELSBEGIN
01542                                                                   ELSBEGIN
01543 ************************************************************      ELSBEGIN
01544 *                                                          *      ELSBEGIN
01545 *        SET SELECTOR AS COMPLETE                          *      ELSBEGIN
01546 *                                                          *      ELSBEGIN
01547 ************************************************************      ELSBEGIN
01548  SET-SELECTOR-AS-COMPLETE.                                        ELSBEGIN
01549      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELSBEGIN
01550      EJECT                                                        ELSBEGIN
01551 ************************************************************      ELSBEGIN
01552 *                                                          *      ELSBEGIN
01553 *        SIGNAL INVALID SELECTOR STATUS                    *      ELSBEGIN
01554 *                                                          *      ELSBEGIN
01555 ************************************************************      ELSBEGIN
01556  SIGNAL-INVALID-SELECTOR-STATUS.                                  ELSBEGIN
01557      SET CIA-AB-UNDEF TO TRUE.                                    ELSBEGIN
01558      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELSBEGIN
01559                                                                   ELSBEGIN
01560                                                                   ELSBEGIN
01561 ************************************************************      ELSBEGIN
01562 *                                                          *      ELSBEGIN
01563 *        MAIN SCREEN.TERMINATE                             *      ELSBEGIN
01564 *                                                          *      ELSBEGIN
01565 ************************************************************      ELSBEGIN
01566  MAIN-SCREEN-TERMINATE.                                           ELSBEGIN
01567      IF MSI-NBR-MBR-SECTNS = 0                                    ELSBEGIN
01568          PERFORM DETERMINE-IF-NO-SECTIONS-APPLY.                  ELSBEGIN
01569      EXEC CICS RETURN END-EXEC.                                   ELSBEGIN
01570      GOBACK.                                                      ELSBEGIN
01571                                                                   ELSBEGIN
01572                                                                   ELSBEGIN
01573 ************************************************************      ELSBEGIN
01574 *                                                          *      ELSBEGIN
01575 *        SIGNAL TERMINAL OR MAPPING ERROR                  *      ELSBEGIN
01576 *                                                          *      ELSBEGIN
01577 ************************************************************      ELSBEGIN
01578  SIGNAL-TERMINAL-OR-MAPPING-ERR.                                  ELSBEGIN
01579      SET CIA-AB-MAPFAIL TO TRUE.                                  ELSBEGIN
01580      EXEC CICS ABEND ABCODE (CIA-ABCODE) END-EXEC.                ELSBEGIN
01581                                                                   ELSBEGIN
01582                                                                   ELSBEGIN
01583 ************************************************************      ELSBEGIN
01584 *                                                          *      ELSBEGIN
01585 *        OBTAIN CURRENT DATE                               *      ELSBEGIN
01586 *                                                          *      ELSBEGIN
01587 ************************************************************      ELSBEGIN
01588  OBTAIN-CURRENT-DATE.                                             ELSBEGIN
01589      MOVE ZERO TO WS-EIBDATE-AREA.                                ELSBEGIN
01590 *    MOVE EIBDATE TO WS-EIBDATE.                                  ELSBEGIN
01591      MOVE EIBDATE              TO WS-EIBDATE-CEN.                 ELSBEGIN
01592      EVALUATE TRUE                                                ELSBEGIN
01593         WHEN WS-EIBDATE-2 = 0                                     ELSBEGIN
01594           MOVE HEX-19  TO WS-EIBDATE-CC                           ELSBEGIN
01595         WHEN WS-EIBDATE-2 = 1                                     ELSBEGIN
01596            MOVE HEX-20  TO WS-EIBDATE-CC                          ELSBEGIN
01597      END-EVALUATE.                                                ELSBEGIN
01598      MOVE WS-EIBDATE-DT         TO HGADATE-JULIAN1.               ELSBEGIN
01599                                                                   ELSBEGIN
01600 *    MOVE WS-EIBDATE-CEN TO  HGADATE-JULIAN1.                     ELSBEGIN
01601 *    MOVE EIBDATE        TO  HGADATE-JULIAN1.                     ELSBEGIN
01602      PERFORM CONVERT-DATE-FROM-JULIAN-TO-GR.                      ELSBEGIN
01603      MOVE HGADATE-DATE2  TO  WS-DATE-OUT.                         ELSBEGIN
01604      MOVE WS-DATE-OUT    TO  DATE1O.                              ELSBEGIN
01605      EJECT                                                        ELSBEGIN
01606 ************************************************************      ELSBEGIN
01607 *                                                          *      ELSBEGIN
01608 *        CONVERT DATE FROM JULIAN TO GREG                  *      ELSBEGIN
01609 *                                                          *      ELSBEGIN
01610 ************************************************************      ELSBEGIN
01611  CONVERT-DATE-FROM-JULIAN-TO-GR.                                  ELSBEGIN
01612      MOVE 'CNV'  TO  HGADATE-FUNC.                                ELSBEGIN
01613      MOVE 'J'    TO  HGADATE-FORM1.                               ELSBEGIN
01614      MOVE 'M'    TO  HGADATE-FORM2.                               ELSBEGIN
01615      MOVE ZEROS  TO  HGADATE-RETURN, HGADATE-AMOUNT,              ELSBEGIN
01616          HGADATE-DATE2.                                           ELSBEGIN
01617      EXEC CICS LINK PROGRAM ('HGADATES')                          ELSBEGIN
01618                     COMMAREA (HGADATES-PARM-LIST)                 ELSBEGIN
01619                     END-EXEC.                                     ELSBEGIN
01620                                                                   ELSBEGIN
01621                                                                   ELSBEGIN
01622 ************************************************************      ELSBEGIN
01623 *                                                          *      ELSBEGIN
01624 *        CONVERT DATE FROM GREG TO JULIAN                  *      ELSBEGIN
01625 *                                                          *      ELSBEGIN
01626 ************************************************************      ELSBEGIN
01627  CONVERT-DATE-FROM-GREG-TO-JULI.                                  ELSBEGIN
01628      MOVE 'CNV'  TO  HGADATE-FUNC.                                ELSBEGIN
01629      MOVE 'M'    TO  HGADATE-FORM1.                               ELSBEGIN
01630      MOVE 'J'    TO  HGADATE-FORM2.                               ELSBEGIN
01631      MOVE ZEROS  TO  HGADATE-RETURN, HGADATE-AMOUNT,              ELSBEGIN
01632          HGADATE-DATE2.                                           ELSBEGIN
01633      EXEC CICS LINK PROGRAM ('HGADATES')                          ELSBEGIN
01634                     COMMAREA (HGADATES-PARM-LIST)                 ELSBEGIN
01635                     END-EXEC.                                     ELSBEGIN
01636                                                                   ELSBEGIN
01637                                                                   ELSBEGIN
01638 ************************************************************      ELSBEGIN
01639 *                                                          *      ELSBEGIN
01640 *        OBTAIN CURRENT TIME                               *      ELSBEGIN
01641 *                                                          *      ELSBEGIN
01642 ************************************************************      ELSBEGIN
01643  OBTAIN-CURRENT-TIME.                                             ELSBEGIN
01644      MOVE EIBTIME      TO  WS-TIME.                               ELSBEGIN
01645      MOVE WS-HH        TO  WS-HH-OUT.                             ELSBEGIN
01646      MOVE WS-MM        TO  WS-MM-OUT.                             ELSBEGIN
01647      MOVE WS-SS        TO  WS-SS-OUT.                             ELSBEGIN
01648      MOVE WS-TIME-OUT  TO  TIME1O.                                ELSBEGIN
01649                                                                   ELSBEGIN
01650 ************************************************************      ELSBEGIN
01651 *                                                          *      ELSBEGIN
01652 *        DO GETMAIN                                        *      ELSBEGIN
01653 *                                                          *      ELSBEGIN
01654 ************************************************************      ELSBEGIN
01655  DO-GETMAIN.                                                      ELSBEGIN
01656      SET CIA-STG-GETMAIN TO TRUE.                                 ELSBEGIN
01657      EXEC CICS                                                    ELSBEGIN
01658         LINK PROGRAM ('ELUSTGMG')                                 ELSBEGIN
01659         COMMAREA (DFHCOMMAREA)                                    ELSBEGIN
01660      END-EXEC.                                                    ELSBEGIN
01661      SET WS-DID-GETMAIN TO TRUE.                                  ELSBEGIN
01662                                                                   ELSBEGIN
01663 ************************************************************      ELSBEGIN
01664 *                                                          *      ELSBEGIN
01665 *        CHECK FOR ELS MESSAGES                            *      ELSBEGIN
01666 *                                                          *      ELSBEGIN
01667 ************************************************************      ELSBEGIN
01668  CHECK-FOR-ELS-MESSAGES.                                          ELSBEGIN
01669      MOVE MSG-RECORD-PREFIX TO CMF-RECORD-PREFIX.                 ELSBEGIN
01670      MOVE MSG-ELEMENT-SYSTEM-NAME TO CMF-ELEMENT-SYSTEM-NAME.     ELSBEGIN
01671      MOVE MSG-CODE-VALUE TO CMF-CODE-VALUE.                       ELSBEGIN
01672      EXEC CICS LINK                                               ELSBEGIN
01673         PROGRAM ('ELUCMIF')                                       ELSBEGIN
01674         COMMAREA (DFHCOMMAREA)                                    ELSBEGIN
01675      END-EXEC.                                                    ELSBEGIN
01676      IF CMF-RC-OK                                                 ELSBEGIN
01677         PERFORM PREPARE-MSG-FOR-OUTPUT.                           ELSBEGIN
01678                                                                   ELSBEGIN
01679 ************************************************************      ELSBEGIN
01680 *                                                          *      ELSBEGIN
01681 *        PREPARE MSG FOR OUTPUT                            *      ELSBEGIN
01682 *                                                          *      ELSBEGIN
01683 ************************************************************      ELSBEGIN
01684  PREPARE-MSG-FOR-OUTPUT.                                          ELSBEGIN
01685      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELSBEGIN
01686      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELSBEGIN
01687          ADDRESS OF CMF-DESCR.                                    ELSBEGIN
01688      SET CMF-DESCR-IDX TO 1.                                      ELSBEGIN
01689      MOVE 1 TO WS-MAP-SUB.                                        ELSBEGIN
01690      PERFORM VARYING CMF-DESCR-IDX                                ELSBEGIN
01691          FROM 1 BY 1 UNTIL                                        ELSBEGIN
01692          CMF-DESCR-IDX > CMF-NBR-DESCR-LINES OR                   ELSBEGIN
01693               (WS-MAP-SUB > 5)                                    ELSBEGIN
01694           MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                     ELSBEGIN
01695              TO MSGO (WS-MAP-SUB)                                 ELSBEGIN
01696           ADD 1 TO WS-MAP-SUB                                     ELSBEGIN
01697       END-PERFORM.                                                ELSBEGIN
