00001  IDENTIFICATION DIVISION.                                         06/29/02
00002                                                                   ELUKYSEL
00003  PROGRAM-ID.         ELUKYSEL.                                       LV001
00004                                                                   ELUKYSEL
00005  AUTHOR.             EDWARD G LISS                                ELUKYSEL
00006                                                                   ELUKYSEL
00007  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELUKYSEL
00008                      A MUTUAL LEGAL RESERVE COMPANY               ELUKYSEL
00009                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELUKYSEL
00010                      233 N. MICHIGAN AVE                          ELUKYSEL
00011                      CHICAGO, ILLINOIS 60601                      ELUKYSEL
00012                                                                   ELUKYSEL
00013  DATE-WRITTEN.       14-NOV-1986.                                 ELUKYSEL
00014                                                                   ELUKYSEL
00015  DATE-COMPILED.                                                   ELUKYSEL
00016                                                                   ELUKYSEL
00017  SECURITY.           COPYRIGHT 1986,                              ELUKYSEL
00018                      HEALTH CARE SERVICE CORPORATION              ELUKYSEL
00019      SKIP3                                                        ELUKYSEL
00020  ENVIRONMENT DIVISION.                                            ELUKYSEL
00021                                                                   ELUKYSEL
00022  CONFIGURATION SECTION.                                           ELUKYSEL
00023  SOURCE-COMPUTER.    IBM-3033.                                    ELUKYSEL
00024  OBJECT-COMPUTER.    IBM-3033.                                    ELUKYSEL
00025 /*****************************************************************ELUKYSEL
00026 *                                                                *ELUKYSEL
00027 *    PROGRAM:    ELUKYSEL                                        *ELUKYSEL
00028 *    DATE:       14-NOV-1986                                     *ELUKYSEL
00029 *    AUTHOR:     EDWARD G LISS                                   *ELUKYSEL
00030 *    FUNCTION:                                                   *ELUKYSEL
00031 *      THIS MODULE IS THE KEY SELECTOR MODULE.  IT DECIDES       *ELUKYSEL
00032 *      WHICH MODULE NEEDS TO BE CALLED TO COMPLETE THE KEY       *ELUKYSEL
00033 *      INFORMATION.                                              *ELUKYSEL
00034 *                                                                *ELUKYSEL
00035 *    NOTES:                                                      *ELUKYSEL
00036 *                                                                *ELUKYSEL
00037 ******************************************************************ELUKYSEL
00038 *                                                                *ELUKYSEL
00039 *                      MAINTENANCE HISTORY                       *ELUKYSEL
00040 *                                                                *ELUKYSEL
00041 *  MOD     DATE     BY  DRPT                ACTION               *ELUKYSEL
00042 * ----- ----------- --- ----- ---------------------------------- *ELUKYSEL
00043 * 01.00 14-NOV-1986 EGL       CREATED                            *ELUKYSEL
00044 * 01.01 06-FEB-1987 EGL       CORRECTED FAM. REL. VARIATION      *ELUKYSEL
00045 *                             TESTS.                             *ELUKYSEL
00046 * 01.02 14-OCT-1987 EGL       ADDED CHECKS TO ABORT QUERY IF THE *ELUKYSEL
00047 *                             SERVICE PERIOD IS NOT COVERED BY   *ELUKYSEL
00048 *                             GROUPS OR CONTRACTS                *ELUKYSEL
00049 * 01.03 12-JAN-1988 EGL       ADDED NEW SELECTOR STATES          *ELUKYSEL
00050 *                             SSB-SS-DATE-REJECT  AND            *ELUKYSEL
00051 *                             SSB-SS-SHOW-NOTICE                 *ELUKYSEL
00052 * 01.04 17-MAY-1988 EGL P5665 ADDED CODE TO REJECT CONTRACTS     *ELUKYSEL
00053 *                             WHICH CONFLICT WITH THE GROUP      *ELUKYSEL
00054 *                             SPECIFIC LINE OF BUSINESS.         *ELUKYSEL
00055 * 01.05 18-MAY-1988 EGL       ADDED CODE TO USE THE NEW STORAGE  *ELUKYSEL
00056 *                             MANAGEMENT ROUTINES.               *ELUKYSEL
00057 * 01.06 07-DEC-1988 EGL       ADDED NEW FAMILY RELATIONSHIP      *ELUKYSEL
00058 *                             LEVELS TO WS-FR-TRUTH-TABLE.       *ELUKYSEL
00059 *                             NEW CODES - F,G,H,I AND J.         *ELUKYSEL
00060 * 01.07 21-DEC-1988 EGL       CORRECTED LOGIC IN FAMILY RELATION-*ELUKYSEL
00061 *                             SHIP VARIATION DETERMINE LOGIC     *ELUKYSEL
00062 *                             TO ALLOW CODE 0 THRU 5 TO BE       *ELUKYSEL
00063 *                             CONSIDERED THE SAME.               *ELUKYSEL
00064 *                                                                *ELUKYSEL
00065 * 02.00 07-MAR-1989 NAC       STRUCTURE CONVERSION 3.5.          *ELUKYSEL
00066 * 02.01 13-MAR-1989 NAC P7768 DISPLAY PROF COVERAGE UNDER PROF   *ELUKYSEL
00067 *                             SUPPLEMENTAL EVEN THOUGH PROF BASIC*ELUKYSEL
00068 *                             DOES NOT EXIST.                    *ELUKYSEL
00069 * 02.02 28-JUN-1989 RJL       ADD FAMILY RELATIONSHIP CODES 'K'  *ELUKYSEL
00070 *                             AND 'L' TO SELECTION PROCESS.      *ELUKYSEL
00071 * 02.03 08-FEB-1990 EGL       ADD FAMILY RELATIONSHIP CODES 'P'  *ELUKYSEL
00072 *                             AND 'R' TO SELECTION PROCESS.      *ELUKYSEL
00073 * 02.04 14-MAY-1990 RJL       MOVE FINAL FAMILY RELATIONSHIP     *ELUKYSEL
00074 *                             VALUE TO SSB AFTER ALL VARIATIONS  *ELUKYSEL
00075 *                             PROCESSED.  MINOR LOGIC CLEANUP.   *ELUKYSEL
00076 * 02.05 09-NOV-1990 AKK       CHANGED '0' TO 'ZERO' WHEN COMPAR- *ELUKYSEL
00077 *                             ING KTG-PARTICIPAT-PROV-OPTION.    *ELUKYSEL
00078 *                             THIS IS DUE TO THE EXPANSION OF    *ELUKYSEL
00079 *                             THE ABOVE FIELD TO X(02).          *ELUKYSEL
00080 * 02.06 29-MAR-1991 GEM       ADDED FAMILY RELATIONSHIP CODES S, *ELUKYSEL
00081 *                             T, U, AND V, TO SELECTION PROCESS. *ELUKYSEL
00082 *                                                                *ELUKYSEL
00083 * 02.07 27-SEP-1991 RJL       CORRECTED FAMILY RELATIONSHIP      *ELUKYSEL
00084 *                             LEVEL CODES I AND J.               *ELUKYSEL
00085 *                                                                *ELUKYSEL
00086 * 02.08 23-SEP-1991 JPB       CHANGED REFERENCES TO FAMILY RELA- *ELUKYSEL
00087 *                             TIONSHIP FROM 1 TO 2 BYTES.        *ELUKYSEL
00088 * 02.09 01-OCT-1991 JPB       CHANGED WS-VALID-ITEM-DEFINITIONS  *ELUKYSEL
00089 *                             FROM PICTURE XXX TO PICTURE XXXX   *ELUKYSEL
00090 *                             AND ADDED LEADING ZERO TO VALUES.  *ELUKYSEL
00091 *                             ALSO CHANGED WS-FR-ARG FROM        *ELUKYSEL
00092 *                             PICTURE XX TO PICTURE XXX.         *ELUKYSEL
00093 * 02.10 02-OCT-1991 JPB       ADDED PC-FAM-REL-LVL-MEDIC PROGRAM *ELUKYSEL
00094 *                             CONSTANT.                          *ELUKYSEL
00095 * 02.11 16-DEC-1991 RJL       ADDED FAMILY RELATIONSHIP LEVELS   *ELUKYSEL
00096 *                             0W, 0X, 0Y AND 0Z.                 *ELUKYSEL
00097 * 02.12 24-JAN-1992 RJL       CORRECTED SIZE OF WS-FR-TABLE      *ELUKYSEL
00098 *                                                                *ELUKYSEL
00099 * 02.13 05-MAY-1992 BAK       ADD SUPPORT FOR MCN AND POS        *ELUKYSEL
00100 *                             NOTIFICATION SCREENS.              *ELUKYSEL
00101 *                                                                *ELUKYSEL
00102 * 02.14 06-JAN-1993 AKK       ADD SUPPORT FOR RPO NOTIFICATION   *ELUKYSEL
00103 *                             SCREENS.                           *ELUKYSEL
00104 *                                                                *ELUKYSEL
00105 * 02.15 03-FEB-1993 AKK       ADD SUPPORT FOR FAMILY RELATION-   *ELUKYSEL
00106 *                             SHIP LEVELS 10,11, 12, AND 13.     *ELUKYSEL
00107 *                                                                *ELUKYSEL
00108 * 02.16 18-OCT-1993 RGO     - ADD SUPPORT FOR FAMILY RELATION-   *ELUKYSEL
00109 *                             SHIP LEVELS 14 THRU 30.            *ELUKYSEL
00110 *                             THESE VALUES WERE ADDED TO THE     *ELUKYSEL
00111 *                             FR-TRUTH-TABLE.                    *ELUKYSEL
00112 *                           - LEVEL 22 - 26, 28 ARE FOR MEDICARE *ELUKYSEL
00113 *                             ONLY.  PROCESS JUST LIKE FRL '0M'. *ELUKYSEL
00114 *                             FIELD PC-FAM-REL-LVL-MEDIC WAS     *ELUKYSEL
00115 *                             REPLACED WITH WS-FRL-MEDICARE      *ELUKYSEL
00116 *                             AND AN 88 LEVEL WAS CREATED.       *ELUKYSEL
00117 *                                                                *ELUKYSEL
00118 * 02.16 10-FEB-1994 AKK       ADD SUPPORT FOR PRODUCT-TYPE       *ELUKYSEL
00119 *                             FOR NOTIFICATION SCREEN.           *ELUKYSEL
00120 *                                                                *ELUKYSEL
00121 * 02.17 19-APR-1994 AKK       ADD FAM-REL-LVL 31 32.             *ELUKYSEL
00122 *                                                                *ELUKYSEL
00123 * 02.18 70-MAR-1995 AKK       ADD SUPPORT FOR CPO                *ELUKYSEL
00124 *                             FOR NOTIFICATION SCREEN.           *ELUKYSEL
00125 *                                                                *ELUKYSEL
00126 * 02.19 23-MAY-1995 AKK       ADD SUPPORT FOR BLUE SCRIPT        *ELUKYSEL
00127 *                             FOR NOTIFICATION SCREEN.           *ELUKYSEL
00128 *                                                                *ELUKYSEL
00129 * 02.20 13-SEP-1995 AKK       ADD SUPPORT FOR ALLIANCE           *ELUKYSEL
00130 *                             FOR NOTIFICATION SCREEN.           *ELUKYSEL
00131 *                                                                *ELUKYSEL
00132 * 02.21 12-FEB-1996 AKK       ERRORED IN ADDING SUPPORT          *ELUKYSEL
00133 *                             ALLIANCE AND NOTIFCATION SCREEN.   *ELUKYSEL
00134 *                                                                *ELUKYSEL
00135 * 02.22 13-MAR-1996 AKK       ADDED SUPPORT FOR CBL AND          *ELUKYSEL
00136 *                             PAN FOR NOTIFCATION SCREEN.        *ELUKYSEL
00137 *                                                                *ELUKYSEL
00138 * 02.23 08-OCT-1997 AKK       ADDED SUPPORT YR2000 AND           *ELUKYSEL
00139 *                             TEXAS MERGER.                      *ELUKYSEL
00140 *                                                                *ELUKYSEL
00141 * 03.00 27-APR-2002 AKK       MOVED ASSIGN CLAUSE TO AREA B      *ELUKYSEL
00142 *                             OS390  CHANGE.                     *ELUKYSEL
00143 *                                                                *ELUKYSEL
00144 * 03.01 15-MAY-2002 AKK       PROD ABENDS DUE TO NO MATCH ON     *ELUKYSEL
00145 *                             FRLS.                              *ELUKYSEL
00146 *                                                                *ELUKYSEL
00144 *       04-DEC-2017 SRI       ADD FAMILY RELATIONSHIP VALUES     *ELUKYSEL
00145 *                             0N 39 40 41 42 43 44 45 46 47      *ELUKYSEL
00147 ******************************************************************ELUKYSEL
00148 *                                                                 ELUKYSEL
00149 *  WHEN FAMILY RELATIONSHIP VALUES ARE ADDED HERE THEY MUST ALSO *ELUKYSEL
00150 *  BE ADDED TO ELTWAITP AS WELL.                                 *ELUKYSEL
00151 ******************************************************************ELUKYSEL
00152 /                                                                 ELUKYSEL
00153  DATA DIVISION.                                                   ELUKYSEL
00154                                                                   ELUKYSEL
00155  WORKING-STORAGE SECTION.                                         ELUKYSEL
00156                                                                   ELUKYSEL
00157  77  HOLD-INDEX                 PIC S9(04)   COMP.                ELUKYSEL
00158                                                                   ELUKYSEL
00159  01  WS-APPLID.                                                   ELUKYSEL
00160      02 FILLER                   PIC X(03).                       ELUKYSEL
00161      02 FILLER                   PIC X(04).                       ELUKYSEL
00162         88 TEXAS-REGION          VALUES                           ELUKYSEL
00163         'XAI1' 'XAI2' 'XAB1' 'XAB2' 'XAB3' 'XAB4' 'XAB5'          ELUKYSEL
00164         'XAB6' 'XAB7' 'XAB8' 'XAB9' 'XAS1' 'XAS2' 'XFB1'          ELUKYSEL
00165         'XFB2' 'XF01'.                                            ELUKYSEL
00166                                                                   ELUKYSEL
00167  01  MISC-WORKING-STORAGE.                                        ELUKYSEL
00168      05  WS-FRL-MEDICARE        PICTURE XX.                       ELUKYSEL
00169          88  WS-FRL-MEDIC-ONLY             VALUE '0M' '22' '23'   ELUKYSEL
00170                                                  '24' '25' '26'.  ELUKYSEL
00171      05  WS-ITEM-REJECT-SW      PIC X      VALUE 'N'.             ELUKYSEL
00172          88  WS-ITEM-REJECTED              VALUE 'Y'.             ELUKYSEL
00173          88  WS-NO-ITEM-REJECTED           VALUE 'N'.             ELUKYSEL
00174                                                                   ELUKYSEL
00175      05  WS-ALLIANCE-NOT-DONE-SW  PIC X        VALUE 'N'.         ELUKYSEL
00176       88  WS-ALLIANCE-NOT-DONE                 VALUE 'N'.         ELUKYSEL
00177       88  WS-ALLIANCE-DONE                     VALUE 'Y'.         ELUKYSEL
00178                                                                   ELUKYSEL
00179      05  WS-ACCEPTED-CONT-SW    PICTURE X  VALUE 'N'.             ELUKYSEL
00180          88  WS-SEL-CONTRACT-FOUND         VALUE 'Y'.             ELUKYSEL
00181          88  WS-ALL-CONTRACTS-REJ          VALUE 'N'.             ELUKYSEL
00182                                                                   ELUKYSEL
00183      05  WS-SEARCH-STATUS-SW    PICTURE X  VALUE 'N'.             ELUKYSEL
00184          88  WS-SEARCH-FAILED              VALUE 'Y'.             ELUKYSEL
00185          88  WS-SEARCH-SUCCESSFULL         VALUE 'N'.             ELUKYSEL
00186                                                                   ELUKYSEL
00187      05  WS-DATA-ITEM-FOUND-SW  PICTURE X  VALUE 'N'.             ELUKYSEL
00188          88  WS-DATA-ITEM-FOUND            VALUE 'Y'.             ELUKYSEL
00189          88  WS-DATA-ITEM-NOT-FOUND        VALUE 'N'.             ELUKYSEL
00190                                                                   ELUKYSEL
00191      05  WS-GRP-EFF-DT-VAR-SW   PICTURE X  VALUE 'N'.             ELUKYSEL
00192          88  WS-GRP-EFF-DT-VAR             VALUE 'Y'.             ELUKYSEL
00193          88  WS-GRP-EFF-DT-NVAR            VALUE 'N'.             ELUKYSEL
00194                                                                   ELUKYSEL
00195      05  WS-MEDCA-FOUND-SW      PICTURE X  VALUE 'N'.             ELUKYSEL
00196          88  WS-MEDCA-FOUND                VALUE 'Y'.             ELUKYSEL
00197          88  WS-NO-MEDCA-FOUND             VALUE 'N'.             ELUKYSEL
00198                                                                   ELUKYSEL
00199      05  WS-NON-MEDCA-FOUND-SW  PICTURE X  VALUE 'N'.             ELUKYSEL
00200          88  WS-NON-MEDCA-FOUND            VALUE 'Y'.             ELUKYSEL
00201          88  WS-NO-NON-MEDCA-FOUND         VALUE 'N'.             ELUKYSEL
00202                                                                   ELUKYSEL
00203      05  WS-FM-RL-FOUND-SW      PICTURE X  VALUE 'N'.             ELUKYSEL
00204          88  WS-FM-RL-FOUND                VALUE 'Y'.             ELUKYSEL
00205          88  WS-NO-FM-RL-FOUND             VALUE 'N'.             ELUKYSEL
00206                                                                   ELUKYSEL
00207      05  WS-NON-FM-RL-FOUND-SW  PICTURE X  VALUE 'N'.             ELUKYSEL
00208          88  WS-NON-FM-RL-FOUND            VALUE 'Y'.             ELUKYSEL
00209          88  WS-NO-NON-FM-RL-FOUND         VALUE 'N'.             ELUKYSEL
00210                                                                   ELUKYSEL
00211      05  WS-PT-AG-FOUND-SW      PICTURE X  VALUE 'N'.             ELUKYSEL
00212          88  WS-PT-AG-FOUND                VALUE 'Y'.             ELUKYSEL
00213          88  WS-NO-PT-AG-FOUND             VALUE 'N'.             ELUKYSEL
00214                                                                   ELUKYSEL
00215      05  WS-NON-PT-AG-FOUND-SW  PICTURE X  VALUE 'N'.             ELUKYSEL
00216          88  WS-NON-PT-AG-FOUND            VALUE 'Y'.             ELUKYSEL
00217          88  WS-NO-NON-PT-AG-FOUND         VALUE 'N'.             ELUKYSEL
00218                                                                   ELUKYSEL
00219      05  WS-PT-AGE-TEST-1       PICTURE XX VALUE SPACE.           ELUKYSEL
00220          88  WS-PT-AGE-SAME-1              VALUE '00' THRU '05'.  ELUKYSEL
00221                                                                   ELUKYSEL
00222      05  WS-PT-AGE-TEST-2       PICTURE XX VALUE SPACE.           ELUKYSEL
00223          88  WS-PT-AGE-SAME-2              VALUE '00' THRU '05'.  ELUKYSEL
00224                                                                   ELUKYSEL
00225      05  WS-FR-FOUND-LIST       PICTURE XX VALUE SPACE.           ELUKYSEL
00226          88  WS-FR-FND-LIST                VALUE '0A' '0S' '0T'   ELUKYSEL
00227                                                  '0U' '0V' '0W'   ELUKYSEL
00228                                                  '0X' '0Y' '0X'   ELUKYSEL
00229                                                  '00' '09'.       ELUKYSEL
00230                                                                   ELUKYSEL
00231      05  WS-PC-VAR-SW           PICTURE X  VALUE 'N'.             ELUKYSEL
00232          88  WS-PC-VAR                     VALUE 'Y'.             ELUKYSEL
00233          88  WS-NO-PC-VAR                  VALUE 'N'.             ELUKYSEL
00234                                                                   ELUKYSEL
00235      05  WS-EFF-DATE-VAR-SW     PICTURE X  VALUE 'N'.             ELUKYSEL
00236          88  WS-EFF-DATE-VAR               VALUE 'Y'.             ELUKYSEL
00237          88  WS-NO-EFF-DATE-VAR            VALUE 'N'.             ELUKYSEL
00238                                                                   ELUKYSEL
00239      05  WS-KTC-MODIFIED-SW     PICTURE X  VALUE 'N'.             ELUKYSEL
00240          88  WS-KTC-MODIFIED               VALUE 'Y'.             ELUKYSEL
00241          88  WS-KTC-NOT-MODIFIED           VALUE 'N'.             ELUKYSEL
00242                                                                   ELUKYSEL
00243      05  WS-KTG-MODIFIED-SW     PICTURE X  VALUE 'N'.             ELUKYSEL
00244          88  WS-KTG-MODIFIED               VALUE 'Y'.             ELUKYSEL
00245          88  WS-KTG-NOT-MODIFIED           VALUE 'N'.             ELUKYSEL
00246                                                                   ELUKYSEL
00247      05  WS-MATCH-FOUND-SW      PICTURE X  VALUE 'N'.             ELUKYSEL
00248          88  WS-MATCH-FOUND                VALUE 'Y'.             ELUKYSEL
00249          88  WS-MATCH-NOT-FOUND            VALUE 'N'.             ELUKYSEL
00250                                                                   ELUKYSEL
00251      05  WS-PC-INDEX-FOUND-SW   PICTURE X  VALUE 'N'.             ELUKYSEL
00252          88  WS-PC-INDEX-FOUND             VALUE 'Y'.             ELUKYSEL
00253          88  WS-PC-INDEX-NOT-FOUND         VALUE 'N'.             ELUKYSEL
00254                                                                   ELUKYSEL
00255      05  WS-LOB-INDEX-FOUND-SW  PICTURE X  VALUE 'N'.             ELUKYSEL
00256          88  WS-LOB-INDEX-FOUND            VALUE 'Y'.             ELUKYSEL
00257          88  WS-LOB-INDEX-NOT-FOUND        VALUE 'N'.             ELUKYSEL
00258                                                                   ELUKYSEL
00259      05  WS-KTG-FOUND-SW        PICTURE X  VALUE 'N'.             ELUKYSEL
00260          88  WS-KTG-FOUND                  VALUE 'Y'.             ELUKYSEL
00261          88  WS-KTG-NOT-FOUND              VALUE 'N'.             ELUKYSEL
00262                                                                   ELUKYSEL
00263      05  WS-L-O-B               PICTURE X.                        ELUKYSEL
00264          88  WS-L-O-B-INST-BAS             VALUE '1'.             ELUKYSEL
00265          88  WS-L-O-B-PROF-BAS             VALUE '2'.             ELUKYSEL
00266          88  WS-L-O-B-SUP                  VALUE '3'.             ELUKYSEL
00267          88  WS-L-O-B-COMP                 VALUE '4'.             ELUKYSEL
00268                                                                   ELUKYSEL
00269      05  WS-L-O-B-CONFLICT-SW   PICTURE X  VALUE 'N'.             ELUKYSEL
00270          88  WS-L-O-B-CONFLICT             VALUE 'Y'.             ELUKYSEL
00271          88  WS-L-O-B-MATCH                VALUE 'N'.             ELUKYSEL
00272                                                                   ELUKYSEL
00273      05  WS-NOTE-VAR-SW          PICTURE X.                       ELUKYSEL
00274          88  WS-NOTE-VAR-FOUND              VALUE 'Y'.            ELUKYSEL
00275          88  WS-NO-NOTE-VAR-FOUND           VALUE 'N'.            ELUKYSEL
00276                                                                   ELUKYSEL
00277      05  WS-EFF-DT              PICTURE S9(7) COMP-3 VALUE ZERO.  ELUKYSEL
00278      05  WS-TERMN-DT            PICTURE S9(7) COMP-3 VALUE ZERO.  ELUKYSEL
00279      05  WS-PROVIDER-CONTROL    PICTURE X(2)  VALUE LOW-VALUE.    ELUKYSEL
00280 /                                                                 ELUKYSEL
00281 *     TRUTH TABLE FOR LINE OF BUSINESS CONFLICTS                  ELUKYSEL
00282 *                                                                 ELUKYSEL
00283 *     EACH ITEM CONSISTS OF:                                      ELUKYSEL
00284 *     - GROUP SPEC L-O-B CONTRACT LEVEL IND.                      ELUKYSEL
00285 *     - CONTRACT L-O-B                                            ELUKYSEL
00286 *     - INSTITUTIONAL BASIC ACCEPT/REJECT IND.                    ELUKYSEL
00287 *     - INSTITUTIONAL SUPPL ACCEPT/REJECT IND.                    ELUKYSEL
00288 *     - INSTITUTIONAL BASIC ACCEPT/REJECT IND.                    ELUKYSEL
00289 *     - INSTITUTIONAL SUPPL ACCEPT/REJECT IND.                    ELUKYSEL
00290 *                                                                 ELUKYSEL
00291 *     A SEARCH KEY IS BUILT AND THE TABLE SEARCHED. THE           ELUKYSEL
00292 *     RESULT IS AN ACCEPT/REJECT FLAG.                            ELUKYSEL
00293 *     IF NOT MATCH RESULTS, A TOTAL CONFLICT EXISTS AND           ELUKYSEL
00294 *     THE CONTRACT SHOULD BE TOTALY REJECTED                      ELUKYSEL
00295 *                                                                 ELUKYSEL
00296  01  WS-L-O-B-TABLE-AREA.                                         ELUKYSEL
00297      05  WS-L-O-B-FUNC.                                           ELUKYSEL
00298          10  WS-L-O-B-CONTRACT-LEVEL PICTURE XX.                  ELUKYSEL
00299          10  WS-L-O-B-IND            PICTURE X.                   ELUKYSEL
00300      05  WS-L-O-B-TABLE-DEFINITION.                               ELUKYSEL
00301          10  FILLER        PICTURE X(7) VALUE '011ARRR'.          ELUKYSEL
00302          10  FILLER        PICTURE X(7) VALUE '021ARAR'.          ELUKYSEL
00303          10  FILLER        PICTURE X(7) VALUE '022ARAR'.          ELUKYSEL
00304          10  FILLER        PICTURE X(7) VALUE '031AAAA'.          ELUKYSEL
00305          10  FILLER        PICTURE X(7) VALUE '032AAAA'.          ELUKYSEL
00306          10  FILLER        PICTURE X(7) VALUE '033AAAA'.          ELUKYSEL
00307          10  FILLER        PICTURE X(7) VALUE '041AARR'.          ELUKYSEL
00308          10  FILLER        PICTURE X(7) VALUE '043AARA'.          ELUKYSEL
00309          10  FILLER        PICTURE X(7) VALUE '052RRAA'.          ELUKYSEL
00310          10  FILLER        PICTURE X(7) VALUE '062RRAA'.          ELUKYSEL
00311          10  FILLER        PICTURE X(7) VALUE '063RAAA'.          ELUKYSEL
00312          10  FILLER        PICTURE X(7) VALUE '074AAAA'.          ELUKYSEL
00313          10  FILLER        PICTURE X(7) VALUE '083RARA'.          ELUKYSEL
00314      05  WS-L-O-B-TABLE  REDEFINES WS-L-O-B-TABLE-DEFINITION      ELUKYSEL
00315                                    OCCURS 13 TIMES                ELUKYSEL
00316                                    ASCENDING KEY WS-L-O-B-KEY     ELUKYSEL
00317                                    INDEXED BY WS-L-O-B-INDEX.     ELUKYSEL
00318          10  WS-L-O-B-KEY       PICTURE X(3).                     ELUKYSEL
00319          10  WS-INST-BAS-ST     PICTURE X.                        ELUKYSEL
00320              88  WS-INST-BAS-REJECT  VALUE 'R'.                   ELUKYSEL
00321              88  WS-INST-BAS-ACCEPT  VALUE 'A'.                   ELUKYSEL
00322          10  WS-INST-SUP-ST     PICTURE X.                        ELUKYSEL
00323              88  WS-INST-SUP-REJECT  VALUE 'R'.                   ELUKYSEL
00324              88  WS-INST-SUP-ACCEPT  VALUE 'A'.                   ELUKYSEL
00325          10  WS-PROF-BAS-ST     PICTURE X.                        ELUKYSEL
00326              88  WS-PROF-BAS-REJECT  VALUE 'R'.                   ELUKYSEL
00327              88  WS-PROF-BAS-ACCEPT  VALUE 'A'.                   ELUKYSEL
00328          10  WS-PROF-SUP-ST     PICTURE X.                        ELUKYSEL
00329              88  WS-PROF-SUP-REJECT  VALUE 'R'.                   ELUKYSEL
00330              88  WS-PROF-SUP-ACCEPT  VALUE 'A'.                   ELUKYSEL
00331 /                                                                 ELUKYSEL
00332 *     TRUTH TABLE FOR RESOLVING FAMILY RELATION SHIPS             ELUKYSEL
00333 *                                                                 ELUKYSEL
00334 *     EACH ITEM HAS A 3-CHARACTER KEY - THE FAMILY                ELUKYSEL
00335 *     RELATIONSHIP LEVEL FROM THE GROUP AND/OR CONTRACT           ELUKYSEL
00336 *     TABLES AND THE FAMILY RELATIONSHIP (MEMBER, SPOUSE OR       ELUKYSEL
00337 *     DEPENDENT) ENTERED BY THE USER.                             ELUKYSEL
00338 *     A SEARCH KEY IS BUILT AND THE TABLE SEARCHED. THE           ELUKYSEL
00339 *     RESULT IS AN ACCEPT/REJECT FLAG.                            ELUKYSEL
00340 *                                                                 ELUKYSEL
00341 *     WHEN FAMILY RELATIONSHIP VALUES ARE ADDED TO OR             ELUKYSEL
00342 *     CHANGED IN THIS TABLE, THE CORRESPONDING CHANGE             ELUKYSEL
00343 *     MUST ALSO BE MADE TO ELTWAITP IN THE                        ELUKYSEL
00344 *     WS-FAMILY-RELATION-LEVEL CONDITIONS (88 LEVELS).            ELUKYSEL
00345 *                                                                 ELUKYSEL
00346  01  WS-FR-TRUTH-TABLE.                                           ELUKYSEL
00347      05  WS-FR-KEY.                                               ELUKYSEL
00348          10  WS-FAM-REL-LVL     PICTURE XX.                       ELUKYSEL
00349          10  WS-FAM-REL         PICTURE  X.                       ELUKYSEL
00350      05  WS-FR-VALID-ITEM-DEFINITION.                             ELUKYSEL
00351          10  FILLER        PICTURE XXXX  VALUE '0ADA'.            ELUKYSEL
00352          10  FILLER        PICTURE XXXX  VALUE '0AMA'.            ELUKYSEL
00353          10  FILLER        PICTURE XXXX  VALUE '0ASA'.            ELUKYSEL
00354          10  FILLER        PICTURE XXXX  VALUE '0BDA'.            ELUKYSEL
00355          10  FILLER        PICTURE XXXX  VALUE '0BMA'.            ELUKYSEL
00356          10  FILLER        PICTURE XXXX  VALUE '0BSA'.            ELUKYSEL
00357          10  FILLER        PICTURE XXXX  VALUE '0CDA'.            ELUKYSEL
00358          10  FILLER        PICTURE XXXX  VALUE '0CMR'.            ELUKYSEL
00359          10  FILLER        PICTURE XXXX  VALUE '0CSR'.            ELUKYSEL
00360          10  FILLER        PICTURE XXXX  VALUE '0DDA'.            ELUKYSEL
00361          10  FILLER        PICTURE XXXX  VALUE '0DMA'.            ELUKYSEL
00362          10  FILLER        PICTURE XXXX  VALUE '0DSA'.            ELUKYSEL
00363          10  FILLER        PICTURE XXXX  VALUE '0EDA'.            ELUKYSEL
00364          10  FILLER        PICTURE XXXX  VALUE '0EMR'.            ELUKYSEL
00365          10  FILLER        PICTURE XXXX  VALUE '0ESR'.            ELUKYSEL
00366          10  FILLER        PICTURE XXXX  VALUE '0FDA'.            ELUKYSEL
00367          10  FILLER        PICTURE XXXX  VALUE '0FMA'.            ELUKYSEL
00368          10  FILLER        PICTURE XXXX  VALUE '0FSA'.            ELUKYSEL
00369          10  FILLER        PICTURE XXXX  VALUE '0GDA'.            ELUKYSEL
00370          10  FILLER        PICTURE XXXX  VALUE '0GMR'.            ELUKYSEL
00371          10  FILLER        PICTURE XXXX  VALUE '0GSR'.            ELUKYSEL
00372          10  FILLER        PICTURE XXXX  VALUE '0HDA'.            ELUKYSEL
00373          10  FILLER        PICTURE XXXX  VALUE '0HMR'.            ELUKYSEL
00374          10  FILLER        PICTURE XXXX  VALUE '0HSR'.            ELUKYSEL
00375          10  FILLER        PICTURE XXXX  VALUE '0IDR'.            ELUKYSEL
00376          10  FILLER        PICTURE XXXX  VALUE '0IMA'.            ELUKYSEL
00377          10  FILLER        PICTURE XXXX  VALUE '0ISR'.            ELUKYSEL
00378          10  FILLER        PICTURE XXXX  VALUE '0JDR'.            ELUKYSEL
00379          10  FILLER        PICTURE XXXX  VALUE '0JMA'.            ELUKYSEL
00380          10  FILLER        PICTURE XXXX  VALUE '0JSR'.            ELUKYSEL
00381          10  FILLER        PICTURE XXXX  VALUE '0KDA'.            ELUKYSEL
00382          10  FILLER        PICTURE XXXX  VALUE '0KMA'.            ELUKYSEL
00383          10  FILLER        PICTURE XXXX  VALUE '0KSA'.            ELUKYSEL
00384          10  FILLER        PICTURE XXXX  VALUE '0LDA'.            ELUKYSEL
00385          10  FILLER        PICTURE XXXX  VALUE '0LMR'.            ELUKYSEL
00386          10  FILLER        PICTURE XXXX  VALUE '0LSR'.            ELUKYSEL
00387          10  FILLER        PICTURE XXXX  VALUE '0MDR'.            ELUKYSEL
00388          10  FILLER        PICTURE XXXX  VALUE '0MMR'.            ELUKYSEL
00389          10  FILLER        PICTURE XXXX  VALUE '0MSR'.            ELUKYSEL
00389          10  FILLER        PICTURE XXXX  VALUE '0NDA'.            ELUKYSEL
00389          10  FILLER        PICTURE XXXX  VALUE '0NMA'.            ELUKYSEL
00389          10  FILLER        PICTURE XXXX  VALUE '0NSA'.            ELUKYSEL
00390          10  FILLER        PICTURE XXXX  VALUE '0PDA'.            ELUKYSEL
00391          10  FILLER        PICTURE XXXX  VALUE '0PMR'.            ELUKYSEL
00392          10  FILLER        PICTURE XXXX  VALUE '0PSR'.            ELUKYSEL
00393          10  FILLER        PICTURE XXXX  VALUE '0RDA'.            ELUKYSEL
00394          10  FILLER        PICTURE XXXX  VALUE '0RMA'.            ELUKYSEL
00395          10  FILLER        PICTURE XXXX  VALUE '0RSA'.            ELUKYSEL
00396          10  FILLER        PICTURE XXXX  VALUE '0SDA'.            ELUKYSEL
00397          10  FILLER        PICTURE XXXX  VALUE '0SMA'.            ELUKYSEL
00398          10  FILLER        PICTURE XXXX  VALUE '0SSA'.            ELUKYSEL
00399          10  FILLER        PICTURE XXXX  VALUE '0TDA'.            ELUKYSEL
00400          10  FILLER        PICTURE XXXX  VALUE '0TMA'.            ELUKYSEL
00401          10  FILLER        PICTURE XXXX  VALUE '0TSA'.            ELUKYSEL
00402          10  FILLER        PICTURE XXXX  VALUE '0UDA'.            ELUKYSEL
00403          10  FILLER        PICTURE XXXX  VALUE '0UMA'.            ELUKYSEL
00404          10  FILLER        PICTURE XXXX  VALUE '0USA'.            ELUKYSEL
00405          10  FILLER        PICTURE XXXX  VALUE '0VDA'.            ELUKYSEL
00406          10  FILLER        PICTURE XXXX  VALUE '0VMA'.            ELUKYSEL
00407          10  FILLER        PICTURE XXXX  VALUE '0VSA'.            ELUKYSEL
00408          10  FILLER        PICTURE XXXX  VALUE '0WDA'.            ELUKYSEL
00409          10  FILLER        PICTURE XXXX  VALUE '0WMA'.            ELUKYSEL
00410          10  FILLER        PICTURE XXXX  VALUE '0WSA'.            ELUKYSEL
00411          10  FILLER        PICTURE XXXX  VALUE '0XDA'.            ELUKYSEL
00412          10  FILLER        PICTURE XXXX  VALUE '0XMA'.            ELUKYSEL
00413          10  FILLER        PICTURE XXXX  VALUE '0XSA'.            ELUKYSEL
00414          10  FILLER        PICTURE XXXX  VALUE '0YDA'.            ELUKYSEL
00415          10  FILLER        PICTURE XXXX  VALUE '0YMA'.            ELUKYSEL
00416          10  FILLER        PICTURE XXXX  VALUE '0YSA'.            ELUKYSEL
00417          10  FILLER        PICTURE XXXX  VALUE '0ZDA'.            ELUKYSEL
00418          10  FILLER        PICTURE XXXX  VALUE '0ZMA'.            ELUKYSEL
00419          10  FILLER        PICTURE XXXX  VALUE '0ZSA'.            ELUKYSEL
00420          10  FILLER        PICTURE XXXX  VALUE '00DA'.            ELUKYSEL
00421          10  FILLER        PICTURE XXXX  VALUE '00MA'.            ELUKYSEL
00422          10  FILLER        PICTURE XXXX  VALUE '00SA'.            ELUKYSEL
00423          10  FILLER        PICTURE XXXX  VALUE '01DR'.            ELUKYSEL
00424          10  FILLER        PICTURE XXXX  VALUE '01MA'.            ELUKYSEL
00425          10  FILLER        PICTURE XXXX  VALUE '01SR'.            ELUKYSEL
00426          10  FILLER        PICTURE XXXX  VALUE '02DR'.            ELUKYSEL
00427          10  FILLER        PICTURE XXXX  VALUE '02MR'.            ELUKYSEL
00428          10  FILLER        PICTURE XXXX  VALUE '02SA'.            ELUKYSEL
00429          10  FILLER        PICTURE XXXX  VALUE '03DR'.            ELUKYSEL
00430          10  FILLER        PICTURE XXXX  VALUE '03MA'.            ELUKYSEL
00431          10  FILLER        PICTURE XXXX  VALUE '03SA'.            ELUKYSEL
00432          10  FILLER        PICTURE XXXX  VALUE '04DA'.            ELUKYSEL
00433          10  FILLER        PICTURE XXXX  VALUE '04MR'.            ELUKYSEL
00434          10  FILLER        PICTURE XXXX  VALUE '04SR'.            ELUKYSEL
00435          10  FILLER        PICTURE XXXX  VALUE '05DA'.            ELUKYSEL
00436          10  FILLER        PICTURE XXXX  VALUE '05MR'.            ELUKYSEL
00437          10  FILLER        PICTURE XXXX  VALUE '05SA'.            ELUKYSEL
00438          10  FILLER        PICTURE XXXX  VALUE '06DA'.            ELUKYSEL
00439          10  FILLER        PICTURE XXXX  VALUE '06MA'.            ELUKYSEL
00440          10  FILLER        PICTURE XXXX  VALUE '06SA'.            ELUKYSEL
00441          10  FILLER        PICTURE XXXX  VALUE '07DA'.            ELUKYSEL
00442          10  FILLER        PICTURE XXXX  VALUE '07MA'.            ELUKYSEL
00443          10  FILLER        PICTURE XXXX  VALUE '07SA'.            ELUKYSEL
00444          10  FILLER        PICTURE XXXX  VALUE '08DA'.            ELUKYSEL
00445          10  FILLER        PICTURE XXXX  VALUE '08MR'.            ELUKYSEL
00446          10  FILLER        PICTURE XXXX  VALUE '08SA'.            ELUKYSEL
00447          10  FILLER        PICTURE XXXX  VALUE '09DA'.            ELUKYSEL
00448          10  FILLER        PICTURE XXXX  VALUE '09MA'.            ELUKYSEL
00449          10  FILLER        PICTURE XXXX  VALUE '09SA'.            ELUKYSEL
00450          10  FILLER        PICTURE XXXX  VALUE '10DR'.            ELUKYSEL
00451          10  FILLER        PICTURE XXXX  VALUE '10MA'.            ELUKYSEL
00452          10  FILLER        PICTURE XXXX  VALUE '10SR'.            ELUKYSEL
00453          10  FILLER        PICTURE XXXX  VALUE '11DR'.            ELUKYSEL
00454          10  FILLER        PICTURE XXXX  VALUE '11MA'.            ELUKYSEL
00455          10  FILLER        PICTURE XXXX  VALUE '11SR'.            ELUKYSEL
00456          10  FILLER        PICTURE XXXX  VALUE '12DA'.            ELUKYSEL
00457          10  FILLER        PICTURE XXXX  VALUE '12MR'.            ELUKYSEL
00458          10  FILLER        PICTURE XXXX  VALUE '12SA'.            ELUKYSEL
00459          10  FILLER        PICTURE XXXX  VALUE '13DA'.            ELUKYSEL
00460          10  FILLER        PICTURE XXXX  VALUE '13MR'.            ELUKYSEL
00461          10  FILLER        PICTURE XXXX  VALUE '13SA'.            ELUKYSEL
00462          10  FILLER        PIC     XXXX  VALUE '14DA'.            ELUKYSEL
00463          10  FILLER        PIC     XXXX  VALUE '14MA'.            ELUKYSEL
00464          10  FILLER        PIC     XXXX  VALUE '14SA'.            ELUKYSEL
00465          10  FILLER        PIC     XXXX  VALUE '15DA'.            ELUKYSEL
00466          10  FILLER        PIC     XXXX  VALUE '15MA'.            ELUKYSEL
00467          10  FILLER        PIC     XXXX  VALUE '15SA'.            ELUKYSEL
00468          10  FILLER        PIC     XXXX  VALUE '16DA'.            ELUKYSEL
00469          10  FILLER        PIC     XXXX  VALUE '16MA'.            ELUKYSEL
00470          10  FILLER        PIC     XXXX  VALUE '16SA'.            ELUKYSEL
00471          10  FILLER        PIC     XXXX  VALUE '17DA'.            ELUKYSEL
00472          10  FILLER        PIC     XXXX  VALUE '17MA'.            ELUKYSEL
00473          10  FILLER        PIC     XXXX  VALUE '17SA'.            ELUKYSEL
00474          10  FILLER        PIC     XXXX  VALUE '18DA'.            ELUKYSEL
00475          10  FILLER        PIC     XXXX  VALUE '18MA'.            ELUKYSEL
00476          10  FILLER        PIC     XXXX  VALUE '18SA'.            ELUKYSEL
00477          10  FILLER        PIC     XXXX  VALUE '19DA'.            ELUKYSEL
00478          10  FILLER        PIC     XXXX  VALUE '19MA'.            ELUKYSEL
00479          10  FILLER        PIC     XXXX  VALUE '19SA'.            ELUKYSEL
00480          10  FILLER        PIC     XXXX  VALUE '20DA'.            ELUKYSEL
00481          10  FILLER        PIC     XXXX  VALUE '20MA'.            ELUKYSEL
00482          10  FILLER        PIC     XXXX  VALUE '20SA'.            ELUKYSEL
00483          10  FILLER        PIC     XXXX  VALUE '21DA'.            ELUKYSEL
00484          10  FILLER        PIC     XXXX  VALUE '21MA'.            ELUKYSEL
00485          10  FILLER        PIC     XXXX  VALUE '21SA'.            ELUKYSEL
00486          10  FILLER        PIC     XXXX  VALUE '22DA'.            ELUKYSEL
00487          10  FILLER        PIC     XXXX  VALUE '22MA'.            ELUKYSEL
00488          10  FILLER        PIC     XXXX  VALUE '22SA'.            ELUKYSEL
00489          10  FILLER        PIC     XXXX  VALUE '23DA'.            ELUKYSEL
00490          10  FILLER        PIC     XXXX  VALUE '23MA'.            ELUKYSEL
00491          10  FILLER        PIC     XXXX  VALUE '23SA'.            ELUKYSEL
00492          10  FILLER        PIC     XXXX  VALUE '24DA'.            ELUKYSEL
00493          10  FILLER        PIC     XXXX  VALUE '24MA'.            ELUKYSEL
00494          10  FILLER        PIC     XXXX  VALUE '24SA'.            ELUKYSEL
00495          10  FILLER        PIC     XXXX  VALUE '25DA'.            ELUKYSEL
00496          10  FILLER        PIC     XXXX  VALUE '25MA'.            ELUKYSEL
00497          10  FILLER        PIC     XXXX  VALUE '25SA'.            ELUKYSEL
00498          10  FILLER        PIC     XXXX  VALUE '26DA'.            ELUKYSEL
00499          10  FILLER        PIC     XXXX  VALUE '26MA'.            ELUKYSEL
00500          10  FILLER        PIC     XXXX  VALUE '26SA'.            ELUKYSEL
00501          10  FILLER        PIC     XXXX  VALUE '27DA'.            ELUKYSEL
00502          10  FILLER        PIC     XXXX  VALUE '27MA'.            ELUKYSEL
00503          10  FILLER        PIC     XXXX  VALUE '27SA'.            ELUKYSEL
00504          10  FILLER        PIC     XXXX  VALUE '28DA'.            ELUKYSEL
00505          10  FILLER        PIC     XXXX  VALUE '28MA'.            ELUKYSEL
00506          10  FILLER        PIC     XXXX  VALUE '28SA'.            ELUKYSEL
00507          10  FILLER        PIC     XXXX  VALUE '29DA'.            ELUKYSEL
00508          10  FILLER        PIC     XXXX  VALUE '29MA'.            ELUKYSEL
00509          10  FILLER        PIC     XXXX  VALUE '29SA'.            ELUKYSEL
00510          10  FILLER        PIC     XXXX  VALUE '30DA'.            ELUKYSEL
00511          10  FILLER        PIC     XXXX  VALUE '30MA'.            ELUKYSEL
00512          10  FILLER        PIC     XXXX  VALUE '30SA'.            ELUKYSEL
00513          10  FILLER        PIC     XXXX  VALUE '31DA'.            ELUKYSEL
00514          10  FILLER        PIC     XXXX  VALUE '31MA'.            ELUKYSEL
00515          10  FILLER        PIC     XXXX  VALUE '31SA'.            ELUKYSEL
00516          10  FILLER        PIC     XXXX  VALUE '32DA'.            ELUKYSEL
00517          10  FILLER        PIC     XXXX  VALUE '32MA'.            ELUKYSEL
00518          10  FILLER        PIC     XXXX  VALUE '32SA'.            ELUKYSEL
00519          10  FILLER        PIC     XXXX  VALUE '33DA'.            ELUKYSEL
00520          10  FILLER        PIC     XXXX  VALUE '33MR'.            ELUKYSEL
00521          10  FILLER        PIC     XXXX  VALUE '33SR'.            ELUKYSEL
00522          10  FILLER        PIC     XXXX  VALUE '34DA'.            ELUKYSEL
00523          10  FILLER        PIC     XXXX  VALUE '34MA'.            ELUKYSEL
00524          10  FILLER        PIC     XXXX  VALUE '34SA'.            ELUKYSEL
00525          10  FILLER        PIC     XXXX  VALUE '35DA'.            ELUKYSEL
00526          10  FILLER        PIC     XXXX  VALUE '35MA'.            ELUKYSEL
00527          10  FILLER        PIC     XXXX  VALUE '35SA'.            ELUKYSEL
00528          10  FILLER        PIC     XXXX  VALUE '36DA'.            ELUKYSEL
00529          10  FILLER        PIC     XXXX  VALUE '36MA'.            ELUKYSEL
00530          10  FILLER        PIC     XXXX  VALUE '36SA'.            ELUKYSEL
00531          10  FILLER        PIC     XXXX  VALUE '37DA'.            ELUKYSEL
00532          10  FILLER        PIC     XXXX  VALUE '37MA'.            ELUKYSEL
00533          10  FILLER        PIC     XXXX  VALUE '37SA'.            ELUKYSEL
00534          10  FILLER        PIC     XXXX  VALUE '38DA'.            ELUKYSEL
00535          10  FILLER        PIC     XXXX  VALUE '38MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '38SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '39DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '39MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '39SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '40DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '40MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '40SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '41DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '41MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '41SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '42DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '42MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '42SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '43DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '43MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '43SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '44DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '44MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '44SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '45DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '45MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '45SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '46DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '46MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '46SA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '47DA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '47MA'.            ELUKYSEL
00536          10  FILLER        PIC     XXXX  VALUE '47SA'.            ELUKYSEL
00537      05  WS-FR-TABLE    REDEFINES WS-FR-VALID-ITEM-DEFINITION     ELUKYSEL
00538                                 OCCURS 216 TIMES                  ELUKYSEL
00539                                 ASCENDING KEY WS-FR-ARG           ELUKYSEL
00540                                 INDEXED BY WS-FR-INDEX.           ELUKYSEL
00541          10  WS-FR-ARG          PICTURE XXX.                      ELUKYSEL
00542          10  WS-FR-STAT         PICTURE X.                        ELUKYSEL
00543              88  WS-FR-ACC              VALUE 'A'.                ELUKYSEL
00544              88  WS-FR-REJ              VALUE 'R'.                ELUKYSEL
00545 /                                                                 ELUKYSEL
00546  COPY ELSPCETC.                                                   ELUKYSEL
00547                                                                   ELUKYSEL
00548 *** LIST OF GROUPS TO SKIP FOR PACKGE CODE PROCESS **             ELUKYSEL
00549   COPY GCPKGSKP.                                                  ELUKYSEL
00550 /                                                                 ELUKYSEL
00551  LINKAGE SECTION.                                                 ELUKYSEL
00552                                                                   ELUKYSEL
00553  01  DFHCOMMAREA.                                                 ELUKYSEL
00554  COPY ELSCOMMC.                                                   ELUKYSEL
00555                                                                   ELUKYSEL
00556  COPY ELSCIA2C.                                                   ELUKYSEL
00557 /                                                                 ELUKYSEL
00558  COPY ELSSSCBC.                                                   ELUKYSEL
00559 /                                                                 ELUKYSEL
00560  COPY ELSKTBCC.                                                   ELUKYSEL
00561 /                                                                 ELUKYSEL
00562  COPY ELSKTBGC.                                                   ELUKYSEL
00563 /***********************************************************      ELUKYSEL
00564 *                                                          *      ELUKYSEL
00565 *                    PROCEDURE DIVISION                    *      ELUKYSEL
00566 *                                                          *      ELUKYSEL
00567 ************************************************************      ELUKYSEL
00568                                                                   ELUKYSEL
00569  PROCEDURE DIVISION.                                              ELUKYSEL
00570                                                                   ELUKYSEL
00571 ************************************************************      ELUKYSEL
00572 *                                                          *      ELUKYSEL
00573 *        KEY SELECTION                                     *      ELUKYSEL
00574 *                                                          *      ELUKYSEL
00575 ************************************************************      ELUKYSEL
00576  KEY-SELECTION.                                                   ELUKYSEL
00577      IF EIBCALEN NOT = LENGTH OF DFHCOMMAREA                      ELUKYSEL
00578          PERFORM INVALID-COMMAREA-ABEND.                          ELUKYSEL
00579      PERFORM INITIALIZE-MODULE.                                   ELUKYSEL
00580      PERFORM TEXAS-REGION-CHECK.                                  ELUKYSEL
00581      PERFORM MAIN-PROCESSING.                                     ELUKYSEL
00582      PERFORM RETURN-TO-CALLER.                                    ELUKYSEL
00583                                                                   ELUKYSEL
00584                                                                   ELUKYSEL
00585 ************************************************************      ELUKYSEL
00586 *                                                          *      ELUKYSEL
00587 *        INITIALIZE MODULE                                 *      ELUKYSEL
00588 *                                                          *      ELUKYSEL
00589 ************************************************************      ELUKYSEL
00590  INITIALIZE-MODULE.                                               ELUKYSEL
00591      PERFORM ESTABLISH-CONTROL-AREAS.                             ELUKYSEL
00592      PERFORM ESTABLISH-CONTRACT-KEY-TABLE.                        ELUKYSEL
00593      PERFORM ESTABLISH-GROUP-KEY-TABLE.                           ELUKYSEL
00594                                                                   ELUKYSEL
00595 ************************************************************      ELUKYSEL
00596 *                                                          *      ELUKYSEL
00597 *        ESTABLISH CONTROL AREAS                           *      ELUKYSEL
00598 *                                                          *      ELUKYSEL
00599 ************************************************************      ELUKYSEL
00600  ESTABLISH-CONTROL-AREAS.                                         ELUKYSEL
00601      CALL 'ELUINISM' USING DFHCOMMAREA                            ELUKYSEL
00602          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELUKYSEL
00603      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELUKYSEL
00604      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYSEL
00605          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELUKYSEL
00606      MOVE SPACES TO SSB-ACTION-MODULE.                            ELUKYSEL
00607                                                                   ELUKYSEL
00608 ************************************************************      ELUKYSEL
00609 *                                                          *      ELUKYSEL
00610 *        ESTABLISH CONTRACT KEY TABLE                      *      ELUKYSEL
00611 *                                                          *      ELUKYSEL
00612 ************************************************************      ELUKYSEL
00613  ESTABLISH-CONTRACT-KEY-TABLE.                                    ELUKYSEL
00614      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELUKYSEL
00615      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYSEL
00616          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELUKYSEL
00617      IF NOT CIA-RC-OK                                             ELUKYSEL
00618          PERFORM READ-THE-CONTRACT-KEY-TABLE.                     ELUKYSEL
00619                                                                   ELUKYSEL
00620 ************************************************************      ELUKYSEL
00621 *                                                          *      ELUKYSEL
00622 *        ESTABLISH GROUP KEY TABLE                         *      ELUKYSEL
00623 *                                                          *      ELUKYSEL
00624 ************************************************************      ELUKYSEL
00625  ESTABLISH-GROUP-KEY-TABLE.                                       ELUKYSEL
00626      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYSEL
00627      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYSEL
00628          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELUKYSEL
00629      IF NOT CIA-RC-OK                                             ELUKYSEL
00630          PERFORM READ-THE-GROUP-KEY-TABLE.                        ELUKYSEL
00631                                                                   ELUKYSEL
00632 ************************************************************      ELUKYSEL
00633 *                                                          *      ELUKYSEL
00634 *        READ THE CONTRACT KEY TABLE                       *      ELUKYSEL
00635 *                                                          *      ELUKYSEL
00636 ************************************************************      ELUKYSEL
00637  READ-THE-CONTRACT-KEY-TABLE.                                     ELUKYSEL
00638      SET CIA-ELSKTBC-DDN   TO  TRUE.                              ELUKYSEL
00639      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELUKYSEL
00640      PERFORM CALL-THE-STORAGE-MANAGER.                            ELUKYSEL
00641      SET CIA-ELSKTBC-DDN TO TRUE.                                 ELUKYSEL
00642      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYSEL
00643          ADDRESS OF KTC-GCCONTR-KEY-TABLE.                        ELUKYSEL
00644                                                                   ELUKYSEL
00645 ************************************************************      ELUKYSEL
00646 *                                                          *      ELUKYSEL
00647 *        READ THE GROUP KEY TABLE                          *      ELUKYSEL
00648 *                                                          *      ELUKYSEL
00649 ************************************************************      ELUKYSEL
00650  READ-THE-GROUP-KEY-TABLE.                                        ELUKYSEL
00651      SET CIA-ELSKTBG-DDN   TO  TRUE.                              ELUKYSEL
00652      SET CIA-STG-RETRIEVE  TO  TRUE.                              ELUKYSEL
00653      PERFORM CALL-THE-STORAGE-MANAGER.                            ELUKYSEL
00654      SET CIA-ELSKTBG-DDN TO TRUE.                                 ELUKYSEL
00655      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELUKYSEL
00656          ADDRESS OF KTG-GCGRPSPC-KEY-TABLE.                       ELUKYSEL
00657                                                                   ELUKYSEL
00658 ************************************************************      ELUKYSEL
00659 *                                                          *      ELUKYSEL
00660 *        RETURN TO CALLER                                  *      ELUKYSEL
00661 *                                                          *      ELUKYSEL
00662 ************************************************************      ELUKYSEL
00663  RETURN-TO-CALLER.                                                ELUKYSEL
00664      IF WS-KTC-MODIFIED                                           ELUKYSEL
00665          PERFORM SAVE-THE-CONTRACT-KEY-TABLE.                     ELUKYSEL
00666      IF WS-KTG-MODIFIED                                           ELUKYSEL
00667          PERFORM SAVE-THE-GROUP-KEY-TABLE.                        ELUKYSEL
00668      GOBACK.                                                      ELUKYSEL
00669                                                                   ELUKYSEL
00670 ************************************************************      ELUKYSEL
00671 *                                                          *      ELUKYSEL
00672 *        SAVE THE CONTRACT KEY TABLE                       *      ELUKYSEL
00673 *                                                          *      ELUKYSEL
00674 ************************************************************      ELUKYSEL
00675  SAVE-THE-CONTRACT-KEY-TABLE.                                     ELUKYSEL
00676      SET  CIA-ELSKTBC-DDN  TO  TRUE.                              ELUKYSEL
00677      MOVE LENGTH OF KTC-GCCONTR-KEY-TABLE                         ELUKYSEL
00678           TO CIA-AREA-LEN.                                        ELUKYSEL
00679      SET CIA-STG-STOW      TO  TRUE.                              ELUKYSEL
00680      PERFORM CALL-THE-STORAGE-MANAGER.                            ELUKYSEL
00681                                                                   ELUKYSEL
00682 ************************************************************      ELUKYSEL
00683 *                                                          *      ELUKYSEL
00684 *        SAVE THE GROUP KEY TABLE                          *      ELUKYSEL
00685 *                                                          *      ELUKYSEL
00686 ************************************************************      ELUKYSEL
00687  SAVE-THE-GROUP-KEY-TABLE.                                        ELUKYSEL
00688      SET  CIA-ELSKTBG-DDN  TO  TRUE.                              ELUKYSEL
00689      MOVE LENGTH OF KTG-GCGRPSPC-KEY-TABLE                        ELUKYSEL
00690           TO CIA-AREA-LEN.                                        ELUKYSEL
00691      SET CIA-STG-STOW      TO  TRUE.                              ELUKYSEL
00692      PERFORM CALL-THE-STORAGE-MANAGER.                            ELUKYSEL
00693                                                                   ELUKYSEL
00694 ************************************************************      ELUKYSEL
00695 *                                                          *      ELUKYSEL
00696 *        CALL THE STORAGE MANAGER                          *      ELUKYSEL
00697 *                                                          *      ELUKYSEL
00698 ************************************************************      ELUKYSEL
00699  CALL-THE-STORAGE-MANAGER.                                        ELUKYSEL
00700      EXEC CICS LINK                                               ELUKYSEL
00701                PROGRAM('ELUSTGMG')                                ELUKYSEL
00702                COMMAREA(DFHCOMMAREA)                              ELUKYSEL
00703                END-EXEC.                                          ELUKYSEL
00704                                                                   ELUKYSEL
00705 /***********************************************************      ELUKYSEL
00706 *                                                          *      ELUKYSEL
00707 *        MAIN PROCESSING                                   *      ELUKYSEL
00708 *                                                          *      ELUKYSEL
00709 ************************************************************      ELUKYSEL
00710  MAIN-PROCESSING.                                                 ELUKYSEL
00711      SET WS-DATA-ITEM-NOT-FOUND TO TRUE.                          ELUKYSEL
00712      PERFORM DETERMINE-SELECTOR-USE                               ELUKYSEL
00713          VARYING SSB-SELECTOR-STATE FROM SSB-SELECTOR-STATE BY 1  ELUKYSEL
00714            UNTIL    SSB-SELECTOR-STATE > SSB-MAX-MODULES          ELUKYSEL
00715                  OR NOT CIA-SEL-TYP-KEY (SSB-SELECTOR-STATE)      ELUKYSEL
00716                  OR WS-DATA-ITEM-FOUND.                           ELUKYSEL
00717      IF WS-DATA-ITEM-FOUND                                        ELUKYSEL
00718      THEN                                                         ELUKYSEL
00719         SUBTRACT 1 FROM SSB-SELECTOR-STATE.                       ELUKYSEL
00720                                                                   ELUKYSEL
00721 ************************************************************      ELUKYSEL
00722 *                                                          *      ELUKYSEL
00723 *        DETERMINE SELECTOR USE                            *      ELUKYSEL
00724 *                                                          *      ELUKYSEL
00725 ************************************************************      ELUKYSEL
00726  DETERMINE-SELECTOR-USE.                                          ELUKYSEL
00727      IF SSB-RESELECTION (SSB-SELECTOR-STATE)                      ELUKYSEL
00728          PERFORM DO-RESELECTION                                   ELUKYSEL
00729      ELSE IF SSB-NOT-USED (SSB-SELECTOR-STATE)                    ELUKYSEL
00730          CONTINUE                                                 ELUKYSEL
00731      ELSE IF SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                ELUKYSEL
00732               OR SSB-REPROCESS    (SSB-SELECTOR-STATE)            ELUKYSEL
00733          PERFORM CHECK-NEXT-DATA-ITEM                             ELUKYSEL
00734      ELSE                                                         ELUKYSEL
00735          PERFORM PROCESS-INPUT-SELECTIONS.                        ELUKYSEL
00736                                                                   ELUKYSEL
00737 ************************************************************      ELUKYSEL
00738 *                                                          *      ELUKYSEL
00739 *        DO RESELECTION                                    *      ELUKYSEL
00740 *                                                          *      ELUKYSEL
00741 * PROCESSING:                                              *      ELUKYSEL
00742 *   1. IGNORE-PREVIOUS-SELECTION WILL BE DONE FIRST.       *      ELUKYSEL
00743 *      SEARCH-FOR-CONTRACT-MEDICARE-V AND                  *      ELUKYSEL
00744 *      SEARCH-FOR-GROUP-MEDICARE-VARI ASK THE QUESTION     *      ELUKYSEL
00745 *      'IS THERE A VARIATION BASED ON MEDICARE?'.          *      ELUKYSEL
00746 *                                                          *      ELUKYSEL
00747 *   2. PROCESS-INPUT-SELECTIONS WILL BE PERFORMED NEXT.    *      ELUKYSEL
00748 *      CHECK-CONTRACTS-FOR-MEDICARE AND                    *      ELUKYSEL
00749 *      CHECK-GROUPS-FOR-MEDICARE WILL PROCESS THE ANSWER   *      ELUKYSEL
00750 *      'IS THERE A VARIATION BASED ON MEDICARE?'.          *      ELUKYSEL
00751 *                                                          *      ELUKYSEL
00752 *                                                          *      ELUKYSEL
00753 ************************************************************      ELUKYSEL
00754  DO-RESELECTION.                                                  ELUKYSEL
00755      SET WS-NO-ITEM-REJECTED TO  TRUE.                            ELUKYSEL
00756      PERFORM PROCESS-INPUT-SELECTIONS.                            ELUKYSEL
00757      IF WS-ITEM-REJECTED                                          ELUKYSEL
00758          PERFORM FLAG-CURRENT-SELECTOR-AS-COMPL                   ELUKYSEL
00759      ELSE                                                         ELUKYSEL
00760          PERFORM IGNORE-PREVIOUS-SELECTION.                       ELUKYSEL
00761                                                                   ELUKYSEL
00762 ************************************************************      ELUKYSEL
00763 *                                                          *      ELUKYSEL
00764 *        IGNORE PREVIOUS SELECTION                         *      ELUKYSEL
00765 *                                                          *      ELUKYSEL
00766 ************************************************************      ELUKYSEL
00767  IGNORE-PREVIOUS-SELECTION.                                       ELUKYSEL
00768      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE)                    ELUKYSEL
00769           TO  TRUE.                                               ELUKYSEL
00770      PERFORM CHECK-NEXT-DATA-ITEM.                                ELUKYSEL
00771                                                                   ELUKYSEL
00772 /***********************************************************      ELUKYSEL
00773 *                                                          *      ELUKYSEL
00774 *        PROCESS INPUT SELECTIONS                          *      ELUKYSEL
00775 *                                                          *      ELUKYSEL
00776 ************************************************************      ELUKYSEL
00777  PROCESS-INPUT-SELECTIONS.                                        ELUKYSEL
00778      IF SSB-SS-INITIAL                                            ELUKYSEL
00779          CONTINUE                                                 ELUKYSEL
00780      ELSE IF SSB-SS-GET-GROUP                                     ELUKYSEL
00781          CONTINUE                                                 ELUKYSEL
00782      ELSE IF SSB-SS-GET-SECTION                                   ELUKYSEL
00783          CONTINUE                                                 ELUKYSEL
00784      ELSE IF SSB-SS-DATE-REJECT                                   ELUKYSEL
00785          PERFORM PROCESS-SERVICE-DATES                            ELUKYSEL
00786      ELSE IF SSB-SS-SHOW-NOTICE                                   ELUKYSEL
00787          CONTINUE                                                 ELUKYSEL
00788      ELSE IF SSB-SS-GET-MEDCA-ELIG                                ELUKYSEL
00789          PERFORM PROCESS-MEDICARE-ELIGIBILITY                     ELUKYSEL
00790      ELSE IF SSB-SS-GET-FAM-REL                                   ELUKYSEL
00791          PERFORM PROCESS-FAMILY-RELATIONSHIP                      ELUKYSEL
00792      ELSE IF SSB-SS-GET-PT-AGE                                    ELUKYSEL
00793          PERFORM PROCESS-PATIENT-AGE                              ELUKYSEL
00794      ELSE IF SSB-SS-GET-GRP-SPEC-EFF-DATE                         ELUKYSEL
00795          PERFORM PROCESS-GROUP-SPECIFIC-EFFECTI                   ELUKYSEL
00796      ELSE IF SSB-SS-GET-PROV-CTL-BAS-INST                         ELUKYSEL
00797          PERFORM PROCESS-BASIC-INSTITUTIONAL-PR                   ELUKYSEL
00798      ELSE IF SSB-SS-GET-PROV-CTL-SUP-INST                         ELUKYSEL
00799          PERFORM PROCESS-SUPP-INST-PROV-CTL                       ELUKYSEL
00800      ELSE IF SSB-SS-GET-PROV-CTL-BAS-PROF                         ELUKYSEL
00801          PERFORM PROCESS-BASIC-PROFESSIONAL-PRO                   ELUKYSEL
00802      ELSE IF SSB-SS-GET-PROV-CTL-SUP-PROF                         ELUKYSEL
00803          PERFORM PROCESS-SUPP-PROF-PROV-CTL                       ELUKYSEL
00804      ELSE IF SSB-SS-GET-CONT-EFF-INST-BAS                         ELUKYSEL
00805          PERFORM PROCESS-BASIC-INSTITUTIONAL-CO                   ELUKYSEL
00806      ELSE IF SSB-SS-GET-CONT-EFF-INST-SUP                         ELUKYSEL
00807          PERFORM PROCESS-SUPP-INST-CONT-EFFDT                     ELUKYSEL
00808      ELSE IF SSB-SS-GET-CONT-EFF-PROF-BAS                         ELUKYSEL
00809          PERFORM PROCESS-BASIC-PROFESSIONAL-CON                   ELUKYSEL
00810      ELSE IF SSB-SS-GET-CONT-EFF-PROF-SUP                         ELUKYSEL
00811          PERFORM PROCESS-SUPP-PROF-CONT-EFFDT                     ELUKYSEL
00812      ELSE                                                         ELUKYSEL
00813          PERFORM SYSTEM-LOGIC-ERROR.                              ELUKYSEL
00814                                                                   ELUKYSEL
00815 /***********************************************************      ELUKYSEL
00816 *                                                          *      ELUKYSEL
00817 *        PROCESS SERVICE DATES                             *      ELUKYSEL
00818 *                                                          *      ELUKYSEL
00819 ************************************************************      ELUKYSEL
00820  PROCESS-SERVICE-DATES.                                           ELUKYSEL
00821      IF SSB-COVRD-FROM-DATE-CEN > SSB-SRV-TO-DT-CEN               ELUKYSEL
00822           OR SSB-COVRD-TO-DATE-CEN < SSB-SRV-FROM-DT-CEN          ELUKYSEL
00823          PERFORM NO-COVERAGE                                      ELUKYSEL
00824      ELSE                                                         ELUKYSEL
00825          PERFORM ELIM-ITEMS-BY-EFFECTIVE-D.                       ELUKYSEL
00826                                                                   ELUKYSEL
00827 ************************************************************      ELUKYSEL
00828 *                                                          *      ELUKYSEL
00829 *        NO COVERAGE                                       *      ELUKYSEL
00830 *                                                          *      ELUKYSEL
00831 ************************************************************      ELUKYSEL
00832  NO-COVERAGE.                                                     ELUKYSEL
00833      SET SSB-SS-GET-SECTION TO TRUE.                              ELUKYSEL
00834      SET SSB-INITIAL-CALL (SSB-SELECTOR-STATE) TO TRUE.           ELUKYSEL
00835      SET SSB-SS-GET-GROUP TO TRUE.                                ELUKYSEL
00836      SET SSB-REPROCESS (SSB-SELECTOR-STATE) TO TRUE.              ELUKYSEL
00837      SET SSB-SS-INITIAL TO TRUE.                                  ELUKYSEL
00838                                                                   ELUKYSEL
00839 ************************************************************      ELUKYSEL
00840 *                                                          *      ELUKYSEL
00841 *        ELIMINATE ITEMS BY EFFECTIVE DATE                 *      ELUKYSEL
00842 *                                                          *      ELUKYSEL
00843 ************************************************************      ELUKYSEL
00844  ELIM-ITEMS-BY-EFFECTIVE-D.                                       ELUKYSEL
00845      PERFORM INITIALIZE-KEY-TABLES.                               ELUKYSEL
00846      PERFORM ELIM-GROUPS-BY-SERVICE-AN                            ELUKYSEL
00847          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
00848                 UNTIL KTG-IDX > KTG-NBR-KEYS.                     ELUKYSEL
00849      PERFORM ELIM-CONTRACTS-BY-SERVICE                            ELUKYSEL
00850          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
00851                 UNTIL KTC-IDX > KTC-NBR-KEYS.                     ELUKYSEL
00852                                                                   ELUKYSEL
00853 ************************************************************      ELUKYSEL
00854 *                                                          *      ELUKYSEL
00855 *        ELIMINATE GROUPS BY SERVICE AND EFFECTIVE DATE    *      ELUKYSEL
00856 *                                                          *      ELUKYSEL
00857 ************************************************************      ELUKYSEL
00858  ELIM-GROUPS-BY-SERVICE-AN.                                       ELUKYSEL
00859      IF SSB-SRV-TO-DT-CEN < KTG-EFF-DT-CENTURY (KTG-IDX)          ELUKYSEL
00860        OR SSB-SRV-FROM-DT-CEN > KTG-TERM-DT-CENTURY (KTG-IDX)     ELUKYSEL
00861      THEN                                                         ELUKYSEL
00862         PERFORM REJECT-GROUP-ITEM.                                ELUKYSEL
00863                                                                   ELUKYSEL
00864 ************************************************************      ELUKYSEL
00865 *                                                          *      ELUKYSEL
00866 *        ELIMINATE CONTRACTS BY SERVICE AND EFFECTIVE DATE *      ELUKYSEL
00867 *                                                          *      ELUKYSEL
00868 ************************************************************      ELUKYSEL
00869  ELIM-CONTRACTS-BY-SERVICE.                                       ELUKYSEL
00870      IF SSB-SRV-TO-DT-CEN < KTC-EFF-DT-CENTURY (KTC-IDX)          ELUKYSEL
00871       OR SSB-SRV-FROM-DT-CEN > KTC-TERM-DT-CENTURY (KTC-IDX)      ELUKYSEL
00872      THEN                                                         ELUKYSEL
00873         PERFORM REJECT-CONTRACT-ITEM.                             ELUKYSEL
00874                                                                   ELUKYSEL
00875 ************************************************************      ELUKYSEL
00876 *                                                          *      ELUKYSEL
00877 *        PROCESS MEDICARE ELIGIBILITY                      *      ELUKYSEL
00878 *                                                          *      ELUKYSEL
00879 ************************************************************      ELUKYSEL
00880  PROCESS-MEDICARE-ELIGIBILITY.                                    ELUKYSEL
00881      IF SSB-FR-MED-GRP-VAR                                        ELUKYSEL
00882          PERFORM ELIM-GROUPS-BY-MEDICARE-E.                       ELUKYSEL
00883      PERFORM ELIM-CONTRACTS-BY-MEDICAR                            ELUKYSEL
00884          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
00885            UNTIL SSB-CONT-VAR-IDX > 4.                            ELUKYSEL
00886                                                                   ELUKYSEL
00887 ************************************************************      ELUKYSEL
00888 *                                                          *      ELUKYSEL
00889 *        ELIMINATE GROUPS BY MEDICARE ELIGIBILITY          *      ELUKYSEL
00890 *                                                          *      ELUKYSEL
00891 ************************************************************      ELUKYSEL
00892  ELIM-GROUPS-BY-MEDICARE-E.                                       ELUKYSEL
00893      PERFORM CHECK-GROUPS-FOR-MEDICARE                            ELUKYSEL
00894          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
00895            UNTIL KTG-IDX > KTG-NBR-KEYS.                          ELUKYSEL
00896                                                                   ELUKYSEL
00897 ************************************************************      ELUKYSEL
00898 *                                                          *      ELUKYSEL
00899 *        CHECK GROUPS FOR MEDICARE                         *      ELUKYSEL
00900 *                                                          *      ELUKYSEL
00901 ************************************************************      ELUKYSEL
00902  CHECK-GROUPS-FOR-MEDICARE.                                       ELUKYSEL
00903      MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-FRL-MEDICARE.           ELUKYSEL
00904      IF SSB-MEDCA-ELIG                                            ELUKYSEL
00905      THEN                                                         ELUKYSEL
00906         IF WS-FRL-MEDIC-ONLY                                      ELUKYSEL
00907         THEN                                                      ELUKYSEL
00908            CONTINUE                                               ELUKYSEL
00909         ELSE                                                      ELUKYSEL
00910            PERFORM REJECT-GROUP-ITEM                              ELUKYSEL
00911         END-IF                                                    ELUKYSEL
00912      ELSE                                                         ELUKYSEL
00913         IF WS-FRL-MEDIC-ONLY                                      ELUKYSEL
00914         THEN                                                      ELUKYSEL
00915            PERFORM REJECT-GROUP-ITEM                              ELUKYSEL
00916         ELSE                                                      ELUKYSEL
00917            CONTINUE                                               ELUKYSEL
00918         END-IF                                                    ELUKYSEL
00919      END-IF.                                                      ELUKYSEL
00920                                                                   ELUKYSEL
00921 ************************************************************      ELUKYSEL
00922 *                                                          *      ELUKYSEL
00923 *        ELIMINATE CONTRACTS BY MEDICARE ELIGIBILITY       *      ELUKYSEL
00924 *                                                          *      ELUKYSEL
00925 ************************************************************      ELUKYSEL
00926  ELIM-CONTRACTS-BY-MEDICAR.                                       ELUKYSEL
00927      IF SSB-FR-MED-CONT-VAR (SSB-CONT-VAR-IDX)                    ELUKYSEL
00928          PERFORM CHECK-CONTRACTS-WITH-SPECIFICX.                  ELUKYSEL
00929                                                                   ELUKYSEL
00930 ************************************************************      ELUKYSEL
00931 *                                                          *      ELUKYSEL
00932 *        CHECK CONTRACTS WITH SPECIFIC MEDICARE VAR        *      ELUKYSEL
00933 *                                                          *      ELUKYSEL
00934 ************************************************************      ELUKYSEL
00935  CHECK-CONTRACTS-WITH-SPECIFICX.                                  ELUKYSEL
00936      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
00937      PERFORM CHECK-CONTRACTS-FOR-MEDICARE                         ELUKYSEL
00938          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
00939            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
00940                                                                   ELUKYSEL
00941 ************************************************************      ELUKYSEL
00942 *                                                          *      ELUKYSEL
00943 *        CHECK CONTRACTS FOR MEDICARE                      *      ELUKYSEL
00944 *                                                          *      ELUKYSEL
00945 ************************************************************      ELUKYSEL
00946  CHECK-CONTRACTS-FOR-MEDICARE.                                    ELUKYSEL
00947      MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-FRL-MEDICARE.           ELUKYSEL
00948      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELUKYSEL
00949      THEN                                                         ELUKYSEL
00950         IF SSB-MEDCA-ELIG                                         ELUKYSEL
00951         THEN                                                      ELUKYSEL
00952            IF WS-FRL-MEDIC-ONLY                                   ELUKYSEL
00953            THEN                                                   ELUKYSEL
00954               CONTINUE                                            ELUKYSEL
00955            ELSE                                                   ELUKYSEL
00956               PERFORM REJECT-SPECIFIC-CONTRACT-ITEM               ELUKYSEL
00957            END-IF                                                 ELUKYSEL
00958         ELSE                                                      ELUKYSEL
00959            IF WS-FRL-MEDIC-ONLY                                   ELUKYSEL
00960            THEN                                                   ELUKYSEL
00961               PERFORM REJECT-SPECIFIC-CONTRACT-ITEM               ELUKYSEL
00962            ELSE                                                   ELUKYSEL
00963               CONTINUE                                            ELUKYSEL
00964            END-IF                                                 ELUKYSEL
00965         END-IF                                                    ELUKYSEL
00966      END-IF.                                                      ELUKYSEL
00967                                                                   ELUKYSEL
00968 /***********************************************************      ELUKYSEL
00969 *                                                          *      ELUKYSEL
00970 *        PROCESS FAMILY RELATIONSHIP                       *      ELUKYSEL
00971 *                                                          *      ELUKYSEL
00972 ************************************************************      ELUKYSEL
00973  PROCESS-FAMILY-RELATIONSHIP.                                     ELUKYSEL
00974      IF     SSB-FR-FR-GRP-VAR                                     ELUKYSEL
00975         AND NOT SSB-FR-UNDEF                                      ELUKYSEL
00976      THEN                                                         ELUKYSEL
00977         PERFORM ELIM-GROUPS-BY-FAMILY-REL.                        ELUKYSEL
00978      PERFORM ELIM-CONTRACTS-BY-FAMILYX                            ELUKYSEL
00979          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
00980            UNTIL SSB-CONT-VAR-IDX > 4.                            ELUKYSEL
00981                                                                   ELUKYSEL
00982 ************************************************************      ELUKYSEL
00983 *                                                          *      ELUKYSEL
00984 *        ELIMINATE GROUPS BY FAMILY RELATIONSHIP           *      ELUKYSEL
00985 *                                                          *      ELUKYSEL
00986 ************************************************************      ELUKYSEL
00987  ELIM-GROUPS-BY-FAMILY-REL.                                       ELUKYSEL
00988      PERFORM CHECK-GROUPS-FOR-FAM-REL                             ELUKYSEL
00989          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
00990            UNTIL KTG-IDX > KTG-NBR-KEYS.                          ELUKYSEL
00991                                                                   ELUKYSEL
00992 ************************************************************      ELUKYSEL
00993 *                                                          *      ELUKYSEL
00994 *        CHECK GROUPS FOR FAM REL                          *      ELUKYSEL
00995 *                                                          *      ELUKYSEL
00996 ************************************************************      ELUKYSEL
00997  CHECK-GROUPS-FOR-FAM-REL.                                        ELUKYSEL
00998      MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-FAM-REL-LVL.            ELUKYSEL
00999      MOVE SSB-FAM-REL               TO WS-FAM-REL.                ELUKYSEL
01000      SEARCH ALL WS-FR-TABLE                                       ELUKYSEL
01001         AT END                                                    ELUKYSEL
01002            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUKYSEL
01003            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUKYSEL
01004         WHEN WS-FR-ARG (WS-FR-INDEX) = WS-FR-KEY                  ELUKYSEL
01005            SET HOLD-INDEX TO WS-FR-INDEX                          ELUKYSEL
01006         END-SEARCH.                                               ELUKYSEL
01007      IF WS-FR-REJ (HOLD-INDEX)                                    ELUKYSEL
01008          PERFORM REJECT-GROUP-ITEM.                               ELUKYSEL
01009                                                                   ELUKYSEL
01010 ************************************************************      ELUKYSEL
01011 *                                                          *      ELUKYSEL
01012 *        ELIMINATE CONTRACTS BY FAMILY RELATIONSHIP        *      ELUKYSEL
01013 *                                                          *      ELUKYSEL
01014 ************************************************************      ELUKYSEL
01015  ELIM-CONTRACTS-BY-FAMILYX.                                       ELUKYSEL
01016      PERFORM CHECK-CONTRACTS-FOR-FAM-REL                          ELUKYSEL
01017          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01018            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01019                                                                   ELUKYSEL
01020 ************************************************************      ELUKYSEL
01021 *                                                          *      ELUKYSEL
01022 *        CHECK CONTRACTS FOR FAM REL                       *      ELUKYSEL
01023 *                                                          *      ELUKYSEL
01024 ************************************************************      ELUKYSEL
01025  CHECK-CONTRACTS-FOR-FAM-REL.                                     ELUKYSEL
01026      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
01027      IF     SSB-FR-FR-CONT-VAR (SSB-CONT-VAR-IDX)                 ELUKYSEL
01028         AND KTC-SEL (KTC-IDX, KTC-SEL-IDX)                        ELUKYSEL
01029         AND NOT SSB-FR-UNDEF                                      ELUKYSEL
01030      THEN                                                         ELUKYSEL
01031         PERFORM LOOKUP-FAMILY-RELATIONSHIP.                       ELUKYSEL
01032                                                                   ELUKYSEL
01033 ************************************************************      ELUKYSEL
01034 *                                                          *      ELUKYSEL
01035 *        LOOKUP FAMILY RELATIONSHIP                        *      ELUKYSEL
01036 *                                                          *      ELUKYSEL
01037 ************************************************************      ELUKYSEL
01038  LOOKUP-FAMILY-RELATIONSHIP.                                      ELUKYSEL
01039      MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-FAM-REL-LVL.            ELUKYSEL
01040      MOVE SSB-FAM-REL               TO WS-FAM-REL.                ELUKYSEL
01041      SEARCH ALL WS-FR-TABLE                                       ELUKYSEL
01042         AT END                                                    ELUKYSEL
01043            SET CIA-AB-PGM-LOGIC TO TRUE                           ELUKYSEL
01044            EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC            ELUKYSEL
01045         WHEN WS-FR-ARG  (WS-FR-INDEX) = WS-FR-KEY                 ELUKYSEL
01046            SET HOLD-INDEX TO WS-FR-INDEX                          ELUKYSEL
01047         END-SEARCH.                                               ELUKYSEL
01048      IF WS-FR-REJ (HOLD-INDEX)                                    ELUKYSEL
01049          PERFORM REJECT-SPECIFIC-CONTRACT-ITEM.                   ELUKYSEL
01050                                                                   ELUKYSEL
01051 ************************************************************      ELUKYSEL
01052 *                                                          *      ELUKYSEL
01053 *        PROCESS PATIENT AGE                               *      ELUKYSEL
01054 *                                                          *      ELUKYSEL
01055 ************************************************************      ELUKYSEL
01056  PROCESS-PATIENT-AGE.                                             ELUKYSEL
01057      IF SSB-PT-AGE-UNDEF                                          ELUKYSEL
01058          CONTINUE                                                 ELUKYSEL
01059      ELSE                                                         ELUKYSEL
01060          PERFORM ELIM-ITEMS-BASED-ON-PATIE.                       ELUKYSEL
01061                                                                   ELUKYSEL
01062 ************************************************************      ELUKYSEL
01063 *                                                          *      ELUKYSEL
01064 *        ELIMINATE ITEMS BASED ON PATIENT AGE              *      ELUKYSEL
01065 *                                                          *      ELUKYSEL
01066 ************************************************************      ELUKYSEL
01067  ELIM-ITEMS-BASED-ON-PATIE.                                       ELUKYSEL
01068      IF SSB-FR-PT-AGE-GRP-VAR                                     ELUKYSEL
01069          PERFORM ELIM-GROUPS-BY-PATIENT-AG.                       ELUKYSEL
01070      PERFORM ELIM-CONTRACTS-BY-PATIENT                            ELUKYSEL
01071          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
01072            UNTIL SSB-CONT-VAR-IDX > 4.                            ELUKYSEL
01073                                                                   ELUKYSEL
01074 ************************************************************      ELUKYSEL
01075 *                                                          *      ELUKYSEL
01076 *        ELIMINATE GROUPS BY PATIENT AGE                   *      ELUKYSEL
01077 *                                                          *      ELUKYSEL
01078 ************************************************************      ELUKYSEL
01079  ELIM-GROUPS-BY-PATIENT-AG.                                       ELUKYSEL
01080      PERFORM CHECK-GROUPS-FOR-PAT-AGE                             ELUKYSEL
01081          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
01082            UNTIL KTG-IDX > KTG-NBR-KEYS.                          ELUKYSEL
01083                                                                   ELUKYSEL
01084 ************************************************************      ELUKYSEL
01085 *                                                          *      ELUKYSEL
01086 *        CHECK GROUPS FOR PAT AGE                          *      ELUKYSEL
01087 *                                                          *      ELUKYSEL
01088 ************************************************************      ELUKYSEL
01089  CHECK-GROUPS-FOR-PAT-AGE.                                        ELUKYSEL
01090      IF     KTG-SEL (KTG-IDX)                                     ELUKYSEL
01091         AND KTG-FAM-REL-LVL (KTG-IDX) NOT = SSB-PT-AGE            ELUKYSEL
01092      THEN                                                         ELUKYSEL
01093          PERFORM REJECT-GROUP-ITEM.                               ELUKYSEL
01094                                                                   ELUKYSEL
01095 ************************************************************      ELUKYSEL
01096 *                                                          *      ELUKYSEL
01097 *        ELIMINATE CONTRACTS BY PATIENT AGE                *      ELUKYSEL
01098 *                                                          *      ELUKYSEL
01099 ************************************************************      ELUKYSEL
01100  ELIM-CONTRACTS-BY-PATIENT.                                       ELUKYSEL
01101      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
01102      IF SSB-FR-PT-AGE-CONT-NVAR (SSB-CONT-VAR-IDX)                ELUKYSEL
01103      THEN                                                         ELUKYSEL
01104         CONTINUE                                                  ELUKYSEL
01105      ELSE                                                         ELUKYSEL
01106         PERFORM CHECK-CONTRACTS-FOR-PAT-AGE                       ELUKYSEL
01107            VARYING KTC-IDX FROM 1 BY 1                            ELUKYSEL
01108              UNTIL KTC-IDX > KTC-NBR-KEYS.                        ELUKYSEL
01109                                                                   ELUKYSEL
01110 ************************************************************      ELUKYSEL
01111 *                                                          *      ELUKYSEL
01112 *        CHECK CONTRACTS FOR PAT AGE                       *      ELUKYSEL
01113 *                                                          *      ELUKYSEL
01114 ************************************************************      ELUKYSEL
01115  CHECK-CONTRACTS-FOR-PAT-AGE.                                     ELUKYSEL
01116      IF     KTC-SEL (KTC-IDX, KTC-SEL-IDX)                        ELUKYSEL
01117         AND SSB-PT-AGE NOT = KTC-FAM-REL-LVL (KTC-IDX)            ELUKYSEL
01118      THEN                                                         ELUKYSEL
01119         PERFORM REJECT-SPECIFIC-CONTRACT-ITEM.                    ELUKYSEL
01120                                                                   ELUKYSEL
01121 ************************************************************      ELUKYSEL
01122 *                                                          *      ELUKYSEL
01123 *        PROCESS GROUP SPECIFIC EFFECTIVE DATE             *      ELUKYSEL
01124 *                                                          *      ELUKYSEL
01125 ************************************************************      ELUKYSEL
01126  PROCESS-GROUP-SPECIFIC-EFFECTI.                                  ELUKYSEL
01127      PERFORM ELIM-GROUPS-BY-EFFECTIVEX                            ELUKYSEL
01128          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
01129            UNTIL KTG-IDX > KTG-NBR-KEYS.                          ELUKYSEL
01130      PERFORM ELIM-CONTRACTS-BY-EFFECTI                            ELUKYSEL
01131          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01132            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01133      PERFORM ELIM-CONTRACTS-BY-LINE-OF.                           ELUKYSEL
01134                                                                   ELUKYSEL
01135 ************************************************************      ELUKYSEL
01136 *                                                          *      ELUKYSEL
01137 *        ELIMINATE GROUPS BY EFFECTIVE DATE                *      ELUKYSEL
01138 *                                                          *      ELUKYSEL
01139 ************************************************************      ELUKYSEL
01140  ELIM-GROUPS-BY-EFFECTIVEX.                                       ELUKYSEL
01141      IF SSB-GROUP-TERM-DATE-CEN < KTG-EFF-DT-CENTURY (KTG-IDX)    ELUKYSEL
01142        OR SSB-GROUP-EFF-DATE-CEN                                  ELUKYSEL
01143                     > KTG-TERM-DT-CENTURY (KTG-IDX)               ELUKYSEL
01144         PERFORM REJECT-GROUP-ITEM.                                ELUKYSEL
01145                                                                   ELUKYSEL
01146 ************************************************************      ELUKYSEL
01147 *                                                          *      ELUKYSEL
01148 *        ELIMINATE CONTRACTS BY EFFECTIVE DATE             *      ELUKYSEL
01149 *                                                          *      ELUKYSEL
01150 ************************************************************      ELUKYSEL
01151  ELIM-CONTRACTS-BY-EFFECTI.                                       ELUKYSEL
01152      IF SSB-GROUP-TERM-DATE-CEN < KTC-EFF-DT-CENTURY (KTC-IDX)    ELUKYSEL
01153        OR SSB-GROUP-EFF-DATE-CEN                                  ELUKYSEL
01154                     > KTC-TERM-DT-CENTURY (KTC-IDX)               ELUKYSEL
01155      THEN                                                         ELUKYSEL
01156          PERFORM REJECT-CONTRACT-ITEM.                            ELUKYSEL
01157                                                                   ELUKYSEL
01158 ************************************************************      ELUKYSEL
01159 *                                                          *      ELUKYSEL
01160 *        PROCESS BASIC INSTITUTIONAL PROVIDER CONTROL      *      ELUKYSEL
01161 *                                                          *      ELUKYSEL
01162 ************************************************************      ELUKYSEL
01163  PROCESS-BASIC-INSTITUTIONAL-PR.                                  ELUKYSEL
01164      PERFORM CHECK-FOR-INST-BAS-PC-MATCH                          ELUKYSEL
01165          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01166            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01167                                                                   ELUKYSEL
01168 ************************************************************      ELUKYSEL
01169 *                                                          *      ELUKYSEL
01170 *        CHECK FOR INST BAS PC MATCH                       *      ELUKYSEL
01171 *                                                          *      ELUKYSEL
01172 ************************************************************      ELUKYSEL
01173  CHECK-FOR-INST-BAS-PC-MATCH.                                     ELUKYSEL
01174      IF     KTC-INST-BAS-SEL (KTC-IDX)                            ELUKYSEL
01175         AND       SSB-INST-BAS-PROVDR-CONTROL                     ELUKYSEL
01176             NOT = KTC-PROVDR-CONTROL (KTC-IDX)                    ELUKYSEL
01177      THEN                                                         ELUKYSEL
01178          PERFORM REJECT-INST-BAS-CONTRACT.                        ELUKYSEL
01179                                                                   ELUKYSEL
01180 ************************************************************      ELUKYSEL
01181 *                                                          *      ELUKYSEL
01182 *        PROCESS SUPPLEMENTAL INSTITUTIONAL PROVIDER CONTRO*      ELUKYSEL
01183 *                                                          *      ELUKYSEL
01184 ************************************************************      ELUKYSEL
01185  PROCESS-SUPP-INST-PROV-CTL.                                      ELUKYSEL
01186      PERFORM CHECK-FOR-INST-SUP-PC-MATCH                          ELUKYSEL
01187          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01188            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01189                                                                   ELUKYSEL
01190 ************************************************************      ELUKYSEL
01191 *                                                          *      ELUKYSEL
01192 *        CHECK FOR INST SUP PC MATCH                       *      ELUKYSEL
01193 *                                                          *      ELUKYSEL
01194 ************************************************************      ELUKYSEL
01195  CHECK-FOR-INST-SUP-PC-MATCH.                                     ELUKYSEL
01196      IF     KTC-INST-SUP-SEL (KTC-IDX)                            ELUKYSEL
01197         AND       SSB-INST-SUP-PROVDR-CONTROL                     ELUKYSEL
01198             NOT = KTC-PROVDR-CONTROL (KTC-IDX)                    ELUKYSEL
01199      THEN                                                         ELUKYSEL
01200          PERFORM REJECT-INST-SUP-CONTRACT.                        ELUKYSEL
01201                                                                   ELUKYSEL
01202 ************************************************************      ELUKYSEL
01203 *                                                          *      ELUKYSEL
01204 *        PROCESS BASIC PROFESSIONAL PROVIDER CONTROL       *      ELUKYSEL
01205 *                                                          *      ELUKYSEL
01206 ************************************************************      ELUKYSEL
01207  PROCESS-BASIC-PROFESSIONAL-PRO.                                  ELUKYSEL
01208      PERFORM CHECK-FOR-PROF-BAS-PC-MATCH                          ELUKYSEL
01209          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01210            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01211                                                                   ELUKYSEL
01212 ************************************************************      ELUKYSEL
01213 *                                                          *      ELUKYSEL
01214 *        CHECK FOR PROF BAS PC MATCH                       *      ELUKYSEL
01215 *                                                          *      ELUKYSEL
01216 ************************************************************      ELUKYSEL
01217  CHECK-FOR-PROF-BAS-PC-MATCH.                                     ELUKYSEL
01218      IF     KTC-PROF-BAS-SEL (KTC-IDX)                            ELUKYSEL
01219        AND       SSB-PROF-BAS-PROVDR-CONTROL                      ELUKYSEL
01220            NOT = KTC-PROVDR-CONTROL (KTC-IDX)                     ELUKYSEL
01221      THEN                                                         ELUKYSEL
01222          PERFORM REJECT-PROF-BAS-CONTRACT.                        ELUKYSEL
01223                                                                   ELUKYSEL
01224 ************************************************************      ELUKYSEL
01225 *                                                          *      ELUKYSEL
01226 *        PROCESS SUPPLEMENTAL PROFESSIONAL PROVIDER CONTROL*      ELUKYSEL
01227 *                                                          *      ELUKYSEL
01228 ************************************************************      ELUKYSEL
01229  PROCESS-SUPP-PROF-PROV-CTL.                                      ELUKYSEL
01230      PERFORM CHECK-FOR-PROF-SUP-PC-MATCH                          ELUKYSEL
01231          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01232            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01233                                                                   ELUKYSEL
01234 ************************************************************      ELUKYSEL
01235 *                                                          *      ELUKYSEL
01236 *        CHECK FOR PROF SUP PC MATCH                       *      ELUKYSEL
01237 *                                                          *      ELUKYSEL
01238 ************************************************************      ELUKYSEL
01239  CHECK-FOR-PROF-SUP-PC-MATCH.                                     ELUKYSEL
01240      IF     KTC-PROF-SUP-SEL (KTC-IDX)                            ELUKYSEL
01241         AND       SSB-PROF-SUP-PROVDR-CONTROL                     ELUKYSEL
01242             NOT = KTC-PROVDR-CONTROL (KTC-IDX)                    ELUKYSEL
01243      THEN                                                         ELUKYSEL
01244          PERFORM REJECT-PROF-SUP-CONTRACT.                        ELUKYSEL
01245                                                                   ELUKYSEL
01246 ************************************************************      ELUKYSEL
01247 *                                                          *      ELUKYSEL
01248 *        PROCESS BASIC INSTITUTIONAL CONTRACT EFF DATE     *      ELUKYSEL
01249 *                                                          *      ELUKYSEL
01250 ************************************************************      ELUKYSEL
01251  PROCESS-BASIC-INSTITUTIONAL-CO.                                  ELUKYSEL
01252      PERFORM CHECK-INST-BAS-CONT-EFF-DATE                         ELUKYSEL
01253          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01254            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01255                                                                   ELUKYSEL
01256 ************************************************************      ELUKYSEL
01257 *                                                          *      ELUKYSEL
01258 *        CHECK INST BAS CONT EFF DATE                      *      ELUKYSEL
01259 *                                                          *      ELUKYSEL
01260 ************************************************************      ELUKYSEL
01261  CHECK-INST-BAS-CONT-EFF-DATE.                                    ELUKYSEL
01262      IF     KTC-INST-BAS-SEL (KTC-IDX)                            ELUKYSEL
01263         AND SSB-INST-BAS-EFF-DT-CEN                               ELUKYSEL
01264             NOT = KTC-EFF-DT-CENTURY (KTC-IDX)                    ELUKYSEL
01265      THEN                                                         ELUKYSEL
01266         PERFORM REJECT-INST-BAS-CONTRACT.                         ELUKYSEL
01267                                                                   ELUKYSEL
01268 ************************************************************      ELUKYSEL
01269 *                                                          *      ELUKYSEL
01270 *        PROCESS SUPPLEMENTAL INSTITUTIONAL CONTRACT EFF DA*      ELUKYSEL
01271 *                                                          *      ELUKYSEL
01272 ************************************************************      ELUKYSEL
01273  PROCESS-SUPP-INST-CONT-EFFDT.                                    ELUKYSEL
01274      PERFORM CHECK-INST-SUP-CONT-EFF-DATE                         ELUKYSEL
01275          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01276            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01277                                                                   ELUKYSEL
01278 ************************************************************      ELUKYSEL
01279 *                                                          *      ELUKYSEL
01280 *        CHECK INST SUP CONT EFF DATE                      *      ELUKYSEL
01281 *                                                          *      ELUKYSEL
01282 ************************************************************      ELUKYSEL
01283  CHECK-INST-SUP-CONT-EFF-DATE.                                    ELUKYSEL
01284      IF     KTC-INST-SUP-SEL (KTC-IDX)                            ELUKYSEL
01285         AND SSB-INST-SUP-EFF-DT-CEN                               ELUKYSEL
01286             NOT = KTC-EFF-DT-CENTURY (KTC-IDX)                    ELUKYSEL
01287      THEN                                                         ELUKYSEL
01288         PERFORM REJECT-INST-SUP-CONTRACT.                         ELUKYSEL
01289                                                                   ELUKYSEL
01290 ************************************************************      ELUKYSEL
01291 *                                                          *      ELUKYSEL
01292 *        PROCESS BASIC PROFESSIONAL CONTRACT EFF DATE      *      ELUKYSEL
01293 *                                                          *      ELUKYSEL
01294 ************************************************************      ELUKYSEL
01295  PROCESS-BASIC-PROFESSIONAL-CON.                                  ELUKYSEL
01296      PERFORM CHECK-PROF-BAS-CONT-EFF-DATE                         ELUKYSEL
01297          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01298            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01299                                                                   ELUKYSEL
01300 ************************************************************      ELUKYSEL
01301 *                                                          *      ELUKYSEL
01302 *        CHECK PROF BAS CONT EFF DATE                      *      ELUKYSEL
01303 *                                                          *      ELUKYSEL
01304 ************************************************************      ELUKYSEL
01305  CHECK-PROF-BAS-CONT-EFF-DATE.                                    ELUKYSEL
01306      IF     KTC-PROF-BAS-SEL (KTC-IDX)                            ELUKYSEL
01307         AND       SSB-PROF-BAS-EFF-DT-CEN                         ELUKYSEL
01308             NOT = KTC-EFF-DT-CENTURY (KTC-IDX)                    ELUKYSEL
01309          PERFORM REJECT-PROF-BAS-CONTRACT.                        ELUKYSEL
01310                                                                   ELUKYSEL
01311 ************************************************************      ELUKYSEL
01312 *                                                          *      ELUKYSEL
01313 *        PROCESS SUPPLEMENTAL PROFESSIONAL CONTRACT EFF DAT*      ELUKYSEL
01314 *                                                          *      ELUKYSEL
01315 ************************************************************      ELUKYSEL
01316  PROCESS-SUPP-PROF-CONT-EFFDT.                                    ELUKYSEL
01317      PERFORM CHECK-PROF-SUP-CONT-EFF-DATE                         ELUKYSEL
01318          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01319            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
01320                                                                   ELUKYSEL
01321 ************************************************************      ELUKYSEL
01322 *                                                          *      ELUKYSEL
01323 *        CHECK PROF SUP CONT EFF DATE                      *      ELUKYSEL
01324 *                                                          *      ELUKYSEL
01325 ************************************************************      ELUKYSEL
01326  CHECK-PROF-SUP-CONT-EFF-DATE.                                    ELUKYSEL
01327      IF     KTC-PROF-SUP-SEL (KTC-IDX)                            ELUKYSEL
01328         AND       SSB-PROF-SUP-EFF-DATE-CC                        ELUKYSEL
01329             NOT = KTC-EFF-DT-CENTURY (KTC-IDX)                    ELUKYSEL
01330      THEN                                                         ELUKYSEL
01331         PERFORM REJECT-PROF-SUP-CONTRACT.                         ELUKYSEL
01332                                                                   ELUKYSEL
01333 ************************************************************      ELUKYSEL
01334 *                                                          *      ELUKYSEL
01335 *        CHECK NEXT DATA ITEM                              *      ELUKYSEL
01336 *                                                          *      ELUKYSEL
01337 ************************************************************      ELUKYSEL
01338  CHECK-NEXT-DATA-ITEM.                                            ELUKYSEL
01339      SET SSB-MODULE-STATUS-IDX TO SSB-SELECTOR-STATE.             ELUKYSEL
01340      IF NOT SSB-COMPLETED (SSB-MODULE-STATUS-IDX)                 ELUKYSEL
01341          PERFORM SELECT-AN-ITEM.                                  ELUKYSEL
01342                                                                   ELUKYSEL
01343 /***********************************************************      ELUKYSEL
01344 *                                                          *      ELUKYSEL
01345 *        SELECT AN ITEM                                    *      ELUKYSEL
01346 *                                                          *      ELUKYSEL
01347 ************************************************************      ELUKYSEL
01348  SELECT-AN-ITEM.                                                  ELUKYSEL
01349      IF CIA-SEL-TYP-TOP (SSB-SELECTOR-STATE)                      ELUKYSEL
01350          PERFORM SWITCH-TO-TOPIC                                  ELUKYSEL
01351      ELSE IF SSB-SS-GET-GROUP                                     ELUKYSEL
01352          PERFORM SELECT-MAIN-SCREEN                               ELUKYSEL
01353      ELSE IF SSB-SS-GET-SECTION                                   ELUKYSEL
01354          PERFORM SELECT-SECTION                                   ELUKYSEL
01355      ELSE IF SSB-SS-DATE-REJECT                                   ELUKYSEL
01356          PERFORM SELECT-SERVICE-DATES                             ELUKYSEL
01357      ELSE IF SSB-SS-SHOW-NOTICE                                   ELUKYSEL
01358          PERFORM SELECT-SHOW-NOTICE                               ELUKYSEL
01359      ELSE IF SSB-SS-GET-MEDCA-ELIG                                ELUKYSEL
01360          PERFORM SELECT-MEDICARE-ELIGIBILITY                      ELUKYSEL
01361      ELSE IF SSB-SS-GET-FAM-REL                                   ELUKYSEL
01362          PERFORM SELECT-FAMILY-RELATIONSHIP                       ELUKYSEL
01363      ELSE IF SSB-SS-GET-PT-AGE                                    ELUKYSEL
01364          PERFORM SELECT-PATIENT-AGE                               ELUKYSEL
01365      ELSE IF SSB-SS-GET-GRP-SPEC-EFF-DATE                         ELUKYSEL
01366          PERFORM SELECT-GROUP-SPECIFIC-EFFECTIV                   ELUKYSEL
01367      ELSE IF SSB-SS-GET-PROV-CTL-BAS-INST                         ELUKYSEL
01368          PERFORM SELECT-BASIC-INSTITUTIONAL-PRO                   ELUKYSEL
01369      ELSE IF SSB-SS-GET-PROV-CTL-SUP-INST                         ELUKYSEL
01370          PERFORM SELECT-SUPPLEMENTAL-INSTITUTIO                   ELUKYSEL
01371      ELSE IF SSB-SS-GET-PROV-CTL-BAS-PROF                         ELUKYSEL
01372          PERFORM SELECT-BASIC-PROFESSIONAL-PROV                   ELUKYSEL
01373      ELSE IF SSB-SS-GET-PROV-CTL-SUP-PROF                         ELUKYSEL
01374          PERFORM SELECT-SUPPLEMENTAL-PROFESSION                   ELUKYSEL
01375      ELSE IF SSB-SS-GET-CONT-EFF-INST-BAS                         ELUKYSEL
01376          PERFORM SELECT-INSTITUTIONAL-BASIC-CON                   ELUKYSEL
01377      ELSE IF SSB-SS-GET-CONT-EFF-INST-SUP                         ELUKYSEL
01378          PERFORM SELECT-INSTITUTIONAL-SUPPL-CON                   ELUKYSEL
01379      ELSE IF SSB-SS-GET-CONT-EFF-PROF-BAS                         ELUKYSEL
01380          PERFORM SELECT-PROFESSIONAL-BASIC-CONT                   ELUKYSEL
01381      ELSE IF SSB-SS-GET-CONT-EFF-PROF-SUP                         ELUKYSEL
01382          PERFORM SELECT-PROFESSIONAL-SUPPL-CONT                   ELUKYSEL
01383      ELSE                                                         ELUKYSEL
01384          PERFORM SYSTEM-LOGIC-ERROR.                              ELUKYSEL
01385                                                                   ELUKYSEL
01386 /***********************************************************      ELUKYSEL
01387 *                                                          *      ELUKYSEL
01388 *        SWITCH TO TOPIC                                   *      ELUKYSEL
01389 *                                                          *      ELUKYSEL
01390 ************************************************************      ELUKYSEL
01391  SWITCH-TO-TOPIC.                                                 ELUKYSEL
01392      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01393      MOVE SPACES TO SSB-ACTION-MODULE.                            ELUKYSEL
01394                                                                   ELUKYSEL
01395 ************************************************************      ELUKYSEL
01396 *                                                          *      ELUKYSEL
01397 *        SELECT MAIN SCREEN                                *      ELUKYSEL
01398 *                                                          *      ELUKYSEL
01399 ************************************************************      ELUKYSEL
01400  SELECT-MAIN-SCREEN.                                              ELUKYSEL
01401      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01402      MOVE 'ELSBEGIN' TO SSB-ACTION-MODULE.                        ELUKYSEL
01403      IF SSB-CMDLN-DATA NOT = LOW-VALUES                           ELUKYSEL
01404      THEN                                                         ELUKYSEL
01405         SET SSB-CMDLN-INPUT (SSB-SELECTOR-STATE) TO TRUE.         ELUKYSEL
01406                                                                   ELUKYSEL
01407 ************************************************************      ELUKYSEL
01408 *                                                          *      ELUKYSEL
01409 *        SELECT SECTION                                    *      ELUKYSEL
01410 *                                                          *      ELUKYSEL
01411 ************************************************************      ELUKYSEL
01412  SELECT-SECTION.                                                  ELUKYSEL
01413      IF TEXAS-REGION                                              ELUKYSEL
01414         MOVE SSB-GRP-NO TO PSKP-GROUP                             ELUKYSEL
01415         IF SKIP-IT                                                ELUKYSEL
01416             MOVE ZEROES TO SSB-PKG-CODE                           ELUKYSEL
01417         END-IF                                                    ELUKYSEL
01418      END-IF.                                                      ELUKYSEL
01419      IF NOT TEXAS-REGION                                          ELUKYSEL
01420          MOVE ZEROES TO SSB-PKG-CODE                              ELUKYSEL
01421      END-IF.                                                      ELUKYSEL
01422      IF SSB-NO-SECTN-NO OR SSB-NO-PKG-CODE                        ELUKYSEL
01423          PERFORM SET-UP-SECTION-SELECT                            ELUKYSEL
01424      ELSE                                                         ELUKYSEL
01425          PERFORM SECTION-ALREADY-KNOWN.                           ELUKYSEL
01426                                                                   ELUKYSEL
01427 ************************************************************      ELUKYSEL
01428 *                                                          *      ELUKYSEL
01429 *        SET UP SECTION SELECT                             *      ELUKYSEL
01430 *                                                          *      ELUKYSEL
01431 ************************************************************      ELUKYSEL
01432  SET-UP-SECTION-SELECT.                                           ELUKYSEL
01433      IF SSB-SUBSCRIBER-NBR = LOW-VALUES                           ELUKYSEL
01434          PERFORM SET-UP-GROUP-SECTION-SELECTION                   ELUKYSEL
01435      ELSE                                                         ELUKYSEL
01436          PERFORM SET-UP-MEMBER-SECTION-SELECTIO.                  ELUKYSEL
01437                                                                   ELUKYSEL
01438 ************************************************************      ELUKYSEL
01439 *                                                          *      ELUKYSEL
01440 *        SET UP GROUP SECTION SELECTION                    *      ELUKYSEL
01441 *                                                          *      ELUKYSEL
01442 ************************************************************      ELUKYSEL
01443  SET-UP-GROUP-SECTION-SELECTION.                                  ELUKYSEL
01444      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01445      MOVE 'ELSGRPSC' TO SSB-ACTION-MODULE.                        ELUKYSEL
01446                                                                   ELUKYSEL
01447 ************************************************************      ELUKYSEL
01448 *                                                          *      ELUKYSEL
01449 *        SET UP MEMBER SECTION SELECTION                   *      ELUKYSEL
01450 *                                                          *      ELUKYSEL
01451 ************************************************************      ELUKYSEL
01452  SET-UP-MEMBER-SECTION-SELECTIO.                                  ELUKYSEL
01453      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01454      MOVE 'ELSMEMSC' TO SSB-ACTION-MODULE.                        ELUKYSEL
01455                                                                   ELUKYSEL
01456 ************************************************************      ELUKYSEL
01457 *                                                          *      ELUKYSEL
01458 *        SECTION ALREADY KNOWN                             *      ELUKYSEL
01459 *                                                          *      ELUKYSEL
01460 ************************************************************      ELUKYSEL
01461  SECTION-ALREADY-KNOWN.                                           ELUKYSEL
01462      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
01463      PERFORM INITIALIZE-KEY-TABLES.                               ELUKYSEL
01464                                                                   ELUKYSEL
01465 ************************************************************      ELUKYSEL
01466 *                                                          *      ELUKYSEL
01467 *        SELECT SERVICE DATES                              *      ELUKYSEL
01468 *                                                          *      ELUKYSEL
01469 ************************************************************      ELUKYSEL
01470  SELECT-SERVICE-DATES.                                            ELUKYSEL
01471 *                                                                 ELUKYSEL
01472 * THIS STATE NEVER SHOWS A MENU SINCE THE DATES ARE KNOWN         ELUKYSEL
01473 * FROM THE MAIN SCREEN (VIA ELSBEGIN).  THEREFORE, THIS           ELUKYSEL
01474 * MODULE MUST BE 'TRICKED' INTO THINKING A SELECTOR WAS RUN       ELUKYSEL
01475 * IN ORDER FOR THE PROCESSING CYCLE TO REMAIN INTACT              ELUKYSEL
01476 * SUBTRACTING 1 FROM THE SSB-SELECTOR-STATE EFFECTIVELY           ELUKYSEL
01477 * FORCES THE REJECTION SIDE TO BE ACTIVATED WITHOUT A             ELUKYSEL
01478 * SELECTOR PROGRAM BEING RUN                                      ELUKYSEL
01479 *                                                                 ELUKYSEL
01480      PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV.                      ELUKYSEL
01481      SUBTRACT 1 FROM SSB-SELECTOR-STATE.                          ELUKYSEL
01482                                                                   ELUKYSEL
01483 ************************************************************      ELUKYSEL
01484 *                                                          *      ELUKYSEL
01485 *        SELECT SHOW NOTICE                                *      ELUKYSEL
01486 *                                                          *      ELUKYSEL
01487 ************************************************************      ELUKYSEL
01488  SELECT-SHOW-NOTICE.                                              ELUKYSEL
01489      PERFORM FIND-GROUP-NOTICE-OPTION.                            ELUKYSEL
01490      IF WS-NOTE-VAR-FOUND                                         ELUKYSEL
01491         SET WS-DATA-ITEM-FOUND TO TRUE                            ELUKYSEL
01492         MOVE 'ELSNOTE1' TO SSB-ACTION-MODULE                      ELUKYSEL
01493      ELSE                                                         ELUKYSEL
01494          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                  ELUKYSEL
01495                                                                   ELUKYSEL
01496 ************************************************************      ELUKYSEL
01497 *                                                          *      ELUKYSEL
01498 *        FIND GROUP NOTICE OPTION                          *      ELUKYSEL
01499 *                                                          *      ELUKYSEL
01500 ************************************************************      ELUKYSEL
01501  FIND-GROUP-NOTICE-OPTION.                                        ELUKYSEL
01502      SET KTG-IDX TO 1.                                            ELUKYSEL
01503      SEARCH KTG-KEY-TBL VARYING KTG-IDX                           ELUKYSEL
01504         AT END                                                    ELUKYSEL
01505            SET WS-NO-NOTE-VAR-FOUND TO TRUE                       ELUKYSEL
01506         WHEN     KTG-PARTICIPAT-PROV-OPTION (KTG-IDX) NOT =       ELUKYSEL
01507              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01508            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01509         WHEN     KTG-PAN-PARTICP-IND (KTG-IDX) NOT =              ELUKYSEL
01510              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01511            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01512         WHEN     KTG-POS-PARTICP-IND (KTG-IDX) NOT =              ELUKYSEL
01513              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01514            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01515         WHEN     KTG-NEW-POS-IND (KTG-IDX) NOT =                  ELUKYSEL
01516              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01517            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01518         WHEN     KTG-RPO-PARTICP-IND (KTG-IDX) NOT =              ELUKYSEL
01519              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01520            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01521         WHEN     KTG-CBL-PARTICP-IND (KTG-IDX) NOT =              ELUKYSEL
01522              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01523            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01524         WHEN     KTG-CPO-PARTICP-IND (KTG-IDX) NOT =              ELUKYSEL
01525              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01526            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01527         WHEN     KTG-PRODUCT-TYPE (KTG-IDX) NOT =                 ELUKYSEL
01528              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01529            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01530         WHEN     KTG-BLUE-SCRIPT (KTG-IDX) NOT =                  ELUKYSEL
01531              ZERO AND KTG-SEL (KTG-IDX)                           ELUKYSEL
01532            SET WS-NOTE-VAR-FOUND TO TRUE                          ELUKYSEL
01533 *       WHEN KTG-SEL (KTG-IDX) AND WS-ALLIANCE-NOT-DONE           ELUKYSEL
01534 *          IF     KTG-ALLIANCE-IND NOT = ZERO                     ELUKYSEL
01535 *             SET WS-NOTE-VAR-FOUND TO TRUE                       ELUKYSEL
01536 *             SET WS-ALLIANCE-DONE TO TRUE                        ELUKYSEL
01537 *          END-IF                                                 ELUKYSEL
01538         END-SEARCH.                                               ELUKYSEL
01539         IF  WS-ALLIANCE-NOT-DONE                                  ELUKYSEL
01540               PERFORM VARYING KTG-IDX FROM 1 BY 1                 ELUKYSEL
01541                  UNTIL KTG-IDX > KTG-NBR-KEYS                     ELUKYSEL
01542                  IF     KTG-ALLIANCE-IND NOT =                    ELUKYSEL
01543                     ZERO AND KTG-SEL (KTG-IDX)                    ELUKYSEL
01544                    SET WS-NOTE-VAR-FOUND TO TRUE                  ELUKYSEL
01545                    SET WS-ALLIANCE-DONE TO TRUE                   ELUKYSEL
01546                 END-IF                                            ELUKYSEL
01547               END-PERFORM                                         ELUKYSEL
01548         END-IF.                                                   ELUKYSEL
01549                                                                   ELUKYSEL
01550 /***********************************************************      ELUKYSEL
01551 *                                                          *      ELUKYSEL
01552 *        SELECT MEDICARE ELIGIBILITY                       *      ELUKYSEL
01553 *                                                          *      ELUKYSEL
01554 ************************************************************      ELUKYSEL
01555  SELECT-MEDICARE-ELIGIBILITY.                                     ELUKYSEL
01556      PERFORM FIND-GROUP-MEDICARE-VARIATION.                       ELUKYSEL
01557      PERFORM FIND-CONTRACT-MEDICARE-VARIATI                       ELUKYSEL
01558          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
01559            UNTIL SSB-CONT-VAR-IDX > 4.                            ELUKYSEL
01560                                                                   ELUKYSEL
01561      IF    SSB-FR-MED-GRP-VAR                                     ELUKYSEL
01562         OR (    (   SSB-PROV-CLASS-INST                           ELUKYSEL
01563                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01564             AND                                                   ELUKYSEL
01565                 (   SSB-FR-MED-CONT-VAR (1)                       ELUKYSEL
01566                  OR SSB-FR-MED-CONT-VAR (2)) )                    ELUKYSEL
01567         OR (    (   SSB-PROV-CLASS-PROF                           ELUKYSEL
01568                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01569             AND                                                   ELUKYSEL
01570                 (   SSB-FR-MED-CONT-VAR (3)                       ELUKYSEL
01571                  OR SSB-FR-MED-CONT-VAR (4)) )                    ELUKYSEL
01572          PERFORM SET-UP-MEDICARE-ELIGIBILITY                      ELUKYSEL
01573      ELSE                                                         ELUKYSEL
01574          PERFORM NO-MEDICARE-VARIATION.                           ELUKYSEL
01575                                                                   ELUKYSEL
01576 /***********************************************************      ELUKYSEL
01577 *                                                          *      ELUKYSEL
01578 *        FIND GROUP MEDICARE VARIATION                     *      ELUKYSEL
01579 *                                                          *      ELUKYSEL
01580 ************************************************************      ELUKYSEL
01581  FIND-GROUP-MEDICARE-VARIATION.                                   ELUKYSEL
01582      SET WS-NO-MEDCA-FOUND TO TRUE.                               ELUKYSEL
01583      SET WS-NO-NON-MEDCA-FOUND TO TRUE.                           ELUKYSEL
01584      PERFORM SEARCH-FOR-GROUP-MEDICARE-VARI                       ELUKYSEL
01585          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
01586            UNTIL    KTG-IDX > KTG-NBR-KEYS                        ELUKYSEL
01587                  OR (    WS-MEDCA-FOUND                           ELUKYSEL
01588                      AND WS-NON-MEDCA-FOUND).                     ELUKYSEL
01589                                                                   ELUKYSEL
01590      IF     WS-MEDCA-FOUND                                        ELUKYSEL
01591         AND WS-NON-MEDCA-FOUND                                    ELUKYSEL
01592      THEN                                                         ELUKYSEL
01593         SET SSB-FR-MED-GRP-VAR TO TRUE                            ELUKYSEL
01594      ELSE                                                         ELUKYSEL
01595         SET SSB-FR-MED-GRP-NVAR TO TRUE                           ELUKYSEL
01596      END-IF.                                                      ELUKYSEL
01597                                                                   ELUKYSEL
01598 ************************************************************      ELUKYSEL
01599 *                                                          *      ELUKYSEL
01600 *        SEARCH FOR GROUP MEDICARE VARIATION               *      ELUKYSEL
01601 *                                                          *      ELUKYSEL
01602 ************************************************************      ELUKYSEL
01603  SEARCH-FOR-GROUP-MEDICARE-VARI.                                  ELUKYSEL
01604      MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-FRL-MEDICARE.           ELUKYSEL
01605      IF KTG-SEL (KTG-IDX)                                         ELUKYSEL
01606      THEN                                                         ELUKYSEL
01607         IF WS-FRL-MEDIC-ONLY                                      ELUKYSEL
01608         THEN                                                      ELUKYSEL
01609            SET WS-MEDCA-FOUND TO TRUE                             ELUKYSEL
01610         ELSE                                                      ELUKYSEL
01611            SET WS-NON-MEDCA-FOUND TO TRUE                         ELUKYSEL
01612         END-IF                                                    ELUKYSEL
01613      ELSE                                                         ELUKYSEL
01614         CONTINUE                                                  ELUKYSEL
01615      END-IF.                                                      ELUKYSEL
01616                                                                   ELUKYSEL
01617 /***********************************************************      ELUKYSEL
01618 *                                                          *      ELUKYSEL
01619 *        FIND CONTRACT MEDICARE VARIATION                  *      ELUKYSEL
01620 *                                                          *      ELUKYSEL
01621 ************************************************************      ELUKYSEL
01622  FIND-CONTRACT-MEDICARE-VARIATI.                                  ELUKYSEL
01623      SET WS-NO-MEDCA-FOUND TO TRUE.                               ELUKYSEL
01624      SET WS-NO-NON-MEDCA-FOUND TO TRUE.                           ELUKYSEL
01625      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
01626      PERFORM SEARCH-FOR-CONTRACT-MEDICARE-V                       ELUKYSEL
01627          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01628            UNTIL    KTC-IDX > KTC-NBR-KEYS                        ELUKYSEL
01629                  OR (    WS-MEDCA-FOUND                           ELUKYSEL
01630                      AND WS-NON-MEDCA-FOUND).                     ELUKYSEL
01631                                                                   ELUKYSEL
01632      IF WS-MEDCA-FOUND AND WS-NON-MEDCA-FOUND                     ELUKYSEL
01633      THEN                                                         ELUKYSEL
01634         SET SSB-FR-MED-CONT-VAR (SSB-CONT-VAR-IDX) TO TRUE        ELUKYSEL
01635      ELSE                                                         ELUKYSEL
01636         SET SSB-FR-MED-CONT-NVAR (SSB-CONT-VAR-IDX) TO TRUE       ELUKYSEL
01637      END-IF.                                                      ELUKYSEL
01638                                                                   ELUKYSEL
01639 ************************************************************      ELUKYSEL
01640 *                                                          *      ELUKYSEL
01641 *        SEARCH FOR CONTRACT MEDICARE VARIATION            *      ELUKYSEL
01642 *                                                          *      ELUKYSEL
01643 ************************************************************      ELUKYSEL
01644  SEARCH-FOR-CONTRACT-MEDICARE-V.                                  ELUKYSEL
01645      MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-FRL-MEDICARE.           ELUKYSEL
01646      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELUKYSEL
01647      THEN                                                         ELUKYSEL
01648         IF WS-FRL-MEDIC-ONLY                                      ELUKYSEL
01649         THEN                                                      ELUKYSEL
01650            SET WS-MEDCA-FOUND TO TRUE                             ELUKYSEL
01651         ELSE                                                      ELUKYSEL
01652            SET WS-NON-MEDCA-FOUND TO TRUE                         ELUKYSEL
01653         END-IF                                                    ELUKYSEL
01654      ELSE                                                         ELUKYSEL
01655         CONTINUE                                                  ELUKYSEL
01656      END-IF.                                                      ELUKYSEL
01657                                                                   ELUKYSEL
01658 /***********************************************************      ELUKYSEL
01659 *                                                          *      ELUKYSEL
01660 *        SET UP MEDICARE ELIGIBILITY                       *      ELUKYSEL
01661 *                                                          *      ELUKYSEL
01662 ************************************************************      ELUKYSEL
01663  SET-UP-MEDICARE-ELIGIBILITY.                                     ELUKYSEL
01664      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01665      MOVE 'ELSMEDEL' TO SSB-ACTION-MODULE.                        ELUKYSEL
01666                                                                   ELUKYSEL
01667 ************************************************************      ELUKYSEL
01668 *                                                          *      ELUKYSEL
01669 *        NO MEDICARE VARIATION                             *      ELUKYSEL
01670 *                                                          *      ELUKYSEL
01671 ************************************************************      ELUKYSEL
01672  NO-MEDICARE-VARIATION.                                           ELUKYSEL
01673      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
01674      SET SSB-MEDCA-UNDEF TO TRUE.                                 ELUKYSEL
01675                                                                   ELUKYSEL
01676 /***********************************************************      ELUKYSEL
01677 *                                                          *      ELUKYSEL
01678 *        SELECT FAMILY RELATIONSHIP                        *      ELUKYSEL
01679 *                                                          *      ELUKYSEL
01680 ************************************************************      ELUKYSEL
01681  SELECT-FAMILY-RELATIONSHIP.                                      ELUKYSEL
01682      PERFORM FIND-GROUP-FAM-REL-VARIATION.                        ELUKYSEL
01683      PERFORM FIND-CONTRACT-FAM-REL-VARIATIO                       ELUKYSEL
01684          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
01685            UNTIL SSB-CONT-VAR-IDX > 4.                            ELUKYSEL
01686 *                                                                 ELUKYSEL
01687 *  NOTE - MEDICARE HAS PRIORITY OVER FAMILY RELATIONSHIP FOR      ELUKYSEL
01688 *         THE APPROPRIATE GROUP OR CONTRACT TYPE.                 ELUKYSEL
01689 *                                                                 ELUKYSEL
01690 *  NOTE - I QUESTION THE DETAILS OF THIS LOGIC.  THERE MAY NEED   ELUKYSEL
01691 *         TO BE CONSIDERATIONS FOR CONTRACT VS. GROUP.  RJL       ELUKYSEL
01692 *                                                                 ELUKYSEL
01693      IF     SSB-MEDCA-ELIG                                        ELUKYSEL
01694         AND                                                       ELUKYSEL
01695             (   (    SSB-FR-MED-GRP-VAR                           ELUKYSEL
01696                  AND SSB-FR-FR-GRP-VAR       )                    ELUKYSEL
01697              OR (    SSB-FR-MED-CONT-VAR (1)                      ELUKYSEL
01698                  AND SSB-FR-FR-CONT-VAR (1)  )                    ELUKYSEL
01699              OR (    SSB-FR-MED-CONT-VAR (2)                      ELUKYSEL
01700                  AND SSB-FR-FR-CONT-VAR (2)  )                    ELUKYSEL
01701              OR (    SSB-FR-MED-CONT-VAR (3)                      ELUKYSEL
01702                  AND SSB-FR-FR-CONT-VAR (3)  )                    ELUKYSEL
01703              OR (    SSB-FR-MED-CONT-VAR (4)                      ELUKYSEL
01704                  AND SSB-FR-FR-CONT-VAR (4)  ) )                  ELUKYSEL
01705          PERFORM NO-FAMILY-RELATIONSHIP-VARIATI                   ELUKYSEL
01706      ELSE                                                         ELUKYSEL
01707          PERFORM SELECT-FAMILY-RELATIONSHIP-AGA.                  ELUKYSEL
01708                                                                   ELUKYSEL
01709 ************************************************************      ELUKYSEL
01710 *                                                          *      ELUKYSEL
01711 *        SELECT FAMILY RELATIONSHIP AGAIN                  *      ELUKYSEL
01712 *                                                          *      ELUKYSEL
01713 ************************************************************      ELUKYSEL
01714  SELECT-FAMILY-RELATIONSHIP-AGA.                                  ELUKYSEL
01715      IF    SSB-FR-FR-GRP-VAR                                      ELUKYSEL
01716         OR (    (   SSB-PROV-CLASS-INST                           ELUKYSEL
01717                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01718             AND (   SSB-FR-FR-CONT-VAR (1)                        ELUKYSEL
01719                  OR SSB-FR-FR-CONT-VAR (2)) )                     ELUKYSEL
01720         OR (    (   SSB-PROV-CLASS-PROF                           ELUKYSEL
01721                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01722             AND (   SSB-FR-FR-CONT-VAR (3)                        ELUKYSEL
01723                  OR SSB-FR-FR-CONT-VAR (4)) )                     ELUKYSEL
01724      THEN                                                         ELUKYSEL
01725         PERFORM SET-UP-FAMILY-RELATIONSHIP                        ELUKYSEL
01726      ELSE                                                         ELUKYSEL
01727         PERFORM NO-FAMILY-RELATIONSHIP-VARIATI.                   ELUKYSEL
01728                                                                   ELUKYSEL
01729 ************************************************************      ELUKYSEL
01730 *                                                          *      ELUKYSEL
01731 *        NO FAMILY RELATIONSHIP VARIATION                  *      ELUKYSEL
01732 *                                                          *      ELUKYSEL
01733 ************************************************************      ELUKYSEL
01734  NO-FAMILY-RELATIONSHIP-VARIATI.                                  ELUKYSEL
01735      PERFORM NO-GROUP-FAM-REL-VARIATION-FOU.                      ELUKYSEL
01736      PERFORM NO-CONTRACT-FAM-REL-VARIATIONX.                      ELUKYSEL
01737      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
01738      SET SSB-FR-UNDEF TO TRUE.                                    ELUKYSEL
01739                                                                   ELUKYSEL
01740 ************************************************************      ELUKYSEL
01741 *                                                          *      ELUKYSEL
01742 *        FIND GROUP FAM REL VARIATION                      *      ELUKYSEL
01743 *                                                          *      ELUKYSEL
01744 ************************************************************      ELUKYSEL
01745  FIND-GROUP-FAM-REL-VARIATION.                                    ELUKYSEL
01746      SET WS-NO-FM-RL-FOUND TO TRUE.                               ELUKYSEL
01747      SET WS-NO-NON-FM-RL-FOUND TO TRUE.                           ELUKYSEL
01748      MOVE SPACE  TO  WS-FAM-REL-LVL.                              ELUKYSEL
01749      PERFORM SEARCH-FOR-GROUP-FAM-REL-VARIA                       ELUKYSEL
01750         VARYING KTG-IDX FROM 1 BY 1                               ELUKYSEL
01751           UNTIL    KTG-IDX > KTG-NBR-KEYS                         ELUKYSEL
01752                 OR (    WS-FM-RL-FOUND                            ELUKYSEL
01753                     AND WS-NON-FM-RL-FOUND).                      ELUKYSEL
01754      IF WS-FM-RL-FOUND AND WS-NON-FM-RL-FOUND                     ELUKYSEL
01755      THEN                                                         ELUKYSEL
01756         PERFORM GROUP-FAM-REL-VARIATION-FOUND                     ELUKYSEL
01757      ELSE                                                         ELUKYSEL
01758         PERFORM NO-GROUP-FAM-REL-VARIATION-FOU.                   ELUKYSEL
01759                                                                   ELUKYSEL
01760 ************************************************************      ELUKYSEL
01761 *                                                          *      ELUKYSEL
01762 *        SEARCH FOR GROUP FAM REL VARIATION                *      ELUKYSEL
01763 *                                                          *      ELUKYSEL
01764 ************************************************************      ELUKYSEL
01765  SEARCH-FOR-GROUP-FAM-REL-VARIA.                                  ELUKYSEL
01766      IF KTG-SEL (KTG-IDX)                                         ELUKYSEL
01767      THEN                                                         ELUKYSEL
01768         MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-FR-FOUND-LIST        ELUKYSEL
01769         IF WS-FR-FND-LIST                                         ELUKYSEL
01770            SET WS-FM-RL-FOUND TO TRUE                             ELUKYSEL
01771         ELSE                                                      ELUKYSEL
01772            PERFORM GROUP-NON-FAM-REL-FOUND                        ELUKYSEL
01773      ELSE                                                         ELUKYSEL
01774         CONTINUE.                                                 ELUKYSEL
01775                                                                   ELUKYSEL
01776 ************************************************************      ELUKYSEL
01777 *                                                          *      ELUKYSEL
01778 *        GROUP NON FAM REL FOUND                           *      ELUKYSEL
01779 *                                                          *      ELUKYSEL
01780 ************************************************************      ELUKYSEL
01781  GROUP-NON-FAM-REL-FOUND.                                         ELUKYSEL
01782      SET WS-NON-FM-RL-FOUND TO TRUE.                              ELUKYSEL
01783      IF WS-FAM-REL-LVL = SPACE                                    ELUKYSEL
01784      THEN                                                         ELUKYSEL
01785         MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-FAM-REL-LVL          ELUKYSEL
01786      ELSE                                                         ELUKYSEL
01787         IF WS-FAM-REL-LVL NOT = KTG-FAM-REL-LVL (KTG-IDX)         ELUKYSEL
01788         THEN                                                      ELUKYSEL
01789            SET WS-FM-RL-FOUND TO TRUE                             ELUKYSEL
01790            SET WS-NON-FM-RL-FOUND TO TRUE                         ELUKYSEL
01791         ELSE                                                      ELUKYSEL
01792            CONTINUE.                                              ELUKYSEL
01793                                                                   ELUKYSEL
01794 ************************************************************      ELUKYSEL
01795 *                                                          *      ELUKYSEL
01796 *        GROUP FAM REL VARIATION FOUND                     *      ELUKYSEL
01797 *                                                          *      ELUKYSEL
01798 ************************************************************      ELUKYSEL
01799  GROUP-FAM-REL-VARIATION-FOUND.                                   ELUKYSEL
01800      SET SSB-FR-FR-GRP-VAR TO TRUE.                               ELUKYSEL
01801                                                                   ELUKYSEL
01802 ************************************************************      ELUKYSEL
01803 *                                                          *      ELUKYSEL
01804 *        NO GROUP FAM REL VARIATION FOUND                  *      ELUKYSEL
01805 *                                                          *      ELUKYSEL
01806 ************************************************************      ELUKYSEL
01807  NO-GROUP-FAM-REL-VARIATION-FOU.                                  ELUKYSEL
01808      SET SSB-FR-FR-GRP-NVAR TO TRUE.                              ELUKYSEL
01809                                                                   ELUKYSEL
01810 ************************************************************      ELUKYSEL
01811 *                                                          *      ELUKYSEL
01812 *        FIND CONTRACT FAM REL VARIATION                   *      ELUKYSEL
01813 *                                                          *      ELUKYSEL
01814 ************************************************************      ELUKYSEL
01815  FIND-CONTRACT-FAM-REL-VARIATIO.                                  ELUKYSEL
01816      SET WS-NO-FM-RL-FOUND TO TRUE.                               ELUKYSEL
01817      SET WS-NO-NON-FM-RL-FOUND TO TRUE.                           ELUKYSEL
01818      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
01819      MOVE SPACES  TO  WS-FAM-REL-LVL.                             ELUKYSEL
01820      PERFORM SEARCH-FOR-CONTRACT-FAM-REL-VA                       ELUKYSEL
01821          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
01822            UNTIL    KTC-IDX > KTC-NBR-KEYS                        ELUKYSEL
01823                  OR (    WS-FM-RL-FOUND                           ELUKYSEL
01824                      AND WS-NON-FM-RL-FOUND).                     ELUKYSEL
01825      IF WS-FM-RL-FOUND AND WS-NON-FM-RL-FOUND                     ELUKYSEL
01826      THEN                                                         ELUKYSEL
01827         PERFORM CONTRACT-FAM-REL-VARIATION-FOU                    ELUKYSEL
01828      ELSE                                                         ELUKYSEL
01829         PERFORM NO-CONTRACT-FAM-REL-VARIATIONX.                   ELUKYSEL
01830                                                                   ELUKYSEL
01831 ************************************************************      ELUKYSEL
01832 *                                                          *      ELUKYSEL
01833 *        SEARCH FOR CONTRACT FAM REL VARIATION             *      ELUKYSEL
01834 *                                                          *      ELUKYSEL
01835 ************************************************************      ELUKYSEL
01836  SEARCH-FOR-CONTRACT-FAM-REL-VA.                                  ELUKYSEL
01837      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELUKYSEL
01838      THEN                                                         ELUKYSEL
01839         MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-FR-FOUND-LIST        ELUKYSEL
01840         IF WS-FR-FND-LIST                                         ELUKYSEL
01841         THEN                                                      ELUKYSEL
01842            SET WS-FM-RL-FOUND TO TRUE                             ELUKYSEL
01843         ELSE                                                      ELUKYSEL
01844            PERFORM CONTRACT-NON-FAM-REL-FOUND.                    ELUKYSEL
01845                                                                   ELUKYSEL
01846 ************************************************************      ELUKYSEL
01847 *                                                          *      ELUKYSEL
01848 *        CONTRACT NON FAM REL FOUND                        *      ELUKYSEL
01849 *                                                          *      ELUKYSEL
01850 ************************************************************      ELUKYSEL
01851  CONTRACT-NON-FAM-REL-FOUND.                                      ELUKYSEL
01852      SET WS-NON-FM-RL-FOUND TO TRUE.                              ELUKYSEL
01853      IF WS-FAM-REL-LVL = SPACE                                    ELUKYSEL
01854      THEN                                                         ELUKYSEL
01855         MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-FAM-REL-LVL          ELUKYSEL
01856      ELSE                                                         ELUKYSEL
01857         IF WS-FAM-REL-LVL NOT = KTC-FAM-REL-LVL (KTC-IDX)         ELUKYSEL
01858         THEN                                                      ELUKYSEL
01859            SET WS-FM-RL-FOUND TO TRUE                             ELUKYSEL
01860            SET WS-NON-FM-RL-FOUND TO TRUE                         ELUKYSEL
01861         ELSE                                                      ELUKYSEL
01862            CONTINUE.                                              ELUKYSEL
01863                                                                   ELUKYSEL
01864 ************************************************************      ELUKYSEL
01865 *                                                          *      ELUKYSEL
01866 *        CONTRACT FAM REL VARIATION FOUND                  *      ELUKYSEL
01867 *                                                          *      ELUKYSEL
01868 ************************************************************      ELUKYSEL
01869  CONTRACT-FAM-REL-VARIATION-FOU.                                  ELUKYSEL
01870      SET SSB-FR-FR-CONT-VAR (SSB-CONT-VAR-IDX) TO TRUE.           ELUKYSEL
01871                                                                   ELUKYSEL
01872 ************************************************************      ELUKYSEL
01873 *                                                          *      ELUKYSEL
01874 *        NO CONTRACT FAM REL VARIATION FOUND               *      ELUKYSEL
01875 *                                                          *      ELUKYSEL
01876 ************************************************************      ELUKYSEL
01877  NO-CONTRACT-FAM-REL-VARIATIONX.                                  ELUKYSEL
01878      SET SSB-FR-FR-CONT-NVAR (SSB-CONT-VAR-IDX) TO TRUE.          ELUKYSEL
01879                                                                   ELUKYSEL
01880 ************************************************************      ELUKYSEL
01881 *                                                          *      ELUKYSEL
01882 *        SET UP FAMILY RELATIONSHIP                        *      ELUKYSEL
01883 *                                                          *      ELUKYSEL
01884 ************************************************************      ELUKYSEL
01885  SET-UP-FAMILY-RELATIONSHIP.                                      ELUKYSEL
01886      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
01887      MOVE 'ELSFAMRL' TO SSB-ACTION-MODULE.                        ELUKYSEL
01888                                                                   ELUKYSEL
01889 ************************************************************      ELUKYSEL
01890 *                                                          *      ELUKYSEL
01891 *        SELECT PATIENT AGE                                *      ELUKYSEL
01892 *                                                          *      ELUKYSEL
01893 ************************************************************      ELUKYSEL
01894  SELECT-PATIENT-AGE.                                              ELUKYSEL
01895      PERFORM FIND-GROUP-PAT-AGE-VARIATION.                        ELUKYSEL
01896      PERFORM FIND-CONTRACT-PAT-AGE-VARIATIO                       ELUKYSEL
01897          VARYING SSB-CONT-VAR-IDX FROM 1 BY 1                     ELUKYSEL
01898                  UNTIL SSB-CONT-VAR-IDX > 4.                      ELUKYSEL
01899 *                                                                 ELUKYSEL
01900 *  NOTE - MEDICARE HAS PRIORITY OVER PATIENT AGE FOR              ELUKYSEL
01901 *         THE APPROPRIATE GROUP OR CONTRACT TYPE.                 ELUKYSEL
01902 *                                                                 ELUKYSEL
01903 *  NOTE - SEE NOTE IN FAMILY RELATIONSHIP VARIATIONS              ELUKYSEL
01904 *  *********************************************************      ELUKYSEL
01905 *  DELETED CHECK FOR SSB-MEDCA-ELIG AND SSB-FR-MED-GRP AND        ELUKYSEL
01906 *                    SSB-FR-MED-CONT-VAR.      RGO. 11/93         ELUKYSEL
01907 *  *********************************************************      ELUKYSEL
01908                                                                   ELUKYSEL
01909      IF              SSB-FR-PT-AGE-GRP-VAR                        ELUKYSEL
01910                   OR SSB-FR-PT-AGE-CONT-VAR (1)                   ELUKYSEL
01911                   OR SSB-FR-PT-AGE-CONT-VAR (2)                   ELUKYSEL
01912                   OR SSB-FR-PT-AGE-CONT-VAR (3)                   ELUKYSEL
01913                  OR  SSB-FR-PT-AGE-CONT-VAR (4)                   ELUKYSEL
01914          PERFORM SELECT-PATIENT-AGE-AGAIN                         ELUKYSEL
01915      ELSE                                                         ELUKYSEL
01916          PERFORM NO-PATIENT-AGE-VARIATION.                        ELUKYSEL
01917                                                                   ELUKYSEL
01918 ************************************************************      ELUKYSEL
01919 *                                                          *      ELUKYSEL
01920 *        SELECT PATIENT AGE AGAIN                          *      ELUKYSEL
01921 *                                                          *      ELUKYSEL
01922 ************************************************************      ELUKYSEL
01923  SELECT-PATIENT-AGE-AGAIN.                                        ELUKYSEL
01924      IF    SSB-FR-PT-AGE-GRP-VAR                                  ELUKYSEL
01925         OR (    (   SSB-PROV-CLASS-INST                           ELUKYSEL
01926                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01927             AND (   SSB-FR-PT-AGE-CONT-VAR (1)                    ELUKYSEL
01928                  OR SSB-FR-PT-AGE-CONT-VAR (2) )  )               ELUKYSEL
01929         OR (    (   SSB-PROV-CLASS-PROF                           ELUKYSEL
01930                  OR SSB-PROV-CLASS-BOTH)                          ELUKYSEL
01931             AND (   SSB-FR-PT-AGE-CONT-VAR (3)                    ELUKYSEL
01932                  OR SSB-FR-PT-AGE-CONT-VAR (4) )  )               ELUKYSEL
01933          PERFORM SET-UP-PATIENT-AGE                               ELUKYSEL
01934      ELSE                                                         ELUKYSEL
01935          PERFORM NO-PATIENT-AGE-VARIATION.                        ELUKYSEL
01936                                                                   ELUKYSEL
01937 ************************************************************      ELUKYSEL
01938 *                                                          *      ELUKYSEL
01939 *        NO PATIENT AGE VARIATION                          *      ELUKYSEL
01940 *                                                          *      ELUKYSEL
01941 ************************************************************      ELUKYSEL
01942  NO-PATIENT-AGE-VARIATION.                                        ELUKYSEL
01943      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
01944      SET SSB-PT-AGE-UNDEF TO TRUE.                                ELUKYSEL
01945                                                                   ELUKYSEL
01946 ************************************************************      ELUKYSEL
01947 *                                                          *      ELUKYSEL
01948 *        FIND GROUP PAT AGE VARIATION                      *      ELUKYSEL
01949 *                                                          *      ELUKYSEL
01950 ************************************************************      ELUKYSEL
01951  FIND-GROUP-PAT-AGE-VARIATION.                                    ELUKYSEL
01952      SET WS-NO-PT-AG-FOUND TO TRUE.                               ELUKYSEL
01953      SET WS-NO-NON-PT-AG-FOUND TO TRUE.                           ELUKYSEL
01954      PERFORM SEARCH-FOR-GROUP-PAT-AGE-VARIA.                      ELUKYSEL
01955      IF WS-PT-AG-FOUND                                            ELUKYSEL
01956          PERFORM GROUP-PAT-AGE-VARIATION-FOUND                    ELUKYSEL
01957      ELSE                                                         ELUKYSEL
01958          PERFORM NO-GROUP-PAT-AGE-VARIATION-FOU.                  ELUKYSEL
01959                                                                   ELUKYSEL
01960 ************************************************************      ELUKYSEL
01961 *                                                          *      ELUKYSEL
01962 *        SEARCH FOR GROUP PAT AGE VARIATION                *      ELUKYSEL
01963 *                                                          *      ELUKYSEL
01964 ************************************************************      ELUKYSEL
01965  SEARCH-FOR-GROUP-PAT-AGE-VARIA.                                  ELUKYSEL
01966      SET KTG-IDX TO 1.                                            ELUKYSEL
01967      SEARCH KTG-KEY-TBL  VARYING KTG-IDX                          ELUKYSEL
01968         AT END                                                    ELUKYSEL
01969            SET WS-NON-PT-AG-FOUND TO TRUE                         ELUKYSEL
01970         WHEN KTG-SEL (KTG-IDX)                                    ELUKYSEL
01971            MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-PT-AGE-TEST-1     ELUKYSEL
01972         END-SEARCH.                                               ELUKYSEL
01973      PERFORM SEARCH-FOR-GROUP-PAT-AGE-AGAIN                       ELUKYSEL
01974         UNTIL    KTG-IDX > KTG-NBR-KEYS                           ELUKYSEL
01975               OR WS-PT-AG-FOUND.                                  ELUKYSEL
01976                                                                   ELUKYSEL
01977 ************************************************************      ELUKYSEL
01978 *                                                          *      ELUKYSEL
01979 *        SEARCH FOR GROUP PAT AGE AGAIN                    *      ELUKYSEL
01980 *                                                          *      ELUKYSEL
01981 ************************************************************      ELUKYSEL
01982  SEARCH-FOR-GROUP-PAT-AGE-AGAIN.                                  ELUKYSEL
01983      MOVE KTG-FAM-REL-LVL (KTG-IDX) TO WS-PT-AGE-TEST-2.          ELUKYSEL
01984      IF     KTG-SEL (KTG-IDX)                                     ELUKYSEL
01985         AND WS-PT-AGE-TEST-1 NOT = WS-PT-AGE-TEST-2               ELUKYSEL
01986      THEN                                                         ELUKYSEL
01987         PERFORM VERIFY-VARYING-FAM-REL-CODES.                     ELUKYSEL
01988      SET KTG-IDX UP BY 1.                                         ELUKYSEL
01989                                                                   ELUKYSEL
01990 ************************************************************      ELUKYSEL
01991 *                                                          *      ELUKYSEL
01992 *        GROUP PAT AGE VARIATION FOUND                     *      ELUKYSEL
01993 *                                                          *      ELUKYSEL
01994 ************************************************************      ELUKYSEL
01995  GROUP-PAT-AGE-VARIATION-FOUND.                                   ELUKYSEL
01996      SET SSB-FR-PT-AGE-GRP-VAR TO TRUE.                           ELUKYSEL
01997                                                                   ELUKYSEL
01998 ************************************************************      ELUKYSEL
01999 *                                                          *      ELUKYSEL
02000 *        NO GROUP PAT AGE VARIATION FOUND                  *      ELUKYSEL
02001 *                                                          *      ELUKYSEL
02002 ************************************************************      ELUKYSEL
02003  NO-GROUP-PAT-AGE-VARIATION-FOU.                                  ELUKYSEL
02004      SET SSB-FR-PT-AGE-GRP-NVAR TO TRUE.                          ELUKYSEL
02005      MOVE WS-PT-AGE-TEST-1 TO SSB-GRP-FAM-REL-LVL.                ELUKYSEL
02006                                                                   ELUKYSEL
02007 ************************************************************      ELUKYSEL
02008 *                                                          *      ELUKYSEL
02009 *        FIND CONTRACT PAT AGE VARIATION                   *      ELUKYSEL
02010 *                                                          *      ELUKYSEL
02011 ************************************************************      ELUKYSEL
02012  FIND-CONTRACT-PAT-AGE-VARIATIO.                                  ELUKYSEL
02013      SET WS-NO-PT-AG-FOUND TO TRUE.                               ELUKYSEL
02014      SET WS-NO-NON-PT-AG-FOUND TO TRUE.                           ELUKYSEL
02015      SET KTC-SEL-IDX TO SSB-CONT-VAR-IDX.                         ELUKYSEL
02016      PERFORM SEARCH-FOR-CONTRACT-PAT-AGE-VA.                      ELUKYSEL
02017      IF WS-PT-AG-FOUND                                            ELUKYSEL
02018          PERFORM CONTRACT-PAT-AGE-VARIATION-FOU                   ELUKYSEL
02019      ELSE                                                         ELUKYSEL
02020          PERFORM NO-CONTRACT-PAT-AGE-VARIATIONX.                  ELUKYSEL
02021                                                                   ELUKYSEL
02022                                                                   ELUKYSEL
02023 ************************************************************      ELUKYSEL
02024 *                                                          *      ELUKYSEL
02025 *        SEARCH FOR CONTRACT PAT AGE VARIATION             *      ELUKYSEL
02026 *                                                          *      ELUKYSEL
02027 ************************************************************      ELUKYSEL
02028  SEARCH-FOR-CONTRACT-PAT-AGE-VA.                                  ELUKYSEL
02029      SET KTC-IDX TO 1.                                            ELUKYSEL
02030      SEARCH KTC-KEY-TBL  VARYING KTC-IDX                          ELUKYSEL
02031         AT END                                                    ELUKYSEL
02032            SET WS-NON-PT-AG-FOUND TO TRUE                         ELUKYSEL
02033         WHEN KTC-SEL (KTC-IDX, KTC-SEL-IDX)                       ELUKYSEL
02034            MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-PT-AGE-TEST-1     ELUKYSEL
02035         END-SEARCH.                                               ELUKYSEL
02036      PERFORM SEARCH-FOR-CONTRACT-PAT-AGE-AG                       ELUKYSEL
02037          UNTIL    KTC-IDX > KTC-NBR-KEYS                          ELUKYSEL
02038                OR WS-PT-AG-FOUND.                                 ELUKYSEL
02039                                                                   ELUKYSEL
02040 ************************************************************      ELUKYSEL
02041 *                                                          *      ELUKYSEL
02042 *        SEARCH FOR CONTRACT PAT AGE AGAIN                 *      ELUKYSEL
02043 *                                                          *      ELUKYSEL
02044 ************************************************************      ELUKYSEL
02045  SEARCH-FOR-CONTRACT-PAT-AGE-AG.                                  ELUKYSEL
02046      MOVE KTC-FAM-REL-LVL (KTC-IDX) TO WS-PT-AGE-TEST-2.          ELUKYSEL
02047      IF     KTC-SEL (KTC-IDX, KTC-SEL-IDX)                        ELUKYSEL
02048         AND WS-PT-AGE-TEST-1 NOT = WS-PT-AGE-TEST-2               ELUKYSEL
02049      THEN                                                         ELUKYSEL
02050          PERFORM VERIFY-VARYING-FAM-REL-CODES.                    ELUKYSEL
02051      SET KTC-IDX UP BY 1.                                         ELUKYSEL
02052                                                                   ELUKYSEL
02053 ************************************************************      ELUKYSEL
02054 *                                                          *      ELUKYSEL
02055 *        CONTRACT PAT AGE VARIATION FOUND                  *      ELUKYSEL
02056 *                                                          *      ELUKYSEL
02057 ************************************************************      ELUKYSEL
02058  CONTRACT-PAT-AGE-VARIATION-FOU.                                  ELUKYSEL
02059      SET SSB-FR-PT-AGE-CONT-VAR (SSB-CONT-VAR-IDX) TO TRUE.       ELUKYSEL
02060                                                                   ELUKYSEL
02061 ************************************************************      ELUKYSEL
02062 *                                                          *      ELUKYSEL
02063 *        NO CONTRACT PAT AGE VARIATION FOUND               *      ELUKYSEL
02064 *                                                          *      ELUKYSEL
02065 ************************************************************      ELUKYSEL
02066  NO-CONTRACT-PAT-AGE-VARIATIONX.                                  ELUKYSEL
02067      SET SSB-FR-PT-AGE-CONT-NVAR (SSB-CONT-VAR-IDX) TO TRUE.      ELUKYSEL
02068      SET SSB-CONT-IDX TO SSB-CONT-VAR-IDX.                        ELUKYSEL
02069      MOVE WS-PT-AGE-TEST-1                                        ELUKYSEL
02070        TO SSB-CONT-FAM-REL-LVL (SSB-CONT-IDX).                    ELUKYSEL
02071                                                                   ELUKYSEL
02072 ************************************************************      ELUKYSEL
02073 *                                                          *      ELUKYSEL
02074 *        VERIFY VARYING FAM REL CODES                      *      ELUKYSEL
02075 *                                                          *      ELUKYSEL
02076 ************************************************************      ELUKYSEL
02077  VERIFY-VARYING-FAM-REL-CODES.                                    ELUKYSEL
02078      IF WS-PT-AGE-SAME-1 AND WS-PT-AGE-SAME-2                     ELUKYSEL
02079          CONTINUE                                                 ELUKYSEL
02080      ELSE                                                         ELUKYSEL
02081          SET WS-PT-AG-FOUND TO TRUE.                              ELUKYSEL
02082                                                                   ELUKYSEL
02083 ************************************************************      ELUKYSEL
02084 *                                                          *      ELUKYSEL
02085 *        SET UP PATIENT AGE                                *      ELUKYSEL
02086 *                                                          *      ELUKYSEL
02087 ************************************************************      ELUKYSEL
02088  SET-UP-PATIENT-AGE.                                              ELUKYSEL
02089      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
02090      MOVE 'ELSPTAGE' TO SSB-ACTION-MODULE.                        ELUKYSEL
02091                                                                   ELUKYSEL
02092 ************************************************************      ELUKYSEL
02093 *                                                          *      ELUKYSEL
02094 *        SELECT GROUP SPECIFIC EFFECTIVE DATE              *      ELUKYSEL
02095 *                                                          *      ELUKYSEL
02096 ************************************************************      ELUKYSEL
02097  SELECT-GROUP-SPECIFIC-EFFECTIV.                                  ELUKYSEL
02098      PERFORM FIND-GROUP-EFF-DATE-VARIATIONS.                      ELUKYSEL
02099      IF WS-GRP-EFF-DT-VAR                                         ELUKYSEL
02100          PERFORM SET-UP-GROUP-EFFECTIVE-DATE                      ELUKYSEL
02101      ELSE                                                         ELUKYSEL
02102          PERFORM NO-GROUP-SPECIFIC-EFFECTIVE-DA.                  ELUKYSEL
02103                                                                   ELUKYSEL
02104 ************************************************************      ELUKYSEL
02105 *                                                          *      ELUKYSEL
02106 *        FIND GROUP EFF DATE VARIATIONS                    *      ELUKYSEL
02107 *                                                          *      ELUKYSEL
02108 ************************************************************      ELUKYSEL
02109  FIND-GROUP-EFF-DATE-VARIATIONS.                                  ELUKYSEL
02110      SET KTG-IDX TO 1.                                            ELUKYSEL
02111      SEARCH KTG-KEY-TBL VARYING KTG-IDX                           ELUKYSEL
02112          AT END                                                   ELUKYSEL
02113              SET WS-GRP-EFF-DT-NVAR TO TRUE                       ELUKYSEL
02114          WHEN KTG-SEL (KTG-IDX)                                   ELUKYSEL
02115              MOVE KTG-EFF-DT-CENTURY  (KTG-IDX) TO WS-EFF-DT      ELUKYSEL
02116              MOVE KTG-TERM-DT-CENTURY (KTG-IDX) TO WS-TERMN-DT    ELUKYSEL
02117          END-SEARCH.                                              ELUKYSEL
02118      SEARCH KTG-KEY-TBL VARYING KTG-IDX                           ELUKYSEL
02119          AT END                                                   ELUKYSEL
02120              SET WS-GRP-EFF-DT-NVAR TO TRUE                       ELUKYSEL
02121          WHEN KTG-SEL (KTG-IDX)                                   ELUKYSEL
02122           AND WS-EFF-DT NOT = KTG-EFF-DT-CENTURY (KTG-IDX)        ELUKYSEL
02123              SET WS-GRP-EFF-DT-VAR TO TRUE                        ELUKYSEL
02124          END-SEARCH.                                              ELUKYSEL
02125                                                                   ELUKYSEL
02126 ************************************************************      ELUKYSEL
02127 *                                                          *      ELUKYSEL
02128 *        SET UP GROUP EFFECTIVE DATE                       *      ELUKYSEL
02129 *                                                          *      ELUKYSEL
02130 ************************************************************      ELUKYSEL
02131  SET-UP-GROUP-EFFECTIVE-DATE.                                     ELUKYSEL
02132      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
02133      MOVE 'ELSGRPSP' TO SSB-ACTION-MODULE.                        ELUKYSEL
02134                                                                   ELUKYSEL
02135 ************************************************************      ELUKYSEL
02136 *                                                          *      ELUKYSEL
02137 *        NO GROUP SPECIFIC EFFECTIVE DATE VARIATION        *      ELUKYSEL
02138 *                                                          *      ELUKYSEL
02139 ************************************************************      ELUKYSEL
02140  NO-GROUP-SPECIFIC-EFFECTIVE-DA.                                  ELUKYSEL
02141      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
02142      MOVE WS-EFF-DT    TO  SSB-GROUP-EFF-DATE-CEN.                ELUKYSEL
02143      MOVE WS-TERMN-DT  TO  SSB-GROUP-TERM-DATE-CEN.               ELUKYSEL
02144      PERFORM ELIM-CONTRACTS-BY-LINE-OF.                           ELUKYSEL
02145                                                                   ELUKYSEL
02146 ************************************************************      ELUKYSEL
02147 *                                                          *      ELUKYSEL
02148 *        ELIMINATE CONTRACTS BY LINE OF BUSINESS CONFLICT  *      ELUKYSEL
02149 *                                                          *      ELUKYSEL
02150 ************************************************************      ELUKYSEL
02151  ELIM-CONTRACTS-BY-LINE-OF.                                       ELUKYSEL
02152      PERFORM LOCATE-GROUP-ITEM-IN-TABLE.                          ELUKYSEL
02153      IF     KTG-SEL (KTG-IDX)                                     ELUKYSEL
02154         AND KTG-L-O-B-CONTRACT-LEVEL-IND (KTG-IDX) NOT = '00'     ELUKYSEL
02155      THEN                                                         ELUKYSEL
02156          PERFORM SCAN-CONTRACT-TABLE-FOR-LOB.                     ELUKYSEL
02157                                                                   ELUKYSEL
02158 ************************************************************      ELUKYSEL
02159 *                                                          *      ELUKYSEL
02160 *        LOCATE GROUP ITEM IN TABLE                        *      ELUKYSEL
02161 *                                                          *      ELUKYSEL
02162 ************************************************************      ELUKYSEL
02163  LOCATE-GROUP-ITEM-IN-TABLE.                                      ELUKYSEL
02164      SET KTG-IDX TO 1.                                            ELUKYSEL
02165      SEARCH KTG-KEY-TBL  VARYING KTG-IDX                          ELUKYSEL
02166         AT END                                                    ELUKYSEL
02167            SET KTG-IDX TO 1                                       ELUKYSEL
02168         WHEN KTG-SEL (KTG-IDX)                                    ELUKYSEL
02169            CONTINUE                                               ELUKYSEL
02170        END-SEARCH.                                                ELUKYSEL
02171                                                                   ELUKYSEL
02172 ************************************************************      ELUKYSEL
02173 *                                                          *      ELUKYSEL
02174 *        SCAN CONTRACT TABLE FOR LOB                       *      ELUKYSEL
02175 *                                                          *      ELUKYSEL
02176 ************************************************************      ELUKYSEL
02177  SCAN-CONTRACT-TABLE-FOR-LOB.                                     ELUKYSEL
02178      MOVE KTG-L-O-B-CONTRACT-LEVEL-IND (KTG-IDX)                  ELUKYSEL
02179          TO WS-L-O-B-CONTRACT-LEVEL.                              ELUKYSEL
02180      PERFORM COMPARE-EACH-CONTRACT-LOB                            ELUKYSEL
02181          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
02182            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
02183                                                                   ELUKYSEL
02184 ************************************************************      ELUKYSEL
02185 *                                                          *      ELUKYSEL
02186 *        COMPARE EACH CONTRACT LOB                         *      ELUKYSEL
02187 *                                                          *      ELUKYSEL
02188 ************************************************************      ELUKYSEL
02189  COMPARE-EACH-CONTRACT-LOB.                                       ELUKYSEL
02190      MOVE KTC-L-O-B (KTC-IDX) TO WS-L-O-B-IND.                    ELUKYSEL
02191      IF WS-L-O-B-IND NOT = '0'                                    ELUKYSEL
02192          PERFORM COMPARE-AVAILABLE-LOB.                           ELUKYSEL
02193                                                                   ELUKYSEL
02194 ************************************************************      ELUKYSEL
02195 *                                                          *      ELUKYSEL
02196 *        COMPARE AVAILABLE LOB                             *      ELUKYSEL
02197 *                                                          *      ELUKYSEL
02198 ************************************************************      ELUKYSEL
02199  COMPARE-AVAILABLE-LOB.                                           ELUKYSEL
02200      SEARCH ALL WS-L-O-B-TABLE                                    ELUKYSEL
02201          AT END                                                   ELUKYSEL
02202            SET WS-L-O-B-CONFLICT TO TRUE                          ELUKYSEL
02203          WHEN WS-L-O-B-KEY (WS-L-O-B-INDEX) =                     ELUKYSEL
02204          WS-L-O-B-FUNC                                            ELUKYSEL
02205            SET WS-L-O-B-MATCH TO TRUE                             ELUKYSEL
02206            SET HOLD-INDEX TO WS-L-O-B-INDEX                       ELUKYSEL
02207        END-SEARCH.                                                ELUKYSEL
02208      IF WS-L-O-B-CONFLICT                                         ELUKYSEL
02209          PERFORM REJECT-CONTRACT-ITEM                             ELUKYSEL
02210      ELSE                                                         ELUKYSEL
02211          PERFORM CHECK-FOR-CONFLICTS.                             ELUKYSEL
02212                                                                   ELUKYSEL
02213 ************************************************************      ELUKYSEL
02214 *                                                          *      ELUKYSEL
02215 *        CHECK FOR CONFLICTS                               *      ELUKYSEL
02216 *                                                          *      ELUKYSEL
02217 ************************************************************      ELUKYSEL
02218  CHECK-FOR-CONFLICTS.                                             ELUKYSEL
02219      IF WS-INST-BAS-REJECT (HOLD-INDEX)                           ELUKYSEL
02220          PERFORM REJECT-INST-BAS-CONTRACT.                        ELUKYSEL
02221      IF WS-INST-SUP-REJECT (HOLD-INDEX)                           ELUKYSEL
02222          PERFORM REJECT-INST-SUP-CONTRACT.                        ELUKYSEL
02223      IF WS-PROF-BAS-REJECT (HOLD-INDEX)                           ELUKYSEL
02224          PERFORM REJECT-PROF-BAS-CONTRACT.                        ELUKYSEL
02225      IF WS-PROF-SUP-REJECT (HOLD-INDEX)                           ELUKYSEL
02226          PERFORM REJECT-PROF-SUP-CONTRACT.                        ELUKYSEL
02227                                                                   ELUKYSEL
02228 ************************************************************      ELUKYSEL
02229 *                                                          *      ELUKYSEL
02230 *        SELECT BASIC INSTITUTIONAL PROVIDER CONTROL       *      ELUKYSEL
02231 *                                                          *      ELUKYSEL
02232 ************************************************************      ELUKYSEL
02233  SELECT-BASIC-INSTITUTIONAL-PRO.                                  ELUKYSEL
02234      SET KTC-SEL-IDX TO 1.                                        ELUKYSEL
02235      IF SSB-PROV-CLASS-PROF                                       ELUKYSEL
02236          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02237      ELSE                                                         ELUKYSEL
02238          PERFORM MORE-BASIC-INSTITUTIONAL-PROVI.                  ELUKYSEL
02239                                                                   ELUKYSEL
02240 ************************************************************      ELUKYSEL
02241 *                                                          *      ELUKYSEL
02242 *        MORE BASIC INSTITUTIONAL PROVIDER CONTROL         *      ELUKYSEL
02243 *                                                          *      ELUKYSEL
02244 ************************************************************      ELUKYSEL
02245  MORE-BASIC-INSTITUTIONAL-PROVI.                                  ELUKYSEL
02246      PERFORM DETERMINE-PROVIDER-CONTROL-VAR.                      ELUKYSEL
02247      IF WS-PC-VAR                                                 ELUKYSEL
02248          PERFORM SET-UP-PROV-CNTL                                 ELUKYSEL
02249      ELSE                                                         ELUKYSEL
02250          PERFORM NO-BASIC-INST-PROV-CNTL-VARIAT.                  ELUKYSEL
02251                                                                   ELUKYSEL
02252 ************************************************************      ELUKYSEL
02253 *                                                          *      ELUKYSEL
02254 *        SELECT SUPPLEMENTAL INSTITUTIONAL PROVIDER CONTROL*      ELUKYSEL
02255 *                                                          *      ELUKYSEL
02256 ************************************************************      ELUKYSEL
02257  SELECT-SUPPLEMENTAL-INSTITUTIO.                                  ELUKYSEL
02258      SET KTC-SEL-IDX TO 2.                                        ELUKYSEL
02259      IF SSB-PROV-CLASS-PROF                                       ELUKYSEL
02260          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02261      ELSE                                                         ELUKYSEL
02262          PERFORM MORE-SUPPLEMENTAL-INSTITUTIONA.                  ELUKYSEL
02263                                                                   ELUKYSEL
02264 ************************************************************      ELUKYSEL
02265 *                                                          *      ELUKYSEL
02266 *        MORE SUPPLEMENTAL INSTITUTIONAL PROVIDER CONTROL  *      ELUKYSEL
02267 *                                                          *      ELUKYSEL
02268 ************************************************************      ELUKYSEL
02269  MORE-SUPPLEMENTAL-INSTITUTIONA.                                  ELUKYSEL
02270      PERFORM DETERMINE-PROVIDER-CONTROL-VAR.                      ELUKYSEL
02271      IF WS-PC-VAR                                                 ELUKYSEL
02272          PERFORM MATCH-SUPPL-TO-BASIC-INST-PC                     ELUKYSEL
02273      ELSE                                                         ELUKYSEL
02274          PERFORM NO-SUPPL-INST-PROV-CNTL-VARIAT.                  ELUKYSEL
02275                                                                   ELUKYSEL
02276 ************************************************************      ELUKYSEL
02277 *                                                          *      ELUKYSEL
02278 *        MATCH SUPPL TO BASIC INST PC                      *      ELUKYSEL
02279 *                                                          *      ELUKYSEL
02280 ************************************************************      ELUKYSEL
02281  MATCH-SUPPL-TO-BASIC-INST-PC.                                    ELUKYSEL
02282      PERFORM FIND-INST-BASIC-SUPPL-MATCH.                         ELUKYSEL
02283      IF WS-MATCH-FOUND                                            ELUKYSEL
02284          PERFORM SELECT-INST-PC-MATCH                             ELUKYSEL
02285      ELSE                                                         ELUKYSEL
02286          PERFORM SET-UP-PROV-CNTL.                                ELUKYSEL
02287                                                                   ELUKYSEL
02288 ************************************************************      ELUKYSEL
02289 *                                                          *      ELUKYSEL
02290 *        FIND INST BASIC SUPPL MATCH                       *      ELUKYSEL
02291 *                                                          *      ELUKYSEL
02292 ************************************************************      ELUKYSEL
02293  FIND-INST-BASIC-SUPPL-MATCH.                                     ELUKYSEL
02294      SET KTC-IDX TO 1.                                            ELUKYSEL
02295      SEARCH KTC-KEY-TBL   VARYING KTC-IDX                         ELUKYSEL
02296          AT END                                                   ELUKYSEL
02297              SET WS-MATCH-NOT-FOUND TO TRUE                       ELUKYSEL
02298          WHEN     KTC-INST-SUP-SEL (KTC-IDX)                      ELUKYSEL
02299               AND SSB-INST-BAS-L-O-B = KTC-L-O-B (KTC-IDX)        ELUKYSEL
02300               AND   SSB-INST-BAS-PROVDR-CONTROL                   ELUKYSEL
02301                   = KTC-PROVDR-CONTROL (KTC-IDX)                  ELUKYSEL
02302               AND   SSB-INST-BAS-FAM-REL-LVL                      ELUKYSEL
02303                   = KTC-FAM-REL-LVL (KTC-IDX)                     ELUKYSEL
02304               AND   SSB-INST-BAS-EFF-DT                           ELUKYSEL
02305                   = KTC-EFF-DT-CENTURY (KTC-IDX)                  ELUKYSEL
02306             SET WS-MATCH-FOUND TO TRUE                            ELUKYSEL
02307         END-SEARCH.                                               ELUKYSEL
02308                                                                   ELUKYSEL
02309 ************************************************************      ELUKYSEL
02310 *                                                          *      ELUKYSEL
02311 *        SELECT INST PC MATCH                              *      ELUKYSEL
02312 *                                                          *      ELUKYSEL
02313 ************************************************************      ELUKYSEL
02314  SELECT-INST-PC-MATCH.                                            ELUKYSEL
02315      MOVE SSB-INST-BAS-CONTRACT TO SSB-INST-SUP-CONTRACT.         ELUKYSEL
02316      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
02317                                                                   ELUKYSEL
02318 ************************************************************      ELUKYSEL
02319 *                                                          *      ELUKYSEL
02320 *        SELECT BASIC PROFESSIONAL PROVIDER CONTROL        *      ELUKYSEL
02321 *                                                          *      ELUKYSEL
02322 ************************************************************      ELUKYSEL
02323  SELECT-BASIC-PROFESSIONAL-PROV.                                  ELUKYSEL
02324      SET KTC-SEL-IDX TO 3.                                        ELUKYSEL
02325      IF SSB-PROV-CLASS-INST                                       ELUKYSEL
02326          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02327      ELSE                                                         ELUKYSEL
02328          PERFORM MORE-BASIC-PROFESSIONAL-PROVID.                  ELUKYSEL
02329                                                                   ELUKYSEL
02330 ************************************************************      ELUKYSEL
02331 *                                                          *      ELUKYSEL
02332 *        MORE BASIC PROFESSIONAL PROVIDER CONTROL          *      ELUKYSEL
02333 *                                                          *      ELUKYSEL
02334 ************************************************************      ELUKYSEL
02335  MORE-BASIC-PROFESSIONAL-PROVID.                                  ELUKYSEL
02336      PERFORM DETERMINE-PROVIDER-CONTROL-VAR.                      ELUKYSEL
02337      IF WS-PC-VAR                                                 ELUKYSEL
02338          PERFORM SET-UP-PROV-CNTL                                 ELUKYSEL
02339      ELSE                                                         ELUKYSEL
02340          PERFORM NO-BASIC-PROF-PROV-CNTL-VARIAT.                  ELUKYSEL
02341                                                                   ELUKYSEL
02342 ************************************************************      ELUKYSEL
02343 *                                                          *      ELUKYSEL
02344 *        SELECT SUPPLEMENTAL PROFESSIONAL PROVIDER CONTROL *      ELUKYSEL
02345 *                                                          *      ELUKYSEL
02346 ************************************************************      ELUKYSEL
02347  SELECT-SUPPLEMENTAL-PROFESSION.                                  ELUKYSEL
02348      SET KTC-SEL-IDX TO 4.                                        ELUKYSEL
02349      IF SSB-PROV-CLASS-INST                                       ELUKYSEL
02350          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02351      ELSE                                                         ELUKYSEL
02352          PERFORM MORE-SUPPLEMENTAL-PROFESSIONAL.                  ELUKYSEL
02353                                                                   ELUKYSEL
02354 ************************************************************      ELUKYSEL
02355 *                                                          *      ELUKYSEL
02356 *        MORE SUPPLEMENTAL PROFESSIONAL PROVIDER CONTROL   *      ELUKYSEL
02357 *                                                          *      ELUKYSEL
02358 ************************************************************      ELUKYSEL
02359  MORE-SUPPLEMENTAL-PROFESSIONAL.                                  ELUKYSEL
02360      PERFORM DETERMINE-PROVIDER-CONTROL-VAR.                      ELUKYSEL
02361      IF WS-PC-VAR                                                 ELUKYSEL
02362          PERFORM MATCH-SUPPL-TO-BASIC-PROF-PC                     ELUKYSEL
02363      ELSE                                                         ELUKYSEL
02364          PERFORM NO-SUPPL-PROF-PROV-CNTL-VARIAT.                  ELUKYSEL
02365                                                                   ELUKYSEL
02366 ************************************************************      ELUKYSEL
02367 *                                                          *      ELUKYSEL
02368 *        MATCH SUPPL TO BASIC PROF PC                      *      ELUKYSEL
02369 *                                                          *      ELUKYSEL
02370 ************************************************************      ELUKYSEL
02371  MATCH-SUPPL-TO-BASIC-PROF-PC.                                    ELUKYSEL
02372      PERFORM FIND-PROF-BASIC-SUPL-MATCH.                          ELUKYSEL
02373      IF WS-MATCH-FOUND                                            ELUKYSEL
02374          PERFORM SELECT-SUPPL-PROF-MATCH                          ELUKYSEL
02375      ELSE                                                         ELUKYSEL
02376          PERFORM SET-UP-PROV-CNTL.                                ELUKYSEL
02377                                                                   ELUKYSEL
02378 ************************************************************      ELUKYSEL
02379 *                                                          *      ELUKYSEL
02380 *        FIND PROF BASIC SUPL MATCH                        *      ELUKYSEL
02381 *                                                          *      ELUKYSEL
02382 ************************************************************      ELUKYSEL
02383  FIND-PROF-BASIC-SUPL-MATCH.                                      ELUKYSEL
02384      SET KTC-IDX TO 1.                                            ELUKYSEL
02385      SEARCH KTC-KEY-TBL   VARYING KTC-IDX                         ELUKYSEL
02386          AT END                                                   ELUKYSEL
02387              SET WS-MATCH-NOT-FOUND TO TRUE                       ELUKYSEL
02388          WHEN     KTC-PROF-SUP-SEL (KTC-IDX)                      ELUKYSEL
02389               AND   SSB-PROF-BAS-L-O-B                            ELUKYSEL
02390                   = KTC-L-O-B (KTC-IDX)                           ELUKYSEL
02391               AND   SSB-PROF-BAS-PROVDR-CONTROL                   ELUKYSEL
02392                   = KTC-PROVDR-CONTROL (KTC-IDX)                  ELUKYSEL
02393               AND   SSB-PROF-BAS-FAM-REL-LVL                      ELUKYSEL
02394                   = KTC-FAM-REL-LVL (KTC-IDX)                     ELUKYSEL
02395               AND   SSB-PROF-BAS-EFF-DT                           ELUKYSEL
02396                   = KTC-EFF-DT-CENTURY (KTC-IDX)                  ELUKYSEL
02397             SET WS-MATCH-FOUND TO TRUE                            ELUKYSEL
02398         END-SEARCH.                                               ELUKYSEL
02399                                                                   ELUKYSEL
02400 ************************************************************      ELUKYSEL
02401 *                                                          *      ELUKYSEL
02402 *        SELECT SUPPL PROF MATCH                           *      ELUKYSEL
02403 *                                                          *      ELUKYSEL
02404 ************************************************************      ELUKYSEL
02405  SELECT-SUPPL-PROF-MATCH.                                         ELUKYSEL
02406      MOVE SSB-PROF-BAS-CONTRACT TO SSB-PROF-SUP-CONTRACT.         ELUKYSEL
02407      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
02408                                                                   ELUKYSEL
02409 ************************************************************      ELUKYSEL
02410 *                                                          *      ELUKYSEL
02411 *        DETERMINE PROVIDER CONTROL VARIATION              *      ELUKYSEL
02412 *                                                          *      ELUKYSEL
02413 ************************************************************      ELUKYSEL
02414  DETERMINE-PROVIDER-CONTROL-VAR.                                  ELUKYSEL
02415      PERFORM ELIM-SOME-PROVIDER-CONTRO.                           ELUKYSEL
02416      SET KTC-IDX TO 1.                                            ELUKYSEL
02417      SEARCH KTC-KEY-TBL VARYING KTC-IDX                           ELUKYSEL
02418         AT END                                                    ELUKYSEL
02419            SET WS-NO-PC-VAR TO TRUE                               ELUKYSEL
02420         WHEN KTC-SEL (KTC-IDX, KTC-SEL-IDX)                       ELUKYSEL
02421            MOVE KTC-PROVDR-CONTROL (KTC-IDX)                      ELUKYSEL
02422              TO WS-PROVIDER-CONTROL                               ELUKYSEL
02423         END-SEARCH.                                               ELUKYSEL
02424      SEARCH KTC-KEY-TBL VARYING KTC-IDX                           ELUKYSEL
02425         AT END                                                    ELUKYSEL
02426            SET WS-NO-PC-VAR TO TRUE                               ELUKYSEL
02427         WHEN     KTC-SEL (KTC-IDX, KTC-SEL-IDX)                   ELUKYSEL
02428              AND       WS-PROVIDER-CONTROL                        ELUKYSEL
02429                  NOT = KTC-PROVDR-CONTROL (KTC-IDX)               ELUKYSEL
02430            SET WS-PC-VAR TO TRUE                                  ELUKYSEL
02431         END-SEARCH.                                               ELUKYSEL
02432                                                                   ELUKYSEL
02433 ************************************************************      ELUKYSEL
02434 *                                                          *      ELUKYSEL
02435 *        ELIMINATE SOME PROVIDER CONTROLS                  *      ELUKYSEL
02436 *                                                          *      ELUKYSEL
02437 ************************************************************      ELUKYSEL
02438  ELIM-SOME-PROVIDER-CONTRO.                                       ELUKYSEL
02439      PERFORM DETERMINE-THE-SELECTED-GROUP.                        ELUKYSEL
02440      IF WS-KTG-FOUND                                              ELUKYSEL
02441          PERFORM PROCESS-SELECTED-GROUP.                          ELUKYSEL
02442                                                                   ELUKYSEL
02443 ************************************************************      ELUKYSEL
02444 *                                                          *      ELUKYSEL
02445 *        PROCESS SELECTED GROUP                            *      ELUKYSEL
02446 *                                                          *      ELUKYSEL
02447 ************************************************************      ELUKYSEL
02448  PROCESS-SELECTED-GROUP.                                          ELUKYSEL
02449      PERFORM INITIALIZE-CONSTANT-INDEX-ITEM.                      ELUKYSEL
02450      PERFORM DETERMINE-LOB-INDEX.                                 ELUKYSEL
02451      IF WS-LOB-INDEX-FOUND                                        ELUKYSEL
02452          PERFORM PROCESS-PROVIDER-CONTROL-ELIMI.                  ELUKYSEL
02453                                                                   ELUKYSEL
02454 ************************************************************      ELUKYSEL
02455 *                                                          *      ELUKYSEL
02456 *        INITIALIZE CONSTANT INDEX ITEMS                   *      ELUKYSEL
02457 *                                                          *      ELUKYSEL
02458 ************************************************************      ELUKYSEL
02459  INITIALIZE-CONSTANT-INDEX-ITEM.                                  ELUKYSEL
02460      SET PCT-LOB-ELIM-IDX TO KTC-SEL-IDX.                         ELUKYSEL
02461      SET PCT-LOB-LVL-IDX  TO KTC-SEL-IDX.                         ELUKYSEL
02462      SET KTG-CLV-IDX      TO KTC-SEL-IDX.                         ELUKYSEL
02463                                                                   ELUKYSEL
02464 ************************************************************      ELUKYSEL
02465 *                                                          *      ELUKYSEL
02466 *        PROCESS PROVIDER CONTROL ELIMINATION              *      ELUKYSEL
02467 *                                                          *      ELUKYSEL
02468 ************************************************************      ELUKYSEL
02469  PROCESS-PROVIDER-CONTROL-ELIMI.                                  ELUKYSEL
02470      SET PCT-PC-LVL-IDX TO PCT-LOB-IDX.                           ELUKYSEL
02471      PERFORM PROVIDER-CONTROL-ELIMINATION                         ELUKYSEL
02472          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
02473            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
02474                                                                   ELUKYSEL
02475 ************************************************************      ELUKYSEL
02476 *                                                          *      ELUKYSEL
02477 *        PROVIDER CONTROL ELIMINATION                      *      ELUKYSEL
02478 *                                                          *      ELUKYSEL
02479 ************************************************************      ELUKYSEL
02480  PROVIDER-CONTROL-ELIMINATION.                                    ELUKYSEL
02481      IF KTC-SEL (KTC-IDX, KTC-SEL-IDX)                            ELUKYSEL
02482          PERFORM PROCESS-ACTIVE-CONTRACT.                         ELUKYSEL
02483                                                                   ELUKYSEL
02484 ************************************************************      ELUKYSEL
02485 *                                                          *      ELUKYSEL
02486 *        PROCESS ACTIVE CONTRACT                           *      ELUKYSEL
02487 *                                                          *      ELUKYSEL
02488 ************************************************************      ELUKYSEL
02489  PROCESS-ACTIVE-CONTRACT.                                         ELUKYSEL
02490      PERFORM DETERMINE-PC-INDEX.                                  ELUKYSEL
02491      IF WS-PC-INDEX-FOUND                                         ELUKYSEL
02492          PERFORM DETERMINE-IF-ELIMINATION-APPLI.                  ELUKYSEL
02493                                                                   ELUKYSEL
02494 ************************************************************      ELUKYSEL
02495 *                                                          *      ELUKYSEL
02496 *        DETERMINE IF ELIMINATION APPLIES                  *      ELUKYSEL
02497 *                                                          *      ELUKYSEL
02498 ************************************************************      ELUKYSEL
02499  DETERMINE-IF-ELIMINATION-APPLI.                                  ELUKYSEL
02500      IF NOT PCT-LOB-LVL-NOT-USED (PCT-LOB-IDX, PCT-LOB-LVL-IDX)   ELUKYSEL
02501          PERFORM TEST-FOR-REJECTION.                              ELUKYSEL
02502                                                                   ELUKYSEL
02503 ************************************************************      ELUKYSEL
02504 *                                                          *      ELUKYSEL
02505 *        TEST FOR REJECTION                                *      ELUKYSEL
02506 *                                                          *      ELUKYSEL
02507 ************************************************************      ELUKYSEL
02508  TEST-FOR-REJECTION.                                              ELUKYSEL
02509      SET PCT-PC-IDX TO PCT-PC-TO-IDX.                             ELUKYSEL
02510      IF PCT-REJ (PCT-LOB-ELIM-IDX, PCT-PC-IDX, PCT-PC-LVL-IDX)    ELUKYSEL
02511          PERFORM REJECT-SPECIFIC-CONTRACT-ITEM.                   ELUKYSEL
02512                                                                   ELUKYSEL
02513 ************************************************************      ELUKYSEL
02514 *                                                          *      ELUKYSEL
02515 *        DETERMINE THE SELECTED GROUP                      *      ELUKYSEL
02516 *                                                          *      ELUKYSEL
02517 ************************************************************      ELUKYSEL
02518  DETERMINE-THE-SELECTED-GROUP.                                    ELUKYSEL
02519      SET KTG-IDX TO 1.                                            ELUKYSEL
02520      SEARCH KTG-KEY-TBL VARYING KTG-IDX                           ELUKYSEL
02521          AT END                                                   ELUKYSEL
02522              SET WS-KTG-NOT-FOUND TO TRUE                         ELUKYSEL
02523          WHEN KTG-SEL (KTG-IDX)                                   ELUKYSEL
02524              SET WS-KTG-FOUND TO TRUE                             ELUKYSEL
02525         END-SEARCH.                                               ELUKYSEL
02526                                                                   ELUKYSEL
02527 ************************************************************      ELUKYSEL
02528 *                                                          *      ELUKYSEL
02529 *        DETERMINE LOB INDEX                               *      ELUKYSEL
02530 *                                                          *      ELUKYSEL
02531 ************************************************************      ELUKYSEL
02532  DETERMINE-LOB-INDEX.                                             ELUKYSEL
02533      SET PCT-LOB-IDX  TO  1.                                      ELUKYSEL
02534      SEARCH PCT-LOB-LVL-TABLE  VARYING PCT-LOB-IDX                ELUKYSEL
02535         AT END                                                    ELUKYSEL
02536            SET WS-LOB-INDEX-NOT-FOUND TO TRUE                     ELUKYSEL
02537         WHEN   KTG-PC-LVL-IND (KTG-IDX, KTG-CLV-IDX)              ELUKYSEL
02538              = PCT-LOB-LVL-CODE (PCT-LOB-IDX)                     ELUKYSEL
02539            SET WS-LOB-INDEX-FOUND TO TRUE                         ELUKYSEL
02540         END-SEARCH.                                               ELUKYSEL
02541                                                                   ELUKYSEL
02542 ************************************************************      ELUKYSEL
02543 *                                                          *      ELUKYSEL
02544 *        DETERMINE PC INDEX                                *      ELUKYSEL
02545 *                                                          *      ELUKYSEL
02546 ************************************************************      ELUKYSEL
02547  DETERMINE-PC-INDEX.                                              ELUKYSEL
02548      SEARCH ALL PCT-PC-TO-INDEX-TABLE                             ELUKYSEL
02549         AT END                                                    ELUKYSEL
02550            SET WS-PC-INDEX-NOT-FOUND TO TRUE                      ELUKYSEL
02551         WHEN   PCT-PC-CODE (PCT-PC-TO-IDX)                        ELUKYSEL
02552              = KTC-PROVDR-CONTROL (KTC-IDX)                       ELUKYSEL
02553            SET WS-PC-INDEX-FOUND TO TRUE                          ELUKYSEL
02554         END-SEARCH.                                               ELUKYSEL
02555                                                                   ELUKYSEL
02556 ************************************************************      ELUKYSEL
02557 *                                                          *      ELUKYSEL
02558 *        SET UP PROV CNTL                                  *      ELUKYSEL
02559 *                                                          *      ELUKYSEL
02560 ************************************************************      ELUKYSEL
02561  SET-UP-PROV-CNTL.                                                ELUKYSEL
02562      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
02563      MOVE 'ELSPRCTL' TO SSB-ACTION-MODULE.                        ELUKYSEL
02564                                                                   ELUKYSEL
02565 ************************************************************      ELUKYSEL
02566 *                                                          *      ELUKYSEL
02567 *        NO BASIC INST PROV CNTL VARIATION                 *      ELUKYSEL
02568 *                                                          *      ELUKYSEL
02569 ************************************************************      ELUKYSEL
02570  NO-BASIC-INST-PROV-CNTL-VARIAT.                                  ELUKYSEL
02571      PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV.                      ELUKYSEL
02572      MOVE WS-PROVIDER-CONTROL TO SSB-INST-BAS-PROVDR-CONTROL.     ELUKYSEL
02573                                                                   ELUKYSEL
02574 ************************************************************      ELUKYSEL
02575 *                                                          *      ELUKYSEL
02576 *        NO SUPPL INST PROV CNTL VARIATION                 *      ELUKYSEL
02577 *                                                          *      ELUKYSEL
02578 ************************************************************      ELUKYSEL
02579  NO-SUPPL-INST-PROV-CNTL-VARIAT.                                  ELUKYSEL
02580      PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV.                      ELUKYSEL
02581      MOVE WS-PROVIDER-CONTROL TO SSB-INST-SUP-PROVDR-CONTROL.     ELUKYSEL
02582                                                                   ELUKYSEL
02583 ************************************************************      ELUKYSEL
02584 *                                                          *      ELUKYSEL
02585 *        NO BASIC PROF PROV CNTL VARIATION                 *      ELUKYSEL
02586 *                                                          *      ELUKYSEL
02587 ************************************************************      ELUKYSEL
02588  NO-BASIC-PROF-PROV-CNTL-VARIAT.                                  ELUKYSEL
02589      PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV.                      ELUKYSEL
02590      MOVE WS-PROVIDER-CONTROL TO SSB-PROF-BAS-PROVDR-CONTROL.     ELUKYSEL
02591                                                                   ELUKYSEL
02592 ************************************************************      ELUKYSEL
02593 *                                                          *      ELUKYSEL
02594 *        NO SUPPL PROF PROV CNTL VARIATION                 *      ELUKYSEL
02595 *                                                          *      ELUKYSEL
02596 ************************************************************      ELUKYSEL
02597  NO-SUPPL-PROF-PROV-CNTL-VARIAT.                                  ELUKYSEL
02598      PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV.                      ELUKYSEL
02599      MOVE WS-PROVIDER-CONTROL TO SSB-PROF-SUP-PROVDR-CONTROL.     ELUKYSEL
02600                                                                   ELUKYSEL
02601 ************************************************************      ELUKYSEL
02602 *                                                          *      ELUKYSEL
02603 *        SELECT INSTITUTIONAL BASIC CONTRACT EFFECTIVE DATE*      ELUKYSEL
02604 *                                                          *      ELUKYSEL
02605 ************************************************************      ELUKYSEL
02606  SELECT-INSTITUTIONAL-BASIC-CON.                                  ELUKYSEL
02607      SET KTC-SEL-IDX TO 1.                                        ELUKYSEL
02608      IF SSB-PROV-CLASS-PROF                                       ELUKYSEL
02609          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02610      ELSE                                                         ELUKYSEL
02611          PERFORM MORE-INSTITUTIONAL-BASIC-CONTR.                  ELUKYSEL
02612                                                                   ELUKYSEL
02613 ************************************************************      ELUKYSEL
02614 *                                                          *      ELUKYSEL
02615 *        MORE INSTITUTIONAL BASIC CONTRACT EFFECTIVE DATE  *      ELUKYSEL
02616 *                                                          *      ELUKYSEL
02617 ************************************************************      ELUKYSEL
02618  MORE-INSTITUTIONAL-BASIC-CONTR.                                  ELUKYSEL
02619      PERFORM DETERMINE-CONTRACT-EFF-DATE-VA.                      ELUKYSEL
02620      IF WS-EFF-DATE-VAR                                           ELUKYSEL
02621          PERFORM SET-UP-CONTRACT-EFF-DATE                         ELUKYSEL
02622      ELSE                                                         ELUKYSEL
02623          PERFORM NO-INST-BASIC-CONT-EFF-DATE-VA.                  ELUKYSEL
02624                                                                   ELUKYSEL
02625 ************************************************************      ELUKYSEL
02626 *                                                          *      ELUKYSEL
02627 *        SELECT INSTITUTIONAL SUPPL CONTRACT EFFECTIVE DATE*      ELUKYSEL
02628 *                                                          *      ELUKYSEL
02629 ************************************************************      ELUKYSEL
02630  SELECT-INSTITUTIONAL-SUPPL-CON.                                  ELUKYSEL
02631      SET KTC-SEL-IDX TO 2.                                        ELUKYSEL
02632      IF SSB-PROV-CLASS-PROF                                       ELUKYSEL
02633          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02634      ELSE                                                         ELUKYSEL
02635          PERFORM MORE-INST-SUPPL-CONTRACT-EFF-D.                  ELUKYSEL
02636                                                                   ELUKYSEL
02637 ************************************************************      ELUKYSEL
02638 *                                                          *      ELUKYSEL
02639 *        MORE INST SUPPL CONTRACT EFF DATE PROCESSING      *      ELUKYSEL
02640 *                                                          *      ELUKYSEL
02641 ************************************************************      ELUKYSEL
02642  MORE-INST-SUPPL-CONTRACT-EFF-D.                                  ELUKYSEL
02643      PERFORM ELIM-CONFLICTS-WITH-INSTX.                           ELUKYSEL
02644      PERFORM DETERMINE-CONTRACT-EFF-DATE-VA.                      ELUKYSEL
02645      IF WS-EFF-DATE-VAR                                           ELUKYSEL
02646          PERFORM SET-UP-CONTRACT-EFF-DATE                         ELUKYSEL
02647      ELSE                                                         ELUKYSEL
02648          PERFORM NO-INST-SUPPL-CONT-EFF-DATE-VA.                  ELUKYSEL
02649                                                                   ELUKYSEL
02650 ************************************************************      ELUKYSEL
02651 *                                                          *      ELUKYSEL
02652 *        ELIMINATE CONFLICTS WITH INST BASIC CONTRACT DATE *      ELUKYSEL
02653 *                                                          *      ELUKYSEL
02654 ************************************************************      ELUKYSEL
02655  ELIM-CONFLICTS-WITH-INSTX.                                       ELUKYSEL
02656      SET WS-NO-ITEM-REJECTED TO TRUE.                             ELUKYSEL
02657      PERFORM CHECK-INST-SUPL-ON-EFF-DATE                          ELUKYSEL
02658          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
02659            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
02660                                                                   ELUKYSEL
02661 ************************************************************      ELUKYSEL
02662 *                                                          *      ELUKYSEL
02663 *        CHECK INST SUPL ON EFF DATE                       *      ELUKYSEL
02664 *                                                          *      ELUKYSEL
02665 ************************************************************      ELUKYSEL
02666  CHECK-INST-SUPL-ON-EFF-DATE.                                     ELUKYSEL
02667      IF    KTC-EFF-DT-CENTURY (KTC-IDX) >                         ELUKYSEL
02668               SSB-INST-BAS-TERMN-DT-CEN                           ELUKYSEL
02669         OR KTC-TERM-DT-CENTURY (KTC-IDX)                          ELUKYSEL
02670                      < SSB-INST-BAS-EFF-DT-CEN                    ELUKYSEL
02671      THEN                                                         ELUKYSEL
02672         PERFORM REJECT-INST-SUPL-ON-EFF-DATE.                     ELUKYSEL
02673                                                                   ELUKYSEL
02674 ************************************************************      ELUKYSEL
02675 *                                                          *      ELUKYSEL
02676 *        REJECT INST SUPL ON EFF DATE                      *      ELUKYSEL
02677 *                                                          *      ELUKYSEL
02678 ************************************************************      ELUKYSEL
02679  REJECT-INST-SUPL-ON-EFF-DATE.                                    ELUKYSEL
02680      SET KTC-INST-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02681      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02682      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02683                                                                   ELUKYSEL
02684 ************************************************************      ELUKYSEL
02685 *                                                          *      ELUKYSEL
02686 *        SELECT PROFESSIONAL BASIC CONTRACT EFFECTIVE DATE *      ELUKYSEL
02687 *                                                          *      ELUKYSEL
02688 ************************************************************      ELUKYSEL
02689  SELECT-PROFESSIONAL-BASIC-CONT.                                  ELUKYSEL
02690      SET KTC-SEL-IDX TO 3.                                        ELUKYSEL
02691      IF SSB-PROV-CLASS-INST                                       ELUKYSEL
02692          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02693      ELSE                                                         ELUKYSEL
02694          PERFORM MORE-PROFESSIONAL-BASIC-CONTRA.                  ELUKYSEL
02695                                                                   ELUKYSEL
02696 ************************************************************      ELUKYSEL
02697 *                                                          *      ELUKYSEL
02698 *        MORE PROFESSIONAL BASIC CONTRACT EFFECTIVE DATE   *      ELUKYSEL
02699 *                                                          *      ELUKYSEL
02700 ************************************************************      ELUKYSEL
02701  MORE-PROFESSIONAL-BASIC-CONTRA.                                  ELUKYSEL
02702      PERFORM DETERMINE-CONTRACT-EFF-DATE-VA.                      ELUKYSEL
02703      IF WS-EFF-DATE-VAR                                           ELUKYSEL
02704          PERFORM SET-UP-CONTRACT-EFF-DATE                         ELUKYSEL
02705      ELSE                                                         ELUKYSEL
02706          PERFORM NO-PROF-BASIC-CONT-EFF-DATE-VA.                  ELUKYSEL
02707                                                                   ELUKYSEL
02708 ************************************************************      ELUKYSEL
02709 *                                                          *      ELUKYSEL
02710 *        SELECT PROFESSIONAL SUPPL CONTRACT EFFECTIVE DATE *      ELUKYSEL
02711 *                                                          *      ELUKYSEL
02712 ************************************************************      ELUKYSEL
02713  SELECT-PROFESSIONAL-SUPPL-CONT.                                  ELUKYSEL
02714      SET KTC-SEL-IDX TO 4.                                        ELUKYSEL
02715      IF SSB-PROV-CLASS-INST                                       ELUKYSEL
02716          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U                   ELUKYSEL
02717      ELSE                                                         ELUKYSEL
02718          PERFORM MORE-PROF-SUPPL-CONTRACT-EFF-D.                  ELUKYSEL
02719                                                                   ELUKYSEL
02720 ************************************************************      ELUKYSEL
02721 *                                                          *      ELUKYSEL
02722 *        MORE PROF SUPPL CONTRACT EFF DATE PROCESSING      *      ELUKYSEL
02723 *                                                          *      ELUKYSEL
02724 ************************************************************      ELUKYSEL
02725  MORE-PROF-SUPPL-CONTRACT-EFF-D.                                  ELUKYSEL
02726      PERFORM ELIM-CONFLICTS-WITH-PROFX.                           ELUKYSEL
02727      PERFORM DETERMINE-CONTRACT-EFF-DATE-VA.                      ELUKYSEL
02728      IF WS-EFF-DATE-VAR                                           ELUKYSEL
02729          PERFORM SET-UP-CONTRACT-EFF-DATE                         ELUKYSEL
02730      ELSE                                                         ELUKYSEL
02731          PERFORM NO-PROF-SUPPL-CONT-EFF-DATE-VA.                  ELUKYSEL
02732                                                                   ELUKYSEL
02733 ************************************************************      ELUKYSEL
02734 *                                                          *      ELUKYSEL
02735 *        ELIMINATE CONFLICTS WITH PROF BASIC CONTRACT DATE *      ELUKYSEL
02736 *                                                          *      ELUKYSEL
02737 ************************************************************      ELUKYSEL
02738  ELIM-CONFLICTS-WITH-PROFX.                                       ELUKYSEL
02739      SET WS-NO-ITEM-REJECTED TO TRUE.                             ELUKYSEL
02740      PERFORM CHECK-PROF-SUPL-ON-EFF-DATE                          ELUKYSEL
02741          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
02742            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
02743                                                                   ELUKYSEL
02744 ************************************************************      ELUKYSEL
02745 *                                                          *      ELUKYSEL
02746 *        CHECK PROF SUPL ON EFF DATE                       *      ELUKYSEL
02747 *                                                          *      ELUKYSEL
02748 ************************************************************      ELUKYSEL
02749  CHECK-PROF-SUPL-ON-EFF-DATE.                                     ELUKYSEL
02750      IF KTC-EFF-DT-CENTURY (KTC-IDX) > SSB-PROF-BAS-TERM-DT-CEN   ELUKYSEL
02751        OR KTC-TERM-DT-CENTURY (KTC-IDX) < SSB-PROF-BAS-EFF-DT-CEN ELUKYSEL
02752      THEN                                                         ELUKYSEL
02753         PERFORM REJECT-PROF-SUPL-ON-EFF-DATE.                     ELUKYSEL
02754                                                                   ELUKYSEL
02755 ************************************************************      ELUKYSEL
02756 *                                                          *      ELUKYSEL
02757 *        REJECT PROF SUPL ON EFF DATE                      *      ELUKYSEL
02758 *                                                          *      ELUKYSEL
02759 ************************************************************      ELUKYSEL
02760  REJECT-PROF-SUPL-ON-EFF-DATE.                                    ELUKYSEL
02761      SET KTC-PROF-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02762      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02763      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02764                                                                   ELUKYSEL
02765 ************************************************************      ELUKYSEL
02766 *                                                          *      ELUKYSEL
02767 *        SET UP CONTRACT EFF DATE                          *      ELUKYSEL
02768 *                                                          *      ELUKYSEL
02769 ************************************************************      ELUKYSEL
02770  SET-UP-CONTRACT-EFF-DATE.                                        ELUKYSEL
02771      SET WS-DATA-ITEM-FOUND TO TRUE.                              ELUKYSEL
02772      MOVE 'ELSCONTR' TO SSB-ACTION-MODULE.                        ELUKYSEL
02773                                                                   ELUKYSEL
02774 ************************************************************      ELUKYSEL
02775 *                                                          *      ELUKYSEL
02776 *        NO INST BASIC CONT EFF DATE VARIATION             *      ELUKYSEL
02777 *                                                          *      ELUKYSEL
02778 ************************************************************      ELUKYSEL
02779  NO-INST-BASIC-CONT-EFF-DATE-VA.                                  ELUKYSEL
02780      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
02781      MOVE WS-EFF-DT   TO SSB-INST-BAS-EFF-DT-CEN.                 ELUKYSEL
02782      MOVE WS-TERMN-DT TO SSB-INST-BAS-TERMN-DT-CEN.               ELUKYSEL
02783                                                                   ELUKYSEL
02784 ************************************************************      ELUKYSEL
02785 *                                                          *      ELUKYSEL
02786 *        NO INST SUPPL CONT EFF DATE VARIATION             *      ELUKYSEL
02787 *                                                          *      ELUKYSEL
02788 ************************************************************      ELUKYSEL
02789  NO-INST-SUPPL-CONT-EFF-DATE-VA.                                  ELUKYSEL
02790      IF WS-ITEM-REJECTED AND WS-SEL-CONTRACT-FOUND                ELUKYSEL
02791          PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV                   ELUKYSEL
02792      ELSE                                                         ELUKYSEL
02793          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                  ELUKYSEL
02794      MOVE WS-EFF-DT   TO SSB-INST-SUP-EFF-DT-CEN.                 ELUKYSEL
02795      MOVE WS-TERMN-DT TO SSB-INST-SUP-TERMN-DT-CEN.               ELUKYSEL
02796                                                                   ELUKYSEL
02797 ************************************************************      ELUKYSEL
02798 *                                                          *      ELUKYSEL
02799 *        NO PROF BASIC CONT EFF DATE VARIATION             *      ELUKYSEL
02800 *                                                          *      ELUKYSEL
02801 ************************************************************      ELUKYSEL
02802  NO-PROF-BASIC-CONT-EFF-DATE-VA.                                  ELUKYSEL
02803      PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                      ELUKYSEL
02804      MOVE WS-EFF-DT   TO SSB-PROF-BAS-EFF-DT-CEN.                 ELUKYSEL
02805      MOVE WS-TERMN-DT TO SSB-PROF-BAS-TERM-DT-CEN.                ELUKYSEL
02806                                                                   ELUKYSEL
02807 ************************************************************      ELUKYSEL
02808 *                                                          *      ELUKYSEL
02809 *        NO PROF SUPPL CONT EFF DATE VARIATION             *      ELUKYSEL
02810 *                                                          *      ELUKYSEL
02811 ************************************************************      ELUKYSEL
02812  NO-PROF-SUPPL-CONT-EFF-DATE-VA.                                  ELUKYSEL
02813      IF WS-ITEM-REJECTED AND WS-SEL-CONTRACT-FOUND                ELUKYSEL
02814          PERFORM FLAG-CURRENT-SELECTOR-AS-DERIV                   ELUKYSEL
02815      ELSE                                                         ELUKYSEL
02816          PERFORM FLAG-CURRENT-SELECTOR-AS-NOT-U.                  ELUKYSEL
02817      MOVE WS-EFF-DT   TO SSB-PROF-SUP-EFF-DATE-CC.                ELUKYSEL
02818      MOVE WS-TERMN-DT TO SSB-PROF-SUP-TERM-DT-CC.                 ELUKYSEL
02819                                                                   ELUKYSEL
02820 ************************************************************      ELUKYSEL
02821 *                                                          *      ELUKYSEL
02822 *        DETERMINE CONTRACT EFF DATE VARIATION             *      ELUKYSEL
02823 *                                                          *      ELUKYSEL
02824 ************************************************************      ELUKYSEL
02825  DETERMINE-CONTRACT-EFF-DATE-VA.                                  ELUKYSEL
02826      SET KTC-IDX TO 1.                                            ELUKYSEL
02827      SEARCH KTC-KEY-TBL VARYING KTC-IDX                           ELUKYSEL
02828         AT END                                                    ELUKYSEL
02829            SET WS-NO-EFF-DATE-VAR TO TRUE                         ELUKYSEL
02830            SET WS-ALL-CONTRACTS-REJ TO TRUE                       ELUKYSEL
02831         WHEN KTC-SEL (KTC-IDX, KTC-SEL-IDX)                       ELUKYSEL
02832            MOVE KTC-EFF-DT-CENTURY (KTC-IDX) TO WS-EFF-DT         ELUKYSEL
02833            MOVE KTC-TERM-DT-CENTURY (KTC-IDX) TO WS-TERMN-DT      ELUKYSEL
02834            SET WS-SEL-CONTRACT-FOUND TO TRUE                      ELUKYSEL
02835         END-SEARCH.                                               ELUKYSEL
02836      SEARCH KTC-KEY-TBL VARYING KTC-IDX                           ELUKYSEL
02837         AT END                                                    ELUKYSEL
02838            SET WS-NO-EFF-DATE-VAR TO TRUE                         ELUKYSEL
02839         WHEN     KTC-SEL (KTC-IDX, KTC-SEL-IDX)                   ELUKYSEL
02840              AND WS-EFF-DT NOT = KTC-EFF-DT-CENTURY (KTC-IDX)     ELUKYSEL
02841            SET WS-EFF-DATE-VAR TO TRUE                            ELUKYSEL
02842         END-SEARCH.                                               ELUKYSEL
02843                                                                   ELUKYSEL
02844 ************************************************************      ELUKYSEL
02845 *                                                          *      ELUKYSEL
02846 *        REJECT GROUP ITEM                                 *      ELUKYSEL
02847 *                                                          *      ELUKYSEL
02848 ************************************************************      ELUKYSEL
02849  REJECT-GROUP-ITEM.                                               ELUKYSEL
02850      SET KTG-REJ (KTG-IDX) TO TRUE.                               ELUKYSEL
02851      SET WS-KTG-MODIFIED  TO  TRUE.                               ELUKYSEL
02852      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02853                                                                   ELUKYSEL
02854 ************************************************************      ELUKYSEL
02855 *                                                          *      ELUKYSEL
02856 *        REJECT SPECIFIC CONTRACT ITEM                     *      ELUKYSEL
02857 *                                                          *      ELUKYSEL
02858 ************************************************************      ELUKYSEL
02859  REJECT-SPECIFIC-CONTRACT-ITEM.                                   ELUKYSEL
02860      SET KTC-REJ (KTC-IDX, KTC-SEL-IDX) TO TRUE.                  ELUKYSEL
02861      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02862      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02863                                                                   ELUKYSEL
02864 ************************************************************      ELUKYSEL
02865 *                                                          *      ELUKYSEL
02866 *        REJECT CONTRACT ITEM                              *      ELUKYSEL
02867 *                                                          *      ELUKYSEL
02868 ************************************************************      ELUKYSEL
02869  REJECT-CONTRACT-ITEM.                                            ELUKYSEL
02870      PERFORM REJECT-INST-BAS-CONTRACT.                            ELUKYSEL
02871      PERFORM REJECT-INST-SUP-CONTRACT.                            ELUKYSEL
02872      PERFORM REJECT-PROF-BAS-CONTRACT.                            ELUKYSEL
02873      PERFORM REJECT-PROF-SUP-CONTRACT.                            ELUKYSEL
02874                                                                   ELUKYSEL
02875 ************************************************************      ELUKYSEL
02876 *                                                          *      ELUKYSEL
02877 *        REJECT INST BAS CONTRACT                          *      ELUKYSEL
02878 *                                                          *      ELUKYSEL
02879 ************************************************************      ELUKYSEL
02880  REJECT-INST-BAS-CONTRACT.                                        ELUKYSEL
02881      SET KTC-INST-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02882      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02883      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02884                                                                   ELUKYSEL
02885 ************************************************************      ELUKYSEL
02886 *                                                          *      ELUKYSEL
02887 *        REJECT INST SUP CONTRACT                          *      ELUKYSEL
02888 *                                                          *      ELUKYSEL
02889 ************************************************************      ELUKYSEL
02890  REJECT-INST-SUP-CONTRACT.                                        ELUKYSEL
02891      SET KTC-INST-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02892      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02893      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02894                                                                   ELUKYSEL
02895 ************************************************************      ELUKYSEL
02896 *                                                          *      ELUKYSEL
02897 *        REJECT PROF BAS CONTRACT                          *      ELUKYSEL
02898 *                                                          *      ELUKYSEL
02899 ************************************************************      ELUKYSEL
02900  REJECT-PROF-BAS-CONTRACT.                                        ELUKYSEL
02901      SET KTC-PROF-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02902      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02903      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02904                                                                   ELUKYSEL
02905 ************************************************************      ELUKYSEL
02906 *                                                          *      ELUKYSEL
02907 *        REJECT PROF SUP CONTRACT                          *      ELUKYSEL
02908 *                                                          *      ELUKYSEL
02909 ************************************************************      ELUKYSEL
02910  REJECT-PROF-SUP-CONTRACT.                                        ELUKYSEL
02911      SET KTC-PROF-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02912      SET WS-KTC-MODIFIED  TO  TRUE.                               ELUKYSEL
02913      SET WS-ITEM-REJECTED TO  TRUE.                               ELUKYSEL
02914                                                                   ELUKYSEL
02915 ************************************************************      ELUKYSEL
02916 *                                                          *      ELUKYSEL
02917 *        FLAG CURRENT SELECTOR AS NOT USED                 *      ELUKYSEL
02918 *                                                          *      ELUKYSEL
02919 ************************************************************      ELUKYSEL
02920  FLAG-CURRENT-SELECTOR-AS-NOT-U.                                  ELUKYSEL
02921      SET SSB-NOT-USED (SSB-SELECTOR-STATE) TO TRUE.               ELUKYSEL
02922                                                                   ELUKYSEL
02923 ************************************************************      ELUKYSEL
02924 *                                                          *      ELUKYSEL
02925 *        FLAG CURRENT SELECTOR AS DERIVED                  *      ELUKYSEL
02926 *                                                          *      ELUKYSEL
02927 ************************************************************      ELUKYSEL
02928  FLAG-CURRENT-SELECTOR-AS-DERIV.                                  ELUKYSEL
02929      SET SSB-DATA-DERIVED (SSB-SELECTOR-STATE) TO TRUE.           ELUKYSEL
02930                                                                   ELUKYSEL
02931 ************************************************************      ELUKYSEL
02932 *                                                          *      ELUKYSEL
02933 *        FLAG CURRENT SELECTOR AS COMPLETE                 *      ELUKYSEL
02934 *                                                          *      ELUKYSEL
02935 ************************************************************      ELUKYSEL
02936  FLAG-CURRENT-SELECTOR-AS-COMPL.                                  ELUKYSEL
02937      SET SSB-COMPLETED (SSB-SELECTOR-STATE) TO TRUE.              ELUKYSEL
02938                                                                   ELUKYSEL
02939 ************************************************************      ELUKYSEL
02940 *                                                          *      ELUKYSEL
02941 *        INITIALIZE KEY TABLES                             *      ELUKYSEL
02942 *                                                          *      ELUKYSEL
02943 ************************************************************      ELUKYSEL
02944  INITIALIZE-KEY-TABLES.                                           ELUKYSEL
02945      PERFORM INITIALIZE-GROUP-SELECTION                           ELUKYSEL
02946          VARYING KTG-IDX FROM 1 BY 1                              ELUKYSEL
02947            UNTIL KTG-IDX > KTG-NBR-KEYS.                          ELUKYSEL
02948      PERFORM INITIALIZE-CONTRACT-SELECTION                        ELUKYSEL
02949          VARYING KTC-IDX FROM 1 BY 1                              ELUKYSEL
02950            UNTIL KTC-IDX > KTC-NBR-KEYS.                          ELUKYSEL
02951      SET WS-KTC-MODIFIED  TO TRUE.                                ELUKYSEL
02952      SET WS-KTG-MODIFIED  TO TRUE.                                ELUKYSEL
02953                                                                   ELUKYSEL
02954 ************************************************************      ELUKYSEL
02955 *                                                          *      ELUKYSEL
02956 *        INITIALIZE GROUP SELECTION                        *      ELUKYSEL
02957 *                                                          *      ELUKYSEL
02958 ************************************************************      ELUKYSEL
02959  INITIALIZE-GROUP-SELECTION.                                      ELUKYSEL
02960      SET KTG-SEL (KTG-IDX) TO TRUE.                               ELUKYSEL
02961                                                                   ELUKYSEL
02962 ************************************************************      ELUKYSEL
02963 *                                                          *      ELUKYSEL
02964 *        INITIALIZE CONTRACT SELECTION                     *      ELUKYSEL
02965 *                                                          *      ELUKYSEL
02966 ************************************************************      ELUKYSEL
02967  INITIALIZE-CONTRACT-SELECTION.                                   ELUKYSEL
02968      MOVE KTC-L-O-B (KTC-IDX) TO WS-L-O-B.                        ELUKYSEL
02969      IF WS-L-O-B-COMP                                             ELUKYSEL
02970          PERFORM ACCEPT-COMP-CONTRACT                             ELUKYSEL
02971      ELSE IF WS-L-O-B-SUP                                         ELUKYSEL
02972          PERFORM ACCEPT-SUP-CONTRACT                              ELUKYSEL
02973      ELSE IF WS-L-O-B-INST-BAS                                    ELUKYSEL
02974          PERFORM ACCEPT-INST-BASIC-CONTRACT                       ELUKYSEL
02975      ELSE IF WS-L-O-B-PROF-BAS                                    ELUKYSEL
02976          PERFORM ACCEPT-PROF-BASIC-CONTRACT                       ELUKYSEL
02977      ELSE                                                         ELUKYSEL
02978          PERFORM SYSTEM-LOGIC-ERROR.                              ELUKYSEL
02979                                                                   ELUKYSEL
02980 ************************************************************      ELUKYSEL
02981 *                                                          *      ELUKYSEL
02982 *        ACCEPT COMP CONTRACT                              *      ELUKYSEL
02983 *                                                          *      ELUKYSEL
02984 ************************************************************      ELUKYSEL
02985  ACCEPT-COMP-CONTRACT.                                            ELUKYSEL
02986      SET KTC-INST-BAS-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
02987      SET KTC-INST-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02988      SET KTC-PROF-BAS-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
02989      SET KTC-PROF-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02990                                                                   ELUKYSEL
02991 ************************************************************      ELUKYSEL
02992 *                                                          *      ELUKYSEL
02993 *        ACCEPT SUP CONTRACT                               *      ELUKYSEL
02994 *                                                          *      ELUKYSEL
02995 ************************************************************      ELUKYSEL
02996  ACCEPT-SUP-CONTRACT.                                             ELUKYSEL
02997      SET KTC-INST-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
02998      SET KTC-INST-SUP-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
02999      SET KTC-PROF-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03000      SET KTC-PROF-SUP-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
03001                                                                   ELUKYSEL
03002 ************************************************************      ELUKYSEL
03003 *                                                          *      ELUKYSEL
03004 *        ACCEPT INST BASIC CONTRACT                        *      ELUKYSEL
03005 *                                                          *      ELUKYSEL
03006 ************************************************************      ELUKYSEL
03007  ACCEPT-INST-BASIC-CONTRACT.                                      ELUKYSEL
03008      SET KTC-INST-BAS-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
03009      SET KTC-INST-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03010      SET KTC-PROF-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03011      SET KTC-PROF-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03012                                                                   ELUKYSEL
03013 ************************************************************      ELUKYSEL
03014 *                                                          *      ELUKYSEL
03015 *        ACCEPT PROF BASIC CONTRACT                        *      ELUKYSEL
03016 *                                                          *      ELUKYSEL
03017 ************************************************************      ELUKYSEL
03018  ACCEPT-PROF-BASIC-CONTRACT.                                      ELUKYSEL
03019      SET KTC-INST-BAS-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03020      SET KTC-INST-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03021      SET KTC-PROF-BAS-SEL (KTC-IDX) TO TRUE.                      ELUKYSEL
03022      SET KTC-PROF-SUP-REJ (KTC-IDX) TO TRUE.                      ELUKYSEL
03023                                                                   ELUKYSEL
03024 ************************************************************      ELUKYSEL
03025 *                                                          *      ELUKYSEL
03026 *        INVALID COMMAREA ABEND                            *      ELUKYSEL
03027 *                                                          *      ELUKYSEL
03028 ************************************************************      ELUKYSEL
03029  INVALID-COMMAREA-ABEND.                                          ELUKYSEL
03030      SET CIA-AB-DFHCOMMAREA TO TRUE.                              ELUKYSEL
03031      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYSEL
03032                                                                   ELUKYSEL
03033 ************************************************************      ELUKYSEL
03034 *                                                          *      ELUKYSEL
03035 *        SYSTEM LOGIC ERROR                                *      ELUKYSEL
03036 *                                                          *      ELUKYSEL
03037 ************************************************************      ELUKYSEL
03038  SYSTEM-LOGIC-ERROR.                                              ELUKYSEL
03039      SET CIA-AB-UNDEF TO TRUE.                                    ELUKYSEL
03040      EXEC CICS ABEND ABCODE(CIA-ABCODE) END-EXEC.                 ELUKYSEL
03041 ************************************************************      ELUKYSEL
03042 *                                                          *      ELUKYSEL
03043 *        TEXAS REGION CHECK                                *      ELUKYSEL
03044 *                                                          *      ELUKYSEL
03045 ************************************************************      ELUKYSEL
03046  TEXAS-REGION-CHECK.                                              ELUKYSEL
03047      EXEC CICS ASSIGN APPLID (WS-APPLID) END-EXEC.                ELUKYSEL
