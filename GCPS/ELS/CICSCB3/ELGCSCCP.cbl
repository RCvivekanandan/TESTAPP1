00001 *      LAST MAINTENANCE TIME: 11.18.54  DATE: 08/22/89            09/03/03
00002 * STRUCTURE(S) MEMBER ELGCSCCPPL - LEVEL 040 AS OF 10/12/88       ELGCSCCP
00003 * FROM PANLIB R360059.STRUCTPL.PANLIB                                LV002
00004 *                                                                 ELGCSCCP
00005  IDENTIFICATION DIVISION.                                         ELGCSCCP
00006                                                                   ELGCSCCP
00007  PROGRAM-ID.         ELGCSCCP.                                    ELGCSCCP
00008                                                                   ELGCSCCP
00009  AUTHOR.             RICK BARILEAU.                               ELGCSCCP
00010                                                                   ELGCSCCP
00011  INSTALLATION.       HEALTH CARE SERVICE CORPORATION              ELGCSCCP
00012                      A MUTUAL LEGAL RESERVE COMPANY               ELGCSCCP
00013                      BLUE CROSS/BLUE SHIELD OF ILLINOIS           ELGCSCCP
00014                      233 N. MICHIGAN AVE                          ELGCSCCP
00015                      CHICAGO, ILLINOIS 60601                      ELGCSCCP
00016                                                                   ELGCSCCP
00017  DATE-WRITTEN.       18-FEB-1988.                                 ELGCSCCP
00018                                                                   ELGCSCCP
00019  DATE-COMPILED.                                                   ELGCSCCP
00020                                                                   ELGCSCCP
00021  SECURITY.           COPYRIGHT 1986,                              ELGCSCCP
00022                      HEALTH CARE SERVICE CORPORATION              ELGCSCCP
00023      SKIP3                                                        ELGCSCCP
00024  ENVIRONMENT DIVISION.                                            ELGCSCCP
00025                                                                   ELGCSCCP
00026  CONFIGURATION SECTION.                                           ELGCSCCP
00027  SOURCE-COMPUTER.    IBM-3090.                                    ELGCSCCP
00028  OBJECT-COMPUTER.    IBM-3090.                                    ELGCSCCP
00029      EJECT                                                        ELGCSCCP
00030 ******************************************************************ELGCSCCP
00031 *                                                                *ELGCSCCP
00032 *    COPYBOOK:   ELGCSCCP                                        *ELGCSCCP
00033 *    DATE:       18-FEB-1988                                     *ELGCSCCP
00034 *    AUTHOR:     RICK BARILEAU                                   *ELGCSCCP
00035 *    FUNCTION:   THIS WILL GENERATE ALL OUTPUT ASSOCIATED WITH   *ELGCSCCP
00036 *                EACH COST CONTAINMENT PROGRAM WITH GROUP SELECTEDELGCSCCP
00037 *    NOTES:      X---                                            *ELGCSCCP
00038 *                                                                *ELGCSCCP
00039 ******************************************************************ELGCSCCP
00040 *                                                                *ELGCSCCP
00041 *                      MAINTENANCE HISTORY                       *ELGCSCCP
00042 *                                                                *ELGCSCCP
00043 *  MOD     DATE     BY  DRPT                ACTION               *ELGCSCCP
00044 * ----- ----------- --- ----- ---------------------------------- *ELGCSCCP
00045 * 01.00 18-FEB-1988 REB       CREATED                            *ELGCSCCP
00046 *                                                                *ELGCSCCP
00047 * 01.01 26-FEB-1988 REB       REC'D OUTPUT REQUIREMENTS FOR REIMB*ELGCSCCP
00048 *                                                                *ELGCSCCP
00049 * 01.02 03-FEB-1988 REB       REMOVED LOGIC TO READ ACCUMS AT    *ELGCSCCP
00050 *                             GROUP SPECIFIC LEVEL BECAUSE THEY  *ELGCSCCP
00051 *                             ARE ALREADY PULLED IN BY ELTCSMRY. *ELGCSCCP
00052 *                                                                *ELGCSCCP
00053 * 01.03 11-MAR-1988 REB       AUGGIE REQUESTED ADDITIONAL        *ELGCSCCP
00054 *                             REQUIREMENTS FOR OUTPUT CONCERNING *ELGCSCCP
00055 *                             ACCUM INFORMATION.                 *ELGCSCCP
00056 *                                                                *ELGCSCCP
00057 * 01.04 24-MAR-1988 REB       PUT CONDITIONAL TO DO PROGRAM SOURCEELGCSCCP
00058 *                             IF INDICATOR HAS A VALUE IN IT.    *ELGCSCCP
00059 *                                                                *ELGCSCCP
00060 * 01.05 25-MAR-1988 REB       NINA NOW WANTS THE INTERNAL        *ELGCSCCP
00061 *                             DESCRIPTOR AND CC IND TEXT TO BE   *ELGCSCCP
00062 *                             DISPLAYED FOR #ACL AND #ADL.       *ELGCSCCP
00063 *                                                                *ELGCSCCP
00064 * 01.06 31-MAR-1988 REB       CAME ACROSS A NEGATIVE VALUE LIMIT *ELGCSCCP
00065 *                             FOR DEDUCTIBLE AT GROUP LEVEL THIS *ELGCSCCP
00066 *                             WAS NOT HANDLED IN SPECS. I WILL   *ELGCSCCP
00067 *                             TRANSLATE DED BASE AMT IN THIS CASE*ELGCSCCP
00068 * 01.07 04-APR-1988 NAC       CHANGED MASOP HEADING FROM SECOND  *ELGCSCCP
00069 *                             TO SURGICAL.                       *ELGCSCCP
00070 *                                                                *ELGCSCCP
00071 * 01.08 06-APR-1988 AKK       DISCREPANCY NO. P4727 CHANGED      *ELGCSCCP
00072 *                             PARTICIPATING TO PREFERRED.        *ELGCSCCP
00073 *                                                                *ELGCSCCP
00074 * 01.09 10-OCT-1988 EGL       1.  CHANGED TO NEW STORAGE MGMT    *ELGCSCCP
00075 *                             2.  MODIFIED FOR NEW CS TABLES     *ELGCSCCP
00076 *                                                                *ELGCSCCP
00077 * 01.10 22-AUG-1989 AKK       DESTRUCT PROGRAM.                  *ELGCSCCP
00078 *                                                                *ELGCSCCP
00079 * 01.11 12-NOV-1990 AKK       CHANGED GCG-IND VALUES TO ZERO     *ELGCSCCP
00080 *                             FROM '0' DUE TO EXPANSION OF       *ELGCSCCP
00081 *                             GROUP SPECIFIC RECORD.             *ELGCSCCP
00082 *                             ALSO CHANGED 8 TO '08' FOR THE SAME*ELGCSCCP
00083 *                             REASON.                            *ELGCSCCP
00084 * 02.00 24-AUG-1995 AKK       COMMENTED OUT THE R/S CODE AS      *ELGCSCCP
00085 *                             IT WAS NOT CHANGED FOR ARMS        *ELGCSCCP
00086 *                             CONVERSIONS.                       *ELGCSCCP
00087 *                                                                *ELGCSCCP
00088 * 02.01 05-SEP-1995 AKK       ADDED SENTENCE TO REFER USERS TO   *ELGCSCCP
00089 *                             MAIN CCP TOPIC FOR R/S INFO.       *ELGCSCCP
00090 *                                                                *ELGCSCCP
00091 *       21-AUG-2003 AKK       GEN TO TEST COMPILE ORDER          *ELGCSCCP
00092 *                                                                *ELGCSCCP
00093 ******************************************************************ELGCSCCP
00094                                                                   ELGCSCCP
00095  DATA DIVISION.                                                   ELGCSCCP
00096  WORKING-STORAGE SECTION.                                         ELGCSCCP
00097  01  WS-MISC.                                                     ELGCSCCP
00098      05  FILLER              PIC X(30) VALUE                      ELGCSCCP
00099      '*** ELGCSCCP BEGINS HERE ***'.                              ELGCSCCP
00100      05  WS-ATBL-SUB         PIC S9(4) COMP SYNC.                 ELGCSCCP
00101                                                                   ELGCSCCP
00102  01  SWITCHES.                                                    ELGCSCCP
00103      05  WS-ACCUM-SW         PIC X(01) VALUE SPACE.               ELGCSCCP
00104          88  ACCUM-FOUND               VALUE 'A'.                 ELGCSCCP
00105      05  WS-PROCESSING-SW    PIC X     VALUE SPACE.               ELGCSCCP
00106          88  WS-PROCESSING-ABM         VALUE 'B'.                 ELGCSCCP
00107          88  WS-PROCESSING-ACL         VALUE 'C'.                 ELGCSCCP
00108          88  WS-PROCESSING-ADL         VALUE 'D'.                 ELGCSCCP
00109                                                                   ELGCSCCP
00110  01  PROGRAM-CONSTANTS.                                           ELGCSCCP
00111      05  PC-ABM              PIC X(06) VALUE '#ABM  '.            ELGCSCCP
00112      05  PC-ACL              PIC X(06) VALUE '#ACL  '.            ELGCSCCP
00113      05  PC-ADL              PIC X(06) VALUE '#ADL  '.            ELGCSCCP
00114      05  PC-APPLICABLE       PIC X(14) VALUE 'APPLICABLE TO '.    ELGCSCCP
00115      05  PC-APPROVAL         PIC X(09) VALUE 'APPROVAL;'.         ELGCSCCP
00116      05  PC-BAR              PIC X(01) VALUE '|'.                 ELGCSCCP
00117      05  PC-BLUE-CROSS       PIC X(12) VALUE ' BLUE CROSS '.      ELGCSCCP
00118      05  PC-BLUE-SHIELD      PIC X(13) VALUE ' BLUE SHIELD '.     ELGCSCCP
00119      05  PC-CODE-FOUR-TEXT   PIC X(39) VALUE ' EITHER NON COMPLIANELGCSCCP
00120 -        'CE OR NON APPROVED '.                                   ELGCSCCP
00121      05  PC-COMMA            PIC X(01) VALUE ','.                 ELGCSCCP
00122      05  PC-DETERMINED-BY    PIC X(33) VALUE 'THE DEDUCTIBLE IS DEELGCSCCP
00123 -        'TERMINED BY '.                                          ELGCSCCP
00124      05  PC-GCCP             PIC X(06) VALUE '#GCCP '.            ELGCSCCP
00125      05  PC-MAJ-MED          PIC X(15) VALUE ' MAJOR MEDICAL '.   ELGCSCCP
00126      05  PC-REQUIRES         PIC X(09) VALUE 'REQUIRES '.         ELGCSCCP
00127      05  PC-SEMI-COLON       PIC X(01) VALUE ';'.                 ELGCSCCP
00128      05  PC-UNLIMITED        PIC X(09) VALUE 'UNLIMITED'.         ELGCSCCP
00129                                                                   ELGCSCCP
00130  01  WS-CC-INDICATOR.                                             ELGCSCCP
00131      05  WS-CC-QUALIFIER     PIC X(01).                           ELGCSCCP
00132          88  ATCP-ID                   VALUE 'A'.                 ELGCSCCP
00133          88  HOSP-ID                   VALUE 'B'.                 ELGCSCCP
00134          88  PAT-ID                    VALUE 'D'.                 ELGCSCCP
00135          88  MOPS-ID                   VALUE '1'.                 ELGCSCCP
00136          88  MASOP-ID                  VALUE '2'.                 ELGCSCCP
00137          88  WEEKN-ID                  VALUE '3'.                 ELGCSCCP
00138          88  MONDC-ID                  VALUE '4'.                 ELGCSCCP
00139          88  IOB-ID                    VALUE '5'.                 ELGCSCCP
00140          88  PAR-ID                    VALUE '6'.                 ELGCSCCP
00141          88  MEDNC-ID                  VALUE '7'.                 ELGCSCCP
00142          88  MSA-ID                    VALUE '8'.                 ELGCSCCP
00143          88  PPO-ID                    VALUE '9'.                 ELGCSCCP
00144      05  WS-CC-CODE-IND      PIC X(01).                           ELGCSCCP
00145          88  CC-CODE-1                 VALUE '1'.                 ELGCSCCP
00146          88  CC-CODE-2                 VALUE '2'.                 ELGCSCCP
00147          88  CC-CODE-3                 VALUE '3'.                 ELGCSCCP
00148          88  CC-CODE-4                 VALUE '4'.                 ELGCSCCP
00149                                                                   ELGCSCCP
00150  01  WS-PROGRAM-INDICATOR.                                        ELGCSCCP
00151      05  WS-PROGRAM-TYPE     PIC X(01).                           ELGCSCCP
00152          88  ATCP-PGM                  VALUE 'A'.                 ELGCSCCP
00153          88  HOSP-PGM                  VALUE 'B'.                 ELGCSCCP
00154          88  PAT-PGM                   VALUE 'D'.                 ELGCSCCP
00155          88  MOPS-PGM                  VALUE '1'.                 ELGCSCCP
00156          88  MASOP-PGM                 VALUE '2'.                 ELGCSCCP
00157          88  WEEKN-PGM                 VALUE '3'.                 ELGCSCCP
00158          88  MONDC-PGM                 VALUE '4'.                 ELGCSCCP
00159          88  IOB-PGM                   VALUE '5'.                 ELGCSCCP
00160          88  PAR-PGM                   VALUE '6'.                 ELGCSCCP
00161          88  MEDNC-PGM                 VALUE '7'.                 ELGCSCCP
00162          88  MSA-PGM                   VALUE '8'.                 ELGCSCCP
00163          88  PPO-PGM                   VALUE '9'.                 ELGCSCCP
00164          88  REIMB-PGM                 VALUE 'R'.                 ELGCSCCP
00165                                                                   ELGCSCCP
00166  01  WS-LOB-INDICATORS.                                           ELGCSCCP
00167      05  WS-BC-IND           PIC X(02).                           ELGCSCCP
00168      05  WS-BS-IND           PIC X(02).                           ELGCSCCP
00169      05  WS-MM-IND           PIC X(02).                           ELGCSCCP
00170                                                                   ELGCSCCP
00171  01  WS-HOLD-INDICATORS.                                          ELGCSCCP
00172      05  WS-APPRVL-SRCE-IND  PIC X(02).                           ELGCSCCP
00173      05  WS-PROG-SRCE-IND    PIC X(02).                           ELGCSCCP
00174                                                                   ELGCSCCP
00175  01  WS-FIXED-TEXT-AREA.                                          ELGCSCCP
00176      05  WS-HEADER-LINE.                                          ELGCSCCP
00177          10  FILLER          PIC X(79) VALUE 'THE FOLLOWING DISPLAELGCSCCP
00178 -        'YED BENEFITS ARE FOR: COST CONTAINMENT PROGRAMS'.       ELGCSCCP
00179      05  WS-DASH-LINE.                                            ELGCSCCP
00180          10  FILLER          PIC X(70) VALUE '--------------------ELGCSCCP
00181 -        '--------------------------------------------------'.    ELGCSCCP
00182          10  FILLER          PIC X(09) VALUE '---------'.         ELGCSCCP
00183      05  WS-NOT-APPLICABLE   PIC X(79) VALUE 'PROGRAMS ARE NOT APPELGCSCCP
00184 -        'LICABLE.'.                                              ELGCSCCP
00185      05  WS-REIMB-SUBROG-A   PIC X(35) VALUE                      ELGCSCCP
00186          'SEE THE COST CONTAINMENT TOPIC FOR'.                    ELGCSCCP
00187      05  WS-REIMB-SUBROG-B   PIC X(38) VALUE                      ELGCSCCP
00188          'REIMBURSEMENT/SUBROGATION INFORMATION.'.                ELGCSCCP
00189                                                                   ELGCSCCP
00190  01  WS-SCREEN-LINE-AREA.                                         ELGCSCCP
00191      05  WS-COINSURANCE-PHRASE.                                   ELGCSCCP
00192          10  WS-PCT-LVL      PIC ZZ9.                             ELGCSCCP
00193          10  FILLER          PIC X(01) VALUE '%'.                 ELGCSCCP
00194          10  FILLER          PIC X(13) VALUE ' COINSURANCE '.     ELGCSCCP
00195                                                                   ELGCSCCP
00196      05  WS-DEDUCTIBLE-PHRASE.                                    ELGCSCCP
00197          10  WS-DEDUCT-VALUE PIC $$,$$$,$$9.99.                   ELGCSCCP
00198          10  WS-DED-LMT-Y    REDEFINES WS-DEDUCT-VALUE            ELGCSCCP
00199                              PIC Z(11)V99.                        ELGCSCCP
00200          10  WS-DED-LMT-Z    REDEFINES WS-DEDUCT-VALUE            ELGCSCCP
00201                              PIC X(13).                           ELGCSCCP
00202              88  WS-DED-UNLIMITED      VALUE                      ELGCSCCP
00203              '$9,999,999.99' '$9,999,999.00'.                     ELGCSCCP
00204          10  FILLER          PIC X(01) VALUE SPACE.               ELGCSCCP
00205          10  WS-DEDUCT-QUAL  PIC X(45).                           ELGCSCCP
00206          10  FILLER          PIC X(12) VALUE ' DEDUCTIBLE '.      ELGCSCCP
00207                                                                   ELGCSCCP
00208      05  WS-MAXIMUM-PHRASE.                                       ELGCSCCP
00209          10  FILLER          PIC X(06) VALUE 'UP TO '.            ELGCSCCP
00210          10  WS-MAX-VALUE    PIC $$,$$$,$$9.99.                   ELGCSCCP
00211          10  WS-VAL-LMT-A    REDEFINES WS-MAX-VALUE               ELGCSCCP
00212                              PIC Z(11)V99.                        ELGCSCCP
00213          10  WS-VAL-LMT-B    REDEFINES WS-MAX-VALUE               ELGCSCCP
00214                              PIC X(13).                           ELGCSCCP
00215              88  WS-MAX-LMT            VALUE                      ELGCSCCP
00216              '$9,999,999.99' '$9,999,999.00'.                     ELGCSCCP
00217          10  FILLER          PIC X(01) VALUE SPACE.               ELGCSCCP
00218          10  WS-MAX-QUAL     PIC X(45).                           ELGCSCCP
00219          10  FILLER          PIC X(09) VALUE ' MAXIMUM '.         ELGCSCCP
00220                                                                   ELGCSCCP
00221      05  WS-REIMB-INVEST-PHRASE.                                  ELGCSCCP
00222          10  FILLER              PIC X(36)  VALUE 'THE INVESTIGATIELGCSCCP
00223 -        'ON DOLLAR MINIMUM IS '.                                 ELGCSCCP
00224          10  WS-RS-INVEST-DOLLAR PIC $$$,$$9.                     ELGCSCCP
00225          10  FILLER              PIC X(01)  VALUE '.'.            ELGCSCCP
00226                                                                   ELGCSCCP
00227  01  WS-OUTPUT-AREA.                                              ELGCSCCP
00228      05  WS-HEADINGS-CNT         PIC S9(04) VALUE +0  COMP-3.     ELGCSCCP
00229      05  WS-LINE-CNT             PIC S9(04) VALUE +0  COMP-3.     ELGCSCCP
00230      05  WS-OUTPUT               PIC X(1580).                     ELGCSCCP
00231      05  WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT                      ELGCSCCP
00232                                  OCCURS 20 TIMES                  ELGCSCCP
00233                                  INDEXED BY WS-OUTPUT-IDX.        ELGCSCCP
00234          10  WS-OUTPUT-LINE.                                      ELGCSCCP
00235              15  WS-LEFT-SIDE    PIC X(30).                       ELGCSCCP
00236              15  WS-DIVIDER      PIC X(01).                       ELGCSCCP
00237              15  FILLER          PIC X(02).                       ELGCSCCP
00238              15  WS-RIGHT-SIDE   PIC X(46).                       ELGCSCCP
00239                                                                   ELGCSCCP
00240  01  WS-LOB-TABLE-AREA.                                           ELGCSCCP
00241      05  WS-LOB-SUB              PIC S9(04) VALUE +0  COMP-3.     ELGCSCCP
00242          88  LOB-1                          VALUE +0001.          ELGCSCCP
00243          88  LOB-2                          VALUE +0002.          ELGCSCCP
00244          88  LOB-3                          VALUE +0003.          ELGCSCCP
00245      05  WS-LOB-TABLE            PIC X(48).                       ELGCSCCP
00246      05  WS-LOB-RED REDEFINES WS-LOB-TABLE                        ELGCSCCP
00247                                  OCCURS 3 TIMES.                  ELGCSCCP
00248          10  WS-LOB-NAME         PIC X(15).                       ELGCSCCP
00249          10  WS-PUNCTUATION      PIC X(01).                       ELGCSCCP
00250 /                                                                 ELGCSCCP
00251  LINKAGE SECTION.                                                 ELGCSCCP
00252  01  DFHCOMMAREA.                                                 ELGCSCCP
00253      COPY ELSCOMMC.                                               ELGCSCCP
00254 /                                                                 ELGCSCCP
00255      COPY ELSCIA2C.                                               ELGCSCCP
00256 /                                                                 ELGCSCCP
00257      COPY ELSSSCBC.                                               ELGCSCCP
00258 /                                                                 ELGCSCCP
00259      COPY ELSIOPMC.                                               ELGCSCCP
00260 /                                                                 ELGCSCCP
00261      COPY ELSKEYSC.                                               ELGCSCCP
00262 /                                                                 ELGCSCCP
00263      COPY ELSOUTPC.                                               ELGCSCCP
00264 /                                                                 ELGCSCCP
00265      COPY ELSCMDSC.                                               ELGCSCCP
00266 /                                                                 ELGCSCCP
00267      COPY ELSCMIFC.                                               ELGCSCCP
00268 /                                                                 ELGCSCCP
00269      COPY ELSTCWAC.                                               ELGCSCCP
00270 /    CONTRACT SUMMARY ACCUMULATOR POINTER TABLE                   ELGCSCCP
00271      COPY ELSCSACC.                                               ELGCSCCP
00272 /    CONTRACT SUMMARY ACCUMULATOR TABLE                           ELGCSCCP
00273      COPY ELSATBLC.                                               ELGCSCCP
00274 /                                                                 ELGCSCCP
00275  01  GROUP-SPECIFIC-RECORD.                                       ELGCSCCP
00276      COPY GCGROUPC.                                               ELGCSCCP
00277 /                                                                 ELGCSCCP
00278  01  GCCP-TABULAR-REC-AREA.                                       ELGCSCCP
00279      COPY GCTGCCPC.                                               ELGCSCCP
00280      EJECT                                                        ELGCSCCP
00281  PROCEDURE DIVISION.                                              ELGCSCCP
00282 ************************************************************      ELGCSCCP
00283 *                                                          *      ELGCSCCP
00284 *                    PROCEDURE DIVISION                    *      ELGCSCCP
00285 *                                                          *      ELGCSCCP
00286 ************************************************************      ELGCSCCP
00287                                                                   ELGCSCCP
00288                                                                   ELGCSCCP
00289 ************************************************************      ELGCSCCP
00290 *                                                          *      ELGCSCCP
00291 *        COST CONTAINMENT FOR CS                           *      ELGCSCCP
00292 *                                                          *      ELGCSCCP
00293 ************************************************************      ELGCSCCP
00294  COST-CONTAINMENT-FOR-CS.                                         ELGCSCCP
00295      PERFORM INITIALIZATION.                                      ELGCSCCP
00296      PERFORM PROCESS.                                             ELGCSCCP
00297      GOBACK.                                                      ELGCSCCP
00298                                                                   ELGCSCCP
00299 ************************************************************      ELGCSCCP
00300 *                                                          *      ELGCSCCP
00301 *        INITIALIZATION                                    *      ELGCSCCP
00302 *                                                          *      ELGCSCCP
00303 ************************************************************      ELGCSCCP
00304  INITIALIZATION.                                                  ELGCSCCP
00305      PERFORM ESTABLISH-ADDRESS-OF-CO.                             ELGCSCCP
00306      PERFORM ESTABLISH-ADDRESSABILITY-OF-WO.                      ELGCSCCP
00307                                                                   ELGCSCCP
00308                                                                   ELGCSCCP
00309 ************************************************************      ELGCSCCP
00310 *                                                          *      ELGCSCCP
00311 *        ESTABLISH ADDRESSABILITY OF CONTROL BLOCKS        *      ELGCSCCP
00312 *                                                          *      ELGCSCCP
00313 ************************************************************      ELGCSCCP
00314  ESTABLISH-ADDRESS-OF-CO.                                         ELGCSCCP
00315      PERFORM CHECK-FOR-VALID-COMMAREA.                            ELGCSCCP
00316      PERFORM ESTABLISH-ADDRESSABILITY-OF-CO.                      ELGCSCCP
00317      PERFORM ESTABLISH-ADDRESSABILITY-OF-SE.                      ELGCSCCP
00318                                                                   ELGCSCCP
00319 ************************************************************      ELGCSCCP
00320 *                                                          *      ELGCSCCP
00321 *        CHECK FOR VALID COMMAREA                          *      ELGCSCCP
00322 *                                                          *      ELGCSCCP
00323 ************************************************************      ELGCSCCP
00324  CHECK-FOR-VALID-COMMAREA.                                        ELGCSCCP
00325      IF EIBCALEN < LENGTH OF DFHCOMMAREA                          ELGCSCCP
00326          EXEC CICS ABEND                                          ELGCSCCP
00327                   ABCODE('EL01')                                  ELGCSCCP
00328            END-EXEC.                                              ELGCSCCP
00329                                                                   ELGCSCCP
00330 ************************************************************      ELGCSCCP
00331 *                                                          *      ELGCSCCP
00332 *        ESTABLISH ADDRESSABILITY OF COMMON INTERFACE AREA *      ELGCSCCP
00333 *                                                          *      ELGCSCCP
00334 ************************************************************      ELGCSCCP
00335  ESTABLISH-ADDRESSABILITY-OF-CO.                                  ELGCSCCP
00336      CALL 'ELUINISM' USING DFHCOMMAREA                            ELGCSCCP
00337                 ADDRESS OF                                        ELGCSCCP
00338          CIA-ELS-COMMON-INTERFACE-AREA.                           ELGCSCCP
00339      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00340         EXEC CICS ABEND                                           ELGCSCCP
00341                   ABCODE('EL02')                                  ELGCSCCP
00342           END-EXEC.                                               ELGCSCCP
00343                                                                   ELGCSCCP
00344 ************************************************************      ELGCSCCP
00345 *                                                          *      ELGCSCCP
00346 *        ESTABLISH ADDRESSABILITY OF SELECTOR STATUS CONTRO*      ELGCSCCP
00347 *                                                          *      ELGCSCCP
00348 ************************************************************      ELGCSCCP
00349  ESTABLISH-ADDRESSABILITY-OF-SE.                                  ELGCSCCP
00350      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELGCSCCP
00351      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00352                 ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.           ELGCSCCP
00353      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00354          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00355                                                                   ELGCSCCP
00356 ************************************************************      ELGCSCCP
00357 *                                                          *      ELGCSCCP
00358 *        SIGNAL UNALLOC AREA ERROR                         *      ELGCSCCP
00359 *                                                          *      ELGCSCCP
00360 ************************************************************      ELGCSCCP
00361  SIGNAL-UNALLOC-AREA-ERROR.                                       ELGCSCCP
00362      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELGCSCCP
00363      PERFORM SIGNAL-ABEND.                                        ELGCSCCP
00364                                                                   ELGCSCCP
00365 ************************************************************      ELGCSCCP
00366 *                                                          *      ELGCSCCP
00367 *        SIGNAL ABEND                                      *      ELGCSCCP
00368 *                                                          *      ELGCSCCP
00369 ************************************************************      ELGCSCCP
00370  SIGNAL-ABEND.                                                    ELGCSCCP
00371      EXEC CICS ABEND                                              ELGCSCCP
00372                ABCODE(CIA-ABCODE)                                 ELGCSCCP
00373         END-EXEC.                                                 ELGCSCCP
00374                                                                   ELGCSCCP
00375 ************************************************************      ELGCSCCP
00376 *                                                          *      ELGCSCCP
00377 *        ESTABLISH ADDRESSABILITY OF WORK AREAS            *      ELGCSCCP
00378 *                                                          *      ELGCSCCP
00379 ************************************************************      ELGCSCCP
00380  ESTABLISH-ADDRESSABILITY-OF-WO.                                  ELGCSCCP
00381      PERFORM ESTABLISH-ADDRESSABILITY-OF-OU.                      ELGCSCCP
00382      PERFORM ESTABLISH-ADDRESSABILITY-OF-KE.                      ELGCSCCP
00383      PERFORM ESTABLISH-ADDRESS-CODES-MANUAL.                      ELGCSCCP
00384      PERFORM ESTABLISH-ADDRESSABILITY-OF-TE.                      ELGCSCCP
00385      PERFORM ESTABLISH-ADDRESSABILITY-OF-CS.                      ELGCSCCP
00386      PERFORM ESTABLISH-ADDRESSABILITY-OF-GR.                      ELGCSCCP
00387      PERFORM ESTABLISH-ADDRESSABILITY-CCP.                        ELGCSCCP
00388                                                                   ELGCSCCP
00389 ************************************************************      ELGCSCCP
00390 *                                                          *      ELGCSCCP
00391 *        ESTABLISH ADDRESSABILITY OF OUTPUT INTERFACE      *      ELGCSCCP
00392 *                                                          *      ELGCSCCP
00393 ************************************************************      ELGCSCCP
00394  ESTABLISH-ADDRESSABILITY-OF-OU.                                  ELGCSCCP
00395      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELGCSCCP
00396      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00397                 ADDRESS OF COF-OUTPUT-INTERFACE.                  ELGCSCCP
00398      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00399          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00400                                                                   ELGCSCCP
00401 ************************************************************      ELGCSCCP
00402 *                                                          *      ELGCSCCP
00403 *        ESTABLISH ADDRESSABILITY OF KEY WORK AREA         *      ELGCSCCP
00404 *                                                          *      ELGCSCCP
00405 ************************************************************      ELGCSCCP
00406  ESTABLISH-ADDRESSABILITY-OF-KE.                                  ELGCSCCP
00407      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELGCSCCP
00408      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00409                 ADDRESS OF KWA-FILE-KEY-WORK-AREA.                ELGCSCCP
00410      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00411          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00412                                                                   ELGCSCCP
00413 ************************************************************      ELGCSCCP
00414 *                                                          *      ELGCSCCP
00415 *        ESTABLISH ADDRESSABILITY OF CODES MANUAL INTERFACE*      ELGCSCCP
00416 *                                                          *      ELGCSCCP
00417 ************************************************************      ELGCSCCP
00418  ESTABLISH-ADDRESS-CODES-MANUAL.                                  ELGCSCCP
00419      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELGCSCCP
00420      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00421                 ADDRESS OF CMF-CODES-MANUAL-INTERFACE.            ELGCSCCP
00422      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00423          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00424                                                                   ELGCSCCP
00425 ************************************************************      ELGCSCCP
00426 *                                                          *      ELGCSCCP
00427 *        ESTABLISH ADDRESSABILITY OF TEXT COMPRESSION WORK *      ELGCSCCP
00428 *                                                          *      ELGCSCCP
00429 ************************************************************      ELGCSCCP
00430  ESTABLISH-ADDRESSABILITY-OF-TE.                                  ELGCSCCP
00431      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELGCSCCP
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00433                 ADDRESS OF TCAR-COMPRESSION-WORK-AREA.            ELGCSCCP
00434      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00435          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00436                                                                   ELGCSCCP
00437 ************************************************************      ELGCSCCP
00438 *                                                          *      ELGCSCCP
00439 *        ESTABLISH ADDRESSABILITY OF CS ACCUMULATOR TABLE A*      ELGCSCCP
00440 *                                                          *      ELGCSCCP
00441 ************************************************************      ELGCSCCP
00442  ESTABLISH-ADDRESSABILITY-OF-CS.                                  ELGCSCCP
00443      SET CIA-ELSCSAC-DDN TO TRUE.                                 ELGCSCCP
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00445                 ADDRESS OF CSAC-ACCUMULATOR-TABLE.                ELGCSCCP
00446      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00447          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00448                                                                   ELGCSCCP
00449 ************************************************************      ELGCSCCP
00450 *                                                          *      ELGCSCCP
00451 *        ESTABLISH ADDRESSABILITY OF GROUP SPECIFIC        *      ELGCSCCP
00452 *                                                          *      ELGCSCCP
00453 ************************************************************      ELGCSCCP
00454  ESTABLISH-ADDRESSABILITY-OF-GR.                                  ELGCSCCP
00455      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELGCSCCP
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00457                 ADDRESS OF GROUP-SPECIFIC-RECORD.                 ELGCSCCP
00458      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00459          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00460                                                                   ELGCSCCP
00461 ************************************************************      ELGCSCCP
00462 *                                                          *      ELGCSCCP
00463 *        ESTABLISH ADDRESSABILITY OF COST CONTAINMENT IOP  *      ELGCSCCP
00464 *                                                          *      ELGCSCCP
00465 ************************************************************      ELGCSCCP
00466  ESTABLISH-ADDRESSABILITY-CCP.                                    ELGCSCCP
00467      SET CIA-GCTABULR-DDN TO TRUE.                                ELGCSCCP
00468      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00469                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELGCSCCP
00470      IF CIA-RC-PTR-NULL                                           ELGCSCCP
00471          PERFORM SIGNAL-UNALLOC-AREA-ERROR.                       ELGCSCCP
00472                                                                   ELGCSCCP
00473 ************************************************************      ELGCSCCP
00474 *                                                          *      ELGCSCCP
00475 *        PROCESS                                           *      ELGCSCCP
00476 *                                                          *      ELGCSCCP
00477 ************************************************************      ELGCSCCP
00478  PROCESS.                                                         ELGCSCCP
00479      PERFORM DISPLAY-CCP-SUBTOPIC-HEADER-LI.                      ELGCSCCP
00480      PERFORM SEARCH-FOR-GCCP-TABULAR.                             ELGCSCCP
00481                                                                   ELGCSCCP
00482 ************************************************************      ELGCSCCP
00483 *                                                          *      ELGCSCCP
00484 *        DISPLAY CCP SUBTOPIC HEADER LINE                  *      ELGCSCCP
00485 *                                                          *      ELGCSCCP
00486 ************************************************************      ELGCSCCP
00487  DISPLAY-CCP-SUBTOPIC-HEADER-LI.                                  ELGCSCCP
00488      SET COF-NEW-PAGE    TO TRUE.                                 ELGCSCCP
00489      MOVE +0             TO COF-NBR-DTL-LINES.                    ELGCSCCP
00490      MOVE +3             TO COF-NBR-HDR-LINES.                    ELGCSCCP
00491      MOVE WS-HEADER-LINE TO COF-HDR-LINE (2).                     ELGCSCCP
00492      MOVE WS-DASH-LINE   TO COF-HDR-LINE (3).                     ELGCSCCP
00493      PERFORM LINK-TO-OUTPUT.                                      ELGCSCCP
00494                                                                   ELGCSCCP
00495 ************************************************************      ELGCSCCP
00496 *                                                          *      ELGCSCCP
00497 *        LINK TO OUTPUT                                    *      ELGCSCCP
00498 *                                                          *      ELGCSCCP
00499 ************************************************************      ELGCSCCP
00500  LINK-TO-OUTPUT.                                                  ELGCSCCP
00501      EXEC CICS LINK                                               ELGCSCCP
00502                PROGRAM ('ELUOUTPT')                               ELGCSCCP
00503                COMMAREA (DFHCOMMAREA)                             ELGCSCCP
00504         END-EXEC.                                                 ELGCSCCP
00505                                                                   ELGCSCCP
00506 ************************************************************      ELGCSCCP
00507 *                                                          *      ELGCSCCP
00508 *        SEARCH FOR GCCP TABULAR                           *      ELGCSCCP
00509 *                                                          *      ELGCSCCP
00510 ************************************************************      ELGCSCCP
00511  SEARCH-FOR-GCCP-TABULAR.                                         ELGCSCCP
00512      SET GCG-INDEX TO +1.                                         ELGCSCCP
00513      SEARCH GCG-GRP-SPEC-TAB-ID                                   ELGCSCCP
00514         AT END                                                    ELGCSCCP
00515              MOVE ZEROES TO KWA-PROVISION-SLOT-NO                 ELGCSCCP
00516         WHEN GCG-TAB-ID (GCG-INDEX) = PC-GCCP                     ELGCSCCP
00517              MOVE GCG-TAB-ID (GCG-INDEX)                          ELGCSCCP
00518                                TO KWA-PROVISION-ID                ELGCSCCP
00519              MOVE GCG-TAB-SLOT-NO (GCG-INDEX)                     ELGCSCCP
00520                                TO KWA-PROVISION-SLOT-NO           ELGCSCCP
00521         END-SEARCH.                                               ELGCSCCP
00522      IF KWA-PROVISION-ID EQUAL PC-GCCP AND                        ELGCSCCP
00523                 KWA-PROVISION-SLOT-NO > ZEROES                    ELGCSCCP
00524          PERFORM READ-GCCP-TABULAR-AND-DETERMIN                   ELGCSCCP
00525      ELSE                                                         ELGCSCCP
00526          PERFORM DISPLAY-COST-CONTAINMENT-PROGR.                  ELGCSCCP
00527                                                                   ELGCSCCP
00528 ************************************************************      ELGCSCCP
00529 *                                                          *      ELGCSCCP
00530 *        READ GCCP TABULAR AND DETERMINE ELIGIBLE PROGRAMS *      ELGCSCCP
00531 *                                                          *      ELGCSCCP
00532 ************************************************************      ELGCSCCP
00533  READ-GCCP-TABULAR-AND-DETERMIN.                                  ELGCSCCP
00534      PERFORM GET-GCCP-TABULAR.                                    ELGCSCCP
00535      PERFORM CHECK-FOR-ELIGIBLE-PROGRAMS                          ELGCSCCP
00536          VARYING GSS-INDEX FROM +1 BY +1                          ELGCSCCP
00537                  UNTIL   GSS-INDEX = GSS-ENTRY-COUNT.             ELGCSCCP
00538                                                                   ELGCSCCP
00539 ************************************************************      ELGCSCCP
00540 *                                                          *      ELGCSCCP
00541 *        DISPLAY COST CONTAINMENT PROGRAMS NOT APPLICABLE M*      ELGCSCCP
00542 *                                                          *      ELGCSCCP
00543 ************************************************************      ELGCSCCP
00544  DISPLAY-COST-CONTAINMENT-PROGR.                                  ELGCSCCP
00545      ADD +1                   TO COF-NBR-DTL-LINES.               ELGCSCCP
00546      MOVE WS-NOT-APPLICABLE   TO COF-DTL-LINE                     ELGCSCCP
00547          (COF-NBR-DTL-LINES).                                     ELGCSCCP
00548      PERFORM LINK-TO-OUTPUT.                                      ELGCSCCP
00549                                                                   ELGCSCCP
00550 ************************************************************      ELGCSCCP
00551 *                                                          *      ELGCSCCP
00552 *        GET GCCP TABULAR                                  *      ELGCSCCP
00553 *                                                          *      ELGCSCCP
00554 ************************************************************      ELGCSCCP
00555  GET-GCCP-TABULAR.                                                ELGCSCCP
00556      PERFORM SETUP-AND-LINK-TO-IO-MODULE.                         ELGCSCCP
00557      IF IOP-RC-OK                                                 ELGCSCCP
00558          PERFORM ESTABLISH-ADDRESSABILITY-OF-GC                   ELGCSCCP
00559      ELSE                                                         ELGCSCCP
00560          PERFORM SIGNAL-NOT-FOUND-GCTAB-ERROR.                    ELGCSCCP
00561                                                                   ELGCSCCP
00562 ************************************************************      ELGCSCCP
00563 *                                                          *      ELGCSCCP
00564 *        SETUP AND LINK TO IO MODULE                       *      ELGCSCCP
00565 *                                                          *      ELGCSCCP
00566 ************************************************************      ELGCSCCP
00567  SETUP-AND-LINK-TO-IO-MODULE.                                     ELGCSCCP
00568      SET CIA-GCTABULR-DDN TO TRUE.                                ELGCSCCP
00569      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
00570                 ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.           ELGCSCCP
00571      SET IOP-RD              TO TRUE.                             ELGCSCCP
00572      SET IOP-STG-MODE-MOVE   TO TRUE.                             ELGCSCCP
00573      SET IOP-FCQ-NONE        TO TRUE.                             ELGCSCCP
00574      SET IOP-KVQ-EQ          TO TRUE.                             ELGCSCCP
00575      MOVE KWA-GCTABULR-KEY   TO IOP-FILE-KEY.                     ELGCSCCP
00576      PERFORM CALL-INPUT-OUTPUT-MODULE.                            ELGCSCCP
00577                                                                   ELGCSCCP
00578 ************************************************************      ELGCSCCP
00579 *                                                          *      ELGCSCCP
00580 *        CALL INPUT OUTPUT MODULE                          *      ELGCSCCP
00581 *                                                          *      ELGCSCCP
00582 ************************************************************      ELGCSCCP
00583  CALL-INPUT-OUTPUT-MODULE.                                        ELGCSCCP
00584      EXEC CICS LINK                                               ELGCSCCP
00585                PROGRAM ('ELUIOPGM')                               ELGCSCCP
00586                COMMAREA (DFHCOMMAREA)                             ELGCSCCP
00587        END-EXEC.                                                  ELGCSCCP
00588                                                                   ELGCSCCP
00589 ************************************************************      ELGCSCCP
00590 *                                                          *      ELGCSCCP
00591 *        ESTABLISH ADDRESSABILITY OF GCCP TABULAR          *      ELGCSCCP
00592 *                                                          *      ELGCSCCP
00593 ************************************************************      ELGCSCCP
00594  ESTABLISH-ADDRESSABILITY-OF-GC.                                  ELGCSCCP
00595      SET ADDRESS OF GCCP-TABULAR-REC-AREA TO                      ELGCSCCP
00596          IOP-REC-PTR.                                             ELGCSCCP
00597      SET IOP-REC-PTR                      TO NULLS.               ELGCSCCP
00598                                                                   ELGCSCCP
00599 ************************************************************      ELGCSCCP
00600 *                                                          *      ELGCSCCP
00601 *        SIGNAL NOT FOUND GCTAB ERROR                      *      ELGCSCCP
00602 *                                                          *      ELGCSCCP
00603 ************************************************************      ELGCSCCP
00604  SIGNAL-NOT-FOUND-GCTAB-ERROR.                                    ELGCSCCP
00605      SET CIA-AB-NOTFND-GCTABULR TO TRUE.                          ELGCSCCP
00606      PERFORM SIGNAL-ABEND.                                        ELGCSCCP
00607                                                                   ELGCSCCP
00608 ************************************************************      ELGCSCCP
00609 *                                                          *      ELGCSCCP
00610 *        CHECK FOR ELIGIBLE PROGRAMS                       *      ELGCSCCP
00611 *                                                          *      ELGCSCCP
00612 ************************************************************      ELGCSCCP
00613  CHECK-FOR-ELIGIBLE-PROGRAMS.                                     ELGCSCCP
00614      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELGCSCCP
00615      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSCCP
00616      IF GCG-ADDL-TRNSPLNT-COVRG-IND NOT = ZERO AND NOT = '08'     ELGCSCCP
00617          AND                                                      ELGCSCCP
00618                 GSS-AT-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00619          PERFORM PROCESS-ADDITIONAL-TRANSPLANTX.                  ELGCSCCP
00620      IF GCG-FRI-SAT-ADM-IND NOT = ZERO AND NOT = '08'             ELGCSCCP
00621          AND                                                      ELGCSCCP
00622                 GSS-FS-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00623          PERFORM PROCESS-WEEKEND-ADMISSION-PROG.                  ELGCSCCP
00624      IF GCG-HOSPICE-IND NOT = ZERO AND NOT = '08'                 ELGCSCCP
00625          AND                                                      ELGCSCCP
00626                 GSS-HO-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00627          PERFORM PROCESS-HOSPICE-PROGRAM.                         ELGCSCCP
00628      IF GCG-INCENTIVE-OB-IND NOT = ZERO AND NOT = '08'            ELGCSCCP
00629          AND                                                      ELGCSCCP
00630                 GSS-IO-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00631          PERFORM PROCESS-INCENTIVE-OB-PROGRAM.                    ELGCSCCP
00632      IF GCG-MAND-ADDL-SURG-OPN-IND NOT = ZERO AND NOT = '08'      ELGCSCCP
00633          AND                                                      ELGCSCCP
00634                 GSS-MA-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00635          PERFORM PROCESS-MASOP-PROGRAM.                           ELGCSCCP
00636      IF GCG-MONDAY-DISCHARGE-IND NOT = ZERO AND NOT = '08'        ELGCSCCP
00637          AND                                                      ELGCSCCP
00638                 GSS-MD-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00639          PERFORM PROCESS-MONDAY-DISCHARGE-PROGR.                  ELGCSCCP
00640      IF GCG-MED-NECESSITY-HCNR-IPS-IN NOT = ZERO                  ELGCSCCP
00641          AND                                                      ELGCSCCP
00642                 GSS-MN-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00643          PERFORM PROCESS-MEDICAL-NECESSITY-PROG.                  ELGCSCCP
00644      IF GCG-MAND-OP-SURG-PROG-IND NOT = ZERO AND NOT = '08'       ELGCSCCP
00645          AND                                                      ELGCSCCP
00646                 GSS-MO-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00647          PERFORM PROCESS-MANDATORY-OUTPATIENT-S.                  ELGCSCCP
00648      IF GCG-MED-SERV-ADV-PROG-IND NOT = ZERO AND NOT = '08'       ELGCSCCP
00649          AND                                                      ELGCSCCP
00650                 GSS-MS-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00651          PERFORM PROCESS-MEDICAL-SERVICES-ADVIS.                  ELGCSCCP
00652      IF GCG-PARTICIPAT-PROV-OPTION NOT = ZERO                     ELGCSCCP
00653          AND                                                      ELGCSCCP
00654                 GSS-PP-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00655          PERFORM PROCESS-PARTICIPATING-PROVIDER.                  ELGCSCCP
00656      IF GCG-PRE-ADM-REVIEW-IND NOT = ZERO AND NOT = '08'          ELGCSCCP
00657          AND                                                      ELGCSCCP
00658                 GSS-PR-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00659          PERFORM PROCESS-PRE-ADMISSION-REVIEW-P.                  ELGCSCCP
00660      IF GCG-PRE-ADM-TESTING-PROGRAM NOT = ZERO AND NOT = '08'     ELGCSCCP
00661          AND                                                      ELGCSCCP
00662                 GSS-PT-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00663          PERFORM PROCESS-PRE-ADMISSION-TESTINGX.                  ELGCSCCP
00664      IF GCG-REIMBUR-SUBROG-IND NOT = ZERO AND NOT = '08'          ELGCSCCP
00665          AND                                                      ELGCSCCP
00666                 GSS-RS-PROG-CODE-CHR (GSS-INDEX)                  ELGCSCCP
00667          PERFORM PROCESS-REIMBURSEMENT-SUBORGAT.                  ELGCSCCP
00668                                                                   ELGCSCCP
00669 ************************************************************      ELGCSCCP
00670 *                                                          *      ELGCSCCP
00671 *        PROCESS ADDITIONAL TRANSPLANT PROGRAM             *      ELGCSCCP
00672 *                                                          *      ELGCSCCP
00673 ************************************************************      ELGCSCCP
00674  PROCESS-ADDITIONAL-TRANSPLANTX.                                  ELGCSCCP
00675      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELGCSCCP
00676      SET ATCP-PGM   TO TRUE.                                      ELGCSCCP
00677      MOVE 'ADDITIONAL TRANSPLANT COVERAGE' TO WS-LEFT-SIDE        ELGCSCCP
00678          (1).                                                     ELGCSCCP
00679      MOVE 'PROGRAM'                        TO WS-LEFT-SIDE (2).   ELGCSCCP
00680      MOVE +2                               TO WS-HEADINGS-CNT.    ELGCSCCP
00681      MOVE GSS-AT-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00682          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00683      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00684          LOW-VALUES                                               ELGCSCCP
00685          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00686      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00687      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00688                                                                   ELGCSCCP
00689 ************************************************************      ELGCSCCP
00690 *                                                          *      ELGCSCCP
00691 *        INITIALIZE WS OUTPUT AREA                         *      ELGCSCCP
00692 *                                                          *      ELGCSCCP
00693 ************************************************************      ELGCSCCP
00694  INITIALIZE-WS-OUTPUT-AREA.                                       ELGCSCCP
00695      INITIALIZE WS-LINE-CNT                                       ELGCSCCP
00696                 WS-OUTPUT                                         ELGCSCCP
00697                 WS-HEADINGS-CNT.                                  ELGCSCCP
00698      SET WS-OUTPUT-IDX TO +1.                                     ELGCSCCP
00699                                                                   ELGCSCCP
00700 ************************************************************      ELGCSCCP
00701 *                                                          *      ELGCSCCP
00702 *        INITIALIZE TEXT COMPRESSION AREA                  *      ELGCSCCP
00703 *                                                          *      ELGCSCCP
00704 ************************************************************      ELGCSCCP
00705  INITIALIZE-TEXT-COMPRESSION-AR.                                  ELGCSCCP
00706      INITIALIZE TCAR-FROM-SUB                                     ELGCSCCP
00707                 TCAR-FROM-LENGTH                                  ELGCSCCP
00708                 TCAR-FROM-AREA.                                   ELGCSCCP
00709                                                                   ELGCSCCP
00710 ************************************************************      ELGCSCCP
00711 *                                                          *      ELGCSCCP
00712 *        DISPLAY ALL INFORMATION STORED IN WS OUTPUT AREA  *      ELGCSCCP
00713 *                                                          *      ELGCSCCP
00714 ************************************************************      ELGCSCCP
00715  DISPLAY-ALL-INFORMATION-STORED.                                  ELGCSCCP
00716      IF WS-LINE-CNT < WS-HEADINGS-CNT                             ELGCSCCP
00717          PERFORM INCREMENT-LINE-COUNTER-FOR-TRA.                  ELGCSCCP
00718      PERFORM MOVE-WS-INFO-TO-OUTPUT                               ELGCSCCP
00719          VARYING WS-OUTPUT-IDX FROM +1 BY +1                      ELGCSCCP
00720                  UNTIL   WS-OUTPUT-IDX > WS-LINE-CNT + 1.         ELGCSCCP
00721      PERFORM LINK-TO-OUTPUT.                                      ELGCSCCP
00722      PERFORM INITIALIZE-WS-OUTPUT-AREA.                           ELGCSCCP
00723                                                                   ELGCSCCP
00724 ************************************************************      ELGCSCCP
00725 *                                                          *      ELGCSCCP
00726 *        INCREMENT LINE COUNTER FOR TRAILING BLANK LINE    *      ELGCSCCP
00727 *                                                          *      ELGCSCCP
00728 ************************************************************      ELGCSCCP
00729  INCREMENT-LINE-COUNTER-FOR-TRA.                                  ELGCSCCP
00730      ADD +1                  TO WS-LINE-CNT.                      ELGCSCCP
00731                                                                   ELGCSCCP
00732 ************************************************************      ELGCSCCP
00733 *                                                          *      ELGCSCCP
00734 *        MOVE WS INFO TO OUTPUT                            *      ELGCSCCP
00735 *                                                          *      ELGCSCCP
00736 ************************************************************      ELGCSCCP
00737  MOVE-WS-INFO-TO-OUTPUT.                                          ELGCSCCP
00738      ADD +1                  TO COF-NBR-DTL-LINES.                ELGCSCCP
00739      MOVE PC-BAR             TO WS-DIVIDER                        ELGCSCCP
00740          (WS-OUTPUT-IDX).                                         ELGCSCCP
00741      MOVE WS-OUTPUT-LINE (WS-OUTPUT-IDX)                          ELGCSCCP
00742                              TO COF-DTL-LINE                      ELGCSCCP
00743          (COF-NBR-DTL-LINES).                                     ELGCSCCP
00744                                                                   ELGCSCCP
00745 ************************************************************      ELGCSCCP
00746 *                                                          *      ELGCSCCP
00747 *        PROCESS WEEKEND ADMISSION PROGRAM                 *      ELGCSCCP
00748 *                                                          *      ELGCSCCP
00749 ************************************************************      ELGCSCCP
00750  PROCESS-WEEKEND-ADMISSION-PROG.                                  ELGCSCCP
00751      SET WEEKN-PGM  TO TRUE.                                      ELGCSCCP
00752      MOVE 'WEEKEND ADMISSION PROGRAM'            TO               ELGCSCCP
00753          WS-LEFT-SIDE (1).                                        ELGCSCCP
00754      MOVE +1                                     TO               ELGCSCCP
00755          WS-HEADINGS-CNT.                                         ELGCSCCP
00756      MOVE GSS-FS-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00757          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00758      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00759          LOW-VALUES                                               ELGCSCCP
00760          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00761      MOVE GSS-FS-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00762      MOVE GSS-FS-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00763      MOVE GSS-FS-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00764      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00765      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00766      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00767                                                                   ELGCSCCP
00768 ************************************************************      ELGCSCCP
00769 *                                                          *      ELGCSCCP
00770 *        PROCESS HOSPICE PROGRAM                           *      ELGCSCCP
00771 *                                                          *      ELGCSCCP
00772 ************************************************************      ELGCSCCP
00773  PROCESS-HOSPICE-PROGRAM.                                         ELGCSCCP
00774      SET HOSP-PGM   TO TRUE.                                      ELGCSCCP
00775      MOVE 'HOSPICE PROGRAM'                      TO               ELGCSCCP
00776          WS-LEFT-SIDE (1).                                        ELGCSCCP
00777      MOVE +1                                     TO               ELGCSCCP
00778          WS-HEADINGS-CNT.                                         ELGCSCCP
00779      MOVE GSS-HO-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00780          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00781      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00782          LOW-VALUES                                               ELGCSCCP
00783          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00784      MOVE GSS-HO-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00785      MOVE GSS-HO-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00786      MOVE GSS-HO-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00787      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00788      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00789      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00790                                                                   ELGCSCCP
00791 ************************************************************      ELGCSCCP
00792 *                                                          *      ELGCSCCP
00793 *        PROCESS INCENTIVE OB PROGRAM                      *      ELGCSCCP
00794 *                                                          *      ELGCSCCP
00795 ************************************************************      ELGCSCCP
00796  PROCESS-INCENTIVE-OB-PROGRAM.                                    ELGCSCCP
00797      SET IOB-PGM    TO TRUE.                                      ELGCSCCP
00798      MOVE 'INCENTIVE OB PROGRAM'    TO WS-LEFT-SIDE               ELGCSCCP
00799          (1).                                                     ELGCSCCP
00800      MOVE +1                        TO WS-HEADINGS-CNT.           ELGCSCCP
00801      MOVE GSS-IO-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00802      MOVE GSS-IO-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00803      MOVE GSS-IO-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00804      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00805      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00806      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00807                                                                   ELGCSCCP
00808 ************************************************************      ELGCSCCP
00809 *                                                          *      ELGCSCCP
00810 *        PROCESS MASOP PROGRAM                             *      ELGCSCCP
00811 *                                                          *      ELGCSCCP
00812 ************************************************************      ELGCSCCP
00813  PROCESS-MASOP-PROGRAM.                                           ELGCSCCP
00814      SET MASOP-PGM  TO TRUE.                                      ELGCSCCP
00815      MOVE 'MANDATORY ADDITIONAL SURGICAL'        TO               ELGCSCCP
00816          WS-LEFT-SIDE (1).                                        ELGCSCCP
00817      MOVE 'OPINION PROGRAM'                      TO               ELGCSCCP
00818          WS-LEFT-SIDE (2).                                        ELGCSCCP
00819      MOVE +2                                     TO               ELGCSCCP
00820          WS-HEADINGS-CNT.                                         ELGCSCCP
00821      MOVE GSS-MA-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00822          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00823      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00824          LOW-VALUES                                               ELGCSCCP
00825          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00826      MOVE GSS-MA-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00827      MOVE GSS-MA-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00828      MOVE GSS-MA-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00829      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00830      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00831      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00832                                                                   ELGCSCCP
00833 ************************************************************      ELGCSCCP
00834 *                                                          *      ELGCSCCP
00835 *        PROCESS MONDAY DISCHARGE PROGRAM                  *      ELGCSCCP
00836 *                                                          *      ELGCSCCP
00837 ************************************************************      ELGCSCCP
00838  PROCESS-MONDAY-DISCHARGE-PROGR.                                  ELGCSCCP
00839      SET MONDC-PGM  TO TRUE.                                      ELGCSCCP
00840      MOVE 'MONDAY DISCHARGE PROGRAM'             TO               ELGCSCCP
00841          WS-LEFT-SIDE (1).                                        ELGCSCCP
00842      MOVE +1                                     TO               ELGCSCCP
00843          WS-HEADINGS-CNT.                                         ELGCSCCP
00844      MOVE GSS-MD-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00845          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00846      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00847          LOW-VALUES                                               ELGCSCCP
00848          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00849      MOVE GSS-MD-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00850      MOVE GSS-MD-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00851      MOVE GSS-MD-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00852      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00853      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00854      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00855                                                                   ELGCSCCP
00856 ************************************************************      ELGCSCCP
00857 *                                                          *      ELGCSCCP
00858 *        PROCESS MEDICAL NECESSITY PROGRAM                 *      ELGCSCCP
00859 *                                                          *      ELGCSCCP
00860 ************************************************************      ELGCSCCP
00861  PROCESS-MEDICAL-NECESSITY-PROG.                                  ELGCSCCP
00862      SET MEDNC-PGM  TO TRUE.                                      ELGCSCCP
00863      MOVE 'MEDICAL NECESSITY PROGRAM'            TO               ELGCSCCP
00864          WS-LEFT-SIDE (1).                                        ELGCSCCP
00865      MOVE +1                                     TO               ELGCSCCP
00866          WS-HEADINGS-CNT.                                         ELGCSCCP
00867      MOVE GSS-MN-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00868          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00869      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00870          LOW-VALUES                                               ELGCSCCP
00871          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00872      MOVE GSS-MN-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00873      MOVE GSS-MN-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00874      MOVE GSS-MN-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00875      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00876      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00877      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00878                                                                   ELGCSCCP
00879 ************************************************************      ELGCSCCP
00880 *                                                          *      ELGCSCCP
00881 *        PROCESS MANDATORY OUTPATIENT SURGERY PROGRAM      *      ELGCSCCP
00882 *                                                          *      ELGCSCCP
00883 ************************************************************      ELGCSCCP
00884  PROCESS-MANDATORY-OUTPATIENT-S.                                  ELGCSCCP
00885      SET MOPS-PGM   TO TRUE.                                      ELGCSCCP
00886      MOVE 'MANDATORY OUTPATIENT SURGERY'         TO               ELGCSCCP
00887          WS-LEFT-SIDE (1).                                        ELGCSCCP
00888      MOVE 'PROGRAM'                              TO               ELGCSCCP
00889          WS-LEFT-SIDE (2).                                        ELGCSCCP
00890      MOVE +2                                     TO               ELGCSCCP
00891          WS-HEADINGS-CNT.                                         ELGCSCCP
00892      MOVE GSS-MO-APPROVAL-SOURCE-IND (GSS-INDEX) TO               ELGCSCCP
00893          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00894      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00895          LOW-VALUES                                               ELGCSCCP
00896          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00897      MOVE GSS-MO-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00898      MOVE GSS-MO-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00899      MOVE GSS-MO-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00900      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00901      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00902      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00903                                                                   ELGCSCCP
00904 ************************************************************      ELGCSCCP
00905 *                                                          *      ELGCSCCP
00906 *        PROCESS MEDICAL SERVICES ADVISORY PROGRAM         *      ELGCSCCP
00907 *                                                          *      ELGCSCCP
00908 ************************************************************      ELGCSCCP
00909  PROCESS-MEDICAL-SERVICES-ADVIS.                                  ELGCSCCP
00910      SET MSA-PGM    TO TRUE.                                      ELGCSCCP
00911      MOVE 'MEDICAL SERVICES ADVISORY'            TO               ELGCSCCP
00912          WS-LEFT-SIDE (1).                                        ELGCSCCP
00913      MOVE 'PROGRAM'                              TO               ELGCSCCP
00914          WS-LEFT-SIDE (2).                                        ELGCSCCP
00915      MOVE +2                                     TO               ELGCSCCP
00916          WS-HEADINGS-CNT.                                         ELGCSCCP
00917      MOVE GSS-MS-PROG-SOURCE-IND (GSS-INDEX)     TO               ELGCSCCP
00918          WS-PROG-SRCE-IND.                                        ELGCSCCP
00919      IF WS-PROG-SRCE-IND NOT = SPACES AND ZEROS AND               ELGCSCCP
00920          LOW-VALUES                                               ELGCSCCP
00921          PERFORM CONSTRUCT-PROGRAM-SOURCE-SENTE.                  ELGCSCCP
00922      MOVE GSS-MS-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00923      MOVE GSS-MS-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00924      MOVE GSS-MS-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00925      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00926      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00927      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00928                                                                   ELGCSCCP
00929 ************************************************************      ELGCSCCP
00930 *                                                          *      ELGCSCCP
00931 *        PROCESS PARTICIPATING PROVIDER OPTION PROGRAM     *      ELGCSCCP
00932 *                                                          *      ELGCSCCP
00933 ************************************************************      ELGCSCCP
00934  PROCESS-PARTICIPATING-PROVIDER.                                  ELGCSCCP
00935      SET PPO-PGM    TO TRUE.                                      ELGCSCCP
00936      MOVE 'PREFERRED PROVIDER OPTION'            TO               ELGCSCCP
00937          WS-LEFT-SIDE (1).                                        ELGCSCCP
00938      MOVE 'PROGRAM'                              TO               ELGCSCCP
00939          WS-LEFT-SIDE (2).                                        ELGCSCCP
00940      MOVE +2                                     TO               ELGCSCCP
00941          WS-HEADINGS-CNT.                                         ELGCSCCP
00942      MOVE GSS-PP-APPROVAL-SRC-IND (GSS-INDEX)    TO               ELGCSCCP
00943          WS-APPRVL-SRCE-IND.                                      ELGCSCCP
00944      IF WS-APPRVL-SRCE-IND NOT EQUAL ZEROS AND SPACES AND         ELGCSCCP
00945          LOW-VALUES                                               ELGCSCCP
00946          PERFORM CONSTRUCT-APPROVAL-SOURCE-SENT.                  ELGCSCCP
00947      MOVE GSS-PP-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00948      MOVE GSS-PP-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00949      MOVE GSS-PP-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00950      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00951      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00952      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00953                                                                   ELGCSCCP
00954 ************************************************************      ELGCSCCP
00955 *                                                          *      ELGCSCCP
00956 *        PROCESS PRE-ADMISSION REVIEW PROGRAM              *      ELGCSCCP
00957 *                                                          *      ELGCSCCP
00958 ************************************************************      ELGCSCCP
00959  PROCESS-PRE-ADMISSION-REVIEW-P.                                  ELGCSCCP
00960      SET PAR-PGM    TO TRUE.                                      ELGCSCCP
00961      MOVE 'PRE-ADMISSION REVIEW PROGRAM'         TO               ELGCSCCP
00962          WS-LEFT-SIDE (1).                                        ELGCSCCP
00963      MOVE +1                                     TO               ELGCSCCP
00964          WS-HEADINGS-CNT.                                         ELGCSCCP
00965      MOVE GSS-PR-PROG-SOURCE-IND (GSS-INDEX)     TO               ELGCSCCP
00966          WS-PROG-SRCE-IND.                                        ELGCSCCP
00967      IF WS-PROG-SRCE-IND NOT = SPACES AND ZEROS AND               ELGCSCCP
00968          LOW-VALUES                                               ELGCSCCP
00969          PERFORM CONSTRUCT-PROGRAM-SOURCE-SENTE.                  ELGCSCCP
00970      MOVE GSS-PR-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00971      MOVE GSS-PR-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00972      MOVE GSS-PR-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00973      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00974      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00975      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00976                                                                   ELGCSCCP
00977 ************************************************************      ELGCSCCP
00978 *                                                          *      ELGCSCCP
00979 *        PROCESS PRE-ADMISSION TESTING PROGRAM             *      ELGCSCCP
00980 *                                                          *      ELGCSCCP
00981 ************************************************************      ELGCSCCP
00982  PROCESS-PRE-ADMISSION-TESTINGX.                                  ELGCSCCP
00983      SET PAT-PGM    TO TRUE.                                      ELGCSCCP
00984      MOVE 'PRE-ADMISSION TESTING PROGRAM'  TO WS-LEFT-SIDE        ELGCSCCP
00985          (1).                                                     ELGCSCCP
00986      MOVE +1                               TO WS-HEADINGS-CNT.    ELGCSCCP
00987      MOVE GSS-PT-BC-IND (GSS-INDEX) TO WS-BC-IND.                 ELGCSCCP
00988      MOVE GSS-PT-BS-IND (GSS-INDEX) TO WS-BS-IND.                 ELGCSCCP
00989      MOVE GSS-PT-MM-IND (GSS-INDEX) TO WS-MM-IND.                 ELGCSCCP
00990      PERFORM CONSTRUCT-APPLICABLE-SENTENCE.                       ELGCSCCP
00991      PERFORM GENERATE-ACCUM-TABULAR-DATA-PE.                      ELGCSCCP
00992      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
00993                                                                   ELGCSCCP
00994 ************************************************************      ELGCSCCP
00995 *                                                          *      ELGCSCCP
00996 *        PROCESS REIMBURSEMENT SUBORGATION PROGRAM         *      ELGCSCCP
00997 *                                                          *      ELGCSCCP
00998 ************************************************************      ELGCSCCP
00999  PROCESS-REIMBURSEMENT-SUBORGAT.                                  ELGCSCCP
01000      SET REIMB-PGM  TO TRUE.                                      ELGCSCCP
01001      MOVE 'REIMBURSEMENT SUBROGATION'        TO WS-LEFT-SIDE (1). ELGCSCCP
01002      MOVE 'PROGRAM'                          TO WS-LEFT-SIDE (2). ELGCSCCP
01003      MOVE +2                                 TO WS-HEADINGS-CNT.  ELGCSCCP
01004      PERFORM CONSTRUCT-REIMBURSEMENT-SUBROG.                      ELGCSCCP
01005 *    IF GSS-RS-RESPONSIBILITY-IND (GSS-INDEX) NOT EQUAL           ELGCSCCP
01006 *        ZEROS                                                    ELGCSCCP
01007 *               AND SPACES AND LOW-VALUES                         ELGCSCCP
01008 *    IF GSS-RS-MEMB-RELATIONSHIP-IND (GSS-INDEX) NOT EQUAL        ELGCSCCP
01009 *        ZEROS                                                    ELGCSCCP
01010 *               AND SPACES AND LOW-VALUES                         ELGCSCCP
01011 *        PERFORM CONSTRUCT-MEMBER-RELATIONSHIPX.                  ELGCSCCP
01012 *    IF GSS-RS-INVEST-DOLR-MIN (GSS-INDEX) GREATER THAN           ELGCSCCP
01013 *        ZEROS                                                    ELGCSCCP
01014 *        PERFORM CONSTRUCT-INVESTIGATION-DOLLAR.                  ELGCSCCP
01015 *    PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
01016                                                                   ELGCSCCP
01017 ************************************************************      ELGCSCCP
01018 *                                                          *      ELGCSCCP
01019 *        CONSTRUCT REIMBURSEMENT SUBROGATION SENTENCE      *      ELGCSCCP
01020 *                                                          *      ELGCSCCP
01021 ************************************************************      ELGCSCCP
01022  CONSTRUCT-REIMBURSEMENT-SUBROG.                                  ELGCSCCP
01023      MOVE WS-REIMB-SUBROG-A TO WS-RIGHT-SIDE(1).                  ELGCSCCP
01024      MOVE WS-REIMB-SUBROG-B TO WS-RIGHT-SIDE(2).                  ELGCSCCP
01025      PERFORM DISPLAY-ALL-INFORMATION-STORED.                      ELGCSCCP
01026                                                                   ELGCSCCP
01027 ************************************************************      ELGCSCCP
01028 *                                                          *      ELGCSCCP
01029 *        CONSTRUCT APPROVAL SOURCE SENTENCE                *      ELGCSCCP
01030 *                                                          *      ELGCSCCP
01031 ************************************************************      ELGCSCCP
01032  CONSTRUCT-APPROVAL-SOURCE-SENT.                                  ELGCSCCP
01033 ******************************************************            ELGCSCCP
01034 ** PRIOR TO ENTERING THIS PARAGRAGH, THE APPROVAL   **            ELGCSCCP
01035 ** SOURCE INDICATOR FOR THE CCP WE ARE CURRENTLY    **            ELGCSCCP
01036 ** WORKING ON IS MOVED TO THE WS-APPRVL-SRCE-IND.   **            ELGCSCCP
01037 ** I CHOSE JUST ONE SYSTEM NAME BECAUSE THE CODE    **            ELGCSCCP
01038 ** VALUES ARE THE SAME AMONG THE CCPS.              **            ELGCSCCP
01039 ******************************************************            ELGCSCCP
01040      MOVE PC-GCCP                  TO                             ELGCSCCP
01041          CMF-RECORD-PREFIX.                                       ELGCSCCP
01042      MOVE WS-APPRVL-SRCE-IND       TO CMF-CODE-VALUE.             ELGCSCCP
01043      MOVE 'MO-APPROVAL-SOURCE-IND' TO CMF-ELEMENT-SYSTEM-NAME.    ELGCSCCP
01044      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01045      ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01046      MOVE PC-REQUIRES              TO TCAR-FROM-LINE              ELGCSCCP
01047          (TCAR-FROM-SUB).                                         ELGCSCCP
01048      PERFORM MOVE-TRANSLATION-INTO-COMPRESS                       ELGCSCCP
01049          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCSCCP
01050                  UNTIL   CMF-DESCR-IDX >                          ELGCSCCP
01051              CMF-NBR-DESCR-LINES.                                 ELGCSCCP
01052      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01053      MOVE PC-APPROVAL              TO TCAR-FROM-LINE              ELGCSCCP
01054          (TCAR-FROM-SUB).                                         ELGCSCCP
01055      PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                      ELGCSCCP
01056      INITIALIZE WS-APPRVL-SRCE-IND.                               ELGCSCCP
01057                                                                   ELGCSCCP
01058 ************************************************************      ELGCSCCP
01059 *                                                          *      ELGCSCCP
01060 *        CONSTRUCT PROGRAM SOURCE SENTENCE                 *      ELGCSCCP
01061 *                                                          *      ELGCSCCP
01062 ************************************************************      ELGCSCCP
01063  CONSTRUCT-PROGRAM-SOURCE-SENTE.                                  ELGCSCCP
01064 ******************************************************            ELGCSCCP
01065 ** PRIOR TO ENTERING THIS PARAGRAGH, THE PROGRAM    **            ELGCSCCP
01066 ** SOURCE INDICATOR FOR THE CCP WE ARE CURRENTLY    **            ELGCSCCP
01067 ** WORKING ON IS MOVED TO THE WS-PROG-SRCE-IND.     **            ELGCSCCP
01068 ** I CHOSE JUST ONE SYSTEM NAME BECAUSE THE CODE    **            ELGCSCCP
01069 ** VALUES ARE THE SAME AMONG THE CCPS.              **            ELGCSCCP
01070 ******************************************************            ELGCSCCP
01071      MOVE PC-GCCP                  TO CMF-RECORD-PREFIX.          ELGCSCCP
01072      MOVE WS-PROG-SRCE-IND         TO CMF-CODE-VALUE.             ELGCSCCP
01073      MOVE 'PR-PROG-SOURCE-IND'     TO CMF-ELEMENT-SYSTEM-NAME.    ELGCSCCP
01074      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01075      ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01076      MOVE PC-REQUIRES              TO TCAR-FROM-LINE              ELGCSCCP
01077          (TCAR-FROM-SUB).                                         ELGCSCCP
01078      PERFORM MOVE-TRANSLATION-INTO-COMPRESS                       ELGCSCCP
01079          VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCSCCP
01080                  UNTIL   CMF-DESCR-IDX >                          ELGCSCCP
01081              CMF-NBR-DESCR-LINES.                                 ELGCSCCP
01082      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01083      MOVE PC-APPROVAL              TO TCAR-FROM-LINE              ELGCSCCP
01084          (TCAR-FROM-SUB).                                         ELGCSCCP
01085      PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                      ELGCSCCP
01086      INITIALIZE WS-PROG-SRCE-IND.                                 ELGCSCCP
01087                                                                   ELGCSCCP
01088 ************************************************************      ELGCSCCP
01089 *                                                          *      ELGCSCCP
01090 *        CALL CODES MANUAL INTERFACE                       *      ELGCSCCP
01091 *                                                          *      ELGCSCCP
01092 ************************************************************      ELGCSCCP
01093  CALL-CODES-MANUAL-INTERFACE.                                     ELGCSCCP
01094      EXEC CICS LINK                                               ELGCSCCP
01095                PROGRAM ('ELUCMIF')                                ELGCSCCP
01096                COMMAREA (DFHCOMMAREA)                             ELGCSCCP
01097         END-EXEC.                                                 ELGCSCCP
01098      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELGCSCCP
01099      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELGCSCCP
01100                 ADDRESS OF CMF-DESCR.                             ELGCSCCP
01101                                                                   ELGCSCCP
01102 ************************************************************      ELGCSCCP
01103 *                                                          *      ELGCSCCP
01104 *        MOVE TRANSLATION INTO COMPRESSION AREA            *      ELGCSCCP
01105 *                                                          *      ELGCSCCP
01106 ************************************************************      ELGCSCCP
01107  MOVE-TRANSLATION-INTO-COMPRESS.                                  ELGCSCCP
01108      ADD +1              TO TCAR-FROM-SUB.                        ELGCSCCP
01109      MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                          ELGCSCCP
01110                          TO TCAR-FROM-LINE                        ELGCSCCP
01111          (TCAR-FROM-SUB).                                         ELGCSCCP
01112                                                                   ELGCSCCP
01113 ************************************************************      ELGCSCCP
01114 *                                                          *      ELGCSCCP
01115 *        CALL TEXT COMPRESSION MODULE                      *      ELGCSCCP
01116 *                                                          *      ELGCSCCP
01117 ************************************************************      ELGCSCCP
01118  CALL-TEXT-COMPRESSION-MODULE.                                    ELGCSCCP
01119      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELGCSCCP
01120                                                                   ELGCSCCP
01121 ************************************************************      ELGCSCCP
01122 *                                                          *      ELGCSCCP
01123 *        SETUP FOR TRANSLATION UNSTRING OPERATION          *      ELGCSCCP
01124 *                                                          *      ELGCSCCP
01125 ************************************************************      ELGCSCCP
01126  SETUP-FOR-TRANSLATION-UNSTRING.                                  ELGCSCCP
01127      MOVE +10            TO TCAR-OUTPUT-FIELD-COUNT.              ELGCSCCP
01128      MOVE +46            TO TCAR-OUTPUT-FIELD-1-LEN.              ELGCSCCP
01129      MOVE +46            TO TCAR-OUTPUT-FIELD-2-LEN.              ELGCSCCP
01130      MOVE +46            TO TCAR-OUTPUT-FIELD-3-LEN.              ELGCSCCP
01131      MOVE +46            TO TCAR-OUTPUT-FIELD-4-LEN.              ELGCSCCP
01132      MOVE +46            TO TCAR-OUTPUT-FIELD-5-LEN.              ELGCSCCP
01133      MOVE +46            TO TCAR-OUTPUT-FIELD-6-LEN.              ELGCSCCP
01134      MOVE +46            TO TCAR-OUTPUT-FIELD-7-LEN.              ELGCSCCP
01135      MOVE +46            TO TCAR-OUTPUT-FIELD-8-LEN.              ELGCSCCP
01136      MOVE +46            TO TCAR-OUTPUT-FIELD-9-LEN.              ELGCSCCP
01137      MOVE +46            TO TCAR-OUTPUT-FIELD-10-LEN.             ELGCSCCP
01138                                                                   ELGCSCCP
01139                                                                   ELGCSCCP
01140 ************************************************************      ELGCSCCP
01141 *                                                          *      ELGCSCCP
01142 *        CALL TEXT UNSTRING MODULE                         *      ELGCSCCP
01143 *                                                          *      ELGCSCCP
01144 ************************************************************      ELGCSCCP
01145  CALL-TEXT-UNSTRING-MODULE.                                       ELGCSCCP
01146      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELGCSCCP
01147                                                                   ELGCSCCP
01148 ************************************************************      ELGCSCCP
01149 *                                                          *      ELGCSCCP
01150 *        MOVE FORMATTED TEXT TO WS OUTPUT AREA             *      ELGCSCCP
01151 *                                                          *      ELGCSCCP
01152 ************************************************************      ELGCSCCP
01153  MOVE-FORMATTED-TEXT-TO-WS-OUTP.                                  ELGCSCCP
01154      ADD +1                      TO WS-LINE-CNT.                  ELGCSCCP
01155      MOVE TCAR-OPF-DATA (TCAR-X) TO WS-RIGHT-SIDE                 ELGCSCCP
01156          (WS-OUTPUT-IDX).                                         ELGCSCCP
01157      SET  WS-OUTPUT-IDX UP BY +1.                                 ELGCSCCP
01158                                                                   ELGCSCCP
01159                                                                   ELGCSCCP
01160 ************************************************************      ELGCSCCP
01161 *                                                          *      ELGCSCCP
01162 *        CONSTRUCT APPLICABLE SENTENCE                     *      ELGCSCCP
01163 *                                                          *      ELGCSCCP
01164 ************************************************************      ELGCSCCP
01165  CONSTRUCT-APPLICABLE-SENTENCE.                                   ELGCSCCP
01166      INITIALIZE WS-LOB-SUB                                        ELGCSCCP
01167                 WS-LOB-TABLE.                                     ELGCSCCP
01168      ADD +1                TO TCAR-FROM-SUB.                      ELGCSCCP
01169      MOVE PC-APPLICABLE    TO TCAR-FROM-LINE                      ELGCSCCP
01170          (TCAR-FROM-SUB).                                         ELGCSCCP
01171      IF WS-BC-IND NOT EQUAL ZEROS AND SPACES AND                  ELGCSCCP
01172          LOW-VALUES                                               ELGCSCCP
01173          PERFORM LIST-BLUE-CROSS.                                 ELGCSCCP
01174      IF WS-BS-IND NOT EQUAL ZEROS AND SPACES AND                  ELGCSCCP
01175          LOW-VALUES                                               ELGCSCCP
01176          PERFORM LIST-BLUE-SHIELD.                                ELGCSCCP
01177      IF WS-MM-IND NOT EQUAL ZEROS AND SPACES AND                  ELGCSCCP
01178          LOW-VALUES                                               ELGCSCCP
01179          PERFORM LIST-MAJOR-MEDICAL.                              ELGCSCCP
01180      IF LOB-3                                                     ELGCSCCP
01181          PERFORM INSERT-PUNCTUATION-FOR-ALL-THR                   ELGCSCCP
01182      ELSE IF LOB-2                                                ELGCSCCP
01183          PERFORM INSERT-PUNCTUATION-FOR-TWO                       ELGCSCCP
01184      ELSE                                                         ELGCSCCP
01185          PERFORM INSERT-PUNCTUATION-FOR-ONE.                      ELGCSCCP
01186      ADD +1                TO TCAR-FROM-SUB.                      ELGCSCCP
01187      MOVE WS-LOB-TABLE     TO TCAR-FROM-LINE                      ELGCSCCP
01188          (TCAR-FROM-SUB).                                         ELGCSCCP
01189      PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                      ELGCSCCP
01190      INITIALIZE WS-LOB-INDICATORS.                                ELGCSCCP
01191                                                                   ELGCSCCP
01192 ************************************************************      ELGCSCCP
01193 *                                                          *      ELGCSCCP
01194 *        LIST BLUE CROSS                                   *      ELGCSCCP
01195 *                                                          *      ELGCSCCP
01196 ************************************************************      ELGCSCCP
01197  LIST-BLUE-CROSS.                                                 ELGCSCCP
01198      ADD +1                TO WS-LOB-SUB.                         ELGCSCCP
01199      MOVE PC-BLUE-CROSS    TO WS-LOB-NAME (WS-LOB-SUB).           ELGCSCCP
01200                                                                   ELGCSCCP
01201 ************************************************************      ELGCSCCP
01202 *                                                          *      ELGCSCCP
01203 *        LIST BLUE SHIELD                                  *      ELGCSCCP
01204 *                                                          *      ELGCSCCP
01205 ************************************************************      ELGCSCCP
01206  LIST-BLUE-SHIELD.                                                ELGCSCCP
01207      ADD +1                TO WS-LOB-SUB.                         ELGCSCCP
01208      MOVE PC-BLUE-SHIELD   TO WS-LOB-NAME (WS-LOB-SUB).           ELGCSCCP
01209                                                                   ELGCSCCP
01210 ************************************************************      ELGCSCCP
01211 *                                                          *      ELGCSCCP
01212 *        LIST MAJOR MEDICAL                                *      ELGCSCCP
01213 *                                                          *      ELGCSCCP
01214 ************************************************************      ELGCSCCP
01215  LIST-MAJOR-MEDICAL.                                              ELGCSCCP
01216      ADD +1                TO WS-LOB-SUB.                         ELGCSCCP
01217      MOVE PC-MAJ-MED       TO WS-LOB-NAME (WS-LOB-SUB).           ELGCSCCP
01218                                                                   ELGCSCCP
01219 ************************************************************      ELGCSCCP
01220 *                                                          *      ELGCSCCP
01221 *        INSERT PUNCTUATION FOR ALL THREE                  *      ELGCSCCP
01222 *                                                          *      ELGCSCCP
01223 ************************************************************      ELGCSCCP
01224  INSERT-PUNCTUATION-FOR-ALL-THR.                                  ELGCSCCP
01225      MOVE PC-COMMA         TO WS-PUNCTUATION (1).                 ELGCSCCP
01226      MOVE PC-COMMA         TO WS-PUNCTUATION (2).                 ELGCSCCP
01227      MOVE PC-SEMI-COLON    TO WS-PUNCTUATION (3).                 ELGCSCCP
01228                                                                   ELGCSCCP
01229 ************************************************************      ELGCSCCP
01230 *                                                          *      ELGCSCCP
01231 *        INSERT PUNCTUATION FOR TWO                        *      ELGCSCCP
01232 *                                                          *      ELGCSCCP
01233 ************************************************************      ELGCSCCP
01234  INSERT-PUNCTUATION-FOR-TWO.                                      ELGCSCCP
01235      MOVE PC-COMMA         TO WS-PUNCTUATION (1).                 ELGCSCCP
01236      MOVE PC-SEMI-COLON    TO WS-PUNCTUATION (2).                 ELGCSCCP
01237                                                                   ELGCSCCP
01238 ************************************************************      ELGCSCCP
01239 *                                                          *      ELGCSCCP
01240 *        INSERT PUNCTUATION FOR ONE                        *      ELGCSCCP
01241 *                                                          *      ELGCSCCP
01242 ************************************************************      ELGCSCCP
01243  INSERT-PUNCTUATION-FOR-ONE.                                      ELGCSCCP
01244      MOVE PC-SEMI-COLON    TO WS-PUNCTUATION (1).                 ELGCSCCP
01245                                                                   ELGCSCCP
01246 ************************************************************      ELGCSCCP
01247 *                                                          *      ELGCSCCP
01248 *        CONSTRUCT RESPONSIBILITY SENTENCE                 *      ELGCSCCP
01249 *                                                          *      ELGCSCCP
01250 ************************************************************      ELGCSCCP
01251 *CONSTRUCT-RESPONSIBILITY-SENTE.                                  ELGCSCCP
01252 *    MOVE PC-GCCP                  TO CMF-RECORD-PREFIX.          ELGCSCCP
01253 *    MOVE GSS-RS-RESPONSIBILITY-IND (GSS-INDEX)                   ELGCSCCP
01254 *                                  TO CMF-CODE-VALUE.             ELGCSCCP
01255 *    MOVE 'RS-RESPONSIBILITY-IND'  TO                             ELGCSCCP
01256 *        CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSCCP
01257 *    PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01258 *    ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01259 *    MOVE PC-REQUIRES              TO TCAR-FROM-LINE              ELGCSCCP
01260 *        (TCAR-FROM-SUB).                                         ELGCSCCP
01261 *    PERFORM MOVE-TRANSLATION-INTO-COMPRESS                       ELGCSCCP
01262 *        VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCSCCP
01263 *                UNTIL   CMF-DESCR-IDX >                          ELGCSCCP
01264 *            CMF-NBR-DESCR-LINES.                                 ELGCSCCP
01265 *    ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01266 *    MOVE PC-SEMI-COLON            TO TCAR-FROM-LINE              ELGCSCCP
01267 *        (TCAR-FROM-SUB).                                         ELGCSCCP
01268 *    PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                      ELGCSCCP
01269                                                                   ELGCSCCP
01270 ************************************************************      ELGCSCCP
01271 *                                                          *      ELGCSCCP
01272 *        CONSTRUCT MEMBER RELATIONSHIP SENTENCE            *      ELGCSCCP
01273 *                                                          *      ELGCSCCP
01274 ************************************************************      ELGCSCCP
01275 *CONSTRUCT-MEMBER-RELATIONSHIPX.                                  ELGCSCCP
01276 *    MOVE PC-GCCP                  TO CMF-RECORD-PREFIX.          ELGCSCCP
01277 *    MOVE GSS-RS-MEMB-RELATIONSHIP-IND (GSS-INDEX)                ELGCSCCP
01278 *                                  TO CMF-CODE-VALUE.             ELGCSCCP
01279 *    MOVE 'RS-MEMB-RELATIONSHIP-IND'                              ELGCSCCP
01280 *                                  TO                             ELGCSCCP
01281 *        CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSCCP
01282 *    PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01283 *    ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01284 *    MOVE PC-APPLICABLE            TO TCAR-FROM-LINE              ELGCSCCP
01285 *        (TCAR-FROM-SUB).                                         ELGCSCCP
01286 *    PERFORM MOVE-TRANSLATION-INTO-COMPRESS                       ELGCSCCP
01287 *        VARYING CMF-DESCR-IDX FROM +1 BY +1                      ELGCSCCP
01288 *                UNTIL   CMF-DESCR-IDX >                          ELGCSCCP
01289 *            CMF-NBR-DESCR-LINES.                                 ELGCSCCP
01290 *    ADD  +1                       TO TCAR-FROM-SUB.              ELGCSCCP
01291 *    MOVE PC-SEMI-COLON            TO TCAR-FROM-LINE              ELGCSCCP
01292 *        (TCAR-FROM-SUB).                                         ELGCSCCP
01293 *    PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                      ELGCSCCP
01294                                                                   ELGCSCCP
01295 ************************************************************      ELGCSCCP
01296 *                                                          *      ELGCSCCP
01297 *        CONSTRUCT INVESTIGATION DOLLAR SENTENCE           *      ELGCSCCP
01298 *                                                          *      ELGCSCCP
01299 ************************************************************      ELGCSCCP
01300 *CONSTRUCT-INVESTIGATION-DOLLAR.                                  ELGCSCCP
01301 *    MOVE GSS-RS-INVEST-DOLR-MIN (GSS-INDEX)                      ELGCSCCP
01302 *                                  TO                             ELGCSCCP
01303 *        WS-RS-INVEST-DOLLAR.                                     ELGCSCCP
01304 *    ADD +1                        TO WS-LINE-CNT.                ELGCSCCP
01305 *    MOVE WS-REIMB-INVEST-PHRASE   TO WS-RIGHT-SIDE               ELGCSCCP
01306 *        (WS-OUTPUT-IDX).                                         ELGCSCCP
01307                                                                   ELGCSCCP
01308 ************************************************************      ELGCSCCP
01309 *                                                          *      ELGCSCCP
01310 *        GENERATE ACCUM TABULAR DATA PERTAINING TO CCP     *      ELGCSCCP
01311 *                                                          *      ELGCSCCP
01312 ************************************************************      ELGCSCCP
01313  GENERATE-ACCUM-TABULAR-DATA-PE.                                  ELGCSCCP
01314      INITIALIZE WS-ACCUM-SW.                                      ELGCSCCP
01315      PERFORM SEARCH-FOR-COINSURANCE-INFORMA.                      ELGCSCCP
01316      PERFORM SEARCH-FOR-DEDUCTIBLE-INFORMAT.                      ELGCSCCP
01317      PERFORM SEARCH-FOR-MAXIMUM-INFORMATION.                      ELGCSCCP
01318      IF ACCUM-FOUND                                               ELGCSCCP
01319          PERFORM COMPRESS-AND-MOVE-INTO-WS-OUTP.                  ELGCSCCP
01320                                                                   ELGCSCCP
01321 ************************************************************      ELGCSCCP
01322 *                                                          *      ELGCSCCP
01323 *        SEARCH FOR COINSURANCE INFORMATION                *      ELGCSCCP
01324 *                                                          *      ELGCSCCP
01325 ************************************************************      ELGCSCCP
01326  SEARCH-FOR-COINSURANCE-INFORMA.                                  ELGCSCCP
01327      SET WS-PROCESSING-ACL TO TRUE.                               ELGCSCCP
01328      SET CSAC-X-IDX TO +2.                                        ELGCSCCP
01329      IF CSAC-GC-TBL-PTR (CSAC-X-IDX) NOT = NULL                   ELGCSCCP
01330          PERFORM PROCESS-ACCUMULATORS.                            ELGCSCCP
01331                                                                   ELGCSCCP
01332 ************************************************************      ELGCSCCP
01333 *                                                          *      ELGCSCCP
01334 *        SEARCH FOR DEDUCTIBLE INFORMATION                 *      ELGCSCCP
01335 *                                                          *      ELGCSCCP
01336 ************************************************************      ELGCSCCP
01337  SEARCH-FOR-DEDUCTIBLE-INFORMAT.                                  ELGCSCCP
01338      SET WS-PROCESSING-ADL TO TRUE.                               ELGCSCCP
01339      SET CSAC-X-IDX TO +3.                                        ELGCSCCP
01340      IF CSAC-GC-TBL-PTR (CSAC-X-IDX) NOT = NULL                   ELGCSCCP
01341          PERFORM PROCESS-ACCUMULATORS.                            ELGCSCCP
01342                                                                   ELGCSCCP
01343 ************************************************************      ELGCSCCP
01344 *                                                          *      ELGCSCCP
01345 *        SEARCH FOR MAXIMUM INFORMATION                    *      ELGCSCCP
01346 *                                                          *      ELGCSCCP
01347 ************************************************************      ELGCSCCP
01348  SEARCH-FOR-MAXIMUM-INFORMATION.                                  ELGCSCCP
01349      SET WS-PROCESSING-ABM TO TRUE.                               ELGCSCCP
01350      SET CSAC-X-IDX TO +1.                                        ELGCSCCP
01351      IF CSAC-GC-TBL-PTR (CSAC-X-IDX) NOT = NULL                   ELGCSCCP
01352          PERFORM PROCESS-ACCUMULATORS.                            ELGCSCCP
01353                                                                   ELGCSCCP
01354 ************************************************************      ELGCSCCP
01355 *                                                          *      ELGCSCCP
01356 *        PROCESS ACCUMULATORS                              *      ELGCSCCP
01357 *                                                          *      ELGCSCCP
01358 ************************************************************      ELGCSCCP
01359  PROCESS-ACCUMULATORS.                                            ELGCSCCP
01360      SET ADDRESS OF ATBL-ACCUMULATOR-TABLE TO                     ELGCSCCP
01361          CSAC-GC-TBL-PTR (CSAC-X-IDX).                            ELGCSCCP
01362      PERFORM DETERMINE-IF-ACCUM-APPLIES-TOX                       ELGCSCCP
01363          VARYING WS-ATBL-SUB FROM +1 BY +1                        ELGCSCCP
01364                  UNTIL   WS-ATBL-SUB > ATBL-TBL-CNT               ELGCSCCP
01365                  OR      WS-CC-QUALIFIER = WS-PROGRAM-TYPE.       ELGCSCCP
01366      INITIALIZE WS-CC-INDICATOR.                                  ELGCSCCP
01367                                                                   ELGCSCCP
01368 ************************************************************      ELGCSCCP
01369 *                                                          *      ELGCSCCP
01370 *        DETERMINE IF ACCUM APPLIES TO CCP                 *      ELGCSCCP
01371 *                                                          *      ELGCSCCP
01372 ************************************************************      ELGCSCCP
01373  DETERMINE-IF-ACCUM-APPLIES-TOX.                                  ELGCSCCP
01374      SET ATBL-X-IDX TO WS-ATBL-SUB.                               ELGCSCCP
01375      MOVE ATBL-COST-CONTAIN-IND (ATBL-X-IDX) TO                   ELGCSCCP
01376          WS-CC-INDICATOR.                                         ELGCSCCP
01377      IF WS-CC-QUALIFIER = WS-PROGRAM-TYPE                         ELGCSCCP
01378          PERFORM ACCUM-APPLIES-TO-CCP.                            ELGCSCCP
01379                                                                   ELGCSCCP
01380 ************************************************************      ELGCSCCP
01381 *                                                          *      ELGCSCCP
01382 *        ACCUM APPLIES TO CCP                              *      ELGCSCCP
01383 *                                                          *      ELGCSCCP
01384 ************************************************************      ELGCSCCP
01385  ACCUM-APPLIES-TO-CCP.                                            ELGCSCCP
01386      IF WS-PROCESSING-ACL                                         ELGCSCCP
01387          PERFORM GENERATE-COINSURANCE                             ELGCSCCP
01388      ELSE IF WS-PROCESSING-ADL                                    ELGCSCCP
01389          PERFORM GENERATE-DEDUCTIBLE                              ELGCSCCP
01390      ELSE IF WS-PROCESSING-ABM                                    ELGCSCCP
01391          PERFORM GENERATE-MAXIMUM                                 ELGCSCCP
01392      ELSE                                                         ELGCSCCP
01393          PERFORM SIGNAL-PROGRAM-LOGIC-ERROR.                      ELGCSCCP
01394                                                                   ELGCSCCP
01395 ************************************************************      ELGCSCCP
01396 *                                                          *      ELGCSCCP
01397 *        GENERATE COINSURANCE                              *      ELGCSCCP
01398 *                                                          *      ELGCSCCP
01399 ************************************************************      ELGCSCCP
01400  GENERATE-COINSURANCE.                                            ELGCSCCP
01401      MOVE ATBL-PERCENT-LEVEL (ATBL-X-IDX) TO                      ELGCSCCP
01402          WS-PCT-LVL.                                              ELGCSCCP
01403      ADD +1                       TO TCAR-FROM-SUB.               ELGCSCCP
01404      MOVE WS-COINSURANCE-PHRASE   TO TCAR-FROM-LINE               ELGCSCCP
01405          (TCAR-FROM-SUB).                                         ELGCSCCP
01406      IF ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX) NOT = SPACES        ELGCSCCP
01407          AND                                                      ELGCSCCP
01408                 ZEROS AND LOW-VALUES                              ELGCSCCP
01409          PERFORM INSERT-ACL-INTERNAL-DESCRIPTOR.                  ELGCSCCP
01410      PERFORM INSERT-ACCUM-COST-CONTAINMENTX.                      ELGCSCCP
01411      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01412      MOVE '.'                      TO TCAR-FROM-LINE              ELGCSCCP
01413          (TCAR-FROM-SUB).                                         ELGCSCCP
01414      SET ACCUM-FOUND TO TRUE.                                     ELGCSCCP
01415                                                                   ELGCSCCP
01416                                                                   ELGCSCCP
01417 ************************************************************      ELGCSCCP
01418 *                                                          *      ELGCSCCP
01419 *        INSERT ACL INTERNAL DESCRIPTOR INTO COMPRESSION AR*      ELGCSCCP
01420 *                                                          *      ELGCSCCP
01421 ************************************************************      ELGCSCCP
01422  INSERT-ACL-INTERNAL-DESCRIPTOR.                                  ELGCSCCP
01423      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01424      MOVE ' FOR '                  TO TCAR-FROM-LINE              ELGCSCCP
01425          (TCAR-FROM-SUB).                                         ELGCSCCP
01426      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01427      MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX)                   ELGCSCCP
01428                                    TO TCAR-FROM-LINE              ELGCSCCP
01429          (TCAR-FROM-SUB).                                         ELGCSCCP
01430                                                                   ELGCSCCP
01431 ************************************************************      ELGCSCCP
01432 *                                                          *      ELGCSCCP
01433 *        GENERATE DEDUCTIBLE                               *      ELGCSCCP
01434 *                                                          *      ELGCSCCP
01435 ************************************************************      ELGCSCCP
01436  GENERATE-DEDUCTIBLE.                                             ELGCSCCP
01437      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = ZEROS OR              ELGCSCCP
01438                  SPACES OR LOW-VALUES                             ELGCSCCP
01439          CONTINUE                                                 ELGCSCCP
01440      ELSE IF ATBL-VALUE-LIMIT (ATBL-X-IDX) < ZERO AND             ELGCSCCP
01441                 GCG-DED-BASE-AMT-SOURCE-IND NOT = SPACES AND      ELGCSCCP
01442          ZEROS AND                                                ELGCSCCP
01443                 LOW-VALUES                                        ELGCSCCP
01444          PERFORM TRANSLATE-DEDUCTIBLE-BASE-AMOU                   ELGCSCCP
01445      ELSE IF ATBL-VALUE-LIMIT (ATBL-X-IDX) > ZEROS                ELGCSCCP
01446          PERFORM DISPLAY-DEDUCTIBLE-WITH-POSITI.                  ELGCSCCP
01447                                                                   ELGCSCCP
01448 ************************************************************      ELGCSCCP
01449 *                                                          *      ELGCSCCP
01450 *        TRANSLATE DEDUCTIBLE BASE AMOUNT INDICATOR        *      ELGCSCCP
01451 *                                                          *      ELGCSCCP
01452 ************************************************************      ELGCSCCP
01453  TRANSLATE-DEDUCTIBLE-BASE-AMOU.                                  ELGCSCCP
01454      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01455      MOVE PC-DETERMINED-BY         TO TCAR-FROM-LINE              ELGCSCCP
01456          (TCAR-FROM-SUB).                                         ELGCSCCP
01457      MOVE GCG-DED-BASE-AMT-SOURCE-IND                             ELGCSCCP
01458                                    TO CMF-CODE-VALUE.             ELGCSCCP
01459      MOVE 'GROUP'                  TO                             ELGCSCCP
01460          CMF-RECORD-PREFIX.                                       ELGCSCCP
01461      MOVE 'DED-BASE-AMT-SOURCE-IND'                               ELGCSCCP
01462                                    TO                             ELGCSCCP
01463          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSCCP
01464      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01465      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01466      MOVE CMF-DESCR-LINE (1)       TO TCAR-FROM-LINE              ELGCSCCP
01467          (TCAR-FROM-SUB).                                         ELGCSCCP
01468      PERFORM INSERT-ACCUM-COST-CONTAINMENTX.                      ELGCSCCP
01469      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01470      MOVE '.'                      TO TCAR-FROM-LINE              ELGCSCCP
01471          (TCAR-FROM-SUB).                                         ELGCSCCP
01472      SET ACCUM-FOUND TO TRUE.                                     ELGCSCCP
01473                                                                   ELGCSCCP
01474 ************************************************************      ELGCSCCP
01475 *                                                          *      ELGCSCCP
01476 *        DISPLAY DEDUCTIBLE WITH POSITIVE VALUE            *      ELGCSCCP
01477 *                                                          *      ELGCSCCP
01478 ************************************************************      ELGCSCCP
01479  DISPLAY-DEDUCTIBLE-WITH-POSITI.                                  ELGCSCCP
01480      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = '5'                   ELGCSCCP
01481          PERFORM CHECK-DEDUCTIBLE-DOLLAR-FORMAT                   ELGCSCCP
01482      ELSE                                                         ELGCSCCP
01483          PERFORM INSERT-DEDUCTIBLE-WITHOUT-DOLL.                  ELGCSCCP
01484      MOVE PC-ADL                   TO                             ELGCSCCP
01485          CMF-RECORD-PREFIX.                                       ELGCSCCP
01486      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)                       ELGCSCCP
01487                                    TO CMF-CODE-VALUE.             ELGCSCCP
01488      MOVE 'DEDL-VALUE-QUALIFIER'   TO                             ELGCSCCP
01489          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSCCP
01490      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01491      MOVE CMF-DESCR-LINE (1)       TO WS-DEDUCT-QUAL.             ELGCSCCP
01492      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01493      MOVE WS-DEDUCTIBLE-PHRASE     TO TCAR-FROM-LINE              ELGCSCCP
01494          (TCAR-FROM-SUB).                                         ELGCSCCP
01495      IF ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX) NOT = SPACES        ELGCSCCP
01496          AND                                                      ELGCSCCP
01497                 ZEROS AND LOW-VALUES                              ELGCSCCP
01498          PERFORM INSERT-ADL-INTERNAL-DESCRIPTOR.                  ELGCSCCP
01499      PERFORM INSERT-ACCUM-COST-CONTAINMENTX.                      ELGCSCCP
01500      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01501      MOVE '.'                      TO TCAR-FROM-LINE              ELGCSCCP
01502          (TCAR-FROM-SUB).                                         ELGCSCCP
01503      SET ACCUM-FOUND TO TRUE.                                     ELGCSCCP
01504                                                                   ELGCSCCP
01505 ************************************************************      ELGCSCCP
01506 *                                                          *      ELGCSCCP
01507 *        CHECK DEDUCTIBLE DOLLAR FORMAT                    *      ELGCSCCP
01508 *                                                          *      ELGCSCCP
01509 ************************************************************      ELGCSCCP
01510  CHECK-DEDUCTIBLE-DOLLAR-FORMAT.                                  ELGCSCCP
01511      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)                           ELGCSCCP
01512                                    TO WS-DEDUCT-VALUE.            ELGCSCCP
01513      IF WS-DED-UNLIMITED                                          ELGCSCCP
01514         MOVE PC-UNLIMITED             TO WS-DED-LMT-Z.            ELGCSCCP
01515                                                                   ELGCSCCP
01516 ************************************************************      ELGCSCCP
01517 *                                                          *      ELGCSCCP
01518 *        INSERT DEDUCTIBLE WITHOUT DOLLAR FORMAT           *      ELGCSCCP
01519 *                                                          *      ELGCSCCP
01520 ************************************************************      ELGCSCCP
01521  INSERT-DEDUCTIBLE-WITHOUT-DOLL.                                  ELGCSCCP
01522      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)                           ELGCSCCP
01523                                    TO WS-DED-LMT-Y.               ELGCSCCP
01524                                                                   ELGCSCCP
01525 ************************************************************      ELGCSCCP
01526 *                                                          *      ELGCSCCP
01527 *        INSERT ADL INTERNAL DESCRIPTOR INTO COMPRESSION AR*      ELGCSCCP
01528 *                                                          *      ELGCSCCP
01529 ************************************************************      ELGCSCCP
01530  INSERT-ADL-INTERNAL-DESCRIPTOR.                                  ELGCSCCP
01531      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01532      MOVE ' FOR '                  TO TCAR-FROM-LINE              ELGCSCCP
01533          (TCAR-FROM-SUB).                                         ELGCSCCP
01534      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01535      MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX)                   ELGCSCCP
01536                                    TO TCAR-FROM-LINE              ELGCSCCP
01537          (TCAR-FROM-SUB).                                         ELGCSCCP
01538                                                                   ELGCSCCP
01539 ************************************************************      ELGCSCCP
01540 *                                                          *      ELGCSCCP
01541 *        GENERATE MAXIMUM                                  *      ELGCSCCP
01542 *                                                          *      ELGCSCCP
01543 ************************************************************      ELGCSCCP
01544  GENERATE-MAXIMUM.                                                ELGCSCCP
01545      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX) = ZEROS OR              ELGCSCCP
01546                  SPACES OR LOW-VALUES                             ELGCSCCP
01547               OR ATBL-VALUE-LIMIT (ATBL-X-IDX) NOT >              ELGCSCCP
01548          ZERO                                                     ELGCSCCP
01549          CONTINUE                                                 ELGCSCCP
01550      ELSE                                                         ELGCSCCP
01551          PERFORM GENERATE-MAXIMUM-2.                              ELGCSCCP
01552                                                                   ELGCSCCP
01553 ************************************************************      ELGCSCCP
01554 *                                                          *      ELGCSCCP
01555 *        GENERATE MAXIMUM 2                                *      ELGCSCCP
01556 *                                                          *      ELGCSCCP
01557 ************************************************************      ELGCSCCP
01558  GENERATE-MAXIMUM-2.                                              ELGCSCCP
01559      IF ATBL-VALUE-QUALIFIER (ATBL-X-IDX)     = '5'               ELGCSCCP
01560          PERFORM CHECK-MAXIMUM-DOLLAR-FORMAT                      ELGCSCCP
01561      ELSE                                                         ELGCSCCP
01562          PERFORM INSERT-MAXIMUM-WITHOUT-DOLLARX.                  ELGCSCCP
01563      MOVE PC-ABM                   TO                             ELGCSCCP
01564          CMF-RECORD-PREFIX.                                       ELGCSCCP
01565      MOVE ATBL-VALUE-QUALIFIER (ATBL-X-IDX)                       ELGCSCCP
01566                                    TO CMF-CODE-VALUE.             ELGCSCCP
01567      MOVE 'BAMA-VALUE-QUALIFIER'   TO                             ELGCSCCP
01568          CMF-ELEMENT-SYSTEM-NAME.                                 ELGCSCCP
01569      PERFORM CALL-CODES-MANUAL-INTERFACE.                         ELGCSCCP
01570      MOVE CMF-DESCR-LINE (1)       TO WS-MAX-QUAL.                ELGCSCCP
01571      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01572      MOVE WS-MAXIMUM-PHRASE        TO TCAR-FROM-LINE              ELGCSCCP
01573          (TCAR-FROM-SUB).                                         ELGCSCCP
01574      IF ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX) NOT = SPACES        ELGCSCCP
01575          AND                                                      ELGCSCCP
01576                 ZEROS AND LOW-VALUES                              ELGCSCCP
01577          PERFORM INSERT-ABM-INTERNAL-DESCRIPTOR.                  ELGCSCCP
01578      PERFORM INSERT-ACCUM-COST-CONTAINMENTX.                      ELGCSCCP
01579      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01580      MOVE '.'                      TO TCAR-FROM-LINE              ELGCSCCP
01581          (TCAR-FROM-SUB).                                         ELGCSCCP
01582      SET ACCUM-FOUND TO TRUE.                                     ELGCSCCP
01583                                                                   ELGCSCCP
01584 ************************************************************      ELGCSCCP
01585 *                                                          *      ELGCSCCP
01586 *        CHECK MAXIMUM DOLLAR FORMAT                       *      ELGCSCCP
01587 *                                                          *      ELGCSCCP
01588 ************************************************************      ELGCSCCP
01589  CHECK-MAXIMUM-DOLLAR-FORMAT.                                     ELGCSCCP
01590      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX)                           ELGCSCCP
01591                                    TO WS-MAX-VALUE.               ELGCSCCP
01592      IF WS-MAX-LMT                                                ELGCSCCP
01593         MOVE PC-UNLIMITED                     TO                  ELGCSCCP
01594             WS-VAL-LMT-B.                                         ELGCSCCP
01595                                                                   ELGCSCCP
01596 ************************************************************      ELGCSCCP
01597 *                                                          *      ELGCSCCP
01598 *        INSERT MAXIMUM WITHOUT DOLLAR FORMAT              *      ELGCSCCP
01599 *                                                          *      ELGCSCCP
01600 ************************************************************      ELGCSCCP
01601  INSERT-MAXIMUM-WITHOUT-DOLLARX.                                  ELGCSCCP
01602      MOVE ATBL-VALUE-LIMIT (ATBL-X-IDX) TO WS-VAL-LMT-A.          ELGCSCCP
01603                                                                   ELGCSCCP
01604 ************************************************************      ELGCSCCP
01605 *                                                          *      ELGCSCCP
01606 *        INSERT ABM INTERNAL DESCRIPTOR INTO COMPRESSION AR*      ELGCSCCP
01607 *                                                          *      ELGCSCCP
01608 ************************************************************      ELGCSCCP
01609  INSERT-ABM-INTERNAL-DESCRIPTOR.                                  ELGCSCCP
01610      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01611      MOVE ' FOR '                  TO TCAR-FROM-LINE              ELGCSCCP
01612          (TCAR-FROM-SUB).                                         ELGCSCCP
01613      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01614      MOVE ATBL-INTERNAL-DESCRIPTOR (ATBL-X-IDX)                   ELGCSCCP
01615                                    TO TCAR-FROM-LINE              ELGCSCCP
01616          (TCAR-FROM-SUB).                                         ELGCSCCP
01617                                                                   ELGCSCCP
01618 ************************************************************      ELGCSCCP
01619 *                                                          *      ELGCSCCP
01620 *        INSERT ACCUM COST CONTAINMENT INDICATOR TEXT      *      ELGCSCCP
01621 *                                                          *      ELGCSCCP
01622 ************************************************************      ELGCSCCP
01623  INSERT-ACCUM-COST-CONTAINMENTX.                                  ELGCSCCP
01624      ADD +1                        TO TCAR-FROM-SUB.              ELGCSCCP
01625      IF CC-CODE-1                                                 ELGCSCCP
01626         MOVE ' REQUIREMENTS MET '    TO TCAR-FROM-LINE            ELGCSCCP
01627             (TCAR-FROM-SUB)                                       ELGCSCCP
01628      ELSE IF CC-CODE-2                                            ELGCSCCP
01629         MOVE ' NON COMPLIANCE '      TO TCAR-FROM-LINE            ELGCSCCP
01630             (TCAR-FROM-SUB)                                       ELGCSCCP
01631      ELSE IF CC-CODE-3                                            ELGCSCCP
01632         MOVE ' NON APPROVAL '        TO TCAR-FROM-LINE            ELGCSCCP
01633             (TCAR-FROM-SUB)                                       ELGCSCCP
01634      ELSE IF CC-CODE-4                                            ELGCSCCP
01635         MOVE PC-CODE-FOUR-TEXT       TO TCAR-FROM-LINE            ELGCSCCP
01636             (TCAR-FROM-SUB).                                      ELGCSCCP
01637                                                                   ELGCSCCP
01638 ************************************************************      ELGCSCCP
01639 *                                                          *      ELGCSCCP
01640 *        COMPRESS AND MOVE INTO WS OUTPUT AREA             *      ELGCSCCP
01641 *                                                          *      ELGCSCCP
01642 ************************************************************      ELGCSCCP
01643  COMPRESS-AND-MOVE-INTO-WS-OUTP.                                  ELGCSCCP
01644      PERFORM CALL-TEXT-COMPRESSION-MODULE.                        ELGCSCCP
01645      PERFORM SETUP-FOR-TRANSLATION-UNSTRING.                      ELGCSCCP
01646      PERFORM CALL-TEXT-UNSTRING-MODULE.                           ELGCSCCP
01647      PERFORM MOVE-FORMATTED-TEXT-TO-WS-OUTP                       ELGCSCCP
01648          VARYING TCAR-X FROM +1 BY +1                             ELGCSCCP
01649                  UNTIL   TCAR-X > TCAR-OUTPUT-FIELDS-USED.        ELGCSCCP
01650      PERFORM INITIALIZE-TEXT-COMPRESSION-AR.                      ELGCSCCP
01651                                                                   ELGCSCCP
01652 ************************************************************      ELGCSCCP
01653 *                                                          *      ELGCSCCP
01654 *        SIGNAL PROGRAM LOGIC ERROR                        *      ELGCSCCP
01655 *                                                          *      ELGCSCCP
01656 ************************************************************      ELGCSCCP
01657  SIGNAL-PROGRAM-LOGIC-ERROR.                                      ELGCSCCP
01658      SET CIA-AB-PGM-LOGIC TO TRUE.                                ELGCSCCP
01659      PERFORM SIGNAL-ABEND.                                        ELGCSCCP
