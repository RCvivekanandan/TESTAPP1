00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELTHEARC.                                         ELTHEARC
00003  AUTHOR.        ALIDA JATICH OF T. M. FLOYD, INC.                    LV001
00004  DATE-WRITTEN.  07/22/86                                          ELTHEARC
00005  DATE-COMPILED.                                                   ELTHEARC
00006                                                                   ELTHEARC
00007 ******************************************************************ELTHEARC
00008 **  ENGLISH LANGUAGE SUPPORT SYSTEM - ENGLISH CONTRACT INQUIRY  **ELTHEARC
00009 **  HEARING CARE TOPIC                                          **ELTHEARC
00010 **                                                              **ELTHEARC
00011 ******************************************************************ELTHEARC
00012 *                                                                *ELTHEARC
00013 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTHEARC
00014 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELTHEARC
00015 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTHEARC
00016 *                                                                *ELTHEARC
00017 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELTHEARC
00018 *                                                                *ELTHEARC
00019 *    XXXX    07/22/86  AMJ  ORIGINAL VERSION                     *ELTHEARC
00020 *                                                                *ELTHEARC
00021 *   T0723    08/14/86  LET  USING THE 1ST HEADER LINE FROM THE   *ELTHEARC
00022 *                           PROLOG FOR THE TOPIC SCREENS.        *ELTHEARC
00023 *                                                                *ELTHEARC
00024 *    XXXX    10/09/86  JTC  VS COBOL II CONVERSION               *ELTHEARC
00025 *                                                                *ELTHEARC
00026 *    XXXX    11/26/86  LET  CHANGED CODE TO ACCOMMODATE THE      *ELTHEARC
00027 *                           MOVING OF THE CERTIFICATION REQUIRE- *ELTHEARC
00028 *                           MENT INDICATOR FROM THE TYPE FORMAT  *ELTHEARC
00029 *                           SECTION TO THE COMMON SECTION.       *ELTHEARC
00030 *                                                                *ELTHEARC
00031 *            10/20/87  AKK  CHANGED 'THIS GROUP OF BENEFITS ARE  *ELTHEARC
00032 *                           HANDLED AS FOLLOWS' TO 'COVERED      *ELTHEARC
00033 *                           SERVICES ARE'.                       *ELTHEARC
00034 *            11/16/88  NAC  INCLUDE ADDITIONAL BENEFIT PROVISONS;*ELTHEARC
00035 *                           INCLUDE STORAGE ENHANCEMENTS.        *ELTHEARC
00036 *                                                                *ELTHEARC
00037 *    VVVV    10/23/89  RKH  ADDED TREATMENT RESTRICTION IND      *ELTHEARC
00038 *                                                                *ELTHEARC
00039 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY INELTHEARC
00040 *                      FROM A SINGLE POSITION TO ZEROS           *ELTHEARC
00041 *                      (FIELD IS CURRENTLY TWO POSITIONS)        *ELTHEARC
00042 *                                                                *ELTHEARC
00043 * XXXXX 08/30/91  JPB  CHANGED TRANSLATION OF EXCP-SCHED-ID TO   *ELTHEARC
00044 *                      SHOWING ITS CODE VALUE                    *ELTHEARC
00045 *                                                                *ELTHEARC
00046 ******************************************************************ELTHEARC
00047                                                                   ELTHEARC
00048  ENVIRONMENT DIVISION.                                            ELTHEARC
00049                                                                   ELTHEARC
00050  DATA DIVISION.                                                   ELTHEARC
00051  WORKING-STORAGE SECTION.                                         ELTHEARC
00052  01  WS-BEGIN                    PIC X(24) VALUE                  ELTHEARC
00053          '** ELTHEARC WS BEGINS **'.                              ELTHEARC
00054 /                                                                 ELTHEARC
00055 ****************************************************************  ELTHEARC
00056 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTHEARC
00057 ****************************************************************  ELTHEARC
00058  01  WORK-FIELDS.                                                 ELTHEARC
00059      05  WS-CIA-PNTR             PIC S9(8) COMP  VALUE ZEROS.     ELTHEARC
00060      05  WS-GRPSP-RECORD-PNTR    PIC S9(8) COMP  VALUE ZEROS.     ELTHEARC
00061      05  WS-SUB                  PIC S999 COMP-3 VALUE +0.        ELTHEARC
00062      05  WS-SUB1                 PIC S999 COMP-3 VALUE +0.        ELTHEARC
00063      05  WS-SUB2                 PIC S999 COMP-3 VALUE +0.        ELTHEARC
00064      05  WS-SUB3                 PIC S999 COMP-3 VALUE +0.        ELTHEARC
00065      05  WS-CIA                  PIC S999 COMP-3 VALUE +0.        ELTHEARC
00066      05  WS-TEMP-NOT-USED-CNT    PIC S999 COMP-3.                 ELTHEARC
00067      05  WS-REC-LEN              PIC S9(4) COMP  VALUE +0.        ELTHEARC
00068      05  WS-PERCENT-FLD.                                          ELTHEARC
00069        10  WS-PERCENTAGE         PIC ZZ9.                         ELTHEARC
00070        10  WS-PERCENT-SIGN       PIC X.                           ELTHEARC
00071      05  WS-EXPLANATION-IND      PIC S9 COMP-3.                   ELTHEARC
00072          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTHEARC
00073          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTHEARC
00074          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTHEARC
00075          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTHEARC
00076          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTHEARC
00077          88  WS-NO-EXPLANATION             VALUE +0.              ELTHEARC
00078      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP-3.                   ELTHEARC
00079      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP-3.                   ELTHEARC
00080                                                                   ELTHEARC
00081  01  WS-EXPLAINS.                                                 ELTHEARC
00082    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTHEARC
00083    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTHEARC
00084    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTHEARC
00085    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTHEARC
00086                                                                   ELTHEARC
00087  01  SWITCHES.                                                    ELTHEARC
00088      05  WS-FIRSTTIME-IND        PIC X.                           ELTHEARC
00089          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTHEARC
00090      05  WS-ADD-A-BLANK-IND      PIC X.                           ELTHEARC
00091          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTHEARC
00092      05  WS-MOVE-LINES-IND       PIC X      VALUE 'Y'.            ELTHEARC
00093          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTHEARC
00094      05  WS-SAME-PROV-LINE-SW    PIC X      VALUE 'N'.            ELTHEARC
00095          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTHEARC
00096                                                                   ELTHEARC
00097 *--------------------------------------------------------------*  ELTHEARC
00098 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTHEARC
00099 *--------------------------------------------------------------*  ELTHEARC
00100  01  WS-BEN-PROV-IDS.                                             ELTHEARC
00101      05  WS-TABLE-MAX-CNT        PIC S9(4) VALUE +9 COMP.         ELTHEARC
00102      05  WS-INST-IP-CNT          PIC S9(4) VALUE +3 COMP.         ELTHEARC
00103      05  WS-INST-IP-TABS.                                         ELTHEARC
00104          10  FILLER              PIC X(6) VALUE 'HAID B'.         ELTHEARC
00105          10  FILLER              PIC X(6) VALUE 'HTEI B'.         ELTHEARC
00106          10  FILLER              PIC X(6) VALUE 'HTI  B'.         ELTHEARC
00107      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTHEARC
00108                                  PIC X(6) OCCURS 3 TIMES.         ELTHEARC
00109                                                                   ELTHEARC
00110      05  WS-INST-OP-CNT          PIC S9(4) VALUE +3 COMP.         ELTHEARC
00111      05  WS-INST-OP-TABS.                                         ELTHEARC
00112          10  FILLER              PIC X(6) VALUE 'HAID B'.         ELTHEARC
00113          10  FILLER              PIC X(6) VALUE 'HTEO B'.         ELTHEARC
00114          10  FILLER              PIC X(6) VALUE 'HTO  B'.         ELTHEARC
00115      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTHEARC
00116                                  PIC X(6) OCCURS 3 TIMES.         ELTHEARC
00117                                                                   ELTHEARC
00118      05  WS-PROF-IP-CNT          PIC S9(4) VALUE +9 COMP.         ELTHEARC
00119      05  WS-PROF-IP-TABS.                                         ELTHEARC
00120          10  FILLER              PIC X(6) VALUE 'HAAC E'.         ELTHEARC
00121          10  FILLER              PIC X(6) VALUE 'HADF E'.         ELTHEARC
00122          10  FILLER              PIC X(6) VALUE 'HAEV E'.         ELTHEARC
00123          10  FILLER              PIC X(6) VALUE 'HAID E'.         ELTHEARC
00124          10  FILLER              PIC X(6) VALUE 'HCPI E'.         ELTHEARC
00125          10  FILLER              PIC X(6) VALUE 'HEAR E'.         ELTHEARC
00126          10  FILLER              PIC X(6) VALUE 'HECO E'.         ELTHEARC
00127          10  FILLER              PIC X(6) VALUE 'HTEI E'.         ELTHEARC
00128          10  FILLER              PIC X(6) VALUE 'HTI  E'.         ELTHEARC
00129      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTHEARC
00130                                  PIC X(6) OCCURS 9 TIMES.         ELTHEARC
00131                                                                   ELTHEARC
00132      05  WS-PROF-OP-CNT          PIC S9(4)  VALUE +9 COMP.        ELTHEARC
00133      05  WS-PROF-OP-TABS.                                         ELTHEARC
00134          10  FILLER              PIC X(6) VALUE 'HAAC E'.         ELTHEARC
00135          10  FILLER              PIC X(6) VALUE 'HADF E'.         ELTHEARC
00136          10  FILLER              PIC X(6) VALUE 'HAEV E'.         ELTHEARC
00137          10  FILLER              PIC X(6) VALUE 'HAID E'.         ELTHEARC
00138          10  FILLER              PIC X(6) VALUE 'HCPO E'.         ELTHEARC
00139          10  FILLER              PIC X(6) VALUE 'HEAR E'.         ELTHEARC
00140          10  FILLER              PIC X(6) VALUE 'HECO E'.         ELTHEARC
00141          10  FILLER              PIC X(6) VALUE 'HTEO E'.         ELTHEARC
00142          10  FILLER              PIC X(6) VALUE 'HTO  E'.         ELTHEARC
00143      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTHEARC
00144                                  PIC X(6) OCCURS 9 TIMES.         ELTHEARC
00145 /                                                                 ELTHEARC
00146 ****************************************************************  ELTHEARC
00147 *              HEADER AND LITERAL TEXT AREA                    *  ELTHEARC
00148 ****************************************************************  ELTHEARC
00149  01  HEADER-LINE-2.                                               ELTHEARC
00150      05  FILLER                  PIC X(12) VALUE 'SECTION NO: '.  ELTHEARC
00151      05  WS-SECT-NO              PIC 9(5) VALUE ZEROS.            ELTHEARC
00152      05  FILLER                  PIC X(23) VALUE                  ELTHEARC
00153              '       EFFECTIVE DATE: '.                           ELTHEARC
00154      05  WS-EFF-DATE             PIC 99/99/99.                    ELTHEARC
00155      05  FILLER                  PIC X(29) VALUE                  ELTHEARC
00156              '        FAMILY RELATIONSHIP: '.                     ELTHEARC
00157      05  WS-FAM-REL              PIC 9(1) VALUE ZERO.             ELTHEARC
00158      05  FILLER                  PIC X VALUE LOW-VALUE.           ELTHEARC
00159                                                                   ELTHEARC
00160  01  HEADER-I-IP-LINE-3.                                          ELTHEARC
00161      05  FILLER                  PIC X(17) VALUE SPACES.          ELTHEARC
00162      05  FILLER                  PIC X(45) VALUE                  ELTHEARC
00163              'HEARING CARE SERVICES INSTITUTIONAL INPATIENT'.     ELTHEARC
00164      05  FILLER                  PIC X(17) VALUE LOW-VALUES.      ELTHEARC
00165                                                                   ELTHEARC
00166  01  HEADER-I-OP-LINE-3.                                          ELTHEARC
00167      05  FILLER                  PIC X(16) VALUE SPACES.          ELTHEARC
00168      05  FILLER                  PIC X(46) VALUE                  ELTHEARC
00169              'HEARING CARE SERVICES INSTITUTIONAL OUTPATIENT'.    ELTHEARC
00170      05  FILLER                  PIC X(17) VALUE LOW-VALUES.      ELTHEARC
00171                                                                   ELTHEARC
00172  01  HEADER-P-IP-LINE-3.                                          ELTHEARC
00173      05  FILLER                  PIC X(17) VALUE SPACES.          ELTHEARC
00174      05  FILLER                  PIC X(44) VALUE                  ELTHEARC
00175              'HEARING CARE SERVICES PROFESSIONAL INPATIENT'.      ELTHEARC
00176      05  FILLER                  PIC X(18) VALUE LOW-VALUES.      ELTHEARC
00177                                                                   ELTHEARC
00178  01  HEADER-P-OP-LINE-3.                                          ELTHEARC
00179      05  FILLER                  PIC X(17) VALUE SPACES.          ELTHEARC
00180      05  FILLER                  PIC X(45) VALUE                  ELTHEARC
00181              'HEARING CARE SERVICES PROFESSIONAL OUTPATIENT'.     ELTHEARC
00182      05  FILLER                  PIC X(17) VALUE LOW-VALUES.      ELTHEARC
00183                                                                   ELTHEARC
00184  01  WS-SERVICES-RENDERED.                                        ELTHEARC
00185      05  FILLER                  PIC X(26) VALUE                  ELTHEARC
00186              'SERVICES MAY BE RENDERED: '.                        ELTHEARC
00187      05  FILLER                  PIC X(53) VALUE LOW-VALUES.      ELTHEARC
00188                                                                   ELTHEARC
00189  01  WS-FOLLOWING-BEN.                                            ELTHEARC
00190      05  FILLER                    PIC X(79) VALUE                ELTHEARC
00191          'COVERED SERVICES ARE:  '.                               ELTHEARC
00192                                                                   ELTHEARC
00193  01  WS-PAY-CONSDR-TEXT1.                                         ELTHEARC
00194      05  FILLER                    PIC X(45)                      ELTHEARC
00195        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTHEARC
00196                                                                   ELTHEARC
00197  01  WS-PAY-CONSDR-TEXT2.                                         ELTHEARC
00198      05  FILLER                    PIC X(44)                      ELTHEARC
00199        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTHEARC
00200                                                                   ELTHEARC
00201  01  WS-PAYABLE-AS.                                               ELTHEARC
00202      05  FILLER                    PIC X(45) VALUE                ELTHEARC
00203          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTHEARC
00204      05  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTHEARC
00205                                                                   ELTHEARC
00206  01  WS-CONTRACT-RELATED.                                         ELTHEARC
00207      05  FILLER                  PIC X(48) VALUE                  ELTHEARC
00208              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTHEARC
00209                                                                   ELTHEARC
00210  01  WS-CERT-REQ.                                                 ELTHEARC
00211      05  FILLER                   PIC X(47) VALUE                 ELTHEARC
00212              'THE CERIFICATION REQUIRED FOR THIS SERVICE IS: '.   ELTHEARC
00213      05  FILLER                   PIC X(32) VALUE LOW-VALUES.     ELTHEARC
00214                                                                   ELTHEARC
00215  01  WS-EXCEPTION-SCHED.                                          ELTHEARC
00216      05  FILLER                  PIC X(49) VALUE                  ELTHEARC
00217              'BENEFITS ARE PRICED BASED ON EXCEPTION SCHEDULE: '. ELTHEARC
00218      05  FILLER                  PIC X(30) VALUE LOW-VALUES.      ELTHEARC
00219                                                                   ELTHEARC
00220  01  WS-RECERT-REQ.                                               ELTHEARC
00221      05  FILLER                   PIC X(56) VALUE                 ELTHEARC
00222              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTHEARC
00223 -            'IS: '.                                              ELTHEARC
00224      05  FILLER                   PIC X(23) VALUE LOW-VALUES.     ELTHEARC
00225                                                                   ELTHEARC
00226  01  WS-RELATED-MED-COND-1.                                       ELTHEARC
00227      05  FILLER                   PIC X(79) VALUE                 ELTHEARC
00228              'BENEFIT ELIGIBILITY REQUIRES THAT THIS SERVICE BE INELTHEARC
00229 -            ' CONJUCTION WITH A RELATED '.                       ELTHEARC
00230                                                                   ELTHEARC
00231  01  WS-RELATED-MED-COND-2.                                       ELTHEARC
00232      05  FILLER                   PIC X(18) VALUE                 ELTHEARC
00233              'MEDICAL CONDITION.'.                                ELTHEARC
00234      05  FILLER                   PIC X(61) VALUE LOW-VALUES.     ELTHEARC
00235                                                                   ELTHEARC
00236  01  WS-BASIC.                                                    ELTHEARC
00237      05  WS-BASIC-LIT            PIC X(16) VALUE                  ELTHEARC
00238              '         BASIC: '.                                  ELTHEARC
00239      05  WS-DTL-BASIC-LONG.                                       ELTHEARC
00240          15  WS-DTL-BASIC        PIC X(50) VALUE SPACES.          ELTHEARC
00241          15  FILLER              PIC X(13) VALUE LOW-VALUES.      ELTHEARC
00242                                                                   ELTHEARC
00243  01  WS-SUPPLEMENTAL.                                             ELTHEARC
00244      05  WS-SUPP-LIT             PIC X(16) VALUE                  ELTHEARC
00245              '  SUPPLEMENTAL: '.                                  ELTHEARC
00246      05  WS-DTL-SUPP-LONG.                                        ELTHEARC
00247          15  WS-DTL-SUPPLEMENTAL PIC X(50) VALUE SPACES.          ELTHEARC
00248          15  FILLER              PIC X(13) VALUE LOW-VALUES.      ELTHEARC
00249                                                                   ELTHEARC
00250  01  WS-PVE.                                                      ELTHEARC
00251      05  FILLER                  PIC X(44) VALUE                  ELTHEARC
00252              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTHEARC
00253      05  FILLER                  PIC X(35) VALUE LOW-VALUES.      ELTHEARC
00254                                                                   ELTHEARC
00255  01  WS-INDICES-PROBLEM.                                          ELTHEARC
00256      05  FILLER                  PIC X(20) VALUE                  ELTHEARC
00257              'PROBLEM WITH INDICES'.                              ELTHEARC
00258      05  FILLER                  PIC X(59) VALUE LOW-VALUES.      ELTHEARC
00259                                                                   ELTHEARC
00260  01  WS-POSSIBLE-ERROR.                                           ELTHEARC
00261      05  FILLER                   PIC X(50) VALUE                 ELTHEARC
00262              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTHEARC
00263      05  FILLER                   PIC X(29) VALUE LOW-VALUES.     ELTHEARC
00264                                                                   ELTHEARC
00265  01  WS-INVALID-REQ.                                              ELTHEARC
00266      05  FILLER                  PIC X(37) VALUE                  ELTHEARC
00267              '*** I N V A L I D   R E Q U E S T ***'.             ELTHEARC
00268      05  FILLER                  PIC X(42) VALUE LOW-VALUES.      ELTHEARC
00269                                                                   ELTHEARC
00270  01  WS-SPILLOVER.                                                ELTHEARC
00271      05  FILLER                  PIC X(10) VALUE                  ELTHEARC
00272              'SPILLOVER '.                                        ELTHEARC
00273                                                                   ELTHEARC
00274  01  WS-OTHER-LITERALS.                                           ELTHEARC
00275    05  WS-NO-TABULAR1.                                            ELTHEARC
00276      10  FILLER                    PIC X(51)  VALUE               ELTHEARC
00277         '*** FOUND A GENERIC CONTRACT FILE INCONSISTENCY IN '.    ELTHEARC
00278      10  FILLER                    PIC X(22)  VALUE               ELTHEARC
00279         'GOING FROM BENEFIT ***'.                                 ELTHEARC
00280                                                                   ELTHEARC
00281    05  WS-NO-TABULAR2.                                            ELTHEARC
00282      10  FILLER                    PIC X(15)  VALUE               ELTHEARC
00283         '*** PROVISION: '.                                        ELTHEARC
00284      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTHEARC
00285      10  FILLER                    PIC X VALUE SPACE.             ELTHEARC
00286      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTHEARC
00287      10  FILLER                    PIC X(13)  VALUE               ELTHEARC
00288         ' TO TABULAR: '.                                          ELTHEARC
00289      10  WS-NO-TAB-ID              PIC X(6).                      ELTHEARC
00290      10  FILLER                    PIC X VALUE SPACE.             ELTHEARC
00291      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTHEARC
00292      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTHEARC
00293                                                                   ELTHEARC
00294    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTHEARC
00295    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTHEARC
00296      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTHEARC
00297                                                                   ELTHEARC
00298 /                                                                 ELTHEARC
00299  LINKAGE SECTION.                                                 ELTHEARC
00300  01  DFHCOMMAREA.                                                 ELTHEARC
00301      COPY ELSCOMMC.                                               ELTHEARC
00302 /  *** CIA  AREA ***                                              ELTHEARC
00303      COPY ELSCIA2C.                                               ELTHEARC
00304 /  *** IO PARM AREA ***                                           ELTHEARC
00305      COPY ELSIOPMC.                                               ELTHEARC
00306 /  *** KEY AREA ***                                               ELTHEARC
00307      COPY ELSKEYSC.                                               ELTHEARC
00308 /  *** OUTPUT TEXT AREA ***                                       ELTHEARC
00309      COPY ELSOUTPC.                                               ELTHEARC
00310 /  *** TOPIC SELECTION AREA ***                                   ELTHEARC
00311      COPY ELSSSCBC.                                               ELTHEARC
00312 /  *** CODE MANUAL INTERFACE ***                                  ELTHEARC
00313      COPY ELSCMIFC.                                               ELTHEARC
00314 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTHEARC
00315      COPY ELSCMDSC.                                               ELTHEARC
00316 /  *** BENEFIT PROVISION TABLE ***                                ELTHEARC
00317      COPY ELSPRVNC.                                               ELTHEARC
00318 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTHEARC
00319      COPY ELSTCWAC.                                               ELTHEARC
00320 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTHEARC
00321      COPY ELSPLGSW.                                               ELTHEARC
00322                                                                   ELTHEARC
00323 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTHEARC
00324      COPY ELSPLGTB.                                               ELTHEARC
00325 /                                                                 ELTHEARC
00326  PROCEDURE DIVISION.                                              ELTHEARC
00327  0000-MAINLINE.                                                   ELTHEARC
00328                                                                   ELTHEARC
00329      PERFORM 1000-INITIALIZATION                                  ELTHEARC
00330         THRU 1000-EXIT.                                           ELTHEARC
00331                                                                   ELTHEARC
00332      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTHEARC
00333                       AND                                         ELTHEARC
00334         (SSB-SERV-CLASS-IP OR SSB-SERV-CLASS-BOTH)                ELTHEARC
00335          PERFORM 2000-INSTITUTIONAL-IP                            ELTHEARC
00336             THRU 2000-EXIT.                                       ELTHEARC
00337                                                                   ELTHEARC
00338      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTHEARC
00339                       AND                                         ELTHEARC
00340         (SSB-SERV-CLASS-OP OR SSB-SERV-CLASS-BOTH)                ELTHEARC
00341          PERFORM 3000-INSTITUTIONAL-OP                            ELTHEARC
00342             THRU 3000-EXIT.                                       ELTHEARC
00343                                                                   ELTHEARC
00344      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTHEARC
00345                       AND                                         ELTHEARC
00346         (SSB-SERV-CLASS-IP OR SSB-SERV-CLASS-BOTH)                ELTHEARC
00347          PERFORM 4000-PROFESSIONAL-IP                             ELTHEARC
00348             THRU 4000-EXIT.                                       ELTHEARC
00349                                                                   ELTHEARC
00350      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTHEARC
00351                       AND                                         ELTHEARC
00352         (SSB-SERV-CLASS-OP OR SSB-SERV-CLASS-BOTH)                ELTHEARC
00353          PERFORM 5000-PROFESSIONAL-OP                             ELTHEARC
00354             THRU 5000-EXIT.                                       ELTHEARC
00355                                                                   ELTHEARC
00356      IF NOT SSB-PROV-CLASS-INST AND                               ELTHEARC
00357        NOT SSB-PROV-CLASS-PROF    AND                             ELTHEARC
00358        NOT SSB-PROV-CLASS-BOTH                                    ELTHEARC
00359          MOVE ' '           TO COF-FUNCTION                       ELTHEARC
00360          MOVE +0            TO COF-NBR-HDR-LINES                  ELTHEARC
00361          MOVE +2            TO COF-NBR-DTL-LINES                  ELTHEARC
00362          MOVE WS-INVALID-REQ TO COF-DTL-LINE (2)                  ELTHEARC
00363          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTHEARC
00364                           COMMAREA(DFHCOMMAREA)                   ELTHEARC
00365                           END-EXEC.                               ELTHEARC
00366                                                                   ELTHEARC
00367      IF NOT SSB-SERV-CLASS-IP AND                                 ELTHEARC
00368         NOT SSB-SERV-CLASS-OP AND                                 ELTHEARC
00369         NOT SSB-SERV-CLASS-BOTH                                   ELTHEARC
00370          MOVE ' '           TO COF-FUNCTION                       ELTHEARC
00371          MOVE +0            TO COF-NBR-HDR-LINES                  ELTHEARC
00372          MOVE +2            TO COF-NBR-DTL-LINES                  ELTHEARC
00373          MOVE WS-INVALID-REQ TO COF-DTL-LINE (2)                  ELTHEARC
00374          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTHEARC
00375                           COMMAREA(DFHCOMMAREA)                   ELTHEARC
00376                           END-EXEC.                               ELTHEARC
00377                                                                   ELTHEARC
00378      MOVE 'E' TO COF-FUNCTION.                                    ELTHEARC
00379      MOVE ZERO TO COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.           ELTHEARC
00380                                                                   ELTHEARC
00381      EXEC CICS LINK                                               ELTHEARC
00382           PROGRAM ('ELUOUTPT')                                    ELTHEARC
00383           COMMAREA (DFHCOMMAREA)                                  ELTHEARC
00384           END-EXEC.                                               ELTHEARC
00385                                                                   ELTHEARC
00386      EXEC CICS RETURN                                             ELTHEARC
00387           END-EXEC.                                               ELTHEARC
00388                                                                   ELTHEARC
00389      GOBACK.                                                      ELTHEARC
00390 /                                                                 ELTHEARC
00391  0098-SIGNAL-UNALL-AREA-ERROR.                                    ELTHEARC
00392      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTHEARC
00393      EXEC CICS ABEND                                              ELTHEARC
00394                ABCODE (CIA-ABCODE)                                ELTHEARC
00395      END-EXEC.                                                    ELTHEARC
00396                                                                   ELTHEARC
00397  1000-INITIALIZATION.                                             ELTHEARC
00398 ****************************************************************  ELTHEARC
00399 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTHEARC
00400 ****************************************************************  ELTHEARC
00401                                                                   ELTHEARC
00402      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTHEARC
00403          EXEC CICS ABEND                                          ELTHEARC
00404                    ABCODE ('EL01')                                ELTHEARC
00405          END-EXEC                                                 ELTHEARC
00406      END-IF.                                                      ELTHEARC
00407                                                                   ELTHEARC
00408      IF ECA-CIA-PTR = NULL                                        ELTHEARC
00409          EXEC CICS ABEND                                          ELTHEARC
00410                    ABCODE ('EL02')                                ELTHEARC
00411          END-EXEC                                                 ELTHEARC
00412      END-IF.                                                      ELTHEARC
00413                                                                   ELTHEARC
00414      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTHEARC
00415                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELTHEARC
00416                                                                   ELTHEARC
00417 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTHEARC
00418                                                                   ELTHEARC
00419      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTHEARC
00420      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00421                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTHEARC
00422      IF NOT CIA-RC-OK                                             ELTHEARC
00423          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00424                                                                   ELTHEARC
00425      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTHEARC
00426      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00427                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELTHEARC
00428      IF NOT CIA-RC-OK                                             ELTHEARC
00429          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00430                                                                   ELTHEARC
00431      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTHEARC
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00433                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELTHEARC
00434      IF NOT CIA-RC-OK                                             ELTHEARC
00435          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00436                                                                   ELTHEARC
00437      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTHEARC
00438      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00439                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELTHEARC
00440      IF NOT CIA-RC-OK                                             ELTHEARC
00441          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00442                                                                   ELTHEARC
00443      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTHEARC
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00445                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELTHEARC
00446      IF NOT CIA-RC-OK                                             ELTHEARC
00447          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00448                                                                   ELTHEARC
00449      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTHEARC
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00451                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELTHEARC
00452      IF NOT CIA-RC-OK                                             ELTHEARC
00453          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTHEARC
00454                                                                   ELTHEARC
00455      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTHEARC
00456      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTHEARC
00457              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTHEARC
00458                                                                   ELTHEARC
00459      SET CIA-STG-GETMAIN TO TRUE.                                 ELTHEARC
00460      EXEC CICS LINK                                               ELTHEARC
00461                PROGRAM('ELUSTGMG')                                ELTHEARC
00462                COMMAREA(DFHCOMMAREA)                              ELTHEARC
00463      END-EXEC.                                                    ELTHEARC
00464                                                                   ELTHEARC
00465      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTHEARC
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00467                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELTHEARC
00468                                                                   ELTHEARC
00469      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTHEARC
00470                                                                   ELTHEARC
00471  1000-EXIT.  EXIT.                                                ELTHEARC
00472 /                                                                 ELTHEARC
00473 ****************************************************************  ELTHEARC
00474 *       HEARING CARE INSTITUTIONAL INPATIENT PROCESSING        *  ELTHEARC
00475 ****************************************************************  ELTHEARC
00476  2000-INSTITUTIONAL-IP.                                           ELTHEARC
00477                                                                   ELTHEARC
00478      MOVE 'Y'   TO WS-FIRSTTIME-IND.                              ELTHEARC
00479      MOVE HEADER-I-IP-LINE-3 TO COF-HDR-LINE (2).                 ELTHEARC
00480                                                                   ELTHEARC
00481      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTHEARC
00482         THRU 9100-EXIT.                                           ELTHEARC
00483                                                                   ELTHEARC
00484      MOVE WS-INST-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTHEARC
00485                                                                   ELTHEARC
00486      PERFORM 2010-MOVE-IN-INST-IP-TABS                            ELTHEARC
00487         THRU 2010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
00488                        UNTIL   WS-SUB > WS-INST-IP-CNT.           ELTHEARC
00489                                                                   ELTHEARC
00490      PERFORM 2020-CALL-COVERAGE                                   ELTHEARC
00491         THRU 2020-EXIT.                                           ELTHEARC
00492                                                                   ELTHEARC
00493      IF PVN-COVG-NONE                                             ELTHEARC
00494          GO TO 2000-EXIT.                                         ELTHEARC
00495                                                                   ELTHEARC
00496      PERFORM 2030-FIND-FIRST-NONZERO                              ELTHEARC
00497         THRU 2030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
00498                        UNTIL   WS-SUB > WS-INST-IP-CNT.           ELTHEARC
00499                                                                   ELTHEARC
00500  2000-EXIT.  EXIT.                                                ELTHEARC
00501 /                                                                 ELTHEARC
00502  2010-MOVE-IN-INST-IP-TABS.                                       ELTHEARC
00503      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
00504      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTHEARC
00505              TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).             ELTHEARC
00506                                                                   ELTHEARC
00507      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),           ELTHEARC
00508                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTHEARC
00509                                                                   ELTHEARC
00510  2010-EXIT.  EXIT.                                                ELTHEARC
00511                                                                   ELTHEARC
00512  2020-CALL-COVERAGE.                                              ELTHEARC
00513                                                                   ELTHEARC
00514      MOVE 'HEARING CARE ' TO SSB-TOPIC-PHRASE.                    ELTHEARC
00515                                                                   ELTHEARC
00516      EXEC CICS LINK                                               ELTHEARC
00517           PROGRAM ('ELGCOVER')                                    ELTHEARC
00518           COMMAREA (DFHCOMMAREA)                                  ELTHEARC
00519           END-EXEC.                                               ELTHEARC
00520                                                                   ELTHEARC
00521      EXEC CICS LINK                                               ELTHEARC
00522           PROGRAM ('ELUOUTPT')                                    ELTHEARC
00523           COMMAREA (DFHCOMMAREA)                                  ELTHEARC
00524           END-EXEC.                                               ELTHEARC
00525                                                                   ELTHEARC
00526      IF PVN-COVG-NONE                                             ELTHEARC
00527          GO TO 2020-EXIT.                                         ELTHEARC
00528                                                                   ELTHEARC
00529      MOVE +1 TO WS-CIA.                                           ELTHEARC
00530                                                                   ELTHEARC
00531      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTHEARC
00532                                                                   ELTHEARC
00533      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTHEARC
00534                    PSP-PROVN-PRICING-METHD,                       ELTHEARC
00535                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTHEARC
00536                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTHEARC
00537                    PSP-TRANSF-OTHER-RESP-IND,                     ELTHEARC
00538                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTHEARC
00539                    PSP-SPILL-OVER-DED-APL-IND,                    ELTHEARC
00540                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTHEARC
00541                    PSP-CERTFN-REQRM-IND,                          ELTHEARC
00542                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTHEARC
00543                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTHEARC
00544                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTHEARC
00545                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTHEARC
00546                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTHEARC
00547                    PSP-BEN-TAB-PROVN-ID-PVE,                      ELTHEARC
00548                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTHEARC
00549                    PSB-CERTN-REPETN-REQRD-IND.                    ELTHEARC
00550                                                                   ELTHEARC
00551      EXEC CICS LINK                                               ELTHEARC
00552           PROGRAM ('ELUPLGRP')                                    ELTHEARC
00553           COMMAREA (DFHCOMMAREA)                                  ELTHEARC
00554           END-EXEC.                                               ELTHEARC
00555                                                                   ELTHEARC
00556      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTHEARC
00557      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
00558              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTHEARC
00559                                                                   ELTHEARC
00560                                                                   ELTHEARC
00561  2020-EXIT.  EXIT.                                                ELTHEARC
00562                                                                   ELTHEARC
00563  2030-FIND-FIRST-NONZERO.                                         ELTHEARC
00564      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
00565                                                                   ELTHEARC
00566      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTHEARC
00567          NEXT SENTENCE                                            ELTHEARC
00568      ELSE                                                         ELTHEARC
00569          PERFORM 2100-BUILD-SCREEN-LINES                          ELTHEARC
00570             THRU 2100-EXIT.                                       ELTHEARC
00571                                                                   ELTHEARC
00572  2030-EXIT.  EXIT.                                                ELTHEARC
00573 /                                                                 ELTHEARC
00574  2100-BUILD-SCREEN-LINES.                                         ELTHEARC
00575      SET PLT-INDEX1 TO                                            ELTHEARC
00576              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTHEARC
00577                                                                   ELTHEARC
00578      IF WS-NOT-FIRST-TIME                                         ELTHEARC
00579         MOVE 'P'    TO COF-FUNCTION                               ELTHEARC
00580         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTHEARC
00581         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
00582                          COMMAREA(DFHCOMMAREA)                    ELTHEARC
00583         END-EXEC                                                  ELTHEARC
00584      ELSE                                                         ELTHEARC
00585        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTHEARC
00586                                                                   ELTHEARC
00587      MOVE +1 TO WS-CIA.                                           ELTHEARC
00588                                                                   ELTHEARC
00589      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTHEARC
00590          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTHEARC
00591              SET PLT-INDEX2 TO 2                                  ELTHEARC
00592          ELSE                                                     ELTHEARC
00593              MOVE WS-INDICES-PROBLEM TO COF-DTL-LINE (1)          ELTHEARC
00594              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTHEARC
00595                 THRU 9200-EXIT                                    ELTHEARC
00596              GO TO 2100-EXIT                                      ELTHEARC
00597      ELSE                                                         ELTHEARC
00598          SET PLT-INDEX2 TO 1.                                     ELTHEARC
00599                                                                   ELTHEARC
00600      PERFORM 2105-LIST-BEN-PROV                                   ELTHEARC
00601         THRU 2105-EXIT.                                           ELTHEARC
00602                                                                   ELTHEARC
00603      PERFORM 2110-PLACE-OF-TREATMENT                              ELTHEARC
00604         THRU 2110-EXIT.                                           ELTHEARC
00605                                                                   ELTHEARC
00606      PERFORM 2120-PRIC-METH                                       ELTHEARC
00607         THRU 2120-EXIT.                                           ELTHEARC
00608                                                                   ELTHEARC
00609      PERFORM 2140-CERTIFICATION                                   ELTHEARC
00610         THRU 2140-EXIT.                                           ELTHEARC
00611                                                                   ELTHEARC
00612      PERFORM 2150-RECERTIFICATION                                 ELTHEARC
00613         THRU 2150-EXIT.                                           ELTHEARC
00614                                                                   ELTHEARC
00615      PERFORM 2155-SERVC-NEC                                       ELTHEARC
00616         THRU 2155-EXIT.                                           ELTHEARC
00617                                                                   ELTHEARC
00618      PERFORM 2160-SPILLOVR-COINS-N-DEDUC                          ELTHEARC
00619         THRU 2160-EXIT.                                           ELTHEARC
00620                                                                   ELTHEARC
00621      PERFORM 2170-AAR-PPF-PVE-TABS                                ELTHEARC
00622         THRU 2170-EXIT.                                           ELTHEARC
00623                                                                   ELTHEARC
00624      PERFORM 2180-ALL-LEVEL-TABS                                  ELTHEARC
00625         THRU 2180-EXIT.                                           ELTHEARC
00626                                                                   ELTHEARC
00627      PERFORM 6000-PAY-CONSID-TEXT THRU 6000-EXIT.                 ELTHEARC
00628                                                                   ELTHEARC
00629      PERFORM 6100-TRANSF-OTHER-RESP-IND  THRU 6100-EXIT.          ELTHEARC
00630                                                                   ELTHEARC
00631  2100-EXIT.  EXIT.                                                ELTHEARC
00632 /                                                                 ELTHEARC
00633  2105-LIST-BEN-PROV.                                              ELTHEARC
00634 ****************************************************************  ELTHEARC
00635 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTHEARC
00636 ****************************************************************  ELTHEARC
00637      MOVE +2              TO WS-CIA.                              ELTHEARC
00638      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE (WS-CIA).              ELTHEARC
00639      MOVE ZERO            TO WS-SUB2.                             ELTHEARC
00640      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTHEARC
00641                           TO WS-SUB3.                             ELTHEARC
00642                                                                   ELTHEARC
00643      PERFORM 2106-ZERO-ALL-WITH-SAME-NO                           ELTHEARC
00644         THRU 2106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTHEARC
00645                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.ELTHEARC
00646                                                                   ELTHEARC
00647      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
00648      MOVE WS-CIA  TO COF-NBR-DTL-LINES.                           ELTHEARC
00649                                                                   ELTHEARC
00650      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
00651                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
00652                     END-EXEC.                                     ELTHEARC
00653                                                                   ELTHEARC
00654  2105-EXIT.  EXIT.                                                ELTHEARC
00655                                                                   ELTHEARC
00656  2106-ZERO-ALL-WITH-SAME-NO.                                      ELTHEARC
00657                                                                   ELTHEARC
00658      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTHEARC
00659          MOVE 'BP'       TO CMF-RECORD-PREFIX                     ELTHEARC
00660          MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME              ELTHEARC
00661          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTHEARC
00662                          TO CMF-CODE-VALUE                        ELTHEARC
00663          MOVE SPACES     TO WS-TEMP-TEXT-AREA                     ELTHEARC
00664          MOVE +58        TO WS-TEMP-NOT-USED-CNT                  ELTHEARC
00665          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00666             THRU 9500-EXIT                                        ELTHEARC
00667          MOVE ZERO TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)        ELTHEARC
00668          ADD +1 TO WS-SUB2                                        ELTHEARC
00669          IF WS-CIA > 20 OR = 20                                   ELTHEARC
00670              MOVE WS-CIA TO COF-NBR-DTL-LINES                     ELTHEARC
00671              EXEC CICS LINK                                       ELTHEARC
00672                   PROGRAM ('ELUOUTPT')                            ELTHEARC
00673                   COMMAREA (DFHCOMMAREA)                          ELTHEARC
00674                   END-EXEC                                        ELTHEARC
00675              MOVE +1 TO WS-CIA.                                   ELTHEARC
00676                                                                   ELTHEARC
00677  2106-EXIT.  EXIT.                                                ELTHEARC
00678 /                                                                 ELTHEARC
00679  2110-PLACE-OF-TREATMENT.                                         ELTHEARC
00680 ****************************************************************  ELTHEARC
00681 *              P L A C E   O F   T R E A T M E N T             *  ELTHEARC
00682 ****************************************************************  ELTHEARC
00683      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00684      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00685                         AND                                       ELTHEARC
00686         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
00687                                                 NOT = ZERO        ELTHEARC
00688          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00689          MOVE +2                  TO WS-CIA                       ELTHEARC
00690          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
00691                                                                   ELTHEARC
00692      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00693      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00694                          AND                                      ELTHEARC
00695         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
00696                                                NOT = ZERO         ELTHEARC
00697                          AND                                      ELTHEARC
00698        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
00699          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00700          MOVE +2                  TO WS-CIA                       ELTHEARC
00701          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
00702                                                                   ELTHEARC
00703      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00704      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00705                          AND                                      ELTHEARC
00706         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
00707                                                 NOT = ZERO        ELTHEARC
00708          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
00709          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
00710                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00711          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
00712                             TO CMF-CODE-VALUE                     ELTHEARC
00713          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
00714          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00715          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00716             THRU 9500-EXIT.                                       ELTHEARC
00717                                                                   ELTHEARC
00718      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00719      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00720                          AND                                      ELTHEARC
00721         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
00722                                                NOT = ZERO         ELTHEARC
00723          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
00724          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
00725                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00726          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
00727                             TO CMF-CODE-VALUE                     ELTHEARC
00728          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
00729          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00730          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00731             THRU 9500-EXIT.                                       ELTHEARC
00732                                                                   ELTHEARC
00733      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
00734         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
00735          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
00736             THRU 9200-EXIT.                                       ELTHEARC
00737                                                                   ELTHEARC
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
00739         SET PLT-INDEX2 TO 2                                       ELTHEARC
00740      ELSE                                                         ELTHEARC
00741         SET PLT-INDEX2 TO 1.                                      ELTHEARC
00742                                                                   ELTHEARC
00743  2110-EXIT.  EXIT.                                                ELTHEARC
00744 /                                                                 ELTHEARC
00745  2120-PRIC-METH.                                                  ELTHEARC
00746 ****************************************************************  ELTHEARC
00747 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTHEARC
00748 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTHEARC
00749 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTHEARC
00750 ****************************************************************  ELTHEARC
00751      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00752      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00753         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
00754                                                              '19' ELTHEARC
00755         MOVE +2           TO WS-CIA                               ELTHEARC
00756         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
00757         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
00758                                                                   ELTHEARC
00759      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00760      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00761         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
00762                                                        '19' AND   ELTHEARC
00763        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
00764         MOVE +2           TO WS-CIA                               ELTHEARC
00765         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
00766         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
00767                                                                   ELTHEARC
00768      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00769      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00770         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
00771                           AND                                     ELTHEARC
00772         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00773         SET PLT-INDEX2 TO 2                                       ELTHEARC
00774         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
00775                                                             ZERO  ELTHEARC
00776            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
00777            ADD +1 TO WS-CIA                                       ELTHEARC
00778            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
00779                                                                   ELTHEARC
00780      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00781      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00782         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
00783                           AND                                     ELTHEARC
00784         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTHEARC
00785         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTHEARC
00786         ADD +1 TO WS-CIA                                          ELTHEARC
00787         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                    ELTHEARC
00788                                                                   ELTHEARC
00789      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTHEARC
00790         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00791         SET PLT-INDEX2 TO 2                                       ELTHEARC
00792         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
00793                                                             ZERO  ELTHEARC
00794            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
00795            ADD +1 TO WS-CIA                                       ELTHEARC
00796            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
00797                                                                   ELTHEARC
00798      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00799      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00800         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
00801                                                         = ZERO    ELTHEARC
00802            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00803                                                         = ZERO    ELTHEARC
00804               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
00805            ELSE                                                   ELTHEARC
00806               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
00807          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00808                                                TO WS-PERCENTAGE   ELTHEARC
00809         ELSE                                                      ELTHEARC
00810            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
00811          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00812                                               TO WS-PERCENTAGE.   ELTHEARC
00813      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00814         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
00815                                             ZERO AND NOT = '19'   ELTHEARC
00816         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
00817         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
00818         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
00819                                                    CMF-CODE-VALUE ELTHEARC
00820         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
00821         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
00822         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
00823            THRU 9600-EXIT.                                        ELTHEARC
00824                                                                   ELTHEARC
00825      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00826      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00827         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTHEARC
00828                                                               ZEROELTHEARC
00829            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00830                                                         = ZERO    ELTHEARC
00831               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
00832            ELSE                                                   ELTHEARC
00833               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
00834          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00835                                                TO WS-PERCENTAGE   ELTHEARC
00836         ELSE                                                      ELTHEARC
00837            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
00838          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
00839                                               TO WS-PERCENTAGE.   ELTHEARC
00840      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
00841         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
00842                                             ZERO AND NOT = '19'   ELTHEARC
00843         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
00844         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
00845         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
00846                                                    CMF-CODE-VALUE ELTHEARC
00847         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTHEARC
00848         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
00849         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
00850            THRU 9600-EXIT.                                        ELTHEARC
00851                                                                   ELTHEARC
00852      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
00853          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTHEARC
00854          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
00855             THRU 9200-EXIT.                                       ELTHEARC
00856                                                                   ELTHEARC
00857      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
00858         SET PLT-INDEX2 TO 2                                       ELTHEARC
00859      ELSE                                                         ELTHEARC
00860         SET PLT-INDEX2 TO 1.                                      ELTHEARC
00861                                                                   ELTHEARC
00862  2120-EXIT.  EXIT.                                                ELTHEARC
00863 /                                                                 ELTHEARC
00864  2140-CERTIFICATION.                                              ELTHEARC
00865 ****************************************************************  ELTHEARC
00866 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTHEARC
00867 ****************************************************************  ELTHEARC
00868      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00869      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00870                         AND                                       ELTHEARC
00871         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
00872                                                 NOT = '00'        ELTHEARC
00873          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00874          MOVE +2                  TO WS-CIA                       ELTHEARC
00875          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
00876                                                                   ELTHEARC
00877      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00878      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00879                          AND                                      ELTHEARC
00880         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
00881                                                NOT = '00'         ELTHEARC
00882                          AND                                      ELTHEARC
00883        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
00884          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00885          MOVE +2                  TO WS-CIA                       ELTHEARC
00886          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
00887                                                                   ELTHEARC
00888      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00889      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00890                          AND                                      ELTHEARC
00891         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
00892                                                 NOT = '00'        ELTHEARC
00893          MOVE 'BP'  TO CMF-RECORD-PREFIX                          ELTHEARC
00894          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
00895                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00896          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00897                             TO CMF-CODE-VALUE                     ELTHEARC
00898          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
00899          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00900          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00901             THRU 9500-EXIT.                                       ELTHEARC
00902                                                                   ELTHEARC
00903      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00904      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00905                          AND                                      ELTHEARC
00906         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
00907                                                NOT = '00'         ELTHEARC
00908          MOVE 'BP'          TO CMF-RECORD-PREFIX                  ELTHEARC
00909          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
00910                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00911          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00912                             TO CMF-CODE-VALUE                     ELTHEARC
00913          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
00914          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00915          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00916             THRU 9500-EXIT.                                       ELTHEARC
00917                                                                   ELTHEARC
00918      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
00919         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
00920          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
00921             THRU 9200-EXIT.                                       ELTHEARC
00922                                                                   ELTHEARC
00923      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
00924         SET PLT-INDEX2 TO 2                                       ELTHEARC
00925      ELSE                                                         ELTHEARC
00926         SET PLT-INDEX2 TO 1.                                      ELTHEARC
00927                                                                   ELTHEARC
00928  2140-EXIT.  EXIT.                                                ELTHEARC
00929 /                                                                 ELTHEARC
00930  2150-RECERTIFICATION.                                            ELTHEARC
00931 ****************************************************************  ELTHEARC
00932 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTHEARC
00933 ****************************************************************  ELTHEARC
00934      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00935      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00936                         AND                                       ELTHEARC
00937         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00938                                                 NOT = ZERO        ELTHEARC
00939          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00940          MOVE +2                  TO WS-CIA                       ELTHEARC
00941          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
00942                                                                   ELTHEARC
00943      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00944      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00945                          AND                                      ELTHEARC
00946         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00947                                                NOT = ZERO         ELTHEARC
00948                          AND                                      ELTHEARC
00949        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
00950          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
00951          MOVE +2                  TO WS-CIA                       ELTHEARC
00952          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
00953                                                                   ELTHEARC
00954      SET PLT-INDEX2 TO 1.                                         ELTHEARC
00955      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00956                          AND                                      ELTHEARC
00957         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00958                                                 NOT = ZERO        ELTHEARC
00959          MOVE 'BPB' TO CMF-RECORD-PREFIX                          ELTHEARC
00960          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
00961                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00962          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
00963                             TO CMF-CODE-VALUE                     ELTHEARC
00964          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
00965          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00966          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00967             THRU 9500-EXIT.                                       ELTHEARC
00968                                                                   ELTHEARC
00969      SET PLT-INDEX2 TO 2.                                         ELTHEARC
00970      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
00971                          AND                                      ELTHEARC
00972         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
00973                                                NOT = ZERO         ELTHEARC
00974          MOVE 'BPB'         TO CMF-RECORD-PREFIX                  ELTHEARC
00975          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
00976                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
00977          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
00978                             TO CMF-CODE-VALUE                     ELTHEARC
00979          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
00980          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
00981          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
00982             THRU 9500-EXIT.                                       ELTHEARC
00983                                                                   ELTHEARC
00984      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
00985         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
00986          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
00987             THRU 9200-EXIT.                                       ELTHEARC
00988                                                                   ELTHEARC
00989      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
00990         SET PLT-INDEX2 TO 2                                       ELTHEARC
00991      ELSE                                                         ELTHEARC
00992         SET PLT-INDEX2 TO 1.                                      ELTHEARC
00993                                                                   ELTHEARC
00994  2150-EXIT.  EXIT.                                                ELTHEARC
00995 /                                                                 ELTHEARC
00996  2155-SERVC-NEC.                                                  ELTHEARC
00997 ****************************************************************  ELTHEARC
00998 *  S E V I C E   N E C E S S A R Y   B I T   I N D I C A T O R *  ELTHEARC
00999 ****************************************************************  ELTHEARC
01000      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01001      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01002                         AND                                       ELTHEARC
01003         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
01004                                               = '1'               ELTHEARC
01005          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01006          MOVE +3                  TO WS-CIA                       ELTHEARC
01007          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
01008          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
01009                                                                   ELTHEARC
01010      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01011      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01012                          AND                                      ELTHEARC
01013         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
01014                                              = '1'                ELTHEARC
01015                          AND                                      ELTHEARC
01016        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01017          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01018          MOVE +3                  TO WS-CIA                       ELTHEARC
01019          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
01020          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
01021                                                                   ELTHEARC
01022      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01023         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01024          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01025             THRU 9200-EXIT.                                       ELTHEARC
01026                                                                   ELTHEARC
01027      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01028         SET PLT-INDEX2 TO 2                                       ELTHEARC
01029      ELSE                                                         ELTHEARC
01030         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01031                                                                   ELTHEARC
01032  2155-EXIT.  EXIT.                                                ELTHEARC
01033 /                                                                 ELTHEARC
01034  2160-SPILLOVR-COINS-N-DEDUC.                                     ELTHEARC
01035 ****************************************************************  ELTHEARC
01036 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTHEARC
01037 ****************************************************************  ELTHEARC
01038      MOVE +1 TO WS-CIA.                                           ELTHEARC
01039                                                                   ELTHEARC
01040      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01041      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01042                           AND                                     ELTHEARC
01043         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTHEARC
01044                                                 NOT = '0'         ELTHEARC
01045         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01046         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTHEARC
01047                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
01048         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTHEARC
01049                                              TO CMF-CODE-VALUE    ELTHEARC
01050         MOVE WS-SPILLOVER      TO WS-TEMP-TEXT-AREA               ELTHEARC
01051         MOVE +69               TO WS-TEMP-NOT-USED-CNT            ELTHEARC
01052         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
01053            THRU 9500-EXIT                                         ELTHEARC
01054         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
01055            THRU 9200-EXIT.                                        ELTHEARC
01056 ****************************************************************  ELTHEARC
01057 *          S P I L L O V E R   D E D U C T I B L E             *  ELTHEARC
01058 ****************************************************************  ELTHEARC
01059      MOVE +1 TO WS-CIA.                                           ELTHEARC
01060                                                                   ELTHEARC
01061      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01062      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01063                           AND                                     ELTHEARC
01064         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01065                                                 NOT = '0'         ELTHEARC
01066         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01067         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTHEARC
01068                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
01069         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
01070                                               TO CMF-CODE-VALUE   ELTHEARC
01071         MOVE WS-SPILLOVER    TO WS-TEMP-TEXT-AREA                 ELTHEARC
01072         MOVE +69             TO WS-TEMP-NOT-USED-CNT              ELTHEARC
01073         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
01074            THRU 9500-EXIT                                         ELTHEARC
01075         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
01076            THRU 9200-EXIT.                                        ELTHEARC
01077                                                                   ELTHEARC
01078      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01079         SET PLT-INDEX2 TO 2                                       ELTHEARC
01080      ELSE                                                         ELTHEARC
01081         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01082                                                                   ELTHEARC
01083  2160-EXIT.  EXIT.                                                ELTHEARC
01084                                                                   ELTHEARC
01085  2170-AAR-PPF-PVE-TABS.                                           ELTHEARC
01086 ****************************************************************  ELTHEARC
01087 *                  A A R   T A B U L A R                       *  ELTHEARC
01088 ****************************************************************  ELTHEARC
01089      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01090      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01091         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01092                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01093         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01094         MOVE +1 TO COF-NBR-DTL-LINES                              ELTHEARC
01095         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTHEARC
01096      ELSE                                                         ELTHEARC
01097         SET PLT-INDEX2 TO 2                                       ELTHEARC
01098         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01099            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01100                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01101            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTHEARC
01102            MOVE +1 TO COF-NBR-DTL-LINES                           ELTHEARC
01103            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTHEARC
01104                                                                   ELTHEARC
01105      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01106         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01107         MOVE 1 TO WS-CIA                                          ELTHEARC
01108         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
01109             COMMAREA(DFHCOMMAREA)                                 ELTHEARC
01110         END-EXEC.                                                 ELTHEARC
01111 *--------------------------------------------------------------*  ELTHEARC
01112 *                  P P F   T A B U L A R                       *  ELTHEARC
01113 *--------------------------------------------------------------*  ELTHEARC
01114      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01115      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01116         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01117                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01118         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01119                                                  KWA-GCTABULR-KEY ELTHEARC
01120         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01121            THRU 9900-EXIT                                         ELTHEARC
01122         IF IOP-RC-OK                                              ELTHEARC
01123            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTHEARC
01124                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01125            END-EXEC                                               ELTHEARC
01126         ELSE                                                      ELTHEARC
01127            NEXT SENTENCE                                          ELTHEARC
01128      ELSE                                                         ELTHEARC
01129         SET PLT-INDEX2 TO 2                                       ELTHEARC
01130         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01131            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01132                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01133          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01134                                                  KWA-GCTABULR-KEY ELTHEARC
01135            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01136               THRU 9900-EXIT                                      ELTHEARC
01137            IF IOP-RC-OK                                           ELTHEARC
01138               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTHEARC
01139                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01140            END-EXEC.                                              ELTHEARC
01141 *--------------------------------------------------------------*  ELTHEARC
01142 *                  P V E   T A B U L A R                       *  ELTHEARC
01143 *--------------------------------------------------------------*  ELTHEARC
01144                                                                   ELTHEARC
01145      MOVE +2    TO WS-CIA.                                        ELTHEARC
01146      MOVE WS-PVE TO COF-DTL-LINE (2).                             ELTHEARC
01147      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
01148         THRU 9200-EXIT.                                           ELTHEARC
01149                                                                   ELTHEARC
01150  2170-EXIT.  EXIT.                                                ELTHEARC
01151 /                                                                 ELTHEARC
01152  2180-ALL-LEVEL-TABS.                                             ELTHEARC
01153 *--------------------------------------------------------------*  ELTHEARC
01154 *                  A B M   T A B U L A R                       *  ELTHEARC
01155 *--------------------------------------------------------------*  ELTHEARC
01156      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01157      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01158                             AND                                   ELTHEARC
01159         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01160                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01161         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01162                                                  KWA-GCTABULR-KEY ELTHEARC
01163         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01164            THRU 9900-EXIT                                         ELTHEARC
01165         IF IOP-RC-OK                                              ELTHEARC
01166            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTHEARC
01167                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01168            END-EXEC                                               ELTHEARC
01169         ELSE                                                      ELTHEARC
01170            NEXT SENTENCE                                          ELTHEARC
01171      ELSE                                                         ELTHEARC
01172         SET PLT-INDEX2 TO 2                                       ELTHEARC
01173         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01174            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01175                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01176          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01177                                                  KWA-GCTABULR-KEY ELTHEARC
01178            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01179               THRU 9900-EXIT                                      ELTHEARC
01180            IF IOP-RC-OK                                           ELTHEARC
01181               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTHEARC
01182                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01183               END-EXEC.                                           ELTHEARC
01184 *--------------------------------------------------------------*  ELTHEARC
01185 *                  A C L   T A B U L A R                       *  ELTHEARC
01186 *--------------------------------------------------------------*  ELTHEARC
01187      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01188      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01189         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01190                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01191         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01192                                                  KWA-GCTABULR-KEY ELTHEARC
01193         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01194            THRU 9900-EXIT                                         ELTHEARC
01195         IF IOP-RC-OK                                              ELTHEARC
01196            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTHEARC
01197                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01198            END-EXEC                                               ELTHEARC
01199         ELSE                                                      ELTHEARC
01200            NEXT SENTENCE                                          ELTHEARC
01201      ELSE                                                         ELTHEARC
01202         SET PLT-INDEX2 TO 2                                       ELTHEARC
01203         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01204            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01205                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01206          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01207                                                  KWA-GCTABULR-KEY ELTHEARC
01208            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01209               THRU 9900-EXIT                                      ELTHEARC
01210            IF IOP-RC-OK                                           ELTHEARC
01211               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTHEARC
01212                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01213               END-EXEC.                                           ELTHEARC
01214 *--------------------------------------------------------------*  ELTHEARC
01215 *                  A D L   T A B U L A R                       *  ELTHEARC
01216 *--------------------------------------------------------------*  ELTHEARC
01217      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01218      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01219         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01220                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01221         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01222                                                  KWA-GCTABULR-KEY ELTHEARC
01223         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01224            THRU 9900-EXIT                                         ELTHEARC
01225         IF IOP-RC-OK                                              ELTHEARC
01226            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTHEARC
01227                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01228            END-EXEC                                               ELTHEARC
01229         ELSE                                                      ELTHEARC
01230            NEXT SENTENCE                                          ELTHEARC
01231      ELSE                                                         ELTHEARC
01232         SET PLT-INDEX2 TO 2                                       ELTHEARC
01233         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01234            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01235                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01236          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01237                                                  KWA-GCTABULR-KEY ELTHEARC
01238            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01239               THRU 9900-EXIT                                      ELTHEARC
01240            IF IOP-RC-OK                                           ELTHEARC
01241               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTHEARC
01242                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01243               END-EXEC.                                           ELTHEARC
01244 *--------------------------------------------------------------*  ELTHEARC
01245 *                  A O L   T A B U L A R                       *  ELTHEARC
01246 *--------------------------------------------------------------*  ELTHEARC
01247      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01248      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01249         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01250                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01251         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01252                                                  KWA-GCTABULR-KEY ELTHEARC
01253         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01254            THRU 9900-EXIT                                         ELTHEARC
01255         IF IOP-RC-OK                                              ELTHEARC
01256            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTHEARC
01257                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01258            END-EXEC                                               ELTHEARC
01259         ELSE                                                      ELTHEARC
01260            NEXT SENTENCE                                          ELTHEARC
01261      ELSE                                                         ELTHEARC
01262         SET PLT-INDEX2 TO 2                                       ELTHEARC
01263         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01264            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01265                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01266          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01267                                                  KWA-GCTABULR-KEY ELTHEARC
01268            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01269               THRU 9900-EXIT                                      ELTHEARC
01270            IF IOP-RC-OK                                           ELTHEARC
01271               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTHEARC
01272                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01273               END-EXEC.                                           ELTHEARC
01274  2180-EXIT.  EXIT.                                                ELTHEARC
01275 /                                                                 ELTHEARC
01276 ****************************************************************  ELTHEARC
01277 *       HEARING CARE INSTITUTIONAL OUTPATIENT PROVESSING       *  ELTHEARC
01278 ****************************************************************  ELTHEARC
01279  3000-INSTITUTIONAL-OP.                                           ELTHEARC
01280                                                                   ELTHEARC
01281      MOVE 'Y'   TO WS-FIRSTTIME-IND.                              ELTHEARC
01282      MOVE HEADER-I-OP-LINE-3 TO COF-HDR-LINE (2).                 ELTHEARC
01283                                                                   ELTHEARC
01284      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTHEARC
01285         THRU 9100-EXIT.                                           ELTHEARC
01286                                                                   ELTHEARC
01287      MOVE WS-INST-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTHEARC
01288                                                                   ELTHEARC
01289      PERFORM 3010-MOVE-IN-INST-OP-TABS                            ELTHEARC
01290         THRU 3010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
01291                        UNTIL   WS-SUB > WS-INST-OP-CNT.           ELTHEARC
01292                                                                   ELTHEARC
01293      PERFORM 3020-CALL-COVERAGE                                   ELTHEARC
01294         THRU 3020-EXIT.                                           ELTHEARC
01295                                                                   ELTHEARC
01296      IF PVN-COVG-NONE                                             ELTHEARC
01297          GO TO 3000-EXIT.                                         ELTHEARC
01298                                                                   ELTHEARC
01299      PERFORM 3030-FIND-FIRST-NONZERO                              ELTHEARC
01300         THRU 3030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
01301                        UNTIL   WS-SUB > WS-INST-OP-CNT.           ELTHEARC
01302                                                                   ELTHEARC
01303  3000-EXIT.  EXIT.                                                ELTHEARC
01304 /                                                                 ELTHEARC
01305  3010-MOVE-IN-INST-OP-TABS.                                       ELTHEARC
01306      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
01307      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTHEARC
01308              TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).             ELTHEARC
01309                                                                   ELTHEARC
01310      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),           ELTHEARC
01311                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTHEARC
01312                                                                   ELTHEARC
01313  3010-EXIT.  EXIT.                                                ELTHEARC
01314                                                                   ELTHEARC
01315  3020-CALL-COVERAGE.                                              ELTHEARC
01316                                                                   ELTHEARC
01317      MOVE 'HEARING CARE ' TO SSB-TOPIC-PHRASE.                    ELTHEARC
01318                                                                   ELTHEARC
01319      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTHEARC
01320                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
01321                     END-EXEC.                                     ELTHEARC
01322                                                                   ELTHEARC
01323      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
01324                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
01325                     END-EXEC.                                     ELTHEARC
01326                                                                   ELTHEARC
01327      IF PVN-COVG-NONE                                             ELTHEARC
01328          GO TO 3020-EXIT.                                         ELTHEARC
01329                                                                   ELTHEARC
01330      MOVE +1 TO WS-CIA.                                           ELTHEARC
01331                                                                   ELTHEARC
01332      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTHEARC
01333                                                                   ELTHEARC
01334      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTHEARC
01335                    PSP-PROVN-PRICING-METHD,                       ELTHEARC
01336                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTHEARC
01337                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTHEARC
01338                    PSP-TRANSF-OTHER-RESP-IND,                     ELTHEARC
01339                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTHEARC
01340                    PSP-SPILL-OVER-DED-APL-IND,                    ELTHEARC
01341                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTHEARC
01342                    PSP-CERTFN-REQRM-IND,                          ELTHEARC
01343                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTHEARC
01344                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTHEARC
01345                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTHEARC
01346                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTHEARC
01347                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTHEARC
01348                    PSP-BEN-TAB-PROVN-ID-PVE,                      ELTHEARC
01349                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTHEARC
01350                    PSB-CERTN-REPETN-REQRD-IND.                    ELTHEARC
01351                                                                   ELTHEARC
01352      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTHEARC
01353                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
01354                     END-EXEC.                                     ELTHEARC
01355                                                                   ELTHEARC
01356      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTHEARC
01357      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
01358              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTHEARC
01359                                                                   ELTHEARC
01360                                                                   ELTHEARC
01361  3020-EXIT.  EXIT.                                                ELTHEARC
01362                                                                   ELTHEARC
01363  3030-FIND-FIRST-NONZERO.                                         ELTHEARC
01364      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
01365                                                                   ELTHEARC
01366      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTHEARC
01367          NEXT SENTENCE                                            ELTHEARC
01368      ELSE                                                         ELTHEARC
01369          PERFORM 3100-BUILD-SCREEN-LINES                          ELTHEARC
01370             THRU 3100-EXIT.                                       ELTHEARC
01371                                                                   ELTHEARC
01372  3030-EXIT.  EXIT.                                                ELTHEARC
01373 /                                                                 ELTHEARC
01374  3100-BUILD-SCREEN-LINES.                                         ELTHEARC
01375      SET PLT-INDEX1 TO                                            ELTHEARC
01376              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTHEARC
01377                                                                   ELTHEARC
01378      IF WS-NOT-FIRST-TIME                                         ELTHEARC
01379         MOVE 'P'    TO COF-FUNCTION                               ELTHEARC
01380         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTHEARC
01381         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
01382                          COMMAREA(DFHCOMMAREA)                    ELTHEARC
01383         END-EXEC                                                  ELTHEARC
01384      ELSE                                                         ELTHEARC
01385        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTHEARC
01386                                                                   ELTHEARC
01387      MOVE +1 TO WS-CIA.                                           ELTHEARC
01388                                                                   ELTHEARC
01389      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTHEARC
01390          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTHEARC
01391              SET PLT-INDEX2 TO 2                                  ELTHEARC
01392          ELSE                                                     ELTHEARC
01393              MOVE WS-INDICES-PROBLEM TO COF-DTL-LINE (1)          ELTHEARC
01394              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTHEARC
01395                 THRU 9200-EXIT                                    ELTHEARC
01396              GO TO 3100-EXIT                                      ELTHEARC
01397      ELSE                                                         ELTHEARC
01398          SET PLT-INDEX2 TO 1.                                     ELTHEARC
01399                                                                   ELTHEARC
01400      PERFORM 3105-LIST-BEN-PROV                                   ELTHEARC
01401         THRU 3105-EXIT.                                           ELTHEARC
01402                                                                   ELTHEARC
01403      PERFORM 3110-PLACE-OF-TREATMENT                              ELTHEARC
01404         THRU 3110-EXIT.                                           ELTHEARC
01405                                                                   ELTHEARC
01406      PERFORM 3120-PRIC-METH                                       ELTHEARC
01407         THRU 3120-EXIT.                                           ELTHEARC
01408                                                                   ELTHEARC
01409      PERFORM 3140-CERTIFICATION                                   ELTHEARC
01410         THRU 3140-EXIT.                                           ELTHEARC
01411                                                                   ELTHEARC
01412      PERFORM 3150-RECERTIFICATION                                 ELTHEARC
01413         THRU 3150-EXIT.                                           ELTHEARC
01414                                                                   ELTHEARC
01415      PERFORM 3155-SERVC-NEC                                       ELTHEARC
01416         THRU 3155-EXIT.                                           ELTHEARC
01417                                                                   ELTHEARC
01418      PERFORM 3160-SPILLOVR-COINS-N-DEDUC                          ELTHEARC
01419         THRU 3160-EXIT.                                           ELTHEARC
01420                                                                   ELTHEARC
01421      PERFORM 3170-AAR-PPF-PVE-TABS                                ELTHEARC
01422         THRU 3170-EXIT.                                           ELTHEARC
01423                                                                   ELTHEARC
01424      PERFORM 3180-ALL-LEVEL-TABS                                  ELTHEARC
01425         THRU 3180-EXIT.                                           ELTHEARC
01426                                                                   ELTHEARC
01427      PERFORM 6000-PAY-CONSID-TEXT THRU 6000-EXIT.                 ELTHEARC
01428                                                                   ELTHEARC
01429      PERFORM 6100-TRANSF-OTHER-RESP-IND  THRU 6100-EXIT.          ELTHEARC
01430                                                                   ELTHEARC
01431  3100-EXIT.  EXIT.                                                ELTHEARC
01432 /                                                                 ELTHEARC
01433  3105-LIST-BEN-PROV.                                              ELTHEARC
01434 ****************************************************************  ELTHEARC
01435 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTHEARC
01436 ****************************************************************  ELTHEARC
01437      MOVE +2              TO WS-CIA.                              ELTHEARC
01438      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE (WS-CIA).              ELTHEARC
01439      MOVE ZERO            TO WS-SUB2.                             ELTHEARC
01440      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTHEARC
01441                           TO WS-SUB3.                             ELTHEARC
01442                                                                   ELTHEARC
01443      PERFORM 3106-ZERO-ALL-WITH-SAME-NO                           ELTHEARC
01444         THRU 3106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTHEARC
01445                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.ELTHEARC
01446                                                                   ELTHEARC
01447      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
01448      MOVE WS-CIA  TO COF-NBR-DTL-LINES.                           ELTHEARC
01449                                                                   ELTHEARC
01450      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
01451                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
01452                     END-EXEC.                                     ELTHEARC
01453                                                                   ELTHEARC
01454  3105-EXIT.  EXIT.                                                ELTHEARC
01455                                                                   ELTHEARC
01456  3106-ZERO-ALL-WITH-SAME-NO.                                      ELTHEARC
01457                                                                   ELTHEARC
01458      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTHEARC
01459          MOVE 'BP'       TO CMF-RECORD-PREFIX                     ELTHEARC
01460          MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME              ELTHEARC
01461          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTHEARC
01462                          TO CMF-CODE-VALUE                        ELTHEARC
01463          MOVE SPACES     TO WS-TEMP-TEXT-AREA                     ELTHEARC
01464          MOVE +58        TO WS-TEMP-NOT-USED-CNT                  ELTHEARC
01465          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01466             THRU 9500-EXIT                                        ELTHEARC
01467          MOVE ZERO TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)        ELTHEARC
01468          ADD +1   TO WS-SUB2                                      ELTHEARC
01469          IF WS-CIA > 20 OR = 20                                   ELTHEARC
01470              MOVE WS-CIA TO COF-NBR-DTL-LINES                     ELTHEARC
01471              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTHEARC
01472                             COMMAREA (DFHCOMMAREA)                ELTHEARC
01473                             END-EXEC                              ELTHEARC
01474              MOVE +1 TO WS-CIA.                                   ELTHEARC
01475                                                                   ELTHEARC
01476  3106-EXIT.  EXIT.                                                ELTHEARC
01477 /                                                                 ELTHEARC
01478  3110-PLACE-OF-TREATMENT.                                         ELTHEARC
01479 ****************************************************************  ELTHEARC
01480 *              P L A C E   O F   T R E A T M E N T             *  ELTHEARC
01481 ****************************************************************  ELTHEARC
01482      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01483      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01484                         AND                                       ELTHEARC
01485         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
01486                                                 NOT = ZERO        ELTHEARC
01487          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01488          MOVE +2                  TO WS-CIA                       ELTHEARC
01489          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
01490                                                                   ELTHEARC
01491      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01492      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01493                          AND                                      ELTHEARC
01494         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
01495                                                NOT = ZERO         ELTHEARC
01496                          AND                                      ELTHEARC
01497        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01498          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01499          MOVE +2                  TO WS-CIA                       ELTHEARC
01500          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
01501                                                                   ELTHEARC
01502      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01503      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01504                          AND                                      ELTHEARC
01505         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
01506                                                 NOT = ZERO        ELTHEARC
01507          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
01508          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
01509                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01510          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
01511                             TO CMF-CODE-VALUE                     ELTHEARC
01512          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
01513          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01514          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01515             THRU 9500-EXIT.                                       ELTHEARC
01516                                                                   ELTHEARC
01517      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01518      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01519                          AND                                      ELTHEARC
01520         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
01521                                                NOT = ZERO         ELTHEARC
01522          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
01523          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
01524                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01525          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
01526                             TO CMF-CODE-VALUE                     ELTHEARC
01527          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
01528          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01529          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01530             THRU 9500-EXIT.                                       ELTHEARC
01531                                                                   ELTHEARC
01532      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01533         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01534          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01535             THRU 9200-EXIT.                                       ELTHEARC
01536                                                                   ELTHEARC
01537      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01538         SET PLT-INDEX2 TO 2                                       ELTHEARC
01539      ELSE                                                         ELTHEARC
01540         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01541                                                                   ELTHEARC
01542  3110-EXIT.  EXIT.                                                ELTHEARC
01543 /                                                                 ELTHEARC
01544  3120-PRIC-METH.                                                  ELTHEARC
01545 ****************************************************************  ELTHEARC
01546 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTHEARC
01547 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTHEARC
01548 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTHEARC
01549 ****************************************************************  ELTHEARC
01550      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01551      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01552         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
01553                                                              '19' ELTHEARC
01554         MOVE +2           TO WS-CIA                               ELTHEARC
01555         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
01556         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
01557                                                                   ELTHEARC
01558      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01559      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01560         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
01561                                                        '19' AND   ELTHEARC
01562        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01563         MOVE +2           TO WS-CIA                               ELTHEARC
01564         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
01565         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
01566                                                                   ELTHEARC
01567      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01568      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01569         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
01570                           AND                                     ELTHEARC
01571         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01572         SET PLT-INDEX2 TO 2                                       ELTHEARC
01573         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
01574                                                             ZERO  ELTHEARC
01575            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
01576            ADD +1 TO WS-CIA                                       ELTHEARC
01577            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
01578                                                                   ELTHEARC
01579      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01580      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01581         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
01582                           AND                                     ELTHEARC
01583         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTHEARC
01584         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTHEARC
01585         ADD +1 TO WS-CIA                                          ELTHEARC
01586         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                    ELTHEARC
01587                                                                   ELTHEARC
01588      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTHEARC
01589         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01590         SET PLT-INDEX2 TO 2                                       ELTHEARC
01591         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
01592                                                             ZERO  ELTHEARC
01593            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
01594            ADD +1 TO WS-CIA                                       ELTHEARC
01595            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
01596                                                                   ELTHEARC
01597      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01598      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01599         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
01600                                                         = ZERO    ELTHEARC
01601            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01602                                                         = ZERO    ELTHEARC
01603               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
01604            ELSE                                                   ELTHEARC
01605               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
01606          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01607                                                TO WS-PERCENTAGE   ELTHEARC
01608         ELSE                                                      ELTHEARC
01609            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
01610          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01611                                               TO WS-PERCENTAGE.   ELTHEARC
01612      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01613         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
01614                                             ZERO AND NOT = '19'   ELTHEARC
01615         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01616         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
01617         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
01618                                                    CMF-CODE-VALUE ELTHEARC
01619         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
01620         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
01621         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
01622            THRU 9600-EXIT.                                        ELTHEARC
01623                                                                   ELTHEARC
01624      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01625      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01626         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTHEARC
01627                                                               ZEROELTHEARC
01628            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01629                                                         = ZERO    ELTHEARC
01630               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
01631            ELSE                                                   ELTHEARC
01632               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
01633          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01634                                                TO WS-PERCENTAGE   ELTHEARC
01635         ELSE                                                      ELTHEARC
01636            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
01637          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
01638                                               TO WS-PERCENTAGE.   ELTHEARC
01639      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01640         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
01641                                             ZERO AND NOT = '19'   ELTHEARC
01642         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01643         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
01644         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
01645                                                    CMF-CODE-VALUE ELTHEARC
01646         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTHEARC
01647         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
01648         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
01649            THRU 9600-EXIT.                                        ELTHEARC
01650                                                                   ELTHEARC
01651      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01652          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTHEARC
01653          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01654             THRU 9200-EXIT.                                       ELTHEARC
01655                                                                   ELTHEARC
01656      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01657         SET PLT-INDEX2 TO 2                                       ELTHEARC
01658      ELSE                                                         ELTHEARC
01659         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01660                                                                   ELTHEARC
01661  3120-EXIT.  EXIT.                                                ELTHEARC
01662 /                                                                 ELTHEARC
01663  3140-CERTIFICATION.                                              ELTHEARC
01664 ****************************************************************  ELTHEARC
01665 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTHEARC
01666 ****************************************************************  ELTHEARC
01667      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01668      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01669                         AND                                       ELTHEARC
01670         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
01671                                                 NOT = '00'        ELTHEARC
01672          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01673          MOVE +2                  TO WS-CIA                       ELTHEARC
01674          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
01675                                                                   ELTHEARC
01676      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01677      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01678                          AND                                      ELTHEARC
01679         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
01680                                                NOT = '00'         ELTHEARC
01681                          AND                                      ELTHEARC
01682        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01683          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01684          MOVE +2                  TO WS-CIA                       ELTHEARC
01685          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
01686                                                                   ELTHEARC
01687      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01688      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01689                          AND                                      ELTHEARC
01690         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
01691                                                 NOT = '00'        ELTHEARC
01692          MOVE 'BP'  TO CMF-RECORD-PREFIX                          ELTHEARC
01693          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
01694                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01695          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01696                             TO CMF-CODE-VALUE                     ELTHEARC
01697          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
01698          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01699          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01700             THRU 9500-EXIT.                                       ELTHEARC
01701                                                                   ELTHEARC
01702      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01703      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01704                          AND                                      ELTHEARC
01705         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
01706                                                NOT = '00'         ELTHEARC
01707          MOVE 'BP'          TO CMF-RECORD-PREFIX                  ELTHEARC
01708          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
01709                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01710          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01711                             TO CMF-CODE-VALUE                     ELTHEARC
01712          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
01713          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01714          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01715             THRU 9500-EXIT.                                       ELTHEARC
01716                                                                   ELTHEARC
01717      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01718         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01719          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01720             THRU 9200-EXIT.                                       ELTHEARC
01721                                                                   ELTHEARC
01722      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01723         SET PLT-INDEX2 TO 2                                       ELTHEARC
01724      ELSE                                                         ELTHEARC
01725         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01726                                                                   ELTHEARC
01727  3140-EXIT.  EXIT.                                                ELTHEARC
01728 /                                                                 ELTHEARC
01729  3150-RECERTIFICATION.                                            ELTHEARC
01730 ****************************************************************  ELTHEARC
01731 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTHEARC
01732 ****************************************************************  ELTHEARC
01733      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01734      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01735                         AND                                       ELTHEARC
01736         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01737                                                 NOT = ZERO        ELTHEARC
01738          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01739          MOVE +2                  TO WS-CIA                       ELTHEARC
01740          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
01741                                                                   ELTHEARC
01742      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01743      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01744                          AND                                      ELTHEARC
01745         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01746                                                NOT = ZERO         ELTHEARC
01747                          AND                                      ELTHEARC
01748        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01749          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01750          MOVE +2                  TO WS-CIA                       ELTHEARC
01751          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
01752                                                                   ELTHEARC
01753      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01754      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01755                          AND                                      ELTHEARC
01756         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01757                                                 NOT = ZERO        ELTHEARC
01758          MOVE 'BPB' TO CMF-RECORD-PREFIX                          ELTHEARC
01759          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
01760                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01761          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
01762                             TO CMF-CODE-VALUE                     ELTHEARC
01763          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
01764          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01765          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01766             THRU 9500-EXIT.                                       ELTHEARC
01767                                                                   ELTHEARC
01768      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01769      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01770                          AND                                      ELTHEARC
01771         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01772                                                NOT = ZERO         ELTHEARC
01773          MOVE 'BPB'         TO CMF-RECORD-PREFIX                  ELTHEARC
01774          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
01775                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
01776          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
01777                             TO CMF-CODE-VALUE                     ELTHEARC
01778          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
01779          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
01780          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
01781             THRU 9500-EXIT.                                       ELTHEARC
01782                                                                   ELTHEARC
01783      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01784         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01785          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01786             THRU 9200-EXIT.                                       ELTHEARC
01787                                                                   ELTHEARC
01788      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01789         SET PLT-INDEX2 TO 2                                       ELTHEARC
01790      ELSE                                                         ELTHEARC
01791         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01792                                                                   ELTHEARC
01793  3150-EXIT.  EXIT.                                                ELTHEARC
01794 /                                                                 ELTHEARC
01795  3155-SERVC-NEC.                                                  ELTHEARC
01796 ****************************************************************  ELTHEARC
01797 * S E R V I C E   N E C E S S A R Y   B I T   I N D I C A T O R*  ELTHEARC
01798 ****************************************************************  ELTHEARC
01799      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01800      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01801                         AND                                       ELTHEARC
01802         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
01803                                                   = '1'           ELTHEARC
01804          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01805          MOVE +3                  TO WS-CIA                       ELTHEARC
01806          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
01807          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
01808                                                                   ELTHEARC
01809      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01810      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
01811                          AND                                      ELTHEARC
01812         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
01813                                                   = '1'           ELTHEARC
01814                          AND                                      ELTHEARC
01815        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
01816          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
01817          MOVE +3                  TO WS-CIA                       ELTHEARC
01818          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
01819          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
01820                                                                   ELTHEARC
01821      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01822         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01823          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
01824             THRU 9200-EXIT.                                       ELTHEARC
01825                                                                   ELTHEARC
01826      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01827         SET PLT-INDEX2 TO 2                                       ELTHEARC
01828      ELSE                                                         ELTHEARC
01829         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01830                                                                   ELTHEARC
01831  3155-EXIT.  EXIT.                                                ELTHEARC
01832 /                                                                 ELTHEARC
01833  3160-SPILLOVR-COINS-N-DEDUC.                                     ELTHEARC
01834 ****************************************************************  ELTHEARC
01835 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTHEARC
01836 ****************************************************************  ELTHEARC
01837      MOVE +1 TO WS-CIA.                                           ELTHEARC
01838                                                                   ELTHEARC
01839      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01840      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01841                           AND                                     ELTHEARC
01842         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTHEARC
01843                                                 NOT = '0'         ELTHEARC
01844         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01845         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTHEARC
01846                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
01847         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTHEARC
01848                                              TO CMF-CODE-VALUE    ELTHEARC
01849         MOVE WS-SPILLOVER      TO WS-TEMP-TEXT-AREA               ELTHEARC
01850         MOVE +69               TO WS-TEMP-NOT-USED-CNT            ELTHEARC
01851         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
01852            THRU 9500-EXIT                                         ELTHEARC
01853         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
01854            THRU 9200-EXIT.                                        ELTHEARC
01855 ****************************************************************  ELTHEARC
01856 *          S P I L L O V E R   D E D U C T I B L E             *  ELTHEARC
01857 ****************************************************************  ELTHEARC
01858      MOVE +1 TO WS-CIA.                                           ELTHEARC
01859                                                                   ELTHEARC
01860      SET PLT-INDEX2 TO 2.                                         ELTHEARC
01861      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01862                           AND                                     ELTHEARC
01863         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
01864                                                 NOT = '0'         ELTHEARC
01865         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
01866         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTHEARC
01867                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
01868         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
01869                                               TO CMF-CODE-VALUE   ELTHEARC
01870         MOVE WS-SPILLOVER    TO WS-TEMP-TEXT-AREA                 ELTHEARC
01871         MOVE +69             TO WS-TEMP-NOT-USED-CNT              ELTHEARC
01872         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
01873            THRU 9500-EXIT                                         ELTHEARC
01874         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
01875            THRU 9200-EXIT.                                        ELTHEARC
01876                                                                   ELTHEARC
01877      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
01878         SET PLT-INDEX2 TO 2                                       ELTHEARC
01879      ELSE                                                         ELTHEARC
01880         SET PLT-INDEX2 TO 1.                                      ELTHEARC
01881                                                                   ELTHEARC
01882  3160-EXIT.  EXIT.                                                ELTHEARC
01883                                                                   ELTHEARC
01884  3170-AAR-PPF-PVE-TABS.                                           ELTHEARC
01885 ****************************************************************  ELTHEARC
01886 *                  A A R   T A B U L A R                       *  ELTHEARC
01887 ****************************************************************  ELTHEARC
01888      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01889      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01890         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01891                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01892         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01893         MOVE +1 TO COF-NBR-DTL-LINES                              ELTHEARC
01894         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTHEARC
01895      ELSE                                                         ELTHEARC
01896         SET PLT-INDEX2 TO 2                                       ELTHEARC
01897         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01898            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01899                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01900            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTHEARC
01901            MOVE +1 TO COF-NBR-DTL-LINES                           ELTHEARC
01902            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTHEARC
01903                                                                   ELTHEARC
01904      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
01905         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
01906         MOVE 1 TO WS-CIA                                          ELTHEARC
01907         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
01908             COMMAREA(DFHCOMMAREA)                                 ELTHEARC
01909         END-EXEC.                                                 ELTHEARC
01910 *--------------------------------------------------------------*  ELTHEARC
01911 *                  P P F   T A B U L A R                       *  ELTHEARC
01912 *--------------------------------------------------------------*  ELTHEARC
01913      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01914      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01915         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01916                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01917         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01918                                                  KWA-GCTABULR-KEY ELTHEARC
01919         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01920            THRU 9900-EXIT                                         ELTHEARC
01921         IF IOP-RC-OK                                              ELTHEARC
01922            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTHEARC
01923                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01924            END-EXEC                                               ELTHEARC
01925         ELSE                                                      ELTHEARC
01926            NEXT SENTENCE                                          ELTHEARC
01927      ELSE                                                         ELTHEARC
01928         SET PLT-INDEX2 TO 2                                       ELTHEARC
01929         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01930            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01931                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01932          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01933                                                  KWA-GCTABULR-KEY ELTHEARC
01934            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01935               THRU 9900-EXIT                                      ELTHEARC
01936            IF IOP-RC-OK                                           ELTHEARC
01937               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTHEARC
01938                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01939               END-EXEC.                                           ELTHEARC
01940 *--------------------------------------------------------------*  ELTHEARC
01941 *                  P V E   T A B U L A R                       *  ELTHEARC
01942 *--------------------------------------------------------------*  ELTHEARC
01943                                                                   ELTHEARC
01944      MOVE +2    TO WS-CIA.                                        ELTHEARC
01945      MOVE WS-PVE TO COF-DTL-LINE (2).                             ELTHEARC
01946      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
01947         THRU 9200-EXIT.                                           ELTHEARC
01948                                                                   ELTHEARC
01949  3170-EXIT.  EXIT.                                                ELTHEARC
01950 /                                                                 ELTHEARC
01951  3180-ALL-LEVEL-TABS.                                             ELTHEARC
01952 *--------------------------------------------------------------*  ELTHEARC
01953 *                  A B M   T A B U L A R                       *  ELTHEARC
01954 *--------------------------------------------------------------*  ELTHEARC
01955      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01956      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
01957                             AND                                   ELTHEARC
01958         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01959                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01960         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01961                                                  KWA-GCTABULR-KEY ELTHEARC
01962         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01963            THRU 9900-EXIT                                         ELTHEARC
01964         IF IOP-RC-OK                                              ELTHEARC
01965            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTHEARC
01966                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01967            END-EXEC                                               ELTHEARC
01968         ELSE                                                      ELTHEARC
01969            NEXT SENTENCE                                          ELTHEARC
01970      ELSE                                                         ELTHEARC
01971         SET PLT-INDEX2 TO 2                                       ELTHEARC
01972         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
01973            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
01974                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01975          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
01976                                                  KWA-GCTABULR-KEY ELTHEARC
01977            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
01978               THRU 9900-EXIT                                      ELTHEARC
01979            IF IOP-RC-OK                                           ELTHEARC
01980               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTHEARC
01981                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
01982               END-EXEC.                                           ELTHEARC
01983 *--------------------------------------------------------------*  ELTHEARC
01984 *                  A C L   T A B U L A R                       *  ELTHEARC
01985 *--------------------------------------------------------------*  ELTHEARC
01986      SET PLT-INDEX2 TO 1.                                         ELTHEARC
01987      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
01988         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
01989                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
01990         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
01991                                                  KWA-GCTABULR-KEY ELTHEARC
01992         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
01993            THRU 9900-EXIT                                         ELTHEARC
01994         IF IOP-RC-OK                                              ELTHEARC
01995            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTHEARC
01996                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
01997            END-EXEC                                               ELTHEARC
01998         ELSE                                                      ELTHEARC
01999            NEXT SENTENCE                                          ELTHEARC
02000      ELSE                                                         ELTHEARC
02001         SET PLT-INDEX2 TO 2                                       ELTHEARC
02002         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02003            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02004                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02005          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02006                                                  KWA-GCTABULR-KEY ELTHEARC
02007            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02008               THRU 9900-EXIT                                      ELTHEARC
02009            IF IOP-RC-OK                                           ELTHEARC
02010               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTHEARC
02011                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02012               END-EXEC.                                           ELTHEARC
02013 *--------------------------------------------------------------*  ELTHEARC
02014 *                  A D L   T A B U L A R                       *  ELTHEARC
02015 *--------------------------------------------------------------*  ELTHEARC
02016      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02017      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02018         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02019                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02020         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02021                                                  KWA-GCTABULR-KEY ELTHEARC
02022         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02023            THRU 9900-EXIT                                         ELTHEARC
02024         IF IOP-RC-OK                                              ELTHEARC
02025            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTHEARC
02026                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02027            END-EXEC                                               ELTHEARC
02028         ELSE                                                      ELTHEARC
02029            NEXT SENTENCE                                          ELTHEARC
02030      ELSE                                                         ELTHEARC
02031         SET PLT-INDEX2 TO 2                                       ELTHEARC
02032         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02033            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02034                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02035          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02036                                                  KWA-GCTABULR-KEY ELTHEARC
02037            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02038               THRU 9900-EXIT                                      ELTHEARC
02039            IF IOP-RC-OK                                           ELTHEARC
02040               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTHEARC
02041                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02042               END-EXEC.                                           ELTHEARC
02043 *--------------------------------------------------------------*  ELTHEARC
02044 *                  A O L   T A B U L A R                       *  ELTHEARC
02045 *--------------------------------------------------------------*  ELTHEARC
02046      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02047      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02048         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02049                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02050         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02051                                                  KWA-GCTABULR-KEY ELTHEARC
02052         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02053            THRU 9900-EXIT                                         ELTHEARC
02054         IF IOP-RC-OK                                              ELTHEARC
02055            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTHEARC
02056                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02057            END-EXEC                                               ELTHEARC
02058         ELSE                                                      ELTHEARC
02059            NEXT SENTENCE                                          ELTHEARC
02060      ELSE                                                         ELTHEARC
02061         SET PLT-INDEX2 TO 2                                       ELTHEARC
02062         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02063            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02064                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02065          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02066                                                  KWA-GCTABULR-KEY ELTHEARC
02067            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02068               THRU 9900-EXIT                                      ELTHEARC
02069            IF IOP-RC-OK                                           ELTHEARC
02070               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTHEARC
02071                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02072               END-EXEC.                                           ELTHEARC
02073  3180-EXIT.  EXIT.                                                ELTHEARC
02074 /                                                                 ELTHEARC
02075  4000-PROFESSIONAL-IP.                                            ELTHEARC
02076 ****************************************************************  ELTHEARC
02077 *       HEARING CARE PROFESSIONAL INPATIENT PROCESSING         *  ELTHEARC
02078 ****************************************************************  ELTHEARC
02079                                                                   ELTHEARC
02080      MOVE 'Y'   TO WS-FIRSTTIME-IND.                              ELTHEARC
02081      MOVE HEADER-P-IP-LINE-3 TO COF-HDR-LINE (2).                 ELTHEARC
02082                                                                   ELTHEARC
02083      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTHEARC
02084         THRU 9100-EXIT.                                           ELTHEARC
02085                                                                   ELTHEARC
02086      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTHEARC
02087                                                                   ELTHEARC
02088      PERFORM 4010-MOVE-IN-PROF-IP-TABS                            ELTHEARC
02089         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
02090                        UNTIL   WS-SUB > WS-PROF-IP-CNT.           ELTHEARC
02091                                                                   ELTHEARC
02092      PERFORM 4020-CALL-COVERAGE                                   ELTHEARC
02093         THRU 4020-EXIT.                                           ELTHEARC
02094                                                                   ELTHEARC
02095      IF PVN-COVG-NONE                                             ELTHEARC
02096          GO TO 4000-EXIT.                                         ELTHEARC
02097                                                                   ELTHEARC
02098      PERFORM 4030-FIND-FIRST-NONZERO                              ELTHEARC
02099         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
02100                        UNTIL   WS-SUB > WS-PROF-IP-CNT.           ELTHEARC
02101                                                                   ELTHEARC
02102  4000-EXIT.  EXIT.                                                ELTHEARC
02103 /                                                                 ELTHEARC
02104  4010-MOVE-IN-PROF-IP-TABS.                                       ELTHEARC
02105      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
02106      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTHEARC
02107              TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).             ELTHEARC
02108                                                                   ELTHEARC
02109      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),           ELTHEARC
02110                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTHEARC
02111                                                                   ELTHEARC
02112  4010-EXIT.  EXIT.                                                ELTHEARC
02113                                                                   ELTHEARC
02114  4020-CALL-COVERAGE.                                              ELTHEARC
02115                                                                   ELTHEARC
02116      MOVE 'HEARING CARE ' TO SSB-TOPIC-PHRASE.                    ELTHEARC
02117                                                                   ELTHEARC
02118      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTHEARC
02119                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02120                     END-EXEC.                                     ELTHEARC
02121                                                                   ELTHEARC
02122      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
02123                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02124                     END-EXEC.                                     ELTHEARC
02125                                                                   ELTHEARC
02126      IF PVN-COVG-NONE                                             ELTHEARC
02127          GO TO 4020-EXIT.                                         ELTHEARC
02128                                                                   ELTHEARC
02129      MOVE +1 TO WS-CIA.                                           ELTHEARC
02130                                                                   ELTHEARC
02131      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTHEARC
02132                                                                   ELTHEARC
02133      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTHEARC
02134                    PSP-PROVN-PRICING-METHD,                       ELTHEARC
02135                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTHEARC
02136                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTHEARC
02137                    PSP-TRANSF-OTHER-RESP-IND,                     ELTHEARC
02138                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTHEARC
02139                    PSP-SPILL-OVER-DED-APL-IND,                    ELTHEARC
02140                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTHEARC
02141                    PSP-CERTFN-REQRM-IND,                          ELTHEARC
02142                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTHEARC
02143                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTHEARC
02144                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTHEARC
02145                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTHEARC
02146                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTHEARC
02147                    PSP-BEN-TAB-PROVN-ID-PVE,                      ELTHEARC
02148                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTHEARC
02149                    PSE-EXCP-SCHED-ID,                             ELTHEARC
02150                    PSE-CERTN-REPETN-REQRD-IND.                    ELTHEARC
02151                                                                   ELTHEARC
02152      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTHEARC
02153                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02154                     END-EXEC.                                     ELTHEARC
02155                                                                   ELTHEARC
02156      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTHEARC
02157      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
02158              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTHEARC
02159                                                                   ELTHEARC
02160                                                                   ELTHEARC
02161  4020-EXIT.  EXIT.                                                ELTHEARC
02162                                                                   ELTHEARC
02163  4030-FIND-FIRST-NONZERO.                                         ELTHEARC
02164      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
02165                                                                   ELTHEARC
02166      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTHEARC
02167          NEXT SENTENCE                                            ELTHEARC
02168      ELSE                                                         ELTHEARC
02169          PERFORM 4100-BUILD-SCREEN-LINES                          ELTHEARC
02170             THRU 4100-EXIT.                                       ELTHEARC
02171                                                                   ELTHEARC
02172  4030-EXIT.  EXIT.                                                ELTHEARC
02173 /                                                                 ELTHEARC
02174  4100-BUILD-SCREEN-LINES.                                         ELTHEARC
02175      SET PLT-INDEX1 TO                                            ELTHEARC
02176              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTHEARC
02177                                                                   ELTHEARC
02178      IF WS-NOT-FIRST-TIME                                         ELTHEARC
02179         MOVE 'P'    TO COF-FUNCTION                               ELTHEARC
02180         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTHEARC
02181         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
02182                          COMMAREA(DFHCOMMAREA)                    ELTHEARC
02183         END-EXEC                                                  ELTHEARC
02184      ELSE                                                         ELTHEARC
02185        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTHEARC
02186                                                                   ELTHEARC
02187      MOVE +1 TO WS-CIA.                                           ELTHEARC
02188                                                                   ELTHEARC
02189      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTHEARC
02190          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTHEARC
02191              SET PLT-INDEX2 TO 2                                  ELTHEARC
02192          ELSE                                                     ELTHEARC
02193              MOVE WS-INDICES-PROBLEM TO COF-DTL-LINE (1)          ELTHEARC
02194              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTHEARC
02195                 THRU 9200-EXIT                                    ELTHEARC
02196              GO TO 4100-EXIT                                      ELTHEARC
02197      ELSE                                                         ELTHEARC
02198          SET PLT-INDEX2 TO 1.                                     ELTHEARC
02199                                                                   ELTHEARC
02200      PERFORM 4105-LIST-BEN-PROV                                   ELTHEARC
02201         THRU 4105-EXIT.                                           ELTHEARC
02202                                                                   ELTHEARC
02203      PERFORM 4110-PLACE-OF-TREATMENT                              ELTHEARC
02204         THRU 4110-EXIT.                                           ELTHEARC
02205                                                                   ELTHEARC
02206      PERFORM 4120-PRIC-METH                                       ELTHEARC
02207         THRU 4120-EXIT.                                           ELTHEARC
02208                                                                   ELTHEARC
02209      PERFORM 4125-EXCEPTION-SCHED                                 ELTHEARC
02210         THRU 4125-EXIT.                                           ELTHEARC
02211                                                                   ELTHEARC
02212      PERFORM 4140-CERTIFICATION                                   ELTHEARC
02213         THRU 4140-EXIT.                                           ELTHEARC
02214                                                                   ELTHEARC
02215      PERFORM 4150-RECERTIFICATION                                 ELTHEARC
02216         THRU 4150-EXIT.                                           ELTHEARC
02217                                                                   ELTHEARC
02218      PERFORM 4155-SERVC-NEC                                       ELTHEARC
02219         THRU 4155-EXIT.                                           ELTHEARC
02220                                                                   ELTHEARC
02221      PERFORM 4160-SPILLOVR-COINS-N-DEDUC                          ELTHEARC
02222         THRU 4160-EXIT.                                           ELTHEARC
02223                                                                   ELTHEARC
02224      PERFORM 4170-AAR-PPF-PVE-TABS                                ELTHEARC
02225         THRU 4170-EXIT.                                           ELTHEARC
02226                                                                   ELTHEARC
02227      PERFORM 4180-ALL-LEVEL-TABS                                  ELTHEARC
02228         THRU 4180-EXIT.                                           ELTHEARC
02229                                                                   ELTHEARC
02230      PERFORM 6000-PAY-CONSID-TEXT THRU 6000-EXIT.                 ELTHEARC
02231                                                                   ELTHEARC
02232      PERFORM 6100-TRANSF-OTHER-RESP-IND  THRU 6100-EXIT.          ELTHEARC
02233                                                                   ELTHEARC
02234  4100-EXIT.  EXIT.                                                ELTHEARC
02235 /                                                                 ELTHEARC
02236  4105-LIST-BEN-PROV.                                              ELTHEARC
02237 ****************************************************************  ELTHEARC
02238 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTHEARC
02239 ****************************************************************  ELTHEARC
02240      MOVE +2              TO WS-CIA.                              ELTHEARC
02241      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE (WS-CIA).              ELTHEARC
02242      MOVE ZERO            TO WS-SUB2.                             ELTHEARC
02243      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTHEARC
02244                           TO WS-SUB3.                             ELTHEARC
02245                                                                   ELTHEARC
02246      PERFORM 4106-ZERO-ALL-WITH-SAME-NO                           ELTHEARC
02247         THRU 4106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTHEARC
02248                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.ELTHEARC
02249                                                                   ELTHEARC
02250      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
02251      MOVE WS-CIA   TO COF-NBR-DTL-LINES.                          ELTHEARC
02252                                                                   ELTHEARC
02253      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
02254                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02255                     END-EXEC.                                     ELTHEARC
02256                                                                   ELTHEARC
02257  4105-EXIT.  EXIT.                                                ELTHEARC
02258                                                                   ELTHEARC
02259  4106-ZERO-ALL-WITH-SAME-NO.                                      ELTHEARC
02260                                                                   ELTHEARC
02261      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTHEARC
02262          MOVE 'BP'       TO CMF-RECORD-PREFIX                     ELTHEARC
02263          MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME              ELTHEARC
02264          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTHEARC
02265                          TO CMF-CODE-VALUE                        ELTHEARC
02266          MOVE SPACES     TO WS-TEMP-TEXT-AREA                     ELTHEARC
02267          MOVE +58        TO WS-TEMP-NOT-USED-CNT                  ELTHEARC
02268          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02269             THRU 9500-EXIT                                        ELTHEARC
02270          MOVE ZERO TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)        ELTHEARC
02271          ADD +1   TO WS-SUB2                                      ELTHEARC
02272          IF WS-CIA > 20 OR = 20                                   ELTHEARC
02273              MOVE WS-CIA TO COF-NBR-DTL-LINES                     ELTHEARC
02274              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTHEARC
02275                             COMMAREA (DFHCOMMAREA)                ELTHEARC
02276                             END-EXEC                              ELTHEARC
02277              MOVE +1 TO WS-CIA.                                   ELTHEARC
02278                                                                   ELTHEARC
02279  4106-EXIT.  EXIT.                                                ELTHEARC
02280 /                                                                 ELTHEARC
02281  4110-PLACE-OF-TREATMENT.                                         ELTHEARC
02282 ****************************************************************  ELTHEARC
02283 *              P L A C E   O F   T R E A T M E N T             *  ELTHEARC
02284 ****************************************************************  ELTHEARC
02285      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02286      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02287                         AND                                       ELTHEARC
02288         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
02289                                                 NOT = ZERO        ELTHEARC
02290          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02291          MOVE +2                  TO WS-CIA                       ELTHEARC
02292          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
02293                                                                   ELTHEARC
02294      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02295      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02296                          AND                                      ELTHEARC
02297         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
02298                                                NOT = ZERO         ELTHEARC
02299                          AND                                      ELTHEARC
02300        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02301          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02302          MOVE +2                  TO WS-CIA                       ELTHEARC
02303          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
02304                                                                   ELTHEARC
02305      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02306      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02307                          AND                                      ELTHEARC
02308         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
02309                                                 NOT = ZERO        ELTHEARC
02310          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
02311          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
02312                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02313          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
02314                             TO CMF-CODE-VALUE                     ELTHEARC
02315          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
02316          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02317          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02318             THRU 9500-EXIT.                                       ELTHEARC
02319                                                                   ELTHEARC
02320      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02321      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02322                          AND                                      ELTHEARC
02323         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
02324                                                NOT = ZERO         ELTHEARC
02325          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
02326          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
02327                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02328          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
02329                             TO CMF-CODE-VALUE                     ELTHEARC
02330          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
02331          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02332          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02333             THRU 9500-EXIT.                                       ELTHEARC
02334                                                                   ELTHEARC
02335      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02336         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02337          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02338             THRU 9200-EXIT.                                       ELTHEARC
02339                                                                   ELTHEARC
02340      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02341         SET PLT-INDEX2 TO 2                                       ELTHEARC
02342      ELSE                                                         ELTHEARC
02343         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02344                                                                   ELTHEARC
02345  4110-EXIT.  EXIT.                                                ELTHEARC
02346 /                                                                 ELTHEARC
02347  4120-PRIC-METH.                                                  ELTHEARC
02348 ****************************************************************  ELTHEARC
02349 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTHEARC
02350 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTHEARC
02351 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTHEARC
02352 ****************************************************************  ELTHEARC
02353      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02354      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02355         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
02356                                                              '19' ELTHEARC
02357         MOVE +2           TO WS-CIA                               ELTHEARC
02358         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
02359         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
02360                                                                   ELTHEARC
02361      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02362      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02363         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
02364                                                        '19' AND   ELTHEARC
02365        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02366         MOVE +2           TO WS-CIA                               ELTHEARC
02367         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
02368         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
02369                                                                   ELTHEARC
02370      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02371      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02372         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
02373                           AND                                     ELTHEARC
02374         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02375         SET PLT-INDEX2 TO 2                                       ELTHEARC
02376         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
02377                                                             ZERO  ELTHEARC
02378            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
02379            ADD +1 TO WS-CIA                                       ELTHEARC
02380            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
02381                                                                   ELTHEARC
02382      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02383      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02384         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
02385                           AND                                     ELTHEARC
02386         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTHEARC
02387         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTHEARC
02388         ADD +1 TO WS-CIA                                          ELTHEARC
02389         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                    ELTHEARC
02390                                                                   ELTHEARC
02391      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTHEARC
02392         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02393         SET PLT-INDEX2 TO 2                                       ELTHEARC
02394         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
02395                                                             ZERO  ELTHEARC
02396            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
02397            ADD +1 TO WS-CIA                                       ELTHEARC
02398            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
02399                                                                   ELTHEARC
02400      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02401      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02402         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
02403                                                         = ZERO    ELTHEARC
02404            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02405                                                         = ZERO    ELTHEARC
02406               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
02407            ELSE                                                   ELTHEARC
02408               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
02409          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02410                                                TO WS-PERCENTAGE   ELTHEARC
02411         ELSE                                                      ELTHEARC
02412            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
02413          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02414                                               TO WS-PERCENTAGE.   ELTHEARC
02415      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02416         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
02417                                             ZERO AND NOT = '19'   ELTHEARC
02418         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
02419         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
02420         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
02421                                                    CMF-CODE-VALUE ELTHEARC
02422         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
02423         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
02424         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
02425            THRU 9600-EXIT.                                        ELTHEARC
02426                                                                   ELTHEARC
02427      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02428      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02429         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTHEARC
02430                                                               ZEROELTHEARC
02431            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02432                                                         = ZERO    ELTHEARC
02433               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
02434            ELSE                                                   ELTHEARC
02435               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
02436          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02437                                                TO WS-PERCENTAGE   ELTHEARC
02438         ELSE                                                      ELTHEARC
02439            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
02440          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
02441                                               TO WS-PERCENTAGE.   ELTHEARC
02442      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02443         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
02444                                             ZERO AND NOT = '19'   ELTHEARC
02445         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
02446         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
02447         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
02448                                                    CMF-CODE-VALUE ELTHEARC
02449         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTHEARC
02450         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
02451         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
02452            THRU 9600-EXIT.                                        ELTHEARC
02453                                                                   ELTHEARC
02454      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02455          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTHEARC
02456          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02457             THRU 9200-EXIT.                                       ELTHEARC
02458                                                                   ELTHEARC
02459      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02460         SET PLT-INDEX2 TO 2                                       ELTHEARC
02461      ELSE                                                         ELTHEARC
02462         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02463                                                                   ELTHEARC
02464  4120-EXIT.  EXIT.                                                ELTHEARC
02465 /                                                                 ELTHEARC
02466  4125-EXCEPTION-SCHED.                                            ELTHEARC
02467 ****************************************************************  ELTHEARC
02468 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTHEARC
02469 ****************************************************************  ELTHEARC
02470      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02471      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02472                         AND                                       ELTHEARC
02473         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
02474                                                 NOT = ZERO        ELTHEARC
02475          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02476          MOVE +2                  TO WS-CIA                       ELTHEARC
02477          MOVE WS-EXCEPTION-SCHED  TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02478                                                                   ELTHEARC
02479      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02480      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02481                          AND                                      ELTHEARC
02482         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
02483                                                NOT = ZERO         ELTHEARC
02484                          AND                                      ELTHEARC
02485        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02486          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02487          MOVE +2                  TO WS-CIA                       ELTHEARC
02488          MOVE WS-EXCEPTION-SCHED  TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02489                                                                   ELTHEARC
02490      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02491      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02492                          AND                                      ELTHEARC
02493         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
02494                                                 NOT = ZERO        ELTHEARC
02495          ADD    +1  TO  WS-CIA                                    ELTHEARC
02496          STRING         WS-BASIC-LIT                              ELTHEARC
02497                         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)ELTHEARC
02498                         DELIMITED BY SIZE                         ELTHEARC
02499          INTO COF-DTL-LINE (WS-CIA)                               ELTHEARC
02500                                                                   ELTHEARC
02501      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02502      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02503                          AND                                      ELTHEARC
02504         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
02505                                                NOT = ZERO         ELTHEARC
02506          ADD    +1  TO  WS-CIA                                    ELTHEARC
02507          STRING         WS-SUPP-LIT                               ELTHEARC
02508                         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)ELTHEARC
02509                         DELIMITED BY SIZE                         ELTHEARC
02510          INTO COF-DTL-LINE (WS-CIA)                               ELTHEARC
02511                                                                   ELTHEARC
02512      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02513         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02514          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02515             THRU 9200-EXIT.                                       ELTHEARC
02516                                                                   ELTHEARC
02517      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02518         SET PLT-INDEX2 TO 2                                       ELTHEARC
02519      ELSE                                                         ELTHEARC
02520         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02521                                                                   ELTHEARC
02522  4125-EXIT.  EXIT.                                                ELTHEARC
02523 /                                                                 ELTHEARC
02524  4140-CERTIFICATION.                                              ELTHEARC
02525 ****************************************************************  ELTHEARC
02526 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTHEARC
02527 ****************************************************************  ELTHEARC
02528      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02529      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02530                         AND                                       ELTHEARC
02531         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
02532                                                 NOT = '00'        ELTHEARC
02533          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02534          MOVE +2                  TO WS-CIA                       ELTHEARC
02535          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02536                                                                   ELTHEARC
02537      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02538      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02539                          AND                                      ELTHEARC
02540         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
02541                                                NOT = '00'         ELTHEARC
02542                          AND                                      ELTHEARC
02543        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02544          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02545          MOVE +2                  TO WS-CIA                       ELTHEARC
02546          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02547                                                                   ELTHEARC
02548      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02549      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02550                          AND                                      ELTHEARC
02551         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
02552                                                 NOT = '00'        ELTHEARC
02553          MOVE 'BP'  TO CMF-RECORD-PREFIX                          ELTHEARC
02554          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
02555                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02556          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02557                             TO CMF-CODE-VALUE                     ELTHEARC
02558          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
02559          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02560          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02561             THRU 9500-EXIT.                                       ELTHEARC
02562                                                                   ELTHEARC
02563      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02564      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02565                          AND                                      ELTHEARC
02566         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
02567                                                NOT = '00'         ELTHEARC
02568          MOVE 'BP'          TO CMF-RECORD-PREFIX                  ELTHEARC
02569          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
02570                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02571          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02572                             TO CMF-CODE-VALUE                     ELTHEARC
02573          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
02574          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02575          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02576             THRU 9500-EXIT.                                       ELTHEARC
02577                                                                   ELTHEARC
02578      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02579         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02580          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02581             THRU 9200-EXIT.                                       ELTHEARC
02582                                                                   ELTHEARC
02583      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02584         SET PLT-INDEX2 TO 2                                       ELTHEARC
02585      ELSE                                                         ELTHEARC
02586         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02587                                                                   ELTHEARC
02588  4140-EXIT.  EXIT.                                                ELTHEARC
02589 /                                                                 ELTHEARC
02590  4150-RECERTIFICATION.                                            ELTHEARC
02591 ****************************************************************  ELTHEARC
02592 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTHEARC
02593 ****************************************************************  ELTHEARC
02594      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02595      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02596                         AND                                       ELTHEARC
02597         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02598                                                 NOT = ZERO        ELTHEARC
02599          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02600          MOVE +2                  TO WS-CIA                       ELTHEARC
02601          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02602                                                                   ELTHEARC
02603      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02604      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02605                          AND                                      ELTHEARC
02606         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02607                                                NOT = ZERO         ELTHEARC
02608                          AND                                      ELTHEARC
02609        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02610          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02611          MOVE +2                  TO WS-CIA                       ELTHEARC
02612          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
02613                                                                   ELTHEARC
02614      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02615      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02616                          AND                                      ELTHEARC
02617         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02618                                                 NOT = ZERO        ELTHEARC
02619          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTHEARC
02620          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
02621                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02622          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
02623                             TO CMF-CODE-VALUE                     ELTHEARC
02624          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
02625          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02626          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02627             THRU 9500-EXIT.                                       ELTHEARC
02628                                                                   ELTHEARC
02629      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02630      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02631                          AND                                      ELTHEARC
02632         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02633                                                NOT = ZERO         ELTHEARC
02634          MOVE 'BPE'         TO CMF-RECORD-PREFIX                  ELTHEARC
02635          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
02636                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
02637          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
02638                             TO CMF-CODE-VALUE                     ELTHEARC
02639          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
02640          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
02641          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
02642             THRU 9500-EXIT.                                       ELTHEARC
02643                                                                   ELTHEARC
02644      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02645         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02646          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02647             THRU 9200-EXIT.                                       ELTHEARC
02648                                                                   ELTHEARC
02649      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02650         SET PLT-INDEX2 TO 2                                       ELTHEARC
02651      ELSE                                                         ELTHEARC
02652         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02653                                                                   ELTHEARC
02654  4150-EXIT.  EXIT.                                                ELTHEARC
02655 /                                                                 ELTHEARC
02656  4155-SERVC-NEC.                                                  ELTHEARC
02657 ****************************************************************  ELTHEARC
02658 * S E R V I C E   N E C E S S A R Y   B I T   I N D I C A T O R*  ELTHEARC
02659 ****************************************************************  ELTHEARC
02660      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02661      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02662                         AND                                       ELTHEARC
02663         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
02664                                                   = '1'           ELTHEARC
02665          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02666          MOVE +3                  TO WS-CIA                       ELTHEARC
02667          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
02668          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
02669                                                                   ELTHEARC
02670      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02671      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
02672                          AND                                      ELTHEARC
02673         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
02674                                                   = '1'           ELTHEARC
02675                          AND                                      ELTHEARC
02676        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
02677          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
02678          MOVE +3                  TO WS-CIA                       ELTHEARC
02679          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
02680          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
02681                                                                   ELTHEARC
02682      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02683         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02684          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
02685             THRU 9200-EXIT.                                       ELTHEARC
02686                                                                   ELTHEARC
02687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02688         SET PLT-INDEX2 TO 2                                       ELTHEARC
02689      ELSE                                                         ELTHEARC
02690         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02691                                                                   ELTHEARC
02692  4155-EXIT.  EXIT.                                                ELTHEARC
02693 /                                                                 ELTHEARC
02694  4160-SPILLOVR-COINS-N-DEDUC.                                     ELTHEARC
02695 ****************************************************************  ELTHEARC
02696 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTHEARC
02697 ****************************************************************  ELTHEARC
02698      MOVE +1 TO WS-CIA.                                           ELTHEARC
02699                                                                   ELTHEARC
02700      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02701      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
02702                           AND                                     ELTHEARC
02703         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTHEARC
02704                                                 NOT = '0'         ELTHEARC
02705         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
02706         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTHEARC
02707                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
02708         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTHEARC
02709                                              TO CMF-CODE-VALUE    ELTHEARC
02710         MOVE WS-SPILLOVER      TO WS-TEMP-TEXT-AREA               ELTHEARC
02711         MOVE +69               TO WS-TEMP-NOT-USED-CNT            ELTHEARC
02712         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
02713            THRU 9500-EXIT                                         ELTHEARC
02714         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
02715            THRU 9200-EXIT.                                        ELTHEARC
02716 ****************************************************************  ELTHEARC
02717 *          S P I L L O V E R   D E D U C T I B L E             *  ELTHEARC
02718 ****************************************************************  ELTHEARC
02719      MOVE +1 TO WS-CIA.                                           ELTHEARC
02720                                                                   ELTHEARC
02721      SET PLT-INDEX2 TO 2.                                         ELTHEARC
02722      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
02723                           AND                                     ELTHEARC
02724         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
02725                                                 NOT = '0'         ELTHEARC
02726         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
02727         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTHEARC
02728                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
02729         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
02730                                               TO CMF-CODE-VALUE   ELTHEARC
02731         MOVE WS-SPILLOVER    TO WS-TEMP-TEXT-AREA                 ELTHEARC
02732         MOVE +69             TO WS-TEMP-NOT-USED-CNT              ELTHEARC
02733         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
02734            THRU 9500-EXIT                                         ELTHEARC
02735         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
02736            THRU 9200-EXIT.                                        ELTHEARC
02737                                                                   ELTHEARC
02738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
02739         SET PLT-INDEX2 TO 2                                       ELTHEARC
02740      ELSE                                                         ELTHEARC
02741         SET PLT-INDEX2 TO 1.                                      ELTHEARC
02742                                                                   ELTHEARC
02743  4160-EXIT.  EXIT.                                                ELTHEARC
02744                                                                   ELTHEARC
02745  4170-AAR-PPF-PVE-TABS.                                           ELTHEARC
02746 ****************************************************************  ELTHEARC
02747 *                  A A R   T A B U L A R                       *  ELTHEARC
02748 ****************************************************************  ELTHEARC
02749      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02750      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02751         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02752                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02753         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02754         MOVE +1 TO COF-NBR-DTL-LINES                              ELTHEARC
02755         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTHEARC
02756      ELSE                                                         ELTHEARC
02757         SET PLT-INDEX2 TO 2                                       ELTHEARC
02758         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02759            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02760                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02761            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTHEARC
02762            MOVE +1 TO COF-NBR-DTL-LINES                           ELTHEARC
02763            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTHEARC
02764                                                                   ELTHEARC
02765      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
02766         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
02767         MOVE 1 TO WS-CIA                                          ELTHEARC
02768         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
02769             COMMAREA(DFHCOMMAREA)                                 ELTHEARC
02770         END-EXEC.                                                 ELTHEARC
02771 *--------------------------------------------------------------*  ELTHEARC
02772 *                  P P F   T A B U L A R                       *  ELTHEARC
02773 *--------------------------------------------------------------*  ELTHEARC
02774      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02775      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02776         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02777                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02778         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02779                                                  KWA-GCTABULR-KEY ELTHEARC
02780         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02781            THRU 9900-EXIT                                         ELTHEARC
02782         IF IOP-RC-OK                                              ELTHEARC
02783            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTHEARC
02784                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02785            END-EXEC                                               ELTHEARC
02786         ELSE                                                      ELTHEARC
02787            NEXT SENTENCE                                          ELTHEARC
02788      ELSE                                                         ELTHEARC
02789         SET PLT-INDEX2 TO 2                                       ELTHEARC
02790         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02791            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02792                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02793          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02794                                                  KWA-GCTABULR-KEY ELTHEARC
02795            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02796               THRU 9900-EXIT                                      ELTHEARC
02797            IF IOP-RC-OK                                           ELTHEARC
02798               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTHEARC
02799                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02800               END-EXEC.                                           ELTHEARC
02801 *--------------------------------------------------------------*  ELTHEARC
02802 *                  P V E   T A B U L A R                       *  ELTHEARC
02803 *--------------------------------------------------------------*  ELTHEARC
02804                                                                   ELTHEARC
02805      MOVE +2    TO WS-CIA.                                        ELTHEARC
02806      MOVE WS-PVE TO COF-DTL-LINE (2).                             ELTHEARC
02807      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
02808         THRU 9200-EXIT.                                           ELTHEARC
02809                                                                   ELTHEARC
02810  4170-EXIT.  EXIT.                                                ELTHEARC
02811 /                                                                 ELTHEARC
02812  4180-ALL-LEVEL-TABS.                                             ELTHEARC
02813 *--------------------------------------------------------------*  ELTHEARC
02814 *                  A B M   T A B U L A R                       *  ELTHEARC
02815 *--------------------------------------------------------------*  ELTHEARC
02816      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02817      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
02818                             AND                                   ELTHEARC
02819         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02820                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02821         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02822                                                  KWA-GCTABULR-KEY ELTHEARC
02823         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02824            THRU 9900-EXIT                                         ELTHEARC
02825         IF IOP-RC-OK                                              ELTHEARC
02826            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTHEARC
02827                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02828            END-EXEC                                               ELTHEARC
02829         ELSE                                                      ELTHEARC
02830            NEXT SENTENCE                                          ELTHEARC
02831      ELSE                                                         ELTHEARC
02832         SET PLT-INDEX2 TO 2                                       ELTHEARC
02833         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02834            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02835                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02836          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02837                                                  KWA-GCTABULR-KEY ELTHEARC
02838            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02839               THRU 9900-EXIT                                      ELTHEARC
02840            IF IOP-RC-OK                                           ELTHEARC
02841               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTHEARC
02842                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02843               END-EXEC.                                           ELTHEARC
02844 *--------------------------------------------------------------*  ELTHEARC
02845 *                  A C L   T A B U L A R                       *  ELTHEARC
02846 *--------------------------------------------------------------*  ELTHEARC
02847      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02848      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02849         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02850                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02851         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02852                                                  KWA-GCTABULR-KEY ELTHEARC
02853         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02854            THRU 9900-EXIT                                         ELTHEARC
02855         IF IOP-RC-OK                                              ELTHEARC
02856            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTHEARC
02857                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02858            END-EXEC                                               ELTHEARC
02859         ELSE                                                      ELTHEARC
02860            NEXT SENTENCE                                          ELTHEARC
02861      ELSE                                                         ELTHEARC
02862         SET PLT-INDEX2 TO 2                                       ELTHEARC
02863         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02864            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02865                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02866          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02867                                                  KWA-GCTABULR-KEY ELTHEARC
02868            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02869               THRU 9900-EXIT                                      ELTHEARC
02870            IF IOP-RC-OK                                           ELTHEARC
02871               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTHEARC
02872                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02873               END-EXEC.                                           ELTHEARC
02874 *--------------------------------------------------------------*  ELTHEARC
02875 *                  A D L   T A B U L A R                       *  ELTHEARC
02876 *--------------------------------------------------------------*  ELTHEARC
02877      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02878      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02879         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02880                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02881         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02882                                                  KWA-GCTABULR-KEY ELTHEARC
02883         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02884            THRU 9900-EXIT                                         ELTHEARC
02885         IF IOP-RC-OK                                              ELTHEARC
02886            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTHEARC
02887                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02888            END-EXEC                                               ELTHEARC
02889         ELSE                                                      ELTHEARC
02890            NEXT SENTENCE                                          ELTHEARC
02891      ELSE                                                         ELTHEARC
02892         SET PLT-INDEX2 TO 2                                       ELTHEARC
02893         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02894            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02895                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02896          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02897                                                  KWA-GCTABULR-KEY ELTHEARC
02898            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02899               THRU 9900-EXIT                                      ELTHEARC
02900            IF IOP-RC-OK                                           ELTHEARC
02901               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTHEARC
02902                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02903               END-EXEC.                                           ELTHEARC
02904 *--------------------------------------------------------------*  ELTHEARC
02905 *                  A O L   T A B U L A R                       *  ELTHEARC
02906 *--------------------------------------------------------------*  ELTHEARC
02907      SET PLT-INDEX2 TO 1.                                         ELTHEARC
02908      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
02909         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
02910                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02911         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
02912                                                  KWA-GCTABULR-KEY ELTHEARC
02913         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
02914            THRU 9900-EXIT                                         ELTHEARC
02915         IF IOP-RC-OK                                              ELTHEARC
02916            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTHEARC
02917                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
02918            END-EXEC                                               ELTHEARC
02919         ELSE                                                      ELTHEARC
02920            NEXT SENTENCE                                          ELTHEARC
02921      ELSE                                                         ELTHEARC
02922         SET PLT-INDEX2 TO 2                                       ELTHEARC
02923         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
02924            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
02925                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
02926          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
02927                                                  KWA-GCTABULR-KEY ELTHEARC
02928            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
02929               THRU 9900-EXIT                                      ELTHEARC
02930            IF IOP-RC-OK                                           ELTHEARC
02931               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTHEARC
02932                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
02933               END-EXEC.                                           ELTHEARC
02934  4180-EXIT.  EXIT.                                                ELTHEARC
02935 /                                                                 ELTHEARC
02936  5000-PROFESSIONAL-OP.                                            ELTHEARC
02937 ****************************************************************  ELTHEARC
02938 *      HEARING CARE PROFESSIONAL OUTPATIENT PROCESSING         *  ELTHEARC
02939 ****************************************************************  ELTHEARC
02940                                                                   ELTHEARC
02941      MOVE 'Y'   TO WS-FIRSTTIME-IND.                              ELTHEARC
02942      MOVE HEADER-P-OP-LINE-3 TO COF-HDR-LINE (2).                 ELTHEARC
02943                                                                   ELTHEARC
02944      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTHEARC
02945         THRU 9100-EXIT.                                           ELTHEARC
02946                                                                   ELTHEARC
02947      MOVE WS-PROF-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTHEARC
02948                                                                   ELTHEARC
02949      PERFORM 5010-MOVE-IN-PROF-OP-TABS                            ELTHEARC
02950         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
02951                        UNTIL   WS-SUB > WS-PROF-OP-CNT.           ELTHEARC
02952                                                                   ELTHEARC
02953      PERFORM 5020-CALL-COVERAGE                                   ELTHEARC
02954         THRU 5020-EXIT.                                           ELTHEARC
02955                                                                   ELTHEARC
02956      IF PVN-COVG-NONE                                             ELTHEARC
02957          GO TO 5000-EXIT.                                         ELTHEARC
02958                                                                   ELTHEARC
02959      PERFORM 5030-FIND-FIRST-NONZERO                              ELTHEARC
02960         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTHEARC
02961                        UNTIL   WS-SUB > WS-PROF-OP-CNT.           ELTHEARC
02962                                                                   ELTHEARC
02963  5000-EXIT.  EXIT.                                                ELTHEARC
02964 /                                                                 ELTHEARC
02965  5010-MOVE-IN-PROF-OP-TABS.                                       ELTHEARC
02966      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
02967      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTHEARC
02968              TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).             ELTHEARC
02969                                                                   ELTHEARC
02970      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),           ELTHEARC
02971                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTHEARC
02972                                                                   ELTHEARC
02973  5010-EXIT.  EXIT.                                                ELTHEARC
02974                                                                   ELTHEARC
02975  5020-CALL-COVERAGE.                                              ELTHEARC
02976                                                                   ELTHEARC
02977      MOVE 'HEARING CARE '  TO SSB-TOPIC-PHRASE.                   ELTHEARC
02978                                                                   ELTHEARC
02979      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTHEARC
02980                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02981                     END-EXEC.                                     ELTHEARC
02982                                                                   ELTHEARC
02983      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
02984                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
02985                     END-EXEC.                                     ELTHEARC
02986                                                                   ELTHEARC
02987      IF PVN-COVG-NONE                                             ELTHEARC
02988          GO TO 5020-EXIT.                                         ELTHEARC
02989                                                                   ELTHEARC
02990      MOVE +1 TO WS-CIA.                                           ELTHEARC
02991                                                                   ELTHEARC
02992      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTHEARC
02993                                                                   ELTHEARC
02994      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTHEARC
02995                    PSP-PROVN-PRICING-METHD,                       ELTHEARC
02996                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTHEARC
02997                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTHEARC
02998                    PSP-TRANSF-OTHER-RESP-IND,                     ELTHEARC
02999                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTHEARC
03000                    PSP-SPILL-OVER-DED-APL-IND,                    ELTHEARC
03001                    PSP-SERV-NECESRY-CORP-BIT-IND,                 ELTHEARC
03002                    PSP-CERTFN-REQRM-IND,                          ELTHEARC
03003                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTHEARC
03004                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTHEARC
03005                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTHEARC
03006                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTHEARC
03007                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTHEARC
03008                    PSP-BEN-TAB-PROVN-ID-PVE,                      ELTHEARC
03009                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTHEARC
03010                    PSE-EXCP-SCHED-ID,                             ELTHEARC
03011                    PSE-CERTN-REPETN-REQRD-IND.                    ELTHEARC
03012                                                                   ELTHEARC
03013      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTHEARC
03014                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
03015                     END-EXEC.                                     ELTHEARC
03016                                                                   ELTHEARC
03017      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTHEARC
03018      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
03019              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTHEARC
03020                                                                   ELTHEARC
03021                                                                   ELTHEARC
03022  5020-EXIT.  EXIT.                                                ELTHEARC
03023                                                                   ELTHEARC
03024  5030-FIND-FIRST-NONZERO.                                         ELTHEARC
03025      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
03026                                                                   ELTHEARC
03027      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTHEARC
03028          NEXT SENTENCE                                            ELTHEARC
03029      ELSE                                                         ELTHEARC
03030          PERFORM 5100-BUILD-SCREEN-LINES                          ELTHEARC
03031             THRU 5100-EXIT.                                       ELTHEARC
03032                                                                   ELTHEARC
03033  5030-EXIT.  EXIT.                                                ELTHEARC
03034 /                                                                 ELTHEARC
03035  5100-BUILD-SCREEN-LINES.                                         ELTHEARC
03036      SET PLT-INDEX1 TO                                            ELTHEARC
03037              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTHEARC
03038                                                                   ELTHEARC
03039      IF WS-NOT-FIRST-TIME                                         ELTHEARC
03040         MOVE 'P'    TO COF-FUNCTION                               ELTHEARC
03041         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTHEARC
03042         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
03043                          COMMAREA(DFHCOMMAREA)                    ELTHEARC
03044         END-EXEC                                                  ELTHEARC
03045      ELSE                                                         ELTHEARC
03046        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTHEARC
03047                                                                   ELTHEARC
03048      MOVE +1 TO WS-CIA.                                           ELTHEARC
03049                                                                   ELTHEARC
03050      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTHEARC
03051          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTHEARC
03052              SET PLT-INDEX2 TO 2                                  ELTHEARC
03053          ELSE                                                     ELTHEARC
03054              MOVE WS-INDICES-PROBLEM TO COF-DTL-LINE (1)          ELTHEARC
03055              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTHEARC
03056                 THRU 9200-EXIT                                    ELTHEARC
03057              GO TO 5100-EXIT                                      ELTHEARC
03058      ELSE                                                         ELTHEARC
03059          SET PLT-INDEX2 TO 1.                                     ELTHEARC
03060                                                                   ELTHEARC
03061      PERFORM 5105-LIST-BEN-PROV                                   ELTHEARC
03062         THRU 5105-EXIT.                                           ELTHEARC
03063                                                                   ELTHEARC
03064      PERFORM 5110-PLACE-OF-TREATMENT                              ELTHEARC
03065         THRU 5110-EXIT.                                           ELTHEARC
03066                                                                   ELTHEARC
03067      PERFORM 5120-PRIC-METH                                       ELTHEARC
03068         THRU 5120-EXIT.                                           ELTHEARC
03069                                                                   ELTHEARC
03070      PERFORM 5125-EXCEPTION-SCHED                                 ELTHEARC
03071         THRU 5125-EXIT.                                           ELTHEARC
03072                                                                   ELTHEARC
03073      PERFORM 5140-CERTIFICATION                                   ELTHEARC
03074         THRU 5140-EXIT.                                           ELTHEARC
03075                                                                   ELTHEARC
03076      PERFORM 5150-RECERTIFICATION                                 ELTHEARC
03077         THRU 5150-EXIT.                                           ELTHEARC
03078                                                                   ELTHEARC
03079      PERFORM 5155-SERVC-NEC                                       ELTHEARC
03080         THRU 5155-EXIT.                                           ELTHEARC
03081                                                                   ELTHEARC
03082      PERFORM 5160-SPILLOVR-COINS-N-DEDUC                          ELTHEARC
03083         THRU 5160-EXIT.                                           ELTHEARC
03084                                                                   ELTHEARC
03085      PERFORM 5170-AAR-PPF-PVE-TABS                                ELTHEARC
03086         THRU 5170-EXIT.                                           ELTHEARC
03087                                                                   ELTHEARC
03088      PERFORM 5180-ALL-LEVEL-TABS                                  ELTHEARC
03089         THRU 5180-EXIT.                                           ELTHEARC
03090                                                                   ELTHEARC
03091      PERFORM 6000-PAY-CONSID-TEXT THRU 6000-EXIT.                 ELTHEARC
03092                                                                   ELTHEARC
03093      PERFORM 6100-TRANSF-OTHER-RESP-IND  THRU 6100-EXIT.          ELTHEARC
03094                                                                   ELTHEARC
03095  5100-EXIT.  EXIT.                                                ELTHEARC
03096 /                                                                 ELTHEARC
03097  5105-LIST-BEN-PROV.                                              ELTHEARC
03098 ****************************************************************  ELTHEARC
03099 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTHEARC
03100 ****************************************************************  ELTHEARC
03101      MOVE +2              TO WS-CIA.                              ELTHEARC
03102      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE (WS-CIA).              ELTHEARC
03103      MOVE ZERO            TO WS-SUB2.                             ELTHEARC
03104      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTHEARC
03105                           TO WS-SUB3.                             ELTHEARC
03106                                                                   ELTHEARC
03107      PERFORM 5106-ZERO-ALL-WITH-SAME-NO                           ELTHEARC
03108         THRU 5106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTHEARC
03109                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.ELTHEARC
03110                                                                   ELTHEARC
03111      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTHEARC
03112      MOVE WS-CIA   TO COF-NBR-DTL-LINES.                          ELTHEARC
03113                                                                   ELTHEARC
03114      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
03115                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
03116                     END-EXEC.                                     ELTHEARC
03117                                                                   ELTHEARC
03118  5105-EXIT.  EXIT.                                                ELTHEARC
03119                                                                   ELTHEARC
03120  5106-ZERO-ALL-WITH-SAME-NO.                                      ELTHEARC
03121                                                                   ELTHEARC
03122      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTHEARC
03123          MOVE 'BP'       TO CMF-RECORD-PREFIX                     ELTHEARC
03124          MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME              ELTHEARC
03125          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTHEARC
03126                          TO CMF-CODE-VALUE                        ELTHEARC
03127          MOVE SPACES     TO WS-TEMP-TEXT-AREA                     ELTHEARC
03128          MOVE +58        TO WS-TEMP-NOT-USED-CNT                  ELTHEARC
03129          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03130             THRU 9500-EXIT                                        ELTHEARC
03131          MOVE ZERO TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)        ELTHEARC
03132          ADD +1   TO WS-SUB2                                      ELTHEARC
03133          IF WS-CIA > 20 OR = 20                                   ELTHEARC
03134              MOVE WS-CIA TO COF-NBR-DTL-LINES                     ELTHEARC
03135              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTHEARC
03136                             COMMAREA (DFHCOMMAREA)                ELTHEARC
03137                             END-EXEC                              ELTHEARC
03138              MOVE +1 TO WS-CIA.                                   ELTHEARC
03139                                                                   ELTHEARC
03140  5106-EXIT.  EXIT.                                                ELTHEARC
03141 /                                                                 ELTHEARC
03142  5110-PLACE-OF-TREATMENT.                                         ELTHEARC
03143 ****************************************************************  ELTHEARC
03144 *              P L A C E   O F   T R E A T M E N T             *  ELTHEARC
03145 ****************************************************************  ELTHEARC
03146      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03147      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03148                         AND                                       ELTHEARC
03149         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
03150                                                 NOT = ZERO        ELTHEARC
03151          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03152          MOVE +2                  TO WS-CIA                       ELTHEARC
03153          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
03154                                                                   ELTHEARC
03155      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03156      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03157                          AND                                      ELTHEARC
03158         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
03159                                                NOT = ZERO         ELTHEARC
03160                          AND                                      ELTHEARC
03161        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03162          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03163          MOVE +2                  TO WS-CIA                       ELTHEARC
03164          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA).      ELTHEARC
03165                                                                   ELTHEARC
03166      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03167      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03168                          AND                                      ELTHEARC
03169         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
03170                                                 NOT = ZERO        ELTHEARC
03171          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
03172          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
03173                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03174          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
03175                             TO CMF-CODE-VALUE                     ELTHEARC
03176          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
03177          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03178          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03179             THRU 9500-EXIT.                                       ELTHEARC
03180                                                                   ELTHEARC
03181      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03182      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03183                          AND                                      ELTHEARC
03184         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTHEARC
03185                                                NOT = ZERO         ELTHEARC
03186          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTHEARC
03187          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTHEARC
03188                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03189          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTHEARC
03190                             TO CMF-CODE-VALUE                     ELTHEARC
03191          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
03192          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03193          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03194             THRU 9500-EXIT.                                       ELTHEARC
03195                                                                   ELTHEARC
03196      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03197         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03198          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03199             THRU 9200-EXIT.                                       ELTHEARC
03200                                                                   ELTHEARC
03201      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03202         SET PLT-INDEX2 TO 2                                       ELTHEARC
03203      ELSE                                                         ELTHEARC
03204         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03205                                                                   ELTHEARC
03206  5110-EXIT.  EXIT.                                                ELTHEARC
03207 /                                                                 ELTHEARC
03208  5120-PRIC-METH.                                                  ELTHEARC
03209 ****************************************************************  ELTHEARC
03210 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTHEARC
03211 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTHEARC
03212 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTHEARC
03213 ****************************************************************  ELTHEARC
03214      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03215      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03216         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
03217                                                              '19' ELTHEARC
03218         MOVE +2           TO WS-CIA                               ELTHEARC
03219         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
03220         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
03221                                                                   ELTHEARC
03222      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03223      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03224         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
03225                                                        '19' AND   ELTHEARC
03226        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03227         MOVE +2           TO WS-CIA                               ELTHEARC
03228         MOVE 'Y'          TO WS-ADD-A-BLANK-IND                   ELTHEARC
03229         MOVE WS-PAYABLE-AS TO COF-DTL-LINE(WS-CIA).               ELTHEARC
03230                                                                   ELTHEARC
03231      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03232      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03233         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
03234                           AND                                     ELTHEARC
03235         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03236         SET PLT-INDEX2 TO 2                                       ELTHEARC
03237         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
03238                                                             ZERO  ELTHEARC
03239            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
03240            ADD +1 TO WS-CIA                                       ELTHEARC
03241            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
03242                                                                   ELTHEARC
03243      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03244      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03245         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTHEARC
03246                           AND                                     ELTHEARC
03247         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTHEARC
03248         MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                    ELTHEARC
03249         ADD +1 TO WS-CIA                                          ELTHEARC
03250         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                    ELTHEARC
03251                                                                   ELTHEARC
03252      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTHEARC
03253         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03254         SET PLT-INDEX2 TO 2                                       ELTHEARC
03255         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTHEARC
03256                                                             ZERO  ELTHEARC
03257            MOVE WS-POSSIBLE-ERROR TO WS-DTL-BASIC                 ELTHEARC
03258            ADD +1 TO WS-CIA                                       ELTHEARC
03259            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA).                 ELTHEARC
03260                                                                   ELTHEARC
03261      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03262      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03263         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
03264                                                         = ZERO    ELTHEARC
03265            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03266                                                         = ZERO    ELTHEARC
03267               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
03268            ELSE                                                   ELTHEARC
03269               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
03270          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03271                                                TO WS-PERCENTAGE   ELTHEARC
03272         ELSE                                                      ELTHEARC
03273            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
03274          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03275                                               TO WS-PERCENTAGE.   ELTHEARC
03276      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03277         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
03278                                             ZERO AND NOT = '19'   ELTHEARC
03279         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
03280         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
03281         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
03282                                                    CMF-CODE-VALUE ELTHEARC
03283         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
03284         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
03285         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
03286            THRU 9600-EXIT.                                        ELTHEARC
03287                                                                   ELTHEARC
03288      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03289      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03290         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTHEARC
03291                                                               ZEROELTHEARC
03292            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03293                                                         = ZERO    ELTHEARC
03294               MOVE SPACES TO WS-PERCENT-FLD                       ELTHEARC
03295            ELSE                                                   ELTHEARC
03296               MOVE '%' TO WS-PERCENT-SIGN                         ELTHEARC
03297          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03298                                                TO WS-PERCENTAGE   ELTHEARC
03299         ELSE                                                      ELTHEARC
03300            MOVE '%' TO WS-PERCENT-SIGN                            ELTHEARC
03301          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTHEARC
03302                                               TO WS-PERCENTAGE.   ELTHEARC
03303      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03304         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTHEARC
03305                                             ZERO AND NOT = '19'   ELTHEARC
03306         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
03307         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTHEARC
03308         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTHEARC
03309                                                    CMF-CODE-VALUE ELTHEARC
03310         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTHEARC
03311         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTHEARC
03312         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTHEARC
03313            THRU 9600-EXIT.                                        ELTHEARC
03314                                                                   ELTHEARC
03315      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03316          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTHEARC
03317          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03318             THRU 9200-EXIT.                                       ELTHEARC
03319                                                                   ELTHEARC
03320      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03321         SET PLT-INDEX2 TO 2                                       ELTHEARC
03322      ELSE                                                         ELTHEARC
03323         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03324                                                                   ELTHEARC
03325  5120-EXIT.  EXIT.                                                ELTHEARC
03326 /                                                                 ELTHEARC
03327  5125-EXCEPTION-SCHED.                                            ELTHEARC
03328 ****************************************************************  ELTHEARC
03329 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTHEARC
03330 ****************************************************************  ELTHEARC
03331      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03332      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03333                         AND                                       ELTHEARC
03334         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
03335                                                 NOT = ZERO        ELTHEARC
03336          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03337          MOVE +2                  TO WS-CIA                       ELTHEARC
03338          MOVE WS-EXCEPTION-SCHED  TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03339                                                                   ELTHEARC
03340      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03341      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03342                          AND                                      ELTHEARC
03343         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
03344                                                NOT = ZERO         ELTHEARC
03345                          AND                                      ELTHEARC
03346        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03347          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03348          MOVE +2                  TO WS-CIA                       ELTHEARC
03349          MOVE WS-EXCEPTION-SCHED  TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03350                                                                   ELTHEARC
03351      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03352      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03353                          AND                                      ELTHEARC
03354         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
03355                                                 NOT = ZERO        ELTHEARC
03356          ADD  +1 TO WS-CIA                                        ELTHEARC
03357          STRING WS-BASIC-LIT                                      ELTHEARC
03358                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTHEARC
03359                       DELIMITED BY SIZE                           ELTHEARC
03360          INTO COF-DTL-LINE (WS-CIA)                               ELTHEARC
03361                                                                   ELTHEARC
03362      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03363      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03364                          AND                                      ELTHEARC
03365         PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)                ELTHEARC
03366                                                NOT = ZERO         ELTHEARC
03367          ADD  +1 TO WS-CIA                                        ELTHEARC
03368          STRING WS-SUPP-LIT                                       ELTHEARC
03369                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTHEARC
03370                       DELIMITED BY SIZE                           ELTHEARC
03371          INTO COF-DTL-LINE (WS-CIA)                               ELTHEARC
03372                                                                   ELTHEARC
03373      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03374         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03375          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03376             THRU 9200-EXIT.                                       ELTHEARC
03377                                                                   ELTHEARC
03378      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03379         SET PLT-INDEX2 TO 2                                       ELTHEARC
03380      ELSE                                                         ELTHEARC
03381         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03382                                                                   ELTHEARC
03383  5125-EXIT.  EXIT.                                                ELTHEARC
03384 /                                                                 ELTHEARC
03385  5140-CERTIFICATION.                                              ELTHEARC
03386 ****************************************************************  ELTHEARC
03387 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTHEARC
03388 ****************************************************************  ELTHEARC
03389      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03390      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03391                         AND                                       ELTHEARC
03392         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
03393                                                 NOT = '00'        ELTHEARC
03394          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03395          MOVE +2                  TO WS-CIA                       ELTHEARC
03396          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03397                                                                   ELTHEARC
03398      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03399      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03400                          AND                                      ELTHEARC
03401         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
03402                                                NOT = '00'         ELTHEARC
03403                          AND                                      ELTHEARC
03404        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03405          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03406          MOVE +2                  TO WS-CIA                       ELTHEARC
03407          MOVE WS-CERT-REQ         TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03408                                                                   ELTHEARC
03409      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03410      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03411                          AND                                      ELTHEARC
03412         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
03413                                                 NOT = '00'        ELTHEARC
03414          MOVE 'BP'  TO CMF-RECORD-PREFIX                          ELTHEARC
03415          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
03416                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03417          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03418                             TO CMF-CODE-VALUE                     ELTHEARC
03419          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
03420          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03421          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03422             THRU 9500-EXIT.                                       ELTHEARC
03423                                                                   ELTHEARC
03424      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03425      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03426                          AND                                      ELTHEARC
03427         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTHEARC
03428                                                NOT = '00'         ELTHEARC
03429          MOVE 'BP'          TO CMF-RECORD-PREFIX                  ELTHEARC
03430          MOVE 'CERTFN-REQRM-IND'                                  ELTHEARC
03431                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03432          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03433                             TO CMF-CODE-VALUE                     ELTHEARC
03434          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
03435          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03436          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03437             THRU 9500-EXIT.                                       ELTHEARC
03438                                                                   ELTHEARC
03439      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03440         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03441          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03442             THRU 9200-EXIT.                                       ELTHEARC
03443                                                                   ELTHEARC
03444      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03445         SET PLT-INDEX2 TO 2                                       ELTHEARC
03446      ELSE                                                         ELTHEARC
03447         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03448                                                                   ELTHEARC
03449  5140-EXIT.  EXIT.                                                ELTHEARC
03450 /                                                                 ELTHEARC
03451  5150-RECERTIFICATION.                                            ELTHEARC
03452 ****************************************************************  ELTHEARC
03453 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTHEARC
03454 ****************************************************************  ELTHEARC
03455      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03456      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03457                         AND                                       ELTHEARC
03458         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03459                                                 NOT = ZERO        ELTHEARC
03460          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03461          MOVE +2                  TO WS-CIA                       ELTHEARC
03462          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03463                                                                   ELTHEARC
03464      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03465      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03466                          AND                                      ELTHEARC
03467         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03468                                                NOT = ZERO         ELTHEARC
03469                          AND                                      ELTHEARC
03470        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03471          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03472          MOVE +2                  TO WS-CIA                       ELTHEARC
03473          MOVE WS-RECERT-REQ       TO COF-DTL-LINE (WS-CIA).       ELTHEARC
03474                                                                   ELTHEARC
03475      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03476      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03477                          AND                                      ELTHEARC
03478         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03479                                                 NOT = ZERO        ELTHEARC
03480          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTHEARC
03481          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
03482                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03483          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
03484                             TO CMF-CODE-VALUE                     ELTHEARC
03485          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTHEARC
03486          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03487          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03488             THRU 9500-EXIT.                                       ELTHEARC
03489                                                                   ELTHEARC
03490      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03491      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03492                          AND                                      ELTHEARC
03493         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03494                                                NOT = ZERO         ELTHEARC
03495          MOVE 'BPE'         TO CMF-RECORD-PREFIX                  ELTHEARC
03496          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTHEARC
03497                             TO CMF-ELEMENT-SYSTEM-NAME            ELTHEARC
03498          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTHEARC
03499                             TO CMF-CODE-VALUE                     ELTHEARC
03500          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTHEARC
03501          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTHEARC
03502          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTHEARC
03503             THRU 9500-EXIT.                                       ELTHEARC
03504                                                                   ELTHEARC
03505      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03506         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03507          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03508             THRU 9200-EXIT.                                       ELTHEARC
03509                                                                   ELTHEARC
03510      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03511         SET PLT-INDEX2 TO 2                                       ELTHEARC
03512      ELSE                                                         ELTHEARC
03513         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03514                                                                   ELTHEARC
03515  5150-EXIT.  EXIT.                                                ELTHEARC
03516 /                                                                 ELTHEARC
03517  5155-SERVC-NEC.                                                  ELTHEARC
03518 ****************************************************************  ELTHEARC
03519 * S E R V I C E   N E C E S S A R Y   B I T  I N D I C A T O R *  ELTHEARC
03520 ****************************************************************  ELTHEARC
03521      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03522      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03523                         AND                                       ELTHEARC
03524         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
03525                                                   = '1'           ELTHEARC
03526          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03527          MOVE +3                  TO WS-CIA                       ELTHEARC
03528          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
03529          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3).         ELTHEARC
03530                                                                   ELTHEARC
03531      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03532      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTHEARC
03533                          AND                                      ELTHEARC
03534         PLP-SERV-NECESRY-CORP-BIT-IND (PLT-INDEX1, PLT-INDEX2)    ELTHEARC
03535                                                   = '1'           ELTHEARC
03536                          AND                                      ELTHEARC
03537        NOT WS-ADD-A-BLANK-LINE                                    ELTHEARC
03538          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTHEARC
03539          MOVE +3                  TO WS-CIA                       ELTHEARC
03540          MOVE WS-RELATED-MED-COND-1 TO  COF-DTL-LINE (2)          ELTHEARC
03541          MOVE WS-RELATED-MED-COND-2 TO  COF-DTL-LINE (3)          ELTHEARC
03542                                                                   ELTHEARC
03543      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03544         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03545          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTHEARC
03546             THRU 9200-EXIT.                                       ELTHEARC
03547                                                                   ELTHEARC
03548      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03549         SET PLT-INDEX2 TO 2                                       ELTHEARC
03550      ELSE                                                         ELTHEARC
03551         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03552                                                                   ELTHEARC
03553  5155-EXIT.  EXIT.                                                ELTHEARC
03554 /                                                                 ELTHEARC
03555  5160-SPILLOVR-COINS-N-DEDUC.                                     ELTHEARC
03556 ****************************************************************  ELTHEARC
03557 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTHEARC
03558 ****************************************************************  ELTHEARC
03559      MOVE +1 TO WS-CIA.                                           ELTHEARC
03560                                                                   ELTHEARC
03561      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03562      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
03563                           AND                                     ELTHEARC
03564         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTHEARC
03565                                                 NOT = '0'         ELTHEARC
03566         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
03567         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTHEARC
03568                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
03569         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTHEARC
03570                                              TO CMF-CODE-VALUE    ELTHEARC
03571         MOVE WS-SPILLOVER      TO WS-TEMP-TEXT-AREA               ELTHEARC
03572         MOVE +69               TO WS-TEMP-NOT-USED-CNT            ELTHEARC
03573         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
03574            THRU 9500-EXIT                                         ELTHEARC
03575         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
03576            THRU 9200-EXIT.                                        ELTHEARC
03577 ****************************************************************  ELTHEARC
03578 *          S P I L L O V E R   D E D U C T I B L E             *  ELTHEARC
03579 ****************************************************************  ELTHEARC
03580      MOVE +1 TO WS-CIA.                                           ELTHEARC
03581                                                                   ELTHEARC
03582      SET PLT-INDEX2 TO 2.                                         ELTHEARC
03583      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
03584                           AND                                     ELTHEARC
03585         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTHEARC
03586                                                 NOT = '0'         ELTHEARC
03587         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTHEARC
03588         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTHEARC
03589                                           CMF-ELEMENT-SYSTEM-NAME ELTHEARC
03590         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTHEARC
03591                                               TO CMF-CODE-VALUE   ELTHEARC
03592         MOVE WS-SPILLOVER    TO WS-TEMP-TEXT-AREA                 ELTHEARC
03593         MOVE +69             TO WS-TEMP-NOT-USED-CNT              ELTHEARC
03594         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTHEARC
03595            THRU 9500-EXIT                                         ELTHEARC
03596         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTHEARC
03597            THRU 9200-EXIT.                                        ELTHEARC
03598                                                                   ELTHEARC
03599      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03600         SET PLT-INDEX2 TO 2                                       ELTHEARC
03601      ELSE                                                         ELTHEARC
03602         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03603                                                                   ELTHEARC
03604  5160-EXIT.  EXIT.                                                ELTHEARC
03605                                                                   ELTHEARC
03606  5170-AAR-PPF-PVE-TABS.                                           ELTHEARC
03607 ****************************************************************  ELTHEARC
03608 *                  A A R   T A B U L A R                       *  ELTHEARC
03609 ****************************************************************  ELTHEARC
03610      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03611      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03612         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03613                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03614         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03615         MOVE +1 TO COF-NBR-DTL-LINES                              ELTHEARC
03616         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1)               ELTHEARC
03617      ELSE                                                         ELTHEARC
03618         SET PLT-INDEX2 TO 2                                       ELTHEARC
03619         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03620            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03621                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03622            MOVE 'Y' TO WS-ADD-A-BLANK-IND                         ELTHEARC
03623            MOVE +1 TO COF-NBR-DTL-LINES                           ELTHEARC
03624            MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(1).           ELTHEARC
03625                                                                   ELTHEARC
03626      IF WS-ADD-A-BLANK-LINE                                       ELTHEARC
03627         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTHEARC
03628         MOVE 1 TO WS-CIA                                          ELTHEARC
03629         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTHEARC
03630             COMMAREA(DFHCOMMAREA)                                 ELTHEARC
03631         END-EXEC.                                                 ELTHEARC
03632 *--------------------------------------------------------------*  ELTHEARC
03633 *                  P P F   T A B U L A R                       *  ELTHEARC
03634 *--------------------------------------------------------------*  ELTHEARC
03635      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03636      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03637         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03638                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03639         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
03640                                                  KWA-GCTABULR-KEY ELTHEARC
03641         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
03642            THRU 9900-EXIT                                         ELTHEARC
03643         IF IOP-RC-OK                                              ELTHEARC
03644            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTHEARC
03645                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
03646            END-EXEC                                               ELTHEARC
03647         ELSE                                                      ELTHEARC
03648            NEXT SENTENCE                                          ELTHEARC
03649      ELSE                                                         ELTHEARC
03650         SET PLT-INDEX2 TO 2                                       ELTHEARC
03651         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03652            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03653                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03654          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
03655                                                  KWA-GCTABULR-KEY ELTHEARC
03656            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
03657               THRU 9900-EXIT                                      ELTHEARC
03658            IF IOP-RC-OK                                           ELTHEARC
03659               EXEC  CICS  LINK  PROGRAM('ELGPPF')                 ELTHEARC
03660                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
03661               END-EXEC.                                           ELTHEARC
03662 *--------------------------------------------------------------*  ELTHEARC
03663 *                  P V E   T A B U L A R                       *  ELTHEARC
03664 *--------------------------------------------------------------*  ELTHEARC
03665                                                                   ELTHEARC
03666      MOVE +2    TO WS-CIA.                                        ELTHEARC
03667      MOVE WS-PVE TO COF-DTL-LINE (2).                             ELTHEARC
03668      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
03669         THRU 9200-EXIT.                                           ELTHEARC
03670                                                                   ELTHEARC
03671  5170-EXIT.  EXIT.                                                ELTHEARC
03672 /                                                                 ELTHEARC
03673  5180-ALL-LEVEL-TABS.                                             ELTHEARC
03674 *--------------------------------------------------------------*  ELTHEARC
03675 *                  A B M   T A B U L A R                       *  ELTHEARC
03676 *--------------------------------------------------------------*  ELTHEARC
03677      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03678      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTHEARC
03679                             AND                                   ELTHEARC
03680         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03681                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03682         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
03683                                                  KWA-GCTABULR-KEY ELTHEARC
03684         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
03685            THRU 9900-EXIT                                         ELTHEARC
03686         IF IOP-RC-OK                                              ELTHEARC
03687            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTHEARC
03688                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
03689            END-EXEC                                               ELTHEARC
03690         ELSE                                                      ELTHEARC
03691            NEXT SENTENCE                                          ELTHEARC
03692      ELSE                                                         ELTHEARC
03693         SET PLT-INDEX2 TO 2                                       ELTHEARC
03694         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03695            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03696                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03697          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
03698                                                  KWA-GCTABULR-KEY ELTHEARC
03699            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
03700               THRU 9900-EXIT                                      ELTHEARC
03701            IF IOP-RC-OK                                           ELTHEARC
03702               EXEC  CICS  LINK  PROGRAM('ELGMAXIM')               ELTHEARC
03703                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
03704               END-EXEC.                                           ELTHEARC
03705 *--------------------------------------------------------------*  ELTHEARC
03706 *                  A C L   T A B U L A R                       *  ELTHEARC
03707 *--------------------------------------------------------------*  ELTHEARC
03708      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03709      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03710         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03711                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03712         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
03713                                                  KWA-GCTABULR-KEY ELTHEARC
03714         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
03715            THRU 9900-EXIT                                         ELTHEARC
03716         IF IOP-RC-OK                                              ELTHEARC
03717            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTHEARC
03718                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
03719            END-EXEC                                               ELTHEARC
03720         ELSE                                                      ELTHEARC
03721            NEXT SENTENCE                                          ELTHEARC
03722      ELSE                                                         ELTHEARC
03723         SET PLT-INDEX2 TO 2                                       ELTHEARC
03724         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03725            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03726                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03727          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
03728                                                  KWA-GCTABULR-KEY ELTHEARC
03729            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
03730               THRU 9900-EXIT                                      ELTHEARC
03731            IF IOP-RC-OK                                           ELTHEARC
03732               EXEC  CICS  LINK  PROGRAM('ELGCOINS')               ELTHEARC
03733                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
03734               END-EXEC.                                           ELTHEARC
03735 *--------------------------------------------------------------*  ELTHEARC
03736 *                  A D L   T A B U L A R                       *  ELTHEARC
03737 *--------------------------------------------------------------*  ELTHEARC
03738      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03739      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03740         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03741                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03742         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
03743                                                  KWA-GCTABULR-KEY ELTHEARC
03744         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
03745            THRU 9900-EXIT                                         ELTHEARC
03746         IF IOP-RC-OK                                              ELTHEARC
03747            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTHEARC
03748                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
03749            END-EXEC                                               ELTHEARC
03750         ELSE                                                      ELTHEARC
03751            NEXT SENTENCE                                          ELTHEARC
03752      ELSE                                                         ELTHEARC
03753         SET PLT-INDEX2 TO 2                                       ELTHEARC
03754         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03755            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03756                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03757          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
03758                                                  KWA-GCTABULR-KEY ELTHEARC
03759            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
03760               THRU 9900-EXIT                                      ELTHEARC
03761            IF IOP-RC-OK                                           ELTHEARC
03762               EXEC  CICS  LINK  PROGRAM('ELGDEDBL')               ELTHEARC
03763                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
03764               END-EXEC.                                           ELTHEARC
03765 *--------------------------------------------------------------*  ELTHEARC
03766 *                  A O L   T A B U L A R                       *  ELTHEARC
03767 *--------------------------------------------------------------*  ELTHEARC
03768      SET PLT-INDEX2 TO 1.                                         ELTHEARC
03769      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTHEARC
03770         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT =    ELTHEARC
03771                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03772         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO  ELTHEARC
03773                                                  KWA-GCTABULR-KEY ELTHEARC
03774         PERFORM 9900-GET-TABULAR-RECORD                           ELTHEARC
03775            THRU 9900-EXIT                                         ELTHEARC
03776         IF IOP-RC-OK                                              ELTHEARC
03777            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTHEARC
03778                 COMMAREA(DFHCOMMAREA)                             ELTHEARC
03779            END-EXEC                                               ELTHEARC
03780         ELSE                                                      ELTHEARC
03781            NEXT SENTENCE                                          ELTHEARC
03782      ELSE                                                         ELTHEARC
03783         SET PLT-INDEX2 TO 2                                       ELTHEARC
03784         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND     ELTHEARC
03785            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) NOT = ELTHEARC
03786                    ZERO AND NOT = SPACES AND   NOT = LOW-VALUES   ELTHEARC
03787          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) TO ELTHEARC
03788                                                  KWA-GCTABULR-KEY ELTHEARC
03789            PERFORM 9900-GET-TABULAR-RECORD                        ELTHEARC
03790               THRU 9900-EXIT                                      ELTHEARC
03791            IF IOP-RC-OK                                           ELTHEARC
03792               EXEC  CICS  LINK  PROGRAM('ELGOUTPX')               ELTHEARC
03793                    COMMAREA(DFHCOMMAREA)                          ELTHEARC
03794               END-EXEC.                                           ELTHEARC
03795  5180-EXIT.  EXIT.                                                ELTHEARC
03796 /                                                                 ELTHEARC
03797  6000-PAY-CONSID-TEXT.                                            ELTHEARC
03798      INITIALIZE TCAR-FROM-AREA.                                   ELTHEARC
03799      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTHEARC
03800             WS-PAY-CONSDR-TEXT2                                   ELTHEARC
03801                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTHEARC
03802      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTHEARC
03803      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTHEARC
03804      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTHEARC
03805                                TCAR-OUTPUT-FIELD-2-LEN.           ELTHEARC
03806      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTHEARC
03807      IF WS-CIA > 17                                               ELTHEARC
03808            PERFORM 9200-TEXT-OUTPUT-REQUEST THRU 9200-EXIT        ELTHEARC
03809            MOVE +1            TO WS-CIA.                          ELTHEARC
03810      ADD +1                TO  WS-CIA.                            ELTHEARC
03811      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTHEARC
03812      ADD +1                TO  WS-CIA.                            ELTHEARC
03813      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTHEARC
03814      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
03815         THRU 9200-EXIT.                                           ELTHEARC
03816                                                                   ELTHEARC
03817  6000-EXIT.   EXIT.                                               ELTHEARC
03818 /***************************************************************  ELTHEARC
03819 *    TRANSFER TO OTHER RESPONSIBILITY INDICATOR                *  ELTHEARC
03820 ****************************************************************  ELTHEARC
03821                                                                   ELTHEARC
03822  6100-TRANSF-OTHER-RESP-IND.                                      ELTHEARC
03823                                                                   ELTHEARC
03824      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTHEARC
03825         SET PLT-INDEX2 TO 2                                       ELTHEARC
03826      ELSE                                                         ELTHEARC
03827         SET PLT-INDEX2 TO 1.                                      ELTHEARC
03828                                                                   ELTHEARC
03829      MOVE +1 TO WS-CIA.                                           ELTHEARC
03830                                                                   ELTHEARC
03831      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTHEARC
03832         EQUAL ZEROS                                               ELTHEARC
03833         GO TO 6100-EXIT.                                          ELTHEARC
03834                                                                   ELTHEARC
03835      MOVE 'BP' TO CMF-RECORD-PREFIX.                              ELTHEARC
03836      MOVE 'TRANSF-OTHER-RESP-IND' TO  CMF-ELEMENT-SYSTEM-NAME.    ELTHEARC
03837      MOVE  PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2)      ELTHEARC
03838            TO CMF-CODE-VALUE.                                     ELTHEARC
03839      MOVE SPACES            TO WS-TEMP-TEXT-AREA.                 ELTHEARC
03840      MOVE +00               TO WS-TEMP-NOT-USED-CNT.              ELTHEARC
03841      PERFORM 9500-CALL-CODES-MANUAL-LONG                          ELTHEARC
03842            THRU 9500-EXIT.                                        ELTHEARC
03843      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTHEARC
03844            THRU 9200-EXIT.                                        ELTHEARC
03845                                                                   ELTHEARC
03846  6100-EXIT.  EXIT.                                                ELTHEARC
03847                                                                   ELTHEARC
03848 /***************************************************************  ELTHEARC
03849 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTHEARC
03850 ****************************************************************  ELTHEARC
03851  9100-HEADER-OUTPUT-REQUEST.                                      ELTHEARC
03852                                                                   ELTHEARC
03853      MOVE +2           TO COF-NBR-HDR-LINES.                      ELTHEARC
03854      MOVE 'P'          TO COF-FUNCTION.                           ELTHEARC
03855                                                                   ELTHEARC
03856      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
03857                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
03858                     END-EXEC.                                     ELTHEARC
03859                                                                   ELTHEARC
03860  9100-EXIT.  EXIT.                                                ELTHEARC
03861                                                                   ELTHEARC
03862 ****************************************************************  ELTHEARC
03863 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTHEARC
03864 ****************************************************************  ELTHEARC
03865  9200-TEXT-OUTPUT-REQUEST.                                        ELTHEARC
03866                                                                   ELTHEARC
03867      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTHEARC
03868      MOVE +0    TO COF-NBR-HDR-LINES.                             ELTHEARC
03869      MOVE ' '   TO COF-FUNCTION.                                  ELTHEARC
03870                                                                   ELTHEARC
03871      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTHEARC
03872                     COMMAREA (DFHCOMMAREA)                        ELTHEARC
03873                     END-EXEC.                                     ELTHEARC
03874                                                                   ELTHEARC
03875  9200-EXIT.  EXIT.                                                ELTHEARC
03876 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTHEARC
03877  9500-CALL-CODES-MANUAL-LONG.                                     ELTHEARC
03878                                                                   ELTHEARC
03879      INITIALIZE CMF-RETURN-CODE,                                  ELTHEARC
03880                 TCAR-FROM-AREA.                                   ELTHEARC
03881                                                                   ELTHEARC
03882      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTHEARC
03883      END-EXEC.                                                    ELTHEARC
03884                                                                   ELTHEARC
03885      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHEARC
03886      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
03887              ADDRESS OF    CMF-DESCR.                             ELTHEARC
03888                                                                   ELTHEARC
03889      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTHEARC
03890         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTHEARC
03891         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTHEARC
03892            CMF-DESCR-LINE(1),        ' ',                         ELTHEARC
03893            CMF-DESCR-LINE(2),        ' ',                         ELTHEARC
03894            CMF-DESCR-LINE(3)                                      ELTHEARC
03895            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTHEARC
03896      ELSE                                                         ELTHEARC
03897         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTHEARC
03898         STRING CMF-DESCR-LINE(1),        ' ',                     ELTHEARC
03899            CMF-DESCR-LINE(2),        ' ',                         ELTHEARC
03900            CMF-DESCR-LINE(3)                                      ELTHEARC
03901            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTHEARC
03902                                                                   ELTHEARC
03903      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTHEARC
03904                                                                   ELTHEARC
03905      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTHEARC
03906      MOVE +3 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTHEARC
03907      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTHEARC
03908      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTHEARC
03909      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTHEARC
03910                                                                   ELTHEARC
03911      IF WS-MOVE-LINES-TO-CIA                                      ELTHEARC
03912         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTHEARC
03913            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTHEARC
03914                                             WS-TEMP-NOT-USED-CNT  ELTHEARC
03915            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTHEARC
03916               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTHEARC
03917                              UNTIL  WS-TEMP-NOT-USED-CNT > +78    ELTHEARC
03918            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTHEARC
03919            ADD +1 TO WS-CIA                                       ELTHEARC
03920            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTHEARC
03921         ELSE                                                      ELTHEARC
03922            ADD +1 TO WS-CIA                                       ELTHEARC
03923            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).         ELTHEARC
03924                                                                   ELTHEARC
03925      IF WS-MOVE-LINES-TO-CIA                                      ELTHEARC
03926         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTHEARC
03927            PERFORM 9560-MOVE-LINES-TO-CIA                         ELTHEARC
03928               THRU 9560-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTHEARC
03929                              UNTIL   WS-SUB1 >                    ELTHEARC
03930                                      TCAR-OUTPUT-FIELDS-USED      ELTHEARC
03931         ELSE                                                      ELTHEARC
03932            NEXT SENTENCE                                          ELTHEARC
03933      ELSE                                                         ELTHEARC
03934         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTHEARC
03935                                                                   ELTHEARC
03936  9500-EXIT.  EXIT.                                                ELTHEARC
03937                                                                   ELTHEARC
03938  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTHEARC
03939      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTHEARC
03940      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTHEARC
03941                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTHEARC
03942                                                                   ELTHEARC
03943  9550-EXIT.  EXIT.                                                ELTHEARC
03944                                                                   ELTHEARC
03945  9560-MOVE-LINES-TO-CIA.                                          ELTHEARC
03946      ADD +1 TO WS-CIA.                                            ELTHEARC
03947      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTHEARC
03948                                                                   ELTHEARC
03949  9560-EXIT.  EXIT.                                                ELTHEARC
03950 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTHEARC
03951  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTHEARC
03952                                                                   ELTHEARC
03953      INITIALIZE CMF-RETURN-CODE,                                  ELTHEARC
03954                 TCAR-FROM-AREA.                                   ELTHEARC
03955                                                                   ELTHEARC
03956      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTHEARC
03957      END-EXEC.                                                    ELTHEARC
03958                                                                   ELTHEARC
03959      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTHEARC
03960      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
03961              ADDRESS OF    CMF-DESCR.                             ELTHEARC
03962                                                                   ELTHEARC
03963                                                                   ELTHEARC
03964      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTHEARC
03965         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTHEARC
03966         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTHEARC
03967            CMF-DESCR-LINE(1),        ' ',                         ELTHEARC
03968            CMF-DESCR-LINE(2),        ' ',                         ELTHEARC
03969            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTHEARC
03970            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTHEARC
03971      ELSE                                                         ELTHEARC
03972         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTHEARC
03973         STRING CMF-DESCR-LINE(1),        ' ',                     ELTHEARC
03974            CMF-DESCR-LINE(2),        ' ',                         ELTHEARC
03975            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTHEARC
03976            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTHEARC
03977                                                                   ELTHEARC
03978      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTHEARC
03979                                                                   ELTHEARC
03980      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTHEARC
03981      MOVE +4 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTHEARC
03982      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN,                         ELTHEARC
03983                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTHEARC
03984                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTHEARC
03985      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTHEARC
03986                                                                   ELTHEARC
03987      IF WS-MOVE-LINES-TO-CIA                                      ELTHEARC
03988         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTHEARC
03989            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTHEARC
03990                                             WS-TEMP-NOT-USED-CNT  ELTHEARC
03991            PERFORM 9650-CONCATENATE-TO-TEMP-TEXT                  ELTHEARC
03992               THRU 9650-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTHEARC
03993                              UNTIL  WS-TEMP-NOT-USED-CNT > +78    ELTHEARC
03994            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTHEARC
03995            ADD +1 TO WS-CIA                                       ELTHEARC
03996            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTHEARC
03997         ELSE                                                      ELTHEARC
03998            ADD +1 TO WS-CIA                                       ELTHEARC
03999            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).         ELTHEARC
04000                                                                   ELTHEARC
04001      IF WS-MOVE-LINES-TO-CIA                                      ELTHEARC
04002         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTHEARC
04003            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTHEARC
04004               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTHEARC
04005                              UNTIL   WS-SUB1 >                    ELTHEARC
04006                              TCAR-OUTPUT-FIELDS-USED              ELTHEARC
04007         ELSE                                                      ELTHEARC
04008            NEXT SENTENCE                                          ELTHEARC
04009      ELSE                                                         ELTHEARC
04010         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTHEARC
04011                                                                   ELTHEARC
04012  9600-EXIT.  EXIT.                                                ELTHEARC
04013                                                                   ELTHEARC
04014  9650-CONCATENATE-TO-TEMP-TEXT.                                   ELTHEARC
04015      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTHEARC
04016      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTHEARC
04017                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTHEARC
04018                                                                   ELTHEARC
04019  9650-EXIT.  EXIT.                                                ELTHEARC
04020                                                                   ELTHEARC
04021  9660-MOVE-LINES-TO-CIA.                                          ELTHEARC
04022      ADD +1 TO WS-CIA.                                            ELTHEARC
04023      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTHEARC
04024                                                                   ELTHEARC
04025  9660-EXIT.  EXIT.                                                ELTHEARC
04026 /                                                                 ELTHEARC
04027 ***************************************************************** ELTHEARC
04028 *            G E T   T A B U L A R   R E C O R D                  ELTHEARC
04029 *                                                                 ELTHEARC
04030 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTHEARC
04031 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTHEARC
04032 * TO DISPLAY.                                                     ELTHEARC
04033 *                                                                 ELTHEARC
04034 ***************************************************************** ELTHEARC
04035  9900-GET-TABULAR-RECORD.                                         ELTHEARC
04036                                                                   ELTHEARC
04037      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHEARC
04038      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTHEARC
04039                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELTHEARC
04040                                                                   ELTHEARC
04041      SET CIA-GCTABULR-DDN TO TRUE.                                ELTHEARC
04042      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTHEARC
04043                                                                   ELTHEARC
04044      SET IOP-RD                     TO TRUE.                      ELTHEARC
04045      SET IOP-FCQ-NONE               TO TRUE.                      ELTHEARC
04046      SET IOP-KVQ-NONE               TO TRUE.                      ELTHEARC
04047                                                                   ELTHEARC
04048      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTHEARC
04049             COMMAREA(DFHCOMMAREA)                                 ELTHEARC
04050      END-EXEC.                                                    ELTHEARC
04051                                                                   ELTHEARC
04052      IF IOP-RC-NOTFND                                             ELTHEARC
04053         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTHEARC
04054         EXEC CICS ABEND                                           ELTHEARC
04055                   ABCODE(CIA-ABCODE)                              ELTHEARC
04056         END-EXEC.                                                 ELTHEARC
04057                                                                   ELTHEARC
04058      IF NOT IOP-RC-OK                                             ELTHEARC
04059         SET CIA-AB-CRITIO          TO TRUE                        ELTHEARC
04060         EXEC CICS ABEND                                           ELTHEARC
04061                   ABCODE(CIA-ABCODE)                              ELTHEARC
04062         END-EXEC.                                                 ELTHEARC
04063                                                                   ELTHEARC
04064  9900-EXIT.  EXIT.                                                ELTHEARC
04065                                                                   ELTHEARC
04066      COPY ELSTCOMP.                                               ELTHEARC
