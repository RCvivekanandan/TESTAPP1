00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTDXEMR.                                            ELTDXEMR
00003  AUTHOR. ANNE KEFFER KING.                                           LV001
00004  DATE-WRITTEN.   10/04/95.                                        ELTDXEMR
00005  DATE-COMPILED.                                                   ELTDXEMR
00006      SKIP3                                                        ELTDXEMR
00007 ******************************************************************ELTDXEMR
00008 *  ELTDXEMR                                                       ELTDXEMR
00009                                                                   ELTDXEMR
00010 *                        PROGRAM ABSTRACT                         ELTDXEMR
00011 *                                                                 ELTDXEMR
00012 *   PROGRAM NAME:   E.L.S. DIAGNOSTIC EMERGENCY BENEFITS          ELTDXEMR
00013 *                                                                 ELTDXEMR
00014 *   PROGRAM I.D.:   ELTDXEMR                                      ELTDXEMR
00015 *                                                                 ELTDXEMR
00016 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDXEMR
00017 *              DIAGNOSTIC EMERGENCY CARE FOR A MEMBER.            ELTDXEMR
00018 *                                                                 ELTDXEMR
00019 *   OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DX EMERG COVERAGE ELTDXEMR
00020 *              AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTDXEMR
00021 *              GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTDXEMR
00022 *              THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTDXEMR
00023 *              RANGE OF DATES.                                    ELTDXEMR
00024 *                                                                 ELTDXEMR
00025 *   RECORDS                                                       ELTDXEMR
00026 *   ACCESSED:  GROUP SPECIFIC, CONTRACT, BENEFIT PROVISION FORMAT ELTDXEMR
00027 *            B AND E.                                             ELTDXEMR
00028      TITLE ' HISTORY OF EMERGENCY CARE BENEFITS'.                 ELTDXEMR
00029 ***************************************************************** ELTDXEMR
00030 *                                                                 ELTDXEMR
00031 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTDXEMR
00032 *          *-*       U P D A T E   H I S T O R Y       *-*        ELTDXEMR
00033 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTDXEMR
00034 *                                                                 ELTDXEMR
00035 **-CHG NUM-* *-DATE-* *WHO* *------DESCRIPTION------------------- ELTDXEMR
00036 *                                                                 ELTDXEMR
00037 *    XXXX    10/04/95  AKK  ORIGINAL MODULE CLONED FROM ELTEMRG   ELTDXEMR
00038 *                                                                 ELTDXEMR
00039 ***************************************************************** ELTDXEMR
00040                                                                   ELTDXEMR
00041      TITLE ' WORKING STORAGE SECTION DIAGNOSTIC EMERGENCY '.      ELTDXEMR
00042  ENVIRONMENT DIVISION.                                            ELTDXEMR
00043      SKIP3                                                        ELTDXEMR
00044  DATA DIVISION.                                                   ELTDXEMR
00045  WORKING-STORAGE SECTION.                                         ELTDXEMR
00046  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDXEMR
00047      '***ELTDXEMR WS BEGINS***'.                                  ELTDXEMR
00048                                                                   ELTDXEMR
00049 ***** W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDXEMR
00050  01  WS-WORK-FIELDS.                                              ELTDXEMR
00051      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTDXEMR
00052      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTDXEMR
00053      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTDXEMR
00054      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTDXEMR
00055      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDXEMR
00056      05  WS-SUPP-SWITCH                PIC X     VALUE SPACE.     ELTDXEMR
00057        88 SUPP-ONLY                    VALUE 'O'.                 ELTDXEMR
00058      05  WS-LOB-SWITCH                 PIC X     VALUE SPACE.     ELTDXEMR
00059        88  WS-BASIC-LOB                    VALUE 'B'.             ELTDXEMR
00060        88  WS-NOT-BASIC-LOB                VALUE SPACE.           ELTDXEMR
00061      05  WS-BASIC-SUPP-SWITCH          PIC X     VALUE SPACE.     ELTDXEMR
00062        88  PROCESSING-BASIC-INFO           VALUE 'B'.             ELTDXEMR
00063        88  PROCESSING-SUPPLEMENTAL         VALUE 'S'.             ELTDXEMR
00064      05  WS-BASIC-SWITCH               PIC X     VALUE SPACE.     ELTDXEMR
00065        88  WS-NOT-BASIC-NOW                VALUE 'Y'.             ELTDXEMR
00066      05  WS-POT-SWITCH                 PIC X     VALUE SPACE.     ELTDXEMR
00067        88  WS-PROCESS-POT                  VALUE 'Y'.             ELTDXEMR
00068      05  WS-INDEX-PROBLEM-SW           PIC X     VALUE SPACE.     ELTDXEMR
00069        88  WS-INDEX-PROBLEM                VALUE 'Y'.             ELTDXEMR
00070      05  WS-FIRSTTIME-IND              PIC X.                     ELTDXEMR
00071        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTDXEMR
00072      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTDXEMR
00073        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTDXEMR
00074      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTDXEMR
00075        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTDXEMR
00076      05  WS-EXPLANATION-IND            PIC S9 COMP-3.             ELTDXEMR
00077        88  WS-EXPLANATION-PRODUCED         VALUE +1 THRU +3.      ELTDXEMR
00078        88  WS-BASIC-EXPLANATION            VALUE +1, +3.          ELTDXEMR
00079        88  WS-BASIC-ONLY-EXPLAIN           VALUE +1.              ELTDXEMR
00080        88  WS-SUPP-EXPLANATION             VALUE +2 THRU +3.      ELTDXEMR
00081        88  WS-SUPP-ONLY-EXPLAIN            VALUE +2.              ELTDXEMR
00082        88  WS-NO-EXPLANATION               VALUE +0.              ELTDXEMR
00083      05  WS-BASIC-EXPLAIN-CNT          PIC S9 COMP-3.             ELTDXEMR
00084      05  WS-SUPP-EXPLAIN-CNT           PIC S9 COMP-3.             ELTDXEMR
00085      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTDXEMR
00086      05  WS-PERCENT-FLD.                                          ELTDXEMR
00087        10  WS-PERCENTAGE               PIC ZZ9.                   ELTDXEMR
00088        10  WS-PERCENT-SIGN             PIC X.                     ELTDXEMR
00089      05  WS-BASIC-CONTRACT             PIC X.                     ELTDXEMR
00090        88  WS-BASIC-PRESENT                VALUE 'B'.             ELTDXEMR
00091      05  WS-SUPP-CONTRACT              PIC X.                     ELTDXEMR
00092        88  WS-SUPP-PRESENT                 VALUE 'S'.             ELTDXEMR
00093  01 WS-OUTPUT-OPTION-VALUES.                                      ELTDXEMR
00094      05 WS-SINGLE-QUOTE                PIC X  VALUE ''''.         ELTDXEMR
00095      05 WS-BASIC-LINE                  PIC X.                     ELTDXEMR
00096         88  WS-BSC-LINE                       VALUE '1' '2' '3'.  ELTDXEMR
00097                                                                   ELTDXEMR
00098  01  WS-TEST-LINE.                                                ELTDXEMR
00099      05  WS-TEST-CHAR                  PIC X.                     ELTDXEMR
00100      05  WS-TEST-DATA                  PIC X(78).                 ELTDXEMR
00101                                                                   ELTDXEMR
00102      TITLE 'BEN PROV  IDS BY TYPE -- ELTDXEMR'.                   ELTDXEMR
00103  01  WS-BEN-PROV-ID.                                              ELTDXEMR
00104      05  TABLE-MAX                     PIC S9(4) COMP   VALUE +10.ELTDXEMR
00105      05  WS-EMR-CNT                    PIC S9(4) COMP   VALUE +10.ELTDXEMR
00106      05  WS-INST-EMR.                                             ELTDXEMR
00107        10  FILLER                      PIC X(6)  VALUE 'EACX B'.  ELTDXEMR
00108        10  FILLER                      PIC X(6)  VALUE 'EAL  B'.  ELTDXEMR
00109        10  FILLER                      PIC X(6)  VALUE 'EAMP B'.  ELTDXEMR
00110        10  FILLER                      PIC X(6)  VALUE 'EARI B'.  ELTDXEMR
00111        10  FILLER                      PIC X(6)  VALUE 'EAX  B'.  ELTDXEMR
00112        10  FILLER                      PIC X(6)  VALUE 'EMCX B'.  ELTDXEMR
00113        10  FILLER                      PIC X(6)  VALUE 'EML  B'.  ELTDXEMR
00114        10  FILLER                      PIC X(6)  VALUE 'EMMP B'.  ELTDXEMR
00115        10  FILLER                      PIC X(6)  VALUE 'EMRI B'.  ELTDXEMR
00116        10  FILLER                      PIC X(6)  VALUE 'EMX  B'.  ELTDXEMR
00117      05  WS-INST-EMR-LIST    REDEFINES    WS-INST-EMR             ELTDXEMR
00118                                        PIC X(6)  OCCURS 10 TIMES. ELTDXEMR
00119                                                                   ELTDXEMR
00120      05  WS-PROF-EMR-CNT               PIC S9(4) COMP   VALUE +10.ELTDXEMR
00121      05  WS-PROF-EMR.                                             ELTDXEMR
00122        10  FILLER                      PIC X(6)  VALUE 'EAC  E'.  ELTDXEMR
00123        10  FILLER                      PIC X(6)  VALUE 'EAMP E'.  ELTDXEMR
00124        10  FILLER                      PIC X(6)  VALUE 'EAPT E'.  ELTDXEMR
00125        10  FILLER                      PIC X(6)  VALUE 'EARI E'.  ELTDXEMR
00126        10  FILLER                      PIC X(6)  VALUE 'EAX  E'.  ELTDXEMR
00127        10  FILLER                      PIC X(6)  VALUE 'EMC  E'.  ELTDXEMR
00128        10  FILLER                      PIC X(6)  VALUE 'EMMP E'.  ELTDXEMR
00129        10  FILLER                      PIC X(6)  VALUE 'EMPT E'.  ELTDXEMR
00130        10  FILLER                      PIC X(6)  VALUE 'EMRI E'.  ELTDXEMR
00131        10  FILLER                      PIC X(6)  VALUE 'EMX  E'.  ELTDXEMR
00132      05  WS-PROF-EMR-LIST    REDEFINES    WS-PROF-EMR             ELTDXEMR
00133                                        PIC X(6)  OCCURS 10 TIMES. ELTDXEMR
00134                                                                   ELTDXEMR
00135      TITLE 'DISPLAY LINES   --- ELTDXEMR '.                       ELTDXEMR
00136                                                                   ELTDXEMR
00137  01  WS-ELS-DISPLAY-LINES.                                        ELTDXEMR
00138    05  WS-HDR-2-INST-EMR.                                         ELTDXEMR
00139      10  FILLER                    PIC X(17) VALUE SPACES.        ELTDXEMR
00140      10  FILLER                    PIC X(44)                      ELTDXEMR
00141          VALUE 'DIAGNOSTIC, EMERGENCY SERVICES INSTITUTIONAL'.    ELTDXEMR
00142      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDXEMR
00143                                                                   ELTDXEMR
00144    05  WS-HDR-2-PROF-EMR.                                         ELTDXEMR
00145      10  FILLER                    PIC X(17) VALUE SPACES.        ELTDXEMR
00146      10  FILLER                    PIC X(43)                      ELTDXEMR
00147          VALUE 'DIAGNOSTIC, EMERGENCY SERVICES PROFESSIONAL'.     ELTDXEMR
00148      10  FILLER                    PIC X(17) VALUE SPACES.        ELTDXEMR
00149    05  WS-HDR-2-COINS-BEN-LVL.                                    ELTDXEMR
00150      10  FILLER                    PIC X(30) VALUE SPACES.        ELTDXEMR
00151      10  FILLER                    PIC X(28)                      ELTDXEMR
00152          VALUE 'COINSURANCE AT BENEFIT LEVEL'.                    ELTDXEMR
00153      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTDXEMR
00154                                                                   ELTDXEMR
00155    05  WS-PVE-TEXT.                                               ELTDXEMR
00156        10 FILLER                PIC X(44) VALUE                   ELTDXEMR
00157          'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.          ELTDXEMR
00158                                                                   ELTDXEMR
00159    05  WS-EMERGENCY-SERVICES    PIC X(31)                         ELTDXEMR
00160          VALUE 'DIAGNOSTIC, EMERGENCY SERVICES '.                 ELTDXEMR
00161                                                                   ELTDXEMR
00162    05  WS-LIFE-THREAT-TREAT.                                      ELTDXEMR
00163      10  FILLER                    PIC X(59)                      ELTDXEMR
00164      VALUE 'TREATMENT FOR LIFE THREATENING CONDITIONS MUST BE RECEELTDXEMR
00165 -      'IVED:'.                                                   ELTDXEMR
00166    05  WS-LIFE-THREAT-TREATA.                                     ELTDXEMR
00167      10  FILLER                    PIC X(12)                      ELTDXEMR
00168       VALUE 'BE RECEIVED:'.                                       ELTDXEMR
00169      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTDXEMR
00170                                                                   ELTDXEMR
00171    05  WS-EAC-TREATMENT-MUST-BE.                                  ELTDXEMR
00172      10  FILLER                    PIC X(48)                      ELTDXEMR
00173      VALUE 'EMERGENCY ACCIDENT TREATMENT MUST BE RECEIVED: '.     ELTDXEMR
00174      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTDXEMR
00175                                                                   ELTDXEMR
00176    05  WS-EMC-TREATMENT-MUST-BE.                                  ELTDXEMR
00177      10  FILLER                    PIC X(47)                      ELTDXEMR
00178      VALUE 'EMERGENCY MEDICAL TREATMENT MUST BE RECEIVED: '.      ELTDXEMR
00179      10  FILLER                    PIC X(32) VALUE LOW-VALUES.    ELTDXEMR
00180                                                                   ELTDXEMR
00181    05  WS-TREATMENT-999-MSG.                                      ELTDXEMR
00182      10  TREATMENT-999-MSGA        PIC X(50)   VALUE              ELTDXEMR
00183      'BASED ON THE SUDDEN UNEXPECTED ONSET OF A MEDICAL'.         ELTDXEMR
00184      10  TREATMENT-999-MSGB        PIC X(79)   VALUE              ELTDXEMR
00185      'CONDITION REQUIRING IMMEDIATE MEDICAL ATTENTION.'.          ELTDXEMR
00186                                                                   ELTDXEMR
00187    05  WS-UNLIMITED                PIC X(03)                      ELTDXEMR
00188          VALUE 'UNL'.                                             ELTDXEMR
00189                                                                   ELTDXEMR
00190    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTDXEMR
00191          VALUE 'SERVICES MAY BE RENDERED '.                       ELTDXEMR
00192                                                                   ELTDXEMR
00193    05  WS-FOLLOWING-BEN.                                          ELTDXEMR
00194      10  FILLER                    PIC X(79) VALUE                ELTDXEMR
00195          'COVERED SERVICES ARE:  '.                               ELTDXEMR
00196                                                                   ELTDXEMR
00197    05  WS-PAYMNT-BASED.                                           ELTDXEMR
00198      10  FILLER                    PIC X(20)                      ELTDXEMR
00199          VALUE 'PAYMENT IS BASED ON '.                            ELTDXEMR
00200      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTDXEMR
00201                                                                   ELTDXEMR
00202    05  WS-BASIC-PHRASE.                                           ELTDXEMR
00203      10  WS-BASIC                  PIC X(07)                      ELTDXEMR
00204         VALUE 'BASIC: '.                                          ELTDXEMR
00205      10  FILLER                    PIC X(72)                      ELTDXEMR
00206         VALUE SPACE.                                              ELTDXEMR
00207                                                                   ELTDXEMR
00208    05  WS-SUPPLEMENTAL-PHRASE.                                    ELTDXEMR
00209      10  WS-SUPPLEMENTAL-A         PIC X(13)                      ELTDXEMR
00210         VALUE 'SUPPLEMENTAL:'.                                    ELTDXEMR
00211      10  FILLER                    PIC X(66)                      ELTDXEMR
00212         VALUE SPACE.                                              ELTDXEMR
00213                                                                   ELTDXEMR
00214    05  WS-BASIC-A.                                                ELTDXEMR
00215      10  WS-BASIC-LIT              PIC X(16)                      ELTDXEMR
00216         VALUE '         BASIC: '.                                 ELTDXEMR
00217      10  WS-DTL-BASIC-LONG.                                       ELTDXEMR
00218        15  WS-DTL-BASIC            PIC X(50) VALUE SPACES.        ELTDXEMR
00219        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTDXEMR
00220      10  WS-DTL-BASIC-LONGA REDEFINES WS-DTL-BASIC-LONG.          ELTDXEMR
00221        15 WS-DTL-BASIC-A           PIC X(63).                     ELTDXEMR
00222                                                                   ELTDXEMR
00223    05  WS-BASIC-W-IN-DAYS-LIFE.                                   ELTDXEMR
00224      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXEMR
00225      10  FILLER                    PIC X(14)                      ELTDXEMR
00226         VALUE 'BASIC: WITHIN '.                                   ELTDXEMR
00227      10  WS-BASIC-DAYS-LIFE        PIC ZZ9.                       ELTDXEMR
00228      10  FILLER                    PIC X(40)                      ELTDXEMR
00229         VALUE ' DAY(S) OF A LIFE THREATENING CONDITION.'.         ELTDXEMR
00230      10  FILLER                    PIC X(13) VALUE LOW-VALUES.    ELTDXEMR
00231                                                                   ELTDXEMR
00232    05  WS-BASIC-W-IN-DAYS-ACC.                                    ELTDXEMR
00233      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXEMR
00234      10  FILLER                    PIC X(14)                      ELTDXEMR
00235         VALUE 'BASIC: WITHIN '.                                   ELTDXEMR
00236      10  WS-BASIC-DAYS-ACC         PIC ZZ9.                       ELTDXEMR
00237      10  FILLER                    PIC X(22)                      ELTDXEMR
00238         VALUE ' DAY(S) OF AN ACCIDENT'.                           ELTDXEMR
00239      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTDXEMR
00240                                                                   ELTDXEMR
00241    05  WS-COMP-W-IN-DAYS-ACC.                                     ELTDXEMR
00242      10  FILLER                    PIC X(07)                      ELTDXEMR
00243         VALUE 'WITHIN '.                                          ELTDXEMR
00244      10  WS-COMP-DAYS-ACC          PIC ZZ9.                       ELTDXEMR
00245      10  WS-COMP-DAYS-ACCA REDEFINES WS-COMP-DAYS-ACC             ELTDXEMR
00246                                    PIC X(03).                     ELTDXEMR
00247      10  FILLER                    PIC X(22)                      ELTDXEMR
00248         VALUE ' DAY(S) OF AN ACCIDENT'.                           ELTDXEMR
00249      10  FILLER                    PIC X(47) VALUE LOW-VALUES.    ELTDXEMR
00250                                                                   ELTDXEMR
00251    05  WS-COMP-W-IN-LIFE-THREAT.                                  ELTDXEMR
00252      10  FILLER                    PIC X(07)                      ELTDXEMR
00253         VALUE 'WITHIN '.                                          ELTDXEMR
00254      10  WS-COMP-DAYS-LIFE         PIC ZZ9.                       ELTDXEMR
00255      10  FILLER                    PIC X(40)                      ELTDXEMR
00256         VALUE ' DAY(S) OF A LIFE THREATENING CONDITION.'.         ELTDXEMR
00257      10  FILLER                    PIC X(11) VALUE LOW-VALUES.    ELTDXEMR
00258                                                                   ELTDXEMR
00259    05  WS-BASIC-W-IN-DAYS-MED.                                    ELTDXEMR
00260      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXEMR
00261      10  FILLER                    PIC X(14)                      ELTDXEMR
00262         VALUE 'BASIC: WITHIN '.                                   ELTDXEMR
00263      10  WS-BASIC-DAYS-MED         PIC ZZ9.                       ELTDXEMR
00264      10  FILLER                    PIC X(27)                      ELTDXEMR
00265         VALUE ' DAY(S) OF ONSET OF ILLNESS'.                      ELTDXEMR
00266      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTDXEMR
00267                                                                   ELTDXEMR
00268    05  WS-COMP-W-IN-DAYS-MED.                                     ELTDXEMR
00269      10  FILLER                    PIC X(07)                      ELTDXEMR
00270         VALUE 'WITHIN '.                                          ELTDXEMR
00271      10  WS-COMP-DAYS-MED         PIC ZZ9.                        ELTDXEMR
00272      10  FILLER                    PIC X(29)                      ELTDXEMR
00273         VALUE ' DAY(S) OF ONSET OF ILLNESS'.                      ELTDXEMR
00274      10  FILLER                    PIC X(42) VALUE LOW-VALUES.    ELTDXEMR
00275                                                                   ELTDXEMR
00276    05  WS-BEYOND-LIMIT-EXPLAIN.                                   ELTDXEMR
00277      10  FILLER                    PIC X(36)                      ELTDXEMR
00278         VALUE 'IF TREATMENT IS BEYOND THE DAY LIMIT'.             ELTDXEMR
00279      10  FILLER                    PIC X(43)  VALUE LOW-VALUES.   ELTDXEMR
00280                                                                   ELTDXEMR
00281    05  WS-BASIC-EXPLAIN1           PIC X(79).                     ELTDXEMR
00282    05  WS-BASIC-EXPLAIN2           PIC X(79).                     ELTDXEMR
00283                                                                   ELTDXEMR
00284    05  WS-BASIC-MAX.                                              ELTDXEMR
00285      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDXEMR
00286      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDXEMR
00287      10  WS-BASIC-MAX-AMT          PIC ZZ9.99-.                   ELTDXEMR
00288      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTDXEMR
00289                                                                   ELTDXEMR
00290    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTDXEMR
00291        VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTDXEMR
00292                                                                   ELTDXEMR
00293    05  WS-SUPPLEMENTAL.                                           ELTDXEMR
00294      10  WS-SUPP-LIT               PIC X(16)                      ELTDXEMR
00295          VALUE '  SUPPLEMENTAL: '.                                ELTDXEMR
00296      10  WS-DTL-SUPP-LONG.                                        ELTDXEMR
00297        15  WS-DTL-SUPPLEMENTAL     PIC X(50) VALUE SPACES.        ELTDXEMR
00298        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTDXEMR
00299      10  WS-DTL-SUPP-LONGA REDEFINES WS-DTL-SUPP-LONG.            ELTDXEMR
00300        15 WS-DTL-SUPP-A           PIC X(63).                      ELTDXEMR
00301                                                                   ELTDXEMR
00302    05  WS-SUPP-W-IN-DAYS-ACC.                                     ELTDXEMR
00303      10  FILLER                    PIC X(22)                      ELTDXEMR
00304         VALUE '  SUPPLEMENTAL: WITHIN'.                           ELTDXEMR
00305      10  WS-SUPP-DAYS-ACC          PIC ZZ9.                       ELTDXEMR
00306      10  FILLER                    PIC X(20)                      ELTDXEMR
00307         VALUE ' DAYS OF AN ACCIDENT'.                             ELTDXEMR
00308      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDXEMR
00309                                                                   ELTDXEMR
00310    05  WS-SUPP-W-IN-DAYS-MED.                                     ELTDXEMR
00311      10  FILLER                    PIC X(22)                      ELTDXEMR
00312         VALUE '  SUPPLEMENTAL: WITHIN'.                           ELTDXEMR
00313      10  WS-SUPP-DAYS-MED          PIC ZZ9.                       ELTDXEMR
00314      10  FILLER                    PIC X(19)                      ELTDXEMR
00315         VALUE ' DAYS OF AN ILLNESS'.                              ELTDXEMR
00316      10  FILLER                    PIC X(35) VALUE LOW-VALUES.    ELTDXEMR
00317                                                                   ELTDXEMR
00318    05  WS-SUPP-EXPLAIN1            PIC X(79).                     ELTDXEMR
00319    05  WS-SUPP-EXPLAIN2            PIC X(79).                     ELTDXEMR
00320                                                                   ELTDXEMR
00321    05  WS-SUPP-MAX.                                               ELTDXEMR
00322      10  FILLER                    PIC X(16)                      ELTDXEMR
00323          VALUE '  SUPPLEMENTAL: '.                                ELTDXEMR
00324      10  WS-SUPP-MAX-AMT           PIC ZZ9.99-.                   ELTDXEMR
00325      10  FILLER                    PIC X(60) VALUE LOW-VALUES.    ELTDXEMR
00326                                                                   ELTDXEMR
00327    05  WS-PAYABLE-AS.                                             ELTDXEMR
00328      10  FILLER                    PIC X(40) VALUE                ELTDXEMR
00329          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTDXEMR
00330      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTDXEMR
00331                                                                   ELTDXEMR
00332    05  WS-PAY-CONSDR-TEXT1.                                       ELTDXEMR
00333      10  FILLER                    PIC X(45)                      ELTDXEMR
00334        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDXEMR
00335                                                                   ELTDXEMR
00336    05  WS-PAY-CONSDR-TEXT2.                                       ELTDXEMR
00337      10  FILLER                    PIC X(44)                      ELTDXEMR
00338        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDXEMR
00339                                                                   ELTDXEMR
00340    05  WS-SPILLOVER-C              PIC X(21)                      ELTDXEMR
00341                                    VALUE 'SPILLOVER COINSURANCE'. ELTDXEMR
00342    05  WS-SPILLOVER-D              PIC X(20)                      ELTDXEMR
00343                                    VALUE 'SPILLOVER DEDUCTIBLE'.  ELTDXEMR
00344                                                                   ELTDXEMR
00345    05  WS-MAXIMUM-AMT-PER.                                        ELTDXEMR
00346      10  FILLER                    PIC X(29)                      ELTDXEMR
00347        VALUE 'THE MAXIMUM AMOUNT PER VISIT '.                     ELTDXEMR
00348      10  FILLER                    PIC X(50) VALUE LOW-VALUES.    ELTDXEMR
00349                                                                   ELTDXEMR
00350    05  WS-FOR-SUPP-ACCIDENT.                                      ELTDXEMR
00351      10  FILLER                    PIC X(26)                      ELTDXEMR
00352        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTDXEMR
00353      10  WS-NO-DAYS                PIC ZZ9.                       ELTDXEMR
00354      10  FILLER                    PIC X(09) VALUE ' DAYS OF '.   ELTDXEMR
00355      10  WS-FOR-SUPP-ACC-DESC      PIC X(41).                     ELTDXEMR
00356                                                                   ELTDXEMR
00357    05  WS-CONTRACT-RELATED.                                       ELTDXEMR
00358      10  FILLER                    PIC X(48)                      ELTDXEMR
00359        VALUE 'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTDXEMR
00360      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTDXEMR
00361                                                                   ELTDXEMR
00362    05  WS-PROF-INPT-CHRGES.                                       ELTDXEMR
00363        10  FILLER                  PIC  X(61) VALUE               ELTDXEMR
00364                'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CAELTDXEMR
00365 -              'RE REPORT: '.                                     ELTDXEMR
00366        10  FILLER                  PIC  X(18) VALUE LOW-VALUES.   ELTDXEMR
00367                                                                   ELTDXEMR
00368    05  WS-PROF-OUTPT-CHRGES.                                      ELTDXEMR
00369        10  FILLER                  PIC  X(62) VALUE               ELTDXEMR
00370                'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CELTDXEMR
00371 -              'ARE REPORT: '.                                    ELTDXEMR
00372        10  FILLER                  PIC  X(17) VALUE LOW-VALUES.   ELTDXEMR
00373                                                                   ELTDXEMR
00374    05  WS-PROVIDER-ELIGIBILITY.                                   ELTDXEMR
00375      10  FILLER                    PIC X(43)                      ELTDXEMR
00376        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY'.       ELTDXEMR
00377      10  FILLER                    PIC X(36) VALUE LOW-VALUES.    ELTDXEMR
00378                                                                   ELTDXEMR
00379    05  WS-NO-TABULAR1.                                            ELTDXEMR
00380      10  FILLER                    PIC X(51)  VALUE               ELTDXEMR
00381         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTDXEMR
00382      10  FILLER                    PIC X(22)  VALUE               ELTDXEMR
00383         'GOING FROM BENEFIT ***'.                                 ELTDXEMR
00384                                                                   ELTDXEMR
00385    05  WS-NO-TABULAR2.                                            ELTDXEMR
00386      10  FILLER                    PIC X(15)  VALUE               ELTDXEMR
00387         '*** PROVISION: '.                                        ELTDXEMR
00388      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTDXEMR
00389      10  FILLER                    PIC X VALUE SPACE.             ELTDXEMR
00390      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTDXEMR
00391      10  FILLER                    PIC X(13)  VALUE               ELTDXEMR
00392         ' TO TABULAR: '.                                          ELTDXEMR
00393      10  WS-NO-TAB-ID              PIC X(6).                      ELTDXEMR
00394      10  FILLER                    PIC X VALUE SPACE.             ELTDXEMR
00395      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTDXEMR
00396      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTDXEMR
00397                                                                   ELTDXEMR
00398    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTDXEMR
00399    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTDXEMR
00400      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTDXEMR
00401                                                                   ELTDXEMR
00402  01  WS-END                            PIC X(16)  VALUE           ELTDXEMR
00403      '*** W/S ENDS ***'.                                          ELTDXEMR
00404      TITLE 'LINKAGE  SECTION --- ELTEMERG'.                       ELTDXEMR
00405  LINKAGE SECTION.                                                 ELTDXEMR
00406  01  DFHCOMMAREA.                                                 ELTDXEMR
00407      COPY ELSCOMMC.                                               ELTDXEMR
00408 /  *** CIA  AREA ***                                              ELTDXEMR
00409      COPY ELSCIA2C.                                               ELTDXEMR
00410 /  *** IO PARM AREA ***                                           ELTDXEMR
00411      COPY ELSIOPMC.                                               ELTDXEMR
00412 /  *** KEY AREA ***                                               ELTDXEMR
00413      COPY ELSKEYSC.                                               ELTDXEMR
00414 /  *** OUTPUT TEXT AREA ***                                       ELTDXEMR
00415      COPY ELSOUTPC.                                               ELTDXEMR
00416 /  *** TOPIC SELECTION AREA ***                                   ELTDXEMR
00417      COPY ELSSSCBC.                                               ELTDXEMR
00418 /  *** CODE MANUAL INTERFACE ***                                  ELTDXEMR
00419      COPY ELSCMIFC.                                               ELTDXEMR
00420 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDXEMR
00421      COPY ELSCMDSC.                                               ELTDXEMR
00422 /  *** BENEFIT PROVISION TABLE ***                                ELTDXEMR
00423      COPY ELSPRVNC.                                               ELTDXEMR
00424 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDXEMR
00425      COPY ELSTCWAC.                                               ELTDXEMR
00426 / *  P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDXEMR
00427      COPY ELSPLGSW.                                               ELTDXEMR
00428                                                                   ELTDXEMR
00429 / *  B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDXEMR
00430      COPY ELSPLGTB.                                               ELTDXEMR
00431 / **     C O N T R A C T   R E C O R D                            ELTDXEMR
00432  01  CONTRACT-RECORD.                                             ELTDXEMR
00433      COPY GCCONTRC.                                               ELTDXEMR
00434      TITLE 'DIAGNOSTIC EMERGENCY CARE TOPIC'                      ELTDXEMR
00435  PROCEDURE DIVISION.                                              ELTDXEMR
00436                                                                   ELTDXEMR
00437 ******************************************************************ELTDXEMR
00438 *                                                                 ELTDXEMR
00439 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDXEMR
00440 *                                                                 ELTDXEMR
00441 ******************************************************************ELTDXEMR
00442  0000-MAINLINE.                                                   ELTDXEMR
00443                                                                   ELTDXEMR
00444      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTDXEMR
00445          EXEC CICS ABEND                                          ELTDXEMR
00446                    ABCODE ('EL01')                                ELTDXEMR
00447          END-EXEC                                                 ELTDXEMR
00448      END-IF.                                                      ELTDXEMR
00449                                                                   ELTDXEMR
00450      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDXEMR
00451                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELTDXEMR
00452                                                                   ELTDXEMR
00453 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTDXEMR
00454                                                                   ELTDXEMR
00455      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDXEMR
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00457                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTDXEMR
00458      IF NOT CIA-RC-OK                                             ELTDXEMR
00459          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00460                                                                   ELTDXEMR
00461      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDXEMR
00462      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00463                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELTDXEMR
00464      IF NOT CIA-RC-OK                                             ELTDXEMR
00465          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00466                                                                   ELTDXEMR
00467      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDXEMR
00468      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00469                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELTDXEMR
00470      IF NOT CIA-RC-OK                                             ELTDXEMR
00471          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00472                                                                   ELTDXEMR
00473      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDXEMR
00474      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00475                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELTDXEMR
00476      IF NOT CIA-RC-OK                                             ELTDXEMR
00477          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00478                                                                   ELTDXEMR
00479      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDXEMR
00480      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00481                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELTDXEMR
00482      IF NOT CIA-RC-OK                                             ELTDXEMR
00483          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00484                                                                   ELTDXEMR
00485      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDXEMR
00486      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00487                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELTDXEMR
00488      IF NOT CIA-RC-OK                                             ELTDXEMR
00489          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTDXEMR
00490                                                                   ELTDXEMR
00491      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXEMR
00492      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDXEMR
00493              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTDXEMR
00494                                                                   ELTDXEMR
00495      SET CIA-STG-GETMAIN TO TRUE.                                 ELTDXEMR
00496      EXEC CICS LINK                                               ELTDXEMR
00497                PROGRAM('ELUSTGMG')                                ELTDXEMR
00498                COMMAREA(DFHCOMMAREA)                              ELTDXEMR
00499      END-EXEC.                                                    ELTDXEMR
00500                                                                   ELTDXEMR
00501      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDXEMR
00502      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00503                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELTDXEMR
00504                                                                   ELTDXEMR
00505      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDXEMR
00506      PERFORM 9999-CHECK-CONTRACT.                                 ELTDXEMR
00507                                                                   ELTDXEMR
00508      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH)           ELTDXEMR
00509         PERFORM 1000-INSTITUTIONAL-EMR-RTNE.                      ELTDXEMR
00510                                                                   ELTDXEMR
00511      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH)            ELTDXEMR
00512         PERFORM 2000-PROFESSIONAL-EMR-RTNE.                       ELTDXEMR
00513                                                                   ELTDXEMR
00514      IF (NOT SSB-PROV-CLASS-PROF AND   NOT SSB-PROV-CLASS-BOTH ANDELTDXEMR
00515          NOT SSB-PROV-CLASS-INST)                                 ELTDXEMR
00516         MOVE SPACE  TO  COF-FUNCTION                              ELTDXEMR
00517         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTDXEMR
00518         MOVE 2  TO  COF-NBR-DTL-LINES                             ELTDXEMR
00519         MOVE '*** I N V A L I D   R E Q U E S T ***'  TO          ELTDXEMR
00520                                         COF-DTL-LINE(2)           ELTDXEMR
00521         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDXEMR
00522               COMMAREA(DFHCOMMAREA)                               ELTDXEMR
00523         END-EXEC.                                                 ELTDXEMR
00524                                                                   ELTDXEMR
00525      MOVE 'E'  TO  COF-FUNCTION.                                  ELTDXEMR
00526      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDXEMR
00527                     COF-NBR-DTL-LINES.                            ELTDXEMR
00528      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00529      END-EXEC.                                                    ELTDXEMR
00530                                                                   ELTDXEMR
00531      EXEC CICS RETURN   END-EXEC.                                 ELTDXEMR
00532      GOBACK.                                                      ELTDXEMR
00533                                                                   ELTDXEMR
00534  0098-SIGNAL-UNALL-AREA-ERROR.                                    ELTDXEMR
00535      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTDXEMR
00536      EXEC CICS ABEND                                              ELTDXEMR
00537                ABCODE (CIA-ABCODE)                                ELTDXEMR
00538      END-EXEC.                                                    ELTDXEMR
00539                                                                   ELTDXEMR
00540      TITLE 'INSTITUTIONAL  IP'.                                   ELTDXEMR
00541  1000-INSTITUTIONAL-EMR-RTNE.                                     ELTDXEMR
00542 ********************EMR****************************************** ELTDXEMR
00543 *        I N S T I T U T I O N A L   I P   R T N E                ELTDXEMR
00544 *                                                                 ELTDXEMR
00545 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDXEMR
00546 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDXEMR
00547 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDXEMR
00548 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDXEMR
00549 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDXEMR
00550 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDXEMR
00551 *  MODULE.                                                        ELTDXEMR
00552 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDXEMR
00553 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDXEMR
00554 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDXEMR
00555 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDXEMR
00556 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDXEMR
00557 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDXEMR
00558 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDXEMR
00559 *                                                                 ELTDXEMR
00560 ***************************************************************** ELTDXEMR
00561      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXEMR
00562      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTDXEMR
00563      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDXEMR
00564                     COF-NBR-DTL-LINES.                            ELTDXEMR
00565      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00566      END-EXEC.                                                    ELTDXEMR
00567      MOVE SPACE  TO  COF-FUNCTION.                                ELTDXEMR
00568      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDXEMR
00569      MOVE WS-HDR-2-INST-EMR  TO  COF-HDR-LINE(2).                 ELTDXEMR
00570                                                                   ELTDXEMR
00571      MOVE WS-EMR-CNT  TO  PVN-NBR-BEN-PROVN.                      ELTDXEMR
00572      PERFORM 1010-MOVE-IN-INST-ACC                                ELTDXEMR
00573         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXEMR
00574         UNTIL WS-SUB  >  WS-EMR-CNT.                              ELTDXEMR
00575      PERFORM 1020-CALL-COVERAGE.                                  ELTDXEMR
00576      IF PVN-COVG-NONE                                             ELTDXEMR
00577         CONTINUE                                                  ELTDXEMR
00578      ELSE                                                         ELTDXEMR
00579         PERFORM 1025-CONT-BENEFIT-PROV.                           ELTDXEMR
00580      PERFORM 1030-FIND-FIRST-NONZERO                              ELTDXEMR
00581         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXEMR
00582         UNTIL WS-SUB  >  WS-EMR-CNT.                              ELTDXEMR
00583                                                                   ELTDXEMR
00584  1010-MOVE-IN-INST-ACC.                                           ELTDXEMR
00585      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXEMR
00586      MOVE WS-INST-EMR-LIST(WS-SUB)  TO                            ELTDXEMR
00587                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXEMR
00588      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTDXEMR
00589                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTDXEMR
00590                                                                   ELTDXEMR
00591  1020-CALL-COVERAGE.                                              ELTDXEMR
00592      MOVE LOW-VALUES  TO  COF-DTL-LINE(1).                        ELTDXEMR
00593      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXEMR
00594      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00595      END-EXEC.                                                    ELTDXEMR
00596                                                                   ELTDXEMR
00597      MOVE WS-EMERGENCY-SERVICES  TO  SSB-TOPIC-PHRASE.            ELTDXEMR
00598      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDXEMR
00599      END-EXEC.                                                    ELTDXEMR
00600      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTDXEMR
00601      MOVE LOW-VALUES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).        ELTDXEMR
00602      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00603      END-EXEC.                                                    ELTDXEMR
00604                                                                   ELTDXEMR
00605  1025-CONT-BENEFIT-PROV.                                          ELTDXEMR
00606      MOVE +1  TO  WS-CIA.                                         ELTDXEMR
00607      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTDXEMR
00608      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXEMR
00609            PSP-PROVN-PRICING-METHD,                               ELTDXEMR
00610            PSP-TRANSF-OTHER-RESP-IND,                             ELTDXEMR
00611            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDXEMR
00612            PSP-SPILL-OVER-DED-APL-IND,                            ELTDXEMR
00613            PSB-PROF-CHRG-HSP-CLM.                                 ELTDXEMR
00614                                                                   ELTDXEMR
00615      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXEMR
00616      END-EXEC.                                                    ELTDXEMR
00617                                                                   ELTDXEMR
00618      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXEMR
00619      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00620                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTDXEMR
00621                                                                   ELTDXEMR
00622  1030-FIND-FIRST-NONZERO.                                         ELTDXEMR
00623      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXEMR
00624      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDXEMR
00625         NEXT SENTENCE                                             ELTDXEMR
00626      ELSE                                                         ELTDXEMR
00627         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTDXEMR
00628                                                                   ELTDXEMR
00629  1040-BUILD-SCREEN-LINES.                                         ELTDXEMR
00630      SET PLT-INDEX1  TO                                           ELTDXEMR
00631                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTDXEMR
00632      IF WS-NOT-FIRST-TIME                                         ELTDXEMR
00633         MOVE 'P'  TO  COF-FUNCTION                                ELTDXEMR
00634         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTDXEMR
00635         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDXEMR
00636             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
00637         END-EXEC                                                  ELTDXEMR
00638      ELSE                                                         ELTDXEMR
00639         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTDXEMR
00640      MOVE 1  TO  WS-CIA.                                          ELTDXEMR
00641      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXEMR
00642         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXEMR
00643            SET PLT-INDEX2  TO  2                                  ELTDXEMR
00644         ELSE                                                      ELTDXEMR
00645            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTDXEMR
00646            SET WS-INDEX-PROBLEM TO TRUE                           ELTDXEMR
00647      ELSE                                                         ELTDXEMR
00648         SET PLT-INDEX2  TO  1                                     ELTDXEMR
00649      END-IF.                                                      ELTDXEMR
00650      PERFORM 1045-PROCESS-BP-TOPIC.                               ELTDXEMR
00651                                                                   ELTDXEMR
00652  1045-PROCESS-BP-TOPIC.                                           ELTDXEMR
00653      PERFORM 1050-LIST-BEN-PROV.                                  ELTDXEMR
00654      PERFORM 1080-PLACE-OF-TREATMENT.                             ELTDXEMR
00655      PERFORM 2200-DAYS-OF-INST-TREATMENT.                         ELTDXEMR
00656      PERFORM 1095-PROVISION-PRICING-METHOD.                       ELTDXEMR
00657      PERFORM 2010-PROF-CHRGS-HSP-BILL.                            ELTDXEMR
00658      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTDXEMR
00659      PERFORM 2020-SPILL-OVER-COINS.                               ELTDXEMR
00660      PERFORM 2030-SPILL-OVER-DED.                                 ELTDXEMR
00661      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXEMR
00662      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXEMR
00663      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXEMR
00664                                                                   ELTDXEMR
00665  1050-LIST-BEN-PROV.                                              ELTDXEMR
00666      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTDXEMR
00667      ADD  +1  TO  WS-CIA.                                         ELTDXEMR
00668      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDXEMR
00669                                                                   ELTDXEMR
00670      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTDXEMR
00671         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTDXEMR
00672         UNTIL  PVN-BEN-PROVN-IDX > WS-EMR-CNT.                    ELTDXEMR
00673      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXEMR
00674                                                                   ELTDXEMR
00675      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTDXEMR
00676      MOVE +1  TO  WS-CIA                                          ELTDXEMR
00677      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00678      END-EXEC.                                                    ELTDXEMR
00679                                                                   ELTDXEMR
00680 ***************************************************************   ELTDXEMR
00681 *IN THE ORIGINAL EAC/EMC BP PROGRAMS POT IS NOT CONCERNED WITH    ELTDXEMR
00682 * BASIC OR SUPPLEMENTAL SPLIT, SO WE ARE NOT IN THIS PROG.        ELTDXEMR
00683 ***************************************************************** ELTDXEMR
00684  1080-PLACE-OF-TREATMENT.                                         ELTDXEMR
00685      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXEMR
00686      SET WS-NOT-BASIC-NOW TO TRUE.                                ELTDXEMR
00687      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTDXEMR
00688                                                              ZERO ELTDXEMR
00689         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00690         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTDXEMR
00691         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTDXEMR
00692                                               TO  CMF-CODE-VALUE  ELTDXEMR
00693         MOVE 1 TO TCAR-FROM-SUB                                   ELTDXEMR
00694         MOVE WS-SERVICES-RENDERED  TO                             ELTDXEMR
00695                   TCAR-FROM-LINE(TCAR-FROM-SUB)                   ELTDXEMR
00696         ADD 1 TO TCAR-FROM-SUB                                    ELTDXEMR
00697         SET WS-PROCESS-POT TO TRUE                                ELTDXEMR
00698         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00699         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00700         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
00701                    WS-BASIC-SWITCH                                ELTDXEMR
00702                    TCAR-FROM-AREA.                                ELTDXEMR
00703                                                                   ELTDXEMR
00704  1095-PROVISION-PRICING-METHOD.                                   ELTDXEMR
00705      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
00706      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
00707         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
00708                                             ZERO AND  NOT = '19'  ELTDXEMR
00709         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDXEMR
00710      ELSE                                                         ELTDXEMR
00711         SET  PLT-INDEX2  TO  2                                    ELTDXEMR
00712         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTDXEMR
00713            PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT = ELTDXEMR
00714                  ZERO AND  NOT = '19' AND  NOT WS-ADD-A-BLANK-LINEELTDXEMR
00715            MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)           ELTDXEMR
00716            SET SUPP-ONLY TO TRUE                                  ELTDXEMR
00717                                                                   ELTDXEMR
00718      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
00719      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
00720         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDXEMR
00721                               AND                                 ELTDXEMR
00722         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTDXEMR
00723         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTDXEMR
00724         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTDXEMR
00725         ADD +1  TO  WS-CIA.                                       ELTDXEMR
00726                                                                   ELTDXEMR
00727      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTDXEMR
00728         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXEMR
00729         SET  PLT-INDEX2  TO  2                                    ELTDXEMR
00730         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDXEMR
00731                                                              ZERO ELTDXEMR
00732            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDXEMR
00733            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDXEMR
00734            ADD +1  TO  WS-CIA.                                    ELTDXEMR
00735                                                                   ELTDXEMR
00736      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
00737      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
00738         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
00739                                                              ZERO ELTDXEMR
00740         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00741         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXEMR
00742         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTDXEMR
00743                                          TO   CMF-CODE-VALUE      ELTDXEMR
00744         IF WS-BASIC-LOB                                           ELTDXEMR
00745            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXEMR
00746         END-IF                                                    ELTDXEMR
00747         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00748         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00749      END-IF.                                                      ELTDXEMR
00750      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
00751                                                                   ELTDXEMR
00752      SET  PLT-INDEX2  TO  2.                                      ELTDXEMR
00753      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
00754         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
00755                                                            ZERO   ELTDXEMR
00756         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00757         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXEMR
00758         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXEMR
00759                                     CMF-CODE-VALUE                ELTDXEMR
00760         IF WS-BASIC-LOB                                           ELTDXEMR
00761            SET PROCESSING-SUPPLEMENTAL TO TRUE                    ELTDXEMR
00762         END-IF                                                    ELTDXEMR
00763         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00764         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00765      END-IF.                                                      ELTDXEMR
00766      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
00767      INITIALIZE WS-SUPP-SWITCH.                                   ELTDXEMR
00768                                                                   ELTDXEMR
00769  2010-PROF-CHRGS-HSP-BILL.                                        ELTDXEMR
00770      SET PLT-INDEX2  TO  1.                                       ELTDXEMR
00771      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTDXEMR
00772          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXEMR
00773              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXEMR
00774                               NOT  =  '0'  AND  NOT  =  LOW-VALUESELTDXEMR
00775                  MOVE WS-PROF-INPT-CHRGES                         ELTDXEMR
00776                                  TO  COF-DTL-LINE (WS-CIA)        ELTDXEMR
00777 *                MOVE 'Y'        TO  WS-ADD-A-BLANK-IND           ELTDXEMR
00778 *                ADD  +1         TO  WS-CIA.                      ELTDXEMR
00779                                                                   ELTDXEMR
00780      SET PLT-INDEX2  TO  2.                                       ELTDXEMR
00781                                                                   ELTDXEMR
00782      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTDXEMR
00783          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXEMR
00784                       AND NOT WS-ADD-A-BLANK-LINE                 ELTDXEMR
00785            IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)      ELTDXEMR
00786                               NOT  =  '0'  AND  NOT  =  LOW-VALUESELTDXEMR
00787              MOVE WS-PROF-INPT-CHRGES                             ELTDXEMR
00788                             TO  COF-DTL-LINE (WS-CIA).            ELTDXEMR
00789 *            MOVE 'Y'       TO  WS-ADD-A-BLANK-IND                ELTDXEMR
00790 *            ADD  +1        TO  WS-CIA                            ELTDXEMR
00791                                                                   ELTDXEMR
00792      SET PLT-INDEX2  TO  1.                                       ELTDXEMR
00793      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTDXEMR
00794          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXEMR
00795              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXEMR
00796                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTDXEMR
00797                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTDXEMR
00798                MOVE 'PROF-CHRG-HSP-CLM'                           ELTDXEMR
00799                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTDXEMR
00800                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTDXEMR
00801                                   TO  CMF-CODE-VALUE              ELTDXEMR
00802                IF WS-BASIC-LOB                                    ELTDXEMR
00803                    SET PROCESSING-BASIC-INFO TO TRUE              ELTDXEMR
00804                END-IF                                             ELTDXEMR
00805                PERFORM 9000-CALL-CODES-MANUAL                     ELTDXEMR
00806                PERFORM 9100-DETERMINE-OUTPUT-METHOD.              ELTDXEMR
00807                                                                   ELTDXEMR
00808      SET PLT-INDEX2  TO  2.                                       ELTDXEMR
00809                                                                   ELTDXEMR
00810      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTDXEMR
00811          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDXEMR
00812              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDXEMR
00813                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTDXEMR
00814                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTDXEMR
00815                MOVE 'PROF-CHRG-HSP-CLM'                           ELTDXEMR
00816                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTDXEMR
00817                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTDXEMR
00818                                   TO  CMF-CODE-VALUE              ELTDXEMR
00819                IF WS-BASIC-LOB                                    ELTDXEMR
00820                    SET PROCESSING-SUPPLEMENTAL TO TRUE            ELTDXEMR
00821                END-IF                                             ELTDXEMR
00822                PERFORM 9000-CALL-CODES-MANUAL                     ELTDXEMR
00823                PERFORM 9100-DETERMINE-OUTPUT-METHOD.              ELTDXEMR
00824                                                                   ELTDXEMR
00825                                                                   ELTDXEMR
00826  2020-SPILL-OVER-COINS.                                           ELTDXEMR
00827      SET PLT-INDEX2  TO  1.                                       ELTDXEMR
00828      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTDXEMR
00829         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXEMR
00830                                                       NOT =  '0'  ELTDXEMR
00831         MOVE WS-SPILLOVER-C  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
00832         ADD 1 TO WS-CIA                                           ELTDXEMR
00833         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00834         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTDXEMR
00835                                           CMF-ELEMENT-SYSTEM-NAME ELTDXEMR
00836         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTDXEMR
00837                                           TO   CMF-CODE-VALUE     ELTDXEMR
00838         IF WS-BASIC-LOB                                           ELTDXEMR
00839            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXEMR
00840         END-IF                                                    ELTDXEMR
00841         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00842         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00843         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
00844                    WS-BASIC-SWITCH                                ELTDXEMR
00845                    TCAR-FROM-AREA.                                ELTDXEMR
00846                                                                   ELTDXEMR
00847      SET PLT-INDEX2  TO  2.                                       ELTDXEMR
00848      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTDXEMR
00849         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXEMR
00850                                                       NOT =  '0'  ELTDXEMR
00851         MOVE WS-SPILLOVER-C  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
00852         ADD 1 TO WS-CIA                                           ELTDXEMR
00853         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00854         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTDXEMR
00855                                           CMF-ELEMENT-SYSTEM-NAME ELTDXEMR
00856         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTDXEMR
00857                                           TO   CMF-CODE-VALUE     ELTDXEMR
00858         IF WS-BASIC-LOB                                           ELTDXEMR
00859            SET PROCESSING-SUPPLEMENTAL TO TRUE                    ELTDXEMR
00860         END-IF                                                    ELTDXEMR
00861         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00862         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00863         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
00864                    WS-BASIC-SWITCH                                ELTDXEMR
00865                    TCAR-FROM-AREA.                                ELTDXEMR
00866                                                                   ELTDXEMR
00867  2030-SPILL-OVER-DED.                                             ELTDXEMR
00868      SET WS-NOT-BASIC-NOW TO TRUE.                                ELTDXEMR
00869      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTDXEMR
00870         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXEMR
00871                                                       NOT =  '0'  ELTDXEMR
00872 *       MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDXEMR
00873         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00874         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTDXEMR
00875         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTDXEMR
00876                                                     CMF-CODE-VALUEELTDXEMR
00877         IF WS-BASIC-LOB                                           ELTDXEMR
00878            SET PROCESSING-SUPPLEMENTAL TO TRUE                    ELTDXEMR
00879         END-IF                                                    ELTDXEMR
00880         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
00881         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
00882         MOVE WS-SPILLOVER-D  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
00883         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
00884                    WS-BASIC-SWITCH                                ELTDXEMR
00885                    TCAR-FROM-AREA.                                ELTDXEMR
00886                                                                   ELTDXEMR
00887  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTDXEMR
00888      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDXEMR
00889         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
00890         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXEMR
00891         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXEMR
00892                                                   CMF-CODE-VALUE  ELTDXEMR
00893         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTDXEMR
00894         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTDXEMR
00895         PERFORM 2100-CODES-MANUAL-LONG                            ELTDXEMR
00896         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXEMR
00897         IF WS-CIA  >  20 OR  =  20                                ELTDXEMR
00898            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTDXEMR
00899            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXEMR
00900                COMMAREA(DFHCOMMAREA)                              ELTDXEMR
00901            END-EXEC                                               ELTDXEMR
00902            MOVE +1  TO  WS-CIA.                                   ELTDXEMR
00903                                                                   ELTDXEMR
00904  1090-PROBLEM-WITH-INDICES.                                       ELTDXEMR
00905                                                                   ELTDXEMR
00906      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXEMR
00907      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXEMR
00908      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXEMR
00909                                                                   ELTDXEMR
00910      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00911      END-EXEC.                                                    ELTDXEMR
00912                                                                   ELTDXEMR
00913 /        P R O F E S S I O N A L   I P   R T N E                  ELTDXEMR
00914 ***************************************************************** ELTDXEMR
00915 *        P R O F E S S I O N A L   I P   R T N E                  ELTDXEMR
00916 *                                                                 ELTDXEMR
00917 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTDXEMR
00918 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTDXEMR
00919 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTDXEMR
00920 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTDXEMR
00921 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTDXEMR
00922 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTDXEMR
00923 *  MODULE.                                                        ELTDXEMR
00924 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTDXEMR
00925 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTDXEMR
00926 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTDXEMR
00927 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTDXEMR
00928 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTDXEMR
00929 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTDXEMR
00930 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTDXEMR
00931 *                                                                 ELTDXEMR
00932 ***************************************************************** ELTDXEMR
00933  2000-PROFESSIONAL-EMR-RTNE.                                      ELTDXEMR
00934      INITIALIZE WS-INDEX-PROBLEM-SW.                              ELTDXEMR
00935      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXEMR
00936      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTDXEMR
00937      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTDXEMR
00938                     COF-NBR-DTL-LINES.                            ELTDXEMR
00939      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00940      END-EXEC.                                                    ELTDXEMR
00941      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDXEMR
00942      MOVE WS-HDR-2-PROF-EMR  TO  COF-HDR-LINE(2).                 ELTDXEMR
00943      MOVE WS-PROF-EMR-CNT  TO  PVN-NBR-BEN-PROVN.                 ELTDXEMR
00944      PERFORM 2010-MOVE-IN-PROF-EMR                                ELTDXEMR
00945         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDXEMR
00946         UNTIL WS-SUB  >  WS-PROF-EMR-CNT.                         ELTDXEMR
00947      PERFORM 2020-CALL-COVERAGE.                                  ELTDXEMR
00948      IF PVN-COVG-NONE                                             ELTDXEMR
00949         CONTINUE                                                  ELTDXEMR
00950      ELSE                                                         ELTDXEMR
00951         PERFORM 2025-CONT-BENEFIT-PROV                            ELTDXEMR
00952      END-IF.                                                      ELTDXEMR
00953      PERFORM 2030-FIND-FIRST-NONZERO                              ELTDXEMR
00954         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDXEMR
00955         UNTIL WS-SUB  >  WS-PROF-EMR-CNT.                         ELTDXEMR
00956                                                                   ELTDXEMR
00957  2010-MOVE-IN-PROF-EMR.                                           ELTDXEMR
00958      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDXEMR
00959      MOVE WS-PROF-EMR-LIST(WS-SUB)  TO                            ELTDXEMR
00960                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDXEMR
00961      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTDXEMR
00962                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTDXEMR
00963                                                                   ELTDXEMR
00964  2020-CALL-COVERAGE.                                              ELTDXEMR
00965      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXEMR
00966      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00967      END-EXEC.                                                    ELTDXEMR
00968                                                                   ELTDXEMR
00969      MOVE WS-EMERGENCY-SERVICES  TO  SSB-TOPIC-PHRASE.            ELTDXEMR
00970                                                                   ELTDXEMR
00971      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDXEMR
00972      END-EXEC.                                                    ELTDXEMR
00973                                                                   ELTDXEMR
00974      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTDXEMR
00975      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
00976      END-EXEC.                                                    ELTDXEMR
00977                                                                   ELTDXEMR
00978  2025-CONT-BENEFIT-PROV.                                          ELTDXEMR
00979      MOVE +1  TO  WS-CIA.                                         ELTDXEMR
00980      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDXEMR
00981      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDXEMR
00982            PSP-PROVN-PRICING-METHD,                               ELTDXEMR
00983            PSP-TRANSF-OTHER-RESP-IND,                             ELTDXEMR
00984            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDXEMR
00985            PSP-SPILL-OVER-DED-APL-IND,                            ELTDXEMR
00986            PSE-BEN-SCOPE-ID,                                      ELTDXEMR
00987            PSE-MAX-AMT-PER-VISIT                                  ELTDXEMR
00988      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDXEMR
00989      END-EXEC.                                                    ELTDXEMR
00990      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDXEMR
00991      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
00992                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTDXEMR
00993                                                                   ELTDXEMR
00994  2030-FIND-FIRST-NONZERO.                                         ELTDXEMR
00995      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXEMR
00996      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDXEMR
00997         NEXT SENTENCE                                             ELTDXEMR
00998      ELSE                                                         ELTDXEMR
00999         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTDXEMR
01000                                                                   ELTDXEMR
01001  2040-BUILD-SCREEN-LINES.                                         ELTDXEMR
01002      SET PLT-INDEX1   TO                                          ELTDXEMR
01003                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTDXEMR
01004      IF WS-NOT-FIRST-TIME                                         ELTDXEMR
01005         MOVE 'P'  TO  COF-FUNCTION                                ELTDXEMR
01006         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDXEMR
01007             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
01008         END-EXEC                                                  ELTDXEMR
01009      ELSE                                                         ELTDXEMR
01010         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTDXEMR
01011      MOVE +1  TO  WS-CIA.                                         ELTDXEMR
01012      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXEMR
01013         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDXEMR
01014            SET PLT-INDEX2  TO  2                                  ELTDXEMR
01015         ELSE                                                      ELTDXEMR
01016            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTDXEMR
01017            SET WS-INDEX-PROBLEM TO TRUE                           ELTDXEMR
01018         END-IF                                                    ELTDXEMR
01019      ELSE                                                         ELTDXEMR
01020         SET PLT-INDEX2  TO  1                                     ELTDXEMR
01021      END-IF.                                                      ELTDXEMR
01022      PERFORM 2045-PROCESS-BP-TOPIC.                               ELTDXEMR
01023                                                                   ELTDXEMR
01024  2045-PROCESS-BP-TOPIC.                                           ELTDXEMR
01025      PERFORM 2050-LIST-BEN-PROV.                                  ELTDXEMR
01026      PERFORM 2080-PLACE-OF-TREATMENT.                             ELTDXEMR
01027      PERFORM 2085-BENEFIT-SCOPE-ID.                               ELTDXEMR
01028      PERFORM 2300-DAYS-OF-PROF-TREATMENT.                         ELTDXEMR
01029      PERFORM 2095-PROVISION-PRICING-METHOD.                       ELTDXEMR
01030      PERFORM 2100-MAX-AMT-PER-VISIT.                              ELTDXEMR
01031      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTDXEMR
01032      PERFORM 2125-SPILL-OVER-COINS.                               ELTDXEMR
01033      PERFORM 2150-SPILL-OVER-DED.                                 ELTDXEMR
01034      PERFORM 4900-BEN-TAB-PVE.                                    ELTDXEMR
01035      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDXEMR
01036                                                                   ELTDXEMR
01037  2050-LIST-BEN-PROV.                                              ELTDXEMR
01038      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTDXEMR
01039      ADD  +1  TO  WS-CIA.                                         ELTDXEMR
01040      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDXEMR
01041      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTDXEMR
01042         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTDXEMR
01043         UNTIL PVN-BEN-PROVN-IDX > WS-EMR-CNT.                     ELTDXEMR
01044      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDXEMR
01045                                                                   ELTDXEMR
01046      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTDXEMR
01047      MOVE +1  TO  WS-CIA.                                         ELTDXEMR
01048      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
01049      END-EXEC.                                                    ELTDXEMR
01050 *    MOVE +1  TO  WS-CIA.                                         ELTDXEMR
01051                                                                   ELTDXEMR
01052  2080-PLACE-OF-TREATMENT.                                         ELTDXEMR
01053      INITIALIZE WS-BASIC-SUPP-SWITCH.                             ELTDXEMR
01054      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTDXEMR
01055                                                              ZERO ELTDXEMR
01056         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01057         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTDXEMR
01058         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTDXEMR
01059                                               TO  CMF-CODE-VALUE  ELTDXEMR
01060         MOVE 1 TO TCAR-FROM-SUB                                   ELTDXEMR
01061         MOVE WS-SERVICES-RENDERED                                 ELTDXEMR
01062                     TO  TCAR-FROM-LINE(TCAR-FROM-SUB)             ELTDXEMR
01063         ADD 1 TO TCAR-FROM-SUB                                    ELTDXEMR
01064         SET WS-PROCESS-POT TO TRUE                                ELTDXEMR
01065         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01066         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01067         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01068                    WS-BASIC-SWITCH                                ELTDXEMR
01069                    TCAR-FROM-AREA.                                ELTDXEMR
01070                                                                   ELTDXEMR
01071  2085-BENEFIT-SCOPE-ID.                                           ELTDXEMR
01072      SET PLT-INDEX2  TO  1.                                       ELTDXEMR
01073      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
01074         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTDXEMR
01075                                       '0000' AND  NOT =  '00  '   ELTDXEMR
01076 *       MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDXEMR
01077         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTDXEMR
01078 *       ADD  +1  TO  WS-CIA.                                      ELTDXEMR
01079                                                                   ELTDXEMR
01080      SET PLT-INDEX2  TO  2.                                       ELTDXEMR
01081      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
01082         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTDXEMR
01083                                  '0000' AND  NOT =  '00  ' AND    ELTDXEMR
01084         NOT WS-ADD-A-BLANK-LINE                                   ELTDXEMR
01085 *       MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDXEMR
01086         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTDXEMR
01087 *       ADD  +1  TO  WS-CIA.                                      ELTDXEMR
01088                                                                   ELTDXEMR
01089      SET PLT-INDEX2  TO  1.                                       ELTDXEMR
01090      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
01091         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTDXEMR
01092                                       '0000' AND  NOT =  '00  '   ELTDXEMR
01093         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTDXEMR
01094         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDXEMR
01095         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDXEMR
01096                                                    CMF-CODE-VALUE ELTDXEMR
01097         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01098         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01099         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01100                    WS-BASIC-SWITCH                                ELTDXEMR
01101                    TCAR-FROM-AREA.                                ELTDXEMR
01102                                                                   ELTDXEMR
01103      SET PLT-INDEX2  TO  2.                                       ELTDXEMR
01104      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
01105         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTDXEMR
01106                                       '0000' AND  NOT =  '00  '   ELTDXEMR
01107         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTDXEMR
01108         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDXEMR
01109         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDXEMR
01110                                    CMF-CODE-VALUE                 ELTDXEMR
01111         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01112         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01113         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01114                    WS-BASIC-SWITCH                                ELTDXEMR
01115                    TCAR-FROM-AREA.                                ELTDXEMR
01116                                                                   ELTDXEMR
01117  2095-PROVISION-PRICING-METHOD.                                   ELTDXEMR
01118      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
01119      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
01120      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
01121         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
01122                                            ZERO AND  NOT =  '19'  ELTDXEMR
01123         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTDXEMR
01124      ELSE                                                         ELTDXEMR
01125         SET  PLT-INDEX2  TO  2                                    ELTDXEMR
01126         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDXEMR
01127            AND PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)    ELTDXEMR
01128                       NOT = ZERO AND  NOT =  '19' AND             ELTDXEMR
01129               NOT WS-ADD-A-BLANK-LINE                             ELTDXEMR
01130            SET SUPP-ONLY TO TRUE                                  ELTDXEMR
01131            MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).          ELTDXEMR
01132                                                                   ELTDXEMR
01133      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
01134      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
01135         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTDXEMR
01136                               AND                                 ELTDXEMR
01137         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTDXEMR
01138         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTDXEMR
01139         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01140         ADD +1  TO  WS-CIA.                                       ELTDXEMR
01141                                                                   ELTDXEMR
01142      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTDXEMR
01143         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXEMR
01144         SET  PLT-INDEX2  TO  2                                    ELTDXEMR
01145         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTDXEMR
01146                                                              ZERO ELTDXEMR
01147            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTDXEMR
01148            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTDXEMR
01149            ADD +1  TO  WS-CIA.                                    ELTDXEMR
01150                                                                   ELTDXEMR
01151      SET  PLT-INDEX2  TO  1.                                      ELTDXEMR
01152      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTDXEMR
01153         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
01154                                                              ZERO ELTDXEMR
01155         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01156         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXEMR
01157         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXEMR
01158                                                    CMF-CODE-VALUE ELTDXEMR
01159         IF WS-BASIC-LOB                                           ELTDXEMR
01160            SET PROCESSING-BASIC-INFO TO TRUE                      ELTDXEMR
01161         END-IF                                                    ELTDXEMR
01162         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01163         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01164         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01165                    WS-BASIC-SWITCH                                ELTDXEMR
01166                    TCAR-FROM-AREA.                                ELTDXEMR
01167                                                                   ELTDXEMR
01168      SET  PLT-INDEX2  TO  2.                                      ELTDXEMR
01169      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
01170         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTDXEMR
01171                                                              ZERO ELTDXEMR
01172         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01173         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTDXEMR
01174         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTDXEMR
01175                                                    CMF-CODE-VALUE ELTDXEMR
01176         IF WS-BASIC-LOB                                           ELTDXEMR
01177            SET PROCESSING-SUPPLEMENTAL TO TRUE                    ELTDXEMR
01178         END-IF                                                    ELTDXEMR
01179         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01180         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01181         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01182                    WS-BASIC-SWITCH                                ELTDXEMR
01183                    TCAR-FROM-AREA.                                ELTDXEMR
01184         INITIALIZE WS-SUPP-SWITCH.                                ELTDXEMR
01185                                                                   ELTDXEMR
01186  2100-MAX-AMT-PER-VISIT.                                          ELTDXEMR
01187      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDXEMR
01188         SET  PLT-INDEX2  TO  1                                    ELTDXEMR
01189         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTDXEMR
01190                                                              ZERO ELTDXEMR
01191 *          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTDXEMR
01192            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTDXEMR
01193            ADD +1  TO  WS-CIA                                     ELTDXEMR
01194            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTDXEMR
01195                                                  WS-BASIC-MAX-AMT ELTDXEMR
01196            MOVE WS-BASIC-MAX  TO  COF-DTL-LINE(WS-CIA)            ELTDXEMR
01197 *          ADD +1  TO  WS-CIA.                                    ELTDXEMR
01198            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXEMR
01199                      COMMAREA(DFHCOMMAREA)                        ELTDXEMR
01200            END-EXEC.                                              ELTDXEMR
01201                                                                   ELTDXEMR
01202      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDXEMR
01203         SET  PLT-INDEX2  TO  2                                    ELTDXEMR
01204         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTDXEMR
01205                                                          ZERO AND ELTDXEMR
01206            NOT WS-ADD-A-BLANK-LINE                                ELTDXEMR
01207 *          MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTDXEMR
01208            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTDXEMR
01209            ADD +1  TO  WS-CIA                                     ELTDXEMR
01210            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTDXEMR
01211                                                   WS-SUPP-MAX-AMT ELTDXEMR
01212            MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
01213 *          ADD +1  TO  WS-CIA                                     ELTDXEMR
01214         ELSE                                                      ELTDXEMR
01215            IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =ELTDXEMR
01216                                                              ZERO ELTDXEMR
01217               MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  ELTDXEMR
01218                                               TO  WS-SUPP-MAX-AMT ELTDXEMR
01219               MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)          ELTDXEMR
01220               ADD +1  TO  WS-CIA                                  ELTDXEMR
01221               EXEC CICS  LINK  PROGRAM('ELUOUTPT')                ELTDXEMR
01222                         COMMAREA(DFHCOMMAREA)                     ELTDXEMR
01223               END-EXEC.                                           ELTDXEMR
01224                                                                   ELTDXEMR
01225 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTDXEMR
01226  2125-SPILL-OVER-COINS.                                           ELTDXEMR
01227      SET  PLT-INDEX2  TO  2.                                      ELTDXEMR
01228      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
01229         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDXEMR
01230                                                         NOT =  '0'ELTDXEMR
01231         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDXEMR
01232         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01233         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTDXEMR
01234                                           CMF-ELEMENT-SYSTEM-NAME ELTDXEMR
01235         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTDXEMR
01236                                                TO  CMF-CODE-VALUE ELTDXEMR
01237         MOVE WS-SPILLOVER-C  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
01238 *       PERFORM 2100-CODES-MANUAL-LONG.                           ELTDXEMR
01239         ADD 1 TO WS-CIA.                                          ELTDXEMR
01240                                                                   ELTDXEMR
01241  2150-SPILL-OVER-DED.                                             ELTDXEMR
01242      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTDXEMR
01243         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDXEMR
01244                                                         NOT =  '0'ELTDXEMR
01245         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTDXEMR
01246         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01247         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTDXEMR
01248         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTDXEMR
01249                                                 TO  CMF-CODE-VALUEELTDXEMR
01250         MOVE WS-SPILLOVER-D  TO  COF-DTL-LINE(WS-CIA)             ELTDXEMR
01251         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDXEMR
01252                COMMAREA(DFHCOMMAREA)                              ELTDXEMR
01253         END-EXEC.                                                 ELTDXEMR
01254                                                                   ELTDXEMR
01255                                                                   ELTDXEMR
01256  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTDXEMR
01257      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDXEMR
01258         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDXEMR
01259         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDXEMR
01260         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDXEMR
01261                                                    CMF-CODE-VALUE ELTDXEMR
01262         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTDXEMR
01263         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTDXEMR
01264         PERFORM 2100-CODES-MANUAL-LONG                            ELTDXEMR
01265         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDXEMR
01266         IF WS-CIA  >  20 OR  =  20                                ELTDXEMR
01267            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTDXEMR
01268            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDXEMR
01269                COMMAREA(DFHCOMMAREA)                              ELTDXEMR
01270            END-EXEC                                               ELTDXEMR
01271            MOVE +1  TO  WS-CIA.                                   ELTDXEMR
01272                                                                   ELTDXEMR
01273  2090-PROBLEM-WITH-INDICES.                                       ELTDXEMR
01274                                                                   ELTDXEMR
01275      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDXEMR
01276      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDXEMR
01277                                                                   ELTDXEMR
01278      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDXEMR
01279      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDXEMR
01280                                                                   ELTDXEMR
01281      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDXEMR
01282      END-EXEC.                                                    ELTDXEMR
01283                                                                   ELTDXEMR
01284 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTDXEMR
01285  2100-CODES-MANUAL-LONG.                                          ELTDXEMR
01286      INITIALIZE CMF-RETURN-CODE,                                  ELTDXEMR
01287                 TCAR-FROM-AREA.                                   ELTDXEMR
01288      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDXEMR
01289      END-EXEC.                                                    ELTDXEMR
01290      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXEMR
01291      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01292                      ADDRESS OF CMF-DESCR.                        ELTDXEMR
01293      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTDXEMR
01294         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTDXEMR
01295         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTDXEMR
01296            CMF-DESCR-LINE(1),        ' ',                         ELTDXEMR
01297            CMF-DESCR-LINE(2),        ' ',                         ELTDXEMR
01298            CMF-DESCR-LINE(3)                                      ELTDXEMR
01299            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTDXEMR
01300      ELSE                                                         ELTDXEMR
01301         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTDXEMR
01302         STRING CMF-DESCR-LINE(1),        ' ',                     ELTDXEMR
01303            CMF-DESCR-LINE(2),        ' ',                         ELTDXEMR
01304            CMF-DESCR-LINE(3)                                      ELTDXEMR
01305            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTDXEMR
01306      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXEMR
01307      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTDXEMR
01308      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTDXEMR
01309      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTDXEMR
01310      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXEMR
01311      IF WS-MOVE-LINES-TO-CIA                                      ELTDXEMR
01312         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTDXEMR
01313            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTDXEMR
01314                                             WS-TEMP-NOT-USED-CNT  ELTDXEMR
01315            PERFORM 2150-CONCATENATE-TO-TEMP-TEXT                  ELTDXEMR
01316              VARYING WS-SUB1  FROM  1  BY  1                      ELTDXEMR
01317              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTDXEMR
01318            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTDXEMR
01319            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTDXEMR
01320            ADD +1  TO  WS-CIA                                     ELTDXEMR
01321         ELSE                                                      ELTDXEMR
01322            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTDXEMR
01323            ADD +1  TO  WS-CIA.                                    ELTDXEMR
01324                                                                   ELTDXEMR
01325      IF WS-MOVE-LINES-TO-CIA                                      ELTDXEMR
01326         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTDXEMR
01327            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTDXEMR
01328            ADD +1  TO  WS-CIA                                     ELTDXEMR
01329         ELSE                                                      ELTDXEMR
01330            NEXT SENTENCE                                          ELTDXEMR
01331      ELSE                                                         ELTDXEMR
01332         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTDXEMR
01333                                                                   ELTDXEMR
01334  2100-SETUP-COMPRESS-UNSTRING.                                    ELTDXEMR
01335      INITIALIZE CMF-RETURN-CODE.                                  ELTDXEMR
01336      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDXEMR
01337      END-EXEC.                                                    ELTDXEMR
01338      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXEMR
01339      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01340                      ADDRESS OF CMF-DESCR.                        ELTDXEMR
01341      IF  WS-NOT-BASIC-NOW                                         ELTDXEMR
01342         CONTINUE                                                  ELTDXEMR
01343      ELSE                                                         ELTDXEMR
01344         MOVE 1 TO TCAR-FROM-SUB.                                  ELTDXEMR
01345      PERFORM 2101-DO-COMPRESS-UNSTRING.                           ELTDXEMR
01346                                                                   ELTDXEMR
01347  2101-DO-COMPRESS-UNSTRING.                                       ELTDXEMR
01348      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXEMR
01349        UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                  ELTDXEMR
01350           MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                  ELTDXEMR
01351              TCAR-FROM-LINE(TCAR-FROM-SUB)                        ELTDXEMR
01352           ADD 1 TO TCAR-FROM-SUB                                  ELTDXEMR
01353      END-PERFORM.                                                 ELTDXEMR
01354      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTDXEMR
01355      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXEMR
01356      PERFORM 2105-SETUP-UNSTRING.                                 ELTDXEMR
01357      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXEMR
01358      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXEMR
01359      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXEMR
01360      ADD 1 TO WS-CIA.                                             ELTDXEMR
01361      PERFORM VARYING TCAR-FROM-SUB FROM 1 BY 1                    ELTDXEMR
01362        UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED              ELTDXEMR
01363           IF TCAR-FROM-SUB = 1                                    ELTDXEMR
01364             EVALUATE TRUE                                         ELTDXEMR
01365                WHEN WS-PROCESS-POT                                ELTDXEMR
01366                  MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO             ELTDXEMR
01367                     COF-DTL-LINE(WS-CIA)                          ELTDXEMR
01368                WHEN OTHER                                         ELTDXEMR
01369                  MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO             ELTDXEMR
01370                       WS-DTL-BASIC-A                              ELTDXEMR
01371                  MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)            ELTDXEMR
01372             END-EVALUATE                                          ELTDXEMR
01373           ELSE                                                    ELTDXEMR
01374              MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                 ELTDXEMR
01375                COF-DTL-LINE(WS-CIA)                               ELTDXEMR
01376           END-IF                                                  ELTDXEMR
01377           ADD 1 TO WS-CIA                                         ELTDXEMR
01378      INITIALIZE WS-POT-SWITCH                                     ELTDXEMR
01379      END-PERFORM.                                                 ELTDXEMR
01380      MOVE SPACE  TO  COF-FUNCTION.                                ELTDXEMR
01381      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDXEMR
01382      MOVE 0 TO  COF-NBR-HDR-LINES.                                ELTDXEMR
01383      EXEC CICS                                                    ELTDXEMR
01384            LINK PROGRAM ('ELUOUTPT')                              ELTDXEMR
01385                 COMMAREA (DFHCOMMAREA)                            ELTDXEMR
01386      END-EXEC.                                                    ELTDXEMR
01387      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
01388      MOVE 1 TO WS-CIA.                                            ELTDXEMR
01389                                                                   ELTDXEMR
01390  2105-SETUP-UNSTRING.                                             ELTDXEMR
01391      MOVE 20 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTDXEMR
01392      MOVE 63 TO TCAR-OUTPUT-FIELD-1-LEN.                          ELTDXEMR
01393      MOVE 79 TO TCAR-OUTPUT-FIELD-2-LEN.                          ELTDXEMR
01394      MOVE 79 TO TCAR-OUTPUT-FIELD-3-LEN.                          ELTDXEMR
01395      MOVE 79 TO TCAR-OUTPUT-FIELD-4-LEN.                          ELTDXEMR
01396      MOVE 79 TO TCAR-OUTPUT-FIELD-5-LEN.                          ELTDXEMR
01397      MOVE 79 TO TCAR-OUTPUT-FIELD-6-LEN.                          ELTDXEMR
01398      MOVE 79 TO TCAR-OUTPUT-FIELD-7-LEN.                          ELTDXEMR
01399      MOVE 79 TO TCAR-OUTPUT-FIELD-8-LEN.                          ELTDXEMR
01400      MOVE 79 TO TCAR-OUTPUT-FIELD-9-LEN.                          ELTDXEMR
01401      MOVE 79 TO TCAR-OUTPUT-FIELD-10-LEN.                         ELTDXEMR
01402      MOVE 79 TO TCAR-OUTPUT-FIELD-11-LEN.                         ELTDXEMR
01403      MOVE 79 TO TCAR-OUTPUT-FIELD-12-LEN.                         ELTDXEMR
01404      MOVE 79 TO TCAR-OUTPUT-FIELD-13-LEN.                         ELTDXEMR
01405      MOVE 79 TO TCAR-OUTPUT-FIELD-14-LEN.                         ELTDXEMR
01406      MOVE 79 TO TCAR-OUTPUT-FIELD-15-LEN.                         ELTDXEMR
01407      MOVE 79 TO TCAR-OUTPUT-FIELD-16-LEN.                         ELTDXEMR
01408      MOVE 79 TO TCAR-OUTPUT-FIELD-17-LEN.                         ELTDXEMR
01409      MOVE 79 TO TCAR-OUTPUT-FIELD-18-LEN.                         ELTDXEMR
01410      MOVE 79 TO TCAR-OUTPUT-FIELD-19-LEN.                         ELTDXEMR
01411      MOVE 79 TO TCAR-OUTPUT-FIELD-20-LEN.                         ELTDXEMR
01412                                                                   ELTDXEMR
01413  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTDXEMR
01414      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTDXEMR
01415      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDXEMR
01416                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDXEMR
01417                                                                   ELTDXEMR
01418 /        D A Y S   O F   I N S T   T R E A T M E N T   R T N E    ELTDXEMR
01419                                                                   ELTDXEMR
01420  2200-DAYS-OF-INST-TREATMENT.                                     ELTDXEMR
01421      INITIALIZE WS-BASIC-CONTRACT                                 ELTDXEMR
01422                 WS-SUPP-CONTRACT.                                 ELTDXEMR
01423      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDXEMR
01424      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01425                          ADDRESS OF CONTRACT-RECORD.              ELTDXEMR
01426      IF CIA-RC-OK                                                 ELTDXEMR
01427         SET WS-BASIC-PRESENT TO TRUE.                             ELTDXEMR
01428                                                                   ELTDXEMR
01429      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDXEMR
01430      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01431                          ADDRESS OF CONTRACT-RECORD.              ELTDXEMR
01432      IF CIA-RC-OK                                                 ELTDXEMR
01433         SET WS-SUPP-PRESENT TO TRUE.                              ELTDXEMR
01434                                                                   ELTDXEMR
01435      IF NOT WS-BASIC-PRESENT  AND                                 ELTDXEMR
01436         NOT WS-SUPP-PRESENT                                       ELTDXEMR
01437           CONTINUE                                                ELTDXEMR
01438      ELSE                                                         ELTDXEMR
01439         PERFORM 2201-HANDLE-DAYS-OF-TREAT                         ELTDXEMR
01440      END-IF.                                                      ELTDXEMR
01441                                                                   ELTDXEMR
01442  2201-HANDLE-DAYS-OF-TREAT.                                       ELTDXEMR
01443      PERFORM 2205-INST-LIFE.                                      ELTDXEMR
01444      PERFORM 2210-INST-ACCIDENT.                                  ELTDXEMR
01445      PERFORM 2220-INST-MEDICAL.                                   ELTDXEMR
01446      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
01447      MOVE 1 TO WS-CIA.                                            ELTDXEMR
01448                                                                   ELTDXEMR
01449  2205-INST-LIFE.                                                  ELTDXEMR
01450      IF WS-BASIC-PRESENT                                          ELTDXEMR
01451          SET CIA-ELSCONIB-DDN TO TRUE                             ELTDXEMR
01452          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01453                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01454         IF GCT-DAYS-BTW-LIFE-THREAT-TRMT NOT =  ZERO              ELTDXEMR
01455            MOVE WS-LIFE-THREAT-TREAT  TO  COF-DTL-LINE(WS-CIA)    ELTDXEMR
01456            ADD +1  TO  WS-CIA                                     ELTDXEMR
01457            MOVE WS-LIFE-THREAT-TREATA  TO  COF-DTL-LINE(WS-CIA)   ELTDXEMR
01458            ADD +1  TO  WS-CIA                                     ELTDXEMR
01459            IF WS-BASIC-LOB                                        ELTDXEMR
01460               MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01461               ADD 1 TO WS-CIA                                     ELTDXEMR
01462            END-IF                                                 ELTDXEMR
01463            MOVE GCT-DAYS-BTW-LIFE-THREAT-TRMT TO                  ELTDXEMR
01464                                       WS-COMP-DAYS-LIFE           ELTDXEMR
01465            MOVE WS-COMP-W-IN-LIFE-THREAT TO                       ELTDXEMR
01466                               COF-DTL-LINE(WS-CIA)                ELTDXEMR
01467            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01468 *          MOVE 1 TO WS-CIA                                       ELTDXEMR
01469 *          PERFORM 2400-BASIC-EXPLANATION                         ELTDXEMR
01470 *          MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01471 *          PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01472         END-IF                                                    ELTDXEMR
01473      END-IF.                                                      ELTDXEMR
01474                                                                   ELTDXEMR
01475      IF WS-SUPP-PRESENT                                           ELTDXEMR
01476          SET CIA-ELSCONIS-DDN TO TRUE                             ELTDXEMR
01477          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01478                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01479         IF GCT-DAYS-BTW-LIFE-THREAT-TRMT NOT =  ZERO              ELTDXEMR
01480            IF WS-BASIC-LOB                                        ELTDXEMR
01481               MOVE WS-SUPPLEMENTAL-PHRASE                         ELTDXEMR
01482                         TO COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01483               ADD 1 TO WS-CIA                                     ELTDXEMR
01484            END-IF                                                 ELTDXEMR
01485            MOVE GCT-DAYS-BTW-LIFE-THREAT-TRMT  TO                 ELTDXEMR
01486                                             WS-COMP-DAYS-LIFE     ELTDXEMR
01487            MOVE WS-COMP-W-IN-LIFE-THREAT TO                       ELTDXEMR
01488                            COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01489            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01490            MOVE 1 TO WS-CIA                                       ELTDXEMR
01491            PERFORM 2500-SUPP-EXPLANATION                          ELTDXEMR
01492         END-IF.                                                   ELTDXEMR
01493                                                                   ELTDXEMR
01494  2210-INST-ACCIDENT.                                              ELTDXEMR
01495      MOVE WS-EAC-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).     ELTDXEMR
01496      ADD +1  TO  WS-CIA.                                          ELTDXEMR
01497      IF WS-BASIC-PRESENT                                          ELTDXEMR
01498          SET CIA-ELSCONIB-DDN TO TRUE                             ELTDXEMR
01499          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01500                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01501      END-IF                                                       ELTDXEMR
01502      IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO                ELTDXEMR
01503         IF WS-BASIC-LOB                                           ELTDXEMR
01504            MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)           ELTDXEMR
01505            ADD 1 TO WS-CIA                                        ELTDXEMR
01506         END-IF                                                    ELTDXEMR
01507         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  999                  ELTDXEMR
01508           MOVE WS-UNLIMITED TO WS-COMP-DAYS-ACCA                  ELTDXEMR
01509         ELSE                                                      ELTDXEMR
01510            MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                 ELTDXEMR
01511                                     WS-COMP-DAYS-ACC              ELTDXEMR
01512         END-IF                                                    ELTDXEMR
01513         MOVE WS-COMP-W-IN-DAYS-ACC  TO                            ELTDXEMR
01514                               COF-DTL-LINE(WS-CIA)                ELTDXEMR
01515         PERFORM 2250-CALL-ELUOUTPT                                ELTDXEMR
01516         MOVE 1 TO WS-CIA                                          ELTDXEMR
01517         PERFORM 2400-BASIC-EXPLANATION                            ELTDXEMR
01518         MOVE 1 TO WS-CIA                                          ELTDXEMR
01519         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
01520         PERFORM 2250-CALL-ELUOUTPT                                ELTDXEMR
01521         INITIALIZE WS-POT-SWITCH                                  ELTDXEMR
01522                       WS-BASIC-SWITCH                             ELTDXEMR
01523                       TCAR-FROM-AREA                              ELTDXEMR
01524      END-IF.                                                      ELTDXEMR
01525                                                                   ELTDXEMR
01526      IF WS-SUPP-PRESENT                                           ELTDXEMR
01527          MOVE 1 TO WS-CIA                                         ELTDXEMR
01528          SET CIA-ELSCONIS-DDN TO TRUE                             ELTDXEMR
01529          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01530                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01531         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTDXEMR
01532          MOVE WS-EAC-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA)  ELTDXEMR
01533          ADD +1  TO  WS-CIA                                       ELTDXEMR
01534          IF WS-BASIC-LOB                                          ELTDXEMR
01535            MOVE WS-SUPPLEMENTAL-PHRASE                            ELTDXEMR
01536                  TO COF-DTL-LINE(WS-CIA)                          ELTDXEMR
01537            ADD 1 TO WS-CIA                                        ELTDXEMR
01538            SET SUPP-ONLY TO TRUE                                  ELTDXEMR
01539          END-IF                                                   ELTDXEMR
01540          MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                   ELTDXEMR
01541                                                WS-COMP-DAYS-ACC   ELTDXEMR
01542          MOVE WS-COMP-W-IN-DAYS-ACC  TO                           ELTDXEMR
01543                                      COF-DTL-LINE(WS-CIA)         ELTDXEMR
01544          PERFORM 2250-CALL-ELUOUTPT                               ELTDXEMR
01545          MOVE 1 TO WS-CIA                                         ELTDXEMR
01546          PERFORM 2500-SUPP-EXPLANATION                            ELTDXEMR
01547          INITIALIZE WS-POT-SWITCH                                 ELTDXEMR
01548                     WS-BASIC-SWITCH                               ELTDXEMR
01549                     TCAR-FROM-AREA                                ELTDXEMR
01550         END-IF.                                                   ELTDXEMR
01551         INITIALIZE WS-SUPP-SWITCH.                                ELTDXEMR
01552                                                                   ELTDXEMR
01553  2220-INST-MEDICAL.                                               ELTDXEMR
01554      ADD +1  TO  WS-CIA.                                          ELTDXEMR
01555      MOVE WS-EMC-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).     ELTDXEMR
01556      ADD +1  TO  WS-CIA.                                          ELTDXEMR
01557      IF WS-BASIC-PRESENT                                          ELTDXEMR
01558          SET CIA-ELSCONIB-DDN TO TRUE                             ELTDXEMR
01559          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01560                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01561         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTDXEMR
01562           IF WS-BASIC-LOB                                         ELTDXEMR
01563              MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)         ELTDXEMR
01564              ADD 1 TO WS-CIA                                      ELTDXEMR
01565           END-IF                                                  ELTDXEMR
01566           MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                   ELTDXEMR
01567                                     WS-COMP-DAYS-MED              ELTDXEMR
01568           MOVE WS-COMP-W-IN-DAYS-MED  TO                          ELTDXEMR
01569                                  COF-DTL-LINE(WS-CIA)             ELTDXEMR
01570           PERFORM 2250-CALL-ELUOUTPT                              ELTDXEMR
01571           MOVE 1 TO WS-CIA                                        ELTDXEMR
01572           PERFORM 2600-BASIC-EXPLANATION                          ELTDXEMR
01573 *         MOVE SPACE TO COF-DTL-LINE(WS-CIA)                      ELTDXEMR
01574 *         PERFORM 2250-CALL-ELUOUTPT                              ELTDXEMR
01575          INITIALIZE WS-POT-SWITCH                                 ELTDXEMR
01576                     WS-BASIC-SWITCH                               ELTDXEMR
01577                     TCAR-FROM-AREA                                ELTDXEMR
01578         END-IF                                                    ELTDXEMR
01579      END-IF.                                                      ELTDXEMR
01580                                                                   ELTDXEMR
01581      IF WS-SUPP-PRESENT                                           ELTDXEMR
01582          MOVE 1 TO WS-CIA                                         ELTDXEMR
01583          SET CIA-ELSCONIS-DDN TO TRUE                             ELTDXEMR
01584          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01585                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01586         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTDXEMR
01587 *          ADD +1  TO  WS-CIA                                     ELTDXEMR
01588            MOVE WS-EMC-TREATMENT-MUST-BE                          ELTDXEMR
01589                       TO  COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01590           ADD +1  TO  WS-CIA                                      ELTDXEMR
01591           IF WS-BASIC-LOB                                         ELTDXEMR
01592 *            MOVE SPACE TO COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01593 *            ADD 1 TO WS-CIA                                      ELTDXEMR
01594              MOVE WS-SUPPLEMENTAL-PHRASE                          ELTDXEMR
01595                         TO COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01596              ADD 1 TO WS-CIA                                      ELTDXEMR
01597              SET SUPP-ONLY TO TRUE                                ELTDXEMR
01598           END-IF                                                  ELTDXEMR
01599            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTDXEMR
01600                                      WS-COMP-DAYS-MED             ELTDXEMR
01601            MOVE WS-COMP-W-IN-DAYS-MED  TO                         ELTDXEMR
01602                                        COF-DTL-LINE(WS-CIA)       ELTDXEMR
01603            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01604            MOVE 1 TO WS-CIA                                       ELTDXEMR
01605            PERFORM 2700-SUPP-EXPLANATION                          ELTDXEMR
01606           MOVE SPACE TO COF-DTL-LINE(WS-CIA)                      ELTDXEMR
01607           PERFORM 2250-CALL-ELUOUTPT                              ELTDXEMR
01608         ELSE                                                      ELTDXEMR
01609          MOVE 1 TO WS-CIA                                         ELTDXEMR
01610         END-IF                                                    ELTDXEMR
01611          INITIALIZE WS-POT-SWITCH                                 ELTDXEMR
01612                     WS-BASIC-SWITCH                               ELTDXEMR
01613                     TCAR-FROM-AREA                                ELTDXEMR
01614      END-IF.                                                      ELTDXEMR
01615      INITIALIZE WS-SUPP-SWITCH.                                   ELTDXEMR
01616                                                                   ELTDXEMR
01617  2250-CALL-ELUOUTPT.                                              ELTDXEMR
01618      MOVE WS-CIA  TO COF-NBR-DTL-LINES.                           ELTDXEMR
01619      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDXEMR
01620             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
01621      END-EXEC.                                                    ELTDXEMR
01622                                                                   ELTDXEMR
01623  2280-ADD-A-LINE-OUTPUT.                                          ELTDXEMR
01624      IF WS-EXPLANATION-PRODUCED                                   ELTDXEMR
01625         MOVE WS-BEYOND-LIMIT-EXPLAIN  TO  COF-DTL-LINE(WS-CIA)    ELTDXEMR
01626         ADD +1  TO  WS-CIA.                                       ELTDXEMR
01627      IF WS-BASIC-EXPLANATION                                      ELTDXEMR
01628         MOVE WS-BASIC-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)          ELTDXEMR
01629         ADD +1  TO  WS-CIA                                        ELTDXEMR
01630         IF WS-BASIC-EXPLAIN-CNT  >  1                             ELTDXEMR
01631            MOVE WS-BASIC-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)       ELTDXEMR
01632            ADD +1  TO  WS-CIA.                                    ELTDXEMR
01633      IF WS-SUPP-EXPLANATION                                       ELTDXEMR
01634         MOVE WS-SUPP-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)           ELTDXEMR
01635         ADD +1  TO  WS-CIA                                        ELTDXEMR
01636         IF WS-SUPP-EXPLAIN-CNT  >  1                              ELTDXEMR
01637            MOVE WS-SUPP-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)        ELTDXEMR
01638            ADD +1  TO  WS-CIA.                                    ELTDXEMR
01639      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTDXEMR
01640                                                                   ELTDXEMR
01641      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTDXEMR
01642      MOVE 1  TO  WS-CIA.                                          ELTDXEMR
01643      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDXEMR
01644             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
01645      END-EXEC.                                                    ELTDXEMR
01646                                                                   ELTDXEMR
01647 /        D A Y S   O F   P R O F   T R E A T M E N T   R T N E    ELTDXEMR
01648                                                                   ELTDXEMR
01649  2300-DAYS-OF-PROF-TREATMENT.                                     ELTDXEMR
01650      INITIALIZE WS-BASIC-CONTRACT                                 ELTDXEMR
01651                 WS-SUPP-CONTRACT.                                 ELTDXEMR
01652                                                                   ELTDXEMR
01653      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTDXEMR
01654      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01655                          ADDRESS OF CONTRACT-RECORD.              ELTDXEMR
01656      IF CIA-RC-OK                                                 ELTDXEMR
01657         SET WS-BASIC-PRESENT TO TRUE.                             ELTDXEMR
01658                                                                   ELTDXEMR
01659      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTDXEMR
01660      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01661                          ADDRESS OF CONTRACT-RECORD.              ELTDXEMR
01662      IF CIA-RC-OK                                                 ELTDXEMR
01663         SET WS-SUPP-PRESENT TO TRUE.                              ELTDXEMR
01664                                                                   ELTDXEMR
01665      IF NOT WS-BASIC-PRESENT  AND                                 ELTDXEMR
01666         NOT WS-SUPP-PRESENT                                       ELTDXEMR
01667          CONTINUE                                                 ELTDXEMR
01668      ELSE                                                         ELTDXEMR
01669         PERFORM 2305-PROF-LIFE                                    ELTDXEMR
01670         PERFORM 2310-PROF-ACCIDENT                                ELTDXEMR
01671         PERFORM 2301-HANDLE-DAYS-PROF-TREAT.                      ELTDXEMR
01672         INITIALIZE TCAR-FROM-AREA.                                ELTDXEMR
01673         MOVE 1 TO WS-CIA.                                         ELTDXEMR
01674                                                                   ELTDXEMR
01675  2305-PROF-LIFE.                                                  ELTDXEMR
01676      IF WS-BASIC-PRESENT                                          ELTDXEMR
01677          SET CIA-ELSCONPB-DDN TO TRUE                             ELTDXEMR
01678          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01679                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01680         IF GCT-DAYS-BTW-LIFE-THREAT-TRMT NOT =  ZERO              ELTDXEMR
01681            MOVE WS-LIFE-THREAT-TREAT  TO  COF-DTL-LINE(WS-CIA)    ELTDXEMR
01682            ADD +1  TO  WS-CIA                                     ELTDXEMR
01683            IF WS-BASIC-LOB                                        ELTDXEMR
01684               MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01685               ADD 1 TO WS-CIA                                     ELTDXEMR
01686            END-IF                                                 ELTDXEMR
01687            MOVE GCT-DAYS-BTW-LIFE-THREAT-TRMT TO                  ELTDXEMR
01688                                        WS-COMP-DAYS-LIFE          ELTDXEMR
01689            MOVE WS-COMP-W-IN-LIFE-THREAT TO                       ELTDXEMR
01690                               COF-DTL-LINE(WS-CIA)                ELTDXEMR
01691            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01692            MOVE 1 TO WS-CIA                                       ELTDXEMR
01693            PERFORM 2400-BASIC-EXPLANATION                         ELTDXEMR
01694            MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01695            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01696            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01697                       WS-BASIC-SWITCH                             ELTDXEMR
01698                       TCAR-FROM-AREA                              ELTDXEMR
01699         END-IF                                                    ELTDXEMR
01700      END-IF.                                                      ELTDXEMR
01701                                                                   ELTDXEMR
01702      IF WS-SUPP-PRESENT                                           ELTDXEMR
01703          SET CIA-ELSCONPS-DDN TO TRUE                             ELTDXEMR
01704          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01705                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01706         IF GCT-DAYS-BTW-LIFE-THREAT-TRMT NOT =  ZERO              ELTDXEMR
01707            IF WS-BASIC-LOB                                        ELTDXEMR
01708               MOVE WS-SUPPLEMENTAL-PHRASE                         ELTDXEMR
01709                          TO COF-DTL-LINE(WS-CIA)                  ELTDXEMR
01710               ADD 1 TO WS-CIA                                     ELTDXEMR
01711               SET WS-SUPP-PRESENT TO TRUE                         ELTDXEMR
01712            END-IF                                                 ELTDXEMR
01713            MOVE GCT-DAYS-BTW-LIFE-THREAT-TRMT  TO                 ELTDXEMR
01714                                             WS-COMP-DAYS-LIFE     ELTDXEMR
01715            MOVE WS-COMP-W-IN-LIFE-THREAT TO                       ELTDXEMR
01716                            COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01717            PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01718            MOVE 1 TO WS-CIA                                       ELTDXEMR
01719            PERFORM 2500-SUPP-EXPLANATION                          ELTDXEMR
01720            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01721                       WS-BASIC-SWITCH                             ELTDXEMR
01722                       TCAR-FROM-AREA.                             ELTDXEMR
01723            INITIALIZE WS-SUPP-SWITCH.                             ELTDXEMR
01724                                                                   ELTDXEMR
01725  2301-HANDLE-DAYS-PROF-TREAT.                                     ELTDXEMR
01726      MOVE WS-EMC-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).     ELTDXEMR
01727      ADD +1  TO  WS-CIA.                                          ELTDXEMR
01728      IF WS-BASIC-PRESENT                                          ELTDXEMR
01729          SET CIA-ELSCONPB-DDN TO TRUE                             ELTDXEMR
01730          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01731                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01732         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTDXEMR
01733            IF WS-BASIC-LOB                                        ELTDXEMR
01734               MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01735               ADD 1 TO WS-CIA                                     ELTDXEMR
01736            END-IF                                                 ELTDXEMR
01737            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTDXEMR
01738                                            WS-COMP-DAYS-MED       ELTDXEMR
01739            MOVE WS-COMP-W-IN-DAYS-MED  TO                         ELTDXEMR
01740                                      COF-DTL-LINE(WS-CIA)         ELTDXEMR
01741            MOVE 1  TO  WS-CIA                                     ELTDXEMR
01742            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01743            MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01744            PERFORM 2600-BASIC-EXPLANATION                         ELTDXEMR
01745            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01746                       WS-BASIC-SWITCH                             ELTDXEMR
01747                       TCAR-FROM-AREA                              ELTDXEMR
01748         END-IF                                                    ELTDXEMR
01749         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTDXEMR
01750 *          MOVE 1 TO WS-CIA                                       ELTDXEMR
01751            IF WS-BASIC-LOB                                        ELTDXEMR
01752               MOVE WS-BASIC-PHRASE                                ELTDXEMR
01753                          TO COF-DTL-LINE(WS-CIA)                  ELTDXEMR
01754               ADD +1 TO WS-CIA                                    ELTDXEMR
01755            END-IF                                                 ELTDXEMR
01756            MOVE TREATMENT-999-MSGA TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01757            ADD +1 TO WS-CIA                                       ELTDXEMR
01758            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01759            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01760 *          PERFORM 2250-CALL-ELUOUTPT                             ELTDXEMR
01761            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01762                       WS-BASIC-SWITCH                             ELTDXEMR
01763                       TCAR-FROM-AREA                              ELTDXEMR
01764         END-IF                                                    ELTDXEMR
01765                                                                   ELTDXEMR
01766      IF WS-SUPP-PRESENT                                           ELTDXEMR
01767          SET CIA-ELSCONPS-DDN TO TRUE                             ELTDXEMR
01768          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01769                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01770         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO              ELTDXEMR
01771              AND 999                                              ELTDXEMR
01772            MOVE WS-EMC-TREATMENT-MUST-BE                          ELTDXEMR
01773                           TO  COF-DTL-LINE(WS-CIA)                ELTDXEMR
01774            ADD +1  TO  WS-CIA                                     ELTDXEMR
01775            IF WS-BASIC-LOB                                        ELTDXEMR
01776               MOVE WS-SUPPLEMENTAL-PHRASE                         ELTDXEMR
01777                         TO COF-DTL-LINE(WS-CIA)                   ELTDXEMR
01778               ADD 1 TO WS-CIA                                     ELTDXEMR
01779               SET SUPP-ONLY TO TRUE                               ELTDXEMR
01780            END-IF                                                 ELTDXEMR
01781            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTDXEMR
01782                                               WS-COMP-DAYS-MED    ELTDXEMR
01783            MOVE WS-COMP-W-IN-DAYS-MED  TO                         ELTDXEMR
01784                                         COF-DTL-LINE(WS-CIA)      ELTDXEMR
01785            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01786            MOVE 1  TO  WS-CIA                                     ELTDXEMR
01787            MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01788            PERFORM 2700-SUPP-EXPLANATION                          ELTDXEMR
01789            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01790                       WS-BASIC-SWITCH                             ELTDXEMR
01791                       TCAR-FROM-AREA                              ELTDXEMR
01792         END-IF                                                    ELTDXEMR
01793         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTDXEMR
01794            MOVE WS-EMC-TREATMENT-MUST-BE                          ELTDXEMR
01795                           TO  COF-DTL-LINE(WS-CIA)                ELTDXEMR
01796 *          ADD +1  TO  WS-CIA                                     ELTDXEMR
01797            IF WS-BASIC-LOB                                        ELTDXEMR
01798               MOVE WS-SUPPLEMENTAL-PHRASE                         ELTDXEMR
01799                          TO COF-DTL-LINE(WS-CIA)                  ELTDXEMR
01800               ADD +1 TO WS-CIA                                    ELTDXEMR
01801            END-IF                                                 ELTDXEMR
01802            MOVE TREATMENT-999-MSGA TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01803            ADD +1 TO WS-CIA                                       ELTDXEMR
01804            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01805            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01806            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01807                       WS-BASIC-SWITCH                             ELTDXEMR
01808                       TCAR-FROM-AREA                              ELTDXEMR
01809         END-IF.                                                   ELTDXEMR
01810         INITIALIZE WS-SUPP-SWITCH.                                ELTDXEMR
01811                                                                   ELTDXEMR
01812 *      PERFORM 2380-ADD-A-LINE-OUTPUT.                            ELTDXEMR
01813                                                                   ELTDXEMR
01814  2310-PROF-ACCIDENT.                                              ELTDXEMR
01815      MOVE WS-EAC-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).     ELTDXEMR
01816      ADD +1  TO  WS-CIA.                                          ELTDXEMR
01817      IF WS-BASIC-PRESENT                                          ELTDXEMR
01818          SET CIA-ELSCONPB-DDN TO TRUE                             ELTDXEMR
01819          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01820                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01821         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTDXEMR
01822            IF WS-BASIC-LOB                                        ELTDXEMR
01823               MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
01824               ADD 1 TO WS-CIA                                     ELTDXEMR
01825            END-IF                                                 ELTDXEMR
01826            IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  999               ELTDXEMR
01827              MOVE WS-UNLIMITED TO WS-COMP-DAYS-ACCA               ELTDXEMR
01828            ELSE                                                   ELTDXEMR
01829               MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO              ELTDXEMR
01830                                        WS-COMP-DAYS-ACC           ELTDXEMR
01831            END-IF                                                 ELTDXEMR
01832            MOVE WS-COMP-W-IN-DAYS-ACC  TO                         ELTDXEMR
01833                               COF-DTL-LINE(WS-CIA)                ELTDXEMR
01834            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01835            MOVE 1  TO  WS-CIA                                     ELTDXEMR
01836            PERFORM 2400-BASIC-EXPLANATION                         ELTDXEMR
01837 *          MOVE 1  TO  WS-CIA                                     ELTDXEMR
01838 *          MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01839 *          PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01840            MOVE 1  TO  WS-CIA                                     ELTDXEMR
01841            MOVE SPACES TO COF-DTL-LINE(WS-CIA)                    ELTDXEMR
01842            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01843            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01844                       WS-BASIC-SWITCH                             ELTDXEMR
01845                       TCAR-FROM-AREA                              ELTDXEMR
01846         END-IF                                                    ELTDXEMR
01847      END-IF.                                                      ELTDXEMR
01848                                                                   ELTDXEMR
01849      IF WS-SUPP-PRESENT                                           ELTDXEMR
01850          MOVE 1 TO WS-CIA                                         ELTDXEMR
01851          SET CIA-ELSCONPS-DDN TO TRUE                             ELTDXEMR
01852          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTDXEMR
01853                          ADDRESS OF CONTRACT-RECORD               ELTDXEMR
01854         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTDXEMR
01855           MOVE WS-EAC-TREATMENT-MUST-BE                           ELTDXEMR
01856                         TO  COF-DTL-LINE(WS-CIA)                  ELTDXEMR
01857           ADD +1  TO  WS-CIA                                      ELTDXEMR
01858            IF WS-BASIC-PRESENT                                    ELTDXEMR
01859               MOVE WS-SUPPLEMENTAL-PHRASE TO COF-DTL-LINE(WS-CIA) ELTDXEMR
01860               ADD 1 TO WS-CIA                                     ELTDXEMR
01861            END-IF                                                 ELTDXEMR
01862           MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                  ELTDXEMR
01863                             WS-COMP-DAYS-ACC                      ELTDXEMR
01864           MOVE WS-COMP-W-IN-DAYS-ACC  TO                          ELTDXEMR
01865                                       COF-DTL-LINE(WS-CIA)        ELTDXEMR
01866            PERFORM 2350-CALL-ELUOUTPT                             ELTDXEMR
01867            MOVE 1  TO  WS-CIA                                     ELTDXEMR
01868            PERFORM 2500-SUPP-EXPLANATION                          ELTDXEMR
01869            INITIALIZE WS-POT-SWITCH                               ELTDXEMR
01870                       WS-BASIC-SWITCH                             ELTDXEMR
01871                       TCAR-FROM-AREA.                             ELTDXEMR
01872 *          PERFORM 2380-ADD-A-LINE-OUTPUT.                        ELTDXEMR
01873                                                                   ELTDXEMR
01874  2350-CALL-ELUOUTPT.                                              ELTDXEMR
01875      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTDXEMR
01876      MOVE 1  TO  WS-CIA.                                          ELTDXEMR
01877      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDXEMR
01878             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
01879      END-EXEC.                                                    ELTDXEMR
01880                                                                   ELTDXEMR
01881                                                                   ELTDXEMR
01882                                                                   ELTDXEMR
01883 /            B A S I C   E X P L A N A T I O N                    ELTDXEMR
01884 ***************************************************************** ELTDXEMR
01885 *            B A S I C   E X P L A N A T I O N                    ELTDXEMR
01886 *                                                                 ELTDXEMR
01887 ***************************************************************** ELTDXEMR
01888  2400-BASIC-EXPLANATION.                                          ELTDXEMR
01889      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTDXEMR
01890      IF WS-BASIC-LOB                                              ELTDXEMR
01891         SET PROCESSING-BASIC-INFO TO TRUE                         ELTDXEMR
01892      END-IF.                                                      ELTDXEMR
01893      IF GCT-CHG-EAC-PROV-IND NOT =  ZERO                          ELTDXEMR
01894         MOVE 1 TO WS-CIA                                          ELTDXEMR
01895         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
01896         ADD 1 TO WS-CIA                                           ELTDXEMR
01897         MOVE WS-BEYOND-LIMIT-EXPLAIN TO COF-DTL-LINE(WS-CIA)      ELTDXEMR
01898         MOVE 'CHG-EAC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTDXEMR
01899         MOVE GCT-CHG-EAC-PROV-IND  TO  CMF-CODE-VALUE             ELTDXEMR
01900         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01901         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01902 *       MOVE 1  TO  WS-CIA                                        ELTDXEMR
01903 *       MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
01904 *       PERFORM 2350-CALL-ELUOUTPT                                ELTDXEMR
01905      END-IF.                                                      ELTDXEMR
01906      INITIALIZE WS-POT-SWITCH                                     ELTDXEMR
01907                 WS-BASIC-SWITCH                                   ELTDXEMR
01908                 TCAR-FROM-AREA.                                   ELTDXEMR
01909                                                                   ELTDXEMR
01910 /            S U P P   E X P L A N A T I O N                      ELTDXEMR
01911 ***************************************************************** ELTDXEMR
01912 *            S U P P   E X P L A N A T I O N                      ELTDXEMR
01913 *                                                                 ELTDXEMR
01914 ***************************************************************** ELTDXEMR
01915  2500-SUPP-EXPLANATION.                                           ELTDXEMR
01916      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTDXEMR
01917      IF WS-BASIC-LOB                                              ELTDXEMR
01918         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXEMR
01919         SET SUPP-ONLY TO TRUE                                     ELTDXEMR
01920      END-IF.                                                      ELTDXEMR
01921      IF GCT-CHG-EAC-PROV-IND NOT =  ZERO                          ELTDXEMR
01922         MOVE 1 TO WS-CIA                                          ELTDXEMR
01923         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
01924         ADD 1 TO WS-CIA                                           ELTDXEMR
01925         MOVE WS-BEYOND-LIMIT-EXPLAIN TO COF-DTL-LINE(WS-CIA)      ELTDXEMR
01926         MOVE 'CHG-EAC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTDXEMR
01927         MOVE GCT-CHG-EAC-PROV-IND  TO  CMF-CODE-VALUE             ELTDXEMR
01928         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01929         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01930      END-IF.                                                      ELTDXEMR
01931                                                                   ELTDXEMR
01932 ***************************************************************** ELTDXEMR
01933 *            B A S I C   E X P L A N A T I O N                    ELTDXEMR
01934 *                                                                 ELTDXEMR
01935 ***************************************************************** ELTDXEMR
01936  2600-BASIC-EXPLANATION.                                          ELTDXEMR
01937      IF WS-BASIC-LOB                                              ELTDXEMR
01938         SET PROCESSING-BASIC-INFO TO TRUE                         ELTDXEMR
01939      END-IF.                                                      ELTDXEMR
01940      IF GCT-CHG-EMC-PROV-IND NOT =  ZERO                          ELTDXEMR
01941         MOVE 1 TO WS-CIA                                          ELTDXEMR
01942         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
01943         ADD 1 TO WS-CIA                                           ELTDXEMR
01944         MOVE WS-BEYOND-LIMIT-EXPLAIN TO COF-DTL-LINE(WS-CIA)      ELTDXEMR
01945         MOVE 'CHG-EMC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTDXEMR
01946         MOVE GCT-CHG-EMC-PROV-IND  TO  CMF-CODE-VALUE             ELTDXEMR
01947         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01948         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01949      END-IF.                                                      ELTDXEMR
01950      INITIALIZE WS-POT-SWITCH                                     ELTDXEMR
01951                 WS-BASIC-SWITCH                                   ELTDXEMR
01952                 TCAR-FROM-AREA.                                   ELTDXEMR
01953                                                                   ELTDXEMR
01954  2700-SUPP-EXPLANATION.                                           ELTDXEMR
01955      IF WS-BASIC-LOB                                              ELTDXEMR
01956         SET PROCESSING-SUPPLEMENTAL TO TRUE                       ELTDXEMR
01957         SET SUPP-ONLY TO TRUE                                     ELTDXEMR
01958      END-IF.                                                      ELTDXEMR
01959      IF GCT-CHG-EMC-PROV-IND NOT =  ZERO                          ELTDXEMR
01960         MOVE 1 TO WS-CIA                                          ELTDXEMR
01961         MOVE SPACE TO COF-DTL-LINE(WS-CIA)                        ELTDXEMR
01962         ADD 1 TO WS-CIA                                           ELTDXEMR
01963         MOVE WS-BEYOND-LIMIT-EXPLAIN TO COF-DTL-LINE(WS-CIA)      ELTDXEMR
01964         MOVE 'CHG-EMC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTDXEMR
01965         MOVE GCT-CHG-EMC-PROV-IND  TO  CMF-CODE-VALUE             ELTDXEMR
01966         PERFORM 9000-CALL-CODES-MANUAL                            ELTDXEMR
01967         PERFORM 9100-DETERMINE-OUTPUT-METHOD                      ELTDXEMR
01968      END-IF.                                                      ELTDXEMR
01969                                                                   ELTDXEMR
01970 /    C O D E S   M A N U A L   W I T H   P E R C E N T A G E      ELTDXEMR
01971  2800-CODE-MANUAL-WITH-PERCENT.                                   ELTDXEMR
01972      INITIALIZE CMF-RETURN-CODE.                                  ELTDXEMR
01973      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTDXEMR
01974      END-EXEC.                                                    ELTDXEMR
01975                                                                   ELTDXEMR
01976      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXEMR
01977      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
01978                      ADDRESS OF CMF-DESCR.                        ELTDXEMR
01979      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXEMR
01980        UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                  ELTDXEMR
01981           MOVE CMF-DESCR-LINE (CMF-DESCR-IDX) TO                  ELTDXEMR
01982              TCAR-FROM-LINE(TCAR-FROM-SUB)                        ELTDXEMR
01983           ADD 1 TO TCAR-FROM-SUB                                  ELTDXEMR
01984      END-PERFORM.                                                 ELTDXEMR
01985      MOVE WS-PERCENT-FLD TO TCAR-FROM-LINE(TCAR-FROM-SUB).        ELTDXEMR
01986      COMPUTE TCAR-AREA-LENGTH = TCAR-FROM-SUB * 79.               ELTDXEMR
01987      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXEMR
01988      PERFORM 2105-SETUP-UNSTRING.                                 ELTDXEMR
01989      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXEMR
01990      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTDXEMR
01991         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDXEMR
01992      ELSE                                                         ELTDXEMR
01993         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTDXEMR
01994      END-IF.                                                      ELTDXEMR
01995      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXEMR
01996      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXEMR
01997 *    ADD 1 TO WS-CIA.                                             ELTDXEMR
01998      PERFORM VARYING TCAR-FROM-SUB FROM 1 BY 1                    ELTDXEMR
01999        UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED              ELTDXEMR
02000        IF TCAR-FROM-SUB = 1                                       ELTDXEMR
02001           MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                    ELTDXEMR
02002              WS-DTL-BASIC-A                                       ELTDXEMR
02003           MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                   ELTDXEMR
02004        ELSE                                                       ELTDXEMR
02005           MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                    ELTDXEMR
02006             COF-DTL-LINE(WS-CIA)                                  ELTDXEMR
02007        END-IF                                                     ELTDXEMR
02008        ADD 1 TO WS-CIA                                            ELTDXEMR
02009      END-PERFORM.                                                 ELTDXEMR
02010      MOVE SPACE  TO  COF-FUNCTION.                                ELTDXEMR
02011      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDXEMR
02012      MOVE 0 TO  COF-NBR-HDR-LINES.                                ELTDXEMR
02013      EXEC CICS                                                    ELTDXEMR
02014            LINK PROGRAM ('ELUOUTPT')                              ELTDXEMR
02015                 COMMAREA (DFHCOMMAREA)                            ELTDXEMR
02016      END-EXEC.                                                    ELTDXEMR
02017      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
02018      MOVE 1 TO WS-CIA.                                            ELTDXEMR
02019                                                                   ELTDXEMR
02020                                                                   ELTDXEMR
02021                                                                   ELTDXEMR
02022  2850-CONCATENATE-TO-TEMP-TEXT.                                   ELTDXEMR
02023      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTDXEMR
02024      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTDXEMR
02025                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTDXEMR
02026                                                                   ELTDXEMR
02027  2860-MOVE-LINES-TO-CIA.                                          ELTDXEMR
02028      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTDXEMR
02029      ADD +1  TO  WS-CIA.                                          ELTDXEMR
02030                                                                   ELTDXEMR
02031  4675-PAY-CONSID-TEXT.                                            ELTDXEMR
02032      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
02033      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDXEMR
02034             WS-PAY-CONSDR-TEXT2                                   ELTDXEMR
02035                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDXEMR
02036      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDXEMR
02037      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDXEMR
02038      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDXEMR
02039                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDXEMR
02040      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDXEMR
02041      IF WS-CIA > 17                                               ELTDXEMR
02042            PERFORM 8000-OUTPUT-TEXT                               ELTDXEMR
02043            MOVE +1            TO WS-CIA.                          ELTDXEMR
02044      ADD +1                TO  WS-CIA.                            ELTDXEMR
02045      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDXEMR
02046      ADD +1                TO  WS-CIA.                            ELTDXEMR
02047      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDXEMR
02048      PERFORM 8000-OUTPUT-TEXT.                                    ELTDXEMR
02049      INITIALIZE TCAR-FROM-AREA.                                   ELTDXEMR
02050      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXEMR
02051                WS-CIA                                             ELTDXEMR
02052                TCAR-FROM-SUB.                                     ELTDXEMR
02053                                                                   ELTDXEMR
02054  4685-TRANSF-OTHER-RESPON-IND.                                    ELTDXEMR
02055      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDXEMR
02056         SET PLT-INDEX2  TO  2                                     ELTDXEMR
02057      ELSE                                                         ELTDXEMR
02058         SET PLT-INDEX2  TO  1.                                    ELTDXEMR
02059                                                                   ELTDXEMR
02060      IF PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2) = ZERO  ELTDXEMR
02061         CONTINUE                                                  ELTDXEMR
02062      ELSE                                                         ELTDXEMR
02063         SET WS-PROCESS-POT TO TRUE                                ELTDXEMR
02064         MOVE 'BP'                     TO  CMF-RECORD-PREFIX       ELTDXEMR
02065         MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME ELTDXEMR
02066         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTDXEMR
02067              TO  CMF-CODE-VALUE                                   ELTDXEMR
02068          MOVE +0                      TO  WS-TEMP-NOT-USED-CNT    ELTDXEMR
02069          MOVE SPACES                  TO  WS-TEMP-TEXT-AREA       ELTDXEMR
02070          PERFORM 9000-CALL-CODES-MANUAL                           ELTDXEMR
02071          PERFORM 9100-DETERMINE-OUTPUT-METHOD                     ELTDXEMR
02072          INITIALIZE WS-POT-SWITCH                                 ELTDXEMR
02073                     WS-BASIC-SWITCH                               ELTDXEMR
02074                     TCAR-FROM-AREA                                ELTDXEMR
02075      END-IF.                                                      ELTDXEMR
02076 /                                                                 ELTDXEMR
02077  4900-BEN-TAB-PVE.                                                ELTDXEMR
02078      MOVE +1 TO WS-CIA.                                           ELTDXEMR
02079      MOVE SPACES TO COF-DTL-LINE(WS-CIA).                         ELTDXEMR
02080      ADD +1 TO WS-CIA.                                            ELTDXEMR
02081      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDXEMR
02082      PERFORM 8000-OUTPUT-TEXT.                                    ELTDXEMR
02083                                                                   ELTDXEMR
02084  8000-OUTPUT-TEXT.                                                ELTDXEMR
02085      MOVE +0     TO COF-NBR-HDR-LINES.                            ELTDXEMR
02086      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXEMR
02087      MOVE ' '    TO COF-FUNCTION.                                 ELTDXEMR
02088         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDXEMR
02089             COMMAREA(DFHCOMMAREA)                                 ELTDXEMR
02090         END-EXEC.                                                 ELTDXEMR
02091      MOVE 1 TO WS-CIA.                                            ELTDXEMR
02092                                                                   ELTDXEMR
02093  9000-CALL-CODES-MANUAL.                                          ELTDXEMR
02094 *    INITIALIZE CMF-RETURN-CODE,                                  ELTDXEMR
02095 *               TCAR-FROM-AREA.                                   ELTDXEMR
02096      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDXEMR
02097                       COMMAREA(DFHCOMMAREA)                       ELTDXEMR
02098      END-EXEC.                                                    ELTDXEMR
02099      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDXEMR
02100      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
02101                      ADDRESS OF CMF-DESCR.                        ELTDXEMR
02102                                                                   ELTDXEMR
02103  9100-DETERMINE-OUTPUT-METHOD.                                    ELTDXEMR
02104      MOVE CMF-DESCR-LINE(1) TO WS-TEST-LINE.                      ELTDXEMR
02105      IF WS-TEST-CHAR = WS-SINGLE-QUOTE                            ELTDXEMR
02106         MOVE SPACE TO WS-TEST-CHAR                                ELTDXEMR
02107         MOVE WS-TEST-DATA TO CMF-DESCR-LINE(1)                    ELTDXEMR
02108         PERFORM 9200-DIRECT-OUTPUT                                ELTDXEMR
02109      ELSE                                                         ELTDXEMR
02110         PERFORM 9300-DISPLAY-CODE-VALUES                          ELTDXEMR
02111      END-IF.                                                      ELTDXEMR
02112                                                                   ELTDXEMR
02113  9200-DIRECT-OUTPUT.                                              ELTDXEMR
02114      MOVE 1 TO COF-NBR-DTL-LINES.                                 ELTDXEMR
02115      IF NOT WS-PROCESS-POT                                        ELTDXEMR
02116        EVALUATE TRUE                                              ELTDXEMR
02117            WHEN PROCESSING-BASIC-INFO                             ELTDXEMR
02118               MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)        ELTDXEMR
02119            WHEN PROCESSING-SUPPLEMENTAL                           ELTDXEMR
02120               MOVE WS-SUPPLEMENTAL-PHRASE                         ELTDXEMR
02121                                  TO COF-DTL-LINE(WS-CIA)          ELTDXEMR
02122            WHEN OTHER                                             ELTDXEMR
02123               CONTINUE                                            ELTDXEMR
02124        END-EVALUATE                                               ELTDXEMR
02125      END-IF.                                                      ELTDXEMR
02126      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXEMR
02127         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXEMR
02128        ADD 1 TO WS-CIA                                            ELTDXEMR
02129        MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                      ELTDXEMR
02130           COF-DTL-LINE(WS-CIA)                                    ELTDXEMR
02131      END-PERFORM.                                                 ELTDXEMR
02132      IF NOT PROCESSING-BASIC-INFO OR WS-PROCESS-POT               ELTDXEMR
02133         ADD 1 TO WS-CIA                                           ELTDXEMR
02134         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
02135      END-IF.                                                      ELTDXEMR
02136      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXEMR
02137      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXEMR
02138                            DFHCOMMAREA.                           ELTDXEMR
02139      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXEMR
02140                WS-CIA                                             ELTDXEMR
02141                TCAR-FROM-SUB.                                     ELTDXEMR
02142                                                                   ELTDXEMR
02143  9300-DISPLAY-CODE-VALUES.                                        ELTDXEMR
02144      PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1                    ELTDXEMR
02145         UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES                 ELTDXEMR
02146         MOVE CMF-DESCR-LINE(CMF-DESCR-IDX) TO                     ELTDXEMR
02147            TCAR-FROM-LINE(TCAR-FROM-SUB)                          ELTDXEMR
02148         ADD 1 TO TCAR-FROM-SUB                                    ELTDXEMR
02149      END-PERFORM.                                                 ELTDXEMR
02150      COMPUTE TCAR-FROM-LENGTH = TCAR-FROM-SUB * 79.               ELTDXEMR
02151      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXEMR
02152      MOVE 1 TO TCAR-FROM-SUB.                                     ELTDXEMR
02153      PERFORM 9610-UNSTRING-TEXT.                                  ELTDXEMR
02154      PERFORM UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED        ELTDXEMR
02155 *          ADD 1 TO WS-CIA                                        ELTDXEMR
02156            IF TCAR-FROM-SUB = 1                                   ELTDXEMR
02157               EVALUATE TRUE                                       ELTDXEMR
02158                  WHEN PROCESSING-BASIC-INFO                       ELTDXEMR
02159                     ADD 1 TO WS-CIA                               ELTDXEMR
02160                     MOVE WS-BASIC-PHRASE TO COF-DTL-LINE(WS-CIA)  ELTDXEMR
02161                  WHEN PROCESSING-SUPPLEMENTAL                     ELTDXEMR
02162                     IF SUPP-ONLY                                  ELTDXEMR
02163                       ADD 1 TO WS-CIA                             ELTDXEMR
02164                     END-IF                                        ELTDXEMR
02165                     MOVE WS-SUPPLEMENTAL-PHRASE                   ELTDXEMR
02166                            TO COF-DTL-LINE(WS-CIA)                ELTDXEMR
02167                  WHEN OTHER                                       ELTDXEMR
02168                     CONTINUE                                      ELTDXEMR
02169               END-EVALUATE                                        ELTDXEMR
02170               ADD 1 TO WS-CIA                                     ELTDXEMR
02171               MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)       ELTDXEMR
02172            ELSE                                                   ELTDXEMR
02173               ADD 1 TO WS-CIA                                     ELTDXEMR
02174               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                ELTDXEMR
02175                   COF-DTL-LINE(WS-CIA)                            ELTDXEMR
02176            END-IF                                                 ELTDXEMR
02177            ADD 1 TO TCAR-FROM-SUB                                 ELTDXEMR
02178      END-PERFORM.                                                 ELTDXEMR
02179      IF NOT PROCESSING-BASIC-INFO                                 ELTDXEMR
02180         ADD 1 TO WS-CIA                                           ELTDXEMR
02181         MOVE SPACES TO COF-DTL-LINE(WS-CIA)                       ELTDXEMR
02182      END-IF.                                                      ELTDXEMR
02183      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTDXEMR
02184      CALL 'ELUOUTPT' USING DFHEIBLK                               ELTDXEMR
02185                            DFHCOMMAREA.                           ELTDXEMR
02186      MOVE 1 TO COF-NBR-DTL-LINES                                  ELTDXEMR
02187                WS-CIA                                             ELTDXEMR
02188                TCAR-FROM-SUB.                                     ELTDXEMR
02189                                                                   ELTDXEMR
02190  9610-UNSTRING-TEXT.                                              ELTDXEMR
02191      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDXEMR
02192      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDXEMR
02193      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDXEMR
02194      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTDXEMR
02195      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTDXEMR
02196      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTDXEMR
02197      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTDXEMR
02198      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTDXEMR
02199      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTDXEMR
02200      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTDXEMR
02201      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTDXEMR
02202      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTDXEMR
02203      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTDXEMR
02204      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTDXEMR
02205      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTDXEMR
02206      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTDXEMR
02207      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTDXEMR
02208      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTDXEMR
02209      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTDXEMR
02210      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTDXEMR
02211      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTDXEMR
02212      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTDXEMR
02213                                                                   ELTDXEMR
02214 / C O M P R E S S   A N D   E X P A N D   S U B R O U T I N E S   ELTDXEMR
02215  9999-DUMMEY.                                                     ELTDXEMR
02216      COPY ELSTCOMP.                                               ELTDXEMR
02217                                                                   ELTDXEMR
02218  9999-CHECK-CONTRACT.                                             ELTDXEMR
02219      INITIALIZE WS-LOB-SWITCH.                                    ELTDXEMR
02220      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTDXEMR
02221      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
02222                      ADDRESS OF CONTRACT-RECORD.                  ELTDXEMR
02223      IF CIA-RC-PTR-NULL                                           ELTDXEMR
02224         CONTINUE                                                  ELTDXEMR
02225      ELSE                                                         ELTDXEMR
02226         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXEMR
02227         IF WS-BSC-LINE                                            ELTDXEMR
02228            SET WS-BASIC-LOB TO TRUE                               ELTDXEMR
02229         END-IF                                                    ELTDXEMR
02230      END-IF.                                                      ELTDXEMR
02231      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTDXEMR
02232      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
02233                      ADDRESS OF CONTRACT-RECORD.                  ELTDXEMR
02234      IF CIA-RC-PTR-NULL                                           ELTDXEMR
02235         CONTINUE                                                  ELTDXEMR
02236      ELSE                                                         ELTDXEMR
02237         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXEMR
02238         IF WS-BSC-LINE                                            ELTDXEMR
02239            SET WS-BASIC-LOB TO TRUE                               ELTDXEMR
02240         END-IF                                                    ELTDXEMR
02241      END-IF.                                                      ELTDXEMR
02242      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTDXEMR
02243      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
02244                      ADDRESS OF CONTRACT-RECORD.                  ELTDXEMR
02245      IF CIA-RC-PTR-NULL                                           ELTDXEMR
02246         CONTINUE                                                  ELTDXEMR
02247      ELSE                                                         ELTDXEMR
02248         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXEMR
02249         IF WS-BSC-LINE                                            ELTDXEMR
02250            SET WS-BASIC-LOB TO TRUE                               ELTDXEMR
02251         END-IF                                                    ELTDXEMR
02252      END-IF.                                                      ELTDXEMR
02253      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTDXEMR
02254      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDXEMR
02255                      ADDRESS OF CONTRACT-RECORD.                  ELTDXEMR
02256      IF CIA-RC-PTR-NULL                                           ELTDXEMR
02257         CONTINUE                                                  ELTDXEMR
02258      ELSE                                                         ELTDXEMR
02259         MOVE GCT-L-O-B TO WS-BASIC-LINE                           ELTDXEMR
02260         IF WS-BSC-LINE                                            ELTDXEMR
02261            SET WS-BASIC-LOB TO TRUE                               ELTDXEMR
02262         END-IF                                                    ELTDXEMR
02263      END-IF.                                                      ELTDXEMR
