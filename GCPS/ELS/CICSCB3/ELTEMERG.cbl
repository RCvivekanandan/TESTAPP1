00001 *      LAST MAINTENANCE TIME:  9.56.37  DATE: 04/19/86            09/03/03
00002  IDENTIFICATION DIVISION.                                         ELTEMERG
00003  PROGRAM-ID. ELTEMERG.                                               LV002
00004  AUTHOR. D SECOR  -  A C I.                                       ELTEMERG
00005  DATE-WRITTEN.   4/16/86.                                         ELTEMERG
00006  DATE-COMPILED.                                                   ELTEMERG
00007      SKIP3                                                        ELTEMERG
00008 ******************************************************************ELTEMERG
00009 *  ELTEMERG                                                       ELTEMERG
00010 *                                                                 ELTEMERG
00011 *                        PROGRAM ABSTRACT                         ELTEMERG
00012 *                                                                 ELTEMERG
00013 *   PROGRAM NAME:   E.L.S. EMERGENCY CARE BENEFITS                ELTEMERG
00014 *                                                                 ELTEMERG
00015 *   PROGRAM I.D.:   ELTEMERG                                      ELTEMERG
00016 *                                                                 ELTEMERG
00017 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTEMERG
00018 *              EMERGENCY COVERAGE GIVEN A MEMBER.                 ELTEMERG
00019 *                                                                 ELTEMERG
00020 *   OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF EMERGENCY COVERAGEELTEMERG
00021 *              AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTEMERG
00022 *              GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTEMERG
00023 *              THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTEMERG
00024 *              RANGE OF DATES.                                    ELTEMERG
00025 *                                                                 ELTEMERG
00026 *   RECORDS                                                       ELTEMERG
00027 *   ACCESSED:  GROUP SPECIFIC, CONTRACT, BENEFIT PROVISION FORMAT ELTEMERG
00028 *            B AND E.                                             ELTEMERG
00029      TITLE ' HISTORY OF EMERGENCY CARE BENEFITS'.                 ELTEMERG
00030 ***************************************************************** ELTEMERG
00031 *                                                                 ELTEMERG
00032 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTEMERG
00033 *          *-*       U P D A T E   H I S T O R Y       *-*        ELTEMERG
00034 *          *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*        ELTEMERG
00035 *                                                                 ELTEMERG
00036 **-CHG NUM-* *-DATE-* *WHO* *------DESCRIPTION------------------- ELTEMERG
00037 *                                                                 ELTEMERG
00038 *    XXXX    04/16/86  DES  ORIGINAL MODULE                       ELTEMERG
00039 *    0001    05/22/86  DES  ADDED PVE MESSAGE                     ELTEMERG
00040 *    0002    07/02/86  LET  DISCREPANCY #176.  ADDED PROFESSIONAL ELTEMERG
00041 *                           CHARGES ON A HOSPITAL CLAIM TO TOPIC  ELTEMERG
00042 *    0003    07/24/86  JTC  CHANGED THE PICTURE OF WS-BASIC-MAX-AMELTEMERG
00043 *                           FROM PIC $$$9 TO PIC ZZ9.99.          ELTEMERG
00044 *                           CHANGED THE PICTURE OF WS-SUPP-MAX-AMTELTEMERG
00045 *                           FROM PIC ZZ9. TO PIC ZZ9.99.          ELTEMERG
00046 *    0004    08/14/86  JTC  REMOVED THE SETUP OF HEADING LINE 1   ELTEMERG
00047 *                           WS-HDR-1.                             ELTEMERG
00048 *                                                                 ELTEMERG
00049 *    0005    10/08/86  JTC  VS COBOL II CONVERSION                ELTEMERG
00050 *                                                                 ELTEMERG
00051 *    0006    04/17/87  NAC  TRANSLATE 999 VALUE TO A HARD-CODED   ELTEMERG
00052 *                           MESSAGE FOR                           ELTEMERG
00053 *                                                                 ELTEMERG
00054 *    0007    10/20/87  AKK  CHANGED 'THIS GROUP OF BENEFITS ARE   ELTEMERG
00055 *                           HANDLED AS FOLLOWS' TO 'COVERED       ELTEMERG
00056 *                           SERVICES ARE'.                        ELTEMERG
00057 *    0008    11/30/87  NAC  CHANGED ELFACL TO ELGCOINS; THE       ELTEMERG
00058 *                           SUBROUTINE WAS MISNAMED.              ELTEMERG
00059 *    0009    11/14/88  NAC  INCLUDE SAC W; INCLUDE STORAGE EN-    ELTEMERG
00060 *                           ENHANCEMENTS.                         ELTEMERG
00061 *    0010    11/18/88  NAC  BY INCLUDING SAC W, ADDITIONAL LOGIC  ELTEMERG
00062 *                           MUST BE INCLUDED TO HANDLE W FORMATS; ELTEMERG
00063 *                           WAS CAUSING ASRA BECAUSE IT WAS LOOK  ELTEMERG
00064 *                           ING AT B FORMAT INSTEAD.              ELTEMERG
00065 *    0011    11/21/88  NAC  EXCLUDE SAC B.                        ELTEMERG
00066 *    0012    10/09/89  RKH  ADDED TRANSFER TO OTHER RESPON        ELTEMERG
00067 *    0013    10/16/90  GEM  ADDED BENEFIT PROVISION IDS           ELTEMERG
00068 *    0014    11/15/90  GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND     ELTEMERG
00069 *                           COMPARE TO THE LITERAL ZERO, INSTEAD  ELTEMERG
00070 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTEMERG
00071 *                                                                 ELTEMERG
00072 ***************************************************************** ELTEMERG
00073                                                                   ELTEMERG
00074      TITLE ' WORKING STORAGE SECTION EMERGENCY CARE BENEFITS'.    ELTEMERG
00075  ENVIRONMENT DIVISION.                                            ELTEMERG
00076      SKIP3                                                        ELTEMERG
00077  DATA DIVISION.                                                   ELTEMERG
00078  WORKING-STORAGE SECTION.                                         ELTEMERG
00079  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTEMERG
00080      '***ELTEMERG WS BEGINS***'.                                  ELTEMERG
00081                                                                   ELTEMERG
00082 ***** W O R K F I E L D S ,   A N D   S W I T C H E S             ELTEMERG
00083  01  WS-WORK-FIELDS.                                              ELTEMERG
00084      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTEMERG
00085      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTEMERG
00086      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTEMERG
00087      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTEMERG
00088      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTEMERG
00089      05  WS-FIRSTTIME-IND              PIC X.                     ELTEMERG
00090        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTEMERG
00091      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTEMERG
00092        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTEMERG
00093      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTEMERG
00094        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTEMERG
00095      05  WS-EXPLANATION-IND            PIC S9 COMP-3.             ELTEMERG
00096        88  WS-EXPLANATION-PRODUCED         VALUE +1 THRU +3.      ELTEMERG
00097        88  WS-BASIC-EXPLANATION            VALUE +1, +3.          ELTEMERG
00098        88  WS-BASIC-ONLY-EXPLAIN           VALUE +1.              ELTEMERG
00099        88  WS-SUPP-EXPLANATION             VALUE +2 THRU +3.      ELTEMERG
00100        88  WS-SUPP-ONLY-EXPLAIN            VALUE +2.              ELTEMERG
00101        88  WS-NO-EXPLANATION               VALUE +0.              ELTEMERG
00102      05  WS-BASIC-EXPLAIN-CNT          PIC S9 COMP-3.             ELTEMERG
00103      05  WS-SUPP-EXPLAIN-CNT           PIC S9 COMP-3.             ELTEMERG
00104      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTEMERG
00105      05  WS-PERCENT-FLD.                                          ELTEMERG
00106        10  WS-PERCENTAGE               PIC ZZ9.                   ELTEMERG
00107        10  WS-PERCENT-SIGN             PIC X.                     ELTEMERG
00108      05  WS-BASIC-CONTRACT             PIC X.                     ELTEMERG
00109        88  WS-BASIC-PRESENT                VALUE 'B'.             ELTEMERG
00110      05  WS-SUPP-CONTRACT              PIC X.                     ELTEMERG
00111        88  WS-SUPP-PRESENT                 VALUE 'S'.             ELTEMERG
00112                                                                   ELTEMERG
00113      TITLE 'BEN PROV  IDS BY TYPE -- ELTEMERG'.                   ELTEMERG
00114  01  WS-BEN-PROV-ID.                                              ELTEMERG
00115      05  TABLE-MAX                     PIC S9(4) COMP   VALUE +22.ELTEMERG
00116      05  WS-INST-ACC-CNT               PIC S9(4) COMP   VALUE +22.ELTEMERG
00117      05  WS-INST-ACC-TAB.                                         ELTEMERG
00118        10  FILLER                      PIC X(6)  VALUE 'EABA B'.  ELTEMERG
00119        10  FILLER                      PIC X(6)  VALUE 'EABC B'.  ELTEMERG
00120        10  FILLER                      PIC X(6)  VALUE 'EABD B'.  ELTEMERG
00121        10  FILLER                      PIC X(6)  VALUE 'EABP B'.  ELTEMERG
00122        10  FILLER                      PIC X(6)  VALUE 'EABR B'.  ELTEMERG
00123        10  FILLER                      PIC X(6)  VALUE 'EACF B'.  ELTEMERG
00124        10  FILLER                      PIC X(6)  VALUE 'EACX B'.  ELTEMERG
00125        10  FILLER                      PIC X(6)  VALUE 'EAER B'.  ELTEMERG
00126        10  FILLER                      PIC X(6)  VALUE 'EAHM B'.  ELTEMERG
00127        10  FILLER                      PIC X(6)  VALUE 'EAIH B'.  ELTEMERG
00128        10  FILLER                      PIC X(6)  VALUE 'EAL  B'.  ELTEMERG
00129        10  FILLER                      PIC X(6)  VALUE 'EAMP B'.  ELTEMERG
00130        10  FILLER                      PIC X(6)  VALUE 'EAMS B'.  ELTEMERG
00131        10  FILLER                      PIC X(6)  VALUE 'EAMV B'.  ELTEMERG
00132        10  FILLER                      PIC X(6)  VALUE 'EAPL B'.  ELTEMERG
00133        10  FILLER                      PIC X(6)  VALUE 'EARI B'.  ELTEMERG
00134        10  FILLER                      PIC X(6)  VALUE 'EARX B'.  ELTEMERG
00135        10  FILLER                      PIC X(6)  VALUE 'EASS B'.  ELTEMERG
00136        10  FILLER                      PIC X(6)  VALUE 'EAX  B'.  ELTEMERG
00137        10  FILLER                      PIC X(6)  VALUE 'ERSI B'.  ELTEMERG
00138        10  FILLER                      PIC X(6)  VALUE 'ERSO B'.  ELTEMERG
00139        10  FILLER                      PIC X(6)  VALUE 'SAC  W'.  ELTEMERG
00140      05  WS-INST-ACC-LIST    REDEFINES    WS-INST-ACC-TAB         ELTEMERG
00141                                        PIC X(6)  OCCURS 22 TIMES. ELTEMERG
00142                                                                   ELTEMERG
00143      05  WS-INST-MED-CNT               PIC S9(4) COMP  VALUE +20. ELTEMERG
00144      05  WS-INST-MED-TAB.                                         ELTEMERG
00145        10  FILLER                      PIC X(6)  VALUE 'EMBA B'.  ELTEMERG
00146        10  FILLER                      PIC X(6)  VALUE 'EMBC B'.  ELTEMERG
00147        10  FILLER                      PIC X(6)  VALUE 'EMBD B'.  ELTEMERG
00148        10  FILLER                      PIC X(6)  VALUE 'EMBP B'.  ELTEMERG
00149        10  FILLER                      PIC X(6)  VALUE 'EMBR B'.  ELTEMERG
00150        10  FILLER                      PIC X(6)  VALUE 'EMCX B'.  ELTEMERG
00151        10  FILLER                      PIC X(6)  VALUE 'EMER B'.  ELTEMERG
00152        10  FILLER                      PIC X(6)  VALUE 'EMHM B'.  ELTEMERG
00153        10  FILLER                      PIC X(6)  VALUE 'EMIH B'.  ELTEMERG
00154        10  FILLER                      PIC X(6)  VALUE 'EML  B'.  ELTEMERG
00155        10  FILLER                      PIC X(6)  VALUE 'EMMP B'.  ELTEMERG
00156        10  FILLER                      PIC X(6)  VALUE 'EMMS B'.  ELTEMERG
00157        10  FILLER                      PIC X(6)  VALUE 'EMMV B'.  ELTEMERG
00158        10  FILLER                      PIC X(6)  VALUE 'EMPL B'.  ELTEMERG
00159        10  FILLER                      PIC X(6)  VALUE 'EMRI B'.  ELTEMERG
00160        10  FILLER                      PIC X(6)  VALUE 'EMRX B'.  ELTEMERG
00161        10  FILLER                      PIC X(6)  VALUE 'EMSS B'.  ELTEMERG
00162        10  FILLER                      PIC X(6)  VALUE 'EMX  B'.  ELTEMERG
00163        10  FILLER                      PIC X(6)  VALUE 'ERSI B'.  ELTEMERG
00164        10  FILLER                      PIC X(6)  VALUE 'ERSO B'.  ELTEMERG
00165      05  WS-INST-MED-LIST    REDEFINES    WS-INST-MED-TAB         ELTEMERG
00166                                        PIC X(6)  OCCURS 20 TIMES. ELTEMERG
00167                                                                   ELTEMERG
00168      05  WS-PROF-ACC-CNT               PIC S9(4) COMP   VALUE +18.ELTEMERG
00169      05  WS-PROF-ACC-TAB.                                         ELTEMERG
00170        10  FILLER                      PIC X(6)  VALUE 'EABA E'.  ELTEMERG
00171        10  FILLER                      PIC X(6)  VALUE 'EABC E'.  ELTEMERG
00172        10  FILLER                      PIC X(6)  VALUE 'EABD E'.  ELTEMERG
00173        10  FILLER                      PIC X(6)  VALUE 'EABP E'.  ELTEMERG
00174        10  FILLER                      PIC X(6)  VALUE 'EABR E'.  ELTEMERG
00175        10  FILLER                      PIC X(6)  VALUE 'EAC  E'.  ELTEMERG
00176        10  FILLER                      PIC X(6)  VALUE 'EACI E'.  ELTEMERG
00177        10  FILLER                      PIC X(6)  VALUE 'EAIH E'.  ELTEMERG
00178        10  FILLER                      PIC X(6)  VALUE 'EAMP E'.  ELTEMERG
00179        10  FILLER                      PIC X(6)  VALUE 'EAMS E'.  ELTEMERG
00180        10  FILLER                      PIC X(6)  VALUE 'EAPL E'.  ELTEMERG
00181        10  FILLER                      PIC X(6)  VALUE 'EAPT E'.  ELTEMERG
00182        10  FILLER                      PIC X(6)  VALUE 'EARI E'.  ELTEMERG
00183        10  FILLER                      PIC X(6)  VALUE 'EARX E'.  ELTEMERG
00184        10  FILLER                      PIC X(6)  VALUE 'EAX  E'.  ELTEMERG
00185        10  FILLER                      PIC X(6)  VALUE 'EAL  E'.  ELTEMERG
00186        10  FILLER                      PIC X(6)  VALUE 'EACF E'.  ELTEMERG
00187        10  FILLER                      PIC X(6)  VALUE 'SAC  E'.  ELTEMERG
00188      05  WS-PROF-ACC-LIST    REDEFINES    WS-PROF-ACC-TAB         ELTEMERG
00189                                        PIC X(6)  OCCURS 18 TIMES. ELTEMERG
00190                                                                   ELTEMERG
00191      05  WS-PROF-MED-CNT               PIC S9(4) COMP   VALUE +16.ELTEMERG
00192      05  WS-PROF-MED-TAB.                                         ELTEMERG
00193        10  FILLER                      PIC X(6)  VALUE 'EMBA E'.  ELTEMERG
00194        10  FILLER                      PIC X(6)  VALUE 'EMBC E'.  ELTEMERG
00195        10  FILLER                      PIC X(6)  VALUE 'EMBD E'.  ELTEMERG
00196        10  FILLER                      PIC X(6)  VALUE 'EMBP E'.  ELTEMERG
00197        10  FILLER                      PIC X(6)  VALUE 'EMBR E'.  ELTEMERG
00198        10  FILLER                      PIC X(6)  VALUE 'EMC  E'.  ELTEMERG
00199        10  FILLER                      PIC X(6)  VALUE 'EMCI E'.  ELTEMERG
00200        10  FILLER                      PIC X(6)  VALUE 'EMIH E'.  ELTEMERG
00201        10  FILLER                      PIC X(6)  VALUE 'EML  E'.  ELTEMERG
00202        10  FILLER                      PIC X(6)  VALUE 'EMMP E'.  ELTEMERG
00203        10  FILLER                      PIC X(6)  VALUE 'EMMS E'.  ELTEMERG
00204        10  FILLER                      PIC X(6)  VALUE 'EMPL E'.  ELTEMERG
00205        10  FILLER                      PIC X(6)  VALUE 'EMPT E'.  ELTEMERG
00206        10  FILLER                      PIC X(6)  VALUE 'EMRI E'.  ELTEMERG
00207        10  FILLER                      PIC X(6)  VALUE 'EMRX E'.  ELTEMERG
00208        10  FILLER                      PIC X(6)  VALUE 'EMX  E'.  ELTEMERG
00209      05  WS-PROF-MED-LIST    REDEFINES    WS-PROF-MED-TAB         ELTEMERG
00210                                        PIC X(6)  OCCURS 16 TIMES. ELTEMERG
00211                                                                   ELTEMERG
00212      TITLE 'DISPLAY   LINES   --- ELTEMERG '.                     ELTEMERG
00213  01  WS-ELS-DISPLAY-LINES.                                        ELTEMERG
00214    05  WS-HDR-2-PROF-ACC.                                         ELTEMERG
00215      10  FILLER                    PIC X(21) VALUE SPACES.        ELTEMERG
00216      10  FILLER                    PIC X(310)                     ELTEMERG
00217          VALUE 'EMERGENCY ACCIDENT PROFESSIONAL'.                 ELTEMERG
00218      10  FILLER                    PIC X(278) VALUE LOW-VALUES.   ELTEMERG
00219                                                                   ELTEMERG
00220    05  WS-HDR-2-PROF-MED.                                         ELTEMERG
00221      10  FILLER                    PIC X(21) VALUE SPACES.        ELTEMERG
00222      10  FILLER                    PIC X(30)                      ELTEMERG
00223          VALUE 'EMERGENCY MEDICAL PROFESSIONAL'.                  ELTEMERG
00224      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTEMERG
00225                                                                   ELTEMERG
00226    05  WS-HDR-2-INST-ACC.                                         ELTEMERG
00227      10  FILLER                    PIC X(21) VALUE SPACES.        ELTEMERG
00228      10  FILLER                    PIC X(32)                      ELTEMERG
00229          VALUE 'EMERGENCY ACCIDENT INSTITUTIONAL'.                ELTEMERG
00230      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTEMERG
00231                                                                   ELTEMERG
00232    05  WS-HDR-2-INST-MED.                                         ELTEMERG
00233      10  FILLER                    PIC X(21) VALUE SPACES.        ELTEMERG
00234      10  FILLER                    PIC X(31)                      ELTEMERG
00235          VALUE 'EMERGENCY MEDICAL INSTITUTIONAL'.                 ELTEMERG
00236      10  FILLER                    PIC X(27) VALUE LOW-VALUES.    ELTEMERG
00237                                                                   ELTEMERG
00238    05  WS-HDR-2-COINS-BEN-LVL.                                    ELTEMERG
00239      10  FILLER                    PIC X(30) VALUE SPACES.        ELTEMERG
00240      10  FILLER                    PIC X(28)                      ELTEMERG
00241          VALUE 'COINSURANCE AT BENEFIT LEVEL'.                    ELTEMERG
00242      10  FILLER                    PIC X(21) VALUE LOW-VALUES.    ELTEMERG
00243                                                                   ELTEMERG
00244    05  WS-STD-ACCIDENT-SERVICES    PIC X(37)                      ELTEMERG
00245          VALUE 'STANDARD EMERGENCY ACCIDENT SERVICES '.           ELTEMERG
00246                                                                   ELTEMERG
00247    05  WS-STD-MEDICAL-SERVICES     PIC X(36)                      ELTEMERG
00248          VALUE 'STANDARD EMERGENCY MEDICAL SERVICES '.            ELTEMERG
00249                                                                   ELTEMERG
00250    05  WS-COMPARISON-INJURY.                                      ELTEMERG
00251      10  FILLER                    PIC X(48)                      ELTEMERG
00252          VALUE 'A COMPARISON OF THE INJURY TO THE EFFECTIVE DATE'.ELTEMERG
00253      10  FILLER                    PIC X(11)                      ELTEMERG
00254          VALUE ' INDICATES:'.                                     ELTEMERG
00255      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTEMERG
00256                                                                   ELTEMERG
00257    05  WS-COMPARISON-ILLNESS.                                     ELTEMERG
00258      10  FILLER                    PIC X(49)                      ELTEMERG
00259         VALUE 'A COMPARISON OF THE ILLNESS TO THE EFFECTIVE DATE'.ELTEMERG
00260      10  FILLER                    PIC X(11)                      ELTEMERG
00261          VALUE ' INDICATES:'.                                     ELTEMERG
00262      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTEMERG
00263                                                                   ELTEMERG
00264    05  WS-TREATMENT-MUST-BE.                                      ELTEMERG
00265      10  FILLER                    PIC X(28)                      ELTEMERG
00266          VALUE 'TREATMENT MUST BE RECEIVED: '.                    ELTEMERG
00267      10  FILLER                    PIC X(51) VALUE LOW-VALUES.    ELTEMERG
00268                                                                   ELTEMERG
00269    05  WS-TREATMENT-999-MSG.                                      ELTEMERG
00270      10  TREATMENT-999-MSGA        PIC X(50)   VALUE              ELTEMERG
00271      'BASED ON THE SUDDEN UNEXPECTED ONSET OF A MEDICAL'.         ELTEMERG
00272      10  TREATMENT-999-MSGB        PIC X(79)   VALUE              ELTEMERG
00273      '                CONDITION REQUIRING IMMEDIATE MEDICAL ATTENTELTEMERG
00274 -    'ION.'.                                                      ELTEMERG
00275                                                                   ELTEMERG
00276    05  WS-SERVICES-RENDERED        PIC X(25)                      ELTEMERG
00277          VALUE 'SERVICES MAY BE RENDERED '.                       ELTEMERG
00278                                                                   ELTEMERG
00279    05  WS-FOLLOWING-BEN.                                          ELTEMERG
00280      10  FILLER                    PIC X(79) VALUE                ELTEMERG
00281          'COVERED SERVICES ARE:  '.                               ELTEMERG
00282                                                                   ELTEMERG
00283    05  WS-PAYMNT-BASED.                                           ELTEMERG
00284      10  FILLER                    PIC X(20)                      ELTEMERG
00285          VALUE 'PAYMENT IS BASED ON '.                            ELTEMERG
00286      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTEMERG
00287                                                                   ELTEMERG
00288    05  WS-BASIC.                                                  ELTEMERG
00289      10  WS-BASIC-LIT              PIC X(16)                      ELTEMERG
00290         VALUE '         BASIC: '.                                 ELTEMERG
00291      10  WS-DTL-BASIC-LONG.                                       ELTEMERG
00292        15  WS-DTL-BASIC            PIC X(50) VALUE SPACES.        ELTEMERG
00293        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTEMERG
00294                                                                   ELTEMERG
00295    05  WS-BASIC-W-IN-DAYS-ACC.                                    ELTEMERG
00296      10  FILLER                    PIC X(09) VALUE SPACES.        ELTEMERG
00297      10  FILLER                    PIC X(14)                      ELTEMERG
00298         VALUE 'BASIC: WITHIN '.                                   ELTEMERG
00299      10  WS-BASIC-DAYS-ACC         PIC ZZ9.                       ELTEMERG
00300      10  FILLER                    PIC X(20)                      ELTEMERG
00301         VALUE ' DAYS OF AN ACCIDENT'.                             ELTEMERG
00302      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTEMERG
00303                                                                   ELTEMERG
00304    05  WS-BASIC-W-IN-DAYS-MED.                                    ELTEMERG
00305      10  FILLER                    PIC X(09) VALUE SPACES.        ELTEMERG
00306      10  FILLER                    PIC X(14)                      ELTEMERG
00307         VALUE 'BASIC: WITHIN '.                                   ELTEMERG
00308      10  WS-BASIC-DAYS-MED         PIC ZZ9.                       ELTEMERG
00309      10  FILLER                    PIC X(25)                      ELTEMERG
00310         VALUE ' DAYS OF ONSET OF ILLNESS'.                        ELTEMERG
00311      10  FILLER                    PIC X(28) VALUE LOW-VALUES.    ELTEMERG
00312                                                                   ELTEMERG
00313    05  WS-BEYOND-LIMIT-EXPLAIN.                                   ELTEMERG
00314      10  FILLER                    PIC X(36)                      ELTEMERG
00315         VALUE 'IF TREATMENT IS BEYOND THE DAY LIMIT'.             ELTEMERG
00316      10  FILLER                    PIC X(43)  VALUE LOW-VALUES.   ELTEMERG
00317                                                                   ELTEMERG
00318    05  WS-BASIC-EXPLAIN1           PIC X(79).                     ELTEMERG
00319    05  WS-BASIC-EXPLAIN2           PIC X(79).                     ELTEMERG
00320                                                                   ELTEMERG
00321    05  WS-BASIC-MAX.                                              ELTEMERG
00322      10  FILLER                    PIC X(09) VALUE SPACES.        ELTEMERG
00323      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTEMERG
00324      10  WS-BASIC-MAX-AMT          PIC ZZ9.99-.                   ELTEMERG
00325      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTEMERG
00326                                                                   ELTEMERG
00327    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTEMERG
00328        VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTEMERG
00329                                                                   ELTEMERG
00330    05  WS-SUPPLEMENTAL.                                           ELTEMERG
00331      10  WS-SUPP-LIT               PIC X(16)                      ELTEMERG
00332          VALUE '  SUPPLEMENTAL: '.                                ELTEMERG
00333      10  WS-DTL-SUPP-LONG.                                        ELTEMERG
00334        15  WS-DTL-SUPPLEMENTAL     PIC X(50) VALUE SPACES.        ELTEMERG
00335        15  FILLER                  PIC X(13) VALUE LOW-VALUES.    ELTEMERG
00336                                                                   ELTEMERG
00337    05  WS-SUPP-W-IN-DAYS-ACC.                                     ELTEMERG
00338      10  FILLER                    PIC X(22)                      ELTEMERG
00339         VALUE '  SUPPLEMENTAL: WITHIN'.                           ELTEMERG
00340      10  WS-SUPP-DAYS-ACC          PIC ZZ9.                       ELTEMERG
00341      10  FILLER                    PIC X(20)                      ELTEMERG
00342         VALUE ' DAYS OF AN ACCIDENT'.                             ELTEMERG
00343      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTEMERG
00344                                                                   ELTEMERG
00345    05  WS-SUPP-W-IN-DAYS-MED.                                     ELTEMERG
00346      10  FILLER                    PIC X(22)                      ELTEMERG
00347         VALUE '  SUPPLEMENTAL: WITHIN'.                           ELTEMERG
00348      10  WS-SUPP-DAYS-MED          PIC ZZ9.                       ELTEMERG
00349      10  FILLER                    PIC X(19)                      ELTEMERG
00350         VALUE ' DAYS OF AN ILLNESS'.                              ELTEMERG
00351      10  FILLER                    PIC X(35) VALUE LOW-VALUES.    ELTEMERG
00352                                                                   ELTEMERG
00353    05  WS-SUPP-EXPLAIN1            PIC X(79).                     ELTEMERG
00354    05  WS-SUPP-EXPLAIN2            PIC X(79).                     ELTEMERG
00355                                                                   ELTEMERG
00356    05  WS-SUPP-MAX.                                               ELTEMERG
00357      10  FILLER                    PIC X(16)                      ELTEMERG
00358          VALUE '  SUPPLEMENTAL: '.                                ELTEMERG
00359      10  WS-SUPP-MAX-AMT           PIC ZZ9.99-.                   ELTEMERG
00360      10  FILLER                    PIC X(60) VALUE LOW-VALUES.    ELTEMERG
00361                                                                   ELTEMERG
00362    05  WS-PAYABLE-AS.                                             ELTEMERG
00363      10  FILLER                    PIC X(40) VALUE                ELTEMERG
00364          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTEMERG
00365      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTEMERG
00366                                                                   ELTEMERG
00367    05  WS-PAY-CONSDR-TEXT1.                                       ELTEMERG
00368      10  FILLER                    PIC X(45)                      ELTEMERG
00369        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTEMERG
00370                                                                   ELTEMERG
00371    05  WS-PAY-CONSDR-TEXT2.                                       ELTEMERG
00372      10  FILLER                    PIC X(44)                      ELTEMERG
00373        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTEMERG
00374                                                                   ELTEMERG
00375    05  WS-SPILLOVER                PIC X(10)  VALUE 'SPILLOVER '. ELTEMERG
00376                                                                   ELTEMERG
00377    05  WS-MAXIMUM-AMT-PER.                                        ELTEMERG
00378      10  FILLER                    PIC X(29)                      ELTEMERG
00379        VALUE 'THE MAXIMUM AMOUNT PER VISIT '.                     ELTEMERG
00380      10  FILLER                    PIC X(50) VALUE LOW-VALUES.    ELTEMERG
00381                                                                   ELTEMERG
00382    05  WS-FOR-SUPP-ACCIDENT.                                      ELTEMERG
00383      10  FILLER                    PIC X(26)                      ELTEMERG
00384        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTEMERG
00385      10  WS-NO-DAYS                PIC ZZ9.                       ELTEMERG
00386      10  FILLER                    PIC X(09) VALUE ' DAYS OF '.   ELTEMERG
00387      10  WS-FOR-SUPP-ACC-DESC      PIC X(41).                     ELTEMERG
00388                                                                   ELTEMERG
00389    05  WS-CONTRACT-RELATED.                                       ELTEMERG
00390      10  FILLER                    PIC X(48)                      ELTEMERG
00391        VALUE 'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTEMERG
00392      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTEMERG
00393                                                                   ELTEMERG
00394    05  WS-PROF-INPT-CHRGES.                                       ELTEMERG
00395        10  FILLER                  PIC  X(61) VALUE               ELTEMERG
00396                'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CAELTEMERG
00397 -              'RE REPORT: '.                                     ELTEMERG
00398        10  FILLER                  PIC  X(18) VALUE LOW-VALUES.   ELTEMERG
00399                                                                   ELTEMERG
00400    05  WS-PROF-OUTPT-CHRGES.                                      ELTEMERG
00401        10  FILLER                  PIC  X(62) VALUE               ELTEMERG
00402                'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CELTEMERG
00403 -              'ARE REPORT: '.                                    ELTEMERG
00404        10  FILLER                  PIC  X(17) VALUE LOW-VALUES.   ELTEMERG
00405                                                                   ELTEMERG
00406    05  WS-PROVIDER-ELIGIBILITY.                                   ELTEMERG
00407      10  FILLER                    PIC X(43)                      ELTEMERG
00408        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY'.       ELTEMERG
00409      10  FILLER                    PIC X(36) VALUE LOW-VALUES.    ELTEMERG
00410                                                                   ELTEMERG
00411    05  WS-NO-TABULAR1.                                            ELTEMERG
00412      10  FILLER                    PIC X(51)  VALUE               ELTEMERG
00413         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTEMERG
00414      10  FILLER                    PIC X(22)  VALUE               ELTEMERG
00415         'GOING FROM BENEFIT ***'.                                 ELTEMERG
00416                                                                   ELTEMERG
00417    05  WS-NO-TABULAR2.                                            ELTEMERG
00418      10  FILLER                    PIC X(15)  VALUE               ELTEMERG
00419         '*** PROVISION: '.                                        ELTEMERG
00420      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTEMERG
00421      10  FILLER                    PIC X VALUE SPACE.             ELTEMERG
00422      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTEMERG
00423      10  FILLER                    PIC X(13)  VALUE               ELTEMERG
00424         ' TO TABULAR: '.                                          ELTEMERG
00425      10  WS-NO-TAB-ID              PIC X(6).                      ELTEMERG
00426      10  FILLER                    PIC X VALUE SPACE.             ELTEMERG
00427      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTEMERG
00428      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTEMERG
00429                                                                   ELTEMERG
00430    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTEMERG
00431    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTEMERG
00432      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTEMERG
00433                                                                   ELTEMERG
00434  01  WS-END                            PIC X(16)  VALUE           ELTEMERG
00435      '*** W/S ENDS ***'.                                          ELTEMERG
00436      TITLE 'LINKAGE  SECTION --- ELTEMERG'.                       ELTEMERG
00437  LINKAGE SECTION.                                                 ELTEMERG
00438  01  DFHCOMMAREA.                                                 ELTEMERG
00439      COPY ELSCOMMC.                                               ELTEMERG
00440 /  *** CIA  AREA ***                                              ELTEMERG
00441      COPY ELSCIA2C.                                               ELTEMERG
00442 /  *** IO PARM AREA ***                                           ELTEMERG
00443      COPY ELSIOPMC.                                               ELTEMERG
00444 /  *** KEY AREA ***                                               ELTEMERG
00445      COPY ELSKEYSC.                                               ELTEMERG
00446 /  *** OUTPUT TEXT AREA ***                                       ELTEMERG
00447      COPY ELSOUTPC.                                               ELTEMERG
00448 /  *** TOPIC SELECTION AREA ***                                   ELTEMERG
00449      COPY ELSSSCBC.                                               ELTEMERG
00450 /  *** CODE MANUAL INTERFACE ***                                  ELTEMERG
00451      COPY ELSCMIFC.                                               ELTEMERG
00452 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTEMERG
00453      COPY ELSCMDSC.                                               ELTEMERG
00454 /  *** BENEFIT PROVISION TABLE ***                                ELTEMERG
00455      COPY ELSPRVNC.                                               ELTEMERG
00456 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTEMERG
00457      COPY ELSTCWAC.                                               ELTEMERG
00458 / *  P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTEMERG
00459      COPY ELSPLGSW.                                               ELTEMERG
00460                                                                   ELTEMERG
00461 / *  B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTEMERG
00462      COPY ELSPLGTB.                                               ELTEMERG
00463 / **     C O N T R A C T   R E C O R D                            ELTEMERG
00464  01  CONTRACT-RECORD.                                             ELTEMERG
00465      COPY GCCONTRC.                                               ELTEMERG
00466      TITLE 'EMERGENCY CARE TOPIC'.                                ELTEMERG
00467  PROCEDURE DIVISION.                                              ELTEMERG
00468                                                                   ELTEMERG
00469 ******************************************************************ELTEMERG
00470 *                                                                 ELTEMERG
00471 *   PERFORM THE MAINLINE OPERATIONS.                              ELTEMERG
00472 *                                                                 ELTEMERG
00473 ******************************************************************ELTEMERG
00474  0000-MAINLINE SECTION.                                           ELTEMERG
00475                                                                   ELTEMERG
00476      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTEMERG
00477          EXEC CICS ABEND                                          ELTEMERG
00478                    ABCODE ('EL01')                                ELTEMERG
00479          END-EXEC                                                 ELTEMERG
00480      END-IF.                                                      ELTEMERG
00481                                                                   ELTEMERG
00482      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTEMERG
00483                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELTEMERG
00484                                                                   ELTEMERG
00485 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTEMERG
00486                                                                   ELTEMERG
00487      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTEMERG
00488      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00489                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTEMERG
00490      IF NOT CIA-RC-OK                                             ELTEMERG
00491          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00492                                                                   ELTEMERG
00493      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTEMERG
00494      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00495                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELTEMERG
00496      IF NOT CIA-RC-OK                                             ELTEMERG
00497          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00498                                                                   ELTEMERG
00499      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTEMERG
00500      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00501                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELTEMERG
00502      IF NOT CIA-RC-OK                                             ELTEMERG
00503          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00504                                                                   ELTEMERG
00505      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTEMERG
00506      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00507                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELTEMERG
00508      IF NOT CIA-RC-OK                                             ELTEMERG
00509          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00510                                                                   ELTEMERG
00511      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTEMERG
00512      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00513                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELTEMERG
00514      IF NOT CIA-RC-OK                                             ELTEMERG
00515          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00516                                                                   ELTEMERG
00517      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTEMERG
00518      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00519                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELTEMERG
00520      IF NOT CIA-RC-OK                                             ELTEMERG
00521          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTEMERG
00522                                                                   ELTEMERG
00523      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTEMERG
00524      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTEMERG
00525              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTEMERG
00526                                                                   ELTEMERG
00527      SET CIA-STG-GETMAIN TO TRUE.                                 ELTEMERG
00528      EXEC CICS LINK                                               ELTEMERG
00529                PROGRAM('ELUSTGMG')                                ELTEMERG
00530                COMMAREA(DFHCOMMAREA)                              ELTEMERG
00531      END-EXEC.                                                    ELTEMERG
00532                                                                   ELTEMERG
00533      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTEMERG
00534      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00535                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELTEMERG
00536                                                                   ELTEMERG
00537      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTEMERG
00538                                                                   ELTEMERG
00539      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTEMERG
00540         SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
00541         PERFORM 1000-INSTITUTIONAL-ACC-RTNE.                      ELTEMERG
00542                                                                   ELTEMERG
00543      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH) AND        ELTEMERG
00544         SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
00545         PERFORM 2000-PROFESSIONAL-ACC-RTNE.                       ELTEMERG
00546                                                                   ELTEMERG
00547      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTEMERG
00548         SSB-SUB-TOPIC     =  'EMC'                                ELTEMERG
00549         PERFORM 3000-INSTITUTIONAL-MED-RTNE.                      ELTEMERG
00550                                                                   ELTEMERG
00551      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH) AND        ELTEMERG
00552         SSB-SUB-TOPIC     =  'EMC'                                ELTEMERG
00553         PERFORM 4000-PROFESSIONAL-MED-RTNE.                       ELTEMERG
00554                                                                   ELTEMERG
00555      IF (NOT SSB-PROV-CLASS-PROF AND   NOT SSB-PROV-CLASS-BOTH ANDELTEMERG
00556          NOT SSB-PROV-CLASS-INST) OR                              ELTEMERG
00557         (SSB-SUB-TOPIC     NOT = 'EAC' AND  NOT = 'EMC')          ELTEMERG
00558         MOVE SPACE  TO  COF-FUNCTION                              ELTEMERG
00559         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTEMERG
00560         MOVE 2  TO  COF-NBR-DTL-LINES                             ELTEMERG
00561         MOVE '*** I N V A L I D   R E Q U E S T ***'  TO          ELTEMERG
00562                                         COF-DTL-LINE(2)           ELTEMERG
00563         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
00564               COMMAREA(DFHCOMMAREA)                               ELTEMERG
00565         END-EXEC.                                                 ELTEMERG
00566                                                                   ELTEMERG
00567      MOVE 'E'  TO  COF-FUNCTION.                                  ELTEMERG
00568      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEMERG
00569                     COF-NBR-DTL-LINES.                            ELTEMERG
00570      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
00571      END-EXEC.                                                    ELTEMERG
00572                                                                   ELTEMERG
00573                                                                   ELTEMERG
00574      EXEC CICS RETURN   END-EXEC.                                 ELTEMERG
00575      GOBACK.                                                      ELTEMERG
00576                                                                   ELTEMERG
00577  0098-SIGNAL-UNALL-AREA-ERROR.                                    ELTEMERG
00578      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTEMERG
00579      EXEC CICS ABEND                                              ELTEMERG
00580                ABCODE (CIA-ABCODE)                                ELTEMERG
00581      END-EXEC.                                                    ELTEMERG
00582                                                                   ELTEMERG
00583      TITLE 'INSTITUTIONAL  IP'.                                   ELTEMERG
00584  1000-INSTITUTIONAL-ACC-RTNE SECTION.                             ELTEMERG
00585 ***************************************************************** ELTEMERG
00586 *        I N S T I T U T I O N A L   I P   R T N E                ELTEMERG
00587 *                                                                 ELTEMERG
00588 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEMERG
00589 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEMERG
00590 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEMERG
00591 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEMERG
00592 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEMERG
00593 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEMERG
00594 *  MODULE.                                                        ELTEMERG
00595 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEMERG
00596 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEMERG
00597 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEMERG
00598 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEMERG
00599 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEMERG
00600 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEMERG
00601 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEMERG
00602 *                                                                 ELTEMERG
00603 ***************************************************************** ELTEMERG
00604      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
00605      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEMERG
00606      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEMERG
00607                     COF-NBR-DTL-LINES.                            ELTEMERG
00608      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
00609      END-EXEC.                                                    ELTEMERG
00610      MOVE SPACE  TO  COF-FUNCTION.                                ELTEMERG
00611      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEMERG
00612      MOVE WS-HDR-2-INST-ACC  TO  COF-HDR-LINE(2).                 ELTEMERG
00613                                                                   ELTEMERG
00614      MOVE WS-INST-ACC-CNT  TO  PVN-NBR-BEN-PROVN.                 ELTEMERG
00615      PERFORM 1010-MOVE-IN-INST-ACC                                ELTEMERG
00616         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEMERG
00617         UNTIL WS-SUB  >  WS-INST-ACC-CNT.                         ELTEMERG
00618                                                                   ELTEMERG
00619      GO TO 1020-CALL-COVERAGE.                                    ELTEMERG
00620  1010-MOVE-IN-INST-ACC.                                           ELTEMERG
00621      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEMERG
00622      MOVE WS-INST-ACC-LIST(WS-SUB)  TO                            ELTEMERG
00623                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEMERG
00624      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEMERG
00625                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEMERG
00626                                                                   ELTEMERG
00627  1020-CALL-COVERAGE.                                              ELTEMERG
00628      MOVE LOW-VALUES  TO  COF-DTL-LINE(1).                        ELTEMERG
00629      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
00630      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
00631      END-EXEC.                                                    ELTEMERG
00632                                                                   ELTEMERG
00633      MOVE WS-STD-ACCIDENT-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTEMERG
00634                                                                   ELTEMERG
00635      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEMERG
00636      END-EXEC.                                                    ELTEMERG
00637                                                                   ELTEMERG
00638      ADD  +1  TO   COF-NBR-DTL-LINES.                             ELTEMERG
00639      MOVE LOW-VALUES  TO  COF-DTL-LINE(COF-NBR-DTL-LINES).        ELTEMERG
00640      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
00641      END-EXEC.                                                    ELTEMERG
00642                                                                   ELTEMERG
00643      IF PVN-COVG-NONE                                             ELTEMERG
00644         GO TO 1099-EXIT.                                          ELTEMERG
00645                                                                   ELTEMERG
00646      MOVE +1  TO  WS-CIA.                                         ELTEMERG
00647                                                                   ELTEMERG
00648      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTEMERG
00649      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEMERG
00650            PSP-PROVN-PRICING-METHD,                               ELTEMERG
00651            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEMERG
00652            PSP-TRANSF-OTHER-RESP-IND,                             ELTEMERG
00653            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEMERG
00654            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEMERG
00655            PSP-SPILL-OVER-DED-APL-IND,                            ELTEMERG
00656            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEMERG
00657            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEMERG
00658            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEMERG
00659            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEMERG
00660            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEMERG
00661            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEMERG
00662            PSP-TRAUM-INJ-EFF-DT-COMP-IND,                         ELTEMERG
00663            PSB-PROF-CHRG-HSP-CLM,                                 ELTEMERG
00664            PSB-TREAT-TIME-FACTOR-IND                              ELTEMERG
00665            PSB-TREAT-TIME-FACTOR                                  ELTEMERG
00666            PSW-TREAT-TIME-FACTOR-IND                              ELTEMERG
00667            PSW-TREAT-TIME-FACTOR.                                 ELTEMERG
00668                                                                   ELTEMERG
00669      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEMERG
00670      END-EXEC.                                                    ELTEMERG
00671                                                                   ELTEMERG
00672      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEMERG
00673      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
00674                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTEMERG
00675                                                                   ELTEMERG
00676      PERFORM 1030-FIND-FIRST-NONZERO                              ELTEMERG
00677         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEMERG
00678         UNTIL WS-SUB  >  WS-INST-ACC-CNT.                         ELTEMERG
00679                                                                   ELTEMERG
00680      GO TO 1099-EXIT.                                             ELTEMERG
00681  1030-FIND-FIRST-NONZERO.                                         ELTEMERG
00682      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
00683      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEMERG
00684         NEXT SENTENCE                                             ELTEMERG
00685      ELSE                                                         ELTEMERG
00686         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTEMERG
00687                                                                   ELTEMERG
00688  1040-BUILD-SCREEN-LINES.                                         ELTEMERG
00689                                                                   ELTEMERG
00690      SET PLT-INDEX1  TO                                           ELTEMERG
00691                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTEMERG
00692      IF WS-NOT-FIRST-TIME                                         ELTEMERG
00693         MOVE 'P'  TO  COF-FUNCTION                                ELTEMERG
00694         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTEMERG
00695         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
00696             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
00697         END-EXEC                                                  ELTEMERG
00698      ELSE                                                         ELTEMERG
00699         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEMERG
00700                                                                   ELTEMERG
00701      MOVE 1  TO  WS-CIA.                                          ELTEMERG
00702      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
00703         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEMERG
00704            SET PLT-INDEX2  TO  2                                  ELTEMERG
00705         ELSE                                                      ELTEMERG
00706            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTEMERG
00707            GO TO 1099-EXIT                                        ELTEMERG
00708      ELSE                                                         ELTEMERG
00709         SET PLT-INDEX2  TO  1.                                    ELTEMERG
00710                                                                   ELTEMERG
00711 **---------------------------------------------------------------+ELTEMERG
00712 **                                                               |ELTEMERG
00713 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEMERG
00714      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEMERG
00715      ADD  +1  TO  WS-CIA.                                         ELTEMERG
00716      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEMERG
00717                                                                   ELTEMERG
00718      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTEMERG
00719         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTEMERG
00720         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-ACC-CNT.               ELTEMERG
00721      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
00722                                                                   ELTEMERG
00723      ADD  +1,  WS-CIA  GIVING   COF-NBR-DTL-LINES.                ELTEMERG
00724      MOVE +1  TO  WS-CIA                                          ELTEMERG
00725      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
00726      END-EXEC.                                                    ELTEMERG
00727 **                                                               |ELTEMERG
00728 **---------------------------------------------------------------+ELTEMERG
00729                                                                   ELTEMERG
00730 **---------------------------------------------------------------+ELTEMERG
00731 **                                                               |ELTEMERG
00732 **    I L L N E S S   /   I N J U R Y   E F F E C T I V E        |ELTEMERG
00733 **     D A T E   C O M P A R I S O N   I N D I C A T O R         |ELTEMERG
00734      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00735      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00736         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
00737                                                        NOT =  ZEROELTEMERG
00738         MOVE WS-COMPARISON-INJURY  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
00739         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00740         ADD +1  TO  WS-CIA                                        ELTEMERG
00741      ELSE                                                         ELTEMERG
00742         SET  PLT-INDEX2  TO  2                                    ELTEMERG
00743         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTEMERG
00744            PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
00745                                                       NOT =  ZERO ELTEMERG
00746            MOVE WS-COMPARISON-INJURY  TO  COF-DTL-LINE(WS-CIA)    ELTEMERG
00747            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
00748            ADD +1  TO  WS-CIA.                                    ELTEMERG
00749                                                                   ELTEMERG
00750      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00751      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00752         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
00753                                                        NOT =  ZEROELTEMERG
00754         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
00755         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
00756                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
00757         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00758                                               TO   CMF-CODE-VALUE ELTEMERG
00759         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
00760         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
00761         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
00762                                                                   ELTEMERG
00763      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
00764      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
00765         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
00766                                                        NOT =  ZEROELTEMERG
00767         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
00768         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
00769                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
00770         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00771                                               TO   CMF-CODE-VALUE ELTEMERG
00772         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
00773         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
00774         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
00775                                                                   ELTEMERG
00776      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
00777         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00778         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
00779         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00780         MOVE +1  TO  WS-CIA                                       ELTEMERG
00781         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
00782               COMMAREA(DFHCOMMAREA)                               ELTEMERG
00783         END-EXEC.                                                 ELTEMERG
00784 **                                                               |ELTEMERG
00785 **---------------------------------------------------------------+ELTEMERG
00786                                                                   ELTEMERG
00787      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
00788         SET PLT-INDEX2  TO  2                                     ELTEMERG
00789      ELSE                                                         ELTEMERG
00790         SET PLT-INDEX2  TO  1.                                    ELTEMERG
00791                                                                   ELTEMERG
00792 **---------------------------------------------------------------+ELTEMERG
00793 **                                                               |ELTEMERG
00794 **                   L I M I T   O N                             |ELTEMERG
00795 **          R E C E P T I O N   O F   T R E A T M E N T          |ELTEMERG
00796 **                         A N D                                 |ELTEMERG
00797 **         E X P L A N A T I O N   O F   P R O C E D U R E       |ELTEMERG
00798 **          W H E N   B E Y O N D   T H A T   L I M I T          |ELTEMERG
00799      PERFORM 2200-DAYS-OF-INST-TREATMENT.                         ELTEMERG
00800 **                                                               |ELTEMERG
00801 **---------------------------------------------------------------+ELTEMERG
00802                                                                   ELTEMERG
00803      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
00804         SET PLT-INDEX2  TO  2                                     ELTEMERG
00805      ELSE                                                         ELTEMERG
00806         SET PLT-INDEX2  TO  1.                                    ELTEMERG
00807                                                                   ELTEMERG
00808 **---------------------------------------------------------------+ELTEMERG
00809 **                                                               |ELTEMERG
00810 **        P L A C E   O F   T R E A T M E N T                    |ELTEMERG
00811      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
00812                                                              ZERO ELTEMERG
00813         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
00814         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEMERG
00815         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
00816                                               TO  CMF-CODE-VALUE  ELTEMERG
00817         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
00818         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTEMERG
00819         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
00820         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
00821         MOVE +1  TO  WS-CIA                                       ELTEMERG
00822         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
00823               COMMAREA(DFHCOMMAREA)                               ELTEMERG
00824         END-EXEC.                                                 ELTEMERG
00825 **                                                               |ELTEMERG
00826 **---------------------------------------------------------------+ELTEMERG
00827                                                                   ELTEMERG
00828 **---------------------------------------------------------------+ELTEMERG
00829 **                                                               |ELTEMERG
00830 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEMERG
00831 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEMERG
00832 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTEMERG
00833      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00834      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00835         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
00836                                             ZERO AND  NOT = '19'  ELTEMERG
00837         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
00838         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00839         ADD +1  TO  WS-CIA.                                       ELTEMERG
00840                                                                   ELTEMERG
00841      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
00842      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
00843         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
00844               ZERO AND  NOT = '19' AND  NOT WS-ADD-A-BLANK-LINE   ELTEMERG
00845         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
00846         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00847         ADD +1  TO  WS-CIA.                                       ELTEMERG
00848                                                                   ELTEMERG
00849      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00850      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00851         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
00852                          AND                                      ELTEMERG
00853         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
00854         SET  PLT-INDEX2  TO  2                                    ELTEMERG
00855         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
00856                                                              ZERO ELTEMERG
00857            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
00858            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
00859            ADD +1  TO  WS-CIA.                                    ELTEMERG
00860                                                                   ELTEMERG
00861      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00862      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00863         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
00864                               AND                                 ELTEMERG
00865         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTEMERG
00866         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEMERG
00867         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEMERG
00868         ADD +1  TO  WS-CIA.                                       ELTEMERG
00869                                                                   ELTEMERG
00870      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEMERG
00871         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
00872         SET  PLT-INDEX2  TO  2                                    ELTEMERG
00873         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
00874                                                              ZERO ELTEMERG
00875            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
00876            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
00877            ADD +1  TO  WS-CIA.                                    ELTEMERG
00878                                                                   ELTEMERG
00879      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
00880      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
00881         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEMERG
00882                                                            =  ZEROELTEMERG
00883            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00884                                                            =  ZEROELTEMERG
00885               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
00886            ELSE                                                   ELTEMERG
00887               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
00888          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00889                                                  TO  WS-PERCENTAGEELTEMERG
00890         ELSE                                                      ELTEMERG
00891            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
00892          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00893                                                 TO  WS-PERCENTAGE.ELTEMERG
00894      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
00895         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
00896                                                              ZERO ELTEMERG
00897         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
00898         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
00899         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
00900                                               TO   CMF-CODE-VALUE ELTEMERG
00901         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
00902         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
00903         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
00904                                                                   ELTEMERG
00905      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
00906      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
00907         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEMERG
00908                                                               ZEROELTEMERG
00909            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00910                                                             = ZEROELTEMERG
00911               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
00912            ELSE                                                   ELTEMERG
00913               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
00914          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00915                                                 TO   WS-PERCENTAGEELTEMERG
00916         ELSE                                                      ELTEMERG
00917            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
00918          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
00919                                                 TO  WS-PERCENTAGE.ELTEMERG
00920      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
00921         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
00922                                                            ZERO   ELTEMERG
00923         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
00924         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
00925         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
00926                                                    CMF-CODE-VALUE ELTEMERG
00927         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
00928         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
00929         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
00930                                                                   ELTEMERG
00931      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
00932         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
00933         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
00934         MOVE +1  TO  WS-CIA                                       ELTEMERG
00935         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
00936                COMMAREA(DFHCOMMAREA)                              ELTEMERG
00937         END-EXEC.                                                 ELTEMERG
00938 **                                                               |ELTEMERG
00939 **---------------------------------------------------------------+ELTEMERG
00940                                                                   ELTEMERG
00941 **---------------------------------------------------------------+ELTEMERG
00942 **                                                               |ELTEMERG
00943 **        P R O F E S S I O N A L   C H A R G E S   O N          |ELTEMERG
00944 **                H O S P I T A L   B I L L                      |ELTEMERG
00945                                                                   ELTEMERG
00946      SET PLT-INDEX2  TO  1.                                       ELTEMERG
00947                                                                   ELTEMERG
00948      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTEMERG
00949          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
00950              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
00951                               NOT  =  '0'  AND  NOT  =  LOW-VALUESELTEMERG
00952                  MOVE WS-PROF-INPT-CHRGES                         ELTEMERG
00953                                  TO  COF-DTL-LINE (WS-CIA)        ELTEMERG
00954                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND           ELTEMERG
00955                  ADD  +1         TO  WS-CIA.                      ELTEMERG
00956                                                                   ELTEMERG
00957      SET PLT-INDEX2  TO  2.                                       ELTEMERG
00958                                                                   ELTEMERG
00959      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTEMERG
00960          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
00961                       AND NOT WS-ADD-A-BLANK-LINE                 ELTEMERG
00962            IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)      ELTEMERG
00963                               NOT  =  '0'  AND  NOT  =  LOW-VALUESELTEMERG
00964              MOVE WS-PROF-INPT-CHRGES                             ELTEMERG
00965                             TO  COF-DTL-LINE (WS-CIA)             ELTEMERG
00966              MOVE 'Y'       TO  WS-ADD-A-BLANK-IND                ELTEMERG
00967              ADD  +1        TO  WS-CIA                            ELTEMERG
00968                                                                   ELTEMERG
00969      SET PLT-INDEX2  TO  1.                                       ELTEMERG
00970                                                                   ELTEMERG
00971      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTEMERG
00972          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
00973              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
00974                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTEMERG
00975                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTEMERG
00976                MOVE 'PROF-CHRG-HSP-CLM'                           ELTEMERG
00977                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTEMERG
00978                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTEMERG
00979                                   TO  CMF-CODE-VALUE              ELTEMERG
00980                MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA           ELTEMERG
00981                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTEMERG
00982                PERFORM 2100-CODES-MANUAL-LONG.                    ELTEMERG
00983                                                                   ELTEMERG
00984      SET PLT-INDEX2  TO  2.                                       ELTEMERG
00985                                                                   ELTEMERG
00986      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTEMERG
00987          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
00988              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
00989                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTEMERG
00990                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTEMERG
00991                MOVE 'PROF-CHRG-HSP-CLM'                           ELTEMERG
00992                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTEMERG
00993                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTEMERG
00994                                   TO  CMF-CODE-VALUE              ELTEMERG
00995                MOVE WS-SUPP-LIT   TO  WS-TEMP-TEXT-AREA           ELTEMERG
00996                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTEMERG
00997                PERFORM 2100-CODES-MANUAL-LONG.                    ELTEMERG
00998                                                                   ELTEMERG
00999      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01000          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTEMERG
01001          ADD  +1   WS-CIA  GIVING  COF-NBR-DTL-LINES              ELTEMERG
01002          MOVE +1   TO  WS-CIA                                     ELTEMERG
01003          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTEMERG
01004                         COMMAREA (DFHCOMMAREA)                    ELTEMERG
01005                         END-EXEC.                                 ELTEMERG
01006 **                                                               |ELTEMERG
01007 **---------------------------------------------------------------+ELTEMERG
01008                                                                   ELTEMERG
01009 **---------------------------------------------------------------+ELTEMERG
01010 **                                                               |ELTEMERG
01011 **       T R E A T M E N T   T I M E   F A C T O R   &   I N D   |ELTEMERG
01012      SET PLT-INDEX2  TO  1.                                       ELTEMERG
01013                                                                   ELTEMERG
01014      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZEROES    ELTEMERG
01015         IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                  ELTEMERG
01016             IF PLB-TREAT-TIME-FACTOR-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
01017                                    NOT = '0' AND                  ELTEMERG
01018                PLB-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
01019                                    NOT = ZERO                     ELTEMERG
01020                MOVE 'BPB'  TO  CMF-RECORD-PREFIX                  ELTEMERG
01021                MOVE 'TREAT-TIME-FACTOR-IND'  TO                   ELTEMERG
01022                        CMF-ELEMENT-SYSTEM-NAME                    ELTEMERG
01023                MOVE PLB-TREAT-TIME-FACTOR-IND                     ELTEMERG
01024                                          (PLT-INDEX1, PLT-INDEX2) ELTEMERG
01025                        TO CMF-CODE-VALUE                          ELTEMERG
01026                MOVE PLB-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2) ELTEMERG
01027                         TO WS-NO-DAYS                             ELTEMERG
01028                MOVE WS-FOR-SUPP-ACCIDENT  TO  WS-TEMP-TEXT-AREA   ELTEMERG
01029                MOVE +41  TO  WS-TEMP-NOT-USED-CNT                 ELTEMERG
01030                PERFORM 2100-CODES-MANUAL-LONG                     ELTEMERG
01031                ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES         ELTEMERG
01032                MOVE +1  TO  WS-CIA                                ELTEMERG
01033                EXEC CICS  LINK  PROGRAM('ELUOUTPT')               ELTEMERG
01034                                 COMMAREA(DFHCOMMAREA)             ELTEMERG
01035                END-EXEC                                           ELTEMERG
01036             END-IF                                                ELTEMERG
01037         END-IF                                                    ELTEMERG
01038         IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'W'                  ELTEMERG
01039             IF PLW-TREAT-TIME-FACTOR-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
01040                                    NOT = '0' AND                  ELTEMERG
01041                PLW-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
01042                                    NOT = ZERO                     ELTEMERG
01043                MOVE 'BPW'  TO  CMF-RECORD-PREFIX                  ELTEMERG
01044                MOVE 'TREAT-TIME-FACTOR-IND'  TO                   ELTEMERG
01045                        CMF-ELEMENT-SYSTEM-NAME                    ELTEMERG
01046                MOVE PLW-TREAT-TIME-FACTOR-IND                     ELTEMERG
01047                                          (PLT-INDEX1, PLT-INDEX2) ELTEMERG
01048                        TO CMF-CODE-VALUE                          ELTEMERG
01049                MOVE PLW-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2) ELTEMERG
01050                         TO WS-NO-DAYS                             ELTEMERG
01051                MOVE WS-FOR-SUPP-ACCIDENT  TO  WS-TEMP-TEXT-AREA   ELTEMERG
01052                MOVE +41  TO  WS-TEMP-NOT-USED-CNT                 ELTEMERG
01053                PERFORM 2100-CODES-MANUAL-LONG                     ELTEMERG
01054                ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES         ELTEMERG
01055                MOVE +1  TO  WS-CIA                                ELTEMERG
01056                EXEC CICS  LINK  PROGRAM('ELUOUTPT')               ELTEMERG
01057                                 COMMAREA(DFHCOMMAREA)             ELTEMERG
01058                END-EXEC                                           ELTEMERG
01059             END-IF                                                ELTEMERG
01060         END-IF                                                    ELTEMERG
01061      END-IF.                                                      ELTEMERG
01062 **                                                               |ELTEMERG
01063 **---------------------------------------------------------------+ELTEMERG
01064                                                                   ELTEMERG
01065 **---------------------------------------------------------------+ELTEMERG
01066 **                                                               |ELTEMERG
01067 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTEMERG
01068      SET PLT-INDEX2  TO  2.                                       ELTEMERG
01069      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTEMERG
01070         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
01071                                                       NOT =  '0'  ELTEMERG
01072         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01073         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01074         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTEMERG
01075                                           CMF-ELEMENT-SYSTEM-NAME ELTEMERG
01076         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTEMERG
01077                                           TO   CMF-CODE-VALUE     ELTEMERG
01078         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01079         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
01080         ADD +1  TO  WS-CIA.                                       ELTEMERG
01081 **                                                               |ELTEMERG
01082 **---------------------------------------------------------------+ELTEMERG
01083                                                                   ELTEMERG
01084 **---------------------------------------------------------------+ELTEMERG
01085 **                                                               |ELTEMERG
01086 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTEMERG
01087      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTEMERG
01088         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTEMERG
01089                                                       NOT =  '0'  ELTEMERG
01090         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01091         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01092         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTEMERG
01093         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTEMERG
01094                                                     CMF-CODE-VALUEELTEMERG
01095         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01096         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
01097         ADD +1  TO  WS-CIA.                                       ELTEMERG
01098 **                                                               |ELTEMERG
01099 **---------------------------------------------------------------+ELTEMERG
01100                                                                   ELTEMERG
01101      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01102         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01103         ADD 1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                 ELTEMERG
01104         MOVE +1  TO  WS-CIA                                       ELTEMERG
01105         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01106             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01107         END-EXEC.                                                 ELTEMERG
01108                                                                   ELTEMERG
01109      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01110         SET PLT-INDEX2  TO  2                                     ELTEMERG
01111      ELSE                                                         ELTEMERG
01112         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01113                                                                   ELTEMERG
01114 **---------------------------------------------------------------+ELTEMERG
01115 **                                                               |ELTEMERG
01116 **               P E R F O R M   T A B U L A R                   |ELTEMERG
01117      PERFORM 2600-GENERAL-TABULAR-RTNE.                           ELTEMERG
01118      PERFORM 4675-PAY-CONSID-TEXT.                                ELTEMERG
01119      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTEMERG
01120 **                                                               |ELTEMERG
01121 **---------------------------------------------------------------+ELTEMERG
01122                                                                   ELTEMERG
01123                                                                   ELTEMERG
01124  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTEMERG
01125                                                                   ELTEMERG
01126      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEMERG
01127         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01128         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEMERG
01129         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEMERG
01130                                                   CMF-CODE-VALUE  ELTEMERG
01131         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEMERG
01132         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01133         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
01134         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEMERG
01135         IF WS-CIA  >  20 OR  =  20                                ELTEMERG
01136            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEMERG
01137            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEMERG
01138                COMMAREA(DFHCOMMAREA)                              ELTEMERG
01139            END-EXEC                                               ELTEMERG
01140            MOVE +1  TO  WS-CIA.                                   ELTEMERG
01141                                                                   ELTEMERG
01142  1090-PROBLEM-WITH-INDICES.                                       ELTEMERG
01143                                                                   ELTEMERG
01144      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEMERG
01145      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
01146      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
01147                                                                   ELTEMERG
01148      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01149      END-EXEC.                                                    ELTEMERG
01150                                                                   ELTEMERG
01151  1099-EXIT.            EXIT.                                      ELTEMERG
01152 /        P R O F E S S I O N A L   I P   R T N E                  ELTEMERG
01153 ***************************************************************** ELTEMERG
01154 *        P R O F E S S I O N A L   I P   R T N E                  ELTEMERG
01155 *                                                                 ELTEMERG
01156 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEMERG
01157 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEMERG
01158 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEMERG
01159 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEMERG
01160 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEMERG
01161 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEMERG
01162 *  MODULE.                                                        ELTEMERG
01163 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEMERG
01164 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEMERG
01165 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEMERG
01166 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEMERG
01167 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEMERG
01168 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEMERG
01169 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEMERG
01170 *                                                                 ELTEMERG
01171 ***************************************************************** ELTEMERG
01172  2000-PROFESSIONAL-ACC-RTNE SECTION.                              ELTEMERG
01173      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
01174      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEMERG
01175      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEMERG
01176                     COF-NBR-DTL-LINES.                            ELTEMERG
01177      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01178      END-EXEC.                                                    ELTEMERG
01179      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEMERG
01180      MOVE WS-HDR-2-PROF-ACC  TO  COF-HDR-LINE(2).                 ELTEMERG
01181                                                                   ELTEMERG
01182      MOVE WS-PROF-ACC-CNT  TO  PVN-NBR-BEN-PROVN.                 ELTEMERG
01183      PERFORM 2010-MOVE-IN-PROF-ACC                                ELTEMERG
01184         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEMERG
01185         UNTIL WS-SUB  >  WS-PROF-ACC-CNT.                         ELTEMERG
01186                                                                   ELTEMERG
01187      GO TO 2020-CALL-COVERAGE.                                    ELTEMERG
01188  2010-MOVE-IN-PROF-ACC.                                           ELTEMERG
01189      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEMERG
01190      MOVE WS-PROF-ACC-LIST(WS-SUB)  TO                            ELTEMERG
01191                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEMERG
01192      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEMERG
01193                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEMERG
01194                                                                   ELTEMERG
01195  2020-CALL-COVERAGE.                                              ELTEMERG
01196      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
01197      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01198      END-EXEC.                                                    ELTEMERG
01199                                                                   ELTEMERG
01200      MOVE WS-STD-ACCIDENT-SERVICES  TO  SSB-TOPIC-PHRASE.         ELTEMERG
01201                                                                   ELTEMERG
01202      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEMERG
01203      END-EXEC.                                                    ELTEMERG
01204                                                                   ELTEMERG
01205      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTEMERG
01206      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01207      END-EXEC.                                                    ELTEMERG
01208                                                                   ELTEMERG
01209      IF PVN-COVG-NONE                                             ELTEMERG
01210         GO TO 2099-EXIT.                                          ELTEMERG
01211                                                                   ELTEMERG
01212      MOVE +1  TO  WS-CIA.                                         ELTEMERG
01213                                                                   ELTEMERG
01214      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTEMERG
01215      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEMERG
01216            PSP-PROVN-PRICING-METHD,                               ELTEMERG
01217            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEMERG
01218            PSP-TRANSF-OTHER-RESP-IND,                             ELTEMERG
01219            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEMERG
01220            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEMERG
01221            PSP-SPILL-OVER-DED-APL-IND,                            ELTEMERG
01222            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEMERG
01223            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEMERG
01224            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEMERG
01225            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEMERG
01226            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEMERG
01227            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEMERG
01228            PSP-TRAUM-INJ-EFF-DT-COMP-IND,                         ELTEMERG
01229            PSE-BEN-SCOPE-ID,                                      ELTEMERG
01230            PSE-MAX-AMT-PER-VISIT.                                 ELTEMERG
01231                                                                   ELTEMERG
01232      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEMERG
01233      END-EXEC.                                                    ELTEMERG
01234                                                                   ELTEMERG
01235      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEMERG
01236      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01237                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTEMERG
01238                                                                   ELTEMERG
01239                                                                   ELTEMERG
01240      PERFORM 2030-FIND-FIRST-NONZERO                              ELTEMERG
01241         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEMERG
01242         UNTIL WS-SUB  >  WS-PROF-ACC-CNT.                         ELTEMERG
01243                                                                   ELTEMERG
01244      GO TO 2099-EXIT.                                             ELTEMERG
01245                                                                   ELTEMERG
01246  2030-FIND-FIRST-NONZERO.                                         ELTEMERG
01247      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
01248      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEMERG
01249         NEXT SENTENCE                                             ELTEMERG
01250      ELSE                                                         ELTEMERG
01251         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTEMERG
01252                                                                   ELTEMERG
01253  2040-BUILD-SCREEN-LINES.                                         ELTEMERG
01254                                                                   ELTEMERG
01255      SET PLT-INDEX1   TO                                          ELTEMERG
01256                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTEMERG
01257      IF WS-NOT-FIRST-TIME                                         ELTEMERG
01258         MOVE 'P'  TO  COF-FUNCTION                                ELTEMERG
01259         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01260             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01261         END-EXEC                                                  ELTEMERG
01262      ELSE                                                         ELTEMERG
01263         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEMERG
01264                                                                   ELTEMERG
01265      MOVE +1  TO  WS-CIA.                                         ELTEMERG
01266      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01267         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEMERG
01268            SET PLT-INDEX2  TO  2                                  ELTEMERG
01269         ELSE                                                      ELTEMERG
01270            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTEMERG
01271            GO TO 2099-EXIT                                        ELTEMERG
01272      ELSE                                                         ELTEMERG
01273         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01274                                                                   ELTEMERG
01275 **---------------------------------------------------------------+ELTEMERG
01276 **                                                               |ELTEMERG
01277 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEMERG
01278      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEMERG
01279      ADD  +1  TO  WS-CIA.                                         ELTEMERG
01280      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEMERG
01281      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTEMERG
01282         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTEMERG
01283         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-ACC-CNT.                ELTEMERG
01284      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
01285                                                                   ELTEMERG
01286      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
01287      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01288      END-EXEC.                                                    ELTEMERG
01289      MOVE +1  TO  WS-CIA.                                         ELTEMERG
01290 **                                                               |ELTEMERG
01291 **---------------------------------------------------------------+ELTEMERG
01292                                                                   ELTEMERG
01293 **---------------------------------------------------------------+ELTEMERG
01294 **                                                               |ELTEMERG
01295 **    I L L N E S S   /   I N J U R Y   E F F E C T I V E        |ELTEMERG
01296 **     D A T E   C O M P A R I S O N   I N D I C A T O R         |ELTEMERG
01297      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01298      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01299         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
01300                                                        NOT =  ZEROELTEMERG
01301         MOVE WS-COMPARISON-INJURY  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
01302         ADD +1  TO  WS-CIA                                        ELTEMERG
01303         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01304      ELSE                                                         ELTEMERG
01305         SET  PLT-INDEX2  TO  2                                    ELTEMERG
01306         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTEMERG
01307            PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
01308                                                       NOT =  ZERO ELTEMERG
01309            MOVE WS-COMPARISON-INJURY  TO  COF-DTL-LINE(WS-CIA)    ELTEMERG
01310            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
01311            ADD +1  TO  WS-CIA.                                    ELTEMERG
01312                                                                   ELTEMERG
01313      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01314      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01315         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
01316                                                        NOT =  ZEROELTEMERG
01317         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01318         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
01319                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
01320         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01321                                               TO   CMF-CODE-VALUE ELTEMERG
01322         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01323         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01324         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01325                                                                   ELTEMERG
01326      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
01327      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01328         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
01329                                                        NOT =  ZEROELTEMERG
01330         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01331         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
01332                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
01333         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01334                                               TO   CMF-CODE-VALUE ELTEMERG
01335         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
01336         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01337         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01338                                                                   ELTEMERG
01339      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01340         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01341         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01342         MOVE +1  TO  WS-CIA                                       ELTEMERG
01343         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01344               COMMAREA(DFHCOMMAREA)                               ELTEMERG
01345         END-EXEC.                                                 ELTEMERG
01346 **                                                               |ELTEMERG
01347 **---------------------------------------------------------------+ELTEMERG
01348                                                                   ELTEMERG
01349      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01350         SET PLT-INDEX2  TO  2                                     ELTEMERG
01351      ELSE                                                         ELTEMERG
01352         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01353                                                                   ELTEMERG
01354 **---------------------------------------------------------------+ELTEMERG
01355 **                                                               |ELTEMERG
01356 **                    L I M I T   O N                            |ELTEMERG
01357 **          R E C E P T I O N   O F   T R E A T M E N T          |ELTEMERG
01358 **                         A N D                                 |ELTEMERG
01359 **         E X P L A N A T I O N   O F   P R O C E D U R E       |ELTEMERG
01360 **          W H E N   B E Y O N D   T H A T   L I M I T          |ELTEMERG
01361      PERFORM 2300-DAYS-OF-PROF-TREATMENT.                         ELTEMERG
01362 **                                                               |ELTEMERG
01363 **---------------------------------------------------------------+ELTEMERG
01364                                                                   ELTEMERG
01365      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01366         SET PLT-INDEX2  TO  2                                     ELTEMERG
01367      ELSE                                                         ELTEMERG
01368         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01369                                                                   ELTEMERG
01370 **---------------------------------------------------------------+ELTEMERG
01371 **                                                               |ELTEMERG
01372 **        P L A C E   O F   T R E A T M E N T                    |ELTEMERG
01373      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
01374                                                              ZERO ELTEMERG
01375         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01376         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEMERG
01377         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
01378                                               TO  CMF-CODE-VALUE  ELTEMERG
01379         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01380         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTEMERG
01381         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
01382         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01383         MOVE +1  TO  WS-CIA                                       ELTEMERG
01384         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01385               COMMAREA(DFHCOMMAREA)                               ELTEMERG
01386         END-EXEC.                                                 ELTEMERG
01387 **                                                               |ELTEMERG
01388 **---------------------------------------------------------------+ELTEMERG
01389                                                                   ELTEMERG
01390 **---------------------------------------------------------------+ELTEMERG
01391 **                                                               |ELTEMERG
01392 **            B E N E F I T   S C O P E   I D                    |ELTEMERG
01393      SET PLT-INDEX2  TO  1.                                       ELTEMERG
01394      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01395         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
01396                                       '0000' AND  NOT =  '00  '   ELTEMERG
01397         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01398         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
01399         ADD  +1  TO  WS-CIA.                                      ELTEMERG
01400                                                                   ELTEMERG
01401      SET PLT-INDEX2  TO  2.                                       ELTEMERG
01402      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01403         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
01404                                  '0000' AND  NOT =  '00  ' AND    ELTEMERG
01405         NOT WS-ADD-A-BLANK-LINE                                   ELTEMERG
01406         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01407         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
01408         ADD  +1  TO  WS-CIA.                                      ELTEMERG
01409                                                                   ELTEMERG
01410      SET PLT-INDEX2  TO  1.                                       ELTEMERG
01411      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01412         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
01413                                       '0000' AND  NOT =  '00  '   ELTEMERG
01414         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTEMERG
01415         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTEMERG
01416         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTEMERG
01417                                                    CMF-CODE-VALUE ELTEMERG
01418         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01419         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01420         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01421                                                                   ELTEMERG
01422      SET PLT-INDEX2  TO  2.                                       ELTEMERG
01423      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01424         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
01425                                       '0000' AND  NOT =  '00  '   ELTEMERG
01426         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTEMERG
01427         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTEMERG
01428         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTEMERG
01429                                                    CMF-CODE-VALUE ELTEMERG
01430         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
01431         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01432         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01433                                                                   ELTEMERG
01434      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01435         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01436         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01437         MOVE 1  TO  WS-CIA                                        ELTEMERG
01438         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01439             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01440         END-EXEC.                                                 ELTEMERG
01441 **                                                               |ELTEMERG
01442 **---------------------------------------------------------------+ELTEMERG
01443                                                                   ELTEMERG
01444      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01445         SET PLT-INDEX2  TO  2                                     ELTEMERG
01446      ELSE                                                         ELTEMERG
01447         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01448                                                                   ELTEMERG
01449 **---------------------------------------------------------------+ELTEMERG
01450 **                                                               |ELTEMERG
01451 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEMERG
01452 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEMERG
01453 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTEMERG
01454      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01455      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01456         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
01457                                            ZERO AND  NOT =  '19'  ELTEMERG
01458         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
01459         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01460         ADD +1  TO  WS-CIA.                                       ELTEMERG
01461                                                                   ELTEMERG
01462      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
01463      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01464         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
01465                                        ZERO AND  NOT =  '19' AND  ELTEMERG
01466         NOT WS-ADD-A-BLANK-LINE                                   ELTEMERG
01467         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
01468         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01469         ADD +1  TO  WS-CIA.                                       ELTEMERG
01470                                                                   ELTEMERG
01471      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01472      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01473         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
01474                              AND                                  ELTEMERG
01475         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
01476         SET  PLT-INDEX2  TO  2                                    ELTEMERG
01477         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
01478                                                              ZERO ELTEMERG
01479            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
01480            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
01481            ADD +1  TO  WS-CIA.                                    ELTEMERG
01482                                                                   ELTEMERG
01483      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01484      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01485         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
01486                               AND                                 ELTEMERG
01487         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTEMERG
01488         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEMERG
01489         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEMERG
01490         ADD +1  TO  WS-CIA.                                       ELTEMERG
01491                                                                   ELTEMERG
01492      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEMERG
01493         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
01494         SET  PLT-INDEX2  TO  2                                    ELTEMERG
01495         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
01496                                                              ZERO ELTEMERG
01497            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
01498            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
01499            ADD +1  TO  WS-CIA.                                    ELTEMERG
01500                                                                   ELTEMERG
01501      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
01502      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
01503         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEMERG
01504                                                            =  ZEROELTEMERG
01505            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01506                                                            =  ZEROELTEMERG
01507               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
01508            ELSE                                                   ELTEMERG
01509               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
01510          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01511                                                  TO  WS-PERCENTAGEELTEMERG
01512         ELSE                                                      ELTEMERG
01513            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
01514          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01515                                                 TO  WS-PERCENTAGE.ELTEMERG
01516      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
01517         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
01518                                                              ZERO ELTEMERG
01519         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01520         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
01521         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
01522                                                    CMF-CODE-VALUE ELTEMERG
01523         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01524         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01525         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
01526                                                                   ELTEMERG
01527      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
01528      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
01529         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEMERG
01530                                                               ZEROELTEMERG
01531            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01532                                                            =  ZEROELTEMERG
01533               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
01534            ELSE                                                   ELTEMERG
01535               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
01536          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01537                                                  TO  WS-PERCENTAGEELTEMERG
01538         ELSE                                                      ELTEMERG
01539            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
01540          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
01541                                                 TO  WS-PERCENTAGE.ELTEMERG
01542      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01543         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
01544                                                              ZERO ELTEMERG
01545         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01546         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
01547         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
01548                                                    CMF-CODE-VALUE ELTEMERG
01549         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
01550         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01551         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
01552                                                                   ELTEMERG
01553      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01554         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01555         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01556         MOVE 1  TO  WS-CIA                                        ELTEMERG
01557         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01558             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01559         END-EXEC.                                                 ELTEMERG
01560 **                                                               |ELTEMERG
01561 **---------------------------------------------------------------+ELTEMERG
01562                                                                   ELTEMERG
01563      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01564         SET PLT-INDEX2  TO  2                                     ELTEMERG
01565      ELSE                                                         ELTEMERG
01566         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01567                                                                   ELTEMERG
01568 **---------------------------------------------------------------+ELTEMERG
01569 **                                                               |ELTEMERG
01570 **       M A X I M U M   A M O U N T   P E R   V I S I T         |ELTEMERG
01571      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
01572         SET  PLT-INDEX2  TO  1                                    ELTEMERG
01573         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
01574                                                              ZERO ELTEMERG
01575            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
01576            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
01577            ADD +1  TO  WS-CIA                                     ELTEMERG
01578            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
01579                                                  WS-BASIC-MAX-AMT ELTEMERG
01580            MOVE WS-BASIC-MAX  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
01581            ADD +1  TO  WS-CIA.                                    ELTEMERG
01582                                                                   ELTEMERG
01583      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
01584         SET  PLT-INDEX2  TO  2                                    ELTEMERG
01585         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
01586                                                          ZERO AND ELTEMERG
01587            NOT WS-ADD-A-BLANK-LINE                                ELTEMERG
01588            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
01589            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
01590            ADD +1  TO  WS-CIA                                     ELTEMERG
01591            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
01592                                                   WS-SUPP-MAX-AMT ELTEMERG
01593            MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)             ELTEMERG
01594            ADD +1  TO  WS-CIA                                     ELTEMERG
01595         ELSE                                                      ELTEMERG
01596            IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =ELTEMERG
01597                                                              ZERO ELTEMERG
01598               MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
01599                                               TO  WS-SUPP-MAX-AMT ELTEMERG
01600               MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)          ELTEMERG
01601               ADD +1  TO  WS-CIA.                                 ELTEMERG
01602                                                                   ELTEMERG
01603      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01604         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01605         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01606         MOVE 1  TO  WS-CIA                                        ELTEMERG
01607         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01608             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01609         END-EXEC.                                                 ELTEMERG
01610 **                                                               |ELTEMERG
01611 **---------------------------------------------------------------+ELTEMERG
01612                                                                   ELTEMERG
01613 **---------------------------------------------------------------+ELTEMERG
01614 **                                                               |ELTEMERG
01615 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTEMERG
01616      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
01617      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01618         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
01619                                                         NOT =  '0'ELTEMERG
01620         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01621         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01622         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTEMERG
01623                                           CMF-ELEMENT-SYSTEM-NAME ELTEMERG
01624         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTEMERG
01625                                                TO  CMF-CODE-VALUE ELTEMERG
01626         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01627         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01628 **                                                               |ELTEMERG
01629 **---------------------------------------------------------------+ELTEMERG
01630                                                                   ELTEMERG
01631 **---------------------------------------------------------------+ELTEMERG
01632 **                                                               |ELTEMERG
01633 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTEMERG
01634      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
01635         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTEMERG
01636                                                         NOT =  '0'ELTEMERG
01637         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01638         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01639         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTEMERG
01640         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTEMERG
01641                                                 TO  CMF-CODE-VALUEELTEMERG
01642         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
01643         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
01644                                                                   ELTEMERG
01645      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
01646         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
01647         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
01648         MOVE 1  TO  WS-CIA                                        ELTEMERG
01649         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
01650                COMMAREA(DFHCOMMAREA)                              ELTEMERG
01651         END-EXEC.                                                 ELTEMERG
01652 **                                                               |ELTEMERG
01653 **---------------------------------------------------------------+ELTEMERG
01654                                                                   ELTEMERG
01655                                                                   ELTEMERG
01656      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
01657         SET PLT-INDEX2  TO  2                                     ELTEMERG
01658      ELSE                                                         ELTEMERG
01659         SET PLT-INDEX2  TO  1.                                    ELTEMERG
01660                                                                   ELTEMERG
01661                                                                   ELTEMERG
01662 **---------------------------------------------------------------+ELTEMERG
01663 **                                                               |ELTEMERG
01664 **         G E N E R A L   T A B U L A R   R T N E               |ELTEMERG
01665      PERFORM 2600-GENERAL-TABULAR-RTNE.                           ELTEMERG
01666      PERFORM 4675-PAY-CONSID-TEXT.                                ELTEMERG
01667      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTEMERG
01668 **                                                               |ELTEMERG
01669 **---------------------------------------------------------------+ELTEMERG
01670  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTEMERG
01671                                                                   ELTEMERG
01672      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEMERG
01673         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
01674         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEMERG
01675         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEMERG
01676                                                    CMF-CODE-VALUE ELTEMERG
01677         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEMERG
01678         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
01679         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
01680         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEMERG
01681         IF WS-CIA  >  20 OR  =  20                                ELTEMERG
01682            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEMERG
01683            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEMERG
01684                COMMAREA(DFHCOMMAREA)                              ELTEMERG
01685            END-EXEC                                               ELTEMERG
01686            MOVE +1  TO  WS-CIA.                                   ELTEMERG
01687                                                                   ELTEMERG
01688  2090-PROBLEM-WITH-INDICES.                                       ELTEMERG
01689                                                                   ELTEMERG
01690      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEMERG
01691      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
01692                                                                   ELTEMERG
01693      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTEMERG
01694      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
01695                                                                   ELTEMERG
01696      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
01697      END-EXEC.                                                    ELTEMERG
01698                                                                   ELTEMERG
01699  2099-EXIT.            EXIT.                                      ELTEMERG
01700                                                                   ELTEMERG
01701 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTEMERG
01702  2100-CODES-MANUAL-LONG SECTION.                                  ELTEMERG
01703      INITIALIZE CMF-RETURN-CODE,                                  ELTEMERG
01704                 TCAR-FROM-AREA.                                   ELTEMERG
01705                                                                   ELTEMERG
01706      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEMERG
01707      END-EXEC.                                                    ELTEMERG
01708                                                                   ELTEMERG
01709      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEMERG
01710      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01711                      ADDRESS OF CMF-DESCR.                        ELTEMERG
01712                                                                   ELTEMERG
01713      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTEMERG
01714         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTEMERG
01715         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTEMERG
01716            CMF-DESCR-LINE(1),        ' ',                         ELTEMERG
01717            CMF-DESCR-LINE(2),        ' ',                         ELTEMERG
01718            CMF-DESCR-LINE(3)                                      ELTEMERG
01719            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTEMERG
01720      ELSE                                                         ELTEMERG
01721         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTEMERG
01722         STRING CMF-DESCR-LINE(1),        ' ',                     ELTEMERG
01723            CMF-DESCR-LINE(2),        ' ',                         ELTEMERG
01724            CMF-DESCR-LINE(3)                                      ELTEMERG
01725            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTEMERG
01726      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEMERG
01727                                                                   ELTEMERG
01728      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTEMERG
01729      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTEMERG
01730      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTEMERG
01731      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEMERG
01732                                                                   ELTEMERG
01733      IF WS-MOVE-LINES-TO-CIA                                      ELTEMERG
01734         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTEMERG
01735            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTEMERG
01736                                             WS-TEMP-NOT-USED-CNT  ELTEMERG
01737            PERFORM 2150-CONCATENATE-TO-TEMP-TEXT                  ELTEMERG
01738              VARYING WS-SUB1  FROM  1  BY  1                      ELTEMERG
01739              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTEMERG
01740            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTEMERG
01741            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
01742            ADD +1  TO  WS-CIA                                     ELTEMERG
01743         ELSE                                                      ELTEMERG
01744            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTEMERG
01745            ADD +1  TO  WS-CIA.                                    ELTEMERG
01746                                                                   ELTEMERG
01747      IF WS-MOVE-LINES-TO-CIA                                      ELTEMERG
01748         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTEMERG
01749            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTEMERG
01750            ADD +1  TO  WS-CIA                                     ELTEMERG
01751         ELSE                                                      ELTEMERG
01752            NEXT SENTENCE                                          ELTEMERG
01753      ELSE                                                         ELTEMERG
01754         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTEMERG
01755                                                                   ELTEMERG
01756      GO TO 2199-EXIT.                                             ELTEMERG
01757                                                                   ELTEMERG
01758  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTEMERG
01759      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTEMERG
01760      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEMERG
01761                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEMERG
01762                                                                   ELTEMERG
01763  2199-EXIT.           EXIT.                                       ELTEMERG
01764                                                                   ELTEMERG
01765 /        D A Y S   O F   I N S T   T R E A T M E N T   R T N E    ELTEMERG
01766 ***************************************************************** ELTEMERG
01767 *        D A Y S   O F   I N S T   T R E A T M E N T   R T N E    ELTEMERG
01768 *                                                                 ELTEMERG
01769 *                                                                 ELTEMERG
01770 ***************************************************************** ELTEMERG
01771  2200-DAYS-OF-INST-TREATMENT SECTION.                             ELTEMERG
01772      INITIALIZE WS-BASIC-CONTRACT                                 ELTEMERG
01773                 WS-SUPP-CONTRACT.                                 ELTEMERG
01774                                                                   ELTEMERG
01775      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTEMERG
01776      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01777                          ADDRESS OF CONTRACT-RECORD.              ELTEMERG
01778      IF CIA-RC-OK                                                 ELTEMERG
01779         SET WS-BASIC-PRESENT TO TRUE.                             ELTEMERG
01780                                                                   ELTEMERG
01781      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTEMERG
01782      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01783                          ADDRESS OF CONTRACT-RECORD.              ELTEMERG
01784      IF CIA-RC-OK                                                 ELTEMERG
01785         SET WS-SUPP-PRESENT TO TRUE.                              ELTEMERG
01786                                                                   ELTEMERG
01787                                                                   ELTEMERG
01788      IF NOT WS-BASIC-PRESENT  AND                                 ELTEMERG
01789         NOT WS-SUPP-PRESENT                                       ELTEMERG
01790         GO TO 2299-EXIT.                                          ELTEMERG
01791                                                                   ELTEMERG
01792      IF WS-BASIC-PRESENT                                          ELTEMERG
01793          SET CIA-ELSCONIB-DDN TO TRUE                             ELTEMERG
01794          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01795                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01796         IF SSB-SUB-TOPIC     =  'EAC'                             ELTEMERG
01797            IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO              ELTEMERG
01798               IF WS-SUPP-PRESENT                                  ELTEMERG
01799                   SET CIA-ELSCONIS-DDN TO TRUE                    ELTEMERG
01800                   CALL 'ELUSETAD' USING DFHCOMMAREA               ELTEMERG
01801                                   ADDRESS OF CONTRACT-RECORD      ELTEMERG
01802                  IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO        ELTEMERG
01803                     GO TO 2299-EXIT                               ELTEMERG
01804                  ELSE                                             ELTEMERG
01805                     NEXT SENTENCE                                 ELTEMERG
01806               ELSE                                                ELTEMERG
01807                  GO TO 2299-EXIT                                  ELTEMERG
01808            ELSE                                                   ELTEMERG
01809               NEXT SENTENCE                                       ELTEMERG
01810         ELSE                                                      ELTEMERG
01811            IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO               ELTEMERG
01812               IF WS-SUPP-PRESENT                                  ELTEMERG
01813                   SET CIA-ELSCONIS-DDN TO TRUE                    ELTEMERG
01814                   CALL 'ELUSETAD' USING DFHCOMMAREA               ELTEMERG
01815                                   ADDRESS OF CONTRACT-RECORD      ELTEMERG
01816                  IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO         ELTEMERG
01817                     GO TO 2299-EXIT                               ELTEMERG
01818                  ELSE                                             ELTEMERG
01819                     NEXT SENTENCE                                 ELTEMERG
01820               ELSE                                                ELTEMERG
01821                  GO TO 2299-EXIT.                                 ELTEMERG
01822                                                                   ELTEMERG
01823      IF NOT WS-BASIC-PRESENT AND                                  ELTEMERG
01824         WS-SUPP-PRESENT                                           ELTEMERG
01825          SET CIA-ELSCONIS-DDN TO TRUE                             ELTEMERG
01826          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01827                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01828         IF SSB-SUB-TOPIC     =  'EAC'                             ELTEMERG
01829            IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO              ELTEMERG
01830               GO TO 2299-EXIT                                     ELTEMERG
01831            ELSE                                                   ELTEMERG
01832               NEXT SENTENCE                                       ELTEMERG
01833         ELSE                                                      ELTEMERG
01834            IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO               ELTEMERG
01835               GO TO 2299-EXIT.                                    ELTEMERG
01836                                                                   ELTEMERG
01837      MOVE WS-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).         ELTEMERG
01838      ADD +1  TO  WS-CIA.                                          ELTEMERG
01839      IF SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
01840         GO TO 2210-INST-ACCIDENT.                                 ELTEMERG
01841                                                                   ELTEMERG
01842      IF WS-BASIC-PRESENT                                          ELTEMERG
01843          SET CIA-ELSCONIB-DDN TO TRUE                             ELTEMERG
01844          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01845                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01846         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTEMERG
01847            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTEMERG
01848                                               WS-BASIC-DAYS-MED   ELTEMERG
01849            MOVE WS-BASIC-W-IN-DAYS-MED  TO                        ELTEMERG
01850                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
01851         END-IF                                                    ELTEMERG
01852         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTEMERG
01853            MOVE TREATMENT-999-MSGA TO WS-DTL-BASIC                ELTEMERG
01854            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTEMERG
01855            ADD +1 TO WS-CIA                                       ELTEMERG
01856            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTEMERG
01857         END-IF                                                    ELTEMERG
01858            ADD +1  TO  WS-CIA                                     ELTEMERG
01859            PERFORM 2250-CALL-ELUOUTPT                             ELTEMERG
01860            PERFORM 2400-BASIC-EXPLANATION.                        ELTEMERG
01861                                                                   ELTEMERG
01862      IF WS-SUPP-PRESENT                                           ELTEMERG
01863          SET CIA-ELSCONIS-DDN TO TRUE                             ELTEMERG
01864          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01865                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01866         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTEMERG
01867            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTEMERG
01868                                               WS-SUPP-DAYS-MED    ELTEMERG
01869            MOVE WS-SUPP-W-IN-DAYS-MED  TO                         ELTEMERG
01870                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
01871         END-IF                                                    ELTEMERG
01872         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTEMERG
01873            MOVE TREATMENT-999-MSGA TO WS-DTL-SUPPLEMENTAL         ELTEMERG
01874            MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)           ELTEMERG
01875            ADD +1 TO WS-CIA                                       ELTEMERG
01876            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTEMERG
01877         END-IF                                                    ELTEMERG
01878            ADD +1  TO  WS-CIA                                     ELTEMERG
01879            PERFORM 2250-CALL-ELUOUTPT                             ELTEMERG
01880            PERFORM 2500-SUPP-EXPLANATION.                         ELTEMERG
01881         GO TO 2280-ADD-A-LINE-OUTPUT.                             ELTEMERG
01882                                                                   ELTEMERG
01883  2210-INST-ACCIDENT.                                              ELTEMERG
01884                                                                   ELTEMERG
01885      IF WS-BASIC-PRESENT                                          ELTEMERG
01886          SET CIA-ELSCONIB-DDN TO TRUE                             ELTEMERG
01887          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01888                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01889         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTEMERG
01890            MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                 ELTEMERG
01891                                                WS-BASIC-DAYS-ACC  ELTEMERG
01892            MOVE WS-BASIC-W-IN-DAYS-ACC  TO                        ELTEMERG
01893                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
01894            ADD +1  TO  WS-CIA                                     ELTEMERG
01895            PERFORM 2250-CALL-ELUOUTPT                             ELTEMERG
01896            PERFORM 2400-BASIC-EXPLANATION.                        ELTEMERG
01897                                                                   ELTEMERG
01898      IF WS-SUPP-PRESENT                                           ELTEMERG
01899          SET CIA-ELSCONIS-DDN TO TRUE                             ELTEMERG
01900          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
01901                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
01902         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTEMERG
01903            MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                 ELTEMERG
01904                                                WS-SUPP-DAYS-ACC   ELTEMERG
01905            MOVE WS-SUPP-W-IN-DAYS-ACC  TO                         ELTEMERG
01906                                        COF-DTL-LINE(WS-CIA)       ELTEMERG
01907            ADD +1  TO  WS-CIA                                     ELTEMERG
01908            PERFORM 2250-CALL-ELUOUTPT                             ELTEMERG
01909            PERFORM 2500-SUPP-EXPLANATION.                         ELTEMERG
01910      GO TO 2280-ADD-A-LINE-OUTPUT.                                ELTEMERG
01911                                                                   ELTEMERG
01912  2250-CALL-ELUOUTPT.                                              ELTEMERG
01913                                                                   ELTEMERG
01914      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
01915      MOVE 1  TO  WS-CIA.                                          ELTEMERG
01916      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
01917             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01918      END-EXEC.                                                    ELTEMERG
01919                                                                   ELTEMERG
01920  2280-ADD-A-LINE-OUTPUT.                                          ELTEMERG
01921      IF WS-EXPLANATION-PRODUCED                                   ELTEMERG
01922         MOVE WS-BEYOND-LIMIT-EXPLAIN  TO  COF-DTL-LINE(WS-CIA)    ELTEMERG
01923         ADD +1  TO  WS-CIA.                                       ELTEMERG
01924      IF WS-BASIC-EXPLANATION                                      ELTEMERG
01925         MOVE WS-BASIC-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)          ELTEMERG
01926         ADD +1  TO  WS-CIA                                        ELTEMERG
01927         IF WS-BASIC-EXPLAIN-CNT  >  1                             ELTEMERG
01928            MOVE WS-BASIC-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
01929            ADD +1  TO  WS-CIA.                                    ELTEMERG
01930      IF WS-SUPP-EXPLANATION                                       ELTEMERG
01931         MOVE WS-SUPP-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)           ELTEMERG
01932         ADD +1  TO  WS-CIA                                        ELTEMERG
01933         IF WS-SUPP-EXPLAIN-CNT  >  1                              ELTEMERG
01934            MOVE WS-SUPP-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)        ELTEMERG
01935            ADD +1  TO  WS-CIA.                                    ELTEMERG
01936      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTEMERG
01937                                                                   ELTEMERG
01938      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
01939      MOVE 1  TO  WS-CIA.                                          ELTEMERG
01940      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
01941             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
01942      END-EXEC.                                                    ELTEMERG
01943                                                                   ELTEMERG
01944  2299-EXIT.           EXIT.                                       ELTEMERG
01945                                                                   ELTEMERG
01946 /        D A Y S   O F   P R O F   T R E A T M E N T   R T N E    ELTEMERG
01947 ***************************************************************** ELTEMERG
01948 *        D A Y S   O F   P R O F   T R E A T M E N T   R T N E    ELTEMERG
01949 *                                                                 ELTEMERG
01950 *                                                                 ELTEMERG
01951 ***************************************************************** ELTEMERG
01952  2300-DAYS-OF-PROF-TREATMENT SECTION.                             ELTEMERG
01953      INITIALIZE WS-BASIC-CONTRACT                                 ELTEMERG
01954                 WS-SUPP-CONTRACT.                                 ELTEMERG
01955                                                                   ELTEMERG
01956      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTEMERG
01957      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01958                          ADDRESS OF CONTRACT-RECORD.              ELTEMERG
01959      IF CIA-RC-OK                                                 ELTEMERG
01960         SET WS-BASIC-PRESENT TO TRUE.                             ELTEMERG
01961                                                                   ELTEMERG
01962      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTEMERG
01963      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
01964                          ADDRESS OF CONTRACT-RECORD.              ELTEMERG
01965      IF CIA-RC-OK                                                 ELTEMERG
01966         SET WS-SUPP-PRESENT TO TRUE.                              ELTEMERG
01967                                                                   ELTEMERG
01968                                                                   ELTEMERG
01969      IF NOT WS-BASIC-PRESENT  AND                                 ELTEMERG
01970         NOT WS-SUPP-PRESENT                                       ELTEMERG
01971         GO TO 2399-EXIT.                                          ELTEMERG
01972                                                                   ELTEMERG
01973      IF WS-BASIC-PRESENT                                          ELTEMERG
01974         SET CIA-ELSCONPB-DDN TO TRUE                              ELTEMERG
01975         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTEMERG
01976                         ADDRESS OF CONTRACT-RECORD                ELTEMERG
01977         IF SSB-SUB-TOPIC     =  'EAC'                             ELTEMERG
01978            IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO              ELTEMERG
01979               IF WS-SUPP-PRESENT                                  ELTEMERG
01980                  SET CIA-ELSCONPS-DDN TO TRUE                     ELTEMERG
01981                  CALL 'ELUSETAD' USING DFHCOMMAREA                ELTEMERG
01982                                        ADDRESS OF CONTRACT-RECORD ELTEMERG
01983                  IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO        ELTEMERG
01984                     GO TO 2399-EXIT                               ELTEMERG
01985                  ELSE                                             ELTEMERG
01986                     NEXT SENTENCE                                 ELTEMERG
01987               ELSE                                                ELTEMERG
01988                  GO TO 2399-EXIT                                  ELTEMERG
01989            ELSE                                                   ELTEMERG
01990               NEXT SENTENCE                                       ELTEMERG
01991         ELSE                                                      ELTEMERG
01992            IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO               ELTEMERG
01993               IF WS-SUPP-PRESENT                                  ELTEMERG
01994                  SET CIA-ELSCONPS-DDN TO TRUE                     ELTEMERG
01995                  CALL 'ELUSETAD' USING DFHCOMMAREA                ELTEMERG
01996                                        ADDRESS OF CONTRACT-RECORD ELTEMERG
01997                  IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO         ELTEMERG
01998                     GO TO 2399-EXIT                               ELTEMERG
01999                  ELSE                                             ELTEMERG
02000                     NEXT SENTENCE                                 ELTEMERG
02001               ELSE                                                ELTEMERG
02002                  GO TO 2399-EXIT.                                 ELTEMERG
02003                                                                   ELTEMERG
02004      IF NOT WS-BASIC-PRESENT  AND                                 ELTEMERG
02005         WS-SUPP-PRESENT                                           ELTEMERG
02006                  SET CIA-ELSCONPS-DDN TO TRUE                     ELTEMERG
02007                  CALL 'ELUSETAD' USING DFHCOMMAREA                ELTEMERG
02008                                        ADDRESS OF CONTRACT-RECORD ELTEMERG
02009         IF SSB-SUB-TOPIC     =  'EAC'                             ELTEMERG
02010            IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  =  ZERO              ELTEMERG
02011               GO TO 2399-EXIT                                     ELTEMERG
02012            ELSE                                                   ELTEMERG
02013               NEXT SENTENCE                                       ELTEMERG
02014         ELSE                                                      ELTEMERG
02015            IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  ZERO               ELTEMERG
02016               GO TO 2399-EXIT.                                    ELTEMERG
02017                                                                   ELTEMERG
02018      MOVE WS-TREATMENT-MUST-BE  TO  COF-DTL-LINE(WS-CIA).         ELTEMERG
02019      ADD +1  TO  WS-CIA.                                          ELTEMERG
02020      IF SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
02021         GO TO 2310-PROF-ACCIDENT.                                 ELTEMERG
02022                                                                   ELTEMERG
02023      IF WS-BASIC-PRESENT                                          ELTEMERG
02024          SET CIA-ELSCONPB-DDN TO TRUE                             ELTEMERG
02025          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
02026                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
02027         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO AND 999      ELTEMERG
02028            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTEMERG
02029                                               WS-BASIC-DAYS-MED   ELTEMERG
02030            MOVE WS-BASIC-W-IN-DAYS-MED  TO                        ELTEMERG
02031                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
02032         END-IF                                                    ELTEMERG
02033         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTEMERG
02034            MOVE TREATMENT-999-MSGA TO WS-DTL-BASIC                ELTEMERG
02035            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTEMERG
02036            ADD +1 TO WS-CIA                                       ELTEMERG
02037            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTEMERG
02038         END-IF                                                    ELTEMERG
02039            ADD +1  TO  WS-CIA                                     ELTEMERG
02040            PERFORM 2350-CALL-ELUOUTPT                             ELTEMERG
02041            PERFORM 2400-BASIC-EXPLANATION.                        ELTEMERG
02042                                                                   ELTEMERG
02043      IF WS-SUPP-PRESENT                                           ELTEMERG
02044          SET CIA-ELSCONPS-DDN TO TRUE                             ELTEMERG
02045          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
02046                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
02047         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  NOT =  ZERO              ELTEMERG
02048            MOVE GCT-DAYS-BTWN-MED-EMRG-TREAT  TO                  ELTEMERG
02049                                               WS-SUPP-DAYS-MED    ELTEMERG
02050            MOVE WS-SUPP-W-IN-DAYS-MED  TO                         ELTEMERG
02051                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
02052         END-IF                                                    ELTEMERG
02053         IF GCT-DAYS-BTWN-MED-EMRG-TREAT  =  999                   ELTEMERG
02054            MOVE TREATMENT-999-MSGA TO WS-DTL-SUPPLEMENTAL         ELTEMERG
02055            MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)           ELTEMERG
02056            ADD +1 TO WS-CIA                                       ELTEMERG
02057            MOVE TREATMENT-999-MSGB TO COF-DTL-LINE(WS-CIA)        ELTEMERG
02058         END-IF                                                    ELTEMERG
02059            ADD +1  TO  WS-CIA                                     ELTEMERG
02060            PERFORM 2350-CALL-ELUOUTPT                             ELTEMERG
02061            PERFORM 2500-SUPP-EXPLANATION.                         ELTEMERG
02062                                                                   ELTEMERG
02063         GO TO 2380-ADD-A-LINE-OUTPUT.                             ELTEMERG
02064                                                                   ELTEMERG
02065  2310-PROF-ACCIDENT.                                              ELTEMERG
02066      IF WS-BASIC-PRESENT                                          ELTEMERG
02067          SET CIA-ELSCONPB-DDN TO TRUE                             ELTEMERG
02068          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
02069                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
02070         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTEMERG
02071            MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                 ELTEMERG
02072                                                WS-BASIC-DAYS-ACC  ELTEMERG
02073            MOVE WS-BASIC-W-IN-DAYS-ACC  TO                        ELTEMERG
02074                                         COF-DTL-LINE(WS-CIA)      ELTEMERG
02075            ADD +1  TO  WS-CIA                                     ELTEMERG
02076            PERFORM 2350-CALL-ELUOUTPT                             ELTEMERG
02077            PERFORM 2400-BASIC-EXPLANATION                         ELTEMERG
02078            GO TO 2380-ADD-A-LINE-OUTPUT.                          ELTEMERG
02079                                                                   ELTEMERG
02080      IF WS-SUPP-PRESENT                                           ELTEMERG
02081          SET CIA-ELSCONPS-DDN TO TRUE                             ELTEMERG
02082          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTEMERG
02083                          ADDRESS OF CONTRACT-RECORD               ELTEMERG
02084         IF GCT-DAYS-BTWN-ACCD-EMRG-TREAT  NOT =  ZERO             ELTEMERG
02085            MOVE GCT-DAYS-BTWN-ACCD-EMRG-TREAT  TO                 ELTEMERG
02086                                                WS-SUPP-DAYS-ACC   ELTEMERG
02087            MOVE WS-SUPP-W-IN-DAYS-ACC  TO                         ELTEMERG
02088                                        COF-DTL-LINE(WS-CIA)       ELTEMERG
02089            ADD +1  TO  WS-CIA                                     ELTEMERG
02090            PERFORM 2500-SUPP-EXPLANATION                          ELTEMERG
02091            GO TO 2380-ADD-A-LINE-OUTPUT.                          ELTEMERG
02092                                                                   ELTEMERG
02093      GO TO 2380-ADD-A-LINE-OUTPUT.                                ELTEMERG
02094                                                                   ELTEMERG
02095  2350-CALL-ELUOUTPT.                                              ELTEMERG
02096      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
02097      MOVE 1  TO  WS-CIA.                                          ELTEMERG
02098      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
02099             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02100      END-EXEC.                                                    ELTEMERG
02101                                                                   ELTEMERG
02102  2380-ADD-A-LINE-OUTPUT.                                          ELTEMERG
02103      IF WS-EXPLANATION-PRODUCED                                   ELTEMERG
02104         MOVE WS-BEYOND-LIMIT-EXPLAIN  TO  COF-DTL-LINE(WS-CIA)    ELTEMERG
02105         ADD +1  TO  WS-CIA.                                       ELTEMERG
02106      IF WS-BASIC-EXPLANATION                                      ELTEMERG
02107         MOVE WS-BASIC-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)          ELTEMERG
02108         ADD +1  TO  WS-CIA                                        ELTEMERG
02109         IF WS-BASIC-EXPLAIN-CNT  >  1                             ELTEMERG
02110            MOVE WS-BASIC-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
02111            ADD +1  TO  WS-CIA.                                    ELTEMERG
02112      IF WS-SUPP-EXPLANATION                                       ELTEMERG
02113         MOVE WS-SUPP-EXPLAIN1  TO  COF-DTL-LINE(WS-CIA)           ELTEMERG
02114         ADD +1  TO  WS-CIA                                        ELTEMERG
02115         IF WS-SUPP-EXPLAIN-CNT  >  1                              ELTEMERG
02116            MOVE WS-SUPP-EXPLAIN2  TO  COF-DTL-LINE(WS-CIA)        ELTEMERG
02117            ADD +1  TO  WS-CIA.                                    ELTEMERG
02118      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTEMERG
02119                                                                   ELTEMERG
02120      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
02121      MOVE 1  TO  WS-CIA.                                          ELTEMERG
02122      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
02123             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02124      END-EXEC.                                                    ELTEMERG
02125                                                                   ELTEMERG
02126  2399-EXIT.           EXIT.                                       ELTEMERG
02127                                                                   ELTEMERG
02128 /            B A S I C   E X P L A N A T I O N                    ELTEMERG
02129 ***************************************************************** ELTEMERG
02130 *            B A S I C   E X P L A N A T I O N                    ELTEMERG
02131 *                                                                 ELTEMERG
02132 ***************************************************************** ELTEMERG
02133  2400-BASIC-EXPLANATION SECTION.                                  ELTEMERG
02134      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTEMERG
02135      IF SSB-SUB-TOPIC     =  'EAC' AND                            ELTEMERG
02136         GCT-CHG-EAC-PROV-IND  =  ZERO                             ELTEMERG
02137         GO TO 2499-EXIT.                                          ELTEMERG
02138                                                                   ELTEMERG
02139      IF SSB-SUB-TOPIC     =  'EMC' AND                            ELTEMERG
02140         GCT-CHG-EMC-PROV-IND  =  ZERO                             ELTEMERG
02141         GO TO 2499-EXIT.                                          ELTEMERG
02142                                                                   ELTEMERG
02143      MOVE 'N'  TO  WS-MOVE-LINES-IND.                             ELTEMERG
02144      MOVE SPACES  TO  WS-DTL-BASIC-LONG  WS-BASIC-EXPLAIN1,       ELTEMERG
02145                       WS-BASIC-EXPLAIN2.                          ELTEMERG
02146      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTEMERG
02147      ADD +1  TO  WS-EXPLANATION-IND.                              ELTEMERG
02148      MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA.                    ELTEMERG
02149      MOVE +63  TO  WS-TEMP-NOT-USED-CNT.                          ELTEMERG
02150      IF SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
02151         MOVE 'CHG-EAC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTEMERG
02152         MOVE GCT-CHG-EAC-PROV-IND  TO  CMF-CODE-VALUE             ELTEMERG
02153         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02154                                                                   ELTEMERG
02155      IF SSB-SUB-TOPIC     =  'EMC'                                ELTEMERG
02156         MOVE 'CHG-EMC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTEMERG
02157         MOVE GCT-CHG-EMC-PROV-IND  TO  CMF-CODE-VALUE             ELTEMERG
02158         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02159                                                                   ELTEMERG
02160      COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -  WS-TEMP-NOT-USED-CNT.ELTEMERG
02161      PERFORM 2450-CONCATENATE-TO-TEMP-TEXT                        ELTEMERG
02162         VARYING WS-SUB1  FROM  1  BY  1                           ELTEMERG
02163         UNTIL  WS-TEMP-NOT-USED-CNT  >  +78.                      ELTEMERG
02164      MOVE WS-TEMP-TEXT-AREA  TO  WS-BASIC-EXPLAIN1.               ELTEMERG
02165      IF TCAR-OUTPUT-FIELDS-USED  >  1                             ELTEMERG
02166         MOVE 2  TO  WS-BASIC-EXPLAIN-CNT                          ELTEMERG
02167         MOVE TCAR-OPF-DATA(2)  TO  WS-BASIC-EXPLAIN2              ELTEMERG
02168      ELSE                                                         ELTEMERG
02169         MOVE 1  TO  WS-BASIC-EXPLAIN-CNT.                         ELTEMERG
02170      GO TO 2499-EXIT.                                             ELTEMERG
02171                                                                   ELTEMERG
02172  2450-CONCATENATE-TO-TEMP-TEXT.                                   ELTEMERG
02173      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTEMERG
02174      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEMERG
02175                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEMERG
02176                                                                   ELTEMERG
02177  2499-EXIT.           EXIT.                                       ELTEMERG
02178                                                                   ELTEMERG
02179 /            S U P P   E X P L A N A T I O N                      ELTEMERG
02180 ***************************************************************** ELTEMERG
02181 *            S U P P   E X P L A N A T I O N                      ELTEMERG
02182 *                                                                 ELTEMERG
02183 ***************************************************************** ELTEMERG
02184  2500-SUPP-EXPLANATION SECTION.                                   ELTEMERG
02185      MOVE ZERO  TO  WS-EXPLANATION-IND.                           ELTEMERG
02186      IF SSB-SUB-TOPIC     =  'EAC' AND                            ELTEMERG
02187         GCT-CHG-EAC-PROV-IND  =  ZERO                             ELTEMERG
02188         GO TO 2599-EXIT.                                          ELTEMERG
02189                                                                   ELTEMERG
02190      IF SSB-SUB-TOPIC     =  'EMC' AND                            ELTEMERG
02191         GCT-CHG-EMC-PROV-IND  =  ZERO                             ELTEMERG
02192         GO TO 2599-EXIT.                                          ELTEMERG
02193                                                                   ELTEMERG
02194      MOVE SPACES  TO  WS-DTL-SUPP-LONG,  WS-SUPP-EXPLAIN1,        ELTEMERG
02195                       WS-SUPP-EXPLAIN2.                           ELTEMERG
02196      MOVE 'N'  TO  WS-MOVE-LINES-IND.                             ELTEMERG
02197      MOVE 'CONTRACT'  TO  CMF-RECORD-PREFIX.                      ELTEMERG
02198      ADD +2  TO  WS-EXPLANATION-IND.                              ELTEMERG
02199      MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA.                     ELTEMERG
02200      MOVE +63  TO  WS-TEMP-NOT-USED-CNT.                          ELTEMERG
02201      IF SSB-SUB-TOPIC     =  'EAC'                                ELTEMERG
02202         MOVE 'CHG-EAC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTEMERG
02203         MOVE GCT-CHG-EAC-PROV-IND  TO  CMF-CODE-VALUE             ELTEMERG
02204         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02205                                                                   ELTEMERG
02206      IF SSB-SUB-TOPIC     =  'EMC'                                ELTEMERG
02207         MOVE 'CHG-EMC-PROV-IND'  TO  CMF-ELEMENT-SYSTEM-NAME      ELTEMERG
02208         MOVE GCT-CHG-EMC-PROV-IND  TO  CMF-CODE-VALUE             ELTEMERG
02209         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02210                                                                   ELTEMERG
02211      COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -  WS-TEMP-NOT-USED-CNT.ELTEMERG
02212      PERFORM 2550-CONCATENATE-TO-TEMP-TEXT                        ELTEMERG
02213         VARYING WS-SUB1  FROM  1  BY  1                           ELTEMERG
02214         UNTIL  WS-TEMP-NOT-USED-CNT  >  +78.                      ELTEMERG
02215                                                                   ELTEMERG
02216      MOVE WS-TEMP-TEXT-AREA  TO  WS-SUPP-EXPLAIN1.                ELTEMERG
02217      IF TCAR-OUTPUT-FIELDS-USED  >  1                             ELTEMERG
02218         MOVE 2  TO  WS-SUPP-EXPLAIN-CNT                           ELTEMERG
02219         MOVE TCAR-OPF-DATA(2)  TO  WS-SUPP-EXPLAIN2               ELTEMERG
02220      ELSE                                                         ELTEMERG
02221         MOVE 1  TO  WS-SUPP-EXPLAIN-CNT.                          ELTEMERG
02222      GO TO 2599-EXIT.                                             ELTEMERG
02223                                                                   ELTEMERG
02224  2550-CONCATENATE-TO-TEMP-TEXT.                                   ELTEMERG
02225      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTEMERG
02226      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEMERG
02227                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEMERG
02228                                                                   ELTEMERG
02229  2599-EXIT.           EXIT.                                       ELTEMERG
02230                                                                   ELTEMERG
02231 /     G E N E R A L   G E T   T A B U L A R   R T N E             ELTEMERG
02232  2600-GENERAL-TABULAR-RTNE SECTION.                               ELTEMERG
02233 **---------------------------------------------------------------+ELTEMERG
02234 **                                                               |ELTEMERG
02235 **                  # A A R   T A B U L A R   F O U N D          |ELTEMERG
02236      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02237      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02238         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02239                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02240         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02241         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTEMERG
02242         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTEMERG
02243      ELSE                                                         ELTEMERG
02244         SET PLT-INDEX2  TO  2                                     ELTEMERG
02245         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTEMERG
02246            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTEMERG
02247                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02248            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
02249            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTEMERG
02250            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTEMERG
02251                                                                   ELTEMERG
02252      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
02253         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02254         MOVE 1  TO  WS-CIA                                        ELTEMERG
02255         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02256             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02257         END-EXEC.                                                 ELTEMERG
02258 **                                                               |ELTEMERG
02259 **---------------------------------------------------------------+ELTEMERG
02260                                                                   ELTEMERG
02261 **---------------------------------------------------------------+ELTEMERG
02262 **                                                               |ELTEMERG
02263 **                  # P P F   T A B U L A R                      |ELTEMERG
02264      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02265      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02266         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02267                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02268         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02269                                                 KWA-GCTABULR-KEY  ELTEMERG
02270         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02271         IF IOP-RC-OK                                              ELTEMERG
02272            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTEMERG
02273                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02274            END-EXEC.                                              ELTEMERG
02275                                                                   ELTEMERG
02276      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02277      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02278         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02279                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTEMERG
02280         KWA-PROVISION-ID    = '#PPF  ' AND                        ELTEMERG
02281         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02282                                                 KWA-GCTABULR-KEY  ELTEMERG
02283         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02284                                                 KWA-GCTABULR-KEY  ELTEMERG
02285         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02286         IF IOP-RC-OK                                              ELTEMERG
02287            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTEMERG
02288                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02289            END-EXEC.                                              ELTEMERG
02290      MOVE SPACE TO KWA-PROVISION-ID.                              ELTEMERG
02291 **                                                               |ELTEMERG
02292 **---------------------------------------------------------------+ELTEMERG
02293                                                                   ELTEMERG
02294 **---------------------------------------------------------------+ELTEMERG
02295 **                                                               |ELTEMERG
02296 **                  # P V E   T A B U L A R                      |ELTEMERG
02297      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02298      MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                            ELTEMERG
02299      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
02300      MOVE WS-PROVIDER-ELIGIBILITY  TO  COF-DTL-LINE(1).           ELTEMERG
02301                                                                   ELTEMERG
02302      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
02303         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02304         MOVE 1  TO  WS-CIA                                        ELTEMERG
02305         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02306             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02307         END-EXEC.                                                 ELTEMERG
02308 **                                                               |ELTEMERG
02309 **---------------------------------------------------------------+ELTEMERG
02310                                                                   ELTEMERG
02311 **---------------------------------------------------------------+ELTEMERG
02312 **                                                               |ELTEMERG
02313 **                  # A B M   T A B U L A R                      |ELTEMERG
02314      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02315      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02316         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02317                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02318         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02319                                                 KWA-GCTABULR-KEY  ELTEMERG
02320         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02321         IF IOP-RC-OK                                              ELTEMERG
02322            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTEMERG
02323                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02324            END-EXEC.                                              ELTEMERG
02325                                                                   ELTEMERG
02326      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02327      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02328         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02329                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTEMERG
02330         KWA-PROVISION-ID = '#ABM  ' AND                           ELTEMERG
02331         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02332                                                 KWA-GCTABULR-KEY  ELTEMERG
02333         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02334                                                 KWA-GCTABULR-KEY  ELTEMERG
02335         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02336         IF IOP-RC-OK                                              ELTEMERG
02337            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTEMERG
02338                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02339            END-EXEC.                                              ELTEMERG
02340      MOVE SPACE TO KWA-PROVISION-ID.                              ELTEMERG
02341 **                                                               |ELTEMERG
02342 **---------------------------------------------------------------+ELTEMERG
02343                                                                   ELTEMERG
02344 **---------------------------------------------------------------+ELTEMERG
02345 **                                                               |ELTEMERG
02346 **                  # A C L   T A B U L A R                      |ELTEMERG
02347      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02348      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02349         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02350                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02351         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02352                                                 KWA-GCTABULR-KEY  ELTEMERG
02353         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02354         IF IOP-RC-OK                                              ELTEMERG
02355            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTEMERG
02356                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02357            END-EXEC.                                              ELTEMERG
02358                                                                   ELTEMERG
02359      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02360      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02361         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02362                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTEMERG
02363         KWA-PROVISION-ID = '#ACL  ' AND                           ELTEMERG
02364         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02365                                                 KWA-GCTABULR-KEY  ELTEMERG
02366         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02367                                                 KWA-GCTABULR-KEY  ELTEMERG
02368         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02369         IF IOP-RC-OK                                              ELTEMERG
02370            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTEMERG
02371                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02372            END-EXEC.                                              ELTEMERG
02373      MOVE SPACE TO KWA-PROVISION-ID.                              ELTEMERG
02374 **                                                               |ELTEMERG
02375 **---------------------------------------------------------------+ELTEMERG
02376                                                                   ELTEMERG
02377 **---------------------------------------------------------------+ELTEMERG
02378 **                                                               |ELTEMERG
02379 **                  # A D L   T A B U L A R                      |ELTEMERG
02380      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02381      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02382         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02383                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02384         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02385                                                 KWA-GCTABULR-KEY  ELTEMERG
02386         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02387         IF IOP-RC-OK                                              ELTEMERG
02388            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTEMERG
02389                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02390            END-EXEC.                                              ELTEMERG
02391                                                                   ELTEMERG
02392      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02393      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02394         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02395                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTEMERG
02396         KWA-PROVISION-ID = '#ADL  ' AND                           ELTEMERG
02397         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02398                                                 KWA-GCTABULR-KEY  ELTEMERG
02399         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02400                                                 KWA-GCTABULR-KEY  ELTEMERG
02401         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02402         IF IOP-RC-OK                                              ELTEMERG
02403            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTEMERG
02404                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02405            END-EXEC.                                              ELTEMERG
02406      MOVE SPACE TO KWA-PROVISION-ID.                              ELTEMERG
02407 **                                                               |ELTEMERG
02408 **---------------------------------------------------------------+ELTEMERG
02409                                                                   ELTEMERG
02410 **---------------------------------------------------------------+ELTEMERG
02411 **                                                               |ELTEMERG
02412 **                  # A O L   T A B U L A R                      |ELTEMERG
02413      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02414      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02415         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02416                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTEMERG
02417         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02418                                                 KWA-GCTABULR-KEY  ELTEMERG
02419         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02420         IF IOP-RC-OK                                              ELTEMERG
02421            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTEMERG
02422                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02423            END-EXEC.                                              ELTEMERG
02424                                                                   ELTEMERG
02425      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02426      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02427         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02428                ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES AND ELTEMERG
02429         KWA-PROVISION-ID = '#AOL  ' AND                           ELTEMERG
02430         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02431                                                 KWA-GCTABULR-KEY  ELTEMERG
02432         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02433                                                 KWA-GCTABULR-KEY  ELTEMERG
02434         PERFORM 2700-GET-TABULAR-RECORD                           ELTEMERG
02435         IF IOP-RC-OK                                              ELTEMERG
02436            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTEMERG
02437                 COMMAREA(DFHCOMMAREA)                             ELTEMERG
02438            END-EXEC.                                              ELTEMERG
02439      MOVE SPACE TO KWA-PROVISION-ID.                              ELTEMERG
02440 **                                                               |ELTEMERG
02441 **---------------------------------------------------------------+ELTEMERG
02442                                                                   ELTEMERG
02443  2699-EXIT.          EXIT.                                        ELTEMERG
02444                                                                   ELTEMERG
02445 /            G E T   T A B U L A R   R E C O R D                  ELTEMERG
02446 ***************************************************************** ELTEMERG
02447 *            G E T   T A B U L A R   R E C O R D                  ELTEMERG
02448 *                                                                 ELTEMERG
02449 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTEMERG
02450 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTEMERG
02451 *  TO DISPLAY.                                                    ELTEMERG
02452 *                                                                 ELTEMERG
02453 ***************************************************************** ELTEMERG
02454  2700-GET-TABULAR-RECORD SECTION.                                 ELTEMERG
02455      SET CIA-GCTABULR-DDN TO TRUE.                                ELTEMERG
02456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
02457                      ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.      ELTEMERG
02458                                                                   ELTEMERG
02459      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTEMERG
02460      SET CIA-GCTABULR-DDN TO TRUE.                                ELTEMERG
02461      SET IOP-RD                     TO TRUE.                      ELTEMERG
02462      SET IOP-FCQ-NONE               TO TRUE.                      ELTEMERG
02463      SET IOP-KVQ-NONE               TO TRUE.                      ELTEMERG
02464                                                                   ELTEMERG
02465      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTEMERG
02466             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02467      END-EXEC.                                                    ELTEMERG
02468                                                                   ELTEMERG
02469      IF IOP-RC-NOTFND                                             ELTEMERG
02470         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTEMERG
02471         EXEC CICS ABEND                                           ELTEMERG
02472                   ABCODE(CIA-ABCODE)                              ELTEMERG
02473         END-EXEC.                                                 ELTEMERG
02474                                                                   ELTEMERG
02475      IF NOT IOP-RC-OK                                             ELTEMERG
02476         SET CIA-AB-CRITIO          TO TRUE                        ELTEMERG
02477         EXEC CICS ABEND                                           ELTEMERG
02478                   ABCODE(CIA-ABCODE)                              ELTEMERG
02479         END-EXEC.                                                 ELTEMERG
02480                                                                   ELTEMERG
02481  2799-EXIT.           EXIT.                                       ELTEMERG
02482                                                                   ELTEMERG
02483 /    C O D E S   M A N U A L   W I T H   P E R C E N T A G E      ELTEMERG
02484  2800-CODE-MANUAL-WITH-PERCENT SECTION.                           ELTEMERG
02485      INITIALIZE CMF-RETURN-CODE,                                  ELTEMERG
02486                 TCAR-FROM-AREA.                                   ELTEMERG
02487                                                                   ELTEMERG
02488      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTEMERG
02489      END-EXEC.                                                    ELTEMERG
02490                                                                   ELTEMERG
02491      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTEMERG
02492      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
02493                      ADDRESS OF CMF-DESCR.                        ELTEMERG
02494                                                                   ELTEMERG
02495                                                                   ELTEMERG
02496      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTEMERG
02497         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTEMERG
02498         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTEMERG
02499            CMF-DESCR-LINE(1),        ' ',                         ELTEMERG
02500            CMF-DESCR-LINE(2),        ' ',                         ELTEMERG
02501            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTEMERG
02502            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTEMERG
02503      ELSE                                                         ELTEMERG
02504         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTEMERG
02505         STRING CMF-DESCR-LINE(1),        ' ',                     ELTEMERG
02506            CMF-DESCR-LINE(2),        ' ',                         ELTEMERG
02507            CMF-DESCR-LINE(3),        ' ',  WS-PERCENT-FLD         ELTEMERG
02508            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTEMERG
02509      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEMERG
02510                                                                   ELTEMERG
02511      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTEMERG
02512      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTEMERG
02513      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTEMERG
02514                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTEMERG
02515                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTEMERG
02516      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEMERG
02517                                                                   ELTEMERG
02518      IF WS-MOVE-LINES-TO-CIA                                      ELTEMERG
02519         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTEMERG
02520            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTEMERG
02521                                             WS-TEMP-NOT-USED-CNT  ELTEMERG
02522            PERFORM 2850-CONCATENATE-TO-TEMP-TEXT                  ELTEMERG
02523              VARYING WS-SUB1  FROM  1  BY  1                      ELTEMERG
02524              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                  ELTEMERG
02525            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTEMERG
02526            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTEMERG
02527            ADD +1  TO  WS-CIA                                     ELTEMERG
02528         ELSE                                                      ELTEMERG
02529            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTEMERG
02530            ADD +1  TO  WS-CIA.                                    ELTEMERG
02531                                                                   ELTEMERG
02532      IF WS-MOVE-LINES-TO-CIA                                      ELTEMERG
02533         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTEMERG
02534            PERFORM 2860-MOVE-LINES-TO-CIA                         ELTEMERG
02535               VARYING WS-SUB1  FROM 2  BY  1                      ELTEMERG
02536               UNTIL WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED           ELTEMERG
02537         ELSE                                                      ELTEMERG
02538            NEXT SENTENCE                                          ELTEMERG
02539      ELSE                                                         ELTEMERG
02540         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTEMERG
02541                                                                   ELTEMERG
02542      GO TO 2899-EXIT.                                             ELTEMERG
02543                                                                   ELTEMERG
02544  2850-CONCATENATE-TO-TEMP-TEXT.                                   ELTEMERG
02545      ADD  +1  TO  WS-TEMP-NOT-USED-CNT.                           ELTEMERG
02546      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTEMERG
02547                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTEMERG
02548                                                                   ELTEMERG
02549  2860-MOVE-LINES-TO-CIA.                                          ELTEMERG
02550      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTEMERG
02551      ADD +1  TO  WS-CIA.                                          ELTEMERG
02552                                                                   ELTEMERG
02553  2899-EXIT.           EXIT.                                       ELTEMERG
02554                                                                   ELTEMERG
02555 /            I N S T I T U T I O N A L   M E D   R T N E          ELTEMERG
02556 ***************************************************************** ELTEMERG
02557 *            I N S T I T U T I O N A L   M E D   R T N E          ELTEMERG
02558 *                                                                 ELTEMERG
02559 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEMERG
02560 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEMERG
02561 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEMERG
02562 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEMERG
02563 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEMERG
02564 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEMERG
02565 *  MODULE.                                                        ELTEMERG
02566 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEMERG
02567 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEMERG
02568 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEMERG
02569 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEMERG
02570 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEMERG
02571 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEMERG
02572 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEMERG
02573 *                                                                 ELTEMERG
02574 ***************************************************************** ELTEMERG
02575  3000-INSTITUTIONAL-MED-RTNE SECTION.                             ELTEMERG
02576      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEMERG
02577      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
02578      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEMERG
02579                     COF-NBR-DTL-LINES.                            ELTEMERG
02580      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
02581      END-EXEC.                                                    ELTEMERG
02582      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEMERG
02583      MOVE WS-HDR-2-INST-MED  TO  COF-HDR-LINE(2).                 ELTEMERG
02584                                                                   ELTEMERG
02585                                                                   ELTEMERG
02586      MOVE WS-INST-MED-CNT  TO  PVN-NBR-BEN-PROVN.                 ELTEMERG
02587      PERFORM 3010-MOVE-IN-INST-MED                                ELTEMERG
02588         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEMERG
02589         UNTIL WS-SUB  >  WS-INST-MED-CNT.                         ELTEMERG
02590                                                                   ELTEMERG
02591      GO TO 3020-CALL-COVERAGE.                                    ELTEMERG
02592  3010-MOVE-IN-INST-MED.                                           ELTEMERG
02593      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEMERG
02594      MOVE WS-INST-MED-LIST(WS-SUB)  TO                            ELTEMERG
02595                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEMERG
02596      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEMERG
02597                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEMERG
02598                                                                   ELTEMERG
02599  3020-CALL-COVERAGE.                                              ELTEMERG
02600      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
02601      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
02602      END-EXEC.                                                    ELTEMERG
02603                                                                   ELTEMERG
02604      MOVE WS-STD-MEDICAL-SERVICES  TO  SSB-TOPIC-PHRASE.          ELTEMERG
02605                                                                   ELTEMERG
02606      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEMERG
02607      END-EXEC.                                                    ELTEMERG
02608                                                                   ELTEMERG
02609      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTEMERG
02610      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
02611      END-EXEC.                                                    ELTEMERG
02612                                                                   ELTEMERG
02613      IF PVN-COVG-NONE                                             ELTEMERG
02614         GO TO 3099-EXIT.                                          ELTEMERG
02615                                                                   ELTEMERG
02616      MOVE +1  TO  WS-CIA.                                         ELTEMERG
02617                                                                   ELTEMERG
02618      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTEMERG
02619      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEMERG
02620            PSP-PROVN-PRICING-METHD,                               ELTEMERG
02621            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEMERG
02622            PSP-TRANSF-OTHER-RESP-IND,                             ELTEMERG
02623            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEMERG
02624            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEMERG
02625            PSP-SPILL-OVER-DED-APL-IND,                            ELTEMERG
02626            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEMERG
02627            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEMERG
02628            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEMERG
02629            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEMERG
02630            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEMERG
02631            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEMERG
02632            PSP-TRAUM-INJ-EFF-DT-COMP-IND,                         ELTEMERG
02633            PSB-PROF-CHRG-HSP-CLM,                                 ELTEMERG
02634            PSB-TREAT-TIME-FACTOR-IND                              ELTEMERG
02635            PSB-TREAT-TIME-FACTOR                                  ELTEMERG
02636            PSW-TREAT-TIME-FACTOR-IND                              ELTEMERG
02637            PSW-TREAT-TIME-FACTOR.                                 ELTEMERG
02638                                                                   ELTEMERG
02639      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEMERG
02640      END-EXEC.                                                    ELTEMERG
02641                                                                   ELTEMERG
02642      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEMERG
02643      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
02644                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTEMERG
02645                                                                   ELTEMERG
02646      PERFORM 3030-FIND-FIRST-NONZERO                              ELTEMERG
02647         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEMERG
02648         UNTIL WS-SUB  >  WS-INST-MED-CNT.                         ELTEMERG
02649                                                                   ELTEMERG
02650      GO TO 3099-EXIT.                                             ELTEMERG
02651  3030-FIND-FIRST-NONZERO.                                         ELTEMERG
02652      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
02653      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEMERG
02654         NEXT SENTENCE                                             ELTEMERG
02655      ELSE                                                         ELTEMERG
02656         PERFORM 3040-BUILD-SCREEN-LINES.                          ELTEMERG
02657                                                                   ELTEMERG
02658  3040-BUILD-SCREEN-LINES.                                         ELTEMERG
02659                                                                   ELTEMERG
02660      SET PLT-INDEX1  TO                                           ELTEMERG
02661                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTEMERG
02662      IF WS-NOT-FIRST-TIME                                         ELTEMERG
02663         MOVE 'P'  TO  COF-FUNCTION                                ELTEMERG
02664         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02665             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02666         END-EXEC                                                  ELTEMERG
02667      ELSE                                                         ELTEMERG
02668         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEMERG
02669                                                                   ELTEMERG
02670      MOVE +1  TO  WS-CIA.                                         ELTEMERG
02671      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
02672         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEMERG
02673            SET PLT-INDEX2  TO  2                                  ELTEMERG
02674         ELSE                                                      ELTEMERG
02675            PERFORM 3090-PROBLEM-WITH-INDICES                      ELTEMERG
02676            GO TO 3099-EXIT                                        ELTEMERG
02677      ELSE                                                         ELTEMERG
02678         SET PLT-INDEX2  TO  1.                                    ELTEMERG
02679                                                                   ELTEMERG
02680 **---------------------------------------------------------------+ELTEMERG
02681 **                                                               |ELTEMERG
02682 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEMERG
02683      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEMERG
02684      ADD  +1  TO  WS-CIA.                                         ELTEMERG
02685      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEMERG
02686                                                                   ELTEMERG
02687      PERFORM 3050-ZERO-ALL-WITH-SAME-NO                           ELTEMERG
02688         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTEMERG
02689         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-MED-CNT.               ELTEMERG
02690      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
02691                                                                   ELTEMERG
02692      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
02693      MOVE 1  TO  WS-CIA.                                          ELTEMERG
02694      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
02695             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
02696      END-EXEC.                                                    ELTEMERG
02697 **                                                               |ELTEMERG
02698 **---------------------------------------------------------------+ELTEMERG
02699                                                                   ELTEMERG
02700 **---------------------------------------------------------------+ELTEMERG
02701 **                                                               |ELTEMERG
02702 **    I L L N E S S   /   I N J U R Y   E F F E C T I V E        |ELTEMERG
02703 **     D A T E   C O M P A R I S O N   I N D I C A T O R         |ELTEMERG
02704      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02705      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02706         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
02707                                                        NOT =  ZEROELTEMERG
02708         MOVE WS-COMPARISON-ILLNESS  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
02709         ADD +1  TO  WS-CIA                                        ELTEMERG
02710         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02711      ELSE                                                         ELTEMERG
02712         SET  PLT-INDEX2  TO  2                                    ELTEMERG
02713         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTEMERG
02714            PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
02715                                                       NOT =  ZERO ELTEMERG
02716            MOVE WS-COMPARISON-ILLNESS  TO  COF-DTL-LINE(WS-CIA)   ELTEMERG
02717            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
02718            ADD +1  TO  WS-CIA.                                    ELTEMERG
02719                                                                   ELTEMERG
02720      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02721      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02722         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
02723                                                        NOT =  ZEROELTEMERG
02724         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02725         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
02726                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
02727         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02728                                               TO   CMF-CODE-VALUE ELTEMERG
02729         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
02730         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
02731         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02732                                                                   ELTEMERG
02733      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
02734      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02735         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
02736                                                        NOT =  ZEROELTEMERG
02737         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02738         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
02739                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
02740         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02741                                               TO   CMF-CODE-VALUE ELTEMERG
02742         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
02743         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
02744         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02745                                                                   ELTEMERG
02746      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
02747         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02748         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
02749         MOVE +1  TO  WS-CIA                                       ELTEMERG
02750         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02751               COMMAREA(DFHCOMMAREA)                               ELTEMERG
02752         END-EXEC.                                                 ELTEMERG
02753 **                                                               |ELTEMERG
02754 **---------------------------------------------------------------+ELTEMERG
02755                                                                   ELTEMERG
02756 **---------------------------------------------------------------+ELTEMERG
02757 **                                                               |ELTEMERG
02758 **                    L I M I T   O N                            |ELTEMERG
02759 **          R E C E P T I O N   O F   T R E A T M E N T          |ELTEMERG
02760 **                         A N D                                 |ELTEMERG
02761 **         E X P L A N A T I O N   O F   P R O C E D U R E       |ELTEMERG
02762 **          W H E N   B E Y O N D   T H A T   L I M I T          |ELTEMERG
02763      PERFORM 2200-DAYS-OF-INST-TREATMENT.                         ELTEMERG
02764 **                                                               |ELTEMERG
02765 **---------------------------------------------------------------+ELTEMERG
02766                                                                   ELTEMERG
02767      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
02768         SET PLT-INDEX2  TO  2                                     ELTEMERG
02769      ELSE                                                         ELTEMERG
02770         SET PLT-INDEX2  TO  1.                                    ELTEMERG
02771                                                                   ELTEMERG
02772 **---------------------------------------------------------------+ELTEMERG
02773 **                                                               |ELTEMERG
02774 **        P L A C E   O F   T R E A T M E N T                    |ELTEMERG
02775      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
02776                                                              ZERO ELTEMERG
02777         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02778         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEMERG
02779         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
02780                                                   CMF-CODE-VALUE  ELTEMERG
02781         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
02782         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTEMERG
02783         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
02784         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
02785         MOVE +1  TO  WS-CIA                                       ELTEMERG
02786         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02787               COMMAREA(DFHCOMMAREA)                               ELTEMERG
02788         END-EXEC.                                                 ELTEMERG
02789 **                                                               |ELTEMERG
02790 **---------------------------------------------------------------+ELTEMERG
02791                                                                   ELTEMERG
02792 **---------------------------------------------------------------+ELTEMERG
02793 **                                                               |ELTEMERG
02794 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEMERG
02795 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEMERG
02796 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTEMERG
02797      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02798      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02799         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
02800                                          ZERO AND  NOT =  '19'    ELTEMERG
02801         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
02802         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02803         ADD +1  TO  WS-CIA.                                       ELTEMERG
02804                                                                   ELTEMERG
02805      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
02806      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02807         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
02808                                        ZERO AND  NOT =  '19' AND  ELTEMERG
02809         NOT WS-ADD-A-BLANK-LINE                                   ELTEMERG
02810         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
02811         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02812         ADD +1  TO  WS-CIA.                                       ELTEMERG
02813                                                                   ELTEMERG
02814      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02815      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02816         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
02817                               AND                                 ELTEMERG
02818         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
02819         SET  PLT-INDEX2  TO  2                                    ELTEMERG
02820         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
02821                                                              ZERO ELTEMERG
02822            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
02823            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
02824            ADD +1  TO  WS-CIA.                                    ELTEMERG
02825                                                                   ELTEMERG
02826      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02827      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02828         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
02829                               AND                                 ELTEMERG
02830         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTEMERG
02831         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEMERG
02832         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEMERG
02833         ADD +1  TO  WS-CIA.                                       ELTEMERG
02834                                                                   ELTEMERG
02835      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEMERG
02836         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
02837         SET  PLT-INDEX2  TO  2                                    ELTEMERG
02838         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
02839                                                              ZERO ELTEMERG
02840            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
02841            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
02842            ADD +1  TO  WS-CIA.                                    ELTEMERG
02843                                                                   ELTEMERG
02844      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
02845      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
02846         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEMERG
02847                                                               ZEROELTEMERG
02848            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02849                                                            =  ZEROELTEMERG
02850               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
02851            ELSE                                                   ELTEMERG
02852               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
02853          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02854                                                  TO  WS-PERCENTAGEELTEMERG
02855         ELSE                                                      ELTEMERG
02856            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
02857          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02858                                                 TO  WS-PERCENTAGE.ELTEMERG
02859      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
02860         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
02861                                                              ZERO ELTEMERG
02862         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02863         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
02864         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
02865                                                    CMF-CODE-VALUE ELTEMERG
02866         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
02867         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
02868         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
02869                                                                   ELTEMERG
02870      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
02871      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
02872         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEMERG
02873                                                               ZEROELTEMERG
02874            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02875                                                            =  ZEROELTEMERG
02876               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTEMERG
02877            ELSE                                                   ELTEMERG
02878               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
02879          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02880                                                 TO   WS-PERCENTAGEELTEMERG
02881         ELSE                                                      ELTEMERG
02882            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
02883          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
02884                                                 TO  WS-PERCENTAGE.ELTEMERG
02885      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02886         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
02887                                                              ZERO ELTEMERG
02888         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02889         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
02890         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
02891                                                    CMF-CODE-VALUE ELTEMERG
02892         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
02893         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
02894         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
02895                                                                   ELTEMERG
02896      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
02897         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02898         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
02899         MOVE 1  TO  WS-CIA                                        ELTEMERG
02900         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
02901                COMMAREA(DFHCOMMAREA)                              ELTEMERG
02902         END-EXEC.                                                 ELTEMERG
02903 **                                                               |ELTEMERG
02904 **---------------------------------------------------------------+ELTEMERG
02905                                                                   ELTEMERG
02906 **---------------------------------------------------------------+ELTEMERG
02907 **                                                               |ELTEMERG
02908 **        P R O F E S S I O N A L   C H A R G E S   O N          |ELTEMERG
02909 **                H O S P I T A L   B I L L                      |ELTEMERG
02910                                                                   ELTEMERG
02911      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02912                                                                   ELTEMERG
02913      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTEMERG
02914          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
02915              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
02916                               NOT  =  '0'  AND  NOT  =  LOW-VALUESELTEMERG
02917                  MOVE WS-PROF-INPT-CHRGES                         ELTEMERG
02918                                  TO  COF-DTL-LINE (WS-CIA)        ELTEMERG
02919                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND           ELTEMERG
02920                  ADD  +1         TO  WS-CIA.                      ELTEMERG
02921                                                                   ELTEMERG
02922      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02923                                                                   ELTEMERG
02924      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTEMERG
02925          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
02926                       AND                                         ELTEMERG
02927             NOT  WS-ADD-A-BLANK-LINE                              ELTEMERG
02928              MOVE WS-PROF-INPT-CHRGES                             ELTEMERG
02929                             TO  COF-DTL-LINE (WS-CIA)             ELTEMERG
02930              MOVE 'Y'       TO  WS-ADD-A-BLANK-IND                ELTEMERG
02931              ADD  +1        TO  WS-CIA.                           ELTEMERG
02932                                                                   ELTEMERG
02933      SET PLT-INDEX2  TO  1.                                       ELTEMERG
02934                                                                   ELTEMERG
02935      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTEMERG
02936          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
02937              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
02938                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTEMERG
02939                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTEMERG
02940                MOVE 'PROF-CHRG-HSP-CLM'                           ELTEMERG
02941                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTEMERG
02942                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTEMERG
02943                                   TO  CMF-CODE-VALUE              ELTEMERG
02944                MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA           ELTEMERG
02945                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTEMERG
02946                PERFORM 2100-CODES-MANUAL-LONG.                    ELTEMERG
02947                                                                   ELTEMERG
02948      SET PLT-INDEX2  TO  2.                                       ELTEMERG
02949                                                                   ELTEMERG
02950      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTEMERG
02951          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTEMERG
02952              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTEMERG
02953                             NOT  =  '0'  AND  NOT  =  LOW-VALUES  ELTEMERG
02954                MOVE 'BPB'         TO  CMF-RECORD-PREFIX           ELTEMERG
02955                MOVE 'PROF-CHRG-HSP-CLM'                           ELTEMERG
02956                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTEMERG
02957                MOVE PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)ELTEMERG
02958                                   TO  CMF-CODE-VALUE              ELTEMERG
02959                MOVE WS-SUPP-LIT   TO  WS-TEMP-TEXT-AREA           ELTEMERG
02960                MOVE 63            TO  WS-TEMP-NOT-USED-CNT        ELTEMERG
02961                PERFORM 2100-CODES-MANUAL-LONG.                    ELTEMERG
02962                                                                   ELTEMERG
02963      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
02964          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTEMERG
02965          ADD  +1   WS-CIA  GIVING  COF-NBR-DTL-LINES              ELTEMERG
02966          MOVE +1   TO  WS-CIA                                     ELTEMERG
02967          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTEMERG
02968                         COMMAREA (DFHCOMMAREA)                    ELTEMERG
02969                         END-EXEC.                                 ELTEMERG
02970 **                                                               |ELTEMERG
02971 **---------------------------------------------------------------+ELTEMERG
02972                                                                   ELTEMERG
02973 **---------------------------------------------------------------+ELTEMERG
02974 **                                                               |ELTEMERG
02975 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTEMERG
02976      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
02977      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02978         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
02979                                                       NOT =  '0'  ELTEMERG
02980         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02981         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02982         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTEMERG
02983                                           CMF-ELEMENT-SYSTEM-NAME ELTEMERG
02984         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTEMERG
02985                                           TO   CMF-CODE-VALUE     ELTEMERG
02986         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
02987         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
02988 **                                                               |ELTEMERG
02989 **---------------------------------------------------------------+ELTEMERG
02990                                                                   ELTEMERG
02991 **---------------------------------------------------------------+ELTEMERG
02992 **                                                               |ELTEMERG
02993 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTEMERG
02994      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
02995         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTEMERG
02996                                                       NOT =  '0'  ELTEMERG
02997         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
02998         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
02999         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTEMERG
03000         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTEMERG
03001                                                     CMF-CODE-VALUEELTEMERG
03002         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03003         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03004                                                                   ELTEMERG
03005      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03006         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03007         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03008         MOVE 1  TO  WS-CIA                                        ELTEMERG
03009         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03010             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03011         END-EXEC.                                                 ELTEMERG
03012 **                                                               |ELTEMERG
03013 **---------------------------------------------------------------+ELTEMERG
03014                                                                   ELTEMERG
03015                                                                   ELTEMERG
03016      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03017         SET PLT-INDEX2  TO  2                                     ELTEMERG
03018      ELSE                                                         ELTEMERG
03019         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03020                                                                   ELTEMERG
03021                                                                   ELTEMERG
03022 **---------------------------------------------------------------+ELTEMERG
03023 **                                                               |ELTEMERG
03024 **         G E N E R A L   T A B U L A R   R T N E               |ELTEMERG
03025      PERFORM 2600-GENERAL-TABULAR-RTNE.                           ELTEMERG
03026      PERFORM 4675-PAY-CONSID-TEXT.                                ELTEMERG
03027      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTEMERG
03028 **                                                               |ELTEMERG
03029 **---------------------------------------------------------------+ELTEMERG
03030                                                                   ELTEMERG
03031  3050-ZERO-ALL-WITH-SAME-NO.                                      ELTEMERG
03032                                                                   ELTEMERG
03033      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEMERG
03034         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03035         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEMERG
03036         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEMERG
03037                                                   CMF-CODE-VALUE  ELTEMERG
03038         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEMERG
03039         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03040         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
03041         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEMERG
03042         IF WS-CIA  >  20 OR  =  20                                ELTEMERG
03043            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEMERG
03044            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEMERG
03045                COMMAREA(DFHCOMMAREA)                              ELTEMERG
03046            END-EXEC                                               ELTEMERG
03047            MOVE +1  TO  WS-CIA.                                   ELTEMERG
03048                                                                   ELTEMERG
03049  3090-PROBLEM-WITH-INDICES.                                       ELTEMERG
03050                                                                   ELTEMERG
03051      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEMERG
03052      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
03053                                                                   ELTEMERG
03054      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTEMERG
03055      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
03056                                                                   ELTEMERG
03057      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
03058      END-EXEC.                                                    ELTEMERG
03059                                                                   ELTEMERG
03060  3099-EXIT.           EXIT.                                       ELTEMERG
03061                                                                   ELTEMERG
03062 /            P R O F E S S I O N A L   M E D   R T N E            ELTEMERG
03063 ***************************************************************** ELTEMERG
03064 *            P R O F E S S I O N A L   M E D   R T N E            ELTEMERG
03065 *                                                                 ELTEMERG
03066 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTEMERG
03067 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTEMERG
03068 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTEMERG
03069 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTEMERG
03070 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTEMERG
03071 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTEMERG
03072 *  MODULE.                                                        ELTEMERG
03073 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTEMERG
03074 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTEMERG
03075 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTEMERG
03076 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTEMERG
03077 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTEMERG
03078 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTEMERG
03079 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTEMERG
03080 *                                                                 ELTEMERG
03081 ***************************************************************** ELTEMERG
03082  4000-PROFESSIONAL-MED-RTNE SECTION.                              ELTEMERG
03083      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTEMERG
03084      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
03085      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTEMERG
03086                     COF-NBR-DTL-LINES.                            ELTEMERG
03087      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
03088      END-EXEC.                                                    ELTEMERG
03089      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTEMERG
03090      MOVE WS-HDR-2-PROF-MED  TO  COF-HDR-LINE(2).                 ELTEMERG
03091                                                                   ELTEMERG
03092      MOVE WS-PROF-MED-CNT  TO  PVN-NBR-BEN-PROVN.                 ELTEMERG
03093      PERFORM 4010-MOVE-IN-PROF-MED                                ELTEMERG
03094         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTEMERG
03095         UNTIL WS-SUB  >  WS-PROF-MED-CNT.                         ELTEMERG
03096                                                                   ELTEMERG
03097      GO TO 4020-CALL-COVERAGE.                                    ELTEMERG
03098  4010-MOVE-IN-PROF-MED.                                           ELTEMERG
03099      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTEMERG
03100      MOVE WS-PROF-MED-LIST(WS-SUB)  TO                            ELTEMERG
03101                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTEMERG
03102      MOVE ZERO  TO  PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),          ELTEMERG
03103                     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).          ELTEMERG
03104                                                                   ELTEMERG
03105  4020-CALL-COVERAGE.                                              ELTEMERG
03106      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
03107      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
03108      END-EXEC.                                                    ELTEMERG
03109                                                                   ELTEMERG
03110      MOVE WS-STD-MEDICAL-SERVICES  TO  SSB-TOPIC-PHRASE.          ELTEMERG
03111                                                                   ELTEMERG
03112      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTEMERG
03113      END-EXEC.                                                    ELTEMERG
03114                                                                   ELTEMERG
03115      ADD +1  TO   COF-NBR-DTL-LINES.                              ELTEMERG
03116      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
03117      END-EXEC.                                                    ELTEMERG
03118                                                                   ELTEMERG
03119      IF PVN-COVG-NONE                                             ELTEMERG
03120         GO TO 4099-EXIT.                                          ELTEMERG
03121                                                                   ELTEMERG
03122      MOVE +1  TO  WS-CIA.                                         ELTEMERG
03123                                                                   ELTEMERG
03124      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTEMERG
03125      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTEMERG
03126            PSP-PROVN-PRICING-METHD,                               ELTEMERG
03127            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTEMERG
03128            PSP-TRANSF-OTHER-RESP-IND,                             ELTEMERG
03129            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTEMERG
03130            PSP-SPILL-OVER-COINS-APL-IND,                          ELTEMERG
03131            PSP-SPILL-OVER-DED-APL-IND,                            ELTEMERG
03132            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTEMERG
03133            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTEMERG
03134            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTEMERG
03135            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTEMERG
03136            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTEMERG
03137            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTEMERG
03138            PSP-TRAUM-INJ-EFF-DT-COMP-IND,                         ELTEMERG
03139            PSE-BEN-SCOPE-ID,                                      ELTEMERG
03140            PSE-MAX-AMT-PER-VISIT.                                 ELTEMERG
03141                                                                   ELTEMERG
03142      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTEMERG
03143      END-EXEC.                                                    ELTEMERG
03144                                                                   ELTEMERG
03145      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTEMERG
03146      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTEMERG
03147                      ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.          ELTEMERG
03148                                                                   ELTEMERG
03149                                                                   ELTEMERG
03150      PERFORM 4030-FIND-FIRST-NONZERO                              ELTEMERG
03151         VARYING WS-SUB  FROM  +1  BY  +1                          ELTEMERG
03152         UNTIL WS-SUB  >  WS-PROF-MED-CNT.                         ELTEMERG
03153                                                                   ELTEMERG
03154      GO TO 4099-EXIT.                                             ELTEMERG
03155  4030-FIND-FIRST-NONZERO.                                         ELTEMERG
03156      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
03157      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTEMERG
03158         NEXT SENTENCE                                             ELTEMERG
03159      ELSE                                                         ELTEMERG
03160         PERFORM 4040-BUILD-SCREEN-LINES.                          ELTEMERG
03161                                                                   ELTEMERG
03162  4040-BUILD-SCREEN-LINES.                                         ELTEMERG
03163                                                                   ELTEMERG
03164      SET PLT-INDEX1   TO                                          ELTEMERG
03165                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTEMERG
03166      IF WS-NOT-FIRST-TIME                                         ELTEMERG
03167         MOVE 'P'  TO  COF-FUNCTION                                ELTEMERG
03168         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03169             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03170         END-EXEC                                                  ELTEMERG
03171      ELSE                                                         ELTEMERG
03172         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTEMERG
03173                                                                   ELTEMERG
03174      MOVE +1  TO  WS-CIA.                                         ELTEMERG
03175      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03176         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTEMERG
03177            SET PLT-INDEX2  TO  2                                  ELTEMERG
03178         ELSE                                                      ELTEMERG
03179            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTEMERG
03180            GO TO 4099-EXIT                                        ELTEMERG
03181      ELSE                                                         ELTEMERG
03182         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03183                                                                   ELTEMERG
03184 **---------------------------------------------------------------+ELTEMERG
03185 **                                                               |ELTEMERG
03186 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTEMERG
03187      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTEMERG
03188      ADD  +1  TO  WS-CIA.                                         ELTEMERG
03189      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTEMERG
03190      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTEMERG
03191         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTEMERG
03192         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-MED-CNT.               ELTEMERG
03193      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTEMERG
03194                                                                   ELTEMERG
03195      ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTEMERG
03196      MOVE 1  TO  WS-CIA.                                          ELTEMERG
03197      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTEMERG
03198             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03199      END-EXEC.                                                    ELTEMERG
03200 **                                                               |ELTEMERG
03201 **---------------------------------------------------------------+ELTEMERG
03202                                                                   ELTEMERG
03203                                                                   ELTEMERG
03204 **---------------------------------------------------------------+ELTEMERG
03205 **                                                               |ELTEMERG
03206 **    I L L N E S S   /   I N J U R Y   E F F E C T I V E        |ELTEMERG
03207 **     D A T E   C O M P A R I S O N   I N D I C A T O R         |ELTEMERG
03208      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03209      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03210         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
03211                                                        NOT =  ZEROELTEMERG
03212         MOVE WS-COMPARISON-ILLNESS  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
03213         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03214         ADD +1  TO  WS-CIA                                        ELTEMERG
03215      ELSE                                                         ELTEMERG
03216         SET  PLT-INDEX2  TO  2                                    ELTEMERG
03217         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTEMERG
03218            PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
03219                                                       NOT =  ZERO ELTEMERG
03220            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
03221            MOVE WS-COMPARISON-ILLNESS  TO  COF-DTL-LINE(WS-CIA)   ELTEMERG
03222            ADD +1  TO  WS-CIA.                                    ELTEMERG
03223                                                                   ELTEMERG
03224      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03225      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03226         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
03227                                                        NOT =  ZEROELTEMERG
03228         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03229         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
03230                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
03231         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03232                                               TO   CMF-CODE-VALUE ELTEMERG
03233         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03234         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03235         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03236                                                                   ELTEMERG
03237      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
03238      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
03239         PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
03240                                                        NOT =  ZEROELTEMERG
03241         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03242         MOVE 'TRAUM-INJ-EFF-DT-COMP-IND'  TO                      ELTEMERG
03243                                            CMF-ELEMENT-SYSTEM-NAMEELTEMERG
03244         MOVE PLP-TRAUM-INJ-EFF-DT-COMP-IND(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03245                                               TO   CMF-CODE-VALUE ELTEMERG
03246         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
03247         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03248         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03249                                                                   ELTEMERG
03250      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03251         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03252         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03253         MOVE +1  TO  WS-CIA                                       ELTEMERG
03254         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03255               COMMAREA(DFHCOMMAREA)                               ELTEMERG
03256         END-EXEC.                                                 ELTEMERG
03257 **                                                               |ELTEMERG
03258 **---------------------------------------------------------------+ELTEMERG
03259                                                                   ELTEMERG
03260 **---------------------------------------------------------------+ELTEMERG
03261 **                                                               |ELTEMERG
03262 **                    L I M I T   O N                            |ELTEMERG
03263 **          R E C E P T I O N   O F   T R E A T M E N T          |ELTEMERG
03264 **                         A N D                                 |ELTEMERG
03265 **         E X P L A N A T I O N   O F   P R O C E D U R E       |ELTEMERG
03266 **          W H E N   B E Y O N D   T H A T   L I M I T          |ELTEMERG
03267      PERFORM 2300-DAYS-OF-PROF-TREATMENT.                         ELTEMERG
03268 **                                                               |ELTEMERG
03269 **---------------------------------------------------------------+ELTEMERG
03270                                                                   ELTEMERG
03271      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03272         SET PLT-INDEX2  TO  2                                     ELTEMERG
03273      ELSE                                                         ELTEMERG
03274         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03275                                                                   ELTEMERG
03276 **---------------------------------------------------------------+ELTEMERG
03277 **                                                               |ELTEMERG
03278 **        P L A C E   O F   T R E A T M E N T                    |ELTEMERG
03279      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
03280                                                              ZERO ELTEMERG
03281         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03282         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTEMERG
03283         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)     ELTEMERG
03284                                               TO  CMF-CODE-VALUE  ELTEMERG
03285         MOVE +54  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03286         MOVE WS-SERVICES-RENDERED  TO  WS-TEMP-TEXT-AREA          ELTEMERG
03287         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
03288         ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03289         MOVE +1  TO  WS-CIA                                       ELTEMERG
03290         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03291               COMMAREA(DFHCOMMAREA)                               ELTEMERG
03292         END-EXEC.                                                 ELTEMERG
03293 **                                                               |ELTEMERG
03294 **---------------------------------------------------------------+ELTEMERG
03295                                                                   ELTEMERG
03296      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03297         SET PLT-INDEX2  TO  2                                     ELTEMERG
03298      ELSE                                                         ELTEMERG
03299         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03300                                                                   ELTEMERG
03301 **---------------------------------------------------------------+ELTEMERG
03302 **                                                               |ELTEMERG
03303 **            B E N E F I T   S C O P E   I D                    |ELTEMERG
03304      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03305      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03306         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
03307                                       '0000' AND  NOT =  '00  '   ELTEMERG
03308         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
03309         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03310         ADD  +1  TO  WS-CIA.                                      ELTEMERG
03311                                                                   ELTEMERG
03312      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
03313      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
03314         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
03315                                   '0000' AND  NOT =  '00  ' AND   ELTEMERG
03316         NOT WS-ADD-A-BLANK-LINE                                   ELTEMERG
03317         MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
03318         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03319         ADD  +1  TO  WS-CIA.                                      ELTEMERG
03320                                                                   ELTEMERG
03321      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03322      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03323         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
03324                                       '0000' AND  NOT =  '00  '   ELTEMERG
03325         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTEMERG
03326         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTEMERG
03327         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTEMERG
03328                                                    CMF-CODE-VALUE ELTEMERG
03329         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03330         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03331         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03332                                                                   ELTEMERG
03333      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
03334      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
03335         PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =           ELTEMERG
03336                                       '0000' AND  NOT =  '00  '   ELTEMERG
03337         MOVE 'BPE'  TO  CMF-RECORD-PREFIX                         ELTEMERG
03338         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTEMERG
03339         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTEMERG
03340                                                    CMF-CODE-VALUE ELTEMERG
03341         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
03342         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03343         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03344                                                                   ELTEMERG
03345      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03346         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03347         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03348         MOVE 1  TO  WS-CIA                                        ELTEMERG
03349         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03350                COMMAREA(DFHCOMMAREA)                              ELTEMERG
03351         END-EXEC.                                                 ELTEMERG
03352 **                                                               |ELTEMERG
03353 **---------------------------------------------------------------+ELTEMERG
03354                                                                   ELTEMERG
03355      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03356         SET PLT-INDEX2  TO  2                                     ELTEMERG
03357      ELSE                                                         ELTEMERG
03358         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03359                                                                   ELTEMERG
03360 **---------------------------------------------------------------+ELTEMERG
03361 **                                                               |ELTEMERG
03362 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTEMERG
03363 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTEMERG
03364 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTEMERG
03365      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03366      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03367         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
03368                                           ZERO AND  NOT =  '19'   ELTEMERG
03369         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
03370         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03371         ADD +1  TO  WS-CIA.                                       ELTEMERG
03372                                                                   ELTEMERG
03373      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
03374      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
03375         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
03376                                        ZERO AND  NOT =  '19' AND  ELTEMERG
03377         NOT WS-ADD-A-BLANK-LINE                                   ELTEMERG
03378         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTEMERG
03379         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03380         ADD +1  TO  WS-CIA.                                       ELTEMERG
03381                                                                   ELTEMERG
03382      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03383      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03384         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
03385                             AND                                   ELTEMERG
03386         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
03387         SET  PLT-INDEX2  TO  2                                    ELTEMERG
03388         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
03389                                                              ZERO ELTEMERG
03390            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
03391            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
03392            ADD +1  TO  WS-CIA.                                    ELTEMERG
03393                                                                   ELTEMERG
03394      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03395      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03396         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTEMERG
03397                               AND                                 ELTEMERG
03398         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      =  ZERO          ELTEMERG
03399         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTEMERG
03400         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTEMERG
03401         ADD +1  TO  WS-CIA.                                       ELTEMERG
03402                                                                   ELTEMERG
03403      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTEMERG
03404         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
03405         SET  PLT-INDEX2  TO  2                                    ELTEMERG
03406         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTEMERG
03407                                                              ZERO ELTEMERG
03408            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTEMERG
03409            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTEMERG
03410            ADD +1  TO  WS-CIA.                                    ELTEMERG
03411                                                                   ELTEMERG
03412      SET  PLT-INDEX2  TO  1.                                      ELTEMERG
03413      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
03414         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTEMERG
03415                                                            =  ZEROELTEMERG
03416            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03417                                                            =  ZEROELTEMERG
03418               MOVE SPACE  TO  WS-PERCENT-FLD                      ELTEMERG
03419            ELSE                                                   ELTEMERG
03420               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
03421          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03422                                                  TO  WS-PERCENTAGEELTEMERG
03423         ELSE                                                      ELTEMERG
03424            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
03425          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03426                                                 TO  WS-PERCENTAGE.ELTEMERG
03427      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTEMERG
03428         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
03429                                                              ZERO ELTEMERG
03430         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03431         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
03432         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
03433                                                    CMF-CODE-VALUE ELTEMERG
03434         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03435         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03436         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
03437                                                                   ELTEMERG
03438      SET  PLT-INDEX2  TO  2.                                      ELTEMERG
03439      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
03440         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTEMERG
03441                                                               ZEROELTEMERG
03442            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03443                                                            =  ZEROELTEMERG
03444               MOVE SPACE  TO  WS-PERCENT-FLD                      ELTEMERG
03445            ELSE                                                   ELTEMERG
03446               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTEMERG
03447          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03448                                                  TO  WS-PERCENTAGEELTEMERG
03449         ELSE                                                      ELTEMERG
03450            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTEMERG
03451          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTEMERG
03452                                                 TO  WS-PERCENTAGE.ELTEMERG
03453      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTEMERG
03454         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTEMERG
03455                                                              ZERO ELTEMERG
03456         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03457         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTEMERG
03458         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTEMERG
03459                                                    CMF-CODE-VALUE ELTEMERG
03460         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTEMERG
03461         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03462         PERFORM 2800-CODE-MANUAL-WITH-PERCENT.                    ELTEMERG
03463                                                                   ELTEMERG
03464      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03465         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03466         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03467         MOVE 1  TO  WS-CIA                                        ELTEMERG
03468         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03469                COMMAREA(DFHCOMMAREA)                              ELTEMERG
03470         END-EXEC.                                                 ELTEMERG
03471 **                                                               |ELTEMERG
03472 **---------------------------------------------------------------+ELTEMERG
03473                                                                   ELTEMERG
03474 **---------------------------------------------------------------+ELTEMERG
03475 **                                                               |ELTEMERG
03476 **       M A X I M U M   A M O U N T   P E R   V I S I T         |ELTEMERG
03477      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTEMERG
03478         SET  PLT-INDEX2  TO  1                                    ELTEMERG
03479         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
03480                                                              ZERO ELTEMERG
03481            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
03482            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
03483            ADD +1  TO  WS-CIA                                     ELTEMERG
03484            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
03485                                                  WS-BASIC-MAX-AMT ELTEMERG
03486            MOVE WS-BASIC-MAX  TO  COF-DTL-LINE(WS-CIA)            ELTEMERG
03487            ADD +1  TO  WS-CIA.                                    ELTEMERG
03488                                                                   ELTEMERG
03489      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTEMERG
03490         SET  PLT-INDEX2  TO  2                                    ELTEMERG
03491         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTEMERG
03492                                                          ZERO AND ELTEMERG
03493            NOT WS-ADD-A-BLANK-LINE                                ELTEMERG
03494            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTEMERG
03495            MOVE WS-MAXIMUM-AMT-PER  TO  COF-DTL-LINE(WS-CIA)      ELTEMERG
03496            ADD +1  TO  WS-CIA                                     ELTEMERG
03497            MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  TO ELTEMERG
03498                                                   WS-SUPP-MAX-AMT ELTEMERG
03499            MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)             ELTEMERG
03500            ADD +1  TO  WS-CIA                                     ELTEMERG
03501         ELSE                                                      ELTEMERG
03502            IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  NOT =ELTEMERG
03503                                                              ZERO ELTEMERG
03504               MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)  ELTEMERG
03505                                               TO  WS-SUPP-MAX-AMT ELTEMERG
03506               MOVE WS-SUPP-MAX  TO  COF-DTL-LINE(WS-CIA)          ELTEMERG
03507               ADD +1  TO  WS-CIA.                                 ELTEMERG
03508                                                                   ELTEMERG
03509      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03510         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03511         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03512         MOVE 1  TO  WS-CIA                                        ELTEMERG
03513         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03514             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03515         END-EXEC.                                                 ELTEMERG
03516 **                                                               |ELTEMERG
03517 **---------------------------------------------------------------+ELTEMERG
03518                                                                   ELTEMERG
03519 **---------------------------------------------------------------+ELTEMERG
03520 **                                                               |ELTEMERG
03521 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTEMERG
03522      SET PLT-INDEX2  TO  2.                                       ELTEMERG
03523      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTEMERG
03524         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTEMERG
03525                                                         NOT =  '0'ELTEMERG
03526         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03527         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03528         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTEMERG
03529                                           CMF-ELEMENT-SYSTEM-NAME ELTEMERG
03530         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTEMERG
03531                                                TO  CMF-CODE-VALUE ELTEMERG
03532         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03533         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03534 **                                                               |ELTEMERG
03535 **---------------------------------------------------------------+ELTEMERG
03536                                                                   ELTEMERG
03537 **---------------------------------------------------------------+ELTEMERG
03538 **                                                               |ELTEMERG
03539 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTEMERG
03540      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES AND ELTEMERG
03541         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTEMERG
03542                                                         NOT =  '0'ELTEMERG
03543         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03544         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03545         MOVE 'SPILL-OVER-DED-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAMEELTEMERG
03546         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTEMERG
03547                                                 TO  CMF-CODE-VALUEELTEMERG
03548         MOVE WS-SPILLOVER  TO  WS-TEMP-TEXT-AREA                  ELTEMERG
03549         PERFORM 2100-CODES-MANUAL-LONG.                           ELTEMERG
03550                                                                   ELTEMERG
03551      IF WS-ADD-A-BLANK-LINE                                       ELTEMERG
03552         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTEMERG
03553         ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES                ELTEMERG
03554         MOVE 1  TO  WS-CIA                                        ELTEMERG
03555         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03556             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03557         END-EXEC.                                                 ELTEMERG
03558 **                                                               |ELTEMERG
03559 **---------------------------------------------------------------+ELTEMERG
03560                                                                   ELTEMERG
03561                                                                   ELTEMERG
03562      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03563         SET PLT-INDEX2  TO  2                                     ELTEMERG
03564      ELSE                                                         ELTEMERG
03565         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03566                                                                   ELTEMERG
03567 **---------------------------------------------------------------+ELTEMERG
03568 **                                                               |ELTEMERG
03569 **         G E N E R A L   T A B U L A R   R T N E               |ELTEMERG
03570      PERFORM 2600-GENERAL-TABULAR-RTNE.                           ELTEMERG
03571      PERFORM 4675-PAY-CONSID-TEXT.                                ELTEMERG
03572      PERFORM 4685-TRANSF-OTHER-RESPON-IND.                        ELTEMERG
03573 **                                                               |ELTEMERG
03574 **---------------------------------------------------------------+ELTEMERG
03575                                                                   ELTEMERG
03576                                                                   ELTEMERG
03577  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTEMERG
03578                                                                   ELTEMERG
03579      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTEMERG
03580         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTEMERG
03581         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTEMERG
03582         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTEMERG
03583                                                   CMF-CODE-VALUE  ELTEMERG
03584         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTEMERG
03585         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTEMERG
03586         PERFORM 2100-CODES-MANUAL-LONG                            ELTEMERG
03587         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTEMERG
03588         IF WS-CIA  >  20 OR  =  20                                ELTEMERG
03589            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTEMERG
03590            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTEMERG
03591                COMMAREA(DFHCOMMAREA)                              ELTEMERG
03592            END-EXEC                                               ELTEMERG
03593            MOVE +1  TO  WS-CIA.                                   ELTEMERG
03594                                                                   ELTEMERG
03595  4090-PROBLEM-WITH-INDICES.                                       ELTEMERG
03596                                                                   ELTEMERG
03597      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTEMERG
03598      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTEMERG
03599                                                                   ELTEMERG
03600      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTEMERG
03601      MOVE 'P'  TO  COF-FUNCTION.                                  ELTEMERG
03602                                                                   ELTEMERG
03603      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTEMERG
03604      END-EXEC.                                                    ELTEMERG
03605                                                                   ELTEMERG
03606  4099-EXIT.           EXIT.                                       ELTEMERG
03607 /                                                                 ELTEMERG
03608  4675-PAY-CONSID-TEXT SECTION.                                    ELTEMERG
03609      INITIALIZE TCAR-FROM-AREA.                                   ELTEMERG
03610      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTEMERG
03611             WS-PAY-CONSDR-TEXT2                                   ELTEMERG
03612                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTEMERG
03613      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTEMERG
03614      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTEMERG
03615      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTEMERG
03616                                TCAR-OUTPUT-FIELD-2-LEN.           ELTEMERG
03617      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTEMERG
03618      IF WS-CIA > 17                                               ELTEMERG
03619            PERFORM 8000-OUTPUT-TEXT                               ELTEMERG
03620            MOVE +1            TO WS-CIA.                          ELTEMERG
03621      ADD +1                TO  WS-CIA.                            ELTEMERG
03622      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTEMERG
03623      ADD +1                TO  WS-CIA.                            ELTEMERG
03624      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTEMERG
03625      PERFORM 8000-OUTPUT-TEXT.                                    ELTEMERG
03626  4675-EXIT.   EXIT.                                               ELTEMERG
03627                                                                   ELTEMERG
03628  4685-TRANSF-OTHER-RESPON-IND SECTION.                            ELTEMERG
03629      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTEMERG
03630         SET PLT-INDEX2  TO  2                                     ELTEMERG
03631      ELSE                                                         ELTEMERG
03632         SET PLT-INDEX2  TO  1.                                    ELTEMERG
03633                                                                   ELTEMERG
03634      IF PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2) = ZERO  ELTEMERG
03635         GO TO 4685-EXIT.                                          ELTEMERG
03636                                                                   ELTEMERG
03637      MOVE 'BP'                     TO  CMF-RECORD-PREFIX.         ELTEMERG
03638      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTEMERG
03639      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTEMERG
03640           TO  CMF-CODE-VALUE.                                     ELTEMERG
03641       MOVE +0                      TO  WS-TEMP-NOT-USED-CNT.      ELTEMERG
03642       MOVE SPACES                  TO  WS-TEMP-TEXT-AREA.         ELTEMERG
03643       PERFORM 2100-CODES-MANUAL-LONG.                             ELTEMERG
03644       ADD +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTEMERG
03645       MOVE +1  TO  WS-CIA.                                        ELTEMERG
03646       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTEMERG
03647               COMMAREA(DFHCOMMAREA)                               ELTEMERG
03648         END-EXEC.                                                 ELTEMERG
03649  4685-EXIT.   EXIT.                                               ELTEMERG
03650 /                                                                 ELTEMERG
03651  8000-OUTPUT-TEXT SECTION.                                        ELTEMERG
03652      MOVE +0     TO COF-NBR-HDR-LINES.                            ELTEMERG
03653      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTEMERG
03654      MOVE ' '    TO COF-FUNCTION.                                 ELTEMERG
03655         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTEMERG
03656             COMMAREA(DFHCOMMAREA)                                 ELTEMERG
03657         END-EXEC.                                                 ELTEMERG
03658      MOVE 1 TO WS-CIA.                                            ELTEMERG
03659  8099-EXIT.  EXIT.                                                ELTEMERG
03660 / C O M P R E S S   A N D   E X P A N D   S U B R O U T I N E S   ELTEMERG
03661  9999-DUMMEY   SECTION.                                           ELTEMERG
03662      COPY ELSTCOMP.                                               ELTEMERG
03663                                                                   ELTEMERG
