00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTSUBST.                                            ELTSUBST
00003  AUTHOR. ALIDA JATICH OF T. M. FLOYD, INC.                           LV001
00004  DATE-WRITTEN. MAY 20, 1986.                                      ELTSUBST
00005  DATE-COMPILED.                                                   ELTSUBST
00006 ******************************************************************ELTSUBST
00007 **                                                              **ELTSUBST
00008 **                                                              **ELTSUBST
00009 **                       PROGRAM ABSTRACT                       **ELTSUBST
00010 **                                                              **ELTSUBST
00011 **  PROGRAM NAME: E.L.S. SUBSTANCE ABUSE TOPIC                  **ELTSUBST
00012 **                                                              **ELTSUBST
00013 **  PROGRAM I.D.: ELTSUBST                                      **ELTSUBST
00014 **                                                              **ELTSUBST
00015 **  PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING           **ELTSUBST
00016 **             BENEFIT COVERAGE FOR SUBSTANCE ABUSE.            **ELTSUBST
00017 **                                                              **ELTSUBST
00018 **  OVERVIEW:  THE PROGRAM DISPLAYS THE SUBSTANCE ABUSE         **ELTSUBST
00019 **             COVERAGE AFFORDED A PATIENT BY HIS GROUP.        **ELTSUBST
00020 **             THE REQUIRED INFORMATION IS OBTAINED BY          **ELTSUBST
00021 **             INTERROGATING THE BENEFIT PROVISIONS FOR         **ELTSUBST
00022 **             THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR   **ELTSUBST
00023 **             RANGE OF DATES.                                  **ELTSUBST
00024 **                                                              **ELTSUBST
00025 **  RECORDS                                                     **ELTSUBST
00026 **  ACCESSED:  GROUP SPECIFIC, CONTRACT, VARIOUS BENEFIT        **ELTSUBST
00027 **             PROVISION AND TABULAR RECORDS,                   **ELTSUBST
00028 **             AND MANY DATA ELEMENT AND CODE VALUE RECORDS.    **ELTSUBST
00029 **                                                              **ELTSUBST
00030 **                                                              **ELTSUBST
00031 **  PROCESSING                                                  **ELTSUBST
00032 **  FUNCTIONS:                                                  **ELTSUBST
00033 **                                                              **ELTSUBST
00034 **                                                              **ELTSUBST
00035 ******************************************************************ELTSUBST
00036 ** COMMENT REGARDING PROGRAM ARCHITECTURE:                      **ELTSUBST
00037 **                                                              **ELTSUBST
00038 ** SOME OF THE INSTITUTIONAL LOGIC WAS CLONED FROM THE ROOM     **ELTSUBST
00039 ** AND BOARD TOPIC PROGRAM, ELTROOMB.  SOME OF THE PROFESSIONAL **ELTSUBST
00040 ** LOGIC WAS CLONED FROM THE SURGERY TOPIC PROGRAM, ELTSURGR.   **ELTSUBST
00041 **                                                              **ELTSUBST
00042 ** ALTHOUGH SOME INSTITUTIONAL AND PROFESSIONAL DISPLAY LINES   **ELTSUBST
00043 ** ARE GENERATED USING SIMILAR LOGIC, SEPARATE ROUTINES ARE     **ELTSUBST
00044 ** USED TO GENERATE MOST OF THE DETAIL LINES FOR PROFESSIONAL   **ELTSUBST
00045 ** AND FOR INSTITUTIONAL.  THIS IS BECAUSE ELTROOMB AND         **ELTSUBST
00046 ** ELTSURGR, FROM WHICH THE LOGIC WAS COPIED, HAVE DIFFERENT    **ELTSUBST
00047 ** ARCHITECTURES AND USE DIFFERENT CONVENTIONS FOR DECIDING HOW **ELTSUBST
00048 ** TO SET UP THE CIA AND WHEN TO LINK TO ELUOUTPT.              **ELTSUBST
00049 **                                                              **ELTSUBST
00050 ******************************************************************ELTSUBST
00051 *                                                                *ELTSUBST
00052 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTSUBST
00053 *       *-*         U P D A T E   H I S T O R Y         *-*      *ELTSUBST
00054 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*      *ELTSUBST
00055 *                                                                *ELTSUBST
00056 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION----------------*ELTSUBST
00057 *                                                                *ELTSUBST
00058 *    XXXX    05/20/86  AMJ  ORIGINAL MODULE WRITTEN              *ELTSUBST
00059 *    0150    06/05/86  AMJ  CHANGED TO DISPLAY CONTRACT FIELD    *ELTSUBST
00060 *                           ONLY WHEN BEN PROV EXISTS FOR THAT   *ELTSUBST
00061 *                           LINE OF BUSINESS.                    *ELTSUBST
00062 *    XXXX    06/18/86  AMJ  FIXED TO SUPPRESS OUTPUT IF          *ELTSUBST
00063 *                           PROVISIONAL PRICING METHOD = 19      *ELTSUBST
00064 *    XXXX    07/24/86  JTC  CHANGED THE PICTURE OF               *ELTSUBST
00065 *                           WS-DTL-BAS-NUM-3 FROM PIC 999 TO     *ELTSUBST
00066 *                                                 PIC ZZ9.99-    *ELTSUBST
00067 *                           CHANGED THE PICTURE OF               *ELTSUBST
00068 *                           WS-DTL-SUP-NUM-3 FROM PIC 999 TO     *ELTSUBST
00069 *                                                 PIC ZZ9.99-    *ELTSUBST
00070 *    T0723   08/14/86  LET  USING THE 1ST HEADER LINE FROM THE   *ELTSUBST
00071 *                           PROLOG ON THE TOPIC SCREENS.         *ELTSUBST
00072 *    XXXX    10/08/86  NAC  VS COBOL II CONVERSION.              *ELTSUBST
00073 *    XXXX    10/21/87  EGL  CHANGED FIXED TEXT.                  *ELTSUBST
00074 *    XXXX    04/06/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.     *ELTSUBST
00075 *    XXXX    10/30/89  RKH  ADDED TRANSF TO OTHER RESP IND.      *ELTSUBST
00076 *    XXXX    10/15/90  GEM  ADDED BENEFIT PROVISION IDS.         *ELTSUBST
00077 *    94-007  02/18/94  JPB  ADDED NETWORK UTILIZATION REVIEW IND *ELTSUBST
00078 ******************************************************************ELTSUBST
00079 /                                                                 ELTSUBST
00080  ENVIRONMENT DIVISION.                                            ELTSUBST
00081      SKIP3                                                        ELTSUBST
00082  DATA DIVISION.                                                   ELTSUBST
00083  WORKING-STORAGE SECTION.                                         ELTSUBST
00084  01  WS-BEGIN                    PIC X(24) VALUE                  ELTSUBST
00085      '***ELTSUBST WS BEGINS***'.                                  ELTSUBST
00086 /                                                                 ELTSUBST
00087 ******************************************************************ELTSUBST
00088 ** WORKFIELDS AND SWITCHES                                      **ELTSUBST
00089 ******************************************************************ELTSUBST
00090  01  WS-WORK-FIELDS.                                              ELTSUBST
00091      05  WS-HEX-00                     PIC X.                     ELTSUBST
00092      05  WS-CHAR-0                     PIC X.                     ELTSUBST
00093      05  WS-SUB                        PIC S999  COMP VALUE +0.   ELTSUBST
00094      05  WS-SUB1                       PIC S999  COMP VALUE +0.   ELTSUBST
00095      05  WS-SUB2                       PIC S999  COMP VALUE +0.   ELTSUBST
00096      05  WS-SUB3                       PIC S999  COMP VALUE +0.   ELTSUBST
00097      05  WS-SUB4                       PIC S999  COMP VALUE +0.   ELTSUBST
00098      05  WS-CIA                        PIC S999  COMP VALUE +0.   ELTSUBST
00099      05  WS-FIRSTTIME-IND              PIC X     VALUE SPACES.    ELTSUBST
00100          88  WS-NOT-FIRST-TIME                   VALUE 'N'.       ELTSUBST
00101      05  WS-ADD-A-BLANK-IND            PIC X     VALUE SPACES.    ELTSUBST
00102          88  WS-ADD-A-BLANK-LINE                 VALUE 'Y'.       ELTSUBST
00103      05  WS-MOVE-LINES-IND             PIC X     VALUE 'Y'.       ELTSUBST
00104          88  WS-MOVE-LINES-TO-CIA                VALUE 'Y'.       ELTSUBST
00105      05  WS-NEW-LINES-GENERATED-IND    PIC X     VALUE 'N'.       ELTSUBST
00106          88  WS-NEW-LINES-GENERATED              VALUE 'Y'.       ELTSUBST
00107                                                                   ELTSUBST
00108      05  WS-BASIC-OR-CMM-IND           PIC X     VALUE 'N'.       ELTSUBST
00109          88  WS-BASIC-EXISSS                     VALUE 'B'.       ELTSUBST
00110          88  WS-CMM-EXISSS                       VALUE 'C'.       ELTSUBST
00111                                                                   ELTSUBST
00112      05  WS-BS-IND                     PIC X     VALUE 'N'.       ELTSUBST
00113          88  WS-BS-EXISSS                        VALUE 'B'.       ELTSUBST
00114                                                                   ELTSUBST
00115      05  WS-SMM-IND                    PIC X     VALUE 'N'.       ELTSUBST
00116          88  WS-SMM-EXISSS                       VALUE 'S'.       ELTSUBST
00117                                                                   ELTSUBST
00118      05  WS-NETWORK-UTIL-IND           PIC XX.                    ELTSUBST
00119          88  WS-VALID-INST-INPATIENT     VALUE '01' '02' '03'     ELTSUBST
00120                                                '05' '06' '08'.    ELTSUBST
00121          88  WS-VALID-INST-OUTPATIENT    VALUE '01' '02'          ELTSUBST
00122                                                '06' '08'.         ELTSUBST
00123          88  WS-VALID-PROF-INPATIENT     VALUE '01' '02' '04'     ELTSUBST
00124                                                '05' '07' '08'.    ELTSUBST
00125                                                                   ELTSUBST
00126      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP.             ELTSUBST
00127      05  WS-PERCENT-FLD.                                          ELTSUBST
00128        10  WS-PERCENTAGE               PIC ZZ9.                   ELTSUBST
00129        10  WS-PERCENT-SIGN             PIC X.                     ELTSUBST
00130      05  WS-HOLD1                      PIC X(10).                 ELTSUBST
00131      05  WS-HOLD2                      PIC X(10).                 ELTSUBST
00132      05  WS-DAYS-RDCN-RAT-IND          PIC X.                     ELTSUBST
00133      05  WS-BEN-MAX-VISIT-DAYS-X.                                 ELTSUBST
00134        10  WS-BEN-MAX-VISIT-DAYS       PIC 999.                   ELTSUBST
00135      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTSUBST
00136      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTSUBST
00137      05  WS-DTL-PP                     PIC X(50).                 ELTSUBST
00138      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTSUBST
00139      05  WS-DTL-ALLOW                  PIC X(63).                 ELTSUBST
00140      05  WS-DTL-ALLOW-AMT              PIC $$$9.99.               ELTSUBST
00141      05  WS-DTL-PER-D                  PIC X(63).                 ELTSUBST
00142      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTSUBST
00143      05  WS-DTL-CERT-REQ-ID            PIC X(79) VALUE SPACES.    ELTSUBST
00144      05  WS-DTL-CERT-REQ-IND           PIC X(79) VALUE SPACES.    ELTSUBST
00145      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTSUBST
00146      05  WS-DESC-CTR                   PIC S999 COMP VALUE +0.    ELTSUBST
00147      05  WS-VARIABLE-INDEMNITY-PRCNT                              ELTSUBST
00148                        PIC S999        COMP VALUE +0.             ELTSUBST
00149      05  WS-ADDITIONAL-PRICING-PRCNT                              ELTSUBST
00150                        PIC S999        COMP VALUE +0.             ELTSUBST
00151      05  WS-ADDN-ALLOW-AMT-PER-DAY                                ELTSUBST
00152                        PIC S999V99     COMP VALUE +0.             ELTSUBST
00153      05  WS-FLAT-RATE-PDM-AMT                                     ELTSUBST
00154                        PIC S9(5)V99    COMP VALUE +0.             ELTSUBST
00155 /                                                                 ELTSUBST
00156 ******************************************************************ELTSUBST
00157 ** BENEFIT PROVISION ID'S BY TYPE                               **ELTSUBST
00158 ******************************************************************ELTSUBST
00159  01  TABLE-MAX                   PIC S9(03) VALUE +7 COMP.        ELTSUBST
00160 * 6  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTSUBST
00161                                                                   ELTSUBST
00162  01  WS-BEN-PROV-ID.                                              ELTSUBST
00163      05  WS-INST-IP-CNT                PIC S999  COMP VALUE +7.   ELTSUBST
00164      05  WS-INST-IP-TAB.                                          ELTSUBST
00165        10  FILLER                      PIC X(6)  VALUE 'ABUI A'.  ELTSUBST
00166        10  FILLER                      PIC X(6)  VALUE 'ABUI W'.  ELTSUBST
00167        10  FILLER                      PIC X(6)  VALUE 'ADXI W'.  ELTSUBST
00168        10  FILLER                      PIC X(6)  VALUE 'ARPI W'.  ELTSUBST
00169        10  FILLER                      PIC X(6)  VALUE 'DDXI W'.  ELTSUBST
00170        10  FILLER                      PIC X(6)  VALUE 'DRPI W'.  ELTSUBST
00171        10  FILLER                      PIC X(6)  VALUE 'DTOX A'.  ELTSUBST
00172      05  WS-INST-IP-LIST REDEFINES WS-INST-IP-TAB                 ELTSUBST
00173                                        PIC X(6)  OCCURS 7 TIMES.  ELTSUBST
00174                                                                   ELTSUBST
00175      05  WS-INST-OP-CNT                PIC S999  COMP VALUE +6.   ELTSUBST
00176      05  WS-INST-OP-TAB.                                          ELTSUBST
00177        10  FILLER                      PIC X(6)  VALUE 'ABUO A'.  ELTSUBST
00178        10  FILLER                      PIC X(6)  VALUE 'ABUO W'.  ELTSUBST
00179        10  FILLER                      PIC X(6)  VALUE 'ADXO W'.  ELTSUBST
00180        10  FILLER                      PIC X(6)  VALUE 'ARPO W'.  ELTSUBST
00181        10  FILLER                      PIC X(6)  VALUE 'DDXO W'.  ELTSUBST
00182        10  FILLER                      PIC X(6)  VALUE 'DRPO W'.  ELTSUBST
00183      05  WS-INST-OP-LIST REDEFINES WS-INST-OP-TAB                 ELTSUBST
00184                                        PIC X(6)  OCCURS 6 TIMES.  ELTSUBST
00185                                                                   ELTSUBST
00186      05  WS-PROF-IP-CNT                PIC S999  COMP VALUE +4.   ELTSUBST
00187      05  WS-PROF-IP-TAB.                                          ELTSUBST
00188        10  FILLER                      PIC X(6)  VALUE 'AHI  D'.  ELTSUBST
00189        10  FILLER                      PIC X(6)  VALUE 'DRI  D'.  ELTSUBST
00190        10  FILLER                      PIC X(6)  VALUE 'ECFA D'.  ELTSUBST
00191        10  FILLER                      PIC X(6)  VALUE 'ECFD D'.  ELTSUBST
00192      05  WS-PROF-IP-LIST REDEFINES WS-PROF-IP-TAB                 ELTSUBST
00193                                        PIC X(6)  OCCURS 4 TIMES.  ELTSUBST
00194 /                                                                 ELTSUBST
00195 ******************************************************************ELTSUBST
00196 ** DATA USED FOR BUILDING DISPLAY LINES.                        **ELTSUBST
00197 ******************************************************************ELTSUBST
00198  01  WS-ELS-DISPLAY-LINES.                                        ELTSUBST
00199      10  WS-VAR-EXCEPT             PIC X(6)  VALUE SPACES.        ELTSUBST
00200                                                                   ELTSUBST
00201    05  WS-HDR-2-INST-IP.                                          ELTSUBST
00202      10  FILLER                    PIC X(20) VALUE SPACES.        ELTSUBST
00203      10  FILLER                    PIC X(39)                      ELTSUBST
00204          VALUE 'SUBSTANCE ABUSE INPATIENT INSTITUTIONAL'.         ELTSUBST
00205      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTSUBST
00206                                                                   ELTSUBST
00207    05  WS-HDR-2-INST-OP.                                          ELTSUBST
00208      10  FILLER                    PIC X(20) VALUE SPACES.        ELTSUBST
00209      10  FILLER                    PIC X(40)                      ELTSUBST
00210          VALUE 'SUBSTANCE ABUSE OUTPATIENT INSTITUTIONAL'.        ELTSUBST
00211      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTSUBST
00212                                                                   ELTSUBST
00213 ******************************************************************ELTSUBST
00214 ** THERE IS NO OUTPATIENT PROFESSIONAL COVERAGE.                **ELTSUBST
00215 ******************************************************************ELTSUBST
00216    05  WS-HDR-2-PROF-IP.                                          ELTSUBST
00217      10  FILLER                    PIC X(25) VALUE SPACES.        ELTSUBST
00218      10  FILLER                    PIC X(28)                      ELTSUBST
00219          VALUE 'SUBSTANCE ABUSE PROFESSIONAL'.                    ELTSUBST
00220      10  FILLER                    PIC X(26) VALUE LOW-VALUES.    ELTSUBST
00221                                                                   ELTSUBST
00222    05  WS-BASIC.                                                  ELTSUBST
00223      10  WS-BASIC-LIT              PIC X(16)                      ELTSUBST
00224          VALUE '         BASIC: '.                                ELTSUBST
00225      10  WS-DTL-BASIC.                                            ELTSUBST
00226          15  WS-DTL-BAS-NUM-3      PIC ZZ9.99- VALUE ZERO.        ELTSUBST
00227          15  WS-FILLER             PIC X(47)   VALUE SPACES.      ELTSUBST
00228      10  FILLER                    PIC X(10)   VALUE LOW-VALUES.  ELTSUBST
00229                                                                   ELTSUBST
00230    05  WS-SUPPLEMENTAL.                                           ELTSUBST
00231      10  WS-SUPP-LIT               PIC X(16)                      ELTSUBST
00232          VALUE '  SUPPLEMENTAL: '.                                ELTSUBST
00233      10  WS-DTL-SUPPLEMENTAL.                                     ELTSUBST
00234          15  WS-DTL-SUP-NUM-3      PIC ZZ9.99- VALUE ZERO.        ELTSUBST
00235          15  WS-FILLER             PIC X(47)   VALUE SPACES.      ELTSUBST
00236      10  FILLER                    PIC X(10)   VALUE LOW-VALUES.  ELTSUBST
00237                                                                   ELTSUBST
00238    05  WS-BASIC-INDEMNITY.                                        ELTSUBST
00239      10  FILLER                    PIC X(49)                      ELTSUBST
00240        VALUE 'BASIC INDEMNITY EXCESS SPILLS TO SUPPLEMENTAL MM '. ELTSUBST
00241      10  WS-DTL-BASIC-INDEMNITY    PIC X(27) VALUE SPACES.        ELTSUBST
00242      10  FILLER                    PIC X(3) VALUE LOW-VALUES.     ELTSUBST
00243                                                                   ELTSUBST
00244    05  WS-NO-TABULAR1.                                            ELTSUBST
00245      10  FILLER                    PIC X(51)  VALUE               ELTSUBST
00246         '*** FOUND A GENERIC CONTRACT FILE INCONSISTENCY IN '.    ELTSUBST
00247      10  FILLER                    PIC X(22)  VALUE               ELTSUBST
00248         'GOING FROM BENEFIT ***'.                                 ELTSUBST
00249                                                                   ELTSUBST
00250    05  WS-NO-TABULAR2.                                            ELTSUBST
00251      10  FILLER                    PIC X(15)  VALUE               ELTSUBST
00252         '*** PROVISION: '.                                        ELTSUBST
00253      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTSUBST
00254      10  FILLER                    PIC X VALUE SPACE.             ELTSUBST
00255      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTSUBST
00256      10  FILLER                    PIC X(13)  VALUE               ELTSUBST
00257         ' TO TABULAR: '.                                          ELTSUBST
00258      10  WS-NO-TAB-ID              PIC X(6).                      ELTSUBST
00259      10  FILLER                    PIC X VALUE SPACE.             ELTSUBST
00260      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTSUBST
00261      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTSUBST
00262                                                                   ELTSUBST
00263    05  WS-PERCENT-OF-ALLOWANCE.                                   ELTSUBST
00264      10  FILLER                    PIC X(44)                      ELTSUBST
00265       VALUE '                THE PERCENT OF ALLOWANCE IS '.       ELTSUBST
00266      10  WS-PCT-ALLOW              PIC ZZ9.                       ELTSUBST
00267      10  FILLER                    PIC X VALUE '%'.               ELTSUBST
00268                                                                   ELTSUBST
00269    05  WS-SERVICES-2ND.                                           ELTSUBST
00270      10  FILLER                    PIC X(21) VALUE SPACES.        ELTSUBST
00271      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTSUBST
00272      10  FILLER                    PIC X(3)  VALUE LOW-VALUES.    ELTSUBST
00273                                                                   ELTSUBST
00274    05  WS-BASIC-PERCENT.                                          ELTSUBST
00275      10  FILLER                    PIC X(9)  VALUE SPACES.        ELTSUBST
00276      10  FILLER                    PIC X(7)  VALUE 'BASIC: '.     ELTSUBST
00277      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTSUBST
00278                                                                   ELTSUBST
00279    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTSUBST
00280      10  FILLER                    PIC X(16)                      ELTSUBST
00281          VALUE '  SUPPLEMENTAL: '.                                ELTSUBST
00282      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTSUBST
00283                                                                   ELTSUBST
00284    05  WS-SUPP-PER-D.                                             ELTSUBST
00285      10  FILLER                    PIC X(16)                      ELTSUBST
00286          VALUE '  SUPPLEMENTAL: '.                                ELTSUBST
00287      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTSUBST
00288      10  FILLER                    PIC X     VALUE SPACE.         ELTSUBST
00289      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTSUBST
00290      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTSUBST
00291                                                                   ELTSUBST
00292    05  WS-BLANK-PREFIX-DET-LINE.                                  ELTSUBST
00293      10  FILLER                    PIC X(16) VALUE SPACES.        ELTSUBST
00294      10  WS-TRUNC-TEXT             PIC X(63) VALUE LOW-VALUES.    ELTSUBST
00295                                                                   ELTSUBST
00296    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTSUBST
00297    05  FILLER REDEFINES WS-TEMP-TEXT-AREA.                        ELTSUBST
00298      10  WS-TEMP-TEXT-CHAR         PIC X OCCURS 79 TIMES.         ELTSUBST
00299                                                                   ELTSUBST
00300    05  WS-TEXT-CONSTANT-WORDS.                                    ELTSUBST
00301      10  WS-BASIC-SHORT-LIT        PIC X(5)  VALUE 'BASIC'.       ELTSUBST
00302      10  WS-DAYS                   PIC X(4)  VALUE 'DAYS'.        ELTSUBST
00303      10  WS-EXCEPT                 PIC X(6)  VALUE 'EXCEPT'.      ELTSUBST
00304      10  WS-FOR                    PIC X(3)  VALUE 'FOR'.         ELTSUBST
00305      10  WS-IS                     PIC X(2)  VALUE 'IS'.          ELTSUBST
00306      10  WS-NO                     PIC X     VALUE 'N'.           ELTSUBST
00307      10  WS-OUTPATIENT             PIC X(11) VALUE 'OUTPATIENT '. ELTSUBST
00308      10  WS-PER                    PIC X(3)  VALUE 'PER'.         ELTSUBST
00309      10  WS-SECONDARY              PIC X(9)  VALUE 'SECONDARY'.   ELTSUBST
00310      10  WS-SPILLOVER              PIC X(10) VALUE 'SPILLOVER'.   ELTSUBST
00311      10  WS-YES                    PIC X     VALUE 'Y'.           ELTSUBST
00312                                                                   ELTSUBST
00313    05  WS-TEXT-CONSTANT-PHRASES.                                  ELTSUBST
00314                                                                   ELTSUBST
00315      10  WS-ADM-REQ-FOR                PIC X(27) VALUE            ELTSUBST
00316          'ADMISSION REQUIREMENTS FOR '.                           ELTSUBST
00317                                                                   ELTSUBST
00318      10  WS-ALCOHOL-ARE                PIC X(29) VALUE            ELTSUBST
00319          ' ALCOHOL REHABILITATION ARE: '.                         ELTSUBST
00320                                                                   ELTSUBST
00321      10  WS-ALCOHOL-REHAB              PIC X(37) VALUE            ELTSUBST
00322          'ALCOHOL REHABILITATION BENEFITS ARE: '.                 ELTSUBST
00323                                                                   ELTSUBST
00324      10  WS-CALL-CONTRACT-CODING       PIC X(48) VALUE            ELTSUBST
00325          'PRICING METHOD NOT CODED.  CALL CONTRACT CODING.'.      ELTSUBST
00326                                                                   ELTSUBST
00327      10  WS-CHECK-CONTRACT-FOR-PVE     PIC X(44) VALUE            ELTSUBST
00328          'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.          ELTSUBST
00329                                                                   ELTSUBST
00330      10  WS-CONTRACT-RELATED           PIC X(50) VALUE            ELTSUBST
00331          'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.    ELTSUBST
00332                                                                   ELTSUBST
00333      10  WS-DAYS-REDUCED               PIC X(21) VALUE            ELTSUBST
00334          'DAYS REDUCTION RATIO '.                                 ELTSUBST
00335                                                                   ELTSUBST
00336      10  WS-DRUG-ARE                   PIC X(26) VALUE            ELTSUBST
00337          ' DRUG REHABILITATION ARE: '.                            ELTSUBST
00338                                                                   ELTSUBST
00339      10  WS-DRUG-REHAB                 PIC X(34) VALUE            ELTSUBST
00340          'DRUG REHABILITATION BENEFITS ARE: '.                    ELTSUBST
00341                                                                   ELTSUBST
00342      10  WS-ELIG-MEMBERS-FOR       PIC X(25) VALUE                ELTSUBST
00343          'THE ELIGIBLE MEMBERS FOR '.                             ELTSUBST
00344                                                                   ELTSUBST
00345      10  WS-FOLLOWING-BEN          PIC X(22) VALUE                ELTSUBST
00346            'COVERED SERVICES ARE: '.                              ELTSUBST
00347                                                                   ELTSUBST
00348      10  WS-MAX-VISIT                  PIC X(33) VALUE            ELTSUBST
00349          'THE MAXIMUM NUMBER OF VISITS IS: '.                     ELTSUBST
00350                                                                   ELTSUBST
00351      10  WS-MAX-PER-VISIT              PIC X(42) VALUE            ELTSUBST
00352          'THE MAXIMUM AMOUNT ELIGIBLE PER VISIT IS: '.            ELTSUBST
00353                                                                   ELTSUBST
00354      10  WS-PAYABLE-FOR                PIC X(40) VALUE            ELTSUBST
00355          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTSUBST
00356                                                                   ELTSUBST
00357                                                                   ELTSUBST
00358      10  WS-PAYMNT-BASED               PIC X(21) VALUE            ELTSUBST
00359          'PAYMENT IS BASED ON: '.                                 ELTSUBST
00360                                                                   ELTSUBST
00361      10  WS-SEE-CCP                    PIC X(54) VALUE            ELTSUBST
00362        'SEE COST CONTAINMENT TOPIC FOR ADDITIONAL LIMITATIONS.'.  ELTSUBST
00363                                                                   ELTSUBST
00364      10  WS-SERVICES-RENDERED          PIC X(25) VALUE            ELTSUBST
00365          'SERVICES MAY BE RENDERED:'.                             ELTSUBST
00366                                                                   ELTSUBST
00367      10  WS-SPILLOVER-COINS            PIC X(23) VALUE            ELTSUBST
00368          'SPILLOVER COINSURANCE:'.                                ELTSUBST
00369                                                                   ELTSUBST
00370      10  WS-SPILLOVER-DED              PIC X(23) VALUE            ELTSUBST
00371          ' SPILLOVER DEDUCTIBLE:'.                                ELTSUBST
00372                                                                   ELTSUBST
00373      10  WS-SPILLOVER-FL-RT-PER-D      PIC X(29) VALUE            ELTSUBST
00374          'SPILLOVER FLAT RATE PER DIEM '.                         ELTSUBST
00375                                                                   ELTSUBST
00376      10  WS-SPILLOVER-ROOM-FLAT-RT     PIC X(28) VALUE            ELTSUBST
00377          'SPILLOVER ROOM FLAT RATE IS '.                          ELTSUBST
00378                                                                   ELTSUBST
00379      10  WS-TOPIC-PHRASE               PIC X(29) VALUE            ELTSUBST
00380          'SUBSTANCE ABUSE BENEFITS     '.                         ELTSUBST
00381                                                                   ELTSUBST
00382      10  WS-VISIT-REDUCT-RATE-BASIC    PIC X(28) VALUE            ELTSUBST
00383          'VISIT REDUCTION RATIO BASIC '.                          ELTSUBST
00384                                                                   ELTSUBST
00385      10  WS-VISIT-REDUCT-RATE-SEC      PIC X(32) VALUE            ELTSUBST
00386          'VISIT REDUCTION RATIO SECONDARY '.                      ELTSUBST
00387                                                                   ELTSUBST
00388      10  WS-ACCUM-MSG1           PIC  X(79) VALUE                 ELTSUBST
00389      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTSUBST
00390 -    'CONSIDERATIONS.'.                                           ELTSUBST
00391                                                                   ELTSUBST
00392      10  WS-NETWORK-UTIL-REV.                                     ELTSUBST
00393          15  WS-NETWORK-UTIL-REV-PHR   PIC X(51)  VALUE           ELTSUBST
00394          'SUBSTANCE ABUSE SERVICES PROCESSING IS BASED ON - '.    ELTSUBST
00395          15  FILLER                    PIC X(28) VALUE LOW-VALUES.ELTSUBST
00396  01  WS-END                            PIC X(16)  VALUE           ELTSUBST
00397      '*** W/S ENDS ***'.                                          ELTSUBST
00398 /                                                                 ELTSUBST
00399  LINKAGE SECTION.                                                 ELTSUBST
00400  01  DFHCOMMAREA.                                                 ELTSUBST
00401      COPY ELSCOMMC.                                               ELTSUBST
00402 /                                                                 ELTSUBST
00403      COPY ELSCIA2C.                                               ELTSUBST
00404 /                                                                 ELTSUBST
00405      COPY ELSIOPMC.                                               ELTSUBST
00406 /                                                                 ELTSUBST
00407      COPY ELSKEYSC.                                               ELTSUBST
00408 /                                                                 ELTSUBST
00409      COPY ELSOUTPC.                                               ELTSUBST
00410 /                                                                 ELTSUBST
00411      COPY ELSSSCBC.                                               ELTSUBST
00412 /                                                                 ELTSUBST
00413      COPY ELSCMIFC.                                               ELTSUBST
00414 /                                                                 ELTSUBST
00415      COPY ELSCMDSC.                                               ELTSUBST
00416 /                                                                 ELTSUBST
00417      COPY ELSPRVNC.                                               ELTSUBST
00418 /                                                                 ELTSUBST
00419 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTSUBST
00420      COPY ELSPLGSW.                                               ELTSUBST
00421 *** BENEFIT PROVISION TABLE OF FLDS                               ELTSUBST
00422      COPY ELSPLGTB.                                               ELTSUBST
00423 /                                                                 ELTSUBST
00424      COPY ELSTCWAC.                                               ELTSUBST
00425 /                                                                 ELTSUBST
00426  01  GROUP-SPECIFIC-RECORD.                                       ELTSUBST
00427      COPY GCGROUPC.                                               ELTSUBST
00428 /                                                                 ELTSUBST
00429  01  CONTRACT-RECORD.                                             ELTSUBST
00430      COPY GCCONTRC.                                               ELTSUBST
00431 /                                                                 ELTSUBST
00432  PROCEDURE DIVISION.                                              ELTSUBST
00433                                                                   ELTSUBST
00434  0000-MAINLINE.                                                   ELTSUBST
00435                                                                   ELTSUBST
00436                                                                   ELTSUBST
00437      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTSUBST
00438          EXEC CICS ABEND                                          ELTSUBST
00439                    ABCODE ('EL01')                                ELTSUBST
00440          END-EXEC                                                 ELTSUBST
00441      END-IF.                                                      ELTSUBST
00442                                                                   ELTSUBST
00443 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTSUBST
00444                                                                   ELTSUBST
00445      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTSUBST
00446          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTSUBST
00447                                                                   ELTSUBST
00448      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTSUBST
00449      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00450          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK                   ELTSUBST
00451                                                                   ELTSUBST
00452      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTSUBST
00453      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00454          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTSUBST
00455                                                                   ELTSUBST
00456      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTSUBST
00457      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00458          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTSUBST
00459                                                                   ELTSUBST
00460      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTSUBST
00461      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00462          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTSUBST
00463                                                                   ELTSUBST
00464      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTSUBST
00465      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00466          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTSUBST
00467                                                                   ELTSUBST
00468      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTSUBST
00469      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00470          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTSUBST
00471                                                                   ELTSUBST
00472      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTSUBST
00473      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00474          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTSUBST
00475                                                                   ELTSUBST
00476      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTSUBST
00477                                                                   ELTSUBST
00478      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTSUBST
00479              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTSUBST
00480                                                                   ELTSUBST
00481      SET CIA-STG-GETMAIN TO TRUE.                                 ELTSUBST
00482      EXEC CICS LINK                                               ELTSUBST
00483                PROGRAM('ELUSTGMG')                                ELTSUBST
00484                COMMAREA(DFHCOMMAREA)                              ELTSUBST
00485      END-EXEC.                                                    ELTSUBST
00486                                                                   ELTSUBST
00487      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTSUBST
00488      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00489          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTSUBST
00490                                                                   ELTSUBST
00491                                                                   ELTSUBST
00492      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTSUBST
00493                                                                   ELTSUBST
00494      MOVE GCG-NETWORK-UTIL-REVIEW-IND                             ELTSUBST
00495        TO WS-NETWORK-UTIL-IND.                                    ELTSUBST
00496                                                                   ELTSUBST
00497 ******************************************************************ELTSUBST
00498 ** SUBSTANCE ABUSE IS SHOWN UNDER THE INSTITUTIONAL INPATIENT   **ELTSUBST
00499 ** AND OUTPATIENT CATEGORIES, AND UNDER THE PROFESSIONAL        **ELTSUBST
00500 ** INPATIENT CATEGORY.  THERE IS NO PROFESSIONAL OUTPATIENT     **ELTSUBST
00501 ** COVERAGE FOR SUBSTANCE ABUSE.  SINCE SUBSTANCE ABUSE HAS NO  **ELTSUBST
00502 ** SUBTOPIC MENU, WE SHOW ALL THREE CATEGORIES IF 'BOTH' WAS    **ELTSUBST
00503 ** CHOSEN ON THE INSTITUTIONAL/PROFESSIONAL SELECTION SCREEN.   **ELTSUBST
00504 ******************************************************************ELTSUBST
00505                                                                   ELTSUBST
00506      IF SSB-PROV-CLASS-INST  OR  SSB-PROV-CLASS-BOTH              ELTSUBST
00507         PERFORM 1000-INSTITUTIONAL-IP-RTNE                        ELTSUBST
00508         PERFORM 1500-INSTITUTIONAL-OP-RTNE.                       ELTSUBST
00509                                                                   ELTSUBST
00510      IF SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH              ELTSUBST
00511         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTSUBST
00512                                                                   ELTSUBST
00513 ******************************************************************ELTSUBST
00514 ** TERMINATE OUTPUT PROCESSING.                                 **ELTSUBST
00515 ******************************************************************ELTSUBST
00516                                                                   ELTSUBST
00517      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTSUBST
00518                                                                   ELTSUBST
00519      SET CIA-STG-FREEMAIN TO TRUE.                                ELTSUBST
00520      EXEC CICS LINK                                               ELTSUBST
00521                PROGRAM('ELUSTGMG')                                ELTSUBST
00522                COMMAREA(DFHCOMMAREA)                              ELTSUBST
00523      END-EXEC.                                                    ELTSUBST
00524                                                                   ELTSUBST
00525                                                                   ELTSUBST
00526      MOVE 'E'   TO  COF-FUNCTION.                                 ELTSUBST
00527      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTSUBST
00528                                                                   ELTSUBST
00529      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTSUBST
00530                     COMMAREA (DFHCOMMAREA)                        ELTSUBST
00531      END-EXEC.                                                    ELTSUBST
00532                                                                   ELTSUBST
00533      EXEC CICS RETURN                                             ELTSUBST
00534      END-EXEC.                                                    ELTSUBST
00535                                                                   ELTSUBST
00536      GOBACK.                                                      ELTSUBST
00537 /                                                                 ELTSUBST
00538 ******************************************************************ELTSUBST
00539 ** SOME OF THE INSTITUTIONAL LOGIC WAS CLONED FROM THE ROOM     **ELTSUBST
00540 ** AND BOARD TOPIC PROGRAM, ELTROOMB.                           **ELTSUBST
00541 ******************************************************************ELTSUBST
00542  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTSUBST
00543                                                                   ELTSUBST
00544      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSUBST
00545                                                                   ELTSUBST
00546      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
00547      PERFORM WITH TEST BEFORE                                     ELTSUBST
00548              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSUBST
00549              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSUBST
00550         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSUBST
00551         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSUBST
00552         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSUBST
00553         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSUBST
00554      END-PERFORM.                                                 ELTSUBST
00555      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
00556                                                                   ELTSUBST
00557      PERFORM WITH TEST BEFORE                                     ELTSUBST
00558         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTSUBST
00559         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTSUBST
00560            SET  PVN-BEN-PROVN-IDX TO WS-SUB                       ELTSUBST
00561            MOVE WS-INST-IP-LIST(WS-SUB)  TO                       ELTSUBST
00562                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX)    ELTSUBST
00563            MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      ELTSUBST
00564                            PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)    ELTSUBST
00565      END-PERFORM.                                                 ELTSUBST
00566                                                                   ELTSUBST
00567                                                                   ELTSUBST
00568      MOVE WS-HDR-2-INST-IP    TO  COF-HDR-LINE(2).                ELTSUBST
00569 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTSUBST
00570      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTSUBST
00571      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTSUBST
00572      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSUBST
00573                                                                   ELTSUBST
00574      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
00575              END-EXEC.                                            ELTSUBST
00576 ****************************************************              ELTSUBST
00577                                                                   ELTSUBST
00578      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTSUBST
00579                                                                   ELTSUBST
00580      IF WS-VALID-INST-INPATIENT                                   ELTSUBST
00581         ADD +1 TO WS-CIA                                          ELTSUBST
00582         PERFORM 6000-DSPLY-NTWRK-UTIL-IND.                        ELTSUBST
00583                                                                   ELTSUBST
00584      MOVE WS-TOPIC-PHRASE     TO  SSB-TOPIC-PHRASE.               ELTSUBST
00585                                                                   ELTSUBST
00586      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTSUBST
00587          END-EXEC.                                                ELTSUBST
00588                                                                   ELTSUBST
00589      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSUBST
00590      MOVE ' '  TO  COF-FUNCTION.                                  ELTSUBST
00591      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
00592              END-EXEC.                                            ELTSUBST
00593                                                                   ELTSUBST
00594      IF PVN-COVG-NONE                                             ELTSUBST
00595         GO TO 1099-EXIT.                                          ELTSUBST
00596                                                                   ELTSUBST
00597      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTSUBST
00598            PSP-PROVN-PRICING-METHD,                               ELTSUBST
00599            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSUBST
00600            PSP-TRANSF-OTHER-RESP-IND,                             ELTSUBST
00601            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSUBST
00602            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSUBST
00603            PSP-SPILL-OVER-DED-APL-IND,                            ELTSUBST
00604            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTSUBST
00605            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSUBST
00606            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSUBST
00607            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSUBST
00608            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSUBST
00609            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSUBST
00610            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSUBST
00611            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTSUBST
00612            PSA-DAYS-RDCN-RAT-IND,                                 ELTSUBST
00613            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTSUBST
00614            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTSUBST
00615            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTSUBST
00616            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTSUBST
00617            PSA-FLAT-RATE-PDM-AMT,                                 ELTSUBST
00618            PSA-REHAB-ADM-RESTRN-IND,                              ELTSUBST
00619            PSW-ADDN-ALLOW-AMT-PER-DAY,                            ELTSUBST
00620            PSW-DAYS-RDCN-RAT-BASIC-APL,                           ELTSUBST
00621            PSW-DAYS-RDCN-RAT-BASIC-BASE,                          ELTSUBST
00622            PSW-DAYS-RDCN-RAT-SEC-APL,                             ELTSUBST
00623            PSW-DAYS-RDCN-RAT-SEC-BASE,                            ELTSUBST
00624            PSW-FLAT-RATE-PDM-AMT,                                 ELTSUBST
00625            PSW-REHAB-ADM-RESTRN-IND.                              ELTSUBST
00626                                                                   ELTSUBST
00627                                                                   ELTSUBST
00628      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSUBST
00629                     COMMAREA (DFHCOMMAREA)                        ELTSUBST
00630      END-EXEC.                                                    ELTSUBST
00631                                                                   ELTSUBST
00632      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSUBST
00633      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
00634          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSUBST
00635                                                                   ELTSUBST
00636      PERFORM WITH TEST BEFORE                                     ELTSUBST
00637         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSUBST
00638         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTSUBST
00639             SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB          ELTSUBST
00640             IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) NOT = ZERO     ELTSUBST
00641                  PERFORM 1040-BUILD-SCREEN-LINES                  ELTSUBST
00642             END-IF                                                ELTSUBST
00643      END-PERFORM.                                                 ELTSUBST
00644                                                                   ELTSUBST
00645  1099-EXIT.   EXIT.                                               ELTSUBST
00646 /                                                                 ELTSUBST
00647  1040-BUILD-SCREEN-LINES SECTION.                                 ELTSUBST
00648                                                                   ELTSUBST
00649      SET PLT-INDEX1 TO                                            ELTSUBST
00650         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTSUBST
00651                                                                   ELTSUBST
00652      IF WS-NOT-FIRST-TIME                                         ELTSUBST
00653         SET COF-NEW-PAGE TO TRUE                                  ELTSUBST
00654         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTSUBST
00655         EXEC CICS LINK PROGRAM('ELUOUTPT')                        ELTSUBST
00656                   COMMAREA (DFHCOMMAREA)                          ELTSUBST
00657         END-EXEC                                                  ELTSUBST
00658      ELSE                                                         ELTSUBST
00659         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTSUBST
00660                                                                   ELTSUBST
00661      MOVE +1 TO WS-CIA.                                           ELTSUBST
00662      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
00663         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO         ELTSUBST
00664            SET PLT-INDEX2 TO 2                                    ELTSUBST
00665         ELSE                                                      ELTSUBST
00666            PERFORM 2400-PROBLEM-WITH-INDICES                      ELTSUBST
00667            MOVE TABLE-MAX TO WS-SUB                               ELTSUBST
00668            GO TO 1040-EXIT                                        ELTSUBST
00669      ELSE                                                         ELTSUBST
00670         SET PLT-INDEX2 TO 1.                                      ELTSUBST
00671      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTSUBST
00672                                                                   ELTSUBST
00673      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTSUBST
00674      ADD +1                 TO WS-CIA.                            ELTSUBST
00675      MOVE WS-FOLLOWING-BEN  TO COF-DTL-LINE(WS-CIA).              ELTSUBST
00676      ADD +1                 TO WS-CIA.                            ELTSUBST
00677      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTSUBST
00678         VARYING WS-SUB2 FROM WS-SUB BY +1                         ELTSUBST
00679         UNTIL WS-SUB2 > WS-INST-IP-CNT.                           ELTSUBST
00680                                                                   ELTSUBST
00681      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSUBST
00682                                                                   ELTSUBST
00683      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00684      ADD +1                   TO WS-CIA.                          ELTSUBST
00685      MOVE WS-SERVICES-RENDERED                                    ELTSUBST
00686                               TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00687      ADD +1                   TO WS-CIA.                          ELTSUBST
00688                                                                   ELTSUBST
00689      MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
00690                                                                   ELTSUBST
00691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00692          PERFORM 4000-PLACE-OF-TREAT-BASIC.                       ELTSUBST
00693                                                                   ELTSUBST
00694      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00695          PERFORM 4050-PLACE-OF-TREAT-SUPP.                        ELTSUBST
00696                                                                   ELTSUBST
00697 ******************************************************************ELTSUBST
00698 ** IF NO DETAIL WAS BUILT FOR EITHER BASIC OR SUPPLEMENTAL,     **ELTSUBST
00699 ** SUPPRESS THE CAPTION LINE.                                   **ELTSUBST
00700 ******************************************************************ELTSUBST
00701      IF WS-NEW-LINES-GENERATED                                    ELTSUBST
00702         MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                    ELTSUBST
00703         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
00704      ELSE                                                         ELTSUBST
00705         ADD -2 TO WS-CIA.                                         ELTSUBST
00706                                                                   ELTSUBST
00707      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSUBST
00708                                                                   ELTSUBST
00709      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00710      ADD +1                   TO WS-CIA.                          ELTSUBST
00711      MOVE WS-PAYABLE-FOR      TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00712      ADD +1                   TO WS-CIA.                          ELTSUBST
00713                                                                   ELTSUBST
00714      MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
00715                                                                   ELTSUBST
00716      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00717         SET PLT-INDEX2 TO 1                                       ELTSUBST
00718         PERFORM 4100-PAYABLE-FOR-BASIC.                           ELTSUBST
00719                                                                   ELTSUBST
00720      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00721         SET PLT-INDEX2 TO 2                                       ELTSUBST
00722         PERFORM 4200-PAYABLE-FOR-SUPP.                            ELTSUBST
00723                                                                   ELTSUBST
00724 ******************************************************************ELTSUBST
00725 ** IF NO DETAIL WAS BUILT FOR EITHER BASIC OR SUPPLEMENTAL,     **ELTSUBST
00726 ** SUPPRESS THE CAPTION LINE.                                   **ELTSUBST
00727 ******************************************************************ELTSUBST
00728      IF WS-NEW-LINES-GENERATED                                    ELTSUBST
00729         MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                    ELTSUBST
00730         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
00731      ELSE                                                         ELTSUBST
00732         ADD -2 TO WS-CIA.                                         ELTSUBST
00733                                                                   ELTSUBST
00734      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00735      ADD +1                   TO WS-CIA.                          ELTSUBST
00736                                                                   ELTSUBST
00737      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00738        SET PLT-INDEX2 TO 1                                        ELTSUBST
00739          IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
00740                  NOT = ZERO AND NOT = LOW-VALUES                  ELTSUBST
00741              MOVE WS-EXCEPT TO WS-VAR-EXCEPT                      ELTSUBST
00742          ELSE                                                     ELTSUBST
00743              MOVE SPACES TO WS-VAR-EXCEPT.                        ELTSUBST
00744                                                                   ELTSUBST
00745      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00746        SET PLT-INDEX2 TO 1                                        ELTSUBST
00747        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00748                     NUMERIC AND                                   ELTSUBST
00749            PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00750                     NOT = ZERO                                    ELTSUBST
00751         IF PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00752                     NUMERIC AND                                   ELTSUBST
00753            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00754                     NOT = ZERO                                    ELTSUBST
00755             MOVE                                                  ELTSUBST
00756              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTSUBST
00757                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
00758             MOVE                                                  ELTSUBST
00759              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTSUBST
00760                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
00761             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
00762             STRING WS-DAYS-REDUCED,                               ELTSUBST
00763                    WS-BASIC-SHORT-LIT,                            ELTSUBST
00764                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
00765                    WS-FOR, ' '                                    ELTSUBST
00766                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
00767                    WS-VAR-EXCEPT,                                 ELTSUBST
00768                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
00769             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
00770             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
00771             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
00772             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
00773             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
00774             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00775             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
00776                                                                   ELTSUBST
00777      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00778          SET PLT-INDEX2 TO 1                                      ELTSUBST
00779          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
00780              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
00781          MOVE 'BPA' TO CMF-RECORD-PREFIX                          ELTSUBST
00782          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
00783                                                                   ELTSUBST
00784      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00785        SET PLT-INDEX2 TO 1                                        ELTSUBST
00786        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00787                         NUMERIC  AND                              ELTSUBST
00788            PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00789                         = ZERO   AND                              ELTSUBST
00790            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00791                          NUMERIC AND                              ELTSUBST
00792            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00793                          = ZERO                                   ELTSUBST
00794                              ADD +1 TO WS-CIA.                    ELTSUBST
00795                                                                   ELTSUBST
00796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00797        SET PLT-INDEX2 TO 1                                        ELTSUBST
00798        IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
00799                     NUMERIC AND                                   ELTSUBST
00800            PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
00801                     NOT = ZERO                                    ELTSUBST
00802         IF PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
00803                     NUMERIC AND                                   ELTSUBST
00804            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
00805                     NOT = ZERO                                    ELTSUBST
00806             MOVE                                                  ELTSUBST
00807              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00808                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
00809             MOVE                                                  ELTSUBST
00810              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00811                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
00812             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
00813             STRING WS-DAYS-REDUCED,                               ELTSUBST
00814                    WS-SECONDARY,                                  ELTSUBST
00815                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
00816                    WS-FOR, ' '                                    ELTSUBST
00817                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
00818                    WS-VAR-EXCEPT,                                 ELTSUBST
00819                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
00820             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
00821             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
00822             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
00823             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
00824             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
00825             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00826             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
00827                                                                   ELTSUBST
00828      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00829          SET PLT-INDEX2 TO 1                                      ELTSUBST
00830          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
00831              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
00832          MOVE 'BPA' TO CMF-RECORD-PREFIX                          ELTSUBST
00833          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
00834                                                                   ELTSUBST
00835      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
00836      ADD +1                   TO WS-CIA.                          ELTSUBST
00837                                                                   ELTSUBST
00838      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00839          IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
00840                  NOT = ZERO AND NOT = LOW-VALUES                  ELTSUBST
00841              MOVE WS-EXCEPT TO WS-VAR-EXCEPT                      ELTSUBST
00842          ELSE                                                     ELTSUBST
00843              MOVE SPACES TO WS-VAR-EXCEPT.                        ELTSUBST
00844                                                                   ELTSUBST
00845      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00846        SET PLT-INDEX2 TO 1                                        ELTSUBST
00847        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00848                     NUMERIC AND                                   ELTSUBST
00849            PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00850                     NOT = ZERO                                    ELTSUBST
00851         IF PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00852                     NUMERIC AND                                   ELTSUBST
00853            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00854                     NOT = ZERO                                    ELTSUBST
00855             MOVE                                                  ELTSUBST
00856              PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTSUBST
00857                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
00858             MOVE                                                  ELTSUBST
00859              PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTSUBST
00860                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
00861             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
00862             STRING WS-DAYS-REDUCED,                               ELTSUBST
00863                    WS-BASIC-SHORT-LIT,                            ELTSUBST
00864                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
00865                    WS-FOR, ' '                                    ELTSUBST
00866                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
00867                    WS-VAR-EXCEPT,                                 ELTSUBST
00868                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
00869             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
00870             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
00871             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
00872             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
00873             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
00874             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00875             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
00876                                                                   ELTSUBST
00877      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00878        SET PLT-INDEX2 TO 1                                        ELTSUBST
00879        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00880                       NUMERIC AND                                 ELTSUBST
00881            PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00882                       = ZERO                                      ELTSUBST
00883         IF PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00884                       NUMERIC AND                                 ELTSUBST
00885            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00886                       = ZERO                                      ELTSUBST
00887                              ADD +1 TO WS-CIA.                    ELTSUBST
00888                                                                   ELTSUBST
00889      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00890          SET PLT-INDEX2 TO 1                                      ELTSUBST
00891          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
00892              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
00893          MOVE 'BPW' TO CMF-RECORD-PREFIX                          ELTSUBST
00894          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
00895                                                                   ELTSUBST
00896      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00897        SET PLT-INDEX2 TO 1                                        ELTSUBST
00898        IF  PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
00899                     NUMERIC AND                                   ELTSUBST
00900            PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
00901                     NOT = ZERO                                    ELTSUBST
00902         IF PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
00903                     NUMERIC AND                                   ELTSUBST
00904            PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
00905                     NOT = ZERO                                    ELTSUBST
00906             MOVE                                                  ELTSUBST
00907              PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
00908                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
00909             MOVE                                                  ELTSUBST
00910              PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
00911                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
00912             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
00913             STRING WS-DAYS-REDUCED,                               ELTSUBST
00914                    WS-SECONDARY,                                  ELTSUBST
00915                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
00916                    WS-FOR, ' '                                    ELTSUBST
00917                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
00918                    WS-VAR-EXCEPT,                                 ELTSUBST
00919                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
00920             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
00921             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
00922             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
00923             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
00924             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
00925             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00926             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
00927                                                                   ELTSUBST
00928      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
00929          SET PLT-INDEX2 TO 1                                      ELTSUBST
00930          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
00931              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
00932          MOVE 'BPW' TO CMF-RECORD-PREFIX                          ELTSUBST
00933          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
00934                                                                   ELTSUBST
00935 ******************************************************************ELTSUBST
00936 ** ELIGIBLE DRUG REHABILITATION                                 **ELTSUBST
00937 ******************************************************************ELTSUBST
00938      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR.                       ELTSUBST
00939      IF CIA-RC-PTR-NULL                                           ELTSUBST
00940         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
00941         IF CIA-RC-PTR-NULL                                        ELTSUBST
00942            CONTINUE                                               ELTSUBST
00943      ELSE                                                         ELTSUBST
00944          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
00945          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00946          ADD +1                   TO WS-CIA                       ELTSUBST
00947          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
00948                                   WS-DRUG-REHAB                   ELTSUBST
00949                                   DELIMITED BY SIZE               ELTSUBST
00950                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
00951          ADD +1                   TO WS-CIA                       ELTSUBST
00952          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
00953          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
00954              PERFORM 1100-ELIG-DRUG-REHAB-BASIC                   ELTSUBST
00955          END-IF                                                   ELTSUBST
00956          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
00957          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
00958              PERFORM 1130-ELIG-DRUG-REHAB-SUPP                    ELTSUBST
00959          END-IF                                                   ELTSUBST
00960          IF WS-NEW-LINES-GENERATED                                ELTSUBST
00961              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
00962              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
00963          ELSE                                                     ELTSUBST
00964              ADD -2 TO WS-CIA                                     ELTSUBST
00965          END-IF                                                   ELTSUBST
00966      END-IF.                                                      ELTSUBST
00967                                                                   ELTSUBST
00968 ******************************************************************ELTSUBST
00969 ** ELIGIBLE ALCOHOL REHABILITATION                              **ELTSUBST
00970 ******************************************************************ELTSUBST
00971      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR.                       ELTSUBST
00972      IF CIA-RC-PTR-NULL                                           ELTSUBST
00973         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
00974         IF CIA-RC-PTR-NULL                                        ELTSUBST
00975            CONTINUE                                               ELTSUBST
00976      ELSE                                                         ELTSUBST
00977          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
00978          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
00979          ADD +1                   TO WS-CIA                       ELTSUBST
00980          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
00981                                   WS-ALCOHOL-REHAB                ELTSUBST
00982                                   DELIMITED BY SIZE               ELTSUBST
00983                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
00984          ADD +1                   TO WS-CIA                       ELTSUBST
00985          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
00986          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
00987              PERFORM 1150-ELIG-ALCOHOL-REHAB-BASIC                ELTSUBST
00988          END-IF                                                   ELTSUBST
00989          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
00990          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
00991              PERFORM 1160-ELIG-ALCOHOL-REHAB-SUPP                 ELTSUBST
00992          END-IF                                                   ELTSUBST
00993          IF WS-NEW-LINES-GENERATED                                ELTSUBST
00994              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
00995              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
00996          ELSE                                                     ELTSUBST
00997              ADD -2 TO WS-CIA                                     ELTSUBST
00998          END-IF                                                   ELTSUBST
00999      END-IF.                                                      ELTSUBST
01000                                                                   ELTSUBST
01001 ******************************************************************ELTSUBST
01002 ** SPILLOVER (APPLIES TO SUPPLEMENTAL COVERAGE IF PRESENT)      **ELTSUBST
01003 ******************************************************************ELTSUBST
01004      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01005         SET PLT-INDEX2 TO 2                                       ELTSUBST
01006         PERFORM 4300-SPILLOVER-COINS                              ELTSUBST
01007         PERFORM 4400-SPILLOVER-DEDUCT                             ELTSUBST
01008         IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01009                     NOT = '0'                                     ELTSUBST
01010          IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSUBST
01011                       NOT = LOW-VALUE                             ELTSUBST
01012           ADD +1     TO WS-CIA                                    ELTSUBST
01013           MOVE 'BP' TO CMF-RECORD-PREFIX                          ELTSUBST
01014           MOVE 'SPILL-OVR-RM-F-RT-APL-IND'                        ELTSUBST
01015                       TO CMF-ELEMENT-SYSTEM-NAME                  ELTSUBST
01016           MOVE                                                    ELTSUBST
01017            PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01018                       TO CMF-CODE-VALUE                           ELTSUBST
01019           EXEC CICS LINK                                          ELTSUBST
01020                PROGRAM('ELUCMIF')                                 ELTSUBST
01021                COMMAREA(DFHCOMMAREA)                              ELTSUBST
01022                END-EXEC                                           ELTSUBST
01023           PERFORM 4599-SET-CMF-DESCR-ADDR                         ELTSUBST
01024           MOVE SPACES          TO TCAR-FROM-AREA                  ELTSUBST
01025           STRING WS-SPILLOVER-FL-RT-PER-D                         ELTSUBST
01026                  CMF-DESCR-LINE(1) ' '                            ELTSUBST
01027                  CMF-DESCR-LINE(2) ' '                            ELTSUBST
01028                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTSUBST
01029           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTSUBST
01030           MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT         ELTSUBST
01031           MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN         ELTSUBST
01032           MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN         ELTSUBST
01033           PERFORM TCPR-000-TEXT-UNSTRING                          ELTSUBST
01034           MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)           ELTSUBST
01035           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTSUBST
01036             ADD +1                TO WS-CIA                       ELTSUBST
01037             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01038             PERFORM 3000-OUTPUT-TEXT                              ELTSUBST
01039           ELSE                                                    ELTSUBST
01040             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
01041                                                                   ELTSUBST
01042      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01043         SET PLT-INDEX2 TO 1                                       ELTSUBST
01044         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
01045                                                                   ELTSUBST
01046      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01047         SET PLT-INDEX2 TO 2                                       ELTSUBST
01048         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
01049                                                                   ELTSUBST
01050      PERFORM 4600-SCAN-TAB.                                       ELTSUBST
01051                                                                   ELTSUBST
01052  1040-EXIT.  EXIT.                                                ELTSUBST
01053 /                                                                 ELTSUBST
01054  1050-ZERO-ALL-WITH-SAME-NO SECTION.                              ELTSUBST
01055      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTSUBST
01056                                                                   ELTSUBST
01057      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTSUBST
01058         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
01059         MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME               ELTSUBST
01060         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTSUBST
01061             CMF-CODE-VALUE                                        ELTSUBST
01062         EXEC CICS LINK                                            ELTSUBST
01063              PROGRAM('ELUCMIF')                                   ELTSUBST
01064              COMMAREA(DFHCOMMAREA)                                ELTSUBST
01065              END-EXEC                                             ELTSUBST
01066         PERFORM 4599-SET-CMF-DESCR-ADDR                           ELTSUBST
01067         MOVE SPACES          TO TCAR-FROM-AREA                    ELTSUBST
01068         STRING CMF-DESCR-LINE(1) ' '                              ELTSUBST
01069                CMF-DESCR-LINE(2) ' '                              ELTSUBST
01070                  DELIMITED BY SIZE INTO TCAR-FROM-AREA            ELTSUBST
01071         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTSUBST
01072         MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT           ELTSUBST
01073         MOVE +55             TO TCAR-OUTPUT-FIELD-1-LEN           ELTSUBST
01074         MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN           ELTSUBST
01075         PERFORM TCPR-000-TEXT-UNSTRING                            ELTSUBST
01076         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTSUBST
01077         MOVE WS-SERVICES-2ND TO COF-DTL-LINE(WS-CIA)              ELTSUBST
01078         IF WS-CIA  <  20                                          ELTSUBST
01079            ADD +1 TO WS-CIA                                       ELTSUBST
01080            MOVE ZERO TO                                           ELTSUBST
01081                PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)                ELTSUBST
01082         ELSE                                                      ELTSUBST
01083            EXEC CICS LINK                                         ELTSUBST
01084                 PROGRAM('ELUOUTPT')                               ELTSUBST
01085                 COMMAREA(DFHCOMMAREA)                             ELTSUBST
01086                 END-EXEC                                          ELTSUBST
01087            MOVE +1 TO WS-CIA                                      ELTSUBST
01088            MOVE ZERO TO                                           ELTSUBST
01089                PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).               ELTSUBST
01090                                                                   ELTSUBST
01091  1050-EXIT.            EXIT.                                      ELTSUBST
01092                                                                   ELTSUBST
01093 ******************************************************************ELTSUBST
01094 ** ELIGIBLE DRUG REHABILITATION                                 **ELTSUBST
01095 ******************************************************************ELTSUBST
01096  1100-ELIG-DRUG-REHAB-BASIC SECTION.                              ELTSUBST
01097                                                                   ELTSUBST
01098      IF GCT-DRUG-ABUSE-ELIG-MEMB-CLS NOT = ZERO                   ELTSUBST
01099          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01100          MOVE 'DRUG-ABUSE-ELIG-MEMB-CLS'                          ELTSUBST
01101              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01102          MOVE GCT-DRUG-ABUSE-ELIG-MEMB-CLS                        ELTSUBST
01103              TO CMF-CODE-VALUE                                    ELTSUBST
01104          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTSUBST
01105          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01106          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01107          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01108      END-IF.                                                      ELTSUBST
01109                                                                   ELTSUBST
01110  1100-EXIT.            EXIT.                                      ELTSUBST
01111  1130-ELIG-DRUG-REHAB-SUPP  SECTION.                              ELTSUBST
01112                                                                   ELTSUBST
01113        IF GCT-DRUG-ABUSE-ELIG-MEMB-CLS NOT = ZERO                 ELTSUBST
01114          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01115          MOVE 'DRUG-ABUSE-ELIG-MEMB-CLS'                          ELTSUBST
01116              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01117          MOVE GCT-DRUG-ABUSE-ELIG-MEMB-CLS                        ELTSUBST
01118              TO CMF-CODE-VALUE                                    ELTSUBST
01119          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
01120          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01121          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01122          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01123      END-IF.                                                      ELTSUBST
01124  1130-EXIT.            EXIT.                                      ELTSUBST
01125                                                                   ELTSUBST
01126 ******************************************************************ELTSUBST
01127 ** ELIGIBLE ALCOHOL REHABILITATION                              **ELTSUBST
01128 ******************************************************************ELTSUBST
01129  1150-ELIG-ALCOHOL-REHAB-BASIC SECTION.                           ELTSUBST
01130                                                                   ELTSUBST
01131      IF GCT-ALCOH-ABUSE-ELIG-MEMB-CLS NOT = ZERO                  ELTSUBST
01132          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01133          MOVE 'ALCOH-ABUSE-ELIG-MEMB-CLS'                         ELTSUBST
01134              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01135          MOVE GCT-ALCOH-ABUSE-ELIG-MEMB-CLS                       ELTSUBST
01136              TO CMF-CODE-VALUE                                    ELTSUBST
01137          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTSUBST
01138          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01139          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01140          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01141      END-IF.                                                      ELTSUBST
01142                                                                   ELTSUBST
01143  1150-EXIT.            EXIT.                                      ELTSUBST
01144 /                                                                 ELTSUBST
01145  1160-ELIG-ALCOHOL-REHAB-SUPP SECTION.                            ELTSUBST
01146                                                                   ELTSUBST
01147      IF GCT-ALCOH-ABUSE-ELIG-MEMB-CLS NOT = ZERO                  ELTSUBST
01148          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01149          MOVE 'ALCOH-ABUSE-ELIG-MEMB-CLS'                         ELTSUBST
01150              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01151          MOVE GCT-ALCOH-ABUSE-ELIG-MEMB-CLS                       ELTSUBST
01152              TO CMF-CODE-VALUE                                    ELTSUBST
01153          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
01154          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01155          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01156          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01157      END-IF.                                                      ELTSUBST
01158                                                                   ELTSUBST
01159  1160-EXIT.            EXIT.                                      ELTSUBST
01160 /                                                                 ELTSUBST
01161  1500-INSTITUTIONAL-OP-RTNE SECTION.                              ELTSUBST
01162                                                                   ELTSUBST
01163      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSUBST
01164                                                                   ELTSUBST
01165      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
01166      PERFORM WITH TEST BEFORE                                     ELTSUBST
01167              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSUBST
01168              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSUBST
01169         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSUBST
01170         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSUBST
01171         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSUBST
01172         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSUBST
01173      END-PERFORM.                                                 ELTSUBST
01174      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
01175                                                                   ELTSUBST
01176      PERFORM WITH TEST BEFORE                                     ELTSUBST
01177         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTSUBST
01178         UNTIL WS-SUB  >  WS-INST-OP-CNT                           ELTSUBST
01179            SET  PVN-BEN-PROVN-IDX TO WS-SUB                       ELTSUBST
01180            MOVE WS-INST-OP-LIST(WS-SUB)  TO                       ELTSUBST
01181                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX)    ELTSUBST
01182            MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      ELTSUBST
01183                            PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)    ELTSUBST
01184      END-PERFORM.                                                 ELTSUBST
01185                                                                   ELTSUBST
01186                                                                   ELTSUBST
01187      MOVE WS-HDR-2-INST-OP    TO  COF-HDR-LINE(2).                ELTSUBST
01188 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTSUBST
01189      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTSUBST
01190      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTSUBST
01191      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSUBST
01192                                                                   ELTSUBST
01193      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
01194              END-EXEC.                                            ELTSUBST
01195 ****************************************************              ELTSUBST
01196                                                                   ELTSUBST
01197      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTSUBST
01198                                                                   ELTSUBST
01199      IF WS-VALID-INST-OUTPATIENT                                  ELTSUBST
01200         ADD +1 TO WS-CIA                                          ELTSUBST
01201         PERFORM 6000-DSPLY-NTWRK-UTIL-IND.                        ELTSUBST
01202                                                                   ELTSUBST
01203      MOVE WS-TOPIC-PHRASE     TO  SSB-TOPIC-PHRASE.               ELTSUBST
01204                                                                   ELTSUBST
01205      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTSUBST
01206          END-EXEC.                                                ELTSUBST
01207                                                                   ELTSUBST
01208      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSUBST
01209      MOVE ' '  TO  COF-FUNCTION.                                  ELTSUBST
01210      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
01211              END-EXEC.                                            ELTSUBST
01212                                                                   ELTSUBST
01213      IF PVN-COVG-NONE                                             ELTSUBST
01214         GO TO 1599-EXIT.                                          ELTSUBST
01215                                                                   ELTSUBST
01216      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTSUBST
01217            PSP-PROVN-PRICING-METHD,                               ELTSUBST
01218            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSUBST
01219            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSUBST
01220            PSP-TRANSF-OTHER-RESP-IND,                             ELTSUBST
01221            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSUBST
01222            PSP-SPILL-OVER-DED-APL-IND,                            ELTSUBST
01223            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTSUBST
01224            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSUBST
01225            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSUBST
01226            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSUBST
01227            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSUBST
01228            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSUBST
01229            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSUBST
01230            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTSUBST
01231            PSA-DAYS-RDCN-RAT-IND,                                 ELTSUBST
01232            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTSUBST
01233            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTSUBST
01234            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTSUBST
01235            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTSUBST
01236            PSA-FLAT-RATE-PDM-AMT,                                 ELTSUBST
01237            PSA-REHAB-ADM-RESTRN-IND,                              ELTSUBST
01238            PSW-ADDN-ALLOW-AMT-PER-DAY,                            ELTSUBST
01239            PSW-DAYS-RDCN-RAT-BASIC-APL,                           ELTSUBST
01240            PSW-DAYS-RDCN-RAT-BASIC-BASE,                          ELTSUBST
01241            PSW-DAYS-RDCN-RAT-SEC-APL,                             ELTSUBST
01242            PSW-DAYS-RDCN-RAT-SEC-BASE,                            ELTSUBST
01243            PSW-FLAT-RATE-PDM-AMT,                                 ELTSUBST
01244            PSW-REHAB-ADM-RESTRN-IND.                              ELTSUBST
01245                                                                   ELTSUBST
01246      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSUBST
01247                     COMMAREA (DFHCOMMAREA)                        ELTSUBST
01248      END-EXEC.                                                    ELTSUBST
01249                                                                   ELTSUBST
01250      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSUBST
01251      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
01252          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSUBST
01253                                                                   ELTSUBST
01254      PERFORM WITH TEST BEFORE                                     ELTSUBST
01255         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSUBST
01256         UNTIL WS-SUB  >  WS-INST-OP-CNT                           ELTSUBST
01257             SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB          ELTSUBST
01258             IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) NOT = ZERO     ELTSUBST
01259                  PERFORM 1540-BUILD-SCREEN-LINES THRU 1540-EXIT   ELTSUBST
01260             END-IF                                                ELTSUBST
01261      END-PERFORM.                                                 ELTSUBST
01262                                                                   ELTSUBST
01263      GO TO 1599-EXIT.                                             ELTSUBST
01264                                                                   ELTSUBST
01265  1540-BUILD-SCREEN-LINES.                                         ELTSUBST
01266                                                                   ELTSUBST
01267      SET PLT-INDEX1 TO                                            ELTSUBST
01268         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTSUBST
01269                                                                   ELTSUBST
01270      MOVE +1 TO WS-CIA.                                           ELTSUBST
01271      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
01272         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO         ELTSUBST
01273            SET PLT-INDEX2 TO 2                                    ELTSUBST
01274         ELSE                                                      ELTSUBST
01275            PERFORM 2400-PROBLEM-WITH-INDICES                      ELTSUBST
01276            MOVE TABLE-MAX TO WS-SUB                               ELTSUBST
01277            GO TO 1540-EXIT                                        ELTSUBST
01278      ELSE                                                         ELTSUBST
01279         SET PLT-INDEX2 TO 1.                                      ELTSUBST
01280      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTSUBST
01281                                                                   ELTSUBST
01282      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTSUBST
01283      ADD +1                 TO WS-CIA.                            ELTSUBST
01284      MOVE WS-FOLLOWING-BEN  TO COF-DTL-LINE(WS-CIA).              ELTSUBST
01285      ADD +1                 TO WS-CIA.                            ELTSUBST
01286      PERFORM 1550-ZERO-ALL-WITH-SAME-NO                           ELTSUBST
01287         VARYING WS-SUB2 FROM WS-SUB BY +1                         ELTSUBST
01288         UNTIL WS-SUB2 > WS-INST-OP-CNT.                           ELTSUBST
01289                                                                   ELTSUBST
01290      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSUBST
01291                                                                   ELTSUBST
01292                                                                   ELTSUBST
01293      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01294      ADD +1                   TO WS-CIA.                          ELTSUBST
01295      MOVE WS-SERVICES-RENDERED                                    ELTSUBST
01296                               TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01297      ADD +1                   TO WS-CIA.                          ELTSUBST
01298                                                                   ELTSUBST
01299      MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
01300                                                                   ELTSUBST
01301      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01302          PERFORM 4000-PLACE-OF-TREAT-BASIC.                       ELTSUBST
01303                                                                   ELTSUBST
01304      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01305          PERFORM 4050-PLACE-OF-TREAT-SUPP.                        ELTSUBST
01306                                                                   ELTSUBST
01307 ******************************************************************ELTSUBST
01308 ** IF NO DETAIL WAS BUILT FOR EITHER BASIC OR SUPPLEMENTAL,     **ELTSUBST
01309 ** SUPPRESS THE CAPTION LINE.                                   **ELTSUBST
01310 ******************************************************************ELTSUBST
01311      IF WS-NEW-LINES-GENERATED                                    ELTSUBST
01312         MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                    ELTSUBST
01313         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
01314      ELSE                                                         ELTSUBST
01315         ADD -2 TO WS-CIA.                                         ELTSUBST
01316                                                                   ELTSUBST
01317      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSUBST
01318                                                                   ELTSUBST
01319      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01320      ADD +1                   TO WS-CIA.                          ELTSUBST
01321      MOVE WS-PAYABLE-FOR      TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01322      ADD +1                   TO WS-CIA.                          ELTSUBST
01323                                                                   ELTSUBST
01324      MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
01325                                                                   ELTSUBST
01326      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01327         SET PLT-INDEX2 TO 1                                       ELTSUBST
01328         PERFORM 4100-PAYABLE-FOR-BASIC.                           ELTSUBST
01329                                                                   ELTSUBST
01330      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01331         SET PLT-INDEX2 TO 2                                       ELTSUBST
01332         PERFORM 4200-PAYABLE-FOR-SUPP.                            ELTSUBST
01333                                                                   ELTSUBST
01334 ******************************************************************ELTSUBST
01335 ** IF NO DETAIL WAS BUILT FOR EITHER BASIC OR SUPPLEMENTAL,     **ELTSUBST
01336 ** SUPPRESS THE CAPTION LINE.                                   **ELTSUBST
01337 ******************************************************************ELTSUBST
01338      IF WS-NEW-LINES-GENERATED                                    ELTSUBST
01339         MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                    ELTSUBST
01340         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
01341      ELSE                                                         ELTSUBST
01342         ADD -2 TO WS-CIA.                                         ELTSUBST
01343                                                                   ELTSUBST
01344      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01345      ADD +1                   TO WS-CIA.                          ELTSUBST
01346                                                                   ELTSUBST
01347      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01348          IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
01349                  NOT = ZERO AND NOT = LOW-VALUES                  ELTSUBST
01350              MOVE WS-EXCEPT TO WS-VAR-EXCEPT                      ELTSUBST
01351          ELSE                                                     ELTSUBST
01352              MOVE SPACES TO WS-VAR-EXCEPT.                        ELTSUBST
01353                                                                   ELTSUBST
01354      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01355        SET PLT-INDEX2 TO 1                                        ELTSUBST
01356        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01357                     NUMERIC AND                                   ELTSUBST
01358            PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01359                     NOT = ZERO                                    ELTSUBST
01360         IF PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01361                     NUMERIC AND                                   ELTSUBST
01362            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01363                     NOT = ZERO                                    ELTSUBST
01364             MOVE                                                  ELTSUBST
01365              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTSUBST
01366                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
01367             MOVE                                                  ELTSUBST
01368              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTSUBST
01369                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
01370             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
01371             STRING WS-DAYS-REDUCED,                               ELTSUBST
01372                    WS-BASIC-SHORT-LIT,                            ELTSUBST
01373                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
01374                    WS-FOR, ' '                                    ELTSUBST
01375                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
01376                    WS-VAR-EXCEPT,                                 ELTSUBST
01377                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
01378             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
01379             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
01380             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
01381             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
01382             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
01383             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01384             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
01385                                                                   ELTSUBST
01386      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01387          SET PLT-INDEX2 TO 1                                      ELTSUBST
01388          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
01389              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
01390          MOVE 'BPA' TO CMF-RECORD-PREFIX                          ELTSUBST
01391          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
01392                                                                   ELTSUBST
01393      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01394        SET PLT-INDEX2 TO 1                                        ELTSUBST
01395        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01396                       NUMERIC AND                                 ELTSUBST
01397            PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01398                       = ZERO                                      ELTSUBST
01399         IF PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01400                       NUMERIC AND                                 ELTSUBST
01401            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01402                       = ZERO                                      ELTSUBST
01403                              ADD +1 TO WS-CIA.                    ELTSUBST
01404                                                                   ELTSUBST
01405      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01406        SET PLT-INDEX2 TO 1                                        ELTSUBST
01407        IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
01408                     NUMERIC AND                                   ELTSUBST
01409            PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
01410                     NOT = ZERO                                    ELTSUBST
01411         IF PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
01412                     NUMERIC AND                                   ELTSUBST
01413            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
01414                     NOT = ZERO                                    ELTSUBST
01415             MOVE                                                  ELTSUBST
01416              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01417                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
01418             MOVE                                                  ELTSUBST
01419              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01420                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
01421             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
01422             STRING WS-DAYS-REDUCED,                               ELTSUBST
01423                    WS-SECONDARY,                                  ELTSUBST
01424                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
01425                    WS-FOR, ' '                                    ELTSUBST
01426                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
01427                    WS-VAR-EXCEPT,                                 ELTSUBST
01428                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
01429             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
01430             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
01431             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
01432             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
01433             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
01434             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01435             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
01436                                                                   ELTSUBST
01437      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01438          SET PLT-INDEX2 TO 1                                      ELTSUBST
01439          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
01440              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
01441          MOVE 'BPA' TO CMF-RECORD-PREFIX                          ELTSUBST
01442          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
01443                                                                   ELTSUBST
01444      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01445        SET PLT-INDEX2 TO 1                                        ELTSUBST
01446        IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTSUBST
01447                         = '0' OR LOW-VALUES                       ELTSUBST
01448               ADD +1 TO WS-CIA.                                   ELTSUBST
01449                                                                   ELTSUBST
01450      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01451          IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
01452                  NOT = ZERO AND NOT = LOW-VALUES                  ELTSUBST
01453              MOVE WS-EXCEPT TO WS-VAR-EXCEPT                      ELTSUBST
01454          ELSE                                                     ELTSUBST
01455              MOVE SPACES TO WS-VAR-EXCEPT.                        ELTSUBST
01456                                                                   ELTSUBST
01457      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01458        SET PLT-INDEX2 TO 1                                        ELTSUBST
01459        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01460                     NUMERIC AND                                   ELTSUBST
01461            PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01462                     NOT = ZERO                                    ELTSUBST
01463         IF PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01464                     NUMERIC AND                                   ELTSUBST
01465            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01466                     NOT = ZERO                                    ELTSUBST
01467             MOVE                                                  ELTSUBST
01468              PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTSUBST
01469                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
01470             MOVE                                                  ELTSUBST
01471              PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTSUBST
01472                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
01473             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
01474             STRING WS-DAYS-REDUCED,                               ELTSUBST
01475                    WS-BASIC-SHORT-LIT,                            ELTSUBST
01476                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
01477                    WS-FOR, ' '                                    ELTSUBST
01478                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
01479                    WS-VAR-EXCEPT                                  ELTSUBST
01480                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
01481             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
01482             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
01483             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
01484             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
01485             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
01486             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01487             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
01488                                                                   ELTSUBST
01489      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01490          SET PLT-INDEX2 TO 1                                      ELTSUBST
01491          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
01492              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
01493          MOVE 'BPW' TO CMF-RECORD-PREFIX                          ELTSUBST
01494          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
01495                                                                   ELTSUBST
01496      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01497        SET PLT-INDEX2 TO 1                                        ELTSUBST
01498        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01499                         NUMERIC AND                               ELTSUBST
01500            PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01501                         = ZERO                                    ELTSUBST
01502         IF PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01503                         NUMERIC AND                               ELTSUBST
01504            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01505                         = ZERO                                    ELTSUBST
01506                              ADD +1 TO WS-CIA.                    ELTSUBST
01507                                                                   ELTSUBST
01508      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01509        SET PLT-INDEX2 TO 1                                        ELTSUBST
01510        IF  PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
01511                     NUMERIC AND                                   ELTSUBST
01512            PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
01513                     NOT = ZERO                                    ELTSUBST
01514         IF PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
01515                     NUMERIC AND                                   ELTSUBST
01516            PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
01517                     NOT = ZERO                                    ELTSUBST
01518             MOVE                                                  ELTSUBST
01519              PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
01520                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
01521             MOVE                                                  ELTSUBST
01522              PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
01523                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
01524             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
01525             STRING WS-DAYS-REDUCED,                               ELTSUBST
01526                    WS-SECONDARY,                                  ELTSUBST
01527                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
01528                    WS-FOR, ' '                                    ELTSUBST
01529                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
01530                    WS-VAR-EXCEPT                                  ELTSUBST
01531                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
01532             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
01533             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
01534             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
01535             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
01536             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
01537             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01538             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
01539                                                                   ELTSUBST
01540      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01541          SET PLT-INDEX2 TO 1                                      ELTSUBST
01542          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
01543              TO WS-DAYS-RDCN-RAT-IND                              ELTSUBST
01544          MOVE 'BPW' TO CMF-RECORD-PREFIX                          ELTSUBST
01545          PERFORM 4500-DAYS-REDUCTION-RATE.                        ELTSUBST
01546                                                                   ELTSUBST
01547 ******************************************************************ELTSUBST
01548 ** ELIGIBLE DRUG REHABILITATION                                 **ELTSUBST
01549 ******************************************************************ELTSUBST
01550      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR.                       ELTSUBST
01551      IF CIA-RC-PTR-NULL                                           ELTSUBST
01552         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
01553         IF CIA-RC-PTR-NULL                                        ELTSUBST
01554            CONTINUE                                               ELTSUBST
01555      ELSE                                                         ELTSUBST
01556          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01557          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01558          ADD +1                   TO WS-CIA                       ELTSUBST
01559          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
01560                                   WS-DRUG-REHAB                   ELTSUBST
01561                                   DELIMITED BY SIZE               ELTSUBST
01562                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
01563          ADD +1                   TO WS-CIA                       ELTSUBST
01564          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
01565          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01566             PERFORM 1100-ELIG-DRUG-REHAB-BASIC                    ELTSUBST
01567          END-IF                                                   ELTSUBST
01568          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
01569          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01570              PERFORM 1130-ELIG-DRUG-REHAB-SUPP                    ELTSUBST
01571          END-IF                                                   ELTSUBST
01572          IF WS-NEW-LINES-GENERATED                                ELTSUBST
01573              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
01574              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
01575          ELSE                                                     ELTSUBST
01576              ADD -2 TO WS-CIA                                     ELTSUBST
01577          END-IF                                                   ELTSUBST
01578      END-IF.                                                      ELTSUBST
01579                                                                   ELTSUBST
01580 ******************************************************************ELTSUBST
01581 ** ELIGIBLE ALCOHOL REHABILITATION                              **ELTSUBST
01582 ******************************************************************ELTSUBST
01583      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                        ELTSUBST
01584      IF CIA-RC-PTR-NULL                                           ELTSUBST
01585         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
01586         IF CIA-RC-PTR-NULL                                        ELTSUBST
01587            CONTINUE                                               ELTSUBST
01588      ELSE                                                         ELTSUBST
01589          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01590          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01591          ADD +1                   TO WS-CIA                       ELTSUBST
01592          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
01593                                   WS-ALCOHOL-REHAB                ELTSUBST
01594                                   DELIMITED BY SIZE               ELTSUBST
01595                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
01596          ADD +1                   TO WS-CIA                       ELTSUBST
01597          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
01598          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01599             PERFORM 1150-ELIG-ALCOHOL-REHAB-BASIC                 ELTSUBST
01600          END-IF                                                   ELTSUBST
01601          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
01602          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01603             PERFORM 1160-ELIG-ALCOHOL-REHAB-SUPP                  ELTSUBST
01604          END-IF                                                   ELTSUBST
01605          IF WS-NEW-LINES-GENERATED                                ELTSUBST
01606              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
01607              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
01608          ELSE                                                     ELTSUBST
01609              ADD -2 TO WS-CIA                                     ELTSUBST
01610          END-IF                                                   ELTSUBST
01611      END-IF.                                                      ELTSUBST
01612                                                                   ELTSUBST
01613 ******************************************************************ELTSUBST
01614 ** DRUG REHABILITATION PRIOR ADMISSION CODE                     **ELTSUBST
01615 ******************************************************************ELTSUBST
01616      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                        ELTSUBST
01617      IF CIA-RC-PTR-NULL                                           ELTSUBST
01618         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
01619         IF CIA-RC-PTR-NULL                                        ELTSUBST
01620            CONTINUE                                               ELTSUBST
01621      ELSE                                                         ELTSUBST
01622          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01623          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01624          ADD +1                   TO WS-CIA                       ELTSUBST
01625          STRING                   WS-OUTPATIENT                   ELTSUBST
01626                                   WS-ADM-REQ-FOR                  ELTSUBST
01627                                   WS-DRUG-ARE                     ELTSUBST
01628                                   DELIMITED BY SIZE               ELTSUBST
01629                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
01630          ADD +1                   TO WS-CIA                       ELTSUBST
01631          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
01632          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01633              PERFORM 1600-PRIOR-ADM-DRUG-REHAB-BAS                ELTSUBST
01634          END-IF                                                   ELTSUBST
01635          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
01636          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01637              PERFORM 1630-PRIOR-ADM-DRUG-REHAB-SUP                ELTSUBST
01638          END-IF                                                   ELTSUBST
01639          IF WS-NEW-LINES-GENERATED                                ELTSUBST
01640              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
01641              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
01642          ELSE                                                     ELTSUBST
01643              ADD -2 TO WS-CIA                                     ELTSUBST
01644          END-IF                                                   ELTSUBST
01645      END-IF.                                                      ELTSUBST
01646                                                                   ELTSUBST
01647                                                                   ELTSUBST
01648 ******************************************************************ELTSUBST
01649 ** ALCOHOL REHABILITATION PRIOR ADMISSION CODE                  **ELTSUBST
01650 ******************************************************************ELTSUBST
01651      PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                        ELTSUBST
01652      IF CIA-RC-PTR-NULL                                           ELTSUBST
01653         PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                     ELTSUBST
01654         IF CIA-RC-PTR-NULL                                        ELTSUBST
01655            CONTINUE                                               ELTSUBST
01656      ELSE                                                         ELTSUBST
01657          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01658          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
01659          ADD +1                   TO WS-CIA                       ELTSUBST
01660          STRING                   WS-OUTPATIENT                   ELTSUBST
01661                                   WS-ADM-REQ-FOR                  ELTSUBST
01662                                   WS-ALCOHOL-ARE                  ELTSUBST
01663                                   DELIMITED BY SIZE               ELTSUBST
01664                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
01665          ADD +1                   TO WS-CIA                       ELTSUBST
01666          PERFORM 1545-SET-CON-REC-ELSCONIB-PTR                    ELTSUBST
01667          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01668              PERFORM 1650-PRIOR-ADM-ALCHL-REHAB-BAS               ELTSUBST
01669          END-IF                                                   ELTSUBST
01670          PERFORM 1545-SET-CON-REC-ELSCONIS-PTR                    ELTSUBST
01671          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
01672              PERFORM 1670-PRIOR-ADM-ALCHL-REHAB-SUP               ELTSUBST
01673          END-IF                                                   ELTSUBST
01674          IF WS-NEW-LINES-GENERATED                                ELTSUBST
01675              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
01676              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
01677          ELSE                                                     ELTSUBST
01678              ADD -2 TO WS-CIA                                     ELTSUBST
01679          END-IF                                                   ELTSUBST
01680      END-IF.                                                      ELTSUBST
01681                                                                   ELTSUBST
01682                                                                   ELTSUBST
01683 ******************************************************************ELTSUBST
01684 ** SPILLOVER (APPLIES TO SUPPLEMENTAL COVERAGE IF PRESENT)      **ELTSUBST
01685 ******************************************************************ELTSUBST
01686      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01687         SET PLT-INDEX2 TO 2                                       ELTSUBST
01688         PERFORM 4300-SPILLOVER-COINS                              ELTSUBST
01689         PERFORM 4400-SPILLOVER-DEDUCT.                            ELTSUBST
01690                                                                   ELTSUBST
01691                                                                   ELTSUBST
01692      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01693         SET PLT-INDEX2 TO 1                                       ELTSUBST
01694         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
01695                                                                   ELTSUBST
01696      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01697         SET PLT-INDEX2 TO 2                                       ELTSUBST
01698         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
01699                                                                   ELTSUBST
01700      PERFORM 4600-SCAN-TAB.                                       ELTSUBST
01701                                                                   ELTSUBST
01702  1540-EXIT.  EXIT.                                                ELTSUBST
01703 /                                                                 ELTSUBST
01704  1545-SET-CON-REC-ELSCONIB-PTR.                                   ELTSUBST
01705      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTSUBST
01706      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
01707          ADDRESS OF CONTRACT-RECORD.                              ELTSUBST
01708 /                                                                 ELTSUBST
01709  1545-SET-CON-REC-ELSCONIS-PTR.                                   ELTSUBST
01710      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTSUBST
01711      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
01712          ADDRESS OF CONTRACT-RECORD.                              ELTSUBST
01713 /                                                                 ELTSUBST
01714  1550-ZERO-ALL-WITH-SAME-NO.                                      ELTSUBST
01715      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTSUBST
01716                                                                   ELTSUBST
01717      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTSUBST
01718         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
01719         MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME               ELTSUBST
01720         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTSUBST
01721             CMF-CODE-VALUE                                        ELTSUBST
01722         EXEC CICS LINK                                            ELTSUBST
01723              PROGRAM('ELUCMIF')                                   ELTSUBST
01724              COMMAREA(DFHCOMMAREA)                                ELTSUBST
01725              END-EXEC                                             ELTSUBST
01726         PERFORM 4599-SET-CMF-DESCR-ADDR                           ELTSUBST
01727         MOVE SPACES          TO TCAR-FROM-AREA                    ELTSUBST
01728         STRING CMF-DESCR-LINE(1) ' '                              ELTSUBST
01729                CMF-DESCR-LINE(2) ' '                              ELTSUBST
01730                  DELIMITED BY SIZE INTO TCAR-FROM-AREA            ELTSUBST
01731         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTSUBST
01732         MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT           ELTSUBST
01733         MOVE +55             TO TCAR-OUTPUT-FIELD-1-LEN           ELTSUBST
01734         MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN           ELTSUBST
01735         PERFORM TCPR-000-TEXT-UNSTRING                            ELTSUBST
01736         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTSUBST
01737         MOVE WS-SERVICES-2ND TO COF-DTL-LINE(WS-CIA)              ELTSUBST
01738         IF WS-CIA  <  20                                          ELTSUBST
01739            ADD +1 TO WS-CIA                                       ELTSUBST
01740            MOVE ZERO TO                                           ELTSUBST
01741                PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)                ELTSUBST
01742         ELSE                                                      ELTSUBST
01743            EXEC CICS LINK                                         ELTSUBST
01744                 PROGRAM('ELUOUTPT')                               ELTSUBST
01745                 COMMAREA(DFHCOMMAREA)                             ELTSUBST
01746                 END-EXEC                                          ELTSUBST
01747            MOVE +1 TO WS-CIA                                      ELTSUBST
01748            MOVE ZERO TO                                           ELTSUBST
01749                PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).               ELTSUBST
01750                                                                   ELTSUBST
01751  1599-EXIT.            EXIT.                                      ELTSUBST
01752 /                                                                 ELTSUBST
01753 ******************************************************************ELTSUBST
01754 ** DRUG REHABILITATION PRIOR ADMISSION CODE                     **ELTSUBST
01755 ******************************************************************ELTSUBST
01756  1600-PRIOR-ADM-DRUG-REHAB-BAS SECTION.                           ELTSUBST
01757                                                                   ELTSUBST
01758      IF GCT-DRUG-ABUSE-OP-PRI-ADM-CD NOT = ZERO                   ELTSUBST
01759              AND NOT = SPACES AND NOT = LOW-VALUES                ELTSUBST
01760          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01761          MOVE 'DRUG-ABUSE-OP-PRI-ADM-CD'                          ELTSUBST
01762              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01763          MOVE GCT-DRUG-ABUSE-OP-PRI-ADM-CD                        ELTSUBST
01764              TO CMF-CODE-VALUE                                    ELTSUBST
01765          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTSUBST
01766          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01767          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01768          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01769      END-IF.                                                      ELTSUBST
01770                                                                   ELTSUBST
01771  1600-EXIT.            EXIT.                                      ELTSUBST
01772                                                                   ELTSUBST
01773  1630-PRIOR-ADM-DRUG-REHAB-SUP SECTION.                           ELTSUBST
01774                                                                   ELTSUBST
01775      IF GCT-DRUG-ABUSE-OP-PRI-ADM-CD NOT = ZERO                   ELTSUBST
01776              AND NOT = SPACES AND NOT = LOW-VALUES                ELTSUBST
01777          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01778          MOVE 'DRUG-ABUSE-OP-PRI-ADM-CD'                          ELTSUBST
01779              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01780          MOVE GCT-DRUG-ABUSE-OP-PRI-ADM-CD                        ELTSUBST
01781              TO CMF-CODE-VALUE                                    ELTSUBST
01782          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
01783          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01784          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01785          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01786      END-IF.                                                      ELTSUBST
01787                                                                   ELTSUBST
01788  1630-EXIT.            EXIT.                                      ELTSUBST
01789                                                                   ELTSUBST
01790 ******************************************************************ELTSUBST
01791 ** ALCOHOL REHABILITATION PRIOR ADMISSION CODE                  **ELTSUBST
01792 ******************************************************************ELTSUBST
01793  1650-PRIOR-ADM-ALCHL-REHAB-BAS SECTION.                          ELTSUBST
01794                                                                   ELTSUBST
01795      IF GCT-ALCOH-ABUSE-OP-PRI-ADM-CD NOT = ZERO                  ELTSUBST
01796              AND NOT = SPACES AND NOT = LOW-VALUES                ELTSUBST
01797          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01798          MOVE 'ALCOH-ABUSE-OP-PRI-ADM-CD'                         ELTSUBST
01799              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01800          MOVE GCT-ALCOH-ABUSE-OP-PRI-ADM-CD                       ELTSUBST
01801              TO CMF-CODE-VALUE                                    ELTSUBST
01802          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTSUBST
01803          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01804          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01805          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01806      END-IF.                                                      ELTSUBST
01807                                                                   ELTSUBST
01808  1650-EXIT.            EXIT.                                      ELTSUBST
01809  1670-PRIOR-ADM-ALCHL-REHAB-SUP SECTION.                          ELTSUBST
01810                                                                   ELTSUBST
01811      IF GCT-ALCOH-ABUSE-OP-PRI-ADM-CD NOT = ZERO                  ELTSUBST
01812              AND NOT = SPACES AND NOT = LOW-VALUES                ELTSUBST
01813          MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                     ELTSUBST
01814          MOVE 'ALCOH-ABUSE-OP-PRI-ADM-CD'                         ELTSUBST
01815              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
01816          MOVE GCT-ALCOH-ABUSE-OP-PRI-ADM-CD                       ELTSUBST
01817              TO CMF-CODE-VALUE                                    ELTSUBST
01818          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
01819          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
01820          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
01821          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
01822      END-IF.                                                      ELTSUBST
01823                                                                   ELTSUBST
01824  1670-EXIT.            EXIT.                                      ELTSUBST
01825 /                                                                 ELTSUBST
01826 ******************************************************************ELTSUBST
01827 *            P R O F E S S I O N A L   I P   R T N E             *ELTSUBST
01828 *  (THIS PART OF THE PROGRAM WAS CLONED FROM THE SURGERY TOPIC,  *ELTSUBST
01829 *  ELTSURGR.)                                                    *ELTSUBST
01830 *                                                                *ELTSUBST
01831 *          THIS ROUTINE HAS A NUMBER OF STEPS.                   *ELTSUBST
01832 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.          *ELTSUBST
01833 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE  *ELTSUBST
01834 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL*ELTSUBST
01835 *  END OF PAGE AND EXIT THIS ROUTINE.                            *ELTSUBST
01836 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING    *ELTSUBST
01837 *  MODULE.                                                       *ELTSUBST
01838 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A  *ELTSUBST
01839 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                 *ELTSUBST
01840 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.         *ELTSUBST
01841 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE    *ELTSUBST
01842 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                    *ELTSUBST
01843 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR        *ELTSUBST
01844 *  BENEFITS HAVE BEEN DISPLAYED.                                 *ELTSUBST
01845 *                                                                *ELTSUBST
01846 ******************************************************************ELTSUBST
01847  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTSUBST
01848      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTSUBST
01849                                                                   ELTSUBST
01850      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
01851      PERFORM WITH TEST BEFORE                                     ELTSUBST
01852              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTSUBST
01853              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTSUBST
01854         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTSUBST
01855         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTSUBST
01856         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTSUBST
01857         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTSUBST
01858      END-PERFORM.                                                 ELTSUBST
01859      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTSUBST
01860                                                                   ELTSUBST
01861      PERFORM WITH TEST BEFORE                                     ELTSUBST
01862         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTSUBST
01863         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTSUBST
01864            SET  PVN-BEN-PROVN-IDX TO WS-SUB                       ELTSUBST
01865            MOVE WS-PROF-IP-LIST(WS-SUB)  TO                       ELTSUBST
01866                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX)    ELTSUBST
01867            MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      ELTSUBST
01868                            PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)    ELTSUBST
01869      END-PERFORM.                                                 ELTSUBST
01870                                                                   ELTSUBST
01871                                                                   ELTSUBST
01872      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE(2).                ELTSUBST
01873 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTSUBST
01874      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTSUBST
01875      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTSUBST
01876      MOVE 'P'  TO  COF-FUNCTION.                                  ELTSUBST
01877                                                                   ELTSUBST
01878      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
01879              END-EXEC.                                            ELTSUBST
01880 ****************************************************              ELTSUBST
01881                                                                   ELTSUBST
01882      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTSUBST
01883                                                                   ELTSUBST
01884      IF WS-VALID-PROF-INPATIENT                                   ELTSUBST
01885         ADD +1 TO WS-CIA                                          ELTSUBST
01886         PERFORM 6000-DSPLY-NTWRK-UTIL-IND.                        ELTSUBST
01887                                                                   ELTSUBST
01888      MOVE WS-TOPIC-PHRASE     TO  SSB-TOPIC-PHRASE.               ELTSUBST
01889                                                                   ELTSUBST
01890      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTSUBST
01891          END-EXEC.                                                ELTSUBST
01892                                                                   ELTSUBST
01893      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTSUBST
01894      MOVE ' '  TO  COF-FUNCTION.                                  ELTSUBST
01895      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTSUBST
01896              END-EXEC.                                            ELTSUBST
01897                                                                   ELTSUBST
01898      IF PVN-COVG-NONE                                             ELTSUBST
01899         GO TO 2059-EXIT.                                          ELTSUBST
01900                                                                   ELTSUBST
01901      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTSUBST
01902            PSP-PROVN-PRICING-METHD,                               ELTSUBST
01903            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTSUBST
01904            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTSUBST
01905            PSP-TRANSF-OTHER-RESP-IND,                             ELTSUBST
01906            PSP-SPILL-OVER-COINS-APL-IND,                          ELTSUBST
01907            PSP-SPILL-OVER-DED-APL-IND,                            ELTSUBST
01908            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTSUBST
01909            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTSUBST
01910            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTSUBST
01911            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTSUBST
01912            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTSUBST
01913            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTSUBST
01914            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTSUBST
01915            PSD-BEN-SCOPE-ID,                                      ELTSUBST
01916            PSD-MAX-AMT-PER-VISIT,                                 ELTSUBST
01917            PSD-BEN-MAX-VISIT-IND,                                 ELTSUBST
01918            PSD-BEN-MAX-VISIT-DAYS,                                ELTSUBST
01919            PSD-DAYS-RDCN-RAT-BASIC-APL,                           ELTSUBST
01920            PSD-DAYS-RDCN-RAT-BASIC-BASE,                          ELTSUBST
01921            PSD-DAYS-RDCN-RAT-SEC-APL,                             ELTSUBST
01922            PSD-DAYS-RDCN-RAT-SEC-BASE.                            ELTSUBST
01923                                                                   ELTSUBST
01924      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTSUBST
01925                     COMMAREA (DFHCOMMAREA)                        ELTSUBST
01926      END-EXEC.                                                    ELTSUBST
01927                                                                   ELTSUBST
01928      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTSUBST
01929      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
01930          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTSUBST
01931                                                                   ELTSUBST
01932      PERFORM WITH TEST BEFORE                                     ELTSUBST
01933         VARYING WS-SUB  FROM  +1  BY  +1                          ELTSUBST
01934         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTSUBST
01935             SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB          ELTSUBST
01936             IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) NOT = ZERO     ELTSUBST
01937                  PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT   ELTSUBST
01938             END-IF                                                ELTSUBST
01939      END-PERFORM.                                                 ELTSUBST
01940                                                                   ELTSUBST
01941      GO TO 2059-EXIT.                                             ELTSUBST
01942                                                                   ELTSUBST
01943  2040-BUILD-SCREEN-LINES.                                         ELTSUBST
01944                                                                   ELTSUBST
01945      SET PLT-INDEX1 TO                                            ELTSUBST
01946                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTSUBST
01947      IF WS-NOT-FIRST-TIME                                         ELTSUBST
01948         MOVE 'P' TO COF-FUNCTION                                  ELTSUBST
01949         EXEC CICS LINK   PROGRAM('ELUOUTPT')                      ELTSUBST
01950              COMMAREA(DFHCOMMAREA)                                ELTSUBST
01951              END-EXEC                                             ELTSUBST
01952      ELSE                                                         ELTSUBST
01953         MOVE 'N' TO WS-FIRSTTIME-IND.                             ELTSUBST
01954                                                                   ELTSUBST
01955      MOVE +1 TO WS-CIA.                                           ELTSUBST
01956      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
01957         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO         ELTSUBST
01958            SET PLT-INDEX2 TO 2                                    ELTSUBST
01959         ELSE                                                      ELTSUBST
01960            PERFORM 2400-PROBLEM-WITH-INDICES                      ELTSUBST
01961            MOVE TABLE-MAX TO WS-SUB                               ELTSUBST
01962            GO TO 2040-EXIT                                        ELTSUBST
01963      ELSE                                                         ELTSUBST
01964         SET PLT-INDEX2 TO 1.                                      ELTSUBST
01965                                                                   ELTSUBST
01966 **---------------------------------------------------------------+ELTSUBST
01967 **                                                               |ELTSUBST
01968 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTSUBST
01969      MOVE WS-FOLLOWING-BEN TO COF-DTL-LINE(WS-CIA).               ELTSUBST
01970      ADD +1 TO WS-CIA.                                            ELTSUBST
01971      MOVE ZERO TO WS-SUB2.                                        ELTSUBST
01972      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTSUBST
01973      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTSUBST
01974         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTSUBST
01975         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTSUBST
01976      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTSUBST
01977                                                                   ELTSUBST
01978      ADD +1, WS-CIA GIVING COF-NBR-DTL-LINES.                     ELTSUBST
01979      EXEC CICS LINK                                               ELTSUBST
01980           PROGRAM('ELUOUTPT')                                     ELTSUBST
01981           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
01982           END-EXEC.                                               ELTSUBST
01983      MOVE +1 TO WS-CIA.                                           ELTSUBST
01984 **                                                               |ELTSUBST
01985 **---------------------------------------------------------------+ELTSUBST
01986                                                                   ELTSUBST
01987      MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
01988      MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01989      ADD +1                   TO WS-CIA.                          ELTSUBST
01990      MOVE WS-SERVICES-RENDERED                                    ELTSUBST
01991                               TO COF-DTL-LINE(WS-CIA).            ELTSUBST
01992      ADD +1                   TO WS-CIA.                          ELTSUBST
01993                                                                   ELTSUBST
01994      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01995          PERFORM 4000-PLACE-OF-TREAT-BASIC.                       ELTSUBST
01996                                                                   ELTSUBST
01997      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
01998          PERFORM 4050-PLACE-OF-TREAT-SUPP.                        ELTSUBST
01999                                                                   ELTSUBST
02000 ******************************************************************ELTSUBST
02001 ** IF NO DETAIL WAS BUILT FOR EITHER BASIC OR SUPPLEMENTAL,     **ELTSUBST
02002 ** SUPPRESS THE CAPTION LINE.                                   **ELTSUBST
02003 ******************************************************************ELTSUBST
02004      IF WS-NEW-LINES-GENERATED                                    ELTSUBST
02005         MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                    ELTSUBST
02006         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
02007      ELSE                                                         ELTSUBST
02008         ADD -2 TO WS-CIA.                                         ELTSUBST
02009                                                                   ELTSUBST
02010                                                                   ELTSUBST
02011 **---------------------------------------------------------------+ELTSUBST
02012 **                                                               |ELTSUBST
02013 **            B E N E F I T   S C O P E   I D                    |ELTSUBST
02014      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02015      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02016         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSUBST
02017            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTSUBST
02018                                         '0000' AND NOT = '00  '   ELTSUBST
02019               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTSUBST
02020               ADD +1 TO WS-CIA                                    ELTSUBST
02021               MOVE 'Y' TO WS-ADD-A-BLANK-IND.                     ELTSUBST
02022                                                                   ELTSUBST
02023      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02024      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02025         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSUBST
02026            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTSUBST
02027                                         '0000' AND NOT = '00  '   ELTSUBST
02028               MOVE WS-PAYMNT-BASED TO COF-DTL-LINE(WS-CIA)        ELTSUBST
02029               MOVE 'Y' TO WS-ADD-A-BLANK-IND                      ELTSUBST
02030               ADD +1 TO WS-CIA.                                   ELTSUBST
02031                                                                   ELTSUBST
02032      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02033      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02034         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSUBST
02035            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTSUBST
02036                                         '0000' AND NOT = '00  '   ELTSUBST
02037               MOVE 'BPD' TO CMF-RECORD-PREFIX                     ELTSUBST
02038               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTSUBST
02039               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTSUBST
02040                                                    CMF-CODE-VALUE ELTSUBST
02041               MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA              ELTSUBST
02042               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTSUBST
02043               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSUBST
02044                                                                   ELTSUBST
02045      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02046         SET PLT-INDEX2 TO 2                                       ELTSUBST
02047         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTSUBST
02048            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =      ELTSUBST
02049                                         '0000' AND NOT = '00  '   ELTSUBST
02050               MOVE 'BPD' TO CMF-RECORD-PREFIX                     ELTSUBST
02051               MOVE 'BEN-SCOPE-ID' TO CMF-ELEMENT-SYSTEM-NAME      ELTSUBST
02052               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) TO    ELTSUBST
02053                                                    CMF-CODE-VALUE ELTSUBST
02054               MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA               ELTSUBST
02055               MOVE +63 TO WS-TEMP-NOT-USED-CNT                    ELTSUBST
02056               PERFORM 2100-CALL-CODES-MANUAL-LONG.                ELTSUBST
02057                                                                   ELTSUBST
02058      IF WS-ADD-A-BLANK-LINE                                       ELTSUBST
02059         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02060         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTSUBST
02061         MOVE 1 TO WS-CIA                                          ELTSUBST
02062         EXEC CICS LINK                                            ELTSUBST
02063              PROGRAM('ELUOUTPT')                                  ELTSUBST
02064              COMMAREA(DFHCOMMAREA)                                ELTSUBST
02065              END-EXEC.                                            ELTSUBST
02066 **                                                               |ELTSUBST
02067 **---------------------------------------------------------------+ELTSUBST
02068                                                                   ELTSUBST
02069      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
02070         SET PLT-INDEX2 TO 2                                       ELTSUBST
02071      ELSE                                                         ELTSUBST
02072         SET PLT-INDEX2 TO 1.                                      ELTSUBST
02073                                                                   ELTSUBST
02074 **---------------------------------------------------------------+ELTSUBST
02075 **                                                               |ELTSUBST
02076 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTSUBST
02077 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTSUBST
02078 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTSUBST
02079      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02080      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02081         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTSUBST
02082                                                              '19' ELTSUBST
02083         MOVE WS-PAYABLE-FOR TO COF-DTL-LINE(WS-CIA)               ELTSUBST
02084         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02085         ADD +1 TO WS-CIA.                                         ELTSUBST
02086                                                                   ELTSUBST
02087      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02088      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02089         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTSUBST
02090                                                        '19' AND   ELTSUBST
02091         NOT WS-ADD-A-BLANK-LINE                                   ELTSUBST
02092         MOVE WS-PAYABLE-FOR TO COF-DTL-LINE(WS-CIA)               ELTSUBST
02093         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02094         ADD +1 TO WS-CIA.                                         ELTSUBST
02095                                                                   ELTSUBST
02096      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02097      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02098         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTSUBST
02099                            AND                                    ELTSUBST
02100         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02101         SET PLT-INDEX2 TO 2                                       ELTSUBST
02102         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTSUBST
02103                                                             ZERO  ELTSUBST
02104            MOVE WS-CALL-CONTRACT-CODING TO WS-DTL-BASIC           ELTSUBST
02105            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTSUBST
02106            ADD +1 TO WS-CIA.                                      ELTSUBST
02107                                                                   ELTSUBST
02108      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02109      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02110         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTSUBST
02111                            AND                                    ELTSUBST
02112         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
02113         MOVE WS-CALL-CONTRACT-CODING TO WS-DTL-BASIC              ELTSUBST
02114         MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                     ELTSUBST
02115         ADD +1 TO WS-CIA.                                         ELTSUBST
02116                                                                   ELTSUBST
02117      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTSUBST
02118         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02119         SET PLT-INDEX2 TO 2                                       ELTSUBST
02120         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) =      ELTSUBST
02121                                                             ZERO  ELTSUBST
02122            MOVE WS-CALL-CONTRACT-CODING TO WS-DTL-BASIC           ELTSUBST
02123            MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                  ELTSUBST
02124            ADD +1 TO WS-CIA.                                      ELTSUBST
02125                                                                   ELTSUBST
02126      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02127      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02128         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02129                                                           = ZERO  ELTSUBST
02130            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02131                                                           = ZERO  ELTSUBST
02132               MOVE SPACES TO WS-PERCENT-FLD                       ELTSUBST
02133            ELSE                                                   ELTSUBST
02134               MOVE '%' TO WS-PERCENT-SIGN                         ELTSUBST
02135          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02136                                                 TO WS-PERCENTAGE  ELTSUBST
02137         ELSE                                                      ELTSUBST
02138            MOVE '%' TO WS-PERCENT-SIGN                            ELTSUBST
02139          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02140                                                TO WS-PERCENTAGE.  ELTSUBST
02141      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02142         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTSUBST
02143                                             ZERO AND NOT = '19'   ELTSUBST
02144         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
02145         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTSUBST
02146         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTSUBST
02147                                                    CMF-CODE-VALUE ELTSUBST
02148         MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
02149         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTSUBST
02150         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSUBST
02151                                                                   ELTSUBST
02152      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02153      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02154         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2) = ELTSUBST
02155                                                               ZEROELTSUBST
02156            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02157                                                           = ZERO  ELTSUBST
02158               MOVE SPACES TO WS-PERCENT-FLD                       ELTSUBST
02159            ELSE                                                   ELTSUBST
02160               MOVE '%' TO WS-PERCENT-SIGN                         ELTSUBST
02161          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02162                                                 TO WS-PERCENTAGE  ELTSUBST
02163         ELSE                                                      ELTSUBST
02164            MOVE '%' TO WS-PERCENT-SIGN                            ELTSUBST
02165          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTSUBST
02166                                                TO WS-PERCENTAGE.  ELTSUBST
02167      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02168         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) NOT =     ELTSUBST
02169                                             ZERO AND NOT = '19'   ELTSUBST
02170         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
02171         MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME     ELTSUBST
02172         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO   ELTSUBST
02173                                                    CMF-CODE-VALUE ELTSUBST
02174         MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                     ELTSUBST
02175         MOVE +63 TO WS-TEMP-NOT-USED-CNT                          ELTSUBST
02176         PERFORM 2200-CODES-MANUAL-WITH-PERCENT.                   ELTSUBST
02177                                                                   ELTSUBST
02178      IF WS-ADD-A-BLANK-LINE                                       ELTSUBST
02179         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02180         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTSUBST
02181         MOVE 1 TO WS-CIA                                          ELTSUBST
02182         EXEC CICS LINK                                            ELTSUBST
02183              PROGRAM('ELUOUTPT')                                  ELTSUBST
02184              COMMAREA(DFHCOMMAREA)                                ELTSUBST
02185              END-EXEC.                                            ELTSUBST
02186 **                                                               |ELTSUBST
02187 **---------------------------------------------------------------+ELTSUBST
02188                                                                   ELTSUBST
02189      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
02190         SET PLT-INDEX2 TO 2                                       ELTSUBST
02191      ELSE                                                         ELTSUBST
02192         SET PLT-INDEX2 TO 1.                                      ELTSUBST
02193                                                                   ELTSUBST
02194 **---------------------------------------------------------------+ELTSUBST
02195 **                                                               |ELTSUBST
02196 **        M A X I M U M   N U M B E R   O F   V I S I T S        |ELTSUBST
02197 **                                                               |ELTSUBST
02198                                                                   ELTSUBST
02199      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02200      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02201          IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02202                  NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES ELTSUBST
02203              MOVE WS-MAX-VISIT                                    ELTSUBST
02204                  TO COF-DTL-LINE (WS-CIA)                         ELTSUBST
02205              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTSUBST
02206              ADD +1 TO WS-CIA.                                    ELTSUBST
02207                                                                   ELTSUBST
02208      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02209      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02210          IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02211                  NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES ELTSUBST
02212              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTSUBST
02213                  MOVE WS-MAX-VISIT                                ELTSUBST
02214                      TO COF-DTL-LINE (WS-CIA)                     ELTSUBST
02215                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTSUBST
02216                  ADD +1 TO WS-CIA.                                ELTSUBST
02217                                                                   ELTSUBST
02218      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02219      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02220          IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02221                  NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES ELTSUBST
02222              MOVE 'BPD' TO CMF-RECORD-PREFIX                      ELTSUBST
02223              MOVE 'BEN-MAX-VISIT-IND'                             ELTSUBST
02224                  TO CMF-ELEMENT-SYSTEM-NAME                       ELTSUBST
02225              MOVE PLD-BEN-MAX-VISIT-IND                           ELTSUBST
02226                  (PLT-INDEX1, PLT-INDEX2)                         ELTSUBST
02227                  TO CMF-CODE-VALUE                                ELTSUBST
02228              EXEC CICS LINK                                       ELTSUBST
02229                   PROGRAM('ELUCMIF')                              ELTSUBST
02230                   COMMAREA(DFHCOMMAREA)                           ELTSUBST
02231                   END-EXEC                                        ELTSUBST
02232              PERFORM 4599-SET-CMF-DESCR-ADDR                      ELTSUBST
02233              MOVE WS-SUPP-LIT     TO WS-TEMP-TEXT-AREA            ELTSUBST
02234              MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN      ELTSUBST
02235              MOVE SPACES          TO TCAR-FROM-AREA               ELTSUBST
02236              MOVE PLD-BEN-MAX-VISIT-DAYS                          ELTSUBST
02237                  (PLT-INDEX1, PLT-INDEX2)                         ELTSUBST
02238                  TO WS-BEN-MAX-VISIT-DAYS                         ELTSUBST
02239              STRING WS-BEN-MAX-VISIT-DAYS-X ' '                   ELTSUBST
02240                     WS-PER ' '                                    ELTSUBST
02241                     CMF-DESCR-LINE(1) ' '                         ELTSUBST
02242                     CMF-DESCR-LINE(2) ' '                         ELTSUBST
02243                       DELIMITED BY SIZE INTO TCAR-FROM-AREA       ELTSUBST
02244              PERFORM TCPR-000-TEXT-COMPRESSION                    ELTSUBST
02245              MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT      ELTSUBST
02246              MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN      ELTSUBST
02247              MOVE +63             TO TCAR-OUTPUT-FIELD-2-LEN      ELTSUBST
02248              PERFORM TCPR-000-TEXT-UNSTRING                       ELTSUBST
02249              MOVE +16             TO WS-TEMP-NOT-USED-CNT         ELTSUBST
02250              PERFORM 2120-CONCAT-TO-TEMP-TEXT                     ELTSUBST
02251                  VARYING WS-SUB1 FROM 1 BY 1                      ELTSUBST
02252                  UNTIL WS-TEMP-NOT-USED-CNT > +78                 ELTSUBST
02253              ADD +1 TO WS-CIA                                     ELTSUBST
02254              MOVE TCAR-OPF-DATA(2) TO WS-TRUNC-TEXT               ELTSUBST
02255              MOVE WS-BLANK-PREFIX-DET-LINE                        ELTSUBST
02256                                   TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02257              ADD +1 TO WS-CIA.                                    ELTSUBST
02258                                                                   ELTSUBST
02259      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02260      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02261          IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02262                  NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES ELTSUBST
02263               MOVE 'BPD' TO CMF-RECORD-PREFIX                     ELTSUBST
02264               MOVE 'BEN-MAX-VISIT-IND'                            ELTSUBST
02265                   TO CMF-ELEMENT-SYSTEM-NAME                      ELTSUBST
02266               MOVE PLD-BEN-MAX-VISIT-IND                          ELTSUBST
02267                  (PLT-INDEX1, PLT-INDEX2)                         ELTSUBST
02268                  TO CMF-CODE-VALUE                                ELTSUBST
02269               EXEC CICS LINK                                      ELTSUBST
02270                    PROGRAM('ELUCMIF')                             ELTSUBST
02271                    COMMAREA(DFHCOMMAREA)                          ELTSUBST
02272                    END-EXEC                                       ELTSUBST
02273               PERFORM 4599-SET-CMF-DESCR-ADDR                     ELTSUBST
02274               MOVE WS-SUPP-LIT     TO WS-TEMP-TEXT-AREA           ELTSUBST
02275               MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN     ELTSUBST
02276               MOVE SPACES          TO TCAR-FROM-AREA              ELTSUBST
02277               MOVE PLD-BEN-MAX-VISIT-DAYS                         ELTSUBST
02278                   (PLT-INDEX1, PLT-INDEX2)                        ELTSUBST
02279                   TO WS-BEN-MAX-VISIT-DAYS                        ELTSUBST
02280               STRING WS-BEN-MAX-VISIT-DAYS-X ' '                  ELTSUBST
02281                      WS-PER ' '                                   ELTSUBST
02282                      CMF-DESCR-LINE(1) ' '                        ELTSUBST
02283                      CMF-DESCR-LINE(2) ' '                        ELTSUBST
02284                        DELIMITED BY SIZE INTO TCAR-FROM-AREA      ELTSUBST
02285               PERFORM TCPR-000-TEXT-COMPRESSION                   ELTSUBST
02286               MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT     ELTSUBST
02287               MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN     ELTSUBST
02288               MOVE +63             TO TCAR-OUTPUT-FIELD-2-LEN     ELTSUBST
02289               PERFORM TCPR-000-TEXT-UNSTRING                      ELTSUBST
02290               MOVE +16             TO WS-TEMP-NOT-USED-CNT        ELTSUBST
02291               PERFORM 2120-CONCAT-TO-TEMP-TEXT                    ELTSUBST
02292                   VARYING WS-SUB1 FROM 1 BY 1                     ELTSUBST
02293                   UNTIL WS-TEMP-NOT-USED-CNT > +78                ELTSUBST
02294               ADD +1 TO WS-CIA                                    ELTSUBST
02295               MOVE TCAR-OPF-DATA(2) TO WS-TRUNC-TEXT              ELTSUBST
02296               MOVE WS-BLANK-PREFIX-DET-LINE                       ELTSUBST
02297                                     TO COF-DTL-LINE(WS-CIA)       ELTSUBST
02298               ADD +1 TO WS-CIA.                                   ELTSUBST
02299                                                                   ELTSUBST
02300      IF WS-ADD-A-BLANK-LINE                                       ELTSUBST
02301         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02302         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTSUBST
02303         MOVE 1 TO WS-CIA                                          ELTSUBST
02304         EXEC CICS LINK                                            ELTSUBST
02305              PROGRAM('ELUOUTPT')                                  ELTSUBST
02306              COMMAREA(DFHCOMMAREA)                                ELTSUBST
02307              END-EXEC.                                            ELTSUBST
02308                                                                   ELTSUBST
02309 **                                                               |ELTSUBST
02310 **---------------------------------------------------------------+ELTSUBST
02311                                                                   ELTSUBST
02312 **---------------------------------------------------------------+ELTSUBST
02313 **                                                               |ELTSUBST
02314 **        M A X I M U M   A M O U N T   E L I G I B L E          |ELTSUBST
02315 **                      P E R   V I S I T                        |ELTSUBST
02316 **                                                               |ELTSUBST
02317                                                                   ELTSUBST
02318      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02319      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02320          IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02321                  NUMERIC AND                                      ELTSUBST
02322             PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02323                  NOT = ZERO                                       ELTSUBST
02324              MOVE WS-MAX-PER-VISIT                                ELTSUBST
02325                  TO COF-DTL-LINE (WS-CIA)                         ELTSUBST
02326              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTSUBST
02327              ADD +1 TO WS-CIA.                                    ELTSUBST
02328                                                                   ELTSUBST
02329      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02330      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02331          IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02332                  NUMERIC AND                                      ELTSUBST
02333             PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02334                  NOT = ZERO                                       ELTSUBST
02335              IF WS-ADD-A-BLANK-IND NOT = 'Y'                      ELTSUBST
02336                  MOVE WS-MAX-PER-VISIT                            ELTSUBST
02337                      TO COF-DTL-LINE (WS-CIA)                     ELTSUBST
02338                  MOVE 'Y' TO WS-ADD-A-BLANK-IND                   ELTSUBST
02339                  ADD +1 TO WS-CIA.                                ELTSUBST
02340                                                                   ELTSUBST
02341      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02342      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02343          IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02344                  NUMERIC AND                                      ELTSUBST
02345             PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02346                  NOT = ZERO                                       ELTSUBST
02347              MOVE SPACES TO WS-DTL-BASIC                          ELTSUBST
02348              MOVE PLD-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02349                  TO WS-DTL-BAS-NUM-3                              ELTSUBST
02350              MOVE WS-BASIC TO COF-DTL-LINE (WS-CIA)               ELTSUBST
02351              MOVE SPACES TO WS-DTL-BASIC                          ELTSUBST
02352              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTSUBST
02353              ADD +1 TO WS-CIA.                                    ELTSUBST
02354                                                                   ELTSUBST
02355      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02356      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02357          IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02358                  NUMERIC AND                                      ELTSUBST
02359             PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02360                  NOT = ZERO                                       ELTSUBST
02361              MOVE SPACES TO WS-DTL-SUPPLEMENTAL                   ELTSUBST
02362              MOVE PLD-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02363                  TO WS-DTL-SUP-NUM-3                              ELTSUBST
02364              MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE (WS-CIA)        ELTSUBST
02365              MOVE SPACES TO WS-DTL-SUPPLEMENTAL                   ELTSUBST
02366              MOVE 'Y' TO WS-ADD-A-BLANK-IND                       ELTSUBST
02367              ADD +1 TO WS-CIA.                                    ELTSUBST
02368                                                                   ELTSUBST
02369      IF WS-ADD-A-BLANK-LINE                                       ELTSUBST
02370         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02371         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTSUBST
02372         MOVE 1 TO WS-CIA                                          ELTSUBST
02373         EXEC CICS LINK                                            ELTSUBST
02374              PROGRAM('ELUOUTPT')                                  ELTSUBST
02375              COMMAREA(DFHCOMMAREA)                                ELTSUBST
02376              END-EXEC.                                            ELTSUBST
02377                                                                   ELTSUBST
02378 **                                                               |ELTSUBST
02379 **---------------------------------------------------------------+ELTSUBST
02380                                                                   ELTSUBST
02381 **---------------------------------------------------------------+ELTSUBST
02382 **                                                               |ELTSUBST
02383 **          V I S I T   R E D U C T I O N   R A T I O            |ELTSUBST
02384 **                                                               |ELTSUBST
02385                                                                   ELTSUBST
02386      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02387        SET PLT-INDEX2 TO 1                                        ELTSUBST
02388        IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTSUBST
02389                         = '0' OR LOW-VALUES                       ELTSUBST
02390               ADD +1 TO WS-CIA.                                   ELTSUBST
02391                                                                   ELTSUBST
02392      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02393          IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
02394                  NOT = ZERO AND NOT = LOW-VALUES                  ELTSUBST
02395              MOVE WS-EXCEPT TO WS-VAR-EXCEPT                      ELTSUBST
02396          ELSE                                                     ELTSUBST
02397              MOVE SPACES TO WS-VAR-EXCEPT.                        ELTSUBST
02398                                                                   ELTSUBST
02399      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02400        SET PLT-INDEX2 TO 1                                        ELTSUBST
02401        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02402                     NUMERIC AND                                   ELTSUBST
02403            PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02404                     NOT = ZERO                                    ELTSUBST
02405         IF PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02406                     NUMERIC AND                                   ELTSUBST
02407            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02408                     NOT = ZERO                                    ELTSUBST
02409             MOVE                                                  ELTSUBST
02410              PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTSUBST
02411                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
02412             MOVE                                                  ELTSUBST
02413              PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTSUBST
02414                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
02415             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
02416             STRING WS-VISIT-REDUCT-RATE-BASIC,                    ELTSUBST
02417                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
02418                    WS-FOR, ' '                                    ELTSUBST
02419                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
02420                    WS-VAR-EXCEPT                                  ELTSUBST
02421                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
02422             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
02423             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
02424             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
02425             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
02426             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
02427             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02428             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
02429                                                                   ELTSUBST
02430      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02431        SET PLT-INDEX2 TO 1                                        ELTSUBST
02432        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02433                         NUMERIC AND                               ELTSUBST
02434            PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02435                         = ZERO                                    ELTSUBST
02436         IF PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02437                         NUMERIC AND                               ELTSUBST
02438            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02439                         = ZERO                                    ELTSUBST
02440                              ADD +1 TO WS-CIA.                    ELTSUBST
02441                                                                   ELTSUBST
02442      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02443        SET PLT-INDEX2 TO 1                                        ELTSUBST
02444        IF  PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
02445                     NUMERIC AND                                   ELTSUBST
02446            PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTSUBST
02447                     NOT = ZERO                                    ELTSUBST
02448         IF PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
02449                     NUMERIC AND                                   ELTSUBST
02450            PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTSUBST
02451                     NOT = ZERO                                    ELTSUBST
02452             MOVE                                                  ELTSUBST
02453              PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02454                    TO WS-DTL-DAYS-REDUCED-APL                     ELTSUBST
02455             MOVE                                                  ELTSUBST
02456              PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTSUBST
02457                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTSUBST
02458             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
02459             STRING WS-VISIT-REDUCT-RATE-SEC,                      ELTSUBST
02460                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTSUBST
02461                    WS-FOR, ' '                                    ELTSUBST
02462                    WS-DTL-DAYS-REDUCED-BASE, ' '                  ELTSUBST
02463                    WS-VAR-EXCEPT                                  ELTSUBST
02464                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
02465             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
02466             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
02467             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
02468             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
02469             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
02470             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02471             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
02472                                                                   ELTSUBST
02473      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02474        SET PLT-INDEX2 TO 1                                        ELTSUBST
02475        IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTSUBST
02476                     NOT = ZERO                                    ELTSUBST
02477         IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTSUBST
02478                     NOT = LOW-VALUES                              ELTSUBST
02479          ADD +1                   TO WS-CIA                       ELTSUBST
02480          MOVE 'BPD'              TO CMF-RECORD-PREFIX             ELTSUBST
02481          MOVE 'DAYS-RDCN-RAT-IND' TO CMF-ELEMENT-SYSTEM-NAME      ELTSUBST
02482          MOVE PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
02483                                  TO CMF-CODE-VALUE                ELTSUBST
02484          EXEC CICS LINK                                           ELTSUBST
02485               PROGRAM('ELUCMIF')                                  ELTSUBST
02486               COMMAREA(DFHCOMMAREA)                               ELTSUBST
02487               END-EXEC                                            ELTSUBST
02488          PERFORM 4599-SET-CMF-DESCR-ADDR                          ELTSUBST
02489          MOVE SPACES          TO TCAR-FROM-AREA                   ELTSUBST
02490          STRING CMF-DESCR-LINE(1) ' '                             ELTSUBST
02491                 CMF-DESCR-LINE(2) ' '                             ELTSUBST
02492                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTSUBST
02493          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTSUBST
02494          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTSUBST
02495          MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN          ELTSUBST
02496          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTSUBST
02497          PERFORM TCPR-000-TEXT-UNSTRING                           ELTSUBST
02498          MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)            ELTSUBST
02499          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTSUBST
02500             ADD +1                TO WS-CIA                       ELTSUBST
02501             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02502             PERFORM 3000-OUTPUT-TEXT                              ELTSUBST
02503          ELSE                                                     ELTSUBST
02504           PERFORM 3000-OUTPUT-TEXT.                               ELTSUBST
02505 **                                                               |ELTSUBST
02506 **---------------------------------------------------------------+ELTSUBST
02507                                                                   ELTSUBST
02508                                                                   ELTSUBST
02509 ******************************************************************ELTSUBST
02510 ** ELIGIBLE DRUG REHABILITATION                                 **ELTSUBST
02511 ******************************************************************ELTSUBST
02512                                                                   ELTSUBST
02513      PERFORM 2045-SET-CON-REC-ELSCONPB-PTR.                       ELTSUBST
02514      IF CIA-RC-PTR-NULL                                           ELTSUBST
02515         PERFORM 2045-SET-CON-REC-ELSCONPS-PTR                     ELTSUBST
02516         IF CIA-RC-PTR-NULL                                        ELTSUBST
02517            CONTINUE                                               ELTSUBST
02518      ELSE                                                         ELTSUBST
02519          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
02520          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02521          ADD +1                   TO WS-CIA                       ELTSUBST
02522          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
02523                                   WS-DRUG-REHAB                   ELTSUBST
02524                                   DELIMITED BY SIZE               ELTSUBST
02525                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
02526          ADD +1                   TO WS-CIA                       ELTSUBST
02527          PERFORM 2045-SET-CON-REC-ELSCONPB-PTR                    ELTSUBST
02528          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
02529              PERFORM 1100-ELIG-DRUG-REHAB-BASIC                   ELTSUBST
02530          END-IF                                                   ELTSUBST
02531          PERFORM 2045-SET-CON-REC-ELSCONPS-PTR                    ELTSUBST
02532          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
02533              PERFORM 1130-ELIG-DRUG-REHAB-SUPP                    ELTSUBST
02534          END-IF                                                   ELTSUBST
02535          IF WS-NEW-LINES-GENERATED                                ELTSUBST
02536              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
02537              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
02538          ELSE                                                     ELTSUBST
02539              ADD -2 TO WS-CIA                                     ELTSUBST
02540          END-IF                                                   ELTSUBST
02541      END-IF.                                                      ELTSUBST
02542                                                                   ELTSUBST
02543 ******************************************************************ELTSUBST
02544 ** ELIGIBLE ALCOHOL REHABILITATION                              **ELTSUBST
02545 ******************************************************************ELTSUBST
02546      PERFORM 2045-SET-CON-REC-ELSCONPB-PTR.                       ELTSUBST
02547      IF CIA-RC-PTR-NULL                                           ELTSUBST
02548         PERFORM 2045-SET-CON-REC-ELSCONPS-PTR                     ELTSUBST
02549         IF CIA-RC-PTR-NULL                                        ELTSUBST
02550            CONTINUE                                               ELTSUBST
02551      ELSE                                                         ELTSUBST
02552          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND                   ELTSUBST
02553          MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02554          ADD +1                   TO WS-CIA                       ELTSUBST
02555          STRING                   WS-ELIG-MEMBERS-FOR             ELTSUBST
02556                                   WS-ALCOHOL-REHAB                ELTSUBST
02557                                   DELIMITED BY SIZE               ELTSUBST
02558                                   INTO COF-DTL-LINE(WS-CIA)       ELTSUBST
02559          ADD +1                   TO WS-CIA                       ELTSUBST
02560          PERFORM 2045-SET-CON-REC-ELSCONPB-PTR                    ELTSUBST
02561          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
02562              PERFORM 1150-ELIG-ALCOHOL-REHAB-BASIC                ELTSUBST
02563          END-IF                                                   ELTSUBST
02564          PERFORM 2045-SET-CON-REC-ELSCONPS-PTR                    ELTSUBST
02565          IF NOT CIA-RC-PTR-NULL                                   ELTSUBST
02566              PERFORM 1160-ELIG-ALCOHOL-REHAB-SUPP                 ELTSUBST
02567          END-IF                                                   ELTSUBST
02568          IF WS-NEW-LINES-GENERATED                                ELTSUBST
02569              MOVE 'N' TO WS-NEW-LINES-GENERATED-IND               ELTSUBST
02570              PERFORM 3000-OUTPUT-TEXT                             ELTSUBST
02571          ELSE                                                     ELTSUBST
02572              ADD -2 TO WS-CIA                                     ELTSUBST
02573          END-IF                                                   ELTSUBST
02574      END-IF.                                                      ELTSUBST
02575                                                                   ELTSUBST
02576 **---------------------------------------------------------------+ELTSUBST
02577 **                                                               |ELTSUBST
02578 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTSUBST
02579      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02580      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02581         PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSUBST
02582                                                        NOT = '0'  ELTSUBST
02583         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
02584         MOVE 'SPILL-OVER-COINS-APL-IND' TO                        ELTSUBST
02585                                           CMF-ELEMENT-SYSTEM-NAME ELTSUBST
02586         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTSUBST
02587                                               TO CMF-CODE-VALUE   ELTSUBST
02588         MOVE WS-SPILLOVER TO WS-TEMP-TEXT-AREA                    ELTSUBST
02589         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02590         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSUBST
02591 **                                                               |ELTSUBST
02592 **---------------------------------------------------------------+ELTSUBST
02593                                                                   ELTSUBST
02594 **---------------------------------------------------------------+ELTSUBST
02595 **                                                               |ELTSUBST
02596 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTSUBST
02597      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02598      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTSUBST
02599         PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSUBST
02600                                                        NOT = '0'  ELTSUBST
02601         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
02602         MOVE 'SPILL-OVER-DED-APL-IND' TO                          ELTSUBST
02603                                           CMF-ELEMENT-SYSTEM-NAME ELTSUBST
02604         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTSUBST
02605                                                TO CMF-CODE-VALUE  ELTSUBST
02606         MOVE WS-SPILLOVER TO WS-TEMP-TEXT-AREA                    ELTSUBST
02607         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02608         PERFORM 2100-CALL-CODES-MANUAL-LONG.                      ELTSUBST
02609 **                                                               |ELTSUBST
02610 **---------------------------------------------------------------+ELTSUBST
02611                                                                   ELTSUBST
02612      IF WS-ADD-A-BLANK-LINE                                       ELTSUBST
02613         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTSUBST
02614         ADD  1, WS-CIA GIVING COF-NBR-DTL-LINES                   ELTSUBST
02615         MOVE 1 TO WS-CIA                                          ELTSUBST
02616         EXEC CICS LINK                                            ELTSUBST
02617             PROGRAM('ELUOUTPT')                                   ELTSUBST
02618             COMMAREA(DFHCOMMAREA)                                 ELTSUBST
02619             END-EXEC.                                             ELTSUBST
02620                                                                   ELTSUBST
02621                                                                   ELTSUBST
02622      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02623         SET PLT-INDEX2 TO 1                                       ELTSUBST
02624         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
02625                                                                   ELTSUBST
02626      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTSUBST
02627         SET PLT-INDEX2 TO 2                                       ELTSUBST
02628         PERFORM 3900-TRANSF-OTHER-RESP-IND.                       ELTSUBST
02629                                                                   ELTSUBST
02630      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO                ELTSUBST
02631         SET PLT-INDEX2 TO 2                                       ELTSUBST
02632      ELSE                                                         ELTSUBST
02633         SET PLT-INDEX2 TO 1.                                      ELTSUBST
02634                                                                   ELTSUBST
02635      PERFORM 4600-SCAN-TAB.                                       ELTSUBST
02636                                                                   ELTSUBST
02637  2040-EXIT.  EXIT.                                                ELTSUBST
02638 /                                                                 ELTSUBST
02639                                                                   ELTSUBST
02640  2045-SET-CON-REC-ELSCONPB-PTR.                                   ELTSUBST
02641      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTSUBST
02642      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
02643          ADDRESS OF CONTRACT-RECORD.                              ELTSUBST
02644 /                                                                 ELTSUBST
02645  2045-SET-CON-REC-ELSCONPS-PTR.                                   ELTSUBST
02646      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTSUBST
02647      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
02648          ADDRESS OF CONTRACT-RECORD.                              ELTSUBST
02649 /                                                                 ELTSUBST
02650  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTSUBST
02651                                                                   ELTSUBST
02652      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTSUBST
02653         MOVE 'BP' TO CMF-RECORD-PREFIX                            ELTSUBST
02654         MOVE 'BEN-PR-ID' TO CMF-ELEMENT-SYSTEM-NAME               ELTSUBST
02655         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTSUBST
02656                                                    CMF-CODE-VALUE ELTSUBST
02657         MOVE SPACES TO WS-TEMP-TEXT-AREA                          ELTSUBST
02658         MOVE +58 TO WS-TEMP-NOT-USED-CNT                          ELTSUBST
02659         PERFORM 2100-CALL-CODES-MANUAL-LONG                       ELTSUBST
02660         MOVE ZERO TO PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)          ELTSUBST
02661         ADD  1 TO WS-SUB2                                         ELTSUBST
02662         IF WS-CIA > 20 OR = 20                                    ELTSUBST
02663            MOVE WS-CIA TO COF-NBR-DTL-LINES                       ELTSUBST
02664            EXEC CICS LINK                                         ELTSUBST
02665                 PROGRAM('ELUOUTPT')                               ELTSUBST
02666                 COMMAREA(DFHCOMMAREA)                             ELTSUBST
02667                 END-EXEC                                          ELTSUBST
02668            MOVE +1 TO WS-CIA.                                     ELTSUBST
02669                                                                   ELTSUBST
02670  2059-EXIT.            EXIT.                                      ELTSUBST
02671 /                                                                 ELTSUBST
02672 ******************************************************************ELTSUBST
02673 ** INTERFACE TO CODES MANUAL DATABASE FOR LONG DESCRIPTION      **ELTSUBST
02674 ******************************************************************ELTSUBST
02675  2100-CALL-CODES-MANUAL-LONG SECTION.                             ELTSUBST
02676                                                                   ELTSUBST
02677      INITIALIZE CMF-RETURN-CODE                                   ELTSUBST
02678                 TCAR-FROM-AREA.                                   ELTSUBST
02679                                                                   ELTSUBST
02680      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTSUBST
02681                       COMMAREA(DFHCOMMAREA)                       ELTSUBST
02682      END-EXEC.                                                    ELTSUBST
02683                                                                   ELTSUBST
02684      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
02685                                                                   ELTSUBST
02686      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTSUBST
02687         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTSUBST
02688         STRING WS-TEMP-TEXT-AREA, ' ',                            ELTSUBST
02689            CMF-DESCR-LINE(1), ' ',                                ELTSUBST
02690            CMF-DESCR-LINE(2), ' ',                                ELTSUBST
02691            CMF-DESCR-LINE(3)                                      ELTSUBST
02692            DELIMITED BY SIZE INTO TCAR-FROM-AREA                  ELTSUBST
02693      ELSE                                                         ELTSUBST
02694         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTSUBST
02695         STRING CMF-DESCR-LINE(1), ' ',                            ELTSUBST
02696            CMF-DESCR-LINE(2), ' ',                                ELTSUBST
02697            CMF-DESCR-LINE(3)                                      ELTSUBST
02698            DELIMITED BY SIZE INTO TCAR-FROM-AREA.                 ELTSUBST
02699                                                                   ELTSUBST
02700      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSUBST
02701                                                                   ELTSUBST
02702      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTSUBST
02703      MOVE +2 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTSUBST
02704 ******************************************************************ELTSUBST
02705 ** TEMPORARY FIX BY ALIDA TO IMPROVE APPEARANCE OF OUTPUT       **ELTSUBST
02706 ******************************************************************ELTSUBST
02707 *    MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTSUBST
02708      MOVE +63 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTSUBST
02709      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSUBST
02710                                                                   ELTSUBST
02711      IF WS-MOVE-LINES-TO-CIA                                      ELTSUBST
02712         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTSUBST
02713            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTSUBST
02714                                             WS-TEMP-NOT-USED-CNT  ELTSUBST
02715            PERFORM  2120-CONCAT-TO-TEMP-TEXT                      ELTSUBST
02716               VARYING  WS-SUB1 FROM 1 BY 1                        ELTSUBST
02717               UNTIL  WS-TEMP-NOT-USED-CNT > +78                   ELTSUBST
02718            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTSUBST
02719            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02720            ADD +1 TO WS-CIA                                       ELTSUBST
02721         ELSE                                                      ELTSUBST
02722            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)          ELTSUBST
02723            ADD +1 TO WS-CIA.                                      ELTSUBST
02724                                                                   ELTSUBST
02725      IF WS-MOVE-LINES-TO-CIA                                      ELTSUBST
02726         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTSUBST
02727 ******************************************************************ELTSUBST
02728 ** TEMPORARY FIX BY ALIDA TO IMPROVE APPEARANCE OF OUTPUT       **ELTSUBST
02729 ******************************************************************ELTSUBST
02730 *          MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTSUBST
02731            MOVE TCAR-OPF-DATA(2) TO WS-TRUNC-TEXT                 ELTSUBST
02732            MOVE WS-BLANK-PREFIX-DET-LINE                          ELTSUBST
02733                                  TO COF-DTL-LINE(WS-CIA)          ELTSUBST
02734            ADD +1 TO WS-CIA                                       ELTSUBST
02735         ELSE                                                      ELTSUBST
02736            NEXT SENTENCE                                          ELTSUBST
02737      ELSE                                                         ELTSUBST
02738         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTSUBST
02739                                                                   ELTSUBST
02740      GO TO 2100-EXIT.                                             ELTSUBST
02741                                                                   ELTSUBST
02742  2100-EXIT.                                                       ELTSUBST
02743      EXIT.                                                        ELTSUBST
02744                                                                   ELTSUBST
02745  2120-CONCAT-TO-TEMP-TEXT SECTION.                                ELTSUBST
02746      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTSUBST
02747      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTSUBST
02748          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT).                 ELTSUBST
02749                                                                   ELTSUBST
02750  2120-EXIT.                                                       ELTSUBST
02751      EXIT.                                                        ELTSUBST
02752 /                                                                 ELTSUBST
02753 ******************************************************************ELTSUBST
02754 ** INTERFACE TO CODES MANUAL DATABASE FOR LONG DESCRIPTION      **ELTSUBST
02755 ** (PERCENTAGE FIELD)                                           **ELTSUBST
02756 ******************************************************************ELTSUBST
02757  2200-CODES-MANUAL-WITH-PERCENT SECTION.                          ELTSUBST
02758                                                                   ELTSUBST
02759      INITIALIZE CMF-RETURN-CODE                                   ELTSUBST
02760                 TCAR-FROM-AREA.                                   ELTSUBST
02761                                                                   ELTSUBST
02762      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTSUBST
02763                       COMMAREA(DFHCOMMAREA)                       ELTSUBST
02764      END-EXEC.                                                    ELTSUBST
02765                                                                   ELTSUBST
02766      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
02767                                                                   ELTSUBST
02768      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTSUBST
02769         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTSUBST
02770         STRING WS-TEMP-TEXT-AREA, ' ',                            ELTSUBST
02771            CMF-DESCR-LINE(1), ' ',                                ELTSUBST
02772            CMF-DESCR-LINE(2), ' ',                                ELTSUBST
02773            CMF-DESCR-LINE(3), ' ', WS-PERCENT-FLD                 ELTSUBST
02774            DELIMITED BY SIZE INTO TCAR-FROM-AREA                  ELTSUBST
02775      ELSE                                                         ELTSUBST
02776         MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN      ELTSUBST
02777         STRING CMF-DESCR-LINE(1), ' ',                            ELTSUBST
02778            CMF-DESCR-LINE(2), ' ',                                ELTSUBST
02779            CMF-DESCR-LINE(3), ' ',        WS-PERCENT-FLD          ELTSUBST
02780            DELIMITED BY SIZE INTO TCAR-FROM-AREA.                 ELTSUBST
02781                                                                   ELTSUBST
02782      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSUBST
02783                                                                   ELTSUBST
02784      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTSUBST
02785      MOVE +4 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTSUBST
02786      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN,                         ELTSUBST
02787                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTSUBST
02788                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTSUBST
02789      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSUBST
02790                                                                   ELTSUBST
02791      IF WS-MOVE-LINES-TO-CIA                                      ELTSUBST
02792         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTSUBST
02793            COMPUTE WS-TEMP-NOT-USED-CNT = 79    -                 ELTSUBST
02794                                             WS-TEMP-NOT-USED-CNT  ELTSUBST
02795            PERFORM  2120-CONCAT-TO-TEMP-TEXT                      ELTSUBST
02796               VARYING  WS-SUB1 FROM 1 BY 1                        ELTSUBST
02797               UNTIL  WS-TEMP-NOT-USED-CNT > +78                   ELTSUBST
02798            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTSUBST
02799            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02800            ADD +1 TO WS-CIA                                       ELTSUBST
02801         ELSE                                                      ELTSUBST
02802            MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)          ELTSUBST
02803            ADD +1 TO WS-CIA.                                      ELTSUBST
02804                                                                   ELTSUBST
02805      IF WS-MOVE-LINES-TO-CIA                                      ELTSUBST
02806         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTSUBST
02807            PERFORM 2210-MOVE-LINES-TO-CIA                         ELTSUBST
02808               VARYING  WS-SUB1 FROM 2 BY 1                        ELTSUBST
02809               UNTIL  WS-SUB1 > TCAR-OUTPUT-FIELDS-USED            ELTSUBST
02810         ELSE                                                      ELTSUBST
02811            NEXT SENTENCE                                          ELTSUBST
02812      ELSE                                                         ELTSUBST
02813         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTSUBST
02814                                                                   ELTSUBST
02815      GO TO 2220-EXIT.                                             ELTSUBST
02816                                                                   ELTSUBST
02817  2210-MOVE-LINES-TO-CIA.                                          ELTSUBST
02818      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTSUBST
02819      ADD +1 TO WS-CIA.                                            ELTSUBST
02820                                                                   ELTSUBST
02821  2220-EXIT.           EXIT.                                       ELTSUBST
02822                                                                   ELTSUBST
02823 /                                                                 ELTSUBST
02824  2400-PROBLEM-WITH-INDICES SECTION.                               ELTSUBST
02825                                                                   ELTSUBST
02826      MOVE 'PROBLEM W/ INDICES' TO COF-DTL-LINE(1).                ELTSUBST
02827      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTSUBST
02828                                                                   ELTSUBST
02829      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTSUBST
02830      MOVE 'P' TO COF-FUNCTION.                                    ELTSUBST
02831                                                                   ELTSUBST
02832      EXEC CICS LINK                                               ELTSUBST
02833           PROGRAM('ELUOUTPT')                                     ELTSUBST
02834           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
02835           END-EXEC.                                               ELTSUBST
02836                                                                   ELTSUBST
02837  2459-EXIT.                                                       ELTSUBST
02838      EXIT.                                                        ELTSUBST
02839                                                                   ELTSUBST
02840 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTSUBST
02841  3000-OUTPUT-TEXT SECTION.                                        ELTSUBST
02842      MOVE +0 TO COF-NBR-HDR-LINES.                                ELTSUBST
02843      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTSUBST
02844      SET COF-CONTINUE TO TRUE.                                    ELTSUBST
02845                                                                   ELTSUBST
02846      EXEC CICS LINK                                               ELTSUBST
02847           PROGRAM('ELUOUTPT')                                     ELTSUBST
02848           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
02849           END-EXEC.                                               ELTSUBST
02850      MOVE +1   TO WS-CIA.                                         ELTSUBST
02851  3000-EXIT.  EXIT.                                                ELTSUBST
02852 /                                                                 ELTSUBST
02853  3900-TRANSF-OTHER-RESP-IND SECTION.                              ELTSUBST
02854                                                                   ELTSUBST
02855      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
02856              NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTSUBST
02857          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTSUBST
02858          MOVE 'TRANSF-OTHER-RESP-IND'                             ELTSUBST
02859              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
02860          MOVE PLP-TRANSF-OTHER-RESP-IND                           ELTSUBST
02861              (PLT-INDEX1, PLT-INDEX2)                             ELTSUBST
02862              TO CMF-CODE-VALUE                                    ELTSUBST
02863          MOVE SPACES       TO WS-TEMP-TEXT-AREA                   ELTSUBST
02864          MOVE +00 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
02865          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
02866          PERFORM 3000-OUTPUT-TEXT                                 ELTSUBST
02867          MOVE 'N' TO WS-NEW-LINES-GENERATED-IND.                  ELTSUBST
02868                                                                   ELTSUBST
02869  3900-EXIT.    EXIT.                                              ELTSUBST
02870 /                                                                 ELTSUBST
02871  4000-PLACE-OF-TREAT-BASIC SECTION.                               ELTSUBST
02872                                                                   ELTSUBST
02873      SET PLT-INDEX2 TO 1.                                         ELTSUBST
02874      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTSUBST
02875              NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTSUBST
02876          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTSUBST
02877          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTSUBST
02878              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
02879          MOVE PLP-PLACE-TREAT-ELIG-IND                            ELTSUBST
02880              (PLT-INDEX1, PLT-INDEX2)                             ELTSUBST
02881              TO CMF-CODE-VALUE                                    ELTSUBST
02882          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTSUBST
02883          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
02884          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
02885          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND.                  ELTSUBST
02886                                                                   ELTSUBST
02887  4000-EXIT.                                                       ELTSUBST
02888      EXIT.                                                        ELTSUBST
02889                                                                   ELTSUBST
02890  4050-PLACE-OF-TREAT-SUPP SECTION.                                ELTSUBST
02891                                                                   ELTSUBST
02892      SET PLT-INDEX2 TO 2.                                         ELTSUBST
02893      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTSUBST
02894              NOT = ZERO AND NOT = SPACES AND NOT = LOW-VALUES     ELTSUBST
02895          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTSUBST
02896          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTSUBST
02897              TO CMF-ELEMENT-SYSTEM-NAME                           ELTSUBST
02898          MOVE PLP-PLACE-TREAT-ELIG-IND                            ELTSUBST
02899              (PLT-INDEX1, PLT-INDEX2)                             ELTSUBST
02900              TO CMF-CODE-VALUE                                    ELTSUBST
02901          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTSUBST
02902          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTSUBST
02903          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTSUBST
02904          MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND.                  ELTSUBST
02905                                                                   ELTSUBST
02906  4050-EXIT.                                                       ELTSUBST
02907      EXIT.                                                        ELTSUBST
02908 /                                                                 ELTSUBST
02909  4100-PAYABLE-FOR-BASIC SECTION.                                  ELTSUBST
02910      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = '19'    ELTSUBST
02911          GO TO 4100-EXIT.                                         ELTSUBST
02912      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTSUBST
02913          MOVE WS-CALL-CONTRACT-CODING TO COF-DTL-LINE(WS-CIA)     ELTSUBST
02914          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTSUBST
02915          GO TO 4100-OUTPUT-TEXT.                                  ELTSUBST
02916      MOVE 'BP'                 TO CMF-RECORD-PREFIX               ELTSUBST
02917      MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME        ELTSUBST
02918      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO      ELTSUBST
02919                                                CMF-CODE-VALUE     ELTSUBST
02920      EXEC CICS LINK                                               ELTSUBST
02921           PROGRAM('ELUCMIF')                                      ELTSUBST
02922           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
02923           END-EXEC.                                               ELTSUBST
02924      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
02925      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSUBST
02926      STRING CMF-DESCR-LINE(1) ' '                                 ELTSUBST
02927             CMF-DESCR-LINE(2)                                     ELTSUBST
02928                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTSUBST
02929      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSUBST
02930      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTSUBST
02931      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTSUBST
02932      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTSUBST
02933      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSUBST
02934                                                                   ELTSUBST
02935      PERFORM 4150-GET-PAYABLE-FIELDS.                             ELTSUBST
02936                                                                   ELTSUBST
02937      IF WS-VARIABLE-INDEMNITY-PRCNT = ZERO                        ELTSUBST
02938       IF WS-ADDITIONAL-PRICING-PRCNT = ZERO                       ELTSUBST
02939        IF WS-ADDN-ALLOW-AMT-PER-DAY = ZERO                        ELTSUBST
02940         IF WS-FLAT-RATE-PDM-AMT = ZERO                            ELTSUBST
02941                 MOVE TCAR-OPF-DATA(1)      TO WS-DTL-BASIC        ELTSUBST
02942                 MOVE WS-BASIC              TO                     ELTSUBST
02943                         COF-DTL-LINE(WS-CIA)                      ELTSUBST
02944         ELSE                                                      ELTSUBST
02945             MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D                 ELTSUBST
02946             MOVE WS-FLAT-RATE-PDM-AMT TO WS-DTL-PER-D-AMT         ELTSUBST
02947             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
02948             STRING WS-DTL-PER-D, ' '                              ELTSUBST
02949                    WS-DTL-PER-D-AMT,                              ELTSUBST
02950                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
02951             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
02952             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
02953             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
02954             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
02955             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
02956             MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                 ELTSUBST
02957             MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)         ELTSUBST
02958       ELSE                                                        ELTSUBST
02959             MOVE TCAR-OPF-DATA(1) TO WS-DTL-ALLOW                 ELTSUBST
02960             MOVE WS-ADDN-ALLOW-AMT-PER-DAY TO WS-DTL-ALLOW-AMT    ELTSUBST
02961             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
02962             STRING WS-DTL-ALLOW, ' '                              ELTSUBST
02963                    WS-DTL-ALLOW-AMT,                              ELTSUBST
02964                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
02965             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
02966             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
02967             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
02968             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
02969             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
02970             MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                 ELTSUBST
02971             MOVE WS-BASIC        TO COF-DTL-LINE(WS-CIA)          ELTSUBST
02972       ELSE                                                        ELTSUBST
02973          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTSUBST
02974          MOVE WS-ADDITIONAL-PRICING-PRCNT TO WS-DTL-PERCENT       ELTSUBST
02975          MOVE SPACES          TO TCAR-FROM-AREA                   ELTSUBST
02976          STRING WS-DTL-PP,                                        ELTSUBST
02977                 WS-DTL-PERCENT,                                   ELTSUBST
02978                 WS-PERCENT-SIGN,                                  ELTSUBST
02979                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTSUBST
02980          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTSUBST
02981          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTSUBST
02982          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTSUBST
02983          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTSUBST
02984          PERFORM TCPR-000-TEXT-UNSTRING                           ELTSUBST
02985          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTSUBST
02986          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTSUBST
02987      ELSE                                                         ELTSUBST
02988        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTSUBST
02989        MOVE WS-VARIABLE-INDEMNITY-PRCNT TO WS-DTL-PERCENT         ELTSUBST
02990        MOVE SPACES          TO TCAR-FROM-AREA                     ELTSUBST
02991        STRING WS-DTL-PP,                                          ELTSUBST
02992               WS-DTL-PERCENT,                                     ELTSUBST
02993               WS-PERCENT-SIGN,                                    ELTSUBST
02994                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTSUBST
02995        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTSUBST
02996        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTSUBST
02997        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTSUBST
02998        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTSUBST
02999        PERFORM TCPR-000-TEXT-UNSTRING                             ELTSUBST
03000        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTSUBST
03001        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTSUBST
03002  4100-OUTPUT-TEXT.                                                ELTSUBST
03003      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTSUBST
03004            ADD +1                TO WS-CIA                        ELTSUBST
03005            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTSUBST
03006            PERFORM 3000-OUTPUT-TEXT                               ELTSUBST
03007      ELSE                                                         ELTSUBST
03008         PERFORM 3000-OUTPUT-TEXT.                                 ELTSUBST
03009  4100-EXIT.  EXIT.                                                ELTSUBST
03010                                                                   ELTSUBST
03011  4150-GET-PAYABLE-FIELDS SECTION.                                 ELTSUBST
03012                                                                   ELTSUBST
03013      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTSUBST
03014                                       NUMERIC AND                 ELTSUBST
03015          PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTSUBST
03016                                       NOT = ZERO                  ELTSUBST
03017        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTSUBST
03018          TO WS-VARIABLE-INDEMNITY-PRCNT.                          ELTSUBST
03019      IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTSUBST
03020                                       NUMERIC AND                 ELTSUBST
03021          PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTSUBST
03022                                       NOT = ZERO                  ELTSUBST
03023        MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTSUBST
03024          TO WS-ADDITIONAL-PRICING-PRCNT.                          ELTSUBST
03025 ******************************************************************ELTSUBST
03026 ** IN PRACTICE, EITHER THE PLA- FIELD OR THE PLW- FIELD WILL    **ELTSUBST
03027 ** CONTAIN ZEROES, OR BOTH WILL CONTAIN ZEROES.                 **ELTSUBST
03028 ******************************************************************ELTSUBST
03029      IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03030                                       NUMERIC AND                 ELTSUBST
03031          PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
03032                                       NOT = ZERO                  ELTSUBST
03033        MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)    ELTSUBST
03034          TO WS-ADDN-ALLOW-AMT-PER-DAY.                            ELTSUBST
03035      IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03036                                       NUMERIC AND                 ELTSUBST
03037          PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)       ELTSUBST
03038                                       NOT = ZERO                  ELTSUBST
03039        MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)    ELTSUBST
03040          TO WS-ADDN-ALLOW-AMT-PER-DAY.                            ELTSUBST
03041      IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)             ELTSUBST
03042                                       NUMERIC AND                 ELTSUBST
03043          PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)            ELTSUBST
03044                                       NOT = ZERO                  ELTSUBST
03045        MOVE PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03046          TO WS-FLAT-RATE-PDM-AMT.                                 ELTSUBST
03047      IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)             ELTSUBST
03048                                       NUMERIC AND                 ELTSUBST
03049          PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)            ELTSUBST
03050                                       NOT = ZERO                  ELTSUBST
03051        MOVE PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03052          TO WS-FLAT-RATE-PDM-AMT.                                 ELTSUBST
03053                                                                   ELTSUBST
03054  4150-EXIT.  EXIT.                                                ELTSUBST
03055 /                                                                 ELTSUBST
03056  4200-PAYABLE-FOR-SUPP SECTION.                                   ELTSUBST
03057      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = '19'    ELTSUBST
03058          GO TO 4200-EXIT.                                         ELTSUBST
03059      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZERO    ELTSUBST
03060          MOVE WS-CALL-CONTRACT-CODING TO COF-DTL-LINE(WS-CIA)     ELTSUBST
03061          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTSUBST
03062          GO TO 4200-OUTPUT-TEXT.                                  ELTSUBST
03063      MOVE 'BP'                 TO CMF-RECORD-PREFIX               ELTSUBST
03064      MOVE 'PROVN-PRICING-METHD' TO CMF-ELEMENT-SYSTEM-NAME        ELTSUBST
03065      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) TO      ELTSUBST
03066                                                CMF-CODE-VALUE     ELTSUBST
03067      EXEC CICS LINK                                               ELTSUBST
03068           PROGRAM('ELUCMIF')                                      ELTSUBST
03069           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
03070           END-EXEC.                                               ELTSUBST
03071      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
03072      MOVE SPACES TO TCAR-FROM-AREA.                               ELTSUBST
03073      STRING CMF-DESCR-LINE(1) ' '                                 ELTSUBST
03074             CMF-DESCR-LINE(2)                                     ELTSUBST
03075                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTSUBST
03076      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTSUBST
03077      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTSUBST
03078      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTSUBST
03079      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTSUBST
03080      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTSUBST
03081                                                                   ELTSUBST
03082      PERFORM 4150-GET-PAYABLE-FIELDS.                             ELTSUBST
03083                                                                   ELTSUBST
03084      IF WS-VARIABLE-INDEMNITY-PRCNT = ZERO                        ELTSUBST
03085       IF WS-ADDITIONAL-PRICING-PRCNT = ZERO                       ELTSUBST
03086        IF WS-ADDN-ALLOW-AMT-PER-DAY = ZERO                        ELTSUBST
03087         IF WS-FLAT-RATE-PDM-AMT = ZERO                            ELTSUBST
03088                MOVE TCAR-OPF-DATA(1)      TO WS-DTL-SUPPLEMENTAL  ELTSUBST
03089                MOVE WS-SUPPLEMENTAL       TO                      ELTSUBST
03090                         COF-DTL-LINE(WS-CIA)                      ELTSUBST
03091         ELSE                                                      ELTSUBST
03092             MOVE TCAR-OPF-DATA(1)      TO WS-DTL-PER-D            ELTSUBST
03093             MOVE WS-FLAT-RATE-PDM-AMT TO WS-DTL-PER-D-AMT         ELTSUBST
03094             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
03095             STRING WS-DTL-PER-D, ' '                              ELTSUBST
03096                    WS-DTL-PER-D-AMT,                              ELTSUBST
03097                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
03098             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
03099             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
03100             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
03101             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
03102             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
03103             MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL          ELTSUBST
03104             MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)         ELTSUBST
03105       ELSE                                                        ELTSUBST
03106            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-ALLOW             ELTSUBST
03107            MOVE WS-ADDN-ALLOW-AMT-PER-DAY TO WS-DTL-ALLOW-AMT     ELTSUBST
03108             MOVE SPACES          TO TCAR-FROM-AREA                ELTSUBST
03109             STRING WS-DTL-ALLOW, ' '                              ELTSUBST
03110                    WS-DTL-ALLOW-AMT,                              ELTSUBST
03111                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTSUBST
03112             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTSUBST
03113             MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT       ELTSUBST
03114             MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN       ELTSUBST
03115             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTSUBST
03116             PERFORM TCPR-000-TEXT-UNSTRING                        ELTSUBST
03117             MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL          ELTSUBST
03118             MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)          ELTSUBST
03119       ELSE                                                        ELTSUBST
03120          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTSUBST
03121          MOVE WS-ADDITIONAL-PRICING-PRCNT TO WS-DTL-PERCENT       ELTSUBST
03122          MOVE SPACES          TO TCAR-FROM-AREA                   ELTSUBST
03123          STRING WS-DTL-PP,                                        ELTSUBST
03124                 WS-DTL-PERCENT,                                   ELTSUBST
03125                 WS-PERCENT-SIGN,                                  ELTSUBST
03126                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTSUBST
03127          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTSUBST
03128          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTSUBST
03129          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTSUBST
03130          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTSUBST
03131          PERFORM TCPR-000-TEXT-UNSTRING                           ELTSUBST
03132          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTSUBST
03133          MOVE WS-SUPPLEMENTAL-PERCENT TO COF-DTL-LINE(WS-CIA)     ELTSUBST
03134      ELSE                                                         ELTSUBST
03135        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTSUBST
03136        MOVE WS-VARIABLE-INDEMNITY-PRCNT TO WS-DTL-PERCENT         ELTSUBST
03137        MOVE SPACES          TO TCAR-FROM-AREA                     ELTSUBST
03138        STRING WS-DTL-PP,                                          ELTSUBST
03139               WS-DTL-PERCENT,                                     ELTSUBST
03140               WS-PERCENT-SIGN,                                    ELTSUBST
03141                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTSUBST
03142        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTSUBST
03143        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTSUBST
03144        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTSUBST
03145        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTSUBST
03146        PERFORM TCPR-000-TEXT-UNSTRING                             ELTSUBST
03147        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTSUBST
03148        MOVE WS-SUPPLEMENTAL-PERCENT TO COF-DTL-LINE(WS-CIA).      ELTSUBST
03149  4200-OUTPUT-TEXT.                                                ELTSUBST
03150      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTSUBST
03151            ADD +1                TO WS-CIA                        ELTSUBST
03152            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTSUBST
03153            PERFORM 3000-OUTPUT-TEXT                               ELTSUBST
03154      ELSE                                                         ELTSUBST
03155         PERFORM 3000-OUTPUT-TEXT.                                 ELTSUBST
03156  4200-EXIT.  EXIT.                                                ELTSUBST
03157 /                                                                 ELTSUBST
03158  4300-SPILLOVER-COINS SECTION.                                    ELTSUBST
03159      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSUBST
03160            = '0' OR LOW-VALUES                                    ELTSUBST
03161           GO TO 4300-EXIT.                                        ELTSUBST
03162      ADD +1    TO WS-CIA.                                         ELTSUBST
03163      MOVE 'BP' TO CMF-RECORD-PREFIX.                              ELTSUBST
03164      MOVE 'SPILL-OVER-COINS-APL-IND' TO CMF-ELEMENT-SYSTEM-NAME.  ELTSUBST
03165      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTSUBST
03166                       TO CMF-CODE-VALUE.                          ELTSUBST
03167      EXEC CICS LINK                                               ELTSUBST
03168           PROGRAM('ELUCMIF')                                      ELTSUBST
03169           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
03170           END-EXEC.                                               ELTSUBST
03171      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
03172      MOVE SPACES          TO TCAR-FROM-AREA                       ELTSUBST
03173      STRING WS-SPILLOVER-COINS                                    ELTSUBST
03174             CMF-DESCR-LINE(1) ' '                                 ELTSUBST
03175             CMF-DESCR-LINE(2) ' '                                 ELTSUBST
03176               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTSUBST
03177      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTSUBST
03178      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTSUBST
03179      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTSUBST
03180      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTSUBST
03181      PERFORM TCPR-000-TEXT-UNSTRING                               ELTSUBST
03182      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTSUBST
03183      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTSUBST
03184         ADD +1                TO WS-CIA                           ELTSUBST
03185         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTSUBST
03186         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
03187      ELSE                                                         ELTSUBST
03188       PERFORM 3000-OUTPUT-TEXT.                                   ELTSUBST
03189  4300-EXIT.  EXIT.                                                ELTSUBST
03190 /                                                                 ELTSUBST
03191  4400-SPILLOVER-DEDUCT SECTION.                                   ELTSUBST
03192      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03193            = '0' OR LOW-VALUES                                    ELTSUBST
03194           GO TO 4400-EXIT.                                        ELTSUBST
03195      ADD +1    TO WS-CIA.                                         ELTSUBST
03196      MOVE 'BP'                      TO CMF-RECORD-PREFIX.         ELTSUBST
03197      MOVE 'SPILL-OVER-DED-APL-IND'  TO CMF-ELEMENT-SYSTEM-NAME.   ELTSUBST
03198      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTSUBST
03199                       TO CMF-CODE-VALUE                           ELTSUBST
03200      EXEC CICS LINK                                               ELTSUBST
03201           PROGRAM('ELUCMIF')                                      ELTSUBST
03202           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
03203           END-EXEC.                                               ELTSUBST
03204      PERFORM 4599-SET-CMF-DESCR-ADDR.                             ELTSUBST
03205      MOVE SPACES          TO TCAR-FROM-AREA                       ELTSUBST
03206      STRING WS-SPILLOVER-DED                                      ELTSUBST
03207             CMF-DESCR-LINE(1) ' '                                 ELTSUBST
03208             CMF-DESCR-LINE(2) ' '                                 ELTSUBST
03209               DELIMITED BY SIZE INTO TCAR-FROM-AREA.              ELTSUBST
03210      PERFORM TCPR-000-TEXT-COMPRESSION                            ELTSUBST
03211      MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT              ELTSUBST
03212      MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN              ELTSUBST
03213      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN              ELTSUBST
03214      PERFORM TCPR-000-TEXT-UNSTRING                               ELTSUBST
03215      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTSUBST
03216      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTSUBST
03217         ADD +1                TO WS-CIA                           ELTSUBST
03218         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTSUBST
03219         PERFORM 3000-OUTPUT-TEXT                                  ELTSUBST
03220      ELSE                                                         ELTSUBST
03221       PERFORM 3000-OUTPUT-TEXT.                                   ELTSUBST
03222  4400-EXIT.  EXIT.                                                ELTSUBST
03223 /                                                                 ELTSUBST
03224  4500-DAYS-REDUCTION-RATE SECTION.                                ELTSUBST
03225                                                                   ELTSUBST
03226      IF WS-DAYS-RDCN-RAT-IND NOT = ZERO                           ELTSUBST
03227           AND WS-DAYS-RDCN-RAT-IND NOT = LOW-VALUES               ELTSUBST
03228        MOVE 'DAYS-RDCN-RAT-IND' TO CMF-ELEMENT-SYSTEM-NAME        ELTSUBST
03229        MOVE WS-DAYS-RDCN-RAT-IND                                  ELTSUBST
03230                                 TO CMF-CODE-VALUE                 ELTSUBST
03231        EXEC CICS LINK                                             ELTSUBST
03232             PROGRAM('ELUCMIF')                                    ELTSUBST
03233             COMMAREA(DFHCOMMAREA)                                 ELTSUBST
03234             END-EXEC                                              ELTSUBST
03235        PERFORM 4599-SET-CMF-DESCR-ADDR                            ELTSUBST
03236        MOVE SPACES              TO TCAR-FROM-AREA                 ELTSUBST
03237        STRING CMF-DESCR-LINE(1) ' '                               ELTSUBST
03238               CMF-DESCR-LINE(2) ' '                               ELTSUBST
03239                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTSUBST
03240        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTSUBST
03241        MOVE +02                 TO TCAR-OUTPUT-FIELD-COUNT        ELTSUBST
03242        MOVE +79                 TO TCAR-OUTPUT-FIELD-1-LEN        ELTSUBST
03243        MOVE +79                 TO TCAR-OUTPUT-FIELD-2-LEN        ELTSUBST
03244        PERFORM TCPR-000-TEXT-UNSTRING                             ELTSUBST
03245        MOVE TCAR-OPF-DATA(1)    TO COF-DTL-LINE(WS-CIA)           ELTSUBST
03246        ADD +1                   TO WS-CIA                         ELTSUBST
03247        IF TCAR-OUTPUT-FIELDS-USED > 1                             ELTSUBST
03248           MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)           ELTSUBST
03249           ADD +1                TO WS-CIA                         ELTSUBST
03250           PERFORM 3000-OUTPUT-TEXT                                ELTSUBST
03251        ELSE                                                       ELTSUBST
03252           PERFORM 3000-OUTPUT-TEXT.                               ELTSUBST
03253                                                                   ELTSUBST
03254  4500-EXIT.                                                       ELTSUBST
03255      EXIT.                                                        ELTSUBST
03256 /                                                                 ELTSUBST
03257  4599-SET-CMF-DESCR-ADDR.                                         ELTSUBST
03258      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTSUBST
03259      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
03260         ADDRESS OF CMF-DESCR.                                     ELTSUBST
03261                                                                   ELTSUBST
03262 /                                                                 ELTSUBST
03263  4600-SCAN-TAB SECTION.                                           ELTSUBST
03264                                                                   ELTSUBST
03265      PERFORM 4700-BEN-TAB-AAR.                                    ELTSUBST
03266      PERFORM 4800-BEN-TAB-PPF.                                    ELTSUBST
03267                                                                   ELTSUBST
03268      MOVE LOW-VALUES TO COF-DTL-LINE(1).                          ELTSUBST
03269      MOVE WS-CHECK-CONTRACT-FOR-PVE                               ELTSUBST
03270          TO COF-DTL-LINE(2).                                      ELTSUBST
03271      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTSUBST
03272      EXEC CICS LINK                                               ELTSUBST
03273           PROGRAM('ELUOUTPT')                                     ELTSUBST
03274           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
03275           END-EXEC.                                               ELTSUBST
03276                                                                   ELTSUBST
03277      PERFORM 5000-BEN-TAB-ADL.                                    ELTSUBST
03278      PERFORM 5100-BEN-TAB-ABM.                                    ELTSUBST
03279      PERFORM 5200-BEN-TAB-ACL.                                    ELTSUBST
03280      PERFORM 5300-BEN-TAB-AOL.                                    ELTSUBST
03281                                                                   ELTSUBST
03282      MOVE +2 TO COF-NBR-DTL-LINES.                                ELTSUBST
03283      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (COF-NBR-DTL-LINES).     ELTSUBST
03284      EXEC CICS LINK                                               ELTSUBST
03285           PROGRAM('ELUOUTPT')                                     ELTSUBST
03286           COMMAREA(DFHCOMMAREA)                                   ELTSUBST
03287           END-EXEC.                                               ELTSUBST
03288                                                                   ELTSUBST
03289  4600-EXIT.  EXIT.                                                ELTSUBST
03290 /                                                                 ELTSUBST
03291  4700-BEN-TAB-AAR SECTION.                                        ELTSUBST
03292      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTSUBST
03293      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03294                                                                   ELTSUBST
03295      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03296          NOT = LOW-VALUES                                         ELTSUBST
03297       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03298          NOT = SPACE                                              ELTSUBST
03299                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTSUBST
03300                                                                   ELTSUBST
03301      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03302                                                                   ELTSUBST
03303      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03304          NOT = LOW-VALUES                                         ELTSUBST
03305       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03306          NOT = SPACE                                              ELTSUBST
03307                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTSUBST
03308                                                                   ELTSUBST
03309      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTSUBST
03310             MOVE +1                  TO WS-CIA                    ELTSUBST
03311             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTSUBST
03312             ADD  +1                  TO WS-CIA                    ELTSUBST
03313             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTSUBST
03314             PERFORM 3000-OUTPUT-TEXT.                             ELTSUBST
03315  4700-EXIT.  EXIT.                                                ELTSUBST
03316 /                                                                 ELTSUBST
03317  4800-BEN-TAB-PPF SECTION.                                        ELTSUBST
03318      MOVE ZEROS   TO  WS-HOLD1,                                   ELTSUBST
03319                       WS-HOLD2.                                   ELTSUBST
03320      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03321      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03322          NOT = LOW-VALUES                                         ELTSUBST
03323       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03324          NOT = SPACE                                              ELTSUBST
03325             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03326                        TO  WS-HOLD1.                              ELTSUBST
03327                                                                   ELTSUBST
03328      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03329      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03330          NOT = LOW-VALUES                                         ELTSUBST
03331       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03332          NOT = SPACE                                              ELTSUBST
03333             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03334                        TO  WS-HOLD2.                              ELTSUBST
03335                                                                   ELTSUBST
03336      IF WS-HOLD1 = WS-HOLD2                                       ELTSUBST
03337         IF WS-HOLD1 = ZEROS                                       ELTSUBST
03338                 GO TO 4800-EXIT                                   ELTSUBST
03339         ELSE                                                      ELTSUBST
03340             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTSUBST
03341             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03342               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTSUBST
03343                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03344               END-EXEC                                            ELTSUBST
03345               GO TO 4800-EXIT.                                    ELTSUBST
03346                                                                   ELTSUBST
03347      IF WS-HOLD1 = ZEROS                                          ELTSUBST
03348             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03349             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03350               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTSUBST
03351                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03352               END-EXEC                                            ELTSUBST
03353      ELSE                                                         ELTSUBST
03354       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTSUBST
03355       PERFORM 5900-GET-TABULAR-RECORD                             ELTSUBST
03356          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTSUBST
03357                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03358          END-EXEC                                                 ELTSUBST
03359          IF WS-HOLD2 = ZEROS                                      ELTSUBST
03360            GO TO 4800-EXIT                                        ELTSUBST
03361          ELSE                                                     ELTSUBST
03362             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03363             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03364               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTSUBST
03365                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03366               END-EXEC.                                           ELTSUBST
03367  4800-EXIT.    EXIT.                                              ELTSUBST
03368 /                                                                 ELTSUBST
03369  5000-BEN-TAB-ADL SECTION.                                        ELTSUBST
03370      MOVE ZEROS   TO  WS-HOLD1,                                   ELTSUBST
03371                       WS-HOLD2.                                   ELTSUBST
03372      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03373      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03374          NOT = LOW-VALUES                                         ELTSUBST
03375       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03376          NOT = SPACE                                              ELTSUBST
03377             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03378                        TO  WS-HOLD1.                              ELTSUBST
03379                                                                   ELTSUBST
03380      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03381      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03382          NOT = LOW-VALUES                                         ELTSUBST
03383       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03384          NOT = SPACE                                              ELTSUBST
03385             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03386                        TO  WS-HOLD2.                              ELTSUBST
03387                                                                   ELTSUBST
03388      IF WS-HOLD1 = WS-HOLD2                                       ELTSUBST
03389         IF WS-HOLD1 = ZEROS                                       ELTSUBST
03390                 GO TO 5000-EXIT                                   ELTSUBST
03391         ELSE                                                      ELTSUBST
03392             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTSUBST
03393             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03394               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTSUBST
03395                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03396               END-EXEC                                            ELTSUBST
03397               GO TO 5000-EXIT.                                    ELTSUBST
03398                                                                   ELTSUBST
03399      IF WS-HOLD1 = ZEROS                                          ELTSUBST
03400          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTSUBST
03401          PERFORM 5900-GET-TABULAR-RECORD                          ELTSUBST
03402          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTSUBST
03403                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03404          END-EXEC                                                 ELTSUBST
03405      ELSE                                                         ELTSUBST
03406          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTSUBST
03407          PERFORM 5900-GET-TABULAR-RECORD                          ELTSUBST
03408          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTSUBST
03409                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03410          END-EXEC                                                 ELTSUBST
03411          IF WS-HOLD2 = ZEROS                                      ELTSUBST
03412            GO TO 5000-EXIT                                        ELTSUBST
03413          ELSE                                                     ELTSUBST
03414             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03415             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03416               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTSUBST
03417                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03418               END-EXEC.                                           ELTSUBST
03419  5000-EXIT.     EXIT.                                             ELTSUBST
03420 /                                                                 ELTSUBST
03421  5100-BEN-TAB-ABM SECTION.                                        ELTSUBST
03422      MOVE ZEROS   TO  WS-HOLD1,                                   ELTSUBST
03423                       WS-HOLD2.                                   ELTSUBST
03424      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03425      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03426          NOT = LOW-VALUES                                         ELTSUBST
03427       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03428          NOT = SPACE                                              ELTSUBST
03429             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03430                        TO  WS-HOLD1.                              ELTSUBST
03431                                                                   ELTSUBST
03432      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03433      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03434          NOT = LOW-VALUES                                         ELTSUBST
03435       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03436          NOT = SPACE                                              ELTSUBST
03437             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03438                        TO  WS-HOLD2.                              ELTSUBST
03439                                                                   ELTSUBST
03440      IF WS-HOLD1 = WS-HOLD2                                       ELTSUBST
03441         IF WS-HOLD1 = ZEROS                                       ELTSUBST
03442                 GO TO 5100-EXIT                                   ELTSUBST
03443         ELSE                                                      ELTSUBST
03444             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTSUBST
03445             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03446               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTSUBST
03447                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03448               END-EXEC                                            ELTSUBST
03449               GO TO 5100-EXIT.                                    ELTSUBST
03450                                                                   ELTSUBST
03451      IF WS-HOLD1 = ZEROS                                          ELTSUBST
03452             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03453             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03454               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTSUBST
03455                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03456               END-EXEC                                            ELTSUBST
03457      ELSE                                                         ELTSUBST
03458       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTSUBST
03459       PERFORM 5900-GET-TABULAR-RECORD                             ELTSUBST
03460          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTSUBST
03461                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03462          END-EXEC                                                 ELTSUBST
03463          IF WS-HOLD2 = ZEROS                                      ELTSUBST
03464            GO TO 5100-EXIT                                        ELTSUBST
03465          ELSE                                                     ELTSUBST
03466             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03467             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03468               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTSUBST
03469                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03470               END-EXEC.                                           ELTSUBST
03471  5100-EXIT.     EXIT.                                             ELTSUBST
03472 /                                                                 ELTSUBST
03473  5200-BEN-TAB-ACL SECTION.                                        ELTSUBST
03474      MOVE ZEROS   TO  WS-HOLD1,                                   ELTSUBST
03475                       WS-HOLD2.                                   ELTSUBST
03476      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03477      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03478          NOT = LOW-VALUES                                         ELTSUBST
03479       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03480          NOT = SPACE                                              ELTSUBST
03481             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03482                        TO  WS-HOLD1.                              ELTSUBST
03483                                                                   ELTSUBST
03484      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03485      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03486          NOT = LOW-VALUES                                         ELTSUBST
03487       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03488          NOT = SPACE                                              ELTSUBST
03489             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03490                        TO  WS-HOLD2.                              ELTSUBST
03491                                                                   ELTSUBST
03492      IF WS-HOLD1 = WS-HOLD2                                       ELTSUBST
03493         IF WS-HOLD1 = ZEROS                                       ELTSUBST
03494                 GO TO 5200-EXIT                                   ELTSUBST
03495         ELSE                                                      ELTSUBST
03496             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTSUBST
03497             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03498               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTSUBST
03499                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03500               END-EXEC                                            ELTSUBST
03501               GO TO 5200-EXIT.                                    ELTSUBST
03502                                                                   ELTSUBST
03503      IF WS-HOLD1 = ZEROS                                          ELTSUBST
03504             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03505             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03506               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTSUBST
03507                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03508               END-EXEC                                            ELTSUBST
03509      ELSE                                                         ELTSUBST
03510       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTSUBST
03511       PERFORM 5900-GET-TABULAR-RECORD                             ELTSUBST
03512          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTSUBST
03513                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03514          END-EXEC                                                 ELTSUBST
03515          IF WS-HOLD2 = ZEROS                                      ELTSUBST
03516            GO TO 5200-EXIT                                        ELTSUBST
03517          ELSE                                                     ELTSUBST
03518             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03519             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03520               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTSUBST
03521                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03522               END-EXEC.                                           ELTSUBST
03523  5200-EXIT.     EXIT.                                             ELTSUBST
03524 /                                                                 ELTSUBST
03525  5300-BEN-TAB-AOL SECTION.                                        ELTSUBST
03526      MOVE ZEROS   TO  WS-HOLD1,                                   ELTSUBST
03527                       WS-HOLD2.                                   ELTSUBST
03528      SET PLT-INDEX2 TO 1.                                         ELTSUBST
03529      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03530          NOT = LOW-VALUES                                         ELTSUBST
03531       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03532          NOT = SPACE                                              ELTSUBST
03533             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03534                        TO  WS-HOLD1.                              ELTSUBST
03535                                                                   ELTSUBST
03536      SET PLT-INDEX2 TO 2.                                         ELTSUBST
03537      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTSUBST
03538          NOT = LOW-VALUES                                         ELTSUBST
03539       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTSUBST
03540          NOT = SPACE                                              ELTSUBST
03541             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTSUBST
03542                        TO  WS-HOLD2.                              ELTSUBST
03543                                                                   ELTSUBST
03544      IF WS-HOLD1 = WS-HOLD2                                       ELTSUBST
03545         IF WS-HOLD1 = ZEROS                                       ELTSUBST
03546                 GO TO 5300-EXIT                                   ELTSUBST
03547         ELSE                                                      ELTSUBST
03548             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTSUBST
03549             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03550               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTSUBST
03551                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03552               END-EXEC                                            ELTSUBST
03553               GO TO 5300-EXIT.                                    ELTSUBST
03554                                                                   ELTSUBST
03555      IF WS-HOLD1 = ZEROS                                          ELTSUBST
03556             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03557             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03558               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTSUBST
03559                             COMMAREA(DFHCOMMAREA)                 ELTSUBST
03560               END-EXEC                                            ELTSUBST
03561      ELSE                                                         ELTSUBST
03562       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTSUBST
03563       PERFORM 5900-GET-TABULAR-RECORD                             ELTSUBST
03564          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTSUBST
03565                          COMMAREA(DFHCOMMAREA)                    ELTSUBST
03566          END-EXEC                                                 ELTSUBST
03567          IF WS-HOLD2 = ZEROS                                      ELTSUBST
03568            GO TO 5300-EXIT                                        ELTSUBST
03569          ELSE                                                     ELTSUBST
03570             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTSUBST
03571             PERFORM 5900-GET-TABULAR-RECORD                       ELTSUBST
03572               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTSUBST
03573                              COMMAREA(DFHCOMMAREA)                ELTSUBST
03574               END-EXEC.                                           ELTSUBST
03575  5300-EXIT.     EXIT.                                             ELTSUBST
03576 /                                                                 ELTSUBST
03577  5900-GET-TABULAR-RECORD SECTION.                                 ELTSUBST
03578 ***************************************************************** ELTSUBST
03579 *            G E T   T A B U L A R   R E C O R D                  ELTSUBST
03580 *                                                                 ELTSUBST
03581 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF            ELTSUBST
03582 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTSUBST
03583 *  TO DISPLAY.                                                    ELTSUBST
03584 *                                                                 ELTSUBST
03585 ***************************************************************** ELTSUBST
03586      SET CIA-GCTABULR-DDN TO TRUE.                                ELTSUBST
03587      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTSUBST
03588          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTSUBST
03589      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTSUBST
03590      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTSUBST
03591      SET IOP-RD                          TO TRUE.                 ELTSUBST
03592      SET IOP-FCQ-NONE                    TO TRUE.                 ELTSUBST
03593      SET IOP-KVQ-NONE                    TO TRUE.                 ELTSUBST
03594      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTSUBST
03595                                                                   ELTSUBST
03596      EXEC CICS LINK                                               ELTSUBST
03597                PROGRAM ('ELUIOPGM')                               ELTSUBST
03598                COMMAREA (DFHCOMMAREA)                             ELTSUBST
03599      END-EXEC.                                                    ELTSUBST
03600                                                                   ELTSUBST
03601      IF IOP-RC-NOTFND                                             ELTSUBST
03602         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTSUBST
03603         EXEC CICS ABEND                                           ELTSUBST
03604                   ABCODE(CIA-ABCODE)                              ELTSUBST
03605         END-EXEC                                                  ELTSUBST
03606      ELSE                                                         ELTSUBST
03607          IF NOT IOP-RC-OK                                         ELTSUBST
03608             SET CIA-AB-CRITIO TO TRUE                             ELTSUBST
03609             EXEC CICS ABEND                                       ELTSUBST
03610                       ABCODE(CIA-ABCODE)                          ELTSUBST
03611             END-EXEC                                              ELTSUBST
03612          END-IF                                                   ELTSUBST
03613      END-IF.                                                      ELTSUBST
03614  5900-EXIT.  EXIT.                                                ELTSUBST
03615                                                                   ELTSUBST
03616  6000-DSPLY-NTWRK-UTIL-IND.                                       ELTSUBST
03617      MOVE 'GROUP' TO CMF-RECORD-PREFIX.                           ELTSUBST
03618      MOVE 'NETWORK-UTIL-REVIEW-IND'                               ELTSUBST
03619        TO  CMF-ELEMENT-SYSTEM-NAME.                               ELTSUBST
03620      MOVE GCG-NETWORK-UTIL-REVIEW-IND                             ELTSUBST
03621        TO CMF-CODE-VALUE.                                         ELTSUBST
03622      MOVE WS-NETWORK-UTIL-REV-PHR                                 ELTSUBST
03623        TO WS-TEMP-TEXT-AREA.                                      ELTSUBST
03624      MOVE 28 TO WS-TEMP-NOT-USED-CNT.                             ELTSUBST
03625      PERFORM 2100-CALL-CODES-MANUAL-LONG.                         ELTSUBST
03626      MOVE 'Y' TO WS-NEW-LINES-GENERATED-IND.                      ELTSUBST
03627      PERFORM 3000-OUTPUT-TEXT                                     ELTSUBST
03628         THRU 3000-EXIT.                                           ELTSUBST
03629                                                                   ELTSUBST
03630      COPY ELSTCOMP.                                               ELTSUBST
