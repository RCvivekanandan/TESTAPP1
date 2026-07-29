00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTCONSL.                                            ELTCONSL
00003 *** THIS PROGRAM WAS CLONED FROM ELTINPTS-ELTINPTSTS (LVL 041)       LV002
00004 *** FROM HCMSGEN.TEST.PANLIB BY WIL HARNDEN 5-19-86               ELTCONSL
00005 *** ORIGINAL AUTHOR WAS                                           ELTCONSL
00006  AUTHOR. JOHN CURIN - KEANE, INC.                                 ELTCONSL
00007  DATE-WRITTEN.   4/08/86.                                         ELTCONSL
00008  DATE-COMPILED.                                                   ELTCONSL
00009      SKIP3                                                        ELTCONSL
00010 ******************************************************************ELTCONSL
00011 *@>ELTCONSL                                                       ELTCONSL
00012 *@¬                                                               ELTCONSL
00013 *                        PROGRAM ABSTRACT                         ELTCONSL
00014 *                                                                 ELTCONSL
00015 *@¬ PROGRAM NAME:   E.L.S. CONSULTATION SERVICES                  ELTCONSL
00016 *@¬                                                               ELTCONSL
00017 *@¬ PROGRAM I.D.:   ELTCONSL                                      ELTCONSL
00018 *@¬                                                               ELTCONSL
00019 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTCONSL
00020 *@¬            CONSULTATION SERVICES COVERAGE GIVEN A MEMBER.     ELTCONSL
00021 *@¬                                                               ELTCONSL
00022 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF CONSULTATION      ELTCONSL
00023 *@¬            SERVICES AFFORD A MEMBER BY HIS GROUP.  THIS INFO  ELTCONSL
00024 *@¬            IS GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS   ELTCONSL
00025 *@¬            FOR                                                ELTCONSL
00026 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTCONSL
00027 *@¬            RANGE OF DATES.                                    ELTCONSL
00028 *@¬                                                               ELTCONSL
00029 *@¬ RECORDS                                                       ELTCONSL
00030 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT,                          ELTCONSL
00031 *@¬            VARIOUS BENEFIT PROVISIONS, AND A                  ELTCONSL
00032 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTCONSL
00033 *@¬                                                               ELTCONSL
00034 *@¬ PROCESSING                                                    ELTCONSL
00035 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTCONSL
00036 *@¬                                                               ELTCONSL
00037 *@¬ UPDATE HISTORY                                                ELTCONSL
00038 *@¬                                                               ELTCONSL
00039 *@¬ 07/24/86  JTC  CHANGED THE PICTURE OF WS-BASIC-MAX-AMT        ELTCONSL
00040 *@¬                FROM PIC ZZ9 TO PIC ZZ9.99-                    ELTCONSL
00041 *@¬                CHANGED THE PICTURE OF WS-SUPPL-MAX-AMT        ELTCONSL
00042 *@¬                FROM PIC ZZ9 TO PIC ZZ9.99-                    ELTCONSL
00043 *@¬                                                               ELTCONSL
00044 *@    08/14/86  LET  USING THE 1ST HEADER LINE FROM THE PROLOG    ELTCONSL
00045 *@                                                                ELTCONSL
00046 *@    10/02/86  JTC  VS COBOL II CONVERSION                       ELTCONSL
00047 *@                                                                ELTCONSL
00048 *@    10/19/87  NAC  REWORK PHRASE FOR COVERED BENEFITS.          ELTCONSL
00049 *@                                                                ELTCONSL
00050 *@    03/28/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.             ELTCONSL
00051 *@                                                                ELTCONSL
00052 *@    10/24/89  RKH  ADDED TRANSF TO OTHER RESP IND               ELTCONSL
00053 *@                                                                ELTCONSL
00054 *@    05/15/90  GEM  REPLACED PLE-COST-CONT-PYMT-ELIG-IND LOGIC   ELTCONSL
00055 *@                   WITH PLP-COST-CONT-PYMT-ELIG-IND PROCESSING, ELTCONSL
00056 *@                   ALSO RENAMED PSE-COST-CONT-PYMT-ELIG-IND TO  ELTCONSL
00057 *@                                PSP-COST-CONT-PYMT-ELIG-IND.    ELTCONSL
00058 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTCONSL
00059 ***************************************************************** ELTCONSL
00060 /                                                                 ELTCONSL
00061  ENVIRONMENT DIVISION.                                            ELTCONSL
00062      SKIP3                                                        ELTCONSL
00063  DATA DIVISION.                                                   ELTCONSL
00064  WORKING-STORAGE SECTION.                                         ELTCONSL
00065  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTCONSL
00066      '***ELTCONSL WS BEGINS***'.                                  ELTCONSL
00067 *** 12/05/86 DUMMY FIELD TO TRY TO GET BY COBOL II COMPILE BUG ***ELTCONSL
00068  01  WS-DUMMY-FIELD              PIC 9(2)   VALUE ZERO.           ELTCONSL
00069  01  WS-BEN-SCOPE                PIC X(4) VALUE 'XXXX'.           ELTCONSL
00070  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTCONSL
00071                                                                   ELTCONSL
00072  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTCONSL
00073                                                                   ELTCONSL
00074 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTCONSL
00075  01  WS-WORK-FIELDS.                                              ELTCONSL
00076      05  WS-CHAR-0                     PIC X.                     ELTCONSL
00077      05  WS-DISPLAY-MAX-VISIT-TEXT     PIC X.                     ELTCONSL
00078      05  WS-MAX-AMT-TEXT-SW            PIC X.                     ELTCONSL
00079      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTCONSL
00080      05  WS-DISPLAY-PVE-TEXT           PIC X.                     ELTCONSL
00081      05  WS-HOLD1                      PIC X(10).                 ELTCONSL
00082      05  WS-HOLD2                      PIC X(10).                 ELTCONSL
00083      05  WS-DTL-PP                     PIC X(50).                 ELTCONSL
00084      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTCONSL
00085      05  WS-DTL-PER-D                  PIC X(63).                 ELTCONSL
00086      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTCONSL
00087      05  WS-DISPLAY-DAY-PSYCH-TEXT     PIC X.                     ELTCONSL
00088      05  WS-DISPLAY-NIGHT-PSYCH-TEXT   PIC X.                     ELTCONSL
00089      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTCONSL
00090      05  WS-DISPLAY-SERVIC-REND-TEXT   PIC X.                     ELTCONSL
00091      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTCONSL
00092      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTCONSL
00093      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTCONSL
00094      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTCONSL
00095      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTCONSL
00096      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTCONSL
00097      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTCONSL
00098      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTCONSL
00099                                                                   ELTCONSL
00100 /     B E N   P R O V   I D S   B Y   T Y P E - INPATIENT         ELTCONSL
00101  01  WS-BEN-PROV-ID-IP.                                           ELTCONSL
00102      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +1. ELTCONSL
00103      05  WS-PROF-IP-TAB.                                          ELTCONSL
00104        10  FILLER                      PIC X(6)  VALUE 'CONI E'.  ELTCONSL
00105      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTCONSL
00106                                        PIC X(6) OCCURS 01 TIMES.  ELTCONSL
00107                                                                   ELTCONSL
00108  01  WS-TABLE-MAX-CNT                  PIC S9(4) COMP   VALUE +2. ELTCONSL
00109                                                                   ELTCONSL
00110 /     B E N   P R O V   I D S   B Y   T Y P E - OUTPATIENT        ELTCONSL
00111  01  WS-BEN-PROV-ID-OP.                                           ELTCONSL
00112      05  WS-PROF-OP-CNT                PIC S9(4) COMP   VALUE +2. ELTCONSL
00113      05  WS-PROF-OP-TAB.                                          ELTCONSL
00114        10  FILLER                      PIC X(6)  VALUE 'CONO E'.  ELTCONSL
00115        10  FILLER                      PIC X(6)  VALUE 'ASOP E'.  ELTCONSL
00116      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTCONSL
00117                                        PIC X(6) OCCURS 02 TIMES.  ELTCONSL
00118                                                                   ELTCONSL
00119 /                L I T E R A L S                                  ELTCONSL
00120  01  WS-PROGRAM-LITERALS.                                         ELTCONSL
00121    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTCONSL
00122    05  WS-DAYS                     PIC X(04) VALUE 'DAYS'.        ELTCONSL
00123    05  WS-YES                      PIC X     VALUE 'Y'.           ELTCONSL
00124    05  WS-NO                       PIC X     VALUE 'N'.           ELTCONSL
00125    05  WS-FIRSTTIME-IND              PIC X.                       ELTCONSL
00126        88  WS-NOT-FIRST-TIME             VALUE 'N'.               ELTCONSL
00127    05  WS-BASIC-LIT                PIC X(10) VALUE                ELTCONSL
00128        '   BASIC: '.                                              ELTCONSL
00129    05  WS-4096                     PIC S9(8) COMP  VALUE +4096.   ELTCONSL
00130    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTCONSL
00131          VALUE 'SPILLOVER COINSURANCE: '.                         ELTCONSL
00132    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTCONSL
00133          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTCONSL
00134    05  WS-SERVICES-RENDERED.                                      ELTCONSL
00135      10  FILLER                    PIC X(26)                      ELTCONSL
00136          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTCONSL
00137    05  WS-PAYMNT-BASED.                                           ELTCONSL
00138      10  FILLER                    PIC X(21)                      ELTCONSL
00139          VALUE 'PAYMENT IS BASED ON: '.                           ELTCONSL
00140                                                                   ELTCONSL
00141 /            D I S P L A Y   L I N E S                            ELTCONSL
00142  01  WS-ELS-DISPLAY-LINES.                                        ELTCONSL
00143    05  WS-HDR-2-PROF-IP.                                          ELTCONSL
00144      10  FILLER                    PIC X(26) VALUE SPACES.        ELTCONSL
00145      10  FILLER                    PIC X(13)                      ELTCONSL
00146          VALUE 'CONSULTATION '.                                   ELTCONSL
00147      10  WS-HDR2-IPOP-MSG          PIC X(23) VALUE SPACES.        ELTCONSL
00148      10  FILLER                    PIC X(17) VALUE LOW-VALUES.    ELTCONSL
00149                                                                   ELTCONSL
00150    05  WS-CONSULTATION-SER-ARE.                                   ELTCONSL
00151      10  FILLER                    PIC X(27)                      ELTCONSL
00152          VALUE 'CONSULTATION SERVICES ARE  '.                     ELTCONSL
00153      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTCONSL
00154                                                                   ELTCONSL
00155    05  WS-FOLLOW-BENEFIT.                                         ELTCONSL
00156      10  FILLER                    PIC X(21) VALUE                ELTCONSL
00157          'COVERED SERVICES ARE:'.                                 ELTCONSL
00158                                                                   ELTCONSL
00159    05  WS-PAY-CONSDR-TEXT1.                                       ELTCONSL
00160      10  FILLER                    PIC X(45)                      ELTCONSL
00161        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTCONSL
00162    05  WS-PAY-CONSDR-TEXT2.                                       ELTCONSL
00163      10  FILLER                    PIC X(44)                      ELTCONSL
00164        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTCONSL
00165                                                                   ELTCONSL
00166    05  WS-SERVICES-2ND.                                           ELTCONSL
00167      10  FILLER                    PIC X(21) VALUE SPACES.        ELTCONSL
00168      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTCONSL
00169      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTCONSL
00170                                                                   ELTCONSL
00171    05  WS-SERVICES-PAYABLE.                                       ELTCONSL
00172      10  FILLER                    PIC X(40) VALUE                ELTCONSL
00173          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTCONSL
00174      10  FILLER                    PIC X(39) VALUE LOW-VALUES.    ELTCONSL
00175                                                                   ELTCONSL
00176    05  WS-BASIC.                                                  ELTCONSL
00177      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00178      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTCONSL
00179      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTCONSL
00180                                                                   ELTCONSL
00181    05  WS-BASIC-VISIT-AMT.                                        ELTCONSL
00182      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00183      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTCONSL
00184      10  WS-BASIC-MAX-AMT          PIC ZZ9.99-.                   ELTCONSL
00185                                                                   ELTCONSL
00186    05  WS-SUPPLEMENTAL.                                           ELTCONSL
00187      10  FILLER                    PIC X(16)                      ELTCONSL
00188          VALUE '  SUPPLEMENTAL: '.                                ELTCONSL
00189      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTCONSL
00190                                                                   ELTCONSL
00191    05  WS-SUPPL-VISIT-AMT.                                        ELTCONSL
00192      10  FILLER                    PIC X(16)                      ELTCONSL
00193          VALUE '  SUPPLEMENTAL: '.                                ELTCONSL
00194      10  WS-SUPPL-MAX-AMT          PIC ZZ9.99-.                   ELTCONSL
00195                                                                   ELTCONSL
00196    05  WS-BASIC-PERCENT.                                          ELTCONSL
00197      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00198      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTCONSL
00199      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTCONSL
00200                                                                   ELTCONSL
00201    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTCONSL
00202      10  FILLER                    PIC X(16)                      ELTCONSL
00203          VALUE '  SUPPLEMENTAL: '.                                ELTCONSL
00204      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTCONSL
00205                                                                   ELTCONSL
00206    05  WS-BASIC-PER-D.                                            ELTCONSL
00207      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00208      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTCONSL
00209      10  WS-DTL-BASIC-PER-D        PIC X(40) VALUE SPACES.        ELTCONSL
00210      10  FILLER                    PIC X     VALUE SPACE.         ELTCONSL
00211      10  WS-DTL-BASIC-PER-D-AMT    PIC ZZZZ9.99.                  ELTCONSL
00212      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTCONSL
00213                                                                   ELTCONSL
00214    05  WS-SUPP-PER-D.                                             ELTCONSL
00215      10  FILLER                    PIC X(16)                      ELTCONSL
00216          VALUE '  SUPPLEMENTAL: '.                                ELTCONSL
00217      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTCONSL
00218      10  FILLER                    PIC X     VALUE SPACE.         ELTCONSL
00219      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTCONSL
00220      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTCONSL
00221                                                                   ELTCONSL
00222    05  WS-MAX-VISITS.                                             ELTCONSL
00223      10  FILLER                    PIC X(34)                      ELTCONSL
00224          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE: '.              ELTCONSL
00225      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTCONSL
00226                                                                   ELTCONSL
00227    05  WS-MAX-AMT-TEXT.                                           ELTCONSL
00228      10  FILLER                    PIC X(33)                      ELTCONSL
00229          VALUE 'THE MAXIMUM AMOUNT PER VISIT IS: '.               ELTCONSL
00230      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTCONSL
00231                                                                   ELTCONSL
00232    05  WS-UNLIMITED.                                              ELTCONSL
00233      10  WS-DTL-UNLIMITED      PIC X(20).                         ELTCONSL
00234      10  FILLER                PIC X(26).                         ELTCONSL
00235    05  WS-MAX-DAYS REDEFINES WS-UNLIMITED.                        ELTCONSL
00236      10  FILLER                PIC X.                             ELTCONSL
00237      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTCONSL
00238      10  FILLER                PIC X.                             ELTCONSL
00239      10  WS-DAYS-LITERAL       PIC X(04).                         ELTCONSL
00240      10  FILLER                PIC X.                             ELTCONSL
00241      10  WS-DTL-MAX-IND        PIC X(20).                         ELTCONSL
00242      10  FILLER                PIC X(16).                         ELTCONSL
00243                                                                   ELTCONSL
00244    05  WS-ASOP.                                                   ELTCONSL
00245      10  FILLER                    PIC X(47)                      ELTCONSL
00246          VALUE 'BENEFITS FOR ASOP ARE AVAILABLE FOR:           '. ELTCONSL
00247      10  FILLER                    PIC X(32) VALUE LOW-VALUES.    ELTCONSL
00248                                                                   ELTCONSL
00249    05  WS-DAYS-REDUCED.                                           ELTCONSL
00250      10  FILLER                    PIC X(24) VALUE                ELTCONSL
00251          ' BASIC DAYS ARE REDUCED '.                              ELTCONSL
00252      10  WS-DTL-DAYS-REDUCED-APL   PIC Z9.                        ELTCONSL
00253      10  FILLER                    PIC X(05) VALUE ' FOR '.       ELTCONSL
00254      10  WS-DTL-DAYS-REDUCED-BASE  PIC Z9.                        ELTCONSL
00255      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTCONSL
00256                                                                   ELTCONSL
00257    05  WS-SPILLOVER-FL-RT-PER-D.                                  ELTCONSL
00258      10  FILLER                    PIC X(29)                      ELTCONSL
00259          VALUE 'SPILLOVER FLAT RATE PER DIEM '.                   ELTCONSL
00260      10  WS-DTL-SPILLOVER-FL-RT    PIC X(46) VALUE SPACES.        ELTCONSL
00261      10  FILLER                    PIC X(04) VALUE LOW-VALUES.    ELTCONSL
00262 *************************************************************     ELTCONSL
00263    05  WS-DIFF-PROVIDER.                                          ELTCONSL
00264      10  FILLER                    PIC X(42) VALUE                ELTCONSL
00265          'IF THE SAME PROVIDER IS BILLING INPATIENT '.            ELTCONSL
00266      10  FILLER                    PIC X(21) VALUE                ELTCONSL
00267          'CONSULTATION/SURGERY:'.                                 ELTCONSL
00268      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTCONSL
00269                                                                   ELTCONSL
00270    05  WS-SAME-PROVIDER.                                          ELTCONSL
00271      10  FILLER                    PIC X(42) VALUE                ELTCONSL
00272          'IF THE SAME PROVIDER IS BILLING INPATIENT '.            ELTCONSL
00273      10  FILLER                    PIC X(21) VALUE                ELTCONSL
00274          'CONSULTATION/MEDICAL:'.                                 ELTCONSL
00275      10  FILLER                    PIC X(16) VALUE LOW-VALUES.    ELTCONSL
00276 ****************************************************************  ELTCONSL
00277    05  WS-MEDI-SURG-OB.                                           ELTCONSL
00278      10  FILLER                    PIC X(22)                      ELTCONSL
00279          VALUE '   MEDICAL/SURGERY/OB '.                          ELTCONSL
00280      10  FILLER                    PIC X(57) VALUE LOW-VALUES.    ELTCONSL
00281                                                                   ELTCONSL
00282    05  WS-MEDI-INP-GT-FC-PSYCH.                                   ELTCONSL
00283      10  FILLER                    PIC X(65)                      ELTCONSL
00284          VALUE '   MEDICAL/INPATIENT GROUP THERAPY/FAMILY COUNSELIELTCONSL
00285 -              '/PSYCHOTHERAPY'.                                  ELTCONSL
00286      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTCONSL
00287                                                                   ELTCONSL
00288    05  WS-MEDI-INP-RAD-THERPY.                                    ELTCONSL
00289      10  FILLER                    PIC X(40)                      ELTCONSL
00290          VALUE '   MEDICAL/INPATIENT RADIATION THERAPY'.          ELTCONSL
00291      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTCONSL
00292                                                                   ELTCONSL
00293    05  WS-MEDI-INP-CHEMTHERPY.                                    ELTCONSL
00294      10  FILLER                    PIC X(33)                      ELTCONSL
00295          VALUE '   MEDICAL/INPATIENT CHEMOTHERAPY'.               ELTCONSL
00296      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTCONSL
00297                                                                   ELTCONSL
00298    05  WS-MEDI-MEDI.                                              ELTCONSL
00299      10  FILLER                    PIC X(18)                      ELTCONSL
00300          VALUE '   MEDICAL/MEDICAL'.                              ELTCONSL
00301      10  FILLER                    PIC X(61) VALUE LOW-VALUES.    ELTCONSL
00302                                                                   ELTCONSL
00303    05  WS-BASIC-1-1.                                              ELTCONSL
00304      10  FILLER                    PIC X(10) VALUE                ELTCONSL
00305          '   BASIC: '.                                            ELTCONSL
00306      10  WS-DTL-BASIC-1-1          PIC X(70) VALUE SPACES.        ELTCONSL
00307                                                                   ELTCONSL
00308    05  WS-BASIC-1-2.                                              ELTCONSL
00309      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00310      10  WS-DTL-BASIC-1-2          PIC X(70) VALUE SPACES.        ELTCONSL
00311                                                                   ELTCONSL
00312    05  WS-BASIC-2-1.                                              ELTCONSL
00313      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00314      10  WS-DTL-BASIC-2-1          PIC X(70) VALUE SPACES.        ELTCONSL
00315                                                                   ELTCONSL
00316    05  WS-BASIC-2-2.                                              ELTCONSL
00317      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00318      10  WS-DTL-BASIC-2-2          PIC X(70) VALUE SPACES.        ELTCONSL
00319                                                                   ELTCONSL
00320    05  WS-BASIC-3-1.                                              ELTCONSL
00321      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00322      10  WS-DTL-BASIC-3-1          PIC X(70) VALUE SPACES.        ELTCONSL
00323                                                                   ELTCONSL
00324    05  WS-BASIC-3-2.                                              ELTCONSL
00325      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00326      10  WS-DTL-BASIC-3-2          PIC X(70) VALUE SPACES.        ELTCONSL
00327                                                                   ELTCONSL
00328    05  WS-BASIC-4-1.                                              ELTCONSL
00329      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00330      10  WS-DTL-BASIC-4-1          PIC X(70) VALUE SPACES.        ELTCONSL
00331                                                                   ELTCONSL
00332    05  WS-BASIC-4-2.                                              ELTCONSL
00333      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00334      10  WS-DTL-BASIC-4-2          PIC X(70) VALUE SPACES.        ELTCONSL
00335                                                                   ELTCONSL
00336    05  WS-SUPP-1-1.                                               ELTCONSL
00337      10 FILLER                     PIC X(17) VALUE                ELTCONSL
00338          '   SUPPLEMENTAL: '.                                     ELTCONSL
00339      10  WS-DTL-SUPP-1-1           PIC X(62) VALUE SPACES.        ELTCONSL
00340                                                                   ELTCONSL
00341    05  WS-SUPP-1-2.                                               ELTCONSL
00342      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00343      10  WS-DTL-SUPP-1-2           PIC X(70) VALUE SPACES.        ELTCONSL
00344                                                                   ELTCONSL
00345    05  WS-SUPP-2-1.                                               ELTCONSL
00346      10 FILLER                     PIC X(17) VALUE                ELTCONSL
00347          '   SUPPLEMENTAL: '.                                     ELTCONSL
00348      10  WS-DTL-SUPP-2-1           PIC X(62) VALUE SPACES.        ELTCONSL
00349                                                                   ELTCONSL
00350    05  WS-SUPP-2-2.                                               ELTCONSL
00351      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00352      10  WS-DTL-SUPP-2-2           PIC X(70) VALUE SPACES.        ELTCONSL
00353                                                                   ELTCONSL
00354    05  WS-SUPP-3-1.                                               ELTCONSL
00355      10 FILLER                     PIC X(17) VALUE                ELTCONSL
00356          '   SUPPLEMENTAL: '.                                     ELTCONSL
00357      10  WS-DTL-SUPP-3-1           PIC X(62) VALUE SPACES.        ELTCONSL
00358                                                                   ELTCONSL
00359    05  WS-SUPP-3-2.                                               ELTCONSL
00360      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00361      10  WS-DTL-SUPP-3-2           PIC X(70) VALUE SPACES.        ELTCONSL
00362                                                                   ELTCONSL
00363    05  WS-SUPP-4-1.                                               ELTCONSL
00364      10 FILLER                     PIC X(17) VALUE                ELTCONSL
00365          '   SUPPLEMENTAL: '.                                     ELTCONSL
00366      10  WS-DTL-SUPP-4-1           PIC X(62) VALUE SPACES.        ELTCONSL
00367                                                                   ELTCONSL
00368    05  WS-SUPP-4-2.                                               ELTCONSL
00369      10  FILLER                    PIC X(09) VALUE SPACES.        ELTCONSL
00370      10  WS-DTL-SUPP-4-2           PIC X(70) VALUE SPACES.        ELTCONSL
00371                                                                   ELTCONSL
00372    05  WS-CONTACT-CONTRACT.                                       ELTCONSL
00373      10  FILLER                    PIC X(50)                      ELTCONSL
00374        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTCONSL
00375      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTCONSL
00376                                                                   ELTCONSL
00377    05  WS-CONTRACT-RELATED.                                       ELTCONSL
00378      10  FILLER                    PIC X(49)                      ELTCONSL
00379        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTCONSL
00380      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTCONSL
00381                                                                   ELTCONSL
00382    05  WS-PVE-TEXT.                                               ELTCONSL
00383      10  FILLER                    PIC X(49)                      ELTCONSL
00384        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.     '. ELTCONSL
00385      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTCONSL
00386                                                                   ELTCONSL
00387    05  WS-INSTITUTIONAL-SERV.                                     ELTCONSL
00388        10  FILLER                 PIC X(30)      VALUE            ELTCONSL
00389                'CONSULTATION SERVICES ARE NOT '.                  ELTCONSL
00390        10  FILLER                 PIC X(33)      VALUE            ELTCONSL
00391                'COVERED AS A INSTITUTIONAL CHARGE'.               ELTCONSL
00392  01  WS-END                            PIC X(16)  VALUE           ELTCONSL
00393      '*** W/S ENDS ***'.                                          ELTCONSL
00394 /             L I N K A G E   S E C T I O N                       ELTCONSL
00395  LINKAGE SECTION.                                                 ELTCONSL
00396  01  DFHCOMMAREA.                                                 ELTCONSL
00397      COPY ELSCOMMC.                                               ELTCONSL
00398 /  *** C I A  AREA ***                                            ELTCONSL
00399      COPY ELSCIA2C.                                               ELTCONSL
00400 /*** IO PARM AREA ***                                             ELTCONSL
00401      COPY ELSIOPMC.                                               ELTCONSL
00402 /  *** KEY AREA ***                                               ELTCONSL
00403      COPY ELSKEYSC.                                               ELTCONSL
00404 /  *** OUTPUT TEXT AREA ***                                       ELTCONSL
00405      COPY ELSOUTPC.                                               ELTCONSL
00406 /  *** TOPIC SELECTION AREA ***                                   ELTCONSL
00407      COPY ELSSSCBC.                                               ELTCONSL
00408 /  *** CODE MANUAL INTERFACE ***                                  ELTCONSL
00409      COPY ELSCMIFC.                                               ELTCONSL
00410 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTCONSL
00411      COPY ELSCMDSC.                                               ELTCONSL
00412 /  *** BENEFIT PROVISION TABLE **                                 ELTCONSL
00413      COPY ELSPRVNC.                                               ELTCONSL
00414 /  *** COMPRESSION TEXT WORK AREA ***                             ELTCONSL
00415      COPY ELSTCWAC.                                               ELTCONSL
00416 /   P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D . S ELTCONSL
00417      COPY ELSPLGSW.                                               ELTCONSL
00418 /    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTCONSL
00419      COPY ELSPLGTB.                                               ELTCONSL
00420 /        G R O U P   S P E C I F I C   R E C O R D                ELTCONSL
00421 /        C O N T R A C T   R E C O R D                            ELTCONSL
00422  01  CONTRACT-RECORD.                                             ELTCONSL
00423      COPY GCCONTRC.                                               ELTCONSL
00424 /                  M A I N L I N E                                ELTCONSL
00425  PROCEDURE DIVISION.                                              ELTCONSL
00426                                                                   ELTCONSL
00427 ******************************************************************ELTCONSL
00428 *                                                                 ELTCONSL
00429 *   PERFORM THE MAINLINE OPERATIONS.                              ELTCONSL
00430 *                                                                 ELTCONSL
00431 ******************************************************************ELTCONSL
00432  0000-MAINLINE.                                                   ELTCONSL
00433                                                                   ELTCONSL
00434      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTCONSL
00435         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTCONSL
00436         EXEC CICS  ABEND  ABCODE('EL01')  END-EXEC.               ELTCONSL
00437                                                                   ELTCONSL
00438      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTCONSL
00439          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTCONSL
00440                                                                   ELTCONSL
00441      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTCONSL
00442      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00443          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTCONSL
00444                                                                   ELTCONSL
00445      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTCONSL
00446      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00447          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTCONSL
00448                                                                   ELTCONSL
00449      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTCONSL
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00451          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTCONSL
00452                                                                   ELTCONSL
00453      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTCONSL
00454      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00455          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTCONSL
00456                                                                   ELTCONSL
00457      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTCONSL
00458      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00459          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTCONSL
00460                                                                   ELTCONSL
00461      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTCONSL
00462      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00463          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTCONSL
00464                                                                   ELTCONSL
00465      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTCONSL
00466                                                                   ELTCONSL
00467      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTCONSL
00468              (WS-TABLE-MAX-CNT *  LENGTH OF PVN-BEN-PROVN-TBL).   ELTCONSL
00469                                                                   ELTCONSL
00470      MOVE '0'  TO  WS-CHAR-0.                                     ELTCONSL
00471                                                                   ELTCONSL
00472      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTCONSL
00473                                                                   ELTCONSL
00474      SET CIA-STG-GETMAIN  TO TRUE.                                ELTCONSL
00475                                                                   ELTCONSL
00476      EXEC CICS LINK                                               ELTCONSL
00477                PROGRAM('ELUSTGMG')                                ELTCONSL
00478                COMMAREA(DFHCOMMAREA)                              ELTCONSL
00479      END-EXEC.                                                    ELTCONSL
00480                                                                   ELTCONSL
00481      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTCONSL
00482      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00483          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTCONSL
00484                                                                   ELTCONSL
00485      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTCONSL
00486         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTCONSL
00487                                                                   ELTCONSL
00488      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTCONSL
00489         PERFORM 2000-PROFESSIONAL-IP-RTNE                         ELTCONSL
00490         PERFORM 2100-PROFESSIONAL-OP-RTNE.                        ELTCONSL
00491                                                                   ELTCONSL
00492 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTCONSL
00493      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
00494      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTCONSL
00495      MOVE 'E'  TO  COF-FUNCTION.                                  ELTCONSL
00496                                                                   ELTCONSL
00497      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
00498      END-EXEC.                                                    ELTCONSL
00499                                                                   ELTCONSL
00500      GOBACK.                                                      ELTCONSL
00501                                                                   ELTCONSL
00502 /        I N S T I T U T I O N A L  I P   R T N E                 ELTCONSL
00503 ***************************************************************** ELTCONSL
00504 *        I N S T I T U T I O N A L  I P   R T N E                 ELTCONSL
00505 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTCONSL
00506 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTCONSL
00507 ***************************************************************** ELTCONSL
00508  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTCONSL
00509      MOVE '1000'  TO  WS-PARA-ID.                                 ELTCONSL
00510                                                                   ELTCONSL
00511      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
00512      MOVE 'INSTITUTIONAL ' TO WS-HDR2-IPOP-MSG.                   ELTCONSL
00513      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONSL
00514      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE(2).                ELTCONSL
00515      MOVE LOW-VALUES             TO COF-DTL-LINE(1).              ELTCONSL
00516      MOVE WS-INSTITUTIONAL-SERV TO COF-DTL-LINE(2).               ELTCONSL
00517      MOVE +2   TO COF-NBR-DTL-LINES.                              ELTCONSL
00518 **********CALL OUTPUT FOR NEW PAGE WITH TEXT*********             ELTCONSL
00519      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONSL
00520      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
00521                                                                   ELTCONSL
00522      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
00523      END-EXEC.                                                    ELTCONSL
00524 /        P R O F E S S I O N A L   I P   R T N E                  ELTCONSL
00525 ***************************************************************** ELTCONSL
00526 *        P R O F E S S I O N A L   I P   R T N E                  ELTCONSL
00527 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTCONSL
00528 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTCONSL
00529 ***************************************************************** ELTCONSL
00530  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTCONSL
00531 ****************************************************              ELTCONSL
00532      MOVE '2000'  TO  WS-PARA-ID.                                 ELTCONSL
00533                                                                   ELTCONSL
00534      MOVE 'INPATIENT PROFESSIONAL ' TO WS-HDR2-IPOP-MSG.          ELTCONSL
00535                                                                   ELTCONSL
00536      MOVE 'Y'     TO WS-FIRSTTIME-IND.                            ELTCONSL
00537                                                                   ELTCONSL
00538      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTCONSL
00539      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTCONSL
00540         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCONSL
00541         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCONSL
00542                                                                   ELTCONSL
00543      GO TO 2020-CALL-COVERAGE.                                    ELTCONSL
00544  2010-MOVE-IN-PROF-IP.                                            ELTCONSL
00545      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCONSL
00546      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTCONSL
00547                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCONSL
00548      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTCONSL
00549                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTCONSL
00550                                                                   ELTCONSL
00551  2020-CALL-COVERAGE.                                              ELTCONSL
00552      MOVE '2020'              TO  WS-PARA-ID.                     ELTCONSL
00553      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE(2).                ELTCONSL
00554 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTCONSL
00555      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTCONSL
00556      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONSL
00557      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
00558                                                                   ELTCONSL
00559      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
00560      END-EXEC.                                                    ELTCONSL
00561 ****************************************************              ELTCONSL
00562                                                                   ELTCONSL
00563      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTCONSL
00564      MOVE WS-CONSULTATION-SER-ARE TO SSB-TOPIC-PHRASE.            ELTCONSL
00565                                                                   ELTCONSL
00566      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCONSL
00567      END-EXEC.                                                    ELTCONSL
00568                                                                   ELTCONSL
00569 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTCONSL
00570      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
00571      MOVE ' '  TO  COF-FUNCTION.                                  ELTCONSL
00572      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
00573      END-EXEC.                                                    ELTCONSL
00574 *4/15 END OF TEMPORARY CODE                                       ELTCONSL
00575                                                                   ELTCONSL
00576      IF PVN-COVG-NONE                                             ELTCONSL
00577         GO TO 2099-EXIT.                                          ELTCONSL
00578                                                                   ELTCONSL
00579      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCONSL
00580                                                                   ELTCONSL
00581      MOVE '1' TO PSP-PLACE-TREAT-ELIG-IND,                        ELTCONSL
00582            PSP-PROVN-PRICING-METHD,                               ELTCONSL
00583            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCONSL
00584            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONSL
00585            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCONSL
00586            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONSL
00587            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCONSL
00588            PSP-SPILL-OVER-DED-APL-IND,                            ELTCONSL
00589 ****       PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTCONSL
00590            PSP-COST-CONT-PYMT-ELIG-IND,                           ELTCONSL
00591            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCONSL
00592            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCONSL
00593            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCONSL
00594            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCONSL
00595            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCONSL
00596            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCONSL
00597            PSE-BEN-SCOPE-ID,                                      ELTCONSL
00598            PSE-MAX-AMT-PER-VISIT,                                 ELTCONSL
00599            PSE-BEN-MAX-VISITS-IND,                                ELTCONSL
00600            PSE-BEN-MAX-VISITS-DAYS,                               ELTCONSL
00601            PSD-FLAT-RATE-PDM-AMT.                                 ELTCONSL
00602 ****       PSD-BEN-MAX-VISIT-IND,                                 ELTCONSL
00603 ****       PSD-BEN-MAX-VISIT-DAYS.                                ELTCONSL
00604                                                                   ELTCONSL
00605      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCONSL
00606      END-EXEC.                                                    ELTCONSL
00607                                                                   ELTCONSL
00608      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCONSL
00609      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
00610          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCONSL
00611                                                                   ELTCONSL
00612      PERFORM 2030-FIND-FIRST-NONZERO                              ELTCONSL
00613         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCONSL
00614         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTCONSL
00615                                                                   ELTCONSL
00616      GO TO 2099-EXIT.                                             ELTCONSL
00617  2030-FIND-FIRST-NONZERO.                                         ELTCONSL
00618      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTCONSL
00619      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCONSL
00620         NEXT SENTENCE                                             ELTCONSL
00621      ELSE                                                         ELTCONSL
00622         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTCONSL
00623                                                                   ELTCONSL
00624  2040-BUILD-SCREEN-LINES.                                         ELTCONSL
00625      MOVE '2040'  TO  WS-PARA-ID.                                 ELTCONSL
00626                                                                   ELTCONSL
00627      SET PLT-INDEX1 TO                                            ELTCONSL
00628         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTCONSL
00629                                                                   ELTCONSL
00630      IF WS-NOT-FIRST-TIME                                         ELTCONSL
00631         MOVE 'P'    TO COF-FUNCTION                               ELTCONSL
00632         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTCONSL
00633         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONSL
00634                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
00635         END-EXEC                                                  ELTCONSL
00636      ELSE                                                         ELTCONSL
00637        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTCONSL
00638                                                                   ELTCONSL
00639      MOVE +1    TO  WS-CIA.                                       ELTCONSL
00640      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTCONSL
00641                                                                   ELTCONSL
00642      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTCONSL
00643         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTCONSL
00644            SET PLT-INDEX2  TO  2                                  ELTCONSL
00645         ELSE                                                      ELTCONSL
00646            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTCONSL
00647            GO TO 2099-EXIT                                        ELTCONSL
00648      ELSE                                                         ELTCONSL
00649         SET PLT-INDEX2  TO  1.                                    ELTCONSL
00650      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCONSL
00651                                                                   ELTCONSL
00652      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTCONSL
00653      ADD +1                 TO WS-CIA.                            ELTCONSL
00654                                                                   ELTCONSL
00655      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTCONSL
00656      ADD +1                 TO WS-CIA.                            ELTCONSL
00657                                                                   ELTCONSL
00658      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTCONSL
00659                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTCONSL
00660                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTCONSL
00661                                                                   ELTCONSL
00662      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTCONSL
00663        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTCONSL
00664        UNTIL WS-SUB2 GREATER WS-PROF-IP-CNT.                      ELTCONSL
00665                                                                   ELTCONSL
00666      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONSL
00667 *************************************************************     ELTCONSL
00668 **** SERVICES MAY BE RENDERED                                     ELTCONSL
00669 **************************************************************    ELTCONSL
00670      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00671        SET  PLT-INDEX2       TO  1                                ELTCONSL
00672       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
00673         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
00674              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTCONSL
00675                                                                   ELTCONSL
00676      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00677        SET PLT-INDEX2        TO 2                                 ELTCONSL
00678       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
00679         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
00680              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTCONSL
00681                                                                   ELTCONSL
00682      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTCONSL
00683          ADD  +1               TO  WS-CIA                         ELTCONSL
00684          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTCONSL
00685          ADD  +1               TO  WS-CIA.                        ELTCONSL
00686                                                                   ELTCONSL
00687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00688        SET  PLT-INDEX2       TO  1                                ELTCONSL
00689       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
00690         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
00691          PERFORM 4000-PLACE-OF-TREATMENT                          ELTCONSL
00692          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
00693          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
00694          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
00695          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
00696          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTCONSL
00697          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTCONSL
00698          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
00699             ADD +1                TO WS-CIA                       ELTCONSL
00700             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
00701             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
00702          ELSE                                                     ELTCONSL
00703           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
00704                                                                   ELTCONSL
00705      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00706        SET PLT-INDEX2        TO 2                                 ELTCONSL
00707       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
00708         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
00709          PERFORM 4000-PLACE-OF-TREATMENT                          ELTCONSL
00710          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
00711          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
00712          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
00713          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
00714          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTCONSL
00715          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTCONSL
00716          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
00717             ADD +1                TO WS-CIA                       ELTCONSL
00718             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
00719             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
00720          ELSE                                                     ELTCONSL
00721           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
00722                                                                   ELTCONSL
00723 ************************************************************      ELTCONSL
00724 ************ PAYMENT IS BASED ON                                  ELTCONSL
00725 ************************************************************      ELTCONSL
00726      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZEROS           ELTCONSL
00727        SET  PLT-INDEX2       TO  1                                ELTCONSL
00728        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
00729         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZEROS   ELTCONSL
00730           IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)             ELTCONSL
00731                NOT = '00  '                                       ELTCONSL
00732              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTCONSL
00733                                                                   ELTCONSL
00734      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00735        SET PLT-INDEX2        TO 2                                 ELTCONSL
00736        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
00737         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
00738            AND NOT = '00  '                                       ELTCONSL
00739              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTCONSL
00740                                                                   ELTCONSL
00741      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTCONSL
00742          ADD  +1               TO  WS-CIA                         ELTCONSL
00743          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTCONSL
00744          ADD  +1               TO  WS-CIA.                        ELTCONSL
00745                                                                   ELTCONSL
00746      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZEROS           ELTCONSL
00747        SET  PLT-INDEX2       TO  1                                ELTCONSL
00748        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
00749         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZEROS   ELTCONSL
00750          IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)              ELTCONSL
00751                NOT = '00  '                                       ELTCONSL
00752          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTCONSL
00753          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTCONSL
00754          MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO        ELTCONSL
00755                                CMF-CODE-VALUE                     ELTCONSL
00756                                WS-BEN-SCOPE                       ELTCONSL
00757          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
00758          STRING CMF-DESCR-LINE(1) ' '                             ELTCONSL
00759                 CMF-DESCR-LINE(2) ' '                             ELTCONSL
00760                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
00761          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
00762          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
00763          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
00764          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
00765          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
00766          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTCONSL
00767          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTCONSL
00768          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
00769             ADD +1                TO WS-CIA                       ELTCONSL
00770             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
00771             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
00772          ELSE                                                     ELTCONSL
00773           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
00774                                                                   ELTCONSL
00775                                                                   ELTCONSL
00776      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00777        SET PLT-INDEX2        TO 2                                 ELTCONSL
00778        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
00779         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
00780            AND NOT = '00  '                                       ELTCONSL
00781          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTCONSL
00782          MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO        ELTCONSL
00783                                CMF-CODE-VALUE                     ELTCONSL
00784          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTCONSL
00785          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
00786          STRING CMF-DESCR-LINE(1) ' '                             ELTCONSL
00787                 CMF-DESCR-LINE(2) ' '                             ELTCONSL
00788                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
00789          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
00790          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
00791          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
00792          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
00793          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
00794          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTCONSL
00795          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTCONSL
00796          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
00797             ADD +1                TO WS-CIA                       ELTCONSL
00798             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
00799             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
00800          ELSE                                                     ELTCONSL
00801           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
00802 **************************************************************    ELTCONSL
00803 **** SERVICES ARE PAYABLE                                         ELTCONSL
00804 **************************************************************    ELTCONSL
00805      MOVE LOW-VALUES           TO  COF-DTL-LINE(WS-CIA).          ELTCONSL
00806      ADD  +1                   TO  WS-CIA.                        ELTCONSL
00807      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTCONSL
00808      ADD  +1                   TO  WS-CIA.                        ELTCONSL
00809                                                                   ELTCONSL
00810      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00811         SET  PLT-INDEX2           TO  1                           ELTCONSL
00812         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTCONSL
00813                                                                   ELTCONSL
00814      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00815         SET  PLT-INDEX2             TO  2                         ELTCONSL
00816         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTCONSL
00817 *************************************************************     ELTCONSL
00818 **** MAX NUMBER VISITS                                            ELTCONSL
00819 *************************************************************     ELTCONSL
00820      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00821       SET PLT-INDEX2 TO 1                                         ELTCONSL
00822       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
00823        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00824                                         NOT = LOW-VALUES          ELTCONSL
00825          MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.                ELTCONSL
00826                                                                   ELTCONSL
00827      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00828       SET  PLT-INDEX2 TO  2                                       ELTCONSL
00829       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
00830        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00831                                         NOT = LOW-VALUES          ELTCONSL
00832          MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.                ELTCONSL
00833                                                                   ELTCONSL
00834      IF WS-DISPLAY-MAX-VISIT-TEXT = WS-YES                        ELTCONSL
00835          ADD +1             TO WS-CIA                             ELTCONSL
00836          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTCONSL
00837          ADD +1             TO WS-CIA.                            ELTCONSL
00838                                                                   ELTCONSL
00839      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00840       SET  PLT-INDEX2           TO  1                             ELTCONSL
00841       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
00842        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00843                                         NOT = LOW-VALUES          ELTCONSL
00844         PERFORM 4500-MAX-VISITS                                   ELTCONSL
00845         IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)        ELTCONSL
00846                 = ZEROS                                           ELTCONSL
00847           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTCONSL
00848           MOVE WS-UNLIMITED           TO  WS-DTL-BASIC            ELTCONSL
00849           MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                 ELTCONSL
00850           PERFORM 3000-OUTPUT-TEXT                                ELTCONSL
00851         ELSE                                                      ELTCONSL
00852           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)    ELTCONSL
00853                TO WS-DTL-MAX-DAYS                                 ELTCONSL
00854           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTCONSL
00855           MOVE WS-DAYS               TO WS-DAYS-LITERAL           ELTCONSL
00856           MOVE SPACES   TO TCAR-FROM-AREA                         ELTCONSL
00857           STRING WS-DTL-MAX-DAYS ' '                              ELTCONSL
00858                  WS-DAYS ' '                                      ELTCONSL
00859                  WS-DTL-MAX-IND                                   ELTCONSL
00860                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTCONSL
00861           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTCONSL
00862           MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                ELTCONSL
00863           MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
00864           MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
00865           PERFORM TCPR-000-TEXT-UNSTRING                          ELTCONSL
00866           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-BASIC              ELTCONSL
00867           MOVE WS-BASIC              TO COF-DTL-LINE(WS-CIA)      ELTCONSL
00868           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTCONSL
00869               ADD +1                 TO WS-CIA                    ELTCONSL
00870               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTCONSL
00871               PERFORM 3000-OUTPUT-TEXT                            ELTCONSL
00872           ELSE                                                    ELTCONSL
00873             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
00874                                                                   ELTCONSL
00875      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00876       SET  PLT-INDEX2           TO  2                             ELTCONSL
00877       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
00878        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00879                                         NOT = LOW-VALUES          ELTCONSL
00880          PERFORM 4500-MAX-VISITS                                  ELTCONSL
00881          IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTCONSL
00882                  = ZEROS                                          ELTCONSL
00883           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTCONSL
00884           MOVE WS-UNLIMITED           TO  WS-DTL-SUPPLEMENTAL     ELTCONSL
00885           MOVE WS-SUPPLEMENTAL        TO  COF-DTL-LINE(WS-CIA)    ELTCONSL
00886           PERFORM 3000-OUTPUT-TEXT                                ELTCONSL
00887         ELSE                                                      ELTCONSL
00888           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)    ELTCONSL
00889                TO WS-DTL-MAX-DAYS                                 ELTCONSL
00890           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTCONSL
00891           MOVE WS-DAYS               TO WS-DAYS-LITERAL           ELTCONSL
00892           MOVE SPACES   TO TCAR-FROM-AREA                         ELTCONSL
00893           STRING WS-DTL-MAX-DAYS ' '                              ELTCONSL
00894                  WS-DAYS ' '                                      ELTCONSL
00895                  WS-DTL-MAX-IND                                   ELTCONSL
00896                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTCONSL
00897           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTCONSL
00898           MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                ELTCONSL
00899           MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
00900           MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
00901           PERFORM TCPR-000-TEXT-UNSTRING                          ELTCONSL
00902           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-SUPPLEMENTAL       ELTCONSL
00903           MOVE WS-SUPPLEMENTAL       TO COF-DTL-LINE(WS-CIA)      ELTCONSL
00904           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTCONSL
00905               ADD +1                 TO WS-CIA                    ELTCONSL
00906               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTCONSL
00907               PERFORM 3000-OUTPUT-TEXT                            ELTCONSL
00908           ELSE                                                    ELTCONSL
00909             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
00910 ******************************************************************ELTCONSL
00911 **** MAXIMUM AMOUNT PER VISIT                                     ELTCONSL
00912 ***************************************************************** ELTCONSL
00913                                                                   ELTCONSL
00914      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00915       SET PLT-INDEX2 TO 1                                         ELTCONSL
00916       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
00917        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00918                                         NOT = ZEROS               ELTCONSL
00919          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTCONSL
00920                                                                   ELTCONSL
00921      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00922       SET  PLT-INDEX2 TO  2                                       ELTCONSL
00923       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
00924        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00925                                         NOT = ZEROS               ELTCONSL
00926          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTCONSL
00927                                                                   ELTCONSL
00928      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTCONSL
00929          ADD +1             TO WS-CIA                             ELTCONSL
00930          MOVE WS-MAX-AMT-TEXT TO COF-DTL-LINE(WS-CIA).            ELTCONSL
00931                                                                   ELTCONSL
00932      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00933       SET  PLT-INDEX2           TO  1                             ELTCONSL
00934       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
00935        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00936                                         NOT = ZEROS               ELTCONSL
00937          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
00938           TO WS-BASIC-MAX-AMT                                     ELTCONSL
00939          ADD +1             TO WS-CIA                             ELTCONSL
00940          MOVE WS-BASIC-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTCONSL
00941                                                                   ELTCONSL
00942      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00943       SET  PLT-INDEX2           TO  2                             ELTCONSL
00944       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
00945        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
00946                                         NOT = ZEROS               ELTCONSL
00947          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
00948           TO WS-SUPPL-MAX-AMT                                     ELTCONSL
00949          ADD +1             TO WS-CIA                             ELTCONSL
00950          MOVE WS-SUPPL-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTCONSL
00951                                                                   ELTCONSL
00952      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTCONSL
00953             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
00954 ****************************************************************  ELTCONSL
00955 ***IF SAME PROVIDER IS BILLING INPATIENT CONSULTATION/MEDICAL     ELTCONSL
00956 ***                                                  /SURGERY     ELTCONSL
00957 ****************************************************************  ELTCONSL
00958      PERFORM 4600-CHECK-SAME-DIFF-PROVID.                         ELTCONSL
00959                                                                   ELTCONSL
00960 ****************************************************************  ELTCONSL
00961                                                                   ELTCONSL
00962      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
00963         SET  PLT-INDEX2          TO  2                            ELTCONSL
00964         PERFORM 4300-SPILLOVER-COINS                              ELTCONSL
00965         PERFORM 4400-SPILLOVER-DEDUCT.                            ELTCONSL
00966                                                                   ELTCONSL
00967      PERFORM 4550-TRANSF-OTHER-RESP-IND.                          ELTCONSL
00968      PERFORM 4650-SCAN-TAB.                                       ELTCONSL
00969      PERFORM 4675-PAY-CONSID-TEXT.                                ELTCONSL
00970                                                                   ELTCONSL
00971  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTCONSL
00972      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTCONSL
00973                                                                   ELTCONSL
00974      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCONSL
00975         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONSL
00976         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCONSL
00977         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCONSL
00978                                                   CMF-CODE-VALUE  ELTCONSL
00979         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
00980         STRING CMF-DESCR-LINE (1) ' '                             ELTCONSL
00981                CMF-DESCR-LINE (2)                                 ELTCONSL
00982                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
00983         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
00984         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
00985         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
00986         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
00987         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
00988         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTCONSL
00989         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
00990         IF WS-CIA  <  20                                          ELTCONSL
00991            ADD +1  TO  WS-CIA                                     ELTCONSL
00992            MOVE ZERO  TO                                          ELTCONSL
00993                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCONSL
00994         ELSE                                                      ELTCONSL
00995            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCONSL
00996                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
00997            END-EXEC                                               ELTCONSL
00998            MOVE +1  TO  WS-CIA                                    ELTCONSL
00999            MOVE ZERO  TO                                          ELTCONSL
01000                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTCONSL
01001                                                                   ELTCONSL
01002  2090-PROBLEM-WITH-INDICES.                                       ELTCONSL
01003                                                                   ELTCONSL
01004      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCONSL
01005      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONSL
01006                                                                   ELTCONSL
01007      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
01008      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
01009                                                                   ELTCONSL
01010      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
01011      END-EXEC.                                                    ELTCONSL
01012                                                                   ELTCONSL
01013  2099-EXIT.            EXIT.                                      ELTCONSL
01014                                                                   ELTCONSL
01015  2000-EXIT.  EXIT.                                                ELTCONSL
01016 /                                                                 ELTCONSL
01017 ***************************************************************** ELTCONSL
01018  2100-PROFESSIONAL-OP-RTNE SECTION.                               ELTCONSL
01019 ****************************************************              ELTCONSL
01020      MOVE '2100'  TO  WS-PARA-ID.                                 ELTCONSL
01021                                                                   ELTCONSL
01022      MOVE 'OUTPATIENT PROFESSIONAL' TO WS-HDR2-IPOP-MSG.          ELTCONSL
01023                                                                   ELTCONSL
01024      MOVE 'Y'     TO WS-FIRSTTIME-IND.                            ELTCONSL
01025      MOVE WS-PROF-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTCONSL
01026      PERFORM 2110-MOVE-IN-PROF-OP                                 ELTCONSL
01027         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTCONSL
01028         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTCONSL
01029                                                                   ELTCONSL
01030      GO TO 2120-CALL-COVERAGE.                                    ELTCONSL
01031  2110-MOVE-IN-PROF-OP.                                            ELTCONSL
01032      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTCONSL
01033      MOVE WS-PROF-OP-LIST(WS-SUB)  TO                             ELTCONSL
01034                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTCONSL
01035      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTCONSL
01036                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTCONSL
01037                                                                   ELTCONSL
01038  2120-CALL-COVERAGE.                                              ELTCONSL
01039      MOVE '2120'              TO  WS-PARA-ID.                     ELTCONSL
01040      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE(2).                ELTCONSL
01041 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTCONSL
01042      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTCONSL
01043      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTCONSL
01044      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
01045                                                                   ELTCONSL
01046      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
01047      END-EXEC.                                                    ELTCONSL
01048 ****************************************************              ELTCONSL
01049                                                                   ELTCONSL
01050      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTCONSL
01051      MOVE WS-CONSULTATION-SER-ARE TO SSB-TOPIC-PHRASE.            ELTCONSL
01052                                                                   ELTCONSL
01053      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTCONSL
01054      END-EXEC.                                                    ELTCONSL
01055                                                                   ELTCONSL
01056 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTCONSL
01057      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
01058      MOVE ' '  TO  COF-FUNCTION.                                  ELTCONSL
01059                                                                   ELTCONSL
01060      IF PVN-COVG-NONE                                             ELTCONSL
01061        MOVE +2 TO COF-NBR-DTL-LINES.                              ELTCONSL
01062                                                                   ELTCONSL
01063      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
01064      END-EXEC.                                                    ELTCONSL
01065 *4/15 END OF TEMPORARY CODE ************************************* ELTCONSL
01066                                                                   ELTCONSL
01067      IF PVN-COVG-NONE                                             ELTCONSL
01068         GO TO 2199-EXIT.                                          ELTCONSL
01069                                                                   ELTCONSL
01070      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTCONSL
01071                                                                   ELTCONSL
01072         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTCONSL
01073            PSP-PROVN-PRICING-METHD,                               ELTCONSL
01074            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTCONSL
01075            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONSL
01076            PSP-TRANSF-OTHER-RESP-IND,                             ELTCONSL
01077            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTCONSL
01078            PSP-SPILL-OVER-COINS-APL-IND,                          ELTCONSL
01079            PSP-SPILL-OVER-DED-APL-IND,                            ELTCONSL
01080            PSP-COST-CONT-PYMT-ELIG-IND,                           ELTCONSL
01081            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTCONSL
01082            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTCONSL
01083            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTCONSL
01084            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTCONSL
01085            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTCONSL
01086            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTCONSL
01087            PSE-BEN-SCOPE-ID,                                      ELTCONSL
01088            PSE-MAX-AMT-PER-VISIT,                                 ELTCONSL
01089            PSE-BEN-MAX-VISITS-IND,                                ELTCONSL
01090            PSE-BEN-MAX-VISITS-DAYS,                               ELTCONSL
01091            PSD-FLAT-RATE-PDM-AMT.                                 ELTCONSL
01092                                                                   ELTCONSL
01093      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTCONSL
01094      END-EXEC.                                                    ELTCONSL
01095                                                                   ELTCONSL
01096      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTCONSL
01097      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01098          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTCONSL
01099                                                                   ELTCONSL
01100      PERFORM 2130-FIND-FIRST-NONZERO                              ELTCONSL
01101         VARYING WS-SUB  FROM  +1  BY  +1                          ELTCONSL
01102         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTCONSL
01103                                                                   ELTCONSL
01104      GO TO 2199-EXIT.                                             ELTCONSL
01105  2130-FIND-FIRST-NONZERO.                                         ELTCONSL
01106      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTCONSL
01107      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTCONSL
01108         NEXT SENTENCE                                             ELTCONSL
01109      ELSE                                                         ELTCONSL
01110         PERFORM 2140-BUILD-SCREEN-LINES.                          ELTCONSL
01111                                                                   ELTCONSL
01112  2140-BUILD-SCREEN-LINES.                                         ELTCONSL
01113      MOVE '2140'  TO  WS-PARA-ID.                                 ELTCONSL
01114                                                                   ELTCONSL
01115      SET PLT-INDEX1 TO                                            ELTCONSL
01116         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTCONSL
01117                                                                   ELTCONSL
01118      IF WS-NOT-FIRST-TIME                                         ELTCONSL
01119         MOVE 'P'    TO COF-FUNCTION                               ELTCONSL
01120         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTCONSL
01121         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTCONSL
01122                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
01123         END-EXEC                                                  ELTCONSL
01124      ELSE                                                         ELTCONSL
01125        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTCONSL
01126                                                                   ELTCONSL
01127      MOVE +1    TO  WS-CIA.                                       ELTCONSL
01128      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTCONSL
01129                                                                   ELTCONSL
01130      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTCONSL
01131         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTCONSL
01132            SET PLT-INDEX2  TO  2                                  ELTCONSL
01133         ELSE                                                      ELTCONSL
01134            PERFORM 2190-PROBLEM-WITH-INDICES                      ELTCONSL
01135            GO TO 2199-EXIT                                        ELTCONSL
01136      ELSE                                                         ELTCONSL
01137         SET PLT-INDEX2  TO  1.                                    ELTCONSL
01138      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTCONSL
01139                                                                   ELTCONSL
01140      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTCONSL
01141      ADD +1                 TO WS-CIA.                            ELTCONSL
01142                                                                   ELTCONSL
01143      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTCONSL
01144      ADD +1                 TO WS-CIA.                            ELTCONSL
01145                                                                   ELTCONSL
01146      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTCONSL
01147                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTCONSL
01148                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTCONSL
01149                                                                   ELTCONSL
01150      PERFORM 2150-ZERO-ALL-WITH-SAME-NO                           ELTCONSL
01151         VARYING WS-SUB2 FROM WS-SUB BY +1                         ELTCONSL
01152         UNTIL WS-SUB2 GREATER WS-PROF-OP-CNT.                     ELTCONSL
01153      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTCONSL
01154 *************************************************************     ELTCONSL
01155 **** SERVICES MAY BE RENDERED                                     ELTCONSL
01156 **************************************************************    ELTCONSL
01157      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01158        SET  PLT-INDEX2       TO  1                                ELTCONSL
01159       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01160         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
01161              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTCONSL
01162                                                                   ELTCONSL
01163      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01164        SET PLT-INDEX2        TO 2                                 ELTCONSL
01165       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01166         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
01167              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTCONSL
01168                                                                   ELTCONSL
01169      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTCONSL
01170          ADD  +1               TO  WS-CIA                         ELTCONSL
01171          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTCONSL
01172          ADD  +1               TO  WS-CIA.                        ELTCONSL
01173                                                                   ELTCONSL
01174      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01175        SET  PLT-INDEX2       TO  1                                ELTCONSL
01176       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01177         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
01178          PERFORM 4000-PLACE-OF-TREATMENT                          ELTCONSL
01179          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01180          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01181          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01182          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01183          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTCONSL
01184          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTCONSL
01185          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
01186             ADD +1                TO WS-CIA                       ELTCONSL
01187             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
01188             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
01189          ELSE                                                     ELTCONSL
01190           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
01191                                                                   ELTCONSL
01192      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01193        SET PLT-INDEX2        TO 2                                 ELTCONSL
01194       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01195         NOT = ZEROS AND NOT = LOW-VALUES                          ELTCONSL
01196          PERFORM 4000-PLACE-OF-TREATMENT                          ELTCONSL
01197          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01198          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01199          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01200          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01201          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTCONSL
01202          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTCONSL
01203          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
01204             ADD +1                TO WS-CIA                       ELTCONSL
01205             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
01206             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
01207          ELSE                                                     ELTCONSL
01208           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
01209                                                                   ELTCONSL
01210 ************************************************************      ELTCONSL
01211 ************ PAYMENT IS BASED ON                                  ELTCONSL
01212 ************************************************************      ELTCONSL
01213      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01214        SET  PLT-INDEX2       TO  1                                ELTCONSL
01215        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
01216         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
01217            AND NOT = '00  '                                       ELTCONSL
01218              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTCONSL
01219                                                                   ELTCONSL
01220      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01221        SET PLT-INDEX2        TO 2                                 ELTCONSL
01222        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
01223         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
01224            AND NOT = '00  '                                       ELTCONSL
01225              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTCONSL
01226                                                                   ELTCONSL
01227      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTCONSL
01228          ADD  +1               TO  WS-CIA                         ELTCONSL
01229          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTCONSL
01230          ADD  +1               TO  WS-CIA.                        ELTCONSL
01231                                                                   ELTCONSL
01232      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01233        SET  PLT-INDEX2       TO  1                                ELTCONSL
01234        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
01235         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
01236            AND NOT = '00  '                                       ELTCONSL
01237          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTCONSL
01238          MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO        ELTCONSL
01239                                CMF-CODE-VALUE                     ELTCONSL
01240          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTCONSL
01241          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
01242          STRING CMF-DESCR-LINE(1) ' '                             ELTCONSL
01243                 CMF-DESCR-LINE(2) ' '                             ELTCONSL
01244                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
01245          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
01246          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01247          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01248          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01249          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01250          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTCONSL
01251          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTCONSL
01252          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
01253             ADD +1                TO WS-CIA                       ELTCONSL
01254             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
01255             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
01256          ELSE                                                     ELTCONSL
01257           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
01258                                                                   ELTCONSL
01259                                                                   ELTCONSL
01260      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01261        SET PLT-INDEX2        TO 2                                 ELTCONSL
01262        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                    ELTCONSL
01263         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTCONSL
01264            AND NOT = '00  '                                       ELTCONSL
01265          MOVE 'BPE' TO CMF-RECORD-PREFIX                          ELTCONSL
01266          MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO        ELTCONSL
01267                                CMF-CODE-VALUE                     ELTCONSL
01268          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTCONSL
01269          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
01270          STRING CMF-DESCR-LINE(1) ' '                             ELTCONSL
01271                 CMF-DESCR-LINE(2) ' '                             ELTCONSL
01272                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
01273          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
01274          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01275          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01276          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01277          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01278          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTCONSL
01279          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTCONSL
01280          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTCONSL
01281             ADD +1                TO WS-CIA                       ELTCONSL
01282             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTCONSL
01283             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
01284          ELSE                                                     ELTCONSL
01285           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
01286 **************************************************************    ELTCONSL
01287 **** SERVICES ARE PAYABLE                                         ELTCONSL
01288 **************************************************************    ELTCONSL
01289      MOVE LOW-VALUES           TO  COF-DTL-LINE(WS-CIA).          ELTCONSL
01290      ADD  +1                   TO  WS-CIA.                        ELTCONSL
01291      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTCONSL
01292      ADD  +1                   TO  WS-CIA.                        ELTCONSL
01293                                                                   ELTCONSL
01294      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01295         SET  PLT-INDEX2           TO  1                           ELTCONSL
01296         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTCONSL
01297                                                                   ELTCONSL
01298      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01299         SET  PLT-INDEX2             TO  2                         ELTCONSL
01300         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTCONSL
01301 *************************************************************     ELTCONSL
01302 **** MAX NUMBER VISITS                                            ELTCONSL
01303 *************************************************************     ELTCONSL
01304      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01305       SET PLT-INDEX2 TO 1                                         ELTCONSL
01306       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
01307        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01308                                         NOT = LOW-VALUES          ELTCONSL
01309          MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.                ELTCONSL
01310                                                                   ELTCONSL
01311      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01312       SET  PLT-INDEX2 TO  2                                       ELTCONSL
01313       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
01314        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01315                                         NOT = LOW-VALUES          ELTCONSL
01316          MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.                ELTCONSL
01317                                                                   ELTCONSL
01318      IF WS-DISPLAY-MAX-VISIT-TEXT = WS-YES                        ELTCONSL
01319          ADD +1             TO WS-CIA                             ELTCONSL
01320          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTCONSL
01321          ADD +1             TO WS-CIA.                            ELTCONSL
01322                                                                   ELTCONSL
01323      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01324       SET  PLT-INDEX2           TO  1                             ELTCONSL
01325       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
01326        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01327                                         NOT = LOW-VALUES          ELTCONSL
01328         PERFORM 4500-MAX-VISITS                                   ELTCONSL
01329         IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01330                 = ZEROS                                           ELTCONSL
01331           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTCONSL
01332           MOVE WS-UNLIMITED           TO  WS-DTL-BASIC            ELTCONSL
01333           MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                 ELTCONSL
01334           PERFORM 3000-OUTPUT-TEXT                                ELTCONSL
01335         ELSE                                                      ELTCONSL
01336           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)    ELTCONSL
01337                TO WS-DTL-MAX-DAYS                                 ELTCONSL
01338           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTCONSL
01339           MOVE WS-DAYS               TO WS-DAYS-LITERAL           ELTCONSL
01340           MOVE SPACES   TO TCAR-FROM-AREA                         ELTCONSL
01341           STRING WS-DTL-MAX-DAYS ' '                              ELTCONSL
01342                  WS-DAYS ' '                                      ELTCONSL
01343                  WS-DTL-MAX-IND                                   ELTCONSL
01344                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTCONSL
01345           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTCONSL
01346           MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                ELTCONSL
01347           MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
01348           MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
01349           PERFORM TCPR-000-TEXT-UNSTRING                          ELTCONSL
01350           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-BASIC              ELTCONSL
01351           MOVE WS-BASIC              TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01352           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTCONSL
01353               ADD +1                 TO WS-CIA                    ELTCONSL
01354               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01355               PERFORM 3000-OUTPUT-TEXT                            ELTCONSL
01356           ELSE                                                    ELTCONSL
01357             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
01358                                                                   ELTCONSL
01359      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01360       SET  PLT-INDEX2           TO  2                             ELTCONSL
01361       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTCONSL
01362        IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01363                                         NOT = LOW-VALUES          ELTCONSL
01364          PERFORM 4500-MAX-VISITS                                  ELTCONSL
01365          IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTCONSL
01366                  = ZEROS                                          ELTCONSL
01367           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTCONSL
01368           MOVE WS-UNLIMITED           TO  WS-DTL-SUPPLEMENTAL     ELTCONSL
01369           MOVE WS-SUPPLEMENTAL        TO  COF-DTL-LINE(WS-CIA)    ELTCONSL
01370           PERFORM 3000-OUTPUT-TEXT                                ELTCONSL
01371         ELSE                                                      ELTCONSL
01372           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)    ELTCONSL
01373                TO WS-DTL-MAX-DAYS                                 ELTCONSL
01374           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTCONSL
01375           MOVE WS-DAYS               TO WS-DAYS-LITERAL           ELTCONSL
01376           MOVE SPACES   TO TCAR-FROM-AREA                         ELTCONSL
01377           STRING WS-DTL-MAX-DAYS ' '                              ELTCONSL
01378                  WS-DAYS ' '                                      ELTCONSL
01379                  WS-DTL-MAX-IND                                   ELTCONSL
01380                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTCONSL
01381           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTCONSL
01382           MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                ELTCONSL
01383           MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
01384           MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                ELTCONSL
01385           PERFORM TCPR-000-TEXT-UNSTRING                          ELTCONSL
01386           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-SUPPLEMENTAL       ELTCONSL
01387           MOVE WS-SUPPLEMENTAL       TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01388           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTCONSL
01389               ADD +1                 TO WS-CIA                    ELTCONSL
01390               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01391               PERFORM 3000-OUTPUT-TEXT                            ELTCONSL
01392           ELSE                                                    ELTCONSL
01393             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
01394 ******************************************************************ELTCONSL
01395 **** MAXIMUM AMOUNT PER VISIT                                     ELTCONSL
01396 ***************************************************************** ELTCONSL
01397                                                                   ELTCONSL
01398      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01399       SET PLT-INDEX2 TO 1                                         ELTCONSL
01400       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
01401        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01402                                         NOT = ZEROS               ELTCONSL
01403          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTCONSL
01404                                                                   ELTCONSL
01405      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01406       SET  PLT-INDEX2 TO  2                                       ELTCONSL
01407       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
01408        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01409                                         NOT = ZEROS               ELTCONSL
01410          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTCONSL
01411                                                                   ELTCONSL
01412      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTCONSL
01413          ADD +1             TO WS-CIA                             ELTCONSL
01414          MOVE WS-MAX-AMT-TEXT TO COF-DTL-LINE(WS-CIA).            ELTCONSL
01415                                                                   ELTCONSL
01416      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTCONSL
01417       SET  PLT-INDEX2           TO  1                             ELTCONSL
01418       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
01419        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01420                                         NOT = ZEROS               ELTCONSL
01421          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01422           TO WS-BASIC-MAX-AMT                                     ELTCONSL
01423          ADD +1             TO WS-CIA                             ELTCONSL
01424          MOVE WS-BASIC-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTCONSL
01425                                                                   ELTCONSL
01426      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01427       SET  PLT-INDEX2           TO  2                             ELTCONSL
01428       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTCONSL
01429        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTCONSL
01430                                         NOT = ZEROS               ELTCONSL
01431          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01432           TO WS-SUPPL-MAX-AMT                                     ELTCONSL
01433          ADD +1             TO WS-CIA                             ELTCONSL
01434          MOVE WS-SUPPL-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTCONSL
01435                                                                   ELTCONSL
01436      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTCONSL
01437             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
01438 **************************************************************    ELTCONSL
01439 *** BENEFITS FOR ASOP ARE AVAILABLE                               ELTCONSL
01440 **************************************************************    ELTCONSL
01441      PERFORM 7600-ASOP-PROCESS.                                   ELTCONSL
01442 **************************************************************    ELTCONSL
01443 ***  SPILL OVER COINS AND DEDUCT                                  ELTCONSL
01444 **************************************************************    ELTCONSL
01445      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTCONSL
01446         SET  PLT-INDEX2          TO  2                            ELTCONSL
01447         PERFORM 4300-SPILLOVER-COINS                              ELTCONSL
01448         PERFORM 4400-SPILLOVER-DEDUCT.                            ELTCONSL
01449         PERFORM 4550-TRANSF-OTHER-RESP-IND.                       ELTCONSL
01450                                                                   ELTCONSL
01451 **************************************************************    ELTCONSL
01452 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTCONSL
01453 ***************************************************************   ELTCONSL
01454      PERFORM 4650-SCAN-TAB.                                       ELTCONSL
01455      PERFORM 4675-PAY-CONSID-TEXT.                                ELTCONSL
01456 *************************************************************     ELTCONSL
01457  2150-ZERO-ALL-WITH-SAME-NO.                                      ELTCONSL
01458      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTCONSL
01459                                                                   ELTCONSL
01460      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTCONSL
01461         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTCONSL
01462         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTCONSL
01463         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTCONSL
01464                                                   CMF-CODE-VALUE  ELTCONSL
01465         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
01466         STRING CMF-DESCR-LINE (1) ' '                             ELTCONSL
01467                CMF-DESCR-LINE (2)                                 ELTCONSL
01468                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
01469         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
01470         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
01471         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
01472         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
01473         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
01474         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTCONSL
01475         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
01476         IF WS-CIA  <  20                                          ELTCONSL
01477            ADD +1  TO  WS-CIA                                     ELTCONSL
01478            MOVE ZERO  TO                                          ELTCONSL
01479                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTCONSL
01480         ELSE                                                      ELTCONSL
01481            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTCONSL
01482                COMMAREA(DFHCOMMAREA)                              ELTCONSL
01483            END-EXEC                                               ELTCONSL
01484            MOVE +1  TO  WS-CIA                                    ELTCONSL
01485            MOVE ZERO  TO                                          ELTCONSL
01486                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTCONSL
01487 ******************************************************************ELTCONSL
01488  2190-PROBLEM-WITH-INDICES.                                       ELTCONSL
01489                                                                   ELTCONSL
01490      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTCONSL
01491      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTCONSL
01492                                                                   ELTCONSL
01493      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
01494      MOVE 'P'  TO  COF-FUNCTION.                                  ELTCONSL
01495                                                                   ELTCONSL
01496      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTCONSL
01497      END-EXEC.                                                    ELTCONSL
01498                                                                   ELTCONSL
01499  2199-EXIT.            EXIT.                                      ELTCONSL
01500                                                                   ELTCONSL
01501  2100-EXIT.  EXIT.                                                ELTCONSL
01502 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTCONSL
01503  3000-OUTPUT-TEXT SECTION.                                        ELTCONSL
01504      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTCONSL
01505      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTCONSL
01506      MOVE ' '  TO  COF-FUNCTION.                                  ELTCONSL
01507                                                                   ELTCONSL
01508      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTCONSL
01509                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
01510      END-EXEC.                                                    ELTCONSL
01511      MOVE +1   TO WS-CIA.                                         ELTCONSL
01512  3000-EXIT.  EXIT.                                                ELTCONSL
01513 /                                                                 ELTCONSL
01514  4000-PLACE-OF-TREATMENT SECTION.                                 ELTCONSL
01515      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTCONSL
01516      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTCONSL
01517      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01518                                               TO  CMF-CODE-VALUE. ELTCONSL
01519      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01520      STRING                                                       ELTCONSL
01521             CMF-DESCR-LINE (1) ' '                                ELTCONSL
01522             CMF-DESCR-LINE (2)                                    ELTCONSL
01523              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCONSL
01524      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01525  4000-EXIT.  EXIT.                                                ELTCONSL
01526 /                                                                 ELTCONSL
01527  4100-PAYABLE-AS-BASIC SECTION.                                   ELTCONSL
01528      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTCONSL
01529          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
01530          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTCONSL
01531          GO TO 4100-OUTPUT-TEXT.                                  ELTCONSL
01532      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTCONSL
01533      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTCONSL
01534      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTCONSL
01535                                                CMF-CODE-VALUE     ELTCONSL
01536      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01537      PERFORM VARYING CMF-DESCR-IDX FROM                           ELTCONSL
01538           1 BY 1 UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES        ELTCONSL
01539                ADD +1 TO TCAR-FROM-SUB                            ELTCONSL
01540                MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTCONSL
01541                    TO TCAR-FROM-LINE (TCAR-FROM-SUB)              ELTCONSL
01542      END-PERFORM.                                                 ELTCONSL
01543      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01544      PERFORM 3000-PREPARE-UNSTRING.                               ELTCONSL
01545      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01546      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01547                                         =  ZEROS                  ELTCONSL
01548       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTCONSL
01549                                         =  ZEROS                  ELTCONSL
01550                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTCONSL
01551                 MOVE WS-BASIC               TO                    ELTCONSL
01552                         COF-DTL-LINE(WS-CIA)                      ELTCONSL
01553       ELSE                                                        ELTCONSL
01554          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTCONSL
01555          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONSL
01556                                        TO  WS-DTL-PERCENT         ELTCONSL
01557          MOVE SPACES          TO TCAR-FROM-AREA                   ELTCONSL
01558          STRING WS-DTL-PP,                                        ELTCONSL
01559                 WS-DTL-PERCENT,                                   ELTCONSL
01560                 WS-PERCENT,                                       ELTCONSL
01561                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
01562          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
01563          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01564          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01565          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01566          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01567          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTCONSL
01568          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTCONSL
01569      ELSE                                                         ELTCONSL
01570        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTCONSL
01571        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTCONSL
01572                                     TO WS-DTL-PERCENT             ELTCONSL
01573        MOVE SPACES          TO TCAR-FROM-AREA                     ELTCONSL
01574        STRING WS-DTL-PP,                                          ELTCONSL
01575               WS-DTL-PERCENT,                                     ELTCONSL
01576               WS-PERCENT,                                         ELTCONSL
01577                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
01578        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTCONSL
01579        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTCONSL
01580        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTCONSL
01581        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTCONSL
01582        PERFORM TCPR-000-TEXT-UNSTRING                             ELTCONSL
01583        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTCONSL
01584        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTCONSL
01585  4100-OUTPUT-TEXT.                                                ELTCONSL
01586      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCONSL
01587            PERFORM VARYING TCAR-FROM-SUB                          ELTCONSL
01588              FROM 2 BY 1 UNTIL TCAR-FROM-SUB                      ELTCONSL
01589              > TCAR-OUTPUT-FIELDS-USED                            ELTCONSL
01590               ADD +1 TO WS-CIA                                    ELTCONSL
01591               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB)                   ELTCONSL
01592                  TO COF-DTL-LINE(WS-CIA)                          ELTCONSL
01593            END-PERFORM                                            ELTCONSL
01594            PERFORM 3000-OUTPUT-TEXT                               ELTCONSL
01595      ELSE                                                         ELTCONSL
01596         PERFORM 3000-OUTPUT-TEXT.                                 ELTCONSL
01597  4100-EXIT.  EXIT.                                                ELTCONSL
01598 /                                                                 ELTCONSL
01599  4200-PAYABLE-AS-SUPP SECTION.                                    ELTCONSL
01600      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTCONSL
01601          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
01602          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTCONSL
01603          GO TO 4200-OUTPUT-TEXT.                                  ELTCONSL
01604      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTCONSL
01605      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTCONSL
01606      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTCONSL
01607                                                CMF-CODE-VALUE     ELTCONSL
01608      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01609      PERFORM VARYING CMF-DESCR-IDX FROM                           ELTCONSL
01610           1 BY 1 UNTIL CMF-DESCR-IDX > CMF-NBR-DESCR-LINES        ELTCONSL
01611                ADD +1 TO TCAR-FROM-SUB                            ELTCONSL
01612                MOVE CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTCONSL
01613                    TO TCAR-FROM-LINE (TCAR-FROM-SUB)              ELTCONSL
01614      END-PERFORM.                                                 ELTCONSL
01615      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01616      PERFORM 3000-PREPARE-UNSTRING.                               ELTCONSL
01617      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01618      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01619                                         =  ZEROS                  ELTCONSL
01620       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTCONSL
01621                                         =  ZEROS                  ELTCONSL
01622                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTCONSL
01623                MOVE WS-SUPPLEMENTAL        TO                     ELTCONSL
01624                         COF-DTL-LINE(WS-CIA)                      ELTCONSL
01625       ELSE                                                        ELTCONSL
01626          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTCONSL
01627          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTCONSL
01628                                        TO  WS-DTL-PERCENT         ELTCONSL
01629          MOVE SPACES          TO TCAR-FROM-AREA                   ELTCONSL
01630          STRING WS-DTL-PP,                                        ELTCONSL
01631                 WS-DTL-PERCENT,                                   ELTCONSL
01632                 WS-PERCENT,                                       ELTCONSL
01633                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTCONSL
01634          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTCONSL
01635          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTCONSL
01636          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTCONSL
01637          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTCONSL
01638          PERFORM TCPR-000-TEXT-UNSTRING                           ELTCONSL
01639          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTCONSL
01640          MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)    ELTCONSL
01641      ELSE                                                         ELTCONSL
01642        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTCONSL
01643        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTCONSL
01644                                    TO WS-DTL-PERCENT              ELTCONSL
01645        MOVE SPACES          TO TCAR-FROM-AREA                     ELTCONSL
01646        STRING WS-DTL-PP,                                          ELTCONSL
01647               WS-DTL-PERCENT,                                     ELTCONSL
01648               WS-PERCENT,                                         ELTCONSL
01649                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
01650        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTCONSL
01651        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTCONSL
01652        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTCONSL
01653        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTCONSL
01654        PERFORM TCPR-000-TEXT-UNSTRING                             ELTCONSL
01655        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTCONSL
01656        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTCONSL
01657  4200-OUTPUT-TEXT.                                                ELTCONSL
01658      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCONSL
01659            PERFORM VARYING TCAR-FROM-SUB                          ELTCONSL
01660              FROM 2 BY 1 UNTIL TCAR-FROM-SUB                      ELTCONSL
01661              > TCAR-OUTPUT-FIELDS-USED                            ELTCONSL
01662               ADD +1 TO WS-CIA                                    ELTCONSL
01663               MOVE TCAR-OPF-DATA(TCAR-FROM-SUB)                   ELTCONSL
01664                  TO COF-DTL-LINE(WS-CIA)                          ELTCONSL
01665            END-PERFORM                                            ELTCONSL
01666            PERFORM 3000-OUTPUT-TEXT                               ELTCONSL
01667      ELSE                                                         ELTCONSL
01668         PERFORM 3000-OUTPUT-TEXT.                                 ELTCONSL
01669  4200-EXIT.  EXIT.                                                ELTCONSL
01670 /                                                                 ELTCONSL
01671  4300-SPILLOVER-COINS SECTION.                                    ELTCONSL
01672      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01673            = '0'   OR LOW-VALUES                                  ELTCONSL
01674           GO TO 4300-EXIT.                                        ELTCONSL
01675      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTCONSL
01676      ADD +1     TO  WS-CIA.                                       ELTCONSL
01677      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTCONSL
01678      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTCONSL
01679      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTCONSL
01680                       TO CMF-CODE-VALUE.                          ELTCONSL
01681      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01682      STRING WS-SPILLOVER-COINS                                    ELTCONSL
01683             CMF-DESCR-LINE (1) ' '                                ELTCONSL
01684             CMF-DESCR-LINE (2)                                    ELTCONSL
01685              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCONSL
01686      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01687      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONSL
01688      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTCONSL
01689      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTCONSL
01690      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01691      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONSL
01692      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCONSL
01693            ADD +1                TO  WS-CIA                       ELTCONSL
01694            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTCONSL
01695      PERFORM 3000-OUTPUT-TEXT.                                    ELTCONSL
01696  4300-EXIT.  EXIT.                                                ELTCONSL
01697 /                                                                 ELTCONSL
01698  4400-SPILLOVER-DEDUCT SECTION.                                   ELTCONSL
01699      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01700            = '0'   OR LOW-VALUES                                  ELTCONSL
01701           GO TO 4400-EXIT.                                        ELTCONSL
01702      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTCONSL
01703      ADD +1     TO  WS-CIA.                                       ELTCONSL
01704      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTCONSL
01705      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTCONSL
01706      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01707                       TO CMF-CODE-VALUE                           ELTCONSL
01708      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01709      STRING WS-SPILLOVER-DEDBL                                    ELTCONSL
01710             CMF-DESCR-LINE (1) ' '                                ELTCONSL
01711             CMF-DESCR-LINE (2)                                    ELTCONSL
01712              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCONSL
01713      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01714      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONSL
01715      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTCONSL
01716      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTCONSL
01717      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01718      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONSL
01719      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCONSL
01720            ADD +1                TO  WS-CIA                       ELTCONSL
01721            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTCONSL
01722      PERFORM 3000-OUTPUT-TEXT.                                    ELTCONSL
01723  4400-EXIT.  EXIT.                                                ELTCONSL
01724 /                                                                 ELTCONSL
01725  4500-MAX-VISITS SECTION.                                         ELTCONSL
01726      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTCONSL
01727      MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  TO      ELTCONSL
01728                                                 CMF-CODE-VALUE    ELTCONSL
01729      MOVE 'BPE'                TO  CMF-RECORD-PREFIX.             ELTCONSL
01730      MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME.       ELTCONSL
01731      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01732      STRING CMF-DESCR-LINE (1) ' '                                ELTCONSL
01733             CMF-DESCR-LINE (2)                                    ELTCONSL
01734              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCONSL
01735      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01736      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONSL
01737      MOVE +63              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTCONSL
01738      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTCONSL
01739      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01740  4500-EXIT.  EXIT.                                                ELTCONSL
01741 /                                                                 ELTCONSL
01742  4550-TRANSF-OTHER-RESP-IND SECTION.                              ELTCONSL
01743                                                                   ELTCONSL
01744      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTCONSL
01745        SET  PLT-INDEX2       TO  1                                ELTCONSL
01746      ELSE                                                         ELTCONSL
01747        SET  PLT-INDEX2       TO  2.                               ELTCONSL
01748                                                                   ELTCONSL
01749      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01750            = '0'   OR LOW-VALUES                                  ELTCONSL
01751           GO TO 4550-EXIT.                                        ELTCONSL
01752                                                                   ELTCONSL
01753      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTCONSL
01754      ADD +1     TO  WS-CIA.                                       ELTCONSL
01755      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTCONSL
01756      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTCONSL
01757      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
01758                       TO CMF-CODE-VALUE.                          ELTCONSL
01759                                                                   ELTCONSL
01760      PERFORM 8000-CODES-MANUAL-CALL.                              ELTCONSL
01761                                                                   ELTCONSL
01762      STRING WS-SPILLOVER-COINS                                    ELTCONSL
01763             CMF-DESCR-LINE (1) ' '                                ELTCONSL
01764             CMF-DESCR-LINE (2)                                    ELTCONSL
01765              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTCONSL
01766                                                                   ELTCONSL
01767      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01768      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONSL
01769      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTCONSL
01770      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTCONSL
01771                                                                   ELTCONSL
01772      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01773      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONSL
01774      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTCONSL
01775            ADD +1                TO  WS-CIA                       ELTCONSL
01776            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTCONSL
01777                                                                   ELTCONSL
01778      PERFORM 3000-OUTPUT-TEXT.                                    ELTCONSL
01779  4550-EXIT.  EXIT.                                                ELTCONSL
01780 /                                                                 ELTCONSL
01781  4600-CHECK-SAME-DIFF-PROVID SECTION.                             ELTCONSL
01782      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTCONSL
01783      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01784          ADDRESS OF CONTRACT-RECORD.                              ELTCONSL
01785      IF CIA-RC-PTR-NULL                                           ELTCONSL
01786         SET CIA-ELSCONPS-DDN TO TRUE                              ELTCONSL
01787         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTCONSL
01788            ADDRESS OF CONTRACT-RECORD                             ELTCONSL
01789         IF CIA-RC-PTR-NULL                                        ELTCONSL
01790            GO TO 4600-EXIT.                                       ELTCONSL
01791                                                                   ELTCONSL
01792      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTCONSL
01793      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01794          ADDRESS OF CONTRACT-RECORD.                              ELTCONSL
01795      IF NOT CIA-RC-PTR-NULL                                       ELTCONSL
01796         PERFORM 6000-MED-SURG-OB-BASIC-SAME.                      ELTCONSL
01797                                                                   ELTCONSL
01798      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTCONSL
01799      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01800          ADDRESS OF CONTRACT-RECORD.                              ELTCONSL
01801      IF NOT CIA-RC-PTR-NULL                                       ELTCONSL
01802         PERFORM 6400-MED-SURG-OB-SUPP-SAME.                       ELTCONSL
01803                                                                   ELTCONSL
01804      PERFORM 6800-OUTPUT-SAME-PROVD-TEXT.                         ELTCONSL
01805                                                                   ELTCONSL
01806      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTCONSL
01807      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01808          ADDRESS OF CONTRACT-RECORD.                              ELTCONSL
01809      IF NOT CIA-RC-PTR-NULL                                       ELTCONSL
01810         PERFORM 7000-MED-MED-BASIC-DIFF.                          ELTCONSL
01811                                                                   ELTCONSL
01812      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTCONSL
01813      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
01814          ADDRESS OF CONTRACT-RECORD.                              ELTCONSL
01815      IF NOT CIA-RC-PTR-NULL                                       ELTCONSL
01816          PERFORM 7200-MED-MED-SUPP-DIFF.                          ELTCONSL
01817                                                                   ELTCONSL
01818      PERFORM 7500-OUTPUT-DIFF-PROVD-TEXT.                         ELTCONSL
01819                                                                   ELTCONSL
01820  4600-EXIT.  EXIT.                                                ELTCONSL
01821 /                                                                 ELTCONSL
01822  4650-SCAN-TAB SECTION.                                           ELTCONSL
01823      PERFORM 4700-BEN-TAB-AAR.                                    ELTCONSL
01824      PERFORM 4800-BEN-TAB-PPF.                                    ELTCONSL
01825      PERFORM 4900-BEN-TAB-PVE.                                    ELTCONSL
01826      PERFORM 5000-BEN-TAB-ADL.                                    ELTCONSL
01827      PERFORM 5100-BEN-TAB-ABM.                                    ELTCONSL
01828      PERFORM 5200-BEN-TAB-ACL.                                    ELTCONSL
01829      PERFORM 5300-BEN-TAB-AOL.                                    ELTCONSL
01830  4650-EXIT.  EXIT.                                                ELTCONSL
01831 /                                                                 ELTCONSL
01832  4675-PAY-CONSID-TEXT SECTION.                                    ELTCONSL
01833      INITIALIZE TCAR-FROM-AREA.                                   ELTCONSL
01834      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTCONSL
01835             WS-PAY-CONSDR-TEXT2                                   ELTCONSL
01836                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTCONSL
01837      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTCONSL
01838      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTCONSL
01839      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTCONSL
01840                                TCAR-OUTPUT-FIELD-2-LEN.           ELTCONSL
01841      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTCONSL
01842      IF WS-CIA > 17                                               ELTCONSL
01843            PERFORM 3000-OUTPUT-TEXT                               ELTCONSL
01844            MOVE +1            TO WS-CIA.                          ELTCONSL
01845      ADD +1                TO  WS-CIA.                            ELTCONSL
01846      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTCONSL
01847      ADD +1                TO  WS-CIA.                            ELTCONSL
01848      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTCONSL
01849      PERFORM 3000-OUTPUT-TEXT.                                    ELTCONSL
01850  4675-EXIT.   EXIT.                                               ELTCONSL
01851 /                                                                 ELTCONSL
01852  4700-BEN-TAB-AAR SECTION.                                        ELTCONSL
01853      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTCONSL
01854      SET PLT-INDEX2 TO 1.                                         ELTCONSL
01855                                                                   ELTCONSL
01856      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01857          NOT = LOW-VALUES                                         ELTCONSL
01858       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01859          NOT = SPACE                                              ELTCONSL
01860                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTCONSL
01861                                                                   ELTCONSL
01862      SET PLT-INDEX2 TO 2.                                         ELTCONSL
01863                                                                   ELTCONSL
01864      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01865          NOT = LOW-VALUES                                         ELTCONSL
01866       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01867          NOT = SPACE                                              ELTCONSL
01868                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTCONSL
01869                                                                   ELTCONSL
01870      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTCONSL
01871             MOVE +1                  TO WS-CIA                    ELTCONSL
01872             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01873             ADD  +1                  TO WS-CIA                    ELTCONSL
01874             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTCONSL
01875             PERFORM 3000-OUTPUT-TEXT.                             ELTCONSL
01876  4700-EXIT.  EXIT.                                                ELTCONSL
01877 /                                                                 ELTCONSL
01878  4800-BEN-TAB-PPF SECTION.                                        ELTCONSL
01879      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCONSL
01880                       WS-HOLD2.                                   ELTCONSL
01881      SET PLT-INDEX2 TO 1.                                         ELTCONSL
01882      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01883          NOT = LOW-VALUES                                         ELTCONSL
01884       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01885          NOT = SPACE                                              ELTCONSL
01886             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTCONSL
01887                        TO  WS-HOLD1.                              ELTCONSL
01888                                                                   ELTCONSL
01889      SET PLT-INDEX2 TO 2.                                         ELTCONSL
01890      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01891          NOT = LOW-VALUES                                         ELTCONSL
01892       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01893          NOT = SPACE                                              ELTCONSL
01894             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTCONSL
01895                        TO  WS-HOLD2.                              ELTCONSL
01896                                                                   ELTCONSL
01897      IF WS-HOLD1 = WS-HOLD2                                       ELTCONSL
01898         IF WS-HOLD1 = ZEROS                                       ELTCONSL
01899                 GO TO 4800-EXIT                                   ELTCONSL
01900         ELSE                                                      ELTCONSL
01901             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCONSL
01902             PERFORM 5900-GET-TAB-REC                              ELTCONSL
01903             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTCONSL
01904                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
01905             END-EXEC                                              ELTCONSL
01906             GO TO 4800-EXIT.                                      ELTCONSL
01907                                                                   ELTCONSL
01908      IF WS-HOLD1 = ZEROS                                          ELTCONSL
01909             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCONSL
01910             PERFORM 5900-GET-TAB-REC                              ELTCONSL
01911             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTCONSL
01912                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
01913             END-EXEC                                              ELTCONSL
01914      ELSE                                                         ELTCONSL
01915       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCONSL
01916       PERFORM 5900-GET-TAB-REC                                    ELTCONSL
01917       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTCONSL
01918                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
01919       END-EXEC                                                    ELTCONSL
01920       IF WS-HOLD2 = ZEROS                                         ELTCONSL
01921           GO TO 4800-EXIT                                         ELTCONSL
01922       ELSE                                                        ELTCONSL
01923          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCONSL
01924          PERFORM 5900-GET-TAB-REC                                 ELTCONSL
01925          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTCONSL
01926                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
01927          END-EXEC.                                                ELTCONSL
01928  4800-EXIT.    EXIT.                                              ELTCONSL
01929 /                                                                 ELTCONSL
01930  4900-BEN-TAB-PVE SECTION.                                        ELTCONSL
01931      MOVE +1 TO WS-CIA.                                           ELTCONSL
01932      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTCONSL
01933      ADD  +1         TO WS-CIA.                                   ELTCONSL
01934      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTCONSL
01935      PERFORM 3000-OUTPUT-TEXT.                                    ELTCONSL
01936  4900-EXIT.  EXIT.                                                ELTCONSL
01937 /                                                                 ELTCONSL
01938  5000-BEN-TAB-ADL SECTION.                                        ELTCONSL
01939      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCONSL
01940                       WS-HOLD2.                                   ELTCONSL
01941      SET PLT-INDEX2 TO 1.                                         ELTCONSL
01942      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01943          NOT = LOW-VALUES                                         ELTCONSL
01944       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01945          NOT = SPACE                                              ELTCONSL
01946             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
01947                        TO  WS-HOLD1.                              ELTCONSL
01948                                                                   ELTCONSL
01949      SET PLT-INDEX2 TO 2.                                         ELTCONSL
01950      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01951          NOT = LOW-VALUES                                         ELTCONSL
01952       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01953          NOT = SPACE                                              ELTCONSL
01954             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
01955                        TO  WS-HOLD2.                              ELTCONSL
01956                                                                   ELTCONSL
01957      IF WS-HOLD1 = WS-HOLD2                                       ELTCONSL
01958         IF WS-HOLD1 = ZEROS                                       ELTCONSL
01959                 GO TO 5000-EXIT                                   ELTCONSL
01960         ELSE                                                      ELTCONSL
01961             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCONSL
01962             PERFORM 5900-GET-TAB-REC                              ELTCONSL
01963             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTCONSL
01964                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
01965             END-EXEC                                              ELTCONSL
01966             GO TO 5000-EXIT.                                      ELTCONSL
01967                                                                   ELTCONSL
01968      IF WS-HOLD1 = ZEROS                                          ELTCONSL
01969             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCONSL
01970             PERFORM 5900-GET-TAB-REC                              ELTCONSL
01971             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTCONSL
01972                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
01973             END-EXEC                                              ELTCONSL
01974      ELSE                                                         ELTCONSL
01975       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCONSL
01976       PERFORM 5900-GET-TAB-REC                                    ELTCONSL
01977       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTCONSL
01978                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
01979       END-EXEC                                                    ELTCONSL
01980       IF WS-HOLD2 = ZEROS                                         ELTCONSL
01981           GO TO 5000-EXIT                                         ELTCONSL
01982       ELSE                                                        ELTCONSL
01983          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCONSL
01984          PERFORM 5900-GET-TAB-REC                                 ELTCONSL
01985          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTCONSL
01986                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
01987          END-EXEC.                                                ELTCONSL
01988  5000-EXIT.     EXIT.                                             ELTCONSL
01989 /                                                                 ELTCONSL
01990  5100-BEN-TAB-ABM SECTION.                                        ELTCONSL
01991      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCONSL
01992                       WS-HOLD2.                                   ELTCONSL
01993      SET PLT-INDEX2 TO 1.                                         ELTCONSL
01994      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
01995          NOT = LOW-VALUES                                         ELTCONSL
01996       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
01997          NOT = SPACE                                              ELTCONSL
01998             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTCONSL
01999                        TO  WS-HOLD1.                              ELTCONSL
02000                                                                   ELTCONSL
02001      SET PLT-INDEX2 TO 2.                                         ELTCONSL
02002      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
02003          NOT = LOW-VALUES                                         ELTCONSL
02004       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
02005          NOT = SPACE                                              ELTCONSL
02006             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTCONSL
02007                        TO  WS-HOLD2.                              ELTCONSL
02008                                                                   ELTCONSL
02009      IF WS-HOLD1 = WS-HOLD2                                       ELTCONSL
02010         IF WS-HOLD1 = ZEROS                                       ELTCONSL
02011                 GO TO 5100-EXIT                                   ELTCONSL
02012         ELSE                                                      ELTCONSL
02013             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCONSL
02014             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02015             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTCONSL
02016                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02017             END-EXEC                                              ELTCONSL
02018             GO TO 5100-EXIT.                                      ELTCONSL
02019                                                                   ELTCONSL
02020      IF WS-HOLD1 = ZEROS                                          ELTCONSL
02021             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCONSL
02022             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02023             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTCONSL
02024                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02025             END-EXEC                                              ELTCONSL
02026      ELSE                                                         ELTCONSL
02027       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCONSL
02028       PERFORM 5900-GET-TAB-REC                                    ELTCONSL
02029       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTCONSL
02030                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
02031       END-EXEC                                                    ELTCONSL
02032       IF WS-HOLD2 = ZEROS                                         ELTCONSL
02033            GO TO 5100-EXIT                                        ELTCONSL
02034       ELSE                                                        ELTCONSL
02035          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCONSL
02036          PERFORM 5900-GET-TAB-REC                                 ELTCONSL
02037          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTCONSL
02038                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
02039          END-EXEC.                                                ELTCONSL
02040  5100-EXIT.     EXIT.                                             ELTCONSL
02041 /                                                                 ELTCONSL
02042  5200-BEN-TAB-ACL SECTION.                                        ELTCONSL
02043      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCONSL
02044                       WS-HOLD2.                                   ELTCONSL
02045      SET PLT-INDEX2 TO 1.                                         ELTCONSL
02046      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
02047          NOT = LOW-VALUES                                         ELTCONSL
02048       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
02049          NOT = SPACE                                              ELTCONSL
02050             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
02051                        TO  WS-HOLD1.                              ELTCONSL
02052                                                                   ELTCONSL
02053      SET PLT-INDEX2 TO 2.                                         ELTCONSL
02054      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
02055          NOT = LOW-VALUES                                         ELTCONSL
02056       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
02057          NOT = SPACE                                              ELTCONSL
02058             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
02059                        TO  WS-HOLD2.                              ELTCONSL
02060                                                                   ELTCONSL
02061      IF WS-HOLD1 = WS-HOLD2                                       ELTCONSL
02062         IF WS-HOLD1 = ZEROS                                       ELTCONSL
02063                 GO TO 5200-EXIT                                   ELTCONSL
02064         ELSE                                                      ELTCONSL
02065             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCONSL
02066             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02067             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTCONSL
02068                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02069             END-EXEC                                              ELTCONSL
02070             GO TO 5200-EXIT.                                      ELTCONSL
02071                                                                   ELTCONSL
02072      IF WS-HOLD1 = ZEROS                                          ELTCONSL
02073             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCONSL
02074             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02075             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTCONSL
02076                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02077             END-EXEC                                              ELTCONSL
02078      ELSE                                                         ELTCONSL
02079       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCONSL
02080       PERFORM 5900-GET-TAB-REC                                    ELTCONSL
02081       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTCONSL
02082                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
02083       END-EXEC                                                    ELTCONSL
02084       IF WS-HOLD2 = ZEROS                                         ELTCONSL
02085           GO TO 5200-EXIT                                         ELTCONSL
02086       ELSE                                                        ELTCONSL
02087          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCONSL
02088          PERFORM 5900-GET-TAB-REC                                 ELTCONSL
02089          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTCONSL
02090                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
02091          END-EXEC.                                                ELTCONSL
02092  5200-EXIT.     EXIT.                                             ELTCONSL
02093 /                                                                 ELTCONSL
02094  5300-BEN-TAB-AOL SECTION.                                        ELTCONSL
02095      MOVE ZEROS   TO  WS-HOLD1,                                   ELTCONSL
02096                       WS-HOLD2.                                   ELTCONSL
02097      SET PLT-INDEX2 TO 1.                                         ELTCONSL
02098      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
02099          NOT = LOW-VALUES                                         ELTCONSL
02100       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
02101          NOT = SPACE                                              ELTCONSL
02102             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
02103                        TO  WS-HOLD1.                              ELTCONSL
02104                                                                   ELTCONSL
02105      SET PLT-INDEX2 TO 2.                                         ELTCONSL
02106      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTCONSL
02107          NOT = LOW-VALUES                                         ELTCONSL
02108       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTCONSL
02109          NOT = SPACE                                              ELTCONSL
02110             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTCONSL
02111                        TO  WS-HOLD2.                              ELTCONSL
02112                                                                   ELTCONSL
02113      IF WS-HOLD1 = WS-HOLD2                                       ELTCONSL
02114         IF WS-HOLD1 = ZEROS                                       ELTCONSL
02115                 GO TO 5300-EXIT                                   ELTCONSL
02116         ELSE                                                      ELTCONSL
02117             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTCONSL
02118             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02119             EXEC CICS  LINK PROGRAM('ELFMAOL')                    ELTCONSL
02120                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02121             END-EXEC                                              ELTCONSL
02122             GO TO 5300-EXIT.                                      ELTCONSL
02123                                                                   ELTCONSL
02124      IF WS-HOLD1 = ZEROS                                          ELTCONSL
02125             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTCONSL
02126             PERFORM 5900-GET-TAB-REC                              ELTCONSL
02127             EXEC CICS  LINK PROGRAM('ELFMAOL')                    ELTCONSL
02128                             COMMAREA(DFHCOMMAREA)                 ELTCONSL
02129             END-EXEC                                              ELTCONSL
02130      ELSE                                                         ELTCONSL
02131       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTCONSL
02132       PERFORM 5900-GET-TAB-REC                                    ELTCONSL
02133       EXEC CICS  LINK PROGRAM('ELFMAOL')                          ELTCONSL
02134                       COMMAREA(DFHCOMMAREA)                       ELTCONSL
02135       END-EXEC                                                    ELTCONSL
02136       IF WS-HOLD2 = ZEROS                                         ELTCONSL
02137           GO TO 5300-EXIT                                         ELTCONSL
02138       ELSE                                                        ELTCONSL
02139          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTCONSL
02140          PERFORM 5900-GET-TAB-REC                                 ELTCONSL
02141          EXEC CICS  LINK PROGRAM('ELFMAOL')                       ELTCONSL
02142                          COMMAREA(DFHCOMMAREA)                    ELTCONSL
02143          END-EXEC.                                                ELTCONSL
02144  5300-EXIT.     EXIT.                                             ELTCONSL
02145 /                                                                 ELTCONSL
02146  5900-GET-TAB-REC SECTION.                                        ELTCONSL
02147      SET CIA-GCTABULR-DDN TO TRUE.                                ELTCONSL
02148      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
02149          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTCONSL
02150      MOVE KWA-GCTABULR-KEY                TO IOP-FILE-KEY.        ELTCONSL
02151      SET CIA-GCTABULR-DDN                 TO TRUE.                ELTCONSL
02152      SET IOP-RD                           TO TRUE.                ELTCONSL
02153      SET IOP-FCQ-NONE                     TO TRUE.                ELTCONSL
02154      SET IOP-KVQ-NONE                     TO TRUE.                ELTCONSL
02155      EXEC CICS  LINK PROGRAM('ELUIOPGM')                          ELTCONSL
02156                      COMMAREA(DFHCOMMAREA)                        ELTCONSL
02157      END-EXEC.                                                    ELTCONSL
02158                                                                   ELTCONSL
02159      IF IOP-RC-NOTFND                                             ELTCONSL
02160         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTCONSL
02161         EXEC CICS ABEND                                           ELTCONSL
02162                   ABCODE(CIA-ABCODE)                              ELTCONSL
02163         END-EXEC.                                                 ELTCONSL
02164                                                                   ELTCONSL
02165      IF NOT IOP-RC-OK                                             ELTCONSL
02166         SET CIA-AB-CRITIO          TO TRUE                        ELTCONSL
02167         EXEC CICS ABEND                                           ELTCONSL
02168                   ABCODE(CIA-ABCODE)                              ELTCONSL
02169         END-EXEC.                                                 ELTCONSL
02170                                                                   ELTCONSL
02171  5900-EXIT.     EXIT.                                             ELTCONSL
02172 /                                                                 ELTCONSL
02173  6000-MED-SURG-OB-BASIC-SAME SECTION.                             ELTCONSL
02174      IF GCT-SAME-PROV-IP-CONSULT-MED   NOT = '0'                  ELTCONSL
02175         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTCONSL
02176         MOVE 'SAME-PROV-IP-CONSULT-MED'                           ELTCONSL
02177                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTCONSL
02178         MOVE GCT-SAME-PROV-IP-CONSULT-MED                         ELTCONSL
02179                               TO  CMF-CODE-VALUE                  ELTCONSL
02180         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
02181         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-1-1        ELTCONSL
02182         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-1-2.       ELTCONSL
02183  6000-EXIT.  EXIT.                                                ELTCONSL
02184 /                                                                 ELTCONSL
02185  6400-MED-SURG-OB-SUPP-SAME SECTION.                              ELTCONSL
02186      IF GCT-SAME-PROV-IP-CONSULT-MED   NOT = '0'                  ELTCONSL
02187         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTCONSL
02188         MOVE 'SAME-PROV-IP-CONSULT-MED'                           ELTCONSL
02189                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTCONSL
02190         MOVE GCT-SAME-PROV-IP-CONSULT-MED                         ELTCONSL
02191                               TO  CMF-CODE-VALUE                  ELTCONSL
02192         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
02193         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-1-1         ELTCONSL
02194         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-1-2.        ELTCONSL
02195  6400-EXIT.  EXIT.                                                ELTCONSL
02196 /                                                                 ELTCONSL
02197  6800-OUTPUT-SAME-PROVD-TEXT SECTION.                             ELTCONSL
02198      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTCONSL
02199             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTCONSL
02200                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02201                  ADD +1                TO WS-CIA                  ELTCONSL
02202                  MOVE WS-SAME-PROVIDER TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02203                  ADD +1                TO WS-CIA.                 ELTCONSL
02204                                                                   ELTCONSL
02205      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTCONSL
02206         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02207         STRING                                                    ELTCONSL
02208                WS-DTL-BASIC-1-1 ' '                               ELTCONSL
02209                WS-BASIC-1-2                                       ELTCONSL
02210                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02211         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02212         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02213         MOVE +69              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02214         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02215         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02216         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-1-1,               ELTCONSL
02217         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
02218         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02219             ADD +1                TO  WS-CIA                      ELTCONSL
02220             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-1-2            ELTCONSL
02221             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02222             MOVE SPACES           TO  WS-DTL-BASIC-1-1,           ELTCONSL
02223                                       WS-DTL-BASIC-1-2            ELTCONSL
02224             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02225         ELSE                                                      ELTCONSL
02226           MOVE SPACES           TO  WS-DTL-BASIC-1-1,             ELTCONSL
02227                                     WS-DTL-BASIC-1-2              ELTCONSL
02228           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02229                                                                   ELTCONSL
02230      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTCONSL
02231         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02232         STRING WS-DTL-SUPP-1-1 ' '                                ELTCONSL
02233                WS-SUPP-1-2                                        ELTCONSL
02234                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02235         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02236         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02237         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02238         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02239         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02240         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-1-1                 ELTCONSL
02241         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTCONSL
02242         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02243             ADD +1                TO  WS-CIA                      ELTCONSL
02244             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-1-2             ELTCONSL
02245             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02246             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTCONSL
02247                                       WS-DTL-SUPP-1-2             ELTCONSL
02248             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02249         ELSE                                                      ELTCONSL
02250           MOVE SPACES           TO  WS-DTL-SUPP-1-1,              ELTCONSL
02251                                     WS-DTL-SUPP-1-2               ELTCONSL
02252           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02253                                                                   ELTCONSL
02254  6800-EXIT.  EXIT.                                                ELTCONSL
02255 /                                                                 ELTCONSL
02256  7000-MED-MED-BASIC-DIFF SECTION.                                 ELTCONSL
02257      IF GCT-SAME-PROV-IP-CONSULT-SRG   NOT = '0'                  ELTCONSL
02258         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTCONSL
02259         MOVE 'SAME-PROV-IP-CONSULT-SRG'                           ELTCONSL
02260                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTCONSL
02261         MOVE GCT-SAME-PROV-IP-CONSULT-SRG                         ELTCONSL
02262                               TO  CMF-CODE-VALUE                  ELTCONSL
02263         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
02264         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-1-1        ELTCONSL
02265         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-1-2.       ELTCONSL
02266  7000-EXIT.  EXIT.                                                ELTCONSL
02267 /                                                                 ELTCONSL
02268  7200-MED-MED-SUPP-DIFF SECTION.                                  ELTCONSL
02269      IF GCT-SAME-PROV-IP-CONSULT-SRG   NOT = '0'                  ELTCONSL
02270         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTCONSL
02271         MOVE 'SAME-PROV-IP-CONSULT-SRG'                           ELTCONSL
02272                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTCONSL
02273         MOVE GCT-SAME-PROV-IP-CONSULT-SRG                         ELTCONSL
02274                               TO  CMF-CODE-VALUE                  ELTCONSL
02275         PERFORM 8000-CODES-MANUAL-CALL                            ELTCONSL
02276         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-1-1         ELTCONSL
02277         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-1-2.        ELTCONSL
02278  7200-EXIT.  EXIT.                                                ELTCONSL
02279 /                                                                 ELTCONSL
02280  7500-OUTPUT-DIFF-PROVD-TEXT SECTION.                             ELTCONSL
02281      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTCONSL
02282             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTCONSL
02283                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02284                  ADD +1                TO WS-CIA                  ELTCONSL
02285                  MOVE WS-DIFF-PROVIDER TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02286                  ADD +1                TO WS-CIA.                 ELTCONSL
02287                                                                   ELTCONSL
02288                                                                   ELTCONSL
02289      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTCONSL
02290         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02291         STRING                                                    ELTCONSL
02292                WS-DTL-BASIC-1-1 ' '                               ELTCONSL
02293                WS-BASIC-1-2                                       ELTCONSL
02294                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02295         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02296         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02297         MOVE +69              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02298         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02299         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02300         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-1-1                ELTCONSL
02301         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
02302         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02303             ADD +1                TO  WS-CIA                      ELTCONSL
02304             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-1-2            ELTCONSL
02305             MOVE WS-DTL-BASIC-1-2 TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02306             MOVE SPACES           TO  WS-DTL-BASIC-1-1,           ELTCONSL
02307                                       WS-DTL-BASIC-1-2            ELTCONSL
02308             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02309         ELSE                                                      ELTCONSL
02310           MOVE SPACES           TO  WS-DTL-BASIC-1-1,             ELTCONSL
02311                                     WS-DTL-BASIC-1-2              ELTCONSL
02312           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02313                                                                   ELTCONSL
02314      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTCONSL
02315         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02316         STRING WS-DTL-SUPP-1-1 ' '                                ELTCONSL
02317                WS-SUPP-1-2                                        ELTCONSL
02318                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02319         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02320         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02321         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02322         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02323         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02324         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-1-1                 ELTCONSL
02325         MOVE WS-SUPP-1-1      TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
02326         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02327             ADD +1                TO  WS-CIA                      ELTCONSL
02328             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-1-2             ELTCONSL
02329             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02330             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTCONSL
02331                                       WS-DTL-SUPP-1-2             ELTCONSL
02332             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02333         ELSE                                                      ELTCONSL
02334           MOVE SPACES           TO  WS-DTL-SUPP-1-1,              ELTCONSL
02335                                     WS-DTL-SUPP-1-2               ELTCONSL
02336           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02337                                                                   ELTCONSL
02338                                                                   ELTCONSL
02339  7500-EXIT.  EXIT.                                                ELTCONSL
02340 /                                                                 ELTCONSL
02341  7600-ASOP-PROCESS  SECTION.                                      ELTCONSL
02342      IF PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX) NOT = 'ASOP E'       ELTCONSL
02343        GO TO 7699-EXIT.                                           ELTCONSL
02344      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTCONSL
02345         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTCONSL
02346        GO TO 7699-EXIT.                                           ELTCONSL
02347      SET PLT-INDEX2 TO 1.                                         ELTCONSL
02348      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTCONSL
02349                     WS-DTL-BASIC-1-2                              ELTCONSL
02350                     WS-DTL-SUPP-1-1                               ELTCONSL
02351                     WS-DTL-SUPP-1-2.                              ELTCONSL
02352      IF PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
02353                                              NOT = '0'            ELTCONSL
02354        IF PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTCONSL
02355                                         NOT = LOW-VALUES          ELTCONSL
02356          MOVE                                                     ELTCONSL
02357           PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTCONSL
02358                                    TO  CMF-CODE-VALUE             ELTCONSL
02359          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTCONSL
02360          MOVE 'COST-CONT-PYMT-ELIG-IND'                           ELTCONSL
02361                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONSL
02362          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
02363          MOVE CMF-DESCR-LINE (1) TO WS-DTL-BASIC-1-1              ELTCONSL
02364          MOVE CMF-DESCR-LINE (2) TO WS-DTL-BASIC-1-2.             ELTCONSL
02365                                                                   ELTCONSL
02366      SET PLT-INDEX2 TO 2.                                         ELTCONSL
02367      IF PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)      ELTCONSL
02368                                              NOT = '0'            ELTCONSL
02369        IF PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTCONSL
02370                                         NOT = LOW-VALUES          ELTCONSL
02371          MOVE                                                     ELTCONSL
02372           PLP-COST-CONT-PYMT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTCONSL
02373                                    TO  CMF-CODE-VALUE             ELTCONSL
02374          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTCONSL
02375          MOVE 'COST-CONT-PYMT-ELIG-IND'                           ELTCONSL
02376                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTCONSL
02377          PERFORM 8000-CODES-MANUAL-CALL                           ELTCONSL
02378          MOVE CMF-DESCR-LINE (1) TO WS-DTL-SUPP-1-1               ELTCONSL
02379          MOVE CMF-DESCR-LINE (2) TO WS-DTL-SUPP-1-2.              ELTCONSL
02380                                                                   ELTCONSL
02381      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTCONSL
02382             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTCONSL
02383                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02384                  ADD +1                TO WS-CIA                  ELTCONSL
02385                  MOVE WS-ASOP          TO COF-DTL-LINE(WS-CIA)    ELTCONSL
02386                  ADD +1                TO WS-CIA.                 ELTCONSL
02387                                                                   ELTCONSL
02388      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTCONSL
02389         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02390         STRING                                                    ELTCONSL
02391                WS-DTL-BASIC-1-1 ' '                               ELTCONSL
02392                WS-BASIC-1-2                                       ELTCONSL
02393                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02394         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02395         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02396         MOVE +69              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02397         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02398         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02399         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-1-1,               ELTCONSL
02400         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTCONSL
02401         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02402             ADD +1                TO  WS-CIA                      ELTCONSL
02403             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-1-2            ELTCONSL
02404             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02405             MOVE SPACES           TO  WS-DTL-BASIC-1-1,           ELTCONSL
02406                                       WS-DTL-BASIC-1-2            ELTCONSL
02407             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02408         ELSE                                                      ELTCONSL
02409           MOVE SPACES           TO  WS-DTL-BASIC-1-1,             ELTCONSL
02410                                     WS-DTL-BASIC-1-2              ELTCONSL
02411           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02412                                                                   ELTCONSL
02413      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTCONSL
02414         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTCONSL
02415         STRING WS-DTL-SUPP-1-1 ' '                                ELTCONSL
02416                WS-SUPP-1-2                                        ELTCONSL
02417                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTCONSL
02418         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTCONSL
02419         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTCONSL
02420         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTCONSL
02421         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTCONSL
02422         PERFORM TCPR-000-TEXT-UNSTRING                            ELTCONSL
02423         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-1-1                 ELTCONSL
02424         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTCONSL
02425         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTCONSL
02426             ADD +1                TO  WS-CIA                      ELTCONSL
02427             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-1-2             ELTCONSL
02428             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTCONSL
02429             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTCONSL
02430                                       WS-DTL-SUPP-1-2             ELTCONSL
02431             PERFORM 3000-OUTPUT-TEXT                              ELTCONSL
02432         ELSE                                                      ELTCONSL
02433           MOVE SPACES           TO  WS-DTL-SUPP-1-1,              ELTCONSL
02434                                     WS-DTL-SUPP-1-2               ELTCONSL
02435           PERFORM 3000-OUTPUT-TEXT.                               ELTCONSL
02436  7699-EXIT.                                                       ELTCONSL
02437      EXIT.                                                        ELTCONSL
02438 /            C O D E   M A N U A L   C A L L                      ELTCONSL
02439  8000-CODES-MANUAL-CALL.                                          ELTCONSL
02440      INITIALIZE CMF-RETURN-CODE,                                  ELTCONSL
02441                 TCAR-FROM-AREA.                                   ELTCONSL
02442                                                                   ELTCONSL
02443      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTCONSL
02444                           COMMAREA(DFHCOMMAREA)                   ELTCONSL
02445      END-EXEC.                                                    ELTCONSL
02446                                                                   ELTCONSL
02447      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTCONSL
02448      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTCONSL
02449          ADDRESS OF CMF-DESCR.                                    ELTCONSL
02450                                                                   ELTCONSL
02451  8000-EXIT.       EXIT.                                           ELTCONSL
02452                                                                   ELTCONSL
02453                                                                   ELTCONSL
02454  3000-PREPARE-UNSTRING SECTION.                                   ELTCONSL
02455      MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTCONSL
02456      MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTCONSL
02457      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTCONSL
02458      MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                         ELTCONSL
02459      MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                         ELTCONSL
02460      MOVE +79 TO TCAR-OUTPUT-FIELD-5-LEN.                         ELTCONSL
02461      MOVE +79 TO TCAR-OUTPUT-FIELD-6-LEN.                         ELTCONSL
02462      MOVE +79 TO TCAR-OUTPUT-FIELD-7-LEN.                         ELTCONSL
02463      MOVE +79 TO TCAR-OUTPUT-FIELD-8-LEN.                         ELTCONSL
02464      MOVE +79 TO TCAR-OUTPUT-FIELD-9-LEN.                         ELTCONSL
02465      MOVE +79 TO TCAR-OUTPUT-FIELD-10-LEN.                        ELTCONSL
02466      MOVE +79 TO TCAR-OUTPUT-FIELD-11-LEN.                        ELTCONSL
02467      MOVE +79 TO TCAR-OUTPUT-FIELD-12-LEN.                        ELTCONSL
02468      MOVE +79 TO TCAR-OUTPUT-FIELD-13-LEN.                        ELTCONSL
02469      MOVE +79 TO TCAR-OUTPUT-FIELD-14-LEN.                        ELTCONSL
02470      MOVE +79 TO TCAR-OUTPUT-FIELD-15-LEN.                        ELTCONSL
02471      MOVE +79 TO TCAR-OUTPUT-FIELD-16-LEN.                        ELTCONSL
02472      MOVE +79 TO TCAR-OUTPUT-FIELD-17-LEN.                        ELTCONSL
02473      MOVE +79 TO TCAR-OUTPUT-FIELD-18-LEN.                        ELTCONSL
02474      MOVE +79 TO TCAR-OUTPUT-FIELD-19-LEN.                        ELTCONSL
02475      MOVE +79 TO TCAR-OUTPUT-FIELD-20-LEN.                        ELTCONSL
02476  3000-EXIT.     EXIT.                                             ELTCONSL
02477                                                                   ELTCONSL
02478 /   C O M P R E S S I O N   A N D   U N S T R I N G   R O U T I N ELTCONSL
02479  COPY ELSTCOMP.                                                   ELTCONSL
02480 /              A B E N D                                          ELTCONSL
02481 ******************************************************************ELTCONSL
02482 *                        A B E N D                                ELTCONSL
02483 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTCONSL
02484 *                                                                 ELTCONSL
02485 ******************************************************************ELTCONSL
02486  9999-ABEND SECTION.                                              ELTCONSL
02487                                                                   ELTCONSL
02488                                                                   ELTCONSL
02489      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTCONSL
02490                                                                   ELTCONSL
02491  9999-EXIT.     EXIT.                                             ELTCONSL
