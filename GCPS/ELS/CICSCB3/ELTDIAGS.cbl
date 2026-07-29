00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTDIAGS.                                            ELTDIAGS
00003  AUTHOR. JOHN CURIN - KEANE, INC.                                    LV002
00004  DATE-WRITTEN.   4/08/86.                                         ELTDIAGS
00005  DATE-COMPILED.                                                   ELTDIAGS
00006      SKIP3                                                        ELTDIAGS
00007 ******************************************************************ELTDIAGS
00008 *@>ELTDIAGS                                                       ELTDIAGS
00009 *@¬                                                               ELTDIAGS
00010 *                        PROGRAM ABSTRACT                         ELTDIAGS
00011 *                                                                 ELTDIAGS
00012 *@¬ PROGRAM NAME:   E.L.S. DIAGNOSTIC PROCEDURE TOPIC             ELTDIAGS
00013 *@¬                                                               ELTDIAGS
00014 *@¬ PROGRAM I.D.:   ELTDIAGS                                      ELTDIAGS
00015 *@¬                                                               ELTDIAGS
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDIAGS
00017 *@¬            DIAGNOSTIC PROCEDURE COVERAGE GIVEN A MEMBER.      ELTDIAGS
00018 *@¬                                                               ELTDIAGS
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DIAGNOSTIC        ELTDIAGS
00020 *@¬            PRROCEDURES IS AFFORD A MEMBER BY HIS GROUP.       ELTDIAGS
00021 *@¬            THIS INFORMATION IS                                ELTDIAGS
00022 *@¬            GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTDIAGS
00023 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTDIAGS
00024 *@¬            RANGE OF DATES.                                    ELTDIAGS
00025 *@¬                                                               ELTDIAGS
00026 *@¬ RECORDS                                                       ELTDIAGS
00027 *@¬ ACCESSED:  GROUP SPECIFIC, VARIOUS BENEFIT PROVISION, AND A   ELTDIAGS
00028 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTDIAGS
00029 *@¬                                                               ELTDIAGS
00030 *@¬ PROCESSING                                                    ELTDIAGS
00031 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTDIAGS
00032 *@¬                                                               ELTDIAGS
00033 *@¬                                                               ELTDIAGS
00034 ***************************************************************** ELTDIAGS
00035      SKIP3                                                        ELTDIAGS
00036 ***************************************************************** ELTDIAGS
00037 *                    U P D A T E   H I S T O R Y                * ELTDIAGS
00038 *                                                               * ELTDIAGS
00039 *   DATE    PGM  DESCRIPTION  (MOST CURRENT AT TOP)             * ELTDIAGS
00040 * --------  ---  ---------------------------------------------- * ELTDIAGS
00041 *                                                               * ELTDIAGS
00042 * 07/02/86  LET  DISCREPANCY FIX #175.  ADDED PROFESSIONAL      * ELTDIAGS
00043 *                CHARGES ON A HOSPITAL CLAIM ELEMENT TO TOPIC.  * ELTDIAGS
00044 *                                                               * ELTDIAGS
00045 * 10/06/86  JTC  VS COBOL II CONVERSION                         * ELTDIAGS
00046 *                                                               * ELTDIAGS
00047 * 03/13/87  JTC  CHANGE KEYWORD FROM MEDPROC TO MED ADDED       * ELTDIAGS
00048 *                ABEND IF ANY OF THE REQUIRED SSB FIELD ARE IN  * ELTDIAGS
00049 *                ERROR                                          * ELTDIAGS
00050 *                                                               * ELTDIAGS
00051 * 10/19/87 AKK   CHANGE 'THIS GROUP OF BENEFITS ARE HANDLED AS  * ELTDIAGS
00052 *                FOLLOWS' TO 'COVERED SERVICES ARE'.            * ELTDIAGS
00053 *                                                               * ELTDIAGS
00054 * 12/11/87  REB  CHANGED CODE TO ALLOW THE HEADINGS INST. AND   * ELTDIAGS
00055 *                PROF. TO DISPLAY.                              * ELTDIAGS
00056 *                                                               * ELTDIAGS
00057 * 12/11/87  REB  ADDED CODE TO DISPLAY ALL INFO PERTAINING TO   * ELTDIAGS
00058 *                ANY INPATIENT BENEFITS.                        * ELTDIAGS
00059 *                                                               * ELTDIAGS
00060 * 12/15/87  REB  ADDED CODE FOR SUBTOPICS 'MED' & 'XRY' TO      * ELTDIAGS
00061 *                DISPLAY INPATIENT BENEFITS. THESE CHANGES ABOVE* ELTDIAGS
00062 *                CORRESPOND TO DISCREPANCY (P4675).             * ELTDIAGS
00063 * 10/18/88  EGL -ADDED MISSING SECTION CLAUSE TO CORRECT ABEND  * ELTDIAGS
00064 *                FOUND DURING TESTING.  DUE TO MISSING SECTION, * ELTDIAGS
00065 *                CODES MANUAL WAS INCORRECTLY LINKED TO WHEN    * ELTDIAGS
00066 *                EVER THE OUTPUT INTERFACE WAS CALLED.          * ELTDIAGS
00067 *               -ALSO, IMPLEMENTED THE NEW STORAGE MANAGEMENT   * ELTDIAGS
00068 *                ROUTINES.                                      * ELTDIAGS
00069 * 11/10/88  AKK -CHANGED CALL TO ELFABM TO ELGMAXIM, HAD BEEN   * ELTDIAGS
00070 *                CAUSING PRODUCTION ABEND                       * ELTDIAGS
00071 * 10/13/89  RKH -ADDED TRANSFER TO OTHER RESPON IND             * ELTDIAGS
00072 *                                                                 ELTDIAGS
00073 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY   ELTDIAGS
00074 *                      FROM A SINGLE POSITION TO ZEROS            ELTDIAGS
00075 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTDIAGS
00076 *                                                                 ELTDIAGS
00077 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTDIAGS
00078 *                                                                 ELTDIAGS
00079 ***************************************************************** ELTDIAGS
00080 *                                                               * ELTDIAGS
00081 ***************************************************************** ELTDIAGS
00082 /                                                                 ELTDIAGS
00083  ENVIRONMENT DIVISION.                                            ELTDIAGS
00084      SKIP3                                                        ELTDIAGS
00085  DATA DIVISION.                                                   ELTDIAGS
00086  WORKING-STORAGE SECTION.                                         ELTDIAGS
00087  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDIAGS
00088      '***ELTDIAGS WS BEGINS***'.                                  ELTDIAGS
00089  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTDIAGS
00090                                                                   ELTDIAGS
00091  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTDIAGS
00092                                                                   ELTDIAGS
00093 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDIAGS
00094  01  WS-WORK-FIELDS.                                              ELTDIAGS
00095      05  WS-CHAR-0                     PIC X.                     ELTDIAGS
00096      05  WS-HOLD1                      PIC X(10).                 ELTDIAGS
00097      05  WS-HOLD2                      PIC X(10).                 ELTDIAGS
00098      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X(01).                 ELTDIAGS
00099      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTDIAGS
00100      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTDIAGS
00101      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTDIAGS
00102      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTDIAGS
00103      05  WS-DTL-PP                     PIC X(50).                 ELTDIAGS
00104      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTDIAGS
00105      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTDIAGS
00106      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTDIAGS
00107      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTDIAGS
00108      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTDIAGS
00109      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTDIAGS
00110      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDIAGS
00111      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTDIAGS
00112      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTDIAGS
00113      05  WS-FIRSTTIME-IND              PIC X.                     ELTDIAGS
00114          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTDIAGS
00115      05  WS-ADD-A-BLANK-IND            PIC  X(01) VALUE 'N'.      ELTDIAGS
00116          88  WS-ADD-A-BLANK-LINE                  VALUE 'Y'.      ELTDIAGS
00117                                                                   ELTDIAGS
00118 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTDIAGS
00119  01  WS-BEN-PROV-ID.                                              ELTDIAGS
00120      05  WS-TABLE-MAX-CNT              PIC S9(4) COMP   VALUE +03.ELTDIAGS
00121      05  WS-INST-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDIAGS
00122      05  WS-PROF-COUNTER               PIC S9(4) COMP  VALUE +0.  ELTDIAGS
00123      05  WS-INST-IP-CNT-LAB            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00124      05  WS-INST-IP-TAB-LAB.                                      ELTDIAGS
00125        10  FILLER                      PIC X(6)  VALUE 'LABI B'.  ELTDIAGS
00126        10  FILLER                      PIC X(6)  VALUE 'PAPI B'.  ELTDIAGS
00127      05  WS-INST-IP-LIST-LAB REDEFINES    WS-INST-IP-TAB-LAB      ELTDIAGS
00128                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00129      05  WS-INST-OP-CNT-LAB            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00130      05  WS-INST-OP-TAB-LAB.                                      ELTDIAGS
00131        10  FILLER                      PIC X(6)  VALUE 'LABO B'.  ELTDIAGS
00132        10  FILLER                      PIC X(6)  VALUE 'PAPO B'.  ELTDIAGS
00133      05  WS-INST-OP-LIST-LAB REDEFINES    WS-INST-OP-TAB-LAB      ELTDIAGS
00134                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00135                                                                   ELTDIAGS
00136      05  WS-INST-IP-CNT-MED            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00137      05  WS-INST-IP-TAB-MED.                                      ELTDIAGS
00138        10  FILLER                      PIC X(6)  VALUE 'DMPI B'.  ELTDIAGS
00139        10  FILLER                      PIC X(6)  VALUE 'DIAI B'.  ELTDIAGS
00140      05  WS-INST-IP-LIST-MED REDEFINES    WS-INST-IP-TAB-MED      ELTDIAGS
00141                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00142      05  WS-INST-OP-CNT-MED            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00143      05  WS-INST-OP-TAB-MED.                                      ELTDIAGS
00144        10  FILLER                      PIC X(6)  VALUE 'DMPO B'.  ELTDIAGS
00145        10  FILLER                      PIC X(6)  VALUE 'DIAO A'.  ELTDIAGS
00146      05  WS-INST-OP-LIST-MED REDEFINES    WS-INST-OP-TAB-MED      ELTDIAGS
00147                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00148                                                                   ELTDIAGS
00149      05  WS-INST-IP-CNT-XRY            PIC S9(4) COMP VALUE +02.  ELTDIAGS
00150      05  WS-INST-IP-TAB-XRY.                                      ELTDIAGS
00151        10  FILLER                      PIC X(6)  VALUE 'XRYI B'.  ELTDIAGS
00152        10  FILLER                      PIC X(6)  VALUE 'RII  B'.  ELTDIAGS
00153      05  WS-INST-IP-LIST-XRY REDEFINES    WS-INST-IP-TAB-XRY      ELTDIAGS
00154                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00155      05  WS-INST-OP-CNT-XRY            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00156      05  WS-INST-OP-TAB-XRY.                                      ELTDIAGS
00157        10  FILLER                      PIC X(6)  VALUE 'XRYO B'.  ELTDIAGS
00158        10  FILLER                      PIC X(6)  VALUE 'RIO  B'.  ELTDIAGS
00159      05  WS-INST-OP-LIST-XRY REDEFINES    WS-INST-OP-TAB-XRY      ELTDIAGS
00160                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00161                                                                   ELTDIAGS
00162      05  WS-PROF-IP-CNT-LAB            PIC S9(4) COMP  VALUE +03. ELTDIAGS
00163      05  WS-PROF-IP-TAB-LAB.                                      ELTDIAGS
00164        10  FILLER                      PIC X(6)  VALUE 'LABI E'.  ELTDIAGS
00165        10  FILLER                      PIC X(6)  VALUE 'PAPI E'.  ELTDIAGS
00166        10  FILLER                      PIC X(6)  VALUE 'PTI  E'.  ELTDIAGS
00167      05  WS-PROF-IP-LIST-LAB REDEFINES    WS-PROF-IP-TAB-LAB      ELTDIAGS
00168                                        PIC X(6)  OCCURS 3 TIMES.  ELTDIAGS
00169      05  WS-PROF-OP-CNT-LAB            PIC S9(4) COMP  VALUE +03. ELTDIAGS
00170      05  WS-PROF-OP-TAB-LAB.                                      ELTDIAGS
00171        10  FILLER                      PIC X(6)  VALUE 'LABO E'.  ELTDIAGS
00172        10  FILLER                      PIC X(6)  VALUE 'PAPO E'.  ELTDIAGS
00173        10  FILLER                      PIC X(6)  VALUE 'PTO  E'.  ELTDIAGS
00174      05  WS-PROF-OP-LIST-LAB REDEFINES    WS-PROF-OP-TAB-LAB      ELTDIAGS
00175                                        PIC X(6)  OCCURS 3 TIMES.  ELTDIAGS
00176                                                                   ELTDIAGS
00177      05  WS-PROF-IP-CNT-MED            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00178      05  WS-PROF-IP-TAB-MED.                                      ELTDIAGS
00179        10  FILLER                      PIC X(6)  VALUE 'DMPI E'.  ELTDIAGS
00180        10  FILLER                      PIC X(6)  VALUE 'DIAI D'.  ELTDIAGS
00181      05  WS-PROF-IP-LIST-MED REDEFINES    WS-PROF-IP-TAB-MED      ELTDIAGS
00182                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00183      05  WS-PROF-OP-CNT-MED            PIC S9(4) COMP VALUE +02.  ELTDIAGS
00184      05  WS-PROF-OP-TAB-MED.                                      ELTDIAGS
00185        10  FILLER                      PIC X(6)  VALUE 'DMPO E'.  ELTDIAGS
00186        10  FILLER                      PIC X(6)  VALUE 'DIAO E'.  ELTDIAGS
00187      05  WS-PROF-OP-LIST-MED REDEFINES    WS-PROF-OP-TAB-MED      ELTDIAGS
00188                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00189                                                                   ELTDIAGS
00190      05  WS-PROF-IP-CNT-XRY            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00191      05  WS-PROF-IP-TAB-XRY.                                      ELTDIAGS
00192        10  FILLER                      PIC X(6)  VALUE 'XRYI E'.  ELTDIAGS
00193        10  FILLER                      PIC X(6)  VALUE 'RII  E'.  ELTDIAGS
00194      05  WS-PROF-IP-LIST-XRY REDEFINES    WS-PROF-IP-TAB-XRY      ELTDIAGS
00195                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00196      05  WS-PROF-OP-CNT-XRY            PIC S9(4) COMP  VALUE +02. ELTDIAGS
00197      05  WS-PROF-OP-TAB-XRY.                                      ELTDIAGS
00198        10  FILLER                      PIC X(6)  VALUE 'XRYO E'.  ELTDIAGS
00199        10  FILLER                      PIC X(6)  VALUE 'RIO  E'.  ELTDIAGS
00200      05  WS-PROF-OP-LIST-XRY REDEFINES    WS-PROF-OP-TAB-XRY      ELTDIAGS
00201                                        PIC X(6)  OCCURS 2 TIMES.  ELTDIAGS
00202                                                                   ELTDIAGS
00203 /                L I T E R A L S                                  ELTDIAGS
00204  01  WS-PROGRAM-LITERALS.                                         ELTDIAGS
00205    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTDIAGS
00206    05  DAYS                        PIC X(04) VALUE 'DAYS'.        ELTDIAGS
00207    05  WS-NO                       PIC X     VALUE 'N'.           ELTDIAGS
00208    05  WS-YES                      PIC X     VALUE 'Y'.           ELTDIAGS
00209    05  WS-BASIC-LIT                PIC X(16) VALUE                ELTDIAGS
00210        '         BASIC: '.                                        ELTDIAGS
00211    05  WS-SECONDARY-LIT            PIC X(16) VALUE                ELTDIAGS
00212        '     SECONDARY: '.                                        ELTDIAGS
00213    05  WS-SUPPLEMENTAL-LIT         PIC X(16) VALUE                ELTDIAGS
00214        '  SUPPLEMENTAL: '.                                        ELTDIAGS
00215    05  WS-DAYS-REDUCED             PIC X(44) VALUE                ELTDIAGS
00216          ' OUTPATIENT DIALYSIS TREATMENTS REDUCE DAYS '.          ELTDIAGS
00217    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTDIAGS
00218    05  WS-PAYMNT-BASED             PIC X(20)                      ELTDIAGS
00219          VALUE 'PAYMENT IS BASED ON:'.                            ELTDIAGS
00220    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTDIAGS
00221          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTDIAGS
00222    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTDIAGS
00223          VALUE 'SPILLOVER COINSURANCE: '.                         ELTDIAGS
00224    05  WS-SERVICES-RENDERED        PIC X(26)                      ELTDIAGS
00225          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTDIAGS
00226                                                                   ELTDIAGS
00227 /            D I S P L A Y   L I N E S                            ELTDIAGS
00228  01  WS-ELS-DISPLAY-LINES.                                        ELTDIAGS
00229    05  WS-HDR-1.                                                  ELTDIAGS
00230      10  FILLER                    PIC X(12) VALUE 'SECTION NO: '.ELTDIAGS
00231      10  WS-HDR1-SECT-NO           PIC X(5)  VALUE SPACES.        ELTDIAGS
00232      10  FILLER                    PIC X(22)                      ELTDIAGS
00233          VALUE '      EFFECTIVE DATE: '.                          ELTDIAGS
00234      10  WS-HDR1-DATE              PIC X(8).                      ELTDIAGS
00235      10  FILLER                    PIC X(28)                      ELTDIAGS
00236          VALUE '      FAMILY RELATIONSHIP: '.                     ELTDIAGS
00237      10  WS-HDR1-FRL               PIC X.                         ELTDIAGS
00238      10  FILLER                    PIC X(4) VALUE LOW-VALUES.     ELTDIAGS
00239                                                                   ELTDIAGS
00240    05  WS-HDR-2-INST-LAB.                                         ELTDIAGS
00241      10  FILLER                    PIC X(20) VALUE SPACES.        ELTDIAGS
00242      10  FILLER                    PIC X(35)                      ELTDIAGS
00243          VALUE 'DIAGNOSTIC LABORATORY INSTITUTIONAL'.             ELTDIAGS
00244      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTDIAGS
00245                                                                   ELTDIAGS
00246    05  WS-HDR-2-INST-MED.                                         ELTDIAGS
00247      10  FILLER                    PIC X(15) VALUE SPACES.        ELTDIAGS
00248      10  FILLER                    PIC X(44)                      ELTDIAGS
00249          VALUE 'DIAGNOSTIC MEDICAL PROCEDURES INSTITUTIONAL'.     ELTDIAGS
00250      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTDIAGS
00251                                                                   ELTDIAGS
00252    05  WS-HDR-2-INST-XRY.                                         ELTDIAGS
00253      10  FILLER                    PIC X(21) VALUE SPACES.        ELTDIAGS
00254      10  FILLER                    PIC X(29)                      ELTDIAGS
00255          VALUE 'DIAGNOSTIC XRAY INSTITUTIONAL'.                   ELTDIAGS
00256      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDIAGS
00257                                                                   ELTDIAGS
00258    05  WS-HDR-2-PROF-LAB.                                         ELTDIAGS
00259      10  FILLER                    PIC X(20) VALUE SPACES.        ELTDIAGS
00260      10  FILLER                    PIC X(34)                      ELTDIAGS
00261          VALUE 'DIAGNOSTIC LABORATORY PROFESSIONAL'.              ELTDIAGS
00262      10  FILLER                    PIC X(25) VALUE LOW-VALUES.    ELTDIAGS
00263                                                                   ELTDIAGS
00264    05  WS-HDR-2-PROF-MED.                                         ELTDIAGS
00265      10  FILLER                    PIC X(16) VALUE SPACES.        ELTDIAGS
00266      10  FILLER                    PIC X(43)                      ELTDIAGS
00267          VALUE 'DIAGNOSTIC MEDICAL PROCEDURES PROFESSIONAL'.      ELTDIAGS
00268      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTDIAGS
00269                                                                   ELTDIAGS
00270    05  WS-HDR-2-PROF-XRY.                                         ELTDIAGS
00271      10  FILLER                    PIC X(20) VALUE SPACES.        ELTDIAGS
00272      10  FILLER                    PIC X(28)                      ELTDIAGS
00273          VALUE 'DIAGNOSTIC XRAY PROFESSIONAL'.                    ELTDIAGS
00274      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTDIAGS
00275                                                                   ELTDIAGS
00276    05  WS-INPATIENT-LAB-ARE.                                      ELTDIAGS
00277      10  FILLER                    PIC X(30)                      ELTDIAGS
00278          VALUE 'INPATIENT LABORATORY SERVICES '.                  ELTDIAGS
00279      10  FILLER                    PIC X(49)  VALUE LOW-VALUES.   ELTDIAGS
00280                                                                   ELTDIAGS
00281    05  WS-OUTPATIENT-LAB-ARE.                                     ELTDIAGS
00282      10  FILLER                    PIC X(42)                      ELTDIAGS
00283          VALUE 'OUTPATIENT DIAGNOSTIC LABORATORY SERVICES '.      ELTDIAGS
00284      10  FILLER                    PIC X(37)  VALUE LOW-VALUES.   ELTDIAGS
00285                                                                   ELTDIAGS
00286    05  WS-INPATIENT-MED-ARE.                                      ELTDIAGS
00287      10  FILLER                    PIC X(29)                      ELTDIAGS
00288          VALUE 'INPATIENT MEDICAL PROCEDURES '.                   ELTDIAGS
00289      10  FILLER                    PIC X(50) VALUE LOW-VALUES.    ELTDIAGS
00290                                                                   ELTDIAGS
00291    05  WS-OUTPATIENT-MED-ARE.                                     ELTDIAGS
00292      10  FILLER                    PIC X(41)                      ELTDIAGS
00293          VALUE 'OUTPATIENT DIAGNOSTIC MEDICAL PROCEDURES '.       ELTDIAGS
00294      10  FILLER                    PIC X(38) VALUE LOW-VALUES.    ELTDIAGS
00295                                                                   ELTDIAGS
00296    05  WS-INPATIENT-XRY-ARE.                                      ELTDIAGS
00297      10  FILLER                    PIC X(24)                      ELTDIAGS
00298          VALUE 'INPATIENT XRAY SERVICES '.                        ELTDIAGS
00299      10  FILLER                    PIC X(55) VALUE LOW-VALUES.    ELTDIAGS
00300                                                                   ELTDIAGS
00301    05  WS-OUTPATIENT-XRY-ARE.                                     ELTDIAGS
00302      10  FILLER                    PIC X(36)                      ELTDIAGS
00303          VALUE 'OUTPATIENT DIAGNOSTIC XRAY SERVICES '.            ELTDIAGS
00304      10  FILLER                    PIC X(43) VALUE LOW-VALUES.    ELTDIAGS
00305                                                                   ELTDIAGS
00306    05  WS-FOLLOW-BENEFIT.                                         ELTDIAGS
00307      10  FILLER                    PIC X(79) VALUE                ELTDIAGS
00308          'COVERED SERVICES ARE:'.                                 ELTDIAGS
00309                                                                   ELTDIAGS
00310    05  WS-PAY-CONSDR-TEXT1.                                       ELTDIAGS
00311      10  FILLER                    PIC  X(45)                     ELTDIAGS
00312        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTDIAGS
00313                                                                   ELTDIAGS
00314    05  WS-PAY-CONSDR-TEXT2.                                       ELTDIAGS
00315      10  FILLER                    PIC X(45)                      ELTDIAGS
00316        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTDIAGS
00317                                                                   ELTDIAGS
00318    05  WS-PROF-INPT-CHRGES         PIC  X(61) VALUE               ELTDIAGS
00319            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTDIAGS
00320 -          'EPORT: '.                                             ELTDIAGS
00321                                                                   ELTDIAGS
00322    05  WS-PROF-OUTPT-CHRGES        PIC  X(62) VALUE               ELTDIAGS
00323            'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE ELTDIAGS
00324 -          'REPORT: '.                                            ELTDIAGS
00325                                                                   ELTDIAGS
00326    05  WS-SERVICES-2ND.                                           ELTDIAGS
00327      10  FILLER                    PIC X(21) VALUE SPACES.        ELTDIAGS
00328      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTDIAGS
00329      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTDIAGS
00330                                                                   ELTDIAGS
00331    05  WS-SERVICES-PAYABLE.                                       ELTDIAGS
00332      10  FILLER                    PIC X(45)                      ELTDIAGS
00333          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTDIAGS
00334      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDIAGS
00335                                                                   ELTDIAGS
00336    05  WS-BASIC.                                                  ELTDIAGS
00337      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDIAGS
00338      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDIAGS
00339      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTDIAGS
00340                                                                   ELTDIAGS
00341    05  WS-SUPPLEMENTAL.                                           ELTDIAGS
00342      10  FILLER                    PIC X(16)                      ELTDIAGS
00343          VALUE '  SUPPLEMENTAL: '.                                ELTDIAGS
00344      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTDIAGS
00345                                                                   ELTDIAGS
00346    05  WS-BASIC-PERCENT.                                          ELTDIAGS
00347      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDIAGS
00348      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDIAGS
00349      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTDIAGS
00350                                                                   ELTDIAGS
00351    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTDIAGS
00352      10  FILLER                    PIC X(16)                      ELTDIAGS
00353          VALUE '  SUPPLEMENTAL: '.                                ELTDIAGS
00354      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTDIAGS
00355                                                                   ELTDIAGS
00356    05  WS-CONTACT-CONTRACT.                                       ELTDIAGS
00357      10  FILLER                    PIC X(50)                      ELTDIAGS
00358        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTDIAGS
00359      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDIAGS
00360                                                                   ELTDIAGS
00361    05  WS-CONTRACT-RELATED.                                       ELTDIAGS
00362      10  FILLER                    PIC X(49)                      ELTDIAGS
00363        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTDIAGS
00364      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDIAGS
00365                                                                   ELTDIAGS
00366    05  WS-PVE-TEXT.                                               ELTDIAGS
00367      10  FILLER                    PIC X(44) VALUE                ELTDIAGS
00368        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTDIAGS
00369                                                                   ELTDIAGS
00370  01  WS-END                            PIC X(16)  VALUE           ELTDIAGS
00371      '*** W/S ENDS ***'.                                          ELTDIAGS
00372 /             L I N K A G E   S E C T I O N                       ELTDIAGS
00373  LINKAGE SECTION.                                                 ELTDIAGS
00374  01  DFHCOMMAREA.                                                 ELTDIAGS
00375      COPY ELSCOMMC.                                               ELTDIAGS
00376 /  *** CIA  AREA ***                                              ELTDIAGS
00377      COPY ELSCIA2C.                                               ELTDIAGS
00378 /  *** IO PARM AREA ***                                           ELTDIAGS
00379      COPY ELSIOPMC.                                               ELTDIAGS
00380 /  *** KEY AREA ***                                               ELTDIAGS
00381      COPY ELSKEYSC.                                               ELTDIAGS
00382 /  *** OUTPUT TEXT AREA ***                                       ELTDIAGS
00383      COPY ELSOUTPC.                                               ELTDIAGS
00384 /  *** TOPIC SELECTION AREA ***                                   ELTDIAGS
00385      COPY ELSSSCBC.                                               ELTDIAGS
00386 /  *** CODE MANUAL INTERFACE ***                                  ELTDIAGS
00387      COPY ELSCMIFC.                                               ELTDIAGS
00388 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDIAGS
00389      COPY ELSCMDSC.                                               ELTDIAGS
00390 /  *** BENEFIT PROVISION TABLE ***                                ELTDIAGS
00391      COPY ELSPRVNC.                                               ELTDIAGS
00392 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDIAGS
00393      COPY ELSTCWAC.                                               ELTDIAGS
00394 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDIAGS
00395      COPY ELSPLGSW.                                               ELTDIAGS
00396                                                                   ELTDIAGS
00397 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDIAGS
00398      COPY ELSPLGTB.                                               ELTDIAGS
00399 /                  M A I N L I N E                                ELTDIAGS
00400  PROCEDURE DIVISION.                                              ELTDIAGS
00401                                                                   ELTDIAGS
00402 ******************************************************************ELTDIAGS
00403 *                                                                 ELTDIAGS
00404 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDIAGS
00405 *                                                                 ELTDIAGS
00406 ******************************************************************ELTDIAGS
00407  0000-MAINLINE SECTION.                                           ELTDIAGS
00408                                                                   ELTDIAGS
00409      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDIAGS
00410         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDIAGS
00411                                                                   ELTDIAGS
00412      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDIAGS
00413                 ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.         ELTDIAGS
00414                                                                   ELTDIAGS
00415      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDIAGS
00416      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00417          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDIAGS
00418      IF NOT CIA-RC-OK                                             ELTDIAGS
00419          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00420                                                                   ELTDIAGS
00421      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDIAGS
00422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00423          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDIAGS
00424      IF NOT CIA-RC-OK                                             ELTDIAGS
00425          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00426                                                                   ELTDIAGS
00427      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDIAGS
00428      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00429          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDIAGS
00430      IF NOT CIA-RC-OK                                             ELTDIAGS
00431          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00432                                                                   ELTDIAGS
00433      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDIAGS
00434      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00435          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDIAGS
00436      IF NOT CIA-RC-OK                                             ELTDIAGS
00437          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00438                                                                   ELTDIAGS
00439      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDIAGS
00440      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00441          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDIAGS
00442      IF NOT CIA-RC-OK                                             ELTDIAGS
00443          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00444                                                                   ELTDIAGS
00445      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDIAGS
00446      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00447          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDIAGS
00448      IF NOT CIA-RC-OK                                             ELTDIAGS
00449          PERFORM 9998-INVALID-PTR.                                ELTDIAGS
00450                                                                   ELTDIAGS
00451      MOVE '0'  TO  WS-CHAR-0.                                     ELTDIAGS
00452                                                                   ELTDIAGS
00453      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDIAGS
00454                                                                   ELTDIAGS
00455      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDIAGS
00456              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDIAGS
00457                                                                   ELTDIAGS
00458      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDIAGS
00459                                                                   ELTDIAGS
00460      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDIAGS
00461                                                                   ELTDIAGS
00462      EXEC CICS LINK                                               ELTDIAGS
00463                PROGRAM('ELUSTGMG')                                ELTDIAGS
00464                COMMAREA(DFHCOMMAREA)                              ELTDIAGS
00465      END-EXEC.                                                    ELTDIAGS
00466                                                                   ELTDIAGS
00467      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDIAGS
00468      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00469          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDIAGS
00470                                                                   ELTDIAGS
00471      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH) AND          ELTDIAGS
00472           SSB-SUB-TOPIC = 'LAB'                                   ELTDIAGS
00473         PERFORM 1000-INSTITUTIONAL-LAB-RTNE.                      ELTDIAGS
00474                                                                   ELTDIAGS
00475      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTDIAGS
00476           SSB-SUB-TOPIC = 'MED'                                   ELTDIAGS
00477         PERFORM 1200-INSTITUTIONAL-MED-RTNE.                      ELTDIAGS
00478                                                                   ELTDIAGS
00479      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTDIAGS
00480           SSB-SUB-TOPIC = 'XRAY'                                  ELTDIAGS
00481         PERFORM 1400-INSTITUTIONAL-XRY-RTNE.                      ELTDIAGS
00482                                                                   ELTDIAGS
00483      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH) AND          ELTDIAGS
00484           SSB-SUB-TOPIC = 'LAB'                                   ELTDIAGS
00485         PERFORM 2000-PROFESSIONAL-LAB-RTNE.                       ELTDIAGS
00486                                                                   ELTDIAGS
00487      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH) AND          ELTDIAGS
00488           SSB-SUB-TOPIC = 'MED'                                   ELTDIAGS
00489         PERFORM 2200-PROFESSIONAL-MED-RTNE.                       ELTDIAGS
00490                                                                   ELTDIAGS
00491      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH) AND          ELTDIAGS
00492           SSB-SUB-TOPIC = 'XRAY'                                  ELTDIAGS
00493         PERFORM 2400-PROFESSIONAL-XRY-RTNE.                       ELTDIAGS
00494                                                                   ELTDIAGS
00495      IF (SSB-SUB-TOPIC = 'LAB'    OR                              ELTDIAGS
00496                           'MED'    OR                             ELTDIAGS
00497                           'XRAY')                AND              ELTDIAGS
00498           (SSB-PROV-CLASS-INST  OR                                ELTDIAGS
00499            SSB-PROV-CLASS-PROF  OR                                ELTDIAGS
00500            SSB-PROV-CLASS-BOTH)                                   ELTDIAGS
00501               CONTINUE                                            ELTDIAGS
00502      ELSE                                                         ELTDIAGS
00503         SET CIA-AB-UNDEF TO TRUE                                  ELTDIAGS
00504         EXEC CICS ABEND                                           ELTDIAGS
00505                   ABCODE(CIA-ABCODE)                              ELTDIAGS
00506         END-EXEC                                                  ELTDIAGS
00507      END-IF.                                                      ELTDIAGS
00508 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTDIAGS
00509       MOVE +0  TO  COF-NBR-HDR-LINES.                             ELTDIAGS
00510       MOVE +0  TO  COF-NBR-DTL-LINES.                             ELTDIAGS
00511       MOVE 'E' TO  COF-FUNCTION.                                  ELTDIAGS
00512                                                                   ELTDIAGS
00513      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDIAGS
00514      END-EXEC.                                                    ELTDIAGS
00515                                                                   ELTDIAGS
00516  0099-RETURN.                                                     ELTDIAGS
00517      GOBACK.                                                      ELTDIAGS
00518                                                                   ELTDIAGS
00519 /        I N S T I T U T I O N A L   L A B    R T N E             ELTDIAGS
00520 ***************************************************************** ELTDIAGS
00521 *        I N S T I T U T I O N A L   L A B    R T N E             ELTDIAGS
00522 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
00523 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
00524 ***************************************************************** ELTDIAGS
00525  1000-INSTITUTIONAL-LAB-RTNE SECTION.                             ELTDIAGS
00526      MOVE '1000'  TO  WS-PARA-ID.                                 ELTDIAGS
00527      MOVE WS-HDR-2-INST-LAB TO  COF-HDR-LINE(2).                  ELTDIAGS
00528                                                                   ELTDIAGS
00529      MOVE WS-INST-IP-CNT-LAB TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00530                                 WS-INST-COUNTER.                  ELTDIAGS
00531      PERFORM 6000-MOVE-IN-INST-IP-LAB                             ELTDIAGS
00532         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00533         UNTIL WS-SUB  >  WS-INST-IP-CNT-LAB.                      ELTDIAGS
00534                                                                   ELTDIAGS
00535      MOVE WS-INPATIENT-LAB-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
00536      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
00537      IF PVN-COVG-NONE                                             ELTDIAGS
00538         NEXT SENTENCE                                             ELTDIAGS
00539      ELSE                                                         ELTDIAGS
00540         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00541                                                                   ELTDIAGS
00542 *** INITIALIZE THE COUNTER TO START FRESH ***                     ELTDIAGS
00543      MOVE +0   TO WS-INST-COUNTER.                                ELTDIAGS
00544      MOVE WS-INST-OP-CNT-LAB TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00545                                 WS-INST-COUNTER.                  ELTDIAGS
00546      PERFORM 6100-MOVE-IN-INST-OP-LAB                             ELTDIAGS
00547         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00548         UNTIL WS-SUB  >  WS-INST-OP-CNT-LAB.                      ELTDIAGS
00549      MOVE WS-OUTPATIENT-LAB-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
00550 ************************************************************      ELTDIAGS
00551 *** NO COVERAGE FOR INST-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
00552 *** OTHERWISE A NEW PAGE WILL START WITH INST-OP INFO.            ELTDIAGS
00553 *** CHANGED BY REB ===> 12/11/87                                  ELTDIAGS
00554 ************************************************************      ELTDIAGS
00555      IF PVN-COVG-NONE                                             ELTDIAGS
00556         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
00557      ELSE                                                         ELTDIAGS
00558         MOVE WS-HDR-2-INST-LAB TO  COF-HDR-LINE(2)                ELTDIAGS
00559         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
00560                                                                   ELTDIAGS
00561 *******************************************                       ELTDIAGS
00562 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
00563 ** ANY COVERAGE FOR INST-OP BENEFITS                              ELTDIAGS
00564 *******************************************                       ELTDIAGS
00565      IF PVN-COVG-NONE                                             ELTDIAGS
00566         NEXT SENTENCE                                             ELTDIAGS
00567      ELSE                                                         ELTDIAGS
00568         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00569  1099-EXIT.            EXIT.                                      ELTDIAGS
00570                                                                   ELTDIAGS
00571 /        I N S T I T U T I O N A L   M E D    R T N E             ELTDIAGS
00572 ***************************************************************** ELTDIAGS
00573 *        I N S T I T U T I O N A L   M E D    R T N E             ELTDIAGS
00574 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
00575 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
00576 ***************************************************************** ELTDIAGS
00577  1200-INSTITUTIONAL-MED-RTNE SECTION.                             ELTDIAGS
00578      MOVE '1200'  TO  WS-PARA-ID.                                 ELTDIAGS
00579      MOVE WS-HDR-2-INST-MED TO  COF-HDR-LINE(2).                  ELTDIAGS
00580                                                                   ELTDIAGS
00581      MOVE WS-INST-IP-CNT-MED TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00582                                 WS-INST-COUNTER.                  ELTDIAGS
00583      PERFORM 6200-MOVE-IN-INST-IP-MED                             ELTDIAGS
00584         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00585         UNTIL WS-SUB  >  WS-INST-IP-CNT-MED.                      ELTDIAGS
00586                                                                   ELTDIAGS
00587      MOVE WS-INPATIENT-MED-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
00588      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
00589      IF PVN-COVG-NONE                                             ELTDIAGS
00590         NEXT SENTENCE                                             ELTDIAGS
00591      ELSE                                                         ELTDIAGS
00592         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00593                                                                   ELTDIAGS
00594 *** INITIALIZE THE COUNTER TO START FRESH ***                     ELTDIAGS
00595      MOVE +0   TO WS-INST-COUNTER.                                ELTDIAGS
00596      MOVE WS-INST-OP-CNT-LAB TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00597                                 WS-INST-COUNTER.                  ELTDIAGS
00598      PERFORM 6300-MOVE-IN-INST-OP-MED                             ELTDIAGS
00599         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00600         UNTIL WS-SUB  >  WS-INST-OP-CNT-LAB.                      ELTDIAGS
00601      MOVE WS-OUTPATIENT-MED-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
00602 ************************************************************      ELTDIAGS
00603 *** NO COVERAGE FOR INST-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
00604 *** OTHERWISE A NEW PAGE WILL START WITH INST-OP INFO.            ELTDIAGS
00605 *** CHANGED BY REB ===> 12/15/87                                  ELTDIAGS
00606 ************************************************************      ELTDIAGS
00607      IF PVN-COVG-NONE                                             ELTDIAGS
00608         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
00609      ELSE                                                         ELTDIAGS
00610         MOVE WS-HDR-2-INST-MED TO  COF-HDR-LINE(2)                ELTDIAGS
00611         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
00612                                                                   ELTDIAGS
00613 *******************************************                       ELTDIAGS
00614 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
00615 ** ANY COVERAGE FOR INST-OP BENEFITS                              ELTDIAGS
00616 *******************************************                       ELTDIAGS
00617      IF PVN-COVG-NONE                                             ELTDIAGS
00618         NEXT SENTENCE                                             ELTDIAGS
00619      ELSE                                                         ELTDIAGS
00620         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00621                                                                   ELTDIAGS
00622  1299-EXIT.            EXIT.                                      ELTDIAGS
00623                                                                   ELTDIAGS
00624 /        I N S T I T U T I O N A L   X R Y    R T N E             ELTDIAGS
00625 ***************************************************************** ELTDIAGS
00626 *        I N S T I T U T I O N A L   X R Y    R T N E             ELTDIAGS
00627 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
00628 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
00629 ***************************************************************** ELTDIAGS
00630  1400-INSTITUTIONAL-XRY-RTNE SECTION.                             ELTDIAGS
00631      MOVE '1400'  TO  WS-PARA-ID.                                 ELTDIAGS
00632      MOVE WS-HDR-2-INST-XRY TO  COF-HDR-LINE(2).                  ELTDIAGS
00633                                                                   ELTDIAGS
00634      MOVE WS-INST-IP-CNT-XRY TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00635                                 WS-INST-COUNTER.                  ELTDIAGS
00636      PERFORM 6400-MOVE-IN-INST-IP-XRY                             ELTDIAGS
00637         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00638         UNTIL WS-SUB  >  WS-INST-IP-CNT-XRY.                      ELTDIAGS
00639                                                                   ELTDIAGS
00640      MOVE WS-INPATIENT-XRY-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
00641      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
00642      IF PVN-COVG-NONE                                             ELTDIAGS
00643         NEXT SENTENCE                                             ELTDIAGS
00644      ELSE                                                         ELTDIAGS
00645         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00646                                                                   ELTDIAGS
00647 *** INITIALIZE THE COUNTER TO START FRESH ***                     ELTDIAGS
00648      MOVE +0   TO WS-INST-COUNTER.                                ELTDIAGS
00649      MOVE WS-INST-OP-CNT-XRY TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00650                                 WS-INST-COUNTER.                  ELTDIAGS
00651      PERFORM 6500-MOVE-IN-INST-OP-XRY                             ELTDIAGS
00652         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00653         UNTIL WS-SUB  >  WS-INST-OP-CNT-XRY.                      ELTDIAGS
00654      MOVE WS-OUTPATIENT-XRY-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
00655 ************************************************************      ELTDIAGS
00656 *** NO COVERAGE FOR INST-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
00657 *** OTHERWISE A NEW PAGE WILL START WITH INST-OP INFO.            ELTDIAGS
00658 *** CHANGED BY REB ===> 12/15/87                                  ELTDIAGS
00659 ************************************************************      ELTDIAGS
00660      IF PVN-COVG-NONE                                             ELTDIAGS
00661         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
00662      ELSE                                                         ELTDIAGS
00663         MOVE WS-HDR-2-INST-XRY TO  COF-HDR-LINE(2)                ELTDIAGS
00664         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
00665                                                                   ELTDIAGS
00666 *******************************************                       ELTDIAGS
00667 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
00668 ** ANY COVERAGE FOR INST-OP BENEFITS                              ELTDIAGS
00669 *******************************************                       ELTDIAGS
00670      IF PVN-COVG-NONE                                             ELTDIAGS
00671         NEXT SENTENCE                                             ELTDIAGS
00672      ELSE                                                         ELTDIAGS
00673         PERFORM 1600-INSTITUTIONAL-COMMON.                        ELTDIAGS
00674                                                                   ELTDIAGS
00675  1499-EXIT.            EXIT.                                      ELTDIAGS
00676 /        I N S T I T U T I O N A L   C O M M O N  R T N E         ELTDIAGS
00677  1600-INSTITUTIONAL-COMMON SECTION.                               ELTDIAGS
00678      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDIAGS
00679                                                                   ELTDIAGS
00680      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDIAGS
00681                    PSP-PROVN-PRICING-METHD,                       ELTDIAGS
00682                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDIAGS
00683                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDIAGS
00684                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDIAGS
00685                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDIAGS
00686                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDIAGS
00687                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDIAGS
00688                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDIAGS
00689                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDIAGS
00690                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDIAGS
00691                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDIAGS
00692                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTDIAGS
00693                    PSA-DAYS-RDCN-RAT-BASIC-APL,                   ELTDIAGS
00694                    PSA-DAYS-RDCN-RAT-BASIC-BASE                   ELTDIAGS
00695                    PSA-DAYS-RDCN-RAT-SEC-APL,                     ELTDIAGS
00696                    PSA-DAYS-RDCN-RAT-SEC-BASE,                    ELTDIAGS
00697                    PSB-PROF-CHRG-HSP-CLM.                         ELTDIAGS
00698                                                                   ELTDIAGS
00699      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDIAGS
00700      END-EXEC.                                                    ELTDIAGS
00701                                                                   ELTDIAGS
00702      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDIAGS
00703      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
00704          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDIAGS
00705                                                                   ELTDIAGS
00706      PERFORM 1630-FIND-FIRST-NONZERO                              ELTDIAGS
00707         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDIAGS
00708         UNTIL WS-SUB  >  WS-INST-COUNTER.                         ELTDIAGS
00709                                                                   ELTDIAGS
00710      GO TO 1699-EXIT.                                             ELTDIAGS
00711  1630-FIND-FIRST-NONZERO.                                         ELTDIAGS
00712      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDIAGS
00713      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDIAGS
00714         NEXT SENTENCE                                             ELTDIAGS
00715      ELSE                                                         ELTDIAGS
00716         PERFORM 1640-BUILD-SCREEN-LINES.                          ELTDIAGS
00717                                                                   ELTDIAGS
00718  1640-BUILD-SCREEN-LINES.                                         ELTDIAGS
00719      MOVE '1640'  TO  WS-PARA-ID.                                 ELTDIAGS
00720                                                                   ELTDIAGS
00721      SET PLT-INDEX1 TO                                            ELTDIAGS
00722         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDIAGS
00723                                                                   ELTDIAGS
00724      IF WS-NOT-FIRST-TIME                                         ELTDIAGS
00725         MOVE 'P' TO COF-FUNCTION                                  ELTDIAGS
00726 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDIAGS
00727 ******** REB ===> 12/15/87.                                       ELTDIAGS
00728 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDIAGS
00729         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDIAGS
00730         EXEC CICS LINK                                            ELTDIAGS
00731                   PROGRAM('ELUOUTPT')                             ELTDIAGS
00732                   COMMAREA(DFHCOMMAREA)                           ELTDIAGS
00733         END-EXEC                                                  ELTDIAGS
00734      ELSE                                                         ELTDIAGS
00735        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDIAGS
00736                                                                   ELTDIAGS
00737      MOVE +1  TO  WS-CIA.                                         ELTDIAGS
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDIAGS
00739         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDIAGS
00740            SET PLT-INDEX2  TO  2                                  ELTDIAGS
00741         ELSE                                                      ELTDIAGS
00742            PERFORM 1690-PROBLEM-WITH-INDICES                      ELTDIAGS
00743            GO TO 1699-EXIT                                        ELTDIAGS
00744      ELSE                                                         ELTDIAGS
00745         SET PLT-INDEX2  TO  1.                                    ELTDIAGS
00746      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDIAGS
00747                                                                   ELTDIAGS
00748      ADD +1                 TO WS-CIA.                            ELTDIAGS
00749      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDIAGS
00750      ADD +1                 TO WS-CIA.                            ELTDIAGS
00751                                                                   ELTDIAGS
00752      MOVE WS-NO  TO  WS-DISPLAY-B-FORMAT-TEXT.                    ELTDIAGS
00753                                                                   ELTDIAGS
00754      PERFORM 1650-ZERO-ALL-WITH-SAME-NO                           ELTDIAGS
00755         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDIAGS
00756         UNTIL WS-SUB2  >  WS-INST-COUNTER.                        ELTDIAGS
00757                                                                   ELTDIAGS
00758      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDIAGS
00759               NOT = '0' AND NOT = LOW-VALUES                      ELTDIAGS
00760           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDIAGS
00761                                                                   ELTDIAGS
00762      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDIAGS
00763      ADD  +1                   TO  WS-CIA.                        ELTDIAGS
00764      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDIAGS
00765      ADD  +1                   TO  WS-CIA.                        ELTDIAGS
00766                                                                   ELTDIAGS
00767      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
00768         SET  PLT-INDEX2           TO  1                           ELTDIAGS
00769         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDIAGS
00770                                                                   ELTDIAGS
00771      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
00772         SET  PLT-INDEX2             TO  2                         ELTDIAGS
00773         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDIAGS
00774                                                                   ELTDIAGS
00775      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
00776         SET  PLT-INDEX2           TO  1                           ELTDIAGS
00777         PERFORM 4500-TRANS-OTHER-RESP-IND                         ELTDIAGS
00778      ELSE                                                         ELTDIAGS
00779         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDIAGS
00780            SET  PLT-INDEX2             TO  2                      ELTDIAGS
00781           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDIAGS
00782                                                                   ELTDIAGS
00783      PERFORM 4250-PROF-CHGR-HSP-CLM.                              ELTDIAGS
00784                                                                   ELTDIAGS
00785      IF SSB-SUB-TOPIC = 'MED'                                     ELTDIAGS
00786       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTDIAGS
00787        SET  PLT-INDEX2          TO  1                             ELTDIAGS
00788        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                    ELTDIAGS
00789         IF (PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
00790                     NOT = ZEROS  AND                              ELTDIAGS
00791            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
00792                      NOT = ZEROS)                                 ELTDIAGS
00793         OR (PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
00794                     NOT = ZEROS  AND                              ELTDIAGS
00795            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
00796                      NOT = ZEROS)                                 ELTDIAGS
00797                        ADD +1  TO WS-CIA                          ELTDIAGS
00798                        MOVE WS-DAYS-REDUCED TO                    ELTDIAGS
00799                               COF-DTL-LINE(WS-CIA)                ELTDIAGS
00800                        ADD +1  TO WS-CIA.                         ELTDIAGS
00801                                                                   ELTDIAGS
00802      IF SSB-SUB-TOPIC = 'MED'                                     ELTDIAGS
00803       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTDIAGS
00804        SET  PLT-INDEX2          TO  1                             ELTDIAGS
00805        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                    ELTDIAGS
00806         IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
00807                     NOT = ZEROS  AND                              ELTDIAGS
00808            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
00809                      NOT = ZEROS                                  ELTDIAGS
00810             MOVE                                                  ELTDIAGS
00811              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTDIAGS
00812                    TO WS-DTL-DAYS-REDUCED-APL                     ELTDIAGS
00813             MOVE                                                  ELTDIAGS
00814              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTDIAGS
00815                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTDIAGS
00816             MOVE SPACES          TO TCAR-FROM-AREA                ELTDIAGS
00817             STRING WS-BASIC-LIT,                                  ELTDIAGS
00818                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTDIAGS
00819                    WS-FOR, ' '                                    ELTDIAGS
00820                    WS-DTL-DAYS-REDUCED-BASE,                      ELTDIAGS
00821                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTDIAGS
00822             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDIAGS
00823             MOVE +01             TO TCAR-OUTPUT-FIELD-COUNT       ELTDIAGS
00824             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTDIAGS
00825             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDIAGS
00826             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTDIAGS
00827             PERFORM 3000-OUTPUT-TEXT.                             ELTDIAGS
00828                                                                   ELTDIAGS
00829      IF SSB-SUB-TOPIC = 'MED'                                     ELTDIAGS
00830       IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTDIAGS
00831        SET  PLT-INDEX2          TO  1                             ELTDIAGS
00832        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                    ELTDIAGS
00833         IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
00834                     NOT = ZEROS  AND                              ELTDIAGS
00835            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
00836                      NOT = ZEROS                                  ELTDIAGS
00837             MOVE                                                  ELTDIAGS
00838              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTDIAGS
00839                    TO WS-DTL-DAYS-REDUCED-APL                     ELTDIAGS
00840             MOVE                                                  ELTDIAGS
00841              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
00842                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTDIAGS
00843             MOVE SPACES          TO TCAR-FROM-AREA                ELTDIAGS
00844             STRING WS-SECONDARY-LIT,                              ELTDIAGS
00845                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTDIAGS
00846                    WS-FOR, ' '                                    ELTDIAGS
00847                    WS-DTL-DAYS-REDUCED-BASE,                      ELTDIAGS
00848                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTDIAGS
00849             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDIAGS
00850             MOVE +01             TO TCAR-OUTPUT-FIELD-COUNT       ELTDIAGS
00851             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTDIAGS
00852             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDIAGS
00853             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTDIAGS
00854             PERFORM 3000-OUTPUT-TEXT.                             ELTDIAGS
00855                                                                   ELTDIAGS
00856      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
00857         SET  PLT-INDEX2          TO  2                            ELTDIAGS
00858         PERFORM 4300-SPILLOVER-COINS                              ELTDIAGS
00859         PERFORM 4400-SPILLOVER-DEDUCT                             ELTDIAGS
00860         PERFORM 3000-OUTPUT-TEXT.                                 ELTDIAGS
00861                                                                   ELTDIAGS
00862      PERFORM 4600-SCAN-TAB.                                       ELTDIAGS
00863      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDIAGS
00864                                                                   ELTDIAGS
00865  1650-ZERO-ALL-WITH-SAME-NO.                                      ELTDIAGS
00866      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDIAGS
00867                                                                   ELTDIAGS
00868      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)         =  WS-SUB3   ELTDIAGS
00869          IF PVN-BEN-FMT (PVN-BEN-PROVN-IDX) = 'B'                 ELTDIAGS
00870              MOVE WS-YES  TO  WS-DISPLAY-B-FORMAT-TEXT.           ELTDIAGS
00871                                                                   ELTDIAGS
00872      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDIAGS
00873         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDIAGS
00874         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDIAGS
00875         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDIAGS
00876                                                   CMF-CODE-VALUE  ELTDIAGS
00877         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDIAGS
00878         STRING CMF-DESCR-LINE(1) ' '                              ELTDIAGS
00879                CMF-DESCR-LINE(2) ' '                              ELTDIAGS
00880                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
00881         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDIAGS
00882                                                                   ELTDIAGS
00883         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDIAGS
00884         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDIAGS
00885         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDIAGS
00886         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDIAGS
00887         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDIAGS
00888         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDIAGS
00889         IF WS-CIA  <  20                                          ELTDIAGS
00890            ADD +1  TO  WS-CIA                                     ELTDIAGS
00891            MOVE ZERO  TO                                          ELTDIAGS
00892                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDIAGS
00893         ELSE                                                      ELTDIAGS
00894            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDIAGS
00895                COMMAREA(DFHCOMMAREA)                              ELTDIAGS
00896            END-EXEC                                               ELTDIAGS
00897            MOVE +1  TO  WS-CIA                                    ELTDIAGS
00898            MOVE ZERO  TO                                          ELTDIAGS
00899                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDIAGS
00900                                                                   ELTDIAGS
00901  1690-PROBLEM-WITH-INDICES.                                       ELTDIAGS
00902                                                                   ELTDIAGS
00903      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDIAGS
00904      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDIAGS
00905                                                                   ELTDIAGS
00906      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDIAGS
00907      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDIAGS
00908                                                                   ELTDIAGS
00909      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDIAGS
00910      END-EXEC.                                                    ELTDIAGS
00911                                                                   ELTDIAGS
00912  1699-EXIT.   EXIT.                                               ELTDIAGS
00913 /        P R O F E S S I O N A L   L A B   R T N E                ELTDIAGS
00914 ***************************************************************** ELTDIAGS
00915 *        P R O F E S S I O N A L   L A B   R T N E                ELTDIAGS
00916 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
00917 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
00918 ***************************************************************** ELTDIAGS
00919  2000-PROFESSIONAL-LAB-RTNE SECTION.                              ELTDIAGS
00920      MOVE '2000'  TO  WS-PARA-ID.                                 ELTDIAGS
00921      MOVE WS-HDR-2-PROF-LAB TO  COF-HDR-LINE(2).                  ELTDIAGS
00922                                                                   ELTDIAGS
00923      MOVE WS-PROF-IP-CNT-LAB TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00924                                 WS-PROF-COUNTER.                  ELTDIAGS
00925      PERFORM 7000-MOVE-IN-PROF-IP-LAB                             ELTDIAGS
00926         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00927         UNTIL WS-SUB  >  WS-PROF-IP-CNT-LAB.                      ELTDIAGS
00928                                                                   ELTDIAGS
00929      MOVE WS-INPATIENT-LAB-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
00930      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
00931      IF PVN-COVG-NONE                                             ELTDIAGS
00932         NEXT SENTENCE                                             ELTDIAGS
00933      ELSE                                                         ELTDIAGS
00934         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
00935                                                                   ELTDIAGS
00936      MOVE +0  TO WS-PROF-COUNTER.                                 ELTDIAGS
00937      MOVE WS-PROF-OP-CNT-LAB TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00938                                 WS-PROF-COUNTER.                  ELTDIAGS
00939      PERFORM 7100-MOVE-IN-PROF-OP-LAB                             ELTDIAGS
00940         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00941         UNTIL WS-SUB  >  WS-PROF-OP-CNT-LAB.                      ELTDIAGS
00942      MOVE WS-OUTPATIENT-LAB-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
00943 ************************************************************      ELTDIAGS
00944 *** NO COVERAGE FOR PROF-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
00945 *** OTHERWISE A NEW PAGE WILL START WITH PROF-OP INFO.            ELTDIAGS
00946 *** CHANGED BY REB ===> 12/11/87                                  ELTDIAGS
00947 ************************************************************      ELTDIAGS
00948      IF PVN-COVG-NONE                                             ELTDIAGS
00949         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
00950      ELSE                                                         ELTDIAGS
00951         MOVE WS-HDR-2-PROF-LAB TO  COF-HDR-LINE(2)                ELTDIAGS
00952         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
00953 *******************************************                       ELTDIAGS
00954 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
00955 ** ANY COVERAGE FOR PROF-OP BENEFITS                              ELTDIAGS
00956 *******************************************                       ELTDIAGS
00957      IF PVN-COVG-NONE                                             ELTDIAGS
00958         NEXT SENTENCE                                             ELTDIAGS
00959      ELSE                                                         ELTDIAGS
00960         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
00961                                                                   ELTDIAGS
00962  2099-EXIT.            EXIT.                                      ELTDIAGS
00963                                                                   ELTDIAGS
00964 /        P R O F E S S I O N A L   M E D    R T N E               ELTDIAGS
00965 ***************************************************************** ELTDIAGS
00966 *        P R O F E S S I O N A L    M E D    R T N E              ELTDIAGS
00967 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
00968 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
00969 ***************************************************************** ELTDIAGS
00970  2200-PROFESSIONAL-MED-RTNE SECTION.                              ELTDIAGS
00971      MOVE '2200'  TO  WS-PARA-ID.                                 ELTDIAGS
00972      MOVE WS-HDR-2-PROF-MED TO  COF-HDR-LINE(2).                  ELTDIAGS
00973                                                                   ELTDIAGS
00974      MOVE WS-PROF-IP-CNT-MED TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00975                                 WS-PROF-COUNTER.                  ELTDIAGS
00976      PERFORM 7200-MOVE-IN-PROF-IP-MED                             ELTDIAGS
00977         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00978         UNTIL WS-SUB  >  WS-PROF-IP-CNT-MED.                      ELTDIAGS
00979                                                                   ELTDIAGS
00980      MOVE WS-INPATIENT-MED-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
00981      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
00982      IF PVN-COVG-NONE                                             ELTDIAGS
00983         NEXT SENTENCE                                             ELTDIAGS
00984      ELSE                                                         ELTDIAGS
00985         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
00986                                                                   ELTDIAGS
00987      MOVE +0  TO WS-PROF-COUNTER.                                 ELTDIAGS
00988      MOVE WS-PROF-OP-CNT-MED TO PVN-NBR-BEN-PROVN,                ELTDIAGS
00989                                 WS-PROF-COUNTER.                  ELTDIAGS
00990      PERFORM 7300-MOVE-IN-PROF-OP-MED                             ELTDIAGS
00991         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
00992         UNTIL WS-SUB  >  WS-PROF-OP-CNT-MED.                      ELTDIAGS
00993      MOVE WS-OUTPATIENT-MED-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
00994 ************************************************************      ELTDIAGS
00995 *** NO COVERAGE FOR PROF-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
00996 *** OTHERWISE A NEW PAGE WILL START WITH PROF-OP INFO.            ELTDIAGS
00997 *** CHANGED BY REB ===> 12/15/87                                  ELTDIAGS
00998 ************************************************************      ELTDIAGS
00999      IF PVN-COVG-NONE                                             ELTDIAGS
01000         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
01001      ELSE                                                         ELTDIAGS
01002         MOVE WS-HDR-2-PROF-MED TO  COF-HDR-LINE(2)                ELTDIAGS
01003         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
01004 *******************************************                       ELTDIAGS
01005 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
01006 ** ANY COVERAGE FOR PROF-OP BENEFITS                              ELTDIAGS
01007 *******************************************                       ELTDIAGS
01008      IF PVN-COVG-NONE                                             ELTDIAGS
01009         NEXT SENTENCE                                             ELTDIAGS
01010      ELSE                                                         ELTDIAGS
01011         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
01012                                                                   ELTDIAGS
01013  2299-EXIT.            EXIT.                                      ELTDIAGS
01014                                                                   ELTDIAGS
01015 /        P R O F E S S I O N A L   X R Y    R T N E               ELTDIAGS
01016 ***************************************************************** ELTDIAGS
01017 *        P R O F E S S I O N A LL   X R Y    R T N E              ELTDIAGS
01018 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDIAGS
01019 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDIAGS
01020 ***************************************************************** ELTDIAGS
01021  2400-PROFESSIONAL-XRY-RTNE SECTION.                              ELTDIAGS
01022      MOVE '2400'  TO  WS-PARA-ID.                                 ELTDIAGS
01023      MOVE WS-HDR-2-PROF-XRY TO  COF-HDR-LINE(2).                  ELTDIAGS
01024                                                                   ELTDIAGS
01025      MOVE WS-PROF-IP-CNT-XRY TO PVN-NBR-BEN-PROVN,                ELTDIAGS
01026                                 WS-PROF-COUNTER.                  ELTDIAGS
01027      PERFORM 7400-MOVE-IN-PROF-IP-XRY                             ELTDIAGS
01028         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
01029         UNTIL WS-SUB  >  WS-PROF-IP-CNT-XRY.                      ELTDIAGS
01030                                                                   ELTDIAGS
01031      MOVE WS-INPATIENT-XRY-ARE TO SSB-TOPIC-PHRASE.               ELTDIAGS
01032      PERFORM 8000-CALL-COVERAGE.                                  ELTDIAGS
01033      IF PVN-COVG-NONE                                             ELTDIAGS
01034         NEXT SENTENCE                                             ELTDIAGS
01035      ELSE                                                         ELTDIAGS
01036         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
01037                                                                   ELTDIAGS
01038      MOVE +0  TO WS-PROF-COUNTER.                                 ELTDIAGS
01039      MOVE WS-PROF-OP-CNT-XRY TO PVN-NBR-BEN-PROVN,                ELTDIAGS
01040                                 WS-PROF-COUNTER.                  ELTDIAGS
01041      PERFORM 7500-MOVE-IN-PROF-OP-XRY                             ELTDIAGS
01042         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDIAGS
01043         UNTIL WS-SUB  >  WS-PROF-OP-CNT-XRY.                      ELTDIAGS
01044      MOVE WS-OUTPATIENT-XRY-ARE TO SSB-TOPIC-PHRASE.              ELTDIAGS
01045 ************************************************************      ELTDIAGS
01046 *** NO COVERAGE FOR PROF-IP BENEFITS WE WILL CONTINUE PAGE.       ELTDIAGS
01047 *** OTHERWISE A NEW PAGE WILL START WITH PROF-OP INFO.            ELTDIAGS
01048 *** CHANGED BY REB ===> 12/15/87                                  ELTDIAGS
01049 ************************************************************      ELTDIAGS
01050      IF PVN-COVG-NONE                                             ELTDIAGS
01051         PERFORM 8300-DISPLAY-COVERAGE                             ELTDIAGS
01052      ELSE                                                         ELTDIAGS
01053         MOVE WS-HDR-2-PROF-XRY TO  COF-HDR-LINE(2)                ELTDIAGS
01054         PERFORM 8000-CALL-COVERAGE.                               ELTDIAGS
01055 *******************************************                       ELTDIAGS
01056 ** THIS CHECK IS TO VERIFY IF THERE IS                            ELTDIAGS
01057 ** ANY COVERAGE FOR PROF-OP BENEFITS                              ELTDIAGS
01058 *******************************************                       ELTDIAGS
01059      IF PVN-COVG-NONE                                             ELTDIAGS
01060         NEXT SENTENCE                                             ELTDIAGS
01061      ELSE                                                         ELTDIAGS
01062         PERFORM 2600-PROFESSIONAL-COMMON.                         ELTDIAGS
01063                                                                   ELTDIAGS
01064  2499-EXIT.            EXIT.                                      ELTDIAGS
01065 /                                                                 ELTDIAGS
01066  2600-PROFESSIONAL-COMMON SECTION.                                ELTDIAGS
01067      MOVE '2600'  TO WS-PARA-ID.                                  ELTDIAGS
01068                                                                   ELTDIAGS
01069      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDIAGS
01070                                                                   ELTDIAGS
01071      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDIAGS
01072                    PSP-PROVN-PRICING-METHD,                       ELTDIAGS
01073                    PSP-TRANSF-OTHER-RESP-IND,                     ELTDIAGS
01074                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTDIAGS
01075                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTDIAGS
01076                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTDIAGS
01077                    PSP-SPILL-OVER-DED-APL-IND,                    ELTDIAGS
01078                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTDIAGS
01079                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTDIAGS
01080                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTDIAGS
01081                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTDIAGS
01082                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTDIAGS
01083                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTDIAGS
01084                    PSE-BEN-SCOPE-ID.                              ELTDIAGS
01085                                                                   ELTDIAGS
01086      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDIAGS
01087      END-EXEC.                                                    ELTDIAGS
01088                                                                   ELTDIAGS
01089      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDIAGS
01090      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
01091          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDIAGS
01092                                                                   ELTDIAGS
01093      PERFORM 2630-FIND-FIRST-NONZERO                              ELTDIAGS
01094         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDIAGS
01095         UNTIL WS-SUB  >  WS-PROF-COUNTER.                         ELTDIAGS
01096                                                                   ELTDIAGS
01097      GO TO 2699-EXIT.                                             ELTDIAGS
01098  2630-FIND-FIRST-NONZERO.                                         ELTDIAGS
01099      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDIAGS
01100      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  ZERO       ELTDIAGS
01101         NEXT SENTENCE                                             ELTDIAGS
01102      ELSE                                                         ELTDIAGS
01103         PERFORM 2640-BUILD-SCREEN-LINES.                          ELTDIAGS
01104                                                                   ELTDIAGS
01105  2640-BUILD-SCREEN-LINES.                                         ELTDIAGS
01106      MOVE '2640'  TO  WS-PARA-ID.                                 ELTDIAGS
01107                                                                   ELTDIAGS
01108      SET PLT-INDEX1 TO                                            ELTDIAGS
01109         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDIAGS
01110                                                                   ELTDIAGS
01111      IF WS-NOT-FIRST-TIME                                         ELTDIAGS
01112         MOVE 'P' TO COF-FUNCTION                                  ELTDIAGS
01113 ******** I COMMENTED THIS MOVE TO SEE IF THE HEADINGS WILL SHOW.  ELTDIAGS
01114 ******** REB ===> 12/15/87.                                       ELTDIAGS
01115 ********MOVE +2     TO COF-NBR-HDR-LINES                          ELTDIAGS
01116         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDIAGS
01117         EXEC CICS LINK                                            ELTDIAGS
01118                   PROGRAM('ELUOUTPT')                             ELTDIAGS
01119                   COMMAREA(DFHCOMMAREA)                           ELTDIAGS
01120         END-EXEC                                                  ELTDIAGS
01121      ELSE                                                         ELTDIAGS
01122        MOVE WS-NO TO WS-FIRSTTIME-IND.                            ELTDIAGS
01123                                                                   ELTDIAGS
01124      MOVE +1  TO  WS-CIA.                                         ELTDIAGS
01125      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDIAGS
01126         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDIAGS
01127            SET PLT-INDEX2  TO  2                                  ELTDIAGS
01128         ELSE                                                      ELTDIAGS
01129            PERFORM 2690-PROBLEM-WITH-INDICES                      ELTDIAGS
01130            GO TO 2699-EXIT                                        ELTDIAGS
01131      ELSE                                                         ELTDIAGS
01132         SET PLT-INDEX2  TO  1.                                    ELTDIAGS
01133      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTDIAGS
01134                                                                   ELTDIAGS
01135      ADD +1                 TO WS-CIA.                            ELTDIAGS
01136      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDIAGS
01137      ADD +1                 TO WS-CIA.                            ELTDIAGS
01138      PERFORM 2650-ZERO-ALL-WITH-SAME-NO                           ELTDIAGS
01139         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTDIAGS
01140         UNTIL WS-SUB2  >  WS-PROF-COUNTER.                        ELTDIAGS
01141                                                                   ELTDIAGS
01142      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTDIAGS
01143               NOT = '0' AND NOT = LOW-VALUES                      ELTDIAGS
01144           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTDIAGS
01145                                                                   ELTDIAGS
01146      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDIAGS
01147      MOVE WS-NO      TO  WS-DISPLAY-PAYMNT-BASED-TEXT.            ELTDIAGS
01148                                                                   ELTDIAGS
01149      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
01150       SET  PLT-INDEX2       TO  1                                 ELTDIAGS
01151       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDIAGS
01152        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDIAGS
01153                 '0000' AND NOT = '00  '                           ELTDIAGS
01154              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDIAGS
01155                                                                   ELTDIAGS
01156      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
01157       SET PLT-INDEX2        TO 2                                  ELTDIAGS
01158       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDIAGS
01159        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDIAGS
01160                 '0000' AND NOT = '00  '                           ELTDIAGS
01161              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTDIAGS
01162                                                                   ELTDIAGS
01163      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTDIAGS
01164          ADD  +1               TO  WS-CIA                         ELTDIAGS
01165          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTDIAGS
01166          ADD  +1               TO  WS-CIA.                        ELTDIAGS
01167                                                                   ELTDIAGS
01168      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
01169       SET  PLT-INDEX2       TO  1                                 ELTDIAGS
01170       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDIAGS
01171        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDIAGS
01172                 '0000' AND NOT = '00  '                           ELTDIAGS
01173         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDIAGS
01174         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDIAGS
01175                               CMF-CODE-VALUE                      ELTDIAGS
01176         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDIAGS
01177         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDIAGS
01178         STRING CMF-DESCR-LINE(1) ' '                              ELTDIAGS
01179                CMF-DESCR-LINE(2) ' '                              ELTDIAGS
01180                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
01181         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDIAGS
01182                                                                   ELTDIAGS
01183         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDIAGS
01184         MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDIAGS
01185         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDIAGS
01186         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDIAGS
01187         MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                     ELTDIAGS
01188         MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)             ELTDIAGS
01189         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTDIAGS
01190            ADD +1                TO WS-CIA                        ELTDIAGS
01191            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDIAGS
01192            PERFORM 3000-OUTPUT-TEXT                               ELTDIAGS
01193         ELSE                                                      ELTDIAGS
01194          PERFORM 3000-OUTPUT-TEXT.                                ELTDIAGS
01195                                                                   ELTDIAGS
01196      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
01197       SET PLT-INDEX2        TO 2                                  ELTDIAGS
01198       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTDIAGS
01199        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT =          ELTDIAGS
01200                 '0000' AND NOT = '00  '                           ELTDIAGS
01201         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTDIAGS
01202         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTDIAGS
01203                               CMF-CODE-VALUE                      ELTDIAGS
01204         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTDIAGS
01205         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDIAGS
01206         STRING CMF-DESCR-LINE(1) ' '                              ELTDIAGS
01207                CMF-DESCR-LINE(2) ' '                              ELTDIAGS
01208                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
01209         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDIAGS
01210                                                                   ELTDIAGS
01211         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDIAGS
01212         MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDIAGS
01213         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDIAGS
01214         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDIAGS
01215         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL              ELTDIAGS
01216         MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)             ELTDIAGS
01217         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTDIAGS
01218            ADD +1                TO WS-CIA                        ELTDIAGS
01219            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDIAGS
01220            PERFORM 3000-OUTPUT-TEXT                               ELTDIAGS
01221         ELSE                                                      ELTDIAGS
01222          PERFORM 3000-OUTPUT-TEXT.                                ELTDIAGS
01223                                                                   ELTDIAGS
01224      ADD  +1                   TO  WS-CIA.                        ELTDIAGS
01225      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDIAGS
01226      ADD  +1                   TO  WS-CIA.                        ELTDIAGS
01227                                                                   ELTDIAGS
01228      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
01229         SET  PLT-INDEX2           TO  1                           ELTDIAGS
01230         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTDIAGS
01231                                                                   ELTDIAGS
01232      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
01233         SET  PLT-INDEX2             TO  2                         ELTDIAGS
01234         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTDIAGS
01235                                                                   ELTDIAGS
01236      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDIAGS
01237         SET  PLT-INDEX2           TO  1                           ELTDIAGS
01238         PERFORM 4500-TRANS-OTHER-RESP-IND                         ELTDIAGS
01239      ELSE                                                         ELTDIAGS
01240         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTDIAGS
01241            SET  PLT-INDEX2             TO  2                      ELTDIAGS
01242           PERFORM 4500-TRANS-OTHER-RESP-IND.                      ELTDIAGS
01243                                                                   ELTDIAGS
01244      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDIAGS
01245         SET  PLT-INDEX2          TO  2                            ELTDIAGS
01246         PERFORM 4300-SPILLOVER-COINS                              ELTDIAGS
01247         PERFORM 4400-SPILLOVER-DEDUCT                             ELTDIAGS
01248         PERFORM 3000-OUTPUT-TEXT.                                 ELTDIAGS
01249                                                                   ELTDIAGS
01250      PERFORM 4600-SCAN-TAB.                                       ELTDIAGS
01251      PERFORM 4675-PAY-CONSID-TEXT.                                ELTDIAGS
01252                                                                   ELTDIAGS
01253  2650-ZERO-ALL-WITH-SAME-NO.                                      ELTDIAGS
01254      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDIAGS
01255                                                                   ELTDIAGS
01256      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTDIAGS
01257         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDIAGS
01258         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDIAGS
01259         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDIAGS
01260                                                   CMF-CODE-VALUE  ELTDIAGS
01261         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDIAGS
01262         STRING CMF-DESCR-LINE(1) ' '                              ELTDIAGS
01263                CMF-DESCR-LINE(2) ' '                              ELTDIAGS
01264                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
01265         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDIAGS
01266                                                                   ELTDIAGS
01267         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTDIAGS
01268         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTDIAGS
01269         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTDIAGS
01270         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDIAGS
01271         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDIAGS
01272         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDIAGS
01273         IF WS-CIA  <  20                                          ELTDIAGS
01274            ADD +1  TO  WS-CIA                                     ELTDIAGS
01275            MOVE ZERO  TO                                          ELTDIAGS
01276                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDIAGS
01277         ELSE                                                      ELTDIAGS
01278            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDIAGS
01279                COMMAREA(DFHCOMMAREA)                              ELTDIAGS
01280            END-EXEC                                               ELTDIAGS
01281            MOVE +1  TO  WS-CIA                                    ELTDIAGS
01282            MOVE ZERO  TO                                          ELTDIAGS
01283                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDIAGS
01284                                                                   ELTDIAGS
01285  2690-PROBLEM-WITH-INDICES.                                       ELTDIAGS
01286                                                                   ELTDIAGS
01287      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDIAGS
01288      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDIAGS
01289                                                                   ELTDIAGS
01290      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDIAGS
01291      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDIAGS
01292                                                                   ELTDIAGS
01293      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDIAGS
01294      END-EXEC.                                                    ELTDIAGS
01295                                                                   ELTDIAGS
01296  2699-EXIT.   EXIT.                                               ELTDIAGS
01297                                                                   ELTDIAGS
01298 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTDIAGS
01299  3000-OUTPUT-TEXT SECTION.                                        ELTDIAGS
01300      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDIAGS
01301      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDIAGS
01302      MOVE ' '  TO  COF-FUNCTION.                                  ELTDIAGS
01303                                                                   ELTDIAGS
01304      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDIAGS
01305                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01306      END-EXEC.                                                    ELTDIAGS
01307      MOVE +1   TO WS-CIA.                                         ELTDIAGS
01308  3000-EXIT.  EXIT.                                                ELTDIAGS
01309 /                                                                 ELTDIAGS
01310  4000-PLACE-OF-TREATMENT SECTION.                                 ELTDIAGS
01311      ADD +1     TO  WS-CIA.                                       ELTDIAGS
01312      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDIAGS
01313      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTDIAGS
01314      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01315                                               TO  CMF-CODE-VALUE. ELTDIAGS
01316      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01317      STRING WS-SERVICES-RENDERED ' '                              ELTDIAGS
01318             CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01319             CMF-DESCR-LINE(2) ' '                                 ELTDIAGS
01320                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDIAGS
01321      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01322                                                                   ELTDIAGS
01323      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDIAGS
01324      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDIAGS
01325      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDIAGS
01326      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01327      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).               ELTDIAGS
01328      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01329         ADD +1                TO WS-CIA                           ELTDIAGS
01330         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTDIAGS
01331      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01332  4000-EXIT.  EXIT.                                                ELTDIAGS
01333 /                                                                 ELTDIAGS
01334  4100-PAYABLE-AS-BASIC SECTION.                                   ELTDIAGS
01335      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDIAGS
01336          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDIAGS
01337          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTDIAGS
01338          GO TO 4100-OUTPUT-TEXT.                                  ELTDIAGS
01339      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTDIAGS
01340      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTDIAGS
01341      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTDIAGS
01342                                                CMF-CODE-VALUE     ELTDIAGS
01343      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01344      STRING CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01345             CMF-DESCR-LINE(2)                                     ELTDIAGS
01346                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTDIAGS
01347      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01348      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTDIAGS
01349      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTDIAGS
01350      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTDIAGS
01351      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01352      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTDIAGS
01353                                         =  ZEROS                  ELTDIAGS
01354       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTDIAGS
01355                                         =  ZEROS                  ELTDIAGS
01356                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTDIAGS
01357                 MOVE WS-BASIC               TO                    ELTDIAGS
01358                         COF-DTL-LINE(WS-CIA)                      ELTDIAGS
01359       ELSE                                                        ELTDIAGS
01360          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTDIAGS
01361          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDIAGS
01362                                        TO  WS-DTL-PERCENT         ELTDIAGS
01363          MOVE SPACES          TO TCAR-FROM-AREA                   ELTDIAGS
01364          STRING WS-DTL-PP,                                        ELTDIAGS
01365                 WS-DTL-PERCENT,                                   ELTDIAGS
01366                 WS-PERCENT,                                       ELTDIAGS
01367                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
01368          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTDIAGS
01369          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDIAGS
01370          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDIAGS
01371          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDIAGS
01372          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDIAGS
01373          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTDIAGS
01374          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTDIAGS
01375      ELSE                                                         ELTDIAGS
01376        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTDIAGS
01377        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
01378                                     TO WS-DTL-PERCENT             ELTDIAGS
01379        MOVE SPACES          TO TCAR-FROM-AREA                     ELTDIAGS
01380        STRING WS-DTL-PP,                                          ELTDIAGS
01381               WS-DTL-PERCENT,                                     ELTDIAGS
01382               WS-PERCENT,                                         ELTDIAGS
01383                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDIAGS
01384        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTDIAGS
01385        MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT            ELTDIAGS
01386        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTDIAGS
01387        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTDIAGS
01388        PERFORM TCPR-000-TEXT-UNSTRING                             ELTDIAGS
01389        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTDIAGS
01390        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTDIAGS
01391  4100-OUTPUT-TEXT.                                                ELTDIAGS
01392      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01393            ADD +1                TO WS-CIA                        ELTDIAGS
01394            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDIAGS
01395            PERFORM 3000-OUTPUT-TEXT                               ELTDIAGS
01396      ELSE                                                         ELTDIAGS
01397         PERFORM 3000-OUTPUT-TEXT.                                 ELTDIAGS
01398  4100-EXIT.  EXIT.                                                ELTDIAGS
01399 /                                                                 ELTDIAGS
01400  4200-PAYABLE-AS-SUPP SECTION.                                    ELTDIAGS
01401      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDIAGS
01402          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDIAGS
01403          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTDIAGS
01404          GO TO 4200-OUTPUT-TEXT.                                  ELTDIAGS
01405      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTDIAGS
01406      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTDIAGS
01407      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTDIAGS
01408                                                CMF-CODE-VALUE     ELTDIAGS
01409      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01410      STRING CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01411             CMF-DESCR-LINE(2)                                     ELTDIAGS
01412                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTDIAGS
01413      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01414      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTDIAGS
01415      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTDIAGS
01416      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTDIAGS
01417      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01418      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTDIAGS
01419                                         =  ZEROS                  ELTDIAGS
01420       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTDIAGS
01421                                         =  ZEROS                  ELTDIAGS
01422                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTDIAGS
01423                MOVE WS-SUPPLEMENTAL        TO                     ELTDIAGS
01424                         COF-DTL-LINE(WS-CIA)                      ELTDIAGS
01425       ELSE                                                        ELTDIAGS
01426          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTDIAGS
01427          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDIAGS
01428                                        TO  WS-DTL-PERCENT         ELTDIAGS
01429          MOVE SPACES          TO TCAR-FROM-AREA                   ELTDIAGS
01430          STRING WS-DTL-PP,                                        ELTDIAGS
01431                 WS-DTL-PERCENT,                                   ELTDIAGS
01432                 WS-PERCENT,                                       ELTDIAGS
01433                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDIAGS
01434          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTDIAGS
01435          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDIAGS
01436          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDIAGS
01437          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDIAGS
01438          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDIAGS
01439          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTDIAGS
01440          MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)    ELTDIAGS
01441      ELSE                                                         ELTDIAGS
01442        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTDIAGS
01443        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTDIAGS
01444                                    TO WS-DTL-PERCENT              ELTDIAGS
01445        MOVE SPACES          TO TCAR-FROM-AREA                     ELTDIAGS
01446        STRING WS-DTL-PP,                                          ELTDIAGS
01447               WS-DTL-PERCENT,                                     ELTDIAGS
01448               WS-PERCENT,                                         ELTDIAGS
01449                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDIAGS
01450        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTDIAGS
01451        MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT            ELTDIAGS
01452        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTDIAGS
01453        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTDIAGS
01454        PERFORM TCPR-000-TEXT-UNSTRING                             ELTDIAGS
01455        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTDIAGS
01456        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTDIAGS
01457  4200-OUTPUT-TEXT.                                                ELTDIAGS
01458      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01459            ADD +1                TO WS-CIA                        ELTDIAGS
01460            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDIAGS
01461            PERFORM 3000-OUTPUT-TEXT                               ELTDIAGS
01462      ELSE                                                         ELTDIAGS
01463         PERFORM 3000-OUTPUT-TEXT.                                 ELTDIAGS
01464  4200-EXIT.  EXIT.                                                ELTDIAGS
01465 /                                                                 ELTDIAGS
01466  4250-PROF-CHGR-HSP-CLM  SECTION.                                 ELTDIAGS
01467                                                                   ELTDIAGS
01468      MOVE 'N'        TO  WS-ADD-A-BLANK-IND.                      ELTDIAGS
01469      SET PLT-INDEX2  TO  1.                                       ELTDIAGS
01470                                                                   ELTDIAGS
01471      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  =  ZERO    ELTDIAGS
01472          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDIAGS
01473              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
01474                          NOT  =  '0'  AND  NOT  =  LOW-VALUES     ELTDIAGS
01475                  ADD  +1         TO  WS-CIA                       ELTDIAGS
01476                  MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)        ELTDIAGS
01477                  ADD  +1         TO  WS-CIA                       ELTDIAGS
01478                  MOVE WS-PROF-OUTPT-CHRGES                        ELTDIAGS
01479                                  TO  COF-DTL-LINE (WS-CIA)        ELTDIAGS
01480                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND.          ELTDIAGS
01481                                                                   ELTDIAGS
01482      SET PLT-INDEX2  TO  2.                                       ELTDIAGS
01483                                                                   ELTDIAGS
01484      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  =  ZERO     ELTDIAGS
01485          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDIAGS
01486                       AND                                         ELTDIAGS
01487             NOT  WS-ADD-A-BLANK-LINE                              ELTDIAGS
01488              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
01489                          NOT  =  '0'  AND  NOT  =  LOW-VALUES     ELTDIAGS
01490                  ADD  +1         TO  WS-CIA                       ELTDIAGS
01491                  MOVE SPACES     TO  COF-DTL-LINE (WS-CIA)        ELTDIAGS
01492                  ADD  +1         TO  WS-CIA                       ELTDIAGS
01493                  MOVE WS-PROF-OUTPT-CHRGES                        ELTDIAGS
01494                                  TO  COF-DTL-LINE (WS-CIA)        ELTDIAGS
01495                  MOVE 'Y'        TO  WS-ADD-A-BLANK-IND.          ELTDIAGS
01496                                                                   ELTDIAGS
01497      SET PLT-INDEX2  TO  1.                                       ELTDIAGS
01498                                                                   ELTDIAGS
01499      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT  ZERO       ELTDIAGS
01500          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDIAGS
01501              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
01502                      NOT  =  '0'  AND  NOT  =  LOW-VALUES         ELTDIAGS
01503                 MOVE 'BPB'         TO  CMF-RECORD-PREFIX          ELTDIAGS
01504                 MOVE 'PROF-CHRG-HSP-CLM'                          ELTDIAGS
01505                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTDIAGS
01506                 MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)ELTDIAGS
01507                                    TO  CMF-CODE-VALUE             ELTDIAGS
01508                 PERFORM 4251-TRANSLATE                            ELTDIAGS
01509                    THRU 4251-EXIT                                 ELTDIAGS
01510                 MOVE TCAR-OPF-DATA (1) TO WS-DTL-BASIC            ELTDIAGS
01511                 ADD  +1                TO WS-CIA                  ELTDIAGS
01512                 MOVE WS-BASIC          TO COF-DTL-LINE (WS-CIA)   ELTDIAGS
01513                 IF TCAR-OPF-DATA (2)  NOT  =  SPACES OR LOW-VALUESELTDIAGS
01514                     ADD  +1            TO WS-CIA                  ELTDIAGS
01515                     MOVE TCAR-OPF-DATA (2)                        ELTDIAGS
01516                                     TO COF-DTL-LINE (WS-CIA).     ELTDIAGS
01517                                                                   ELTDIAGS
01518      SET PLT-INDEX2  TO  2.                                       ELTDIAGS
01519                                                                   ELTDIAGS
01520      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  ZERO        ELTDIAGS
01521          IF WS-DISPLAY-B-FORMAT-TEXT  =  'Y'                      ELTDIAGS
01522              IF PLB-PROF-CHRG-HSP-CLM (PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
01523                      NOT  =  '0'  AND  NOT  =  LOW-VALUES         ELTDIAGS
01524                 MOVE 'BPB'         TO  CMF-RECORD-PREFIX          ELTDIAGS
01525                 MOVE 'PROF-CHRG-HSP-CLM'                          ELTDIAGS
01526                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTDIAGS
01527                 MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)ELTDIAGS
01528                                    TO  CMF-CODE-VALUE             ELTDIAGS
01529                 PERFORM 4251-TRANSLATE                            ELTDIAGS
01530                    THRU 4251-EXIT                                 ELTDIAGS
01531                 MOVE TCAR-OPF-DATA (1) TO WS-DTL-SUPPLEMENTAL     ELTDIAGS
01532                 ADD  +1                TO WS-CIA                  ELTDIAGS
01533                 MOVE WS-SUPPLEMENTAL   TO COF-DTL-LINE (WS-CIA)   ELTDIAGS
01534                 IF TCAR-OPF-DATA (2)  NOT  =  SPACES OR LOW-VALUESELTDIAGS
01535                     ADD  +1            TO WS-CIA                  ELTDIAGS
01536                     MOVE TCAR-OPF-DATA (2)                        ELTDIAGS
01537                                     TO COF-DTL-LINE (WS-CIA).     ELTDIAGS
01538      SKIP3                                                        ELTDIAGS
01539  4251-TRANSLATE.                                                  ELTDIAGS
01540                                                                   ELTDIAGS
01541      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01542      STRING CMF-DESCR-LINE (1) ' ' CMF-DESCR-LINE (2)             ELTDIAGS
01543         DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.                  ELTDIAGS
01544                                                                   ELTDIAGS
01545      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01546      MOVE  +02  TO  TCAR-OUTPUT-FIELD-COUNT.                      ELTDIAGS
01547      MOVE  +63  TO  TCAR-OUTPUT-FIELD-1-LEN.                      ELTDIAGS
01548      MOVE  +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                      ELTDIAGS
01549      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01550                                                                   ELTDIAGS
01551  4251-EXIT.  EXIT.                                                ELTDIAGS
01552 /                                                                 ELTDIAGS
01553  4300-SPILLOVER-COINS SECTION.                                    ELTDIAGS
01554      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDIAGS
01555            = '0'   OR LOW-VALUES                                  ELTDIAGS
01556           GO TO 4300-EXIT.                                        ELTDIAGS
01557      ADD +1     TO  WS-CIA.                                       ELTDIAGS
01558      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDIAGS
01559      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDIAGS
01560      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDIAGS
01561                       TO CMF-CODE-VALUE.                          ELTDIAGS
01562      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01563      STRING WS-SPILLOVER-COINS ' '                                ELTDIAGS
01564             CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01565             CMF-DESCR-LINE(2) ' '                                 ELTDIAGS
01566                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDIAGS
01567      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01568                                                                   ELTDIAGS
01569      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDIAGS
01570      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDIAGS
01571      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDIAGS
01572      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01573      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).               ELTDIAGS
01574      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01575         ADD +1                TO WS-CIA                           ELTDIAGS
01576         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTDIAGS
01577      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01578  4300-EXIT.  EXIT.                                                ELTDIAGS
01579 /                                                                 ELTDIAGS
01580  4400-SPILLOVER-DEDUCT SECTION.                                   ELTDIAGS
01581      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01582            = '0'   OR LOW-VALUES                                  ELTDIAGS
01583           GO TO 4400-EXIT.                                        ELTDIAGS
01584      ADD +1     TO  WS-CIA.                                       ELTDIAGS
01585      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDIAGS
01586      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTDIAGS
01587      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDIAGS
01588                       TO CMF-CODE-VALUE                           ELTDIAGS
01589      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01590      STRING WS-SPILLOVER-DEDBL ' '                                ELTDIAGS
01591             CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01592             CMF-DESCR-LINE(2) ' '                                 ELTDIAGS
01593                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDIAGS
01594      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01595                                                                   ELTDIAGS
01596      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDIAGS
01597      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDIAGS
01598      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDIAGS
01599      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01600      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).               ELTDIAGS
01601      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01602         ADD +1                TO WS-CIA                           ELTDIAGS
01603         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTDIAGS
01604      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01605  4400-EXIT.  EXIT.                                                ELTDIAGS
01606 /                                                                 ELTDIAGS
01607  4500-TRANS-OTHER-RESP-IND SECTION.                               ELTDIAGS
01608      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01609            = ZEROS OR LOW-VALUES                                  ELTDIAGS
01610           GO TO 4500-EXIT.                                        ELTDIAGS
01611                                                                   ELTDIAGS
01612      ADD +1     TO  WS-CIA.                                       ELTDIAGS
01613      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDIAGS
01614      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTDIAGS
01615      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDIAGS
01616                       TO CMF-CODE-VALUE                           ELTDIAGS
01617      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDIAGS
01618      STRING WS-SPILLOVER-DEDBL ' '                                ELTDIAGS
01619             CMF-DESCR-LINE(1) ' '                                 ELTDIAGS
01620             CMF-DESCR-LINE(2) ' '                                 ELTDIAGS
01621                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDIAGS
01622      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01623                                                                   ELTDIAGS
01624      MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTDIAGS
01625      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTDIAGS
01626      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTDIAGS
01627      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01628      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA).               ELTDIAGS
01629      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDIAGS
01630         ADD +1                TO WS-CIA                           ELTDIAGS
01631         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTDIAGS
01632      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01633  4500-EXIT.  EXIT.                                                ELTDIAGS
01634 /                                                                 ELTDIAGS
01635  4600-SCAN-TAB SECTION.                                           ELTDIAGS
01636      PERFORM 4700-BEN-TAB-AAR.                                    ELTDIAGS
01637                                                                   ELTDIAGS
01638      ADD +1           TO WS-CIA.                                  ELTDIAGS
01639      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDIAGS
01640      ADD +2           TO WS-CIA.                                  ELTDIAGS
01641      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01642                                                                   ELTDIAGS
01643      PERFORM 4800-BEN-TAB-PPF.                                    ELTDIAGS
01644      PERFORM 5000-BEN-TAB-ADL.                                    ELTDIAGS
01645      PERFORM 5100-BEN-TAB-ABM.                                    ELTDIAGS
01646      PERFORM 5200-BEN-TAB-ACL.                                    ELTDIAGS
01647      PERFORM 5300-BEN-TAB-AOL.                                    ELTDIAGS
01648                                                                   ELTDIAGS
01649  4600-EXIT.  EXIT.                                                ELTDIAGS
01650 /                                                                 ELTDIAGS
01651  4675-PAY-CONSID-TEXT SECTION.                                    ELTDIAGS
01652      INITIALIZE TCAR-FROM-AREA.                                   ELTDIAGS
01653      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDIAGS
01654             WS-PAY-CONSDR-TEXT2                                   ELTDIAGS
01655                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDIAGS
01656      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDIAGS
01657      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDIAGS
01658      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDIAGS
01659                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDIAGS
01660      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDIAGS
01661      IF WS-CIA > 17                                               ELTDIAGS
01662            PERFORM 3000-OUTPUT-TEXT                               ELTDIAGS
01663            MOVE +1            TO WS-CIA.                          ELTDIAGS
01664      ADD +1                TO  WS-CIA.                            ELTDIAGS
01665      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDIAGS
01666      ADD +1                TO  WS-CIA.                            ELTDIAGS
01667      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDIAGS
01668      PERFORM 3000-OUTPUT-TEXT.                                    ELTDIAGS
01669  4675-EXIT.   EXIT.                                               ELTDIAGS
01670 /                                                                 ELTDIAGS
01671  4700-BEN-TAB-AAR SECTION.                                        ELTDIAGS
01672      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTDIAGS
01673      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01674                                                                   ELTDIAGS
01675      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01676          NOT = LOW-VALUES                                         ELTDIAGS
01677       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01678          NOT = SPACE                                              ELTDIAGS
01679                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDIAGS
01680                                                                   ELTDIAGS
01681      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01682                                                                   ELTDIAGS
01683      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01684          NOT = LOW-VALUES                                         ELTDIAGS
01685       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01686          NOT = SPACE                                              ELTDIAGS
01687                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDIAGS
01688                                                                   ELTDIAGS
01689      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTDIAGS
01690             MOVE +2                  TO WS-CIA                    ELTDIAGS
01691             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTDIAGS
01692             ADD  +1                  TO WS-CIA                    ELTDIAGS
01693             PERFORM 3000-OUTPUT-TEXT.                             ELTDIAGS
01694  4700-EXIT.  EXIT.                                                ELTDIAGS
01695 /                                                                 ELTDIAGS
01696  4800-BEN-TAB-PPF SECTION.                                        ELTDIAGS
01697      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDIAGS
01698                       WS-HOLD2.                                   ELTDIAGS
01699      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01700      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01701          NOT = LOW-VALUES                                         ELTDIAGS
01702       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01703          NOT = SPACE                                              ELTDIAGS
01704             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01705                        TO  WS-HOLD1.                              ELTDIAGS
01706                                                                   ELTDIAGS
01707      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01708      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01709          NOT = LOW-VALUES                                         ELTDIAGS
01710       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01711          NOT = SPACE                                              ELTDIAGS
01712             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01713                        TO  WS-HOLD2.                              ELTDIAGS
01714                                                                   ELTDIAGS
01715      IF WS-HOLD1 = WS-HOLD2                                       ELTDIAGS
01716         IF WS-HOLD1 = ZEROS                                       ELTDIAGS
01717                 GO TO 4800-EXIT                                   ELTDIAGS
01718         ELSE                                                      ELTDIAGS
01719             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDIAGS
01720             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01721             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTDIAGS
01722                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01723             END-EXEC                                              ELTDIAGS
01724             GO TO 4800-EXIT.                                      ELTDIAGS
01725                                                                   ELTDIAGS
01726      IF WS-HOLD1 = ZEROS                                          ELTDIAGS
01727             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDIAGS
01728             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01729             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTDIAGS
01730                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01731             END-EXEC                                              ELTDIAGS
01732      ELSE                                                         ELTDIAGS
01733       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDIAGS
01734       PERFORM 5900-GET-TAB-REC                                    ELTDIAGS
01735       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTDIAGS
01736                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01737       END-EXEC                                                    ELTDIAGS
01738       IF WS-HOLD2 = ZEROS                                         ELTDIAGS
01739            GO TO 4800-EXIT                                        ELTDIAGS
01740       ELSE                                                        ELTDIAGS
01741          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDIAGS
01742          PERFORM 5900-GET-TAB-REC                                 ELTDIAGS
01743          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTDIAGS
01744                          COMMAREA(DFHCOMMAREA)                    ELTDIAGS
01745          END-EXEC.                                                ELTDIAGS
01746  4800-EXIT.    EXIT.                                              ELTDIAGS
01747 /                                                                 ELTDIAGS
01748  5000-BEN-TAB-ADL SECTION.                                        ELTDIAGS
01749      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDIAGS
01750                       WS-HOLD2.                                   ELTDIAGS
01751      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01752      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01753          NOT = LOW-VALUES                                         ELTDIAGS
01754       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01755          NOT = SPACE                                              ELTDIAGS
01756             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01757                        TO  WS-HOLD1.                              ELTDIAGS
01758                                                                   ELTDIAGS
01759      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01760      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01761          NOT = LOW-VALUES                                         ELTDIAGS
01762       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01763          NOT = SPACE                                              ELTDIAGS
01764             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01765                        TO  WS-HOLD2.                              ELTDIAGS
01766                                                                   ELTDIAGS
01767      IF WS-HOLD1 = WS-HOLD2                                       ELTDIAGS
01768         IF WS-HOLD1 = ZEROS                                       ELTDIAGS
01769                 GO TO 5000-EXIT                                   ELTDIAGS
01770         ELSE                                                      ELTDIAGS
01771             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDIAGS
01772             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01773             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDIAGS
01774                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01775             END-EXEC                                              ELTDIAGS
01776             GO TO 5000-EXIT.                                      ELTDIAGS
01777                                                                   ELTDIAGS
01778      IF WS-HOLD1 = ZEROS                                          ELTDIAGS
01779             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDIAGS
01780             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01781             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDIAGS
01782                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01783             END-EXEC                                              ELTDIAGS
01784      ELSE                                                         ELTDIAGS
01785       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDIAGS
01786       PERFORM 5900-GET-TAB-REC                                    ELTDIAGS
01787       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTDIAGS
01788                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01789       END-EXEC                                                    ELTDIAGS
01790       IF WS-HOLD2 = ZEROS                                         ELTDIAGS
01791            GO TO 5000-EXIT                                        ELTDIAGS
01792       ELSE                                                        ELTDIAGS
01793          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDIAGS
01794          PERFORM 5900-GET-TAB-REC                                 ELTDIAGS
01795          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTDIAGS
01796                          COMMAREA(DFHCOMMAREA)                    ELTDIAGS
01797          END-EXEC.                                                ELTDIAGS
01798  5000-EXIT.     EXIT.                                             ELTDIAGS
01799 /                                                                 ELTDIAGS
01800  5100-BEN-TAB-ABM SECTION.                                        ELTDIAGS
01801      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDIAGS
01802                       WS-HOLD2.                                   ELTDIAGS
01803      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01804      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01805          NOT = LOW-VALUES                                         ELTDIAGS
01806       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01807          NOT = SPACE                                              ELTDIAGS
01808             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01809                        TO  WS-HOLD1.                              ELTDIAGS
01810                                                                   ELTDIAGS
01811      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01812      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01813          NOT = LOW-VALUES                                         ELTDIAGS
01814       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01815          NOT = SPACE                                              ELTDIAGS
01816             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01817                        TO  WS-HOLD2.                              ELTDIAGS
01818                                                                   ELTDIAGS
01819      IF WS-HOLD1 = WS-HOLD2                                       ELTDIAGS
01820         IF WS-HOLD1 = ZEROS                                       ELTDIAGS
01821                 GO TO 5100-EXIT                                   ELTDIAGS
01822         ELSE                                                      ELTDIAGS
01823             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDIAGS
01824             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01825             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDIAGS
01826                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01827             END-EXEC                                              ELTDIAGS
01828             GO TO 5100-EXIT.                                      ELTDIAGS
01829                                                                   ELTDIAGS
01830      IF WS-HOLD1 = ZEROS                                          ELTDIAGS
01831             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDIAGS
01832             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01833             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDIAGS
01834                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01835             END-EXEC                                              ELTDIAGS
01836      ELSE                                                         ELTDIAGS
01837       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDIAGS
01838       PERFORM 5900-GET-TAB-REC                                    ELTDIAGS
01839       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTDIAGS
01840                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01841       END-EXEC                                                    ELTDIAGS
01842       IF WS-HOLD2 = ZEROS                                         ELTDIAGS
01843           GO TO 5100-EXIT                                         ELTDIAGS
01844       ELSE                                                        ELTDIAGS
01845          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDIAGS
01846          PERFORM 5900-GET-TAB-REC                                 ELTDIAGS
01847          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTDIAGS
01848                          COMMAREA(DFHCOMMAREA)                    ELTDIAGS
01849          END-EXEC.                                                ELTDIAGS
01850  5100-EXIT.     EXIT.                                             ELTDIAGS
01851 /                                                                 ELTDIAGS
01852  5200-BEN-TAB-ACL SECTION.                                        ELTDIAGS
01853      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDIAGS
01854                       WS-HOLD2.                                   ELTDIAGS
01855      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01856      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01857          NOT = LOW-VALUES                                         ELTDIAGS
01858       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01859          NOT = SPACE                                              ELTDIAGS
01860             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01861                        TO  WS-HOLD1.                              ELTDIAGS
01862                                                                   ELTDIAGS
01863      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01864      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01865          NOT = LOW-VALUES                                         ELTDIAGS
01866       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01867          NOT = SPACE                                              ELTDIAGS
01868             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01869                        TO  WS-HOLD2.                              ELTDIAGS
01870                                                                   ELTDIAGS
01871      IF WS-HOLD1 = WS-HOLD2                                       ELTDIAGS
01872         IF WS-HOLD1 = ZEROS                                       ELTDIAGS
01873                 GO TO 5200-EXIT                                   ELTDIAGS
01874         ELSE                                                      ELTDIAGS
01875             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDIAGS
01876             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01877             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDIAGS
01878                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01879             END-EXEC                                              ELTDIAGS
01880             GO TO 5200-EXIT.                                      ELTDIAGS
01881                                                                   ELTDIAGS
01882      IF WS-HOLD1 = ZEROS                                          ELTDIAGS
01883             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDIAGS
01884             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01885             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDIAGS
01886                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01887             END-EXEC                                              ELTDIAGS
01888      ELSE                                                         ELTDIAGS
01889       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDIAGS
01890       PERFORM 5900-GET-TAB-REC                                    ELTDIAGS
01891       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTDIAGS
01892                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01893       END-EXEC                                                    ELTDIAGS
01894       IF WS-HOLD2 = ZEROS                                         ELTDIAGS
01895            GO TO 5200-EXIT                                        ELTDIAGS
01896       ELSE                                                        ELTDIAGS
01897          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDIAGS
01898          PERFORM 5900-GET-TAB-REC                                 ELTDIAGS
01899          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTDIAGS
01900                          COMMAREA(DFHCOMMAREA)                    ELTDIAGS
01901          END-EXEC.                                                ELTDIAGS
01902  5200-EXIT.     EXIT.                                             ELTDIAGS
01903 /                                                                 ELTDIAGS
01904  5300-BEN-TAB-AOL SECTION.                                        ELTDIAGS
01905      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDIAGS
01906                       WS-HOLD2.                                   ELTDIAGS
01907      SET PLT-INDEX2 TO 1.                                         ELTDIAGS
01908      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01909          NOT = LOW-VALUES                                         ELTDIAGS
01910       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01911          NOT = SPACE                                              ELTDIAGS
01912             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01913                        TO  WS-HOLD1.                              ELTDIAGS
01914                                                                   ELTDIAGS
01915      SET PLT-INDEX2 TO 2.                                         ELTDIAGS
01916      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDIAGS
01917          NOT = LOW-VALUES                                         ELTDIAGS
01918       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDIAGS
01919          NOT = SPACE                                              ELTDIAGS
01920             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDIAGS
01921                        TO  WS-HOLD2.                              ELTDIAGS
01922                                                                   ELTDIAGS
01923      IF WS-HOLD1 = WS-HOLD2                                       ELTDIAGS
01924         IF WS-HOLD1 = ZEROS                                       ELTDIAGS
01925                 GO TO 5300-EXIT                                   ELTDIAGS
01926         ELSE                                                      ELTDIAGS
01927             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDIAGS
01928             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01929             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDIAGS
01930                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01931             END-EXEC                                              ELTDIAGS
01932             GO TO 5300-EXIT.                                      ELTDIAGS
01933                                                                   ELTDIAGS
01934      IF WS-HOLD1 = ZEROS                                          ELTDIAGS
01935             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDIAGS
01936             PERFORM 5900-GET-TAB-REC                              ELTDIAGS
01937             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDIAGS
01938                             COMMAREA(DFHCOMMAREA)                 ELTDIAGS
01939             END-EXEC                                              ELTDIAGS
01940      ELSE                                                         ELTDIAGS
01941       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDIAGS
01942       PERFORM 5900-GET-TAB-REC                                    ELTDIAGS
01943       EXEC CICS  LINK PROGRAM('ELGOUTPX')                         ELTDIAGS
01944                       COMMAREA(DFHCOMMAREA)                       ELTDIAGS
01945       END-EXEC                                                    ELTDIAGS
01946       IF WS-HOLD2 = ZEROS                                         ELTDIAGS
01947            GO TO 5300-EXIT                                        ELTDIAGS
01948       ELSE                                                        ELTDIAGS
01949          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDIAGS
01950          PERFORM 5900-GET-TAB-REC                                 ELTDIAGS
01951          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTDIAGS
01952                          COMMAREA(DFHCOMMAREA)                    ELTDIAGS
01953          END-EXEC.                                                ELTDIAGS
01954  5300-EXIT.     EXIT.                                             ELTDIAGS
01955 /                                                                 ELTDIAGS
01956  5900-GET-TAB-REC SECTION.                                        ELTDIAGS
01957                                                                   ELTDIAGS
01958      SET CIA-GCTABULR-DDN TO TRUE.                                ELTDIAGS
01959      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
01960          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTDIAGS
01961                                                                   ELTDIAGS
01962      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTDIAGS
01963      SET  CIA-GCTABULR-DDN          TO TRUE.                      ELTDIAGS
01964                                                                   ELTDIAGS
01965      SET IOP-RD                     TO TRUE.                      ELTDIAGS
01966      SET IOP-FCQ-NONE               TO TRUE.                      ELTDIAGS
01967      SET IOP-KVQ-NONE               TO TRUE.                      ELTDIAGS
01968                                                                   ELTDIAGS
01969      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTDIAGS
01970             COMMAREA(DFHCOMMAREA)                                 ELTDIAGS
01971      END-EXEC.                                                    ELTDIAGS
01972                                                                   ELTDIAGS
01973      IF IOP-RC-NOTFND                                             ELTDIAGS
01974         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTDIAGS
01975         EXEC CICS ABEND                                           ELTDIAGS
01976                   ABCODE(CIA-ABCODE)                              ELTDIAGS
01977         END-EXEC.                                                 ELTDIAGS
01978                                                                   ELTDIAGS
01979      IF NOT IOP-RC-OK                                             ELTDIAGS
01980         SET CIA-AB-CRITIO          TO TRUE                        ELTDIAGS
01981         EXEC CICS ABEND                                           ELTDIAGS
01982                   ABCODE(CIA-ABCODE)                              ELTDIAGS
01983         END-EXEC.                                                 ELTDIAGS
01984  5900-EXIT.                                                       ELTDIAGS
01985 /                                                                 ELTDIAGS
01986  6000-MOVE-IN-INST-IP-LAB SECTION.                                ELTDIAGS
01987      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
01988      MOVE WS-INST-IP-LIST-LAB(WS-SUB)  TO                         ELTDIAGS
01989                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
01990      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
01991                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
01992  6000-EXIT.  EXIT.                                                ELTDIAGS
01993                                                                   ELTDIAGS
01994 /                                                                 ELTDIAGS
01995  6100-MOVE-IN-INST-OP-LAB SECTION.                                ELTDIAGS
01996      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
01997      MOVE WS-INST-OP-LIST-LAB(WS-SUB)  TO                         ELTDIAGS
01998                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX)    ELTDIAGS
01999      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02000                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02001  6100-EXIT.  EXIT.                                                ELTDIAGS
02002 /                                                                 ELTDIAGS
02003  6200-MOVE-IN-INST-IP-MED SECTION.                                ELTDIAGS
02004      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02005      MOVE WS-INST-IP-LIST-MED(WS-SUB)  TO                         ELTDIAGS
02006                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02007      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02008                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02009  6200-EXIT.  EXIT.                                                ELTDIAGS
02010 /                                                                 ELTDIAGS
02011  6300-MOVE-IN-INST-OP-MED SECTION.                                ELTDIAGS
02012      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02013      MOVE WS-INST-OP-LIST-MED(WS-SUB)  TO                         ELTDIAGS
02014                         PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).      ELTDIAGS
02015      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02016                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02017  6300-EXIT.  EXIT.                                                ELTDIAGS
02018 /                                                                 ELTDIAGS
02019  6400-MOVE-IN-INST-IP-XRY SECTION.                                ELTDIAGS
02020      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02021      MOVE WS-INST-IP-LIST-XRY(WS-SUB)  TO                         ELTDIAGS
02022                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02023      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02024                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02025  6400-EXIT.  EXIT.                                                ELTDIAGS
02026                                                                   ELTDIAGS
02027 /                                                                 ELTDIAGS
02028  6500-MOVE-IN-INST-OP-XRY SECTION.                                ELTDIAGS
02029      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02030      MOVE WS-INST-OP-LIST-XRY(WS-SUB)  TO                         ELTDIAGS
02031                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02032      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02033                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02034  6500-EXIT.  EXIT.                                                ELTDIAGS
02035 /                                                                 ELTDIAGS
02036  7000-MOVE-IN-PROF-IP-LAB SECTION.                                ELTDIAGS
02037      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02038      MOVE WS-PROF-IP-LIST-LAB(WS-SUB)  TO                         ELTDIAGS
02039                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02040      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02041                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02042  7000-EXIT.  EXIT.                                                ELTDIAGS
02043 /                                                                 ELTDIAGS
02044  7100-MOVE-IN-PROF-OP-LAB SECTION.                                ELTDIAGS
02045      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02046      MOVE WS-PROF-OP-LIST-LAB(WS-SUB)  TO                         ELTDIAGS
02047                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02048      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02049                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02050  7100-EXIT.  EXIT.                                                ELTDIAGS
02051 /                                                                 ELTDIAGS
02052  7200-MOVE-IN-PROF-IP-MED SECTION.                                ELTDIAGS
02053      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02054      MOVE WS-PROF-IP-LIST-MED(WS-SUB)  TO                         ELTDIAGS
02055                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02056      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02057                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02058  7200-EXIT.  EXIT.                                                ELTDIAGS
02059 /                                                                 ELTDIAGS
02060  7300-MOVE-IN-PROF-OP-MED SECTION.                                ELTDIAGS
02061      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02062      MOVE WS-PROF-OP-LIST-MED(WS-SUB)  TO                         ELTDIAGS
02063                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02064      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02065                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02066  7300-EXIT.  EXIT.                                                ELTDIAGS
02067 /                                                                 ELTDIAGS
02068  7400-MOVE-IN-PROF-IP-XRY SECTION.                                ELTDIAGS
02069      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02070      MOVE WS-PROF-IP-LIST-XRY(WS-SUB)  TO                         ELTDIAGS
02071                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02072      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02073                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02074  7400-EXIT.  EXIT.                                                ELTDIAGS
02075 /                                                                 ELTDIAGS
02076  7500-MOVE-IN-PROF-OP-XRY SECTION.                                ELTDIAGS
02077      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDIAGS
02078      MOVE WS-PROF-OP-LIST-XRY(WS-SUB)  TO                         ELTDIAGS
02079                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDIAGS
02080      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDIAGS
02081                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDIAGS
02082  7500-EXIT.  EXIT.                                                ELTDIAGS
02083 /                                                                 ELTDIAGS
02084  8000-CALL-COVERAGE SECTION.                                      ELTDIAGS
02085      MOVE '8000'            TO  WS-PARA-ID.                       ELTDIAGS
02086 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDIAGS
02087      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDIAGS
02088      MOVE WS-YES TO WS-FIRSTTIME-IND.                             ELTDIAGS
02089      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDIAGS
02090      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDIAGS
02091                                                                   ELTDIAGS
02092      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDIAGS
02093      END-EXEC.                                                    ELTDIAGS
02094                                                                   ELTDIAGS
02095      PERFORM 8300-DISPLAY-COVERAGE.                               ELTDIAGS
02096  8000-EXIT.  EXIT.                                                ELTDIAGS
02097 *                                                                 ELTDIAGS
02098  8300-DISPLAY-COVERAGE SECTION.                                   ELTDIAGS
02099 ********************************************************          ELTDIAGS
02100 ***** REARRANGING THIS PARAGRAPH SO THAT YOU ARE NOT ***          ELTDIAGS
02101 ***** FORCED TO DO A LINK FOR A NEW PAGE WHEN YOU    ***          ELTDIAGS
02102 ***** DO NOT NEED ONE.     12/11/87 ==> REB.         ***          ELTDIAGS
02103 ********************************************************          ELTDIAGS
02104      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDIAGS
02105      END-EXEC.                                                    ELTDIAGS
02106                                                                   ELTDIAGS
02107 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDIAGS
02108      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDIAGS
02109      MOVE ' '  TO  COF-FUNCTION.                                  ELTDIAGS
02110      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDIAGS
02111      END-EXEC.                                                    ELTDIAGS
02112 *4/15 END OF TEMPORARY CODE                                       ELTDIAGS
02113  8300-EXIT.  EXIT.                                                ELTDIAGS
02114 /  C O D E S   M A N U A L   C A L L                              ELTDIAGS
02115  8500-CALL-CODES-MANUAL SECTION.                                  ELTDIAGS
02116      INITIALIZE CMF-RETURN-CODE,                                  ELTDIAGS
02117                 TCAR-FROM-AREA.                                   ELTDIAGS
02118      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTDIAGS
02119               COMMAREA(DFHCOMMAREA)                               ELTDIAGS
02120      END-EXEC.                                                    ELTDIAGS
02121                                                                   ELTDIAGS
02122      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDIAGS
02123      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDIAGS
02124          ADDRESS OF CMF-DESCR.                                    ELTDIAGS
02125  8500-EXIT.     EXIT.                                             ELTDIAGS
02126 /   C O M P R E S S I O N  A N D  U N S T R I N G   R O U T I N E ELTDIAGS
02127  8600-ELSTCOMP SECTION.                                           ELTDIAGS
02128 ****                                                              ELTDIAGS
02129 **** 8600-ELSTCOMP SECTION REQUIRED TO END PREV SECTION           ELTDIAGS
02130 ****                                                              ELTDIAGS
02131  COPY ELSTCOMP.                                                   ELTDIAGS
02132 /              A B E N D                                          ELTDIAGS
02133 ******************************************************************ELTDIAGS
02134 *                        A B E N D                                ELTDIAGS
02135 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTDIAGS
02136 *                                                                 ELTDIAGS
02137 ******************************************************************ELTDIAGS
02138  9998-INVALID-PTR SECTION.                                        ELTDIAGS
02139      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTDIAGS
02140      EXEC CICS ABEND   ABCODE(CIA-ABCODE) END-EXEC.               ELTDIAGS
02141                                                                   ELTDIAGS
02142  9999-ABEND SECTION.                                              ELTDIAGS
02143                                                                   ELTDIAGS
02144      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTDIAGS
02145                                                                   ELTDIAGS
02146  9999-EXIT.     EXIT.                                             ELTDIAGS
