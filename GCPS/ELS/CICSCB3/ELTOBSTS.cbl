00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTOBSTS.                                            ELTOBSTS
00003  AUTHOR. JOHN CURIN - KEANE, INC.                                    LV001
00004  DATE-WRITTEN.   4/03/86.                                         ELTOBSTS
00005  DATE-COMPILED.                                                   ELTOBSTS
00006      SKIP3                                                        ELTOBSTS
00007 ******************************************************************ELTOBSTS
00008 *@>ELTOBSTS                                                       ELTOBSTS
00009 *@¬                                                               ELTOBSTS
00010 *                        PROGRAM ABSTRACT                         ELTOBSTS
00011 *                                                                 ELTOBSTS
00012 *@¬ PROGRAM NAME:   E.L.S. OB/GYNE TOPIC                          ELTOBSTS
00013 *@¬                                                               ELTOBSTS
00014 *@¬ PROGRAM I.D.:   ELTOBSTS                                      ELTOBSTS
00015 *@¬                                                               ELTOBSTS
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE OB/GYNE ELTOBSTS
00017 *@¬            COVERAGE GIVEN A MEMBER.                           ELTOBSTS
00018 *@¬                                                               ELTOBSTS
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF OB/GYNE COVERAGE  ELTOBSTS
00020 *@¬            AFFORD A MEMBER BY HIS GROUP.  THIS INFORMATION IS ELTOBSTS
00021 *@¬            GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS FOR  ELTOBSTS
00022 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTOBSTS
00023 *@¬            RANGE OF DATES.                                    ELTOBSTS
00024 *@¬                                                               ELTOBSTS
00025 *@¬ RECORDS                                                       ELTOBSTS
00026 *@¬ ACCESSED:  GROUP SPECIFIC, VARIOUS BENEFIT PROVISION, AND A   ELTOBSTS
00027 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTOBSTS
00028 *@¬                                                               ELTOBSTS
00029 *@¬ PROCESSING                                                    ELTOBSTS
00030 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTOBSTS
00031 *@¬                                                               ELTOBSTS
00032 *@¬                                                               ELTOBSTS
00033 ***************************************************************** ELTOBSTS
00034 *                                                                 ELTOBSTS
00035 *                                                                 ELTOBSTS
00036 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*            ELTOBSTS
00037 *        *-*     U P D A T E  H I S T O R Y        *-*            ELTOBSTS
00038 *        *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*            ELTOBSTS
00039 *                                                                 ELTOBSTS
00040 **-CHG NUM-* *-DATE-* *WHO* *-----DESCRIPTION---------------      ELTOBSTS
00041 *    XXXX    04/28/86  JTC   ORIGINAL IMPLEMENTATION              ELTOBSTS
00042 *    0001    08/14/86  JTC   REMOVED THE SETUP OF HEADING LINE 1  ELTOBSTS
00043 *                            WS-HDR-1.  ALSO ADDED A FIX SO EACH  ELTOBSTS
00044 *                            GROUP OF BENEFIT PROVISIONS STARTS ONELTOBSTS
00045 *                            A NEW PAGE.                          ELTOBSTS
00046 *    XXXX    09/25/86  NAC   VS COBOL II CONVERSION.              ELTOBSTS
00047 *    XXXX    06/22/87  REB   ADDED CODE TO ALLOW MORE THAN 2 LINESELTOBSTS
00048 *                            OF THE CODE MANUAL FOR THE NORMAL    ELTOBSTS
00049 *                            NEWBORN ELIG IND FOR EACH L-O-B.     ELTOBSTS
00050 *    XXXX    02/07/89  AKK   MADE CHANGES TO ACCOMODATE STORAGE   ELTOBSTS
00051 *                            MANAGEMENT ENHANCEMENTS.             ELTOBSTS
00052 *    XXXX    08/24/90  GEM   DELETED BENEFIT PROVISION IDS 'NNBC EELTOBSTS
00053 *                            ADDED   BENEFIT PROVISION IDS 'NCBI EELTOBSTS
00054 *    XXXX    11/16/90  GEM   CHANGED PLP-TRANSF-OTHER-RESP-IND COMELTOBSTS
00055 *                            PARE TO THE LITREAL ZERO INSTEAD OF 0ELTOBSTS
00056 *    XXXX    05/25/95  AKK   ADDED SUPPORT FOR TREATMENT          ELTOBSTS
00057 *                            RESTRICTION INDICATOR.               ELTOBSTS
00058 *    XXXX    06/14/95  AKK   CORRECTED STORAGE VIOLATION IN       ELTOBSTS
00059 *                            PARAGRAPH 6115.                      ELTOBSTS
00060 /                                                                 ELTOBSTS
00061  ENVIRONMENT DIVISION.                                            ELTOBSTS
00062      SKIP3                                                        ELTOBSTS
00063  DATA DIVISION.                                                   ELTOBSTS
00064  WORKING-STORAGE SECTION.                                         ELTOBSTS
00065  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTOBSTS
00066      '***ELTOBSTS WS BEGINS***'.                                  ELTOBSTS
00067 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTOBSTS
00068  01  WS-WORK-FIELDS.                                              ELTOBSTS
00069      05  WS-HEX-00                     PIC X.                     ELTOBSTS
00070      05  WS-CHAR-0                     PIC X.                     ELTOBSTS
00071      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTOBSTS
00072      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTOBSTS
00073      05  WS-DISPLAY-PAYABLE-AS-TEXT    PIC X.                     ELTOBSTS
00074      05  WS-HOLD1                      PIC X(10).                 ELTOBSTS
00075      05  WS-HOLD2                      PIC X(10).                 ELTOBSTS
00076      05  WS-DTL-PP                     PIC X(63).                 ELTOBSTS
00077      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTOBSTS
00078      05  WS-DTL-PER-D                  PIC X(63).                 ELTOBSTS
00079      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTOBSTS
00080      05  WS-FIRSTTIME-IND              PIC X.                     ELTOBSTS
00081          88  WS-NOT-FIRST-TIME          VALUE 'N'.                ELTOBSTS
00082      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTOBSTS
00083      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTOBSTS
00084      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTOBSTS
00085      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTOBSTS
00086      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTOBSTS
00087                                                                   ELTOBSTS
00088 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTOBSTS
00089  01  TABLE-MAX                   PIC S9(03) VALUE +13 COMP.       ELTOBSTS
00090 * 13 REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTOBSTS
00091  01  WS-BEN-PROV-ID.                                              ELTOBSTS
00092      05  WS-INST-IP-CNT                PIC S999 COMP-3  VALUE +13.ELTOBSTS
00093      05  WS-INST-IP-TAB.                                          ELTOBSTS
00094        10  FILLER                      PIC X(6)  VALUE 'OBND W'.  ELTOBSTS
00095        10  FILLER                      PIC X(6)  VALUE 'OBNM W'.  ELTOBSTS
00096        10  FILLER                      PIC X(6)  VALUE 'OBNS W'.  ELTOBSTS
00097        10  FILLER                      PIC X(6)  VALUE 'EABI W'.  ELTOBSTS
00098        10  FILLER                      PIC X(6)  VALUE 'TABI W'.  ELTOBSTS
00099        10  FILLER                      PIC X(6)  VALUE 'MBD  W'.  ELTOBSTS
00100        10  FILLER                      PIC X(6)  VALUE 'MBM  W'.  ELTOBSTS
00101        10  FILLER                      PIC X(6)  VALUE 'MBS  W'.  ELTOBSTS
00102        10  FILLER                      PIC X(6)  VALUE 'OBCD W'.  ELTOBSTS
00103        10  FILLER                      PIC X(6)  VALUE 'OBCM W'.  ELTOBSTS
00104        10  FILLER                      PIC X(6)  VALUE 'OBCS W'.  ELTOBSTS
00105        10  FILLER                      PIC X(6)  VALUE 'EABO W'.  ELTOBSTS
00106        10  FILLER                      PIC X(6)  VALUE 'TABO W'.  ELTOBSTS
00107      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTOBSTS
00108                                        PIC X(6)  OCCURS 13 TIMES. ELTOBSTS
00109                                                                   ELTOBSTS
00110      05  WS-PROF-IP-CNT                PIC S999 COMP-3  VALUE +13.ELTOBSTS
00111      05  WS-PROF-IP-TAB.                                          ELTOBSTS
00112        10  FILLER                      PIC X(6)  VALUE 'OBND C'.  ELTOBSTS
00113        10  FILLER                      PIC X(6)  VALUE 'OBNM C'.  ELTOBSTS
00114        10  FILLER                      PIC X(6)  VALUE 'OBNS C'.  ELTOBSTS
00115        10  FILLER                      PIC X(6)  VALUE 'OBCD C'.  ELTOBSTS
00116        10  FILLER                      PIC X(6)  VALUE 'OBCM C'.  ELTOBSTS
00117        10  FILLER                      PIC X(6)  VALUE 'OBCS C'.  ELTOBSTS
00118        10  FILLER                      PIC X(6)  VALUE 'EABO C'.  ELTOBSTS
00119        10  FILLER                      PIC X(6)  VALUE 'EABI C'.  ELTOBSTS
00120        10  FILLER                      PIC X(6)  VALUE 'TABI C'.  ELTOBSTS
00121        10  FILLER                      PIC X(6)  VALUE 'TABO C'.  ELTOBSTS
00122        10  FILLER                      PIC X(6)  VALUE 'NCBI E'.  ELTOBSTS
00123        10  FILLER                      PIC X(6)  VALUE 'RPNV E'.  ELTOBSTS
00124        10  FILLER                      PIC X(6)  VALUE 'RPOV E'.  ELTOBSTS
00125      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTOBSTS
00126                                        PIC X(6)  OCCURS 13 TIMES. ELTOBSTS
00127 /                L I T E R A L S                                  ELTOBSTS
00128  01  WS-PROGRAM-LITERALS.                                         ELTOBSTS
00129    05  WS-PERCENT                  PIC X(07) VALUE 'PERCENT'.     ELTOBSTS
00130    05  WS-DAYS                     PIC X(04) VALUE 'DAYS'.        ELTOBSTS
00131    05  WS-YES                      PIC X     VALUE 'Y'.           ELTOBSTS
00132    05  WS-NO                       PIC X     VALUE 'N'.           ELTOBSTS
00133    05  WS-BASIC-LIT                PIC X(16) VALUE                ELTOBSTS
00134        '         BASIC: '.                                        ELTOBSTS
00135    05  WS-SUPPLEMENTAL-LIT         PIC X(16) VALUE                ELTOBSTS
00136        '  SUPPLEMENTAL: '.                                        ELTOBSTS
00137    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTOBSTS
00138          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTOBSTS
00139    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTOBSTS
00140          VALUE 'SPILLOVER COINSURANCE: '.                         ELTOBSTS
00141    05  WS-SPILLOVER-FL-RT-PER-D    PIC X(30)                      ELTOBSTS
00142          VALUE 'SPILLOVER FLAT RATE PER DIEM: '.                  ELTOBSTS
00143    05  WS-RULE-BILL-NEWBORN-CHGS   PIC X(36)                      ELTOBSTS
00144          VALUE 'RULES FOR BILLING NEWBORN CHARGES: '.             ELTOBSTS
00145    05  WS-SERVICES-RENDERED        PIC X(26)                      ELTOBSTS
00146          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTOBSTS
00147                                                                   ELTOBSTS
00148 /            D I S P L A Y   L I N E S                            ELTOBSTS
00149  01  WS-ELS-DISPLAY-LINES.                                        ELTOBSTS
00150    05  WS-HDR-2-PROF-IP.                                          ELTOBSTS
00151      10  FILLER                    PIC X(21) VALUE SPACES.        ELTOBSTS
00152      10  FILLER                    PIC X(33)                      ELTOBSTS
00153          VALUE 'OBSTETRICAL SERVICES PROFESSIONAL'.               ELTOBSTS
00154      10  FILLER                    PIC X(25) VALUE LOW-VALUES.    ELTOBSTS
00155                                                                   ELTOBSTS
00156    05  WS-HDR-2-INST-IP.                                          ELTOBSTS
00157      10  FILLER                    PIC X(21) VALUE SPACES.        ELTOBSTS
00158      10  FILLER                    PIC X(34)                      ELTOBSTS
00159          VALUE 'OBSTETRICAL SERVICES INSTITUTIONAL'.              ELTOBSTS
00160      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTOBSTS
00161                                                                   ELTOBSTS
00162    05  WS-OB-SERVICES-ARE.                                        ELTOBSTS
00163      10  FILLER                    PIC X(26)                      ELTOBSTS
00164          VALUE 'OBSTETRICAL SERVICES'.                            ELTOBSTS
00165                                                                   ELTOBSTS
00166    05  WS-FOLLOW-BENEFIT.                                         ELTOBSTS
00167      10  FILLER                  PIC  X(22) VALUE                 ELTOBSTS
00168            'COVERED SERVICES ARE: '.                              ELTOBSTS
00169                                                                   ELTOBSTS
00170    05  WS-SERVICES-2ND.                                           ELTOBSTS
00171      10  FILLER                    PIC X(21) VALUE SPACES.        ELTOBSTS
00172      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTOBSTS
00173      10  FILLER                    PIC X(3) VALUE LOW-VALUES.     ELTOBSTS
00174                                                                   ELTOBSTS
00175                                                                   ELTOBSTS
00176    05  WS-CERT-REQ.                                               ELTOBSTS
00177      10  FILLER                  PIC  X(45) VALUE                 ELTOBSTS
00178          'CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.          ELTOBSTS
00179                                                                   ELTOBSTS
00180    05  WS-TREAT-RESTRN.                                           ELTOBSTS
00181      10  FILLER                  PIC  X(44) VALUE                 ELTOBSTS
00182          'THESE SERVICES ARE RESTRICTED AS FOLLOWS:  '.           ELTOBSTS
00183                                                                   ELTOBSTS
00184    05  WS-PAYABLE-AS.                                             ELTOBSTS
00185      10  FILLER                  PIC  X(40) VALUE                 ELTOBSTS
00186          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTOBSTS
00187                                                                   ELTOBSTS
00188                                                                   ELTOBSTS
00189    05  WS-PAYMNT-BASED.                                           ELTOBSTS
00190      10  FILLER                    PIC X(20)                      ELTOBSTS
00191          VALUE 'PAYMENT IS BASED ON '.                            ELTOBSTS
00192      10  WS-DTL-PAYMNT-BASED       PIC X(56) VALUE SPACES.        ELTOBSTS
00193      10  FILLER                    PIC X(3) VALUE LOW-VALUES.     ELTOBSTS
00194                                                                   ELTOBSTS
00195    05  WS-MAX-VISITS.                                             ELTOBSTS
00196      10  FILLER                    PIC X(33)                      ELTOBSTS
00197          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE '.               ELTOBSTS
00198      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTOBSTS
00199                                                                   ELTOBSTS
00200    05  WS-UNLIMITED.                                              ELTOBSTS
00201      10  WS-DTL-UNLIMITED      PIC X(20).                         ELTOBSTS
00202      10  FILLER                PIC X(26).                         ELTOBSTS
00203    05  WS-MAX-DAYS REDEFINES WS-UNLIMITED.                        ELTOBSTS
00204      10  FILLER                PIC X.                             ELTOBSTS
00205      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTOBSTS
00206      10  FILLER                PIC X.                             ELTOBSTS
00207      10  WS-DAYS-LITERAL       PIC X(04).                         ELTOBSTS
00208      10  WS-DTL-MAX-IND        PIC X(20).                         ELTOBSTS
00209      10  FILLER                PIC X(16).                         ELTOBSTS
00210                                                                   ELTOBSTS
00211    05  WS-BASIC.                                                  ELTOBSTS
00212      10  FILLER                    PIC X(09) VALUE SPACES.        ELTOBSTS
00213      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTOBSTS
00214      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTOBSTS
00215                                                                   ELTOBSTS
00216    05  WS-BASIC-PERCENT.                                          ELTOBSTS
00217      10  FILLER                    PIC X(09) VALUE SPACES.        ELTOBSTS
00218      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTOBSTS
00219      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTOBSTS
00220                                                                   ELTOBSTS
00221    05  WS-SUPPLEMENTAL.                                           ELTOBSTS
00222      10  FILLER                    PIC X(16)                      ELTOBSTS
00223          VALUE '  SUPPLEMENTAL: '.                                ELTOBSTS
00224      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTOBSTS
00225                                                                   ELTOBSTS
00226    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTOBSTS
00227      10  FILLER                    PIC X(16)                      ELTOBSTS
00228          VALUE '  SUPPLEMENTAL: '.                                ELTOBSTS
00229      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTOBSTS
00230                                                                   ELTOBSTS
00231    05  WS-NORMAL-NEWBORN-CHGS.                                    ELTOBSTS
00232      10  FILLER                    PIC X(33)                      ELTOBSTS
00233          VALUE 'NORMAL NEWBORN BABY CHARGES ARE '.                ELTOBSTS
00234      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTOBSTS
00235                                                                   ELTOBSTS
00236    05  WS-CONTACT-CONTRACT.                                       ELTOBSTS
00237      10  FILLER                    PIC X(50)                      ELTOBSTS
00238        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTOBSTS
00239                                                                   ELTOBSTS
00240    05  WS-CONTRACT-RELATED.                                       ELTOBSTS
00241      10  FILLER                    PIC X(50) VALUE                ELTOBSTS
00242        'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS.'.      ELTOBSTS
00243                                                                   ELTOBSTS
00244    05  WS-PVE-TEXT.                                               ELTOBSTS
00245      10  FILLER                    PIC X(44)      VALUE           ELTOBSTS
00246        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTOBSTS
00247                                                                   ELTOBSTS
00248  01  WS-ACCUM-MSG1.                                               ELTOBSTS
00249      05  FILLER                  PIC  X(79) VALUE                 ELTOBSTS
00250      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTOBSTS
00251 -    'CONSIDERATIONS.'.                                           ELTOBSTS
00252                                                                   ELTOBSTS
00253  01  WS-END                            PIC X(16)  VALUE           ELTOBSTS
00254      '*** W/S ENDS ***'.                                          ELTOBSTS
00255 /             L I N K A G E   S E C T I O N                       ELTOBSTS
00256  LINKAGE SECTION.                                                 ELTOBSTS
00257  01  DFHCOMMAREA.                                                 ELTOBSTS
00258     COPY ELSCOMMC.                                                ELTOBSTS
00259 /                                                                 ELTOBSTS
00260      COPY ELSCIA2C.                                               ELTOBSTS
00261 /                                                                 ELTOBSTS
00262 ***  IO PARM AREA  ***                                            ELTOBSTS
00263      COPY ELSIOPMC.                                               ELTOBSTS
00264 /                                                                 ELTOBSTS
00265      COPY ELSKEYSC.                                               ELTOBSTS
00266 /                                                                 ELTOBSTS
00267      COPY ELSOUTPC.                                               ELTOBSTS
00268 /                                                                 ELTOBSTS
00269      COPY ELSSSCBC.                                               ELTOBSTS
00270 /                                                                 ELTOBSTS
00271      COPY ELSCMIFC.                                               ELTOBSTS
00272 /                                                                 ELTOBSTS
00273      COPY ELSCMDSC.                                               ELTOBSTS
00274 /                                                                 ELTOBSTS
00275      COPY ELSPRVNC.                                               ELTOBSTS
00276 /                                                                 ELTOBSTS
00277 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTOBSTS
00278      COPY ELSPLGSW.                                               ELTOBSTS
00279 *** BENEFIT PROVISION TABLE OF FLDS                               ELTOBSTS
00280      COPY ELSPLGTB.                                               ELTOBSTS
00281 /                                                                 ELTOBSTS
00282      COPY ELSTCWAC.                                               ELTOBSTS
00283 /        G R O U P   S P E C I F I C   R E C O R D                ELTOBSTS
00284  01  GROUP-SPECIFIC-RECORD.                                       ELTOBSTS
00285      COPY GCGROUPC.                                               ELTOBSTS
00286 /                  M A I N L I N E                                ELTOBSTS
00287  PROCEDURE DIVISION.                                              ELTOBSTS
00288                                                                   ELTOBSTS
00289 ******************************************************************ELTOBSTS
00290 *                                                                 ELTOBSTS
00291 *   PERFORM THE MAINLINE OPERATIONS.                              ELTOBSTS
00292 *                                                                 ELTOBSTS
00293 ******************************************************************ELTOBSTS
00294  0000-MAINLINE.                                                   ELTOBSTS
00295                                                                   ELTOBSTS
00296      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTOBSTS
00297          EXEC CICS ABEND                                          ELTOBSTS
00298                    ABCODE ('EL01')                                ELTOBSTS
00299          END-EXEC                                                 ELTOBSTS
00300      END-IF.                                                      ELTOBSTS
00301                                                                   ELTOBSTS
00302 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTOBSTS
00303                                                                   ELTOBSTS
00304      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTOBSTS
00305          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTOBSTS
00306      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTOBSTS
00307      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00308          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTOBSTS
00309      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTOBSTS
00310      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00311          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTOBSTS
00312      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTOBSTS
00313      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00314          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTOBSTS
00315      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTOBSTS
00316      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00317          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTOBSTS
00318      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTOBSTS
00319      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00320          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTOBSTS
00321      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTOBSTS
00322      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00323          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTOBSTS
00324      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTOBSTS
00325      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00326          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTOBSTS
00327                                                                   ELTOBSTS
00328      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTOBSTS
00329      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00330          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTOBSTS
00331                                                                   ELTOBSTS
00332      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTOBSTS
00333              (CIA-MVO * LENGTH OF PVN-BEN-PROVN-TBL).             ELTOBSTS
00334                                                                   ELTOBSTS
00335                                                                   ELTOBSTS
00336      SET CIA-STG-GETMAIN TO TRUE.                                 ELTOBSTS
00337      EXEC CICS LINK                                               ELTOBSTS
00338                PROGRAM('ELUSTGMG')                                ELTOBSTS
00339                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
00340      END-EXEC.                                                    ELTOBSTS
00341                                                                   ELTOBSTS
00342      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTOBSTS
00343      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00344          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTOBSTS
00345                                                                   ELTOBSTS
00346      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTOBSTS
00347                                                                   ELTOBSTS
00348      IF (SSB-PROV-CLASS-INST   OR  SSB-PROV-CLASS-BOTH)           ELTOBSTS
00349         PERFORM 1000-INSTITUTIONAL-IP-RTNE THRU 1000-EXIT.        ELTOBSTS
00350                                                                   ELTOBSTS
00351      IF (SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH)            ELTOBSTS
00352         PERFORM 2000-PROFESSIONAL-IP-RTNE THRU 2000-EXIT.         ELTOBSTS
00353                                                                   ELTOBSTS
00354      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTOBSTS
00355      SET CIA-STG-FREEMAIN TO TRUE.                                ELTOBSTS
00356      EXEC CICS LINK                                               ELTOBSTS
00357                PROGRAM('ELUSTGMG')                                ELTOBSTS
00358                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
00359      END-EXEC.                                                    ELTOBSTS
00360                                                                   ELTOBSTS
00361 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTOBSTS
00362                                                                   ELTOBSTS
00363      MOVE 'E'   TO  COF-FUNCTION.                                 ELTOBSTS
00364      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTOBSTS
00365                                                                   ELTOBSTS
00366      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBSTS
00367                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00368      END-EXEC.                                                    ELTOBSTS
00369                                                                   ELTOBSTS
00370      EXEC CICS RETURN   END-EXEC.                                 ELTOBSTS
00371                                                                   ELTOBSTS
00372      GOBACK.                                                      ELTOBSTS
00373                                                                   ELTOBSTS
00374 /        I N S T I T U T I O N A L  I P   R T N E                 ELTOBSTS
00375 ***************************************************************** ELTOBSTS
00376 *        I N S T I T U T I O N A L  I P   R T N E                 ELTOBSTS
00377 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTOBSTS
00378 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTOBSTS
00379 ***************************************************************** ELTOBSTS
00380  1000-INSTITUTIONAL-IP-RTNE.                                      ELTOBSTS
00381                                                                   ELTOBSTS
00382      MOVE WS-YES TO WS-FIRSTTIME-IND.                             ELTOBSTS
00383                                                                   ELTOBSTS
00384      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTOBSTS
00385      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00386              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTOBSTS
00387              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTOBSTS
00388         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00389         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00390         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00391         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00392      END-PERFORM.                                                 ELTOBSTS
00393      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTOBSTS
00394                                                                   ELTOBSTS
00395      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00396         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTOBSTS
00397         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTOBSTS
00398            SET PVN-BEN-PROVN-IDX TO WS-SUB                        ELTOBSTS
00399            MOVE WS-INST-IP-LIST (WS-SUB)                          ELTOBSTS
00400                       TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)    ELTOBSTS
00401            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTOBSTS
00402                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTOBSTS
00403      END-PERFORM.                                                 ELTOBSTS
00404                                                                   ELTOBSTS
00405      MOVE WS-HDR-2-INST-IP    TO  COF-HDR-LINE (2).               ELTOBSTS
00406                                                                   ELTOBSTS
00407      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTOBSTS
00408      MOVE +2                  TO  COF-NBR-HDR-LINES.              ELTOBSTS
00409      MOVE 'P'                 TO  COF-FUNCTION.                   ELTOBSTS
00410                                                                   ELTOBSTS
00411      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBSTS
00412                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00413      END-EXEC.                                                    ELTOBSTS
00414                                                                   ELTOBSTS
00415      MOVE WS-OB-SERVICES-ARE  TO  SSB-TOPIC-PHRASE.               ELTOBSTS
00416                                                                   ELTOBSTS
00417      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTOBSTS
00418                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00419      END-EXEC.                                                    ELTOBSTS
00420                                                                   ELTOBSTS
00421      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBSTS
00422                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00423      END-EXEC.                                                    ELTOBSTS
00424                                                                   ELTOBSTS
00425      IF PVN-COVG-NONE                                             ELTOBSTS
00426          GO TO 1000-EXIT.                                         ELTOBSTS
00427                                                                   ELTOBSTS
00428      MOVE +1  TO  WS-CIA.                                         ELTOBSTS
00429                                                                   ELTOBSTS
00430      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTOBSTS
00431            PSP-TREAT-RESTRN-IND,                                  ELTOBSTS
00432            PSP-CERTFN-REQRM-IND,                                  ELTOBSTS
00433            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTOBSTS
00434            PSP-PROVN-PRICING-METHD,                               ELTOBSTS
00435            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTOBSTS
00436            PSP-TRANSF-OTHER-RESP-IND,                             ELTOBSTS
00437            PSP-SPILL-OVER-COINS-APL-IND,                          ELTOBSTS
00438            PSP-SPILL-OVER-DED-APL-IND,                            ELTOBSTS
00439            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTOBSTS
00440            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTOBSTS
00441            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTOBSTS
00442            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTOBSTS
00443            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTOBSTS
00444            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTOBSTS
00445            PSP-BEN-TAB-PROVN-ID-PPF.                              ELTOBSTS
00446       MOVE '0' TO   PSC-BEN-SCOPE-ID,                             ELTOBSTS
00447                     PSE-BEN-SCOPE-ID,                             ELTOBSTS
00448                     PSE-BEN-MAX-VISITS-IND,                       ELTOBSTS
00449                     PSE-BEN-MAX-VISITS-DAYS.                      ELTOBSTS
00450                                                                   ELTOBSTS
00451      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTOBSTS
00452                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00453      END-EXEC.                                                    ELTOBSTS
00454                                                                   ELTOBSTS
00455      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTOBSTS
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00457         ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                       ELTOBSTS
00458                                                                   ELTOBSTS
00459      MOVE WS-NO  TO WS-DISPLAY-PAYMNT-BASED-TEXT.                 ELTOBSTS
00460                                                                   ELTOBSTS
00461      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00462         VARYING WS-SUB  FROM  +1  BY  +1                          ELTOBSTS
00463         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTOBSTS
00464              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTOBSTS
00465              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTOBSTS
00466                   PERFORM 1040-BUILD-SCREEN-LINES THRU 1040-EXIT  ELTOBSTS
00467              END-IF                                               ELTOBSTS
00468      END-PERFORM.                                                 ELTOBSTS
00469                                                                   ELTOBSTS
00470  1000-EXIT.  EXIT.                                                ELTOBSTS
00471                                                                   ELTOBSTS
00472 /                                                                 ELTOBSTS
00473  1040-BUILD-SCREEN-LINES.                                         ELTOBSTS
00474                                                                   ELTOBSTS
00475      SET PLT-INDEX1  TO                                           ELTOBSTS
00476              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTOBSTS
00477                                                                   ELTOBSTS
00478      IF WS-NOT-FIRST-TIME                                         ELTOBSTS
00479         MOVE 'P' TO COF-FUNCTION                                  ELTOBSTS
00480         MOVE +0  TO COF-NBR-DTL-LINES                             ELTOBSTS
00481         EXEC CICS LINK PROGRAM ('ELUOUTPT')                       ELTOBSTS
00482                        COMMAREA (DFHCOMMAREA)                     ELTOBSTS
00483         END-EXEC                                                  ELTOBSTS
00484                                                                   ELTOBSTS
00485      ELSE                                                         ELTOBSTS
00486        MOVE 'N' TO WS-FIRSTTIME-IND.                              ELTOBSTS
00487                                                                   ELTOBSTS
00488      MOVE  +1  TO  WS-CIA.                                        ELTOBSTS
00489                                                                   ELTOBSTS
00490      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTOBSTS
00491          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTOBSTS
00492              SET PLT-INDEX2  TO  2                                ELTOBSTS
00493          ELSE                                                     ELTOBSTS
00494            MOVE TABLE-MAX TO WS-SUB                               ELTOBSTS
00495            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTOBSTS
00496            GO TO 1040-EXIT                                        ELTOBSTS
00497      ELSE                                                         ELTOBSTS
00498         SET PLT-INDEX2  TO  1.                                    ELTOBSTS
00499                                                                   ELTOBSTS
00500      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTOBSTS
00501                                                                   ELTOBSTS
00502      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTOBSTS
00503      ADD +1                 TO WS-CIA.                            ELTOBSTS
00504      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTOBSTS
00505      ADD +1                 TO WS-CIA.                            ELTOBSTS
00506      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTOBSTS
00507         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTOBSTS
00508         UNTIL WS-SUB2  >  WS-INST-IP-CNT.                         ELTOBSTS
00509                                                                   ELTOBSTS
00510      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTOBSTS
00511               NOT = '0' OR LOW-VALUES                             ELTOBSTS
00512           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTOBSTS
00513                                                                   ELTOBSTS
00514      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBSTS
00515                                                                   ELTOBSTS
00516      MOVE WS-NO              TO WS-DISPLAY-PAYABLE-AS-TEXT.       ELTOBSTS
00517      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00518         SET  PLT-INDEX2           TO  1                           ELTOBSTS
00519         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
00520                   NOT = '19'                                      ELTOBSTS
00521                     MOVE WS-YES TO WS-DISPLAY-PAYABLE-AS-TEXT.    ELTOBSTS
00522                                                                   ELTOBSTS
00523      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00524         SET  PLT-INDEX2           TO  2                           ELTOBSTS
00525         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
00526                   NOT = '19'                                      ELTOBSTS
00527                     MOVE WS-YES TO WS-DISPLAY-PAYABLE-AS-TEXT.    ELTOBSTS
00528                                                                   ELTOBSTS
00529      IF WS-DISPLAY-PAYABLE-AS-TEXT  = WS-YES                      ELTOBSTS
00530          ADD  +1             TO  WS-CIA                           ELTOBSTS
00531          MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)             ELTOBSTS
00532          ADD  +1             TO  WS-CIA.                          ELTOBSTS
00533                                                                   ELTOBSTS
00534      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00535         SET  PLT-INDEX2           TO  1                           ELTOBSTS
00536         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
00537                   NOT = '19'                                      ELTOBSTS
00538            PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT.          ELTOBSTS
00539                                                                   ELTOBSTS
00540      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00541         SET  PLT-INDEX2             TO  2                         ELTOBSTS
00542         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
00543                   NOT = '19'                                      ELTOBSTS
00544             PERFORM 4200-PAYABLE-AS-SUPP THRU 4200-EXIT.          ELTOBSTS
00545                                                                   ELTOBSTS
00546      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTOBSTS
00547            NOT = ZERO                                             ELTOBSTS
00548         PERFORM 6115-CERT-REQ-IND                                 ELTOBSTS
00549            THRU 6115-EXIT                                         ELTOBSTS
00550      END-IF.                                                      ELTOBSTS
00551                                                                   ELTOBSTS
00552      IF PLP-TREAT-RESTRN-IND (PLT-INDEX1, 1)                      ELTOBSTS
00553            NOT = ZERO     OR                                      ELTOBSTS
00554          PLP-TREAT-RESTRN-IND (PLT-INDEX1, 2)                     ELTOBSTS
00555            NOT = ZERO                                             ELTOBSTS
00556         PERFORM 6116-TREAT-RESTRICTION                            ELTOBSTS
00557            THRU 6116-EXIT                                         ELTOBSTS
00558      END-IF.                                                      ELTOBSTS
00559                                                                   ELTOBSTS
00560      IF (PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTOBSTS
00561       AND GCG-BC-NRM-NWBRN-ELIG-IND NOT = '0')                    ELTOBSTS
00562              OR                                                   ELTOBSTS
00563         (PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTOBSTS
00564          AND GCG-MM-NRM-NWBRN-ELIG-IND NOT = '0')                 ELTOBSTS
00565           ADD  +1                      TO  WS-CIA                 ELTOBSTS
00566           MOVE WS-NORMAL-NEWBORN-CHGS  TO  COF-DTL-LINE(WS-CIA)   ELTOBSTS
00567           ADD  +1                      TO  WS-CIA.                ELTOBSTS
00568                                                                   ELTOBSTS
00569      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00570       IF GCG-BC-NRM-NWBRN-ELIG-IND NOT = '0' AND NOT = LOW-VALUES ELTOBSTS
00571         MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX      ELTOBSTS
00572         MOVE 'BC-NRM-NWBRN-ELIG-IND'   TO  CMF-ELEMENT-SYSTEM-NAMEELTOBSTS
00573         MOVE GCG-BC-NRM-NWBRN-ELIG-IND TO CMF-CODE-VALUE          ELTOBSTS
00574         EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA) ELTOBSTS
00575                END-EXEC                                           ELTOBSTS
00576         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00577         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00578             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00579       END-IF                                                      ELTOBSTS
00580       MOVE SPACES TO TCAR-FROM-AREA                               ELTOBSTS
00581       IF CMF-NBR-DESCR-LINES <  3                                 ELTOBSTS
00582          PERFORM 3105-OUTPUT-FOR-TWO-LINES                        ELTOBSTS
00583       ELSE                                                        ELTOBSTS
00584          IF CMF-NBR-DESCR-LINES = 3                               ELTOBSTS
00585             PERFORM 3205-OUTPUT-FOR-THREE-LINES                   ELTOBSTS
00586          ELSE                                                     ELTOBSTS
00587             PERFORM 3305-OUTPUT-FOR-FOUR-LINES                    ELTOBSTS
00588          END-IF                                                   ELTOBSTS
00589       END-IF                                                      ELTOBSTS
00590       MOVE +1 TO TCAR-FROM-SUB                                    ELTOBSTS
00591       MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO WS-DTL-BASIC           ELTOBSTS
00592       MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)               ELTOBSTS
00593       ADD  +1 TO TCAR-FROM-SUB                                    ELTOBSTS
00594       PERFORM 3555-OUTPUT-NEWBORN-DATA UNTIL                      ELTOBSTS
00595           TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED                 ELTOBSTS
00596       PERFORM 3000-OUTPUT-TEXT.                                   ELTOBSTS
00597                                                                   ELTOBSTS
00598      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTOBSTS
00599       IF GCG-MM-NRM-NWBRN-ELIG-IND NOT = '0' AND NOT = LOW-VALUES ELTOBSTS
00600         MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX      ELTOBSTS
00601         MOVE 'MM-NRM-NWBRN-ELIG-IND'   TO  CMF-ELEMENT-SYSTEM-NAMEELTOBSTS
00602         MOVE GCG-MM-NRM-NWBRN-ELIG-IND TO CMF-CODE-VALUE          ELTOBSTS
00603         EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA) ELTOBSTS
00604                END-EXEC                                           ELTOBSTS
00605         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00606         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00607             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00608       END-IF                                                      ELTOBSTS
00609       MOVE SPACES TO TCAR-FROM-AREA                               ELTOBSTS
00610       IF CMF-NBR-DESCR-LINES <  3                                 ELTOBSTS
00611          PERFORM 3105-OUTPUT-FOR-TWO-LINES                        ELTOBSTS
00612       ELSE                                                        ELTOBSTS
00613          IF CMF-NBR-DESCR-LINES = 3                               ELTOBSTS
00614             PERFORM 3205-OUTPUT-FOR-THREE-LINES                   ELTOBSTS
00615          ELSE                                                     ELTOBSTS
00616             PERFORM 3305-OUTPUT-FOR-FOUR-LINES                    ELTOBSTS
00617          END-IF                                                   ELTOBSTS
00618       END-IF                                                      ELTOBSTS
00619       MOVE +1 TO TCAR-FROM-SUB                                    ELTOBSTS
00620       MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO WS-DTL-SUPPLEMENTAL    ELTOBSTS
00621       MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)               ELTOBSTS
00622       ADD  +1 TO TCAR-FROM-SUB                                    ELTOBSTS
00623       PERFORM 3555-OUTPUT-NEWBORN-DATA UNTIL                      ELTOBSTS
00624           TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED                 ELTOBSTS
00625       PERFORM 3000-OUTPUT-TEXT.                                   ELTOBSTS
00626                                                                   ELTOBSTS
00627      IF GCG-NRM-NWBRN-BILG-IND NOT = '0'                          ELTOBSTS
00628         ADD +1                      TO WS-CIA                     ELTOBSTS
00629         MOVE 'GROUP'                TO  CMF-RECORD-PREFIX         ELTOBSTS
00630         MOVE 'NRM-NWBRN-BILG-IND'   TO  CMF-ELEMENT-SYSTEM-NAME   ELTOBSTS
00631         MOVE GCG-NRM-NWBRN-BILG-IND TO  CMF-CODE-VALUE            ELTOBSTS
00632         EXEC CICS  LINK PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)  ELTOBSTS
00633                 END-EXEC                                          ELTOBSTS
00634         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00635         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00636             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00637         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
00638         STRING WS-RULE-BILL-NEWBORN-CHGS ' '                      ELTOBSTS
00639                CMF-DESCR-LINE(1) ' '                              ELTOBSTS
00640                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
00641                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
00642         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
00643                                                                   ELTOBSTS
00644         MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
00645         MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
00646         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
00647         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
00648         MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
00649         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTOBSTS
00650            ADD +1                TO WS-CIA                        ELTOBSTS
00651            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTOBSTS
00652            PERFORM 3000-OUTPUT-TEXT                               ELTOBSTS
00653         ELSE                                                      ELTOBSTS
00654          PERFORM 3000-OUTPUT-TEXT.                                ELTOBSTS
00655                                                                   ELTOBSTS
00656      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00657         SET  PLT-INDEX2          TO  2                            ELTOBSTS
00658         PERFORM 4300-SPILLOVER-COINS THRU 4300-EXIT               ELTOBSTS
00659         PERFORM 4400-SPILLOVER-DEDUCT THRU 4400-EXIT              ELTOBSTS
00660         IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTOBSTS
00661             NOT = '0' AND NOT = LOW-VALUES                        ELTOBSTS
00662          ADD +1                      TO WS-CIA                    ELTOBSTS
00663          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBSTS
00664          MOVE 'SPILL-OVR-RM-F-RT-APL-IND'                         ELTOBSTS
00665                     TO CMF-ELEMENT-SYSTEM-NAME                    ELTOBSTS
00666          MOVE                                                     ELTOBSTS
00667             PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
00668                     TO CMF-CODE-VALUE                             ELTOBSTS
00669          EXEC CICS  LINK  PROGRAM('ELUCMIF')                      ELTOBSTS
00670                           COMMAREA(DFHCOMMAREA)                   ELTOBSTS
00671                   END-EXEC                                        ELTOBSTS
00672                                                                   ELTOBSTS
00673         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00674         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00675             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00676          MOVE SPACES TO TCAR-FROM-AREA                            ELTOBSTS
00677          STRING WS-SPILLOVER-FL-RT-PER-D ' '                      ELTOBSTS
00678                 CMF-DESCR-LINE(1) ' '                             ELTOBSTS
00679                 CMF-DESCR-LINE(2) ' '                             ELTOBSTS
00680                     DELIMITED BY SIZE INTO TCAR-FROM-AREA         ELTOBSTS
00681          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
00682                                                                   ELTOBSTS
00683          MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                      ELTOBSTS
00684          MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTOBSTS
00685          MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                      ELTOBSTS
00686          PERFORM TCPR-000-TEXT-UNSTRING                           ELTOBSTS
00687          MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)            ELTOBSTS
00688          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTOBSTS
00689            ADD +1                TO WS-CIA                        ELTOBSTS
00690            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTOBSTS
00691            PERFORM 3000-OUTPUT-TEXT                               ELTOBSTS
00692          ELSE                                                     ELTOBSTS
00693           PERFORM 3000-OUTPUT-TEXT.                               ELTOBSTS
00694                                                                   ELTOBSTS
00695      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00696         SET  PLT-INDEX2          TO  2                            ELTOBSTS
00697         PERFORM 4450-TRANS-OTHR-RESPON-IND  THRU                  ELTOBSTS
00698                 4450-EXIT                                         ELTOBSTS
00699      ELSE                                                         ELTOBSTS
00700         IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTOBSTS
00701            SET  PLT-INDEX2          TO  1                         ELTOBSTS
00702            PERFORM 4450-TRANS-OTHR-RESPON-IND  THRU               ELTOBSTS
00703                    4450-EXIT.                                     ELTOBSTS
00704                                                                   ELTOBSTS
00705      PERFORM 4600-SCAN-TAB.                                       ELTOBSTS
00706                                                                   ELTOBSTS
00707  1040-EXIT.  EXIT.                                                ELTOBSTS
00708 /                                                                 ELTOBSTS
00709  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTOBSTS
00710      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTOBSTS
00711                                                                   ELTOBSTS
00712      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTOBSTS
00713         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBSTS
00714         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTOBSTS
00715         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTOBSTS
00716                                                   CMF-CODE-VALUE  ELTOBSTS
00717         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTOBSTS
00718                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
00719                END-EXEC                                           ELTOBSTS
00720         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00721         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00722             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00723         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
00724         STRING CMF-DESCR-LINE(1) ' '                              ELTOBSTS
00725                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
00726                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
00727         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
00728                                                                   ELTOBSTS
00729         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
00730         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
00731         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
00732         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
00733         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTOBSTS
00734         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTOBSTS
00735         IF WS-CIA < 20                                            ELTOBSTS
00736            ADD +1  TO  WS-CIA                                     ELTOBSTS
00737            MOVE ZERO  TO                                          ELTOBSTS
00738                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTOBSTS
00739         ELSE                                                      ELTOBSTS
00740            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTOBSTS
00741                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
00742                END-EXEC                                           ELTOBSTS
00743            MOVE +1  TO  WS-CIA                                    ELTOBSTS
00744            MOVE ZERO  TO                                          ELTOBSTS
00745                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTOBSTS
00746                                                                   ELTOBSTS
00747  1090-PROBLEM-WITH-INDICES.                                       ELTOBSTS
00748                                                                   ELTOBSTS
00749      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTOBSTS
00750      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTOBSTS
00751                                                                   ELTOBSTS
00752      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTOBSTS
00753      MOVE 'P'  TO  COF-FUNCTION.                                  ELTOBSTS
00754                                                                   ELTOBSTS
00755      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTOBSTS
00756              END-EXEC.                                            ELTOBSTS
00757                                                                   ELTOBSTS
00758                                                                   ELTOBSTS
00759 /        P R O F E S S I O N A L   I P   R T N E                  ELTOBSTS
00760 ***************************************************************** ELTOBSTS
00761 *        P R O F E S S I O N A L   I P   R T N E                  ELTOBSTS
00762 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTOBSTS
00763 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTOBSTS
00764 ***************************************************************** ELTOBSTS
00765  2000-PROFESSIONAL-IP-RTNE.                                       ELTOBSTS
00766                                                                   ELTOBSTS
00767      MOVE WS-YES TO WS-FIRSTTIME-IND.                             ELTOBSTS
00768                                                                   ELTOBSTS
00769      MOVE TABLE-MAX     TO  PVN-NBR-BEN-PROVN.                    ELTOBSTS
00770      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00771              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTOBSTS
00772              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTOBSTS
00773         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00774         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00775         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00776         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTOBSTS
00777      END-PERFORM.                                                 ELTOBSTS
00778      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTOBSTS
00779                                                                   ELTOBSTS
00780      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00781         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTOBSTS
00782         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTOBSTS
00783             SET PVN-BEN-PROVN-IDX TO WS-SUB                       ELTOBSTS
00784             MOVE WS-PROF-IP-LIST (WS-SUB)                         ELTOBSTS
00785                     TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)      ELTOBSTS
00786             MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)   ELTOBSTS
00787                            PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   ELTOBSTS
00788      END-PERFORM.                                                 ELTOBSTS
00789                                                                   ELTOBSTS
00790      MOVE WS-HDR-2-PROF-IP    TO  COF-HDR-LINE (2).               ELTOBSTS
00791                                                                   ELTOBSTS
00792      MOVE +0                  TO  COF-NBR-DTL-LINES.              ELTOBSTS
00793      MOVE +2                  TO  COF-NBR-HDR-LINES.              ELTOBSTS
00794      MOVE 'P'                 TO  COF-FUNCTION.                   ELTOBSTS
00795                                                                   ELTOBSTS
00796      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBSTS
00797                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00798      END-EXEC.                                                    ELTOBSTS
00799                                                                   ELTOBSTS
00800      MOVE WS-OB-SERVICES-ARE  TO  SSB-TOPIC-PHRASE.               ELTOBSTS
00801                                                                   ELTOBSTS
00802      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTOBSTS
00803                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00804      END-EXEC.                                                    ELTOBSTS
00805                                                                   ELTOBSTS
00806      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBSTS
00807                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00808      END-EXEC.                                                    ELTOBSTS
00809                                                                   ELTOBSTS
00810      IF PVN-COVG-NONE                                             ELTOBSTS
00811          GO TO 2000-EXIT.                                         ELTOBSTS
00812                                                                   ELTOBSTS
00813      MOVE +1  TO  WS-CIA.                                         ELTOBSTS
00814                                                                   ELTOBSTS
00815      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTOBSTS
00816            PSP-PROVN-PRICING-METHD,                               ELTOBSTS
00817            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTOBSTS
00818            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTOBSTS
00819            PSP-TRANSF-OTHER-RESP-IND,                             ELTOBSTS
00820            PSP-TREAT-RESTRN-IND,                                  ELTOBSTS
00821            PSP-CERTFN-REQRM-IND,                                  ELTOBSTS
00822            PSP-SPILL-OVER-COINS-APL-IND,                          ELTOBSTS
00823            PSP-SPILL-OVER-DED-APL-IND,                            ELTOBSTS
00824            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTOBSTS
00825            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTOBSTS
00826            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTOBSTS
00827            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTOBSTS
00828            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTOBSTS
00829            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTOBSTS
00830            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTOBSTS
00831            PSC-BEN-SCOPE-ID,                                      ELTOBSTS
00832            PSE-BEN-SCOPE-ID,                                      ELTOBSTS
00833            PSE-BEN-MAX-VISITS-IND,                                ELTOBSTS
00834            PSE-BEN-MAX-VISITS-DAYS.                               ELTOBSTS
00835      MOVE ZERO TO  PSP-SPILL-OVR-RM-F-RT-APL-IND.                 ELTOBSTS
00836                                                                   ELTOBSTS
00837                                                                   ELTOBSTS
00838      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTOBSTS
00839                     COMMAREA (DFHCOMMAREA)                        ELTOBSTS
00840      END-EXEC.                                                    ELTOBSTS
00841                                                                   ELTOBSTS
00842      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTOBSTS
00843      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
00844          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTOBSTS
00845                                                                   ELTOBSTS
00846      MOVE WS-NO  TO WS-DISPLAY-PAYMNT-BASED-TEXT.                 ELTOBSTS
00847                                                                   ELTOBSTS
00848      PERFORM WITH TEST BEFORE                                     ELTOBSTS
00849         VARYING WS-SUB  FROM  +1  BY  +1                          ELTOBSTS
00850         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTOBSTS
00851              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTOBSTS
00852              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTOBSTS
00853                   PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT  ELTOBSTS
00854              END-IF                                               ELTOBSTS
00855      END-PERFORM.                                                 ELTOBSTS
00856                                                                   ELTOBSTS
00857  2000-EXIT.  EXIT.                                                ELTOBSTS
00858                                                                   ELTOBSTS
00859 /                                                                 ELTOBSTS
00860  2040-BUILD-SCREEN-LINES.                                         ELTOBSTS
00861                                                                   ELTOBSTS
00862      SET PLT-INDEX1  TO                                           ELTOBSTS
00863              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTOBSTS
00864                                                                   ELTOBSTS
00865      IF WS-NOT-FIRST-TIME                                         ELTOBSTS
00866         MOVE 'P' TO COF-FUNCTION                                  ELTOBSTS
00867         MOVE +0  TO COF-NBR-DTL-LINES                             ELTOBSTS
00868         EXEC CICS LINK PROGRAM ('ELUOUTPT')                       ELTOBSTS
00869                        COMMAREA (DFHCOMMAREA)                     ELTOBSTS
00870         END-EXEC                                                  ELTOBSTS
00871                                                                   ELTOBSTS
00872      ELSE                                                         ELTOBSTS
00873        MOVE 'N' TO WS-FIRSTTIME-IND.                              ELTOBSTS
00874                                                                   ELTOBSTS
00875      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBSTS
00876         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTOBSTS
00877            SET PLT-INDEX2  TO  2                                  ELTOBSTS
00878         ELSE                                                      ELTOBSTS
00879            MOVE TABLE-MAX TO WS-SUB                               ELTOBSTS
00880            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTOBSTS
00881            GO TO 2040-EXIT                                        ELTOBSTS
00882      ELSE                                                         ELTOBSTS
00883         SET PLT-INDEX2  TO  1.                                    ELTOBSTS
00884                                                                   ELTOBSTS
00885      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTOBSTS
00886                                                                   ELTOBSTS
00887      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTOBSTS
00888      ADD +1                 TO WS-CIA.                            ELTOBSTS
00889      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTOBSTS
00890      ADD +1                 TO WS-CIA.                            ELTOBSTS
00891      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTOBSTS
00892         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTOBSTS
00893         UNTIL WS-SUB2  >  WS-PROF-IP-CNT.                         ELTOBSTS
00894                                                                   ELTOBSTS
00895      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTOBSTS
00896                 NOT = '0' OR LOW-VALUES                           ELTOBSTS
00897           PERFORM 4000-PLACE-OF-TREATMENT.                        ELTOBSTS
00898                                                                   ELTOBSTS
00899                                                                   ELTOBSTS
00900      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBSTS
00901      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00902        SET  PLT-INDEX2       TO  1                                ELTOBSTS
00903        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                    ELTOBSTS
00904         IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTOBSTS
00905            AND NOT = LOW-VALUES                                   ELTOBSTS
00906              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTOBSTS
00907                                                                   ELTOBSTS
00908      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00909       SET  PLT-INDEX2       TO  1                                 ELTOBSTS
00910       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTOBSTS
00911        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO     ELTOBSTS
00912            AND NOT = LOW-VALUES                                   ELTOBSTS
00913              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTOBSTS
00914                                                                   ELTOBSTS
00915      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00916        SET PLT-INDEX2        TO 2                                 ELTOBSTS
00917        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                    ELTOBSTS
00918         IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTOBSTS
00919            AND NOT = LOW-VALUES                                   ELTOBSTS
00920              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTOBSTS
00921                                                                   ELTOBSTS
00922      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
00923       SET PLT-INDEX2        TO 2                                  ELTOBSTS
00924       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTOBSTS
00925        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO     ELTOBSTS
00926            AND NOT = LOW-VALUES                                   ELTOBSTS
00927              MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.         ELTOBSTS
00928                                                                   ELTOBSTS
00929      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTOBSTS
00930          ADD  +1               TO  WS-CIA                         ELTOBSTS
00931          MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)           ELTOBSTS
00932          ADD  +1               TO  WS-CIA                         ELTOBSTS
00933          MOVE WS-NO            TO  WS-DISPLAY-PAYMNT-BASED-TEXT.  ELTOBSTS
00934                                                                   ELTOBSTS
00935      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00936        SET  PLT-INDEX2       TO  1                                ELTOBSTS
00937        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                    ELTOBSTS
00938         IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO    ELTOBSTS
00939            AND NOT = LOW-VALUES                                   ELTOBSTS
00940          MOVE 'BPC' TO CMF-RECORD-PREFIX                          ELTOBSTS
00941          MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO        ELTOBSTS
00942                                CMF-CODE-VALUE                     ELTOBSTS
00943          MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME         ELTOBSTS
00944          EXEC CICS  LINK  PROGRAM('ELUCMIF')                      ELTOBSTS
00945                 COMMAREA(DFHCOMMAREA)                             ELTOBSTS
00946                 END-EXEC                                          ELTOBSTS
00947          SET CIA-ELSCMDSC-DDN TO TRUE                             ELTOBSTS
00948          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTOBSTS
00949             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00950          MOVE SPACES TO TCAR-FROM-AREA                            ELTOBSTS
00951          STRING CMF-DESCR-LINE(1) ' '                             ELTOBSTS
00952                 CMF-DESCR-LINE(2) ' '                             ELTOBSTS
00953                     DELIMITED BY SIZE INTO TCAR-FROM-AREA         ELTOBSTS
00954          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
00955                                                                   ELTOBSTS
00956          MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                      ELTOBSTS
00957          MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTOBSTS
00958          MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                      ELTOBSTS
00959          PERFORM TCPR-000-TEXT-UNSTRING                           ELTOBSTS
00960          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTOBSTS
00961          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTOBSTS
00962          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTOBSTS
00963             ADD +1                TO WS-CIA                       ELTOBSTS
00964             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTOBSTS
00965             PERFORM 3000-OUTPUT-TEXT                              ELTOBSTS
00966          ELSE                                                     ELTOBSTS
00967           PERFORM 3000-OUTPUT-TEXT.                               ELTOBSTS
00968                                                                   ELTOBSTS
00969      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
00970       SET  PLT-INDEX2       TO  1                                 ELTOBSTS
00971       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTOBSTS
00972        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO     ELTOBSTS
00973            AND NOT = LOW-VALUES                                   ELTOBSTS
00974         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTOBSTS
00975         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTOBSTS
00976                               CMF-CODE-VALUE                      ELTOBSTS
00977         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTOBSTS
00978         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTOBSTS
00979                 COMMAREA(DFHCOMMAREA)                             ELTOBSTS
00980                 END-EXEC                                          ELTOBSTS
00981         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
00982         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
00983             ADDRESS OF CMF-DESCR                                  ELTOBSTS
00984         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
00985         STRING CMF-DESCR-LINE(1) ' '                              ELTOBSTS
00986                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
00987                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
00988         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
00989                                                                   ELTOBSTS
00990         MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
00991         MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
00992         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
00993         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
00994         MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                     ELTOBSTS
00995         MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
00996         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTOBSTS
00997             ADD +1                TO WS-CIA                       ELTOBSTS
00998             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTOBSTS
00999             PERFORM 3000-OUTPUT-TEXT                              ELTOBSTS
01000         ELSE                                                      ELTOBSTS
01001           PERFORM 3000-OUTPUT-TEXT.                               ELTOBSTS
01002                                                                   ELTOBSTS
01003      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01004       SET PLT-INDEX2        TO 2                                  ELTOBSTS
01005       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                     ELTOBSTS
01006        IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO     ELTOBSTS
01007            AND NOT = LOW-VALUES                                   ELTOBSTS
01008         MOVE 'BPC' TO CMF-RECORD-PREFIX                           ELTOBSTS
01009         MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTOBSTS
01010                                CMF-CODE-VALUE                     ELTOBSTS
01011         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTOBSTS
01012         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTOBSTS
01013                 COMMAREA(DFHCOMMAREA)                             ELTOBSTS
01014                 END-EXEC                                          ELTOBSTS
01015         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
01016         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
01017             ADDRESS OF CMF-DESCR                                  ELTOBSTS
01018         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
01019         STRING CMF-DESCR-LINE(1) ' '                              ELTOBSTS
01020                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
01021                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
01022         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
01023                                                                   ELTOBSTS
01024         MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
01025         MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
01026         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
01027         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
01028         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL              ELTOBSTS
01029         MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
01030         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTOBSTS
01031             ADD +1                TO WS-CIA                       ELTOBSTS
01032             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTOBSTS
01033             PERFORM 3000-OUTPUT-TEXT                              ELTOBSTS
01034         ELSE                                                      ELTOBSTS
01035           PERFORM 3000-OUTPUT-TEXT.                               ELTOBSTS
01036                                                                   ELTOBSTS
01037      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01038       SET PLT-INDEX2        TO 2                                  ELTOBSTS
01039       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTOBSTS
01040        IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2) NOT = ZERO     ELTOBSTS
01041            AND NOT = LOW-VALUES                                   ELTOBSTS
01042         MOVE 'BPE' TO CMF-RECORD-PREFIX                           ELTOBSTS
01043         MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO         ELTOBSTS
01044                               CMF-CODE-VALUE                      ELTOBSTS
01045         MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME          ELTOBSTS
01046         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTOBSTS
01047                 COMMAREA(DFHCOMMAREA)                             ELTOBSTS
01048                 END-EXEC                                          ELTOBSTS
01049         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
01050         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
01051             ADDRESS OF CMF-DESCR                                  ELTOBSTS
01052         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
01053         STRING CMF-DESCR-LINE(1) ' '                              ELTOBSTS
01054                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
01055                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
01056         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
01057                                                                   ELTOBSTS
01058         MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
01059         MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
01060         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
01061         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
01062         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL              ELTOBSTS
01063         MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
01064         IF TCAR-OUTPUT-FIELDS-USED > 1                            ELTOBSTS
01065             ADD +1                TO WS-CIA                       ELTOBSTS
01066             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTOBSTS
01067             PERFORM 3000-OUTPUT-TEXT                              ELTOBSTS
01068         ELSE                                                      ELTOBSTS
01069           PERFORM 3000-OUTPUT-TEXT.                               ELTOBSTS
01070                                                                   ELTOBSTS
01071      MOVE WS-NO              TO WS-DISPLAY-PAYABLE-AS-TEXT.       ELTOBSTS
01072      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
01073         SET  PLT-INDEX2           TO  1                           ELTOBSTS
01074         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01075                   NOT = '19'                                      ELTOBSTS
01076                     MOVE WS-YES TO WS-DISPLAY-PAYABLE-AS-TEXT.    ELTOBSTS
01077                                                                   ELTOBSTS
01078      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01079         SET  PLT-INDEX2           TO  2                           ELTOBSTS
01080         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01081                   NOT = '19'                                      ELTOBSTS
01082                     MOVE WS-YES TO WS-DISPLAY-PAYABLE-AS-TEXT.    ELTOBSTS
01083                                                                   ELTOBSTS
01084      IF WS-DISPLAY-PAYABLE-AS-TEXT  = WS-YES                      ELTOBSTS
01085          ADD  +1             TO  WS-CIA                           ELTOBSTS
01086          MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)             ELTOBSTS
01087          ADD  +1             TO  WS-CIA.                          ELTOBSTS
01088                                                                   ELTOBSTS
01089      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
01090         SET  PLT-INDEX2           TO  1                           ELTOBSTS
01091         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01092                   NOT = '19'                                      ELTOBSTS
01093                     PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT. ELTOBSTS
01094                                                                   ELTOBSTS
01095      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01096         SET  PLT-INDEX2             TO  2                         ELTOBSTS
01097         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01098                   NOT = '19'                                      ELTOBSTS
01099                     PERFORM 4200-PAYABLE-AS-SUPP THRU 4200-EXIT.  ELTOBSTS
01100                                                                   ELTOBSTS
01101      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
01102       SET  PLT-INDEX2           TO  1                             ELTOBSTS
01103       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTOBSTS
01104                              AND NOT = LOW-VALUES                 ELTOBSTS
01105         ADD +1                      TO WS-CIA                     ELTOBSTS
01106         MOVE 'BPE'                TO  CMF-RECORD-PREFIX           ELTOBSTS
01107         MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTOBSTS
01108         PERFORM 4500-MAX-VISITS                                   ELTOBSTS
01109         IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01110                 = ZEROS                                           ELTOBSTS
01111           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTOBSTS
01112           MOVE WS-UNLIMITED           TO  WS-DTL-BASIC            ELTOBSTS
01113           MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                 ELTOBSTS
01114           PERFORM 3000-OUTPUT-TEXT                                ELTOBSTS
01115         ELSE                                                      ELTOBSTS
01116           ADD +1                      TO WS-CIA                   ELTOBSTS
01117           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2) TO ELTOBSTS
01118                   WS-DTL-MAX-DAYS                                 ELTOBSTS
01119           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTOBSTS
01120           MOVE SPACES          TO TCAR-FROM-AREA                  ELTOBSTS
01121           STRING WS-DTL-MAX-DAYS ' '                              ELTOBSTS
01122                 WS-DAYS ' '                                       ELTOBSTS
01123                 WS-DTL-MAX-IND                                    ELTOBSTS
01124                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTOBSTS
01125           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTOBSTS
01126           MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT         ELTOBSTS
01127           MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN         ELTOBSTS
01128           MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN         ELTOBSTS
01129           PERFORM TCPR-000-TEXT-UNSTRING                          ELTOBSTS
01130           MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                   ELTOBSTS
01131           MOVE WS-BASIC                TO  COF-DTL-LINE(WS-CIA)   ELTOBSTS
01132           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTOBSTS
01133               ADD +1                TO WS-CIA                     ELTOBSTS
01134               MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)       ELTOBSTS
01135               PERFORM 3000-OUTPUT-TEXT                            ELTOBSTS
01136           ELSE                                                    ELTOBSTS
01137             PERFORM 3000-OUTPUT-TEXT.                             ELTOBSTS
01138                                                                   ELTOBSTS
01139      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01140       SET  PLT-INDEX2           TO  2                             ELTOBSTS
01141       IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTOBSTS
01142                        AND NOT = LOW-VALUES                       ELTOBSTS
01143         ADD +1                      TO WS-CIA                     ELTOBSTS
01144         PERFORM 4500-MAX-VISITS                                   ELTOBSTS
01145          IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTOBSTS
01146                  = ZEROS                                          ELTOBSTS
01147           MOVE TCAR-OPF-DATA(1)       TO WS-DTL-UNLIMITED         ELTOBSTS
01148           MOVE WS-UNLIMITED           TO  WS-DTL-SUPPLEMENTAL     ELTOBSTS
01149           MOVE WS-SUPPLEMENTAL        TO  COF-DTL-LINE(WS-CIA)    ELTOBSTS
01150           PERFORM 3000-OUTPUT-TEXT                                ELTOBSTS
01151          ELSE                                                     ELTOBSTS
01152           MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2) TO ELTOBSTS
01153                   WS-DTL-MAX-DAYS                                 ELTOBSTS
01154           MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND            ELTOBSTS
01155           MOVE SPACES          TO TCAR-FROM-AREA                  ELTOBSTS
01156           STRING WS-DTL-MAX-DAYS ' '                              ELTOBSTS
01157                 WS-DAYS ' '                                       ELTOBSTS
01158                 WS-DTL-MAX-IND                                    ELTOBSTS
01159                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTOBSTS
01160           PERFORM TCPR-000-TEXT-COMPRESSION                       ELTOBSTS
01161           MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT         ELTOBSTS
01162           MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN         ELTOBSTS
01163           MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN         ELTOBSTS
01164           PERFORM TCPR-000-TEXT-UNSTRING                          ELTOBSTS
01165           MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL            ELTOBSTS
01166           MOVE WS-SUPPLEMENTAL         TO  COF-DTL-LINE(WS-CIA)   ELTOBSTS
01167           IF TCAR-OUTPUT-FIELDS-USED > 1                          ELTOBSTS
01168               ADD +1                TO WS-CIA                     ELTOBSTS
01169               MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)       ELTOBSTS
01170               PERFORM 3000-OUTPUT-TEXT                            ELTOBSTS
01171           ELSE                                                    ELTOBSTS
01172             PERFORM 3000-OUTPUT-TEXT.                             ELTOBSTS
01173                                                                   ELTOBSTS
01174      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTOBSTS
01175            NOT = ZERO                                             ELTOBSTS
01176          PERFORM 6115-CERT-REQ-IND                                ELTOBSTS
01177              THRU 6115-EXIT                                       ELTOBSTS
01178      END-IF.                                                      ELTOBSTS
01179                                                                   ELTOBSTS
01180      IF PLP-TREAT-RESTRN-IND (PLT-INDEX1, 1)                      ELTOBSTS
01181            NOT = ZERO     OR                                      ELTOBSTS
01182          PLP-TREAT-RESTRN-IND (PLT-INDEX1, 2)                     ELTOBSTS
01183            NOT = ZERO                                             ELTOBSTS
01184          PERFORM 6116-TREAT-RESTRICTION                           ELTOBSTS
01185            THRU 6116-EXIT                                         ELTOBSTS
01186      END-IF.                                                      ELTOBSTS
01187                                                                   ELTOBSTS
01188      IF (PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTOBSTS
01189       AND GCG-BS-NRM-NWBRN-ELIG-IND NOT = '0')                    ELTOBSTS
01190              OR                                                   ELTOBSTS
01191         (PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTOBSTS
01192          AND GCG-MM-NRM-NWBRN-ELIG-IND NOT = '0')                 ELTOBSTS
01193           ADD  +1                      TO  WS-CIA                 ELTOBSTS
01194           MOVE WS-NORMAL-NEWBORN-CHGS  TO  COF-DTL-LINE(WS-CIA)   ELTOBSTS
01195           ADD  +1                      TO  WS-CIA.                ELTOBSTS
01196                                                                   ELTOBSTS
01197      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
01198       IF GCG-BS-NRM-NWBRN-ELIG-IND NOT = '0' AND NOT = LOW-VALUES ELTOBSTS
01199         MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX      ELTOBSTS
01200         MOVE 'BS-NRM-NWBRN-ELIG-IND'   TO  CMF-ELEMENT-SYSTEM-NAMEELTOBSTS
01201         MOVE GCG-BS-NRM-NWBRN-ELIG-IND TO CMF-CODE-VALUE          ELTOBSTS
01202         EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA) ELTOBSTS
01203                 END-EXEC                                          ELTOBSTS
01204         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
01205         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
01206             ADDRESS OF CMF-DESCR                                  ELTOBSTS
01207       END-IF                                                      ELTOBSTS
01208       MOVE SPACES TO TCAR-FROM-AREA                               ELTOBSTS
01209       IF CMF-NBR-DESCR-LINES <  3                                 ELTOBSTS
01210          PERFORM 3105-OUTPUT-FOR-TWO-LINES                        ELTOBSTS
01211       ELSE                                                        ELTOBSTS
01212          IF CMF-NBR-DESCR-LINES = 3                               ELTOBSTS
01213             PERFORM 3205-OUTPUT-FOR-THREE-LINES                   ELTOBSTS
01214          ELSE                                                     ELTOBSTS
01215             PERFORM 3305-OUTPUT-FOR-FOUR-LINES                    ELTOBSTS
01216          END-IF                                                   ELTOBSTS
01217       END-IF                                                      ELTOBSTS
01218       MOVE +1 TO TCAR-FROM-SUB                                    ELTOBSTS
01219       MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO WS-DTL-BASIC           ELTOBSTS
01220       MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)               ELTOBSTS
01221       ADD  +1 TO TCAR-FROM-SUB                                    ELTOBSTS
01222       PERFORM 3555-OUTPUT-NEWBORN-DATA UNTIL                      ELTOBSTS
01223           TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED                 ELTOBSTS
01224       PERFORM 3000-OUTPUT-TEXT.                                   ELTOBSTS
01225                                                                   ELTOBSTS
01226      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTOBSTS
01227       IF GCG-MM-NRM-NWBRN-ELIG-IND NOT = '0' AND NOT = LOW-VALUES ELTOBSTS
01228         MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX      ELTOBSTS
01229         MOVE 'MM-NRM-NWBRN-ELIG-IND'   TO  CMF-ELEMENT-SYSTEM-NAMEELTOBSTS
01230         MOVE GCG-MM-NRM-NWBRN-ELIG-IND TO CMF-CODE-VALUE          ELTOBSTS
01231         EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA) ELTOBSTS
01232                 END-EXEC                                          ELTOBSTS
01233         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
01234         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
01235             ADDRESS OF CMF-DESCR                                  ELTOBSTS
01236       END-IF                                                      ELTOBSTS
01237       MOVE SPACES TO TCAR-FROM-AREA                               ELTOBSTS
01238       IF CMF-NBR-DESCR-LINES <  3                                 ELTOBSTS
01239          PERFORM 3105-OUTPUT-FOR-TWO-LINES                        ELTOBSTS
01240       ELSE                                                        ELTOBSTS
01241          IF CMF-NBR-DESCR-LINES = 3                               ELTOBSTS
01242             PERFORM 3205-OUTPUT-FOR-THREE-LINES                   ELTOBSTS
01243          ELSE                                                     ELTOBSTS
01244             PERFORM 3305-OUTPUT-FOR-FOUR-LINES                    ELTOBSTS
01245          END-IF                                                   ELTOBSTS
01246       END-IF                                                      ELTOBSTS
01247       MOVE +1 TO TCAR-FROM-SUB                                    ELTOBSTS
01248       MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO WS-DTL-SUPPLEMENTAL    ELTOBSTS
01249       MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)               ELTOBSTS
01250       ADD  +1 TO TCAR-FROM-SUB                                    ELTOBSTS
01251       PERFORM 3555-OUTPUT-NEWBORN-DATA UNTIL                      ELTOBSTS
01252           TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED                 ELTOBSTS
01253       PERFORM 3000-OUTPUT-TEXT.                                   ELTOBSTS
01254                                                                   ELTOBSTS
01255      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01256          SET  PLT-INDEX2          TO  2                           ELTOBSTS
01257          PERFORM 4300-SPILLOVER-COINS THRU 4300-EXIT              ELTOBSTS
01258          PERFORM 4400-SPILLOVER-DEDUCT THRU 4400-EXIT.            ELTOBSTS
01259                                                                   ELTOBSTS
01260      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
01261         SET  PLT-INDEX2          TO  2                            ELTOBSTS
01262         PERFORM 4450-TRANS-OTHR-RESPON-IND  THRU                  ELTOBSTS
01263                 4450-EXIT                                         ELTOBSTS
01264      ELSE                                                         ELTOBSTS
01265         IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)     NOT =  ZERO    ELTOBSTS
01266            SET  PLT-INDEX2          TO  1                         ELTOBSTS
01267            PERFORM 4450-TRANS-OTHR-RESPON-IND THRU                ELTOBSTS
01268                    4450-EXIT.                                     ELTOBSTS
01269                                                                   ELTOBSTS
01270      PERFORM 4600-SCAN-TAB.                                       ELTOBSTS
01271                                                                   ELTOBSTS
01272  2040-EXIT.  EXIT.                                                ELTOBSTS
01273 /                                                                 ELTOBSTS
01274  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTOBSTS
01275      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTOBSTS
01276                                                                   ELTOBSTS
01277      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTOBSTS
01278         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBSTS
01279         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTOBSTS
01280         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTOBSTS
01281                                                   CMF-CODE-VALUE  ELTOBSTS
01282         EXEC CICS  LINK  PROGRAM('ELUCMIF')                       ELTOBSTS
01283                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
01284                END-EXEC                                           ELTOBSTS
01285         SET CIA-ELSCMDSC-DDN TO TRUE                              ELTOBSTS
01286         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBSTS
01287             ADDRESS OF CMF-DESCR                                  ELTOBSTS
01288         MOVE SPACES TO TCAR-FROM-AREA                             ELTOBSTS
01289         STRING CMF-DESCR-LINE(1) ' '                              ELTOBSTS
01290                CMF-DESCR-LINE(2) ' '                              ELTOBSTS
01291                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTOBSTS
01292         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTOBSTS
01293                                                                   ELTOBSTS
01294         MOVE +02 TO TCAR-OUTPUT-FIELD-COUNT                       ELTOBSTS
01295         MOVE +55 TO TCAR-OUTPUT-FIELD-1-LEN                       ELTOBSTS
01296         MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN                       ELTOBSTS
01297         PERFORM TCPR-000-TEXT-UNSTRING                            ELTOBSTS
01298         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SERVICES-2ND              ELTOBSTS
01299         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTOBSTS
01300         IF WS-CIA < 20                                            ELTOBSTS
01301            ADD +1  TO  WS-CIA                                     ELTOBSTS
01302            MOVE ZERO  TO                                          ELTOBSTS
01303                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTOBSTS
01304         ELSE                                                      ELTOBSTS
01305            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTOBSTS
01306                COMMAREA(DFHCOMMAREA)                              ELTOBSTS
01307                END-EXEC                                           ELTOBSTS
01308            MOVE +1  TO  WS-CIA                                    ELTOBSTS
01309            MOVE ZERO  TO                                          ELTOBSTS
01310                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTOBSTS
01311                                                                   ELTOBSTS
01312  2090-PROBLEM-WITH-INDICES.                                       ELTOBSTS
01313                                                                   ELTOBSTS
01314      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTOBSTS
01315      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTOBSTS
01316                                                                   ELTOBSTS
01317      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTOBSTS
01318      MOVE 'P'  TO  COF-FUNCTION.                                  ELTOBSTS
01319                                                                   ELTOBSTS
01320      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTOBSTS
01321             END-EXEC.                                             ELTOBSTS
01322                                                                   ELTOBSTS
01323 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTOBSTS
01324  3000-OUTPUT-TEXT.                                                ELTOBSTS
01325      MOVE WS-CIA TO COF-NBR-DTL-LINES.                            ELTOBSTS
01326      MOVE ' '    TO  COF-FUNCTION.                                ELTOBSTS
01327                                                                   ELTOBSTS
01328      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTOBSTS
01329             END-EXEC.                                             ELTOBSTS
01330                                                                   ELTOBSTS
01331      MOVE +1     TO  WS-CIA.                                      ELTOBSTS
01332  3000-EXIT.            EXIT.                                      ELTOBSTS
01333 *                                                                 ELTOBSTS
01334  3105-OUTPUT-FOR-TWO-LINES.                                       ELTOBSTS
01335       STRING CMF-DESCR-LINE(1) ' '                                ELTOBSTS
01336              CMF-DESCR-LINE(2) ' '                                ELTOBSTS
01337                  DELIMITED BY SIZE INTO TCAR-FROM-AREA.           ELTOBSTS
01338       PERFORM TCPR-000-TEXT-COMPRESSION.                          ELTOBSTS
01339       MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                        ELTOBSTS
01340       MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN.                        ELTOBSTS
01341       MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                        ELTOBSTS
01342       PERFORM TCPR-000-TEXT-UNSTRING.                             ELTOBSTS
01343  3105-EXIT.     EXIT.                                             ELTOBSTS
01344 *                                                                 ELTOBSTS
01345  3205-OUTPUT-FOR-THREE-LINES.                                     ELTOBSTS
01346       STRING CMF-DESCR-LINE(1) ' '                                ELTOBSTS
01347              CMF-DESCR-LINE(2) ' '                                ELTOBSTS
01348              CMF-DESCR-LINE(3) ' '                                ELTOBSTS
01349                  DELIMITED BY SIZE INTO TCAR-FROM-AREA.           ELTOBSTS
01350       PERFORM TCPR-000-TEXT-COMPRESSION.                          ELTOBSTS
01351       MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                        ELTOBSTS
01352       MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN.                        ELTOBSTS
01353       MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                        ELTOBSTS
01354       MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                        ELTOBSTS
01355       PERFORM TCPR-000-TEXT-UNSTRING.                             ELTOBSTS
01356  3205-EXIT.     EXIT.                                             ELTOBSTS
01357 *                                                                 ELTOBSTS
01358  3305-OUTPUT-FOR-FOUR-LINES.                                      ELTOBSTS
01359       STRING CMF-DESCR-LINE(1) ' '                                ELTOBSTS
01360              CMF-DESCR-LINE(2) ' '                                ELTOBSTS
01361              CMF-DESCR-LINE(3) ' '                                ELTOBSTS
01362              CMF-DESCR-LINE(4) ' '                                ELTOBSTS
01363                  DELIMITED BY SIZE INTO TCAR-FROM-AREA.           ELTOBSTS
01364       PERFORM TCPR-000-TEXT-COMPRESSION.                          ELTOBSTS
01365       MOVE +20 TO TCAR-OUTPUT-FIELD-COUNT.                        ELTOBSTS
01366       MOVE +63 TO TCAR-OUTPUT-FIELD-1-LEN.                        ELTOBSTS
01367       MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                        ELTOBSTS
01368       MOVE +79 TO TCAR-OUTPUT-FIELD-3-LEN.                        ELTOBSTS
01369       MOVE +79 TO TCAR-OUTPUT-FIELD-4-LEN.                        ELTOBSTS
01370       PERFORM TCPR-000-TEXT-UNSTRING.                             ELTOBSTS
01371  3305-EXIT.     EXIT.                                             ELTOBSTS
01372 /                                                                 ELTOBSTS
01373  3555-OUTPUT-NEWBORN-DATA.                                        ELTOBSTS
01374       ADD +1 TO WS-CIA.                                           ELTOBSTS
01375       MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO COF-DTL-LINE(WS-CIA).  ELTOBSTS
01376       ADD +1 TO TCAR-FROM-SUB.                                    ELTOBSTS
01377  3555-EXIT.          EXIT.                                        ELTOBSTS
01378 /                                                                 ELTOBSTS
01379  4000-PLACE-OF-TREATMENT.                                         ELTOBSTS
01380      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTOBSTS
01381      ADD +1 TO WS-CIA.                                            ELTOBSTS
01382      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTOBSTS
01383      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTOBSTS
01384      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01385                                               TO  CMF-CODE-VALUE. ELTOBSTS
01386      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTOBSTS
01387              END-EXEC.                                            ELTOBSTS
01388                                                                   ELTOBSTS
01389      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01390      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01391          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01392      MOVE SPACES TO TCAR-FROM-AREA.                               ELTOBSTS
01393      STRING WS-SERVICES-RENDERED ' '                              ELTOBSTS
01394             CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01395             CMF-DESCR-LINE(2) ' '                                 ELTOBSTS
01396                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTOBSTS
01397      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01398                                                                   ELTOBSTS
01399      MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTOBSTS
01400      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTOBSTS
01401      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTOBSTS
01402      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01403      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTOBSTS
01404      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01405         ADD +1                TO WS-CIA                           ELTOBSTS
01406         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTOBSTS
01407      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01408  4000-EXIT.  EXIT.                                                ELTOBSTS
01409 /                                                                 ELTOBSTS
01410  4100-PAYABLE-AS-BASIC.                                           ELTOBSTS
01411      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTOBSTS
01412          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTOBSTS
01413          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTOBSTS
01414          GO TO 4100-OUTPUT-TEXT.                                  ELTOBSTS
01415      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTOBSTS
01416      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTOBSTS
01417      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTOBSTS
01418                                                CMF-CODE-VALUE     ELTOBSTS
01419      EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)    ELTOBSTS
01420              END-EXEC.                                            ELTOBSTS
01421      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01423          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01424      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTOBSTS
01425      STRING CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01426             CMF-DESCR-LINE(2)                                     ELTOBSTS
01427                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTOBSTS
01428      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01429      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTOBSTS
01430      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTOBSTS
01431      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTOBSTS
01432      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01433      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTOBSTS
01434                                         =  ZEROS                  ELTOBSTS
01435       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTOBSTS
01436                                         =  ZEROS                  ELTOBSTS
01437                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTOBSTS
01438                 MOVE WS-BASIC               TO                    ELTOBSTS
01439                         COF-DTL-LINE(WS-CIA)                      ELTOBSTS
01440       ELSE                                                        ELTOBSTS
01441          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTOBSTS
01442          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBSTS
01443                                        TO  WS-DTL-PERCENT         ELTOBSTS
01444          MOVE SPACES          TO TCAR-FROM-AREA                   ELTOBSTS
01445          STRING WS-DTL-PP,                                        ELTOBSTS
01446                 WS-DTL-PERCENT,                                   ELTOBSTS
01447                 WS-PERCENT,                                       ELTOBSTS
01448                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTOBSTS
01449          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
01450          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTOBSTS
01451          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTOBSTS
01452          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTOBSTS
01453          PERFORM TCPR-000-TEXT-UNSTRING                           ELTOBSTS
01454          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTOBSTS
01455          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTOBSTS
01456      ELSE                                                         ELTOBSTS
01457        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTOBSTS
01458        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTOBSTS
01459                                     TO WS-DTL-PERCENT             ELTOBSTS
01460        MOVE SPACES          TO TCAR-FROM-AREA                     ELTOBSTS
01461        STRING WS-DTL-PP,                                          ELTOBSTS
01462               WS-DTL-PERCENT,                                     ELTOBSTS
01463               WS-PERCENT,                                         ELTOBSTS
01464                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTOBSTS
01465        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTOBSTS
01466        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTOBSTS
01467        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTOBSTS
01468        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTOBSTS
01469        PERFORM TCPR-000-TEXT-UNSTRING                             ELTOBSTS
01470        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTOBSTS
01471        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTOBSTS
01472  4100-OUTPUT-TEXT.                                                ELTOBSTS
01473      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01474            ADD +1                TO WS-CIA                        ELTOBSTS
01475            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTOBSTS
01476            PERFORM 3000-OUTPUT-TEXT                               ELTOBSTS
01477      ELSE                                                         ELTOBSTS
01478         PERFORM 3000-OUTPUT-TEXT.                                 ELTOBSTS
01479  4100-EXIT.  EXIT.                                                ELTOBSTS
01480                                                                   ELTOBSTS
01481 /                                                                 ELTOBSTS
01482  4200-PAYABLE-AS-SUPP.                                            ELTOBSTS
01483      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTOBSTS
01484          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTOBSTS
01485          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTOBSTS
01486          GO TO 4200-OUTPUT-TEXT.                                  ELTOBSTS
01487      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTOBSTS
01488      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTOBSTS
01489      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTOBSTS
01490                                                CMF-CODE-VALUE     ELTOBSTS
01491      EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)    ELTOBSTS
01492             END-EXEC.                                             ELTOBSTS
01493      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01494      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01495          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01496      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTOBSTS
01497      STRING CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01498             CMF-DESCR-LINE(2)                                     ELTOBSTS
01499                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTOBSTS
01500      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01501      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTOBSTS
01502      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTOBSTS
01503      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTOBSTS
01504      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01505      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTOBSTS
01506                                         =  ZEROS                  ELTOBSTS
01507       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTOBSTS
01508                                         =  ZEROS                  ELTOBSTS
01509                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTOBSTS
01510                MOVE WS-SUPPLEMENTAL        TO                     ELTOBSTS
01511                         COF-DTL-LINE(WS-CIA)                      ELTOBSTS
01512       ELSE                                                        ELTOBSTS
01513          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTOBSTS
01514          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBSTS
01515                                        TO  WS-DTL-PERCENT         ELTOBSTS
01516          MOVE SPACES          TO TCAR-FROM-AREA                   ELTOBSTS
01517          STRING WS-DTL-PP,                                        ELTOBSTS
01518                 WS-DTL-PERCENT,                                   ELTOBSTS
01519                 WS-PERCENT,                                       ELTOBSTS
01520                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTOBSTS
01521          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
01522          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTOBSTS
01523          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTOBSTS
01524          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTOBSTS
01525          PERFORM TCPR-000-TEXT-UNSTRING                           ELTOBSTS
01526          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTOBSTS
01527          MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)    ELTOBSTS
01528      ELSE                                                         ELTOBSTS
01529        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTOBSTS
01530        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTOBSTS
01531                                    TO WS-DTL-PERCENT              ELTOBSTS
01532        MOVE SPACES          TO TCAR-FROM-AREA                     ELTOBSTS
01533        STRING WS-DTL-PP,                                          ELTOBSTS
01534               WS-DTL-PERCENT,                                     ELTOBSTS
01535               WS-PERCENT,                                         ELTOBSTS
01536                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTOBSTS
01537        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTOBSTS
01538        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTOBSTS
01539        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTOBSTS
01540        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTOBSTS
01541        PERFORM TCPR-000-TEXT-UNSTRING                             ELTOBSTS
01542        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTOBSTS
01543        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTOBSTS
01544  4200-OUTPUT-TEXT.                                                ELTOBSTS
01545      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01546            ADD +1                TO WS-CIA                        ELTOBSTS
01547            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTOBSTS
01548            PERFORM 3000-OUTPUT-TEXT                               ELTOBSTS
01549      ELSE                                                         ELTOBSTS
01550         PERFORM 3000-OUTPUT-TEXT.                                 ELTOBSTS
01551  4200-EXIT.  EXIT.                                                ELTOBSTS
01552 /                                                                 ELTOBSTS
01553  4300-SPILLOVER-COINS.                                            ELTOBSTS
01554      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBSTS
01555               =  '0'  OR LOW-VALUES                               ELTOBSTS
01556           GO TO 4300-EXIT.                                        ELTOBSTS
01557      MOVE LOW-VALUES             TO  COF-DTL-LINE(WS-CIA).        ELTOBSTS
01558      ADD +1                      TO  WS-CIA.                      ELTOBSTS
01559      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTOBSTS
01560      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTOBSTS
01561      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTOBSTS
01562                       TO CMF-CODE-VALUE.                          ELTOBSTS
01563      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTOBSTS
01564              END-EXEC.                                            ELTOBSTS
01565                                                                   ELTOBSTS
01566      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01567      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01568          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01569      MOVE SPACES TO TCAR-FROM-AREA.                               ELTOBSTS
01570      STRING WS-SPILLOVER-COINS ' '                                ELTOBSTS
01571             CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01572             CMF-DESCR-LINE(2) ' '                                 ELTOBSTS
01573                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTOBSTS
01574      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01575                                                                   ELTOBSTS
01576      MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTOBSTS
01577      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTOBSTS
01578      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTOBSTS
01579      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01580      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTOBSTS
01581      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01582         ADD +1                TO WS-CIA                           ELTOBSTS
01583         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTOBSTS
01584      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01585  4300-EXIT.  EXIT.                                                ELTOBSTS
01586 /                                                                 ELTOBSTS
01587  4400-SPILLOVER-DEDUCT.                                           ELTOBSTS
01588      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01589             = '0'   OR LOW-VALUES                                 ELTOBSTS
01590           GO TO 4400-EXIT.                                        ELTOBSTS
01591      MOVE LOW-VALUES             TO  COF-DTL-LINE(WS-CIA).        ELTOBSTS
01592      ADD +1                      TO  WS-CIA.                      ELTOBSTS
01593      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTOBSTS
01594      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTOBSTS
01595      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBSTS
01596                       TO CMF-CODE-VALUE                           ELTOBSTS
01597      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTOBSTS
01598             END-EXEC.                                             ELTOBSTS
01599                                                                   ELTOBSTS
01600      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01601      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01602          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01603      MOVE SPACES TO TCAR-FROM-AREA.                               ELTOBSTS
01604      STRING WS-SPILLOVER-DEDBL ' '                                ELTOBSTS
01605             CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01606             CMF-DESCR-LINE(2) ' '                                 ELTOBSTS
01607                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTOBSTS
01608      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01609                                                                   ELTOBSTS
01610      MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTOBSTS
01611      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTOBSTS
01612      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTOBSTS
01613      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01614      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTOBSTS
01615      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01616         ADD +1                TO WS-CIA                           ELTOBSTS
01617         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTOBSTS
01618      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01619  4400-EXIT.  EXIT.                                                ELTOBSTS
01620 /                                                                 ELTOBSTS
01621  4450-TRANS-OTHR-RESPON-IND.                                      ELTOBSTS
01622      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01623             = ZERO OR LOW-VALUES                                  ELTOBSTS
01624           GO TO 4450-EXIT.                                        ELTOBSTS
01625      MOVE LOW-VALUES             TO  COF-DTL-LINE(WS-CIA).        ELTOBSTS
01626      ADD +1                      TO  WS-CIA.                      ELTOBSTS
01627      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTOBSTS
01628      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTOBSTS
01629      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTOBSTS
01630                       TO CMF-CODE-VALUE                           ELTOBSTS
01631      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTOBSTS
01632                       COMMAREA(DFHCOMMAREA)                       ELTOBSTS
01633      END-EXEC.                                                    ELTOBSTS
01634                                                                   ELTOBSTS
01635      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01636      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01637          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01638      MOVE SPACES TO TCAR-FROM-AREA.                               ELTOBSTS
01639      STRING WS-SPILLOVER-DEDBL ' '                                ELTOBSTS
01640             CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01641             CMF-DESCR-LINE(2) ' '                                 ELTOBSTS
01642                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTOBSTS
01643      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01644                                                                   ELTOBSTS
01645      MOVE +03 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTOBSTS
01646      MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN.                         ELTOBSTS
01647      MOVE +79 TO TCAR-OUTPUT-FIELD-2-LEN.                         ELTOBSTS
01648      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01649      MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)                ELTOBSTS
01650      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTOBSTS
01651         ADD +1                TO WS-CIA                           ELTOBSTS
01652         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA).            ELTOBSTS
01653      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01654  4450-EXIT.  EXIT.                                                ELTOBSTS
01655 /                                                                 ELTOBSTS
01656  4500-MAX-VISITS.                                                 ELTOBSTS
01657      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTOBSTS
01658      MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  TO      ELTOBSTS
01659                                                 CMF-CODE-VALUE    ELTOBSTS
01660      MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)                   ELTOBSTS
01661      ADD +1             TO WS-CIA.                                ELTOBSTS
01662                                                                   ELTOBSTS
01663      MOVE 'BPE'                TO  CMF-RECORD-PREFIX.             ELTOBSTS
01664      MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME.       ELTOBSTS
01665      EXEC CICS  LINK  PROGRAM('ELUCMIF') COMMAREA(DFHCOMMAREA)    ELTOBSTS
01666              END-EXEC.                                            ELTOBSTS
01667      SET CIA-ELSCMDSC-DDN TO TRUE                                 ELTOBSTS
01668      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01669          ADDRESS OF CMF-DESCR                                     ELTOBSTS
01670      STRING CMF-DESCR-LINE(1) ' '                                 ELTOBSTS
01671             CMF-DESCR-LINE(2)                                     ELTOBSTS
01672                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTOBSTS
01673      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBSTS
01674      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTOBSTS
01675      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTOBSTS
01676      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTOBSTS
01677      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
01678  4500-EXIT.  EXIT.                                                ELTOBSTS
01679 /                                                                 ELTOBSTS
01680  4600-SCAN-TAB.                                                   ELTOBSTS
01681                                                                   ELTOBSTS
01682      PERFORM 4700-BEN-TAB-AAR THRU 4700-EXIT.                     ELTOBSTS
01683      PERFORM 4800-BEN-TAB-PPF THRU 4800-EXIT.                     ELTOBSTS
01684      ADD +1           TO WS-CIA.                                  ELTOBSTS
01685      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTOBSTS
01686      ADD +2           TO WS-CIA.                                  ELTOBSTS
01687      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01688      PERFORM 5000-BEN-TAB-ADL THRU 5000-EXIT.                     ELTOBSTS
01689      PERFORM 5100-BEN-TAB-ABM THRU 5100-EXIT.                     ELTOBSTS
01690      PERFORM 5200-BEN-TAB-ACL THRU 5200-EXIT.                     ELTOBSTS
01691      PERFORM 5300-BEN-TAB-AOL THRU 5300-EXIT.                     ELTOBSTS
01692                                                                   ELTOBSTS
01693      ADD   +2     TO  WS-CIA.                                     ELTOBSTS
01694      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTOBSTS
01695      PERFORM 3000-OUTPUT-TEXT.                                    ELTOBSTS
01696                                                                   ELTOBSTS
01697 /                                                                 ELTOBSTS
01698  4700-BEN-TAB-AAR.                                                ELTOBSTS
01699      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTOBSTS
01700      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01701                                                                   ELTOBSTS
01702      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01703          NOT = LOW-VALUES                                         ELTOBSTS
01704       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01705          NOT = SPACE                                              ELTOBSTS
01706                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTOBSTS
01707                                                                   ELTOBSTS
01708      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01709                                                                   ELTOBSTS
01710      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01711          NOT = LOW-VALUES                                         ELTOBSTS
01712       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01713          NOT = SPACE                                              ELTOBSTS
01714                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTOBSTS
01715                                                                   ELTOBSTS
01716      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTOBSTS
01717             MOVE +1                  TO WS-CIA                    ELTOBSTS
01718             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTOBSTS
01719             ADD  +1                  TO WS-CIA                    ELTOBSTS
01720             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTOBSTS
01721             PERFORM 3000-OUTPUT-TEXT.                             ELTOBSTS
01722  4700-EXIT.  EXIT.                                                ELTOBSTS
01723 /                                                                 ELTOBSTS
01724  4800-BEN-TAB-PPF.                                                ELTOBSTS
01725      MOVE ZEROS   TO  WS-HOLD1,                                   ELTOBSTS
01726                       WS-HOLD2.                                   ELTOBSTS
01727      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01728      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01729          NOT = LOW-VALUES                                         ELTOBSTS
01730       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01731          NOT = SPACE                                              ELTOBSTS
01732             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01733                        TO  WS-HOLD1.                              ELTOBSTS
01734                                                                   ELTOBSTS
01735      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01736      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01737          NOT = LOW-VALUES                                         ELTOBSTS
01738       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01739          NOT = SPACE                                              ELTOBSTS
01740             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01741                        TO  WS-HOLD2.                              ELTOBSTS
01742                                                                   ELTOBSTS
01743      IF WS-HOLD1 = WS-HOLD2                                       ELTOBSTS
01744         IF WS-HOLD1 = ZEROS                                       ELTOBSTS
01745                 GO TO 4800-EXIT                                   ELTOBSTS
01746         ELSE                                                      ELTOBSTS
01747             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTOBSTS
01748             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01749               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTOBSTS
01750                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01751               END-EXEC                                            ELTOBSTS
01752               GO TO 4800-EXIT.                                    ELTOBSTS
01753                                                                   ELTOBSTS
01754      IF WS-HOLD1 = ZEROS                                          ELTOBSTS
01755             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01756             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01757               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTOBSTS
01758                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01759               END-EXEC                                            ELTOBSTS
01760      ELSE                                                         ELTOBSTS
01761       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTOBSTS
01762       PERFORM 5900-GET-TAB-REC                                    ELTOBSTS
01763          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTOBSTS
01764                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01765          END-EXEC                                                 ELTOBSTS
01766          IF WS-HOLD2 = ZEROS                                      ELTOBSTS
01767            GO TO 4800-EXIT                                        ELTOBSTS
01768          ELSE                                                     ELTOBSTS
01769             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01770             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01771               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTOBSTS
01772                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01773               END-EXEC.                                           ELTOBSTS
01774  4800-EXIT.    EXIT.                                              ELTOBSTS
01775 /                                                                 ELTOBSTS
01776  5000-BEN-TAB-ADL.                                                ELTOBSTS
01777      MOVE ZEROS   TO  WS-HOLD1,                                   ELTOBSTS
01778                       WS-HOLD2.                                   ELTOBSTS
01779      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01780      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01781          NOT = LOW-VALUES                                         ELTOBSTS
01782       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01783          NOT = SPACE                                              ELTOBSTS
01784             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01785                        TO  WS-HOLD1.                              ELTOBSTS
01786                                                                   ELTOBSTS
01787      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01788      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01789          NOT = LOW-VALUES                                         ELTOBSTS
01790       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01791          NOT = SPACE                                              ELTOBSTS
01792             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01793                        TO  WS-HOLD2.                              ELTOBSTS
01794                                                                   ELTOBSTS
01795      IF WS-HOLD1 = WS-HOLD2                                       ELTOBSTS
01796         IF WS-HOLD1 = ZEROS                                       ELTOBSTS
01797                 GO TO 5000-EXIT                                   ELTOBSTS
01798         ELSE                                                      ELTOBSTS
01799             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTOBSTS
01800             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01801               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTOBSTS
01802                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01803               END-EXEC                                            ELTOBSTS
01804               GO TO 5000-EXIT.                                    ELTOBSTS
01805                                                                   ELTOBSTS
01806      IF WS-HOLD1 = ZEROS                                          ELTOBSTS
01807          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTOBSTS
01808          PERFORM 5900-GET-TAB-REC                                 ELTOBSTS
01809          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTOBSTS
01810                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01811          END-EXEC                                                 ELTOBSTS
01812      ELSE                                                         ELTOBSTS
01813          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTOBSTS
01814          PERFORM 5900-GET-TAB-REC                                 ELTOBSTS
01815          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTOBSTS
01816                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01817          END-EXEC                                                 ELTOBSTS
01818          IF WS-HOLD2 = ZEROS                                      ELTOBSTS
01819            GO TO 5000-EXIT                                        ELTOBSTS
01820          ELSE                                                     ELTOBSTS
01821             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01822             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01823               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTOBSTS
01824                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01825               END-EXEC.                                           ELTOBSTS
01826  5000-EXIT.     EXIT.                                             ELTOBSTS
01827 /                                                                 ELTOBSTS
01828  5100-BEN-TAB-ABM.                                                ELTOBSTS
01829      MOVE ZEROS   TO  WS-HOLD1,                                   ELTOBSTS
01830                       WS-HOLD2.                                   ELTOBSTS
01831      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01832      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01833          NOT = LOW-VALUES                                         ELTOBSTS
01834       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01835          NOT = SPACE                                              ELTOBSTS
01836             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01837                        TO  WS-HOLD1.                              ELTOBSTS
01838                                                                   ELTOBSTS
01839      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01840      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01841          NOT = LOW-VALUES                                         ELTOBSTS
01842       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01843          NOT = SPACE                                              ELTOBSTS
01844             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01845                        TO  WS-HOLD2.                              ELTOBSTS
01846                                                                   ELTOBSTS
01847      IF WS-HOLD1 = WS-HOLD2                                       ELTOBSTS
01848         IF WS-HOLD1 = ZEROS                                       ELTOBSTS
01849                 GO TO 5100-EXIT                                   ELTOBSTS
01850         ELSE                                                      ELTOBSTS
01851             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTOBSTS
01852             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01853               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTOBSTS
01854                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01855               END-EXEC                                            ELTOBSTS
01856               GO TO 5100-EXIT.                                    ELTOBSTS
01857                                                                   ELTOBSTS
01858      IF WS-HOLD1 = ZEROS                                          ELTOBSTS
01859             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01860             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01861               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTOBSTS
01862                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01863               END-EXEC                                            ELTOBSTS
01864      ELSE                                                         ELTOBSTS
01865       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTOBSTS
01866       PERFORM 5900-GET-TAB-REC                                    ELTOBSTS
01867          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTOBSTS
01868                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01869          END-EXEC                                                 ELTOBSTS
01870          IF WS-HOLD2 = ZEROS                                      ELTOBSTS
01871            GO TO 5100-EXIT                                        ELTOBSTS
01872          ELSE                                                     ELTOBSTS
01873             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01874             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01875               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTOBSTS
01876                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01877               END-EXEC.                                           ELTOBSTS
01878  5100-EXIT.     EXIT.                                             ELTOBSTS
01879 /                                                                 ELTOBSTS
01880  5200-BEN-TAB-ACL.                                                ELTOBSTS
01881      MOVE ZEROS   TO  WS-HOLD1,                                   ELTOBSTS
01882                       WS-HOLD2.                                   ELTOBSTS
01883      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01884      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01885          NOT = LOW-VALUES                                         ELTOBSTS
01886       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01887          NOT = SPACE                                              ELTOBSTS
01888             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01889                        TO  WS-HOLD1.                              ELTOBSTS
01890                                                                   ELTOBSTS
01891      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01892      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01893          NOT = LOW-VALUES                                         ELTOBSTS
01894       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01895          NOT = SPACE                                              ELTOBSTS
01896             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01897                        TO  WS-HOLD2.                              ELTOBSTS
01898                                                                   ELTOBSTS
01899      IF WS-HOLD1 = WS-HOLD2                                       ELTOBSTS
01900         IF WS-HOLD1 = ZEROS                                       ELTOBSTS
01901                 GO TO 5200-EXIT                                   ELTOBSTS
01902         ELSE                                                      ELTOBSTS
01903             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTOBSTS
01904             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01905               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTOBSTS
01906                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01907               END-EXEC                                            ELTOBSTS
01908               GO TO 5200-EXIT.                                    ELTOBSTS
01909                                                                   ELTOBSTS
01910      IF WS-HOLD1 = ZEROS                                          ELTOBSTS
01911             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01912             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01913               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTOBSTS
01914                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01915               END-EXEC                                            ELTOBSTS
01916      ELSE                                                         ELTOBSTS
01917       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTOBSTS
01918       PERFORM 5900-GET-TAB-REC                                    ELTOBSTS
01919          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTOBSTS
01920                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01921          END-EXEC                                                 ELTOBSTS
01922          IF WS-HOLD2 = ZEROS                                      ELTOBSTS
01923            GO TO 5200-EXIT                                        ELTOBSTS
01924          ELSE                                                     ELTOBSTS
01925             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01926             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01927               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTOBSTS
01928                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01929               END-EXEC.                                           ELTOBSTS
01930  5200-EXIT.     EXIT.                                             ELTOBSTS
01931 /                                                                 ELTOBSTS
01932  5300-BEN-TAB-AOL.                                                ELTOBSTS
01933      MOVE ZEROS   TO  WS-HOLD1,                                   ELTOBSTS
01934                       WS-HOLD2.                                   ELTOBSTS
01935      SET PLT-INDEX2 TO 1.                                         ELTOBSTS
01936      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01937          NOT = LOW-VALUES                                         ELTOBSTS
01938       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01939          NOT = SPACE                                              ELTOBSTS
01940             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01941                        TO  WS-HOLD1.                              ELTOBSTS
01942                                                                   ELTOBSTS
01943      SET PLT-INDEX2 TO 2.                                         ELTOBSTS
01944      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTOBSTS
01945          NOT = LOW-VALUES                                         ELTOBSTS
01946       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
01947          NOT = SPACE                                              ELTOBSTS
01948             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTOBSTS
01949                        TO  WS-HOLD2.                              ELTOBSTS
01950                                                                   ELTOBSTS
01951      IF WS-HOLD1 = WS-HOLD2                                       ELTOBSTS
01952         IF WS-HOLD1 = ZEROS                                       ELTOBSTS
01953                 GO TO 5300-EXIT                                   ELTOBSTS
01954         ELSE                                                      ELTOBSTS
01955             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTOBSTS
01956             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01957               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTOBSTS
01958                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01959               END-EXEC                                            ELTOBSTS
01960               GO TO 5300-EXIT.                                    ELTOBSTS
01961                                                                   ELTOBSTS
01962      IF WS-HOLD1 = ZEROS                                          ELTOBSTS
01963             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01964             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01965               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTOBSTS
01966                             COMMAREA(DFHCOMMAREA)                 ELTOBSTS
01967               END-EXEC                                            ELTOBSTS
01968      ELSE                                                         ELTOBSTS
01969       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTOBSTS
01970       PERFORM 5900-GET-TAB-REC                                    ELTOBSTS
01971          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTOBSTS
01972                          COMMAREA(DFHCOMMAREA)                    ELTOBSTS
01973          END-EXEC                                                 ELTOBSTS
01974          IF WS-HOLD2 = ZEROS                                      ELTOBSTS
01975            GO TO 5300-EXIT                                        ELTOBSTS
01976          ELSE                                                     ELTOBSTS
01977             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTOBSTS
01978             PERFORM 5900-GET-TAB-REC                              ELTOBSTS
01979               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTOBSTS
01980                              COMMAREA(DFHCOMMAREA)                ELTOBSTS
01981               END-EXEC.                                           ELTOBSTS
01982  5300-EXIT.     EXIT.                                             ELTOBSTS
01983 /                                                                 ELTOBSTS
01984  5900-GET-TAB-REC.                                                ELTOBSTS
01985      SET CIA-GCTABULR-DDN TO TRUE.                                ELTOBSTS
01986      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
01987          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTOBSTS
01988      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTOBSTS
01989      SET  CIA-GCTABULR-DDN               TO TRUE.                 ELTOBSTS
01990      SET IOP-RD                          TO TRUE.                 ELTOBSTS
01991      SET IOP-FCQ-NONE                    TO TRUE.                 ELTOBSTS
01992      SET IOP-KVQ-NONE                    TO TRUE.                 ELTOBSTS
01993      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTOBSTS
01994                                                                   ELTOBSTS
01995      EXEC CICS LINK                                               ELTOBSTS
01996                PROGRAM ('ELUIOPGM')                               ELTOBSTS
01997                COMMAREA (DFHCOMMAREA)                             ELTOBSTS
01998      END-EXEC.                                                    ELTOBSTS
01999                                                                   ELTOBSTS
02000      IF IOP-RC-NOTFND                                             ELTOBSTS
02001         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTOBSTS
02002         EXEC CICS ABEND                                           ELTOBSTS
02003                   ABCODE(CIA-ABCODE)                              ELTOBSTS
02004         END-EXEC                                                  ELTOBSTS
02005      ELSE                                                         ELTOBSTS
02006          IF NOT IOP-RC-OK                                         ELTOBSTS
02007             SET CIA-AB-CRITIO TO TRUE                             ELTOBSTS
02008             EXEC CICS ABEND                                       ELTOBSTS
02009                       ABCODE(CIA-ABCODE)                          ELTOBSTS
02010             END-EXEC                                              ELTOBSTS
02011          END-IF                                                   ELTOBSTS
02012      END-IF.                                                      ELTOBSTS
02013  5900-EXIT.       EXIT.                                           ELTOBSTS
02014                                                                   ELTOBSTS
02015  6115-CERT-REQ-IND.                                               ELTOBSTS
02016 ****************************************************************  ELTOBSTS
02017 *       C E R T I F I C A T E  R  E Q U I R E M E N T          *  ELTOBSTS
02018 ****************************************************************  ELTOBSTS
02019      SET  PLT-INDEX2  TO  1.                                      ELTOBSTS
02020      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBSTS
02021         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02022               NOT =  ZERO                                         ELTOBSTS
02023          MOVE 1 TO WS-CIA                                         ELTOBSTS
02024          MOVE SPACE                 TO  COF-DTL-LINE (WS-CIA)     ELTOBSTS
02025          MOVE +2                    TO  WS-CIA                    ELTOBSTS
02026          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTOBSTS
02027                                                                   ELTOBSTS
02028      SET  PLT-INDEX2  TO  2.                                      ELTOBSTS
02029      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
02030         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02031               NOT  =  ZERO                                        ELTOBSTS
02032 *        MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTOBSTS
02033          MOVE +2                    TO  WS-CIA                    ELTOBSTS
02034          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTOBSTS
02035                                                                   ELTOBSTS
02036      SET  PLT-INDEX2  TO  1.                                      ELTOBSTS
02037      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBSTS
02038         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02039               NOT  =  ZERO                                        ELTOBSTS
02040          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBSTS
02041          MOVE 'CERTFN-REQRM-IND'                                  ELTOBSTS
02042                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBSTS
02043          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBSTS
02044                TO  CMF-CODE-VALUE                                 ELTOBSTS
02045          PERFORM 9500-CALL-CODES-MANUAL                           ELTOBSTS
02046             THRU 9500-EXIT                                        ELTOBSTS
02047          INITIALIZE TCAR-FROM-AREA                                ELTOBSTS
02048          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02049 *        MOVE WS-BASIC-LIT  TO  TCAR-FROM-LINE (TCAR-FROM-SUB)    ELTOBSTS
02050 *        ADD 1 TO TCAR-FROM-SUB                                   ELTOBSTS
02051          SET CIA-ELSCMDSC-DDN TO TRUE                             ELTOBSTS
02052          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTOBSTS
02053               ADDRESS OF CMF-DESCR                                ELTOBSTS
02054          SET CMF-DESCR-IDX TO 1                                   ELTOBSTS
02055          PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL          ELTOBSTS
02056               CMF-DESCR-IDX  > CMF-NBR-DESCR-LINES                ELTOBSTS
02057               MOVE  CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTOBSTS
02058                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELTOBSTS
02059               ADD 1 TO TCAR-FROM-SUB                              ELTOBSTS
02060          END-PERFORM                                              ELTOBSTS
02061          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
02062          PERFORM 9550-UNSTRING                                    ELTOBSTS
02063          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02064          MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO WS-DTL-BASIC       ELTOBSTS
02065          ADD 1 TO WS-CIA                                          ELTOBSTS
02066          MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                    ELTOBSTS
02067 *        ADD 1 TO WS-CIA                                          ELTOBSTS
02068          PERFORM VARYING TCAR-FROM-SUB FROM 2 BY 1                ELTOBSTS
02069             UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED         ELTOBSTS
02070             ADD 1 TO WS-CIA                                       ELTOBSTS
02071             MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                  ELTOBSTS
02072                COF-DTL-LINE(WS-CIA)                               ELTOBSTS
02073          END-PERFORM                                              ELTOBSTS
02074          PERFORM 3000-OUTPUT-TEXT                                 ELTOBSTS
02075       END-IF.                                                     ELTOBSTS
02076                                                                   ELTOBSTS
02077      SET PLT-INDEX2  TO  2.                                       ELTOBSTS
02078      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
02079         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02080              NOT  =  ZERO                                         ELTOBSTS
02081          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBSTS
02082          MOVE 'CERTFN-REQRM-IND'                                  ELTOBSTS
02083                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBSTS
02084          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBSTS
02085               TO  CMF-CODE-VALUE                                  ELTOBSTS
02086          PERFORM 9500-CALL-CODES-MANUAL                           ELTOBSTS
02087             THRU 9500-EXIT                                        ELTOBSTS
02088          SET CMF-DESCR-IDX TO 1                                   ELTOBSTS
02089          INITIALIZE TCAR-FROM-AREA                                ELTOBSTS
02090          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02091 *        MOVE WS-SUPPLEMENTAL-LIT                                 ELTOBSTS
02092 *               TO  TCAR-FROM-LINE (TCAR-FROM-SUB)                ELTOBSTS
02093 *        ADD 1 TO TCAR-FROM-SUB                                   ELTOBSTS
02094          SET CIA-ELSCMDSC-DDN TO TRUE                             ELTOBSTS
02095          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTOBSTS
02096               ADDRESS OF CMF-DESCR                                ELTOBSTS
02097          SET CMF-DESCR-IDX TO 1                                   ELTOBSTS
02098          PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL          ELTOBSTS
02099               CMF-DESCR-IDX  > CMF-NBR-DESCR-LINES                ELTOBSTS
02100               MOVE  CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTOBSTS
02101                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELTOBSTS
02102               ADD 1 TO TCAR-FROM-SUB                              ELTOBSTS
02103          END-PERFORM                                              ELTOBSTS
02104          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
02105          PERFORM 9550-UNSTRING                                    ELTOBSTS
02106          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02107          MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                       ELTOBSTS
02108                             TO WS-DTL-SUPPLEMENTAL                ELTOBSTS
02109          ADD 1 TO WS-CIA                                          ELTOBSTS
02110          MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
02111 *        ADD 1 TO WS-CIA                                          ELTOBSTS
02112          PERFORM VARYING TCAR-FROM-SUB FROM 2 BY 1                ELTOBSTS
02113             UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED         ELTOBSTS
02114             ADD 1 TO WS-CIA                                       ELTOBSTS
02115             MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                  ELTOBSTS
02116                COF-DTL-LINE(WS-CIA)                               ELTOBSTS
02117          END-PERFORM                                              ELTOBSTS
02118          PERFORM 3000-OUTPUT-TEXT                                 ELTOBSTS
02119      END-IF.                                                      ELTOBSTS
02120                                                                   ELTOBSTS
02121 *    IF WS-ADD-A-BLANK-LINE                                       ELTOBSTS
02122 *       MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTOBSTS
02123 *        PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTOBSTS
02124 *           THRU 9200-EXIT.                                       ELTOBSTS
02125                                                                   ELTOBSTS
02126      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBSTS
02127         SET PLT-INDEX2  TO  2                                     ELTOBSTS
02128      ELSE                                                         ELTOBSTS
02129         SET PLT-INDEX2  TO  1.                                    ELTOBSTS
02130                                                                   ELTOBSTS
02131  6115-EXIT.  EXIT.                                                ELTOBSTS
02132                                                                   ELTOBSTS
02133                                                                   ELTOBSTS
02134  6116-TREAT-RESTRICTION.                                          ELTOBSTS
02135 ****************************************************************  ELTOBSTS
02136 *       T R E A T M E N T   R E S T R I C T I O N              *  ELTOBSTS
02137 ****************************************************************  ELTOBSTS
02138      SET  PLT-INDEX2  TO  1.                                      ELTOBSTS
02139      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBSTS
02140         AND  PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02141               NOT =  ZERO                                         ELTOBSTS
02142          MOVE 1 TO WS-CIA                                         ELTOBSTS
02143          MOVE SPACE                 TO  COF-DTL-LINE (WS-CIA)     ELTOBSTS
02144          ADD  +1                    TO  WS-CIA                    ELTOBSTS
02145          MOVE WS-TREAT-RESTRN       TO  COF-DTL-LINE (WS-CIA).    ELTOBSTS
02146                                                                   ELTOBSTS
02147      SET  PLT-INDEX2  TO  2.                                      ELTOBSTS
02148      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
02149         AND  PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02150               NOT  =  ZERO                                        ELTOBSTS
02151          MOVE +2                   TO  WS-CIA                     ELTOBSTS
02152          MOVE WS-TREAT-RESTRN       TO  COF-DTL-LINE (WS-CIA).    ELTOBSTS
02153                                                                   ELTOBSTS
02154      SET  PLT-INDEX2  TO  1.                                      ELTOBSTS
02155      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
02156         AND  PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02157               NOT  =  ZERO                                        ELTOBSTS
02158          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBSTS
02159          MOVE 'TREAT-RESTRN-IND'                                  ELTOBSTS
02160                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBSTS
02161          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBSTS
02162                TO  CMF-CODE-VALUE                                 ELTOBSTS
02163          PERFORM 9500-CALL-CODES-MANUAL                           ELTOBSTS
02164             THRU 9500-EXIT                                        ELTOBSTS
02165          INITIALIZE TCAR-FROM-AREA                                ELTOBSTS
02166          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02167 *        MOVE WS-BASIC-LIT  TO  TCAR-FROM-LINE (TCAR-FROM-SUB)    ELTOBSTS
02168 *        ADD 1 TO TCAR-FROM-SUB                                   ELTOBSTS
02169          SET CIA-ELSCMDSC-DDN TO TRUE                             ELTOBSTS
02170          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTOBSTS
02171               ADDRESS OF CMF-DESCR                                ELTOBSTS
02172          SET CMF-DESCR-IDX TO 1                                   ELTOBSTS
02173          PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL          ELTOBSTS
02174               CMF-DESCR-IDX  > CMF-NBR-DESCR-LINES                ELTOBSTS
02175               MOVE  CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTOBSTS
02176                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELTOBSTS
02177               ADD 1 TO TCAR-FROM-SUB                              ELTOBSTS
02178          END-PERFORM                                              ELTOBSTS
02179          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
02180          PERFORM 9550-UNSTRING                                    ELTOBSTS
02181          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02182          MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO WS-DTL-BASIC       ELTOBSTS
02183          ADD 1 TO WS-CIA                                          ELTOBSTS
02184          MOVE WS-BASIC TO COF-DTL-LINE(WS-CIA)                    ELTOBSTS
02185 *        ADD 1 TO WS-CIA                                          ELTOBSTS
02186          PERFORM VARYING TCAR-FROM-SUB FROM 2 BY 1                ELTOBSTS
02187             UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED         ELTOBSTS
02188             ADD 1 TO WS-CIA                                       ELTOBSTS
02189             MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                  ELTOBSTS
02190                COF-DTL-LINE(WS-CIA)                               ELTOBSTS
02191          END-PERFORM                                              ELTOBSTS
02192          PERFORM 3000-OUTPUT-TEXT                                 ELTOBSTS
02193      END-IF.                                                      ELTOBSTS
02194                                                                   ELTOBSTS
02195      SET PLT-INDEX2  TO  2.                                       ELTOBSTS
02196      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBSTS
02197         AND  PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBSTS
02198              NOT  =  ZERO                                         ELTOBSTS
02199          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBSTS
02200          MOVE 'TREAT-RESTRN-IND'                                  ELTOBSTS
02201                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBSTS
02202          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBSTS
02203               TO  CMF-CODE-VALUE                                  ELTOBSTS
02204          PERFORM 9500-CALL-CODES-MANUAL                           ELTOBSTS
02205             THRU 9500-EXIT                                        ELTOBSTS
02206          INITIALIZE TCAR-FROM-AREA                                ELTOBSTS
02207          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02208 *        MOVE WS-SUPPLEMENTAL-LIT                                 ELTOBSTS
02209 *               TO  TCAR-FROM-LINE (TCAR-FROM-SUB)                ELTOBSTS
02210          ADD 1 TO TCAR-FROM-SUB                                   ELTOBSTS
02211          SET CIA-ELSCMDSC-DDN TO TRUE                             ELTOBSTS
02212          CALL 'ELUSETAD' USING DFHCOMMAREA                        ELTOBSTS
02213               ADDRESS OF CMF-DESCR                                ELTOBSTS
02214          SET CMF-DESCR-IDX TO 1                                   ELTOBSTS
02215          PERFORM VARYING CMF-DESCR-IDX FROM 1 BY 1 UNTIL          ELTOBSTS
02216               CMF-DESCR-IDX  > CMF-NBR-DESCR-LINES                ELTOBSTS
02217               MOVE  CMF-DESCR-LINE (CMF-DESCR-IDX)                ELTOBSTS
02218                          TO TCAR-FROM-LINE (TCAR-FROM-SUB)        ELTOBSTS
02219               ADD 1 TO TCAR-FROM-SUB                              ELTOBSTS
02220          END-PERFORM                                              ELTOBSTS
02221          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTOBSTS
02222          PERFORM 9550-UNSTRING                                    ELTOBSTS
02223          MOVE 1 TO TCAR-FROM-SUB                                  ELTOBSTS
02224          MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)                       ELTOBSTS
02225                       TO WS-DTL-SUPPLEMENTAL                      ELTOBSTS
02226          MOVE WS-SUPPLEMENTAL TO COF-DTL-LINE(WS-CIA)             ELTOBSTS
02227 *        ADD 1 TO WS-CIA                                          ELTOBSTS
02228          PERFORM VARYING TCAR-FROM-SUB FROM 2 BY 1                ELTOBSTS
02229             UNTIL TCAR-FROM-SUB > TCAR-OUTPUT-FIELDS-USED         ELTOBSTS
02230             ADD 1 TO WS-CIA                                       ELTOBSTS
02231             MOVE TCAR-OPF-DATA(TCAR-FROM-SUB) TO                  ELTOBSTS
02232                COF-DTL-LINE(WS-CIA)                               ELTOBSTS
02233          END-PERFORM                                              ELTOBSTS
02234          PERFORM 3000-OUTPUT-TEXT                                 ELTOBSTS
02235      END-IF.                                                      ELTOBSTS
02236                                                                   ELTOBSTS
02237 *    IF WS-ADD-A-BLANK-LINE                                       ELTOBSTS
02238 *       MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTOBSTS
02239 *        PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTOBSTS
02240 *           THRU 9200-EXIT.                                       ELTOBSTS
02241                                                                   ELTOBSTS
02242      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBSTS
02243         SET PLT-INDEX2  TO  2                                     ELTOBSTS
02244      ELSE                                                         ELTOBSTS
02245         SET PLT-INDEX2  TO  1.                                    ELTOBSTS
02246                                                                   ELTOBSTS
02247  6116-EXIT.  EXIT.                                                ELTOBSTS
02248                                                                   ELTOBSTS
02249                                                                   ELTOBSTS
02250  9500-CALL-CODES-MANUAL.                                          ELTOBSTS
02251      EXEC CICS LINK PROGRAM ('ELUCMIF')                           ELTOBSTS
02252                COMMAREA (DFHCOMMAREA)                             ELTOBSTS
02253      END-EXEC.                                                    ELTOBSTS
02254      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTOBSTS
02255      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBSTS
02256                            ADDRESS OF CMF-DESCR.                  ELTOBSTS
02257  9500-EXIT.  EXIT.                                                ELTOBSTS
02258                                                                   ELTOBSTS
02259  9550-UNSTRING.                                                   ELTOBSTS
02260      MOVE 10 TO TCAR-OUTPUT-FIELD-COUNT.                          ELTOBSTS
02261      MOVE 63 TO TCAR-OUTPUT-FIELD-1-LEN.                          ELTOBSTS
02262      MOVE 79 TO TCAR-OUTPUT-FIELD-2-LEN.                          ELTOBSTS
02263      MOVE 79 TO TCAR-OUTPUT-FIELD-3-LEN.                          ELTOBSTS
02264      MOVE 79 TO TCAR-OUTPUT-FIELD-4-LEN.                          ELTOBSTS
02265      MOVE 79 TO TCAR-OUTPUT-FIELD-5-LEN.                          ELTOBSTS
02266      MOVE 79 TO TCAR-OUTPUT-FIELD-6-LEN.                          ELTOBSTS
02267      MOVE 79 TO TCAR-OUTPUT-FIELD-7-LEN.                          ELTOBSTS
02268      MOVE 79 TO TCAR-OUTPUT-FIELD-8-LEN.                          ELTOBSTS
02269      MOVE 79 TO TCAR-OUTPUT-FIELD-9-LEN.                          ELTOBSTS
02270      MOVE 79 TO TCAR-OUTPUT-FIELD-10-LEN.                         ELTOBSTS
02271      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBSTS
02272                                                                   ELTOBSTS
02273 /   C O M P R E S S I O N   A N D  U N S T R I N G  R O U T I N E ELTOBSTS
02274  COPY ELSTCOMP.                                                   ELTOBSTS
