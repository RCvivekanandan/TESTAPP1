00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTDUREQ.                                            ELTDUREQ
00003  AUTHOR. WIL HARNDEN - TMS , INC.                                    LV002
00004  DATE-WRITTEN.   7/17/86.                                         ELTDUREQ
00005  DATE-COMPILED.                                                   ELTDUREQ
00006      SKIP3                                                        ELTDUREQ
00007 ******************************************************************ELTDUREQ
00008 *@>ELTAMBUL                                                       ELTDUREQ
00009 *@¬                                                               ELTDUREQ
00010 *                        PROGRAM ABSTRACT                         ELTDUREQ
00011 *                                                                 ELTDUREQ
00012 *@¬ PROGRAM NAME:   E.L.S. DURABLE MEDICAL EQUIPMENT              ELTDUREQ
00013 *@¬                                                               ELTDUREQ
00014 *@¬ PROGRAM I.D.:   ELTDUREQ                                      ELTDUREQ
00015 *@¬                                                               ELTDUREQ
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTDUREQ
00017 *@¬            DURABLE MEDICAL EQUIPMENT                          ELTDUREQ
00018 *@¬                      SERVICES COVERAGE GIVEN A MEMBER.        ELTDUREQ
00019 *@¬                                                               ELTDUREQ
00020 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF DURABLE MEDICAL   ELTDUREQ
00021 *@¬       EQUIPMENT SERVICES GIVEN A MEMBER BY HIS GROUP. THIS    ELTDUREQ
00022 *@¬      INFO IS OBTAINED BY INTEROGATING THE BENEFIT PROVISIONS  ELTDUREQ
00023 *@¬      FOR THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR       ELTDUREQ
00024 *@¬      RANGE OF DATES.                                          ELTDUREQ
00025 *@¬                                                               ELTDUREQ
00026 *@¬ RECORDS                                                       ELTDUREQ
00027 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT,                          ELTDUREQ
00028 *@¬            VARIOUS BENEFIT PROVISIONS, AND A                  ELTDUREQ
00029 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTDUREQ
00030 *@¬                                                               ELTDUREQ
00031 *@¬ PROCESSING                                                    ELTDUREQ
00032 *@¬ FUNCTIONS: INSTITUTIONAL - INPATIENT AND OUTPATIENT           ELTDUREQ
00033 *@¬            PROFESSIONAL  - INPATIENT AND OUTPATIENT           ELTDUREQ
00034 *@¬                                                               ELTDUREQ
00035 *@¬ UPDATE HISTORY                                                ELTDUREQ
00036 *@¬                                                               ELTDUREQ
00037 *@¬ 10/07/86  JTC    VS COBOL II CONVERSION                       ELTDUREQ
00038 *@¬                                                               ELTDUREQ
00039 *@¬   11/15/86  LET    REVISED CODE TO ACCOMADATE THE MOVING OF THELTDUREQ
00040 *@¬                    CERTIFICATION REQUIREMENT INDICATOR FROM THELTDUREQ
00041 *@¬                    FORMAT TYPE SECTION TO THE COMMON SECTION  ELTDUREQ
00042 *@¬                                                               ELTDUREQ
00043 *@¬10/20/87    AKK     CHANGED 'THIS GROUP OF BENEFITS ARE        ELTDUREQ
00044 *@¬                    HANDLED AS FOLLOWS' TO 'COVERED BENEFITS   ELTDUREQ
00045 *@¬                    ARE'                                       ELTDUREQ
00046 *@¬                                                               ELTDUREQ
00047 *@¬03/21/89    GEM     STORAGE MANAGEMENT ENHANCEMENTS            ELTDUREQ
00048 *@¬                                                               ELTDUREQ
00049 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY INELTDUREQ
00050 *                      FROM A SINGLE POSITION TO ZEROS            ELTDUREQ
00051 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTDUREQ
00052 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTDUREQ
00053 *                                                                 ELTDUREQ
00054 ***************************************************************** ELTDUREQ
00055 /                                                                 ELTDUREQ
00056  ENVIRONMENT DIVISION.                                            ELTDUREQ
00057      SKIP3                                                        ELTDUREQ
00058  DATA DIVISION.                                                   ELTDUREQ
00059  WORKING-STORAGE SECTION.                                         ELTDUREQ
00060  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTDUREQ
00061      '***ELTDUREQ WS BEGINS***'.                                  ELTDUREQ
00062  01  WS-BEN-SCOPE                PIC X(4) VALUE 'XXXX'.           ELTDUREQ
00063  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTDUREQ
00064  01  WS-IND1                     PIC X(01) VALUE SPACE.           ELTDUREQ
00065  01  WS-IND2                     PIC X(01) VALUE SPACE.           ELTDUREQ
00066  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTDUREQ
00067                                                                   ELTDUREQ
00068 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTDUREQ
00069  01  WS-WORK-FIELDS.                                              ELTDUREQ
00070      05  WS-CHAR-0                     PIC X.                     ELTDUREQ
00071      05  WS-DISPLAY-MAX-VISIT-TEXT     PIC X.                     ELTDUREQ
00072      05  WS-MAX-AMT-TEXT-SW            PIC X.                     ELTDUREQ
00073      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTDUREQ
00074      05  WS-DISPLAY-PVE-TEXT           PIC X.                     ELTDUREQ
00075      05  WS-SERVICES-PAYBLE-SW         PIC X.                     ELTDUREQ
00076      05  WS-FIRSTTIME-IND              PIC X.                     ELTDUREQ
00077          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTDUREQ
00078      05  WS-HOLD1                      PIC X(10).                 ELTDUREQ
00079      05  WS-HOLD2                      PIC X(10).                 ELTDUREQ
00080      05  WS-DTL-PP                     PIC X(50).                 ELTDUREQ
00081      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTDUREQ
00082      05  WS-DTL-PER-D                  PIC X(63).                 ELTDUREQ
00083      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTDUREQ
00084      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTDUREQ
00085      05  WS-DISPLAY-SERVIC-REND-TEXT   PIC X.                     ELTDUREQ
00086      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTDUREQ
00087      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTDUREQ
00088      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTDUREQ
00089      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTDUREQ
00090      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTDUREQ
00091      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTDUREQ
00092      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTDUREQ
00093      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTDUREQ
00094                                                                   ELTDUREQ
00095 /  B E N   P R O V   I D S   B Y   T Y P E - PROFESSIONAL-IP      ELTDUREQ
00096  01  WS-TABLE-MAX-CNT                  PIC S9(4) COMP   VALUE +4. ELTDUREQ
00097  01  WS-BEN-PROV-ID-IP.                                           ELTDUREQ
00098      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +2. ELTDUREQ
00099      05  WS-PROF-IP-TAB.                                          ELTDUREQ
00100        10  FILLER                      PIC X(6)  VALUE 'DMEI E'.  ELTDUREQ
00101        10  FILLER                      PIC X(6)  VALUE 'DMRI E'.  ELTDUREQ
00102      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTDUREQ
00103                                        PIC X(6) OCCURS 02 TIMES.  ELTDUREQ
00104                                                                   ELTDUREQ
00105 /  B E N   P R O V   I D S   B Y   T Y P E - PROFESSIONAL-OP      ELTDUREQ
00106  01  WS-BEN-PROV-ID-OP.                                           ELTDUREQ
00107      05  WS-PROF-OP-CNT                PIC S9(4) COMP   VALUE +2. ELTDUREQ
00108      05  WS-PROF-OP-TAB.                                          ELTDUREQ
00109        10  FILLER                      PIC X(6)  VALUE 'DMEO E'.  ELTDUREQ
00110        10  FILLER                      PIC X(6)  VALUE 'DMRO E'.  ELTDUREQ
00111      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTDUREQ
00112                                        PIC X(6) OCCURS 02 TIMES.  ELTDUREQ
00113                                                                   ELTDUREQ
00114                                                                   ELTDUREQ
00115 /  B E N   P R O V   I D S   B Y   T Y P E - INSTITUTIONAL-IP     ELTDUREQ
00116  01  WS-BEN-PROV-ID-II.                                           ELTDUREQ
00117      05  WS-INST-IP-CNT                PIC S9(4)  COMP  VALUE +4. ELTDUREQ
00118      05  WS-INST-IP-TAB.                                          ELTDUREQ
00119        10  FILLER                      PIC X(6)  VALUE 'DMEI B'.  ELTDUREQ
00120        10  FILLER                      PIC X(6)  VALUE 'DMRI B'.  ELTDUREQ
00121        10  FILLER                      PIC X(6)  VALUE 'EADM B'.  ELTDUREQ
00122        10  FILLER                      PIC X(6)  VALUE 'RDM  B'.  ELTDUREQ
00123      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTDUREQ
00124                                        PIC X(6) OCCURS 04 TIMES.  ELTDUREQ
00125                                                                   ELTDUREQ
00126 /  B E N   P R O V   I D S   B Y   T Y P E - INSTITUTIONAL -OP    ELTDUREQ
00127  01  WS-BEN-PROV-ID-IO.                                           ELTDUREQ
00128      05  WS-INST-OP-CNT                PIC S9(4) COMP   VALUE +4. ELTDUREQ
00129      05  WS-INST-OP-TAB.                                          ELTDUREQ
00130        10  FILLER                      PIC X(6)  VALUE 'DMEO B'.  ELTDUREQ
00131        10  FILLER                      PIC X(6)  VALUE 'DMRO B'.  ELTDUREQ
00132        10  FILLER                      PIC X(6)  VALUE 'EADM B'.  ELTDUREQ
00133        10  FILLER                      PIC X(6)  VALUE 'RDM  B'.  ELTDUREQ
00134      05  WS-INST-OP-LIST     REDEFINES    WS-INST-OP-TAB          ELTDUREQ
00135                                        PIC X(6) OCCURS 04 TIMES.  ELTDUREQ
00136                                                                   ELTDUREQ
00137 /                L I T E R A L S                                  ELTDUREQ
00138  01  WS-PROGRAM-LITERALS.                                         ELTDUREQ
00139    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTDUREQ
00140    05  WS-DAYS                     PIC X(04) VALUE 'DAYS'.        ELTDUREQ
00141    05  WS-YES                      PIC X     VALUE 'Y'.           ELTDUREQ
00142    05  WS-NO                       PIC X     VALUE 'N'.           ELTDUREQ
00143    05  WS-BASIC-LIT                PIC X(10) VALUE                ELTDUREQ
00144        '   BASIC: '.                                              ELTDUREQ
00145    05  WS-4096                     PIC S9(8) COMP  VALUE +4096.   ELTDUREQ
00146    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTDUREQ
00147          VALUE 'SPILLOVER COINSURANCE: '.                         ELTDUREQ
00148    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTDUREQ
00149          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTDUREQ
00150    05  WS-SERVICES-RENDERED.                                      ELTDUREQ
00151      10  FILLER                    PIC X(26)                      ELTDUREQ
00152          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTDUREQ
00153    05  WS-PAYMNT-BASED.                                           ELTDUREQ
00154      10  FILLER                    PIC X(21)                      ELTDUREQ
00155          VALUE 'PAYMENT IS BASED ON: '.                           ELTDUREQ
00156                                                                   ELTDUREQ
00157 /            D I S P L A Y   L I N E S                            ELTDUREQ
00158  01  WS-ELS-DISPLAY-LINES.                                        ELTDUREQ
00159    05  WS-HDR-1.                                                  ELTDUREQ
00160      10  FILLER                    PIC X(12) VALUE 'SECTION NO: '.ELTDUREQ
00161      10  WS-HDR1-SECT-NO           PIC X(5)  VALUE SPACES.        ELTDUREQ
00162      10  FILLER                    PIC X(22)                      ELTDUREQ
00163          VALUE '      EFFECTIVE DATE: '.                          ELTDUREQ
00164      10  WS-HDR1-DATE              PIC X(8).                      ELTDUREQ
00165      10  FILLER                    PIC X(28)                      ELTDUREQ
00166          VALUE '      FAMILY RELATIONSHIP: '.                     ELTDUREQ
00167      10  WS-HDR1-FRL               PIC X.                         ELTDUREQ
00168      10  FILLER                    PIC X(4) VALUE LOW-VALUES.     ELTDUREQ
00169                                                                   ELTDUREQ
00170    05  WS-HDR-2-IP.                                               ELTDUREQ
00171      10  FILLER                    PIC X(18) VALUE SPACES.        ELTDUREQ
00172      10  FILLER                    PIC X(26) VALUE                ELTDUREQ
00173      'DURABLE MEDICAL EQUIPMENT '.                                ELTDUREQ
00174      10  WS-HDR2-IPOP-MSG          PIC X(24) VALUE SPACES.        ELTDUREQ
00175      10  FILLER                    PIC X(11) VALUE LOW-VALUES.    ELTDUREQ
00176                                                                   ELTDUREQ
00177    05  WS-DUR-MEDICAL-EQUIP.                                      ELTDUREQ
00178      10  FILLER                    PIC X(27) VALUE                ELTDUREQ
00179      'DURABLE MEDICAL EQUIPMENT  '.                               ELTDUREQ
00180      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDUREQ
00181                                                                   ELTDUREQ
00182    05  WS-FOLLOW-BENEFIT.                                         ELTDUREQ
00183      10  FILLER                    PIC X(79) VALUE                ELTDUREQ
00184          'COVERED SERVICES ARE:  '.                               ELTDUREQ
00185                                                                   ELTDUREQ
00186    05  WS-PAY-CONSDR-TEXT1.                                       ELTDUREQ
00187      10  FILLER                    PIC X(49)                      ELTDUREQ
00188        VALUE 'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.     ELTDUREQ
00189                                                                   ELTDUREQ
00190    05  WS-PAY-CONSDR-TEXT2.                                       ELTDUREQ
00191      10  FILLER                    PIC X(45)                      ELTDUREQ
00192        VALUE 'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.      ELTDUREQ
00193                                                                   ELTDUREQ
00194    05  WS-SERVICES-2ND.                                           ELTDUREQ
00195      10  FILLER                    PIC X(21) VALUE SPACES.        ELTDUREQ
00196      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTDUREQ
00197      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTDUREQ
00198                                                                   ELTDUREQ
00199    05  WS-SERVICES-PAYABLE.                                       ELTDUREQ
00200      10  FILLER                    PIC X(45)                      ELTDUREQ
00201          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTDUREQ
00202      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTDUREQ
00203                                                                   ELTDUREQ
00204    05  WS-BASIC.                                                  ELTDUREQ
00205      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDUREQ
00206      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDUREQ
00207      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTDUREQ
00208                                                                   ELTDUREQ
00209    05  WS-BASIC-VISIT-AMT.                                        ELTDUREQ
00210      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDUREQ
00211      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDUREQ
00212      10  WS-BASIC-MAX-AMT          PIC ZZ9.                       ELTDUREQ
00213                                                                   ELTDUREQ
00214    05  WS-SUPPLEMENTAL.                                           ELTDUREQ
00215      10  FILLER                    PIC X(16)                      ELTDUREQ
00216          VALUE '  SUPPLEMENTAL: '.                                ELTDUREQ
00217      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTDUREQ
00218                                                                   ELTDUREQ
00219    05  WS-SUPPL-VISIT-AMT.                                        ELTDUREQ
00220      10  FILLER                    PIC X(16)                      ELTDUREQ
00221          VALUE '  SUPPLEMENTAL: '.                                ELTDUREQ
00222      10  WS-SUPPL-MAX-AMT          PIC ZZ9.                       ELTDUREQ
00223                                                                   ELTDUREQ
00224    05  WS-BASIC-PERCENT.                                          ELTDUREQ
00225      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDUREQ
00226      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDUREQ
00227      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTDUREQ
00228                                                                   ELTDUREQ
00229    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTDUREQ
00230      10  FILLER                    PIC X(16)                      ELTDUREQ
00231          VALUE '  SUPPLEMENTAL: '.                                ELTDUREQ
00232      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTDUREQ
00233                                                                   ELTDUREQ
00234    05  WS-BASIC-PER-D.                                            ELTDUREQ
00235      10  FILLER                    PIC X(09) VALUE SPACES.        ELTDUREQ
00236      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTDUREQ
00237      10  WS-DTL-BASIC-PER-D        PIC X(40) VALUE SPACES.        ELTDUREQ
00238      10  FILLER                    PIC X     VALUE SPACE.         ELTDUREQ
00239      10  WS-DTL-BASIC-PER-D-AMT    PIC ZZZZ9.99.                  ELTDUREQ
00240      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTDUREQ
00241                                                                   ELTDUREQ
00242    05  WS-SUPP-PER-D.                                             ELTDUREQ
00243      10  FILLER                    PIC X(16)                      ELTDUREQ
00244          VALUE '  SUPPLEMENTAL: '.                                ELTDUREQ
00245      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTDUREQ
00246      10  FILLER                    PIC X     VALUE SPACE.         ELTDUREQ
00247      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTDUREQ
00248      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTDUREQ
00249                                                                   ELTDUREQ
00250    05  WS-MAX-VISITS.                                             ELTDUREQ
00251      10  FILLER                    PIC X(34)                      ELTDUREQ
00252          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE: '.              ELTDUREQ
00253      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTDUREQ
00254                                                                   ELTDUREQ
00255    05  WS-MAX-AMT-TEXT.                                           ELTDUREQ
00256      10  FILLER                    PIC X(33)                      ELTDUREQ
00257          VALUE 'THE MAXIMUM AMOUNT PER VISIT IS: '.               ELTDUREQ
00258      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTDUREQ
00259                                                                   ELTDUREQ
00260    05  WS-UNLIMITED.                                              ELTDUREQ
00261      10  WS-DTL-UNLIMITED      PIC X(20).                         ELTDUREQ
00262      10  FILLER                PIC X(26).                         ELTDUREQ
00263    05  WS-MAX-DAYS REDEFINES WS-UNLIMITED.                        ELTDUREQ
00264      10  FILLER                PIC X.                             ELTDUREQ
00265      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTDUREQ
00266      10  FILLER                PIC X.                             ELTDUREQ
00267      10  WS-DAYS-LITERAL       PIC X(04).                         ELTDUREQ
00268      10  FILLER                PIC X.                             ELTDUREQ
00269      10  WS-DTL-MAX-IND        PIC X(20).                         ELTDUREQ
00270      10  FILLER                PIC X(16).                         ELTDUREQ
00271                                                                   ELTDUREQ
00272    05  WS-DAYS-REDUCED.                                           ELTDUREQ
00273      10  FILLER                    PIC X(24) VALUE                ELTDUREQ
00274          ' BASIC DAYS ARE REDUCED '.                              ELTDUREQ
00275      10  WS-DTL-DAYS-REDUCED-APL   PIC Z9.                        ELTDUREQ
00276      10  FILLER                    PIC X(05) VALUE ' FOR '.       ELTDUREQ
00277      10  WS-DTL-DAYS-REDUCED-BASE  PIC Z9.                        ELTDUREQ
00278      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTDUREQ
00279                                                                   ELTDUREQ
00280    05  WS-SPILLOVER-FL-RT-PER-D.                                  ELTDUREQ
00281      10  FILLER                    PIC X(29)                      ELTDUREQ
00282          VALUE 'SPILLOVER FLAT RATE PER DIEM '.                   ELTDUREQ
00283      10  WS-DTL-SPILLOVER-FL-RT    PIC X(46) VALUE SPACES.        ELTDUREQ
00284      10  FILLER                    PIC X(04) VALUE LOW-VALUES.    ELTDUREQ
00285 *************************************************************     ELTDUREQ
00286    05  WS-CERTIFICATION.                                          ELTDUREQ
00287      10  FILLER                    PIC X(44) VALUE                ELTDUREQ
00288          'CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.          ELTDUREQ
00289      10  FILLER                    PIC X(35) VALUE LOW-VALUES.    ELTDUREQ
00290 ****************************************************************  ELTDUREQ
00291                                                                   ELTDUREQ
00292 *************************************************************     ELTDUREQ
00293    05  WS-RECERTIFICATION.                                        ELTDUREQ
00294      10  FILLER                    PIC X(56) VALUE                ELTDUREQ
00295      'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE IS: '.  ELTDUREQ
00296      10  FILLER                    PIC X(23) VALUE LOW-VALUES.    ELTDUREQ
00297 ****************************************************************  ELTDUREQ
00298    05  WS-BASIC-1-1.                                              ELTDUREQ
00299      10  FILLER                    PIC X(10) VALUE                ELTDUREQ
00300          '   BASIC: '.                                            ELTDUREQ
00301      10  WS-DTL-BASIC-1-1          PIC X(69) VALUE SPACES.        ELTDUREQ
00302                                                                   ELTDUREQ
00303    05  WS-BASIC-1-2.                                              ELTDUREQ
00304      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00305      10  WS-DTL-BASIC-1-2          PIC X(76) VALUE SPACES.        ELTDUREQ
00306                                                                   ELTDUREQ
00307    05  WS-BASIC-2-1.                                              ELTDUREQ
00308      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00309      10  WS-DTL-BASIC-2-1          PIC X(76) VALUE SPACES.        ELTDUREQ
00310                                                                   ELTDUREQ
00311    05  WS-BASIC-2-2.                                              ELTDUREQ
00312      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00313      10  WS-DTL-BASIC-2-2          PIC X(76) VALUE SPACES.        ELTDUREQ
00314                                                                   ELTDUREQ
00315    05  WS-BASIC-3-1.                                              ELTDUREQ
00316      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00317      10  WS-DTL-BASIC-3-1          PIC X(76) VALUE SPACES.        ELTDUREQ
00318                                                                   ELTDUREQ
00319    05  WS-BASIC-3-2.                                              ELTDUREQ
00320      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00321      10  WS-DTL-BASIC-3-2          PIC X(76) VALUE SPACES.        ELTDUREQ
00322                                                                   ELTDUREQ
00323    05  WS-BASIC-4-1.                                              ELTDUREQ
00324      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00325      10  WS-DTL-BASIC-4-1          PIC X(76) VALUE SPACES.        ELTDUREQ
00326                                                                   ELTDUREQ
00327    05  WS-BASIC-4-2.                                              ELTDUREQ
00328      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00329      10  WS-DTL-BASIC-4-2          PIC X(76) VALUE SPACES.        ELTDUREQ
00330 ***********************************************************       ELTDUREQ
00331    05  WS-SUPP-1-1.                                               ELTDUREQ
00332      10 FILLER                     PIC X(17) VALUE                ELTDUREQ
00333          '   SUPPLEMENTAL: '.                                     ELTDUREQ
00334      10  WS-DTL-SUPP-1-1           PIC X(62) VALUE SPACES.        ELTDUREQ
00335                                                                   ELTDUREQ
00336    05  WS-SUPP-1-2.                                               ELTDUREQ
00337      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00338      10  WS-DTL-SUPP-1-2           PIC X(76) VALUE SPACES.        ELTDUREQ
00339                                                                   ELTDUREQ
00340    05  WS-SUPP-2-1.                                               ELTDUREQ
00341      10 FILLER                     PIC X(03) VALUE SPACES.        ELTDUREQ
00342      10  WS-DTL-SUPP-2-1           PIC X(76) VALUE SPACES.        ELTDUREQ
00343                                                                   ELTDUREQ
00344    05  WS-SUPP-2-2.                                               ELTDUREQ
00345      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00346      10  WS-DTL-SUPP-2-2           PIC X(76) VALUE SPACES.        ELTDUREQ
00347                                                                   ELTDUREQ
00348    05  WS-SUPP-3-1.                                               ELTDUREQ
00349      10 FILLER                     PIC X(03) VALUE SPACES.        ELTDUREQ
00350      10  WS-DTL-SUPP-3-1           PIC X(76) VALUE SPACES.        ELTDUREQ
00351                                                                   ELTDUREQ
00352    05  WS-SUPP-3-2.                                               ELTDUREQ
00353      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00354      10  WS-DTL-SUPP-3-2           PIC X(76) VALUE SPACES.        ELTDUREQ
00355                                                                   ELTDUREQ
00356    05  WS-SUPP-4-1.                                               ELTDUREQ
00357      10 FILLER                     PIC X(03) VALUE SPACES.        ELTDUREQ
00358      10  WS-DTL-SUPP-4-1           PIC X(76) VALUE SPACES.        ELTDUREQ
00359                                                                   ELTDUREQ
00360    05  WS-SUPP-4-2.                                               ELTDUREQ
00361      10  FILLER                    PIC X(03) VALUE SPACES.        ELTDUREQ
00362      10  WS-DTL-SUPP-4-2           PIC X(76) VALUE SPACES.        ELTDUREQ
00363 *************************************************************     ELTDUREQ
00364    05  WS-CONTACT-CONTRACT.                                       ELTDUREQ
00365      10  FILLER                    PIC X(50)                      ELTDUREQ
00366        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTDUREQ
00367      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTDUREQ
00368                                                                   ELTDUREQ
00369    05  WS-CONTRACT-RELATED.                                       ELTDUREQ
00370      10  FILLER                    PIC X(49)                      ELTDUREQ
00371        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTDUREQ
00372      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDUREQ
00373                                                                   ELTDUREQ
00374    05  WS-PVE-TEXT.                                               ELTDUREQ
00375      10  FILLER                    PIC X(49)                      ELTDUREQ
00376        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.     '. ELTDUREQ
00377      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTDUREQ
00378                                                                   ELTDUREQ
00379    05  WS-PGM-ERROR.                                              ELTDUREQ
00380      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTDUREQ
00381      10  FILLER                    PIC X(35)  VALUE               ELTDUREQ
00382         '***  P R O G R A M   E R R O R  ***'.                    ELTDUREQ
00383      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTDUREQ
00384                                                                   ELTDUREQ
00385    05  WS-BAD-INST-PROF-SEL.                                      ELTDUREQ
00386      10  FILLER                    PIC XX VALUE SPACE.            ELTDUREQ
00387      10  FILLER                    PIC X(47) VALUE                ELTDUREQ
00388         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTDUREQ
00389      10  FILLER                    PIC X(48) VALUE                ELTDUREQ
00390         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTDUREQ
00391      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTDUREQ
00392                                                                   ELTDUREQ
00393    05  WS-BAD-IN-OUT-SEL.                                         ELTDUREQ
00394      10  FILLER                    PIC X(08) VALUE SPACE.         ELTDUREQ
00395      10  FILLER                    PIC X(51) VALUE                ELTDUREQ
00396         '*** I N V A L I D   I N P U T   /   O U T P U T ***'.    ELTDUREQ
00397      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTDUREQ
00398                                                                   ELTDUREQ
00399  01  WS-END                            PIC X(16)  VALUE           ELTDUREQ
00400      '*** W/S ENDS ***'.                                          ELTDUREQ
00401 /             L I N K A G E   S E C T I O N                       ELTDUREQ
00402  LINKAGE SECTION.                                                 ELTDUREQ
00403  01  DFHCOMMAREA.                                                 ELTDUREQ
00404      COPY ELSCOMMC.                                               ELTDUREQ
00405 /                   B L L   C E L L S                             ELTDUREQ
00406 /  *** CIA  AREA ***                                              ELTDUREQ
00407      COPY ELSCIA2C.                                               ELTDUREQ
00408 /  *** IO PARM AREA ***                                           ELTDUREQ
00409      COPY ELSIOPMC.                                               ELTDUREQ
00410 /  *** KEY AREA ***                                               ELTDUREQ
00411      COPY ELSKEYSC.                                               ELTDUREQ
00412 /  *** OUTPUT TEXT AREA ***                                       ELTDUREQ
00413      COPY ELSOUTPC.                                               ELTDUREQ
00414 /  *** TOPIC SELECTION AREA ***                                   ELTDUREQ
00415      COPY ELSSSCBC.                                               ELTDUREQ
00416 /  *** CODE MANUAL INTERFACE ***                                  ELTDUREQ
00417      COPY ELSCMIFC.                                               ELTDUREQ
00418 /  *** CODE MANUAL DESCRIPTION AREA ***                           ELTDUREQ
00419      COPY ELSCMDSC.                                               ELTDUREQ
00420 /  *** BENEFIT PROVISION TABLE ***                                ELTDUREQ
00421      COPY ELSPRVNC.                                               ELTDUREQ
00422 /  *** COMPRESSION TEXT WORK-AREA ***                             ELTDUREQ
00423      COPY ELSTCWAC.                                               ELTDUREQ
00424 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTDUREQ
00425      COPY ELSPLGSW.                                               ELTDUREQ
00426                                                                   ELTDUREQ
00427 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTDUREQ
00428      COPY ELSPLGTB.                                               ELTDUREQ
00429 /        G R O U P   S P E C I F I C   R E C O R D                ELTDUREQ
00430  01  GROUP-SPECIFIC-RECORD.                                       ELTDUREQ
00431      COPY GCGROUPC.                                               ELTDUREQ
00432 /                  M A I N L I N E                                ELTDUREQ
00433  PROCEDURE DIVISION.                                              ELTDUREQ
00434                                                                   ELTDUREQ
00435 ******************************************************************ELTDUREQ
00436 *                                                                 ELTDUREQ
00437 *   PERFORM THE MAINLINE OPERATIONS.                              ELTDUREQ
00438 *                                                                 ELTDUREQ
00439 ******************************************************************ELTDUREQ
00440  0000-MAINLINE.                                                   ELTDUREQ
00441                                                                   ELTDUREQ
00442      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTDUREQ
00443         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTDUREQ
00444         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTDUREQ
00445                                                                   ELTDUREQ
00446      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTDUREQ
00447          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTDUREQ
00448                                                                   ELTDUREQ
00449      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTDUREQ
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00451          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTDUREQ
00452                                                                   ELTDUREQ
00453      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTDUREQ
00454      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00455          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTDUREQ
00456                                                                   ELTDUREQ
00457      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTDUREQ
00458      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00459          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTDUREQ
00460                                                                   ELTDUREQ
00461      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTDUREQ
00462      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00463          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTDUREQ
00464                                                                   ELTDUREQ
00465      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTDUREQ
00466      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00467          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTDUREQ
00468                                                                   ELTDUREQ
00469      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTDUREQ
00470      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00471          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTDUREQ
00472                                                                   ELTDUREQ
00473      MOVE '0'  TO  WS-CHAR-0.                                     ELTDUREQ
00474                                                                   ELTDUREQ
00475      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTDUREQ
00476                                                                   ELTDUREQ
00477      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTDUREQ
00478              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTDUREQ
00479                                                                   ELTDUREQ
00480      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTDUREQ
00481                                                                   ELTDUREQ
00482      SET CIA-STG-GETMAIN  TO TRUE.                                ELTDUREQ
00483                                                                   ELTDUREQ
00484      EXEC CICS LINK                                               ELTDUREQ
00485                PROGRAM('ELUSTGMG')                                ELTDUREQ
00486                COMMAREA(DFHCOMMAREA)                              ELTDUREQ
00487      END-EXEC.                                                    ELTDUREQ
00488                                                                   ELTDUREQ
00489      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTDUREQ
00490      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00491          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTDUREQ
00492                                                                   ELTDUREQ
00493      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTDUREQ
00494         (SSB-SERV-CLASS-IP OR  SSB-SERV-CLASS-BOTH)               ELTDUREQ
00495         PERFORM 1000-INSTITUTIONAL-IP-RTNE THRU                   ELTDUREQ
00496                 1099-EXIT.                                        ELTDUREQ
00497                                                                   ELTDUREQ
00498      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH) AND        ELTDUREQ
00499         (SSB-SERV-CLASS-IP OR  SSB-SERV-CLASS-BOTH)               ELTDUREQ
00500         PERFORM 2000-PROFESSIONAL-IP-RTNE   THRU                  ELTDUREQ
00501                 2099-EXIT.                                        ELTDUREQ
00502                                                                   ELTDUREQ
00503      IF (SSB-PROV-CLASS-INST OR    SSB-PROV-CLASS-BOTH) AND       ELTDUREQ
00504         (SSB-SERV-CLASS-OP OR  SSB-SERV-CLASS-BOTH)               ELTDUREQ
00505         PERFORM 1100-INSTITUTIONAL-OP-RTNE THRU                   ELTDUREQ
00506                 1199-EXIT.                                        ELTDUREQ
00507                                                                   ELTDUREQ
00508      IF (SSB-PROV-CLASS-PROF OR   SSB-PROV-CLASS-BOTH) AND        ELTDUREQ
00509         (SSB-SERV-CLASS-OP OR  SSB-SERV-CLASS-BOTH)               ELTDUREQ
00510         PERFORM 2100-PROFESSIONAL-OP-RTNE  THRU                   ELTDUREQ
00511                 2199-EXIT.                                        ELTDUREQ
00512                                                                   ELTDUREQ
00513      IF NOT SSB-PROV-CLASS-INST AND    NOT SSB-PROV-CLASS-PROF    ELTDUREQ
00514                                   AND  NOT SSB-PROV-CLASS-BOTH    ELTDUREQ
00515         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTDUREQ
00516         MOVE WS-BAD-INST-PROF-SEL  TO  COF-DTL-LINE(5)            ELTDUREQ
00517         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTDUREQ
00518         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTDUREQ
00519         MOVE SPACE  TO  COF-FUNCTION                              ELTDUREQ
00520         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
00521               COMMAREA(DFHCOMMAREA)                               ELTDUREQ
00522         END-EXEC.                                                 ELTDUREQ
00523                                                                   ELTDUREQ
00524      IF NOT SSB-SERV-CLASS-IP AND NOT SSB-SERV-CLASS-OP           ELTDUREQ
00525                                       AND  NOT SSB-SERV-CLASS-BOTHELTDUREQ
00526         MOVE WS-PGM-ERROR  TO  COF-DTL-LINE(3)                    ELTDUREQ
00527         MOVE WS-BAD-IN-OUT-SEL  TO  COF-DTL-LINE(5)               ELTDUREQ
00528         MOVE ZERO  TO  COF-NBR-HDR-LINES                          ELTDUREQ
00529         MOVE +5  TO  COF-NBR-DTL-LINES                            ELTDUREQ
00530         MOVE SPACE  TO  COF-FUNCTION                              ELTDUREQ
00531         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
00532               COMMAREA(DFHCOMMAREA)                               ELTDUREQ
00533         END-EXEC.                                                 ELTDUREQ
00534                                                                   ELTDUREQ
00535 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTDUREQ
00536      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00537      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
00538      MOVE 'E'  TO  COF-FUNCTION.                                  ELTDUREQ
00539                                                                   ELTDUREQ
00540      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDUREQ
00541                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
00542      END-EXEC.                                                    ELTDUREQ
00543                                                                   ELTDUREQ
00544  0099-RETURN.                                                     ELTDUREQ
00545      EXEC CICS RETURN   END-EXEC.                                 ELTDUREQ
00546                                                                   ELTDUREQ
00547      GOBACK.                                                      ELTDUREQ
00548                                                                   ELTDUREQ
00549 /        I N S T I T U T I O N A L  I P   R T N E                 ELTDUREQ
00550 ***************************************************************** ELTDUREQ
00551 *        I N S T I T U T I O N A L  I P   R T N E                 ELTDUREQ
00552 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTDUREQ
00553 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTDUREQ
00554 ***************************************************************** ELTDUREQ
00555  1000-INSTITUTIONAL-IP-RTNE.                                      ELTDUREQ
00556      MOVE '1000'  TO  WS-PARA-ID.                                 ELTDUREQ
00557                                                                   ELTDUREQ
00558      MOVE 'INSTITUTIONAL INPATIENT' TO WS-HDR2-IPOP-MSG.          ELTDUREQ
00559      MOVE 'Y'            TO WS-FIRSTTIME-IND.                     ELTDUREQ
00560      MOVE WS-INST-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTDUREQ
00561      PERFORM 1010-MOVE-IN-INST-IP                                 ELTDUREQ
00562         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDUREQ
00563         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTDUREQ
00564                                                                   ELTDUREQ
00565      GO TO 1020-CALL-COVERAGE.                                    ELTDUREQ
00566  1010-MOVE-IN-INST-IP.                                            ELTDUREQ
00567      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDUREQ
00568      MOVE WS-INST-IP-LIST(WS-SUB)  TO                             ELTDUREQ
00569                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDUREQ
00570      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDUREQ
00571                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDUREQ
00572                                                                   ELTDUREQ
00573  1020-CALL-COVERAGE.                                              ELTDUREQ
00574      MOVE '1020'              TO  WS-PARA-ID.                     ELTDUREQ
00575      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTDUREQ
00576 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDUREQ
00577      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
00578      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00579      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
00580                                                                   ELTDUREQ
00581      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
00582      END-EXEC.                                                    ELTDUREQ
00583 ****************************************************              ELTDUREQ
00584                                                                   ELTDUREQ
00585      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTDUREQ
00586      MOVE WS-DUR-MEDICAL-EQUIP TO SSB-TOPIC-PHRASE.               ELTDUREQ
00587                                                                   ELTDUREQ
00588      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
00589      END-EXEC.                                                    ELTDUREQ
00590                                                                   ELTDUREQ
00591 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDUREQ
00592      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00593      MOVE ' '  TO  COF-FUNCTION.                                  ELTDUREQ
00594      IF PVN-COVG-NONE                                             ELTDUREQ
00595        MOVE +2 TO COF-NBR-DTL-LINES.                              ELTDUREQ
00596      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
00597      END-EXEC.                                                    ELTDUREQ
00598 *4/15 END OF TEMPORARY CODE                                       ELTDUREQ
00599                                                                   ELTDUREQ
00600      IF PVN-COVG-NONE                                             ELTDUREQ
00601         GO TO 1099-EXIT.                                          ELTDUREQ
00602                                                                   ELTDUREQ
00603                                                                   ELTDUREQ
00604      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDUREQ
00605         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTDUREQ
00606            PSP-PROVN-PRICING-METHD,                               ELTDUREQ
00607            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDUREQ
00608            PSP-TRANSF-OTHER-RESP-IND,                             ELTDUREQ
00609            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDUREQ
00610            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDUREQ
00611            PSP-SPILL-OVER-DED-APL-IND,                            ELTDUREQ
00612            PSP-CERTFN-REQRM-IND,                                  ELTDUREQ
00613            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDUREQ
00614            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDUREQ
00615            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDUREQ
00616            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDUREQ
00617            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDUREQ
00618            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDUREQ
00619            PSB-CERTN-REPETN-REQRD-IND.                            ELTDUREQ
00620                                                                   ELTDUREQ
00621      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
00622      END-EXEC.                                                    ELTDUREQ
00623                                                                   ELTDUREQ
00624      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDUREQ
00625      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00626          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDUREQ
00627                                                                   ELTDUREQ
00628      PERFORM 1030-FIND-FIRST-NONZERO                              ELTDUREQ
00629         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDUREQ
00630         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTDUREQ
00631      GO TO 1099-EXIT.                                             ELTDUREQ
00632                                                                   ELTDUREQ
00633  1030-FIND-FIRST-NONZERO.                                         ELTDUREQ
00634      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDUREQ
00635      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDUREQ
00636         NEXT SENTENCE                                             ELTDUREQ
00637      ELSE                                                         ELTDUREQ
00638         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTDUREQ
00639                                                                   ELTDUREQ
00640  1040-BUILD-SCREEN-LINES.                                         ELTDUREQ
00641      MOVE '1040'  TO  WS-PARA-ID.                                 ELTDUREQ
00642                                                                   ELTDUREQ
00643      SET PLT-INDEX1 TO                                            ELTDUREQ
00644         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDUREQ
00645                                                                   ELTDUREQ
00646      IF WS-NOT-FIRST-TIME                                         ELTDUREQ
00647         MOVE 'P'    TO COF-FUNCTION                               ELTDUREQ
00648         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDUREQ
00649         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
00650                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
00651         END-EXEC                                                  ELTDUREQ
00652      ELSE                                                         ELTDUREQ
00653        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDUREQ
00654                                                                   ELTDUREQ
00655      MOVE +1    TO  WS-CIA.                                       ELTDUREQ
00656      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTDUREQ
00657                                                                   ELTDUREQ
00658      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDUREQ
00659         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDUREQ
00660            SET PLT-INDEX2  TO  2                                  ELTDUREQ
00661         ELSE                                                      ELTDUREQ
00662            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTDUREQ
00663            GO TO 1099-EXIT                                        ELTDUREQ
00664      ELSE                                                         ELTDUREQ
00665         SET PLT-INDEX2  TO  1.                                    ELTDUREQ
00666      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDUREQ
00667                                                                   ELTDUREQ
00668      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
00669      ADD +1                 TO WS-CIA.                            ELTDUREQ
00670                                                                   ELTDUREQ
00671      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
00672      ADD +1                 TO WS-CIA.                            ELTDUREQ
00673                                                                   ELTDUREQ
00674      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTDUREQ
00675                    WS-SERVICES-PAYBLE-SW                          ELTDUREQ
00676                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTDUREQ
00677                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTDUREQ
00678                                                                   ELTDUREQ
00679      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTDUREQ
00680        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTDUREQ
00681        UNTIL WS-SUB2 GREATER WS-INST-IP-CNT.                      ELTDUREQ
00682                                                                   ELTDUREQ
00683      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDUREQ
00684 *************************************************************     ELTDUREQ
00685 **** SERVICES MAY BE RENDERED                                     ELTDUREQ
00686 **************************************************************    ELTDUREQ
00687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
00688        SET  PLT-INDEX2       TO  1                                ELTDUREQ
00689       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00690         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00691              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
00692                                                                   ELTDUREQ
00693      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00694        SET PLT-INDEX2        TO 2                                 ELTDUREQ
00695       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00696         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00697              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
00698                                                                   ELTDUREQ
00699      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTDUREQ
00700          ADD  +1               TO  WS-CIA                         ELTDUREQ
00701          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTDUREQ
00702          ADD  +1               TO  WS-CIA.                        ELTDUREQ
00703                                                                   ELTDUREQ
00704      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
00705        SET  PLT-INDEX2       TO  1                                ELTDUREQ
00706       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00707         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00708          PERFORM 4000-PLACE-OF-TREATMENT                          ELTDUREQ
00709             THRU 4000-EXIT                                        ELTDUREQ
00710          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
00711          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
00712          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
00713          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
00714          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTDUREQ
00715          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
00716          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
00717             ADD +1                TO WS-CIA                       ELTDUREQ
00718             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
00719             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
00720                THRU 3000-EXIT                                     ELTDUREQ
00721          ELSE                                                     ELTDUREQ
00722           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
00723                THRU 3000-EXIT.                                    ELTDUREQ
00724                                                                   ELTDUREQ
00725      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00726        SET PLT-INDEX2        TO 2                                 ELTDUREQ
00727       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00728         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00729          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
00730          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
00731          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
00732          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
00733          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
00734          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTDUREQ
00735          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
00736          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
00737             ADD +1                TO WS-CIA                       ELTDUREQ
00738             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
00739             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
00740                THRU 3000-EXIT                                     ELTDUREQ
00741          ELSE                                                     ELTDUREQ
00742           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
00743                THRU 3000-EXIT.                                    ELTDUREQ
00744                                                                   ELTDUREQ
00745 **************************************************************    ELTDUREQ
00746 **** SERVICES ARE PAYABLE                                         ELTDUREQ
00747 **************************************************************    ELTDUREQ
00748      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
00749         SET  PLT-INDEX2           TO  1                           ELTDUREQ
00750        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00751         NOT EQUAL '19'                                            ELTDUREQ
00752         PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT.             ELTDUREQ
00753                                                                   ELTDUREQ
00754      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00755         SET  PLT-INDEX2             TO  2                         ELTDUREQ
00756        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00757         NOT EQUAL '19'                                            ELTDUREQ
00758         PERFORM 4200-PAYABLE-AS-SUPP  THRU 4200-EXIT.             ELTDUREQ
00759                                                                   ELTDUREQ
00760 ****************************************************************  ELTDUREQ
00761 *** CERTIFICATION REQUIRED   TRANSLATION                          ELTDUREQ
00762 ****************************************************************  ELTDUREQ
00763      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
00764         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
00765         NEXT SENTENCE                                             ELTDUREQ
00766      ELSE                                                         ELTDUREQ
00767         PERFORM 6000-INST-CERTIFICATION  THRU 6000-EXIT.          ELTDUREQ
00768                                                                   ELTDUREQ
00769 ****************************************************************  ELTDUREQ
00770 *** REQUIREMENT FOR RECERTIFICATION TRANSLATION                   ELTDUREQ
00771 ****************************************************************  ELTDUREQ
00772      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
00773         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
00774         NEXT SENTENCE                                             ELTDUREQ
00775      ELSE                                                         ELTDUREQ
00776         PERFORM 6100-INST-RECERTIFICATION  THRU 6199-EXIT.        ELTDUREQ
00777                                                                   ELTDUREQ
00778 ****************************************************************  ELTDUREQ
00779 *** SPILLOVER COINS AND DEDUCTIBLE                                ELTDUREQ
00780 ****************************************************************  ELTDUREQ
00781      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00782         SET  PLT-INDEX2          TO  2                            ELTDUREQ
00783         PERFORM 4300-SPILLOVER-COINS   THRU 4300-EXIT             ELTDUREQ
00784         PERFORM 4400-SPILLOVER-DEDUCT  THRU 4400-EXIT.            ELTDUREQ
00785                                                                   ELTDUREQ
00786 ****************************************************************  ELTDUREQ
00787 *** TRANSFER TO OTHER RESPONSIBILITY                              ELTDUREQ
00788 ****************************************************************  ELTDUREQ
00789                                                                   ELTDUREQ
00790      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
00791         SET  PLT-INDEX2           TO  1                           ELTDUREQ
00792        IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
00793         NOT EQUAL ZERO                                            ELTDUREQ
00794         PERFORM 5400-TRANSFER-OTHER-RESP-IND  THRU                ELTDUREQ
00795                 5400-EXIT.                                        ELTDUREQ
00796                                                                   ELTDUREQ
00797      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00798         SET  PLT-INDEX2             TO  2                         ELTDUREQ
00799        IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
00800         NOT EQUAL ZERO                                            ELTDUREQ
00801         PERFORM 5400-TRANSFER-OTHER-RESP-IND  THRU                ELTDUREQ
00802                 5400-EXIT.                                        ELTDUREQ
00803                                                                   ELTDUREQ
00804 ***************************************************************** ELTDUREQ
00805 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTDUREQ
00806 ***************************************************************   ELTDUREQ
00807      PERFORM 4650-SCAN-TAB         THRU 4650-EXIT.                ELTDUREQ
00808      PERFORM 4675-PAY-CONSID-TEXT  THRU 4675-EXIT.                ELTDUREQ
00809                                                                   ELTDUREQ
00810  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTDUREQ
00811      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDUREQ
00812                                                                   ELTDUREQ
00813      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDUREQ
00814         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDUREQ
00815         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDUREQ
00816         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDUREQ
00817                                                   CMF-CODE-VALUE  ELTDUREQ
00818         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDUREQ
00819         STRING CMF-DESCR-LINE (1) ' '                             ELTDUREQ
00820                CMF-DESCR-LINE (2)                                 ELTDUREQ
00821                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
00822         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDUREQ
00823         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTDUREQ
00824         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTDUREQ
00825         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTDUREQ
00826         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDUREQ
00827         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDUREQ
00828         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
00829         IF WS-CIA  <  20                                          ELTDUREQ
00830            ADD +1  TO  WS-CIA                                     ELTDUREQ
00831            MOVE ZERO  TO                                          ELTDUREQ
00832                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDUREQ
00833         ELSE                                                      ELTDUREQ
00834            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDUREQ
00835                COMMAREA(DFHCOMMAREA)                              ELTDUREQ
00836            END-EXEC                                               ELTDUREQ
00837            MOVE +1  TO  WS-CIA                                    ELTDUREQ
00838            MOVE ZERO  TO                                          ELTDUREQ
00839                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDUREQ
00840                                                                   ELTDUREQ
00841  1090-PROBLEM-WITH-INDICES.                                       ELTDUREQ
00842                                                                   ELTDUREQ
00843      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDUREQ
00844      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
00845                                                                   ELTDUREQ
00846      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00847      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
00848                                                                   ELTDUREQ
00849      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDUREQ
00850                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
00851      END-EXEC.                                                    ELTDUREQ
00852                                                                   ELTDUREQ
00853  1099-EXIT.  EXIT.                                                ELTDUREQ
00854 /        I N S T I T U T I O N A L  O P   R T N E                 ELTDUREQ
00855 ***************************************************************** ELTDUREQ
00856  1100-INSTITUTIONAL-OP-RTNE.                                      ELTDUREQ
00857      MOVE '1100'  TO  WS-PARA-ID.                                 ELTDUREQ
00858                                                                   ELTDUREQ
00859      MOVE 'INSTITUTIONAL OUTPATIENT' TO WS-HDR2-IPOP-MSG.         ELTDUREQ
00860      MOVE 'Y'            TO WS-FIRSTTIME-IND.                     ELTDUREQ
00861      MOVE WS-INST-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTDUREQ
00862      PERFORM 1111-MOVE-IN-INST-OP                                 ELTDUREQ
00863         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDUREQ
00864         UNTIL WS-SUB  >  WS-INST-OP-CNT.                          ELTDUREQ
00865                                                                   ELTDUREQ
00866      GO TO 1120-CALL-COVERAGE.                                    ELTDUREQ
00867  1111-MOVE-IN-INST-OP.                                            ELTDUREQ
00868      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDUREQ
00869      MOVE WS-INST-OP-LIST(WS-SUB)  TO                             ELTDUREQ
00870                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDUREQ
00871      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDUREQ
00872                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDUREQ
00873                                                                   ELTDUREQ
00874  1120-CALL-COVERAGE.                                              ELTDUREQ
00875      MOVE '1120'              TO  WS-PARA-ID.                     ELTDUREQ
00876      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTDUREQ
00877 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDUREQ
00878      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
00879      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00880      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
00881                                                                   ELTDUREQ
00882      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
00883      END-EXEC.                                                    ELTDUREQ
00884 ****************************************************              ELTDUREQ
00885                                                                   ELTDUREQ
00886      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTDUREQ
00887      MOVE WS-DUR-MEDICAL-EQUIP TO SSB-TOPIC-PHRASE.               ELTDUREQ
00888                                                                   ELTDUREQ
00889      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
00890      END-EXEC.                                                    ELTDUREQ
00891                                                                   ELTDUREQ
00892 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDUREQ
00893      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
00894      MOVE ' '  TO  COF-FUNCTION.                                  ELTDUREQ
00895      IF PVN-COVG-NONE                                             ELTDUREQ
00896        MOVE +2 TO COF-NBR-DTL-LINES.                              ELTDUREQ
00897      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
00898      END-EXEC.                                                    ELTDUREQ
00899 *4/15 END OF TEMPORARY CODE                                       ELTDUREQ
00900                                                                   ELTDUREQ
00901      IF PVN-COVG-NONE                                             ELTDUREQ
00902         GO TO 1199-EXIT.                                          ELTDUREQ
00903                                                                   ELTDUREQ
00904      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDUREQ
00905                                                                   ELTDUREQ
00906      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDUREQ
00907            PSP-PROVN-PRICING-METHD,                               ELTDUREQ
00908            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDUREQ
00909            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDUREQ
00910            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDUREQ
00911            PSP-SPILL-OVER-DED-APL-IND,                            ELTDUREQ
00912            PSP-CERTFN-REQRM-IND,                                  ELTDUREQ
00913            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDUREQ
00914            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDUREQ
00915            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDUREQ
00916            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDUREQ
00917            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDUREQ
00918            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDUREQ
00919            PSB-CERTN-REPETN-REQRD-IND.                            ELTDUREQ
00920                                                                   ELTDUREQ
00921      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
00922      END-EXEC.                                                    ELTDUREQ
00923                                                                   ELTDUREQ
00924      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDUREQ
00925      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
00926          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDUREQ
00927                                                                   ELTDUREQ
00928      PERFORM 1130-FIND-FIRST-NONZERO                              ELTDUREQ
00929         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDUREQ
00930         UNTIL WS-SUB  >  WS-INST-OP-CNT.                          ELTDUREQ
00931                                                                   ELTDUREQ
00932      GO TO 1199-EXIT.                                             ELTDUREQ
00933  1130-FIND-FIRST-NONZERO.                                         ELTDUREQ
00934      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDUREQ
00935      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDUREQ
00936         NEXT SENTENCE                                             ELTDUREQ
00937      ELSE                                                         ELTDUREQ
00938         PERFORM 1140-BUILD-SCREEN-LINES.                          ELTDUREQ
00939                                                                   ELTDUREQ
00940  1140-BUILD-SCREEN-LINES.                                         ELTDUREQ
00941      MOVE '1140'  TO  WS-PARA-ID.                                 ELTDUREQ
00942                                                                   ELTDUREQ
00943      SET PLT-INDEX1 TO                                            ELTDUREQ
00944         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDUREQ
00945                                                                   ELTDUREQ
00946      IF WS-NOT-FIRST-TIME                                         ELTDUREQ
00947         MOVE 'P'    TO COF-FUNCTION                               ELTDUREQ
00948         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDUREQ
00949         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
00950                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
00951         END-EXEC                                                  ELTDUREQ
00952      ELSE                                                         ELTDUREQ
00953        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDUREQ
00954                                                                   ELTDUREQ
00955      MOVE +1    TO  WS-CIA.                                       ELTDUREQ
00956                                                                   ELTDUREQ
00957      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDUREQ
00958         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDUREQ
00959            SET PLT-INDEX2  TO  2                                  ELTDUREQ
00960         ELSE                                                      ELTDUREQ
00961            PERFORM 1190-PROBLEM-WITH-INDICES                      ELTDUREQ
00962            GO TO 1199-EXIT                                        ELTDUREQ
00963      ELSE                                                         ELTDUREQ
00964         SET PLT-INDEX2  TO  1.                                    ELTDUREQ
00965      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDUREQ
00966                                                                   ELTDUREQ
00967      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
00968      ADD +1                 TO WS-CIA.                            ELTDUREQ
00969                                                                   ELTDUREQ
00970      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
00971      ADD +1                 TO WS-CIA.                            ELTDUREQ
00972                                                                   ELTDUREQ
00973      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTDUREQ
00974                    WS-SERVICES-PAYBLE-SW                          ELTDUREQ
00975                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTDUREQ
00976                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTDUREQ
00977                                                                   ELTDUREQ
00978      PERFORM 1150-ZERO-ALL-WITH-SAME-NO                           ELTDUREQ
00979        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTDUREQ
00980        UNTIL WS-SUB2 GREATER WS-INST-OP-CNT.                      ELTDUREQ
00981                                                                   ELTDUREQ
00982      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDUREQ
00983 *************************************************************     ELTDUREQ
00984 **** SERVICES MAY BE RENDERED                                     ELTDUREQ
00985 **************************************************************    ELTDUREQ
00986      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
00987        SET  PLT-INDEX2       TO  1                                ELTDUREQ
00988       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00989         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00990              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
00991                                                                   ELTDUREQ
00992      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
00993        SET PLT-INDEX2        TO 2                                 ELTDUREQ
00994       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
00995         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
00996              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
00997                                                                   ELTDUREQ
00998      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTDUREQ
00999          ADD  +1               TO  WS-CIA                         ELTDUREQ
01000          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTDUREQ
01001          ADD  +1               TO  WS-CIA.                        ELTDUREQ
01002                                                                   ELTDUREQ
01003      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01004        SET  PLT-INDEX2       TO  1                                ELTDUREQ
01005       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01006         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01007          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01008          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01009          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01010          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01011          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01012          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTDUREQ
01013          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01014          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01015             ADD +1                TO WS-CIA                       ELTDUREQ
01016             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01017             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01018                THRU 3000-EXIT                                     ELTDUREQ
01019          ELSE                                                     ELTDUREQ
01020           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01021                THRU 3000-EXIT.                                    ELTDUREQ
01022                                                                   ELTDUREQ
01023      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01024        SET PLT-INDEX2        TO 2                                 ELTDUREQ
01025       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01026         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01027          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01028          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01029          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01030          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01031          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01032          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTDUREQ
01033          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01034          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01035             ADD +1                TO WS-CIA                       ELTDUREQ
01036             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01037             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01038                THRU 3000-EXIT                                     ELTDUREQ
01039          ELSE                                                     ELTDUREQ
01040           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01041                THRU 3000-EXIT.                                    ELTDUREQ
01042                                                                   ELTDUREQ
01043 **************************************************************    ELTDUREQ
01044 **** SERVICES ARE PAYABLE                                         ELTDUREQ
01045 **************************************************************    ELTDUREQ
01046      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01047         SET  PLT-INDEX2           TO  1                           ELTDUREQ
01048        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01049         NOT EQUAL '19'                                            ELTDUREQ
01050         PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT.             ELTDUREQ
01051                                                                   ELTDUREQ
01052      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01053         SET  PLT-INDEX2             TO  2                         ELTDUREQ
01054        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01055         NOT EQUAL '19'                                            ELTDUREQ
01056         PERFORM 4200-PAYABLE-AS-SUPP  THRU 4200-EXIT.             ELTDUREQ
01057                                                                   ELTDUREQ
01058 ****************************************************************  ELTDUREQ
01059 *** CERTIFICATION REQUIRED   TRANSLATION                          ELTDUREQ
01060 ****************************************************************  ELTDUREQ
01061      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01062         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01063         NEXT SENTENCE                                             ELTDUREQ
01064      ELSE                                                         ELTDUREQ
01065         PERFORM 6000-INST-CERTIFICATION  THRU 6000-EXIT.          ELTDUREQ
01066                                                                   ELTDUREQ
01067 ****************************************************************  ELTDUREQ
01068 *** REQUIREMENT FOR RECERTIFICATION TRANSLATION                   ELTDUREQ
01069 ****************************************************************  ELTDUREQ
01070      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01071         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01072         NEXT SENTENCE                                             ELTDUREQ
01073      ELSE                                                         ELTDUREQ
01074         PERFORM 6100-INST-RECERTIFICATION  THRU 6199-EXIT.        ELTDUREQ
01075                                                                   ELTDUREQ
01076 ****************************************************************  ELTDUREQ
01077 *** SPILLOVER COINS AND DEDUCTIBLE                                ELTDUREQ
01078 ****************************************************************  ELTDUREQ
01079      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01080         SET  PLT-INDEX2          TO  2                            ELTDUREQ
01081         PERFORM 4300-SPILLOVER-COINS   THRU 4300-EXIT             ELTDUREQ
01082         PERFORM 4400-SPILLOVER-DEDUCT  THRU 4400-EXIT.            ELTDUREQ
01083                                                                   ELTDUREQ
01084 ***************************************************************** ELTDUREQ
01085 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTDUREQ
01086 ***************************************************************   ELTDUREQ
01087      PERFORM 4650-SCAN-TAB         THRU 4650-EXIT.                ELTDUREQ
01088      PERFORM 4675-PAY-CONSID-TEXT  THRU 4675-EXIT.                ELTDUREQ
01089                                                                   ELTDUREQ
01090  1150-ZERO-ALL-WITH-SAME-NO.                                      ELTDUREQ
01091      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDUREQ
01092                                                                   ELTDUREQ
01093      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDUREQ
01094         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDUREQ
01095         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDUREQ
01096         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDUREQ
01097                                                   CMF-CODE-VALUE  ELTDUREQ
01098         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDUREQ
01099         STRING CMF-DESCR-LINE (1) ' '                             ELTDUREQ
01100                CMF-DESCR-LINE (2)                                 ELTDUREQ
01101                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
01102         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDUREQ
01103         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTDUREQ
01104         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTDUREQ
01105         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTDUREQ
01106         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDUREQ
01107         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDUREQ
01108         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
01109         IF WS-CIA  <  20                                          ELTDUREQ
01110            ADD +1  TO  WS-CIA                                     ELTDUREQ
01111            MOVE ZERO  TO                                          ELTDUREQ
01112                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDUREQ
01113         ELSE                                                      ELTDUREQ
01114            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDUREQ
01115                COMMAREA(DFHCOMMAREA)                              ELTDUREQ
01116            END-EXEC                                               ELTDUREQ
01117            MOVE +1  TO  WS-CIA                                    ELTDUREQ
01118            MOVE ZERO  TO                                          ELTDUREQ
01119                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDUREQ
01120                                                                   ELTDUREQ
01121  1190-PROBLEM-WITH-INDICES.                                       ELTDUREQ
01122                                                                   ELTDUREQ
01123      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDUREQ
01124      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
01125                                                                   ELTDUREQ
01126      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01127      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
01128                                                                   ELTDUREQ
01129      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01130      END-EXEC.                                                    ELTDUREQ
01131                                                                   ELTDUREQ
01132  1199-EXIT.            EXIT.                                      ELTDUREQ
01133                                                                   ELTDUREQ
01134 /        P R O F E S S I O N A L   I P   R T N E                  ELTDUREQ
01135 ***************************************************************** ELTDUREQ
01136  2000-PROFESSIONAL-IP-RTNE.                                       ELTDUREQ
01137      MOVE '2000'  TO  WS-PARA-ID.                                 ELTDUREQ
01138                                                                   ELTDUREQ
01139      MOVE 'PROFESSIONAL INPATIENT' TO WS-HDR2-IPOP-MSG.           ELTDUREQ
01140      MOVE 'Y'            TO WS-FIRSTTIME-IND.                     ELTDUREQ
01141                                                                   ELTDUREQ
01142      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTDUREQ
01143      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTDUREQ
01144         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDUREQ
01145         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTDUREQ
01146                                                                   ELTDUREQ
01147      GO TO 2020-CALL-COVERAGE.                                    ELTDUREQ
01148  2010-MOVE-IN-PROF-IP.                                            ELTDUREQ
01149      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDUREQ
01150      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTDUREQ
01151                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDUREQ
01152      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDUREQ
01153                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDUREQ
01154                                                                   ELTDUREQ
01155  2020-CALL-COVERAGE.                                              ELTDUREQ
01156      MOVE '2020'              TO  WS-PARA-ID.                     ELTDUREQ
01157      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTDUREQ
01158 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDUREQ
01159      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
01160      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01161      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
01162                                                                   ELTDUREQ
01163      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01164      END-EXEC.                                                    ELTDUREQ
01165 ****************************************************              ELTDUREQ
01166      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTDUREQ
01167      MOVE WS-DUR-MEDICAL-EQUIP TO SSB-TOPIC-PHRASE.               ELTDUREQ
01168                                                                   ELTDUREQ
01169      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
01170      END-EXEC.                                                    ELTDUREQ
01171 *****************************************************             ELTDUREQ
01172                                                                   ELTDUREQ
01173 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDUREQ
01174      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01175      MOVE ' '  TO  COF-FUNCTION.                                  ELTDUREQ
01176      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01177      END-EXEC.                                                    ELTDUREQ
01178 *4/15 END OF TEMPORARY CODE                                       ELTDUREQ
01179                                                                   ELTDUREQ
01180      IF PVN-COVG-NONE                                             ELTDUREQ
01181         GO TO 2099-EXIT.                                          ELTDUREQ
01182                                                                   ELTDUREQ
01183      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDUREQ
01184      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTDUREQ
01185            PSP-PROVN-PRICING-METHD,                               ELTDUREQ
01186            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDUREQ
01187            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDUREQ
01188            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDUREQ
01189            PSP-SPILL-OVER-DED-APL-IND,                            ELTDUREQ
01190            PSP-CERTFN-REQRM-IND,                                  ELTDUREQ
01191            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDUREQ
01192            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDUREQ
01193            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDUREQ
01194            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDUREQ
01195            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDUREQ
01196            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDUREQ
01197            PSE-CERTN-REPETN-REQRD-IND.                            ELTDUREQ
01198                                                                   ELTDUREQ
01199      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
01200      END-EXEC.                                                    ELTDUREQ
01201                                                                   ELTDUREQ
01202      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDUREQ
01203      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
01204          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDUREQ
01205                                                                   ELTDUREQ
01206      PERFORM 2030-FIND-FIRST-NONZERO                              ELTDUREQ
01207         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDUREQ
01208         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTDUREQ
01209                                                                   ELTDUREQ
01210      GO TO 2099-EXIT.                                             ELTDUREQ
01211  2030-FIND-FIRST-NONZERO.                                         ELTDUREQ
01212      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDUREQ
01213      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDUREQ
01214         NEXT SENTENCE                                             ELTDUREQ
01215      ELSE                                                         ELTDUREQ
01216         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTDUREQ
01217                                                                   ELTDUREQ
01218  2040-BUILD-SCREEN-LINES.                                         ELTDUREQ
01219      MOVE '2040'  TO  WS-PARA-ID.                                 ELTDUREQ
01220                                                                   ELTDUREQ
01221      SET PLT-INDEX1 TO                                            ELTDUREQ
01222         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDUREQ
01223                                                                   ELTDUREQ
01224      IF WS-NOT-FIRST-TIME                                         ELTDUREQ
01225         MOVE 'P'    TO COF-FUNCTION                               ELTDUREQ
01226         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDUREQ
01227         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
01228                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
01229         END-EXEC                                                  ELTDUREQ
01230      ELSE                                                         ELTDUREQ
01231        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDUREQ
01232                                                                   ELTDUREQ
01233      MOVE +1    TO  WS-CIA.                                       ELTDUREQ
01234                                                                   ELTDUREQ
01235      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTDUREQ
01236                                                                   ELTDUREQ
01237      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDUREQ
01238         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDUREQ
01239            SET PLT-INDEX2  TO  2                                  ELTDUREQ
01240         ELSE                                                      ELTDUREQ
01241            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTDUREQ
01242            GO TO 2099-EXIT                                        ELTDUREQ
01243      ELSE                                                         ELTDUREQ
01244         SET PLT-INDEX2  TO  1.                                    ELTDUREQ
01245      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDUREQ
01246                                                                   ELTDUREQ
01247      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
01248      ADD +1                 TO WS-CIA.                            ELTDUREQ
01249                                                                   ELTDUREQ
01250      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
01251      ADD +1                 TO WS-CIA.                            ELTDUREQ
01252                                                                   ELTDUREQ
01253      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTDUREQ
01254                    WS-SERVICES-PAYBLE-SW                          ELTDUREQ
01255                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTDUREQ
01256                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTDUREQ
01257                                                                   ELTDUREQ
01258      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTDUREQ
01259        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTDUREQ
01260        UNTIL WS-SUB2 GREATER WS-PROF-IP-CNT.                      ELTDUREQ
01261                                                                   ELTDUREQ
01262      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDUREQ
01263 *************************************************************     ELTDUREQ
01264 **** SERVICES MAY BE RENDERED                                     ELTDUREQ
01265 **************************************************************    ELTDUREQ
01266      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01267        SET  PLT-INDEX2       TO  1                                ELTDUREQ
01268       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01269         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01270         IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
01271           NOT = SPACES                                            ELTDUREQ
01272              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
01273                                                                   ELTDUREQ
01274      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01275        SET PLT-INDEX2        TO 2                                 ELTDUREQ
01276       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01277         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01278         IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
01279           NOT = SPACES                                            ELTDUREQ
01280              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
01281                                                                   ELTDUREQ
01282      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTDUREQ
01283          ADD  +1               TO  WS-CIA                         ELTDUREQ
01284          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTDUREQ
01285          ADD  +1               TO  WS-CIA.                        ELTDUREQ
01286                                                                   ELTDUREQ
01287      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01288        SET  PLT-INDEX2       TO  1                                ELTDUREQ
01289       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01290         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01291         IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
01292           NOT = SPACES                                            ELTDUREQ
01293          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01294          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01295          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01296          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01297          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01298          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTDUREQ
01299          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01300          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01301             ADD +1                TO WS-CIA                       ELTDUREQ
01302             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01303             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01304                THRU 3000-EXIT                                     ELTDUREQ
01305          ELSE                                                     ELTDUREQ
01306           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01307                THRU 3000-EXIT.                                    ELTDUREQ
01308                                                                   ELTDUREQ
01309      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01310        SET PLT-INDEX2        TO 2                                 ELTDUREQ
01311       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01312         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01313         IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
01314           NOT = SPACES                                            ELTDUREQ
01315          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01316          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01317          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01318          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01319          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01320          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTDUREQ
01321          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01322          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01323             ADD +1                TO WS-CIA                       ELTDUREQ
01324             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01325             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01326                THRU 3000-EXIT                                     ELTDUREQ
01327          ELSE                                                     ELTDUREQ
01328           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01329                THRU 3000-EXIT.                                    ELTDUREQ
01330                                                                   ELTDUREQ
01331 **************************************************************    ELTDUREQ
01332 **** SERVICES ARE PAYABLE                                         ELTDUREQ
01333 **************************************************************    ELTDUREQ
01334      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01335         SET  PLT-INDEX2           TO  1                           ELTDUREQ
01336        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01337         NOT EQUAL '19'                                            ELTDUREQ
01338         PERFORM 4100-PAYABLE-AS-BASIC THRU 4100-EXIT.             ELTDUREQ
01339                                                                   ELTDUREQ
01340      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01341         SET  PLT-INDEX2             TO  2                         ELTDUREQ
01342        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01343         NOT EQUAL '19'                                            ELTDUREQ
01344         PERFORM 4200-PAYABLE-AS-SUPP  THRU 4200-EXIT.             ELTDUREQ
01345                                                                   ELTDUREQ
01346 ****************************************************************  ELTDUREQ
01347 *** CERTIFICATION REQUIRED   TRANSLATION                          ELTDUREQ
01348 ****************************************************************  ELTDUREQ
01349      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01350         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01351         NEXT SENTENCE                                             ELTDUREQ
01352      ELSE                                                         ELTDUREQ
01353         PERFORM 7000-PROF-CERTIFICATION  THRU 7000-EXIT.          ELTDUREQ
01354                                                                   ELTDUREQ
01355 ****************************************************************  ELTDUREQ
01356 *** REQUIREMENT FOR RECERTIFICATION TRANSLATION                   ELTDUREQ
01357 ****************************************************************  ELTDUREQ
01358      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01359         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01360         NEXT SENTENCE                                             ELTDUREQ
01361      ELSE                                                         ELTDUREQ
01362         PERFORM 7100-PROF-RECERTIFICATION  THRU 7100-EXIT.        ELTDUREQ
01363                                                                   ELTDUREQ
01364 ****************************************************************  ELTDUREQ
01365 *** SPILLOVER COINS AND DEDUCTIBLE                                ELTDUREQ
01366 ****************************************************************  ELTDUREQ
01367      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01368         SET  PLT-INDEX2          TO  2                            ELTDUREQ
01369         PERFORM 4300-SPILLOVER-COINS    THRU 4300-EXIT            ELTDUREQ
01370         PERFORM 4400-SPILLOVER-DEDUCT   THRU 4400-EXIT.           ELTDUREQ
01371                                                                   ELTDUREQ
01372 ***************************************************************** ELTDUREQ
01373 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTDUREQ
01374 ***************************************************************   ELTDUREQ
01375      PERFORM 4650-SCAN-TAB         THRU 4650-EXIT.                ELTDUREQ
01376      PERFORM 4675-PAY-CONSID-TEXT  THRU 4675-EXIT.                ELTDUREQ
01377                                                                   ELTDUREQ
01378  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTDUREQ
01379      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDUREQ
01380                                                                   ELTDUREQ
01381      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDUREQ
01382         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDUREQ
01383         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDUREQ
01384         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDUREQ
01385                                                   CMF-CODE-VALUE  ELTDUREQ
01386         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDUREQ
01387         STRING CMF-DESCR-LINE (1) ' '                             ELTDUREQ
01388                CMF-DESCR-LINE (2)                                 ELTDUREQ
01389                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
01390         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDUREQ
01391         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTDUREQ
01392         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTDUREQ
01393         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTDUREQ
01394         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDUREQ
01395         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDUREQ
01396         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
01397         IF WS-CIA  <  20                                          ELTDUREQ
01398            ADD +1  TO  WS-CIA                                     ELTDUREQ
01399            MOVE ZERO  TO                                          ELTDUREQ
01400                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDUREQ
01401         ELSE                                                      ELTDUREQ
01402            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDUREQ
01403                COMMAREA(DFHCOMMAREA)                              ELTDUREQ
01404            END-EXEC                                               ELTDUREQ
01405            MOVE +1  TO  WS-CIA                                    ELTDUREQ
01406            MOVE ZERO  TO                                          ELTDUREQ
01407                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDUREQ
01408                                                                   ELTDUREQ
01409  2090-PROBLEM-WITH-INDICES.                                       ELTDUREQ
01410                                                                   ELTDUREQ
01411      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDUREQ
01412      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
01413                                                                   ELTDUREQ
01414      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01415      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
01416                                                                   ELTDUREQ
01417      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01418      END-EXEC.                                                    ELTDUREQ
01419                                                                   ELTDUREQ
01420  2099-EXIT.            EXIT.                                      ELTDUREQ
01421 /                                                                 ELTDUREQ
01422 ***************************************************************** ELTDUREQ
01423  2100-PROFESSIONAL-OP-RTNE.                                       ELTDUREQ
01424 ****************************************************              ELTDUREQ
01425      MOVE '2100'  TO  WS-PARA-ID.                                 ELTDUREQ
01426                                                                   ELTDUREQ
01427      MOVE 'PROFESSIONAL OUTPATIENT' TO WS-HDR2-IPOP-MSG.          ELTDUREQ
01428                                                                   ELTDUREQ
01429      MOVE 'Y'     TO WS-FIRSTTIME-IND.                            ELTDUREQ
01430                                                                   ELTDUREQ
01431      MOVE WS-PROF-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTDUREQ
01432      PERFORM 2110-MOVE-IN-PROF-OP                                 ELTDUREQ
01433         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTDUREQ
01434         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTDUREQ
01435                                                                   ELTDUREQ
01436      GO TO 2120-CALL-COVERAGE.                                    ELTDUREQ
01437  2110-MOVE-IN-PROF-OP.                                            ELTDUREQ
01438      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTDUREQ
01439      MOVE WS-PROF-OP-LIST(WS-SUB)  TO                             ELTDUREQ
01440                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTDUREQ
01441      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTDUREQ
01442                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTDUREQ
01443                                                                   ELTDUREQ
01444  2120-CALL-COVERAGE.                                              ELTDUREQ
01445      MOVE '2120'              TO  WS-PARA-ID.                     ELTDUREQ
01446      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTDUREQ
01447 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTDUREQ
01448      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
01449      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01450      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
01451                                                                   ELTDUREQ
01452      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01453      END-EXEC.                                                    ELTDUREQ
01454 ****************************************************              ELTDUREQ
01455                                                                   ELTDUREQ
01456      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTDUREQ
01457      MOVE WS-DUR-MEDICAL-EQUIP    TO SSB-TOPIC-PHRASE.            ELTDUREQ
01458                                                                   ELTDUREQ
01459      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
01460      END-EXEC.                                                    ELTDUREQ
01461                                                                   ELTDUREQ
01462 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTDUREQ
01463      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01464      MOVE ' '  TO  COF-FUNCTION.                                  ELTDUREQ
01465                                                                   ELTDUREQ
01466      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01467      END-EXEC.                                                    ELTDUREQ
01468 *4/15 END OF TEMPORARY CODE ************************************* ELTDUREQ
01469                                                                   ELTDUREQ
01470      IF PVN-COVG-NONE                                             ELTDUREQ
01471         GO TO 2199-EXIT.                                          ELTDUREQ
01472                                                                   ELTDUREQ
01473      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTDUREQ
01474                                                                   ELTDUREQ
01475         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTDUREQ
01476            PSP-PROVN-PRICING-METHD,                               ELTDUREQ
01477            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTDUREQ
01478            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTDUREQ
01479            PSP-SPILL-OVER-COINS-APL-IND,                          ELTDUREQ
01480            PSP-SPILL-OVER-DED-APL-IND,                            ELTDUREQ
01481            PSP-CERTFN-REQRM-IND,                                  ELTDUREQ
01482            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTDUREQ
01483            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTDUREQ
01484            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTDUREQ
01485            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTDUREQ
01486            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTDUREQ
01487            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTDUREQ
01488            PSE-CERTN-REPETN-REQRD-IND.                            ELTDUREQ
01489                                                                   ELTDUREQ
01490      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTDUREQ
01491      END-EXEC.                                                    ELTDUREQ
01492                                                                   ELTDUREQ
01493      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTDUREQ
01494      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
01495          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTDUREQ
01496                                                                   ELTDUREQ
01497      PERFORM 2130-FIND-FIRST-NONZERO                              ELTDUREQ
01498         VARYING WS-SUB  FROM  +1  BY  +1                          ELTDUREQ
01499         UNTIL WS-SUB  >  WS-PROF-OP-CNT.                          ELTDUREQ
01500                                                                   ELTDUREQ
01501      GO TO 2199-EXIT.                                             ELTDUREQ
01502  2130-FIND-FIRST-NONZERO.                                         ELTDUREQ
01503      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTDUREQ
01504      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTDUREQ
01505         NEXT SENTENCE                                             ELTDUREQ
01506      ELSE                                                         ELTDUREQ
01507         PERFORM 2140-BUILD-SCREEN-LINES.                          ELTDUREQ
01508                                                                   ELTDUREQ
01509  2140-BUILD-SCREEN-LINES.                                         ELTDUREQ
01510      MOVE '2140'  TO  WS-PARA-ID.                                 ELTDUREQ
01511                                                                   ELTDUREQ
01512      SET PLT-INDEX1 TO                                            ELTDUREQ
01513         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTDUREQ
01514                                                                   ELTDUREQ
01515      IF WS-NOT-FIRST-TIME                                         ELTDUREQ
01516         MOVE 'P'    TO COF-FUNCTION                               ELTDUREQ
01517         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTDUREQ
01518         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTDUREQ
01519                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
01520         END-EXEC                                                  ELTDUREQ
01521      ELSE                                                         ELTDUREQ
01522        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTDUREQ
01523                                                                   ELTDUREQ
01524      MOVE +1    TO  WS-CIA.                                       ELTDUREQ
01525      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTDUREQ
01526                                                                   ELTDUREQ
01527      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTDUREQ
01528         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTDUREQ
01529            SET PLT-INDEX2  TO  2                                  ELTDUREQ
01530         ELSE                                                      ELTDUREQ
01531            PERFORM 2190-PROBLEM-WITH-INDICES                      ELTDUREQ
01532            GO TO 2199-EXIT                                        ELTDUREQ
01533      ELSE                                                         ELTDUREQ
01534         SET PLT-INDEX2  TO  1.                                    ELTDUREQ
01535      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTDUREQ
01536                                                                   ELTDUREQ
01537      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
01538      ADD +1                 TO WS-CIA.                            ELTDUREQ
01539                                                                   ELTDUREQ
01540      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTDUREQ
01541      ADD +1                 TO WS-CIA.                            ELTDUREQ
01542                                                                   ELTDUREQ
01543      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTDUREQ
01544                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTDUREQ
01545                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTDUREQ
01546                                                                   ELTDUREQ
01547      PERFORM 2150-ZERO-ALL-WITH-SAME-NO                           ELTDUREQ
01548         VARYING WS-SUB2 FROM WS-SUB BY +1                         ELTDUREQ
01549         UNTIL WS-SUB2 GREATER WS-PROF-OP-CNT.                     ELTDUREQ
01550                                                                   ELTDUREQ
01551      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTDUREQ
01552 *************************************************************     ELTDUREQ
01553 **** SERVICES MAY BE RENDERED                                     ELTDUREQ
01554 **************************************************************    ELTDUREQ
01555      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01556        SET  PLT-INDEX2       TO  1                                ELTDUREQ
01557       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01558         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01559              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
01560                                                                   ELTDUREQ
01561      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01562        SET PLT-INDEX2        TO 2                                 ELTDUREQ
01563       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01564         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01565              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTDUREQ
01566                                                                   ELTDUREQ
01567      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTDUREQ
01568          ADD  +1               TO  WS-CIA                         ELTDUREQ
01569          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTDUREQ
01570          ADD  +1               TO  WS-CIA.                        ELTDUREQ
01571                                                                   ELTDUREQ
01572      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01573        SET  PLT-INDEX2       TO  1                                ELTDUREQ
01574       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01575         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01576          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01577          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01578          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01579          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01580          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01581          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTDUREQ
01582          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01583          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01584             ADD +1                TO WS-CIA                       ELTDUREQ
01585             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01586             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01587                THRU 3000-EXIT                                     ELTDUREQ
01588          ELSE                                                     ELTDUREQ
01589           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01590                THRU 3000-EXIT.                                    ELTDUREQ
01591                                                                   ELTDUREQ
01592      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01593        SET PLT-INDEX2        TO 2                                 ELTDUREQ
01594       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01595         NOT = ZEROS AND NOT = LOW-VALUES                          ELTDUREQ
01596          PERFORM 4000-PLACE-OF-TREATMENT THRU 4000-EXIT           ELTDUREQ
01597          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01598          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01599          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01600          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01601          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTDUREQ
01602          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01603          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTDUREQ
01604             ADD +1                TO WS-CIA                       ELTDUREQ
01605             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTDUREQ
01606             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
01607                THRU 3000-EXIT                                     ELTDUREQ
01608          ELSE                                                     ELTDUREQ
01609           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
01610                THRU 3000-EXIT.                                    ELTDUREQ
01611                                                                   ELTDUREQ
01612 **************************************************************    ELTDUREQ
01613 **** SERVICES ARE PAYABLE                                         ELTDUREQ
01614 **************************************************************    ELTDUREQ
01615      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTDUREQ
01616         SET  PLT-INDEX2           TO  1                           ELTDUREQ
01617        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01618         NOT EQUAL '19'                                            ELTDUREQ
01619         PERFORM 4100-PAYABLE-AS-BASIC  THRU 4100-EXIT.            ELTDUREQ
01620                                                                   ELTDUREQ
01621      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01622         SET  PLT-INDEX2             TO  2                         ELTDUREQ
01623        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01624         NOT EQUAL '19'                                            ELTDUREQ
01625         PERFORM 4200-PAYABLE-AS-SUPP  THRU 4200-EXIT.             ELTDUREQ
01626                                                                   ELTDUREQ
01627 ****************************************************************  ELTDUREQ
01628 *** CERTIFICATION REQUIRED   TRANSLATION                          ELTDUREQ
01629 ****************************************************************  ELTDUREQ
01630      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01631         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01632         NEXT SENTENCE                                             ELTDUREQ
01633      ELSE                                                         ELTDUREQ
01634         PERFORM 7000-PROF-CERTIFICATION  THRU 7000-EXIT.          ELTDUREQ
01635                                                                   ELTDUREQ
01636 ****************************************************************  ELTDUREQ
01637 *** REQUIREMENT FOR RECERTIFICATION TRANSLATION                   ELTDUREQ
01638 ****************************************************************  ELTDUREQ
01639      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTDUREQ
01640         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTDUREQ
01641         NEXT SENTENCE                                             ELTDUREQ
01642      ELSE                                                         ELTDUREQ
01643         PERFORM 7100-PROF-RECERTIFICATION  THRU 7100-EXIT.        ELTDUREQ
01644                                                                   ELTDUREQ
01645 **************************************************************    ELTDUREQ
01646 ***  SPILL OVER COINS AND DEDUCT                                  ELTDUREQ
01647 **************************************************************    ELTDUREQ
01648      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTDUREQ
01649         SET  PLT-INDEX2          TO  2                            ELTDUREQ
01650         PERFORM 4300-SPILLOVER-COINS      THRU 4300-EXIT          ELTDUREQ
01651         PERFORM 4400-SPILLOVER-DEDUCT     THRU 4400-EXIT.         ELTDUREQ
01652                                                                   ELTDUREQ
01653 **************************************************************    ELTDUREQ
01654 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTDUREQ
01655 ***************************************************************   ELTDUREQ
01656      PERFORM 4650-SCAN-TAB        THRU 4650-EXIT.                 ELTDUREQ
01657      PERFORM 4675-PAY-CONSID-TEXT THRU 4675-EXIT.                 ELTDUREQ
01658 *************************************************************     ELTDUREQ
01659                                                                   ELTDUREQ
01660  2150-ZERO-ALL-WITH-SAME-NO.                                      ELTDUREQ
01661      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTDUREQ
01662                                                                   ELTDUREQ
01663      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTDUREQ
01664         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTDUREQ
01665         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTDUREQ
01666         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTDUREQ
01667                                                   CMF-CODE-VALUE  ELTDUREQ
01668         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTDUREQ
01669         STRING CMF-DESCR-LINE (1) ' '                             ELTDUREQ
01670                CMF-DESCR-LINE (2)                                 ELTDUREQ
01671                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
01672         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTDUREQ
01673         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTDUREQ
01674         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTDUREQ
01675         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTDUREQ
01676         PERFORM TCPR-000-TEXT-UNSTRING                            ELTDUREQ
01677         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTDUREQ
01678         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
01679         IF WS-CIA  <  20                                          ELTDUREQ
01680            ADD +1  TO  WS-CIA                                     ELTDUREQ
01681            MOVE ZERO  TO                                          ELTDUREQ
01682                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTDUREQ
01683         ELSE                                                      ELTDUREQ
01684            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTDUREQ
01685                COMMAREA(DFHCOMMAREA)                              ELTDUREQ
01686            END-EXEC                                               ELTDUREQ
01687            MOVE +1  TO  WS-CIA                                    ELTDUREQ
01688            MOVE ZERO  TO                                          ELTDUREQ
01689                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTDUREQ
01690 ******************************************************************ELTDUREQ
01691  2190-PROBLEM-WITH-INDICES.                                       ELTDUREQ
01692                                                                   ELTDUREQ
01693      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTDUREQ
01694      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTDUREQ
01695                                                                   ELTDUREQ
01696      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01697      MOVE 'P'  TO  COF-FUNCTION.                                  ELTDUREQ
01698                                                                   ELTDUREQ
01699      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTDUREQ
01700      END-EXEC.                                                    ELTDUREQ
01701                                                                   ELTDUREQ
01702  2199-EXIT.            EXIT.                                      ELTDUREQ
01703                                                                   ELTDUREQ
01704 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTDUREQ
01705  3000-OUTPUT-TEXT.                                                ELTDUREQ
01706      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTDUREQ
01707      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTDUREQ
01708      MOVE ' '  TO  COF-FUNCTION.                                  ELTDUREQ
01709                                                                   ELTDUREQ
01710      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTDUREQ
01711                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
01712      END-EXEC.                                                    ELTDUREQ
01713      MOVE +1   TO WS-CIA.                                         ELTDUREQ
01714  3000-EXIT.  EXIT.                                                ELTDUREQ
01715 /                                                                 ELTDUREQ
01716  4000-PLACE-OF-TREATMENT.                                         ELTDUREQ
01717      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDUREQ
01718      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTDUREQ
01719      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
01720                                               TO  CMF-CODE-VALUE. ELTDUREQ
01721      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01722      STRING                                                       ELTDUREQ
01723             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
01724             CMF-DESCR-LINE (2)                                    ELTDUREQ
01725              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTDUREQ
01726      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01727  4000-EXIT.  EXIT.                                                ELTDUREQ
01728 /                                                                 ELTDUREQ
01729  4100-PAYABLE-AS-BASIC.                                           ELTDUREQ
01730      MOVE WS-YES               TO  WS-SERVICES-PAYBLE-SW.         ELTDUREQ
01731      MOVE LOW-VALUES           TO  COF-DTL-LINE(WS-CIA).          ELTDUREQ
01732      ADD  +1                   TO  WS-CIA.                        ELTDUREQ
01733      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTDUREQ
01734      ADD  +1                   TO  WS-CIA.                        ELTDUREQ
01735                                                                   ELTDUREQ
01736      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDUREQ
01737          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
01738          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTDUREQ
01739          GO TO 4100-OUTPUT-TEXT.                                  ELTDUREQ
01740      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTDUREQ
01741      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTDUREQ
01742      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTDUREQ
01743                                                CMF-CODE-VALUE     ELTDUREQ
01744      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01745      STRING CMF-DESCR-LINE(1) ' '                                 ELTDUREQ
01746             CMF-DESCR-LINE(2)                                     ELTDUREQ
01747                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTDUREQ
01748      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01749      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTDUREQ
01750      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTDUREQ
01751      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTDUREQ
01752      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01753      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
01754                                         =  ZEROS                  ELTDUREQ
01755       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
01756                                         =  ZEROS                  ELTDUREQ
01757                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTDUREQ
01758                 MOVE WS-BASIC               TO                    ELTDUREQ
01759                         COF-DTL-LINE(WS-CIA)                      ELTDUREQ
01760       ELSE                                                        ELTDUREQ
01761          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTDUREQ
01762          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDUREQ
01763                                        TO  WS-DTL-PERCENT         ELTDUREQ
01764          MOVE SPACES          TO TCAR-FROM-AREA                   ELTDUREQ
01765          STRING WS-DTL-PP,                                        ELTDUREQ
01766                 WS-DTL-PERCENT,                                   ELTDUREQ
01767                 WS-PERCENT,                                       ELTDUREQ
01768                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDUREQ
01769          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTDUREQ
01770          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01771          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01772          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01773          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01774          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTDUREQ
01775          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTDUREQ
01776      ELSE                                                         ELTDUREQ
01777        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTDUREQ
01778        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTDUREQ
01779                                     TO WS-DTL-PERCENT             ELTDUREQ
01780        MOVE SPACES          TO TCAR-FROM-AREA                     ELTDUREQ
01781        STRING WS-DTL-PP,                                          ELTDUREQ
01782               WS-DTL-PERCENT,                                     ELTDUREQ
01783               WS-PERCENT,                                         ELTDUREQ
01784                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
01785        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTDUREQ
01786        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
01787        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
01788        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
01789        PERFORM TCPR-000-TEXT-UNSTRING                             ELTDUREQ
01790        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTDUREQ
01791        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTDUREQ
01792  4100-OUTPUT-TEXT.                                                ELTDUREQ
01793      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDUREQ
01794            ADD +1                TO WS-CIA                        ELTDUREQ
01795            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDUREQ
01796            PERFORM 3000-OUTPUT-TEXT                               ELTDUREQ
01797                THRU 3000-EXIT                                     ELTDUREQ
01798      ELSE                                                         ELTDUREQ
01799         PERFORM 3000-OUTPUT-TEXT                                  ELTDUREQ
01800                THRU 3000-EXIT.                                    ELTDUREQ
01801  4100-EXIT.  EXIT.                                                ELTDUREQ
01802 /                                                                 ELTDUREQ
01803  4200-PAYABLE-AS-SUPP.                                            ELTDUREQ
01804      IF WS-SERVICES-PAYBLE-SW NOT EQUAL WS-YES                    ELTDUREQ
01805        MOVE LOW-VALUES TO  COF-DTL-LINE(WS-CIA)                   ELTDUREQ
01806        ADD  +1         TO  WS-CIA                                 ELTDUREQ
01807        MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA)         ELTDUREQ
01808        ADD  +1                   TO  WS-CIA.                      ELTDUREQ
01809                                                                   ELTDUREQ
01810      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTDUREQ
01811          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
01812          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTDUREQ
01813          GO TO 4200-OUTPUT-TEXT.                                  ELTDUREQ
01814      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTDUREQ
01815      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTDUREQ
01816      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTDUREQ
01817                                                CMF-CODE-VALUE     ELTDUREQ
01818      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01819      STRING CMF-DESCR-LINE(1) ' '                                 ELTDUREQ
01820             CMF-DESCR-LINE(2)                                     ELTDUREQ
01821                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTDUREQ
01822      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01823      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTDUREQ
01824      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTDUREQ
01825      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTDUREQ
01826      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01827      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
01828                                         =  ZEROS                  ELTDUREQ
01829       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
01830                                         =  ZEROS                  ELTDUREQ
01831                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTDUREQ
01832                MOVE WS-SUPPLEMENTAL        TO                     ELTDUREQ
01833                         COF-DTL-LINE(WS-CIA)                      ELTDUREQ
01834       ELSE                                                        ELTDUREQ
01835          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTDUREQ
01836          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTDUREQ
01837                                        TO  WS-DTL-PERCENT         ELTDUREQ
01838          MOVE SPACES          TO TCAR-FROM-AREA                   ELTDUREQ
01839          STRING WS-DTL-PP,                                        ELTDUREQ
01840                 WS-DTL-PERCENT,                                   ELTDUREQ
01841                 WS-PERCENT,                                       ELTDUREQ
01842                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTDUREQ
01843          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTDUREQ
01844          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTDUREQ
01845          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTDUREQ
01846          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTDUREQ
01847          PERFORM TCPR-000-TEXT-UNSTRING                           ELTDUREQ
01848          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTDUREQ
01849          MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)    ELTDUREQ
01850      ELSE                                                         ELTDUREQ
01851        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTDUREQ
01852        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTDUREQ
01853                                    TO WS-DTL-PERCENT              ELTDUREQ
01854        MOVE SPACES          TO TCAR-FROM-AREA                     ELTDUREQ
01855        STRING WS-DTL-PP,                                          ELTDUREQ
01856               WS-DTL-PERCENT,                                     ELTDUREQ
01857               WS-PERCENT,                                         ELTDUREQ
01858                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTDUREQ
01859        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTDUREQ
01860        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
01861        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
01862        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
01863        PERFORM TCPR-000-TEXT-UNSTRING                             ELTDUREQ
01864        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTDUREQ
01865        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTDUREQ
01866  4200-OUTPUT-TEXT.                                                ELTDUREQ
01867      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDUREQ
01868            ADD +1                TO WS-CIA                        ELTDUREQ
01869            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTDUREQ
01870            PERFORM 3000-OUTPUT-TEXT                               ELTDUREQ
01871                THRU 3000-EXIT                                     ELTDUREQ
01872      ELSE                                                         ELTDUREQ
01873         PERFORM 3000-OUTPUT-TEXT                                  ELTDUREQ
01874                THRU 3000-EXIT.                                    ELTDUREQ
01875  4200-EXIT.  EXIT.                                                ELTDUREQ
01876 /                                                                 ELTDUREQ
01877  4300-SPILLOVER-COINS.                                            ELTDUREQ
01878      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
01879            = '0'   OR LOW-VALUES                                  ELTDUREQ
01880           GO TO 4300-EXIT.                                        ELTDUREQ
01881      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTDUREQ
01882      ADD +1     TO  WS-CIA.                                       ELTDUREQ
01883      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDUREQ
01884      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTDUREQ
01885      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTDUREQ
01886                       TO CMF-CODE-VALUE.                          ELTDUREQ
01887      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01888      STRING WS-SPILLOVER-COINS                                    ELTDUREQ
01889             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
01890             CMF-DESCR-LINE (2)                                    ELTDUREQ
01891              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTDUREQ
01892      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01893      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDUREQ
01894      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTDUREQ
01895      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTDUREQ
01896      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01897      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDUREQ
01898      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDUREQ
01899            ADD +1                TO  WS-CIA                       ELTDUREQ
01900            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTDUREQ
01901      PERFORM 3000-OUTPUT-TEXT                                     ELTDUREQ
01902                THRU 3000-EXIT.                                    ELTDUREQ
01903  4300-EXIT.  EXIT.                                                ELTDUREQ
01904 /                                                                 ELTDUREQ
01905  4400-SPILLOVER-DEDUCT.                                           ELTDUREQ
01906      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
01907            = '0'   OR LOW-VALUES                                  ELTDUREQ
01908           GO TO 4400-EXIT.                                        ELTDUREQ
01909      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTDUREQ
01910      ADD +1     TO  WS-CIA.                                       ELTDUREQ
01911      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTDUREQ
01912      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTDUREQ
01913      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
01914                       TO CMF-CODE-VALUE                           ELTDUREQ
01915      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01916      STRING WS-SPILLOVER-DEDBL                                    ELTDUREQ
01917             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
01918             CMF-DESCR-LINE (2)                                    ELTDUREQ
01919              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTDUREQ
01920      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01921      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDUREQ
01922      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTDUREQ
01923      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTDUREQ
01924      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01925      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDUREQ
01926      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTDUREQ
01927            ADD +1                TO  WS-CIA                       ELTDUREQ
01928            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTDUREQ
01929      PERFORM 3000-OUTPUT-TEXT                                     ELTDUREQ
01930                THRU 3000-EXIT.                                    ELTDUREQ
01931  4400-EXIT.  EXIT.                                                ELTDUREQ
01932 /                                                                 ELTDUREQ
01933  4500-MAX-VISITS.                                                 ELTDUREQ
01934      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTDUREQ
01935      MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  TO      ELTDUREQ
01936                                                 CMF-CODE-VALUE    ELTDUREQ
01937      MOVE 'BPE'                TO  CMF-RECORD-PREFIX.             ELTDUREQ
01938      MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME.       ELTDUREQ
01939      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
01940      STRING CMF-DESCR-LINE (1) ' '                                ELTDUREQ
01941             CMF-DESCR-LINE (2)                                    ELTDUREQ
01942              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTDUREQ
01943      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01944      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDUREQ
01945      MOVE +63              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTDUREQ
01946      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTDUREQ
01947      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01948  4500-EXIT.  EXIT.                                                ELTDUREQ
01949 /                                                                 ELTDUREQ
01950  4650-SCAN-TAB.                                                   ELTDUREQ
01951      PERFORM 4700-BEN-TAB-AAR  THRU 4700-EXIT.                    ELTDUREQ
01952      PERFORM 4800-BEN-TAB-PPF  THRU 4800-EXIT.                    ELTDUREQ
01953      PERFORM 4900-BEN-TAB-PVE  THRU 4900-EXIT.                    ELTDUREQ
01954      PERFORM 5000-BEN-TAB-ADL  THRU 5000-EXIT.                    ELTDUREQ
01955      PERFORM 5100-BEN-TAB-ABM  THRU 5100-EXIT.                    ELTDUREQ
01956      PERFORM 5200-BEN-TAB-ACL  THRU 5200-EXIT.                    ELTDUREQ
01957      PERFORM 5300-BEN-TAB-AOL  THRU 5300-EXIT.                    ELTDUREQ
01958  4650-EXIT.  EXIT.                                                ELTDUREQ
01959 /                                                                 ELTDUREQ
01960  4675-PAY-CONSID-TEXT.                                            ELTDUREQ
01961      INITIALIZE TCAR-FROM-AREA.                                   ELTDUREQ
01962      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTDUREQ
01963             WS-PAY-CONSDR-TEXT2                                   ELTDUREQ
01964                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTDUREQ
01965      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
01966      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTDUREQ
01967      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
01968                                TCAR-OUTPUT-FIELD-2-LEN.           ELTDUREQ
01969      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTDUREQ
01970      IF WS-CIA > 17                                               ELTDUREQ
01971            PERFORM 3000-OUTPUT-TEXT                               ELTDUREQ
01972                THRU 3000-EXIT                                     ELTDUREQ
01973            MOVE +1            TO WS-CIA.                          ELTDUREQ
01974      ADD +1                TO  WS-CIA.                            ELTDUREQ
01975      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTDUREQ
01976      ADD +1                TO  WS-CIA.                            ELTDUREQ
01977      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTDUREQ
01978      PERFORM 3000-OUTPUT-TEXT                                     ELTDUREQ
01979                THRU 3000-EXIT.                                    ELTDUREQ
01980  4675-EXIT.   EXIT.                                               ELTDUREQ
01981 /                                                                 ELTDUREQ
01982  4700-BEN-TAB-AAR.                                                ELTDUREQ
01983      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTDUREQ
01984      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
01985                                                                   ELTDUREQ
01986      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01987          NOT = LOW-VALUES                                         ELTDUREQ
01988       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
01989          NOT = SPACE                                              ELTDUREQ
01990                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDUREQ
01991                                                                   ELTDUREQ
01992      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
01993                                                                   ELTDUREQ
01994      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
01995          NOT = LOW-VALUES                                         ELTDUREQ
01996       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
01997          NOT = SPACE                                              ELTDUREQ
01998                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTDUREQ
01999                                                                   ELTDUREQ
02000      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTDUREQ
02001             MOVE +1                  TO WS-CIA                    ELTDUREQ
02002             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTDUREQ
02003             ADD  +1                  TO WS-CIA                    ELTDUREQ
02004             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTDUREQ
02005             PERFORM 3000-OUTPUT-TEXT                              ELTDUREQ
02006                THRU 3000-EXIT.                                    ELTDUREQ
02007  4700-EXIT.  EXIT.                                                ELTDUREQ
02008 /                                                                 ELTDUREQ
02009  4800-BEN-TAB-PPF.                                                ELTDUREQ
02010      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDUREQ
02011                       WS-HOLD2.                                   ELTDUREQ
02012      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02013      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02014          NOT = LOW-VALUES                                         ELTDUREQ
02015       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02016          NOT = SPACE                                              ELTDUREQ
02017             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02018                        TO  WS-HOLD1.                              ELTDUREQ
02019                                                                   ELTDUREQ
02020      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02021      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02022          NOT = LOW-VALUES                                         ELTDUREQ
02023       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02024          NOT = SPACE                                              ELTDUREQ
02025             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02026                        TO  WS-HOLD2.                              ELTDUREQ
02027                                                                   ELTDUREQ
02028      IF WS-HOLD1 = WS-HOLD2                                       ELTDUREQ
02029         IF WS-HOLD1 = ZEROS                                       ELTDUREQ
02030                 GO TO 4800-EXIT                                   ELTDUREQ
02031         ELSE                                                      ELTDUREQ
02032             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDUREQ
02033             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02034             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTDUREQ
02035                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02036             END-EXEC                                              ELTDUREQ
02037             GO TO 4800-EXIT.                                      ELTDUREQ
02038                                                                   ELTDUREQ
02039      IF WS-HOLD1 = ZEROS                                          ELTDUREQ
02040             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02041             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02042             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTDUREQ
02043                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02044             END-EXEC                                              ELTDUREQ
02045      ELSE                                                         ELTDUREQ
02046       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDUREQ
02047       PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                    ELTDUREQ
02048       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTDUREQ
02049                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
02050       END-EXEC                                                    ELTDUREQ
02051       IF WS-HOLD2 = ZEROS                                         ELTDUREQ
02052           GO TO 4800-EXIT                                         ELTDUREQ
02053       ELSE                                                        ELTDUREQ
02054          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDUREQ
02055             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02056             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTDUREQ
02057                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02058             END-EXEC.                                             ELTDUREQ
02059  4800-EXIT.    EXIT.                                              ELTDUREQ
02060 /                                                                 ELTDUREQ
02061  4900-BEN-TAB-PVE.                                                ELTDUREQ
02062      MOVE +1 TO WS-CIA.                                           ELTDUREQ
02063      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTDUREQ
02064      ADD  +1         TO WS-CIA.                                   ELTDUREQ
02065      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTDUREQ
02066      PERFORM 3000-OUTPUT-TEXT                                     ELTDUREQ
02067                THRU 3000-EXIT.                                    ELTDUREQ
02068  4900-EXIT.  EXIT.                                                ELTDUREQ
02069 /                                                                 ELTDUREQ
02070  5000-BEN-TAB-ADL.                                                ELTDUREQ
02071      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDUREQ
02072                       WS-HOLD2.                                   ELTDUREQ
02073      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02074      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02075          NOT = LOW-VALUES                                         ELTDUREQ
02076       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02077          NOT = SPACE                                              ELTDUREQ
02078             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02079                        TO  WS-HOLD1.                              ELTDUREQ
02080                                                                   ELTDUREQ
02081      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02082      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02083          NOT = LOW-VALUES                                         ELTDUREQ
02084       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02085          NOT = SPACE                                              ELTDUREQ
02086             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02087                        TO  WS-HOLD2.                              ELTDUREQ
02088                                                                   ELTDUREQ
02089      IF WS-HOLD1 = WS-HOLD2                                       ELTDUREQ
02090         IF WS-HOLD1 = ZEROS                                       ELTDUREQ
02091                 GO TO 5000-EXIT                                   ELTDUREQ
02092         ELSE                                                      ELTDUREQ
02093             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDUREQ
02094             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02095             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDUREQ
02096                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02097             END-EXEC                                              ELTDUREQ
02098             GO TO 5000-EXIT.                                      ELTDUREQ
02099                                                                   ELTDUREQ
02100      IF WS-HOLD1 = ZEROS                                          ELTDUREQ
02101             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02102             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02103             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDUREQ
02104                            COMMAREA(DFHCOMMAREA)                  ELTDUREQ
02105             END-EXEC                                              ELTDUREQ
02106      ELSE                                                         ELTDUREQ
02107       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDUREQ
02108       PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                    ELTDUREQ
02109         EXEC CICS  LINK PROGRAM('ELGDEDBL')                       ELTDUREQ
02110                         COMMAREA(DFHCOMMAREA)                     ELTDUREQ
02111         END-EXEC                                                  ELTDUREQ
02112         IF WS-HOLD2 = ZEROS                                       ELTDUREQ
02113            GO TO 5000-EXIT                                        ELTDUREQ
02114         ELSE                                                      ELTDUREQ
02115             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02116             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02117             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTDUREQ
02118                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02119             END-EXEC.                                             ELTDUREQ
02120  5000-EXIT.     EXIT.                                             ELTDUREQ
02121 /                                                                 ELTDUREQ
02122  5100-BEN-TAB-ABM.                                                ELTDUREQ
02123      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDUREQ
02124                       WS-HOLD2.                                   ELTDUREQ
02125      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02126      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02127          NOT = LOW-VALUES                                         ELTDUREQ
02128       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02129          NOT = SPACE                                              ELTDUREQ
02130             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02131                        TO  WS-HOLD1.                              ELTDUREQ
02132                                                                   ELTDUREQ
02133      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02134      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02135          NOT = LOW-VALUES                                         ELTDUREQ
02136       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02137          NOT = SPACE                                              ELTDUREQ
02138             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02139                        TO  WS-HOLD2.                              ELTDUREQ
02140                                                                   ELTDUREQ
02141      IF WS-HOLD1 = WS-HOLD2                                       ELTDUREQ
02142         IF WS-HOLD1 = ZEROS                                       ELTDUREQ
02143                 GO TO 5100-EXIT                                   ELTDUREQ
02144         ELSE                                                      ELTDUREQ
02145             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDUREQ
02146             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02147             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDUREQ
02148                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02149             END-EXEC                                              ELTDUREQ
02150             GO TO 5100-EXIT.                                      ELTDUREQ
02151                                                                   ELTDUREQ
02152      IF WS-HOLD1 = ZEROS                                          ELTDUREQ
02153             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02154             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02155             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTDUREQ
02156                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02157             END-EXEC                                              ELTDUREQ
02158      ELSE                                                         ELTDUREQ
02159       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDUREQ
02160       PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                    ELTDUREQ
02161       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTDUREQ
02162                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
02163       END-EXEC                                                    ELTDUREQ
02164       IF WS-HOLD2 = ZEROS                                         ELTDUREQ
02165           GO TO 5100-EXIT                                         ELTDUREQ
02166       ELSE                                                        ELTDUREQ
02167          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDUREQ
02168          PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                 ELTDUREQ
02169          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTDUREQ
02170                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
02171          END-EXEC.                                                ELTDUREQ
02172  5100-EXIT.     EXIT.                                             ELTDUREQ
02173 /                                                                 ELTDUREQ
02174  5200-BEN-TAB-ACL.                                                ELTDUREQ
02175      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDUREQ
02176                       WS-HOLD2.                                   ELTDUREQ
02177      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02178      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02179          NOT = LOW-VALUES                                         ELTDUREQ
02180       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02181          NOT = SPACE                                              ELTDUREQ
02182             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02183                        TO  WS-HOLD1.                              ELTDUREQ
02184                                                                   ELTDUREQ
02185      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02186      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02187          NOT = LOW-VALUES                                         ELTDUREQ
02188       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02189          NOT = SPACE                                              ELTDUREQ
02190             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02191                        TO  WS-HOLD2.                              ELTDUREQ
02192                                                                   ELTDUREQ
02193      IF WS-HOLD1 = WS-HOLD2                                       ELTDUREQ
02194         IF WS-HOLD1 = ZEROS                                       ELTDUREQ
02195                 GO TO 5200-EXIT                                   ELTDUREQ
02196         ELSE                                                      ELTDUREQ
02197             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDUREQ
02198             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02199             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDUREQ
02200                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02201             END-EXEC                                              ELTDUREQ
02202             GO TO 5200-EXIT.                                      ELTDUREQ
02203                                                                   ELTDUREQ
02204      IF WS-HOLD1 = ZEROS                                          ELTDUREQ
02205             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02206             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02207             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDUREQ
02208                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02209             END-EXEC                                              ELTDUREQ
02210      ELSE                                                         ELTDUREQ
02211       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDUREQ
02212       PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                    ELTDUREQ
02213       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTDUREQ
02214                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
02215       END-EXEC                                                    ELTDUREQ
02216       IF WS-HOLD2 = ZEROS                                         ELTDUREQ
02217            GO TO 5200-EXIT                                        ELTDUREQ
02218          ELSE                                                     ELTDUREQ
02219             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02220             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02221             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTDUREQ
02222                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02223             END-EXEC.                                             ELTDUREQ
02224  5200-EXIT.     EXIT.                                             ELTDUREQ
02225 /                                                                 ELTDUREQ
02226  5300-BEN-TAB-AOL.                                                ELTDUREQ
02227      MOVE ZEROS   TO  WS-HOLD1,                                   ELTDUREQ
02228                       WS-HOLD2.                                   ELTDUREQ
02229      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02230      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02231          NOT = LOW-VALUES                                         ELTDUREQ
02232       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02233          NOT = SPACE                                              ELTDUREQ
02234             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02235                        TO  WS-HOLD1.                              ELTDUREQ
02236                                                                   ELTDUREQ
02237      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02238      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTDUREQ
02239          NOT = LOW-VALUES                                         ELTDUREQ
02240       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTDUREQ
02241          NOT = SPACE                                              ELTDUREQ
02242             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTDUREQ
02243                        TO  WS-HOLD2.                              ELTDUREQ
02244                                                                   ELTDUREQ
02245      IF WS-HOLD1 = WS-HOLD2                                       ELTDUREQ
02246         IF WS-HOLD1 = ZEROS                                       ELTDUREQ
02247                 GO TO 5300-EXIT                                   ELTDUREQ
02248         ELSE                                                      ELTDUREQ
02249             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTDUREQ
02250             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02251             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDUREQ
02252                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02253             END-EXEC                                              ELTDUREQ
02254             GO TO 5300-EXIT.                                      ELTDUREQ
02255                                                                   ELTDUREQ
02256      IF WS-HOLD1 = ZEROS                                          ELTDUREQ
02257             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTDUREQ
02258             PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT              ELTDUREQ
02259             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTDUREQ
02260                             COMMAREA(DFHCOMMAREA)                 ELTDUREQ
02261             END-EXEC                                              ELTDUREQ
02262      ELSE                                                         ELTDUREQ
02263       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTDUREQ
02264       PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                    ELTDUREQ
02265       EXEC CICS  LINK PROGRAM('ELGOUTPX')                         ELTDUREQ
02266                       COMMAREA(DFHCOMMAREA)                       ELTDUREQ
02267       END-EXEC                                                    ELTDUREQ
02268       IF WS-HOLD2 = ZEROS                                         ELTDUREQ
02269            GO TO 5300-EXIT                                        ELTDUREQ
02270       ELSE                                                        ELTDUREQ
02271          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTDUREQ
02272          PERFORM 5900-GET-TAB-REC  THRU 5900-EXIT                 ELTDUREQ
02273          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTDUREQ
02274                          COMMAREA(DFHCOMMAREA)                    ELTDUREQ
02275          END-EXEC.                                                ELTDUREQ
02276  5300-EXIT.     EXIT.                                             ELTDUREQ
02277 /                                                                 ELTDUREQ
02278  5400-TRANSFER-OTHER-RESP-IND.                                    ELTDUREQ
02279      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTDUREQ
02280      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTDUREQ
02281      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
02282           TO  CMF-CODE-VALUE.                                     ELTDUREQ
02283      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTDUREQ
02284      STRING                                                       ELTDUREQ
02285             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02286             CMF-DESCR-LINE (2)                                    ELTDUREQ
02287              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTDUREQ
02288      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTDUREQ
02289  5400-EXIT.     EXIT.                                             ELTDUREQ
02290 /                                                                 ELTDUREQ
02291  5900-GET-TAB-REC.                                                ELTDUREQ
02292                                                                   ELTDUREQ
02293      SET CIA-GCTABULR-DDN TO TRUE.                                ELTDUREQ
02294      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
02295          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTDUREQ
02296                                                                   ELTDUREQ
02297      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTDUREQ
02298      SET  CIA-GCTABULR-DDN          TO TRUE.                      ELTDUREQ
02299                                                                   ELTDUREQ
02300      SET IOP-RD                     TO TRUE.                      ELTDUREQ
02301      SET IOP-FCQ-NONE               TO TRUE.                      ELTDUREQ
02302      SET IOP-KVQ-NONE               TO TRUE.                      ELTDUREQ
02303                                                                   ELTDUREQ
02304      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTDUREQ
02305             COMMAREA(DFHCOMMAREA)                                 ELTDUREQ
02306      END-EXEC.                                                    ELTDUREQ
02307                                                                   ELTDUREQ
02308      IF IOP-RC-NOTFND                                             ELTDUREQ
02309         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTDUREQ
02310         EXEC CICS ABEND                                           ELTDUREQ
02311                   ABCODE(CIA-ABCODE)                              ELTDUREQ
02312         END-EXEC.                                                 ELTDUREQ
02313                                                                   ELTDUREQ
02314      IF NOT IOP-RC-OK                                             ELTDUREQ
02315         SET CIA-AB-CRITIO          TO TRUE                        ELTDUREQ
02316         EXEC CICS ABEND                                           ELTDUREQ
02317                   ABCODE(CIA-ABCODE)                              ELTDUREQ
02318         END-EXEC.                                                 ELTDUREQ
02319                                                                   ELTDUREQ
02320  5900-EXIT.     EXIT.                                             ELTDUREQ
02321 /                                                                 ELTDUREQ
02322  6000-INST-CERTIFICATION.                                         ELTDUREQ
02323      MOVE '6000' TO WS-PARA-ID.                                   ELTDUREQ
02324      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02325      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTDUREQ
02326                     WS-DTL-BASIC-1-2                              ELTDUREQ
02327                     WS-DTL-BASIC-2-1                              ELTDUREQ
02328                     WS-DTL-BASIC-2-2                              ELTDUREQ
02329                     WS-DTL-BASIC-3-1                              ELTDUREQ
02330                     WS-DTL-BASIC-3-2                              ELTDUREQ
02331                     WS-DTL-BASIC-4-1                              ELTDUREQ
02332                     WS-DTL-BASIC-4-2                              ELTDUREQ
02333                     WS-DTL-SUPP-1-1                               ELTDUREQ
02334                     WS-DTL-SUPP-1-2                               ELTDUREQ
02335                     WS-DTL-SUPP-2-1                               ELTDUREQ
02336                     WS-DTL-SUPP-2-2                               ELTDUREQ
02337                     WS-DTL-SUPP-3-1                               ELTDUREQ
02338                     WS-DTL-SUPP-3-2                               ELTDUREQ
02339                     WS-DTL-SUPP-4-1                               ELTDUREQ
02340                     WS-DTL-SUPP-4-2.                              ELTDUREQ
02341      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTDUREQ
02342                                              NOT = ZEROES         ELTDUREQ
02343        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02344                                         NOT = LOW-VALUES          ELTDUREQ
02345          MOVE                                                     ELTDUREQ
02346           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02347                                    TO  CMF-CODE-VALUE             ELTDUREQ
02348          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTDUREQ
02349          MOVE 'CERTFN-REQRM-IND'                                  ELTDUREQ
02350                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02351          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02352          STRING                                                   ELTDUREQ
02353             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02354             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02355             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02356             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02357             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02358             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02359             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02360             CMF-DESCR-LINE (8)                                    ELTDUREQ
02361              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02362             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02363             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02364             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02365             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02366             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02367             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02368             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02369             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02370             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02371             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02372             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02373            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTDUREQ
02374            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTDUREQ
02375            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTDUREQ
02376            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTDUREQ
02377            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTDUREQ
02378            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTDUREQ
02379            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTDUREQ
02380            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTDUREQ
02381                                                                   ELTDUREQ
02382      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02383      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTDUREQ
02384                                              NOT = ZEROES         ELTDUREQ
02385        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02386                                         NOT = LOW-VALUES          ELTDUREQ
02387          MOVE                                                     ELTDUREQ
02388           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02389                                    TO  CMF-CODE-VALUE             ELTDUREQ
02390          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTDUREQ
02391          MOVE 'CERTFN-REQRM-IND'                                  ELTDUREQ
02392                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02393          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02394          STRING                                                   ELTDUREQ
02395             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02396             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02397             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02398             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02399             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02400             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02401             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02402             CMF-DESCR-LINE (8)                                    ELTDUREQ
02403              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02404             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02405             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02406             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02407             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02408             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02409             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02410             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02411             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02412             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02413             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02414             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02415            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTDUREQ
02416            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTDUREQ
02417            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTDUREQ
02418            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTDUREQ
02419            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTDUREQ
02420            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTDUREQ
02421            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTDUREQ
02422            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTDUREQ
02423                                                                   ELTDUREQ
02424                                                                   ELTDUREQ
02425      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTDUREQ
02426             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTDUREQ
02427                  ADD +1                TO WS-CIA                  ELTDUREQ
02428                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTDUREQ
02429                  ADD +1                TO WS-CIA                  ELTDUREQ
02430             MOVE WS-CERTIFICATION  TO COF-DTL-LINE(WS-CIA).       ELTDUREQ
02431                                                                   ELTDUREQ
02432      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02433                  ADD +1                TO WS-CIA                  ELTDUREQ
02434         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
02435         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTDUREQ
02436             ADD +1                TO  WS-CIA                      ELTDUREQ
02437             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02438         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTDUREQ
02439             ADD +1                TO  WS-CIA                      ELTDUREQ
02440             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02441         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTDUREQ
02442             ADD +1                TO  WS-CIA                      ELTDUREQ
02443             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02444         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTDUREQ
02445             ADD +1                TO  WS-CIA                      ELTDUREQ
02446             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02447         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTDUREQ
02448             ADD +1                TO  WS-CIA                      ELTDUREQ
02449             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02450         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTDUREQ
02451             ADD +1                TO  WS-CIA                      ELTDUREQ
02452             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02453         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTDUREQ
02454             ADD +1                TO  WS-CIA                      ELTDUREQ
02455             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02456      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02457           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02458                THRU 3000-EXIT.                                    ELTDUREQ
02459                                                                   ELTDUREQ
02460      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02461         ADD +1                TO  WS-CIA                          ELTDUREQ
02462         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTDUREQ
02463         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTDUREQ
02464             ADD +1                TO  WS-CIA                      ELTDUREQ
02465             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02466                                                                   ELTDUREQ
02467         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTDUREQ
02468             ADD +1                TO  WS-CIA                      ELTDUREQ
02469             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02470                                                                   ELTDUREQ
02471         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTDUREQ
02472             ADD +1                TO  WS-CIA                      ELTDUREQ
02473             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02474                                                                   ELTDUREQ
02475         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTDUREQ
02476             ADD +1                TO  WS-CIA                      ELTDUREQ
02477             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02478                                                                   ELTDUREQ
02479         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTDUREQ
02480             ADD +1                TO  WS-CIA                      ELTDUREQ
02481             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02482                                                                   ELTDUREQ
02483         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTDUREQ
02484             ADD +1                TO  WS-CIA                      ELTDUREQ
02485             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02486                                                                   ELTDUREQ
02487         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTDUREQ
02488             ADD +1                TO  WS-CIA                      ELTDUREQ
02489             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02490                                                                   ELTDUREQ
02491      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02492           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02493                THRU 3000-EXIT.                                    ELTDUREQ
02494  6000-EXIT.                                                       ELTDUREQ
02495      EXIT.                                                        ELTDUREQ
02496 /                                                                 ELTDUREQ
02497  6100-INST-RECERTIFICATION.                                       ELTDUREQ
02498      MOVE '6000' TO WS-PARA-ID.                                   ELTDUREQ
02499      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02500      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTDUREQ
02501                     WS-DTL-BASIC-1-2                              ELTDUREQ
02502                     WS-DTL-BASIC-2-1                              ELTDUREQ
02503                     WS-DTL-BASIC-2-2                              ELTDUREQ
02504                     WS-DTL-BASIC-3-1                              ELTDUREQ
02505                     WS-DTL-BASIC-3-2                              ELTDUREQ
02506                     WS-DTL-BASIC-4-1                              ELTDUREQ
02507                     WS-DTL-BASIC-4-2                              ELTDUREQ
02508                     WS-DTL-SUPP-1-1                               ELTDUREQ
02509                     WS-DTL-SUPP-1-2                               ELTDUREQ
02510                     WS-DTL-SUPP-2-1                               ELTDUREQ
02511                     WS-DTL-SUPP-2-2                               ELTDUREQ
02512                     WS-DTL-SUPP-3-1                               ELTDUREQ
02513                     WS-DTL-SUPP-3-2                               ELTDUREQ
02514                     WS-DTL-SUPP-4-1                               ELTDUREQ
02515                     WS-DTL-SUPP-4-2.                              ELTDUREQ
02516      IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
02517                                              NOT = ZEROES         ELTDUREQ
02518        IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02519                                         NOT = LOW-VALUE           ELTDUREQ
02520          MOVE                                                     ELTDUREQ
02521           PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02522                                    TO  CMF-CODE-VALUE             ELTDUREQ
02523          MOVE 'BPB'                TO  CMF-RECORD-PREFIX          ELTDUREQ
02524          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTDUREQ
02525                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02526          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02527          STRING                                                   ELTDUREQ
02528             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02529             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02530             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02531             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02532             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02533             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02534             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02535             CMF-DESCR-LINE (8)                                    ELTDUREQ
02536              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02537             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02538             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02539             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02540             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02541             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02542             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02543             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02544             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02545             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02546             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02547             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02548            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTDUREQ
02549            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTDUREQ
02550            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTDUREQ
02551            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTDUREQ
02552            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTDUREQ
02553            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTDUREQ
02554            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTDUREQ
02555            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTDUREQ
02556                                                                   ELTDUREQ
02557      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02558      IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
02559                                              NOT = ZEROES         ELTDUREQ
02560        IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02561                                         NOT = LOW-VALUE           ELTDUREQ
02562          MOVE                                                     ELTDUREQ
02563           PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02564                                    TO  CMF-CODE-VALUE             ELTDUREQ
02565          MOVE 'BPB'                TO  CMF-RECORD-PREFIX          ELTDUREQ
02566          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTDUREQ
02567                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02568          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02569          STRING                                                   ELTDUREQ
02570             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02571             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02572             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02573             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02574             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02575             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02576             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02577             CMF-DESCR-LINE (8)                                    ELTDUREQ
02578              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02579             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02580             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02581             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02582             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02583             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02584             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02585             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02586             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02587             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02588             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02589             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02590            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTDUREQ
02591            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTDUREQ
02592            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTDUREQ
02593            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTDUREQ
02594            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTDUREQ
02595            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTDUREQ
02596            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTDUREQ
02597            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTDUREQ
02598                                                                   ELTDUREQ
02599                                                                   ELTDUREQ
02600      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTDUREQ
02601             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTDUREQ
02602                  ADD +1                TO WS-CIA                  ELTDUREQ
02603                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTDUREQ
02604                  ADD +1                TO WS-CIA                  ELTDUREQ
02605             MOVE WS-RECERTIFICATION  TO COF-DTL-LINE(WS-CIA).     ELTDUREQ
02606                                                                   ELTDUREQ
02607      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02608                  ADD +1                TO WS-CIA                  ELTDUREQ
02609         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
02610         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTDUREQ
02611             ADD +1                TO  WS-CIA                      ELTDUREQ
02612             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02613         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTDUREQ
02614             ADD +1                TO  WS-CIA                      ELTDUREQ
02615             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02616         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTDUREQ
02617             ADD +1                TO  WS-CIA                      ELTDUREQ
02618             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02619         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTDUREQ
02620             ADD +1                TO  WS-CIA                      ELTDUREQ
02621             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02622         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTDUREQ
02623             ADD +1                TO  WS-CIA                      ELTDUREQ
02624             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02625         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTDUREQ
02626             ADD +1                TO  WS-CIA                      ELTDUREQ
02627             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02628         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTDUREQ
02629             ADD +1                TO  WS-CIA                      ELTDUREQ
02630             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02631      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02632           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02633                THRU 3000-EXIT.                                    ELTDUREQ
02634                                                                   ELTDUREQ
02635      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02636         ADD +1                TO  WS-CIA                          ELTDUREQ
02637         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTDUREQ
02638         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTDUREQ
02639             ADD +1                TO  WS-CIA                      ELTDUREQ
02640             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02641                                                                   ELTDUREQ
02642         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTDUREQ
02643             ADD +1                TO  WS-CIA                      ELTDUREQ
02644             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02645                                                                   ELTDUREQ
02646         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTDUREQ
02647             ADD +1                TO  WS-CIA                      ELTDUREQ
02648             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02649                                                                   ELTDUREQ
02650         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTDUREQ
02651             ADD +1                TO  WS-CIA                      ELTDUREQ
02652             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02653                                                                   ELTDUREQ
02654         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTDUREQ
02655             ADD +1                TO  WS-CIA                      ELTDUREQ
02656             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02657                                                                   ELTDUREQ
02658         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTDUREQ
02659             ADD +1                TO  WS-CIA                      ELTDUREQ
02660             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02661                                                                   ELTDUREQ
02662         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTDUREQ
02663             ADD +1                TO  WS-CIA                      ELTDUREQ
02664             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02665                                                                   ELTDUREQ
02666      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02667           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02668                THRU 3000-EXIT.                                    ELTDUREQ
02669  6199-EXIT.                                                       ELTDUREQ
02670      EXIT.                                                        ELTDUREQ
02671  7000-PROF-CERTIFICATION.                                         ELTDUREQ
02672      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02673      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTDUREQ
02674                     WS-DTL-BASIC-1-2                              ELTDUREQ
02675                     WS-DTL-BASIC-2-1                              ELTDUREQ
02676                     WS-DTL-BASIC-2-2                              ELTDUREQ
02677                     WS-DTL-BASIC-3-1                              ELTDUREQ
02678                     WS-DTL-BASIC-3-2                              ELTDUREQ
02679                     WS-DTL-BASIC-4-1                              ELTDUREQ
02680                     WS-DTL-BASIC-4-2                              ELTDUREQ
02681                     WS-DTL-SUPP-1-1                               ELTDUREQ
02682                     WS-DTL-SUPP-1-2                               ELTDUREQ
02683                     WS-DTL-SUPP-2-1                               ELTDUREQ
02684                     WS-DTL-SUPP-2-2                               ELTDUREQ
02685                     WS-DTL-SUPP-3-1                               ELTDUREQ
02686                     WS-DTL-SUPP-3-2                               ELTDUREQ
02687                     WS-DTL-SUPP-4-1                               ELTDUREQ
02688                     WS-DTL-SUPP-4-2.                              ELTDUREQ
02689      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTDUREQ
02690                                              NOT = ZEROES         ELTDUREQ
02691       IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)            ELTDUREQ
02692                                         NOT = SPACES              ELTDUREQ
02693        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02694                                         NOT = LOW-VALUES          ELTDUREQ
02695          MOVE                                                     ELTDUREQ
02696           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02697                                    TO  CMF-CODE-VALUE             ELTDUREQ
02698          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTDUREQ
02699          MOVE 'CERTFN-REQRM-IND'                                  ELTDUREQ
02700                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02701          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02702          STRING                                                   ELTDUREQ
02703             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02704             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02705             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02706             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02707             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02708             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02709             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02710             CMF-DESCR-LINE (8)                                    ELTDUREQ
02711              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02712             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02713             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02714             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02715             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02716             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02717             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02718             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02719             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02720             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02721             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02722             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02723            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTDUREQ
02724            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTDUREQ
02725            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTDUREQ
02726            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTDUREQ
02727            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTDUREQ
02728            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTDUREQ
02729            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTDUREQ
02730            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTDUREQ
02731                                                                   ELTDUREQ
02732      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02733      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTDUREQ
02734                                              NOT = ZEROES         ELTDUREQ
02735        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02736                                         NOT = LOW-VALUES          ELTDUREQ
02737          MOVE                                                     ELTDUREQ
02738           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTDUREQ
02739                                    TO  CMF-CODE-VALUE             ELTDUREQ
02740          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTDUREQ
02741          MOVE 'CERTFN-REQRM-IND'                                  ELTDUREQ
02742                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02743          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02744          STRING                                                   ELTDUREQ
02745             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02746             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02747             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02748             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02749             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02750             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02751             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02752             CMF-DESCR-LINE (8)                                    ELTDUREQ
02753              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02754             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02755             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02756             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02757             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02758             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02759             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02760             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02761             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02762             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02763             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02764             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02765            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTDUREQ
02766            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTDUREQ
02767            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTDUREQ
02768            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTDUREQ
02769            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTDUREQ
02770            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTDUREQ
02771            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTDUREQ
02772            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTDUREQ
02773                                                                   ELTDUREQ
02774                                                                   ELTDUREQ
02775      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTDUREQ
02776             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTDUREQ
02777                  ADD +1                TO WS-CIA                  ELTDUREQ
02778                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTDUREQ
02779                  ADD +1                TO WS-CIA                  ELTDUREQ
02780             MOVE WS-CERTIFICATION    TO COF-DTL-LINE(WS-CIA).     ELTDUREQ
02781                                                                   ELTDUREQ
02782      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02783                  ADD +1                TO WS-CIA                  ELTDUREQ
02784         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
02785         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTDUREQ
02786             ADD +1                TO  WS-CIA                      ELTDUREQ
02787             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02788         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTDUREQ
02789             ADD +1                TO  WS-CIA                      ELTDUREQ
02790             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02791         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTDUREQ
02792             ADD +1                TO  WS-CIA                      ELTDUREQ
02793             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02794         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTDUREQ
02795             ADD +1                TO  WS-CIA                      ELTDUREQ
02796             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02797         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTDUREQ
02798             ADD +1                TO  WS-CIA                      ELTDUREQ
02799             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02800         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTDUREQ
02801             ADD +1                TO  WS-CIA                      ELTDUREQ
02802             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02803         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTDUREQ
02804             ADD +1                TO  WS-CIA                      ELTDUREQ
02805             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02806      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02807           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02808                THRU 3000-EXIT.                                    ELTDUREQ
02809                                                                   ELTDUREQ
02810      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02811         ADD +1                TO  WS-CIA                          ELTDUREQ
02812         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTDUREQ
02813         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTDUREQ
02814             ADD +1                TO  WS-CIA                      ELTDUREQ
02815             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02816                                                                   ELTDUREQ
02817         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTDUREQ
02818             ADD +1                TO  WS-CIA                      ELTDUREQ
02819             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02820                                                                   ELTDUREQ
02821         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTDUREQ
02822             ADD +1                TO  WS-CIA                      ELTDUREQ
02823             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02824                                                                   ELTDUREQ
02825         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTDUREQ
02826             ADD +1                TO  WS-CIA                      ELTDUREQ
02827             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02828                                                                   ELTDUREQ
02829         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTDUREQ
02830             ADD +1                TO  WS-CIA                      ELTDUREQ
02831             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02832                                                                   ELTDUREQ
02833         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTDUREQ
02834             ADD +1                TO  WS-CIA                      ELTDUREQ
02835             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02836                                                                   ELTDUREQ
02837         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTDUREQ
02838             ADD +1                TO  WS-CIA                      ELTDUREQ
02839             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02840                                                                   ELTDUREQ
02841      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02842           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02843                THRU 3000-EXIT.                                    ELTDUREQ
02844  7000-EXIT.                                                       ELTDUREQ
02845      EXIT.                                                        ELTDUREQ
02846 /                                                                 ELTDUREQ
02847  7100-PROF-RECERTIFICATION.                                       ELTDUREQ
02848      SET PLT-INDEX2 TO 1.                                         ELTDUREQ
02849      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTDUREQ
02850                     WS-DTL-BASIC-1-2                              ELTDUREQ
02851                     WS-DTL-BASIC-2-1                              ELTDUREQ
02852                     WS-DTL-BASIC-2-2                              ELTDUREQ
02853                     WS-DTL-BASIC-3-1                              ELTDUREQ
02854                     WS-DTL-BASIC-3-2                              ELTDUREQ
02855                     WS-DTL-BASIC-4-1                              ELTDUREQ
02856                     WS-DTL-BASIC-4-2                              ELTDUREQ
02857                     WS-DTL-SUPP-1-1                               ELTDUREQ
02858                     WS-DTL-SUPP-1-2                               ELTDUREQ
02859                     WS-DTL-SUPP-2-1                               ELTDUREQ
02860                     WS-DTL-SUPP-2-2                               ELTDUREQ
02861                     WS-DTL-SUPP-3-1                               ELTDUREQ
02862                     WS-DTL-SUPP-3-2                               ELTDUREQ
02863                     WS-DTL-SUPP-4-1                               ELTDUREQ
02864                     WS-DTL-SUPP-4-2.                              ELTDUREQ
02865      IF PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
02866                                              NOT = ZEROES         ELTDUREQ
02867       IF PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)      ELTDUREQ
02868                                         NOT = SPACE               ELTDUREQ
02869        IF PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02870                                         NOT = LOW-VALUE           ELTDUREQ
02871          MOVE                                                     ELTDUREQ
02872           PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02873                                    TO  CMF-CODE-VALUE             ELTDUREQ
02874          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTDUREQ
02875          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTDUREQ
02876                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02877          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02878          STRING                                                   ELTDUREQ
02879             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02880             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02881             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02882             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02883             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02884             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02885             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02886             CMF-DESCR-LINE (8)                                    ELTDUREQ
02887              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02888             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02889             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02890             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02891             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02892             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02893             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02894             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02895             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02896             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02897             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02898             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02899            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTDUREQ
02900            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTDUREQ
02901            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTDUREQ
02902            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTDUREQ
02903            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTDUREQ
02904            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTDUREQ
02905            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTDUREQ
02906            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTDUREQ
02907                                                                   ELTDUREQ
02908      SET PLT-INDEX2 TO 2.                                         ELTDUREQ
02909      IF PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTDUREQ
02910                                              NOT = ZEROES         ELTDUREQ
02911        IF PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02912                                         NOT = LOW-VALUE           ELTDUREQ
02913          MOVE                                                     ELTDUREQ
02914           PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)     ELTDUREQ
02915                                    TO  CMF-CODE-VALUE             ELTDUREQ
02916          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTDUREQ
02917          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTDUREQ
02918                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTDUREQ
02919          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTDUREQ
02920          STRING                                                   ELTDUREQ
02921             CMF-DESCR-LINE (1) ' '                                ELTDUREQ
02922             CMF-DESCR-LINE (2) ' '                                ELTDUREQ
02923             CMF-DESCR-LINE (3) ' '                                ELTDUREQ
02924             CMF-DESCR-LINE (4) ' '                                ELTDUREQ
02925             CMF-DESCR-LINE (5) ' '                                ELTDUREQ
02926             CMF-DESCR-LINE (6) ' '                                ELTDUREQ
02927             CMF-DESCR-LINE (7) ' '                                ELTDUREQ
02928             CMF-DESCR-LINE (8)                                    ELTDUREQ
02929              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTDUREQ
02930             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTDUREQ
02931             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTDUREQ
02932             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTDUREQ
02933             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTDUREQ
02934             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTDUREQ
02935             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTDUREQ
02936             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTDUREQ
02937             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTDUREQ
02938             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTDUREQ
02939             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTDUREQ
02940             PERFORM TCPR-000-TEXT-UNSTRING                        ELTDUREQ
02941            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTDUREQ
02942            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTDUREQ
02943            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTDUREQ
02944            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTDUREQ
02945            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTDUREQ
02946            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTDUREQ
02947            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTDUREQ
02948            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTDUREQ
02949                                                                   ELTDUREQ
02950                                                                   ELTDUREQ
02951      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTDUREQ
02952             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTDUREQ
02953                  ADD +1                TO WS-CIA                  ELTDUREQ
02954                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTDUREQ
02955                  ADD +1                TO WS-CIA                  ELTDUREQ
02956             MOVE WS-RECERTIFICATION  TO COF-DTL-LINE(WS-CIA).     ELTDUREQ
02957                                                                   ELTDUREQ
02958      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02959                  ADD +1                TO WS-CIA                  ELTDUREQ
02960         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTDUREQ
02961         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTDUREQ
02962             ADD +1                TO  WS-CIA                      ELTDUREQ
02963             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02964         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTDUREQ
02965             ADD +1                TO  WS-CIA                      ELTDUREQ
02966             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02967         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTDUREQ
02968             ADD +1                TO  WS-CIA                      ELTDUREQ
02969             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02970         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTDUREQ
02971             ADD +1                TO  WS-CIA                      ELTDUREQ
02972             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02973         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTDUREQ
02974             ADD +1                TO  WS-CIA                      ELTDUREQ
02975             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02976         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTDUREQ
02977             ADD +1                TO  WS-CIA                      ELTDUREQ
02978             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02979         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTDUREQ
02980             ADD +1                TO  WS-CIA                      ELTDUREQ
02981             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
02982      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTDUREQ
02983           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
02984                THRU 3000-EXIT.                                    ELTDUREQ
02985                                                                   ELTDUREQ
02986      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
02987         ADD +1                TO  WS-CIA                          ELTDUREQ
02988         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTDUREQ
02989         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTDUREQ
02990             ADD +1                TO  WS-CIA                      ELTDUREQ
02991             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02992                                                                   ELTDUREQ
02993         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTDUREQ
02994             ADD +1                TO  WS-CIA                      ELTDUREQ
02995             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
02996                                                                   ELTDUREQ
02997         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTDUREQ
02998             ADD +1                TO  WS-CIA                      ELTDUREQ
02999             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
03000                                                                   ELTDUREQ
03001         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTDUREQ
03002             ADD +1                TO  WS-CIA                      ELTDUREQ
03003             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
03004                                                                   ELTDUREQ
03005         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTDUREQ
03006             ADD +1                TO  WS-CIA                      ELTDUREQ
03007             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
03008                                                                   ELTDUREQ
03009         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTDUREQ
03010             ADD +1                TO  WS-CIA                      ELTDUREQ
03011             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTDUREQ
03012                                                                   ELTDUREQ
03013         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTDUREQ
03014             ADD +1                TO  WS-CIA                      ELTDUREQ
03015             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTDUREQ
03016                                                                   ELTDUREQ
03017      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTDUREQ
03018           PERFORM 3000-OUTPUT-TEXT                                ELTDUREQ
03019                THRU 3000-EXIT.                                    ELTDUREQ
03020  7100-EXIT.                                                       ELTDUREQ
03021      EXIT.                                                        ELTDUREQ
03022 /            C A L L   T O   C O D E S   M A N U A L              ELTDUREQ
03023  8500-CALL-CODES-MANUAL.                                          ELTDUREQ
03024      MOVE '8500'  TO  WS-PARA-ID.                                 ELTDUREQ
03025                                                                   ELTDUREQ
03026      INITIALIZE CMF-RETURN-CODE,                                  ELTDUREQ
03027                 TCAR-FROM-AREA.                                   ELTDUREQ
03028                                                                   ELTDUREQ
03029      EXEC CICS  LINK PROGRAM('ELUCMIF')                           ELTDUREQ
03030          COMMAREA(DFHCOMMAREA)                                    ELTDUREQ
03031          END-EXEC.                                                ELTDUREQ
03032                                                                   ELTDUREQ
03033      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTDUREQ
03034      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTDUREQ
03035          ADDRESS OF CMF-DESCR.                                    ELTDUREQ
03036                                                                   ELTDUREQ
03037  8500-EXIT.     EXIT.                                             ELTDUREQ
03038 /   C O M P R E S S I O N   A N D   U N S T R I N G   R O U T I N ELTDUREQ
03039  COPY ELSTCOMP.                                                   ELTDUREQ
03040 /              A B E N D                                          ELTDUREQ
