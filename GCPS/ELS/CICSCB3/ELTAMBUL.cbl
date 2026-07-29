00001  IDENTIFICATION DIVISION.                                         09/03/03
00002  PROGRAM-ID. ELTAMBUL.                                            ELTAMBUL
00003  AUTHOR. WIL HARNDEN - TMS , INC.                                    LV002
00004  DATE-WRITTEN.   7/08/86.                                         ELTAMBUL
00005  DATE-COMPILED.                                                   ELTAMBUL
00006      SKIP3                                                        ELTAMBUL
00007 ******************************************************************ELTAMBUL
00008 *@>ELTAMBUL                                                       ELTAMBUL
00009 *@¬                                                               ELTAMBUL
00010 *                        PROGRAM ABSTRACT                         ELTAMBUL
00011 *                                                                 ELTAMBUL
00012 *@¬ PROGRAM NAME:   E.L.S. AMBULANCE    SERVICES                  ELTAMBUL
00013 *@¬                                                               ELTAMBUL
00014 *@¬ PROGRAM I.D.:   ELTAMBUL                                      ELTAMBUL
00015 *@¬                                                               ELTAMBUL
00016 *@¬ PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTAMBUL
00017 *@¬            AMBULANCE SERVICES COVERAGE GIVEN A MEMBER.        ELTAMBUL
00018 *@¬                                                               ELTAMBUL
00019 *@¬ OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF AMBULANCE         ELTAMBUL
00020 *@¬            SERVICES GIVEN  A MEMBER BY HIS GROUP.  THIS INFO  ELTAMBUL
00021 *@¬            IS GOTTEN BY INTEROGATING THE BENEFIT PROVISIONS   ELTAMBUL
00022 *@¬            FOR                                                ELTAMBUL
00023 *@¬            THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR     ELTAMBUL
00024 *@¬            RANGE OF DATES.                                    ELTAMBUL
00025 *@¬                                                               ELTAMBUL
00026 *@¬ RECORDS                                                       ELTAMBUL
00027 *@¬ ACCESSED:  GROUP SPECIFIC, CONTRACT,                          ELTAMBUL
00028 *@¬            VARIOUS BENEFIT PROVISIONS, AND A                  ELTAMBUL
00029 *@¬          LARGE NUMBER OF DATA ELEMENT AND CODE VALUE RECORDS. ELTAMBUL
00030 *@¬                                                               ELTAMBUL
00031 *@¬ PROCESSING                                                    ELTAMBUL
00032 *@¬ FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTAMBUL
00033 *@¬                                                               ELTAMBUL
00034 *@¬                                                               ELTAMBUL
00035 *@¬ UPDATE HISTORY                                                ELTAMBUL
00036 *@¬                                                               ELTAMBUL
00037 *@¬ 07/24/86  JTC    CHANGED THE PICTURE OF WS-BASIC-MAX-AMT      ELTAMBUL
00038 *@¬                  FROM PIC ZZ9 TO PIC ZZ9.99-                  ELTAMBUL
00039 *@¬                  CHANGED THE PICTURE OF WS-SUPPL-MAX-AMT      ELTAMBUL
00040 *@¬                  FROM PIC ZZ9 TO PIC ZZ9.99-                  ELTAMBUL
00041 *@¬                                                               ELTAMBUL
00042 *@¬ 09/25/86  JTC    VS COBOL II CONVERSION                       ELTAMBUL
00043 *@¬                                                               ELTAMBUL
00044 *@¬   11/25/86  LET    CERTIFICATION REQUIREMENT INDICATOR WAS    ELTAMBUL
00045 *@¬                    MOVED FROM THE FORMAT TYPE SECTION TO THE  ELTAMBUL
00046 *@¬                    COMMON SECTION                             ELTAMBUL
00047 *@¬                                                               ELTAMBUL
00048 *@¬   10/19/87  NAC    REWORD PHRASE FOR COVERED BENEFITS.        ELTAMBUL
00049 *@¬                                                               ELTAMBUL
00050 *@¬   03/24/89  GEM    STORAGE MANAGEMENT ENHANCEMENTS.           ELTAMBUL
00051 *@¬                                                               ELTAMBUL
00052 *@¬   10/16/89  RKH    ADDED TRANSFER TO OTHER RESPONSIBILITY IND ELTAMBUL
00053 *@¬                                                               ELTAMBUL
00054 *@¬   11/12/90  AKK    CHANGED '0' COMPARE ON PLP-TRANSF-OTHER    ELTAMBUL
00055 *@¬                    RESP-IND TO 'ZERO' DUE TO EXPANSION OF     ELTAMBUL
00056 *@¬                    THE GCBENPVC.  THE CHANGE TO GCBENPVC      ELTAMBUL
00057 *@¬                    CAUSED A CORRESPONDING CHANGE IN ELSPLGTB. ELTAMBUL
00058 *@¬                                                               ELTAMBUL
00059 **       13-AUG-2003 AKK       REGEN FOR ORDER OF COMPILE TEST    ELTAMBUL
00060 ***************************************************************** ELTAMBUL
00061 /                                                                 ELTAMBUL
00062  ENVIRONMENT DIVISION.                                            ELTAMBUL
00063      SKIP3                                                        ELTAMBUL
00064  DATA DIVISION.                                                   ELTAMBUL
00065  WORKING-STORAGE SECTION.                                         ELTAMBUL
00066  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTAMBUL
00067      '***ELTAMBUL WS BEGINS***'.                                  ELTAMBUL
00068  01  WS-BEN-SCOPE                PIC X(4) VALUE 'XXXX'.           ELTAMBUL
00069  01  WS-PARA-ID                  PIC X(4) VALUE 'XXXX'.           ELTAMBUL
00070  01  WS-IND1                     PIC X(02) VALUE SPACES.          ELTAMBUL
00071  01  WS-ABEND-CODE               PIC X(4) VALUE 'XXXX'.           ELTAMBUL
00072                                                                   ELTAMBUL
00073 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTAMBUL
00074  01  WS-WORK-FIELDS.                                              ELTAMBUL
00075      05  WS-CHAR-0                     PIC X.                     ELTAMBUL
00076      05  WS-DISPLAY-MAX-VISIT-TEXT     PIC X.                     ELTAMBUL
00077      05  WS-MAX-AMT-TEXT-SW            PIC X.                     ELTAMBUL
00078      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTAMBUL
00079      05  WS-DISPLAY-PVE-TEXT           PIC X.                     ELTAMBUL
00080      05  WS-SERVICES-PAYBLE-SW         PIC X.                     ELTAMBUL
00081      05  WS-HOLD1                      PIC X(10).                 ELTAMBUL
00082      05  WS-HOLD2                      PIC X(10).                 ELTAMBUL
00083      05  WS-DTL-PP                     PIC X(50).                 ELTAMBUL
00084      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTAMBUL
00085      05  WS-DTL-PER-D                  PIC X(63).                 ELTAMBUL
00086      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTAMBUL
00087      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X.                     ELTAMBUL
00088      05  WS-DISPLAY-SERVIC-REND-TEXT   PIC X.                     ELTAMBUL
00089      05  WS-CIA                        PIC S999 COMP-3 VALUE +0.  ELTAMBUL
00090      05  WS-SUB                        PIC S999 COMP-3 VALUE +0.  ELTAMBUL
00091      05  WS-SUB2                       PIC S999 COMP-3 VALUE +0.  ELTAMBUL
00092      05  WS-SUB3                       PIC S999 COMP-3 VALUE +0.  ELTAMBUL
00093      05  WS-DESC-CTR                   PIC S999 COMP-3 VALUE +0.  ELTAMBUL
00094      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTAMBUL
00095      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTAMBUL
00096      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTAMBUL
00097      05  WS-FIRSTTIME-IND              PIC X.                     ELTAMBUL
00098          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTAMBUL
00099                                                                   ELTAMBUL
00100 /     B E N   P R O V   I D S   B Y   T Y P E - PROFESSIONAL      ELTAMBUL
00101  01  WS-BEN-PROV-ID-IP.                                           ELTAMBUL
00102      05  WS-PROF-IP-CNT                PIC S9(4) COMP   VALUE +2. ELTAMBUL
00103      05  WS-PROF-IP-TAB.                                          ELTAMBUL
00104        10  FILLER                      PIC X(6)  VALUE 'AMB  E'.  ELTAMBUL
00105        10  FILLER                      PIC X(6)  VALUE 'AMBM E'.  ELTAMBUL
00106      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTAMBUL
00107                                        PIC X(6) OCCURS 02 TIMES.  ELTAMBUL
00108                                                                   ELTAMBUL
00109  01   WS-TABLE-MAX-CNT                  PIC S9(4) COMP   VALUE +2.ELTAMBUL
00110                                                                   ELTAMBUL
00111 /     B E N   P R O V   I D S   B Y   T Y P E - INSTITUTIONAL     ELTAMBUL
00112  01  WS-BEN-PROV-ID-II.                                           ELTAMBUL
00113      05  WS-INST-IP-CNT                PIC S9(4) COMP   VALUE +2. ELTAMBUL
00114      05  WS-INST-IP-TAB.                                          ELTAMBUL
00115        10  FILLER                      PIC X(6)  VALUE 'AMB  B'.  ELTAMBUL
00116        10  FILLER                      PIC X(6)  VALUE 'AMBM B'.  ELTAMBUL
00117      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTAMBUL
00118                                        PIC X(6) OCCURS 02 TIMES.  ELTAMBUL
00119                                                                   ELTAMBUL
00120                                                                   ELTAMBUL
00121 /                L I T E R A L S                                  ELTAMBUL
00122  01  WS-PROGRAM-LITERALS.                                         ELTAMBUL
00123    05  WS-PERCENT                  PIC X     VALUE '%'.           ELTAMBUL
00124    05  WS-DAYS                     PIC X(04) VALUE 'DAYS'.        ELTAMBUL
00125    05  WS-YES                      PIC X     VALUE 'Y'.           ELTAMBUL
00126    05  WS-NO                       PIC X     VALUE 'N'.           ELTAMBUL
00127    05  WS-BASIC-LIT                PIC X(10) VALUE                ELTAMBUL
00128        '   BASIC: '.                                              ELTAMBUL
00129    05  WS-4096                     PIC S9(8) COMP  VALUE +4096.   ELTAMBUL
00130    05  WS-SPILLOVER-COINS          PIC X(23)                      ELTAMBUL
00131          VALUE 'SPILLOVER COINSURANCE: '.                         ELTAMBUL
00132    05  WS-SPILLOVER-DEDBL          PIC X(22)                      ELTAMBUL
00133          VALUE 'SPILLOVER DEDUCTIBLE: '.                          ELTAMBUL
00134    05  WS-SERVICES-RENDERED.                                      ELTAMBUL
00135      10  FILLER                    PIC X(26)                      ELTAMBUL
00136          VALUE 'SERVICES MAY BE RENDERED: '.                      ELTAMBUL
00137    05  WS-PAYMNT-BASED.                                           ELTAMBUL
00138      10  FILLER                    PIC X(21)                      ELTAMBUL
00139          VALUE 'PAYMENT IS BASED ON: '.                           ELTAMBUL
00140                                                                   ELTAMBUL
00141 /            D I S P L A Y   L I N E S                            ELTAMBUL
00142  01  WS-ELS-DISPLAY-LINES.                                        ELTAMBUL
00143    05  WS-HDR-2-IP.                                               ELTAMBUL
00144      10  FILLER                    PIC X(25) VALUE SPACES.        ELTAMBUL
00145      10  FILLER                    PIC X(18)                      ELTAMBUL
00146          VALUE 'AMBULANCE SERVICE '.                              ELTAMBUL
00147      10  WS-HDR2-IPOP-MSG          PIC X(13) VALUE SPACES.        ELTAMBUL
00148      10  FILLER                    PIC X(23) VALUE LOW-VALUES.    ELTAMBUL
00149                                                                   ELTAMBUL
00150    05  WS-AMBULANCE-SER-ARE.                                      ELTAMBUL
00151      10  FILLER                    PIC X(24)                      ELTAMBUL
00152          VALUE 'AMBULANCE SERVICES ARE  '.                        ELTAMBUL
00153      10  FILLER                    PIC X(32) VALUE LOW-VALUES.    ELTAMBUL
00154                                                                   ELTAMBUL
00155    05  WS-FOLLOW-BENEFIT.                                         ELTAMBUL
00156      10  FILLER                    PIC X(21) VALUE                ELTAMBUL
00157          'COVERED SERVICES ARE:'.                                 ELTAMBUL
00158                                                                   ELTAMBUL
00159    05  WS-SERVICES-2ND.                                           ELTAMBUL
00160      10  FILLER                    PIC X(21) VALUE SPACES.        ELTAMBUL
00161      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTAMBUL
00162      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTAMBUL
00163                                                                   ELTAMBUL
00164    05  WS-SERVICES-PAYABLE.                                       ELTAMBUL
00165      10  FILLER                    PIC X(45)                      ELTAMBUL
00166          VALUE 'THESE SERVICES ARE PRICED ACCORDING TO: '.        ELTAMBUL
00167      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTAMBUL
00168                                                                   ELTAMBUL
00169    05  WS-BASIC.                                                  ELTAMBUL
00170      10  FILLER                    PIC X(09) VALUE SPACES.        ELTAMBUL
00171      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTAMBUL
00172      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTAMBUL
00173                                                                   ELTAMBUL
00174    05  WS-BASIC-VISIT-AMT.                                        ELTAMBUL
00175      10  FILLER                    PIC X(09) VALUE SPACES.        ELTAMBUL
00176      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTAMBUL
00177      10  WS-BASIC-MAX-AMT          PIC ZZ9.99-.                   ELTAMBUL
00178                                                                   ELTAMBUL
00179    05  WS-SUPPLEMENTAL.                                           ELTAMBUL
00180      10  FILLER                    PIC X(16)                      ELTAMBUL
00181          VALUE '  SUPPLEMENTAL: '.                                ELTAMBUL
00182      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTAMBUL
00183                                                                   ELTAMBUL
00184    05  WS-SUPPL-VISIT-AMT.                                        ELTAMBUL
00185      10  FILLER                    PIC X(16)                      ELTAMBUL
00186          VALUE '  SUPPLEMENTAL: '.                                ELTAMBUL
00187      10  WS-SUPPL-MAX-AMT          PIC ZZ9.99-.                   ELTAMBUL
00188                                                                   ELTAMBUL
00189    05  WS-BASIC-PERCENT.                                          ELTAMBUL
00190      10  FILLER                    PIC X(09) VALUE SPACES.        ELTAMBUL
00191      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTAMBUL
00192      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTAMBUL
00193                                                                   ELTAMBUL
00194    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTAMBUL
00195      10  FILLER                    PIC X(16)                      ELTAMBUL
00196          VALUE '  SUPPLEMENTAL: '.                                ELTAMBUL
00197      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTAMBUL
00198                                                                   ELTAMBUL
00199    05  WS-BASIC-PER-D.                                            ELTAMBUL
00200      10  FILLER                    PIC X(09) VALUE SPACES.        ELTAMBUL
00201      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTAMBUL
00202      10  WS-DTL-BASIC-PER-D        PIC X(40) VALUE SPACES.        ELTAMBUL
00203      10  FILLER                    PIC X     VALUE SPACE.         ELTAMBUL
00204      10  WS-DTL-BASIC-PER-D-AMT    PIC ZZZZ9.99.                  ELTAMBUL
00205      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTAMBUL
00206                                                                   ELTAMBUL
00207    05  WS-SUPP-PER-D.                                             ELTAMBUL
00208      10  FILLER                    PIC X(16)                      ELTAMBUL
00209          VALUE '  SUPPLEMENTAL: '.                                ELTAMBUL
00210      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTAMBUL
00211      10  FILLER                    PIC X     VALUE SPACE.         ELTAMBUL
00212      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTAMBUL
00213      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTAMBUL
00214                                                                   ELTAMBUL
00215    05  WS-MAX-VISITS.                                             ELTAMBUL
00216      10  FILLER                    PIC X(34)                      ELTAMBUL
00217          VALUE 'THE MAXIMUM NUMBER OF VISITS ARE: '.              ELTAMBUL
00218      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTAMBUL
00219                                                                   ELTAMBUL
00220    05  WS-MAX-AMT-TEXT.                                           ELTAMBUL
00221      10  FILLER                    PIC X(33)                      ELTAMBUL
00222          VALUE 'THE MAXIMUM AMOUNT PER VISIT IS: '.               ELTAMBUL
00223      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTAMBUL
00224                                                                   ELTAMBUL
00225    05  WS-UNLIMITED.                                              ELTAMBUL
00226      10  WS-DTL-UNLIMITED      PIC X(20).                         ELTAMBUL
00227      10  FILLER                PIC X(26).                         ELTAMBUL
00228    05  WS-MAX-DAYS REDEFINES WS-UNLIMITED.                        ELTAMBUL
00229      10  FILLER                PIC X.                             ELTAMBUL
00230      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTAMBUL
00231      10  FILLER                PIC X.                             ELTAMBUL
00232      10  WS-DAYS-LITERAL       PIC X(04).                         ELTAMBUL
00233      10  FILLER                PIC X.                             ELTAMBUL
00234      10  WS-DTL-MAX-IND        PIC X(20).                         ELTAMBUL
00235      10  FILLER                PIC X(16).                         ELTAMBUL
00236                                                                   ELTAMBUL
00237    05  WS-DAYS-REDUCED.                                           ELTAMBUL
00238      10  FILLER                    PIC X(24) VALUE                ELTAMBUL
00239          ' BASIC DAYS ARE REDUCED '.                              ELTAMBUL
00240      10  WS-DTL-DAYS-REDUCED-APL   PIC Z9.                        ELTAMBUL
00241      10  FILLER                    PIC X(05) VALUE ' FOR '.       ELTAMBUL
00242      10  WS-DTL-DAYS-REDUCED-BASE  PIC Z9.                        ELTAMBUL
00243      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTAMBUL
00244                                                                   ELTAMBUL
00245    05  WS-SPILLOVER-FL-RT-PER-D.                                  ELTAMBUL
00246      10  FILLER                    PIC X(29)                      ELTAMBUL
00247          VALUE 'SPILLOVER FLAT RATE PER DIEM '.                   ELTAMBUL
00248      10  WS-DTL-SPILLOVER-FL-RT    PIC X(46) VALUE SPACES.        ELTAMBUL
00249      10  FILLER                    PIC X(04) VALUE LOW-VALUES.    ELTAMBUL
00250 *************************************************************     ELTAMBUL
00251    05  WS-TYPE-AMBUL-SERVC.                                       ELTAMBUL
00252      10  FILLER                    PIC X(42) VALUE                ELTAMBUL
00253          'THE TYPE OF AMBULANCE SERVICE COVERED IS: '.            ELTAMBUL
00254      10  FILLER                    PIC X(37) VALUE LOW-VALUES.    ELTAMBUL
00255 ****************************************************************  ELTAMBUL
00256                                                                   ELTAMBUL
00257 *************************************************************     ELTAMBUL
00258    05  WS-CERTIFICATION.                                          ELTAMBUL
00259      10  FILLER                    PIC X(48) VALUE                ELTAMBUL
00260          'THE CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.      ELTAMBUL
00261      10  FILLER                    PIC X(31) VALUE LOW-VALUES.    ELTAMBUL
00262 ****************************************************************  ELTAMBUL
00263    05  WS-BASIC-1-1.                                              ELTAMBUL
00264      10  FILLER                    PIC X(10) VALUE                ELTAMBUL
00265          '   BASIC: '.                                            ELTAMBUL
00266      10  WS-DTL-BASIC-1-1          PIC X(69) VALUE SPACES.        ELTAMBUL
00267                                                                   ELTAMBUL
00268    05  WS-BASIC-1-2.                                              ELTAMBUL
00269      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00270      10  WS-DTL-BASIC-1-2          PIC X(76) VALUE SPACES.        ELTAMBUL
00271                                                                   ELTAMBUL
00272    05  WS-BASIC-2-1.                                              ELTAMBUL
00273      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00274      10  WS-DTL-BASIC-2-1          PIC X(76) VALUE SPACES.        ELTAMBUL
00275                                                                   ELTAMBUL
00276    05  WS-BASIC-2-2.                                              ELTAMBUL
00277      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00278      10  WS-DTL-BASIC-2-2          PIC X(76) VALUE SPACES.        ELTAMBUL
00279                                                                   ELTAMBUL
00280    05  WS-BASIC-3-1.                                              ELTAMBUL
00281      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00282      10  WS-DTL-BASIC-3-1          PIC X(76) VALUE SPACES.        ELTAMBUL
00283                                                                   ELTAMBUL
00284    05  WS-BASIC-3-2.                                              ELTAMBUL
00285      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00286      10  WS-DTL-BASIC-3-2          PIC X(76) VALUE SPACES.        ELTAMBUL
00287                                                                   ELTAMBUL
00288    05  WS-BASIC-4-1.                                              ELTAMBUL
00289      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00290      10  WS-DTL-BASIC-4-1          PIC X(76) VALUE SPACES.        ELTAMBUL
00291                                                                   ELTAMBUL
00292    05  WS-BASIC-4-2.                                              ELTAMBUL
00293      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00294      10  WS-DTL-BASIC-4-2          PIC X(76) VALUE SPACES.        ELTAMBUL
00295 ***********************************************************       ELTAMBUL
00296 ***********************************************************       ELTAMBUL
00297    05  WS-SUPP-1-1.                                               ELTAMBUL
00298      10 FILLER                     PIC X(17) VALUE                ELTAMBUL
00299          '   SUPPLEMENTAL: '.                                     ELTAMBUL
00300      10  WS-DTL-SUPP-1-1           PIC X(62) VALUE SPACES.        ELTAMBUL
00301                                                                   ELTAMBUL
00302    05  WS-SUPP-1-2.                                               ELTAMBUL
00303      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00304      10  WS-DTL-SUPP-1-2           PIC X(76) VALUE SPACES.        ELTAMBUL
00305                                                                   ELTAMBUL
00306    05  WS-SUPP-2-1.                                               ELTAMBUL
00307      10 FILLER                     PIC X(03) VALUE SPACES.        ELTAMBUL
00308      10  WS-DTL-SUPP-2-1           PIC X(76) VALUE SPACES.        ELTAMBUL
00309                                                                   ELTAMBUL
00310    05  WS-SUPP-2-2.                                               ELTAMBUL
00311      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00312      10  WS-DTL-SUPP-2-2           PIC X(76) VALUE SPACES.        ELTAMBUL
00313                                                                   ELTAMBUL
00314    05  WS-SUPP-3-1.                                               ELTAMBUL
00315      10 FILLER                     PIC X(03) VALUE SPACES.        ELTAMBUL
00316      10  WS-DTL-SUPP-3-1           PIC X(76) VALUE SPACES.        ELTAMBUL
00317                                                                   ELTAMBUL
00318    05  WS-SUPP-3-2.                                               ELTAMBUL
00319      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00320      10  WS-DTL-SUPP-3-2           PIC X(76) VALUE SPACES.        ELTAMBUL
00321                                                                   ELTAMBUL
00322    05  WS-SUPP-4-1.                                               ELTAMBUL
00323      10 FILLER                     PIC X(03) VALUE SPACES.        ELTAMBUL
00324      10  WS-DTL-SUPP-4-1           PIC X(76) VALUE SPACES.        ELTAMBUL
00325                                                                   ELTAMBUL
00326    05  WS-SUPP-4-2.                                               ELTAMBUL
00327      10  FILLER                    PIC X(03) VALUE SPACES.        ELTAMBUL
00328      10  WS-DTL-SUPP-4-2           PIC X(76) VALUE SPACES.        ELTAMBUL
00329 *************************************************************     ELTAMBUL
00330    05  WS-CONTACT-CONTRACT.                                       ELTAMBUL
00331      10  FILLER                    PIC X(50)                      ELTAMBUL
00332        VALUE ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTAMBUL
00333      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTAMBUL
00334                                                                   ELTAMBUL
00335    05  WS-CONTRACT-RELATED.                                       ELTAMBUL
00336      10  FILLER                    PIC X(49)                      ELTAMBUL
00337        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTAMBUL
00338      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTAMBUL
00339                                                                   ELTAMBUL
00340    05  WS-PVE-TEXT.                                               ELTAMBUL
00341      10  FILLER                    PIC X(49)                      ELTAMBUL
00342        VALUE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.     '. ELTAMBUL
00343      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTAMBUL
00344                                                                   ELTAMBUL
00345    05  WS-PAY-CONSDR-TEXT1.                                       ELTAMBUL
00346      10  FILLER                    PIC X(45)                      ELTAMBUL
00347        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTAMBUL
00348    05  WS-PAY-CONSDR-TEXT2.                                       ELTAMBUL
00349      10  FILLER                    PIC X(44)                      ELTAMBUL
00350        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTAMBUL
00351                                                                   ELTAMBUL
00352    05  WS-INSTITUTIONAL-SERV.                                     ELTAMBUL
00353        10  FILLER                 PIC X(57)      VALUE            ELTAMBUL
00354      'AMBULANCE    SERVICES ARE NOT COVERED AS A INSTITUTIONAL '. ELTAMBUL
00355        10  FILLER                 PIC X(06)      VALUE            ELTAMBUL
00356      'CHARGE'.                                                    ELTAMBUL
00357  01  WS-END                            PIC X(16)  VALUE           ELTAMBUL
00358      '*** W/S ENDS ***'.                                          ELTAMBUL
00359 /             L I N K A G E   S E C T I O N                       ELTAMBUL
00360  LINKAGE SECTION.                                                 ELTAMBUL
00361  01  DFHCOMMAREA.                                                 ELTAMBUL
00362      COPY ELSCOMMC.                                               ELTAMBUL
00363 /                                                                 ELTAMBUL
00364      COPY ELSCIA2C.                                               ELTAMBUL
00365 /                                                                 ELTAMBUL
00366 ***  IO PARM AREA ***                                             ELTAMBUL
00367      COPY ELSIOPMC.                                               ELTAMBUL
00368 /                                                                 ELTAMBUL
00369      COPY ELSKEYSC.                                               ELTAMBUL
00370 /                                                                 ELTAMBUL
00371      COPY ELSOUTPC.                                               ELTAMBUL
00372 /                                                                 ELTAMBUL
00373      COPY ELSSSCBC.                                               ELTAMBUL
00374 /                                                                 ELTAMBUL
00375      COPY ELSCMIFC.                                               ELTAMBUL
00376 /                                                                 ELTAMBUL
00377      COPY ELSCMDSC.                                               ELTAMBUL
00378 /                                                                 ELTAMBUL
00379      COPY ELSPRVNC.                                               ELTAMBUL
00380 /                                                                 ELTAMBUL
00381 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTAMBUL
00382      COPY ELSPLGSW.                                               ELTAMBUL
00383 *** BENEFIT PROVISION TABLE OF FLDS                               ELTAMBUL
00384      COPY ELSPLGTB.                                               ELTAMBUL
00385 /                                                                 ELTAMBUL
00386      COPY ELSTCWAC.                                               ELTAMBUL
00387 /                  M A I N L I N E                                ELTAMBUL
00388  PROCEDURE DIVISION.                                              ELTAMBUL
00389                                                                   ELTAMBUL
00390 ******************************************************************ELTAMBUL
00391 *                                                                 ELTAMBUL
00392 *   PERFORM THE MAINLINE OPERATIONS.                              ELTAMBUL
00393 *                                                                 ELTAMBUL
00394 ******************************************************************ELTAMBUL
00395  0000-MAINLINE.                                                   ELTAMBUL
00396                                                                   ELTAMBUL
00397      MOVE '0'  TO  WS-CHAR-0.                                     ELTAMBUL
00398                                                                   ELTAMBUL
00399      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTAMBUL
00400          SET CIA-AB-DFHCOMMAREA TO TRUE                           ELTAMBUL
00401          EXEC CICS ABEND ABCODE('EL01') END-EXEC.                 ELTAMBUL
00402                                                                   ELTAMBUL
00403      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTAMBUL
00404          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTAMBUL
00405                                                                   ELTAMBUL
00406      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTAMBUL
00407      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00408          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTAMBUL
00409                                                                   ELTAMBUL
00410      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTAMBUL
00411      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00412          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTAMBUL
00413                                                                   ELTAMBUL
00414      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTAMBUL
00415      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00416          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTAMBUL
00417                                                                   ELTAMBUL
00418      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTAMBUL
00419      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00420          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTAMBUL
00421                                                                   ELTAMBUL
00422      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTAMBUL
00423      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00424          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTAMBUL
00425                                                                   ELTAMBUL
00426      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTAMBUL
00427      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00428          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTAMBUL
00429                                                                   ELTAMBUL
00430      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTAMBUL
00431                                                                   ELTAMBUL
00432      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTAMBUL
00433                                                                   ELTAMBUL
00434      COMPUTE CIA-AREA-LEN    = LENGTH OF PVN-FIXED-PART +         ELTAMBUL
00435            (WS-TABLE-MAX-CNT  *  LENGTH OF PVN-BEN-PROVN-TBL).    ELTAMBUL
00436                                                                   ELTAMBUL
00437      SET CIA-STG-GETMAIN TO TRUE.                                 ELTAMBUL
00438                                                                   ELTAMBUL
00439      EXEC CICS LINK                                               ELTAMBUL
00440                PROGRAM('ELUSTGMG')                                ELTAMBUL
00441                COMMAREA(DFHCOMMAREA)                              ELTAMBUL
00442      END-EXEC.                                                    ELTAMBUL
00443                                                                   ELTAMBUL
00444      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTAMBUL
00445      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00446          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTAMBUL
00447                                                                   ELTAMBUL
00448      IF (SSB-PROV-CLASS-INST OR SSB-PROV-CLASS-BOTH)              ELTAMBUL
00449         PERFORM 1000-INSTITUTIONAL-IP-RTNE.                       ELTAMBUL
00450                                                                   ELTAMBUL
00451      IF (SSB-PROV-CLASS-PROF OR SSB-PROV-CLASS-BOTH)              ELTAMBUL
00452         PERFORM 2000-PROFESSIONAL-IP-RTNE.                        ELTAMBUL
00453                                                                   ELTAMBUL
00454 ******NOTIFY THE OUTPUT ROUTINE THAT WE ARE DONE***********       ELTAMBUL
00455      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00456      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTAMBUL
00457      MOVE 'E'  TO  COF-FUNCTION.                                  ELTAMBUL
00458                                                                   ELTAMBUL
00459      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00460                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00461      END-EXEC.                                                    ELTAMBUL
00462                                                                   ELTAMBUL
00463  0099-RETURN.                                                     ELTAMBUL
00464      EXEC CICS RETURN   END-EXEC.                                 ELTAMBUL
00465                                                                   ELTAMBUL
00466      GOBACK.                                                      ELTAMBUL
00467                                                                   ELTAMBUL
00468 /        I N S T I T U T I O N A L  I P   R T N E                 ELTAMBUL
00469 ***************************************************************** ELTAMBUL
00470 *        I N S T I T U T I O N A L  I P   R T N E                 ELTAMBUL
00471 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTAMBUL
00472 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTAMBUL
00473 ***************************************************************** ELTAMBUL
00474  1000-INSTITUTIONAL-IP-RTNE SECTION.                              ELTAMBUL
00475      MOVE '1000'  TO  WS-PARA-ID.                                 ELTAMBUL
00476                                                                   ELTAMBUL
00477      MOVE 'Y'     TO WS-FIRSTTIME-IND.                            ELTAMBUL
00478      MOVE 'INSTITUTIONAL' TO WS-HDR2-IPOP-MSG.                    ELTAMBUL
00479                                                                   ELTAMBUL
00480      MOVE WS-INST-IP-CNT  TO PVN-NBR-BEN-PROVN.                   ELTAMBUL
00481      PERFORM 1010-MOVE-IN-INST-IP                                 ELTAMBUL
00482         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTAMBUL
00483         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTAMBUL
00484                                                                   ELTAMBUL
00485      GO TO 1020-CALL-COVERAGE.                                    ELTAMBUL
00486  1010-MOVE-IN-INST-IP.                                            ELTAMBUL
00487      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTAMBUL
00488      MOVE WS-INST-IP-LIST(WS-SUB)  TO                             ELTAMBUL
00489                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTAMBUL
00490      MOVE ZERO  TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),          ELTAMBUL
00491                    PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).          ELTAMBUL
00492                                                                   ELTAMBUL
00493  1020-CALL-COVERAGE.                                              ELTAMBUL
00494      MOVE '1020'              TO  WS-PARA-ID.                     ELTAMBUL
00495      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTAMBUL
00496 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTAMBUL
00497      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTAMBUL
00498      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00499      MOVE 'P'  TO  COF-FUNCTION.                                  ELTAMBUL
00500                                                                   ELTAMBUL
00501      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00502                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00503      END-EXEC.                                                    ELTAMBUL
00504 ****************************************************              ELTAMBUL
00505                                                                   ELTAMBUL
00506      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTAMBUL
00507      MOVE WS-AMBULANCE-SER-ARE      TO  SSB-TOPIC-PHRASE.         ELTAMBUL
00508                                                                   ELTAMBUL
00509      EXEC CICS  LINK  PROGRAM('ELGCOVER')                         ELTAMBUL
00510                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00511      END-EXEC.                                                    ELTAMBUL
00512                                                                   ELTAMBUL
00513 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTAMBUL
00514      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00515      MOVE ' '  TO  COF-FUNCTION.                                  ELTAMBUL
00516      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00517                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00518      END-EXEC.                                                    ELTAMBUL
00519 *4/15 END OF TEMPORARY CODE                                       ELTAMBUL
00520                                                                   ELTAMBUL
00521      IF PVN-COVG-NONE                                             ELTAMBUL
00522         GO TO 1099-EXIT.                                          ELTAMBUL
00523                                                                   ELTAMBUL
00524      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTAMBUL
00525                                                                   ELTAMBUL
00526 ****** SET REQUIRED PAYMENT LEVEL SWITCHES ON ***************     ELTAMBUL
00527 *************************************************************     ELTAMBUL
00528         MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                   ELTAMBUL
00529            PSP-PROVN-PRICING-METHD,                               ELTAMBUL
00530            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTAMBUL
00531            PSP-TRANSF-OTHER-RESP-IND,                             ELTAMBUL
00532            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTAMBUL
00533            PSP-SPILL-OVER-COINS-APL-IND,                          ELTAMBUL
00534            PSP-SPILL-OVER-DED-APL-IND,                            ELTAMBUL
00535            PSP-CERTFN-REQRM-IND,                                  ELTAMBUL
00536            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTAMBUL
00537            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTAMBUL
00538            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTAMBUL
00539            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTAMBUL
00540            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTAMBUL
00541            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTAMBUL
00542            PSB-MAX-AMT-PER-VISIT,                                 ELTAMBUL
00543            PSB-AMBULANCE-ELIG-IND.                                ELTAMBUL
00544                                                                   ELTAMBUL
00545      EXEC CICS  LINK  PROGRAM('ELUPLGRP')                         ELTAMBUL
00546                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00547      END-EXEC.                                                    ELTAMBUL
00548                                                                   ELTAMBUL
00549      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTAMBUL
00550      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00551          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTAMBUL
00552                                                                   ELTAMBUL
00553      PERFORM 1030-FIND-FIRST-NONZERO                              ELTAMBUL
00554         VARYING WS-SUB  FROM  +1  BY  +1                          ELTAMBUL
00555         UNTIL WS-SUB  >  WS-INST-IP-CNT.                          ELTAMBUL
00556                                                                   ELTAMBUL
00557      GO TO 1099-EXIT.                                             ELTAMBUL
00558  1030-FIND-FIRST-NONZERO.                                         ELTAMBUL
00559      SET PLT-INDEX1,  PVN-BEN-PROVN-IDX TO WS-SUB.                ELTAMBUL
00560      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTAMBUL
00561         NEXT SENTENCE                                             ELTAMBUL
00562      ELSE                                                         ELTAMBUL
00563         PERFORM 1040-BUILD-SCREEN-LINES.                          ELTAMBUL
00564                                                                   ELTAMBUL
00565  1040-BUILD-SCREEN-LINES.                                         ELTAMBUL
00566      MOVE '1040'  TO  WS-PARA-ID.                                 ELTAMBUL
00567                                                                   ELTAMBUL
00568      SET PLT-INDEX1 TO                                            ELTAMBUL
00569         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTAMBUL
00570                                                                   ELTAMBUL
00571      IF WS-NOT-FIRST-TIME                                         ELTAMBUL
00572         MOVE 'P'    TO COF-FUNCTION                               ELTAMBUL
00573         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTAMBUL
00574         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTAMBUL
00575                          COMMAREA(DFHCOMMAREA)                    ELTAMBUL
00576         END-EXEC                                                  ELTAMBUL
00577      ELSE                                                         ELTAMBUL
00578        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTAMBUL
00579                                                                   ELTAMBUL
00580      MOVE +1    TO  WS-CIA.                                       ELTAMBUL
00581      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTAMBUL
00582                                                                   ELTAMBUL
00583      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTAMBUL
00584         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTAMBUL
00585            SET PLT-INDEX2  TO  2                                  ELTAMBUL
00586         ELSE                                                      ELTAMBUL
00587            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTAMBUL
00588            GO TO 1099-EXIT                                        ELTAMBUL
00589      ELSE                                                         ELTAMBUL
00590         SET PLT-INDEX2  TO  1.                                    ELTAMBUL
00591      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTAMBUL
00592                                                                   ELTAMBUL
00593      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTAMBUL
00594      ADD +1                 TO WS-CIA.                            ELTAMBUL
00595                                                                   ELTAMBUL
00596      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTAMBUL
00597      ADD +1                 TO WS-CIA.                            ELTAMBUL
00598                                                                   ELTAMBUL
00599      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTAMBUL
00600                    WS-SERVICES-PAYBLE-SW                          ELTAMBUL
00601                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTAMBUL
00602                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTAMBUL
00603                                                                   ELTAMBUL
00604      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTAMBUL
00605        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTAMBUL
00606        UNTIL WS-SUB2 GREATER WS-INST-IP-CNT.                      ELTAMBUL
00607                                                                   ELTAMBUL
00608      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTAMBUL
00609 *************************************************************     ELTAMBUL
00610 **** SERVICES MAY BE RENDERED                                     ELTAMBUL
00611 **************************************************************    ELTAMBUL
00612      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00613        SET  PLT-INDEX2       TO  1                                ELTAMBUL
00614       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00615         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00616              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTAMBUL
00617                                                                   ELTAMBUL
00618      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00619        SET PLT-INDEX2        TO 2                                 ELTAMBUL
00620       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00621         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00622              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTAMBUL
00623                                                                   ELTAMBUL
00624      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTAMBUL
00625          ADD  +1               TO  WS-CIA                         ELTAMBUL
00626          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTAMBUL
00627          ADD  +1               TO  WS-CIA.                        ELTAMBUL
00628                                                                   ELTAMBUL
00629      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00630        SET  PLT-INDEX2       TO  1                                ELTAMBUL
00631       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00632         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00633          PERFORM 4000-PLACE-OF-TREATMENT                          ELTAMBUL
00634          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
00635          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
00636          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
00637          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
00638          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTAMBUL
00639          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTAMBUL
00640          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTAMBUL
00641             ADD +1                TO WS-CIA                       ELTAMBUL
00642             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTAMBUL
00643             PERFORM 3000-OUTPUT-TEXT                              ELTAMBUL
00644          ELSE                                                     ELTAMBUL
00645           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
00646                                                                   ELTAMBUL
00647      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00648        SET PLT-INDEX2        TO 2                                 ELTAMBUL
00649       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00650         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00651          PERFORM 4000-PLACE-OF-TREATMENT                          ELTAMBUL
00652          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
00653          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
00654          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
00655          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
00656          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTAMBUL
00657          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTAMBUL
00658          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTAMBUL
00659             ADD +1                TO WS-CIA                       ELTAMBUL
00660             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTAMBUL
00661             PERFORM 3000-OUTPUT-TEXT                              ELTAMBUL
00662          ELSE                                                     ELTAMBUL
00663           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
00664                                                                   ELTAMBUL
00665 **************************************************************    ELTAMBUL
00666 **** SERVICES ARE PAYABLE                                         ELTAMBUL
00667 **************************************************************    ELTAMBUL
00668      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00669         SET  PLT-INDEX2           TO  1                           ELTAMBUL
00670        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00671         NOT EQUAL '19'                                            ELTAMBUL
00672         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTAMBUL
00673                                                                   ELTAMBUL
00674      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00675         SET  PLT-INDEX2             TO  2                         ELTAMBUL
00676        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00677         NOT EQUAL '19'                                            ELTAMBUL
00678         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTAMBUL
00679                                                                   ELTAMBUL
00680 ******************************************************************ELTAMBUL
00681 **** MAXIMUM AMOUNT PER VISIT                                     ELTAMBUL
00682 ***************************************************************** ELTAMBUL
00683                                                                   ELTAMBUL
00684      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00685       SET PLT-INDEX2 TO 1                                         ELTAMBUL
00686       IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
00687        IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
00688                                         NOT = ZEROS               ELTAMBUL
00689          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTAMBUL
00690                                                                   ELTAMBUL
00691      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00692       SET  PLT-INDEX2 TO  2                                       ELTAMBUL
00693       IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
00694        IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
00695                                         NOT = ZEROS               ELTAMBUL
00696          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTAMBUL
00697                                                                   ELTAMBUL
00698      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTAMBUL
00699          ADD +1             TO WS-CIA                             ELTAMBUL
00700          MOVE WS-MAX-AMT-TEXT TO COF-DTL-LINE(WS-CIA).            ELTAMBUL
00701                                                                   ELTAMBUL
00702      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00703       SET  PLT-INDEX2           TO  1                             ELTAMBUL
00704       IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
00705        IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
00706                                         NOT = ZEROS               ELTAMBUL
00707          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
00708           TO WS-BASIC-MAX-AMT                                     ELTAMBUL
00709          ADD +1             TO WS-CIA                             ELTAMBUL
00710          MOVE WS-BASIC-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTAMBUL
00711                                                                   ELTAMBUL
00712      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00713       SET  PLT-INDEX2           TO  2                             ELTAMBUL
00714       IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
00715        IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
00716                                         NOT = ZEROS               ELTAMBUL
00717          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
00718           TO WS-SUPPL-MAX-AMT                                     ELTAMBUL
00719          ADD +1             TO WS-CIA                             ELTAMBUL
00720          MOVE WS-SUPPL-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTAMBUL
00721                                                                   ELTAMBUL
00722      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTAMBUL
00723             PERFORM 3000-OUTPUT-TEXT.                             ELTAMBUL
00724 ****************************************************************  ELTAMBUL
00725 *** TYPE OF AMBULANCE SERVICE COVERED TRANSLATION                 ELTAMBUL
00726 ****************************************************************  ELTAMBUL
00727      PERFORM 7000-AMBUL-SERVC-COVERAGE.                           ELTAMBUL
00728                                                                   ELTAMBUL
00729 ****************************************************************  ELTAMBUL
00730 *** CERTIFICATION REQUIRED            TRANSLATION                 ELTAMBUL
00731 ****************************************************************  ELTAMBUL
00732      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
00733         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
00734         NEXT SENTENCE                                             ELTAMBUL
00735      ELSE                                                         ELTAMBUL
00736         PERFORM 7100-INST-CERTIFICATION.                          ELTAMBUL
00737                                                                   ELTAMBUL
00738 ****************************************************************  ELTAMBUL
00739 *** TRANSFER TO OTHER RESPONSIBILITY INDICATOR                    ELTAMBUL
00740 ****************************************************************  ELTAMBUL
00741      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
00742         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
00743         NEXT SENTENCE                                             ELTAMBUL
00744      ELSE                                                         ELTAMBUL
00745         PERFORM 7200-TRANS-OTHER-RESP.                            ELTAMBUL
00746                                                                   ELTAMBUL
00747 ****************************************************************  ELTAMBUL
00748 *** SPILLOVER COINS AND DEDUCTIBLE                                ELTAMBUL
00749 ****************************************************************  ELTAMBUL
00750      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00751         SET  PLT-INDEX2          TO  2                            ELTAMBUL
00752         PERFORM 4300-SPILLOVER-COINS                              ELTAMBUL
00753         PERFORM 4400-SPILLOVER-DEDUCT.                            ELTAMBUL
00754                                                                   ELTAMBUL
00755 ***************************************************************** ELTAMBUL
00756 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTAMBUL
00757 ***************************************************************   ELTAMBUL
00758      PERFORM 4650-SCAN-TAB.                                       ELTAMBUL
00759      PERFORM 4675-PAY-CONSID-TEXT.                                ELTAMBUL
00760                                                                   ELTAMBUL
00761  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTAMBUL
00762      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTAMBUL
00763                                                                   ELTAMBUL
00764      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB3             ELTAMBUL
00765         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTAMBUL
00766         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTAMBUL
00767         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTAMBUL
00768                                                   CMF-CODE-VALUE  ELTAMBUL
00769         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTAMBUL
00770         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTAMBUL
00771         STRING CMF-DESCR-LINE (1) ' '                             ELTAMBUL
00772                CMF-DESCR-LINE (2)                                 ELTAMBUL
00773                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTAMBUL
00774         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTAMBUL
00775         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTAMBUL
00776         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTAMBUL
00777         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTAMBUL
00778         PERFORM TCPR-000-TEXT-UNSTRING                            ELTAMBUL
00779         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTAMBUL
00780         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
00781         IF WS-CIA  <  20                                          ELTAMBUL
00782            ADD +1  TO  WS-CIA                                     ELTAMBUL
00783            MOVE ZERO  TO                                          ELTAMBUL
00784                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTAMBUL
00785         ELSE                                                      ELTAMBUL
00786            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTAMBUL
00787                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
00788            END-EXEC                                               ELTAMBUL
00789            MOVE +1  TO  WS-CIA                                    ELTAMBUL
00790            MOVE ZERO  TO                                          ELTAMBUL
00791                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTAMBUL
00792                                                                   ELTAMBUL
00793  1090-PROBLEM-WITH-INDICES.                                       ELTAMBUL
00794                                                                   ELTAMBUL
00795      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTAMBUL
00796      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTAMBUL
00797                                                                   ELTAMBUL
00798      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00799      MOVE 'P'  TO  COF-FUNCTION.                                  ELTAMBUL
00800                                                                   ELTAMBUL
00801      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00802                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00803      END-EXEC.                                                    ELTAMBUL
00804                                                                   ELTAMBUL
00805  1099-EXIT.            EXIT.                                      ELTAMBUL
00806                                                                   ELTAMBUL
00807  1000-EXIT.  EXIT.                                                ELTAMBUL
00808 /        P R O F E S S I O N A L   I P   R T N E                  ELTAMBUL
00809 ***************************************************************** ELTAMBUL
00810 *        P R O F E S S I O N A L   I P   R T N E                  ELTAMBUL
00811 *    PERFORM THE CODE NECESSARY TO WRITE THE CURRENT PAGE, THEN   ELTAMBUL
00812 *  DO WHATEVER IS NEEDED FOR THE NEXT PAGE.                       ELTAMBUL
00813 ***************************************************************** ELTAMBUL
00814  2000-PROFESSIONAL-IP-RTNE SECTION.                               ELTAMBUL
00815 ****************************************************              ELTAMBUL
00816      MOVE '2000'  TO  WS-PARA-ID.                                 ELTAMBUL
00817      MOVE 'PROFESSIONAL' TO WS-HDR2-IPOP-MSG.                     ELTAMBUL
00818                                                                   ELTAMBUL
00819      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTAMBUL
00820                                                                   ELTAMBUL
00821      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTAMBUL
00822                                                                   ELTAMBUL
00823      PERFORM 2010-MOVE-IN-PROF-IP                                 ELTAMBUL
00824         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTAMBUL
00825         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTAMBUL
00826                                                                   ELTAMBUL
00827      GO TO 2020-CALL-COVERAGE.                                    ELTAMBUL
00828  2010-MOVE-IN-PROF-IP.                                            ELTAMBUL
00829      SET  PVN-BEN-PROVN-IDX TO WS-SUB.                            ELTAMBUL
00830      MOVE WS-PROF-IP-LIST(WS-SUB)  TO                             ELTAMBUL
00831                            PVN-BEN-PROVN-ID(PVN-BEN-PROVN-IDX).   ELTAMBUL
00832      MOVE ZERO  TO PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX),           ELTAMBUL
00833                    PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX).           ELTAMBUL
00834                                                                   ELTAMBUL
00835  2020-CALL-COVERAGE.                                              ELTAMBUL
00836      MOVE '2020'              TO  WS-PARA-ID.                     ELTAMBUL
00837      MOVE WS-HDR-2-IP         TO  COF-HDR-LINE(2).                ELTAMBUL
00838 **********CALL OUTPUT FOR NEW PAGE WITH NEW HEADING LINES*********ELTAMBUL
00839      MOVE +0  TO  COF-NBR-DTL-LINES.                              ELTAMBUL
00840      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00841      MOVE 'P'  TO  COF-FUNCTION.                                  ELTAMBUL
00842                                                                   ELTAMBUL
00843      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00844                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00845      END-EXEC.                                                    ELTAMBUL
00846 ****************************************************              ELTAMBUL
00847      MOVE +0                        TO  COF-NBR-DTL-LINES.        ELTAMBUL
00848      MOVE WS-AMBULANCE-SER-ARE TO SSB-TOPIC-PHRASE.               ELTAMBUL
00849                                                                   ELTAMBUL
00850      EXEC CICS  LINK  PROGRAM('ELGCOVER')                         ELTAMBUL
00851                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00852      END-EXEC.                                                    ELTAMBUL
00853 *****************************************************             ELTAMBUL
00854                                                                   ELTAMBUL
00855 *4/15 TEMPORARY CALL TO ELUOUTPT TO GET COVERAGE TEXT DISPLAYED   ELTAMBUL
00856      IF COF-NBR-DTL-LINES GREATER +2                              ELTAMBUL
00857        MOVE SPACES TO COF-DTL-LINE (1)                            ELTAMBUL
00858        MOVE SPACES TO COF-DTL-LINE (2).                           ELTAMBUL
00859      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
00860      MOVE ' '  TO  COF-FUNCTION.                                  ELTAMBUL
00861      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
00862                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00863      END-EXEC.                                                    ELTAMBUL
00864 *4/15 END OF TEMPORARY CODE                                       ELTAMBUL
00865                                                                   ELTAMBUL
00866      IF PVN-COVG-NONE                                             ELTAMBUL
00867         GO TO 2099-EXIT.                                          ELTAMBUL
00868                                                                   ELTAMBUL
00869      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTAMBUL
00870                                                                   ELTAMBUL
00871      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTAMBUL
00872            PSP-PROVN-PRICING-METHD,                               ELTAMBUL
00873            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTAMBUL
00874            PSP-TRANSF-OTHER-RESP-IND,                             ELTAMBUL
00875            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTAMBUL
00876            PSP-SPILL-OVER-COINS-APL-IND,                          ELTAMBUL
00877            PSP-SPILL-OVER-DED-APL-IND,                            ELTAMBUL
00878            PSP-CERTFN-REQRM-IND,                                  ELTAMBUL
00879            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTAMBUL
00880            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTAMBUL
00881            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTAMBUL
00882            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTAMBUL
00883            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTAMBUL
00884            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTAMBUL
00885            PSE-BEN-SCOPE-ID,                                      ELTAMBUL
00886            PSE-MAX-AMT-PER-VISIT,                                 ELTAMBUL
00887            PSE-BEN-MAX-VISITS-IND,                                ELTAMBUL
00888            PSE-BEN-MAX-VISITS-DAYS,                               ELTAMBUL
00889            PSE-AMBULANCE-ELIG-IND.                                ELTAMBUL
00890                                                                   ELTAMBUL
00891      EXEC CICS  LINK  PROGRAM('ELUPLGRP')                         ELTAMBUL
00892                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
00893      END-EXEC.                                                    ELTAMBUL
00894                                                                   ELTAMBUL
00895      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTAMBUL
00896      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
00897          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTAMBUL
00898                                                                   ELTAMBUL
00899      PERFORM 2030-FIND-FIRST-NONZERO                              ELTAMBUL
00900         VARYING WS-SUB  FROM  +1  BY  +1                          ELTAMBUL
00901         UNTIL WS-SUB  >  WS-PROF-IP-CNT.                          ELTAMBUL
00902                                                                   ELTAMBUL
00903                                                                   ELTAMBUL
00904      GO TO 2099-EXIT.                                             ELTAMBUL
00905  2030-FIND-FIRST-NONZERO.                                         ELTAMBUL
00906      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTAMBUL
00907      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = ZERO                ELTAMBUL
00908         CONTINUE                                                  ELTAMBUL
00909      ELSE                                                         ELTAMBUL
00910         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTAMBUL
00911                                                                   ELTAMBUL
00912  2040-BUILD-SCREEN-LINES.                                         ELTAMBUL
00913      MOVE '2040'  TO  WS-PARA-ID.                                 ELTAMBUL
00914                                                                   ELTAMBUL
00915      SET PLT-INDEX1 TO                                            ELTAMBUL
00916         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTAMBUL
00917                                                                   ELTAMBUL
00918      IF WS-NOT-FIRST-TIME                                         ELTAMBUL
00919         MOVE 'P'    TO COF-FUNCTION                               ELTAMBUL
00920         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTAMBUL
00921         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTAMBUL
00922                          COMMAREA(DFHCOMMAREA)                    ELTAMBUL
00923         END-EXEC                                                  ELTAMBUL
00924      ELSE                                                         ELTAMBUL
00925        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTAMBUL
00926                                                                   ELTAMBUL
00927      MOVE +1    TO  WS-CIA.                                       ELTAMBUL
00928      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTAMBUL
00929                                                                   ELTAMBUL
00930      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTAMBUL
00931         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTAMBUL
00932            SET PLT-INDEX2  TO  2                                  ELTAMBUL
00933         ELSE                                                      ELTAMBUL
00934            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTAMBUL
00935            GO TO 2099-EXIT                                        ELTAMBUL
00936      ELSE                                                         ELTAMBUL
00937         SET PLT-INDEX2  TO  1.                                    ELTAMBUL
00938      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) TO WS-SUB3.         ELTAMBUL
00939                                                                   ELTAMBUL
00940      MOVE LOW-VALUES        TO COF-DTL-LINE(WS-CIA).              ELTAMBUL
00941      ADD +1                 TO WS-CIA.                            ELTAMBUL
00942                                                                   ELTAMBUL
00943      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTAMBUL
00944      ADD +1                 TO WS-CIA.                            ELTAMBUL
00945                                                                   ELTAMBUL
00946      MOVE WS-NO TO WS-MAX-AMT-TEXT-SW                             ELTAMBUL
00947                    WS-SERVICES-PAYBLE-SW                          ELTAMBUL
00948                    WS-DISPLAY-SERVIC-REND-TEXT                    ELTAMBUL
00949                    WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTAMBUL
00950                                                                   ELTAMBUL
00951      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTAMBUL
00952        VARYING WS-SUB2 FROM WS-SUB BY +1                          ELTAMBUL
00953        UNTIL WS-SUB2 GREATER WS-PROF-IP-CNT.                      ELTAMBUL
00954                                                                   ELTAMBUL
00955      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTAMBUL
00956 *************************************************************     ELTAMBUL
00957 **** SERVICES MAY BE RENDERED                                     ELTAMBUL
00958 **************************************************************    ELTAMBUL
00959      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00960        SET  PLT-INDEX2       TO  1                                ELTAMBUL
00961       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00962         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00963              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTAMBUL
00964                                                                   ELTAMBUL
00965      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00966        SET PLT-INDEX2        TO 2                                 ELTAMBUL
00967       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00968         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00969              MOVE WS-YES TO WS-DISPLAY-SERVIC-REND-TEXT.          ELTAMBUL
00970                                                                   ELTAMBUL
00971      IF WS-DISPLAY-SERVIC-REND-TEXT = WS-YES                      ELTAMBUL
00972          ADD  +1               TO  WS-CIA                         ELTAMBUL
00973          MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE(WS-CIA)        ELTAMBUL
00974          ADD  +1               TO  WS-CIA.                        ELTAMBUL
00975                                                                   ELTAMBUL
00976      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00977        SET  PLT-INDEX2       TO  1                                ELTAMBUL
00978       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00979         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00980          PERFORM 4000-PLACE-OF-TREATMENT                          ELTAMBUL
00981          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
00982          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
00983          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
00984          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
00985          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC                    ELTAMBUL
00986          MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)            ELTAMBUL
00987          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTAMBUL
00988             ADD +1                TO WS-CIA                       ELTAMBUL
00989             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTAMBUL
00990             PERFORM 3000-OUTPUT-TEXT                              ELTAMBUL
00991          ELSE                                                     ELTAMBUL
00992           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
00993                                                                   ELTAMBUL
00994      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
00995        SET PLT-INDEX2        TO 2                                 ELTAMBUL
00996       IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
00997         NOT = ZEROS AND NOT = LOW-VALUES                          ELTAMBUL
00998          PERFORM 4000-PLACE-OF-TREATMENT                          ELTAMBUL
00999          MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
01000          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
01001          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
01002          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
01003          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL             ELTAMBUL
01004          MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)            ELTAMBUL
01005          IF TCAR-OUTPUT-FIELDS-USED > 1                           ELTAMBUL
01006             ADD +1                TO WS-CIA                       ELTAMBUL
01007             MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)         ELTAMBUL
01008             PERFORM 3000-OUTPUT-TEXT                              ELTAMBUL
01009          ELSE                                                     ELTAMBUL
01010           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
01011                                                                   ELTAMBUL
01012 ****************************************************************  ELTAMBUL
01013 *** TRANSFER TO OTHER RESPONSIBILITY INDICATOR                    ELTAMBUL
01014 ****************************************************************  ELTAMBUL
01015      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
01016         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
01017         NEXT SENTENCE                                             ELTAMBUL
01018      ELSE                                                         ELTAMBUL
01019         PERFORM 7200-TRANS-OTHER-RESP.                            ELTAMBUL
01020                                                                   ELTAMBUL
01021 **************************************************************    ELTAMBUL
01022 **** SERVICES ARE PAYABLE                                         ELTAMBUL
01023 **************************************************************    ELTAMBUL
01024      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01025         SET  PLT-INDEX2           TO  1                           ELTAMBUL
01026        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01027         NOT EQUAL '19'                                            ELTAMBUL
01028         PERFORM 4100-PAYABLE-AS-BASIC.                            ELTAMBUL
01029                                                                   ELTAMBUL
01030      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01031         SET  PLT-INDEX2             TO  2                         ELTAMBUL
01032        IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01033         NOT EQUAL '19'                                            ELTAMBUL
01034         PERFORM 4200-PAYABLE-AS-SUPP.                             ELTAMBUL
01035                                                                   ELTAMBUL
01036 ******************************************************************ELTAMBUL
01037 **** MAXIMUM AMOUNT PER VISIT                                     ELTAMBUL
01038 ***************************************************************** ELTAMBUL
01039                                                                   ELTAMBUL
01040      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01041       SET PLT-INDEX2 TO 1                                         ELTAMBUL
01042       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
01043        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
01044                                         NOT = ZEROS               ELTAMBUL
01045          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTAMBUL
01046                                                                   ELTAMBUL
01047      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01048       SET  PLT-INDEX2 TO  2                                       ELTAMBUL
01049       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
01050        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
01051                                         NOT = ZEROS               ELTAMBUL
01052          MOVE WS-YES TO WS-MAX-AMT-TEXT-SW.                       ELTAMBUL
01053                                                                   ELTAMBUL
01054      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTAMBUL
01055          ADD +1             TO WS-CIA                             ELTAMBUL
01056          MOVE WS-MAX-AMT-TEXT TO COF-DTL-LINE(WS-CIA).            ELTAMBUL
01057                                                                   ELTAMBUL
01058      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01059       SET  PLT-INDEX2           TO  1                             ELTAMBUL
01060       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
01061        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
01062                                         NOT = ZEROS               ELTAMBUL
01063          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01064           TO WS-BASIC-MAX-AMT                                     ELTAMBUL
01065          ADD +1             TO WS-CIA                             ELTAMBUL
01066          MOVE WS-BASIC-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTAMBUL
01067                                                                   ELTAMBUL
01068      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01069       SET  PLT-INDEX2           TO  2                             ELTAMBUL
01070       IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2) NUMERIC   ELTAMBUL
01071        IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
01072                                         NOT = ZEROS               ELTAMBUL
01073          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01074           TO WS-SUPPL-MAX-AMT                                     ELTAMBUL
01075          ADD +1             TO WS-CIA                             ELTAMBUL
01076          MOVE WS-SUPPL-VISIT-AMT TO COF-DTL-LINE(WS-CIA).         ELTAMBUL
01077                                                                   ELTAMBUL
01078      IF WS-MAX-AMT-TEXT-SW        = WS-YES                        ELTAMBUL
01079             PERFORM 3000-OUTPUT-TEXT.                             ELTAMBUL
01080 ****************************************************************  ELTAMBUL
01081 *** TYPE OF AMBULANCE SERVICE COVERED TRANSLATION                 ELTAMBUL
01082 ****************************************************************  ELTAMBUL
01083      PERFORM 6000-AMBUL-SERVC-COVERAGE.                           ELTAMBUL
01084                                                                   ELTAMBUL
01085 ****************************************************************  ELTAMBUL
01086 *** CERTIFICATION REQUIRED            TRANSLATION                 ELTAMBUL
01087 ****************************************************************  ELTAMBUL
01088      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
01089         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
01090         NEXT SENTENCE                                             ELTAMBUL
01091      ELSE                                                         ELTAMBUL
01092         PERFORM 6100-PROF-CERTIFICATION.                          ELTAMBUL
01093                                                                   ELTAMBUL
01094 ****************************************************************  ELTAMBUL
01095 *** SPILLOVER COINS AND DEDUCTIBLE                                ELTAMBUL
01096 ****************************************************************  ELTAMBUL
01097      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
01098         SET  PLT-INDEX2          TO  2                            ELTAMBUL
01099         PERFORM 4300-SPILLOVER-COINS                              ELTAMBUL
01100         PERFORM 4400-SPILLOVER-DEDUCT.                            ELTAMBUL
01101                                                                   ELTAMBUL
01102 ***************************************************************** ELTAMBUL
01103 *** AAR PPF PVE AND AND ALL LEVEL  TABULARS                       ELTAMBUL
01104 ***************************************************************   ELTAMBUL
01105      PERFORM 4650-SCAN-TAB.                                       ELTAMBUL
01106      PERFORM 4675-PAY-CONSID-TEXT.                                ELTAMBUL
01107                                                                   ELTAMBUL
01108  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTAMBUL
01109      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTAMBUL
01110                                                                   ELTAMBUL
01111      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX) = WS-SUB              ELTAMBUL
01112         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTAMBUL
01113         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTAMBUL
01114         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTAMBUL
01115                                                   CMF-CODE-VALUE  ELTAMBUL
01116         PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT             ELTAMBUL
01117         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTAMBUL
01118         STRING CMF-DESCR-LINE (1) ' '                             ELTAMBUL
01119                CMF-DESCR-LINE (2)                                 ELTAMBUL
01120                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTAMBUL
01121         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTAMBUL
01122         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTAMBUL
01123         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTAMBUL
01124         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTAMBUL
01125         PERFORM TCPR-000-TEXT-UNSTRING                            ELTAMBUL
01126         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTAMBUL
01127         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
01128         IF WS-CIA  <  20                                          ELTAMBUL
01129            ADD +1  TO  WS-CIA                                     ELTAMBUL
01130            MOVE ZERO  TO                                          ELTAMBUL
01131                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTAMBUL
01132         ELSE                                                      ELTAMBUL
01133            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTAMBUL
01134                COMMAREA(DFHCOMMAREA)                              ELTAMBUL
01135            END-EXEC                                               ELTAMBUL
01136            MOVE +1  TO  WS-CIA                                    ELTAMBUL
01137            MOVE ZERO  TO                                          ELTAMBUL
01138                        PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).       ELTAMBUL
01139                                                                   ELTAMBUL
01140  2090-PROBLEM-WITH-INDICES.                                       ELTAMBUL
01141                                                                   ELTAMBUL
01142      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTAMBUL
01143      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTAMBUL
01144                                                                   ELTAMBUL
01145      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
01146      MOVE 'P'  TO  COF-FUNCTION.                                  ELTAMBUL
01147                                                                   ELTAMBUL
01148      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
01149                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01150      END-EXEC.                                                    ELTAMBUL
01151                                                                   ELTAMBUL
01152  2099-EXIT.            EXIT.                                      ELTAMBUL
01153                                                                   ELTAMBUL
01154  2000-EXIT.  EXIT.                                                ELTAMBUL
01155 /        O U T P U T  F O R  C O M M O N  L I N E S               ELTAMBUL
01156  3000-OUTPUT-TEXT SECTION.                                        ELTAMBUL
01157      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTAMBUL
01158      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTAMBUL
01159      MOVE ' '  TO  COF-FUNCTION.                                  ELTAMBUL
01160                                                                   ELTAMBUL
01161      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTAMBUL
01162                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01163      END-EXEC.                                                    ELTAMBUL
01164      MOVE +1   TO WS-CIA.                                         ELTAMBUL
01165  3000-EXIT.  EXIT.                                                ELTAMBUL
01166 /                                                                 ELTAMBUL
01167  4000-PLACE-OF-TREATMENT SECTION.                                 ELTAMBUL
01168      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTAMBUL
01169      MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.    ELTAMBUL
01170      MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01171                                               TO  CMF-CODE-VALUE. ELTAMBUL
01172      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01173      MOVE SPACES           TO  TCAR-FROM-AREA.                    ELTAMBUL
01174      STRING                                                       ELTAMBUL
01175             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01176             CMF-DESCR-LINE (2)                                    ELTAMBUL
01177              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTAMBUL
01178      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01179  4000-EXIT.  EXIT.                                                ELTAMBUL
01180 /                                                                 ELTAMBUL
01181  4100-PAYABLE-AS-BASIC SECTION.                                   ELTAMBUL
01182      MOVE WS-YES               TO  WS-SERVICES-PAYBLE-SW.         ELTAMBUL
01183      MOVE LOW-VALUES           TO  COF-DTL-LINE(WS-CIA).          ELTAMBUL
01184      ADD  +1                   TO  WS-CIA.                        ELTAMBUL
01185      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTAMBUL
01186      ADD  +1                   TO  WS-CIA.                        ELTAMBUL
01187                                                                   ELTAMBUL
01188      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTAMBUL
01189          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01190          MOVE 1                   TO TCAR-OUTPUT-FIELDS-USED      ELTAMBUL
01191          GO TO 4100-OUTPUT-TEXT.                                  ELTAMBUL
01192      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTAMBUL
01193      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTAMBUL
01194      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTAMBUL
01195                                                CMF-CODE-VALUE     ELTAMBUL
01196      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01197      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTAMBUL
01198      STRING CMF-DESCR-LINE(1) ' '                                 ELTAMBUL
01199             CMF-DESCR-LINE(2)                                     ELTAMBUL
01200                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTAMBUL
01201      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01202      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTAMBUL
01203      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTAMBUL
01204      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTAMBUL
01205      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01206      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01207                                         =  ZEROS                  ELTAMBUL
01208       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTAMBUL
01209                                         =  ZEROS                  ELTAMBUL
01210                 MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-BASIC      ELTAMBUL
01211                 MOVE WS-BASIC               TO                    ELTAMBUL
01212                         COF-DTL-LINE(WS-CIA)                      ELTAMBUL
01213       ELSE                                                        ELTAMBUL
01214          MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                       ELTAMBUL
01215          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTAMBUL
01216                                        TO  WS-DTL-PERCENT         ELTAMBUL
01217          MOVE SPACES          TO TCAR-FROM-AREA                   ELTAMBUL
01218          STRING WS-DTL-PP,                                        ELTAMBUL
01219                 WS-DTL-PERCENT,                                   ELTAMBUL
01220                 WS-PERCENT,                                       ELTAMBUL
01221                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTAMBUL
01222          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTAMBUL
01223          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
01224          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
01225          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
01226          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
01227          MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                ELTAMBUL
01228          MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)            ELTAMBUL
01229      ELSE                                                         ELTAMBUL
01230        MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP                         ELTAMBUL
01231        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTAMBUL
01232                                     TO WS-DTL-PERCENT             ELTAMBUL
01233        MOVE SPACES          TO TCAR-FROM-AREA                     ELTAMBUL
01234        STRING WS-DTL-PP,                                          ELTAMBUL
01235               WS-DTL-PERCENT,                                     ELTAMBUL
01236               WS-PERCENT,                                         ELTAMBUL
01237                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTAMBUL
01238        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTAMBUL
01239        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
01240        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01241        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
01242        PERFORM TCPR-000-TEXT-UNSTRING                             ELTAMBUL
01243        MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                  ELTAMBUL
01244        MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).             ELTAMBUL
01245  4100-OUTPUT-TEXT.                                                ELTAMBUL
01246      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTAMBUL
01247            ADD +1                TO WS-CIA                        ELTAMBUL
01248            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTAMBUL
01249            PERFORM 3000-OUTPUT-TEXT                               ELTAMBUL
01250      ELSE                                                         ELTAMBUL
01251         PERFORM 3000-OUTPUT-TEXT.                                 ELTAMBUL
01252  4100-EXIT.  EXIT.                                                ELTAMBUL
01253 /                                                                 ELTAMBUL
01254  4200-PAYABLE-AS-SUPP SECTION.                                    ELTAMBUL
01255      IF WS-SERVICES-PAYBLE-SW NOT EQUAL WS-YES                    ELTAMBUL
01256        MOVE LOW-VALUES TO  COF-DTL-LINE(WS-CIA)                   ELTAMBUL
01257        ADD  +1         TO  WS-CIA                                 ELTAMBUL
01258        MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA)         ELTAMBUL
01259        ADD  +1                   TO  WS-CIA.                      ELTAMBUL
01260                                                                   ELTAMBUL
01261      IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2) = ZEROS   ELTAMBUL
01262          MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01263          MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED     ELTAMBUL
01264          GO TO 4200-OUTPUT-TEXT.                                  ELTAMBUL
01265      MOVE 'BP'                  TO  CMF-RECORD-PREFIX             ELTAMBUL
01266      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME       ELTAMBUL
01267      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTAMBUL
01268                                                CMF-CODE-VALUE     ELTAMBUL
01269      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01270      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTAMBUL
01271      STRING CMF-DESCR-LINE(1) ' '                                 ELTAMBUL
01272             CMF-DESCR-LINE(2)                                     ELTAMBUL
01273                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTAMBUL
01274      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01275      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTAMBUL
01276      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTAMBUL
01277      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTAMBUL
01278      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01279      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01280                                         =  ZEROS                  ELTAMBUL
01281       IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)     ELTAMBUL
01282                                         =  ZEROS                  ELTAMBUL
01283                MOVE TCAR-OPF-DATA(1)       TO  WS-DTL-SUPPLEMENTALELTAMBUL
01284                MOVE WS-SUPPLEMENTAL        TO                     ELTAMBUL
01285                         COF-DTL-LINE(WS-CIA)                      ELTAMBUL
01286       ELSE                                                        ELTAMBUL
01287          MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                 ELTAMBUL
01288          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTAMBUL
01289                                        TO  WS-DTL-PERCENT         ELTAMBUL
01290          MOVE SPACES          TO TCAR-FROM-AREA                   ELTAMBUL
01291          STRING WS-DTL-PP,                                        ELTAMBUL
01292                 WS-DTL-PERCENT,                                   ELTAMBUL
01293                 WS-PERCENT,                                       ELTAMBUL
01294                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTAMBUL
01295          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTAMBUL
01296          MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT          ELTAMBUL
01297          MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN          ELTAMBUL
01298          MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN          ELTAMBUL
01299          PERFORM TCPR-000-TEXT-UNSTRING                           ELTAMBUL
01300          MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                 ELTAMBUL
01301          MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)    ELTAMBUL
01302      ELSE                                                         ELTAMBUL
01303        MOVE TCAR-OPF-DATA(1)       TO WS-DTL-PP                   ELTAMBUL
01304        MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTAMBUL
01305                                    TO WS-DTL-PERCENT              ELTAMBUL
01306        MOVE SPACES          TO TCAR-FROM-AREA                     ELTAMBUL
01307        STRING WS-DTL-PP,                                          ELTAMBUL
01308               WS-DTL-PERCENT,                                     ELTAMBUL
01309               WS-PERCENT,                                         ELTAMBUL
01310                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTAMBUL
01311        PERFORM TCPR-000-TEXT-COMPRESSION                          ELTAMBUL
01312        MOVE +03             TO TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
01313        MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01314        MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
01315        PERFORM TCPR-000-TEXT-UNSTRING                             ELTAMBUL
01316        MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                   ELTAMBUL
01317        MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).     ELTAMBUL
01318  4200-OUTPUT-TEXT.                                                ELTAMBUL
01319      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTAMBUL
01320            ADD +1                TO WS-CIA                        ELTAMBUL
01321            MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)          ELTAMBUL
01322            PERFORM 3000-OUTPUT-TEXT                               ELTAMBUL
01323      ELSE                                                         ELTAMBUL
01324         PERFORM 3000-OUTPUT-TEXT.                                 ELTAMBUL
01325  4200-EXIT.  EXIT.                                                ELTAMBUL
01326 /                                                                 ELTAMBUL
01327  4300-SPILLOVER-COINS SECTION.                                    ELTAMBUL
01328      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01329            = '0'   OR LOW-VALUES                                  ELTAMBUL
01330           GO TO 4300-EXIT.                                        ELTAMBUL
01331      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTAMBUL
01332      ADD +1     TO  WS-CIA.                                       ELTAMBUL
01333      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTAMBUL
01334      MOVE 'SPILL-OVER-COINS-APL-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.ELTAMBUL
01335      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTAMBUL
01336                       TO CMF-CODE-VALUE.                          ELTAMBUL
01337      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01338      MOVE SPACES           TO  TCAR-FROM-AREA.                    ELTAMBUL
01339      STRING WS-SPILLOVER-COINS                                    ELTAMBUL
01340             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01341             CMF-DESCR-LINE (2)                                    ELTAMBUL
01342              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTAMBUL
01343      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01344      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTAMBUL
01345      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTAMBUL
01346      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTAMBUL
01347      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01348      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTAMBUL
01349      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTAMBUL
01350            ADD +1                TO  WS-CIA                       ELTAMBUL
01351            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTAMBUL
01352      PERFORM 3000-OUTPUT-TEXT.                                    ELTAMBUL
01353  4300-EXIT.  EXIT.                                                ELTAMBUL
01354 /                                                                 ELTAMBUL
01355  4400-SPILLOVER-DEDUCT SECTION.                                   ELTAMBUL
01356      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01357            = '0'   OR LOW-VALUES                                  ELTAMBUL
01358           GO TO 4400-EXIT.                                        ELTAMBUL
01359      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTAMBUL
01360      ADD +1     TO  WS-CIA.                                       ELTAMBUL
01361      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTAMBUL
01362      MOVE 'SPILL-OVER-DED-APL-IND'   TO  CMF-ELEMENT-SYSTEM-NAME. ELTAMBUL
01363      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTAMBUL
01364                       TO CMF-CODE-VALUE                           ELTAMBUL
01365      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01366      MOVE SPACES           TO  TCAR-FROM-AREA.                    ELTAMBUL
01367      STRING WS-SPILLOVER-DEDBL                                    ELTAMBUL
01368             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01369             CMF-DESCR-LINE (2)                                    ELTAMBUL
01370              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTAMBUL
01371      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01372      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTAMBUL
01373      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTAMBUL
01374      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTAMBUL
01375      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01376      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTAMBUL
01377      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTAMBUL
01378            ADD +1                TO  WS-CIA                       ELTAMBUL
01379            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTAMBUL
01380      PERFORM 3000-OUTPUT-TEXT.                                    ELTAMBUL
01381  4400-EXIT.  EXIT.                                                ELTAMBUL
01382 /                                                                 ELTAMBUL
01383  4500-MAX-VISITS SECTION.                                         ELTAMBUL
01384      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTAMBUL
01385      MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)  TO      ELTAMBUL
01386                                                 CMF-CODE-VALUE    ELTAMBUL
01387      MOVE 'BPE'                TO  CMF-RECORD-PREFIX.             ELTAMBUL
01388      MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME.       ELTAMBUL
01389      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
01390      MOVE SPACES           TO  TCAR-FROM-AREA.                    ELTAMBUL
01391      STRING CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01392             CMF-DESCR-LINE (2)                                    ELTAMBUL
01393              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTAMBUL
01394      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01395      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTAMBUL
01396      MOVE +63              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTAMBUL
01397      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTAMBUL
01398      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01399  4500-EXIT.  EXIT.                                                ELTAMBUL
01400 /                                                                 ELTAMBUL
01401  4650-SCAN-TAB SECTION.                                           ELTAMBUL
01402                                                                   ELTAMBUL
01403      PERFORM 4700-BEN-TAB-AAR.                                    ELTAMBUL
01404      PERFORM 4800-BEN-TAB-PPF.                                    ELTAMBUL
01405      PERFORM 4900-BEN-TAB-PVE.                                    ELTAMBUL
01406      PERFORM 5000-BEN-TAB-ADL.                                    ELTAMBUL
01407      PERFORM 5100-BEN-TAB-ABM.                                    ELTAMBUL
01408      PERFORM 5200-BEN-TAB-ACL.                                    ELTAMBUL
01409      PERFORM 5300-BEN-TAB-AOL.                                    ELTAMBUL
01410                                                                   ELTAMBUL
01411  4650-EXIT.  EXIT.                                                ELTAMBUL
01412 /                                                                 ELTAMBUL
01413  4675-PAY-CONSID-TEXT SECTION.                                    ELTAMBUL
01414      INITIALIZE TCAR-FROM-AREA.                                   ELTAMBUL
01415      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTAMBUL
01416             WS-PAY-CONSDR-TEXT2                                   ELTAMBUL
01417                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTAMBUL
01418      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
01419      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTAMBUL
01420      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01421                                TCAR-OUTPUT-FIELD-2-LEN.           ELTAMBUL
01422      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
01423      IF WS-CIA > 17                                               ELTAMBUL
01424            PERFORM 3000-OUTPUT-TEXT.                              ELTAMBUL
01425      ADD +1                TO  WS-CIA.                            ELTAMBUL
01426      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTAMBUL
01427      ADD +1                TO  WS-CIA.                            ELTAMBUL
01428      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTAMBUL
01429      PERFORM 3000-OUTPUT-TEXT.                                    ELTAMBUL
01430  4675-EXIT.   EXIT.                                               ELTAMBUL
01431 /                                                                 ELTAMBUL
01432  4700-BEN-TAB-AAR SECTION.                                        ELTAMBUL
01433      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTAMBUL
01434      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01435                                                                   ELTAMBUL
01436      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01437          NOT = LOW-VALUES                                         ELTAMBUL
01438       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01439          NOT = SPACE                                              ELTAMBUL
01440                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTAMBUL
01441                                                                   ELTAMBUL
01442      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01443                                                                   ELTAMBUL
01444      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01445          NOT = LOW-VALUES                                         ELTAMBUL
01446       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01447          NOT = SPACE                                              ELTAMBUL
01448                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTAMBUL
01449                                                                   ELTAMBUL
01450      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTAMBUL
01451             MOVE +1                  TO WS-CIA                    ELTAMBUL
01452             MOVE LOW-VALUES          TO COF-DTL-LINE(WS-CIA)      ELTAMBUL
01453             ADD  +1                  TO WS-CIA                    ELTAMBUL
01454             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTAMBUL
01455             PERFORM 3000-OUTPUT-TEXT.                             ELTAMBUL
01456  4700-EXIT.  EXIT.                                                ELTAMBUL
01457 /                                                                 ELTAMBUL
01458  4800-BEN-TAB-PPF SECTION.                                        ELTAMBUL
01459      MOVE ZEROS   TO  WS-HOLD1,                                   ELTAMBUL
01460                       WS-HOLD2.                                   ELTAMBUL
01461      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01462      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01463          NOT = LOW-VALUES                                         ELTAMBUL
01464       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01465          NOT = SPACE                                              ELTAMBUL
01466             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01467                        TO  WS-HOLD1.                              ELTAMBUL
01468                                                                   ELTAMBUL
01469      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01470      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01471          NOT = LOW-VALUES                                         ELTAMBUL
01472       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01473          NOT = SPACE                                              ELTAMBUL
01474             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01475                        TO  WS-HOLD2.                              ELTAMBUL
01476                                                                   ELTAMBUL
01477      IF WS-HOLD1 = WS-HOLD2                                       ELTAMBUL
01478         IF WS-HOLD1 = ZEROS                                       ELTAMBUL
01479                 GO TO 4800-EXIT                                   ELTAMBUL
01480         ELSE                                                      ELTAMBUL
01481             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTAMBUL
01482             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01483             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTAMBUL
01484                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01485             END-EXEC                                              ELTAMBUL
01486             GO TO 4800-EXIT.                                      ELTAMBUL
01487                                                                   ELTAMBUL
01488      IF WS-HOLD1 = ZEROS                                          ELTAMBUL
01489             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01490             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01491             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTAMBUL
01492                              COMMAREA(DFHCOMMAREA)                ELTAMBUL
01493             END-EXEC                                              ELTAMBUL
01494      ELSE                                                         ELTAMBUL
01495       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTAMBUL
01496       PERFORM 5900-GET-TAB-REC                                    ELTAMBUL
01497       EXEC CICS  LINK PROGRAM('ELGPPF')                           ELTAMBUL
01498                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01499       END-EXEC                                                    ELTAMBUL
01500       IF WS-HOLD2 = ZEROS                                         ELTAMBUL
01501            GO TO 4800-EXIT                                        ELTAMBUL
01502       ELSE                                                        ELTAMBUL
01503             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01504             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01505             EXEC CICS  LINK PROGRAM('ELGPPF')                     ELTAMBUL
01506                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01507             END-EXEC.                                             ELTAMBUL
01508  4800-EXIT.    EXIT.                                              ELTAMBUL
01509 /                                                                 ELTAMBUL
01510  4900-BEN-TAB-PVE SECTION.                                        ELTAMBUL
01511      MOVE +1 TO WS-CIA.                                           ELTAMBUL
01512      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTAMBUL
01513      ADD  +1         TO WS-CIA.                                   ELTAMBUL
01514      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTAMBUL
01515      PERFORM 3000-OUTPUT-TEXT.                                    ELTAMBUL
01516  4900-EXIT.  EXIT.                                                ELTAMBUL
01517 /                                                                 ELTAMBUL
01518  5000-BEN-TAB-ADL SECTION.                                        ELTAMBUL
01519      MOVE ZEROS   TO  WS-HOLD1,                                   ELTAMBUL
01520                       WS-HOLD2.                                   ELTAMBUL
01521      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01522      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01523          NOT = LOW-VALUES                                         ELTAMBUL
01524       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01525          NOT = SPACE                                              ELTAMBUL
01526             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01527                        TO  WS-HOLD1.                              ELTAMBUL
01528                                                                   ELTAMBUL
01529      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01530      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01531          NOT = LOW-VALUES                                         ELTAMBUL
01532       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01533          NOT = SPACE                                              ELTAMBUL
01534             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01535                        TO  WS-HOLD2.                              ELTAMBUL
01536                                                                   ELTAMBUL
01537      IF WS-HOLD1 = WS-HOLD2                                       ELTAMBUL
01538         IF WS-HOLD1 = ZEROS                                       ELTAMBUL
01539                 GO TO 5000-EXIT                                   ELTAMBUL
01540         ELSE                                                      ELTAMBUL
01541             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTAMBUL
01542             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01543             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTAMBUL
01544                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01545             END-EXEC                                              ELTAMBUL
01546             GO TO 5000-EXIT.                                      ELTAMBUL
01547                                                                   ELTAMBUL
01548      IF WS-HOLD1 = ZEROS                                          ELTAMBUL
01549             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01550             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01551             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTAMBUL
01552                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01553             END-EXEC                                              ELTAMBUL
01554      ELSE                                                         ELTAMBUL
01555       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTAMBUL
01556       PERFORM 5900-GET-TAB-REC                                    ELTAMBUL
01557       EXEC CICS  LINK PROGRAM('ELGDEDBL')                         ELTAMBUL
01558                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01559       END-EXEC                                                    ELTAMBUL
01560       IF WS-HOLD2 = ZEROS                                         ELTAMBUL
01561           GO TO 5000-EXIT                                         ELTAMBUL
01562       ELSE                                                        ELTAMBUL
01563             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01564             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01565             EXEC CICS  LINK PROGRAM('ELGDEDBL')                   ELTAMBUL
01566                          COMMAREA(DFHCOMMAREA)                    ELTAMBUL
01567             END-EXEC.                                             ELTAMBUL
01568  5000-EXIT.     EXIT.                                             ELTAMBUL
01569 /                                                                 ELTAMBUL
01570  5100-BEN-TAB-ABM SECTION.                                        ELTAMBUL
01571      MOVE ZEROS   TO  WS-HOLD1,                                   ELTAMBUL
01572                       WS-HOLD2.                                   ELTAMBUL
01573      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01574      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01575          NOT = LOW-VALUES                                         ELTAMBUL
01576       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01577          NOT = SPACE                                              ELTAMBUL
01578             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01579                        TO  WS-HOLD1.                              ELTAMBUL
01580                                                                   ELTAMBUL
01581      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01582      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01583          NOT = LOW-VALUES                                         ELTAMBUL
01584       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01585          NOT = SPACE                                              ELTAMBUL
01586             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01587                        TO  WS-HOLD2.                              ELTAMBUL
01588                                                                   ELTAMBUL
01589      IF WS-HOLD1 = WS-HOLD2                                       ELTAMBUL
01590         IF WS-HOLD1 = ZEROS                                       ELTAMBUL
01591                 GO TO 5100-EXIT                                   ELTAMBUL
01592         ELSE                                                      ELTAMBUL
01593             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTAMBUL
01594             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01595             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTAMBUL
01596                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01597             END-EXEC                                              ELTAMBUL
01598             GO TO 5100-EXIT.                                      ELTAMBUL
01599                                                                   ELTAMBUL
01600      IF WS-HOLD1 = ZEROS                                          ELTAMBUL
01601             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01602             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01603             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTAMBUL
01604                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01605             END-EXEC                                              ELTAMBUL
01606      ELSE                                                         ELTAMBUL
01607       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTAMBUL
01608       PERFORM 5900-GET-TAB-REC                                    ELTAMBUL
01609       EXEC CICS  LINK PROGRAM('ELGMAXIM')                         ELTAMBUL
01610                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01611       END-EXEC                                                    ELTAMBUL
01612       IF WS-HOLD2 = ZEROS                                         ELTAMBUL
01613            GO TO 5100-EXIT                                        ELTAMBUL
01614       ELSE                                                        ELTAMBUL
01615             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01616             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01617             EXEC CICS  LINK PROGRAM('ELGMAXIM')                   ELTAMBUL
01618                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01619             END-EXEC.                                             ELTAMBUL
01620  5100-EXIT.     EXIT.                                             ELTAMBUL
01621 /                                                                 ELTAMBUL
01622  5200-BEN-TAB-ACL SECTION.                                        ELTAMBUL
01623      MOVE ZEROS   TO  WS-HOLD1,                                   ELTAMBUL
01624                       WS-HOLD2.                                   ELTAMBUL
01625      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01626      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01627          NOT = LOW-VALUES                                         ELTAMBUL
01628       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01629          NOT = SPACE                                              ELTAMBUL
01630             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01631                        TO  WS-HOLD1.                              ELTAMBUL
01632                                                                   ELTAMBUL
01633      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01634      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01635          NOT = LOW-VALUES                                         ELTAMBUL
01636       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01637          NOT = SPACE                                              ELTAMBUL
01638             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01639                        TO  WS-HOLD2.                              ELTAMBUL
01640                                                                   ELTAMBUL
01641      IF WS-HOLD1 = WS-HOLD2                                       ELTAMBUL
01642         IF WS-HOLD1 = ZEROS                                       ELTAMBUL
01643                 GO TO 5200-EXIT                                   ELTAMBUL
01644         ELSE                                                      ELTAMBUL
01645             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTAMBUL
01646             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01647             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTAMBUL
01648                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01649             END-EXEC                                              ELTAMBUL
01650             GO TO 5200-EXIT.                                      ELTAMBUL
01651                                                                   ELTAMBUL
01652      IF WS-HOLD1 = ZEROS                                          ELTAMBUL
01653             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01654             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01655             EXEC CICS  LINK PROGRAM('ELGCOINS')                   ELTAMBUL
01656                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01657             END-EXEC                                              ELTAMBUL
01658      ELSE                                                         ELTAMBUL
01659       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTAMBUL
01660       PERFORM 5900-GET-TAB-REC                                    ELTAMBUL
01661       EXEC CICS  LINK PROGRAM('ELGCOINS')                         ELTAMBUL
01662                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01663       END-EXEC                                                    ELTAMBUL
01664       IF WS-HOLD2 = ZEROS                                         ELTAMBUL
01665          GO TO 5200-EXIT                                          ELTAMBUL
01666       ELSE                                                        ELTAMBUL
01667           MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                      ELTAMBUL
01668           PERFORM 5900-GET-TAB-REC                                ELTAMBUL
01669           EXEC CICS  LINK PROGRAM('ELGCOINS')                     ELTAMBUL
01670                           COMMAREA(DFHCOMMAREA)                   ELTAMBUL
01671           END-EXEC.                                               ELTAMBUL
01672  5200-EXIT.     EXIT.                                             ELTAMBUL
01673 /                                                                 ELTAMBUL
01674  5300-BEN-TAB-AOL SECTION.                                        ELTAMBUL
01675      MOVE ZEROS   TO  WS-HOLD1,                                   ELTAMBUL
01676                       WS-HOLD2.                                   ELTAMBUL
01677      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01678      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01679          NOT = LOW-VALUES                                         ELTAMBUL
01680       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01681          NOT = SPACE                                              ELTAMBUL
01682             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01683                        TO  WS-HOLD1.                              ELTAMBUL
01684                                                                   ELTAMBUL
01685      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01686      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01687          NOT = LOW-VALUES                                         ELTAMBUL
01688       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTAMBUL
01689          NOT = SPACE                                              ELTAMBUL
01690             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTAMBUL
01691                        TO  WS-HOLD2.                              ELTAMBUL
01692                                                                   ELTAMBUL
01693      IF WS-HOLD1 = WS-HOLD2                                       ELTAMBUL
01694         IF WS-HOLD1 = ZEROS                                       ELTAMBUL
01695                 GO TO 5300-EXIT                                   ELTAMBUL
01696         ELSE                                                      ELTAMBUL
01697             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTAMBUL
01698             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01699             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTAMBUL
01700                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01701             END-EXEC                                              ELTAMBUL
01702             GO TO 5300-EXIT.                                      ELTAMBUL
01703                                                                   ELTAMBUL
01704      IF WS-HOLD1 = ZEROS                                          ELTAMBUL
01705             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTAMBUL
01706             PERFORM 5900-GET-TAB-REC                              ELTAMBUL
01707             EXEC CICS  LINK PROGRAM('ELGOUTPX')                   ELTAMBUL
01708                             COMMAREA(DFHCOMMAREA)                 ELTAMBUL
01709             END-EXEC                                              ELTAMBUL
01710      ELSE                                                         ELTAMBUL
01711       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTAMBUL
01712       PERFORM 5900-GET-TAB-REC                                    ELTAMBUL
01713       EXEC CICS  LINK PROGRAM('ELGOUTPX')                         ELTAMBUL
01714                       COMMAREA(DFHCOMMAREA)                       ELTAMBUL
01715       END-EXEC                                                    ELTAMBUL
01716       IF WS-HOLD2 = ZEROS                                         ELTAMBUL
01717           GO TO 5300-EXIT                                         ELTAMBUL
01718       ELSE                                                        ELTAMBUL
01719          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTAMBUL
01720          PERFORM 5900-GET-TAB-REC                                 ELTAMBUL
01721          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTAMBUL
01722                          COMMAREA(DFHCOMMAREA)                    ELTAMBUL
01723          END-EXEC.                                                ELTAMBUL
01724  5300-EXIT.     EXIT.                                             ELTAMBUL
01725 /                                                                 ELTAMBUL
01726  5900-GET-TAB-REC SECTION.                                        ELTAMBUL
01727                                                                   ELTAMBUL
01728      SET CIA-GCTABULR-DDN TO TRUE.                                ELTAMBUL
01729      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
01730          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTAMBUL
01731      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTAMBUL
01732      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTAMBUL
01733      SET IOP-RD                          TO TRUE.                 ELTAMBUL
01734      SET IOP-FCQ-NONE                    TO TRUE.                 ELTAMBUL
01735      SET IOP-KVQ-NONE                    TO TRUE.                 ELTAMBUL
01736                                                                   ELTAMBUL
01737      EXEC CICS LINK PROGRAM ('ELUIOPGM')                          ELTAMBUL
01738                     COMMAREA (DFHCOMMAREA)                        ELTAMBUL
01739      END-EXEC.                                                    ELTAMBUL
01740                                                                   ELTAMBUL
01741      IF IOP-RC-NOTFND                                             ELTAMBUL
01742         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTAMBUL
01743         EXEC CICS ABEND                                           ELTAMBUL
01744                   ABCODE(CIA-ABCODE)                              ELTAMBUL
01745         END-EXEC.                                                 ELTAMBUL
01746                                                                   ELTAMBUL
01747      IF NOT IOP-RC-OK                                             ELTAMBUL
01748         SET CIA-AB-CRITIO          TO TRUE                        ELTAMBUL
01749         EXEC CICS ABEND                                           ELTAMBUL
01750                   ABCODE(CIA-ABCODE)                              ELTAMBUL
01751         END-EXEC.                                                 ELTAMBUL
01752                                                                   ELTAMBUL
01753  5900-EXIT.                                                       ELTAMBUL
01754 /                                                                 ELTAMBUL
01755  6000-AMBUL-SERVC-COVERAGE SECTION.                               ELTAMBUL
01756      MOVE '6000' TO WS-PARA-ID.                                   ELTAMBUL
01757      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
01758         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
01759        GO TO 6099-EXIT.                                           ELTAMBUL
01760      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01761      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTAMBUL
01762                     WS-DTL-BASIC-1-2                              ELTAMBUL
01763                     WS-DTL-BASIC-2-1                              ELTAMBUL
01764                     WS-DTL-BASIC-2-2                              ELTAMBUL
01765                     WS-DTL-BASIC-3-1                              ELTAMBUL
01766                     WS-DTL-BASIC-3-2                              ELTAMBUL
01767                     WS-DTL-BASIC-4-1                              ELTAMBUL
01768                     WS-DTL-BASIC-4-2                              ELTAMBUL
01769                     WS-DTL-SUPP-1-1                               ELTAMBUL
01770                     WS-DTL-SUPP-1-2                               ELTAMBUL
01771                     WS-DTL-SUPP-2-1                               ELTAMBUL
01772                     WS-DTL-SUPP-2-2                               ELTAMBUL
01773                     WS-DTL-SUPP-3-1                               ELTAMBUL
01774                     WS-DTL-SUPP-3-2                               ELTAMBUL
01775                     WS-DTL-SUPP-4-1                               ELTAMBUL
01776                     WS-DTL-SUPP-4-2.                              ELTAMBUL
01777      IF PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
01778                                              NOT = '0'            ELTAMBUL
01779        IF PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01780                                         NOT = LOW-VALUES          ELTAMBUL
01781          MOVE                                                     ELTAMBUL
01782           PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01783                                    TO  CMF-CODE-VALUE             ELTAMBUL
01784          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTAMBUL
01785          MOVE 'AMBULANCE-ELIG-IND'                                ELTAMBUL
01786                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
01787          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
01788          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
01789          STRING                                                   ELTAMBUL
01790             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01791             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
01792             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
01793             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
01794             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
01795             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
01796             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
01797             CMF-DESCR-LINE (8)                                    ELTAMBUL
01798              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
01799             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
01800             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
01801             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01802             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
01803             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
01804             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
01805             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
01806             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
01807             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
01808             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
01809             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
01810            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTAMBUL
01811            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTAMBUL
01812            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTAMBUL
01813            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTAMBUL
01814            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTAMBUL
01815            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTAMBUL
01816            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTAMBUL
01817            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTAMBUL
01818                                                                   ELTAMBUL
01819      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01820      IF PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
01821                                              NOT = '00'           ELTAMBUL
01822        IF PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01823                                         NOT = LOW-VALUES          ELTAMBUL
01824          MOVE                                                     ELTAMBUL
01825           PLE-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
01826                                    TO  CMF-CODE-VALUE             ELTAMBUL
01827          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTAMBUL
01828          MOVE 'AMBULANCE-ELIG-IND'                                ELTAMBUL
01829                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
01830          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
01831          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
01832          STRING                                                   ELTAMBUL
01833             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01834             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
01835             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
01836             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
01837             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
01838             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
01839             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
01840             CMF-DESCR-LINE (8)                                    ELTAMBUL
01841              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
01842             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
01843             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
01844             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01845             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
01846             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
01847             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
01848             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
01849             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
01850             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
01851             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
01852             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
01853            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTAMBUL
01854            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTAMBUL
01855            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTAMBUL
01856            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTAMBUL
01857            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTAMBUL
01858            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTAMBUL
01859            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTAMBUL
01860            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTAMBUL
01861                                                                   ELTAMBUL
01862                                                                   ELTAMBUL
01863      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTAMBUL
01864             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTAMBUL
01865                  ADD +1                TO WS-CIA                  ELTAMBUL
01866                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTAMBUL
01867                  ADD +1                TO WS-CIA                  ELTAMBUL
01868             MOVE WS-TYPE-AMBUL-SERVC TO COF-DTL-LINE(WS-CIA).     ELTAMBUL
01869                                                                   ELTAMBUL
01870      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
01871                  ADD +1                TO WS-CIA                  ELTAMBUL
01872         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
01873         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTAMBUL
01874             ADD +1                TO  WS-CIA                      ELTAMBUL
01875             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01876         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTAMBUL
01877             ADD +1                TO  WS-CIA                      ELTAMBUL
01878             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01879         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTAMBUL
01880             ADD +1                TO  WS-CIA                      ELTAMBUL
01881             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01882         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTAMBUL
01883             ADD +1                TO  WS-CIA                      ELTAMBUL
01884             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01885         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTAMBUL
01886             ADD +1                TO  WS-CIA                      ELTAMBUL
01887             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01888         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTAMBUL
01889             ADD +1                TO  WS-CIA                      ELTAMBUL
01890             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01891         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTAMBUL
01892             ADD +1                TO  WS-CIA                      ELTAMBUL
01893             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
01894      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
01895           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
01896                                                                   ELTAMBUL
01897      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
01898         ADD +1                TO  WS-CIA                          ELTAMBUL
01899         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTAMBUL
01900         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTAMBUL
01901             ADD +1                TO  WS-CIA                      ELTAMBUL
01902             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01903                                                                   ELTAMBUL
01904         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTAMBUL
01905             ADD +1                TO  WS-CIA                      ELTAMBUL
01906             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01907                                                                   ELTAMBUL
01908         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTAMBUL
01909             ADD +1                TO  WS-CIA                      ELTAMBUL
01910             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01911                                                                   ELTAMBUL
01912         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTAMBUL
01913             ADD +1                TO  WS-CIA                      ELTAMBUL
01914             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01915                                                                   ELTAMBUL
01916         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTAMBUL
01917             ADD +1                TO  WS-CIA                      ELTAMBUL
01918             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01919                                                                   ELTAMBUL
01920         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTAMBUL
01921             ADD +1                TO  WS-CIA                      ELTAMBUL
01922             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
01923                                                                   ELTAMBUL
01924         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTAMBUL
01925             ADD +1                TO  WS-CIA                      ELTAMBUL
01926             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
01927                                                                   ELTAMBUL
01928      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
01929           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
01930  6099-EXIT.                                                       ELTAMBUL
01931      EXIT.                                                        ELTAMBUL
01932 /                                                                 ELTAMBUL
01933  6100-PROF-CERTIFICATION   SECTION.                               ELTAMBUL
01934      MOVE '6100' TO WS-PARA-ID.                                   ELTAMBUL
01935      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
01936      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTAMBUL
01937                     WS-DTL-BASIC-1-2                              ELTAMBUL
01938                     WS-DTL-BASIC-2-1                              ELTAMBUL
01939                     WS-DTL-BASIC-2-2                              ELTAMBUL
01940                     WS-DTL-BASIC-3-1                              ELTAMBUL
01941                     WS-DTL-BASIC-3-2                              ELTAMBUL
01942                     WS-DTL-BASIC-4-1                              ELTAMBUL
01943                     WS-DTL-BASIC-4-2                              ELTAMBUL
01944                     WS-DTL-SUPP-1-1                               ELTAMBUL
01945                     WS-DTL-SUPP-1-2                               ELTAMBUL
01946                     WS-DTL-SUPP-2-1                               ELTAMBUL
01947                     WS-DTL-SUPP-2-2                               ELTAMBUL
01948                     WS-DTL-SUPP-3-1                               ELTAMBUL
01949                     WS-DTL-SUPP-3-2                               ELTAMBUL
01950                     WS-DTL-SUPP-4-1                               ELTAMBUL
01951                     WS-DTL-SUPP-4-2.                              ELTAMBUL
01952      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTAMBUL
01953                                              NOT = ZEROES         ELTAMBUL
01954       IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)            ELTAMBUL
01955                                         NOT = SPACES              ELTAMBUL
01956        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
01957                                         NOT = LOW-VALUES          ELTAMBUL
01958          MOVE                                                     ELTAMBUL
01959           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
01960                                    TO  CMF-CODE-VALUE             ELTAMBUL
01961          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTAMBUL
01962          MOVE 'CERTFN-REQRM-IND'                                  ELTAMBUL
01963                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
01964          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
01965          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
01966          STRING                                                   ELTAMBUL
01967             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
01968             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
01969             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
01970             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
01971             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
01972             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
01973             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
01974             CMF-DESCR-LINE (8)                                    ELTAMBUL
01975              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
01976             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
01977             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
01978             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
01979             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
01980             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
01981             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
01982             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
01983             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
01984             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
01985             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
01986             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
01987            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTAMBUL
01988            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTAMBUL
01989            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTAMBUL
01990            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTAMBUL
01991            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTAMBUL
01992            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTAMBUL
01993            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTAMBUL
01994            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTAMBUL
01995                                                                   ELTAMBUL
01996      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
01997      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTAMBUL
01998                                              NOT = ZEROES         ELTAMBUL
01999        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02000                                         NOT = LOW-VALUE           ELTAMBUL
02001          MOVE                                                     ELTAMBUL
02002           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02003                                    TO  CMF-CODE-VALUE             ELTAMBUL
02004          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTAMBUL
02005          MOVE 'CERTFN-REQRM-IND'                                  ELTAMBUL
02006                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
02007          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
02008          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
02009          STRING                                                   ELTAMBUL
02010             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02011             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
02012             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
02013             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
02014             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
02015             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
02016             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
02017             CMF-DESCR-LINE (8)                                    ELTAMBUL
02018              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
02019             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
02020             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
02021             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
02022             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
02023             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
02024             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
02025             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
02026             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
02027             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
02028             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
02029             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
02030            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTAMBUL
02031            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTAMBUL
02032            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTAMBUL
02033            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTAMBUL
02034            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTAMBUL
02035            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTAMBUL
02036            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTAMBUL
02037            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTAMBUL
02038                                                                   ELTAMBUL
02039                                                                   ELTAMBUL
02040      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTAMBUL
02041             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTAMBUL
02042                  ADD +1                TO WS-CIA                  ELTAMBUL
02043                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTAMBUL
02044                  ADD +1                TO WS-CIA                  ELTAMBUL
02045             MOVE WS-CERTIFICATION    TO COF-DTL-LINE(WS-CIA).     ELTAMBUL
02046                                                                   ELTAMBUL
02047      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02048                  ADD +1                TO WS-CIA                  ELTAMBUL
02049         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
02050         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTAMBUL
02051             ADD +1                TO  WS-CIA                      ELTAMBUL
02052             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02053         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTAMBUL
02054             ADD +1                TO  WS-CIA                      ELTAMBUL
02055             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02056         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTAMBUL
02057             ADD +1                TO  WS-CIA                      ELTAMBUL
02058             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02059         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTAMBUL
02060             ADD +1                TO  WS-CIA                      ELTAMBUL
02061             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02062         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTAMBUL
02063             ADD +1                TO  WS-CIA                      ELTAMBUL
02064             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02065         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTAMBUL
02066             ADD +1                TO  WS-CIA                      ELTAMBUL
02067             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02068         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTAMBUL
02069             ADD +1                TO  WS-CIA                      ELTAMBUL
02070             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02071      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02072           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02073                                                                   ELTAMBUL
02074      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02075         ADD +1                TO  WS-CIA                          ELTAMBUL
02076         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTAMBUL
02077         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTAMBUL
02078             ADD +1                TO  WS-CIA                      ELTAMBUL
02079             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02080                                                                   ELTAMBUL
02081         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTAMBUL
02082             ADD +1                TO  WS-CIA                      ELTAMBUL
02083             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02084                                                                   ELTAMBUL
02085         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTAMBUL
02086             ADD +1                TO  WS-CIA                      ELTAMBUL
02087             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02088                                                                   ELTAMBUL
02089         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTAMBUL
02090             ADD +1                TO  WS-CIA                      ELTAMBUL
02091             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02092                                                                   ELTAMBUL
02093         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTAMBUL
02094             ADD +1                TO  WS-CIA                      ELTAMBUL
02095             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02096                                                                   ELTAMBUL
02097         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTAMBUL
02098             ADD +1                TO  WS-CIA                      ELTAMBUL
02099             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02100                                                                   ELTAMBUL
02101         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTAMBUL
02102             ADD +1                TO  WS-CIA                      ELTAMBUL
02103             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02104                                                                   ELTAMBUL
02105      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02106           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02107  6199-EXIT.                                                       ELTAMBUL
02108      EXIT.                                                        ELTAMBUL
02109 /                                                                 ELTAMBUL
02110  7000-AMBUL-SERVC-COVERAGE SECTION.                               ELTAMBUL
02111      MOVE '7000' TO WS-PARA-ID.                                   ELTAMBUL
02112      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS AND          ELTAMBUL
02113         PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) = ZEROS              ELTAMBUL
02114        GO TO 7099-EXIT.                                           ELTAMBUL
02115      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
02116      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTAMBUL
02117                     WS-DTL-BASIC-1-2                              ELTAMBUL
02118                     WS-DTL-BASIC-2-1                              ELTAMBUL
02119                     WS-DTL-BASIC-2-2                              ELTAMBUL
02120                     WS-DTL-BASIC-3-1                              ELTAMBUL
02121                     WS-DTL-BASIC-3-2                              ELTAMBUL
02122                     WS-DTL-BASIC-4-1                              ELTAMBUL
02123                     WS-DTL-BASIC-4-2                              ELTAMBUL
02124                     WS-DTL-SUPP-1-1                               ELTAMBUL
02125                     WS-DTL-SUPP-1-2                               ELTAMBUL
02126                     WS-DTL-SUPP-2-1                               ELTAMBUL
02127                     WS-DTL-SUPP-2-2                               ELTAMBUL
02128                     WS-DTL-SUPP-3-1                               ELTAMBUL
02129                     WS-DTL-SUPP-3-2                               ELTAMBUL
02130                     WS-DTL-SUPP-4-1                               ELTAMBUL
02131                     WS-DTL-SUPP-4-2.                              ELTAMBUL
02132      IF PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02133                                              NOT = '00'           ELTAMBUL
02134       IF PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)          ELTAMBUL
02135                                         NOT = SPACES              ELTAMBUL
02136        IF PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
02137                                         NOT = LOW-VALUES          ELTAMBUL
02138          MOVE                                                     ELTAMBUL
02139           PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
02140                                    TO  CMF-CODE-VALUE             ELTAMBUL
02141          MOVE 'BPB'                TO  CMF-RECORD-PREFIX          ELTAMBUL
02142          MOVE 'AMBULANCE-ELIG-IND'                                ELTAMBUL
02143                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
02144          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
02145          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
02146          STRING                                                   ELTAMBUL
02147             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02148             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
02149             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
02150             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
02151             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
02152             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
02153             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
02154             CMF-DESCR-LINE (8)                                    ELTAMBUL
02155              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
02156             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
02157             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
02158             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
02159             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
02160             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
02161             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
02162             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
02163             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
02164             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
02165             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
02166             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
02167            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTAMBUL
02168            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTAMBUL
02169            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTAMBUL
02170            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTAMBUL
02171            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTAMBUL
02172            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTAMBUL
02173            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTAMBUL
02174            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTAMBUL
02175                                                                   ELTAMBUL
02176      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
02177      IF PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02178                                              NOT = '0'            ELTAMBUL
02179        IF PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
02180                                         NOT = LOW-VALUES          ELTAMBUL
02181          MOVE                                                     ELTAMBUL
02182           PLB-AMBULANCE-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTAMBUL
02183                                    TO  CMF-CODE-VALUE             ELTAMBUL
02184          MOVE 'BPB'                TO  CMF-RECORD-PREFIX          ELTAMBUL
02185          MOVE 'AMBULANCE-ELIG-IND'                                ELTAMBUL
02186                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
02187          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
02188          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
02189          STRING                                                   ELTAMBUL
02190             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02191             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
02192             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
02193             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
02194             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
02195             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
02196             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
02197             CMF-DESCR-LINE (8)                                    ELTAMBUL
02198              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
02199             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
02200             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
02201             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
02202             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
02203             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
02204             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
02205             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
02206             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
02207             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
02208             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
02209             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
02210            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTAMBUL
02211            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTAMBUL
02212            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTAMBUL
02213            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTAMBUL
02214            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTAMBUL
02215            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTAMBUL
02216            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTAMBUL
02217            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTAMBUL
02218                                                                   ELTAMBUL
02219                                                                   ELTAMBUL
02220      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTAMBUL
02221             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTAMBUL
02222                  ADD +1                TO WS-CIA                  ELTAMBUL
02223                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTAMBUL
02224                  ADD +1                TO WS-CIA                  ELTAMBUL
02225             MOVE WS-TYPE-AMBUL-SERVC TO COF-DTL-LINE(WS-CIA).     ELTAMBUL
02226                                                                   ELTAMBUL
02227      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02228                  ADD +1                TO WS-CIA                  ELTAMBUL
02229         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
02230         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTAMBUL
02231             ADD +1                TO  WS-CIA                      ELTAMBUL
02232             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02233         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTAMBUL
02234             ADD +1                TO  WS-CIA                      ELTAMBUL
02235             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02236         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTAMBUL
02237             ADD +1                TO  WS-CIA                      ELTAMBUL
02238             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02239         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTAMBUL
02240             ADD +1                TO  WS-CIA                      ELTAMBUL
02241             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02242         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTAMBUL
02243             ADD +1                TO  WS-CIA                      ELTAMBUL
02244             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02245         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTAMBUL
02246             ADD +1                TO  WS-CIA                      ELTAMBUL
02247             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02248         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTAMBUL
02249             ADD +1                TO  WS-CIA                      ELTAMBUL
02250             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02251      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02252           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02253                                                                   ELTAMBUL
02254      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02255         ADD +1                TO  WS-CIA                          ELTAMBUL
02256         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTAMBUL
02257         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTAMBUL
02258             ADD +1                TO  WS-CIA                      ELTAMBUL
02259             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02260                                                                   ELTAMBUL
02261         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTAMBUL
02262             ADD +1                TO  WS-CIA                      ELTAMBUL
02263             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02264                                                                   ELTAMBUL
02265         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTAMBUL
02266             ADD +1                TO  WS-CIA                      ELTAMBUL
02267             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02268                                                                   ELTAMBUL
02269         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTAMBUL
02270             ADD +1                TO  WS-CIA                      ELTAMBUL
02271             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02272                                                                   ELTAMBUL
02273         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTAMBUL
02274             ADD +1                TO  WS-CIA                      ELTAMBUL
02275             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02276                                                                   ELTAMBUL
02277         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTAMBUL
02278             ADD +1                TO  WS-CIA                      ELTAMBUL
02279             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02280                                                                   ELTAMBUL
02281         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTAMBUL
02282             ADD +1                TO  WS-CIA                      ELTAMBUL
02283             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02284                                                                   ELTAMBUL
02285      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02286           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02287  7099-EXIT.                                                       ELTAMBUL
02288      EXIT.                                                        ELTAMBUL
02289 /                                                                 ELTAMBUL
02290  7100-INST-CERTIFICATION   SECTION.                               ELTAMBUL
02291      MOVE '7100' TO WS-PARA-ID.                                   ELTAMBUL
02292      SET PLT-INDEX2 TO 1.                                         ELTAMBUL
02293      MOVE SPACES TO WS-DTL-BASIC-1-1                              ELTAMBUL
02294                     WS-DTL-BASIC-1-2                              ELTAMBUL
02295                     WS-DTL-BASIC-2-1                              ELTAMBUL
02296                     WS-DTL-BASIC-2-2                              ELTAMBUL
02297                     WS-DTL-BASIC-3-1                              ELTAMBUL
02298                     WS-DTL-BASIC-3-2                              ELTAMBUL
02299                     WS-DTL-BASIC-4-1                              ELTAMBUL
02300                     WS-DTL-BASIC-4-2                              ELTAMBUL
02301                     WS-DTL-SUPP-1-1                               ELTAMBUL
02302                     WS-DTL-SUPP-1-2                               ELTAMBUL
02303                     WS-DTL-SUPP-2-1                               ELTAMBUL
02304                     WS-DTL-SUPP-2-2                               ELTAMBUL
02305                     WS-DTL-SUPP-3-1                               ELTAMBUL
02306                     WS-DTL-SUPP-3-2                               ELTAMBUL
02307                     WS-DTL-SUPP-4-1                               ELTAMBUL
02308                     WS-DTL-SUPP-4-2.                              ELTAMBUL
02309      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTAMBUL
02310                                              NOT = ZEROES         ELTAMBUL
02311        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02312                                         NOT = LOW-VALUES          ELTAMBUL
02313          MOVE                                                     ELTAMBUL
02314           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02315                                    TO  CMF-CODE-VALUE             ELTAMBUL
02316          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTAMBUL
02317          MOVE 'CERTFN-REQRM-IND'                                  ELTAMBUL
02318                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
02319          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
02320          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
02321          STRING                                                   ELTAMBUL
02322             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02323             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
02324             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
02325             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
02326             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
02327             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
02328             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
02329             CMF-DESCR-LINE (8)                                    ELTAMBUL
02330              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
02331             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
02332             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
02333             MOVE +69       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
02334             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
02335             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
02336             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
02337             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
02338             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
02339             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
02340             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
02341             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
02342            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-1-1              ELTAMBUL
02343            MOVE TCAR-OPF-DATA(2) TO WS-DTL-BASIC-1-2              ELTAMBUL
02344            MOVE TCAR-OPF-DATA(3) TO WS-DTL-BASIC-2-1              ELTAMBUL
02345            MOVE TCAR-OPF-DATA(4) TO WS-DTL-BASIC-2-2              ELTAMBUL
02346            MOVE TCAR-OPF-DATA(5) TO WS-DTL-BASIC-3-1              ELTAMBUL
02347            MOVE TCAR-OPF-DATA(6) TO WS-DTL-BASIC-3-2              ELTAMBUL
02348            MOVE TCAR-OPF-DATA(7) TO WS-DTL-BASIC-4-1              ELTAMBUL
02349            MOVE TCAR-OPF-DATA(8) TO WS-DTL-BASIC-4-2.             ELTAMBUL
02350                                                                   ELTAMBUL
02351      SET PLT-INDEX2 TO 2.                                         ELTAMBUL
02352      IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTAMBUL
02353                                              NOT = ZEROES         ELTAMBUL
02354        IF PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02355                                         NOT = LOW-VALUES          ELTAMBUL
02356          MOVE                                                     ELTAMBUL
02357           PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)           ELTAMBUL
02358                                    TO  CMF-CODE-VALUE             ELTAMBUL
02359          MOVE 'BP'                 TO  CMF-RECORD-PREFIX          ELTAMBUL
02360          MOVE 'CERTFN-REQRM-IND'                                  ELTAMBUL
02361                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTAMBUL
02362          PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT            ELTAMBUL
02363          MOVE SPACES TO  TCAR-FROM-AREA                           ELTAMBUL
02364          STRING                                                   ELTAMBUL
02365             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02366             CMF-DESCR-LINE (2) ' '                                ELTAMBUL
02367             CMF-DESCR-LINE (3) ' '                                ELTAMBUL
02368             CMF-DESCR-LINE (4) ' '                                ELTAMBUL
02369             CMF-DESCR-LINE (5) ' '                                ELTAMBUL
02370             CMF-DESCR-LINE (6) ' '                                ELTAMBUL
02371             CMF-DESCR-LINE (7) ' '                                ELTAMBUL
02372             CMF-DESCR-LINE (8)                                    ELTAMBUL
02373              DELIMITED BY SIZE INTO TCAR-FROM-AREA                ELTAMBUL
02374             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTAMBUL
02375             MOVE +08       TO  TCAR-OUTPUT-FIELD-COUNT            ELTAMBUL
02376             MOVE +62       TO  TCAR-OUTPUT-FIELD-1-LEN            ELTAMBUL
02377             MOVE +76       TO  TCAR-OUTPUT-FIELD-2-LEN            ELTAMBUL
02378             MOVE +76       TO  TCAR-OUTPUT-FIELD-3-LEN            ELTAMBUL
02379             MOVE +76       TO  TCAR-OUTPUT-FIELD-4-LEN            ELTAMBUL
02380             MOVE +76       TO  TCAR-OUTPUT-FIELD-5-LEN            ELTAMBUL
02381             MOVE +76       TO  TCAR-OUTPUT-FIELD-6-LEN            ELTAMBUL
02382             MOVE +76       TO  TCAR-OUTPUT-FIELD-7-LEN            ELTAMBUL
02383             MOVE +76       TO  TCAR-OUTPUT-FIELD-8-LEN            ELTAMBUL
02384             PERFORM TCPR-000-TEXT-UNSTRING                        ELTAMBUL
02385            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-1-1               ELTAMBUL
02386            MOVE TCAR-OPF-DATA(2) TO WS-DTL-SUPP-1-2               ELTAMBUL
02387            MOVE TCAR-OPF-DATA(3) TO WS-DTL-SUPP-2-1               ELTAMBUL
02388            MOVE TCAR-OPF-DATA(4) TO WS-DTL-SUPP-2-2               ELTAMBUL
02389            MOVE TCAR-OPF-DATA(5) TO WS-DTL-SUPP-3-1               ELTAMBUL
02390            MOVE TCAR-OPF-DATA(6) TO WS-DTL-SUPP-3-2               ELTAMBUL
02391            MOVE TCAR-OPF-DATA(7) TO WS-DTL-SUPP-4-1               ELTAMBUL
02392            MOVE TCAR-OPF-DATA(8) TO WS-DTL-SUPP-4-2.              ELTAMBUL
02393                                                                   ELTAMBUL
02394                                                                   ELTAMBUL
02395      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTAMBUL
02396             WS-DTL-SUPP-1-1  NOT = SPACES                         ELTAMBUL
02397                  ADD +1                TO WS-CIA                  ELTAMBUL
02398                  MOVE LOW-VALUES       TO COF-DTL-LINE(WS-CIA)    ELTAMBUL
02399                  ADD +1                TO WS-CIA                  ELTAMBUL
02400             MOVE WS-CERTIFICATION  TO COF-DTL-LINE(WS-CIA).       ELTAMBUL
02401                                                                   ELTAMBUL
02402      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02403                  ADD +1                TO WS-CIA                  ELTAMBUL
02404         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTAMBUL
02405         IF WS-DTL-BASIC-1-2 NOT = SPACES                          ELTAMBUL
02406             ADD +1                TO  WS-CIA                      ELTAMBUL
02407             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02408         IF WS-DTL-BASIC-2-1 NOT = SPACES                          ELTAMBUL
02409             ADD +1                TO  WS-CIA                      ELTAMBUL
02410             MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02411         IF WS-DTL-BASIC-2-2 NOT = SPACES                          ELTAMBUL
02412             ADD +1                TO  WS-CIA                      ELTAMBUL
02413             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02414         IF WS-DTL-BASIC-3-1 NOT = SPACES                          ELTAMBUL
02415             ADD +1                TO  WS-CIA                      ELTAMBUL
02416             MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02417         IF WS-DTL-BASIC-3-2 NOT = SPACES                          ELTAMBUL
02418             ADD +1                TO  WS-CIA                      ELTAMBUL
02419             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02420         IF WS-DTL-BASIC-4-1 NOT = SPACES                          ELTAMBUL
02421             ADD +1                TO  WS-CIA                      ELTAMBUL
02422             MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02423         IF WS-DTL-BASIC-4-2 NOT = SPACES                          ELTAMBUL
02424             ADD +1                TO  WS-CIA                      ELTAMBUL
02425             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02426      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTAMBUL
02427           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02428                                                                   ELTAMBUL
02429      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02430         ADD +1                TO  WS-CIA                          ELTAMBUL
02431         MOVE WS-SUPP-1-1     TO  COF-DTL-LINE(WS-CIA)             ELTAMBUL
02432         IF WS-DTL-SUPP-1-2  NOT = SPACES                          ELTAMBUL
02433             ADD +1                TO  WS-CIA                      ELTAMBUL
02434             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02435                                                                   ELTAMBUL
02436         IF WS-DTL-SUPP-2-1  NOT = SPACES                          ELTAMBUL
02437             ADD +1                TO  WS-CIA                      ELTAMBUL
02438             MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02439                                                                   ELTAMBUL
02440         IF WS-DTL-SUPP-2-2  NOT = SPACES                          ELTAMBUL
02441             ADD +1                TO  WS-CIA                      ELTAMBUL
02442             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02443                                                                   ELTAMBUL
02444         IF WS-DTL-SUPP-3-1  NOT = SPACES                          ELTAMBUL
02445             ADD +1                TO  WS-CIA                      ELTAMBUL
02446             MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02447                                                                   ELTAMBUL
02448         IF WS-DTL-SUPP-3-2  NOT = SPACES                          ELTAMBUL
02449             ADD +1                TO  WS-CIA                      ELTAMBUL
02450             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02451                                                                   ELTAMBUL
02452         IF WS-DTL-SUPP-4-1  NOT = SPACES                          ELTAMBUL
02453             ADD +1                TO  WS-CIA                      ELTAMBUL
02454             MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)        ELTAMBUL
02455                                                                   ELTAMBUL
02456         IF WS-DTL-SUPP-4-2  NOT = SPACES                          ELTAMBUL
02457             ADD +1                TO  WS-CIA                      ELTAMBUL
02458             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA).       ELTAMBUL
02459                                                                   ELTAMBUL
02460      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTAMBUL
02461           PERFORM 3000-OUTPUT-TEXT.                               ELTAMBUL
02462  7199-EXIT.                                                       ELTAMBUL
02463      EXIT.                                                        ELTAMBUL
02464 /                                                                 ELTAMBUL
02465  7200-TRANS-OTHER-RESP SECTION.                                   ELTAMBUL
02466                                                                   ELTAMBUL
02467      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTAMBUL
02468         SET  PLT-INDEX2       TO  1                               ELTAMBUL
02469      ELSE                                                         ELTAMBUL
02470         SET PLT-INDEX2        TO 2.                               ELTAMBUL
02471                                                                   ELTAMBUL
02472      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTAMBUL
02473         ZERO                                                      ELTAMBUL
02474         GO TO 7200-EXIT.                                          ELTAMBUL
02475                                                                   ELTAMBUL
02476      MOVE LOW-VALUES TO COF-DTL-LINE(WS-CIA).                     ELTAMBUL
02477      ADD +1     TO  WS-CIA.                                       ELTAMBUL
02478      MOVE 'BP'                       TO  CMF-RECORD-PREFIX.       ELTAMBUL
02479      MOVE 'TRANSF-OTHER-RESP-IND'    TO  CMF-ELEMENT-SYSTEM-NAME. ELTAMBUL
02480                                                                   ELTAMBUL
02481      MOVE  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)     ELTAMBUL
02482                       TO CMF-CODE-VALUE.                          ELTAMBUL
02483      PERFORM 8500-CALL-CODES-MANUAL THRU 8500-EXIT.               ELTAMBUL
02484      MOVE SPACES           TO  TCAR-FROM-AREA.                    ELTAMBUL
02485      STRING WS-SPILLOVER-DEDBL                                    ELTAMBUL
02486             CMF-DESCR-LINE (1) ' '                                ELTAMBUL
02487             CMF-DESCR-LINE (2)                                    ELTAMBUL
02488              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTAMBUL
02489      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTAMBUL
02490      MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTAMBUL
02491      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTAMBUL
02492      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTAMBUL
02493      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTAMBUL
02494      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTAMBUL
02495      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTAMBUL
02496            ADD +1                TO  WS-CIA                       ELTAMBUL
02497            MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).        ELTAMBUL
02498      PERFORM 3000-OUTPUT-TEXT.                                    ELTAMBUL
02499  7200-EXIT.                                                       ELTAMBUL
02500      EXIT.                                                        ELTAMBUL
02501 /   C O D E S   M A N U A L   C A L L                             ELTAMBUL
02502  8500-CALL-CODES-MANUAL.                                          ELTAMBUL
02503      INITIALIZE CMF-RETURN-CODE                                   ELTAMBUL
02504                 TCAR-FROM-AREA.                                   ELTAMBUL
02505      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTAMBUL
02506                 COMMAREA(DFHCOMMAREA)                             ELTAMBUL
02507      END-EXEC.                                                    ELTAMBUL
02508                                                                   ELTAMBUL
02509      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTAMBUL
02510      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTAMBUL
02511          ADDRESS OF CMF-DESCR.                                    ELTAMBUL
02512                                                                   ELTAMBUL
02513  8500-EXIT.      EXIT.                                            ELTAMBUL
02514 /   C O M P R E S S I O N   A N D   U N S T R I N G   R O U T I N ELTAMBUL
02515  COPY ELSTCOMP.                                                   ELTAMBUL
02516 /              A B E N D                                          ELTAMBUL
02517 ******************************************************************ELTAMBUL
02518 *                        A B E N D                                ELTAMBUL
02519 *    THIS SECTION ABENDS USING THE ABEND CODE EARLIER DEFINED.    ELTAMBUL
02520 *                                                                 ELTAMBUL
02521 ******************************************************************ELTAMBUL
02522  9999-ABEND SECTION.                                              ELTAMBUL
02523                                                                   ELTAMBUL
02524                                                                   ELTAMBUL
02525      EXEC CICS ABEND   ABCODE(WS-ABEND-CODE) END-EXEC.            ELTAMBUL
02526                                                                   ELTAMBUL
02527  9999-EXIT.     EXIT.                                             ELTAMBUL
