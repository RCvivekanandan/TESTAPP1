00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTPSYCH.                                            ELTPSYCH
00003  AUTHOR. JOHN CURIN - KEANE.                                         LV001
00004  DATE-WRITTEN.   5/06/86.                                         ELTPSYCH
00005  DATE-COMPILED.                                                   ELTPSYCH
00006      SKIP3                                                        ELTPSYCH
00007 ******************************************************************ELTPSYCH
00008 *  ELTPSYCH                                                       ELTPSYCH
00009 *                                                                 ELTPSYCH
00010 *                        PROGRAM ABSTRACT                         ELTPSYCH
00011 *                                                                 ELTPSYCH
00012 *   PROGRAM NAME:   E.L.S. PSYCHIATRIC SERVICES TOPIC             ELTPSYCH
00013 *                                                                 ELTPSYCH
00014 *   PROGRAM I.D.:   ELTPSYCH                                      ELTPSYCH
00015 *                                                                 ELTPSYCH
00016 *   PURPOSE:   TO DISPLAY ENGLISH VERBIAGE DESCRIBING THE         ELTPSYCH
00017 *              PSYCHIATRIC SERVICES                               ELTPSYCH
00018 *              BENEFIT PROVISION COVERAGE GIVEN A MEMBER.         ELTPSYCH
00019 *                                                                 ELTPSYCH
00020 *   OVERVIEW:  THE PROGRAM DISPLAYS THE TYPE OF PSYCHIATRIC       ELTPSYCH
00021 *              SERVICES AFFORDED A MEMBER BY HIS GROUP.           ELTPSYCH
00022 *              THIS INFORMATION IS GOTTEN BY INTEROGATING THE     ELTPSYCH
00023 *              BENEFIT PROVISIONS FOR THE GROUP WITHIN THE        ELTPSYCH
00024 *              CONTRACT FOR A PARTICULAR RANGE OF DATES.          ELTPSYCH
00025 *                                                                 ELTPSYCH
00026 *   RECORDS                                                       ELTPSYCH
00027 *   ACCESSED:  CONTRACT, GROUP SPECIFIC, VARIOUS BENEFIT          ELTPSYCH
00028 *              PROVISION, AND A LARGE NUMBER OF DATA ELEMENT      ELTPSYCH
00029 *              AND CODE VALUE RECORDS.                            ELTPSYCH
00030 *                                                                 ELTPSYCH
00031 *   PROCESSING                                                    ELTPSYCH
00032 *   FUNCTIONS: XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXELTPSYCH
00033 *                                                                 ELTPSYCH
00034 *   UPDATE HISTORY                                                ELTPSYCH
00035 *                                                                 ELTPSYCH
00036 *   07/24/86 JTC  CHANGED THE PICTURE OF WS-DTL-MAX-AMOUNT        ELTPSYCH
00037 *                 FROM PIC $$$9 TO PIC ZZ9.99-.                   ELTPSYCH
00038 *                 ALSO CHANGED THE DISPLAY OF THE MAXIMUM AMOUNT. ELTPSYCH
00039 *                 THERE WAS AN ERROR.  A SUPPLEMENTAL MAX AMOUNT, ELTPSYCH
00040 *                 WOULD HAVE BEEN DISPLAYED AS A BASIC MAX AMOUNT.ELTPSYCH
00041 *   08/14/86 JTC  REMOVED THE SETUP OF HEADING LINE 1             ELTPSYCH
00042 *                 WS-HDR-1.                                       ELTPSYCH
00043 *                                                                 ELTPSYCH
00044 *   10/06/86 NAC  VS COBOL II CONVERSION.                         ELTPSYCH
00045 *   10/21/87 EGL  CHANGED FIXED TEXT.                             ELTPSYCH
00046 *   11/01/88 NAC  CHANGED IPO/PSO FORMATS FROM W TO B FOR INST;   ELTPSYCH
00047 *                 INCORPORATE NEW STORAGE ENHANCEMENTS.           ELTPSYCH
00048 *   11/14/88 NAC  CORRECT LOGIC FOR DETERMINING TYPE OF CONTRACT. ELTPSYCH
00049 *   10/13/89 RKH  ADDED TRANSFER TO TOHER RESPONSIBILITY IND      ELTPSYCH
00050 *   08/28/90 GEM  ADDED BENEFIT PROVISION IDS.                    ELTPSYCH
00051 *   11/16/90 GEM  CHANGED PLP-TRANSF-OTHER-RESP-IND COMPARE TO THEELTPSYCH
00052 *                 LITERAL ZERO INSTEAD OF THE DIGIT '0'.          ELTPSYCH
00053 *   07/08/91 JPB  CHANGED BEN-MAX-VISITS-IND TO BEN-MAX-VISIT-IND ELTPSYCH
00054 *                 FOR BPD FORMAT.                                 ELTPSYCH
00055 *   02/11/94 JPB  ADDED TRANSLATION AND DISPLAY OF NETWORK/UTILI- ELTPSYCH
00056 *                 ZATION REVIEW INDICATOR.                        ELTPSYCH
00057 ***************************************************************** ELTPSYCH
00058 /                                                                 ELTPSYCH
00059  ENVIRONMENT DIVISION.                                            ELTPSYCH
00060 /                                                                 ELTPSYCH
00061  DATA DIVISION.                                                   ELTPSYCH
00062  WORKING-STORAGE SECTION.                                         ELTPSYCH
00063  01  WS-BEGIN                    PIC X(24)  VALUE                 ELTPSYCH
00064      '***ELTPSYCH WS BEGINS***'.                                  ELTPSYCH
00065 /     W O R K F I E L D S ,   A N D   S W I T C H E S             ELTPSYCH
00066  01  WS-WORK-FIELDS.                                              ELTPSYCH
00067      05  WS-HEX-00                     PIC X.                     ELTPSYCH
00068      05  WS-CHAR-0                     PIC X.                     ELTPSYCH
00069      05  WS-DTL-DAYS-REDUCED-APL       PIC Z9.                    ELTPSYCH
00070      05  WS-DTL-DAYS-REDUCED-BASE      PIC Z9.                    ELTPSYCH
00071      05  WS-HOLD1                      PIC X(10).                 ELTPSYCH
00072      05  WS-HOLD2                      PIC X(10).                 ELTPSYCH
00073      05  WS-DISPLAY-AAR-TEXT           PIC X.                     ELTPSYCH
00074      05  WS-DISPLAY-B-FORMAT-TEXT      PIC X.                     ELTPSYCH
00075      05  WS-DISPLAY-DAY-PSYCH-TEXT     PIC X.                     ELTPSYCH
00076      05  WS-DISPLAY-NIGHT-PSYCH-TEXT   PIC X.                     ELTPSYCH
00077      05  WS-DISPLAY-MAX-AMT-TEXT       PIC X.                     ELTPSYCH
00078      05  WS-DISPLAY-SHOCK-TEXT         PIC X.                     ELTPSYCH
00079      05  WS-DISPLAY-MAX-VISITS-TEXT    PIC X.                     ELTPSYCH
00080      05  WS-YES                        PIC X     VALUE 'Y'.       ELTPSYCH
00081      05  WS-NO                         PIC X     VALUE 'N'.       ELTPSYCH
00082      05  WS-PER-DIEM                   PIC $$$$$9.99.             ELTPSYCH
00083      05  WS-ALLOW                      PIC $$$9.99.               ELTPSYCH
00084      05  WS-DTL-MAX-AMOUNT             PIC ZZ9.99-.               ELTPSYCH
00085      05  WS-DTL-MAX-DAYS               PIC ZZ9.                   ELTPSYCH
00086      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTPSYCH
00087      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTPSYCH
00088      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTPSYCH
00089      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTPSYCH
00090      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTPSYCH
00091      05  WS-FIRSTTIME-IND              PIC X.                     ELTPSYCH
00092        88  WS-NOT-FIRST-TIME               VALUE 'N'.             ELTPSYCH
00093      05  WS-ADD-A-BLANK-IND            PIC X.                     ELTPSYCH
00094        88  WS-ADD-A-BLANK-LINE             VALUE 'Y'.             ELTPSYCH
00095      05  WS-MOVE-LINES-IND             PIC X VALUE 'Y'.           ELTPSYCH
00096        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTPSYCH
00097      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTPSYCH
00098      05  WS-PRCNT-PERDM-ALLOW          PIC X(9).                  ELTPSYCH
00099      05  WS-PERCENT-FLD.                                          ELTPSYCH
00100        10  WS-PERCENTAGE               PIC ZZ9.                   ELTPSYCH
00101        10  WS-PERCENT-SIGN             PIC X.                     ELTPSYCH
00102      05  WS-BASIC-CONTRACT             PIC X.                     ELTPSYCH
00103        88  WS-BASIC-PRESENT                VALUE 'B'.             ELTPSYCH
00104      05  WS-SUPP-CONTRACT              PIC X.                     ELTPSYCH
00105        88  WS-SUPP-PRESENT                 VALUE 'S'.             ELTPSYCH
00106      05  WS-NETWORK-UTIL-IND           PIC XX.                    ELTPSYCH
00107        88  WS-VALID-INST-INPATIENT     VALUE '01' '02' '03'       ELTPSYCH
00108                                              '05' '06' '08'.      ELTPSYCH
00109        88  WS-VALID-INST-OUTPATIENT    VALUE '01' '02' '06' '08'. ELTPSYCH
00110        88  WS-VALID-PROF-INPATIENT     VALUE '01' '02' '04'       ELTPSYCH
00111                                              '05' '07' '08'.      ELTPSYCH
00112        88  WS-VALID-PROF-OUTPATIENT    VALUE '01' '02' '07' '08'. ELTPSYCH
00113                                                                   ELTPSYCH
00114 /     B E N   P R O V   I D S   B Y   T Y P E                     ELTPSYCH
00115  01  TABLE-MAX                   PIC S9(03) VALUE +13 COMP.       ELTPSYCH
00116 * 11 REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTPSYCH
00117  01  WS-BEN-PROV-ID.                                              ELTPSYCH
00118      05  WS-INST-IP-CNT                PIC S999 COMP-3  VALUE +13.ELTPSYCH
00119      05  WS-INST-IP-TAB.                                          ELTPSYCH
00120        10  FILLER                      PIC X(6)  VALUE 'PSYI W'.  ELTPSYCH
00121        10  FILLER                      PIC X(6)  VALUE 'DPSY A'.  ELTPSYCH
00122        10  FILLER                      PIC X(6)  VALUE 'NPSY A'.  ELTPSYCH
00123        10  FILLER                      PIC X(6)  VALUE 'IPI  B'.  ELTPSYCH
00124        10  FILLER                      PIC X(6)  VALUE 'STI  B'.  ELTPSYCH
00125        10  FILLER                      PIC X(6)  VALUE 'GPI  B'.  ELTPSYCH
00126        10  FILLER                      PIC X(6)  VALUE 'FCAI B'.  ELTPSYCH
00127        10  FILLER                      PIC X(6)  VALUE 'FCCI B'.  ELTPSYCH
00128        10  FILLER                      PIC X(6)  VALUE 'HYPI B'.  ELTPSYCH
00129        10  FILLER                      PIC X(6)  VALUE 'PSI  B'.  ELTPSYCH
00130        10  FILLER                      PIC X(6)  VALUE 'PASS A'.  ELTPSYCH
00131        10  FILLER                      PIC X(6)  VALUE 'SOSI B'.  ELTPSYCH
00132        10  FILLER                      PIC X(6)  VALUE 'VOCI B'.  ELTPSYCH
00133      05  WS-INST-IP-LIST     REDEFINES    WS-INST-IP-TAB          ELTPSYCH
00134                                        PIC X(6)  OCCURS 13 TIMES. ELTPSYCH
00135                                                                   ELTPSYCH
00136      05  WS-INST-OP-CNT                PIC S999 COMP-3  VALUE +10.ELTPSYCH
00137      05  WS-INST-OP-TAB.                                          ELTPSYCH
00138        10  FILLER                      PIC X(6)  VALUE 'PSYO W'.  ELTPSYCH
00139        10  FILLER                      PIC X(6)  VALUE 'IPO  B'.  ELTPSYCH
00140        10  FILLER                      PIC X(6)  VALUE 'GPO  B'.  ELTPSYCH
00141        10  FILLER                      PIC X(6)  VALUE 'STO  A'.  ELTPSYCH
00142        10  FILLER                      PIC X(6)  VALUE 'FCAO B'.  ELTPSYCH
00143        10  FILLER                      PIC X(6)  VALUE 'FCCO B'.  ELTPSYCH
00144        10  FILLER                      PIC X(6)  VALUE 'HYPO B'.  ELTPSYCH
00145        10  FILLER                      PIC X(6)  VALUE 'PSO  B'.  ELTPSYCH
00146        10  FILLER                      PIC X(6)  VALUE 'SOSO B'.  ELTPSYCH
00147        10  FILLER                      PIC X(6)  VALUE 'VOCO B'.  ELTPSYCH
00148      05  WS-INST-OP-LIST     REDEFINES    WS-INST-OP-TAB          ELTPSYCH
00149                                        PIC X(6)  OCCURS 10 TIMES. ELTPSYCH
00150                                                                   ELTPSYCH
00151      05  WS-PROF-IP-CNT                PIC S999 COMP-3  VALUE +13.ELTPSYCH
00152      05  WS-PROF-IP-TAB.                                          ELTPSYCH
00153        10  FILLER                      PIC X(6)  VALUE 'DPV  D'.  ELTPSYCH
00154        10  FILLER                      PIC X(6)  VALUE 'NPV  D'.  ELTPSYCH
00155        10  FILLER                      PIC X(6)  VALUE 'MNI  D'.  ELTPSYCH
00156        10  FILLER                      PIC X(6)  VALUE 'ECFM D'.  ELTPSYCH
00157        10  FILLER                      PIC X(6)  VALUE 'ETAI C'.  ELTPSYCH
00158        10  FILLER                      PIC X(6)  VALUE 'ESTI E'.  ELTPSYCH
00159        10  FILLER                      PIC X(6)  VALUE 'GPI  D'.  ELTPSYCH
00160        10  FILLER                      PIC X(6)  VALUE 'FCAI D'.  ELTPSYCH
00161        10  FILLER                      PIC X(6)  VALUE 'FCCI D'.  ELTPSYCH
00162        10  FILLER                      PIC X(6)  VALUE 'HYPI D'.  ELTPSYCH
00163        10  FILLER                      PIC X(6)  VALUE 'IPI  D'.  ELTPSYCH
00164        10  FILLER                      PIC X(6)  VALUE 'PSI  D'.  ELTPSYCH
00165        10  FILLER                      PIC X(6)  VALUE 'SOSI E'.  ELTPSYCH
00166      05  WS-PROF-IP-LIST     REDEFINES    WS-PROF-IP-TAB          ELTPSYCH
00167                                        PIC X(6)  OCCURS 13 TIMES. ELTPSYCH
00168                                                                   ELTPSYCH
00169      05  WS-PROF-OP-CNT                PIC S999 COMP-3  VALUE +9. ELTPSYCH
00170      05  WS-PROF-OP-TAB.                                          ELTPSYCH
00171        10  FILLER                      PIC X(6)  VALUE 'ESTO E'.  ELTPSYCH
00172        10  FILLER                      PIC X(6)  VALUE 'ETAO C'.  ELTPSYCH
00173        10  FILLER                      PIC X(6)  VALUE 'IPO  E'.  ELTPSYCH
00174        10  FILLER                      PIC X(6)  VALUE 'GPO  E'.  ELTPSYCH
00175        10  FILLER                      PIC X(6)  VALUE 'FCAO E'.  ELTPSYCH
00176        10  FILLER                      PIC X(6)  VALUE 'FCCO E'.  ELTPSYCH
00177        10  FILLER                      PIC X(6)  VALUE 'HYPO E'.  ELTPSYCH
00178        10  FILLER                      PIC X(6)  VALUE 'PSO  E'.  ELTPSYCH
00179        10  FILLER                      PIC X(6)  VALUE 'SOSO E'.  ELTPSYCH
00180      05  WS-PROF-OP-LIST     REDEFINES    WS-PROF-OP-TAB          ELTPSYCH
00181                                        PIC X(6)  OCCURS 9 TIMES.  ELTPSYCH
00182                                                                   ELTPSYCH
00183 /            D I S P L A Y   L I N E S                            ELTPSYCH
00184  01  WS-ELS-DISPLAY-LINES.                                        ELTPSYCH
00185                                                                   ELTPSYCH
00186    05  WS-HDR-2-PROF-IP.                                          ELTPSYCH
00187      10  FILLER                    PIC X(16) VALUE SPACES.        ELTPSYCH
00188      10  FILLER                    PIC X(43)                      ELTPSYCH
00189          VALUE 'PSYCHIATRIC SERVICES INPATIENT PROFESSIONAL'.     ELTPSYCH
00190      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTPSYCH
00191                                                                   ELTPSYCH
00192    05  WS-HDR-2-PROF-OP.                                          ELTPSYCH
00193      10  FILLER                    PIC X(17) VALUE SPACES.        ELTPSYCH
00194      10  FILLER                    PIC X(44)                      ELTPSYCH
00195          VALUE 'PSYCHIATRIC SERVICES OUTPATIENT PROFESSIONAL'.    ELTPSYCH
00196      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTPSYCH
00197                                                                   ELTPSYCH
00198    05  WS-HDR-2-INST-IP.                                          ELTPSYCH
00199      10  FILLER                    PIC X(16) VALUE SPACES.        ELTPSYCH
00200      10  FILLER                    PIC X(44)                      ELTPSYCH
00201          VALUE 'PSYCHIATRIC SERVICES INPATIENT INSTITUTIONAL'.    ELTPSYCH
00202      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTPSYCH
00203                                                                   ELTPSYCH
00204    05  WS-HDR-2-INST-OP.                                          ELTPSYCH
00205      10  FILLER                    PIC X(16) VALUE SPACES.        ELTPSYCH
00206      10  FILLER                    PIC X(45)                      ELTPSYCH
00207          VALUE 'PSYCHIATRIC SERVICES OUTPATIENT INSTITUTIONAL'.   ELTPSYCH
00208      10  FILLER                    PIC X(19) VALUE LOW-VALUES.    ELTPSYCH
00209 *                                                                 ELTPSYCH
00210    05  WS-NETWORK-UTIL-REV.                                       ELTPSYCH
00211      10  WS-NETWORK-UTIL-REV-PHR   PIC X(46)      VALUE           ELTPSYCH
00212          'PSYCHIATRIC SERVICES PROCESSING IS BASED ON - '.        ELTPSYCH
00213      10  FILLER                    PIC X(33) VALUE LOW-VALUES.    ELTPSYCH
00214                                                                   ELTPSYCH
00215    05  WS-PAYMNT-BASED.                                           ELTPSYCH
00216      10  FILLER                    PIC X(20)                      ELTPSYCH
00217          VALUE 'PAYMENT IS BASED ON:'.                            ELTPSYCH
00218      10  FILLER                    PIC X(59) VALUE LOW-VALUES.    ELTPSYCH
00219                                                                   ELTPSYCH
00220    05  WS-BASIC.                                                  ELTPSYCH
00221      10  WS-BASIC-LIT              PIC X(16)                      ELTPSYCH
00222          VALUE '         BASIC: '.                                ELTPSYCH
00223      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTPSYCH
00224                                                                   ELTPSYCH
00225    05  WS-FOLLOWING-BEN.                                          ELTPSYCH
00226      10  FILLER                    PIC X(79)                      ELTPSYCH
00227          VALUE 'COVERED SERVICES ARE:'.                           ELTPSYCH
00228                                                                   ELTPSYCH
00229    05  WS-SERVICES-RENDERED.                                      ELTPSYCH
00230      10  FILLER                    PIC X(79)                      ELTPSYCH
00231          VALUE 'SERVICES MAY BE RENDERED:'.                       ELTPSYCH
00232                                                                   ELTPSYCH
00233    05  WS-SUPPLEMENTAL.                                           ELTPSYCH
00234      10  WS-SUPP-LIT               PIC X(16)                      ELTPSYCH
00235          VALUE '  SUPPLEMENTAL: '.                                ELTPSYCH
00236      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTPSYCH
00237                                                                   ELTPSYCH
00238    05  WS-MAX-LEAVES.                                             ELTPSYCH
00239      10  FILLER                    PIC X(44) VALUE                ELTPSYCH
00240          'THE MAXIMUM NUMBER OF LEAVES PER ADMISSION: '.          ELTPSYCH
00241      10  WS-DTL-MAX-LEAVES         PIC ZZ9.                       ELTPSYCH
00242                                                                   ELTPSYCH
00243    05  WS-PAYABLE-AS.                                             ELTPSYCH
00244      10  FILLER                  PIC  X(40) VALUE                 ELTPSYCH
00245          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTPSYCH
00246                                                                   ELTPSYCH
00247    05  WS-CERT-REQ               PIC  X(48) VALUE                 ELTPSYCH
00248              'THE CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.  ELTPSYCH
00249                                                                   ELTPSYCH
00250    05  WS-PROF-INPT-CHRGES         PIC X(61)  VALUE               ELTPSYCH
00251        'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE REPORELTPSYCH
00252 -      'T: '.                                                     ELTPSYCH
00253                                                                   ELTPSYCH
00254    05  WS-PROF-OUTPT-CHRGES        PIC X(62)  VALUE               ELTPSYCH
00255        'IF PROFESSIONAL CHARGES ARE BILLED ON OUTPATIENT CARE REPOELTPSYCH
00256 -      'RT: '.                                                    ELTPSYCH
00257                                                                   ELTPSYCH
00258    05  WS-SPILLOVER-COINS          PIC X(23)  VALUE               ELTPSYCH
00259        'SPILLOVER COINSURANCE: '.                                 ELTPSYCH
00260                                                                   ELTPSYCH
00261    05  WS-SPILLOVER-DEDUCT         PIC X(22)  VALUE               ELTPSYCH
00262        'SPILLOVER DEDUCTIBLE: '.                                  ELTPSYCH
00263                                                                   ELTPSYCH
00264    05  WS-SPILLOVER-FL-RT-PER-D    PIC X(30)  VALUE               ELTPSYCH
00265        'SPILLOVER FLAT RATE PER DIEM: '.                          ELTPSYCH
00266                                                                   ELTPSYCH
00267    05  WS-MAX-VISITS               PIC X(34)  VALUE               ELTPSYCH
00268        'THE MAXIMUM NUMBER OF VISITS ARE: '.                      ELTPSYCH
00269                                                                   ELTPSYCH
00270    05  WS-MAX-AMOUNT               PIC X(42)  VALUE               ELTPSYCH
00271        'THE MAXIMUM AMOUNT ELIGIBLE PER VISIT IS: '.              ELTPSYCH
00272                                                                   ELTPSYCH
00273    05  WS-SAME-PROVIDER-ELECT      PIC X(69)  VALUE               ELTPSYCH
00274        'IF THE SAME PROVIDER IS BILLING ELECTRO-SHOCK THERAPY AND ELTPSYCH
00275 -      'ANESTHESIA'.                                              ELTPSYCH
00276                                                                   ELTPSYCH
00277    05  WS-NO-SAME-PROVIDER-ELECT   PIC X(49)  VALUE               ELTPSYCH
00278        'ELECTRO-SHOCK THERAPY/ANESTHESIA ARE NOT COVERED.'.       ELTPSYCH
00279                                                                   ELTPSYCH
00280    05  WS-SECONDARY                PIC X(09) VALUE 'SECONDARY'.   ELTPSYCH
00281                                                                   ELTPSYCH
00282    05  WS-DAYS-REDUCED             PIC X(22) VALUE                ELTPSYCH
00283          ' DAYS REDUCTION RATIO '.                                ELTPSYCH
00284                                                                   ELTPSYCH
00285    05  WS-PER                      PIC X(03) VALUE 'PER'.         ELTPSYCH
00286                                                                   ELTPSYCH
00287    05  WS-FOR                      PIC X(03) VALUE 'FOR'.         ELTPSYCH
00288                                                                   ELTPSYCH
00289    05  WS-DAY-PSYCH                PIC X(51)  VALUE               ELTPSYCH
00290        'DAY PSYCHIATRIC REQUIREMENT FOR PRIOR ADMISSION IS '.     ELTPSYCH
00291                                                                   ELTPSYCH
00292    05  WS-NIGHT-PSYCH              PIC X(53)  VALUE               ELTPSYCH
00293        'NIGHT PSYCHIATRIC REQUIREMENT FOR PRIOR ADMISSION IS '.   ELTPSYCH
00294                                                                   ELTPSYCH
00295    05  WS-CONTRACT-RELATED.                                       ELTPSYCH
00296      10  FILLER                    PIC X(49)                      ELTPSYCH
00297        VALUE 'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTPSYCH
00298      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTPSYCH
00299                                                                   ELTPSYCH
00300    05  WS-PVE-TEXT.                                               ELTPSYCH
00301      10  FILLER                    PIC X(44)  VALUE               ELTPSYCH
00302        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTPSYCH
00303      10  FILLER                    PIC X(35)  VALUE LOW-VALUES.   ELTPSYCH
00304                                                                   ELTPSYCH
00305    05  WS-ACCUM-MSG1.                                             ELTPSYCH
00306      10  FILLER                  PIC  X(79) VALUE                 ELTPSYCH
00307      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTPSYCH
00308 -    'CONSIDERATIONS.'.                                           ELTPSYCH
00309                                                                   ELTPSYCH
00310    05  WS-NO-TABULAR1.                                            ELTPSYCH
00311      10  FILLER                    PIC X(51)  VALUE               ELTPSYCH
00312         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTPSYCH
00313      10  FILLER                    PIC X(22)  VALUE               ELTPSYCH
00314         'GOING FROM BENEFIT ***'.                                 ELTPSYCH
00315                                                                   ELTPSYCH
00316    05  WS-NO-TABULAR2.                                            ELTPSYCH
00317      10  FILLER                    PIC X(15)  VALUE               ELTPSYCH
00318         '*** PROVISION: '.                                        ELTPSYCH
00319      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTPSYCH
00320      10  FILLER                    PIC X VALUE SPACE.             ELTPSYCH
00321      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTPSYCH
00322      10  FILLER                    PIC X(13)  VALUE               ELTPSYCH
00323         ' TO TABULAR: '.                                          ELTPSYCH
00324      10  WS-NO-TAB-ID              PIC X(6).                      ELTPSYCH
00325      10  FILLER                    PIC X VALUE SPACE.             ELTPSYCH
00326      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTPSYCH
00327      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTPSYCH
00328                                                                   ELTPSYCH
00329    05  WS-PGM-ERROR.                                              ELTPSYCH
00330      10  FILLER                    PIC X(20)  VALUE SPACES.       ELTPSYCH
00331      10  FILLER                    PIC X(35)  VALUE               ELTPSYCH
00332         '***  P R O G R A M   E R R O R  ***'.                    ELTPSYCH
00333      10  FILLER                    PIC X(24)  VALUE LOW-VALUES.   ELTPSYCH
00334                                                                   ELTPSYCH
00335    05  WS-BAD-INST-PROF-SEL.                                      ELTPSYCH
00336      10  FILLER                    PIC XX VALUE SPACE.            ELTPSYCH
00337      10  FILLER                    PIC X(47) VALUE                ELTPSYCH
00338         '*** I N V A L I D   I N S T I T U T I O N A L /'.        ELTPSYCH
00339      10  FILLER                    PIC X(48) VALUE                ELTPSYCH
00340         ' P R O F E S S I O N A L   S E L E C T I O N ***'.       ELTPSYCH
00341      10  FILLER                    PIC XX VALUE LOW-VALUES.       ELTPSYCH
00342                                                                   ELTPSYCH
00343    05  WS-BAD-IN-OUT-SEL.                                         ELTPSYCH
00344      10  FILLER                    PIC X(08) VALUE SPACE.         ELTPSYCH
00345      10  FILLER                    PIC X(51) VALUE                ELTPSYCH
00346         '*** I N V A L I D   I N P U T   /   O U T P U T ***'.    ELTPSYCH
00347      10  FILLER                    PIC X(20) VALUE LOW-VALUES.    ELTPSYCH
00348                                                                   ELTPSYCH
00349    05  WS-POSSIBLE-ERROR           PIC X(50)                      ELTPSYCH
00350       VALUE 'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'. ELTPSYCH
00351                                                                   ELTPSYCH
00352    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTPSYCH
00353    05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.              ELTPSYCH
00354      10  WS-TEMP-TEXT-CHAR         PIC X   OCCURS  79  TIMES.     ELTPSYCH
00355  01  WS-END                            PIC X(16)  VALUE           ELTPSYCH
00356      '*** W/S ENDS ***'.                                          ELTPSYCH
00357 /             L I N K A G E   S E C T I O N                       ELTPSYCH
00358  LINKAGE SECTION.                                                 ELTPSYCH
00359  01  DFHCOMMAREA.                                                 ELTPSYCH
00360     COPY ELSCOMMC.                                                ELTPSYCH
00361 /                                                                 ELTPSYCH
00362      COPY ELSCIA2C.                                               ELTPSYCH
00363 /                                                                 ELTPSYCH
00364 ***  IO PARM AREA  ***                                            ELTPSYCH
00365      COPY ELSIOPMC.                                               ELTPSYCH
00366 /                                                                 ELTPSYCH
00367      COPY ELSKEYSC.                                               ELTPSYCH
00368 /                                                                 ELTPSYCH
00369      COPY ELSOUTPC.                                               ELTPSYCH
00370 /                                                                 ELTPSYCH
00371      COPY ELSSSCBC.                                               ELTPSYCH
00372 /                                                                 ELTPSYCH
00373      COPY ELSCMIFC.                                               ELTPSYCH
00374 /                                                                 ELTPSYCH
00375      COPY ELSCMDSC.                                               ELTPSYCH
00376 /                                                                 ELTPSYCH
00377      COPY ELSPRVNC.                                               ELTPSYCH
00378 /                                                                 ELTPSYCH
00379 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTPSYCH
00380      COPY ELSPLGSW.                                               ELTPSYCH
00381 *** BENEFIT PROVISION TABLE OF FLDS                               ELTPSYCH
00382      COPY ELSPLGTB.                                               ELTPSYCH
00383 /                                                                 ELTPSYCH
00384      COPY ELSTCWAC.                                               ELTPSYCH
00385 /        G R O U P   S P E C I F I C   R E C O R D                ELTPSYCH
00386  01  GROUP-SPECIFIC-RECORD.                                       ELTPSYCH
00387      COPY GCGROUPC.                                               ELTPSYCH
00388 /        C O N T R A C T   R E C O R D                            ELTPSYCH
00389  01  CONTRACT-RECORD.                                             ELTPSYCH
00390      COPY GCCONTRC.                                               ELTPSYCH
00391 /                  M A I N L I N E                                ELTPSYCH
00392  PROCEDURE DIVISION.                                              ELTPSYCH
00393                                                                   ELTPSYCH
00394 ******************************************************************ELTPSYCH
00395 *                                                                 ELTPSYCH
00396 *   PERFORM THE MAINLINE OPERATIONS.                              ELTPSYCH
00397 *                                                                 ELTPSYCH
00398 ******************************************************************ELTPSYCH
00399  0000-MAINLINE.                                                   ELTPSYCH
00400                                                                   ELTPSYCH
00401                                                                   ELTPSYCH
00402      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTPSYCH
00403          EXEC CICS ABEND                                          ELTPSYCH
00404                    ABCODE ('EL01')                                ELTPSYCH
00405          END-EXEC                                                 ELTPSYCH
00406      END-IF.                                                      ELTPSYCH
00407                                                                   ELTPSYCH
00408      IF ECA-CIA-PTR = NULL                                        ELTPSYCH
00409          EXEC CICS ABEND                                          ELTPSYCH
00410                    ABCODE ('EL02')                                ELTPSYCH
00411          END-EXEC                                                 ELTPSYCH
00412      END-IF.                                                      ELTPSYCH
00413                                                                   ELTPSYCH
00414      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTPSYCH
00415                      ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.    ELTPSYCH
00416                                                                   ELTPSYCH
00417 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTPSYCH
00418                                                                   ELTPSYCH
00419      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTPSYCH
00420      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00421                      ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.      ELTPSYCH
00422      IF NOT CIA-RC-OK                                             ELTPSYCH
00423          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00424                                                                   ELTPSYCH
00425      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTPSYCH
00426      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00427                      ADDRESS OF COF-OUTPUT-INTERFACE.             ELTPSYCH
00428      IF NOT CIA-RC-OK                                             ELTPSYCH
00429          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00430                                                                   ELTPSYCH
00431      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTPSYCH
00432      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00433                      ADDRESS OF KWA-FILE-KEY-WORK-AREA.           ELTPSYCH
00434      IF NOT CIA-RC-OK                                             ELTPSYCH
00435          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00436                                                                   ELTPSYCH
00437      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTPSYCH
00438      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00439                      ADDRESS OF CMF-CODES-MANUAL-INTERFACE.       ELTPSYCH
00440      IF NOT CIA-RC-OK                                             ELTPSYCH
00441          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00442                                                                   ELTPSYCH
00443      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTPSYCH
00444      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00445                      ADDRESS OF TCAR-COMPRESSION-WORK-AREA.       ELTPSYCH
00446      IF NOT CIA-RC-OK                                             ELTPSYCH
00447          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00448                                                                   ELTPSYCH
00449      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTPSYCH
00450      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00451                      ADDRESS OF GROUP-SPECIFIC-RECORD.            ELTPSYCH
00452      IF NOT CIA-RC-OK                                             ELTPSYCH
00453          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00454                                                                   ELTPSYCH
00455      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTPSYCH
00456      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00457                      ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.       ELTPSYCH
00458      IF NOT CIA-RC-OK                                             ELTPSYCH
00459          PERFORM 0098-SIGNAL-UNALL-AREA-ERROR.                    ELTPSYCH
00460                                                                   ELTPSYCH
00461      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPSYCH
00462      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTPSYCH
00463              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTPSYCH
00464                                                                   ELTPSYCH
00465      SET CIA-STG-GETMAIN TO TRUE.                                 ELTPSYCH
00466      EXEC CICS LINK                                               ELTPSYCH
00467                PROGRAM('ELUSTGMG')                                ELTPSYCH
00468                COMMAREA(DFHCOMMAREA)                              ELTPSYCH
00469      END-EXEC.                                                    ELTPSYCH
00470                                                                   ELTPSYCH
00471      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTPSYCH
00472      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00473                      ADDRESS OF PVN-BENEFIT-PROVISION-LIST.       ELTPSYCH
00474                                                                   ELTPSYCH
00475      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTPSYCH
00476                                                                   ELTPSYCH
00477      MOVE GCG-NETWORK-UTIL-REVIEW-IND                             ELTPSYCH
00478        TO WS-NETWORK-UTIL-IND.                                    ELTPSYCH
00479                                                                   ELTPSYCH
00480      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTPSYCH
00481                       AND                                         ELTPSYCH
00482         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTPSYCH
00483         PERFORM 1000-INSTITUTIONAL-IP-RTNE THRU 1000-EXIT.        ELTPSYCH
00484                                                                   ELTPSYCH
00485      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTPSYCH
00486                       AND                                         ELTPSYCH
00487         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTPSYCH
00488         PERFORM 2000-PROFESSIONAL-IP-RTNE THRU 2000-EXIT.         ELTPSYCH
00489                                                                   ELTPSYCH
00490      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTPSYCH
00491                       AND                                         ELTPSYCH
00492         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTPSYCH
00493         PERFORM 3000-INSTITUTIONAL-OP-RTNE THRU 3000-EXIT.        ELTPSYCH
00494                                                                   ELTPSYCH
00495      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTPSYCH
00496                       AND                                         ELTPSYCH
00497         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTPSYCH
00498         PERFORM 4000-PROFESSIONAL-OP-RTNE THRU 4000-EXIT.         ELTPSYCH
00499                                                                   ELTPSYCH
00500                                                                   ELTPSYCH
00501      IF (SSB-PROV-CLASS-INST OR                                   ELTPSYCH
00502          SSB-PROV-CLASS-BOTH OR                                   ELTPSYCH
00503          SSB-PROV-CLASS-PROF)                                     ELTPSYCH
00504                         AND                                       ELTPSYCH
00505         (SSB-SERV-CLASS-IP   OR                                   ELTPSYCH
00506          SSB-SERV-CLASS-OP   OR                                   ELTPSYCH
00507          SSB-SERV-CLASS-BOTH)                                     ELTPSYCH
00508            CONTINUE                                               ELTPSYCH
00509      ELSE                                                         ELTPSYCH
00510          SET CIA-AB-UNDEF TO TRUE                                 ELTPSYCH
00511          EXEC CICS ABEND                                          ELTPSYCH
00512                    ABCODE(CIA-ABCODE)                             ELTPSYCH
00513          END-EXEC                                                 ELTPSYCH
00514      END-IF.                                                      ELTPSYCH
00515                                                                   ELTPSYCH
00516      MOVE 'E'  TO  COF-FUNCTION.                                  ELTPSYCH
00517      MOVE ZERO  TO  COF-NBR-HDR-LINES,                            ELTPSYCH
00518                     COF-NBR-DTL-LINES.                            ELTPSYCH
00519      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
00520              END-EXEC.                                            ELTPSYCH
00521                                                                   ELTPSYCH
00522      EXEC CICS RETURN   END-EXEC.                                 ELTPSYCH
00523                                                                   ELTPSYCH
00524      GOBACK.                                                      ELTPSYCH
00525                                                                   ELTPSYCH
00526  0098-SIGNAL-UNALL-AREA-ERROR.                                    ELTPSYCH
00527      SET CIA-AB-UNALLOC-AREA TO TRUE.                             ELTPSYCH
00528                                                                   ELTPSYCH
00529      EXEC CICS ABEND                                              ELTPSYCH
00530                ABCODE (CIA-ABCODE)                                ELTPSYCH
00531      END-EXEC.                                                    ELTPSYCH
00532                                                                   ELTPSYCH
00533 /        I N S T I T U T I O N A L   I P   R T N E                ELTPSYCH
00534 ***************************************************************** ELTPSYCH
00535 *        I N S T I T U T I O N A L   I P   R T N E                ELTPSYCH
00536 *                                                                 ELTPSYCH
00537 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPSYCH
00538 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPSYCH
00539 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPSYCH
00540 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPSYCH
00541 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPSYCH
00542 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPSYCH
00543 *  MODULE.                                                        ELTPSYCH
00544 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPSYCH
00545 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPSYCH
00546 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPSYCH
00547 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPSYCH
00548 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPSYCH
00549 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPSYCH
00550 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPSYCH
00551 *                                                                 ELTPSYCH
00552 ***************************************************************** ELTPSYCH
00553  1000-INSTITUTIONAL-IP-RTNE.                                      ELTPSYCH
00554                                                                   ELTPSYCH
00555      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
00556      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTPSYCH
00557      MOVE ZERO TO   COF-NBR-DTL-LINES.                            ELTPSYCH
00558      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
00559      MOVE WS-HDR-2-INST-IP TO  COF-HDR-LINE(2).                   ELTPSYCH
00560      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
00561              END-EXEC.                                            ELTPSYCH
00562      INITIALIZE COF-DTL.                                          ELTPSYCH
00563                                                                   ELTPSYCH
00564      IF WS-VALID-INST-INPATIENT                                   ELTPSYCH
00565         ADD +1 TO WS-CIA                                          ELTPSYCH
00566         PERFORM 7300-DSPLY-NTWRK-UTIL-IND.                        ELTPSYCH
00567                                                                   ELTPSYCH
00568      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
00569      PERFORM WITH TEST BEFORE                                     ELTPSYCH
00570              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPSYCH
00571              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPSYCH
00572         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPSYCH
00573         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
00574         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
00575         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPSYCH
00576      END-PERFORM.                                                 ELTPSYCH
00577      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
00578                                                                   ELTPSYCH
00579      PERFORM WITH TEST BEFORE                                     ELTPSYCH
00580         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTPSYCH
00581         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTPSYCH
00582            SET PVN-BEN-PROVN-IDX TO WS-SUB                        ELTPSYCH
00583            MOVE WS-INST-IP-LIST (WS-SUB)                          ELTPSYCH
00584                       TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)    ELTPSYCH
00585            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTPSYCH
00586                           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)    ELTPSYCH
00587      END-PERFORM.                                                 ELTPSYCH
00588                                                                   ELTPSYCH
00589      MOVE 'PSYCHIATRIC SERVICES     '  TO  SSB-TOPIC-PHRASE.      ELTPSYCH
00590                                                                   ELTPSYCH
00591      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
00592          END-EXEC.                                                ELTPSYCH
00593                                                                   ELTPSYCH
00594      ADD  +1  TO  COF-NBR-DTL-LINES.                              ELTPSYCH
00595      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
00596              END-EXEC.                                            ELTPSYCH
00597      INITIALIZE COF-DTL.                                          ELTPSYCH
00598                                                                   ELTPSYCH
00599      IF PVN-COVG-NONE                                             ELTPSYCH
00600         GO TO 1000-EXIT.                                          ELTPSYCH
00601                                                                   ELTPSYCH
00602      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
00603      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTPSYCH
00604                                                                   ELTPSYCH
00605      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPSYCH
00606            PSP-PROVN-PRICING-METHD,                               ELTPSYCH
00607            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTPSYCH
00608            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTPSYCH
00609            PSP-TRANSF-OTHER-RESP-IND,                             ELTPSYCH
00610            PSP-SPILL-OVER-COINS-APL-IND,                          ELTPSYCH
00611            PSP-SPILL-OVER-DED-APL-IND,                            ELTPSYCH
00612            PSP-SPILL-OVR-RM-F-RT-APL-IND,                         ELTPSYCH
00613            PSP-CERTFN-REQRM-IND,                                  ELTPSYCH
00614            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTPSYCH
00615            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTPSYCH
00616            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTPSYCH
00617            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTPSYCH
00618            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTPSYCH
00619            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTPSYCH
00620            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTPSYCH
00621            PSA-DAYS-RDCN-RAT-IND,                                 ELTPSYCH
00622            PSA-DAYS-RDCN-RAT-BASIC-APL,                           ELTPSYCH
00623            PSA-DAYS-RDCN-RAT-BASIC-BASE,                          ELTPSYCH
00624            PSA-DAYS-RDCN-RAT-SEC-APL,                             ELTPSYCH
00625            PSA-DAYS-RDCN-RAT-SEC-BASE,                            ELTPSYCH
00626            PSA-FLAT-RATE-PDM-AMT,                                 ELTPSYCH
00627            PSB-PROF-CHRG-HSP-CLM,                                 ELTPSYCH
00628            PSW-ADDN-ALLOW-AMT-PER-DAY,                            ELTPSYCH
00629            PSW-DAYS-RDCN-RAT-IND,                                 ELTPSYCH
00630            PSW-DAYS-RDCN-RAT-BASIC-APL,                           ELTPSYCH
00631            PSW-DAYS-RDCN-RAT-BASIC-BASE,                          ELTPSYCH
00632            PSW-DAYS-RDCN-RAT-SEC-APL,                             ELTPSYCH
00633            PSW-DAYS-RDCN-RAT-SEC-BASE,                            ELTPSYCH
00634            PSW-FLAT-RATE-PDM-AMT.                                 ELTPSYCH
00635                                                                   ELTPSYCH
00636      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
00637              END-EXEC.                                            ELTPSYCH
00638                                                                   ELTPSYCH
00639      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPSYCH
00640      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
00641              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTPSYCH
00642                                                                   ELTPSYCH
00643      PERFORM WITH TEST BEFORE                                     ELTPSYCH
00644         VARYING WS-SUB  FROM  +1  BY  +1                          ELTPSYCH
00645         UNTIL WS-SUB  >  WS-INST-IP-CNT                           ELTPSYCH
00646              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTPSYCH
00647              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTPSYCH
00648                   PERFORM 1040-BUILD-SCREEN-LINES THRU 1040-EXIT  ELTPSYCH
00649              END-IF                                               ELTPSYCH
00650      END-PERFORM.                                                 ELTPSYCH
00651                                                                   ELTPSYCH
00652  1000-EXIT.  EXIT.                                                ELTPSYCH
00653                                                                   ELTPSYCH
00654  1040-BUILD-SCREEN-LINES.                                         ELTPSYCH
00655                                                                   ELTPSYCH
00656      SET PLT-INDEX1  TO                                           ELTPSYCH
00657                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTPSYCH
00658      IF WS-NOT-FIRST-TIME                                         ELTPSYCH
00659         MOVE 'P'  TO  COF-FUNCTION                                ELTPSYCH
00660         MOVE WS-CIA  TO  COF-NBR-DTL-LINES                        ELTPSYCH
00661         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPSYCH
00662             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
00663              END-EXEC                                             ELTPSYCH
00664         INITIALIZE COF-DTL                                        ELTPSYCH
00665      ELSE                                                         ELTPSYCH
00666         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTPSYCH
00667                                                                   ELTPSYCH
00668      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
00669      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
00670         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
00671            SET PLT-INDEX2  TO  2                                  ELTPSYCH
00672         ELSE                                                      ELTPSYCH
00673            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
00674            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTPSYCH
00675            GO TO 1040-EXIT                                        ELTPSYCH
00676      ELSE                                                         ELTPSYCH
00677         SET PLT-INDEX2  TO  1.                                    ELTPSYCH
00678                                                                   ELTPSYCH
00679 **---------------------------------------------------------------+ELTPSYCH
00680 **                                                               |ELTPSYCH
00681 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTPSYCH
00682      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTPSYCH
00683      ADD  +1  TO  WS-CIA.                                         ELTPSYCH
00684      MOVE ZERO  TO  WS-SUB2.                                      ELTPSYCH
00685      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTPSYCH
00686                                                                   ELTPSYCH
00687      MOVE WS-NO             TO WS-DISPLAY-DAY-PSYCH-TEXT,         ELTPSYCH
00688                                WS-DISPLAY-NIGHT-PSYCH-TEXT,       ELTPSYCH
00689                                WS-DISPLAY-B-FORMAT-TEXT,          ELTPSYCH
00690                                WS-DISPLAY-SHOCK-TEXT.             ELTPSYCH
00691                                                                   ELTPSYCH
00692      PERFORM 1050-ZERO-ALL-WITH-SAME-NO                           ELTPSYCH
00693         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTPSYCH
00694         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.                ELTPSYCH
00695      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPSYCH
00696                                                                   ELTPSYCH
00697      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTPSYCH
00698      MOVE +1  TO  WS-CIA                                          ELTPSYCH
00699      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
00700               END-EXEC.                                           ELTPSYCH
00701      INITIALIZE COF-DTL.                                          ELTPSYCH
00702 **                                                               |ELTPSYCH
00703 **---------------------------------------------------------------+ELTPSYCH
00704                                                                   ELTPSYCH
00705      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
00706      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00707        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00708               NOT = ZERO                                          ELTPSYCH
00709         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
00710         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
00711         ADD +1  TO  WS-CIA.                                       ELTPSYCH
00712                                                                   ELTPSYCH
00713      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
00714      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00715       AND  NOT WS-ADD-A-BLANK-LINE                                ELTPSYCH
00716        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00717               NOT = ZERO                                          ELTPSYCH
00718         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
00719         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
00720         ADD +1  TO  WS-CIA.                                       ELTPSYCH
00721                                                                   ELTPSYCH
00722      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00723             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTPSYCH
00724                                                                   ELTPSYCH
00725      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00726             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTPSYCH
00727                                                                   ELTPSYCH
00728      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
00729            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
00730            ADD  +1   TO  WS-CIA                                   ELTPSYCH
00731            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
00732                                                                   ELTPSYCH
00733 **---------------------------------------------------------------+ELTPSYCH
00734 **                                                               |ELTPSYCH
00735 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTPSYCH
00736 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTPSYCH
00737 **     A D D I T I O N A L   P R I C I N G   P E R C E N T   O R |ELTPSYCH
00738 **     F L A T  R A T E  P E R  D I E M                      O R |ELTPSYCH
00739 **     A D D I T I O N A L   A L L O W A N C E  A M O U N T      |ELTPSYCH
00740      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
00741      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
00742         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
00743                                                              '19' ELTPSYCH
00744         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
00745         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
00746         ADD +1  TO  WS-CIA.                                       ELTPSYCH
00747                                                                   ELTPSYCH
00748      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
00749      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
00750         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
00751                                                        '19' AND   ELTPSYCH
00752         NOT WS-ADD-A-BLANK-LINE                                   ELTPSYCH
00753         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
00754         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
00755         ADD +1  TO  WS-CIA.                                       ELTPSYCH
00756                                                                   ELTPSYCH
00757      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
00758      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
00759         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
00760                            AND                                    ELTPSYCH
00761         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00762         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
00763         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
00764                                                             ZERO  ELTPSYCH
00765            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
00766            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
00767            ADD +1  TO  WS-CIA.                                    ELTPSYCH
00768                                                                   ELTPSYCH
00769      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
00770      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
00771         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
00772                            AND                                    ELTPSYCH
00773         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPSYCH
00774         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPSYCH
00775         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTPSYCH
00776         ADD +1  TO  WS-CIA.                                       ELTPSYCH
00777                                                                   ELTPSYCH
00778      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPSYCH
00779         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00780         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
00781         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
00782                                                             ZERO  ELTPSYCH
00783            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
00784            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
00785            ADD +1  TO  WS-CIA.                                    ELTPSYCH
00786                                                                   ELTPSYCH
00787      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
00788      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00789         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
00790                                                            =  ZEROELTPSYCH
00791            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00792                                                            =  ZEROELTPSYCH
00793               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
00794            ELSE                                                   ELTPSYCH
00795               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
00796          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00797                                                  TO  WS-PERCENTAGEELTPSYCH
00798          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
00799         ELSE                                                      ELTPSYCH
00800          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTPSYCH
00801          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00802                                                 TO  WS-PERCENTAGE ELTPSYCH
00803          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
00804                                                                   ELTPSYCH
00805      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00806       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
00807        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00808                                                            =  ZEROELTPSYCH
00809        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00810                                                            =  ZEROELTPSYCH
00811         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
00812                                                            =  ZEROELTPSYCH
00813            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00814                                                            =  ZEROELTPSYCH
00815               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00816            ELSE                                                   ELTPSYCH
00817             MOVE                                                  ELTPSYCH
00818               PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00819                                                  TO  WS-PER-DIEM  ELTPSYCH
00820          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00821         ELSE                                                      ELTPSYCH
00822          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
00823                                                 TO  WS-ALLOW      ELTPSYCH
00824          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
00825                                                                   ELTPSYCH
00826      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00827       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
00828        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00829                                                            =  ZEROELTPSYCH
00830        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00831                                                            =  ZEROELTPSYCH
00832         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
00833                                                            =  ZEROELTPSYCH
00834            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00835                                                            =  ZEROELTPSYCH
00836               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00837            ELSE                                                   ELTPSYCH
00838             MOVE                                                  ELTPSYCH
00839               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00840                                                  TO  WS-PER-DIEM  ELTPSYCH
00841          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00842         ELSE                                                      ELTPSYCH
00843          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
00844                                                 TO  WS-ALLOW      ELTPSYCH
00845          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
00846                                                                   ELTPSYCH
00847      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
00848         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
00849                                             ZERO AND  NOT =  '19' ELTPSYCH
00850         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
00851         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
00852         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
00853                                                    CMF-CODE-VALUE ELTPSYCH
00854         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPSYCH
00855         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
00856         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
00857                                                                   ELTPSYCH
00858      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
00859      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00860         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPSYCH
00861                                                               ZEROELTPSYCH
00862            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00863                                                            =  ZEROELTPSYCH
00864               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
00865            ELSE                                                   ELTPSYCH
00866               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
00867          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00868                                                  TO  WS-PERCENTAGEELTPSYCH
00869          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
00870         ELSE                                                      ELTPSYCH
00871            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPSYCH
00872          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
00873                                                 TO  WS-PERCENTAGE ELTPSYCH
00874          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
00875                                                                   ELTPSYCH
00876      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00877       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
00878        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00879                                                            =  ZEROELTPSYCH
00880        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00881                                                            =  ZEROELTPSYCH
00882         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
00883                                                            =  ZEROELTPSYCH
00884            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00885                                                            =  ZEROELTPSYCH
00886               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00887            ELSE                                                   ELTPSYCH
00888             MOVE                                                  ELTPSYCH
00889               PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00890                                                  TO  WS-PER-DIEM  ELTPSYCH
00891          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00892         ELSE                                                      ELTPSYCH
00893          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
00894                                                 TO  WS-ALLOW      ELTPSYCH
00895          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
00896                                                                   ELTPSYCH
00897      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00898       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
00899        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00900                                                            =  ZEROELTPSYCH
00901        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
00902                                                            =  ZEROELTPSYCH
00903         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
00904                                                            =  ZEROELTPSYCH
00905            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00906                                                            =  ZEROELTPSYCH
00907               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00908            ELSE                                                   ELTPSYCH
00909             MOVE                                                  ELTPSYCH
00910               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00911                                                  TO  WS-PER-DIEM  ELTPSYCH
00912          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
00913         ELSE                                                      ELTPSYCH
00914          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
00915                                                 TO  WS-ALLOW      ELTPSYCH
00916          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
00917                                                                   ELTPSYCH
00918      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
00919         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
00920                                             ZERO AND  NOT =  '19' ELTPSYCH
00921         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
00922         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
00923         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
00924                                                    CMF-CODE-VALUE ELTPSYCH
00925         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPSYCH
00926         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
00927         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
00928                                                                   ELTPSYCH
00929      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
00930         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
00931         ADD  +1   TO  WS-CIA                                      ELTPSYCH
00932         PERFORM 8000-OUTPUT-TEXT                                  ELTPSYCH
00933      ELSE                                                         ELTPSYCH
00934       PERFORM 8000-OUTPUT-TEXT.                                   ELTPSYCH
00935 **                                                               |ELTPSYCH
00936 **---------------------------------------------------------------+ELTPSYCH
00937                                                                   ELTPSYCH
00938 **---------------------------------------------------------------+ELTPSYCH
00939 **                                                               |ELTPSYCH
00940 **   CERTIFICATION REQUIREMENT                                   |ELTPSYCH
00941                                                                   ELTPSYCH
00942      PERFORM 5140-CERTIFICATION THRU 5140-EXIT.                   ELTPSYCH
00943                                                                   ELTPSYCH
00944 **                                                               |ELTPSYCH
00945 **                                                               |ELTPSYCH
00946 **---------------------------------------------------------------+ELTPSYCH
00947                                                                   ELTPSYCH
00948 **---------------------------------------------------------------+ELTPSYCH
00949 **    P R O F E S S I O N A L   C H A R G E S                    |ELTPSYCH
00950 **       O N   H O S P I T A L   B I L L                         |ELTPSYCH
00951                                                                   ELTPSYCH
00952      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
00953      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00954       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
00955        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
00956                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
00957          MOVE WS-PROF-INPT-CHRGES TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
00958          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTPSYCH
00959          ADD  +1                  TO WS-CIA.                      ELTPSYCH
00960                                                                   ELTPSYCH
00961      SET  PLT-INDEX2          TO  2.                              ELTPSYCH
00962      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00963       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
00964            AND NOT WS-ADD-A-BLANK-LINE                            ELTPSYCH
00965        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
00966                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
00967          MOVE WS-PROF-INPT-CHRGES TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
00968          MOVE 'Y'                 TO WS-ADD-A-BLANK-IND           ELTPSYCH
00969          ADD  +1                  TO WS-CIA.                      ELTPSYCH
00970                                                                   ELTPSYCH
00971      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
00972      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
00973       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
00974        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
00975                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
00976          MOVE 'BPB'               TO  CMF-RECORD-PREFIX           ELTPSYCH
00977          MOVE 'PROF-CHRG-HSP-CLM' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
00978          MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00979                                   TO  CMF-CODE-VALUE              ELTPSYCH
00980          MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA         ELTPSYCH
00981          MOVE 63                    TO  WS-TEMP-NOT-USED-CNT      ELTPSYCH
00982          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.      ELTPSYCH
00983                                                                   ELTPSYCH
00984      SET  PLT-INDEX2          TO  2.                              ELTPSYCH
00985      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
00986       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
00987        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
00988                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
00989          MOVE 'BPB'               TO  CMF-RECORD-PREFIX           ELTPSYCH
00990          MOVE 'PROF-CHRG-HSP-CLM' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
00991          MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
00992                                   TO  CMF-CODE-VALUE              ELTPSYCH
00993          MOVE WS-SUPP-LIT         TO  WS-TEMP-TEXT-AREA           ELTPSYCH
00994          MOVE 63                  TO  WS-TEMP-NOT-USED-CNT        ELTPSYCH
00995          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.      ELTPSYCH
00996                                                                   ELTPSYCH
00997      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
00998            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
00999            ADD  +1   TO  WS-CIA                                   ELTPSYCH
01000            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
01001                                                                   ELTPSYCH
01002 **                                                               |ELTPSYCH
01003 **---------------------------------------------------------------+ELTPSYCH
01004                                                                   ELTPSYCH
01005 **---------------------------------------------------------------+ELTPSYCH
01006 **                                                               |ELTPSYCH
01007 **   M A X I M U M  N U M B E R   O F  L E A V E S               |ELTPSYCH
01008      IF GCG-MAX-LEAVES-ADM NOT = ZEROS                            ELTPSYCH
01009          ADD  +1                 TO WS-CIA                        ELTPSYCH
01010          MOVE GCG-MAX-LEAVES-ADM TO WS-DTL-MAX-LEAVES             ELTPSYCH
01011          MOVE WS-MAX-LEAVES      TO COF-DTL-LINE(WS-CIA)          ELTPSYCH
01012          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01013 **                                                               |ELTPSYCH
01014 **---------------------------------------------------------------+ELTPSYCH
01015                                                                   ELTPSYCH
01016      PERFORM 5200-DAY-NIGHT-PSYCH.                                ELTPSYCH
01017                                                                   ELTPSYCH
01018 **---------------------------------------------------------------+ELTPSYCH
01019 **                                                               |ELTPSYCH
01020 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTPSYCH
01021                                                                   ELTPSYCH
01022      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
01023      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01024       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
01025        IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01026                     NOT = '0'                                     ELTPSYCH
01027         IF PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01028                     NOT = LOW-VALUES                              ELTPSYCH
01029          ADD +1                   TO WS-CIA                       ELTPSYCH
01030          MOVE 'BPA'               TO  CMF-RECORD-PREFIX           ELTPSYCH
01031          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
01032          MOVE PLA-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01033                                   TO  CMF-CODE-VALUE              ELTPSYCH
01034          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTPSYCH
01035          MOVE ZEROS   TO  WS-TEMP-NOT-USED-CNT                    ELTPSYCH
01036          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
01037          ADD +1 TO  WS-CIA                                        ELTPSYCH
01038          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01039                                                                   ELTPSYCH
01040      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01041       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
01042        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01043                     NOT = ZEROS  AND                              ELTPSYCH
01044            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01045                      NOT = ZEROS                                  ELTPSYCH
01046             MOVE                                                  ELTPSYCH
01047              PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTPSYCH
01048                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
01049             MOVE                                                  ELTPSYCH
01050              PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01051                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
01052             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
01053             STRING WS-DAYS-REDUCED,                               ELTPSYCH
01054                    WS-BASIC-LIT,                                  ELTPSYCH
01055                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
01056                    WS-FOR, ' '                                    ELTPSYCH
01057                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
01058                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
01059             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
01060             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
01061             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
01062             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
01063             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
01064             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
01065             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
01066                                                                   ELTPSYCH
01067      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01068       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
01069        IF  PLA-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01070                     NOT = ZEROS  AND                              ELTPSYCH
01071            PLA-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01072                     NOT  = ZEROS                                  ELTPSYCH
01073                              ADD +1  TO  WS-CIA.                  ELTPSYCH
01074                                                                   ELTPSYCH
01075      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01076       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
01077        IF  PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
01078                     NOT = ZEROS  AND                              ELTPSYCH
01079            PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
01080                      NOT = ZEROS                                  ELTPSYCH
01081             MOVE                                                  ELTPSYCH
01082              PLA-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01083                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
01084             MOVE                                                  ELTPSYCH
01085              PLA-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01086                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
01087             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
01088             STRING WS-DAYS-REDUCED,                               ELTPSYCH
01089                    WS-SECONDARY,                                  ELTPSYCH
01090                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
01091                    WS-FOR, ' '                                    ELTPSYCH
01092                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
01093                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
01094             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
01095             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
01096             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
01097             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
01098             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
01099             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
01100             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
01101                                                                   ELTPSYCH
01102      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
01103          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTPSYCH
01104          ADD +1   TO WS-CIA                                       ELTPSYCH
01105          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01106                                                                   ELTPSYCH
01107      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01108       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
01109        IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01110                     NOT = '0'                                     ELTPSYCH
01111         IF PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01112                     NOT = LOW-VALUES                              ELTPSYCH
01113          ADD +1                   TO WS-CIA                       ELTPSYCH
01114          MOVE 'BPW'               TO  CMF-RECORD-PREFIX           ELTPSYCH
01115          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
01116          MOVE PLW-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01117                                   TO  CMF-CODE-VALUE              ELTPSYCH
01118          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTPSYCH
01119          MOVE ZEROS   TO  WS-TEMP-NOT-USED-CNT                    ELTPSYCH
01120          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
01121          ADD +1 TO  WS-CIA                                        ELTPSYCH
01122          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01123                                                                   ELTPSYCH
01124      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01125       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
01126        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01127                     NOT = ZEROS  AND                              ELTPSYCH
01128            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01129                      NOT = ZEROS                                  ELTPSYCH
01130             MOVE                                                  ELTPSYCH
01131              PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTPSYCH
01132                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
01133             MOVE                                                  ELTPSYCH
01134              PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01135                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
01136             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
01137             STRING WS-DAYS-REDUCED,                               ELTPSYCH
01138                    WS-BASIC-LIT,                                  ELTPSYCH
01139                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
01140                    WS-FOR, ' '                                    ELTPSYCH
01141                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
01142                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
01143             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
01144             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
01145             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
01146             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
01147             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
01148             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
01149             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
01150                                                                   ELTPSYCH
01151      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01152       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
01153        IF  PLW-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01154                     NOT = ZEROS  AND                              ELTPSYCH
01155            PLW-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01156                     NOT  = ZEROS                                  ELTPSYCH
01157                              ADD +1  TO  WS-CIA.                  ELTPSYCH
01158                                                                   ELTPSYCH
01159      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01160       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
01161        IF  PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
01162                     NOT = ZEROS  AND                              ELTPSYCH
01163            PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
01164                      NOT = ZEROS                                  ELTPSYCH
01165             MOVE                                                  ELTPSYCH
01166              PLW-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01167                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
01168             MOVE                                                  ELTPSYCH
01169              PLW-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01170                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
01171             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
01172             STRING WS-DAYS-REDUCED,                               ELTPSYCH
01173                    WS-SECONDARY,                                  ELTPSYCH
01174                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
01175                    WS-FOR, ' '                                    ELTPSYCH
01176                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
01177                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
01178             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
01179             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
01180             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
01181             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
01182             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
01183             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
01184             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
01185                                                                   ELTPSYCH
01186      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
01187          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTPSYCH
01188          ADD +1   TO WS-CIA                                       ELTPSYCH
01189          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01190                                                                   ELTPSYCH
01191 **                                                               |ELTPSYCH
01192 **---------------------------------------------------------------+ELTPSYCH
01193                                                                   ELTPSYCH
01194      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
01195      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01196        PERFORM 7000-SPILLOVER-COINS                               ELTPSYCH
01197        PERFORM 7200-SPILLOVER-DEDUCT                              ELTPSYCH
01198        IF PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01199          NOT = '0' AND NOT = LOW-VALUES                           ELTPSYCH
01200           ADD +1  TO  WS-CIA                                      ELTPSYCH
01201           MOVE 'BP' TO CMF-RECORD-PREFIX                          ELTPSYCH
01202           MOVE 'SPILL-OVR-RM-F-RT-APL-IND' TO                     ELTPSYCH
01203                        CMF-ELEMENT-SYSTEM-NAME                    ELTPSYCH
01204           MOVE                                                    ELTPSYCH
01205            PLP-SPILL-OVR-RM-F-RT-APL-IND(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01206             TO CMF-CODE-VALUE                                     ELTPSYCH
01207           MOVE WS-SPILLOVER-FL-RT-PER-D TO WS-TEMP-TEXT-AREA      ELTPSYCH
01208           MOVE 50 TO WS-TEMP-NOT-USED-CNT                         ELTPSYCH
01209           PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT      ELTPSYCH
01210           PERFORM 8000-OUTPUT-TEXT                                ELTPSYCH
01211      ELSE                                                         ELTPSYCH
01212       PERFORM 8000-OUTPUT-TEXT.                                   ELTPSYCH
01213                                                                   ELTPSYCH
01214      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
01215         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
01216            SET PLT-INDEX2  TO  2                                  ELTPSYCH
01217            PERFORM  7100-TRANS-OTHER-RESP-IND THRU                ELTPSYCH
01218                     7199-EXIT                                     ELTPSYCH
01219            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
01220         ELSE                                                      ELTPSYCH
01221            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
01222            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTPSYCH
01223            GO TO 1040-EXIT                                        ELTPSYCH
01224      ELSE                                                         ELTPSYCH
01225         SET PLT-INDEX2  TO  1                                     ELTPSYCH
01226         PERFORM  7100-TRANS-OTHER-RESP-IND                        ELTPSYCH
01227              THRU   7199-EXIT                                     ELTPSYCH
01228         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01229                                                                   ELTPSYCH
01230      PERFORM 6000-SCAN-TAB.                                       ELTPSYCH
01231                                                                   ELTPSYCH
01232  1040-EXIT.  EXIT.                                                ELTPSYCH
01233 /                                                                 ELTPSYCH
01234  1050-ZERO-ALL-WITH-SAME-NO.                                      ELTPSYCH
01235                                                                   ELTPSYCH
01236      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
01237         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'DPSY '                ELTPSYCH
01238               MOVE WS-YES TO WS-DISPLAY-DAY-PSYCH-TEXT.           ELTPSYCH
01239                                                                   ELTPSYCH
01240      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
01241         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'NPSY '                ELTPSYCH
01242               MOVE WS-YES TO WS-DISPLAY-NIGHT-PSYCH-TEXT.         ELTPSYCH
01243                                                                   ELTPSYCH
01244      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
01245        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                    ELTPSYCH
01246               MOVE WS-YES TO WS-DISPLAY-B-FORMAT-TEXT.            ELTPSYCH
01247                                                                   ELTPSYCH
01248      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
01249         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
01250         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTPSYCH
01251         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTPSYCH
01252                                                   CMF-CODE-VALUE  ELTPSYCH
01253         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTPSYCH
01254         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
01255         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
01256         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTPSYCH
01257         ADD  1  TO  WS-SUB2                                       ELTPSYCH
01258         IF WS-CIA  >  20 OR  =  20                                ELTPSYCH
01259            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTPSYCH
01260            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTPSYCH
01261                COMMAREA(DFHCOMMAREA)                              ELTPSYCH
01262                END-EXEC                                           ELTPSYCH
01263            INITIALIZE COF-DTL                                     ELTPSYCH
01264            MOVE +1  TO  WS-CIA.                                   ELTPSYCH
01265                                                                   ELTPSYCH
01266  1090-PROBLEM-WITH-INDICES.                                       ELTPSYCH
01267                                                                   ELTPSYCH
01268      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTPSYCH
01269      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPSYCH
01270                                                                   ELTPSYCH
01271      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
01272      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
01273                                                                   ELTPSYCH
01274      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
01275              END-EXEC.                                            ELTPSYCH
01276      INITIALIZE COF-DTL.                                          ELTPSYCH
01277                                                                   ELTPSYCH
01278 /        P R O F E S S I O N A L   I P   R T N E                  ELTPSYCH
01279 ***************************************************************** ELTPSYCH
01280 *        P R O F E S S I O N A L   I P   R T N E                  ELTPSYCH
01281 *                                                                 ELTPSYCH
01282 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPSYCH
01283 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPSYCH
01284 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPSYCH
01285 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPSYCH
01286 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPSYCH
01287 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPSYCH
01288 *  MODULE.                                                        ELTPSYCH
01289 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPSYCH
01290 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPSYCH
01291 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPSYCH
01292 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPSYCH
01293 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPSYCH
01294 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPSYCH
01295 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPSYCH
01296 *                                                                 ELTPSYCH
01297 ***************************************************************** ELTPSYCH
01298  2000-PROFESSIONAL-IP-RTNE.                                       ELTPSYCH
01299                                                                   ELTPSYCH
01300      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTPSYCH
01301      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
01302      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
01303      MOVE ZERO TO COF-NBR-DTL-LINES.                              ELTPSYCH
01304      MOVE WS-HDR-2-PROF-IP  TO  COF-HDR-LINE(2).                  ELTPSYCH
01305      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
01306              END-EXEC.                                            ELTPSYCH
01307      INITIALIZE COF-DTL.                                          ELTPSYCH
01308                                                                   ELTPSYCH
01309      IF WS-VALID-PROF-INPATIENT                                   ELTPSYCH
01310         ADD +1 TO WS-CIA                                          ELTPSYCH
01311         PERFORM 7300-DSPLY-NTWRK-UTIL-IND.                        ELTPSYCH
01312                                                                   ELTPSYCH
01313      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
01314      PERFORM WITH TEST BEFORE                                     ELTPSYCH
01315              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPSYCH
01316              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPSYCH
01317         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPSYCH
01318         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
01319         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
01320         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPSYCH
01321      END-PERFORM.                                                 ELTPSYCH
01322      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
01323                                                                   ELTPSYCH
01324      PERFORM WITH TEST BEFORE                                     ELTPSYCH
01325         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTPSYCH
01326         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTPSYCH
01327             SET PVN-BEN-PROVN-IDX TO WS-SUB                       ELTPSYCH
01328             MOVE WS-PROF-IP-LIST (WS-SUB)                         ELTPSYCH
01329                     TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)      ELTPSYCH
01330             MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)   ELTPSYCH
01331                            PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   ELTPSYCH
01332      END-PERFORM.                                                 ELTPSYCH
01333                                                                   ELTPSYCH
01334      MOVE 'PSYCHIATRIC SERVICES     '  TO  SSB-TOPIC-PHRASE.      ELTPSYCH
01335                                                                   ELTPSYCH
01336      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
01337          END-EXEC.                                                ELTPSYCH
01338                                                                   ELTPSYCH
01339      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTPSYCH
01340      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
01341               END-EXEC.                                           ELTPSYCH
01342      INITIALIZE COF-DTL.                                          ELTPSYCH
01343                                                                   ELTPSYCH
01344      IF PVN-COVG-NONE                                             ELTPSYCH
01345         GO TO 2000-EXIT.                                          ELTPSYCH
01346                                                                   ELTPSYCH
01347      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
01348      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTPSYCH
01349                                                                   ELTPSYCH
01350      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPSYCH
01351            PSP-PROVN-PRICING-METHD,                               ELTPSYCH
01352            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTPSYCH
01353            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTPSYCH
01354            PSP-TRANSF-OTHER-RESP-IND,                             ELTPSYCH
01355            PSP-SPILL-OVER-COINS-APL-IND,                          ELTPSYCH
01356            PSP-SPILL-OVER-DED-APL-IND,                            ELTPSYCH
01357            PSP-CERTFN-REQRM-IND,                                  ELTPSYCH
01358            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTPSYCH
01359            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTPSYCH
01360            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTPSYCH
01361            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTPSYCH
01362            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTPSYCH
01363            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTPSYCH
01364            PSC-BEN-SCOPE-ID,                                      ELTPSYCH
01365            PSD-BEN-SCOPE-ID,                                      ELTPSYCH
01366            PSD-DAYS-RDCN-RAT-IND,                                 ELTPSYCH
01367            PSD-DAYS-RDCN-RAT-BASIC-APL,                           ELTPSYCH
01368            PSD-DAYS-RDCN-RAT-BASIC-BASE,                          ELTPSYCH
01369            PSD-DAYS-RDCN-RAT-SEC-APL,                             ELTPSYCH
01370            PSD-DAYS-RDCN-RAT-SEC-BASE,                            ELTPSYCH
01371            PSD-FLAT-RATE-PDM-AMT,                                 ELTPSYCH
01372            PSD-MAX-AMT-PER-VISIT,                                 ELTPSYCH
01373            PSD-BEN-MAX-VISIT-IND,                                 ELTPSYCH
01374            PSD-BEN-MAX-VISIT-DAYS,                                ELTPSYCH
01375            PSE-BEN-SCOPE-ID,                                      ELTPSYCH
01376            PSE-MAX-AMT-PER-VISIT,                                 ELTPSYCH
01377            PSE-BEN-MAX-VISITS-IND,                                ELTPSYCH
01378            PSE-BEN-MAX-VISITS-DAYS.                               ELTPSYCH
01379                                                                   ELTPSYCH
01380                                                                   ELTPSYCH
01381      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
01382             END-EXEC.                                             ELTPSYCH
01383                                                                   ELTPSYCH
01384      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPSYCH
01385      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
01386              ADDRESS OF    PLT-PAYMENT-LEVEL-TABLE.               ELTPSYCH
01387                                                                   ELTPSYCH
01388      PERFORM WITH TEST BEFORE                                     ELTPSYCH
01389         VARYING WS-SUB  FROM  +1  BY  +1                          ELTPSYCH
01390         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTPSYCH
01391              SET PVN-BEN-PROVN-IDX TO WS-SUB                      ELTPSYCH
01392              IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO   ELTPSYCH
01393                   PERFORM 2040-BUILD-SCREEN-LINES THRU 2040-EXIT  ELTPSYCH
01394              END-IF                                               ELTPSYCH
01395      END-PERFORM.                                                 ELTPSYCH
01396  2000-EXIT.  EXIT.                                                ELTPSYCH
01397 /                                                                 ELTPSYCH
01398  2040-BUILD-SCREEN-LINES.                                         ELTPSYCH
01399                                                                   ELTPSYCH
01400      SET PLT-INDEX1   TO                                          ELTPSYCH
01401                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTPSYCH
01402      IF WS-NOT-FIRST-TIME                                         ELTPSYCH
01403         MOVE 'P'  TO  COF-FUNCTION                                ELTPSYCH
01404         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPSYCH
01405             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
01406              END-EXEC                                             ELTPSYCH
01407         INITIALIZE COF-DTL                                        ELTPSYCH
01408      ELSE                                                         ELTPSYCH
01409         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTPSYCH
01410                                                                   ELTPSYCH
01411      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
01412      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
01413         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
01414            SET PLT-INDEX2  TO  2                                  ELTPSYCH
01415         ELSE                                                      ELTPSYCH
01416            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
01417            PERFORM 2090-PROBLEM-WITH-INDICES                      ELTPSYCH
01418            GO TO 2040-EXIT                                        ELTPSYCH
01419      ELSE                                                         ELTPSYCH
01420         SET PLT-INDEX2  TO  1.                                    ELTPSYCH
01421                                                                   ELTPSYCH
01422                                                                   ELTPSYCH
01423 **---------------------------------------------------------------+ELTPSYCH
01424 **                                                               |ELTPSYCH
01425 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTPSYCH
01426      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTPSYCH
01427      ADD  +1  TO  WS-CIA.                                         ELTPSYCH
01428      MOVE ZERO  TO  WS-SUB2.                                      ELTPSYCH
01429      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTPSYCH
01430                                                                   ELTPSYCH
01431      MOVE WS-NO             TO WS-DISPLAY-DAY-PSYCH-TEXT,         ELTPSYCH
01432                                WS-DISPLAY-NIGHT-PSYCH-TEXT,       ELTPSYCH
01433                                WS-DISPLAY-SHOCK-TEXT.             ELTPSYCH
01434                                                                   ELTPSYCH
01435      PERFORM 2050-ZERO-ALL-WITH-SAME-NO                           ELTPSYCH
01436         VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTPSYCH
01437         UNTIL PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.                 ELTPSYCH
01438      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPSYCH
01439                                                                   ELTPSYCH
01440      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTPSYCH
01441      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
01442               END-EXEC.                                           ELTPSYCH
01443      INITIALIZE COF-DTL.                                          ELTPSYCH
01444      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
01445 **                                                               |ELTPSYCH
01446 **---------------------------------------------------------------+ELTPSYCH
01447                                                                   ELTPSYCH
01448      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01449      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01450        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01451               NOT = ZERO                                          ELTPSYCH
01452         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
01453         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01454         ADD +1  TO  WS-CIA.                                       ELTPSYCH
01455                                                                   ELTPSYCH
01456      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
01457      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01458       AND  NOT WS-ADD-A-BLANK-LINE                                ELTPSYCH
01459        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01460               NOT = ZERO                                          ELTPSYCH
01461         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
01462         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01463         ADD +1  TO  WS-CIA.                                       ELTPSYCH
01464                                                                   ELTPSYCH
01465      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01466             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTPSYCH
01467                                                                   ELTPSYCH
01468      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01469             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTPSYCH
01470                                                                   ELTPSYCH
01471      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
01472            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
01473            ADD  +1   TO  WS-CIA                                   ELTPSYCH
01474            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
01475                                                                   ELTPSYCH
01476 **---------------------------------------------------------------+ELTPSYCH
01477 **                                                               |ELTPSYCH
01478 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTPSYCH
01479 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTPSYCH
01480 **     A D D I T I O N A L   P R I C I N G   P E R C E N T       |ELTPSYCH
01481      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01482      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
01483         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01484                                                              '19' ELTPSYCH
01485         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
01486         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01487         ADD +1  TO  WS-CIA.                                       ELTPSYCH
01488                                                                   ELTPSYCH
01489      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
01490      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
01491         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01492                                                        '19' AND   ELTPSYCH
01493         NOT WS-ADD-A-BLANK-LINE                                   ELTPSYCH
01494         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
01495         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01496         ADD +1  TO  WS-CIA.                                       ELTPSYCH
01497                                                                   ELTPSYCH
01498      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01499      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
01500         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
01501                            AND                                    ELTPSYCH
01502         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01503         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
01504         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
01505                                                             ZERO  ELTPSYCH
01506            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
01507            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
01508            ADD +1  TO  WS-CIA.                                    ELTPSYCH
01509                                                                   ELTPSYCH
01510      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01511      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
01512         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
01513                            AND                                    ELTPSYCH
01514         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPSYCH
01515         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPSYCH
01516         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTPSYCH
01517         ADD +1  TO  WS-CIA.                                       ELTPSYCH
01518                                                                   ELTPSYCH
01519      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPSYCH
01520         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01521         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
01522         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
01523                                                             ZERO  ELTPSYCH
01524            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
01525            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
01526            ADD +1  TO  WS-CIA.                                    ELTPSYCH
01527                                                                   ELTPSYCH
01528      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01529      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01530         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01531                                                            =  ZEROELTPSYCH
01532            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01533                                                            =  ZEROELTPSYCH
01534               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
01535            ELSE                                                   ELTPSYCH
01536               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
01537          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01538                                                  TO  WS-PERCENTAGEELTPSYCH
01539          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
01540         ELSE                                                      ELTPSYCH
01541          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTPSYCH
01542          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01543                                                 TO  WS-PERCENTAGE ELTPSYCH
01544          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
01545                                                                   ELTPSYCH
01546      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01547       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01548         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01549                                                            =  ZEROELTPSYCH
01550          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01551                                                            =  ZEROELTPSYCH
01552            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01553                                                            =  ZEROELTPSYCH
01554               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
01555            ELSE                                                   ELTPSYCH
01556             MOVE                                                  ELTPSYCH
01557               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01558                                                  TO  WS-PER-DIEM  ELTPSYCH
01559             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTPSYCH
01560                                                                   ELTPSYCH
01561      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
01562         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01563                                             ZERO AND  NOT =  '19' ELTPSYCH
01564         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
01565         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
01566         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
01567                                                    CMF-CODE-VALUE ELTPSYCH
01568         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPSYCH
01569         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
01570         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
01571                                                                   ELTPSYCH
01572      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
01573      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01574         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPSYCH
01575                                                               ZEROELTPSYCH
01576            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01577                                                            =  ZEROELTPSYCH
01578               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
01579            ELSE                                                   ELTPSYCH
01580               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
01581          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01582                                                  TO  WS-PERCENTAGEELTPSYCH
01583          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
01584         ELSE                                                      ELTPSYCH
01585            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPSYCH
01586          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
01587                                                 TO  WS-PERCENTAGE ELTPSYCH
01588          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
01589                                                                   ELTPSYCH
01590      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01591       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01592         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
01593                                                            =  ZEROELTPSYCH
01594          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
01595                                                            =  ZEROELTPSYCH
01596            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01597                                                            =  ZEROELTPSYCH
01598               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
01599            ELSE                                                   ELTPSYCH
01600             MOVE                                                  ELTPSYCH
01601               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01602                                                  TO  WS-PER-DIEM  ELTPSYCH
01603             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTPSYCH
01604                                                                   ELTPSYCH
01605      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
01606         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01607                                             ZERO AND  NOT =  '19' ELTPSYCH
01608         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
01609         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
01610         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
01611                                                    CMF-CODE-VALUE ELTPSYCH
01612         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPSYCH
01613         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
01614         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
01615                                                                   ELTPSYCH
01616      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
01617         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01618         ADD  +1   TO  WS-CIA                                      ELTPSYCH
01619         PERFORM 8000-OUTPUT-TEXT                                  ELTPSYCH
01620      ELSE                                                         ELTPSYCH
01621       PERFORM 8000-OUTPUT-TEXT.                                   ELTPSYCH
01622 **                                                               |ELTPSYCH
01623 **---------------------------------------------------------------+ELTPSYCH
01624                                                                   ELTPSYCH
01625 **---------------------------------------------------------------+ELTPSYCH
01626 **                                                               |ELTPSYCH
01627 **            B E N E F I T   S C O P E   I D                    |ELTPSYCH
01628      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01629      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01630         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
01631          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01632           AND PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01633                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01634               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01635               ADD  +1  TO  WS-CIA                                 ELTPSYCH
01636               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTPSYCH
01637                                                                   ELTPSYCH
01638      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01639         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
01640          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01641           AND  PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01642                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01643               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01644               ADD  +1  TO  WS-CIA                                 ELTPSYCH
01645               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND.                   ELTPSYCH
01646                                                                   ELTPSYCH
01647      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01648         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
01649          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01650           AND PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01651                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01652               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01653               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
01654               ADD  +1  TO  WS-CIA.                                ELTPSYCH
01655                                                                   ELTPSYCH
01656      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
01657      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01658         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
01659          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01660           AND PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01661                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01662               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01663               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
01664               ADD  +1  TO  WS-CIA.                                ELTPSYCH
01665                                                                   ELTPSYCH
01666      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01667         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
01668          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01669            AND PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01670                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01671               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01672               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
01673               ADD  +1  TO  WS-CIA.                                ELTPSYCH
01674                                                                   ELTPSYCH
01675      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01676         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
01677          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
01678           AND PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01679                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01680               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
01681               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
01682               ADD  +1  TO  WS-CIA.                                ELTPSYCH
01683                                                                   ELTPSYCH
01684      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
01685      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01686         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
01687            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01688                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01689               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01690               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01691               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01692                                                    CMF-CODE-VALUE ELTPSYCH
01693               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
01694               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01695               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01696                                                                   ELTPSYCH
01697      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01698         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
01699             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01700                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01701               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01702               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01703               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01704                                                    CMF-CODE-VALUE ELTPSYCH
01705               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
01706               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01707               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01708                                                                   ELTPSYCH
01709      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01710         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
01711            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01712                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01713               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01714               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01715               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01716                                                    CMF-CODE-VALUE ELTPSYCH
01717               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
01718               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01719               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01720                                                                   ELTPSYCH
01721      SET PLT-INDEX2  TO  2.                                       ELTPSYCH
01722      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01723         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
01724            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01725                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01726               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01727               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01728               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01729                                                    CMF-CODE-VALUE ELTPSYCH
01730               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
01731               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01732               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01733                                                                   ELTPSYCH
01734      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01735         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
01736             IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
01737                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01738               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01739               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01740               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01741                                                    CMF-CODE-VALUE ELTPSYCH
01742               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
01743               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01744               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01745                                                                   ELTPSYCH
01746      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01747         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
01748            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
01749                                         '0000' AND  NOT =  '00  ' ELTPSYCH
01750               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
01751               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01752               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01753                                                    CMF-CODE-VALUE ELTPSYCH
01754               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
01755               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
01756               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
01757                                                                   ELTPSYCH
01758      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
01759         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
01760         ADD  +1   TO  WS-CIA                                      ELTPSYCH
01761         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01762 **                                                               |ELTPSYCH
01763 **---------------------------------------------------------------+ELTPSYCH
01764                                                                   ELTPSYCH
01765                                                                   ELTPSYCH
01766      PERFORM 5200-DAY-NIGHT-PSYCH.                                ELTPSYCH
01767                                                                   ELTPSYCH
01768 **---------------------------------------------------------------+ELTPSYCH
01769 **    M  A  X   V  I  S  I  T  S                                 |ELTPSYCH
01770                                                                   ELTPSYCH
01771      MOVE WS-NO TO WS-DISPLAY-MAX-AMT-TEXT                        ELTPSYCH
01772                    WS-DISPLAY-MAX-VISITS-TEXT.                    ELTPSYCH
01773                                                                   ELTPSYCH
01774      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
01775      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01776       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTPSYCH
01777        AND NOT = LOW-VALUES)                                      ELTPSYCH
01778                          OR                                       ELTPSYCH
01779       (PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'   ELTPSYCH
01780        AND NOT = LOW-VALUES)                                      ELTPSYCH
01781          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTPSYCH
01782                                                                   ELTPSYCH
01783      SET  PLT-INDEX2 TO  2.                                       ELTPSYCH
01784      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01785       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTPSYCH
01786        AND NOT = LOW-VALUES)                                      ELTPSYCH
01787                          OR                                       ELTPSYCH
01788       (PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'   ELTPSYCH
01789        AND NOT = LOW-VALUES)                                      ELTPSYCH
01790          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTPSYCH
01791                                                                   ELTPSYCH
01792      IF WS-DISPLAY-MAX-VISITS-TEXT = WS-YES                       ELTPSYCH
01793          ADD +1             TO WS-CIA                             ELTPSYCH
01794          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTPSYCH
01795          ADD +1             TO WS-CIA.                            ELTPSYCH
01796                                                                   ELTPSYCH
01797      SET  PLT-INDEX2           TO  1.                             ELTPSYCH
01798      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01799       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01800        IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01801                  NOT = ZEROS                                      ELTPSYCH
01802         MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
01803                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
01804         MOVE SPACES   TO TCAR-FROM-AREA                           ELTPSYCH
01805         STRING WS-BASIC-LIT ' '                                   ELTPSYCH
01806                WS-DTL-MAX-DAYS ' '                                ELTPSYCH
01807                WS-PER ' '                                         ELTPSYCH
01808                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTPSYCH
01809         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTPSYCH
01810         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTPSYCH
01811         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
01812         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
01813         PERFORM TCPR-000-TEXT-UNSTRING                            ELTPSYCH
01814         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTPSYCH
01815         MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO    ELTPSYCH
01816                                                 CMF-CODE-VALUE    ELTPSYCH
01817         MOVE 'BPD'                TO  CMF-RECORD-PREFIX           ELTPSYCH
01818         MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME      ELTPSYCH
01819         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
01820         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01821                                                                   ELTPSYCH
01822      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01823       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01824        IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
01825                  NOT = ZEROS                                      ELTPSYCH
01826         MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
01827                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
01828         MOVE SPACES   TO TCAR-FROM-AREA                           ELTPSYCH
01829         STRING WS-BASIC-LIT ' '                                   ELTPSYCH
01830                WS-DTL-MAX-DAYS ' '                                ELTPSYCH
01831                WS-PER ' '                                         ELTPSYCH
01832                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTPSYCH
01833         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTPSYCH
01834         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTPSYCH
01835         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
01836         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
01837         PERFORM TCPR-000-TEXT-UNSTRING                            ELTPSYCH
01838         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTPSYCH
01839         MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) TO    ELTPSYCH
01840                                                 CMF-CODE-VALUE    ELTPSYCH
01841         MOVE 'BPE'                TO  CMF-RECORD-PREFIX           ELTPSYCH
01842         MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
01843         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
01844         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01845                                                                   ELTPSYCH
01846      SET  PLT-INDEX2           TO  2.                             ELTPSYCH
01847      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01848       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01849         IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
01850                NOT = ZEROS                                        ELTPSYCH
01851          MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
01852                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
01853          MOVE SPACES   TO TCAR-FROM-AREA                          ELTPSYCH
01854          STRING WS-SUPP-LIT ' '                                   ELTPSYCH
01855                 WS-DTL-MAX-DAYS                                   ELTPSYCH
01856                 WS-PER ' '                                        ELTPSYCH
01857                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTPSYCH
01858          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTPSYCH
01859          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTPSYCH
01860          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
01861          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
01862          PERFORM TCPR-000-TEXT-UNSTRING                           ELTPSYCH
01863          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTPSYCH
01864          MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
01865                                                 CMF-CODE-VALUE    ELTPSYCH
01866          MOVE 'BPD'                TO  CMF-RECORD-PREFIX          ELTPSYCH
01867          MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
01868          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
01869          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01870                                                                   ELTPSYCH
01871      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01872       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01873         IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
01874                NOT = ZEROS                                        ELTPSYCH
01875          MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
01876                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
01877          MOVE SPACES   TO TCAR-FROM-AREA                          ELTPSYCH
01878          STRING WS-SUPP-LIT ' '                                   ELTPSYCH
01879                 WS-DTL-MAX-DAYS                                   ELTPSYCH
01880                 WS-PER ' '                                        ELTPSYCH
01881                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTPSYCH
01882          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTPSYCH
01883          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTPSYCH
01884          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
01885          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
01886          PERFORM TCPR-000-TEXT-UNSTRING                           ELTPSYCH
01887          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTPSYCH
01888          MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) TO   ELTPSYCH
01889                                                 CMF-CODE-VALUE    ELTPSYCH
01890          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTPSYCH
01891          MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
01892          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
01893          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
01894                                                                   ELTPSYCH
01895 **                                                               |ELTPSYCH
01896 **---------------------------------------------------------------+ELTPSYCH
01897                                                                   ELTPSYCH
01898 **---------------------------------------------------------------+ELTPSYCH
01899 **    M  A  X   A  M  O  U  N  T   P  E  R   V  I  S  I  T       |ELTPSYCH
01900      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
01901      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01902       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01903         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01904                NOT = ZEROS                                        ELTPSYCH
01905            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
01906                                                                   ELTPSYCH
01907      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01908       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01909         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01910                NOT = ZEROS                                        ELTPSYCH
01911            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
01912                                                                   ELTPSYCH
01913      SET  PLT-INDEX2 TO  2.                                       ELTPSYCH
01914      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01915       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01916         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01917                NOT = ZEROS                                        ELTPSYCH
01918            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
01919                                                                   ELTPSYCH
01920      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01921       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01922         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
01923                NOT = ZEROS                                        ELTPSYCH
01924            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
01925                                                                   ELTPSYCH
01926      IF WS-DISPLAY-MAX-AMT-TEXT = WS-YES                          ELTPSYCH
01927          ADD +1             TO WS-CIA                             ELTPSYCH
01928          MOVE WS-MAX-AMOUNT TO COF-DTL-LINE(WS-CIA)               ELTPSYCH
01929          ADD +1             TO WS-CIA.                            ELTPSYCH
01930                                                                   ELTPSYCH
01931      SET  PLT-INDEX2           TO  1.                             ELTPSYCH
01932      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01933       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01934        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01935                  NOT = ZEROS                                      ELTPSYCH
01936         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
01937                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
01938         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTPSYCH
01939         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
01940         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01941                                                                   ELTPSYCH
01942      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
01943       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01944        IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01945                  NOT = ZEROS                                      ELTPSYCH
01946         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
01947                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
01948         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTPSYCH
01949         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
01950         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01951                                                                   ELTPSYCH
01952      SET  PLT-INDEX2           TO  2.                             ELTPSYCH
01953      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01954       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
01955        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01956                  NOT = ZEROS                                      ELTPSYCH
01957         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
01958                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
01959         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTPSYCH
01960         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
01961         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01962                                                                   ELTPSYCH
01963      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
01964       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
01965        IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
01966                  NOT = ZEROS                                      ELTPSYCH
01967         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
01968                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
01969         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTPSYCH
01970         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
01971         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
01972 **                                                               |ELTPSYCH
01973 **---------------------------------------------------------------+ELTPSYCH
01974                                                                   ELTPSYCH
01975 **---------------------------------------------------------------+ELTPSYCH
01976 **                                                               |ELTPSYCH
01977 **   CERTIFICATION REQUIREMENT                                   |ELTPSYCH
01978                                                                   ELTPSYCH
01979      PERFORM 5140-CERTIFICATION THRU 5140-EXIT.                   ELTPSYCH
01980                                                                   ELTPSYCH
01981 **                                                               |ELTPSYCH
01982 **                                                               |ELTPSYCH
01983 **---------------------------------------------------------------+ELTPSYCH
01984                                                                   ELTPSYCH
01985                                                                   ELTPSYCH
01986      INITIALIZE WS-BASIC-CONTRACT                                 ELTPSYCH
01987                 WS-SUPP-CONTRACT.                                 ELTPSYCH
01988                                                                   ELTPSYCH
01989      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTPSYCH
01990      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
01991                          ADDRESS OF CONTRACT-RECORD.              ELTPSYCH
01992      IF CIA-RC-OK                                                 ELTPSYCH
01993         SET WS-BASIC-PRESENT TO TRUE.                             ELTPSYCH
01994                                                                   ELTPSYCH
01995      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTPSYCH
01996      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
01997                          ADDRESS OF CONTRACT-RECORD.              ELTPSYCH
01998      IF CIA-RC-OK                                                 ELTPSYCH
01999         SET WS-SUPP-PRESENT TO TRUE.                              ELTPSYCH
02000                                                                   ELTPSYCH
02001      IF WS-DISPLAY-SHOCK-TEXT = WS-YES                            ELTPSYCH
02002        IF NOT WS-BASIC-PRESENT AND                                ELTPSYCH
02003           NOT WS-SUPP-PRESENT                                     ELTPSYCH
02004             ADD +1 TO WS-CIA                                      ELTPSYCH
02005             MOVE WS-NO-SAME-PROVIDER-ELECT TO                     ELTPSYCH
02006                                        COF-DTL-LINE (WS-CIA)      ELTPSYCH
02007             PERFORM 8000-OUTPUT-TEXT                              ELTPSYCH
02008        ELSE                                                       ELTPSYCH
02009            ADD +1 TO WS-CIA                                       ELTPSYCH
02010            MOVE WS-SAME-PROVIDER-ELECT TO COF-DTL-LINE (WS-CIA)   ELTPSYCH
02011            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
02012            IF WS-BASIC-PRESENT                                    ELTPSYCH
02013                SET CIA-ELSCONPB-DDN TO TRUE                       ELTPSYCH
02014                CALL 'ELUSETAD' USING DFHCOMMAREA                  ELTPSYCH
02015                                    ADDRESS OF CONTRACT-RECORD     ELTPSYCH
02016                PERFORM 5400-ELECT-SHK-THRPY-BASIC THRU            ELTPSYCH
02017                        5499-EXIT                                  ELTPSYCH
02018            END-IF                                                 ELTPSYCH
02019            IF WS-SUPP-PRESENT                                     ELTPSYCH
02020                SET CIA-ELSCONPS-DDN TO TRUE                       ELTPSYCH
02021                CALL 'ELUSETAD' USING DFHCOMMAREA                  ELTPSYCH
02022                                    ADDRESS OF CONTRACT-RECORD     ELTPSYCH
02023                PERFORM 5500-ELECT-SHK-THRPY-SUPP THRU             ELTPSYCH
02024                        5599-EXIT                                  ELTPSYCH
02025            END-IF                                                 ELTPSYCH
02026        END-IF                                                     ELTPSYCH
02027      END-IF.                                                      ELTPSYCH
02028 **---------------------------------------------------------------+ELTPSYCH
02029 **                                                               |ELTPSYCH
02030 **   D A Y S  R E D U C T I O N  R A T I O                       |ELTPSYCH
02031                                                                   ELTPSYCH
02032      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
02033      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02034       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
02035        IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
02036                     NOT = '0'                                     ELTPSYCH
02037         IF PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
02038                     NOT = LOW-VALUES                              ELTPSYCH
02039          ADD +1                   TO WS-CIA                       ELTPSYCH
02040          MOVE 'BPD'               TO  CMF-RECORD-PREFIX           ELTPSYCH
02041          MOVE 'DAYS-RDCN-RAT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
02042          MOVE PLD-DAYS-RDCN-RAT-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02043                                   TO  CMF-CODE-VALUE              ELTPSYCH
02044          MOVE SPACES  TO  WS-TEMP-TEXT-AREA                       ELTPSYCH
02045          MOVE ZEROS   TO  WS-TEMP-NOT-USED-CNT                    ELTPSYCH
02046          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
02047          ADD +1 TO  WS-CIA                                        ELTPSYCH
02048          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
02049                                                                   ELTPSYCH
02050      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02051       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
02052        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
02053                     NOT = ZEROS  AND                              ELTPSYCH
02054            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02055                      NOT = ZEROS                                  ELTPSYCH
02056             MOVE                                                  ELTPSYCH
02057              PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2) ELTPSYCH
02058                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
02059             MOVE                                                  ELTPSYCH
02060              PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02061                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
02062             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
02063             STRING WS-DAYS-REDUCED,                               ELTPSYCH
02064                    WS-BASIC-LIT,                                  ELTPSYCH
02065                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
02066                    WS-FOR, ' '                                    ELTPSYCH
02067                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
02068                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
02069             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
02070             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
02071             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
02072             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
02073             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
02074             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
02075             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
02076                                                                   ELTPSYCH
02077      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02078       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
02079        IF  PLD-DAYS-RDCN-RAT-BASIC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
02080                     NOT = ZEROS  AND                              ELTPSYCH
02081            PLD-DAYS-RDCN-RAT-BASIC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02082                     NOT  = ZEROS                                  ELTPSYCH
02083                              ADD +1  TO  WS-CIA.                  ELTPSYCH
02084                                                                   ELTPSYCH
02085      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02086       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
02087        IF  PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
02088                     NOT = ZEROS  AND                              ELTPSYCH
02089            PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)    ELTPSYCH
02090                      NOT = ZEROS                                  ELTPSYCH
02091             MOVE                                                  ELTPSYCH
02092              PLD-DAYS-RDCN-RAT-SEC-APL (PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
02093                    TO WS-DTL-DAYS-REDUCED-APL                     ELTPSYCH
02094             MOVE                                                  ELTPSYCH
02095              PLD-DAYS-RDCN-RAT-SEC-BASE (PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02096                    TO WS-DTL-DAYS-REDUCED-BASE                    ELTPSYCH
02097             MOVE SPACES          TO TCAR-FROM-AREA                ELTPSYCH
02098             STRING WS-DAYS-REDUCED,                               ELTPSYCH
02099                    WS-SECONDARY,                                  ELTPSYCH
02100                    WS-DTL-DAYS-REDUCED-APL, ' '                   ELTPSYCH
02101                    WS-FOR, ' '                                    ELTPSYCH
02102                    WS-DTL-DAYS-REDUCED-BASE,                      ELTPSYCH
02103                      DELIMITED BY SIZE INTO TCAR-FROM-AREA        ELTPSYCH
02104             PERFORM TCPR-000-TEXT-COMPRESSION                     ELTPSYCH
02105             MOVE +05             TO TCAR-OUTPUT-FIELD-COUNT       ELTPSYCH
02106             MOVE +79             TO TCAR-OUTPUT-FIELD-1-LEN       ELTPSYCH
02107             MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN       ELTPSYCH
02108             PERFORM TCPR-000-TEXT-UNSTRING                        ELTPSYCH
02109             MOVE TCAR-OPF-DATA(1) TO COF-DTL-LINE(WS-CIA)         ELTPSYCH
02110             MOVE WS-YES           TO WS-ADD-A-BLANK-IND.          ELTPSYCH
02111                                                                   ELTPSYCH
02112      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
02113          MOVE 'N' TO WS-ADD-A-BLANK-IND                           ELTPSYCH
02114          ADD +1   TO WS-CIA                                       ELTPSYCH
02115          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
02116                                                                   ELTPSYCH
02117 **                                                               |ELTPSYCH
02118 **---------------------------------------------------------------+ELTPSYCH
02119                                                                   ELTPSYCH
02120      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
02121      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
02122           PERFORM 7000-SPILLOVER-COINS                            ELTPSYCH
02123           PERFORM 7200-SPILLOVER-DEDUCT                           ELTPSYCH
02124           PERFORM 8000-OUTPUT-TEXT.                               ELTPSYCH
02125                                                                   ELTPSYCH
02126      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
02127         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
02128            SET PLT-INDEX2  TO  2                                  ELTPSYCH
02129            PERFORM  7100-TRANS-OTHER-RESP-IND                     ELTPSYCH
02130              THRU   7199-EXIT                                     ELTPSYCH
02131            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
02132         ELSE                                                      ELTPSYCH
02133            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
02134            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTPSYCH
02135            GO TO 1040-EXIT                                        ELTPSYCH
02136      ELSE                                                         ELTPSYCH
02137         SET PLT-INDEX2  TO  1                                     ELTPSYCH
02138         PERFORM  7100-TRANS-OTHER-RESP-IND                        ELTPSYCH
02139              THRU   7199-EXIT                                     ELTPSYCH
02140         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
02141                                                                   ELTPSYCH
02142      PERFORM 6000-SCAN-TAB.                                       ELTPSYCH
02143                                                                   ELTPSYCH
02144                                                                   ELTPSYCH
02145  2040-EXIT.  EXIT.                                                ELTPSYCH
02146 /                                                                 ELTPSYCH
02147  2050-ZERO-ALL-WITH-SAME-NO.                                      ELTPSYCH
02148                                                                   ELTPSYCH
02149      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02150         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'DPV  '                ELTPSYCH
02151               MOVE WS-YES TO WS-DISPLAY-DAY-PSYCH-TEXT.           ELTPSYCH
02152                                                                   ELTPSYCH
02153      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02154         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'NPV  '                ELTPSYCH
02155               MOVE WS-YES TO WS-DISPLAY-NIGHT-PSYCH-TEXT.         ELTPSYCH
02156                                                                   ELTPSYCH
02157      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02158        IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'ETAI ' OR 'ESTI '      ELTPSYCH
02159               MOVE WS-YES TO WS-DISPLAY-SHOCK-TEXT.               ELTPSYCH
02160                                                                   ELTPSYCH
02161      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02162         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
02163         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTPSYCH
02164         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTPSYCH
02165                                                    CMF-CODE-VALUE ELTPSYCH
02166         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTPSYCH
02167         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
02168         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
02169         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTPSYCH
02170         ADD  1  TO  WS-SUB2                                       ELTPSYCH
02171         IF WS-CIA  >  20 OR  =  20                                ELTPSYCH
02172            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTPSYCH
02173            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTPSYCH
02174                COMMAREA(DFHCOMMAREA)                              ELTPSYCH
02175                 END-EXEC                                          ELTPSYCH
02176            INITIALIZE COF-DTL                                     ELTPSYCH
02177            MOVE +1  TO  WS-CIA.                                   ELTPSYCH
02178                                                                   ELTPSYCH
02179  2090-PROBLEM-WITH-INDICES.                                       ELTPSYCH
02180                                                                   ELTPSYCH
02181      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTPSYCH
02182      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPSYCH
02183                                                                   ELTPSYCH
02184      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
02185      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
02186                                                                   ELTPSYCH
02187      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02188             END-EXEC.                                             ELTPSYCH
02189      INITIALIZE COF-DTL.                                          ELTPSYCH
02190                                                                   ELTPSYCH
02191  2099-EXIT.            EXIT.                                      ELTPSYCH
02192                                                                   ELTPSYCH
02193 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPSYCH
02194  2100-CALL-CODES-MANUAL-LONG.                                     ELTPSYCH
02195                                                                   ELTPSYCH
02196      INITIALIZE CMF-RETURN-CODE                                   ELTPSYCH
02197                 TCAR-FROM-AREA.                                   ELTPSYCH
02198                                                                   ELTPSYCH
02199      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTPSYCH
02200             END-EXEC.                                             ELTPSYCH
02201                                                                   ELTPSYCH
02202      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPSYCH
02203      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
02204                            ADDRESS OF CMF-DESCR.                  ELTPSYCH
02205                                                                   ELTPSYCH
02206      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPSYCH
02207         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPSYCH
02208         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTPSYCH
02209      ELSE                                                         ELTPSYCH
02210         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTPSYCH
02211                                                                   ELTPSYCH
02212      PERFORM 2140-MOVE-LINES-OUT THRU 2140-EXIT                   ELTPSYCH
02213          VARYING WS-SUB1 FROM 1 BY 1                              ELTPSYCH
02214          UNTIL WS-SUB1 > CMF-NBR-DESCR-LINES.                     ELTPSYCH
02215                                                                   ELTPSYCH
02216      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPSYCH
02217                                                                   ELTPSYCH
02218      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPSYCH
02219      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPSYCH
02220      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN                        ELTPSYCH
02221                    TCAR-OUTPUT-FIELD-3-LEN                        ELTPSYCH
02222                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPSYCH
02223      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPSYCH
02224                                                                   ELTPSYCH
02225      IF WS-MOVE-LINES-TO-CIA                                      ELTPSYCH
02226         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPSYCH
02227            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPSYCH
02228                                             WS-TEMP-NOT-USED-CNT  ELTPSYCH
02229            PERFORM  2150-CONCATENATE-TO-TEMP-TEXT                 ELTPSYCH
02230               VARYING  WS-SUB1  FROM  1  BY  1                    ELTPSYCH
02231               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTPSYCH
02232            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPSYCH
02233            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
02234            ADD +1  TO  WS-CIA                                     ELTPSYCH
02235         ELSE                                                      ELTPSYCH
02236            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
02237            ADD +1  TO  WS-CIA.                                    ELTPSYCH
02238                                                                   ELTPSYCH
02239      IF WS-MOVE-LINES-TO-CIA                                      ELTPSYCH
02240         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPSYCH
02241            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTPSYCH
02242               VARYING  WS-SUB1  FROM  2  BY  1                    ELTPSYCH
02243               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTPSYCH
02244         ELSE                                                      ELTPSYCH
02245            NEXT SENTENCE                                          ELTPSYCH
02246      ELSE                                                         ELTPSYCH
02247         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPSYCH
02248                                                                   ELTPSYCH
02249      GO TO 2199-EXIT.                                             ELTPSYCH
02250                                                                   ELTPSYCH
02251  2140-MOVE-LINES-OUT.                                             ELTPSYCH
02252      STRING TCAR-FROM-AREA DELIMITED BY '        '                ELTPSYCH
02253             ' ' DELIMITED BY SIZE                                 ELTPSYCH
02254             CMF-DESCR-LINE (WS-SUB1) DELIMITED BY SIZE            ELTPSYCH
02255      INTO TCAR-FROM-AREA.                                         ELTPSYCH
02256  2140-EXIT.  EXIT.                                                ELTPSYCH
02257                                                                   ELTPSYCH
02258                                                                   ELTPSYCH
02259  2150-CONCATENATE-TO-TEMP-TEXT.                                   ELTPSYCH
02260      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTPSYCH
02261      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTPSYCH
02262                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTPSYCH
02263                                                                   ELTPSYCH
02264  2199-EXIT.           EXIT.                                       ELTPSYCH
02265                                                                   ELTPSYCH
02266 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTPSYCH
02267  2200-CODES-MANUAL-WITH-AMOUNT.                                   ELTPSYCH
02268                                                                   ELTPSYCH
02269      INITIALIZE CMF-RETURN-CODE                                   ELTPSYCH
02270                 TCAR-FROM-AREA.                                   ELTPSYCH
02271                                                                   ELTPSYCH
02272      EXEC CICS  LINK  PROGRAM('ELUCMIF')   COMMAREA(DFHCOMMAREA)  ELTPSYCH
02273             END-EXEC.                                             ELTPSYCH
02274                                                                   ELTPSYCH
02275      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTPSYCH
02276      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
02277                            ADDRESS OF CMF-DESCR.                  ELTPSYCH
02278                                                                   ELTPSYCH
02279                                                                   ELTPSYCH
02280      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTPSYCH
02281         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTPSYCH
02282         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTPSYCH
02283            CMF-DESCR-LINE(1),        ' ',                         ELTPSYCH
02284            CMF-DESCR-LINE(2),        ' ',                         ELTPSYCH
02285            CMF-DESCR-LINE(3), ' ',        WS-PRCNT-PERDM-ALLOW    ELTPSYCH
02286            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTPSYCH
02287      ELSE                                                         ELTPSYCH
02288         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTPSYCH
02289         STRING CMF-DESCR-LINE(1),        ' ',                     ELTPSYCH
02290            CMF-DESCR-LINE(2),        ' ',                         ELTPSYCH
02291            CMF-DESCR-LINE(3),        ' ',  WS-PRCNT-PERDM-ALLOW   ELTPSYCH
02292            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTPSYCH
02293                                                                   ELTPSYCH
02294      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTPSYCH
02295                                                                   ELTPSYCH
02296      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTPSYCH
02297      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTPSYCH
02298      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTPSYCH
02299                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTPSYCH
02300                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTPSYCH
02301      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTPSYCH
02302                                                                   ELTPSYCH
02303      IF WS-MOVE-LINES-TO-CIA                                      ELTPSYCH
02304         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTPSYCH
02305            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTPSYCH
02306                                             WS-TEMP-NOT-USED-CNT  ELTPSYCH
02307            PERFORM  2250-CONCATENATE-TO-TEMP-TEXT                 ELTPSYCH
02308               VARYING  WS-SUB1  FROM  1  BY  1                    ELTPSYCH
02309               UNTIL  WS-TEMP-NOT-USED-CNT  >  +78                 ELTPSYCH
02310            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTPSYCH
02311            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
02312            ADD +1  TO  WS-CIA                                     ELTPSYCH
02313         ELSE                                                      ELTPSYCH
02314            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
02315            ADD +1  TO  WS-CIA.                                    ELTPSYCH
02316                                                                   ELTPSYCH
02317      IF WS-MOVE-LINES-TO-CIA                                      ELTPSYCH
02318         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTPSYCH
02319            PERFORM 2260-MOVE-LINES-TO-CIA                         ELTPSYCH
02320               VARYING  WS-SUB1  FROM  2  BY  1                    ELTPSYCH
02321               UNTIL  WS-SUB1  >  TCAR-OUTPUT-FIELDS-USED          ELTPSYCH
02322         ELSE                                                      ELTPSYCH
02323            NEXT SENTENCE                                          ELTPSYCH
02324      ELSE                                                         ELTPSYCH
02325         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTPSYCH
02326                                                                   ELTPSYCH
02327      GO TO 2299-EXIT.                                             ELTPSYCH
02328                                                                   ELTPSYCH
02329  2250-CONCATENATE-TO-TEMP-TEXT.                                   ELTPSYCH
02330      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTPSYCH
02331      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTPSYCH
02332                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTPSYCH
02333                                                                   ELTPSYCH
02334  2260-MOVE-LINES-TO-CIA.                                          ELTPSYCH
02335      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTPSYCH
02336      ADD +1  TO  WS-CIA.                                          ELTPSYCH
02337                                                                   ELTPSYCH
02338  2299-EXIT.           EXIT.                                       ELTPSYCH
02339                                                                   ELTPSYCH
02340 /            G E T   T A B U L A R   R E C O R D                  ELTPSYCH
02341 ***************************************************************** ELTPSYCH
02342 *            G E T   T A B U L A R   R E C O R D                  ELTPSYCH
02343 *                                                                 ELTPSYCH
02344 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTPSYCH
02345 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTPSYCH
02346 *  TO DISPLAY.                                                    ELTPSYCH
02347 *                                                                 ELTPSYCH
02348 ***************************************************************** ELTPSYCH
02349  2300-GET-TABULAR-RECORD.                                         ELTPSYCH
02350                                                                   ELTPSYCH
02351      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPSYCH
02352      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
02353              ADDRESS OF    IOP-INPUT-OUTPUT-PARAMETERS.           ELTPSYCH
02354                                                                   ELTPSYCH
02355      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTPSYCH
02356      SET CIA-GCTABULR-DDN TO TRUE.                                ELTPSYCH
02357      SET IOP-RD                          TO TRUE.                 ELTPSYCH
02358      SET IOP-FCQ-NONE                    TO TRUE.                 ELTPSYCH
02359      SET IOP-KVQ-NONE                    TO TRUE.                 ELTPSYCH
02360      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTPSYCH
02361                                                                   ELTPSYCH
02362      EXEC CICS LINK                                               ELTPSYCH
02363                PROGRAM ('ELUIOPGM')                               ELTPSYCH
02364                COMMAREA (DFHCOMMAREA)                             ELTPSYCH
02365      END-EXEC.                                                    ELTPSYCH
02366                                                                   ELTPSYCH
02367                                                                   ELTPSYCH
02368      IF IOP-RC-NOTFND                                             ELTPSYCH
02369         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTPSYCH
02370         EXEC CICS ABEND                                           ELTPSYCH
02371                   ABCODE(CIA-ABCODE)                              ELTPSYCH
02372         END-EXEC                                                  ELTPSYCH
02373      ELSE                                                         ELTPSYCH
02374          IF NOT IOP-RC-OK                                         ELTPSYCH
02375             SET CIA-AB-CRITIO TO TRUE                             ELTPSYCH
02376             EXEC CICS ABEND                                       ELTPSYCH
02377                       ABCODE(CIA-ABCODE)                          ELTPSYCH
02378             END-EXEC                                              ELTPSYCH
02379          END-IF                                                   ELTPSYCH
02380      END-IF.                                                      ELTPSYCH
02381  2399-EXIT.           EXIT.                                       ELTPSYCH
02382                                                                   ELTPSYCH
02383 /            I N S T I T U T I O N A L   O P   R T N E            ELTPSYCH
02384 ***************************************************************** ELTPSYCH
02385 *            I N S T I T U T I O N A L   O P   R T N E            ELTPSYCH
02386 *                                                                 ELTPSYCH
02387 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPSYCH
02388 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPSYCH
02389 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPSYCH
02390 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPSYCH
02391 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPSYCH
02392 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPSYCH
02393 *  MODULE.                                                        ELTPSYCH
02394 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPSYCH
02395 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPSYCH
02396 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPSYCH
02397 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPSYCH
02398 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPSYCH
02399 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPSYCH
02400 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPSYCH
02401 *                                                                 ELTPSYCH
02402 ***************************************************************** ELTPSYCH
02403  3000-INSTITUTIONAL-OP-RTNE.                                      ELTPSYCH
02404                                                                   ELTPSYCH
02405      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
02406      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTPSYCH
02407      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
02408      MOVE ZERO TO   COF-NBR-DTL-LINES.                            ELTPSYCH
02409      MOVE WS-HDR-2-INST-OP TO  COF-HDR-LINE(2).                   ELTPSYCH
02410      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02411              END-EXEC.                                            ELTPSYCH
02412      INITIALIZE COF-DTL.                                          ELTPSYCH
02413                                                                   ELTPSYCH
02414      IF WS-VALID-INST-OUTPATIENT                                  ELTPSYCH
02415         ADD +1 TO WS-CIA                                          ELTPSYCH
02416         PERFORM 7300-DSPLY-NTWRK-UTIL-IND.                        ELTPSYCH
02417                                                                   ELTPSYCH
02418      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
02419      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02420              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPSYCH
02421              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPSYCH
02422         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02423         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02424         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02425         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02426      END-PERFORM.                                                 ELTPSYCH
02427      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
02428                                                                   ELTPSYCH
02429      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02430         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTPSYCH
02431         UNTIL WS-SUB  >  WS-PROF-IP-CNT                           ELTPSYCH
02432            SET PVN-BEN-PROVN-IDX TO WS-SUB                        ELTPSYCH
02433            MOVE WS-INST-OP-LIST (WS-SUB)                          ELTPSYCH
02434                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTPSYCH
02435            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTPSYCH
02436                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)          ELTPSYCH
02437      END-PERFORM.                                                 ELTPSYCH
02438                                                                   ELTPSYCH
02439      MOVE 'PSYCHIATRIC SERVICES     '  TO  SSB-TOPIC-PHRASE.      ELTPSYCH
02440                                                                   ELTPSYCH
02441      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
02442          END-EXEC.                                                ELTPSYCH
02443                                                                   ELTPSYCH
02444      ADD +1  TO  COF-NBR-DTL-LINES.                               ELTPSYCH
02445      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02446              END-EXEC.                                            ELTPSYCH
02447      INITIALIZE COF-DTL.                                          ELTPSYCH
02448                                                                   ELTPSYCH
02449      IF PVN-COVG-NONE                                             ELTPSYCH
02450         GO TO 3000-EXIT.                                          ELTPSYCH
02451                                                                   ELTPSYCH
02452      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
02453      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTPSYCH
02454                                                                   ELTPSYCH
02455      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPSYCH
02456            PSP-PROVN-PRICING-METHD,                               ELTPSYCH
02457            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTPSYCH
02458            PSP-TRANSF-OTHER-RESP-IND,                             ELTPSYCH
02459            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTPSYCH
02460            PSP-SPILL-OVER-COINS-APL-IND,                          ELTPSYCH
02461            PSP-SPILL-OVER-DED-APL-IND,                            ELTPSYCH
02462            PSP-CERTFN-REQRM-IND,                                  ELTPSYCH
02463            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTPSYCH
02464            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTPSYCH
02465            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTPSYCH
02466            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTPSYCH
02467            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTPSYCH
02468            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTPSYCH
02469            PSA-ADDN-ALLOW-AMT-PER-DAY,                            ELTPSYCH
02470            PSA-FLAT-RATE-PDM-AMT,                                 ELTPSYCH
02471            PSB-PROF-CHRG-HSP-CLM,                                 ELTPSYCH
02472            PSW-ADDN-ALLOW-AMT-PER-DAY,                            ELTPSYCH
02473            PSW-FLAT-RATE-PDM-AMT.                                 ELTPSYCH
02474                                                                   ELTPSYCH
02475      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
02476             END-EXEC.                                             ELTPSYCH
02477                                                                   ELTPSYCH
02478                                                                   ELTPSYCH
02479      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPSYCH
02480      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
02481               ADDRESS OF   PLT-PAYMENT-LEVEL-TABLE.               ELTPSYCH
02482                                                                   ELTPSYCH
02483      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02484         VARYING WS-SUB  FROM  +1  BY  +1                          ELTPSYCH
02485         UNTIL WS-SUB  >  WS-INST-OP-CNT                           ELTPSYCH
02486             SET PVN-BEN-PROVN-IDX TO WS-SUB                       ELTPSYCH
02487             IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO    ELTPSYCH
02488                 PERFORM 3040-BUILD-SCREEN-LINES THRU 3040-EXIT    ELTPSYCH
02489             END-IF                                                ELTPSYCH
02490      END-PERFORM.                                                 ELTPSYCH
02491                                                                   ELTPSYCH
02492  3000-EXIT.  EXIT.                                                ELTPSYCH
02493 /                                                                 ELTPSYCH
02494  3040-BUILD-SCREEN-LINES.                                         ELTPSYCH
02495                                                                   ELTPSYCH
02496      SET PLT-INDEX1  TO                                           ELTPSYCH
02497                      PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).         ELTPSYCH
02498      IF WS-NOT-FIRST-TIME                                         ELTPSYCH
02499         MOVE 'P'  TO  COF-FUNCTION                                ELTPSYCH
02500         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPSYCH
02501             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
02502              END-EXEC                                             ELTPSYCH
02503         INITIALIZE COF-DTL                                        ELTPSYCH
02504      ELSE                                                         ELTPSYCH
02505         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTPSYCH
02506                                                                   ELTPSYCH
02507      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
02508      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
02509         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZEROES       ELTPSYCH
02510            SET PLT-INDEX2  TO  2                                  ELTPSYCH
02511         ELSE                                                      ELTPSYCH
02512            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
02513            PERFORM 3090-PROBLEM-WITH-INDICES                      ELTPSYCH
02514            GO TO 3040-EXIT                                        ELTPSYCH
02515      ELSE                                                         ELTPSYCH
02516         SET PLT-INDEX2  TO  1.                                    ELTPSYCH
02517                                                                   ELTPSYCH
02518                                                                   ELTPSYCH
02519 **---------------------------------------------------------------+ELTPSYCH
02520 **                                                               |ELTPSYCH
02521 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTPSYCH
02522      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTPSYCH
02523      ADD  +1  TO  WS-CIA.                                         ELTPSYCH
02524      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTPSYCH
02525      MOVE ZERO  TO  WS-SUB2.                                      ELTPSYCH
02526                                                                   ELTPSYCH
02527      MOVE WS-NO TO WS-DISPLAY-SHOCK-TEXT,                         ELTPSYCH
02528                    WS-DISPLAY-B-FORMAT-TEXT.                      ELTPSYCH
02529                                                                   ELTPSYCH
02530      PERFORM 3050-ZERO-ALL-WITH-SAME-NO                           ELTPSYCH
02531         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTPSYCH
02532         UNTIL  PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.                ELTPSYCH
02533      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPSYCH
02534                                                                   ELTPSYCH
02535      ADD  +1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                 ELTPSYCH
02536      MOVE 1  TO  WS-CIA.                                          ELTPSYCH
02537      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTPSYCH
02538             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
02539              END-EXEC.                                            ELTPSYCH
02540      INITIALIZE COF-DTL.                                          ELTPSYCH
02541 **                                                               |ELTPSYCH
02542 **---------------------------------------------------------------+ELTPSYCH
02543                                                                   ELTPSYCH
02544      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
02545      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02546        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02547               NOT = ZERO                                          ELTPSYCH
02548         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
02549         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
02550         ADD +1  TO  WS-CIA.                                       ELTPSYCH
02551                                                                   ELTPSYCH
02552      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
02553      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02554       AND  NOT WS-ADD-A-BLANK-LINE                                ELTPSYCH
02555        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02556               NOT = ZERO                                          ELTPSYCH
02557         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
02558         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
02559         ADD +1  TO  WS-CIA.                                       ELTPSYCH
02560                                                                   ELTPSYCH
02561      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02562             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTPSYCH
02563                                                                   ELTPSYCH
02564      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02565             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTPSYCH
02566                                                                   ELTPSYCH
02567      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
02568            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
02569            ADD  +1   TO  WS-CIA                                   ELTPSYCH
02570            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
02571                                                                   ELTPSYCH
02572 **---------------------------------------------------------------+ELTPSYCH
02573 **                                                               |ELTPSYCH
02574 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTPSYCH
02575 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTPSYCH
02576 **     A D D I T I O N A L   P R I C I N G   P E R C E N T   O R |ELTPSYCH
02577 **     F L A T  R A T E  P E R  D I E M                      O R |ELTPSYCH
02578 **     A D D I T I O N A L   A L L O W A N C E  A M O U N T      |ELTPSYCH
02579      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
02580      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
02581         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
02582                                                              '19' ELTPSYCH
02583         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
02584         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
02585         ADD +1  TO  WS-CIA.                                       ELTPSYCH
02586                                                                   ELTPSYCH
02587      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
02588      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPSYCH
02589         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
02590                                                        '19' AND   ELTPSYCH
02591         NOT WS-ADD-A-BLANK-LINE                                   ELTPSYCH
02592         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
02593         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
02594         ADD +1  TO  WS-CIA.                                       ELTPSYCH
02595                                                                   ELTPSYCH
02596      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
02597      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
02598         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
02599                            AND                                    ELTPSYCH
02600         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02601         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
02602         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
02603                                                             ZERO  ELTPSYCH
02604            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
02605            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
02606            ADD +1  TO  WS-CIA.                                    ELTPSYCH
02607                                                                   ELTPSYCH
02608      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
02609      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
02610         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
02611                            AND                                    ELTPSYCH
02612         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTPSYCH
02613         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPSYCH
02614         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTPSYCH
02615         ADD +1  TO  WS-CIA.                                       ELTPSYCH
02616                                                                   ELTPSYCH
02617      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPSYCH
02618         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02619         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
02620         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
02621                                                             ZERO  ELTPSYCH
02622            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
02623            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
02624            ADD +1  TO  WS-CIA.                                    ELTPSYCH
02625                                                                   ELTPSYCH
02626      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
02627      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02628         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
02629                                                            =  ZEROELTPSYCH
02630            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02631                                                            =  ZEROELTPSYCH
02632               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
02633            ELSE                                                   ELTPSYCH
02634               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
02635          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02636                                                  TO  WS-PERCENTAGEELTPSYCH
02637          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
02638         ELSE                                                      ELTPSYCH
02639          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTPSYCH
02640          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02641                                                 TO  WS-PERCENTAGE ELTPSYCH
02642          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
02643                                                                   ELTPSYCH
02644      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02645       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
02646         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
02647                                                            =  ZEROELTPSYCH
02648            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02649                                                            =  ZEROELTPSYCH
02650               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02651            ELSE                                                   ELTPSYCH
02652             MOVE                                                  ELTPSYCH
02653               PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02654                                                  TO  WS-PER-DIEM  ELTPSYCH
02655          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02656         ELSE                                                      ELTPSYCH
02657          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02658                                                 TO  WS-ALLOW      ELTPSYCH
02659          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
02660                                                                   ELTPSYCH
02661      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02662       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
02663         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
02664                                                            =  ZEROELTPSYCH
02665            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02666                                                            =  ZEROELTPSYCH
02667               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02668            ELSE                                                   ELTPSYCH
02669             MOVE                                                  ELTPSYCH
02670               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02671                                                  TO  WS-PER-DIEM  ELTPSYCH
02672          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02673         ELSE                                                      ELTPSYCH
02674          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02675                                                 TO  WS-ALLOW      ELTPSYCH
02676          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
02677                                                                   ELTPSYCH
02678      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
02679         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
02680                                             ZERO AND  NOT =  '19' ELTPSYCH
02681         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
02682         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
02683         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
02684                                                    CMF-CODE-VALUE ELTPSYCH
02685         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPSYCH
02686         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
02687         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
02688                                                                   ELTPSYCH
02689      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
02690      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02691         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPSYCH
02692                                                               ZEROELTPSYCH
02693            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02694                                                            =  ZEROELTPSYCH
02695               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTPSYCH
02696            ELSE                                                   ELTPSYCH
02697               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
02698          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02699                                                  TO  WS-PERCENTAGEELTPSYCH
02700          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTPSYCH
02701         ELSE                                                      ELTPSYCH
02702            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPSYCH
02703          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
02704                                                 TO  WS-PERCENTAGE ELTPSYCH
02705          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTPSYCH
02706                                                                   ELTPSYCH
02707      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02708       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTPSYCH
02709         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
02710                                                            =  ZEROELTPSYCH
02711            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02712                                                            =  ZEROELTPSYCH
02713               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02714            ELSE                                                   ELTPSYCH
02715             MOVE                                                  ELTPSYCH
02716               PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02717                                                  TO  WS-PER-DIEM  ELTPSYCH
02718          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02719         ELSE                                                      ELTPSYCH
02720          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02721                                                 TO  WS-ALLOW      ELTPSYCH
02722          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
02723                                                                   ELTPSYCH
02724      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02725       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'W'                     ELTPSYCH
02726         IF PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
02727                                                            =  ZEROELTPSYCH
02728            IF PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02729                                                            =  ZEROELTPSYCH
02730               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02731            ELSE                                                   ELTPSYCH
02732             MOVE                                                  ELTPSYCH
02733               PLW-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02734                                                  TO  WS-PER-DIEM  ELTPSYCH
02735          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTPSYCH
02736         ELSE                                                      ELTPSYCH
02737          MOVE PLW-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
02738                                                 TO  WS-ALLOW      ELTPSYCH
02739          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTPSYCH
02740                                                                   ELTPSYCH
02741      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTPSYCH
02742         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
02743                                             ZERO AND  NOT =  '19' ELTPSYCH
02744         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
02745         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
02746         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
02747                                                    CMF-CODE-VALUE ELTPSYCH
02748         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPSYCH
02749         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
02750         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
02751                                                                   ELTPSYCH
02752      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
02753         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
02754         ADD  +1   TO  WS-CIA                                      ELTPSYCH
02755         PERFORM 8000-OUTPUT-TEXT                                  ELTPSYCH
02756      ELSE                                                         ELTPSYCH
02757       PERFORM 8000-OUTPUT-TEXT.                                   ELTPSYCH
02758                                                                   ELTPSYCH
02759 **---------------------------------------------------------------+ELTPSYCH
02760 **                                                               |ELTPSYCH
02761 **   CERTIFICATION REQUIREMENT                                   |ELTPSYCH
02762                                                                   ELTPSYCH
02763      PERFORM 5140-CERTIFICATION THRU 5140-EXIT.                   ELTPSYCH
02764                                                                   ELTPSYCH
02765 **                                                               |ELTPSYCH
02766 **                                                               |ELTPSYCH
02767 **---------------------------------------------------------------+ELTPSYCH
02768                                                                   ELTPSYCH
02769 **---------------------------------------------------------------+ELTPSYCH
02770 **    P R O F E S S I O N A L   C H A R G E S                    |ELTPSYCH
02771 **       O N   H O S P I T A L   B I L L                         |ELTPSYCH
02772                                                                   ELTPSYCH
02773      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
02774      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02775       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
02776        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
02777                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
02778          MOVE WS-PROF-OUTPT-CHRGES TO COF-DTL-LINE(WS-CIA)        ELTPSYCH
02779          MOVE 'Y'                  TO WS-ADD-A-BLANK-IND          ELTPSYCH
02780          ADD  +1                   TO WS-CIA.                     ELTPSYCH
02781                                                                   ELTPSYCH
02782      SET  PLT-INDEX2          TO  2.                              ELTPSYCH
02783      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02784       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
02785            AND NOT WS-ADD-A-BLANK-LINE                            ELTPSYCH
02786        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
02787                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
02788          MOVE WS-PROF-OUTPT-CHRGES TO COF-DTL-LINE(WS-CIA)        ELTPSYCH
02789          MOVE 'Y'                  TO WS-ADD-A-BLANK-IND          ELTPSYCH
02790          ADD  +1                   TO WS-CIA.                     ELTPSYCH
02791                                                                   ELTPSYCH
02792      SET  PLT-INDEX2          TO  1.                              ELTPSYCH
02793      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
02794       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
02795        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
02796                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
02797          MOVE 'BPB'               TO  CMF-RECORD-PREFIX           ELTPSYCH
02798          MOVE 'PROF-CHRG-HSP-CLM' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
02799          MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02800                                   TO  CMF-CODE-VALUE              ELTPSYCH
02801          MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA         ELTPSYCH
02802          MOVE 63                    TO  WS-TEMP-NOT-USED-CNT      ELTPSYCH
02803          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
02804          ADD +1 TO  WS-CIA.                                       ELTPSYCH
02805                                                                   ELTPSYCH
02806      SET  PLT-INDEX2          TO  2.                              ELTPSYCH
02807      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02808       IF WS-DISPLAY-B-FORMAT-TEXT = 'Y'                           ELTPSYCH
02809        IF PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
02810                     NOT = '0' AND NOT = LOW-VALUES                ELTPSYCH
02811          MOVE 'BPB'               TO  CMF-RECORD-PREFIX           ELTPSYCH
02812          MOVE 'PROF-CHRG-HSP-CLM' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
02813          MOVE PLB-PROF-CHRG-HSP-CLM(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
02814                                   TO  CMF-CODE-VALUE              ELTPSYCH
02815          MOVE WS-SUPP-LIT         TO  WS-TEMP-TEXT-AREA           ELTPSYCH
02816          MOVE 63                  TO  WS-TEMP-NOT-USED-CNT        ELTPSYCH
02817          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
02818          ADD +1 TO  WS-CIA.                                       ELTPSYCH
02819                                                                   ELTPSYCH
02820      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
02821            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
02822            ADD  +1   TO  WS-CIA                                   ELTPSYCH
02823            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
02824                                                                   ELTPSYCH
02825 **                                                               |ELTPSYCH
02826 **---------------------------------------------------------------+ELTPSYCH
02827                                                                   ELTPSYCH
02828      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
02829      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTPSYCH
02830           PERFORM 7000-SPILLOVER-COINS                            ELTPSYCH
02831           PERFORM 7200-SPILLOVER-DEDUCT                           ELTPSYCH
02832           PERFORM 8000-OUTPUT-TEXT.                               ELTPSYCH
02833                                                                   ELTPSYCH
02834      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
02835         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
02836            SET PLT-INDEX2  TO  2                                  ELTPSYCH
02837            PERFORM  7100-TRANS-OTHER-RESP-IND                     ELTPSYCH
02838              THRU   7199-EXIT                                     ELTPSYCH
02839            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
02840         ELSE                                                      ELTPSYCH
02841            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
02842            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTPSYCH
02843            GO TO 1040-EXIT                                        ELTPSYCH
02844      ELSE                                                         ELTPSYCH
02845         SET PLT-INDEX2  TO  1                                     ELTPSYCH
02846         PERFORM  7100-TRANS-OTHER-RESP-IND                        ELTPSYCH
02847              THRU   7199-EXIT                                     ELTPSYCH
02848         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
02849                                                                   ELTPSYCH
02850      PERFORM 6000-SCAN-TAB.                                       ELTPSYCH
02851                                                                   ELTPSYCH
02852  3040-EXIT.  EXIT.                                                ELTPSYCH
02853 /                                                                 ELTPSYCH
02854  3050-ZERO-ALL-WITH-SAME-NO.                                      ELTPSYCH
02855                                                                   ELTPSYCH
02856      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02857        IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'B'                    ELTPSYCH
02858               MOVE WS-YES TO WS-DISPLAY-B-FORMAT-TEXT.            ELTPSYCH
02859                                                                   ELTPSYCH
02860      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
02861         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
02862         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTPSYCH
02863         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTPSYCH
02864                                                   CMF-CODE-VALUE  ELTPSYCH
02865         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTPSYCH
02866         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
02867         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
02868         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTPSYCH
02869         ADD  1  TO  WS-SUB2                                       ELTPSYCH
02870         IF WS-CIA  >  20 OR  =  20                                ELTPSYCH
02871            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTPSYCH
02872            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTPSYCH
02873                COMMAREA(DFHCOMMAREA)                              ELTPSYCH
02874                END-EXEC                                           ELTPSYCH
02875            INITIALIZE COF-DTL                                     ELTPSYCH
02876            MOVE +1  TO  WS-CIA.                                   ELTPSYCH
02877                                                                   ELTPSYCH
02878  3090-PROBLEM-WITH-INDICES.                                       ELTPSYCH
02879                                                                   ELTPSYCH
02880      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTPSYCH
02881      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPSYCH
02882                                                                   ELTPSYCH
02883      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
02884      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
02885                                                                   ELTPSYCH
02886      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02887              END-EXEC.                                            ELTPSYCH
02888      INITIALIZE COF-DTL.                                          ELTPSYCH
02889                                                                   ELTPSYCH
02890 /            P R O F E S S I O N A L   O P   R T N E              ELTPSYCH
02891 ***************************************************************** ELTPSYCH
02892 *            P R O F E S S I O N A L   O P   R T N E              ELTPSYCH
02893 *                                                                 ELTPSYCH
02894 *          THIS ROUTINE HAS A NUMBER OF STEPS.                    ELTPSYCH
02895 *  1. BUILD THE HEADING LINES AND MOVE THEM TO THE CIA.           ELTPSYCH
02896 *  2. SETUP THE BENEFIT PROVISION LIST PRIOR TO THE CALL TO THE   ELTPSYCH
02897 *  COVERAGE CHECKING MODULE.  IF THERE IS NO COVERAGE THEN SIGNAL ELTPSYCH
02898 *  END OF PAGE AND EXIT THIS ROUTINE.                             ELTPSYCH
02899 *  3. BUILD A LIST OF DESIRED FIELDS FOR THE PAYMENT GROUPING     ELTPSYCH
02900 *  MODULE.                                                        ELTPSYCH
02901 *  4. FIND THE FIRST NON-ZERO ENTRY IN THE LIST (ENTRIES WITH A   ELTPSYCH
02902 *  ZERO HAVE NO COVERAGE AND ARE NOT DISPLAYED).                  ELTPSYCH
02903 *  5. SHOW ALL THE BENEFIT PROVISION IDS THAT ARE EQUAL.          ELTPSYCH
02904 *  6. THEN ACCESS ALL THE REQUESTED FIELDS TRANSLATE THE CODE     ELTPSYCH
02905 *  VALUES TO ENGLISH AND BUILD DISPLAY LINES.                     ELTPSYCH
02906 *  7. STEPS 4 THROUGH 6 ARE REPEATED UNTIL ALL DISSIMILAR         ELTPSYCH
02907 *  BENEFITS HAVE BEEN DISPLAYED.                                  ELTPSYCH
02908 *                                                                 ELTPSYCH
02909 ***************************************************************** ELTPSYCH
02910  4000-PROFESSIONAL-OP-RTNE.                                       ELTPSYCH
02911                                                                   ELTPSYCH
02912      MOVE 'Y'  TO  WS-FIRSTTIME-IND.                              ELTPSYCH
02913      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
02914      MOVE ZERO TO   COF-NBR-DTL-LINES.                            ELTPSYCH
02915      MOVE +2  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
02916      MOVE WS-HDR-2-PROF-OP  TO  COF-HDR-LINE(2).                  ELTPSYCH
02917      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02918              END-EXEC.                                            ELTPSYCH
02919      INITIALIZE COF-DTL.                                          ELTPSYCH
02920                                                                   ELTPSYCH
02921      IF WS-VALID-PROF-OUTPATIENT                                  ELTPSYCH
02922         ADD +1 TO WS-CIA                                          ELTPSYCH
02923         PERFORM 7300-DSPLY-NTWRK-UTIL-IND.                        ELTPSYCH
02924                                                                   ELTPSYCH
02925      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
02926      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02927              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTPSYCH
02928              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTPSYCH
02929         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02930         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02931         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02932         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTPSYCH
02933      END-PERFORM.                                                 ELTPSYCH
02934      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTPSYCH
02935                                                                   ELTPSYCH
02936      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02937         VARYING  WS-SUB  FROM  +1  BY  +1                         ELTPSYCH
02938         UNTIL WS-SUB  >  WS-PROF-OP-CNT                           ELTPSYCH
02939            SET PVN-BEN-PROVN-IDX TO WS-SUB                        ELTPSYCH
02940            MOVE WS-PROF-OP-LIST (WS-SUB)                          ELTPSYCH
02941                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX)           ELTPSYCH
02942            MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)    ELTPSYCH
02943                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)          ELTPSYCH
02944      END-PERFORM.                                                 ELTPSYCH
02945                                                                   ELTPSYCH
02946                                                                   ELTPSYCH
02947      MOVE 'PSYCHIATRIC SERVICES     '  TO  SSB-TOPIC-PHRASE.      ELTPSYCH
02948                                                                   ELTPSYCH
02949      EXEC CICS  LINK  PROGRAM('ELGCOVER')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
02950          END-EXEC.                                                ELTPSYCH
02951                                                                   ELTPSYCH
02952      ADD +1  TO   COF-NBR-DTL-LINES.                              ELTPSYCH
02953      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
02954              END-EXEC.                                            ELTPSYCH
02955      INITIALIZE COF-DTL.                                          ELTPSYCH
02956                                                                   ELTPSYCH
02957      IF PVN-COVG-NONE                                             ELTPSYCH
02958         GO TO 4000-EXIT.                                          ELTPSYCH
02959                                                                   ELTPSYCH
02960      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
02961      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTPSYCH
02962                                                                   ELTPSYCH
02963      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTPSYCH
02964            PSP-PROVN-PRICING-METHD,                               ELTPSYCH
02965            PSP-ADDITIONAL-PRICING-PRCNT,                          ELTPSYCH
02966            PSP-TRANSF-OTHER-RESP-IND,                             ELTPSYCH
02967            PSP-VARIABLE-INDEMNITY-PRCNT,                          ELTPSYCH
02968            PSP-SPILL-OVER-COINS-APL-IND,                          ELTPSYCH
02969            PSP-SPILL-OVER-DED-APL-IND,                            ELTPSYCH
02970            PSP-CERTFN-REQRM-IND,                                  ELTPSYCH
02971            PSP-BEN-TAB-PROVN-ID-AAR,                              ELTPSYCH
02972            PSP-BEN-TAB-PROVN-ID-ABM,                              ELTPSYCH
02973            PSP-BEN-TAB-PROVN-ID-ACL,                              ELTPSYCH
02974            PSP-BEN-TAB-PROVN-ID-ADL,                              ELTPSYCH
02975            PSP-BEN-TAB-PROVN-ID-AOL,                              ELTPSYCH
02976            PSP-BEN-TAB-PROVN-ID-PPF,                              ELTPSYCH
02977            PSC-BEN-SCOPE-ID,                                      ELTPSYCH
02978            PSD-BEN-SCOPE-ID,                                      ELTPSYCH
02979            PSD-FLAT-RATE-PDM-AMT,                                 ELTPSYCH
02980            PSD-MAX-AMT-PER-VISIT,                                 ELTPSYCH
02981            PSD-BEN-MAX-VISIT-IND,                                 ELTPSYCH
02982            PSD-BEN-MAX-VISIT-DAYS,                                ELTPSYCH
02983            PSE-BEN-SCOPE-ID,                                      ELTPSYCH
02984            PSE-MAX-AMT-PER-VISIT,                                 ELTPSYCH
02985            PSE-BEN-MAX-VISITS-IND,                                ELTPSYCH
02986            PSE-BEN-MAX-VISITS-DAYS.                               ELTPSYCH
02987                                                                   ELTPSYCH
02988      EXEC CICS  LINK  PROGRAM('ELUPLGRP')  COMMAREA(DFHCOMMAREA)  ELTPSYCH
02989             END-EXEC.                                             ELTPSYCH
02990                                                                   ELTPSYCH
02991      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTPSYCH
02992      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
02993                ADDRESS OF  PLT-PAYMENT-LEVEL-TABLE.               ELTPSYCH
02994                                                                   ELTPSYCH
02995      PERFORM WITH TEST BEFORE                                     ELTPSYCH
02996         VARYING WS-SUB  FROM  +1  BY  +1                          ELTPSYCH
02997         UNTIL WS-SUB  >  WS-PROF-OP-CNT                           ELTPSYCH
02998             SET PVN-BEN-PROVN-IDX TO WS-SUB                       ELTPSYCH
02999             IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO    ELTPSYCH
03000                 PERFORM 4040-BUILD-SCREEN-LINES THRU 4040-EXIT    ELTPSYCH
03001             END-IF                                                ELTPSYCH
03002      END-PERFORM.                                                 ELTPSYCH
03003                                                                   ELTPSYCH
03004  4000-EXIT.  EXIT.                                                ELTPSYCH
03005 /                                                                 ELTPSYCH
03006  4040-BUILD-SCREEN-LINES.                                         ELTPSYCH
03007                                                                   ELTPSYCH
03008      SET PLT-INDEX1   TO                                          ELTPSYCH
03009                       PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).        ELTPSYCH
03010      IF WS-NOT-FIRST-TIME                                         ELTPSYCH
03011         MOVE 'P'  TO  COF-FUNCTION                                ELTPSYCH
03012         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTPSYCH
03013             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
03014              END-EXEC                                             ELTPSYCH
03015         INITIALIZE COF-DTL                                        ELTPSYCH
03016      ELSE                                                         ELTPSYCH
03017         MOVE 'N'  TO  WS-FIRSTTIME-IND.                           ELTPSYCH
03018                                                                   ELTPSYCH
03019      MOVE +1  TO  WS-CIA.                                         ELTPSYCH
03020      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
03021         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
03022            SET PLT-INDEX2  TO  2                                  ELTPSYCH
03023         ELSE                                                      ELTPSYCH
03024            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
03025            PERFORM 4090-PROBLEM-WITH-INDICES                      ELTPSYCH
03026            GO TO 4040-EXIT                                        ELTPSYCH
03027      ELSE                                                         ELTPSYCH
03028         SET PLT-INDEX2  TO  1.                                    ELTPSYCH
03029                                                                   ELTPSYCH
03030 **---------------------------------------------------------------+ELTPSYCH
03031 **                                                               |ELTPSYCH
03032 **        L I S T   O F   B E N E F I T   P R O V I S I O N S    |ELTPSYCH
03033      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE(WS-CIA).             ELTPSYCH
03034      ADD  +1  TO  WS-CIA.                                         ELTPSYCH
03035      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         TO  WS-SUB3.ELTPSYCH
03036      MOVE ZERO  TO  WS-SUB2.                                      ELTPSYCH
03037                                                                   ELTPSYCH
03038      MOVE WS-NO TO WS-DISPLAY-SHOCK-TEXT.                         ELTPSYCH
03039                                                                   ELTPSYCH
03040      PERFORM 4050-ZERO-ALL-WITH-SAME-NO                           ELTPSYCH
03041         VARYING  PVN-BEN-PROVN-IDX FROM WS-SUB BY +1              ELTPSYCH
03042         UNTIL  PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.                ELTPSYCH
03043      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTPSYCH
03044                                                                   ELTPSYCH
03045      ADD  1,  WS-CIA  GIVING  COF-NBR-DTL-LINES.                  ELTPSYCH
03046      MOVE 1  TO  WS-CIA.                                          ELTPSYCH
03047      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTPSYCH
03048             COMMAREA(DFHCOMMAREA)                                 ELTPSYCH
03049             END-EXEC.                                             ELTPSYCH
03050      INITIALIZE COF-DTL.                                          ELTPSYCH
03051 **                                                               |ELTPSYCH
03052 **---------------------------------------------------------------+ELTPSYCH
03053                                                                   ELTPSYCH
03054      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03055      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03056        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03057               NOT = ZERO                                          ELTPSYCH
03058         MOVE WS-SERVICES-RENDERED TO  COF-DTL-LINE(WS-CIA)        ELTPSYCH
03059         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03060         ADD +1  TO  WS-CIA.                                       ELTPSYCH
03061                                                                   ELTPSYCH
03062      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
03063      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03064       AND  NOT WS-ADD-A-BLANK-LINE                                ELTPSYCH
03065        AND PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03066               NOT = ZERO                                          ELTPSYCH
03067         MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE(WS-CIA)       ELTPSYCH
03068         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03069         ADD +1  TO  WS-CIA.                                       ELTPSYCH
03070                                                                   ELTPSYCH
03071      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03072             PERFORM 5000-PLACE-OF-TREATMENT-BASIC.                ELTPSYCH
03073                                                                   ELTPSYCH
03074      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03075             PERFORM 5100-PLACE-OF-TREATMENT-SUPP.                 ELTPSYCH
03076                                                                   ELTPSYCH
03077      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
03078            MOVE 'N'  TO  WS-ADD-A-BLANK-IND                       ELTPSYCH
03079            ADD  +1   TO  WS-CIA                                   ELTPSYCH
03080            PERFORM 8000-OUTPUT-TEXT.                              ELTPSYCH
03081                                                                   ELTPSYCH
03082 **---------------------------------------------------------------+ELTPSYCH
03083 **                                                               |ELTPSYCH
03084 **   P R O V I S I O N   P R I C I N G   M E T H O D   A N D     |ELTPSYCH
03085 **     V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R |ELTPSYCH
03086 **       A D D I T I O N A L   P R I C I N G   P E R C E N T     |ELTPSYCH
03087      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03088      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
03089         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
03090                                                              '19' ELTPSYCH
03091         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03092         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
03093         ADD +1  TO  WS-CIA.                                       ELTPSYCH
03094                                                                   ELTPSYCH
03095      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
03096      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
03097         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
03098                                                        '19' AND   ELTPSYCH
03099         NOT WS-ADD-A-BLANK-LINE                                   ELTPSYCH
03100         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03101         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTPSYCH
03102         ADD +1  TO  WS-CIA.                                       ELTPSYCH
03103                                                                   ELTPSYCH
03104      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03105      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
03106         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
03107                            AND                                    ELTPSYCH
03108         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03109         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
03110         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
03111                                                              ZERO ELTPSYCH
03112            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
03113            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
03114            ADD +1  TO  WS-CIA.                                    ELTPSYCH
03115                                                                   ELTPSYCH
03116      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03117      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
03118         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTPSYCH
03119                            AND                                    ELTPSYCH
03120         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTPSYCH
03121         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTPSYCH
03122         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTPSYCH
03123         ADD +1  TO  WS-CIA.                                       ELTPSYCH
03124                                                                   ELTPSYCH
03125      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTPSYCH
03126         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03127         SET  PLT-INDEX2  TO  2                                    ELTPSYCH
03128         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTPSYCH
03129                                                              ZERO ELTPSYCH
03130            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTPSYCH
03131            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTPSYCH
03132            ADD +1  TO  WS-CIA.                                    ELTPSYCH
03133                                                                   ELTPSYCH
03134      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03135      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03136         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
03137                                                            =  ZEROELTPSYCH
03138            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03139                                                            =  ZEROELTPSYCH
03140               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
03141            ELSE                                                   ELTPSYCH
03142               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
03143          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03144                                                  TO  WS-PERCENTAGEELTPSYCH
03145         ELSE                                                      ELTPSYCH
03146            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPSYCH
03147          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03148                                                 TO  WS-PERCENTAGE.ELTPSYCH
03149                                                                   ELTPSYCH
03150      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03151       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03152         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
03153                                                            =  ZEROELTPSYCH
03154          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
03155                                                            =  ZEROELTPSYCH
03156            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03157                                                            =  ZEROELTPSYCH
03158               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
03159            ELSE                                                   ELTPSYCH
03160             MOVE                                                  ELTPSYCH
03161               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03162                                                  TO  WS-PER-DIEM  ELTPSYCH
03163             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTPSYCH
03164                                                                   ELTPSYCH
03165      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTPSYCH
03166         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
03167                                             ZERO AND  NOT =  '19' ELTPSYCH
03168         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
03169         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
03170         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
03171                                                    CMF-CODE-VALUE ELTPSYCH
03172         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTPSYCH
03173         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
03174         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
03175                                                                   ELTPSYCH
03176      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
03177      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03178         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTPSYCH
03179                                                               ZEROELTPSYCH
03180            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03181                                                            =  ZEROELTPSYCH
03182               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
03183            ELSE                                                   ELTPSYCH
03184               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTPSYCH
03185          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03186                                                  TO  WS-PERCENTAGEELTPSYCH
03187         ELSE                                                      ELTPSYCH
03188            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTPSYCH
03189          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTPSYCH
03190                                                 TO  WS-PERCENTAGE.ELTPSYCH
03191                                                                   ELTPSYCH
03192      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03193       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03194         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTPSYCH
03195                                                            =  ZEROELTPSYCH
03196          IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)  ELTPSYCH
03197                                                            =  ZEROELTPSYCH
03198            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03199                                                            =  ZEROELTPSYCH
03200               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTPSYCH
03201            ELSE                                                   ELTPSYCH
03202             MOVE                                                  ELTPSYCH
03203               PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03204                                                  TO  WS-PER-DIEM  ELTPSYCH
03205             MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW.           ELTPSYCH
03206                                                                   ELTPSYCH
03207      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTPSYCH
03208         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTPSYCH
03209                                             ZERO AND  NOT =  '19' ELTPSYCH
03210         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
03211         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTPSYCH
03212         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTPSYCH
03213                                                    CMF-CODE-VALUE ELTPSYCH
03214         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTPSYCH
03215         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
03216         PERFORM 2200-CODES-MANUAL-WITH-AMOUNT THRU 2299-EXIT.     ELTPSYCH
03217                                                                   ELTPSYCH
03218      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
03219         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03220         ADD  1    TO  WS-CIA                                      ELTPSYCH
03221         PERFORM 8000-OUTPUT-TEXT                                  ELTPSYCH
03222      ELSE                                                         ELTPSYCH
03223       PERFORM 8000-OUTPUT-TEXT.                                   ELTPSYCH
03224 **                                                               |ELTPSYCH
03225 **---------------------------------------------------------------+ELTPSYCH
03226                                                                   ELTPSYCH
03227 **---------------------------------------------------------------+ELTPSYCH
03228 **                                                               |ELTPSYCH
03229 **            B E N E F I T   S C O P E   I D                    |ELTPSYCH
03230      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03231      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03232         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
03233            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03234                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03235               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03236               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03237               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03238                                                                   ELTPSYCH
03239      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03240         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
03241          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
03242           AND PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03243                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03244               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03245               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03246               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03247                                                                   ELTPSYCH
03248      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03249         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
03250          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
03251           AND PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03252                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03253               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03254               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03255               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03256                                                                   ELTPSYCH
03257      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
03258      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03259         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
03260          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
03261           AND PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03262                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03263               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03264               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03265               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03266                                                                   ELTPSYCH
03267      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03268         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
03269          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
03270           AND PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03271                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03272               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03273               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03274               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03275                                                                   ELTPSYCH
03276      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03277         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
03278          AND NOT WS-ADD-A-BLANK-LINE                              ELTPSYCH
03279           AND PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03280                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03281               MOVE WS-PAYMNT-BASED  TO  COF-DTL-LINE(WS-CIA)      ELTPSYCH
03282               MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                    ELTPSYCH
03283               ADD  +1  TO  WS-CIA.                                ELTPSYCH
03284                                                                   ELTPSYCH
03285      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03286      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03287         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
03288            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03289                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03290               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03291               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03292               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03293                                                    CMF-CODE-VALUE ELTPSYCH
03294               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
03295               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03296               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03297                                                                   ELTPSYCH
03298      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03299         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
03300            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03301                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03302               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03303               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03304               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03305                                                    CMF-CODE-VALUE ELTPSYCH
03306               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
03307               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03308               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03309                                                                   ELTPSYCH
03310      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03311         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
03312            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03313                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03314               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03315               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03316               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03317                                                    CMF-CODE-VALUE ELTPSYCH
03318               MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA            ELTPSYCH
03319               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03320               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03321                                                                   ELTPSYCH
03322      SET PLT-INDEX2  TO  2.                                       ELTPSYCH
03323      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03324         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                   ELTPSYCH
03325            IF PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03326                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03327               MOVE 'BPC'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03328               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03329               MOVE PLC-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03330                                                    CMF-CODE-VALUE ELTPSYCH
03331               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
03332               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03333               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03334                                                                   ELTPSYCH
03335      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03336         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                   ELTPSYCH
03337            IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03338                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03339               MOVE 'BPD'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03340               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03341               MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03342                                                    CMF-CODE-VALUE ELTPSYCH
03343               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
03344               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03345               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03346                                                                   ELTPSYCH
03347      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03348         IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                   ELTPSYCH
03349            IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  NOT =     ELTPSYCH
03350                                         '0000' AND  NOT =  '00  ' ELTPSYCH
03351               MOVE 'BPE'  TO  CMF-RECORD-PREFIX                   ELTPSYCH
03352               MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03353               MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03354                                                    CMF-CODE-VALUE ELTPSYCH
03355               MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA             ELTPSYCH
03356               MOVE +63  TO  WS-TEMP-NOT-USED-CNT                  ELTPSYCH
03357               PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT. ELTPSYCH
03358                                                                   ELTPSYCH
03359      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
03360         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTPSYCH
03361         ADD  +1   TO  WS-CIA                                      ELTPSYCH
03362         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03363                                                                   ELTPSYCH
03364 **                                                               |ELTPSYCH
03365 **---------------------------------------------------------------+ELTPSYCH
03366                                                                   ELTPSYCH
03367 **---------------------------------------------------------------+ELTPSYCH
03368 **    M  A  X   V  I  S  I  T  S                                 |ELTPSYCH
03369                                                                   ELTPSYCH
03370      MOVE WS-NO TO WS-DISPLAY-MAX-AMT-TEXT                        ELTPSYCH
03371                    WS-DISPLAY-MAX-VISITS-TEXT.                    ELTPSYCH
03372                                                                   ELTPSYCH
03373      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03374      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03375       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTPSYCH
03376        AND NOT = LOW-VALUES)                                      ELTPSYCH
03377                          OR                                       ELTPSYCH
03378       (PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'   ELTPSYCH
03379        AND NOT = LOW-VALUES)                                      ELTPSYCH
03380          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTPSYCH
03381                                                                   ELTPSYCH
03382      SET  PLT-INDEX2 TO  2.                                       ELTPSYCH
03383      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03384       IF (PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTPSYCH
03385        AND NOT = LOW-VALUES)                                      ELTPSYCH
03386                          OR                                       ELTPSYCH
03387       (PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'   ELTPSYCH
03388        AND NOT = LOW-VALUES)                                      ELTPSYCH
03389          MOVE WS-YES TO WS-DISPLAY-MAX-VISITS-TEXT.               ELTPSYCH
03390                                                                   ELTPSYCH
03391      IF WS-DISPLAY-MAX-VISITS-TEXT = WS-YES                       ELTPSYCH
03392          ADD +1             TO WS-CIA                             ELTPSYCH
03393          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTPSYCH
03394          ADD +1             TO WS-CIA.                            ELTPSYCH
03395                                                                   ELTPSYCH
03396      SET  PLT-INDEX2           TO  1.                             ELTPSYCH
03397      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03398       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03399        IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
03400                  NOT = ZEROS                                      ELTPSYCH
03401         MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03402                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
03403         MOVE SPACES   TO TCAR-FROM-AREA                           ELTPSYCH
03404         STRING WS-BASIC-LIT ' '                                   ELTPSYCH
03405                WS-DTL-MAX-DAYS ' '                                ELTPSYCH
03406                WS-PER ' '                                         ELTPSYCH
03407                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTPSYCH
03408         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTPSYCH
03409         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTPSYCH
03410         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
03411         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
03412         PERFORM TCPR-000-TEXT-UNSTRING                            ELTPSYCH
03413         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTPSYCH
03414         MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO    ELTPSYCH
03415                                                 CMF-CODE-VALUE    ELTPSYCH
03416         MOVE 'BPD'                TO  CMF-RECORD-PREFIX           ELTPSYCH
03417         MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME      ELTPSYCH
03418         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
03419         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03420                                                                   ELTPSYCH
03421      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03422       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03423        IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03424                  NOT = ZEROS                                      ELTPSYCH
03425         MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
03426                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
03427         MOVE SPACES   TO TCAR-FROM-AREA                           ELTPSYCH
03428         STRING WS-BASIC-LIT ' '                                   ELTPSYCH
03429                WS-DTL-MAX-DAYS ' '                                ELTPSYCH
03430                WS-PER ' '                                         ELTPSYCH
03431                    DELIMITED BY SIZE INTO TCAR-FROM-AREA          ELTPSYCH
03432         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTPSYCH
03433         MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                  ELTPSYCH
03434         MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
03435         MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                  ELTPSYCH
03436         PERFORM TCPR-000-TEXT-UNSTRING                            ELTPSYCH
03437         MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA           ELTPSYCH
03438         MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) TO    ELTPSYCH
03439                                                 CMF-CODE-VALUE    ELTPSYCH
03440         MOVE 'BPE'                TO  CMF-RECORD-PREFIX           ELTPSYCH
03441         MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
03442         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
03443         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03444                                                                   ELTPSYCH
03445      SET  PLT-INDEX2           TO  2.                             ELTPSYCH
03446      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03447       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03448         IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03449                NOT = ZEROS                                        ELTPSYCH
03450          MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
03451                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
03452          MOVE SPACES   TO TCAR-FROM-AREA                          ELTPSYCH
03453          STRING WS-SUPP-LIT ' '                                   ELTPSYCH
03454                 WS-DTL-MAX-DAYS                                   ELTPSYCH
03455                 WS-PER ' '                                        ELTPSYCH
03456                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTPSYCH
03457          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTPSYCH
03458          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTPSYCH
03459          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
03460          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
03461          PERFORM TCPR-000-TEXT-UNSTRING                           ELTPSYCH
03462          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTPSYCH
03463          MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)  TO   ELTPSYCH
03464                                                 CMF-CODE-VALUE    ELTPSYCH
03465          MOVE 'BPD'                TO  CMF-RECORD-PREFIX          ELTPSYCH
03466          MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME     ELTPSYCH
03467          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
03468          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
03469                                                                   ELTPSYCH
03470      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03471       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03472         IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03473                NOT = ZEROS                                        ELTPSYCH
03474          MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)     ELTPSYCH
03475                TO WS-DTL-MAX-DAYS                                 ELTPSYCH
03476          MOVE SPACES   TO TCAR-FROM-AREA                          ELTPSYCH
03477          STRING WS-SUPP-LIT ' '                                   ELTPSYCH
03478                 WS-DTL-MAX-DAYS                                   ELTPSYCH
03479                 WS-PER ' '                                        ELTPSYCH
03480                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTPSYCH
03481          PERFORM TCPR-000-TEXT-COMPRESSION                        ELTPSYCH
03482          MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT                 ELTPSYCH
03483          MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
03484          MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN                 ELTPSYCH
03485          PERFORM TCPR-000-TEXT-UNSTRING                           ELTPSYCH
03486          MOVE TCAR-OPF-DATA(1)      TO WS-TEMP-TEXT-AREA          ELTPSYCH
03487          MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) TO   ELTPSYCH
03488                                                 CMF-CODE-VALUE    ELTPSYCH
03489          MOVE 'BPE'                TO  CMF-RECORD-PREFIX          ELTPSYCH
03490          MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME    ELTPSYCH
03491          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
03492          PERFORM 8000-OUTPUT-TEXT.                                ELTPSYCH
03493                                                                   ELTPSYCH
03494 **                                                               |ELTPSYCH
03495 **---------------------------------------------------------------+ELTPSYCH
03496                                                                   ELTPSYCH
03497 **---------------------------------------------------------------+ELTPSYCH
03498 **    M  A  X   A  M  O  U  N  T   P  E  R   V  I  S  I  T       |ELTPSYCH
03499      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03500      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03501       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03502         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
03503                NOT = ZEROS                                        ELTPSYCH
03504            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
03505                                                                   ELTPSYCH
03506      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03507       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03508         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
03509                NOT = ZEROS                                        ELTPSYCH
03510            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
03511                                                                   ELTPSYCH
03512      SET  PLT-INDEX2 TO  2.                                       ELTPSYCH
03513      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03514       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03515         IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
03516                NOT = ZEROS                                        ELTPSYCH
03517            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
03518                                                                   ELTPSYCH
03519      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03520       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03521         IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)          ELTPSYCH
03522                NOT = ZEROS                                        ELTPSYCH
03523            MOVE WS-YES TO WS-DISPLAY-MAX-AMT-TEXT.                ELTPSYCH
03524                                                                   ELTPSYCH
03525      IF WS-DISPLAY-MAX-AMT-TEXT = WS-YES                          ELTPSYCH
03526          ADD +1             TO WS-CIA                             ELTPSYCH
03527          MOVE WS-MAX-AMOUNT TO COF-DTL-LINE(WS-CIA)               ELTPSYCH
03528          ADD +1             TO WS-CIA.                            ELTPSYCH
03529                                                                   ELTPSYCH
03530      SET  PLT-INDEX2           TO  1.                             ELTPSYCH
03531      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03532       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03533        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
03534                  NOT = ZEROS                                      ELTPSYCH
03535         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03536                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
03537         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTPSYCH
03538         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
03539         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03540                                                                   ELTPSYCH
03541      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTPSYCH
03542       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03543        IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
03544                  NOT = ZEROS                                      ELTPSYCH
03545         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03546                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
03547         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-BASIC                    ELTPSYCH
03548         MOVE WS-BASIC          TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
03549         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03550                                                                   ELTPSYCH
03551      SET  PLT-INDEX2           TO  2.                             ELTPSYCH
03552      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03553       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                     ELTPSYCH
03554        IF PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
03555                  NOT = ZEROS                                      ELTPSYCH
03556         MOVE PLD-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03557                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
03558         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTPSYCH
03559         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
03560         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03561                                                                   ELTPSYCH
03562      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03563       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                     ELTPSYCH
03564        IF PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)           ELTPSYCH
03565                  NOT = ZEROS                                      ELTPSYCH
03566         MOVE PLE-MAX-AMT-PER-VISIT(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03567                TO WS-DTL-MAX-AMOUNT                               ELTPSYCH
03568         MOVE WS-DTL-MAX-AMOUNT TO WS-DTL-SUPPLEMENTAL             ELTPSYCH
03569         MOVE WS-SUPPLEMENTAL   TO CMF-DESCR-LINE(WS-CIA)          ELTPSYCH
03570         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03571 **                                                               |ELTPSYCH
03572 **---------------------------------------------------------------+ELTPSYCH
03573                                                                   ELTPSYCH
03574 **---------------------------------------------------------------+ELTPSYCH
03575 **                                                               |ELTPSYCH
03576 **   CERTIFICATION REQUIREMENT                                   |ELTPSYCH
03577                                                                   ELTPSYCH
03578      PERFORM 5140-CERTIFICATION THRU 5140-EXIT.                   ELTPSYCH
03579                                                                   ELTPSYCH
03580 **                                                               |ELTPSYCH
03581 **                                                               |ELTPSYCH
03582 **---------------------------------------------------------------+ELTPSYCH
03583                                                                   ELTPSYCH
03584      INITIALIZE WS-BASIC-CONTRACT                                 ELTPSYCH
03585                 WS-SUPP-CONTRACT.                                 ELTPSYCH
03586                                                                   ELTPSYCH
03587      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTPSYCH
03588      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
03589                          ADDRESS OF CONTRACT-RECORD.              ELTPSYCH
03590      IF CIA-RC-OK                                                 ELTPSYCH
03591         SET WS-BASIC-PRESENT TO TRUE.                             ELTPSYCH
03592                                                                   ELTPSYCH
03593      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTPSYCH
03594      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTPSYCH
03595                          ADDRESS OF CONTRACT-RECORD.              ELTPSYCH
03596      IF CIA-RC-OK                                                 ELTPSYCH
03597         SET WS-SUPP-PRESENT TO TRUE.                              ELTPSYCH
03598                                                                   ELTPSYCH
03599                                                                   ELTPSYCH
03600      IF WS-DISPLAY-SHOCK-TEXT = WS-YES                            ELTPSYCH
03601        IF NOT WS-BASIC-PRESENT  AND                               ELTPSYCH
03602           NOT WS-SUPP-PRESENT                                     ELTPSYCH
03603             ADD +1 TO WS-CIA                                      ELTPSYCH
03604             MOVE WS-NO-SAME-PROVIDER-ELECT TO                     ELTPSYCH
03605                                        COF-DTL-LINE (WS-CIA)      ELTPSYCH
03606             PERFORM 8000-OUTPUT-TEXT                              ELTPSYCH
03607        ELSE                                                       ELTPSYCH
03608            ADD +1 TO WS-CIA                                       ELTPSYCH
03609            MOVE WS-SAME-PROVIDER-ELECT TO COF-DTL-LINE (WS-CIA)   ELTPSYCH
03610            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
03611            IF WS-BASIC-PRESENT                                    ELTPSYCH
03612                SET CIA-ELSCONIB-DDN TO TRUE                       ELTPSYCH
03613                CALL 'ELUSETAD' USING DFHCOMMAREA                  ELTPSYCH
03614                                    ADDRESS OF CONTRACT-RECORD     ELTPSYCH
03615                PERFORM 5400-ELECT-SHK-THRPY-BASIC THRU            ELTPSYCH
03616                        5499-EXIT                                  ELTPSYCH
03617            END-IF                                                 ELTPSYCH
03618            IF WS-SUPP-PRESENT                                     ELTPSYCH
03619                SET CIA-ELSCONIS-DDN TO TRUE                       ELTPSYCH
03620                CALL 'ELUSETAD' USING DFHCOMMAREA                  ELTPSYCH
03621                                    ADDRESS OF CONTRACT-RECORD     ELTPSYCH
03622                PERFORM 5500-ELECT-SHK-THRPY-SUPP THRU             ELTPSYCH
03623                        5599-EXIT                                  ELTPSYCH
03624            END-IF                                                 ELTPSYCH
03625        END-IF                                                     ELTPSYCH
03626      END-IF.                                                      ELTPSYCH
03627      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03628      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTPSYCH
03629           PERFORM 7000-SPILLOVER-COINS                            ELTPSYCH
03630           PERFORM 7200-SPILLOVER-DEDUCT                           ELTPSYCH
03631           PERFORM 8000-OUTPUT-TEXT.                               ELTPSYCH
03632                                                                   ELTPSYCH
03633      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTPSYCH
03634         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZEROES  ELTPSYCH
03635            SET PLT-INDEX2  TO  2                                  ELTPSYCH
03636            PERFORM  7100-TRANS-OTHER-RESP-IND                     ELTPSYCH
03637              THRU   7199-EXIT                                     ELTPSYCH
03638            PERFORM 8000-OUTPUT-TEXT                               ELTPSYCH
03639         ELSE                                                      ELTPSYCH
03640            MOVE TABLE-MAX TO WS-SUB                               ELTPSYCH
03641            PERFORM 1090-PROBLEM-WITH-INDICES                      ELTPSYCH
03642            GO TO 1040-EXIT                                        ELTPSYCH
03643      ELSE                                                         ELTPSYCH
03644         SET PLT-INDEX2  TO  1                                     ELTPSYCH
03645         PERFORM  7100-TRANS-OTHER-RESP-IND                        ELTPSYCH
03646              THRU   7199-EXIT                                     ELTPSYCH
03647         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03648                                                                   ELTPSYCH
03649      PERFORM 6000-SCAN-TAB.                                       ELTPSYCH
03650                                                                   ELTPSYCH
03651  4040-EXIT.  EXIT.                                                ELTPSYCH
03652 /                                                                 ELTPSYCH
03653  4050-ZERO-ALL-WITH-SAME-NO.                                      ELTPSYCH
03654                                                                   ELTPSYCH
03655      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
03656        IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'ESTO ' OR 'ETAO '      ELTPSYCH
03657               MOVE WS-YES TO WS-DISPLAY-SHOCK-TEXT.               ELTPSYCH
03658                                                                   ELTPSYCH
03659      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTPSYCH
03660         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
03661         MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME             ELTPSYCH
03662         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX) TO                     ELTPSYCH
03663                                                   CMF-CODE-VALUE  ELTPSYCH
03664         MOVE SPACES  TO  WS-TEMP-TEXT-AREA                        ELTPSYCH
03665         MOVE +58  TO  WS-TEMP-NOT-USED-CNT                        ELTPSYCH
03666          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT       ELTPSYCH
03667         MOVE ZERO  TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)        ELTPSYCH
03668         ADD  1  TO  WS-SUB2                                       ELTPSYCH
03669         IF WS-CIA  >  20 OR  =  20                                ELTPSYCH
03670            MOVE WS-CIA  TO  COF-NBR-DTL-LINES                     ELTPSYCH
03671            EXEC CICS  LINK  PROGRAM('ELUOUTPT')                   ELTPSYCH
03672                COMMAREA(DFHCOMMAREA)                              ELTPSYCH
03673                 END-EXEC                                          ELTPSYCH
03674            INITIALIZE COF-DTL                                     ELTPSYCH
03675            MOVE +1  TO  WS-CIA.                                   ELTPSYCH
03676                                                                   ELTPSYCH
03677  4090-PROBLEM-WITH-INDICES.                                       ELTPSYCH
03678      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTPSYCH
03679      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTPSYCH
03680                                                                   ELTPSYCH
03681      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTPSYCH
03682      MOVE 'P'  TO  COF-FUNCTION.                                  ELTPSYCH
03683                                                                   ELTPSYCH
03684      EXEC CICS  LINK  PROGRAM('ELUOUTPT') COMMAREA(DFHCOMMAREA)   ELTPSYCH
03685             END-EXEC.                                             ELTPSYCH
03686      INITIALIZE COF-DTL.                                          ELTPSYCH
03687                                                                   ELTPSYCH
03688 /                                                                 ELTPSYCH
03689  5000-PLACE-OF-TREATMENT-BASIC.                                   ELTPSYCH
03690 **---------------------------------------------------------------+ELTPSYCH
03691 **                                                               |ELTPSYCH
03692 **        P L A C E   O F   T R E A T M E N T                    |ELTPSYCH
03693      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03694      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTPSYCH
03695                                                              ZERO ELTPSYCH
03696         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
03697         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTPSYCH
03698         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTPSYCH
03699                                                   CMF-CODE-VALUE  ELTPSYCH
03700         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTPSYCH
03701         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPSYCH
03702          PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT.      ELTPSYCH
03703  5099-EXIT.  EXIT.                                                ELTPSYCH
03704 /                                                                 ELTPSYCH
03705  5100-PLACE-OF-TREATMENT-SUPP.                                    ELTPSYCH
03706 **---------------------------------------------------------------+ELTPSYCH
03707 **                                                               |ELTPSYCH
03708 **        P L A C E   O F   T R E A T M E N T                    |ELTPSYCH
03709      SET  PLT-INDEX2  TO  2.                                      ELTPSYCH
03710      IF  PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  NOT =  ELTPSYCH
03711                                                              ZERO ELTPSYCH
03712         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
03713         MOVE 'PLACE-TREAT-ELIG-IND'  TO  CMF-ELEMENT-SYSTEM-NAME  ELTPSYCH
03714         MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)  TO ELTPSYCH
03715                                                   CMF-CODE-VALUE  ELTPSYCH
03716         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTPSYCH
03717         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPSYCH
03718         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
03719         ADD  +1      TO  WS-CIA.                                  ELTPSYCH
03720  5199-EXIT.  EXIT.                                                ELTPSYCH
03721 /                                                                 ELTPSYCH
03722  5140-CERTIFICATION.                                              ELTPSYCH
03723 ****************************************************************  ELTPSYCH
03724 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTPSYCH
03725 ****************************************************************  ELTPSYCH
03726      SET  PLT-INDEX2  TO  1.                                      ELTPSYCH
03727      IF     PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO        ELTPSYCH
03728         AND PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03729                                                 NOT = '00'        ELTPSYCH
03730      THEN                                                         ELTPSYCH
03731         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTPSYCH
03732         MOVE +2 TO  WS-CIA                                        ELTPSYCH
03733         MOVE WS-CERT-REQ TO COF-DTL-LINE (WS-CIA).                ELTPSYCH
03734                                                                   ELTPSYCH
03735      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03736      IF     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO        ELTPSYCH
03737         AND PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03738                                                 NOT = '00'        ELTPSYCH
03739         AND NOT WS-ADD-A-BLANK-LINE                               ELTPSYCH
03740      THEN                                                         ELTPSYCH
03741         MOVE 'Y' TO WS-ADD-A-BLANK-IND                            ELTPSYCH
03742         MOVE +2 TO WS-CIA                                         ELTPSYCH
03743         MOVE WS-CERT-REQ TO COF-DTL-LINE (WS-CIA).                ELTPSYCH
03744                                                                   ELTPSYCH
03745      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03746      IF     PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO        ELTPSYCH
03747         AND PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03748                                                 NOT = '00'        ELTPSYCH
03749      THEN                                                         ELTPSYCH
03750          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTPSYCH
03751          MOVE 'CERTFN-REQRM-IND' TO CMF-ELEMENT-SYSTEM-NAME       ELTPSYCH
03752          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03753            TO CMF-CODE-VALUE                                      ELTPSYCH
03754          MOVE WS-BASIC-LIT TO WS-TEMP-TEXT-AREA                   ELTPSYCH
03755          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTPSYCH
03756          ADD 1 TO WS-CIA                                          ELTPSYCH
03757          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTPSYCH
03758             THRU 2199-EXIT.                                       ELTPSYCH
03759                                                                   ELTPSYCH
03760      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03761      IF     PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO        ELTPSYCH
03762         AND PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03763                                                 NOT = '00'        ELTPSYCH
03764      THEN                                                         ELTPSYCH
03765          MOVE 'BP' TO CMF-RECORD-PREFIX                           ELTPSYCH
03766          MOVE 'CERTFN-REQRM-IND' TO CMF-ELEMENT-SYSTEM-NAME       ELTPSYCH
03767          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTPSYCH
03768            TO CMF-CODE-VALUE                                      ELTPSYCH
03769          MOVE WS-SUPP-LIT TO WS-TEMP-TEXT-AREA                    ELTPSYCH
03770          MOVE +63 TO WS-TEMP-NOT-USED-CNT                         ELTPSYCH
03771          ADD 1 TO WS-CIA                                          ELTPSYCH
03772          PERFORM 2100-CALL-CODES-MANUAL-LONG                      ELTPSYCH
03773             THRU 2199-EXIT.                                       ELTPSYCH
03774                                                                   ELTPSYCH
03775      IF WS-ADD-A-BLANK-LINE                                       ELTPSYCH
03776      THEN                                                         ELTPSYCH
03777         MOVE 'N' TO WS-ADD-A-BLANK-IND                            ELTPSYCH
03778         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03779                                                                   ELTPSYCH
03780      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTPSYCH
03781      THEN                                                         ELTPSYCH
03782         SET PLT-INDEX2  TO  2                                     ELTPSYCH
03783      ELSE                                                         ELTPSYCH
03784         SET PLT-INDEX2  TO  1.                                    ELTPSYCH
03785                                                                   ELTPSYCH
03786  5140-EXIT.  EXIT.                                                ELTPSYCH
03787                                                                   ELTPSYCH
03788  5200-DAY-NIGHT-PSYCH.                                            ELTPSYCH
03789      IF WS-DISPLAY-DAY-PSYCH-TEXT = WS-YES                        ELTPSYCH
03790         IF GCG-DAY-PSYCH-PRIOR-ADM-CD NOT = '0'                   ELTPSYCH
03791           ADD +1                         TO  WS-CIA               ELTPSYCH
03792           MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX    ELTPSYCH
03793           MOVE 'DAY-PSYCH-PRIOR-ADM-CD'                           ELTPSYCH
03794                                 TO  CMF-ELEMENT-SYSTEM-NAME       ELTPSYCH
03795           MOVE GCG-DAY-PSYCH-PRIOR-ADM-CD                         ELTPSYCH
03796                                 TO  CMF-CODE-VALUE                ELTPSYCH
03797           MOVE WS-DAY-PSYCH     TO WS-TEMP-TEXT-AREA              ELTPSYCH
03798           MOVE 28               TO WS-TEMP-NOT-USED-CNT           ELTPSYCH
03799           PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT      ELTPSYCH
03800           PERFORM 8000-OUTPUT-TEXT.                               ELTPSYCH
03801                                                                   ELTPSYCH
03802      IF WS-DISPLAY-NIGHT-PSYCH-TEXT = WS-YES                      ELTPSYCH
03803        IF GCG-NIGHT-PSYCH-PRIOR-ADM-CD NOT = '0'                  ELTPSYCH
03804           ADD +1                         TO  WS-CIA               ELTPSYCH
03805           MOVE 'GROUP'                   TO  CMF-RECORD-PREFIX    ELTPSYCH
03806           MOVE 'NIGHT-PSYCH-PRIOR-ADM-CD'                         ELTPSYCH
03807                                TO  CMF-ELEMENT-SYSTEM-NAME        ELTPSYCH
03808           MOVE GCG-NIGHT-PSYCH-PRIOR-ADM-CD                       ELTPSYCH
03809                                TO CMF-CODE-VALUE                  ELTPSYCH
03810           MOVE WS-NIGHT-PSYCH TO WS-TEMP-TEXT-AREA                ELTPSYCH
03811           MOVE 26               TO WS-TEMP-NOT-USED-CNT           ELTPSYCH
03812           PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT      ELTPSYCH
03813           PERFORM 8000-OUTPUT-TEXT.                               ELTPSYCH
03814                                                                   ELTPSYCH
03815  5299-EXIT.    EXIT.                                              ELTPSYCH
03816 /                                                                 ELTPSYCH
03817  5400-ELECT-SHK-THRPY-BASIC.                                      ELTPSYCH
03818 **---------------------------------------------------------------+ELTPSYCH
03819 **                                                               |ELTPSYCH
03820 **    E L E C T R I C   S H O C K   T H E R A P H Y              |ELTPSYCH
03821 **---------------------------------------------------------------+ELTPSYCH
03822      IF GCT-SAME-PROV-ELCSHK-THRPY-ANS NOT = '0' OR LOW-VALUES    ELTPSYCH
03823         MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                      ELTPSYCH
03824         MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                         ELTPSYCH
03825                         TO CMF-ELEMENT-SYSTEM-NAME                ELTPSYCH
03826         MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                       ELTPSYCH
03827                         TO CMF-CODE-VALUE                         ELTPSYCH
03828         MOVE WS-BASIC-LIT          TO  WS-TEMP-TEXT-AREA          ELTPSYCH
03829         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPSYCH
03830         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
03831         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03832  5499-EXIT.  EXIT.                                                ELTPSYCH
03833 /                                                                 ELTPSYCH
03834  5500-ELECT-SHK-THRPY-SUPP.                                       ELTPSYCH
03835 **---------------------------------------------------------------+ELTPSYCH
03836 **                                                               |ELTPSYCH
03837 **    E L E C T R I C   S H O C K   T H E R A P H Y              |ELTPSYCH
03838 **---------------------------------------------------------------+ELTPSYCH
03839      IF GCT-SAME-PROV-ELCSHK-THRPY-ANS NOT = '0' OR LOW-VALUES    ELTPSYCH
03840         MOVE 'CONTRACT' TO CMF-RECORD-PREFIX                      ELTPSYCH
03841         MOVE 'SAME-PROV-ELCSHK-THRPY-ANS'                         ELTPSYCH
03842                         TO CMF-ELEMENT-SYSTEM-NAME                ELTPSYCH
03843         MOVE GCT-SAME-PROV-ELCSHK-THRPY-ANS                       ELTPSYCH
03844                         TO CMF-CODE-VALUE                         ELTPSYCH
03845         MOVE WS-SUPP-LIT           TO  WS-TEMP-TEXT-AREA          ELTPSYCH
03846         MOVE 63                    TO  WS-TEMP-NOT-USED-CNT       ELTPSYCH
03847         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
03848         PERFORM 8000-OUTPUT-TEXT.                                 ELTPSYCH
03849  5599-EXIT.  EXIT.                                                ELTPSYCH
03850 /                                                                 ELTPSYCH
03851  6000-SCAN-TAB.                                                   ELTPSYCH
03852                                                                   ELTPSYCH
03853      PERFORM 6200-BEN-TAB-AAR THRU 6200-EXIT.                     ELTPSYCH
03854      PERFORM 6300-BEN-TAB-PPF THRU 6300-EXIT.                     ELTPSYCH
03855      ADD +1 TO WS-CIA.                                            ELTPSYCH
03856      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTPSYCH
03857      ADD +2           TO WS-CIA.                                  ELTPSYCH
03858      PERFORM 8000-OUTPUT-TEXT.                                    ELTPSYCH
03859      PERFORM 6500-BEN-TAB-ADL THRU 6500-EXIT.                     ELTPSYCH
03860      PERFORM 6600-BEN-TAB-ABM THRU 6600-EXIT.                     ELTPSYCH
03861      PERFORM 6700-BEN-TAB-ACL THRU 6700-EXIT.                     ELTPSYCH
03862      PERFORM 6800-BEN-TAB-AOL THRU 6800-EXIT.                     ELTPSYCH
03863      ADD   +2     TO  WS-CIA.                                     ELTPSYCH
03864      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTPSYCH
03865      PERFORM 8000-OUTPUT-TEXT.                                    ELTPSYCH
03866                                                                   ELTPSYCH
03867 /                                                                 ELTPSYCH
03868  6200-BEN-TAB-AAR.                                                ELTPSYCH
03869      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTPSYCH
03870      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03871                                                                   ELTPSYCH
03872      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03873          NOT = LOW-VALUES                                         ELTPSYCH
03874       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03875          NOT = SPACE                                              ELTPSYCH
03876                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTPSYCH
03877                                                                   ELTPSYCH
03878      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03879                                                                   ELTPSYCH
03880      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03881          NOT = LOW-VALUES                                         ELTPSYCH
03882       IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03883          NOT = SPACE                                              ELTPSYCH
03884                 MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.               ELTPSYCH
03885                                                                   ELTPSYCH
03886      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTPSYCH
03887             MOVE +2                  TO WS-CIA                    ELTPSYCH
03888             MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)      ELTPSYCH
03889             PERFORM 8000-OUTPUT-TEXT.                             ELTPSYCH
03890  6200-EXIT.  EXIT.                                                ELTPSYCH
03891 /                                                                 ELTPSYCH
03892  6300-BEN-TAB-PPF.                                                ELTPSYCH
03893      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPSYCH
03894                       WS-HOLD2.                                   ELTPSYCH
03895      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03896      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03897          NOT = LOW-VALUES                                         ELTPSYCH
03898       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03899          NOT = SPACE                                              ELTPSYCH
03900             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
03901                        TO  WS-HOLD1.                              ELTPSYCH
03902                                                                   ELTPSYCH
03903      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03904      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03905          NOT = LOW-VALUES                                         ELTPSYCH
03906       IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03907          NOT = SPACE                                              ELTPSYCH
03908             MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
03909                        TO  WS-HOLD2.                              ELTPSYCH
03910                                                                   ELTPSYCH
03911      IF WS-HOLD1 = WS-HOLD2                                       ELTPSYCH
03912         IF WS-HOLD1 = ZEROS                                       ELTPSYCH
03913                 GO TO 6300-EXIT                                   ELTPSYCH
03914         ELSE                                                      ELTPSYCH
03915             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPSYCH
03916             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
03917               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTPSYCH
03918                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
03919               END-EXEC                                            ELTPSYCH
03920               GO TO 6300-EXIT.                                    ELTPSYCH
03921                                                                   ELTPSYCH
03922      IF WS-HOLD1 = ZEROS                                          ELTPSYCH
03923             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
03924             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
03925               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTPSYCH
03926                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
03927               END-EXEC                                            ELTPSYCH
03928      ELSE                                                         ELTPSYCH
03929       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTPSYCH
03930       PERFORM 2300-GET-TABULAR-RECORD                             ELTPSYCH
03931          EXEC CICS  LINK PROGRAM('ELGPPF')                        ELTPSYCH
03932                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
03933          END-EXEC                                                 ELTPSYCH
03934          IF WS-HOLD2 = ZEROS                                      ELTPSYCH
03935            GO TO 6300-EXIT                                        ELTPSYCH
03936          ELSE                                                     ELTPSYCH
03937             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
03938             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
03939               EXEC CICS  LINK PROGRAM('ELGPPF')                   ELTPSYCH
03940                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
03941               END-EXEC.                                           ELTPSYCH
03942  6300-EXIT.    EXIT.                                              ELTPSYCH
03943 /                                                                 ELTPSYCH
03944  6500-BEN-TAB-ADL.                                                ELTPSYCH
03945      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPSYCH
03946                       WS-HOLD2.                                   ELTPSYCH
03947      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
03948      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03949          NOT = LOW-VALUES                                         ELTPSYCH
03950       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03951          NOT = SPACE                                              ELTPSYCH
03952             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
03953                        TO  WS-HOLD1.                              ELTPSYCH
03954                                                                   ELTPSYCH
03955      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
03956      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
03957          NOT = LOW-VALUES                                         ELTPSYCH
03958       IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
03959          NOT = SPACE                                              ELTPSYCH
03960             MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
03961                        TO  WS-HOLD2.                              ELTPSYCH
03962                                                                   ELTPSYCH
03963      IF WS-HOLD1 = WS-HOLD2                                       ELTPSYCH
03964         IF WS-HOLD1 = ZEROS                                       ELTPSYCH
03965                 GO TO 6500-EXIT                                   ELTPSYCH
03966         ELSE                                                      ELTPSYCH
03967             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPSYCH
03968             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
03969               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTPSYCH
03970                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
03971               END-EXEC                                            ELTPSYCH
03972               GO TO 6500-EXIT.                                    ELTPSYCH
03973                                                                   ELTPSYCH
03974      IF WS-HOLD1 = ZEROS                                          ELTPSYCH
03975          MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                       ELTPSYCH
03976          PERFORM 2300-GET-TABULAR-RECORD                          ELTPSYCH
03977          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTPSYCH
03978                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
03979          END-EXEC                                                 ELTPSYCH
03980      ELSE                                                         ELTPSYCH
03981          MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                       ELTPSYCH
03982          PERFORM 2300-GET-TABULAR-RECORD                          ELTPSYCH
03983          EXEC CICS  LINK PROGRAM('ELGDEDBL')                      ELTPSYCH
03984                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
03985          END-EXEC                                                 ELTPSYCH
03986          IF WS-HOLD2 = ZEROS                                      ELTPSYCH
03987            GO TO 6500-EXIT                                        ELTPSYCH
03988          ELSE                                                     ELTPSYCH
03989             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
03990             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
03991               EXEC CICS  LINK PROGRAM('ELGDEDBL')                 ELTPSYCH
03992                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
03993               END-EXEC.                                           ELTPSYCH
03994  6500-EXIT.     EXIT.                                             ELTPSYCH
03995 /                                                                 ELTPSYCH
03996  6600-BEN-TAB-ABM.                                                ELTPSYCH
03997      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPSYCH
03998                       WS-HOLD2.                                   ELTPSYCH
03999      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
04000      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04001          NOT = LOW-VALUES                                         ELTPSYCH
04002       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04003          NOT = SPACE                                              ELTPSYCH
04004             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04005                        TO  WS-HOLD1.                              ELTPSYCH
04006                                                                   ELTPSYCH
04007      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
04008      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04009          NOT = LOW-VALUES                                         ELTPSYCH
04010       IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04011          NOT = SPACE                                              ELTPSYCH
04012             MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04013                        TO  WS-HOLD2.                              ELTPSYCH
04014                                                                   ELTPSYCH
04015      IF WS-HOLD1 = WS-HOLD2                                       ELTPSYCH
04016         IF WS-HOLD1 = ZEROS                                       ELTPSYCH
04017                 GO TO 6600-EXIT                                   ELTPSYCH
04018         ELSE                                                      ELTPSYCH
04019             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPSYCH
04020             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04021               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTPSYCH
04022                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04023               END-EXEC                                            ELTPSYCH
04024               GO TO 6600-EXIT.                                    ELTPSYCH
04025                                                                   ELTPSYCH
04026      IF WS-HOLD1 = ZEROS                                          ELTPSYCH
04027             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04028             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04029               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTPSYCH
04030                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04031               END-EXEC                                            ELTPSYCH
04032      ELSE                                                         ELTPSYCH
04033       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTPSYCH
04034       PERFORM 2300-GET-TABULAR-RECORD                             ELTPSYCH
04035          EXEC CICS  LINK PROGRAM('ELGMAXIM')                      ELTPSYCH
04036                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
04037          END-EXEC                                                 ELTPSYCH
04038          IF WS-HOLD2 = ZEROS                                      ELTPSYCH
04039            GO TO 6600-EXIT                                        ELTPSYCH
04040          ELSE                                                     ELTPSYCH
04041             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04042             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04043               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTPSYCH
04044                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
04045               END-EXEC.                                           ELTPSYCH
04046  6600-EXIT.     EXIT.                                             ELTPSYCH
04047 /                                                                 ELTPSYCH
04048  6700-BEN-TAB-ACL.                                                ELTPSYCH
04049      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPSYCH
04050                       WS-HOLD2.                                   ELTPSYCH
04051      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
04052      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04053          NOT = LOW-VALUES                                         ELTPSYCH
04054       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04055          NOT = SPACE                                              ELTPSYCH
04056             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04057                        TO  WS-HOLD1.                              ELTPSYCH
04058                                                                   ELTPSYCH
04059      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
04060      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04061          NOT = LOW-VALUES                                         ELTPSYCH
04062       IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04063          NOT = SPACE                                              ELTPSYCH
04064             MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04065                        TO  WS-HOLD2.                              ELTPSYCH
04066                                                                   ELTPSYCH
04067      IF WS-HOLD1 = WS-HOLD2                                       ELTPSYCH
04068         IF WS-HOLD1 = ZEROS                                       ELTPSYCH
04069                 GO TO 6700-EXIT                                   ELTPSYCH
04070         ELSE                                                      ELTPSYCH
04071             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPSYCH
04072             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04073               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTPSYCH
04074                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04075               END-EXEC                                            ELTPSYCH
04076               GO TO 6700-EXIT.                                    ELTPSYCH
04077                                                                   ELTPSYCH
04078      IF WS-HOLD1 = ZEROS                                          ELTPSYCH
04079             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04080             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04081               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTPSYCH
04082                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04083               END-EXEC                                            ELTPSYCH
04084      ELSE                                                         ELTPSYCH
04085       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTPSYCH
04086       PERFORM 2300-GET-TABULAR-RECORD                             ELTPSYCH
04087          EXEC CICS  LINK PROGRAM('ELGCOINS')                      ELTPSYCH
04088                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
04089          END-EXEC                                                 ELTPSYCH
04090          IF WS-HOLD2 = ZEROS                                      ELTPSYCH
04091            GO TO 6700-EXIT                                        ELTPSYCH
04092          ELSE                                                     ELTPSYCH
04093             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04094             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04095               EXEC CICS  LINK PROGRAM('ELGCOINS')                 ELTPSYCH
04096                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
04097               END-EXEC.                                           ELTPSYCH
04098  6700-EXIT.     EXIT.                                             ELTPSYCH
04099 /                                                                 ELTPSYCH
04100  6800-BEN-TAB-AOL.                                                ELTPSYCH
04101      MOVE ZEROS   TO  WS-HOLD1,                                   ELTPSYCH
04102                       WS-HOLD2.                                   ELTPSYCH
04103      SET PLT-INDEX2 TO 1.                                         ELTPSYCH
04104      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04105          NOT = LOW-VALUES                                         ELTPSYCH
04106       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04107          NOT = SPACE                                              ELTPSYCH
04108             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04109                        TO  WS-HOLD1.                              ELTPSYCH
04110                                                                   ELTPSYCH
04111      SET PLT-INDEX2 TO 2.                                         ELTPSYCH
04112      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTPSYCH
04113          NOT = LOW-VALUES                                         ELTPSYCH
04114       IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04115          NOT = SPACE                                              ELTPSYCH
04116             MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04117                        TO  WS-HOLD2.                              ELTPSYCH
04118                                                                   ELTPSYCH
04119      IF WS-HOLD1 = WS-HOLD2                                       ELTPSYCH
04120         IF WS-HOLD1 = ZEROS                                       ELTPSYCH
04121                 GO TO 6800-EXIT                                   ELTPSYCH
04122         ELSE                                                      ELTPSYCH
04123             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTPSYCH
04124             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04125               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTPSYCH
04126                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04127               END-EXEC                                            ELTPSYCH
04128               GO TO 6800-EXIT.                                    ELTPSYCH
04129                                                                   ELTPSYCH
04130      IF WS-HOLD1 = ZEROS                                          ELTPSYCH
04131             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04132             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04133               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTPSYCH
04134                             COMMAREA(DFHCOMMAREA)                 ELTPSYCH
04135               END-EXEC                                            ELTPSYCH
04136      ELSE                                                         ELTPSYCH
04137       MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                          ELTPSYCH
04138       PERFORM 2300-GET-TABULAR-RECORD                             ELTPSYCH
04139          EXEC CICS  LINK PROGRAM('ELGOUTPX')                      ELTPSYCH
04140                          COMMAREA(DFHCOMMAREA)                    ELTPSYCH
04141          END-EXEC                                                 ELTPSYCH
04142          IF WS-HOLD2 = ZEROS                                      ELTPSYCH
04143            GO TO 6800-EXIT                                        ELTPSYCH
04144          ELSE                                                     ELTPSYCH
04145             MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                    ELTPSYCH
04146             PERFORM 2300-GET-TABULAR-RECORD                       ELTPSYCH
04147               EXEC CICS  LINK PROGRAM('ELGOUTPX')                 ELTPSYCH
04148                              COMMAREA(DFHCOMMAREA)                ELTPSYCH
04149               END-EXEC.                                           ELTPSYCH
04150  6800-EXIT.     EXIT.                                             ELTPSYCH
04151 /                                                                 ELTPSYCH
04152  7000-SPILLOVER-COINS.                                            ELTPSYCH
04153 **---------------------------------------------------------------+ELTPSYCH
04154 **                                                               |ELTPSYCH
04155 **   S P I L L   O V E R   C O I N S U R A N C E   A P P L I C   |ELTPSYCH
04156      IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
04157                                                       NOT =  '0'  ELTPSYCH
04158         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
04159         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTPSYCH
04160                                           CMF-ELEMENT-SYSTEM-NAME ELTPSYCH
04161         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTPSYCH
04162                                           TO   CMF-CODE-VALUE     ELTPSYCH
04163         MOVE WS-SPILLOVER-COINS TO WS-TEMP-TEXT-AREA              ELTPSYCH
04164         MOVE 56 TO WS-TEMP-NOT-USED-CNT                           ELTPSYCH
04165         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
04166         ADD +1  TO  WS-CIA.                                       ELTPSYCH
04167  7099-EXIT.    EXIT.                                              ELTPSYCH
04168 /                                                                 ELTPSYCH
04169  7100-TRANS-OTHER-RESP-IND.                                       ELTPSYCH
04170 **---------------------------------------------------------------+ELTPSYCH
04171 **                                                               |ELTPSYCH
04172 **   TRANSFER TO OTHER RESPONSIBILITY INDICATOR                  |ELTPSYCH
04173 **                                                               |ELTPSYCH
04174                                                                   ELTPSYCH
04175      IF PLP-TRANSF-OTHER-RESP-IND(PLT-INDEX1, PLT-INDEX2) = ZERO  ELTPSYCH
04176         GO TO 7199-EXIT.                                          ELTPSYCH
04177                                                                   ELTPSYCH
04178      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTPSYCH
04179      MOVE 'TRANSF-OTHER-RESP-IND' TO      CMF-ELEMENT-SYSTEM-NAME.ELTPSYCH
04180      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTPSYCH
04181           TO  CMF-CODE-VALUE.                                     ELTPSYCH
04182      MOVE SPACES                   TO WS-TEMP-TEXT-AREA.          ELTPSYCH
04183      MOVE +0                       TO WS-TEMP-NOT-USED-CNT.       ELTPSYCH
04184      PERFORM 2100-CALL-CODES-MANUAL-LONG THRU                     ELTPSYCH
04185              2199-EXIT.                                           ELTPSYCH
04186      ADD +1  TO  WS-CIA.                                          ELTPSYCH
04187 **                                                               |ELTPSYCH
04188 **---------------------------------------------------------------+ELTPSYCH
04189  7199-EXIT.    EXIT.                                              ELTPSYCH
04190                                                                   ELTPSYCH
04191 /                                                                 ELTPSYCH
04192  7200-SPILLOVER-DEDUCT.                                           ELTPSYCH
04193 **---------------------------------------------------------------+ELTPSYCH
04194 **                                                               |ELTPSYCH
04195 **   S P I L L   O V E R   D E D U C T I B L E   A P P L I C     |ELTPSYCH
04196      IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)        ELTPSYCH
04197                                                       NOT =  '0'  ELTPSYCH
04198         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTPSYCH
04199         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTPSYCH
04200                                           CMF-ELEMENT-SYSTEM-NAME ELTPSYCH
04201         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2) TOELTPSYCH
04202                                                     CMF-CODE-VALUEELTPSYCH
04203         MOVE WS-SPILLOVER-DEDUCT TO WS-TEMP-TEXT-AREA             ELTPSYCH
04204         MOVE 57 TO WS-TEMP-NOT-USED-CNT                           ELTPSYCH
04205         PERFORM 2100-CALL-CODES-MANUAL-LONG THRU 2199-EXIT        ELTPSYCH
04206         ADD +1  TO  WS-CIA.                                       ELTPSYCH
04207 **                                                               |ELTPSYCH
04208 **---------------------------------------------------------------+ELTPSYCH
04209  7299-EXIT.    EXIT.                                              ELTPSYCH
04210                                                                   ELTPSYCH
04211 /                                                                 ELTPSYCH
04212 **---------------------------------------------------------------+ELTPSYCH
04213 **                                                               |ELTPSYCH
04214 **   TRANSLATE AND DISPLAY NETWORK/UTILIZATION REVIEW INDICATOR  |ELTPSYCH
04215 **                                                               |ELTPSYCH
04216 **---------------------------------------------------------------+ELTPSYCH
04217  7300-DSPLY-NTWRK-UTIL-IND.                                       ELTPSYCH
04218      MOVE 'GROUP' TO CMF-RECORD-PREFIX.                           ELTPSYCH
04219      MOVE 'NETWORK-UTIL-REVIEW-IND'                               ELTPSYCH
04220        TO  CMF-ELEMENT-SYSTEM-NAME.                               ELTPSYCH
04221      MOVE GCG-NETWORK-UTIL-REVIEW-IND                             ELTPSYCH
04222        TO CMF-CODE-VALUE.                                         ELTPSYCH
04223      MOVE WS-NETWORK-UTIL-REV-PHR                                 ELTPSYCH
04224        TO WS-TEMP-TEXT-AREA.                                      ELTPSYCH
04225      MOVE 33 TO WS-TEMP-NOT-USED-CNT.                             ELTPSYCH
04226      PERFORM 2100-CALL-CODES-MANUAL-LONG                          ELTPSYCH
04227         THRU 2199-EXIT.                                           ELTPSYCH
04228      PERFORM 8000-OUTPUT-TEXT.                                    ELTPSYCH
04229                                                                   ELTPSYCH
04230  8000-OUTPUT-TEXT.                                                ELTPSYCH
04231       MOVE +0     TO COF-NBR-HDR-LINES.                           ELTPSYCH
04232       MOVE WS-CIA TO COF-NBR-DTL-LINES.                           ELTPSYCH
04233       MOVE ' '    TO  COF-FUNCTION.                               ELTPSYCH
04234       EXEC CICS  LINK  PROGRAM('ELUOUTPT')                        ELTPSYCH
04235              COMMAREA(DFHCOMMAREA)                                ELTPSYCH
04236              END-EXEC.                                            ELTPSYCH
04237       INITIALIZE COF-DTL.                                         ELTPSYCH
04238       MOVE 1  TO  WS-CIA.                                         ELTPSYCH
04239 /C O M P R E S S  A N D  E X P A N D  S U B R O U T I N E         ELTPSYCH
04240      COPY ELSTCOMP.                                               ELTPSYCH
04241                                                                   ELTPSYCH
