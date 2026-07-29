00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID. ELTINPTS.                                            ELTINPTS
00003  AUTHOR. GEORGE E MOORE.                                             LV001
00004  DATE-WRITTEN.   4/08/86.                                         ELTINPTS
00005  DATE-COMPILED.                                                   ELTINPTS
00006      SKIP2                                                        ELTINPTS
00007 ******************************************************************ELTINPTS
00008 *                                                                 ELTINPTS
00009 *                  PROGRAM ABSTRACT OF ELTINPTS                   ELTINPTS
00010 *                                                                 ELTINPTS
00011 * PROGRAM NAME:   IN HOSPITAL MEDICAL SERVICES                    ELTINPTS
00012 *                                                                 ELTINPTS
00013 * PROGRAM I.D.:   ELTINPTS                                        ELTINPTS
00014 *                                                                 ELTINPTS
00015 * PURPOSE:  TO DISPLAY BENEFIT PROVISION COVERED FOR IN HOSPITAL  ELTINPTS
00016 *           MEDICAL SERVICES.                                     ELTINPTS
00017 *                                                                 ELTINPTS
00018 * OVERVIEW: THE PROGRAM DISPLAYS THE TYPE OF IN HOSPITAL MEDICAL  ELTINPTS
00019 *            SERVICES AFFORD A MEMBER BY HIS GROUP. THIS INFORMA- ELTINPTS
00020 *            TION IS GOTTEN BY INTEROGATING THE BENEFIT PROVISIONSELTINPTS
00021 *            FOR THE GROUP WITHIN THE CONTRACT FOR A PARTICULAR   ELTINPTS
00022 *            RANGE OF DATES.                                      ELTINPTS
00023 *                                                                 ELTINPTS
00024 * RECORDS                                                         ELTINPTS
00025 * ACCESSED: GROUP SPECIFIC RECORDS, CONTRACTS, BENEFIT PROVISIONS,ELTINPTS
00026 *           DATA ELEMENTS, AND CODE VALUE RECORDS.                ELTINPTS
00027 *                                                                 ELTINPTS
00028 ******************************************************************ELTINPTS
00029      EJECT                                                        ELTINPTS
00030 ******************************************************************ELTINPTS
00031 * M A I N T E N A N C E                                           ELTINPTS
00032 *                                                                 ELTINPTS
00033 * DATE      PGM   DESCRIPTION                                     ELTINPTS
00034 * --------  ---   ------------------------------------------------ELTINPTS
00035 * 03/01/91  GEM   REWRITE FOR BENEFIT PROVISIONS ENHANCEMENTS.    ELTINPTS
00036 *                                                                 ELTINPTS
00037 * 06/05/91  GEM   ADDED LOGIC TO PROCESS FMT D BENEFITS PROVISION ELTINPTS
00038 *                 WHEN PROFESSIONAL OUTPATIENT SELECTED.          ELTINPTS
00039 *                                                                 ELTINPTS
00040 * 06/14/91  JPB   CHANGED BEN-MAX-VISITS-IND TO BEN-MAX-VISIT-IND ELTINPTS
00041 *                 FOR FORMAT D.                                   ELTINPTS
00042 *                                                                 ELTINPTS
00043 * 09/06/91  JPB   ELIMINATED DISPLAY OF SPILLOVER COINSURANCE AND ELTINPTS
00044 *                 DEDUCTIBLE MESSAGE WHEN NOT APPLICABLE.         ELTINPTS
00045 *                                                                 ELTINPTS
00046 * 10/01/91  AKK   CORRECTED ERROR IN SETTING OF PLT-INDEX1 AND2   ELTINPTS
00047 *                 IN PROVISION PRICING METHOD SENTENCE. CAUSED    ELTINPTS
00048 *                 INCORRECT OUTPUT.                               ELTINPTS
00049 *                                                                 ELTINPTS
00050 * 01/09/92  JPB   ADDED CHECK FOR SPACES OR LOW-VALUES TO CERTN-  ELTINPTS
00051 *                 REPETN-REQRD AND PROVN-PRICING-METHD            ELTINPTS
00052 ******************************************************************ELTINPTS
00053  ENVIRONMENT DIVISION.                                            ELTINPTS
00054      SKIP2                                                        ELTINPTS
00055  DATA DIVISION.                                                   ELTINPTS
00056  WORKING-STORAGE SECTION.                                         ELTINPTS
00057  01  WS-BEGIN                          PIC X(24)  VALUE           ELTINPTS
00058      '***ELTINPTS WS BEGINS***'.                                  ELTINPTS
00059  01  WS-ABEND-CODE                     PIC X(04) VALUE 'XXXX'.    ELTINPTS
00060 *     W O R K  A R E A S   A N D   S W I T C H E S                ELTINPTS
00061  01  WS-WORK-FIELDS.                                              ELTINPTS
00062      05  WS-PVN-HOLD-IDX               INDEX.                     ELTINPTS
00063      05  WS-CHAR-0                     PIC X(01).                 ELTINPTS
00064      05  WS-DISPLAY-MAX-VISIT-TEXT     PIC X(01).                 ELTINPTS
00065      05  WS-DISPLAY-AAR-TEXT           PIC X(01).                 ELTINPTS
00066      05  WS-HOLD1                      PIC X(10).                 ELTINPTS
00067      05  WS-HOLD2                      PIC X(10).                 ELTINPTS
00068      05  WS-DTL-PP                     PIC X(50).                 ELTINPTS
00069      05  WS-DTL-PERCENT                PIC ZZ9.                   ELTINPTS
00070      05  WS-DTL-PER-D                  PIC X(63).                 ELTINPTS
00071      05  WS-DTL-PER-D-AMT              PIC $$$$$$9.99.            ELTINPTS
00072      05  WS-DISPLAY-DAY-PSYCH-TEXT     PIC X(01).                 ELTINPTS
00073      05  WS-DISPLAY-NIGHT-PSYCH-TEXT   PIC X(01).                 ELTINPTS
00074      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X(01).                 ELTINPTS
00075      05  WS-CIA                        PIC S999  COMP-3 VALUE +0. ELTINPTS
00076      05  WS-SUB                        PIC S999  COMP-3 VALUE +0. ELTINPTS
00077      05  WS-SUB1                       PIC S999  COMP-3 VALUE +0. ELTINPTS
00078      05  WS-SUB2                       PIC S999  COMP-3 VALUE +0. ELTINPTS
00079      05  WS-SUB3                       PIC S999  COMP-3 VALUE +0. ELTINPTS
00080      05  WS-DESC-CTR                   PIC S999  COMP-3 VALUE +0. ELTINPTS
00081      05  WS-REC-LEN                    PIC S9(4) COMP VALUE +0.   ELTINPTS
00082      05  WS-FIXED-TAB-LEN              PIC S9(4) COMP VALUE +3.   ELTINPTS
00083      05  WS-VARIABLE-LEN               PIC S9(4) COMP VALUE +16.  ELTINPTS
00084      05  WS-FIRSTTIME-IND              PIC X(01).                 ELTINPTS
00085          88  WS-NOT-FIRST-TIME             VALUE 'N'.             ELTINPTS
00086          88  WS-FIRST-TIME                 VALUE 'Y'.             ELTINPTS
00087      05  WS-TEMP-TEXT-AREA             PIC X(79).                 ELTINPTS
00088      05  FILLER       REDEFINES     WS-TEMP-TEXT-AREA.            ELTINPTS
00089        10  WS-TEMP-TEXT-CHAR           PIC X  OCCURS 79 TIMES.    ELTINPTS
00090      05  WS-TEMP-NOT-USED-CNT          PIC S999 COMP-3.           ELTINPTS
00091      05  WS-MOVE-LINES-IND             PIC X  VALUE 'Y'.          ELTINPTS
00092        88  WS-MOVE-LINES-TO-CIA            VALUE 'Y'.             ELTINPTS
00093                                                                   ELTINPTS
00094 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELTINPTS
00095 *     B E N   P R O V   I D S   B Y   T Y P E                   * ELTINPTS
00096 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * ELTINPTS
00097                                                                   ELTINPTS
00098  01  WS-MAX-BEN-PROV-IDS     COMP      PIC S9(03) VALUE +23.      ELTINPTS
00099  01  WS-NBR-BEN-PROV-IDS     COMP      PIC S9(03) VALUE +00.      ELTINPTS
00100                                                                   ELTINPTS
00101  01  WS-BEN-PROV-IDS.                                             ELTINPTS
00102      05  WS-INST-IP-CNT      COMP      PIC S9(04) VALUE +01.      ELTINPTS
00103      05  WS-INST-IP-TAB.                                          ELTINPTS
00104        10  FILLER                      PIC X(06)  VALUE 'SMVI B'. ELTINPTS
00105      05  WS-INST-IP-BP       REDEFINES    WS-INST-IP-TAB          ELTINPTS
00106                                        PIC X(06) OCCURS 01 TIMES. ELTINPTS
00107                                                                   ELTINPTS
00108      05  WS-INST-OP-CNT      COMP      PIC S9(04) VALUE +08.      ELTINPTS
00109      05  WS-INST-OP-TAB.                                          ELTINPTS
00110        10  FILLER                      PIC X(06)  VALUE 'EAMV B'. ELTINPTS
00111        10  FILLER                      PIC X(06)  VALUE 'EMMV B'. ELTINPTS
00112        10  FILLER                      PIC X(06)  VALUE 'FCAO B'. ELTINPTS
00113        10  FILLER                      PIC X(06)  VALUE 'FCCO B'. ELTINPTS
00114        10  FILLER                      PIC X(06)  VALUE 'FHO  B'. ELTINPTS
00115        10  FILLER                      PIC X(06)  VALUE 'GPO  B'. ELTINPTS
00116        10  FILLER                      PIC X(06)  VALUE 'IPO  B'. ELTINPTS
00117        10  FILLER                      PIC X(06)  VALUE 'SMVO B'. ELTINPTS
00118      05  WS-INST-OP-BP       REDEFINES    WS-INST-OP-TAB          ELTINPTS
00119                                        PIC X(06) OCCURS 08 TIMES. ELTINPTS
00120                                                                   ELTINPTS
00121      05  WS-PROF-IP-CNT      COMP      PIC S9(04) VALUE +23.      ELTINPTS
00122      05  WS-PROF-IP-TAB.                                          ELTINPTS
00123        10  FILLER                      PIC X(06)  VALUE 'AHI  D'. ELTINPTS
00124        10  FILLER                      PIC X(06)  VALUE 'CHCV D'. ELTINPTS
00125        10  FILLER                      PIC X(06)  VALUE 'CSVI D'. ELTINPTS
00126        10  FILLER                      PIC X(06)  VALUE 'DPV  D'. ELTINPTS
00127        10  FILLER                      PIC X(06)  VALUE 'DRI  D'. ELTINPTS
00128        10  FILLER                      PIC X(06)  VALUE 'ECFA D'. ELTINPTS
00129        10  FILLER                      PIC X(06)  VALUE 'ECFD D'. ELTINPTS
00130        10  FILLER                      PIC X(06)  VALUE 'ECFT D'. ELTINPTS
00131        10  FILLER                      PIC X(06)  VALUE 'ECFV D'. ELTINPTS
00132        10  FILLER                      PIC X(06)  VALUE 'EHV  D'. ELTINPTS
00133        10  FILLER                      PIC X(06)  VALUE 'FCAI D'. ELTINPTS
00134        10  FILLER                      PIC X(06)  VALUE 'FCCI D'. ELTINPTS
00135        10  FILLER                      PIC X(06)  VALUE 'FHI  D'. ELTINPTS
00136        10  FILLER                      PIC X(06)  VALUE 'GPI  D'. ELTINPTS
00137        10  FILLER                      PIC X(06)  VALUE 'HVD  D'. ELTINPTS
00138        10  FILLER                      PIC X(06)  VALUE 'ICV  D'. ELTINPTS
00139        10  FILLER                      PIC X(06)  VALUE 'IHV  D'. ELTINPTS
00140        10  FILLER                      PIC X(06)  VALUE 'IPI  D'. ELTINPTS
00141        10  FILLER                      PIC X(06)  VALUE 'MNI  D'. ELTINPTS
00142        10  FILLER                      PIC X(06)  VALUE 'NHV  D'. ELTINPTS
00143        10  FILLER                      PIC X(06)  VALUE 'NPV  D'. ELTINPTS
00144        10  FILLER                      PIC X(06)  VALUE 'PSI  D'. ELTINPTS
00145        10  FILLER                      PIC X(06)  VALUE 'TBV  D'. ELTINPTS
00146      05  WS-PROF-IP-BP       REDEFINES    WS-PROF-IP-TAB          ELTINPTS
00147                                        PIC X(06) OCCURS 23 TIMES. ELTINPTS
00148                                                                   ELTINPTS
00149      05  WS-PROF-OP-CNT      COMP      PIC S9(04) VALUE +13.      ELTINPTS
00150      05  WS-PROF-OP-TAB.                                          ELTINPTS
00151        10  FILLER                      PIC X(06)  VALUE 'CFOV E'. ELTINPTS
00152        10  FILLER                      PIC X(06)  VALUE 'CSVO E'. ELTINPTS
00153        10  FILLER                      PIC X(06)  VALUE 'FCAO E'. ELTINPTS
00154        10  FILLER                      PIC X(06)  VALUE 'FCCO E'. ELTINPTS
00155        10  FILLER                      PIC X(06)  VALUE 'FHO  E'. ELTINPTS
00156        10  FILLER                      PIC X(06)  VALUE 'GPO  E'. ELTINPTS
00157        10  FILLER                      PIC X(06)  VALUE 'HHV  D'. ELTINPTS
00158        10  FILLER                      PIC X(06)  VALUE 'HVIS E'. ELTINPTS
00159        10  FILLER                      PIC X(06)  VALUE 'HVO  E'. ELTINPTS
00160        10  FILLER                      PIC X(06)  VALUE 'IPO  E'. ELTINPTS
00161        10  FILLER                      PIC X(06)  VALUE 'OVIS E'. ELTINPTS
00162        10  FILLER                      PIC X(06)  VALUE 'RPNV E'. ELTINPTS
00163        10  FILLER                      PIC X(06)  VALUE 'RPOV E'. ELTINPTS
00164      05  WS-PROF-OP-BP       REDEFINES    WS-PROF-OP-TAB          ELTINPTS
00165                                        PIC X(06) OCCURS 13 TIMES. ELTINPTS
00166                                                                   ELTINPTS
00167                                                                   ELTINPTS
00168 /                L I T E R A L S                                  ELTINPTS
00169  01  WS-PROGRAM-LITERALS.                                         ELTINPTS
00170    05  WS-PERCENT                      PIC X(01) VALUE '%'.       ELTINPTS
00171    05  WS-DAYS                         PIC X(04) VALUE 'DAYS'.    ELTINPTS
00172    05  WS-YES                          PIC X(01) VALUE 'Y'.       ELTINPTS
00173    05  WS-NO                           PIC X(01) VALUE 'N'.       ELTINPTS
00174    05  WS-BASIC-LIT                    PIC X(16) VALUE            ELTINPTS
00175        '         BASIC: '.                                        ELTINPTS
00176    05  WS-SUPPLEMENT-LIT               PIC X(16) VALUE            ELTINPTS
00177        '  SUPPLEMENTAL: '.                                        ELTINPTS
00178    05  WS-SPILLOVER-COINS              PIC X(23)  VALUE           ELTINPTS
00179                'SPILLOVER COINSURANCE: '.                         ELTINPTS
00180    05  WS-SPILLOVER-DEDBL              PIC X(22)  VALUE           ELTINPTS
00181                'SPILLOVER DEDUCTIBLE: '.                          ELTINPTS
00182    05  WS-SERVICES-RENDERED.                                      ELTINPTS
00183      10  FILLER                        PIC X(26)  VALUE           ELTINPTS
00184                'SERVICES MAY BE RENDERED: '.                      ELTINPTS
00185    05  WS-PAYMNT-BASED.                                           ELTINPTS
00186      10  FILLER                        PIC X(20)  VALUE           ELTINPTS
00187                'PAYMENT IS BASED ON:'.                            ELTINPTS
00188                                                                   ELTINPTS
00189 /            D I S P L A Y   L I N E S                            ELTINPTS
00190  01  WS-ELS-DISPLAY-LINES.                                        ELTINPTS
00191    05  WS-HDR-2-IP-INST.                                          ELTINPTS
00192      10  FILLER                    PIC X(24) VALUE SPACES.        ELTINPTS
00193      10  FILLER                    PIC X(31) VALUE                ELTINPTS
00194             'VISITS, INPATIENT INSTITUTIONAL'.                    ELTINPTS
00195      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTINPTS
00196                                                                   ELTINPTS
00197    05  WS-HDR-2-OP-INST.                                          ELTINPTS
00198      10  FILLER                    PIC X(23) VALUE SPACES.        ELTINPTS
00199      10  FILLER                    PIC X(32) VALUE                ELTINPTS
00200             'VISITS, OUTPATIENT INSTITUTIONAL'.                   ELTINPTS
00201      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTINPTS
00202                                                                   ELTINPTS
00203    05  WS-HDR-2-IP-PROF.                                          ELTINPTS
00204      10  FILLER                    PIC X(24) VALUE SPACES.        ELTINPTS
00205      10  FILLER                    PIC X(30) VALUE                ELTINPTS
00206             'VISITS, INPATIENT PROFESSIONAL'.                     ELTINPTS
00207      10  FILLER                    PIC X(25) VALUE LOW-VALUES.    ELTINPTS
00208                                                                   ELTINPTS
00209    05  WS-HDR-2-OP-PROF.                                          ELTINPTS
00210      10  FILLER                    PIC X(24) VALUE SPACES.        ELTINPTS
00211      10  FILLER                    PIC X(31) VALUE                ELTINPTS
00212             'VISITS, OUTPATIENT PROFESSIONAL'.                    ELTINPTS
00213      10  FILLER                    PIC X(24) VALUE LOW-VALUES.    ELTINPTS
00214                                                                   ELTINPTS
00215    05  WS-VISITS.                                                 ELTINPTS
00216      10  FILLER                    PIC X(07) VALUE                ELTINPTS
00217                'VISITS '.                                         ELTINPTS
00218      10  FILLER                    PIC X(65) VALUE LOW-VALUES.    ELTINPTS
00219                                                                   ELTINPTS
00220    05  WS-FOLLOW-BENEFIT.                                         ELTINPTS
00221      10  FILLER                    PIC X(79) VALUE                ELTINPTS
00222          'COVERED SERVICES ARE:  '.                               ELTINPTS
00223                                                                   ELTINPTS
00224    05  WS-FOLLOW-BEN-CON.                                         ELTINPTS
00225      10  FILLER                    PIC X(79) VALUE                ELTINPTS
00226          'COVERED SERVICES (CONTINUED) '.                         ELTINPTS
00227                                                                   ELTINPTS
00228    05  WS-PAY-CONSDR-TEXT1.                                       ELTINPTS
00229      10  FILLER                    PIC X(45) VALUE                ELTINPTS
00230               'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTINPTS
00231                                                                   ELTINPTS
00232    05  WS-PAY-CONSDR-TEXT2.                                       ELTINPTS
00233      10  FILLER                    PIC X(44) VALUE                ELTINPTS
00234               'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTINPTS
00235                                                                   ELTINPTS
00236    05  WS-SERVICES-2ND.                                           ELTINPTS
00237      10  FILLER                    PIC X(21) VALUE SPACES.        ELTINPTS
00238      10  WS-DTL-SERVICES-2ND       PIC X(55) VALUE SPACES.        ELTINPTS
00239      10  FILLER                    PIC X(03) VALUE LOW-VALUES.    ELTINPTS
00240                                                                   ELTINPTS
00241    05  WS-SERVICES-PAYABLE.                                       ELTINPTS
00242      10  FILLER                    PIC X(45) VALUE                ELTINPTS
00243          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTINPTS
00244      10  FILLER                    PIC X(34) VALUE LOW-VALUES.    ELTINPTS
00245                                                                   ELTINPTS
00246    05  WS-BASIC.                                                  ELTINPTS
00247      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00248      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTINPTS
00249      10  WS-DTL-BASIC              PIC X(63) VALUE SPACES.        ELTINPTS
00250                                                                   ELTINPTS
00251    05  WS-SUPPLEMENTAL.                                           ELTINPTS
00252      10  FILLER                    PIC X(16) VALUE                ELTINPTS
00253                '  SUPPLEMENTAL: '.                                ELTINPTS
00254      10  WS-DTL-SUPPLEMENTAL       PIC X(63) VALUE SPACES.        ELTINPTS
00255                                                                   ELTINPTS
00256    05  WS-BASIC-PERCENT.                                          ELTINPTS
00257      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00258      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTINPTS
00259      10  WS-DTL-BASIC-PER          PIC X(63) VALUE SPACES.        ELTINPTS
00260                                                                   ELTINPTS
00261    05  WS-SUPPLEMENTAL-PERCENT.                                   ELTINPTS
00262      10  FILLER                    PIC X(16) VALUE                ELTINPTS
00263                '  SUPPLEMENTAL: '.                                ELTINPTS
00264      10  WS-DTL-SUPP-PER           PIC X(63) VALUE SPACES.        ELTINPTS
00265                                                                   ELTINPTS
00266    05  WS-BASIC-PER-D.                                            ELTINPTS
00267      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00268      10  FILLER                    PIC X(07) VALUE 'BASIC: '.     ELTINPTS
00269      10  WS-DTL-BASIC-PER-D        PIC X(40) VALUE SPACES.        ELTINPTS
00270      10  FILLER                    PIC X     VALUE SPACE.         ELTINPTS
00271      10  WS-DTL-BASIC-PER-D-AMT    PIC ZZZZ9.99.                  ELTINPTS
00272      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTINPTS
00273                                                                   ELTINPTS
00274    05  WS-SUPP-PER-D.                                             ELTINPTS
00275      10  FILLER                    PIC X(16) VALUE                ELTINPTS
00276                '  SUPPLEMENTAL: '.                                ELTINPTS
00277      10  WS-DTL-SUPP-PER-D         PIC X(40) VALUE SPACES.        ELTINPTS
00278      10  FILLER                    PIC X     VALUE SPACE.         ELTINPTS
00279      10  WS-DTL-SUPP-PER-D-AMT     PIC ZZZZ9.99.                  ELTINPTS
00280      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTINPTS
00281                                                                   ELTINPTS
00282    05  WS-MAX-VISITS.                                             ELTINPTS
00283      10  FILLER                    PIC X(33) VALUE                ELTINPTS
00284                'THE MAXIMUM NUMBER OF VISITS ARE:'.               ELTINPTS
00285      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTINPTS
00286                                                                   ELTINPTS
00287    05  WS-UNLIMITED.                                              ELTINPTS
00288      10  WS-DTL-UNLIMITED      PIC X(20).                         ELTINPTS
00289      10  FILLER                PIC X(26).                         ELTINPTS
00290    05  WS-MAX-DAYS REDEFINES WS-UNLIMITED.                        ELTINPTS
00291      10  FILLER                PIC X(01).                         ELTINPTS
00292      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTINPTS
00293      10  FILLER                PIC X(01).                         ELTINPTS
00294      10  WS-DAYS-LITERAL       PIC X(04).                         ELTINPTS
00295      10  FILLER                PIC X(01).                         ELTINPTS
00296      10  WS-DTL-MAX-IND        PIC X(20).                         ELTINPTS
00297      10  FILLER                PIC X(16).                         ELTINPTS
00298                                                                   ELTINPTS
00299    05  WS-DAY-PSYCH.                                              ELTINPTS
00300      10  FILLER                    PIC X(48) VALUE                ELTINPTS
00301          'DAY PSYCHIATRIC PRIOR ADMISSION REQUIREMENT IS: '.      ELTINPTS
00302      10  FILLER                    PIC X(32) VALUE LOW-VALUES.    ELTINPTS
00303                                                                   ELTINPTS
00304    05  WS-NIGHT-PSYCH.                                            ELTINPTS
00305      10  FILLER                    PIC X(50) VALUE                ELTINPTS
00306          'NIGHT PSYCHIATRIC PRIOR ADMISSION REQUIREMENT IS: '.    ELTINPTS
00307      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTINPTS
00308                                                                   ELTINPTS
00309    05  WS-DAYS-REDUCED.                                           ELTINPTS
00310      10  FILLER                    PIC X(24) VALUE                ELTINPTS
00311          ' BASIC DAYS ARE REDUCED '.                              ELTINPTS
00312      10  WS-DTL-DAYS-REDUCED-APL   PIC Z9.                        ELTINPTS
00313      10  FILLER                    PIC X(05) VALUE ' FOR '.       ELTINPTS
00314      10  WS-DTL-DAYS-REDUCED-BASE  PIC Z9.                        ELTINPTS
00315      10  FILLER                    PIC X(45) VALUE LOW-VALUES.    ELTINPTS
00316                                                                   ELTINPTS
00317    05  WS-SPILLOVER-FL-RT-PER-D.                                  ELTINPTS
00318      10  FILLER                    PIC X(29)                      ELTINPTS
00319          VALUE 'SPILLOVER FLAT RATE PER DIEM '.                   ELTINPTS
00320      10  WS-DTL-SPILLOVER-FL-RT    PIC X(46) VALUE SPACES.        ELTINPTS
00321      10  FILLER                    PIC X(04) VALUE LOW-VALUES.    ELTINPTS
00322                                                                   ELTINPTS
00323    05  WS-DIFF-PROVIDER.                                          ELTINPTS
00324      10  FILLER                    PIC X(37) VALUE                ELTINPTS
00325                'IF THE DIFFERENT PROVIDER IS BILLING '.           ELTINPTS
00326      10  FILLER                    PIC X(41) VALUE LOW-VALUES.    ELTINPTS
00327                                                                   ELTINPTS
00328    05  WS-SAME-PROVIDER.                                          ELTINPTS
00329      10  FILLER                    PIC X(32) VALUE                ELTINPTS
00330                'IF THE SAME PROVIDER IS BILLING '.                ELTINPTS
00331      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTINPTS
00332                                                                   ELTINPTS
00333    05  WS-MEDI-SURG-OB.                                           ELTINPTS
00334      10  FILLER                    PIC X(22) VALUE                ELTINPTS
00335                '   MEDICAL/SURGERY/OB '.                          ELTINPTS
00336      10  FILLER                    PIC X(57) VALUE LOW-VALUES.    ELTINPTS
00337                                                                   ELTINPTS
00338    05  WS-MEDI-INP-GT-FC-PSYCH.                                   ELTINPTS
00339      10  FILLER                    PIC X(65) VALUE                ELTINPTS
00340                '   MEDICAL/INPATIENT GROUP THERAPY/FAMILY COUNSELIELTINPTS
00341 -              '/PSYCHOTHERAPY'.                                  ELTINPTS
00342      10  FILLER                    PIC X(14) VALUE LOW-VALUES.    ELTINPTS
00343                                                                   ELTINPTS
00344    05  WS-MEDI-INP-RAD-THERPY.                                    ELTINPTS
00345      10  FILLER                    PIC X(40) VALUE                ELTINPTS
00346                '   MEDICAL/INPATIENT RADIATION THERAPY'.          ELTINPTS
00347      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTINPTS
00348                                                                   ELTINPTS
00349    05  WS-MEDI-INP-CHEMTHERPY.                                    ELTINPTS
00350      10  FILLER                    PIC X(33) VALUE                ELTINPTS
00351                '   MEDICAL/INPATIENT CHEMOTHERAPY'.               ELTINPTS
00352      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTINPTS
00353                                                                   ELTINPTS
00354    05  WS-MEDI-MEDI.                                              ELTINPTS
00355      10  FILLER                    PIC X(18) VALUE                ELTINPTS
00356                '   MEDICAL/MEDICAL'.                              ELTINPTS
00357      10  FILLER                    PIC X(61) VALUE LOW-VALUES.    ELTINPTS
00358                                                                   ELTINPTS
00359    05  WS-BASIC-1-1.                                              ELTINPTS
00360      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00361      10  WS-DTL-BASIC-1-1          PIC X(70) VALUE SPACES.        ELTINPTS
00362                                                                   ELTINPTS
00363    05  WS-BASIC-1-2.                                              ELTINPTS
00364      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00365      10  WS-DTL-BASIC-1-2          PIC X(70) VALUE SPACES.        ELTINPTS
00366                                                                   ELTINPTS
00367    05  WS-BASIC-2-1.                                              ELTINPTS
00368      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00369      10  WS-DTL-BASIC-2-1          PIC X(70) VALUE SPACES.        ELTINPTS
00370                                                                   ELTINPTS
00371    05  WS-BASIC-2-2.                                              ELTINPTS
00372      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00373      10  WS-DTL-BASIC-2-2          PIC X(70) VALUE SPACES.        ELTINPTS
00374                                                                   ELTINPTS
00375    05  WS-BASIC-3-1.                                              ELTINPTS
00376      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00377      10  WS-DTL-BASIC-3-1          PIC X(70) VALUE SPACES.        ELTINPTS
00378                                                                   ELTINPTS
00379    05  WS-BASIC-3-2.                                              ELTINPTS
00380      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00381      10  WS-DTL-BASIC-3-2          PIC X(70) VALUE SPACES.        ELTINPTS
00382                                                                   ELTINPTS
00383    05  WS-BASIC-4-1.                                              ELTINPTS
00384      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00385      10  WS-DTL-BASIC-4-1          PIC X(70) VALUE SPACES.        ELTINPTS
00386                                                                   ELTINPTS
00387    05  WS-BASIC-4-2.                                              ELTINPTS
00388      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00389      10  WS-DTL-BASIC-4-2          PIC X(70) VALUE SPACES.        ELTINPTS
00390                                                                   ELTINPTS
00391    05  WS-SUPP-1-1.                                               ELTINPTS
00392      10 FILLER                     PIC X(17) VALUE                ELTINPTS
00393          '   SUPPLEMENTAL: '.                                     ELTINPTS
00394      10  WS-DTL-SUPP-1-1           PIC X(62) VALUE SPACES.        ELTINPTS
00395                                                                   ELTINPTS
00396    05  WS-SUPP-1-2.                                               ELTINPTS
00397      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00398      10  WS-DTL-SUPP-1-2           PIC X(70) VALUE SPACES.        ELTINPTS
00399                                                                   ELTINPTS
00400    05  WS-SUPP-2-1.                                               ELTINPTS
00401      10 FILLER                     PIC X(17) VALUE                ELTINPTS
00402          '   SUPPLEMENTAL: '.                                     ELTINPTS
00403      10  WS-DTL-SUPP-2-1           PIC X(62) VALUE SPACES.        ELTINPTS
00404                                                                   ELTINPTS
00405    05  WS-SUPP-2-2.                                               ELTINPTS
00406      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00407      10  WS-DTL-SUPP-2-2           PIC X(70) VALUE SPACES.        ELTINPTS
00408                                                                   ELTINPTS
00409    05  WS-SUPP-3-1.                                               ELTINPTS
00410      10 FILLER                     PIC X(17) VALUE                ELTINPTS
00411          '   SUPPLEMENTAL: '.                                     ELTINPTS
00412      10  WS-DTL-SUPP-3-1           PIC X(62) VALUE SPACES.        ELTINPTS
00413                                                                   ELTINPTS
00414    05  WS-SUPP-3-2.                                               ELTINPTS
00415      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00416      10  WS-DTL-SUPP-3-2           PIC X(70) VALUE SPACES.        ELTINPTS
00417                                                                   ELTINPTS
00418    05  WS-SUPP-4-1.                                               ELTINPTS
00419      10 FILLER                     PIC X(17) VALUE                ELTINPTS
00420          '   SUPPLEMENTAL: '.                                     ELTINPTS
00421      10  WS-DTL-SUPP-4-1           PIC X(62) VALUE SPACES.        ELTINPTS
00422                                                                   ELTINPTS
00423    05  WS-SUPP-4-2.                                               ELTINPTS
00424      10  FILLER                    PIC X(09) VALUE SPACES.        ELTINPTS
00425      10  WS-DTL-SUPP-4-2           PIC X(70) VALUE SPACES.        ELTINPTS
00426                                                                   ELTINPTS
00427    05  WS-CONTACT-CONTRACT.                                       ELTINPTS
00428      10  FILLER                    PIC X(50) VALUE                ELTINPTS
00429              ' PRICING METHOD NOT CODED CONTACT: CONTRACT CODING'.ELTINPTS
00430      10  FILLER                    PIC X(29) VALUE LOW-VALUES.    ELTINPTS
00431                                                                   ELTINPTS
00432    05  WS-CONTRACT-RELATED.                                       ELTINPTS
00433      10  FILLER                    PIC X(49) VALUE                ELTINPTS
00434              'THIS CONTRACT HAS RELATED SERVICES CONSIDERATIONS'. ELTINPTS
00435      10  FILLER                    PIC X(30) VALUE LOW-VALUES.    ELTINPTS
00436                                                                   ELTINPTS
00437    05  WS-PVE-TEXT.                                               ELTINPTS
00438      10  FILLER                    PIC X(44) VALUE                ELTINPTS
00439        'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.            ELTINPTS
00440                                                                   ELTINPTS
00441    05  WS-INHOSP-MEDICAL-SERV      PIC X(68) VALUE                ELTINPTS
00442        ' IN HOSPITAL MEDICAL SERVICES NOT COVERED AS A INSTITUTIONELTINPTS
00443 -      'AL CHARGE'.                                               ELTINPTS
00444                                                                   ELTINPTS
00445    05  WS-MAX-AMT-TEXT.                                           ELTINPTS
00446      10  FILLER                  PIC  X(32) VALUE                 ELTINPTS
00447              'THE MAXIMUM AMOUNT PER VISIT IS '.                  ELTINPTS
00448      10  FILLER                  PIC  X(47) VALUE LOW-VALUES.     ELTINPTS
00449                                                                   ELTINPTS
00450    05  WS-MAXIMUM-AMT.                                            ELTINPTS
00451      10 WS-BASIC-SUPP            PIC  X(16) VALUE SPACES.         ELTINPTS
00452      10 WS-EDIT-MAX-AMT          PIC  ZZ9.99-.                    ELTINPTS
00453                                                                   ELTINPTS
00454    05  WS-PROF-CHRG-HSP-TEXT.                                     ELTINPTS
00455        10  FILLER                PIC  X(61) VALUE                 ELTINPTS
00456            'IF PROFESSIONAL CHARGES ARE BILLED ON INPATIENT CARE RELTINPTS
00457 -          'EPORT: '.                                             ELTINPTS
00458        10  FILLER                PIC  X(18) VALUE LOW-VALUES.     ELTINPTS
00459                                                                   ELTINPTS
00460    05  WS-FOR-BASIC                PIC X(13)                      ELTINPTS
00461         VALUE '  FOR BASIC: '.                                    ELTINPTS
00462                                                                   ELTINPTS
00463    05  WS-FOR-SUPP                 PIC X(20)                      ELTINPTS
00464         VALUE '  FOR SUPPLEMENTAL: '.                             ELTINPTS
00465                                                                   ELTINPTS
00466    05  WS-AND-MUST-BEGIN.                                         ELTINPTS
00467      10  FILLER                    PIC X(40)                      ELTINPTS
00468                  VALUE ' FOR THIS SERVICE AND MUST BEGIN WITHIN '.ELTINPTS
00469      10  WS-MUST-BEGIN-DAYS        PIC ZZ9.                       ELTINPTS
00470      10  FILLER                    PIC X(20)                      ELTINPTS
00471                                      VALUE ' DAYS FROM DISCHARGE'.ELTINPTS
00472                                                                   ELTINPTS
00473    05  WS-BASIC-THE.                                              ELTINPTS
00474      10  FILLER                    PIC X(20)                      ELTINPTS
00475          VALUE '         BASIC: THE '.                            ELTINPTS
00476                                                                   ELTINPTS
00477    05  WS-SUPP-THE.                                               ELTINPTS
00478      10  FILLER                    PIC X(20)                      ELTINPTS
00479          VALUE '  SUPPLEMENTAL: THE '.                            ELTINPTS
00480                                                                   ELTINPTS
00481    05  WS-FOR-SUPP-ACC-TEXT.                                      ELTINPTS
00482      10  FILLER                    PIC X(26)                      ELTINPTS
00483        VALUE 'FOR SUPPLEMENTAL ACCIDENT '.                        ELTINPTS
00484      10  WS-NO-DAYS                PIC ZZ9.                       ELTINPTS
00485      10  FILLER                    PIC X(09) VALUE ' DAYS OF '.   ELTINPTS
00486      10  WS-FOR-SUPP-ACC-DESC      PIC X(41).                     ELTINPTS
00487                                                                   ELTINPTS
00488    05  WS-ELIG-METH-TREAT-TEXT.                                   ELTINPTS
00489      10  FILLER                    PIC X(35)                      ELTINPTS
00490                       VALUE 'THE ELIGIBLE METHOD OF TREATMENT IS'.ELTINPTS
00491                                                                   ELTINPTS
00492    05  WS-RECERT-TEXT.                                            ELTINPTS
00493      10  FILLER                   PIC  X(56) VALUE                ELTINPTS
00494              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTINPTS
00495 -            'IS: '.                                              ELTINPTS
00496      10  FILLER                   PIC  X(23) VALUE LOW-VALUES.    ELTINPTS
00497                                                                   ELTINPTS
00498    05  WS-EXCEPTION-SCHED.                                        ELTINPTS
00499      10  FILLER                  PIC  X(49) VALUE                 ELTINPTS
00500              'BENEFITS ARE PRICED BASED ON EXCEPTION SCHEDULE: '. ELTINPTS
00501      10  FILLER                  PIC  X(30) VALUE LOW-VALUES.     ELTINPTS
00502                                                                   ELTINPTS
00503  01  WS-END                            PIC X(16) VALUE            ELTINPTS
00504      '*** W/S ENDS ***'.                                          ELTINPTS
00505 *             L I N K A G E   S E C T I O N                       ELTINPTS
00506  LINKAGE SECTION.                                                 ELTINPTS
00507  01  DFHCOMMAREA.                                                 ELTINPTS
00508      COPY ELSCOMMC.                                               ELTINPTS
00509 *  *** CIA  AREA ***                                              ELTINPTS
00510      COPY ELSCIA2C.                                               ELTINPTS
00511 *  *** IO PARM AREA ***                                           ELTINPTS
00512      COPY ELSIOPMC.                                               ELTINPTS
00513 *  *** KEY AREA ***                                               ELTINPTS
00514      COPY ELSKEYSC.                                               ELTINPTS
00515 *  *** OUTPUT TEXT AREA ***                                       ELTINPTS
00516      COPY ELSOUTPC.                                               ELTINPTS
00517 *  *** TOPIC SELECTION AREA ***                                   ELTINPTS
00518      COPY ELSSSCBC.                                               ELTINPTS
00519 *  *** CODE MANUAL INTERFACE ***                                  ELTINPTS
00520      COPY ELSCMIFC.                                               ELTINPTS
00521 *  *** CODE MANUAL DESCRIPTION AREA ***                           ELTINPTS
00522      COPY ELSCMDSC.                                               ELTINPTS
00523 *  *** BENEFIT PROVISION TABLE ***                                ELTINPTS
00524      COPY ELSPRVNC.                                               ELTINPTS
00525 *  *** COMPRESSION TEXT WORK-AREA ***                             ELTINPTS
00526      COPY ELSTCWAC.                                               ELTINPTS
00527 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTINPTS
00528      COPY ELSPLGSW.                                               ELTINPTS
00529                                                                   ELTINPTS
00530 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTINPTS
00531      COPY ELSPLGTB.                                               ELTINPTS
00532                                                                   ELTINPTS
00533 *        G R O U P   S P E C I F I C   R E C O R D                ELTINPTS
00534  01  GROUP-SPECIFIC-RECORD.                                       ELTINPTS
00535      COPY GCGROUPC.                                               ELTINPTS
00536                                                                   ELTINPTS
00537 *        C O N T R A C T   R E C O R D                            ELTINPTS
00538  01  CONTRACT-RECORD.                                             ELTINPTS
00539      COPY GCCONTRC.                                               ELTINPTS
00540                                                                   ELTINPTS
00541  PROCEDURE DIVISION.                                              ELTINPTS
00542  0000-MAINLINE.                                                   ELTINPTS
00543      PERFORM 1000-INITIALIZATION.                                 ELTINPTS
00544                                                                   ELTINPTS
00545      IF (SSB-PROV-CLASS-INST  OR  SSB-PROV-CLASS-BOTH)            ELTINPTS
00546                       AND                                         ELTINPTS
00547         (SSB-SERV-CLASS-IP    OR  SSB-SERV-CLASS-BOTH)            ELTINPTS
00548          PERFORM 2000-INSTITUTIONAL-IP.                           ELTINPTS
00549                                                                   ELTINPTS
00550      IF (SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH)            ELTINPTS
00551                       AND                                         ELTINPTS
00552         (SSB-SERV-CLASS-IP    OR  SSB-SERV-CLASS-BOTH)            ELTINPTS
00553          PERFORM 3000-PROFESSIONAL-IP.                            ELTINPTS
00554                                                                   ELTINPTS
00555      IF (SSB-PROV-CLASS-INST  OR  SSB-PROV-CLASS-BOTH)            ELTINPTS
00556                       AND                                         ELTINPTS
00557         (SSB-SERV-CLASS-OP    OR  SSB-SERV-CLASS-BOTH)            ELTINPTS
00558          PERFORM 4000-INSTITUTIONAL-OP.                           ELTINPTS
00559                                                                   ELTINPTS
00560      IF (SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH)            ELTINPTS
00561                       AND                                         ELTINPTS
00562         (SSB-SERV-CLASS-OP    OR  SSB-SERV-CLASS-BOTH)            ELTINPTS
00563          PERFORM 5000-PROFESSIONAL-OP.                            ELTINPTS
00564                                                                   ELTINPTS
00565      IF (SSB-PROV-CLASS-INST  OR                                  ELTINPTS
00566          SSB-PROV-CLASS-PROF  OR  SSB-PROV-CLASS-BOTH)            ELTINPTS
00567                            AND                                    ELTINPTS
00568         (SSB-SERV-CLASS-IP    OR  SSB-SERV-CLASS-OP               ELTINPTS
00569                               OR  SSB-SERV-CLASS-BOTH)            ELTINPTS
00570                         CONTINUE                                  ELTINPTS
00571      ELSE                                                         ELTINPTS
00572          SET CIA-AB-UNDEF TO TRUE                                 ELTINPTS
00573          EXEC CICS ABEND  ABCODE(CIA-ABCODE)  END-EXEC.           ELTINPTS
00574                                                                   ELTINPTS
00575      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTINPTS
00576                                                                   ELTINPTS
00577      SET CIA-STG-FREEMAIN TO TRUE.                                ELTINPTS
00578      EXEC CICS  LINK  PROGRAM('ELUSTGMG')                         ELTINPTS
00579                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
00580                                                                   ELTINPTS
00581      MOVE 'E'   TO  COF-FUNCTION.                                 ELTINPTS
00582      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTINPTS
00583                                                                   ELTINPTS
00584      EXEC CICS LINK PROGRAM('ELUOUTPT')                           ELTINPTS
00585                     COMMAREA(DFHCOMMAREA)  END-EXEC.              ELTINPTS
00586                                                                   ELTINPTS
00587      GOBACK.                                                      ELTINPTS
00588                                                                   ELTINPTS
00589 ****************************************************************  ELTINPTS
00590 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTINPTS
00591 ****************************************************************  ELTINPTS
00592  1000-INITIALIZATION.                                             ELTINPTS
00593      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTINPTS
00594         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTINPTS
00595         EXEC CICS   ABEND ABCODE('EL01')   END-EXEC.              ELTINPTS
00596                                                                   ELTINPTS
00597      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTINPTS
00598          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTINPTS
00599                                                                   ELTINPTS
00600      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTINPTS
00601      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00602          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTINPTS
00603                                                                   ELTINPTS
00604      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTINPTS
00605      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00606          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTINPTS
00607                                                                   ELTINPTS
00608      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTINPTS
00609      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00610          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTINPTS
00611                                                                   ELTINPTS
00612      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTINPTS
00613      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00614          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTINPTS
00615                                                                   ELTINPTS
00616      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTINPTS
00617      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00618          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTINPTS
00619                                                                   ELTINPTS
00620      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTINPTS
00621      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00622          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTINPTS
00623                                                                   ELTINPTS
00624      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTINPTS
00625                                                                   ELTINPTS
00626      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTINPTS
00627             (WS-MAX-BEN-PROV-IDS * LENGTH OF PVN-BEN-PROVN-TBL).  ELTINPTS
00628                                                                   ELTINPTS
00629      SET CIA-STG-GETMAIN  TO TRUE.                                ELTINPTS
00630      EXEC CICS LINK  PROGRAM('ELUSTGMG')                          ELTINPTS
00631                      COMMAREA(DFHCOMMAREA)  END-EXEC.             ELTINPTS
00632                                                                   ELTINPTS
00633      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTINPTS
00634      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00635          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTINPTS
00636                                                                   ELTINPTS
00637      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTINPTS
00638                                                                   ELTINPTS
00639 ******************************************************************ELTINPTS
00640 *        I N S T I T U T I O N A L   I P                         *ELTINPTS
00641 ******************************************************************ELTINPTS
00642  2000-INSTITUTIONAL-IP.                                           ELTINPTS
00643      MOVE WS-HDR-2-IP-INST TO COF-HDR-LINE(2).                    ELTINPTS
00644      SET WS-FIRST-TIME TO TRUE.                                   ELTINPTS
00645                                                                   ELTINPTS
00646      PERFORM 9200-HEADER-OUTPUT-REQUEST.                          ELTINPTS
00647                                                                   ELTINPTS
00648      MOVE WS-INST-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTINPTS
00649                                                                   ELTINPTS
00650      PERFORM 2010-LOAD-PVN-BEN-PROVN-TABLE                        ELTINPTS
00651         VARYING WS-SUB FROM +1 BY +1                              ELTINPTS
00652            UNTIL WS-SUB  GREATER THAN  WS-INST-IP-CNT.            ELTINPTS
00653                                                                   ELTINPTS
00654      PERFORM 6000-CALL-COVERAGE.                                  ELTINPTS
00655                                                                   ELTINPTS
00656      IF PVN-COVG-NONE                                             ELTINPTS
00657         CONTINUE                                                  ELTINPTS
00658      ELSE                                                         ELTINPTS
00659         PERFORM 2020-PROCESS-COVERED-PROVISION                    ELTINPTS
00660      END-IF.                                                      ELTINPTS
00661                                                                   ELTINPTS
00662  2010-LOAD-PVN-BEN-PROVN-TABLE.                                   ELTINPTS
00663      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
00664      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTINPTS
00665           TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).                ELTINPTS
00666      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)            ELTINPTS
00667                   PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).           ELTINPTS
00668                                                                   ELTINPTS
00669  2020-PROCESS-COVERED-PROVISION.                                  ELTINPTS
00670      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTINPTS
00671                                                                   ELTINPTS
00672      MOVE '1' TO PSB-MAX-AMT-PER-VISIT,                           ELTINPTS
00673                  PSB-HOSP-ADM-RESTRN-IND,                         ELTINPTS
00674                  PSB-HOSP-COND-RELATSP-IND.                       ELTINPTS
00675                                                                   ELTINPTS
00676      PERFORM 6100-INTLZE-PSP-PYMNT-LVL-SWTS.                      ELTINPTS
00677                                                                   ELTINPTS
00678      PERFORM WITH TEST BEFORE                                     ELTINPTS
00679              VARYING WS-SUB  FROM  +1  BY  +1                     ELTINPTS
00680              UNTIL WS-SUB  >  WS-INST-IP-CNT                      ELTINPTS
00681                                                                   ELTINPTS
00682         SET PVN-BEN-PROVN-IDX TO WS-SUB                           ELTINPTS
00683         IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO        ELTINPTS
00684            PERFORM 2030-BUILD-SCREEN                              ELTINPTS
00685         END-IF                                                    ELTINPTS
00686                                                                   ELTINPTS
00687      END-PERFORM.                                                 ELTINPTS
00688                                                                   ELTINPTS
00689 ****************************************************************  ELTINPTS
00690 *          BUILD SCREEN                                        *  ELTINPTS
00691 ****************************************************************  ELTINPTS
00692  2030-BUILD-SCREEN.                                               ELTINPTS
00693      SET PLT-INDEX1 TO                                            ELTINPTS
00694         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTINPTS
00695                                                                   ELTINPTS
00696      IF WS-NOT-FIRST-TIME                                         ELTINPTS
00697         SET COF-NEW-PAGE TO TRUE                                  ELTINPTS
00698         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTINPTS
00699         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTINPTS
00700                          COMMAREA(DFHCOMMAREA)  END-EXEC          ELTINPTS
00701      ELSE                                                         ELTINPTS
00702         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTINPTS
00703                                                                   ELTINPTS
00704      MOVE +1    TO  WS-CIA.                                       ELTINPTS
00705      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTINPTS
00706                                                                   ELTINPTS
00707      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)        =  ZEROES      ELTINPTS
00708         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES      ELTINPTS
00709            SET PLT-INDEX2  TO  2                                  ELTINPTS
00710            PERFORM 2040-BUILD-SCREEN-LINES                        ELTINPTS
00711         ELSE                                                      ELTINPTS
00712            PERFORM 8100-DISPLAY-INDICES-PROBLEM                   ELTINPTS
00713      ELSE                                                         ELTINPTS
00714         SET PLT-INDEX2  TO  1                                     ELTINPTS
00715         PERFORM 2040-BUILD-SCREEN-LINES.                          ELTINPTS
00716                                                                   ELTINPTS
00717 ****************************************************************  ELTINPTS
00718 *          BUILD SCREEN LINES                                  *  ELTINPTS
00719 ****************************************************************  ELTINPTS
00720  2040-BUILD-SCREEN-LINES.                                         ELTINPTS
00721      MOVE WS-INST-IP-CNT TO WS-NBR-BEN-PROV-IDS.                  ELTINPTS
00722      PERFORM 6300-LIST-BEN-PROVS.                                 ELTINPTS
00723                                                                   ELTINPTS
00724      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
00725             NOT  =  ZEROS                                         ELTINPTS
00726         PERFORM 6200-PLACE-OF-TREATMENT.                          ELTINPTS
00727                                                                   ELTINPTS
00728      PERFORM 6500-PROVN-PRICING-METHD.                            ELTINPTS
00729      PERFORM 2300-MAX-AMT-PER-VISIT.                              ELTINPTS
00730      PERFORM 2500-HOSP-ADM-RESTRN.                                ELTINPTS
00731      PERFORM 2600-HOSP-COND-REL-IND.                              ELTINPTS
00732                                                                   ELTINPTS
00733      PERFORM 8000-SPILLOVER-COINS-N-DED.                          ELTINPTS
00734      PERFORM 8400-TRANSF-OTHER-RESP-IND.                          ELTINPTS
00735      PERFORM 8500-SCAN-TAB.                                       ELTINPTS
00736      PERFORM 8600-PAY-CONSID-TEXT.                                ELTINPTS
00737                                                                   ELTINPTS
00738 ****************************************************************  ELTINPTS
00739 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTINPTS
00740 ****************************************************************  ELTINPTS
00741  2300-MAX-AMT-PER-VISIT.                                          ELTINPTS
00742      SET  PLT-INDEX2  TO  1.                                      ELTINPTS
00743      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTINPTS
00744         IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
00745                                                  NOT =  ZERO      ELTINPTS
00746            ADD +1                  TO  WS-CIA                     ELTINPTS
00747            MOVE WS-MAX-AMT-TEXT    TO  COF-DTL-LINE (WS-CIA)      ELTINPTS
00748            MOVE WS-BASIC-LIT       TO  WS-BASIC-SUPP              ELTINPTS
00749            MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)    ELTINPTS
00750                                    TO  WS-EDIT-MAX-AMT            ELTINPTS
00751            ADD  +1                 TO  WS-CIA                     ELTINPTS
00752            MOVE WS-MAXIMUM-AMT     TO  COF-DTL-LINE (WS-CIA)      ELTINPTS
00753            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
00754                                                                   ELTINPTS
00755      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
00756      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTINPTS
00757         IF PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
00758                                                 NOT  =  ZERO      ELTINPTS
00759            ADD +1                   TO  WS-CIA                    ELTINPTS
00760            MOVE WS-MAX-AMT-TEXT     TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
00761            MOVE WS-SUPPLEMENT-LIT   TO  WS-BASIC-SUPP             ELTINPTS
00762            MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)    ELTINPTS
00763                                     TO  WS-EDIT-MAX-AMT           ELTINPTS
00764            ADD  +1                  TO  WS-CIA                    ELTINPTS
00765            MOVE WS-MAXIMUM-AMT      TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
00766            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
00767                                                                   ELTINPTS
00768 ******************************************************************ELTINPTS
00769 *    H O S P I T A L   A D M I S S I O N   R E S T R I C T I O N *ELTINPTS
00770 ******************************************************************ELTINPTS
00771  2500-HOSP-ADM-RESTRN.                                            ELTINPTS
00772      SET PLT-INDEX2  TO  1.                                       ELTINPTS
00773      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)   NOT =  ZEROES AND   ELTINPTS
00774         PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTINPTS
00775         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTINPTS
00776         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTINPTS
00777         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00778                                     TO  CMF-CODE-VALUE            ELTINPTS
00779         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
00780         MOVE WS-FOR-BASIC  TO  WS-TEMP-TEXT-AREA                  ELTINPTS
00781         MOVE +66           TO  WS-TEMP-NOT-USED-CNT               ELTINPTS
00782         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00783                                 TO  WS-MUST-BEGIN-DAYS            ELTINPTS
00784         MOVE WS-AND-MUST-BEGIN  TO  WS-TEMP-TEXT-AREA             ELTINPTS
00785         PERFORM 9400-CODES-MANUAL-LONG                            ELTINPTS
00786         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
00787                                                                   ELTINPTS
00788      SET PLT-INDEX2  TO  2.                                       ELTINPTS
00789      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZEROES AND   ELTINPTS
00790         PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0' ELTINPTS
00791         MOVE 'BPB'  TO  CMF-RECORD-PREFIX                         ELTINPTS
00792         MOVE 'HOSP-ADM-RESTRN-IND'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTINPTS
00793         MOVE PLB-HOSP-ADM-RESTRN-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00794                                     TO  CMF-CODE-VALUE            ELTINPTS
00795         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
00796         MOVE WS-FOR-SUPP  TO  WS-TEMP-TEXT-AREA                   ELTINPTS
00797         MOVE +59          TO  WS-TEMP-NOT-USED-CNT                ELTINPTS
00798         MOVE PLB-HSP-ADM-RESTRN-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00799                                 TO  WS-MUST-BEGIN-DAYS            ELTINPTS
00800         MOVE WS-AND-MUST-BEGIN  TO  WS-TEMP-TEXT-AREA             ELTINPTS
00801         PERFORM 9400-CODES-MANUAL-LONG                            ELTINPTS
00802         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
00803                                                                   ELTINPTS
00804 ****************************************************************  ELTINPTS
00805 *      H O S P I T A L   C O N D I T I O N   R E L .   I N D . *  ELTINPTS
00806 ****************************************************************  ELTINPTS
00807  2600-HOSP-COND-REL-IND.                                          ELTINPTS
00808      SET PLT-INDEX2  TO  1.                                       ELTINPTS
00809      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
00810         IF PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00811                                               NOT =  ZERO         ELTINPTS
00812            MOVE 'BPB'  TO  CMF-RECORD-PREFIX                      ELTINPTS
00813            MOVE 'HOSP-COND-RELATSP-IND'                           ELTINPTS
00814                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
00815            MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2) ELTINPTS
00816                                    TO  CMF-CODE-VALUE             ELTINPTS
00817            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
00818            MOVE WS-BASIC-THE  TO  WS-TEMP-TEXT-AREA               ELTINPTS
00819            MOVE +59           TO  WS-TEMP-NOT-USED-CNT            ELTINPTS
00820            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
00821            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
00822                                                                   ELTINPTS
00823      SET PLT-INDEX2  TO  2.                                       ELTINPTS
00824      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)      NOT =  ZEROES    ELTINPTS
00825         IF PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
00826                                                  NOT =  ZERO      ELTINPTS
00827            MOVE 'BPB'  TO  CMF-RECORD-PREFIX                      ELTINPTS
00828            MOVE 'HOSP-COND-RELATSP-IND'                           ELTINPTS
00829                                    TO  CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
00830            MOVE PLB-HOSP-COND-RELATSP-IND(PLT-INDEX1, PLT-INDEX2) ELTINPTS
00831                                    TO  CMF-CODE-VALUE             ELTINPTS
00832            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
00833            MOVE WS-SUPP-THE   TO  WS-TEMP-TEXT-AREA               ELTINPTS
00834            MOVE +59           TO  WS-TEMP-NOT-USED-CNT            ELTINPTS
00835            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
00836            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
00837                                                                   ELTINPTS
00838 ******************************************************************ELTINPTS
00839 *        P R O F E S S I O N A L   I P                           *ELTINPTS
00840 ******************************************************************ELTINPTS
00841  3000-PROFESSIONAL-IP.                                            ELTINPTS
00842      MOVE WS-HDR-2-IP-PROF    TO  COF-HDR-LINE(2).                ELTINPTS
00843      SET WS-FIRST-TIME TO TRUE.                                   ELTINPTS
00844                                                                   ELTINPTS
00845      PERFORM 9200-HEADER-OUTPUT-REQUEST.                          ELTINPTS
00846                                                                   ELTINPTS
00847      MOVE WS-PROF-IP-CNT TO PVN-NBR-BEN-PROVN.                    ELTINPTS
00848                                                                   ELTINPTS
00849      PERFORM 3010-LOAD-PVN-BEN-PROVN-TABLE                        ELTINPTS
00850         VARYING WS-SUB FROM +1 BY +1                              ELTINPTS
00851            UNTIL WS-SUB  GREATER THAN  WS-PROF-IP-CNT .           ELTINPTS
00852                                                                   ELTINPTS
00853      PERFORM 6000-CALL-COVERAGE.                                  ELTINPTS
00854                                                                   ELTINPTS
00855      IF PVN-COVG-NONE                                             ELTINPTS
00856         CONTINUE                                                  ELTINPTS
00857      ELSE                                                         ELTINPTS
00858         PERFORM 3020-PROCESS-COVERED-PROVISION                    ELTINPTS
00859      END-IF.                                                      ELTINPTS
00860                                                                   ELTINPTS
00861  3010-LOAD-PVN-BEN-PROVN-TABLE.                                   ELTINPTS
00862      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
00863      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTINPTS
00864           TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).                ELTINPTS
00865      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)            ELTINPTS
00866           PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).                   ELTINPTS
00867                                                                   ELTINPTS
00868  3020-PROCESS-COVERED-PROVISION.                                  ELTINPTS
00869      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTINPTS
00870                                                                   ELTINPTS
00871      MOVE '1' TO PSD-BEN-SCOPE-ID,                                ELTINPTS
00872                    PSD-FLAT-RATE-PDM-AMT,                         ELTINPTS
00873                    PSD-BEN-MAX-VISIT-IND,                         ELTINPTS
00874                    PSD-BEN-MAX-VISIT-DAYS.                        ELTINPTS
00875                                                                   ELTINPTS
00876      PERFORM 6100-INTLZE-PSP-PYMNT-LVL-SWTS.                      ELTINPTS
00877                                                                   ELTINPTS
00878      PERFORM WITH TEST BEFORE                                     ELTINPTS
00879              VARYING WS-SUB  FROM  +1  BY  +1                     ELTINPTS
00880              UNTIL WS-SUB  >  WS-PROF-IP-CNT                      ELTINPTS
00881                                                                   ELTINPTS
00882          SET PVN-BEN-PROVN-IDX TO WS-SUB                          ELTINPTS
00883          IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO       ELTINPTS
00884             PERFORM 3030-BUILD-SCREEN                             ELTINPTS
00885          END-IF                                                   ELTINPTS
00886                                                                   ELTINPTS
00887      END-PERFORM.                                                 ELTINPTS
00888                                                                   ELTINPTS
00889 ****************************************************************  ELTINPTS
00890 *          BUILD SCREEN                                        *  ELTINPTS
00891 ****************************************************************  ELTINPTS
00892  3030-BUILD-SCREEN.                                               ELTINPTS
00893      SET PLT-INDEX1 TO                                            ELTINPTS
00894         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTINPTS
00895                                                                   ELTINPTS
00896      IF WS-NOT-FIRST-TIME                                         ELTINPTS
00897         SET COF-NEW-PAGE TO TRUE                                  ELTINPTS
00898         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTINPTS
00899         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTINPTS
00900                          COMMAREA(DFHCOMMAREA)  END-EXEC          ELTINPTS
00901      ELSE                                                         ELTINPTS
00902         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTINPTS
00903                                                                   ELTINPTS
00904      MOVE +1    TO  WS-CIA.                                       ELTINPTS
00905      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTINPTS
00906                                                                   ELTINPTS
00907      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)        =  ZEROES      ELTINPTS
00908         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES      ELTINPTS
00909            SET PLT-INDEX2  TO  2                                  ELTINPTS
00910            PERFORM 3040-BUILD-SCREEN-LINES                        ELTINPTS
00911         ELSE                                                      ELTINPTS
00912            PERFORM 8100-DISPLAY-INDICES-PROBLEM                   ELTINPTS
00913      ELSE                                                         ELTINPTS
00914         SET PLT-INDEX2  TO  1                                     ELTINPTS
00915         PERFORM 3040-BUILD-SCREEN-LINES.                          ELTINPTS
00916                                                                   ELTINPTS
00917 ****************************************************************  ELTINPTS
00918 *          BUILD SCREEN LINES                                  *  ELTINPTS
00919 ****************************************************************  ELTINPTS
00920  3040-BUILD-SCREEN-LINES.                                         ELTINPTS
00921      MOVE WS-PROF-IP-CNT TO WS-NBR-BEN-PROV-IDS.                  ELTINPTS
00922      MOVE WS-NO TO WS-DISPLAY-DAY-PSYCH-TEXT,                     ELTINPTS
00923                    WS-DISPLAY-NIGHT-PSYCH-TEXT.                   ELTINPTS
00924      PERFORM 6300-LIST-BEN-PROVS.                                 ELTINPTS
00925                                                                   ELTINPTS
00926      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
00927             NOT  =  ZEROS                                         ELTINPTS
00928         PERFORM 6200-PLACE-OF-TREATMENT.                          ELTINPTS
00929                                                                   ELTINPTS
00930      PERFORM 3300-BENEFIT-SCOPE-ID.                               ELTINPTS
00931      PERFORM 3400-PROVN-PRICING-METHD.                            ELTINPTS
00932      PERFORM 3500-MAX-VISIT.                                      ELTINPTS
00933                                                                   ELTINPTS
00934      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTINPTS
00935      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
00936          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTINPTS
00937                                                                   ELTINPTS
00938      IF WS-DISPLAY-DAY-PSYCH-TEXT = WS-YES                        ELTINPTS
00939         PERFORM 3600-DISPLAY-DAY-PSYCH-TEXT.                      ELTINPTS
00940                                                                   ELTINPTS
00941      IF WS-DISPLAY-NIGHT-PSYCH-TEXT = WS-YES                      ELTINPTS
00942         PERFORM 3700-DISPLAY-NIGHT-PSYCH-TEXT.                    ELTINPTS
00943                                                                   ELTINPTS
00944      PERFORM 8000-SPILLOVER-COINS-N-DED.                          ELTINPTS
00945      PERFORM 8200-CHECK-SAME-DIFF-PROVID.                         ELTINPTS
00946      PERFORM 8400-TRANSF-OTHER-RESP-IND.                          ELTINPTS
00947      PERFORM 8500-SCAN-TAB.                                       ELTINPTS
00948      PERFORM 8600-PAY-CONSID-TEXT.                                ELTINPTS
00949                                                                   ELTINPTS
00950 ****************************************************************  ELTINPTS
00951 *          BENEFIT SCOPE ID                                    *  ELTINPTS
00952 ****************************************************************  ELTINPTS
00953  3300-BENEFIT-SCOPE-ID.                                           ELTINPTS
00954      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
00955                                                                   ELTINPTS
00956      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
00957         SET  PLT-INDEX2      TO  1                                ELTINPTS
00958         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
00959                NOT = '0000' AND NOT = '00  '                      ELTINPTS
00960            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTINPTS
00961                                                                   ELTINPTS
00962      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
00963         SET PLT-INDEX2       TO 2                                 ELTINPTS
00964         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
00965                NOT = '0000' AND NOT = '00  '                      ELTINPTS
00966            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTINPTS
00967                                                                   ELTINPTS
00968      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTINPTS
00969         ADD  +1              TO  WS-CIA                           ELTINPTS
00970         MOVE WS-PAYMNT-BASED TO  COF-DTL-LINE(WS-CIA)             ELTINPTS
00971         ADD  +1              TO  WS-CIA.                          ELTINPTS
00972                                                                   ELTINPTS
00973      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
00974         SET  PLT-INDEX2       TO  1                               ELTINPTS
00975         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
00976                NOT = '0000' AND NOT = '00  '                      ELTINPTS
00977            MOVE 'BPD'           TO  CMF-RECORD-PREFIX             ELTINPTS
00978            MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
00979                                 TO  CMF-CODE-VALUE                ELTINPTS
00980            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTINPTS
00981            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
00982            MOVE WS-BASIC-LIT    TO WS-TEMP-TEXT-AREA              ELTINPTS
00983            MOVE +63             TO WS-TEMP-NOT-USED-CNT           ELTINPTS
00984            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
00985            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
00986                                                                   ELTINPTS
00987      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
00988         SET PLT-INDEX2       TO  2                                ELTINPTS
00989         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
00990                NOT = '0000' AND NOT = '00  '                      ELTINPTS
00991            MOVE 'BPD'           TO  CMF-RECORD-PREFIX             ELTINPTS
00992            MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
00993                                 TO  CMF-CODE-VALUE                ELTINPTS
00994            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTINPTS
00995            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
00996            MOVE WS-SUPPLEMENT-LIT   TO WS-TEMP-TEXT-AREA          ELTINPTS
00997            MOVE +63                 TO WS-TEMP-NOT-USED-CNT       ELTINPTS
00998            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
00999            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
01000                                                                   ELTINPTS
01001 ****************************************************************  ELTINPTS
01002 *          PRICING METHOD                                      *  ELTINPTS
01003 ****************************************************************  ELTINPTS
01004  3400-PROVN-PRICING-METHD.                                        ELTINPTS
01005      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01006      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTINPTS
01007      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01008      SET  PLT-INDEX2          TO  1.                              ELTINPTS
01009      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01010         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01011                                                  =  ZEROS         ELTINPTS
01012            MOVE WS-CONTACT-CONTRACT TO  COF-DTL-LINE(WS-CIA)      ELTINPTS
01013            MOVE 1                   TO  TCAR-OUTPUT-FIELDS-USED   ELTINPTS
01014            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01015         ELSE                                                      ELTINPTS
01016            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01017            PERFORM 3420-PAY-AS-BASIC.                             ELTINPTS
01018                                                                   ELTINPTS
01019      SET  PLT-INDEX2           TO  2.                             ELTINPTS
01020      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01021         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01022                                                  =  ZEROS         ELTINPTS
01023            SET  PLT-INDEX2           TO  2                        ELTINPTS
01024            MOVE WS-CONTACT-CONTRACT  TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01025            MOVE 1                    TO  TCAR-OUTPUT-FIELDS-USED  ELTINPTS
01026            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01027         ELSE                                                      ELTINPTS
01028            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01029            PERFORM 3440-PAY-AS-SUPP.                              ELTINPTS
01030                                                                   ELTINPTS
01031  3420-PAY-AS-BASIC.                                               ELTINPTS
01032      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01033                                         =  ZEROS                  ELTINPTS
01034         IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01035                                         =  ZEROS                  ELTINPTS
01036            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01037                                         =  ZEROS                  ELTINPTS
01038               MOVE TCAR-OPF-DATA(1)  TO  WS-DTL-BASIC             ELTINPTS
01039               MOVE WS-BASIC          TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01040            ELSE                                                   ELTINPTS
01041               PERFORM 3460-PAY-FLAT-RATE                          ELTINPTS
01042               MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC               ELTINPTS
01043               MOVE WS-BASIC         TO COF-DTL-LINE(WS-CIA)       ELTINPTS
01044         ELSE                                                      ELTINPTS
01045            PERFORM 6560-PAY-ADDTNL-PRICE                          ELTINPTS
01046            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER              ELTINPTS
01047            MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)          ELTINPTS
01048      ELSE                                                         ELTINPTS
01049         PERFORM 6550-PAY-VAR-INDM-PCT                             ELTINPTS
01050         MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                 ELTINPTS
01051         MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).            ELTINPTS
01052                                                                   ELTINPTS
01053      PERFORM 9050-OUTPUT-TEXT.                                    ELTINPTS
01054                                                                   ELTINPTS
01055  3440-PAY-AS-SUPP.                                                ELTINPTS
01056      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01057                                        =  ZEROS                   ELTINPTS
01058         IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01059                                        =  ZEROS                   ELTINPTS
01060            IF PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01061                                        =  ZEROS                   ELTINPTS
01062               MOVE TCAR-OPF-DATA(1)    TO  WS-DTL-SUPPLEMENTAL    ELTINPTS
01063               MOVE WS-SUPPLEMENTAL     TO  COF-DTL-LINE(WS-CIA)   ELTINPTS
01064            ELSE                                                   ELTINPTS
01065               PERFORM 3460-PAY-FLAT-RATE                          ELTINPTS
01066               MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPPLEMENTAL        ELTINPTS
01067               MOVE WS-SUPPLEMENTAL  TO COF-DTL-LINE(WS-CIA)       ELTINPTS
01068         ELSE                                                      ELTINPTS
01069            PERFORM 6560-PAY-ADDTNL-PRICE                          ELTINPTS
01070            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER               ELTINPTS
01071            MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)  ELTINPTS
01072      ELSE                                                         ELTINPTS
01073         PERFORM 6550-PAY-VAR-INDM-PCT                             ELTINPTS
01074         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                  ELTINPTS
01075         MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).    ELTINPTS
01076                                                                   ELTINPTS
01077      PERFORM 9050-OUTPUT-TEXT.                                    ELTINPTS
01078                                                                   ELTINPTS
01079  3460-PAY-FLAT-RATE.                                              ELTINPTS
01080      MOVE TCAR-OPF-DATA(1) TO WS-DTL-PER-D.                       ELTINPTS
01081      MOVE PLD-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)           ELTINPTS
01082                   TO WS-DTL-PER-D-AMT.                            ELTINPTS
01083      MOVE SPACES  TO TCAR-FROM-AREA.                              ELTINPTS
01084      STRING WS-DTL-PER-D, ' '                                     ELTINPTS
01085             WS-DTL-PER-D-AMT,                                     ELTINPTS
01086             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTINPTS
01087      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01088      MOVE +02     TO TCAR-OUTPUT-FIELD-COUNT.                     ELTINPTS
01089      MOVE +63     TO TCAR-OUTPUT-FIELD-1-LEN.                     ELTINPTS
01090      MOVE +79     TO TCAR-OUTPUT-FIELD-2-LEN.                     ELTINPTS
01091      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01092                                                                   ELTINPTS
01093  3500-MAX-VISIT.                                                  ELTINPTS
01094      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
01095         SET PLT-INDEX2 TO 1                                       ELTINPTS
01096         IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01097                                                NOT =  ZERO        ELTINPTS
01098            IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01099                                               NOT = LOW-VALUES    ELTINPTS
01100               MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.           ELTINPTS
01101                                                                   ELTINPTS
01102      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT  =   ZERO       ELTINPTS
01103         SET  PLT-INDEX2 TO  2                                     ELTINPTS
01104         IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01105                                                NOT =  ZERO        ELTINPTS
01106            IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01107                                                NOT = LOW-VALUES   ELTINPTS
01108               MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.           ELTINPTS
01109                                                                   ELTINPTS
01110      IF WS-DISPLAY-MAX-VISIT-TEXT = WS-YES                        ELTINPTS
01111          ADD +1             TO WS-CIA                             ELTINPTS
01112          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTINPTS
01113          ADD +1             TO WS-CIA.                            ELTINPTS
01114                                                                   ELTINPTS
01115      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTINPTS
01116         SET  PLT-INDEX2 TO 1                                      ELTINPTS
01117         PERFORM 3510-DISPLAY-MAX-VISIT.                           ELTINPTS
01118                                                                   ELTINPTS
01119      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTINPTS
01120         SET  PLT-INDEX2 TO 2                                      ELTINPTS
01121         PERFORM 3510-DISPLAY-MAX-VISIT.                           ELTINPTS
01122                                                                   ELTINPTS
01123  3510-DISPLAY-MAX-VISIT.                                          ELTINPTS
01124      IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'   ELTINPTS
01125         IF PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01126                                               NOT = LOW-VALUES    ELTINPTS
01127            PERFORM 3520-TRANSLATE-VISIT-TO-ENG                    ELTINPTS
01128            IF PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01129                                                         = ZEROS   ELTINPTS
01130               PERFORM 5900-MOVE-BASIC-R-SUPP-LITERAL              ELTINPTS
01131               PERFORM 9100-OUTPUT-TEXT                            ELTINPTS
01132         ELSE                                                      ELTINPTS
01133            MOVE PLD-BEN-MAX-VISIT-DAYS(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01134                 TO WS-DTL-MAX-DAYS                                ELTINPTS
01135            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND           ELTINPTS
01136            MOVE WS-DAYS               TO WS-DAYS-LITERAL          ELTINPTS
01137            MOVE SPACES   TO TCAR-FROM-AREA                        ELTINPTS
01138            STRING WS-DTL-MAX-DAYS ' '                             ELTINPTS
01139                   WS-DAYS ' '                                     ELTINPTS
01140                   WS-DTL-MAX-IND                                  ELTINPTS
01141                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTINPTS
01142            PERFORM TCPR-000-TEXT-COMPRESSION                      ELTINPTS
01143            MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT               ELTINPTS
01144            MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN               ELTINPTS
01145            MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN               ELTINPTS
01146            PERFORM TCPR-000-TEXT-UNSTRING                         ELTINPTS
01147            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-BASIC             ELTINPTS
01148            MOVE WS-BASIC              TO COF-DTL-LINE(WS-CIA)     ELTINPTS
01149            IF TCAR-OUTPUT-FIELDS-USED > 1                         ELTINPTS
01150               ADD +1                 TO WS-CIA                    ELTINPTS
01151               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTINPTS
01152               PERFORM 9100-OUTPUT-TEXT                            ELTINPTS
01153            ELSE                                                   ELTINPTS
01154               PERFORM 9100-OUTPUT-TEXT.                           ELTINPTS
01155                                                                   ELTINPTS
01156  3520-TRANSLATE-VISIT-TO-ENG.                                     ELTINPTS
01157      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTINPTS
01158      MOVE PLD-BEN-MAX-VISIT-IND(PLT-INDEX1, PLT-INDEX2)           ELTINPTS
01159                                TO  CMF-CODE-VALUE.                ELTINPTS
01160      MOVE 'BPD'                TO  CMF-RECORD-PREFIX.             ELTINPTS
01161      MOVE 'BEN-MAX-VISIT-IND' TO  CMF-ELEMENT-SYSTEM-NAME.        ELTINPTS
01162      PERFORM 9300-CALL-CODES-MANUAL.                              ELTINPTS
01163      STRING CMF-DESCR-LINE (1) ' '                                ELTINPTS
01164             CMF-DESCR-LINE (2)                                    ELTINPTS
01165              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTINPTS
01166      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01167      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTINPTS
01168      MOVE +63              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTINPTS
01169      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTINPTS
01170      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01171                                                                   ELTINPTS
01172  3600-DISPLAY-DAY-PSYCH-TEXT.                                     ELTINPTS
01173      IF GCG-DAY-PSYCH-PRIOR-ADM-CD NOT = '0' AND NOT = LOW-VALUES ELTINPTS
01174         ADD +1                        TO WS-CIA                   ELTINPTS
01175         MOVE 'GROUP'              TO  CMF-RECORD-PREFIX           ELTINPTS
01176         MOVE 'DAY-PSYCH-PRIOR-ADM-CD'                             ELTINPTS
01177                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTINPTS
01178         MOVE GCG-DAY-PSYCH-PRIOR-ADM-CD                           ELTINPTS
01179                                   TO  CMF-CODE-VALUE              ELTINPTS
01180         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
01181         MOVE WS-DAY-PSYCH             TO WS-TEMP-TEXT-AREA        ELTINPTS
01182         MOVE +31                      TO WS-TEMP-NOT-USED-CNT     ELTINPTS
01183         PERFORM 9400-CODES-MANUAL-LONG                            ELTINPTS
01184         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
01185                                                                   ELTINPTS
01186  3700-DISPLAY-NIGHT-PSYCH-TEXT.                                   ELTINPTS
01187      IF GCG-NIGHT-PSYCH-PRIOR-ADM-CD NOT = '0'                    ELTINPTS
01188                  AND NOT = LOW-VALUES                             ELTINPTS
01189         ADD +1                      TO  WS-CIA                    ELTINPTS
01190         MOVE 'GROUP'                TO  CMF-RECORD-PREFIX         ELTINPTS
01191         MOVE 'NIGHT-PSYCH-PRIOR-ADM-CD'                           ELTINPTS
01192                                     TO  CMF-ELEMENT-SYSTEM-NAME   ELTINPTS
01193         MOVE GCG-NIGHT-PSYCH-PRIOR-ADM-CD TO CMF-CODE-VALUE       ELTINPTS
01194         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
01195         MOVE WS-NIGHT-PSYCH      TO WS-TEMP-TEXT-AREA             ELTINPTS
01196         MOVE +29                 TO WS-TEMP-NOT-USED-CNT          ELTINPTS
01197         PERFORM 9400-CODES-MANUAL-LONG                            ELTINPTS
01198         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
01199                                                                   ELTINPTS
01200 ******************************************************************ELTINPTS
01201 *        I N S T I T U T I O N A L   O P                         *ELTINPTS
01202 ******************************************************************ELTINPTS
01203  4000-INSTITUTIONAL-OP.                                           ELTINPTS
01204      MOVE WS-HDR-2-OP-INST    TO  COF-HDR-LINE(2).                ELTINPTS
01205      SET WS-FIRST-TIME TO TRUE.                                   ELTINPTS
01206                                                                   ELTINPTS
01207      PERFORM 9200-HEADER-OUTPUT-REQUEST.                          ELTINPTS
01208                                                                   ELTINPTS
01209      MOVE WS-INST-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTINPTS
01210                                                                   ELTINPTS
01211      PERFORM 4010-LOAD-PVN-BEN-PROVN-TABLE                        ELTINPTS
01212         VARYING WS-SUB FROM +1 BY +1                              ELTINPTS
01213            UNTIL WS-SUB  GREATER THAN  WS-INST-OP-CNT             ELTINPTS
01214                                                                   ELTINPTS
01215      PERFORM 6000-CALL-COVERAGE.                                  ELTINPTS
01216                                                                   ELTINPTS
01217      IF PVN-COVG-NONE                                             ELTINPTS
01218         CONTINUE                                                  ELTINPTS
01219      ELSE                                                         ELTINPTS
01220         PERFORM 4020-PROCESS-COVERED-PROVISION.                   ELTINPTS
01221                                                                   ELTINPTS
01222  4010-LOAD-PVN-BEN-PROVN-TABLE.                                   ELTINPTS
01223      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
01224      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTINPTS
01225                TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).           ELTINPTS
01226      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)            ELTINPTS
01227                   PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).           ELTINPTS
01228                                                                   ELTINPTS
01229  4020-PROCESS-COVERED-PROVISION.                                  ELTINPTS
01230      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTINPTS
01231                                                                   ELTINPTS
01232      MOVE '1' TO PSB-TREAT-TIME-FACTOR-IND,                       ELTINPTS
01233                  PSB-PROF-CHRG-HSP-CLM,                           ELTINPTS
01234                  PSB-MAX-AMT-PER-VISIT,                           ELTINPTS
01235                  PSB-ELIG-METHD-OF-TREAT-IND,                     ELTINPTS
01236                  PSB-CERTN-REPETN-REQRD-IND.                      ELTINPTS
01237                                                                   ELTINPTS
01238      PERFORM 6100-INTLZE-PSP-PYMNT-LVL-SWTS.                      ELTINPTS
01239                                                                   ELTINPTS
01240      PERFORM WITH TEST BEFORE                                     ELTINPTS
01241              VARYING WS-SUB  FROM  +1  BY  +1                     ELTINPTS
01242                 UNTIL WS-SUB  >  WS-INST-OP-CNT                   ELTINPTS
01243                                                                   ELTINPTS
01244         SET PVN-BEN-PROVN-IDX TO WS-SUB                           ELTINPTS
01245         IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO        ELTINPTS
01246            PERFORM 4030-BUILD-SCREEN                              ELTINPTS
01247         END-IF                                                    ELTINPTS
01248                                                                   ELTINPTS
01249      END-PERFORM.                                                 ELTINPTS
01250                                                                   ELTINPTS
01251 ****************************************************************  ELTINPTS
01252 *          BUILD SCREEN                                        *  ELTINPTS
01253 ****************************************************************  ELTINPTS
01254  4030-BUILD-SCREEN.                                               ELTINPTS
01255      SET PLT-INDEX1 TO                                            ELTINPTS
01256         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTINPTS
01257                                                                   ELTINPTS
01258      IF WS-NOT-FIRST-TIME                                         ELTINPTS
01259         SET COF-NEW-PAGE TO TRUE                                  ELTINPTS
01260         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTINPTS
01261         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTINPTS
01262                          COMMAREA(DFHCOMMAREA)  END-EXEC          ELTINPTS
01263      ELSE                                                         ELTINPTS
01264         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTINPTS
01265                                                                   ELTINPTS
01266      MOVE +1    TO  WS-CIA.                                       ELTINPTS
01267      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTINPTS
01268                                                                   ELTINPTS
01269      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)        =  ZEROES      ELTINPTS
01270         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES      ELTINPTS
01271            SET PLT-INDEX2  TO  2                                  ELTINPTS
01272            PERFORM 4040-BUILD-SCREEN-LINES                        ELTINPTS
01273         ELSE                                                      ELTINPTS
01274            PERFORM 8100-DISPLAY-INDICES-PROBLEM                   ELTINPTS
01275      ELSE                                                         ELTINPTS
01276         SET PLT-INDEX2  TO  1                                     ELTINPTS
01277         PERFORM 4040-BUILD-SCREEN-LINES.                          ELTINPTS
01278                                                                   ELTINPTS
01279 ****************************************************************  ELTINPTS
01280 *          BUILD SCREEN LINES                                  *  ELTINPTS
01281 ****************************************************************  ELTINPTS
01282  4040-BUILD-SCREEN-LINES.                                         ELTINPTS
01283      MOVE WS-INST-OP-CNT TO WS-NBR-BEN-PROV-IDS.                  ELTINPTS
01284      PERFORM 6300-LIST-BEN-PROVS.                                 ELTINPTS
01285                                                                   ELTINPTS
01286      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01287             NOT  =  ZEROS                                         ELTINPTS
01288         PERFORM 6200-PLACE-OF-TREATMENT.                          ELTINPTS
01289                                                                   ELTINPTS
01290      SET PVN-BEN-PROVN-IDX TO WS-SUB                              ELTINPTS
01291      PERFORM 6500-PROVN-PRICING-METHD.                            ELTINPTS
01292      PERFORM 2300-MAX-AMT-PER-VISIT.                              ELTINPTS
01293      PERFORM 4400-TREAT-TIME-FACTOR.                              ELTINPTS
01294      PERFORM 4500-ELIG-METHOD-TREAT.                              ELTINPTS
01295      PERFORM 4600-CERT-REPT-REQRD.                                ELTINPTS
01296                                                                   ELTINPTS
01297      PERFORM 8000-SPILLOVER-COINS-N-DED.                          ELTINPTS
01298      PERFORM 8400-TRANSF-OTHER-RESP-IND.                          ELTINPTS
01299      PERFORM 8500-SCAN-TAB.                                       ELTINPTS
01300      PERFORM 8600-PAY-CONSID-TEXT.                                ELTINPTS
01301                                                                   ELTINPTS
01302 ***************************************************************** ELTINPTS
01303 *        T R E A T M E N T   T I M E   F A C T O R   &   I N D  * ELTINPTS
01304 ***************************************************************** ELTINPTS
01305  4400-TREAT-TIME-FACTOR.                                          ELTINPTS
01306      SET PLT-INDEX2  TO  1.                                       ELTINPTS
01307      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZEROES        ELTINPTS
01308         IF PLB-TREAT-TIME-FACTOR-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01309                                              NOT =  ZERO          ELTINPTS
01310            IF PLB-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01311                                              NOT =  ZERO          ELTINPTS
01312               MOVE 'BPB'  TO  CMF-RECORD-PREFIX                   ELTINPTS
01313               MOVE 'TREAT-TIME-FACTOR-IND'                        ELTINPTS
01314                                       TO CMF-ELEMENT-SYSTEM-NAME  ELTINPTS
01315               MOVE PLB-TREAT-TIME-FACTOR-IND                      ELTINPTS
01316                                          (PLT-INDEX1, PLT-INDEX2) ELTINPTS
01317                                       TO CMF-CODE-VALUE           ELTINPTS
01318                MOVE PLB-TREAT-TIME-FACTOR(PLT-INDEX1, PLT-INDEX2) ELTINPTS
01319                                       TO WS-NO-DAYS               ELTINPTS
01320                MOVE WS-FOR-SUPP-ACC-TEXT  TO  WS-TEMP-TEXT-AREA   ELTINPTS
01321                MOVE +41  TO  WS-TEMP-NOT-USED-CNT                 ELTINPTS
01322                PERFORM 9400-CODES-MANUAL-LONG                     ELTINPTS
01323                ADD +1 TO WS-CIA                                   ELTINPTS
01324                PERFORM 9100-OUTPUT-TEXT.                          ELTINPTS
01325                                                                   ELTINPTS
01326 ****************************************************************  ELTINPTS
01327 *     E L I G I B L E   M E T H O D   T R E A T M E N T        *  ELTINPTS
01328 ****************************************************************  ELTINPTS
01329  4500-ELIG-METHOD-TREAT.                                          ELTINPTS
01330      SET PLT-INDEX2  TO  1.                                       ELTINPTS
01331      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT = ZEROES         ELTINPTS
01332         IF PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01333                                              NOT = ZERO           ELTINPTS
01334           ADD +1  TO  WS-CIA                                      ELTINPTS
01335           MOVE WS-ELIG-METH-TREAT-TEXT TO COF-DTL-LINE(WS-CIA)    ELTINPTS
01336           MOVE 'BPB'  TO  CMF-RECORD-PREFIX                       ELTINPTS
01337           MOVE 'ELIG-METHD-OF-TREAT-IND'                          ELTINPTS
01338                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTINPTS
01339           MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)ELTINPTS
01340                                   TO  CMF-CODE-VALUE              ELTINPTS
01341           MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                ELTINPTS
01342           MOVE +63           TO  WS-TEMP-NOT-USED-CNT             ELTINPTS
01343           PERFORM 9400-CODES-MANUAL-LONG                          ELTINPTS
01344           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
01345                                                                   ELTINPTS
01346      SET PLT-INDEX2  TO  2.                                       ELTINPTS
01347      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT = ZEROES         ELTINPTS
01348         IF PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01349                                              NOT = ZERO           ELTINPTS
01350           ADD +1  TO  WS-CIA                                      ELTINPTS
01351           MOVE WS-ELIG-METH-TREAT-TEXT  TO  COF-DTL-LINE(WS-CIA)  ELTINPTS
01352           MOVE 'BPB'  TO  CMF-RECORD-PREFIX                       ELTINPTS
01353           MOVE 'ELIG-METHD-OF-TREAT-IND'                          ELTINPTS
01354                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTINPTS
01355           MOVE PLB-ELIG-METHD-OF-TREAT-IND(PLT-INDEX1, PLT-INDEX2)ELTINPTS
01356                                   TO   CMF-CODE-VALUE             ELTINPTS
01357           MOVE WS-SUPPLEMENT-LIT  TO  WS-TEMP-TEXT-AREA           ELTINPTS
01358           MOVE +63  TO  WS-TEMP-NOT-USED-CNT                      ELTINPTS
01359           PERFORM 9400-CODES-MANUAL-LONG                          ELTINPTS
01360           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
01361                                                                   ELTINPTS
01362 ****************************************************************  ELTINPTS
01363 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTINPTS
01364 ****************************************************************  ELTINPTS
01365  4600-CERT-REPT-REQRD.                                            ELTINPTS
01366      SET  PLT-INDEX2  TO  1.                                      ELTINPTS
01367      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)    NOT =  ZERO        ELTINPTS
01368         IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01369            NOT =  ZERO AND SPACES AND LOW-VALUES                  ELTINPTS
01370          ADD +1  TO  WS-CIA                                       ELTINPTS
01371          MOVE WS-RECERT-TEXT  TO  COF-DTL-LINE (WS-CIA)           ELTINPTS
01372          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTINPTS
01373          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTINPTS
01374                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTINPTS
01375          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTINPTS
01376                               TO  CMF-CODE-VALUE                  ELTINPTS
01377          MOVE WS-BASIC-LIT    TO  WS-TEMP-TEXT-AREA               ELTINPTS
01378          MOVE +63             TO  WS-TEMP-NOT-USED-CNT            ELTINPTS
01379          PERFORM 9400-CODES-MANUAL-LONG                           ELTINPTS
01380          PERFORM 9100-OUTPUT-TEXT.                                ELTINPTS
01381                                                                   ELTINPTS
01382      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
01383      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)    NOT =  ZERO        ELTINPTS
01384         IF PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01385            NOT =  ZERO AND SPACES AND LOW-VALUES                  ELTINPTS
01386          ADD +1  TO  WS-CIA                                       ELTINPTS
01387          MOVE WS-RECERT-TEXT  TO  COF-DTL-LINE (WS-CIA)           ELTINPTS
01388          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTINPTS
01389          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTINPTS
01390                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTINPTS
01391          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTINPTS
01392                               TO  CMF-CODE-VALUE                  ELTINPTS
01393          MOVE WS-SUPPLEMENT-LIT TO  WS-TEMP-TEXT-AREA             ELTINPTS
01394          MOVE +63             TO  WS-TEMP-NOT-USED-CNT            ELTINPTS
01395          PERFORM 9400-CODES-MANUAL-LONG                           ELTINPTS
01396          PERFORM 9100-OUTPUT-TEXT.                                ELTINPTS
01397                                                                   ELTINPTS
01398 ******************************************************************ELTINPTS
01399 *        P R O F E S S I O N A L   O P                           *ELTINPTS
01400 ******************************************************************ELTINPTS
01401  5000-PROFESSIONAL-OP.                                            ELTINPTS
01402      MOVE WS-HDR-2-OP-PROF    TO  COF-HDR-LINE(2).                ELTINPTS
01403      SET WS-FIRST-TIME TO TRUE.                                   ELTINPTS
01404                                                                   ELTINPTS
01405      PERFORM 9200-HEADER-OUTPUT-REQUEST.                          ELTINPTS
01406                                                                   ELTINPTS
01407      MOVE WS-PROF-OP-CNT TO PVN-NBR-BEN-PROVN.                    ELTINPTS
01408                                                                   ELTINPTS
01409      PERFORM 5010-LOAD-PVN-BEN-PROVN-TABLE                        ELTINPTS
01410         VARYING WS-SUB FROM +1 BY +1                              ELTINPTS
01411            UNTIL WS-SUB  GREATER THAN  WS-PROF-OP-CNT.            ELTINPTS
01412                                                                   ELTINPTS
01413      PERFORM 6000-CALL-COVERAGE.                                  ELTINPTS
01414                                                                   ELTINPTS
01415      IF PVN-COVG-NONE                                             ELTINPTS
01416         CONTINUE                                                  ELTINPTS
01417      ELSE                                                         ELTINPTS
01418         PERFORM 5020-PROCESS-COVERED-PROVISION                    ELTINPTS
01419      END-IF.                                                      ELTINPTS
01420                                                                   ELTINPTS
01421  5010-LOAD-PVN-BEN-PROVN-TABLE.                                   ELTINPTS
01422      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
01423      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTINPTS
01424                TO PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).           ELTINPTS
01425      MOVE ZERO TO PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)            ELTINPTS
01426                   PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).           ELTINPTS
01427                                                                   ELTINPTS
01428  5020-PROCESS-COVERED-PROVISION.                                  ELTINPTS
01429      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTINPTS
01430                                                                   ELTINPTS
01431      MOVE '1' TO PSD-BEN-SCOPE-ID,                                ELTINPTS
01432                  PSD-FLAT-RATE-PDM-AMT,                           ELTINPTS
01433                  PSD-BEN-MAX-VISIT-IND,                           ELTINPTS
01434                  PSD-BEN-MAX-VISIT-DAYS,                          ELTINPTS
01435                  PSE-BEN-SCOPE-ID,                                ELTINPTS
01436                  PSE-BEN-MAX-VISITS-IND,                          ELTINPTS
01437                  PSE-BEN-MAX-VISITS-DAYS,                         ELTINPTS
01438                  PSE-MAX-AMT-PER-VISIT,                           ELTINPTS
01439                  PSE-EXCP-SCHED-ID.                               ELTINPTS
01440                                                                   ELTINPTS
01441                                                                   ELTINPTS
01442      PERFORM 6100-INTLZE-PSP-PYMNT-LVL-SWTS.                      ELTINPTS
01443                                                                   ELTINPTS
01444      PERFORM WITH TEST BEFORE                                     ELTINPTS
01445              VARYING WS-SUB  FROM  +1  BY  +1                     ELTINPTS
01446              UNTIL WS-SUB  >  WS-PROF-OP-CNT                      ELTINPTS
01447                                                                   ELTINPTS
01448         SET PVN-BEN-PROVN-IDX TO WS-SUB                           ELTINPTS
01449         IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) NOT = ZERO        ELTINPTS
01450            PERFORM 5030-BUILD-SCREEN                              ELTINPTS
01451         END-IF                                                    ELTINPTS
01452                                                                   ELTINPTS
01453      END-PERFORM.                                                 ELTINPTS
01454                                                                   ELTINPTS
01455 ****************************************************************  ELTINPTS
01456 *          BUILD SCREEN                                        *  ELTINPTS
01457 ****************************************************************  ELTINPTS
01458  5030-BUILD-SCREEN.                                               ELTINPTS
01459      SET PLT-INDEX1 TO                                            ELTINPTS
01460         PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).                      ELTINPTS
01461                                                                   ELTINPTS
01462      IF WS-NOT-FIRST-TIME                                         ELTINPTS
01463         SET COF-NEW-PAGE TO TRUE                                  ELTINPTS
01464         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTINPTS
01465         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTINPTS
01466                          COMMAREA(DFHCOMMAREA)  END-EXEC          ELTINPTS
01467      ELSE                                                         ELTINPTS
01468         SET WS-NOT-FIRST-TIME TO TRUE.                            ELTINPTS
01469                                                                   ELTINPTS
01470      MOVE +1    TO  WS-CIA.                                       ELTINPTS
01471      MOVE WS-NO TO WS-DISPLAY-MAX-VISIT-TEXT.                     ELTINPTS
01472                                                                   ELTINPTS
01473      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)        =  ZEROES      ELTINPTS
01474         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT =  ZEROES      ELTINPTS
01475            SET PLT-INDEX2  TO  2                                  ELTINPTS
01476            PERFORM 5140-BUILD-SCREEN-LINES                        ELTINPTS
01477         ELSE                                                      ELTINPTS
01478            PERFORM 8100-DISPLAY-INDICES-PROBLEM                   ELTINPTS
01479      ELSE                                                         ELTINPTS
01480         SET PLT-INDEX2  TO  1                                     ELTINPTS
01481         PERFORM 5140-BUILD-SCREEN-LINES.                          ELTINPTS
01482                                                                   ELTINPTS
01483 ****************************************************************  ELTINPTS
01484 *          BUILD SCREEN LINES                                  *  ELTINPTS
01485 ****************************************************************  ELTINPTS
01486  5140-BUILD-SCREEN-LINES.                                         ELTINPTS
01487      MOVE WS-PROF-OP-CNT TO WS-NBR-BEN-PROV-IDS.                  ELTINPTS
01488      PERFORM 6300-LIST-BEN-PROVS.                                 ELTINPTS
01489                                                                   ELTINPTS
01490      IF PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01491             NOT  =  ZEROS                                         ELTINPTS
01492         PERFORM 6200-PLACE-OF-TREATMENT.                          ELTINPTS
01493                                                                   ELTINPTS
01494      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
01495      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'D'                      ELTINPTS
01496         PERFORM 3300-BENEFIT-SCOPE-ID                             ELTINPTS
01497         PERFORM 3400-PROVN-PRICING-METHD                          ELTINPTS
01498         PERFORM 3500-MAX-VISIT                                    ELTINPTS
01499      ELSE                                                         ELTINPTS
01500         PERFORM 5300-BENEFIT-SCOPE-ID                             ELTINPTS
01501         PERFORM 5400-PROVN-PRICING-METHD                          ELTINPTS
01502         PERFORM 5500-EXCEPTION-SCHED                              ELTINPTS
01503         PERFORM 5600-MAX-VISIT                                    ELTINPTS
01504         PERFORM 5800-MAX-PER-VISIT.                               ELTINPTS
01505                                                                   ELTINPTS
01506      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTINPTS
01507      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
01508          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTINPTS
01509                                                                   ELTINPTS
01510      PERFORM 8000-SPILLOVER-COINS-N-DED.                          ELTINPTS
01511      PERFORM 8200-CHECK-SAME-DIFF-PROVID.                         ELTINPTS
01512      PERFORM 8400-TRANSF-OTHER-RESP-IND.                          ELTINPTS
01513      PERFORM 8500-SCAN-TAB.                                       ELTINPTS
01514      PERFORM 8600-PAY-CONSID-TEXT.                                ELTINPTS
01515                                                                   ELTINPTS
01516 ****************************************************************  ELTINPTS
01517 *          BENEFIT SCOPE ID                                    *  ELTINPTS
01518 ****************************************************************  ELTINPTS
01519  5300-BENEFIT-SCOPE-ID.                                           ELTINPTS
01520      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTINPTS
01521                                                                   ELTINPTS
01522      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01523         SET  PLT-INDEX2      TO  1                                ELTINPTS
01524         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
01525                NOT = '0000' AND NOT = '00  '                      ELTINPTS
01526            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTINPTS
01527                                                                   ELTINPTS
01528      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01529         SET PLT-INDEX2       TO 2                                 ELTINPTS
01530         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
01531                NOT = '0000' AND NOT = '00  '                      ELTINPTS
01532            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTINPTS
01533                                                                   ELTINPTS
01534      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTINPTS
01535         ADD  +1              TO  WS-CIA                           ELTINPTS
01536         MOVE WS-PAYMNT-BASED TO  COF-DTL-LINE(WS-CIA)             ELTINPTS
01537         ADD  +1              TO  WS-CIA.                          ELTINPTS
01538                                                                   ELTINPTS
01539      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01540         SET  PLT-INDEX2       TO  1                               ELTINPTS
01541         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
01542                NOT = '0000' AND NOT = '00  '                      ELTINPTS
01543            MOVE 'BPE'           TO  CMF-RECORD-PREFIX             ELTINPTS
01544            MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01545                                 TO  CMF-CODE-VALUE                ELTINPTS
01546            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTINPTS
01547            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
01548            MOVE WS-BASIC-LIT    TO WS-TEMP-TEXT-AREA              ELTINPTS
01549            MOVE +63             TO WS-TEMP-NOT-USED-CNT           ELTINPTS
01550            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
01551            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
01552                                                                   ELTINPTS
01553      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
01554         SET PLT-INDEX2       TO  2                                ELTINPTS
01555         IF PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTINPTS
01556                NOT = '0000' AND NOT = '00  '                      ELTINPTS
01557            MOVE 'BPE'           TO  CMF-RECORD-PREFIX             ELTINPTS
01558            MOVE PLE-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01559                                 TO  CMF-CODE-VALUE                ELTINPTS
01560            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTINPTS
01561            PERFORM 9300-CALL-CODES-MANUAL                         ELTINPTS
01562            MOVE WS-SUPPLEMENT-LIT   TO WS-TEMP-TEXT-AREA          ELTINPTS
01563            MOVE +63                 TO WS-TEMP-NOT-USED-CNT       ELTINPTS
01564            PERFORM 9400-CODES-MANUAL-LONG                         ELTINPTS
01565            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
01566                                                                   ELTINPTS
01567      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01568      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTINPTS
01569      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01570                                                                   ELTINPTS
01571 ****************************************************************  ELTINPTS
01572 *          PRICING METHOD                                      *  ELTINPTS
01573 ****************************************************************  ELTINPTS
01574  5400-PROVN-PRICING-METHD.                                        ELTINPTS
01575      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01576         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01577                                                  =  ZEROS         ELTINPTS
01578            SET  PLT-INDEX2           TO  1                        ELTINPTS
01579            MOVE WS-CONTACT-CONTRACT  TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01580            MOVE 1                    TO  TCAR-OUTPUT-FIELDS-USED  ELTINPTS
01581            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01582         ELSE                                                      ELTINPTS
01583            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01584            PERFORM 6520-PAY-AS-BASIC.                             ELTINPTS
01585                                                                   ELTINPTS
01586      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01587         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01588                                                  =  ZEROS         ELTINPTS
01589            SET  PLT-INDEX2           TO  2                        ELTINPTS
01590            MOVE WS-CONTACT-CONTRACT  TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01591            MOVE 1                    TO  TCAR-OUTPUT-FIELDS-USED  ELTINPTS
01592            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01593         ELSE                                                      ELTINPTS
01594            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01595            PERFORM 6530-PAY-AS-SUPP.                              ELTINPTS
01596                                                                   ELTINPTS
01597 ****************************************************************  ELTINPTS
01598 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTINPTS
01599 ****************************************************************  ELTINPTS
01600  5500-EXCEPTION-SCHED.                                            ELTINPTS
01601      SET  PLT-INDEX2  TO  1.                                      ELTINPTS
01602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTINPTS
01603         IF PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTINPTS
01604                                             NOT  =  ZERO          ELTINPTS
01605            ADD  +1                    TO  WS-CIA                  ELTINPTS
01606            MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA)   ELTINPTS
01607            ADD  +1                    TO  WS-CIA                  ELTINPTS
01608            STRING WS-BASIC-LIT                                    ELTINPTS
01609                 PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01610                 DELIMITED BY SIZE                                 ELTINPTS
01611            INTO COF-DTL-LINE (WS-CIA).                            ELTINPTS
01612                                                                   ELTINPTS
01613      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
01614      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTINPTS
01615         IF PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTINPTS
01616                                             NOT  =  ZERO          ELTINPTS
01617            ADD  +1                    TO  WS-CIA                  ELTINPTS
01618            MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA)   ELTINPTS
01619            ADD  +1                    TO  WS-CIA                  ELTINPTS
01620            STRING WS-SUPPLEMENT-LIT                               ELTINPTS
01621                   PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01622                   DELIMITED BY SIZE                               ELTINPTS
01623            INTO COF-DTL-LINE (WS-CIA)                             ELTINPTS
01624            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
01625                                                                   ELTINPTS
01626  5600-MAX-VISIT.                                                  ELTINPTS
01627      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
01628         SET PLT-INDEX2 TO 1                                       ELTINPTS
01629         IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)         ELTINPTS
01630                                                NOT =  ZERO        ELTINPTS
01631            IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01632                                               NOT = LOW-VALUES    ELTINPTS
01633               MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.           ELTINPTS
01634                                                                   ELTINPTS
01635      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT  =   ZERO       ELTINPTS
01636         SET  PLT-INDEX2 TO  2                                     ELTINPTS
01637         IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)         ELTINPTS
01638                                                NOT =  ZERO        ELTINPTS
01639            IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01640                                                NOT = LOW-VALUES   ELTINPTS
01641               MOVE WS-YES TO WS-DISPLAY-MAX-VISIT-TEXT.           ELTINPTS
01642                                                                   ELTINPTS
01643      IF WS-DISPLAY-MAX-VISIT-TEXT = WS-YES                        ELTINPTS
01644          ADD +1             TO WS-CIA                             ELTINPTS
01645          MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)               ELTINPTS
01646          ADD +1             TO WS-CIA.                            ELTINPTS
01647                                                                   ELTINPTS
01648      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTINPTS
01649         SET  PLT-INDEX2 TO 1                                      ELTINPTS
01650         PERFORM 5700-DISPLAY-MAX-VISIT.                           ELTINPTS
01651                                                                   ELTINPTS
01652      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTINPTS
01653         SET  PLT-INDEX2 TO 2                                      ELTINPTS
01654         PERFORM 5700-DISPLAY-MAX-VISIT.                           ELTINPTS
01655                                                                   ELTINPTS
01656  5700-DISPLAY-MAX-VISIT.                                          ELTINPTS
01657      IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2) NOT = '0'  ELTINPTS
01658         IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)         ELTINPTS
01659                                               NOT = LOW-VALUES    ELTINPTS
01660            PERFORM 5720-TRANSLATE-VISIT-TO-ENG                    ELTINPTS
01661            IF PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)     ELTINPTS
01662                                                         = ZEROS   ELTINPTS
01663               PERFORM 5900-MOVE-BASIC-R-SUPP-LITERAL              ELTINPTS
01664               PERFORM 9100-OUTPUT-TEXT                            ELTINPTS
01665         ELSE                                                      ELTINPTS
01666            MOVE PLE-BEN-MAX-VISITS-DAYS(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01667                 TO WS-DTL-MAX-DAYS                                ELTINPTS
01668            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-MAX-IND           ELTINPTS
01669            MOVE WS-DAYS               TO WS-DAYS-LITERAL          ELTINPTS
01670            MOVE SPACES   TO TCAR-FROM-AREA                        ELTINPTS
01671            STRING WS-DTL-MAX-DAYS ' '                             ELTINPTS
01672                   WS-DAYS ' '                                     ELTINPTS
01673                   WS-DTL-MAX-IND                                  ELTINPTS
01674                   DELIMITED BY SIZE INTO TCAR-FROM-AREA           ELTINPTS
01675            PERFORM TCPR-000-TEXT-COMPRESSION                      ELTINPTS
01676            MOVE +03      TO TCAR-OUTPUT-FIELD-COUNT               ELTINPTS
01677            MOVE +63      TO TCAR-OUTPUT-FIELD-1-LEN               ELTINPTS
01678            MOVE +79      TO TCAR-OUTPUT-FIELD-1-LEN               ELTINPTS
01679            PERFORM TCPR-000-TEXT-UNSTRING                         ELTINPTS
01680            MOVE TCAR-OPF-DATA(1)      TO WS-DTL-BASIC             ELTINPTS
01681            MOVE WS-BASIC              TO COF-DTL-LINE(WS-CIA)     ELTINPTS
01682            IF TCAR-OUTPUT-FIELDS-USED > 1                         ELTINPTS
01683               ADD +1                 TO WS-CIA                    ELTINPTS
01684               MOVE TCAR-OPF-DATA(2)  TO COF-DTL-LINE(WS-CIA)      ELTINPTS
01685               PERFORM 9100-OUTPUT-TEXT                            ELTINPTS
01686            ELSE                                                   ELTINPTS
01687               PERFORM 9100-OUTPUT-TEXT.                           ELTINPTS
01688                                                                   ELTINPTS
01689  5720-TRANSLATE-VISIT-TO-ENG.                                     ELTINPTS
01690      MOVE LOW-VALUES TO WS-UNLIMITED.                             ELTINPTS
01691      MOVE PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)          ELTINPTS
01692                                TO  CMF-CODE-VALUE.                ELTINPTS
01693      MOVE 'BPE'                TO  CMF-RECORD-PREFIX.             ELTINPTS
01694      MOVE 'BEN-MAX-VISITS-IND' TO  CMF-ELEMENT-SYSTEM-NAME.       ELTINPTS
01695      PERFORM 9300-CALL-CODES-MANUAL.                              ELTINPTS
01696      STRING CMF-DESCR-LINE (1) ' '                                ELTINPTS
01697             CMF-DESCR-LINE (2)                                    ELTINPTS
01698              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTINPTS
01699      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01700      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTINPTS
01701      MOVE +63              TO  TCAR-OUTPUT-FIELD-1-LEN.           ELTINPTS
01702      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN.           ELTINPTS
01703      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01704                                                                   ELTINPTS
01705 ****************************************************************  ELTINPTS
01706 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTINPTS
01707 ****************************************************************  ELTINPTS
01708  5800-MAX-PER-VISIT.                                              ELTINPTS
01709      SET  PLT-INDEX2  TO  1.                                      ELTINPTS
01710      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTINPTS
01711         IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
01712                                             NOT  =  ZERO          ELTINPTS
01713          ADD +1                     TO  WS-CIA                    ELTINPTS
01714          MOVE WS-MAX-AMT-TEXT       TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
01715          MOVE WS-BASIC-LIT          TO  WS-BASIC-SUPP             ELTINPTS
01716          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01717                                     TO  WS-EDIT-MAX-AMT           ELTINPTS
01718          ADD  +1                    TO  WS-CIA                    ELTINPTS
01719          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
01720          PERFORM 9100-OUTPUT-TEXT.                                ELTINPTS
01721                                                                   ELTINPTS
01722      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
01723      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTINPTS
01724         IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
01725                                              NOT  =  ZERO         ELTINPTS
01726          ADD +1                     TO  WS-CIA                    ELTINPTS
01727          MOVE WS-MAX-AMT-TEXT       TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
01728          MOVE WS-SUPPLEMENT-LIT     TO  WS-BASIC-SUPP             ELTINPTS
01729          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01730                                     TO  WS-EDIT-MAX-AMT           ELTINPTS
01731          ADD  +1                    TO  WS-CIA                    ELTINPTS
01732          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA)     ELTINPTS
01733          PERFORM 9100-OUTPUT-TEXT.                                ELTINPTS
01734                                                                   ELTINPTS
01735  5900-MOVE-BASIC-R-SUPP-LITERAL.                                  ELTINPTS
01736      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01737         MOVE TCAR-OPF-DATA(1)    TO  WS-DTL-UNLIMITED             ELTINPTS
01738         MOVE WS-UNLIMITED        TO  WS-DTL-BASIC                 ELTINPTS
01739         MOVE WS-BASIC            TO  COF-DTL-LINE(WS-CIA).        ELTINPTS
01740                                                                   ELTINPTS
01741      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTINPTS
01742         MOVE TCAR-OPF-DATA(1)    TO  WS-DTL-UNLIMITED             ELTINPTS
01743         MOVE WS-UNLIMITED        TO  WS-DTL-SUPPLEMENTAL          ELTINPTS
01744         MOVE WS-SUPPLEMENTAL     TO  COF-DTL-LINE(WS-CIA).        ELTINPTS
01745                                                                   ELTINPTS
01746 ******************************************************************ELTINPTS
01747 *        C A L L   C O V E R A G E                               *ELTINPTS
01748 ******************************************************************ELTINPTS
01749  6000-CALL-COVERAGE.                                              ELTINPTS
01750      MOVE WS-VISITS TO SSB-TOPIC-PHRASE.                          ELTINPTS
01751                                                                   ELTINPTS
01752      EXEC CICS  LINK  PROGRAM('ELGCOVER')                         ELTINPTS
01753                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
01754                                                                   ELTINPTS
01755      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTINPTS
01756                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
01757                                                                   ELTINPTS
01758 ******************************************************************ELTINPTS
01759 *        I N T I A L I Z E   P A Y   L V L   S W I T C H E S     *ELTINPTS
01760 ******************************************************************ELTINPTS
01761  6100-INTLZE-PSP-PYMNT-LVL-SWTS.                                  ELTINPTS
01762      MOVE '1'   TO  PSP-PLACE-TREAT-ELIG-IND,                     ELTINPTS
01763                 PSP-PROVN-PRICING-METHD,                          ELTINPTS
01764                 PSP-TRANSF-OTHER-RESP-IND,                        ELTINPTS
01765                 PSP-ADDITIONAL-PRICING-PRCNT,                     ELTINPTS
01766                 PSP-VARIABLE-INDEMNITY-PRCNT,                     ELTINPTS
01767                 PSP-SPILL-OVER-COINS-APL-IND,                     ELTINPTS
01768                 PSP-SPILL-OVER-DED-APL-IND,                       ELTINPTS
01769                 PSP-SPILL-OVR-RM-F-RT-APL-IND,                    ELTINPTS
01770                 PSP-BEN-TAB-PROVN-ID-AAR,                         ELTINPTS
01771                 PSP-BEN-TAB-PROVN-ID-ABM,                         ELTINPTS
01772                 PSP-BEN-TAB-PROVN-ID-ACL,                         ELTINPTS
01773                 PSP-BEN-TAB-PROVN-ID-ADL,                         ELTINPTS
01774                 PSP-BEN-TAB-PROVN-ID-AOL,                         ELTINPTS
01775                 PSP-BEN-TAB-PROVN-ID-PPF.                         ELTINPTS
01776      EXEC CICS  LINK  PROGRAM('ELUPLGRP')                         ELTINPTS
01777                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
01778      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTINPTS
01779      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
01780            ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                    ELTINPTS
01781                                                                   ELTINPTS
01782 ****************************************************************  ELTINPTS
01783 *          PLACE OF TREATMENT                                  *  ELTINPTS
01784 ****************************************************************  ELTINPTS
01785  6200-PLACE-OF-TREATMENT.                                         ELTINPTS
01786       ADD +1       TO  WS-CIA.                                    ELTINPTS
01787       MOVE 'BP'                   TO  CMF-RECORD-PREFIX.          ELTINPTS
01788       MOVE 'PLACE-TREAT-ELIG-IND' TO CMF-ELEMENT-SYSTEM-NAME.     ELTINPTS
01789       MOVE PLP-PLACE-TREAT-ELIG-IND(PLT-INDEX1, PLT-INDEX2)       ELTINPTS
01790                                   TO  CMF-CODE-VALUE.             ELTINPTS
01791       PERFORM 9300-CALL-CODES-MANUAL.                             ELTINPTS
01792       STRING WS-SERVICES-RENDERED ' '                             ELTINPTS
01793              CMF-DESCR-LINE (1) ' '                               ELTINPTS
01794              CMF-DESCR-LINE (2)                                   ELTINPTS
01795              DELIMITED BY SIZE INTO TCAR-FROM-AREA.               ELTINPTS
01796       PERFORM TCPR-000-TEXT-COMPRESSION.                          ELTINPTS
01797       MOVE +03     TO  TCAR-OUTPUT-FIELD-COUNT.                   ELTINPTS
01798       MOVE +79     TO  TCAR-OUTPUT-FIELD-1-LEN.                   ELTINPTS
01799       MOVE +79     TO  TCAR-OUTPUT-FIELD-2-LEN.                   ELTINPTS
01800       PERFORM TCPR-000-TEXT-UNSTRING.                             ELTINPTS
01801       MOVE TCAR-OPF-DATA(1)    TO  COF-DTL-LINE(WS-CIA).          ELTINPTS
01802       IF TCAR-OUTPUT-FIELDS-USED > 1                              ELTINPTS
01803          ADD +1                    TO  WS-CIA                     ELTINPTS
01804          MOVE TCAR-OPF-DATA(2)     TO  COF-DTL-LINE(WS-CIA)       ELTINPTS
01805          PERFORM 9100-OUTPUT-TEXT.                                ELTINPTS
01806                                                                   ELTINPTS
01807  6300-LIST-BEN-PROVS.                                             ELTINPTS
01808      MOVE PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)  TO  WS-SUB3.       ELTINPTS
01809                                                                   ELTINPTS
01810      MOVE WS-FOLLOW-BENEFIT TO COF-DTL-LINE(WS-CIA).              ELTINPTS
01811      ADD +1    TO WS-CIA.                                         ELTINPTS
01812                                                                   ELTINPTS
01813      MOVE WS-NO TO WS-DISPLAY-PAYMNT-BASED-TEXT.                  ELTINPTS
01814                                                                   ELTINPTS
01815      PERFORM 6400-ZERO-ALL-WITH-SAME-NO                           ELTINPTS
01816         VARYING WS-SUB2  FROM  WS-SUB  BY  +1                     ELTINPTS
01817            UNTIL WS-SUB2  >  WS-NBR-BEN-PROV-IDS.                 ELTINPTS
01818                                                                   ELTINPTS
01819         IF WS-CIA > +1                                            ELTINPTS
01820            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
01821                                                                   ELTINPTS
01822  6400-ZERO-ALL-WITH-SAME-NO.                                      ELTINPTS
01823      SET PVN-BEN-PROVN-IDX TO WS-SUB2.                            ELTINPTS
01824                                                                   ELTINPTS
01825      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTINPTS
01826         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'DPV  '                ELTINPTS
01827             MOVE WS-YES TO WS-DISPLAY-DAY-PSYCH-TEXT.             ELTINPTS
01828                                                                   ELTINPTS
01829      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTINPTS
01830         IF PVN-BEN-ID(PVN-BEN-PROVN-IDX) = 'NPV  '                ELTINPTS
01831             MOVE WS-YES TO WS-DISPLAY-NIGHT-PSYCH-TEXT.           ELTINPTS
01832                                                                   ELTINPTS
01833      IF PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)         =  WS-SUB3    ELTINPTS
01834         MOVE 'BP'             TO  CMF-RECORD-PREFIX               ELTINPTS
01835         MOVE 'BEN-PR-ID'      TO  CMF-ELEMENT-SYSTEM-NAME         ELTINPTS
01836         MOVE PVN-BEN-ID(PVN-BEN-PROVN-IDX)  TO  CMF-CODE-VALUE    ELTINPTS
01837         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
01838         STRING CMF-DESCR-LINE (1) ' '                             ELTINPTS
01839                CMF-DESCR-LINE (2)                                 ELTINPTS
01840                DELIMITED BY SIZE INTO TCAR-FROM-AREA              ELTINPTS
01841         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
01842         MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
01843         MOVE +55              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
01844         MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
01845         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
01846         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SERVICES-2ND             ELTINPTS
01847         MOVE WS-SERVICES-2ND  TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
01848         IF WS-CIA <  12                                           ELTINPTS
01849            ADD +1    TO  WS-CIA                                   ELTINPTS
01850            MOVE ZERO TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX)      ELTINPTS
01851         ELSE                                                      ELTINPTS
01852            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01853            MOVE +2   TO WS-CIA                                    ELTINPTS
01854            MOVE WS-FOLLOW-BEN-CON TO COF-DTL-LINE(WS-CIA)         ELTINPTS
01855            ADD +1    TO WS-CIA                                    ELTINPTS
01856            MOVE ZERO TO  PVN-COVG-SAME-AS(PVN-BEN-PROVN-IDX).     ELTINPTS
01857                                                                   ELTINPTS
01858 ****************************************************************  ELTINPTS
01859 *          PRICING METHOD                                      *  ELTINPTS
01860 ****************************************************************  ELTINPTS
01861  6500-PROVN-PRICING-METHD.                                        ELTINPTS
01862      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01863      MOVE WS-SERVICES-PAYABLE  TO  COF-DTL-LINE(WS-CIA).          ELTINPTS
01864      ADD  +1                   TO  WS-CIA.                        ELTINPTS
01865      SET  PLT-INDEX2           TO  1.                             ELTINPTS
01866      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01867         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01868                 = ZEROS OR SPACES OR LOW-VALUES                   ELTINPTS
01869            MOVE WS-CONTACT-CONTRACT  TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01870            MOVE 1                    TO  TCAR-OUTPUT-FIELDS-USED  ELTINPTS
01871            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01872         ELSE                                                      ELTINPTS
01873            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01874            PERFORM 6520-PAY-AS-BASIC.                             ELTINPTS
01875                                                                   ELTINPTS
01876      SET  PLT-INDEX2           TO  2.                             ELTINPTS
01877      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTINPTS
01878         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)        ELTINPTS
01879                 = ZEROS OR SPACES OR LOW-VALUES                   ELTINPTS
01880            MOVE WS-CONTACT-CONTRACT  TO  COF-DTL-LINE(WS-CIA)     ELTINPTS
01881            MOVE 1                    TO  TCAR-OUTPUT-FIELDS-USED  ELTINPTS
01882            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
01883         ELSE                                                      ELTINPTS
01884            PERFORM 6540-BASIC-N-SUPP-COMMON                       ELTINPTS
01885            PERFORM 6530-PAY-AS-SUPP.                              ELTINPTS
01886                                                                   ELTINPTS
01887  6520-PAY-AS-BASIC.                                               ELTINPTS
01888      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01889                                         =  ZEROS                  ELTINPTS
01890         IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01891                                         =  ZEROS                  ELTINPTS
01892            MOVE TCAR-OPF-DATA(1)  TO  WS-DTL-BASIC                ELTINPTS
01893            MOVE WS-BASIC          TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
01894         ELSE                                                      ELTINPTS
01895            PERFORM 6560-PAY-ADDTNL-PRICE                          ELTINPTS
01896            MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER              ELTINPTS
01897            MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA)          ELTINPTS
01898      ELSE                                                         ELTINPTS
01899         PERFORM 6550-PAY-VAR-INDM-PCT                             ELTINPTS
01900         MOVE TCAR-OPF-DATA(1) TO WS-DTL-BASIC-PER                 ELTINPTS
01901         MOVE WS-BASIC-PERCENT TO COF-DTL-LINE(WS-CIA).            ELTINPTS
01902                                                                   ELTINPTS
01903      PERFORM 9050-OUTPUT-TEXT.                                    ELTINPTS
01904                                                                   ELTINPTS
01905  6530-PAY-AS-SUPP.                                                ELTINPTS
01906      IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
01907                                        =  ZEROS                   ELTINPTS
01908         IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01909                                        =  ZEROS                   ELTINPTS
01910            MOVE TCAR-OPF-DATA(1)    TO  WS-DTL-SUPPLEMENTAL       ELTINPTS
01911            MOVE WS-SUPPLEMENTAL     TO  COF-DTL-LINE(WS-CIA)      ELTINPTS
01912         ELSE                                                      ELTINPTS
01913            PERFORM 6560-PAY-ADDTNL-PRICE                          ELTINPTS
01914            MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER               ELTINPTS
01915            MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA)  ELTINPTS
01916      ELSE                                                         ELTINPTS
01917         PERFORM 6550-PAY-VAR-INDM-PCT                             ELTINPTS
01918         MOVE TCAR-OPF-DATA(1) TO WS-DTL-SUPP-PER                  ELTINPTS
01919         MOVE WS-SUPPLEMENTAL-PERCENT TO  COF-DTL-LINE(WS-CIA).    ELTINPTS
01920                                                                   ELTINPTS
01921      PERFORM 9050-OUTPUT-TEXT.                                    ELTINPTS
01922                                                                   ELTINPTS
01923  6540-BASIC-N-SUPP-COMMON.                                        ELTINPTS
01924      MOVE 'BP'                  TO  CMF-RECORD-PREFIX.            ELTINPTS
01925      MOVE 'PROVN-PRICING-METHD' TO  CMF-ELEMENT-SYSTEM-NAME.      ELTINPTS
01926      MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO     ELTINPTS
01927           CMF-CODE-VALUE.                                         ELTINPTS
01928      PERFORM 9300-CALL-CODES-MANUAL.                              ELTINPTS
01929      STRING CMF-DESCR-LINE(1) ' '                                 ELTINPTS
01930             CMF-DESCR-LINE(2)                                     ELTINPTS
01931                DELIMITED BY SIZE INTO TCAR-FROM-AREA.             ELTINPTS
01932      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01933      MOVE +02             TO TCAR-OUTPUT-FIELD-COUNT.             ELTINPTS
01934      MOVE +63             TO TCAR-OUTPUT-FIELD-1-LEN.             ELTINPTS
01935      MOVE +79             TO TCAR-OUTPUT-FIELD-2-LEN.             ELTINPTS
01936      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01937                                                                   ELTINPTS
01938  6550-PAY-VAR-INDM-PCT.                                           ELTINPTS
01939      MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP.                          ELTINPTS
01940      MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01941                   TO WS-DTL-PERCENT.                              ELTINPTS
01942      MOVE SPACES  TO TCAR-FROM-AREA.                              ELTINPTS
01943      STRING WS-DTL-PP,                                            ELTINPTS
01944             WS-DTL-PERCENT,                                       ELTINPTS
01945             WS-PERCENT,                                           ELTINPTS
01946             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTINPTS
01947      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01948      MOVE +03     TO TCAR-OUTPUT-FIELD-COUNT.                     ELTINPTS
01949      MOVE +63     TO TCAR-OUTPUT-FIELD-1-LEN.                     ELTINPTS
01950      MOVE +79     TO TCAR-OUTPUT-FIELD-2-LEN.                     ELTINPTS
01951      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01952                                                                   ELTINPTS
01953  6560-PAY-ADDTNL-PRICE.                                           ELTINPTS
01954      MOVE TCAR-OPF-DATA(1) TO WS-DTL-PP.                          ELTINPTS
01955      MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01956                   TO  WS-DTL-PERCENT.                             ELTINPTS
01957      MOVE SPACES  TO  TCAR-FROM-AREA.                             ELTINPTS
01958      STRING WS-DTL-PP,                                            ELTINPTS
01959             WS-DTL-PERCENT,                                       ELTINPTS
01960             WS-PERCENT,                                           ELTINPTS
01961             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTINPTS
01962      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
01963      MOVE +03     TO TCAR-OUTPUT-FIELD-COUNT.                     ELTINPTS
01964      MOVE +63     TO TCAR-OUTPUT-FIELD-1-LEN.                     ELTINPTS
01965      MOVE +79     TO TCAR-OUTPUT-FIELD-2-LEN.                     ELTINPTS
01966      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
01967                                                                   ELTINPTS
01968  8000-SPILLOVER-COINS-N-DED.                                      ELTINPTS
01969      MOVE +1     TO  WS-CIA.                                      ELTINPTS
01970      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
01971      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT   = ZERO          ELTINPTS
01972         IF PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTINPTS
01973            = ZERO OR LOW-VALUES OR SPACES                         ELTINPTS
01974              CONTINUE                                             ELTINPTS
01975         ELSE PERFORM 8010-SPILLOVER-COINS.                        ELTINPTS
01976                                                                   ELTINPTS
01977      MOVE +1     TO  WS-CIA.                                      ELTINPTS
01978      SET  PLT-INDEX2  TO  2.                                      ELTINPTS
01979      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT   = ZERO          ELTINPTS
01980         IF PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)     ELTINPTS
01981            = ZERO OR LOW-VALUES OR SPACES                         ELTINPTS
01982              CONTINUE                                             ELTINPTS
01983         ELSE PERFORM 8020-SPILLOVER-DEDUCT.                       ELTINPTS
01984                                                                   ELTINPTS
01985  8010-SPILLOVER-COINS.                                            ELTINPTS
01986      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTINPTS
01987      MOVE 'SPILL-OVER-COINS-APL-IND'                              ELTINPTS
01988                               TO  CMF-ELEMENT-SYSTEM-NAME.        ELTINPTS
01989      MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2)    ELTINPTS
01990                               TO  CMF-CODE-VALUE.                 ELTINPTS
01991      MOVE WS-SPILLOVER-COINS  TO  WS-TEMP-TEXT-AREA .             ELTINPTS
01992      MOVE +56                 TO  WS-TEMP-NOT-USED-CNT.           ELTINPTS
01993      PERFORM 9300-CALL-CODES-MANUAL.                              ELTINPTS
01994      PERFORM 9400-CODES-MANUAL-LONG.                              ELTINPTS
01995      PERFORM 9100-OUTPUT-TEXT.                                    ELTINPTS
01996                                                                   ELTINPTS
01997  8020-SPILLOVER-DEDUCT.                                           ELTINPTS
01998      MOVE 'BP'                TO  CMF-RECORD-PREFIX.              ELTINPTS
01999      MOVE 'SPILL-OVER-DED-APL-IND'                                ELTINPTS
02000                               TO  CMF-ELEMENT-SYSTEM-NAME.        ELTINPTS
02001      MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02002                               TO  CMF-CODE-VALUE.                 ELTINPTS
02003      MOVE WS-SPILLOVER-DEDBL  TO  WS-TEMP-TEXT-AREA.              ELTINPTS
02004      MOVE +57                 TO  WS-TEMP-NOT-USED-CNT.           ELTINPTS
02005      PERFORM 9300-CALL-CODES-MANUAL.                              ELTINPTS
02006      PERFORM 9400-CODES-MANUAL-LONG.                              ELTINPTS
02007      PERFORM 9100-OUTPUT-TEXT.                                    ELTINPTS
02008                                                                   ELTINPTS
02009  8100-DISPLAY-INDICES-PROBLEM.                                    ELTINPTS
02010      MOVE 'PROBLEM W/ INDICES'  TO  COF-DTL-LINE(1).              ELTINPTS
02011      MOVE +1  TO  COF-NBR-DTL-LINES.                              ELTINPTS
02012      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTINPTS
02013      MOVE 'P'  TO  COF-FUNCTION.                                  ELTINPTS
02014      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTINPTS
02015                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
02016      MOVE WS-NBR-BEN-PROV-IDS TO WS-SUB.                          ELTINPTS
02017                                                                   ELTINPTS
02018  8200-CHECK-SAME-DIFF-PROVID.                                     ELTINPTS
02019      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTINPTS
02020      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02021          ADDRESS OF CONTRACT-RECORD.                              ELTINPTS
02022                                                                   ELTINPTS
02023      IF CIA-RC-PTR-NULL                                           ELTINPTS
02024         SET CIA-ELSCONPS-DDN TO TRUE                              ELTINPTS
02025         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTINPTS
02026             ADDRESS OF CONTRACT-RECORD                            ELTINPTS
02027                                                                   ELTINPTS
02028      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTINPTS
02029      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02030          ADDRESS OF CONTRACT-RECORD.                              ELTINPTS
02031                                                                   ELTINPTS
02032      IF NOT CIA-RC-PTR-NULL                                       ELTINPTS
02033         PERFORM 8210-MED-SURG-OB-BASIC-SAME                       ELTINPTS
02034         PERFORM 8220-MED-GT-FC-PSYH-BASIC-SAME                    ELTINPTS
02035         PERFORM 8230-MED-RAD-THPY-BASIC-SAME                      ELTINPTS
02036         PERFORM 8240-MED-CHEMO-THPY-BASIC-SAME.                   ELTINPTS
02037                                                                   ELTINPTS
02038      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTINPTS
02039      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02040          ADDRESS OF CONTRACT-RECORD.                              ELTINPTS
02041                                                                   ELTINPTS
02042      IF NOT CIA-RC-PTR-NULL                                       ELTINPTS
02043         PERFORM 8250-MED-SURG-OB-SUPP-SAME                        ELTINPTS
02044         PERFORM 8260-MED-GT-FC-PSYH-SUPP-SAME                     ELTINPTS
02045         PERFORM 8270-MED-RAD-THPY-SUPP-SAME                       ELTINPTS
02046         PERFORM 8280-MED-CHEMO-THPY-SUPP-SAME.                    ELTINPTS
02047                                                                   ELTINPTS
02048      PERFORM 8290-OUTPUT-SAME-PROVD-TEXT.                         ELTINPTS
02049                                                                   ELTINPTS
02050      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTINPTS
02051      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02052          ADDRESS OF CONTRACT-RECORD.                              ELTINPTS
02053      IF NOT CIA-RC-PTR-NULL                                       ELTINPTS
02054         PERFORM 8300-MED-MED-BASIC-DIFF                           ELTINPTS
02055         PERFORM 8310-MED-SURG-OB-BASIC-DIFF.                      ELTINPTS
02056                                                                   ELTINPTS
02057      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTINPTS
02058      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02059          ADDRESS OF CONTRACT-RECORD.                              ELTINPTS
02060      IF NOT CIA-RC-PTR-NULL                                       ELTINPTS
02061          PERFORM 8320-MED-MED-SUPP-DIFF                           ELTINPTS
02062          PERFORM 8330-MED-SURG-OB-SUPP-DIFF.                      ELTINPTS
02063                                                                   ELTINPTS
02064      PERFORM 8340-OUTPUT-DIFF-PROVD-TEXT.                         ELTINPTS
02065                                                                   ELTINPTS
02066  8210-MED-SURG-OB-BASIC-SAME.                                     ELTINPTS
02067      IF GCT-SAME-PROV-SOM-BIL-ELIG-IND NOT = ZERO                 ELTINPTS
02068         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02069         MOVE 'SAME-PROV-SOM-BIL-ELIG-IND'                         ELTINPTS
02070                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02071         MOVE GCT-SAME-PROV-SOM-BIL-ELIG-IND                       ELTINPTS
02072                                     TO  CMF-CODE-VALUE            ELTINPTS
02073         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02074         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-1-1        ELTINPTS
02075         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-1-2.       ELTINPTS
02076                                                                   ELTINPTS
02077  8220-MED-GT-FC-PSYH-BASIC-SAME.                                  ELTINPTS
02078      IF GCT-SAME-PROV-IP-G-T-F-C-P-M NOT = ZERO                   ELTINPTS
02079         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02080         MOVE 'SAME-PROV-IP-G-T-F-C-P-M'                           ELTINPTS
02081                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02082         MOVE GCT-SAME-PROV-IP-G-T-F-C-P-M                         ELTINPTS
02083                                     TO  CMF-CODE-VALUE            ELTINPTS
02084         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02085         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-2-1        ELTINPTS
02086         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-2-2.       ELTINPTS
02087                                                                   ELTINPTS
02088  8230-MED-RAD-THPY-BASIC-SAME.                                    ELTINPTS
02089      IF GCT-SAME-PROV-IP-RAD-THRPY-MED NOT = ZERO                 ELTINPTS
02090         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02091         MOVE 'SAME-PROV-IP-RAD-THRPY-MED'                         ELTINPTS
02092                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02093         MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED                       ELTINPTS
02094                                     TO  CMF-CODE-VALUE            ELTINPTS
02095         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02096         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-3-1        ELTINPTS
02097         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-3-2.       ELTINPTS
02098                                                                   ELTINPTS
02099  8240-MED-CHEMO-THPY-BASIC-SAME.                                  ELTINPTS
02100      IF GCT-SAME-PROV-IP-MED-CHMOTHRPY NOT = ZERO                 ELTINPTS
02101         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02102         MOVE 'SAME-PROV-IP-MED-CHMOTHRPY'                         ELTINPTS
02103                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02104         MOVE GCT-SAME-PROV-IP-MED-CHMOTHRPY                       ELTINPTS
02105                                     TO  CMF-CODE-VALUE            ELTINPTS
02106         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02107         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-4-1        ELTINPTS
02108         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-4-2.       ELTINPTS
02109                                                                   ELTINPTS
02110  8250-MED-SURG-OB-SUPP-SAME.                                      ELTINPTS
02111      IF GCT-SAME-PROV-SOM-BIL-ELIG-IND NOT = ZERO                 ELTINPTS
02112         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02113         MOVE 'SAME-PROV-SOM-BIL-ELIG-IND'                         ELTINPTS
02114                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02115         MOVE GCT-SAME-PROV-SOM-BIL-ELIG-IND                       ELTINPTS
02116                                     TO  CMF-CODE-VALUE            ELTINPTS
02117         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02118         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-1-1         ELTINPTS
02119         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-1-2.        ELTINPTS
02120                                                                   ELTINPTS
02121  8260-MED-GT-FC-PSYH-SUPP-SAME.                                   ELTINPTS
02122      IF GCT-SAME-PROV-IP-G-T-F-C-P-M NOT = ZERO                   ELTINPTS
02123         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02124         MOVE 'SAME-PROV-IP-G-T-F-C-P-M'                           ELTINPTS
02125                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02126         MOVE GCT-SAME-PROV-IP-G-T-F-C-P-M                         ELTINPTS
02127                                     TO  CMF-CODE-VALUE            ELTINPTS
02128         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02129         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-2-1         ELTINPTS
02130         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-2-2.        ELTINPTS
02131                                                                   ELTINPTS
02132  8270-MED-RAD-THPY-SUPP-SAME.                                     ELTINPTS
02133      IF GCT-SAME-PROV-IP-RAD-THRPY-MED NOT = ZERO                 ELTINPTS
02134         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02135         MOVE 'SAME-PROV-IP-RAD-THRPY-MED'                         ELTINPTS
02136                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02137         MOVE GCT-SAME-PROV-IP-RAD-THRPY-MED                       ELTINPTS
02138                                     TO  CMF-CODE-VALUE            ELTINPTS
02139         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02140         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-3-1         ELTINPTS
02141         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-3-2.        ELTINPTS
02142                                                                   ELTINPTS
02143  8280-MED-CHEMO-THPY-SUPP-SAME.                                   ELTINPTS
02144      IF GCT-SAME-PROV-IP-MED-CHMOTHRPY NOT = ZERO                 ELTINPTS
02145         MOVE 'CONTRACT'             TO  CMF-RECORD-PREFIX         ELTINPTS
02146         MOVE 'SAME-PROV-IP-MED-CHMOTHRPY'                         ELTINPTS
02147                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02148         MOVE GCT-SAME-PROV-IP-MED-CHMOTHRPY                       ELTINPTS
02149                                     TO  CMF-CODE-VALUE            ELTINPTS
02150         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02151         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-4-1         ELTINPTS
02152         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-4-2.        ELTINPTS
02153                                                                   ELTINPTS
02154  8290-OUTPUT-SAME-PROVD-TEXT.                                     ELTINPTS
02155      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTINPTS
02156          WS-DTL-BASIC-2-1 NOT = SPACES OR                         ELTINPTS
02157           WS-DTL-BASIC-3-1 NOT = SPACES OR                        ELTINPTS
02158            WS-DTL-BASIC-4-1 NOT = SPACES OR                       ELTINPTS
02159             WS-DTL-SUPP-1-1  NOT = SPACES OR                      ELTINPTS
02160              WS-DTL-SUPP-2-1 NOT = SPACES OR                      ELTINPTS
02161               WS-DTL-SUPP-3-1 NOT = SPACES OR                     ELTINPTS
02162                WS-DTL-SUPP-4-1 NOT = SPACES                       ELTINPTS
02163                  ADD +1                TO WS-CIA                  ELTINPTS
02164                  MOVE WS-SAME-PROVIDER TO COF-DTL-LINE(WS-CIA)    ELTINPTS
02165                  ADD +1                TO WS-CIA.                 ELTINPTS
02166                                                                   ELTINPTS
02167      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTINPTS
02168           OR WS-DTL-SUPP-1-1 NOT = SPACES                         ELTINPTS
02169               MOVE WS-MEDI-SURG-OB TO COF-DTL-LINE(WS-CIA)        ELTINPTS
02170               ADD +1               TO WS-CIA.                     ELTINPTS
02171                                                                   ELTINPTS
02172      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTINPTS
02173         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02174         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02175                WS-BASIC-1-1 ' '                                   ELTINPTS
02176                WS-BASIC-1-2                                       ELTINPTS
02177                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02178         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02179         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02180         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02181         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02182         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02183         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-1-1,               ELTINPTS
02184         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02185         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02186             ADD +1                TO  WS-CIA                      ELTINPTS
02187             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-1-2            ELTINPTS
02188             MOVE WS-BASIC-1-2     TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02189             MOVE SPACES           TO  WS-DTL-BASIC-1-1,           ELTINPTS
02190                                       WS-DTL-BASIC-1-2            ELTINPTS
02191             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02192         ELSE                                                      ELTINPTS
02193           MOVE SPACES           TO  WS-DTL-BASIC-1-1,             ELTINPTS
02194                                     WS-DTL-BASIC-1-2              ELTINPTS
02195           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
02196                                                                   ELTINPTS
02197      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTINPTS
02198         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02199         STRING WS-DTL-SUPP-1-1 ' '                                ELTINPTS
02200                WS-SUPP-1-2                                        ELTINPTS
02201                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02202         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02203         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02204         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02205         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02206         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02207         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-1-1                 ELTINPTS
02208         MOVE WS-SUPP-1-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02209         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02210             ADD +1                TO  WS-CIA                      ELTINPTS
02211             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-1-2             ELTINPTS
02212             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02213             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTINPTS
02214                                       WS-DTL-SUPP-1-2             ELTINPTS
02215             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02216         ELSE                                                      ELTINPTS
02217             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTINPTS
02218                                       WS-DTL-SUPP-1-2             ELTINPTS
02219             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02220                                                                   ELTINPTS
02221      IF WS-DTL-BASIC-2-1 NOT = SPACES                             ELTINPTS
02222           OR WS-DTL-SUPP-2-1 NOT = SPACES                         ELTINPTS
02223               ADD +1             TO WS-CIA                        ELTINPTS
02224               MOVE WS-MEDI-INP-GT-FC-PSYCH                        ELTINPTS
02225                                  TO COF-DTL-LINE(WS-CIA)          ELTINPTS
02226               ADD +1             TO WS-CIA.                       ELTINPTS
02227                                                                   ELTINPTS
02228      IF WS-DTL-BASIC-2-1 NOT = SPACES                             ELTINPTS
02229         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02230         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02231                WS-BASIC-2-1 ' '                                   ELTINPTS
02232                WS-BASIC-2-2                                       ELTINPTS
02233                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02234         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02235         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02236         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02237         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02238         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02239         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-2-1                ELTINPTS
02240         MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02241         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02242             ADD +1                TO  WS-CIA                      ELTINPTS
02243             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-2-2            ELTINPTS
02244             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02245             MOVE SPACES           TO  WS-DTL-BASIC-2-1,           ELTINPTS
02246                                       WS-DTL-BASIC-2-2            ELTINPTS
02247             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02248         ELSE                                                      ELTINPTS
02249           MOVE SPACES           TO  WS-DTL-BASIC-2-1,             ELTINPTS
02250                                     WS-DTL-BASIC-2-2              ELTINPTS
02251           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
02252                                                                   ELTINPTS
02253      IF WS-DTL-SUPP-2-1 NOT = SPACES                              ELTINPTS
02254         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02255         STRING WS-DTL-SUPP-2-1 ' '                                ELTINPTS
02256                WS-SUPP-2-2                                        ELTINPTS
02257                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02258         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02259         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02260         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02261         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02262         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02263         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-2-1                 ELTINPTS
02264         MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02265         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02266             ADD +1                TO  WS-CIA                      ELTINPTS
02267             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-2-2             ELTINPTS
02268             MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02269             MOVE SPACES           TO  WS-DTL-SUPP-2-1,            ELTINPTS
02270                                       WS-DTL-SUPP-2-2             ELTINPTS
02271             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02272         ELSE                                                      ELTINPTS
02273           MOVE SPACES           TO  WS-DTL-SUPP-2-1,              ELTINPTS
02274                                     WS-DTL-SUPP-2-2               ELTINPTS
02275           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
02276                                                                   ELTINPTS
02277      IF WS-DTL-BASIC-3-1 NOT = SPACES                             ELTINPTS
02278           OR WS-DTL-SUPP-3-1 NOT = SPACES                         ELTINPTS
02279               ADD +1             TO WS-CIA                        ELTINPTS
02280               MOVE WS-MEDI-INP-RAD-THERPY                         ELTINPTS
02281                                      TO COF-DTL-LINE(WS-CIA)      ELTINPTS
02282               ADD +1                 TO WS-CIA.                   ELTINPTS
02283                                                                   ELTINPTS
02284      IF WS-DTL-BASIC-3-1 NOT = SPACES                             ELTINPTS
02285         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02286         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02287                WS-BASIC-3-1 ' '                                   ELTINPTS
02288                WS-BASIC-3-2                                       ELTINPTS
02289                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02290         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02291         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02292         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02293         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02294         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02295         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-3-1                ELTINPTS
02296         MOVE WS-BASIC-3-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02297         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02298             ADD +1                TO  WS-CIA                      ELTINPTS
02299             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-3-2            ELTINPTS
02300             MOVE WS-BASIC-3-2     TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02301             MOVE SPACES           TO  WS-DTL-BASIC-3-1,           ELTINPTS
02302                                       WS-DTL-BASIC-3-2            ELTINPTS
02303             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02304         ELSE                                                      ELTINPTS
02305             MOVE SPACES           TO  WS-DTL-BASIC-3-1,           ELTINPTS
02306                                       WS-DTL-BASIC-3-2            ELTINPTS
02307             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02308                                                                   ELTINPTS
02309      IF WS-DTL-SUPP-3-1 NOT = SPACES                              ELTINPTS
02310         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02311         STRING WS-DTL-SUPP-3-1 ' '                                ELTINPTS
02312                WS-SUPP-3-2                                        ELTINPTS
02313                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02314         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02315         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02316         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02317         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02318         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02319         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-3-1                 ELTINPTS
02320         MOVE WS-SUPP-3-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02321         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02322             ADD +1                TO  WS-CIA                      ELTINPTS
02323             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-3-2             ELTINPTS
02324             MOVE WS-SUPP-3-2      TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02325             MOVE SPACES           TO  WS-DTL-SUPP-3-1,            ELTINPTS
02326                                       WS-DTL-SUPP-3-2             ELTINPTS
02327             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02328         ELSE                                                      ELTINPTS
02329           MOVE SPACES           TO  WS-DTL-SUPP-3-1,              ELTINPTS
02330                                     WS-DTL-SUPP-3-2               ELTINPTS
02331           PERFORM 9100-OUTPUT-TEXT.                               ELTINPTS
02332                                                                   ELTINPTS
02333      IF WS-DTL-BASIC-4-1 NOT = SPACES                             ELTINPTS
02334           OR WS-DTL-SUPP-4-1 NOT = SPACES                         ELTINPTS
02335                ADD +1             TO WS-CIA                       ELTINPTS
02336                MOVE WS-MEDI-INP-CHEMTHERPY TO                     ELTINPTS
02337                                    COF-DTL-LINE(WS-CIA)           ELTINPTS
02338                ADD +1         TO WS-CIA.                          ELTINPTS
02339                                                                   ELTINPTS
02340      IF WS-DTL-BASIC-4-1 NOT = SPACES                             ELTINPTS
02341         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02342         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02343                WS-BASIC-4-1 ' '                                   ELTINPTS
02344                WS-BASIC-4-2                                       ELTINPTS
02345                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02346         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02347         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02348         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02349         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02350         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02351         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-4-1                ELTINPTS
02352         MOVE WS-BASIC-4-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02353         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02354             ADD +1                TO  WS-CIA                      ELTINPTS
02355             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-4-2            ELTINPTS
02356             MOVE WS-BASIC-4-2     TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02357             MOVE SPACES           TO  WS-DTL-BASIC-4-1,           ELTINPTS
02358                                       WS-DTL-BASIC-4-2            ELTINPTS
02359             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02360         ELSE                                                      ELTINPTS
02361           MOVE SPACES           TO  WS-DTL-BASIC-4-1,             ELTINPTS
02362                                     WS-DTL-BASIC-4-2              ELTINPTS
02363             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02364                                                                   ELTINPTS
02365      IF WS-DTL-SUPP-4-1 NOT = SPACES                              ELTINPTS
02366         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02367         STRING WS-DTL-SUPP-4-1 ' '                                ELTINPTS
02368                WS-SUPP-4-2                                        ELTINPTS
02369                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02370         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02371         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02372         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02373         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02374         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02375         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-4-1                 ELTINPTS
02376         MOVE WS-SUPP-4-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02377         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02378             ADD +1                TO  WS-CIA                      ELTINPTS
02379             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-4-2             ELTINPTS
02380             MOVE WS-SUPP-4-2      TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02381             MOVE SPACES           TO  WS-DTL-SUPP-4-1,            ELTINPTS
02382                                       WS-DTL-SUPP-4-2             ELTINPTS
02383             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02384         ELSE                                                      ELTINPTS
02385           MOVE SPACES           TO  WS-DTL-SUPP-4-1,              ELTINPTS
02386                                     WS-DTL-SUPP-4-2               ELTINPTS
02387             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02388                                                                   ELTINPTS
02389  8300-MED-MED-BASIC-DIFF.                                         ELTINPTS
02390      IF GCT-DIFF-PROV-M-M-BIL-ELIG-IND NOT = '0'                  ELTINPTS
02391         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTINPTS
02392         MOVE 'DIFF-PROV-M-M-BIL-ELIG-IND'                         ELTINPTS
02393                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02394         MOVE GCT-DIFF-PROV-M-M-BIL-ELIG-IND                       ELTINPTS
02395                               TO  CMF-CODE-VALUE                  ELTINPTS
02396         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02397         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-1-1        ELTINPTS
02398         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-1-2.       ELTINPTS
02399                                                                   ELTINPTS
02400  8310-MED-SURG-OB-BASIC-DIFF.                                     ELTINPTS
02401      IF GCT-DIFF-PROV-SOB-BIL-ELIG-IND NOT = '0'                  ELTINPTS
02402         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTINPTS
02403         MOVE 'DIFF-PROV-SOB-BIL-ELIG-IND'                         ELTINPTS
02404                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02405         MOVE GCT-DIFF-PROV-SOB-BIL-ELIG-IND                       ELTINPTS
02406                               TO  CMF-CODE-VALUE                  ELTINPTS
02407         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02408         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-BASIC-2-1        ELTINPTS
02409         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-BASIC-2-2.       ELTINPTS
02410                                                                   ELTINPTS
02411  8320-MED-MED-SUPP-DIFF.                                          ELTINPTS
02412      IF GCT-DIFF-PROV-M-M-BIL-ELIG-IND NOT = '0'                  ELTINPTS
02413         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTINPTS
02414         MOVE 'DIFF-PROV-M-M-BIL-ELIG-IND'                         ELTINPTS
02415                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02416         MOVE GCT-DIFF-PROV-M-M-BIL-ELIG-IND                       ELTINPTS
02417                               TO  CMF-CODE-VALUE                  ELTINPTS
02418         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02419         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-1-1         ELTINPTS
02420         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-1-2.        ELTINPTS
02421                                                                   ELTINPTS
02422  8330-MED-SURG-OB-SUPP-DIFF.                                      ELTINPTS
02423      IF GCT-DIFF-PROV-SOB-BIL-ELIG-IND NOT = '0'                  ELTINPTS
02424         MOVE 'CONTRACT'                TO  CMF-RECORD-PREFIX      ELTINPTS
02425         MOVE 'DIFF-PROV-SOB-BIL-ELIG-IND'                         ELTINPTS
02426                                     TO CMF-ELEMENT-SYSTEM-NAME    ELTINPTS
02427         MOVE GCT-DIFF-PROV-SOB-BIL-ELIG-IND                       ELTINPTS
02428                               TO  CMF-CODE-VALUE                  ELTINPTS
02429         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02430         MOVE CMF-DESCR-LINE (1) TO        WS-DTL-SUPP-2-1         ELTINPTS
02431         MOVE CMF-DESCR-LINE (2) TO        WS-DTL-SUPP-2-2.        ELTINPTS
02432                                                                   ELTINPTS
02433  8340-OUTPUT-DIFF-PROVD-TEXT.                                     ELTINPTS
02434      IF WS-DTL-BASIC-1-1 NOT = SPACES OR                          ELTINPTS
02435          WS-DTL-BASIC-2-1 NOT = SPACES OR                         ELTINPTS
02436             WS-DTL-SUPP-1-1  NOT = SPACES OR                      ELTINPTS
02437              WS-DTL-SUPP-2-1 NOT = SPACES                         ELTINPTS
02438                  ADD +1                TO WS-CIA                  ELTINPTS
02439                  MOVE WS-DIFF-PROVIDER TO COF-DTL-LINE(WS-CIA)    ELTINPTS
02440                  ADD +1                TO WS-CIA.                 ELTINPTS
02441                                                                   ELTINPTS
02442      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTINPTS
02443           OR WS-DTL-SUPP-1-1 NOT = SPACES                         ELTINPTS
02444               MOVE WS-MEDI-MEDI TO COF-DTL-LINE(WS-CIA)           ELTINPTS
02445               ADD +1            TO WS-CIA.                        ELTINPTS
02446                                                                   ELTINPTS
02447      IF WS-DTL-BASIC-1-1 NOT = SPACES                             ELTINPTS
02448         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02449         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02450                WS-BASIC-1-1 ' '                                   ELTINPTS
02451                WS-BASIC-1-2                                       ELTINPTS
02452                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02453         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02454         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02455         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02456         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02457         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02458         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-1-1                ELTINPTS
02459         MOVE WS-BASIC-1-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02460         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02461             ADD +1                TO  WS-CIA                      ELTINPTS
02462             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-1-2            ELTINPTS
02463             MOVE WS-DTL-BASIC-1-2 TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02464             MOVE SPACES           TO  WS-DTL-BASIC-1-1,           ELTINPTS
02465                                       WS-DTL-BASIC-1-2            ELTINPTS
02466            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
02467         ELSE                                                      ELTINPTS
02468           MOVE SPACES           TO  WS-DTL-BASIC-1-1,             ELTINPTS
02469                                     WS-DTL-BASIC-1-2              ELTINPTS
02470             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02471                                                                   ELTINPTS
02472      IF WS-DTL-SUPP-1-1 NOT = SPACES                              ELTINPTS
02473         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02474         STRING WS-DTL-SUPP-1-1 ' '                                ELTINPTS
02475                WS-SUPP-1-2                                        ELTINPTS
02476                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02477         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02478         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02479         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02480         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02481         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02482         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-1-1                 ELTINPTS
02483         MOVE WS-SUPP-1-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02484         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02485             ADD +1                TO  WS-CIA                      ELTINPTS
02486             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-1-2             ELTINPTS
02487             MOVE WS-SUPP-1-2      TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02488             MOVE SPACES           TO  WS-DTL-SUPP-1-1,            ELTINPTS
02489                                       WS-DTL-SUPP-1-2             ELTINPTS
02490             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02491         ELSE                                                      ELTINPTS
02492           MOVE SPACES           TO  WS-DTL-SUPP-1-1,              ELTINPTS
02493                                     WS-DTL-SUPP-1-2               ELTINPTS
02494             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02495                                                                   ELTINPTS
02496      IF WS-DTL-BASIC-2-1 NOT = SPACES                             ELTINPTS
02497           OR WS-DTL-SUPP-2-1 NOT = SPACES                         ELTINPTS
02498               ADD +1             TO WS-CIA                        ELTINPTS
02499               MOVE WS-MEDI-SURG-OB TO COF-DTL-LINE(WS-CIA)        ELTINPTS
02500               ADD +1               TO WS-CIA.                     ELTINPTS
02501                                                                   ELTINPTS
02502      IF WS-DTL-BASIC-2-1 NOT = SPACES                             ELTINPTS
02503         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02504         STRING WS-BASIC-LIT ' '                                   ELTINPTS
02505                WS-BASIC-2-1 ' '                                   ELTINPTS
02506                WS-BASIC-2-2                                       ELTINPTS
02507                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02508         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02509         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02510         MOVE +70              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02511         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02512         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02513         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-BASIC-2-1                ELTINPTS
02514         MOVE WS-BASIC-2-1     TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02515         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02516             ADD +1                TO  WS-CIA                      ELTINPTS
02517             MOVE TCAR-OPF-DATA(2) TO  WS-DTL-BASIC-2-2            ELTINPTS
02518             MOVE WS-BASIC-2-2     TO  COF-DTL-LINE(WS-CIA)        ELTINPTS
02519             MOVE SPACES           TO  WS-DTL-BASIC-2-1,           ELTINPTS
02520                                       WS-DTL-BASIC-2-2            ELTINPTS
02521             PERFORM 9100-OUTPUT-TEXT                              ELTINPTS
02522         ELSE                                                      ELTINPTS
02523           MOVE SPACES           TO  WS-DTL-BASIC-2-1,             ELTINPTS
02524                                     WS-DTL-BASIC-2-2              ELTINPTS
02525             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
02526                                                                   ELTINPTS
02527      IF WS-DTL-SUPP-2-1 NOT = SPACES                              ELTINPTS
02528         MOVE SPACES           TO  TCAR-FROM-AREA                  ELTINPTS
02529         STRING WS-DTL-SUPP-2-1 ' '                                ELTINPTS
02530                WS-SUPP-2-2                                        ELTINPTS
02531                 DELIMITED BY SIZE INTO TCAR-FROM-AREA             ELTINPTS
02532         PERFORM TCPR-000-TEXT-COMPRESSION                         ELTINPTS
02533         MOVE +03              TO  TCAR-OUTPUT-FIELD-COUNT         ELTINPTS
02534         MOVE +62              TO  TCAR-OUTPUT-FIELD-1-LEN         ELTINPTS
02535         MOVE +70              TO  TCAR-OUTPUT-FIELD-2-LEN         ELTINPTS
02536         PERFORM TCPR-000-TEXT-UNSTRING                            ELTINPTS
02537         MOVE TCAR-OPF-DATA(1) TO  WS-DTL-SUPP-2-1                 ELTINPTS
02538         MOVE WS-SUPP-2-1      TO  COF-DTL-LINE(WS-CIA)            ELTINPTS
02539         IF TCAR-OPF-DATA(2) NOT = SPACES                          ELTINPTS
02540            ADD +1                TO  WS-CIA                       ELTINPTS
02541            MOVE TCAR-OPF-DATA(2) TO  WS-DTL-SUPP-2-2              ELTINPTS
02542            MOVE WS-SUPP-2-2      TO  COF-DTL-LINE(WS-CIA)         ELTINPTS
02543            MOVE SPACES           TO  WS-DTL-SUPP-2-1,             ELTINPTS
02544                                      WS-DTL-SUPP-2-2              ELTINPTS
02545            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
02546         ELSE                                                      ELTINPTS
02547            MOVE SPACES           TO  WS-DTL-SUPP-2-1,             ELTINPTS
02548                                      WS-DTL-SUPP-2-2              ELTINPTS
02549            PERFORM 9100-OUTPUT-TEXT.                              ELTINPTS
02550                                                                   ELTINPTS
02551  8400-TRANSF-OTHER-RESP-IND.                                      ELTINPTS
02552      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) NOT = ZEROS          ELTINPTS
02553         SET PLT-INDEX2      TO  1                                 ELTINPTS
02554      ELSE                                                         ELTINPTS
02555         IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS       ELTINPTS
02556            SET PLT-INDEX2   TO  2.                                ELTINPTS
02557                                                                   ELTINPTS
02558      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)        ELTINPTS
02559         =  ZEROS  OR  LOW-VALUES OR SPACES                        ELTINPTS
02560            CONTINUE                                               ELTINPTS
02561      ELSE                                                         ELTINPTS
02562         ADD +1     TO  WS-CIA                                     ELTINPTS
02563         MOVE 'BP'                 TO  CMF-RECORD-PREFIX           ELTINPTS
02564         MOVE 'TRANSF-OTHER-RESP-IND'                              ELTINPTS
02565                                   TO  CMF-ELEMENT-SYSTEM-NAME     ELTINPTS
02566         MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)   ELTINPTS
02567                                   TO  CMF-CODE-VALUE              ELTINPTS
02568         PERFORM 9300-CALL-CODES-MANUAL                            ELTINPTS
02569         MOVE SPACES              TO   WS-TEMP-TEXT-AREA           ELTINPTS
02570         MOVE +00                 TO   WS-TEMP-NOT-USED-CNT        ELTINPTS
02571         PERFORM 9400-CODES-MANUAL-LONG                            ELTINPTS
02572         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
02573                                                                   ELTINPTS
02574  8500-SCAN-TAB.                                                   ELTINPTS
02575      PERFORM 8510-BEN-TAB-AAR.                                    ELTINPTS
02576      ADD +1           TO WS-CIA.                                  ELTINPTS
02577      MOVE WS-PVE-TEXT TO COF-DTL-LINE(WS-CIA).                    ELTINPTS
02578      ADD +1           TO WS-CIA.                                  ELTINPTS
02579      PERFORM 9100-OUTPUT-TEXT.                                    ELTINPTS
02580      PERFORM 8520-BEN-TAB-PPF.                                    ELTINPTS
02581      PERFORM 8530-BEN-TAB-ADL.                                    ELTINPTS
02582      PERFORM 8540-BEN-TAB-ABM.                                    ELTINPTS
02583      PERFORM 8550-BEN-TAB-ACL.                                    ELTINPTS
02584      PERFORM 8560-BEN-TAB-AOL.                                    ELTINPTS
02585                                                                   ELTINPTS
02586  8510-BEN-TAB-AAR.                                                ELTINPTS
02587      MOVE WS-NO TO WS-DISPLAY-AAR-TEXT.                           ELTINPTS
02588      SET PLT-INDEX2 TO 1.                                         ELTINPTS
02589                                                                   ELTINPTS
02590      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02591                 NOT = LOW-VALUES                                  ELTINPTS
02592         IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02593                 NOT = SPACE                                       ELTINPTS
02594            MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.                    ELTINPTS
02595                                                                   ELTINPTS
02596      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02597                                                                   ELTINPTS
02598      IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02599                 NOT = LOW-VALUES                                  ELTINPTS
02600         IF PLP-BEN-TAB-PROVN-ID-AAR (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02601                 NOT = SPACE                                       ELTINPTS
02602            MOVE WS-YES TO WS-DISPLAY-AAR-TEXT.                    ELTINPTS
02603                                                                   ELTINPTS
02604      IF WS-DISPLAY-AAR-TEXT = WS-YES                              ELTINPTS
02605         MOVE +2                  TO WS-CIA                        ELTINPTS
02606         MOVE WS-CONTRACT-RELATED TO COF-DTL-LINE(WS-CIA)          ELTINPTS
02607         ADD  +1                  TO WS-CIA                        ELTINPTS
02608         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
02609                                                                   ELTINPTS
02610  8520-BEN-TAB-PPF.                                                ELTINPTS
02611      MOVE ZEROS   TO  WS-HOLD1, WS-HOLD2.                         ELTINPTS
02612      SET PLT-INDEX2 TO 1.                                         ELTINPTS
02613      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02614                 NOT = LOW-VALUES                                  ELTINPTS
02615         IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02616                 NOT = SPACE                                       ELTINPTS
02617            MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02618                 TO  WS-HOLD1.                                     ELTINPTS
02619                                                                   ELTINPTS
02620      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02621      IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02622                 NOT = LOW-VALUES                                  ELTINPTS
02623         IF PLP-BEN-TAB-PROVN-ID-PPF (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02624                 NOT = SPACE                                       ELTINPTS
02625            MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02626                 TO  WS-HOLD2.                                     ELTINPTS
02627                                                                   ELTINPTS
02628      IF WS-HOLD1 = WS-HOLD2                                       ELTINPTS
02629         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02630            CONTINUE                                               ELTINPTS
02631         ELSE                                                      ELTINPTS
02632            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02633            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02634            EXEC CICS  LINK  PROGRAM('ELGPPF')                     ELTINPTS
02635                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02636         END-IF                                                    ELTINPTS
02637      ELSE                                                         ELTINPTS
02638         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02639            MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                     ELTINPTS
02640            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02641            EXEC CICS  LINK  PROGRAM('ELGPPF')                     ELTINPTS
02642                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02643         ELSE                                                      ELTINPTS
02644            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02645            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02646            EXEC CICS  LINK  PROGRAM('ELGPPF')                     ELTINPTS
02647                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02648            IF WS-HOLD2 = ZEROS                                    ELTINPTS
02649               CONTINUE                                            ELTINPTS
02650            ELSE                                                   ELTINPTS
02651               MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                  ELTINPTS
02652               PERFORM 9000-GET-TAB-REC                            ELTINPTS
02653               EXEC CICS  LINK  PROGRAM('ELGPPF')                  ELTINPTS
02654                                COMMAREA(DFHCOMMAREA)  END-EXEC    ELTINPTS
02655            END-IF                                                 ELTINPTS
02656         END-IF                                                    ELTINPTS
02657      END-IF.                                                      ELTINPTS
02658                                                                   ELTINPTS
02659  8530-BEN-TAB-ADL.                                                ELTINPTS
02660      MOVE ZEROS     TO  WS-HOLD1, WS-HOLD2.                       ELTINPTS
02661      SET PLT-INDEX2 TO  1.                                        ELTINPTS
02662      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02663                 NOT = LOW-VALUES                                  ELTINPTS
02664         IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02665                 NOT = SPACE                                       ELTINPTS
02666            MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02667                 TO  WS-HOLD1.                                     ELTINPTS
02668                                                                   ELTINPTS
02669      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02670      IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02671                 NOT = LOW-VALUES                                  ELTINPTS
02672         IF PLP-BEN-TAB-PROVN-ID-ADL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02673                 NOT = SPACE                                       ELTINPTS
02674            MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02675                 TO  WS-HOLD2.                                     ELTINPTS
02676                                                                   ELTINPTS
02677      IF WS-HOLD1 = WS-HOLD2                                       ELTINPTS
02678         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02679            CONTINUE                                               ELTINPTS
02680         ELSE                                                      ELTINPTS
02681            MOVE WS-HOLD1 TO KWA-GCTABULR-KEY                      ELTINPTS
02682            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02683            EXEC CICS  LINK  PROGRAM('ELGDEDBL')                   ELTINPTS
02684                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02685         END-IF                                                    ELTINPTS
02686      ELSE                                                         ELTINPTS
02687         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02688            MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                     ELTINPTS
02689            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02690            EXEC CICS  LINK  PROGRAM('ELGDEDBL')                   ELTINPTS
02691                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02692         ELSE                                                      ELTINPTS
02693            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02694            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02695            EXEC CICS  LINK  PROGRAM('ELGDEDBL')                   ELTINPTS
02696                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02697            IF WS-HOLD2 = ZEROS                                    ELTINPTS
02698               CONTINUE                                            ELTINPTS
02699            ELSE                                                   ELTINPTS
02700               MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                  ELTINPTS
02701               PERFORM 9000-GET-TAB-REC                            ELTINPTS
02702               EXEC CICS  LINK  PROGRAM('ELGDEDBL')                ELTINPTS
02703                                COMMAREA(DFHCOMMAREA)  END-EXEC    ELTINPTS
02704            END-IF                                                 ELTINPTS
02705         END-IF                                                    ELTINPTS
02706      END-IF.                                                      ELTINPTS
02707                                                                   ELTINPTS
02708  8540-BEN-TAB-ABM.                                                ELTINPTS
02709      MOVE ZEROS   TO  WS-HOLD1, WS-HOLD2.                         ELTINPTS
02710      SET PLT-INDEX2 TO 1.                                         ELTINPTS
02711      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02712                 NOT = LOW-VALUES                                  ELTINPTS
02713         IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02714                 NOT = SPACE                                       ELTINPTS
02715            MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02716                 TO  WS-HOLD1.                                     ELTINPTS
02717                                                                   ELTINPTS
02718      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02719      IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02720                 NOT = LOW-VALUES                                  ELTINPTS
02721         IF PLP-BEN-TAB-PROVN-ID-ABM (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02722                 NOT = SPACE                                       ELTINPTS
02723            MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02724                 TO  WS-HOLD2.                                     ELTINPTS
02725                                                                   ELTINPTS
02726      IF WS-HOLD1 = WS-HOLD2                                       ELTINPTS
02727         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02728            CONTINUE                                               ELTINPTS
02729         ELSE                                                      ELTINPTS
02730            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02731            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02732            EXEC CICS  LINK PROGRAM('ELGMAXIM')                    ELTINPTS
02733                            COMMAREA(DFHCOMMAREA)  END-EXEC        ELTINPTS
02734         END-IF                                                    ELTINPTS
02735      ELSE                                                         ELTINPTS
02736         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02737            MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                     ELTINPTS
02738            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02739            EXEC CICS  LINK PROGRAM('ELGMAXIM')                    ELTINPTS
02740                            COMMAREA(DFHCOMMAREA)  END-EXEC        ELTINPTS
02741         ELSE                                                      ELTINPTS
02742            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02743            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02744            EXEC CICS  LINK PROGRAM('ELGMAXIM')                    ELTINPTS
02745                            COMMAREA(DFHCOMMAREA)  END-EXEC        ELTINPTS
02746            IF WS-HOLD2 = ZEROS                                    ELTINPTS
02747               CONTINUE                                            ELTINPTS
02748            ELSE                                                   ELTINPTS
02749               MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                  ELTINPTS
02750               PERFORM 9000-GET-TAB-REC                            ELTINPTS
02751               EXEC CICS  LINK PROGRAM('ELGMAXIM')                 ELTINPTS
02752                               COMMAREA(DFHCOMMAREA)  END-EXEC     ELTINPTS
02753            END-IF                                                 ELTINPTS
02754         END-IF                                                    ELTINPTS
02755      END-IF.                                                      ELTINPTS
02756                                                                   ELTINPTS
02757  8550-BEN-TAB-ACL.                                                ELTINPTS
02758      MOVE ZEROS   TO  WS-HOLD1, WS-HOLD2.                         ELTINPTS
02759      SET PLT-INDEX2 TO 1.                                         ELTINPTS
02760      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02761                 NOT = LOW-VALUES                                  ELTINPTS
02762         IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02763                 NOT = SPACE                                       ELTINPTS
02764            MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02765                 TO  WS-HOLD1.                                     ELTINPTS
02766                                                                   ELTINPTS
02767      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02768      IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02769                 NOT = LOW-VALUES                                  ELTINPTS
02770         IF PLP-BEN-TAB-PROVN-ID-ACL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02771                 NOT = SPACE                                       ELTINPTS
02772            MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02773                 TO  WS-HOLD2.                                     ELTINPTS
02774                                                                   ELTINPTS
02775      IF WS-HOLD1 = WS-HOLD2                                       ELTINPTS
02776         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02777            CONTINUE                                               ELTINPTS
02778         ELSE                                                      ELTINPTS
02779             MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                    ELTINPTS
02780             PERFORM 9000-GET-TAB-REC                              ELTINPTS
02781             EXEC CICS  LINK  PROGRAM('ELGCOINS')                  ELTINPTS
02782                              COMMAREA(DFHCOMMAREA)  END-EXEC      ELTINPTS
02783         END-IF                                                    ELTINPTS
02784      ELSE                                                         ELTINPTS
02785         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02786            MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                     ELTINPTS
02787            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02788            EXEC CICS  LINK  PROGRAM('ELGCOINS')                   ELTINPTS
02789                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02790         ELSE                                                      ELTINPTS
02791            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02792            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02793            EXEC CICS  LINK  PROGRAM('ELGCOINS')                   ELTINPTS
02794                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02795            IF WS-HOLD2 = ZEROS                                    ELTINPTS
02796               CONTINUE                                            ELTINPTS
02797            ELSE                                                   ELTINPTS
02798               MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                  ELTINPTS
02799               PERFORM 9000-GET-TAB-REC                            ELTINPTS
02800               EXEC CICS  LINK  PROGRAM('ELGCOINS')                ELTINPTS
02801                                COMMAREA(DFHCOMMAREA)  END-EXEC    ELTINPTS
02802            END-IF                                                 ELTINPTS
02803         END-IF                                                    ELTINPTS
02804      END-IF.                                                      ELTINPTS
02805                                                                   ELTINPTS
02806  8560-BEN-TAB-AOL.                                                ELTINPTS
02807      MOVE ZEROS   TO  WS-HOLD1, WS-HOLD2.                         ELTINPTS
02808      SET PLT-INDEX2 TO 1.                                         ELTINPTS
02809      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02810                 NOT = LOW-VALUES                                  ELTINPTS
02811         IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02812                 NOT = SPACE                                       ELTINPTS
02813            MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02814                 TO  WS-HOLD1.                                     ELTINPTS
02815                                                                   ELTINPTS
02816      SET PLT-INDEX2 TO 2.                                         ELTINPTS
02817      IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)         ELTINPTS
02818                 NOT = LOW-VALUES                                  ELTINPTS
02819         IF PLP-BEN-TAB-PROVN-ID-AOL (PLT-INDEX1, PLT-INDEX2)      ELTINPTS
02820                 NOT = SPACE                                       ELTINPTS
02821            MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  ELTINPTS
02822                 TO  WS-HOLD2.                                     ELTINPTS
02823                                                                   ELTINPTS
02824      IF WS-HOLD1 = WS-HOLD2                                       ELTINPTS
02825         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02826            CONTINUE                                               ELTINPTS
02827         ELSE                                                      ELTINPTS
02828            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02829            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02830            EXEC CICS  LINK  PROGRAM('ELGOUTPX')                   ELTINPTS
02831                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02832         END-IF                                                    ELTINPTS
02833      ELSE                                                         ELTINPTS
02834         IF WS-HOLD1 = ZEROS                                       ELTINPTS
02835            MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                     ELTINPTS
02836            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02837            EXEC CICS  LINK  PROGRAM('ELGOUTPX')                   ELTINPTS
02838                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02839         ELSE                                                      ELTINPTS
02840            MOVE WS-HOLD1  TO KWA-GCTABULR-KEY                     ELTINPTS
02841            PERFORM 9000-GET-TAB-REC                               ELTINPTS
02842            EXEC CICS  LINK  PROGRAM('ELGOUTPX')                   ELTINPTS
02843                             COMMAREA(DFHCOMMAREA)  END-EXEC       ELTINPTS
02844            IF WS-HOLD2 = ZEROS                                    ELTINPTS
02845               CONTINUE                                            ELTINPTS
02846            ELSE                                                   ELTINPTS
02847               MOVE WS-HOLD2  TO KWA-GCTABULR-KEY                  ELTINPTS
02848               PERFORM 9000-GET-TAB-REC                            ELTINPTS
02849               EXEC CICS  LINK  PROGRAM('ELGOUTPX')                ELTINPTS
02850                                COMMAREA(DFHCOMMAREA)  END-EXEC    ELTINPTS
02851            END-IF                                                 ELTINPTS
02852         END-IF                                                    ELTINPTS
02853      END-IF.                                                      ELTINPTS
02854                                                                   ELTINPTS
02855  8600-PAY-CONSID-TEXT.                                            ELTINPTS
02856      INITIALIZE TCAR-FROM-AREA.                                   ELTINPTS
02857      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTINPTS
02858             WS-PAY-CONSDR-TEXT2                                   ELTINPTS
02859                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTINPTS
02860      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
02861      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTINPTS
02862      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTINPTS
02863                                TCAR-OUTPUT-FIELD-2-LEN.           ELTINPTS
02864      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
02865      IF WS-CIA > 17                                               ELTINPTS
02866            PERFORM 9100-OUTPUT-TEXT                               ELTINPTS
02867            MOVE +1            TO WS-CIA.                          ELTINPTS
02868      ADD +1                TO  WS-CIA.                            ELTINPTS
02869      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTINPTS
02870      ADD +1                TO  WS-CIA.                            ELTINPTS
02871      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTINPTS
02872      PERFORM 9100-OUTPUT-TEXT.                                    ELTINPTS
02873                                                                   ELTINPTS
02874  9000-GET-TAB-REC.                                                ELTINPTS
02875      SET CIA-GCTABULR-DDN TO TRUE.                                ELTINPTS
02876      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02877          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTINPTS
02878      MOVE KWA-GCTABULR-KEY          TO IOP-FILE-KEY.              ELTINPTS
02879      SET CIA-GCTABULR-DDN           TO TRUE.                      ELTINPTS
02880      SET IOP-RD                     TO TRUE.                      ELTINPTS
02881      SET IOP-FCQ-NONE               TO TRUE.                      ELTINPTS
02882      SET IOP-KVQ-NONE               TO TRUE.                      ELTINPTS
02883                                                                   ELTINPTS
02884      EXEC CICS  LINK  PROGRAM('ELUIOPGM')                         ELTINPTS
02885                       COMMAREA(DFHCOMMAREA)  END-EXEC.            ELTINPTS
02886                                                                   ELTINPTS
02887      IF IOP-RC-NOTFND                                             ELTINPTS
02888         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTINPTS
02889         EXEC CICS  ABEND                                          ELTINPTS
02890                    ABCODE(CIA-ABCODE)  END-EXEC.                  ELTINPTS
02891                                                                   ELTINPTS
02892      IF NOT IOP-RC-OK                                             ELTINPTS
02893         SET CIA-AB-CRITIO          TO TRUE                        ELTINPTS
02894         EXEC CICS  ABEND                                          ELTINPTS
02895                    ABCODE(CIA-ABCODE)  END-EXEC.                  ELTINPTS
02896                                                                   ELTINPTS
02897  9050-OUTPUT-TEXT.                                                ELTINPTS
02898      IF TCAR-OUTPUT-FIELDS-USED > 1                               ELTINPTS
02899         ADD +1                TO WS-CIA                           ELTINPTS
02900         MOVE TCAR-OPF-DATA(2) TO COF-DTL-LINE(WS-CIA)             ELTINPTS
02901         PERFORM 9100-OUTPUT-TEXT                                  ELTINPTS
02902      ELSE                                                         ELTINPTS
02903         PERFORM 9100-OUTPUT-TEXT.                                 ELTINPTS
02904                                                                   ELTINPTS
02905  9100-OUTPUT-TEXT.                                                ELTINPTS
02906      MOVE +0  TO  COF-NBR-HDR-LINES.                              ELTINPTS
02907      MOVE WS-CIA TO  COF-NBR-DTL-LINES.                           ELTINPTS
02908      MOVE ' '  TO  COF-FUNCTION.                                  ELTINPTS
02909                                                                   ELTINPTS
02910      EXEC CICS  LINK  PROGRAM('ELUOUTPT')                         ELTINPTS
02911                       COMMAREA(DFHCOMMAREA)                       ELTINPTS
02912      END-EXEC.                                                    ELTINPTS
02913      MOVE +1   TO WS-CIA.                                         ELTINPTS
02914                                                                   ELTINPTS
02915 ****************************************************************  ELTINPTS
02916 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTINPTS
02917 ****************************************************************  ELTINPTS
02918  9200-HEADER-OUTPUT-REQUEST.                                      ELTINPTS
02919      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTINPTS
02920      SET COF-NEW-PAGE TO TRUE.                                    ELTINPTS
02921                                                                   ELTINPTS
02922      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTINPTS
02923                     COMMAREA (DFHCOMMAREA)                        ELTINPTS
02924                     END-EXEC.                                     ELTINPTS
02925                                                                   ELTINPTS
02926 ****************************************************************  ELTINPTS
02927 *          C A L L   C O D E S   M A N U A L                   *  ELTINPTS
02928 ****************************************************************  ELTINPTS
02929  9300-CALL-CODES-MANUAL.                                          ELTINPTS
02930      INITIALIZE CMF-RETURN-CODE,                                  ELTINPTS
02931                 TCAR-FROM-AREA.                                   ELTINPTS
02932                                                                   ELTINPTS
02933      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTINPTS
02934                       COMMAREA(DFHCOMMAREA)                       ELTINPTS
02935      END-EXEC.                                                    ELTINPTS
02936                                                                   ELTINPTS
02937      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTINPTS
02938      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTINPTS
02939          ADDRESS OF CMF-DESCR.                                    ELTINPTS
02940                                                                   ELTINPTS
02941  9400-CODES-MANUAL-LONG.                                          ELTINPTS
02942      IF WS-TEMP-NOT-USED-CNT > ZERO                               ELTINPTS
02943          MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN     ELTINPTS
02944          STRING CMF-DESCR-LINE(1), ' '                            ELTINPTS
02945             CMF-DESCR-LINE(2), ' '                                ELTINPTS
02946             CMF-DESCR-LINE(3), ' '                                ELTINPTS
02947             CMF-DESCR-LINE(4), ' '                                ELTINPTS
02948             CMF-DESCR-LINE(5), ' '                                ELTINPTS
02949             CMF-DESCR-LINE(6), ' '                                ELTINPTS
02950             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTINPTS
02951                                                                   ELTINPTS
02952      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTINPTS
02953          MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTINPTS
02954          STRING WS-TEMP-TEXT-AREA, ' '                            ELTINPTS
02955             CMF-DESCR-LINE(1), ' '                                ELTINPTS
02956             CMF-DESCR-LINE(2), ' '                                ELTINPTS
02957             CMF-DESCR-LINE(3), ' '                                ELTINPTS
02958             CMF-DESCR-LINE(4), ' '                                ELTINPTS
02959             CMF-DESCR-LINE(5), ' '                                ELTINPTS
02960             CMF-DESCR-LINE(6), ' '                                ELTINPTS
02961             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTINPTS
02962      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTINPTS
02963      MOVE +07              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTINPTS
02964      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN,           ELTINPTS
02965                                TCAR-OUTPUT-FIELD-3-LEN,           ELTINPTS
02966                                TCAR-OUTPUT-FIELD-4-LEN,           ELTINPTS
02967                                TCAR-OUTPUT-FIELD-5-LEN,           ELTINPTS
02968                                TCAR-OUTPUT-FIELD-6-LEN,           ELTINPTS
02969                                TCAR-OUTPUT-FIELD-7-LEN.           ELTINPTS
02970      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTINPTS
02971                                                                   ELTINPTS
02972      IF WS-MOVE-LINES-TO-CIA                                      ELTINPTS
02973         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTINPTS
02974            COMPUTE WS-TEMP-NOT-USED-CNT = 79 -                    ELTINPTS
02975                                           WS-TEMP-NOT-USED-CNT    ELTINPTS
02976            PERFORM 9420-CONCATENATE-TO-TEMP-TEXT                  ELTINPTS
02977               VARYING WS-SUB1 FROM 1 BY 1                         ELTINPTS
02978               UNTIL WS-TEMP-NOT-USED-CNT >  +78                   ELTINPTS
02979            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTINPTS
02980            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTINPTS
02981            ADD +1 TO WS-CIA                                       ELTINPTS
02982         ELSE                                                      ELTINPTS
02983           MOVE TCAR-OPF-DATA(1)  TO COF-DTL-LINE(WS-CIA)          ELTINPTS
02984           ADD +1 TO WS-CIA.                                       ELTINPTS
02985                                                                   ELTINPTS
02986      IF WS-MOVE-LINES-TO-CIA                                      ELTINPTS
02987         IF TCAR-OUTPUT-FIELDS-USED >  1                           ELTINPTS
02988            PERFORM 9440-MOVE-LINES-TO-CIA                         ELTINPTS
02989               VARYING WS-SUB1  FROM  2  BY  1                     ELTINPTS
02990               UNTIL  WS-SUB1 > TCAR-OUTPUT-FIELDS-USED            ELTINPTS
02991         ELSE                                                      ELTINPTS
02992           CONTINUE                                                ELTINPTS
02993      ELSE                                                         ELTINPTS
02994         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTINPTS
02995                                                                   ELTINPTS
02996  9420-CONCATENATE-TO-TEMP-TEXT.                                   ELTINPTS
02997      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTINPTS
02998      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTINPTS
02999           WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT).                ELTINPTS
03000                                                                   ELTINPTS
03001  9440-MOVE-LINES-TO-CIA.                                          ELTINPTS
03002      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTINPTS
03003      IF WS-CIA > 19                                               ELTINPTS
03004             PERFORM 9100-OUTPUT-TEXT.                             ELTINPTS
03005      ADD +1  TO  WS-CIA.                                          ELTINPTS
03006                                                                   ELTINPTS
03007 * C O M P R E S S I O N   A N D   U N S T R I N G   R O U T I N E ELTINPTS
03008  COPY ELSTCOMP.                                                   ELTINPTS
