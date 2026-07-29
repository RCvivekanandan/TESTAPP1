00001  IDENTIFICATION DIVISION.                                         06/29/02
00002  PROGRAM-ID.    ELTOBREL.                                         ELTOBREL
00003  AUTHOR.        BOB OEHMEN.                                          LV001
00004  DATE-WRITTEN.  08/17/95                                          ELTOBREL
00005  DATE-COMPILED.                                                   ELTOBREL
00006      SKIP3                                                        ELTOBREL
00007 ****************************************************************  ELTOBREL
00008 *      ELTOBREL - ELS:  OB RELATED STUFF TOPIC PROGRAM         *  ELTOBREL
00009 * THIS PROGRAM DISPLAYS ALL THE INFORMATION FROM THE CODES     *  ELTOBREL
00010 * MANUAL.  THE WORDS 'BASIC' AND/OR 'SUPPLEMENTAL WILL ONLY BE *  ELTOBREL
00011 * DISPLAYED WHEN THERE IS AN LOB OF '1', '2' OR '3'.           *  ELTOBREL
00012 ****************************************************************  ELTOBREL
00013      TITLE 'PROGRAM HISTORY'.                                     ELTOBREL
00014 ****************************************************************  ELTOBREL
00015 *                                                              *  ELTOBREL
00016 *                                                              *  ELTOBREL
00017 *              U P D A T E   H I S T O R Y                     *  ELTOBREL
00018 *                                                              *  ELTOBREL
00019 *   DATE    PGM  DESCRIPTION                                   *  ELTOBREL
00020 * --------  ---  --------------------------------------------- *  ELTOBREL
00021 *  10/31/95 RGO  CHANGED                                       *  ELTOBREL
00022 *                                                              *  ELTOBREL
00023 *                                                              *  ELTOBREL
00024 *                                                              *  ELTOBREL
00025 ****************************************************************  ELTOBREL
00026      TITLE 'WORKING STORAGE SECTION'.                             ELTOBREL
00027  ENVIRONMENT DIVISION.                                            ELTOBREL
00028                                                                   ELTOBREL
00029  DATA DIVISION.                                                   ELTOBREL
00030  WORKING-STORAGE SECTION.                                         ELTOBREL
00031  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTOBREL
00032          '** ELTOBREL WS BEGINS **'.                              ELTOBREL
00033  01  WS-PARA-ID1                 PIC  X(04) VALUE 'XXXX'.         ELTOBREL
00034  01  WS-PARA-ID2                 PIC  X(04) VALUE 'XXXX'.         ELTOBREL
00035                                                                   ELTOBREL
00036 /***************************************************************  ELTOBREL
00037 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTOBREL
00038 ****************************************************************  ELTOBREL
00039  01  WORK-FIELDS.                                                 ELTOBREL
00040      05  FOUND-BP-INST           PIC X(1) VALUE SPACE.            ELTOBREL
00041      05  FOUND-BP-PROF           PIC X(1) VALUE SPACE.            ELTOBREL
00042      05  WS-SUB                  PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00043      05  WS-SUB-CMF              PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00044      05  WS-SUB1                 PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00045      05  WS-SUB2                 PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00046      05  WS-SUB3                 PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00047      05  WS-SUB4                 PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00048      05  WS-CIA                  PIC S9(03) COMP-3 VALUE +0.      ELTOBREL
00049      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP-3.               ELTOBREL
00050      05  WS-REC-LEN              PIC S9(04) COMP   VALUE +0.      ELTOBREL
00051      05  WS-PERCENT-FLD.                                          ELTOBREL
00052        10  WS-PERCENTAGE         PIC ZZ9.                         ELTOBREL
00053        10  WS-PERCENT-SIGN       PIC X.                           ELTOBREL
00054                                                                   ELTOBREL
00055                                                                   ELTOBREL
00056  01  SWITCHES.                                                    ELTOBREL
00057      05  DISPLAY-BAS-SUP         PIC X(01) VALUE 'N'.             ELTOBREL
00058      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTOBREL
00059          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTOBREL
00060      05  CALL-ELUOUTPT-IND       PIC X(01).                       ELTOBREL
00061          88  YES-CALL-ELUOUTPT              VALUE 'Y'.            ELTOBREL
00062      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTOBREL
00063          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTOBREL
00064      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTOBREL
00065          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTOBREL
00066                                                                   ELTOBREL
00067  01  FIRST-CODE-LINE.                                             ELTOBREL
00068      05  FIRST-CODE-CHAR79       PIC X(79).                       ELTOBREL
00069      05  FIRST-CODE-BROKE-UP   REDEFINES FIRST-CODE-CHAR79.       ELTOBREL
00070          10  FIRST-CHAR          PIC X(1).                        ELTOBREL
00071              88  FIRST-CHAR-SHOW-AS-IS  VALUE QUOTE.              ELTOBREL
00072          10  FIRST-THE-REST      PIC X(78).                       ELTOBREL
00073                                                                   ELTOBREL
00074 /--------------------------------------------------------------*  ELTOBREL
00075 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTOBREL
00076 *--------------------------------------------------------------*  ELTOBREL
00077  01  WS-BEN-PROV-IDS.                                             ELTOBREL
00078      05  WS-TABLE-MAX-CNT        PIC S9(4)  VALUE +19 COMP.       ELTOBREL
00079      05  WS-LIST-BP-CNT          PIC S9(04) VALUE +0 COMP.        ELTOBREL
00080      05  WS-INST-IP-CNT          PIC S9(04) VALUE +1  COMP.       ELTOBREL
00081      05  WS-INST-IP-TABS.                                         ELTOBREL
00082          10  FILLER              PIC  X(06) VALUE 'NRB  A'.       ELTOBREL
00083      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTOBREL
00084                                  PIC  X(06) OCCURS 1 TIMES.       ELTOBREL
00085                                                                   ELTOBREL
00086      05  WS-INST-OP-CNT          PIC  S9(04) VALUE +1 COMP.       ELTOBREL
00087      05  WS-INST-OP-TABS.                                         ELTOBREL
00088          10  FILLER              PIC  X(06) VALUE 'BDRO B'.       ELTOBREL
00089      05  WS-INST-OP-BP  REDEFINES WS-INST-OP-TABS                 ELTOBREL
00090                                  PIC  X(06) OCCURS 1 TIMES.       ELTOBREL
00091                                                                   ELTOBREL
00092      05  WS-PROF-IP-CNT          PIC S9(04) VALUE +4 COMP.        ELTOBREL
00093      05  WS-PROF-IP-TABS.                                         ELTOBREL
00094          10  FILLER              PIC  X(06) VALUE 'ASEM C'.       ELTOBREL
00095          10  FILLER              PIC  X(06) VALUE 'INVI C'.       ELTOBREL
00096          10  FILLER              PIC  X(06) VALUE 'NCRC C'.       ELTOBREL
00097          10  FILLER              PIC  X(06) VALUE 'NNBC E'.       ELTOBREL
00098      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTOBREL
00099                                  PIC  X(06) OCCURS 4 TIMES.       ELTOBREL
00100                                                                   ELTOBREL
00101      05  WS-PROF-OP-CNT          PIC S9(04) VALUE +4 COMP.        ELTOBREL
00102      05  WS-PROF-OP-TABS.                                         ELTOBREL
00103          10  FILLER              PIC  X(06) VALUE 'ASEM C'.       ELTOBREL
00104          10  FILLER              PIC  X(06) VALUE 'INVI C'.       ELTOBREL
00105          10  FILLER              PIC  X(06) VALUE 'NCRC C'.       ELTOBREL
00106          10  FILLER              PIC  X(06) VALUE 'NNBC E'.       ELTOBREL
00107      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTOBREL
00108                                  PIC  X(06) OCCURS 4 TIMES.       ELTOBREL
00109                                                                   ELTOBREL
00110 /***************************************************************  ELTOBREL
00111 *       LITERAL TEXT AREA CREATED FOR OB RELATED               *  ELTOBREL
00112 *   - GROUP SPECIFIC FIELDS THAT ARE TO BE DISPLAYED           *  ELTOBREL
00113 ****************************************************************  ELTOBREL
00114  01  WS-HEALTHY-EXP-YES.                                          ELTOBREL
00115      05  FILLER                  PIC  X(48) VALUE                 ELTOBREL
00116           'HEALTHY EXPECTATIONS DOES APPLY.               '.      ELTOBREL
00117  01  WS-HEALTHY-EXP-NO.                                           ELTOBREL
00118      05  FILLER                  PIC  X(48) VALUE                 ELTOBREL
00119           'HEALTHY EXPECTATIONS DOES NOT APPLY.           '.      ELTOBREL
00120                                                                   ELTOBREL
00121  01  WS-NEWBORN-AGE-HD1.                                          ELTOBREL
00122      05  FILLER                  PIC  X(49) VALUE                 ELTOBREL
00123              'MAXIMUM NUMBER OF DAYS A NEWBORN IS ELIGIBLE FOR '. ELTOBREL
00124      05  FILLER                  PIC  X(30) VALUE                 ELTOBREL
00125              'THE SPECIAL CONSIDERATIONS'.                        ELTOBREL
00126                                                                   ELTOBREL
00127                                                                   ELTOBREL
00128                                                                   ELTOBREL
00129  01  WS-NORMAL-NEWBORN-CHGS.                                      ELTOBREL
00130   10  FILLER                    PIC X(33)                         ELTOBREL
00131       VALUE 'NORMAL NEWBORN BABY CHARGES ARE:'.                   ELTOBREL
00132   10  FILLER                    PIC X(46) VALUE LOW-VALUES.       ELTOBREL
00133                                                                   ELTOBREL
00134  01  WS-RULE-BILL-NEWBORN-CHGS   PIC X(36)                        ELTOBREL
00135           VALUE 'RULES FOR BILLING NEWBORN CHARGES: '.            ELTOBREL
00136                                                                   ELTOBREL
00137  01  WS-SPECIAL-NEWBORN-COV.                                      ELTOBREL
00138   10  FILLER                    PIC X(79) VALUE                   ELTOBREL
00139        'SPECIAL NEWBORN ELIGIBILITY/PROCESSING REQUIREMENTS: '.   ELTOBREL
00140                                                                   ELTOBREL
00141  01  BLANK-LINE                 PIC X(79) VALUE SPACES.           ELTOBREL
00142                                                                   ELTOBREL
00143  01  WS-SPECIAL-AGE.                                              ELTOBREL
00144   10  FILLER                    PIC X(26)                         ELTOBREL
00145       VALUE 'SPECIAL NEWBORN AGE IS:  '.                          ELTOBREL
00146   10  WS-NWBRN-AGE-LIMIT        PIC X(03).                        ELTOBREL
00147   10  FILLER                    PIC X(50) VALUE SPACES.           ELTOBREL
00148 ****************************************************************  ELTOBREL
00149  01  HEADER-LINE-2.                                               ELTOBREL
00150      05  FILLER                  PIC  X(12) VALUE 'SECTION NO: '. ELTOBREL
00151      05  WS-SECT-NO              PIC  9(05) VALUE ZEROS.          ELTOBREL
00152      05  FILLER                  PIC  X(23) VALUE                 ELTOBREL
00153              '       EFFECTIVE DATE: '.                           ELTOBREL
00154      05  WS-EFF-DATE             PIC 99/99/99.                    ELTOBREL
00155      05  FILLER                  PIC  X(29) VALUE                 ELTOBREL
00156              '        FAMILY RELATIONSHIP: '.                     ELTOBREL
00157      05  WS-FAM-REL              PIC  9(01) VALUE ZERO.           ELTOBREL
00158      05  FILLER                  PIC  X(01) VALUE LOW-VALUE.      ELTOBREL
00159                                                                   ELTOBREL
00160  01  HEADER-I-IP-LINE-3.                                          ELTOBREL
00161      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTOBREL
00162      05  FILLER                  PIC  X(43) VALUE                 ELTOBREL
00163              'OBSTETRICAL RELATED INSTITUTIONAL          '.       ELTOBREL
00164      05  FILLER                  PIC  X(18) VALUE LOW-VALUES.     ELTOBREL
00165                                                                   ELTOBREL
00166                                                                   ELTOBREL
00167  01  HEADER-P-IP-LINE-3.                                          ELTOBREL
00168      05  FILLER                  PIC  X(18) VALUE SPACES.         ELTOBREL
00169      05  FILLER                  PIC  X(42) VALUE                 ELTOBREL
00170              'OBSTETRICAL RELATED PROFESSIONAL          '.        ELTOBREL
00171      05  FILLER                  PIC  X(19) VALUE LOW-VALUES.     ELTOBREL
00172                                                                   ELTOBREL
00173                                                                   ELTOBREL
00174                                                                   ELTOBREL
00175  01  WS-CERT-REQ.                                                 ELTOBREL
00176      05  FILLER                  PIC  X(44) VALUE                 ELTOBREL
00177          'CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.          ELTOBREL
00178      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTOBREL
00179                                                                   ELTOBREL
00180  01  WS-SERVICES-RENDERED.                                        ELTOBREL
00181      05  FILLER                  PIC  X(26) VALUE                 ELTOBREL
00182              'SERVICES MAY BE RENDERED: '.                        ELTOBREL
00183      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTOBREL
00184                                                                   ELTOBREL
00185  01  WS-FOLLOWING-BEN.                                            ELTOBREL
00186      05  FILLER                  PIC  X(79) VALUE                 ELTOBREL
00187            'COVERED SERVICES ARE:'.                               ELTOBREL
00188                                                                   ELTOBREL
00189  01  WS-PAY-CONSDR-TEXT1.                                         ELTOBREL
00190      05  FILLER                  PIC X(49)                        ELTOBREL
00191        VALUE  'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT'.    ELTOBREL
00192                                                                   ELTOBREL
00193  01  WS-PAY-CONSDR-TEXT2.                                         ELTOBREL
00194      05  FILLER                  PIC X(45)                        ELTOBREL
00195        VALUE  'OF POCKET EXPENSE FOR PAYMENT CONSIDERATION.'.     ELTOBREL
00196                                                                   ELTOBREL
00197  01  WS-PAYABLE-AS.                                               ELTOBREL
00198      10  FILLER                  PIC  X(45) VALUE                 ELTOBREL
00199              'THESE SERVICES ARE PRICED ACCORDING TO: '.          ELTOBREL
00200      10  FILLER                  PIC  X(34) VALUE LOW-VALUES.     ELTOBREL
00201                                                                   ELTOBREL
00202  01  WS-CONTRACT-RELATED.                                         ELTOBREL
00203      05  FILLER                  PIC  X(48) VALUE                 ELTOBREL
00204              'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS'.  ELTOBREL
00205                                                                   ELTOBREL
00206  01  WS-BASIC.                                                    ELTOBREL
00207      05  WS-BASIC-LIT            PIC  X(07) VALUE                 ELTOBREL
00208              'BASIC: '.                                           ELTOBREL
00209  01  WS-DTL-BASIC-LONG.                                           ELTOBREL
00210      15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.             ELTOBREL
00211      15  FILLER              PIC  X(13) VALUE LOW-VALUES.         ELTOBREL
00212                                                                   ELTOBREL
00213  01  WS-SUPPLEMENTAL.                                             ELTOBREL
00214      05  WS-SUPP-LIT             PIC  X(14) VALUE                 ELTOBREL
00215              'SUPPLEMENTAL: '.                                    ELTOBREL
00216      05  WS-DTL-SUPP-LONG.                                        ELTOBREL
00217          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTOBREL
00218          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTOBREL
00219                                                                   ELTOBREL
00220  01  WS-PVE.                                                      ELTOBREL
00221      05  FILLER                  PIC  X(44) VALUE                 ELTOBREL
00222              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTOBREL
00223      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTOBREL
00224                                                                   ELTOBREL
00225  01  WS-INDICES-PROBLEM.                                          ELTOBREL
00226      05  FILLER                  PIC  X(20) VALUE                 ELTOBREL
00227              'PROBLEM WITH INDICES'.                              ELTOBREL
00228      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTOBREL
00229                                                                   ELTOBREL
00230  01  WS-POSSIBLE-ERROR.                                           ELTOBREL
00231      05  FILLER                   PIC  X(50) VALUE                ELTOBREL
00232              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTOBREL
00233      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTOBREL
00234                                                                   ELTOBREL
00235  01  WS-INVALID-REQ.                                              ELTOBREL
00236      05  FILLER                  PIC  X(37) VALUE                 ELTOBREL
00237              '*** I N V A L I D   R E Q U E S T ***'.             ELTOBREL
00238      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTOBREL
00239                                                                   ELTOBREL
00240  01  WS-SPILLOVER.                                                ELTOBREL
00241      05  FILLER                  PIC  X(10) VALUE                 ELTOBREL
00242              'SPILLOVER '.                                        ELTOBREL
00243                                                                   ELTOBREL
00244  01  WS-OTHER-LITERALS.                                           ELTOBREL
00245    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTOBREL
00246    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTOBREL
00247      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTOBREL
00248 /***************************************************************  ELTOBREL
00249 *  LITERALS FOR OB RELATED.  9/21/95 RGO.                         ELTOBREL
00250 ****************************************************************  ELTOBREL
00251  01  WS-TREAT-RESTRN.                                             ELTOBREL
00252      10  FILLER                  PIC  X(44) VALUE                 ELTOBREL
00253        'THESE SERVICES ARE RESTRICTED AS FOLLOWS:  '.             ELTOBREL
00254                                                                   ELTOBREL
00255  01  WS-EXCEPTION-SCHED.                                          ELTOBREL
00256      05  FILLER                  PIC X(49) VALUE                  ELTOBREL
00257              'BENEFITS ARE PRICED BASED ON EXCEPTION SCHEDULE: '. ELTOBREL
00258      05  FILLER                  PIC X(30) VALUE LOW-VALUES.      ELTOBREL
00259                                                                   ELTOBREL
00260  01  WS-MULT-PRICE.                                               ELTOBREL
00261      05  FILLER                  PIC X(48) VALUE                  ELTOBREL
00262              'PRICING METHOD IF RELATED SURGICAL PROCEDURES PE'.  ELTOBREL
00263      05  FILLER                  PIC X(31) VALUE                  ELTOBREL
00264              'RFORMED DURING SAME ADMISSION: '.                   ELTOBREL
00265 ******************************************************************ELTOBREL
00266 *   LITERALS FROM ELTINPTS                   *********************ELTOBREL
00267 ******************************************************************ELTOBREL
00268  01  WS-PROGRAM-LITERALS.                                         ELTOBREL
00269    05  WS-PERCENT                      PIC X(01) VALUE '%'.       ELTOBREL
00270    05  WS-DAYS                         PIC X(04) VALUE 'DAYS'.    ELTOBREL
00271    05  WS-YES                          PIC X(01) VALUE 'Y'.       ELTOBREL
00272    05  WS-NO                           PIC X(01) VALUE 'N'.       ELTOBREL
00273    05  WS-PAYMNT-BASED.                                           ELTOBREL
00274      10  FILLER                        PIC X(20)  VALUE           ELTOBREL
00275                'PAYMENT IS BASED ON:'.                            ELTOBREL
00276      05  WS-DISPLAY-PAYMNT-BASED-TEXT  PIC X(01).                 ELTOBREL
00277      05  WS-DISPLAY-MAX-VISIT-TEXT     PIC X(01).                 ELTOBREL
00278    05  WS-MAX-VISITS.                                             ELTOBREL
00279      10  FILLER                    PIC X(33) VALUE                ELTOBREL
00280                'THE MAXIMUM NUMBER OF VISITS ARE:'.               ELTOBREL
00281      10  FILLER                    PIC X(46) VALUE LOW-VALUES.    ELTOBREL
00282                                                                   ELTOBREL
00283    05  WS-MAX-DAYS.                                               ELTOBREL
00284      10  FILLER                PIC X(01).                         ELTOBREL
00285      10  WS-DTL-MAX-DAYS       PIC ZZ9.                           ELTOBREL
00286      10  FILLER                PIC X(01).                         ELTOBREL
00287      10  WS-DAYS-LITERAL       PIC X(07) VALUE 'VISITS '.         ELTOBREL
00288      10  FILLER                PIC X(01).                         ELTOBREL
00289      10  WS-DTL-MAX-IND        PIC X(66).                         ELTOBREL
00290    05  WS-MAX-AMT-TEXT.                                           ELTOBREL
00291      10  FILLER                  PIC  X(32) VALUE                 ELTOBREL
00292              'THE MAXIMUM AMOUNT PER VISIT IS '.                  ELTOBREL
00293      10  FILLER                  PIC  X(47) VALUE LOW-VALUES.     ELTOBREL
00294                                                                   ELTOBREL
00295    05  WS-MAXIMUM-AMT.                                            ELTOBREL
00296      10 WS-BASIC-SUPP            PIC  X(16) VALUE SPACES.         ELTOBREL
00297      10 WS-EDIT-MAX-AMT          PIC  ZZ9.99-.                    ELTOBREL
00298                                                                   ELTOBREL
00299 ******************************************************************ELTOBREL
00300 *   LITERALS FROM ELTPSYCH, FOR PROVISION PRICING METHOD FIELDS***ELTOBREL
00301 ******************************************************************ELTOBREL
00302  01  WS-MORE-WORK-FIELDS.                                         ELTOBREL
00303    05  WS-PER-DIEM               PIC $$$$$9.99.                   ELTOBREL
00304    05  WS-ALLOW                  PIC $$$9.99.                     ELTOBREL
00305    05  WS-PRCNT-PERDM-ALLOW      PIC X(9).                        ELTOBREL
00306      TITLE 'LINKAGE SECTION'.                                     ELTOBREL
00307  LINKAGE SECTION.                                                 ELTOBREL
00308  01  DFHCOMMAREA.                                                 ELTOBREL
00309      COPY ELSCOMMC.                                               ELTOBREL
00310 /***************************************************************  ELTOBREL
00311 *  *** CIA  AREA ***                                              ELTOBREL
00312 ****************************************************************  ELTOBREL
00313      COPY ELSCIA2C.                                               ELTOBREL
00314 /***************************************************************  ELTOBREL
00315 /  *** IO PARM AREA ***                                           ELTOBREL
00316 ****************************************************************  ELTOBREL
00317      COPY ELSIOPMC.                                               ELTOBREL
00318 /***************************************************************  ELTOBREL
00319 *  *** KEY AREA     ***                                           ELTOBREL
00320 ****************************************************************  ELTOBREL
00321      COPY ELSKEYSC.                                               ELTOBREL
00322 /***************************************************************  ELTOBREL
00323 *  *** OUTPUT TEXT AREA ***                                       ELTOBREL
00324 ****************************************************************  ELTOBREL
00325      COPY ELSOUTPC.                                               ELTOBREL
00326 /***************************************************************  ELTOBREL
00327 *  *** TOPIC SELECTION AREA ***                                   ELTOBREL
00328 ****************************************************************  ELTOBREL
00329      COPY ELSSSCBC.                                               ELTOBREL
00330 /***************************************************************  ELTOBREL
00331 *  *** CODE MANUAL INTERFACE ***                                  ELTOBREL
00332 ****************************************************************  ELTOBREL
00333      COPY ELSCMIFC.                                               ELTOBREL
00334 /***************************************************************  ELTOBREL
00335 *  *** CODE MANUAL DESCRIPTION AREA ***                           ELTOBREL
00336 ****************************************************************  ELTOBREL
00337      COPY ELSCMDSC.                                               ELTOBREL
00338 /***************************************************************  ELTOBREL
00339 *  *** BENEFIT PROVISION TABLE ***                                ELTOBREL
00340 ****************************************************************  ELTOBREL
00341      COPY ELSPRVNC.                                               ELTOBREL
00342 /***************************************************************  ELTOBREL
00343 *  *** COMPRESSION TEXT WORK-AREA ***                             ELTOBREL
00344 ****************************************************************  ELTOBREL
00345      COPY ELSTCWAC.                                               ELTOBREL
00346 /***************************************************************  ELTOBREL
00347 *    P A Y M E N T   L E V E L   F L D   R E Q U E S T   I N D S  ELTOBREL
00348 ****************************************************************  ELTOBREL
00349      COPY ELSPLGSW.                                               ELTOBREL
00350                                                                   ELTOBREL
00351 /***************************************************************  ELTOBREL
00352 *    B E N E F I T   P R O V I S I O N   T A B L E   O F   F L D SELTOBREL
00353 ****************************************************************  ELTOBREL
00354      COPY ELSPLGTB.                                               ELTOBREL
00355                                                                   ELTOBREL
00356 /***************************************************************  ELTOBREL
00357 *    G R O U P  S P E C I F I C                                   ELTOBREL
00358 ****************************************************************  ELTOBREL
00359  01 GROUP-SPECIFIC-RECORD.                                        ELTOBREL
00360      COPY GCGROUPC.                                               ELTOBREL
00361                                                                   ELTOBREL
00362 /***************************************************************  ELTOBREL
00363 *    C O N T R A C T   R E C O R D                                ELTOBREL
00364 ****************************************************************  ELTOBREL
00365  01 CONTRACT-RECORD.                                              ELTOBREL
00366      COPY GCCONTRC.                                               ELTOBREL
00367 ****************************************************************  ELTOBREL
00368 *                         0000-MAINLINE                           ELTOBREL
00369 *  PARAGRAPH   DESCRIPTION                                        ELTOBREL
00370 * 1. 1000-  ESTABLISH THE POINTERS TO THE VARIOUS LAYOUTS & FILES ELTOBREL
00371 * 2. 1100-  WILL LOOP ON ALL BP'S ON THE GROUP SPECIFIC FILE,     ELTOBREL
00372 *           LOOKING FOR 3 SPECIFIC BP'S.                          ELTOBREL
00373 * 3. 1050-  LOOK AT ALL CONTRACT RECORDS, AND SET A SWITCH TO 'Y' ELTOBREL
00374 *           IF ANY OF THEM HAVE AN LOB = 1,2 OR 3.                ELTOBREL
00375 *           ONLY THOSE GROUP/SECTIONS THAT HAVE THIS WILL DISPLAY ELTOBREL
00376 *           'BASIC' AND/OR 'SUPPLEMENTAL.                         ELTOBREL
00377 *                                                                 ELTOBREL
00378 * 4. 1200-  INST AND PROF. DISPLAY SELECT GROUP SPECIFIC FIELDS   ELTOBREL
00379 *           PRIOR TO DISPLAYING THE BENIFIT PROVISION INFORMATION.ELTOBREL
00380 * 5. 2000-  DISPLAY INSTITUTIONAL BENEFIT PROVISONS               ELTOBREL
00381 * 6. 4000-  DISPLAY PROFESSIONAL BENEFIT PROVISONS.               ELTOBREL
00382 *                                                                 ELTOBREL
00383 * SWITCHES.                                                       ELTOBREL
00384 * WS-CIA    THIS IS THE LINE COUNTER SUBSCRIPT TO THE TABLE THAT  ELTOBREL
00385 *           HOLDS THE DETAIL LINES SENT TO PROGRAM ELUOUTPT.      ELTOBREL
00386 *           IT IS SET TO ZERO AT THE END OF EACH PARAGRAPH.       ELTOBREL
00387 *                                                                 ELTOBREL
00388 ****************************************************************  ELTOBREL
00389      TITLE 'PROCEDURE DIVISION ELTOBREL'.                         ELTOBREL
00390  PROCEDURE DIVISION.                                              ELTOBREL
00391  0000-MAINLINE.                                                   ELTOBREL
00392                                                                   ELTOBREL
00393      PERFORM 1000-INITIALIZATION                                  ELTOBREL
00394         THRU 1000-EXIT.                                           ELTOBREL
00395                                                                   ELTOBREL
00396      PERFORM 1050-CHECK-LOB.                                      ELTOBREL
00397      PERFORM 1100-FIND-BPS-INST.                                  ELTOBREL
00398                                                                   ELTOBREL
00399                                                                   ELTOBREL
00400                                                                   ELTOBREL
00401      IF (FOUND-BP-INST = 'Y' AND                                  ELTOBREL
00402         (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH))         ELTOBREL
00403          PERFORM 1200-DISPLAY-GRP-INST-FIELDS.                    ELTOBREL
00404                                                                   ELTOBREL
00405      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTOBREL
00406          PERFORM 2000-INSTITUTIONAL-IP                            ELTOBREL
00407             THRU 2000-EXIT.                                       ELTOBREL
00408                                                                   ELTOBREL
00409      PERFORM 1100-FIND-BPS-PROF.                                  ELTOBREL
00410      IF (FOUND-BP-PROF = 'Y' AND                                  ELTOBREL
00411         (SSB-PROV-CLASS-PROF    OR  SSB-PROV-CLASS-BOTH))         ELTOBREL
00412          PERFORM 1200-DISPLAY-GRP-PROF-FIELDS.                    ELTOBREL
00413                                                                   ELTOBREL
00414      IF (SSB-PROV-CLASS-PROF    OR  SSB-PROV-CLASS-BOTH)          ELTOBREL
00415          PERFORM 4000-PROFESSIONAL-IP                             ELTOBREL
00416             THRU 4000-EXIT.                                       ELTOBREL
00417                                                                   ELTOBREL
00418      IF NOT SSB-PROV-CLASS-INST    AND                            ELTOBREL
00419         NOT SSB-PROV-CLASS-PROF    AND                            ELTOBREL
00420         NOT SSB-PROV-CLASS-BOTH                                   ELTOBREL
00421          MOVE ' '             TO  COF-FUNCTION                    ELTOBREL
00422          MOVE +0              TO  COF-NBR-HDR-LINES               ELTOBREL
00423          MOVE +2              TO  COF-NBR-DTL-LINES               ELTOBREL
00424          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTOBREL
00425          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTOBREL
00426                           COMMAREA(DFHCOMMAREA)                   ELTOBREL
00427                           END-EXEC.                               ELTOBREL
00428                                                                   ELTOBREL
00429      IF SSB-SERV-CLASS-IP  AND                                    ELTOBREL
00430         SSB-SERV-CLASS-OP  AND                                    ELTOBREL
00431         SSB-SERV-CLASS-BOTH                                       ELTOBREL
00432          MOVE ' '             TO  COF-FUNCTION                    ELTOBREL
00433          MOVE +0              TO  COF-NBR-HDR-LINES               ELTOBREL
00434          MOVE +2              TO  COF-NBR-DTL-LINES               ELTOBREL
00435          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTOBREL
00436          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTOBREL
00437                           COMMAREA(DFHCOMMAREA)                   ELTOBREL
00438                           END-EXEC.                               ELTOBREL
00439                                                                   ELTOBREL
00440      MOVE 'E'   TO  COF-FUNCTION.                                 ELTOBREL
00441      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTOBREL
00442                                                                   ELTOBREL
00443      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
00444                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
00445                     END-EXEC.                                     ELTOBREL
00446                                                                   ELTOBREL
00447      EXEC CICS RETURN END-EXEC.                                   ELTOBREL
00448                                                                   ELTOBREL
00449      GOBACK.                                                      ELTOBREL
00450                                                                   ELTOBREL
00451      TITLE 'INITIALIZATION ELTOBREL'.                             ELTOBREL
00452  1000-INITIALIZATION.                                             ELTOBREL
00453 ****************************************************************  ELTOBREL
00454 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTOBREL
00455 ****************************************************************  ELTOBREL
00456                                                                   ELTOBREL
00457      MOVE '1000'  TO  WS-PARA-ID1.                                ELTOBREL
00458                                                                   ELTOBREL
00459      IF EIBCALEN  NOT =  LENGTH OF DFHCOMMAREA                    ELTOBREL
00460         SET CIA-AB-DFHCOMMAREA TO TRUE                            ELTOBREL
00461         EXEC CICS  ABEND ABCODE('EL01')  END-EXEC.                ELTOBREL
00462                                                                   ELTOBREL
00463      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTOBREL
00464          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTOBREL
00465                                                                   ELTOBREL
00466      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTOBREL
00467      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00468          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTOBREL
00469                                                                   ELTOBREL
00470      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTOBREL
00471      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00472          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTOBREL
00473                                                                   ELTOBREL
00474      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTOBREL
00475      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00476          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTOBREL
00477                                                                   ELTOBREL
00478      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTOBREL
00479      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00480          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTOBREL
00481                                                                   ELTOBREL
00482      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTOBREL
00483      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00484          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTOBREL
00485                                                                   ELTOBREL
00486      SET CIA-ELSGRPSP-DDN TO TRUE.                                ELTOBREL
00487      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00488          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTOBREL
00489                                                                   ELTOBREL
00490                                                                   ELTOBREL
00491      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTOBREL
00492                                                                   ELTOBREL
00493      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTOBREL
00494              (WS-TABLE-MAX-CNT * LENGTH OF PVN-BEN-PROVN-TBL).    ELTOBREL
00495                                                                   ELTOBREL
00496      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTOBREL
00497                                                                   ELTOBREL
00498      SET CIA-STG-GETMAIN  TO TRUE.                                ELTOBREL
00499                                                                   ELTOBREL
00500      EXEC CICS LINK                                               ELTOBREL
00501                PROGRAM('ELUSTGMG')                                ELTOBREL
00502                COMMAREA(DFHCOMMAREA)                              ELTOBREL
00503      END-EXEC.                                                    ELTOBREL
00504                                                                   ELTOBREL
00505      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTOBREL
00506      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00507          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTOBREL
00508                                                                   ELTOBREL
00509  1000-EXIT.  EXIT.                                                ELTOBREL
00510 ****************************************************************  ELTOBREL
00511 * 1050-CHECK-LOB   CHECK ANY OF THE CONTRACT RECORDS TO SEE    *  ELTOBREL
00512 *      IF THE LOB EQUALS '1,','2' OR '3'.                      *  ELTOBREL
00513 *      IF SO, THEN WE WILL DISPLAY 'BASIC:' AND/OR             *  ELTOBREL
00514 *         'SUPPLEMENTAL:' ON THE SCREEN LATER ON.              *  ELTOBREL
00515 ****************************************************************  ELTOBREL
00516  1050-CHECK-LOB.                                                  ELTOBREL
00517                                                                   ELTOBREL
00518      MOVE 'N' TO DISPLAY-BAS-SUP.                                 ELTOBREL
00519                                                                   ELTOBREL
00520      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTOBREL
00521      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00522                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00523                                                                   ELTOBREL
00524      IF CIA-RC-OK                                                 ELTOBREL
00525         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTOBREL
00526             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTOBREL
00527                                                                   ELTOBREL
00528      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTOBREL
00529      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00530                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00531                                                                   ELTOBREL
00532      IF CIA-RC-OK                                                 ELTOBREL
00533         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTOBREL
00534             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTOBREL
00535                                                                   ELTOBREL
00536      SET CIA-ELSCONIS-DDN TO TRUE.                                ELTOBREL
00537      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00538                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00539                                                                   ELTOBREL
00540      IF CIA-RC-OK                                                 ELTOBREL
00541         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTOBREL
00542             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTOBREL
00543                                                                   ELTOBREL
00544      SET CIA-ELSCONPS-DDN TO TRUE.                                ELTOBREL
00545      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00546                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00547                                                                   ELTOBREL
00548      IF CIA-RC-OK                                                 ELTOBREL
00549         IF GCT-L-O-B = '1' OR '2' OR '3'                          ELTOBREL
00550             MOVE 'Y' TO DISPLAY-BAS-SUP.                          ELTOBREL
00551                                                                   ELTOBREL
00552 ****************************************************************  ELTOBREL
00553 * 1100-FIND-BPS-INST.  CHECK TO SEE IF THE INSTITUTIONAL BP    *  ELTOBREL
00554 *      IS ON THE CONTRACT RECORD.                              *  ELTOBREL
00555 ****************************************************************  ELTOBREL
00556  1100-FIND-BPS-INST.                                              ELTOBREL
00557                                                                   ELTOBREL
00558      SET CIA-ELSCONIB-DDN TO TRUE.                                ELTOBREL
00559      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00560                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00561                                                                   ELTOBREL
00562      MOVE 'N' TO FOUND-BP-INST.                                   ELTOBREL
00563                                                                   ELTOBREL
00564      IF CIA-RC-OK                                                 ELTOBREL
00565          PERFORM 1150-SEARCH-INST                                 ELTOBREL
00566              VARYING GCT-INDEX FROM 1 BY 1                        ELTOBREL
00567              UNTIL GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS       ELTOBREL
00568                    OR FOUND-BP-INST = 'Y'.                        ELTOBREL
00569                                                                   ELTOBREL
00570      IF FOUND-BP-INST = 'N'                                       ELTOBREL
00571         SET CIA-ELSCONIS-DDN TO TRUE                              ELTOBREL
00572         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBREL
00573                         ADDRESS OF CONTRACT-RECORD                ELTOBREL
00574                                                                   ELTOBREL
00575         IF CIA-RC-OK                                              ELTOBREL
00576             PERFORM 1150-SEARCH-INST                              ELTOBREL
00577                 VARYING GCT-INDEX FROM 1 BY 1                     ELTOBREL
00578                 UNTIL GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS    ELTOBREL
00579                       OR FOUND-BP-INST = 'Y'.                     ELTOBREL
00580 ****************************************************************  ELTOBREL
00581 * 1100-FIND-BPS-PROF.  CHECK TO SEE IF THE PROFESSIONAL  BP    *  ELTOBREL
00582 *      IS ON THE CONTRACT RECORD.                              *  ELTOBREL
00583 ****************************************************************  ELTOBREL
00584  1100-FIND-BPS-PROF.                                              ELTOBREL
00585                                                                   ELTOBREL
00586      SET CIA-ELSCONPB-DDN TO TRUE.                                ELTOBREL
00587      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
00588                      ADDRESS OF CONTRACT-RECORD.                  ELTOBREL
00589                                                                   ELTOBREL
00590      MOVE 'N' TO FOUND-BP-PROF.                                   ELTOBREL
00591                                                                   ELTOBREL
00592      IF CIA-RC-OK                                                 ELTOBREL
00593          PERFORM 1150-SEARCH-PROF                                 ELTOBREL
00594              VARYING GCT-INDEX FROM 1 BY 1                        ELTOBREL
00595              UNTIL GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS       ELTOBREL
00596                    OR FOUND-BP-PROF = 'Y'.                        ELTOBREL
00597                                                                   ELTOBREL
00598      IF FOUND-BP-PROF = 'N'                                       ELTOBREL
00599         SET CIA-ELSCONPS-DDN TO TRUE                              ELTOBREL
00600         CALL 'ELUSETAD' USING DFHCOMMAREA                         ELTOBREL
00601                         ADDRESS OF CONTRACT-RECORD                ELTOBREL
00602                                                                   ELTOBREL
00603         IF CIA-RC-OK                                              ELTOBREL
00604             PERFORM 1150-SEARCH-PROF                              ELTOBREL
00605                 VARYING GCT-INDEX FROM 1 BY 1                     ELTOBREL
00606                 UNTIL GCT-INDEX > GCT-COUNT-BEN-PROVN-POINTERS    ELTOBREL
00607                       OR FOUND-BP-PROF = 'Y'.                     ELTOBREL
00608 ****************************************************************  ELTOBREL
00609 * 1150-SEARCH-INST                                             *  ELTOBREL
00610 *                                                              *  ELTOBREL
00611 ****************************************************************  ELTOBREL
00612  1150-SEARCH-INST.                                                ELTOBREL
00613                                                                   ELTOBREL
00614      IF GCT-BEN-PROVN-ID(GCT-INDEX) = 'NRB  A'                    ELTOBREL
00615         AND                                                       ELTOBREL
00616         GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) > 0                     ELTOBREL
00617         MOVE 'Y' TO FOUND-BP-INST.                                ELTOBREL
00618                                                                   ELTOBREL
00619 ****************************************************************  ELTOBREL
00620 * 1150-SEARCH-PROF                                             *  ELTOBREL
00621 *                                                              *  ELTOBREL
00622 ****************************************************************  ELTOBREL
00623  1150-SEARCH-PROF.                                                ELTOBREL
00624                                                                   ELTOBREL
00625      IF (GCT-BEN-PROVN-ID(GCT-INDEX) = 'NCRC C' OR 'NNBC E')      ELTOBREL
00626         AND                                                       ELTOBREL
00627         GCT-BEN-PROVN-SLOT-NO (GCT-INDEX) > 0                     ELTOBREL
00628         MOVE 'Y' TO FOUND-BP-PROF.                                ELTOBREL
00629                                                                   ELTOBREL
00630 ****************************************************************  ELTOBREL
00631 * 1200-DISPLAY-GRP-INST-FIELDS.  INSTITUTIONAL                 *  ELTOBREL
00632 *      DISPLAY THE RELATED GROUP SPECIFIC FIELDS.              *  ELTOBREL
00633 *      FOR INSTITUTIONAL, DISPLAY 'BC- ' FIELDS.               *  ELTOBREL
00634 *      FOR COMPREHENSIVE MAJOR MEDICAL (LOB =4) ON  CONTRACT,  *  ELTOBREL
00635 *         DO NOT DISPLAY BASIC VS. SUPPLEMENTAL, AND DO NOT    *  ELTOBREL
00636 *         DISPLAY THE 'MM- ' FIELDS.  AUGGIE  STATES THEY HAVE *  ELTOBREL
00637 *         NO MEANING FOR THIS KIND OF CONTRACT. 11/16/95       *  ELTOBREL
00638 ****************************************************************  ELTOBREL
00639  1200-DISPLAY-GRP-INST-FIELDS.                                    ELTOBREL
00640                                                                   ELTOBREL
00641         MOVE 2 TO WS-CIA.                                         ELTOBREL
00642                                                                   ELTOBREL
00643         IF GCG-HEALTHY-EXPECTATIONS-IND > '00'                    ELTOBREL
00644            AND NOT = SPACES                                       ELTOBREL
00645            MOVE WS-HEALTHY-EXP-YES TO                             ELTOBREL
00646                 COF-DTL-LINE (WS-CIA)                             ELTOBREL
00647         ELSE                                                      ELTOBREL
00648              MOVE WS-HEALTHY-EXP-NO TO COF-DTL-LINE (WS-CIA).     ELTOBREL
00649                                                                   ELTOBREL
00650         IF GCG-NRM-NWBRN-BILG-IND > '0'                           ELTOBREL
00651             ADD +2 TO WS-CIA                                      ELTOBREL
00652             MOVE WS-RULE-BILL-NEWBORN-CHGS TO                     ELTOBREL
00653                  COF-DTL-LINE (WS-CIA)                            ELTOBREL
00654             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00655             MOVE 'NRM-NWBRN-BILG-IND' TO CMF-ELEMENT-SYSTEM-NAME  ELTOBREL
00656             MOVE GCG-NRM-NWBRN-BILG-IND TO                        ELTOBREL
00657                  CMF-CODE-VALUE                                   ELTOBREL
00658             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00659             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00660             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00661                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00662                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00663                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00664                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00665             ELSE                                                  ELTOBREL
00666                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00667             END-IF                                                ELTOBREL
00668                                                                   ELTOBREL
00669         END-IF.                                                   ELTOBREL
00670                                                                   ELTOBREL
00671         IF GCG-BC-NRM-NWBRN-ELIG-IND > '0'                        ELTOBREL
00672             ADD 2 TO WS-CIA                                       ELTOBREL
00673             MOVE WS-NORMAL-NEWBORN-CHGS TO COF-DTL-LINE (WS-CIA)  ELTOBREL
00674             IF DISPLAY-BAS-SUP = 'Y'                              ELTOBREL
00675                 ADD 1 TO WS-CIA                                   ELTOBREL
00676                 MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)        ELTOBREL
00677             END-IF                                                ELTOBREL
00678             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00679             MOVE 'BC-NRM-NWBRN-ELIG-IND'                          ELTOBREL
00680                                    TO CMF-ELEMENT-SYSTEM-NAME     ELTOBREL
00681             MOVE GCG-BC-NRM-NWBRN-ELIG-IND TO                     ELTOBREL
00682                  CMF-CODE-VALUE                                   ELTOBREL
00683                                                                   ELTOBREL
00684             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00685             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00686             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00687                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00688                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00689                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00690                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00691             ELSE                                                  ELTOBREL
00692                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00693             END-IF                                                ELTOBREL
00694                                                                   ELTOBREL
00695         END-IF.                                                   ELTOBREL
00696                                                                   ELTOBREL
00697         IF (GCG-MM-NRM-NWBRN-ELIG-IND > '0'                       ELTOBREL
00698                   AND DISPLAY-BAS-SUP = 'Y')                      ELTOBREL
00699             ADD 1 TO WS-CIA                                       ELTOBREL
00700             IF GCG-BC-NRM-NWBRN-ELIG-IND < '1'                    ELTOBREL
00701                 MOVE WS-NORMAL-NEWBORN-CHGS                       ELTOBREL
00702                         TO COF-DTL-LINE (WS-CIA)                  ELTOBREL
00703                 ADD 1 TO WS-CIA                                   ELTOBREL
00704             END-IF                                                ELTOBREL
00705             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTOBREL
00706                                                                   ELTOBREL
00707             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00708             MOVE 'MM-NRM-NWBRN-ELIG-IND'                          ELTOBREL
00709                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTOBREL
00710             MOVE GCG-MM-NRM-NWBRN-ELIG-IND TO                     ELTOBREL
00711                  CMF-CODE-VALUE                                   ELTOBREL
00712                                                                   ELTOBREL
00713             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00714             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00715             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00716                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00717                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00718                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00719                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00720             ELSE                                                  ELTOBREL
00721                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00722             END-IF                                                ELTOBREL
00723                                                                   ELTOBREL
00724         END-IF.                                                   ELTOBREL
00725                                                                   ELTOBREL
00726         IF GCG-BC-SPCL-NWBRN-COVERAGE > '0'                       ELTOBREL
00727             ADD 2 TO WS-CIA                                       ELTOBREL
00728             MOVE WS-SPECIAL-NEWBORN-COV TO COF-DTL-LINE (WS-CIA)  ELTOBREL
00729             IF DISPLAY-BAS-SUP = 'Y'                              ELTOBREL
00730                 ADD 1 TO WS-CIA                                   ELTOBREL
00731                 MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)        ELTOBREL
00732             END-IF                                                ELTOBREL
00733                                                                   ELTOBREL
00734             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00735             MOVE 'BC-SPCL-NWBRN-COVERAGE'                         ELTOBREL
00736                                   TO CMF-ELEMENT-SYSTEM-NAME      ELTOBREL
00737             MOVE GCG-BC-SPCL-NWBRN-COVERAGE TO                    ELTOBREL
00738                  CMF-CODE-VALUE                                   ELTOBREL
00739             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00740                                                                   ELTOBREL
00741             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00742             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00743                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00744                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00745                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00746                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00747             ELSE                                                  ELTOBREL
00748                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00749             END-IF                                                ELTOBREL
00750                                                                   ELTOBREL
00751             IF GCG-BC-SPCL-NWBRN-COVERAGE = '5'                   ELTOBREL
00752                MOVE GCG-NWBRN-AGE-LIMIT TO WS-NWBRN-AGE-LIMIT     ELTOBREL
00753                ADD 1 TO WS-CIA                                    ELTOBREL
00754                MOVE WS-SPECIAL-AGE TO COF-DTL-LINE (WS-CIA)       ELTOBREL
00755             END-IF                                                ELTOBREL
00756         END-IF.                                                   ELTOBREL
00757         IF (GCG-MM-SPCL-NWBRN-COVERAGE > '0'                      ELTOBREL
00758                    AND DISPLAY-BAS-SUP = 'Y')                     ELTOBREL
00759             IF GCG-BC-SPCL-NWBRN-COVERAGE < '1'                   ELTOBREL
00760                 ADD 2 TO WS-CIA                                   ELTOBREL
00761                 MOVE WS-SPECIAL-NEWBORN-COV                       ELTOBREL
00762                      TO COF-DTL-LINE (WS-CIA)                     ELTOBREL
00763             END-IF                                                ELTOBREL
00764             ADD 1 TO WS-CIA                                       ELTOBREL
00765             MOVE WS-SUPP-LIT TO COF-DTL-LINE(WS-CIA)              ELTOBREL
00766                                                                   ELTOBREL
00767             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00768             MOVE 'MM-SPCL-NWBRN-COVERAGE'                         ELTOBREL
00769                                  TO CMF-ELEMENT-SYSTEM-NAME       ELTOBREL
00770             MOVE GCG-MM-SPCL-NWBRN-COVERAGE TO                    ELTOBREL
00771                  CMF-CODE-VALUE                                   ELTOBREL
00772                                                                   ELTOBREL
00773             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00774             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00775             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00776                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00777                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00778                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00779                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00780             ELSE                                                  ELTOBREL
00781                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00782             END-IF                                                ELTOBREL
00783                                                                   ELTOBREL
00784             IF GCG-MM-SPCL-NWBRN-COVERAGE = '5'                   ELTOBREL
00785                MOVE GCG-NWBRN-AGE-LIMIT TO WS-NWBRN-AGE-LIMIT     ELTOBREL
00786                ADD 1 TO WS-CIA                                    ELTOBREL
00787                MOVE WS-SPECIAL-AGE TO COF-DTL-LINE (WS-CIA)       ELTOBREL
00788             END-IF                                                ELTOBREL
00789         END-IF.                                                   ELTOBREL
00790                                                                   ELTOBREL
00791        MOVE 'N' TO CALL-ELUOUTPT-IND.                             ELTOBREL
00792        MOVE WS-CIA TO COF-NBR-DTL-LINES.                          ELTOBREL
00793        MOVE 'P' TO COF-FUNCTION.                                  ELTOBREL
00794        MOVE HEADER-I-IP-LINE-3 TO COF-HDR-LINE (2).               ELTOBREL
00795        MOVE 2 TO COF-NBR-HDR-LINES.                               ELTOBREL
00796        EXEC CICS LINK PROGRAM ('ELUOUTPT')                        ELTOBREL
00797                       COMMAREA (DFHCOMMAREA)                      ELTOBREL
00798                       END-EXEC.                                   ELTOBREL
00799                                                                   ELTOBREL
00800 ****************************************************************  ELTOBREL
00801 * 1200-DISPLAY-GRP-PROF-FIELDS.  PROFESSIONAL                  *  ELTOBREL
00802 *      DISPLAY THE RELATED GROUP SPECIFIC FIELDS.              *  ELTOBREL
00803 *      FOR PROFESSIONAL, DISPLAY 'BS- ' FIELDS.                *  ELTOBREL
00804 *      FOR COMPREHENSIVE MAJOR MEDICAL (LOB =4) ON  CONTRACT,  *  ELTOBREL
00805 *         DO NOT DISPLAY BASIC VS. SUPPLEMENTAL, AND DO NOT    *  ELTOBREL
00806 *         DISPLAY THE 'MM- ' FIELDS.  AUGGIES STATES THEY HAVE *  ELTOBREL
00807 *         NO MEANING FOR THIS KIND OF CONTRACT.                *  ELTOBREL
00808 ****************************************************************  ELTOBREL
00809  1200-DISPLAY-GRP-PROF-FIELDS.                                    ELTOBREL
00810                                                                   ELTOBREL
00811         MOVE 2 TO WS-CIA.                                         ELTOBREL
00812                                                                   ELTOBREL
00813         IF GCG-HEALTHY-EXPECTATIONS-IND >  '00'                   ELTOBREL
00814            AND NOT = SPACES                                       ELTOBREL
00815            MOVE WS-HEALTHY-EXP-YES TO                             ELTOBREL
00816                 COF-DTL-LINE (WS-CIA)                             ELTOBREL
00817         ELSE                                                      ELTOBREL
00818              MOVE WS-HEALTHY-EXP-NO TO COF-DTL-LINE (WS-CIA).     ELTOBREL
00819                                                                   ELTOBREL
00820         IF GCG-NRM-NWBRN-BILG-IND > '0'                           ELTOBREL
00821             ADD +2 TO WS-CIA                                      ELTOBREL
00822             MOVE WS-RULE-BILL-NEWBORN-CHGS TO                     ELTOBREL
00823                  COF-DTL-LINE (WS-CIA)                            ELTOBREL
00824             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00825             MOVE 'NRM-NWBRN-BILG-IND' TO CMF-ELEMENT-SYSTEM-NAME  ELTOBREL
00826             MOVE GCG-NRM-NWBRN-BILG-IND TO                        ELTOBREL
00827                  CMF-CODE-VALUE                                   ELTOBREL
00828             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00829             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00830             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00831                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00832                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00833                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00834                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00835             ELSE                                                  ELTOBREL
00836                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00837             END-IF                                                ELTOBREL
00838                                                                   ELTOBREL
00839         END-IF.                                                   ELTOBREL
00840                                                                   ELTOBREL
00841         IF GCG-BS-NRM-NWBRN-ELIG-IND > '0'                        ELTOBREL
00842             ADD 2 TO WS-CIA                                       ELTOBREL
00843             MOVE WS-NORMAL-NEWBORN-CHGS TO COF-DTL-LINE (WS-CIA)  ELTOBREL
00844             IF DISPLAY-BAS-SUP = 'Y'                              ELTOBREL
00845                 ADD 1 TO WS-CIA                                   ELTOBREL
00846                 MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)        ELTOBREL
00847             END-IF                                                ELTOBREL
00848             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00849             MOVE 'BS-NRM-NWBRN-ELIG-IND'                          ELTOBREL
00850                                    TO CMF-ELEMENT-SYSTEM-NAME     ELTOBREL
00851             MOVE GCG-BS-NRM-NWBRN-ELIG-IND TO                     ELTOBREL
00852                  CMF-CODE-VALUE                                   ELTOBREL
00853                                                                   ELTOBREL
00854             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00855             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00856             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00857                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00858                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00859                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00860                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00861             ELSE                                                  ELTOBREL
00862                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00863             END-IF                                                ELTOBREL
00864                                                                   ELTOBREL
00865         END-IF.                                                   ELTOBREL
00866                                                                   ELTOBREL
00867         IF (GCG-MM-NRM-NWBRN-ELIG-IND > '0'                       ELTOBREL
00868                   AND DISPLAY-BAS-SUP = 'Y')                      ELTOBREL
00869             ADD 1 TO WS-CIA                                       ELTOBREL
00870             IF GCG-BS-NRM-NWBRN-ELIG-IND < '1'                    ELTOBREL
00871                 MOVE WS-NORMAL-NEWBORN-CHGS                       ELTOBREL
00872                         TO COF-DTL-LINE (WS-CIA)                  ELTOBREL
00873                 ADD 1 TO WS-CIA                                   ELTOBREL
00874             END-IF                                                ELTOBREL
00875             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTOBREL
00876                                                                   ELTOBREL
00877             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00878             MOVE 'MM-NRM-NWBRN-ELIG-IND'                          ELTOBREL
00879                                 TO CMF-ELEMENT-SYSTEM-NAME        ELTOBREL
00880             MOVE GCG-MM-NRM-NWBRN-ELIG-IND TO                     ELTOBREL
00881                  CMF-CODE-VALUE                                   ELTOBREL
00882                                                                   ELTOBREL
00883             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00884             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00885             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00886                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00887                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00888                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00889                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00890             ELSE                                                  ELTOBREL
00891                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00892             END-IF                                                ELTOBREL
00893                                                                   ELTOBREL
00894         END-IF.                                                   ELTOBREL
00895                                                                   ELTOBREL
00896         IF GCG-BS-SPCL-NWBRN-COVERAGE > '0'                       ELTOBREL
00897             ADD 2 TO WS-CIA                                       ELTOBREL
00898             MOVE WS-SPECIAL-NEWBORN-COV TO COF-DTL-LINE (WS-CIA)  ELTOBREL
00899             IF DISPLAY-BAS-SUP = 'Y'                              ELTOBREL
00900                 ADD 1 TO WS-CIA                                   ELTOBREL
00901                 MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)        ELTOBREL
00902             END-IF                                                ELTOBREL
00903                                                                   ELTOBREL
00904             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00905             MOVE 'BS-SPCL-NWBRN-COVERAGE'                         ELTOBREL
00906                                   TO CMF-ELEMENT-SYSTEM-NAME      ELTOBREL
00907             MOVE GCG-BS-SPCL-NWBRN-COVERAGE TO                    ELTOBREL
00908                  CMF-CODE-VALUE                                   ELTOBREL
00909                                                                   ELTOBREL
00910             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00911             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00912             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00913                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00914                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00915                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00916                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00917             ELSE                                                  ELTOBREL
00918                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00919             END-IF                                                ELTOBREL
00920                                                                   ELTOBREL
00921             IF GCG-BS-SPCL-NWBRN-COVERAGE = '5'                   ELTOBREL
00922                MOVE GCG-NWBRN-AGE-LIMIT TO                        ELTOBREL
00923                     WS-NWBRN-AGE-LIMIT                            ELTOBREL
00924                ADD +1 TO WS-CIA                                   ELTOBREL
00925                MOVE WS-SPECIAL-AGE TO COF-DTL-LINE (WS-CIA)       ELTOBREL
00926             END-IF                                                ELTOBREL
00927         END-IF.                                                   ELTOBREL
00928         IF (GCG-MM-SPCL-NWBRN-COVERAGE > '0'                      ELTOBREL
00929                   AND DISPLAY-BAS-SUP = 'Y')                      ELTOBREL
00930             IF GCG-BS-SPCL-NWBRN-COVERAGE = '0' OR LOW-VALUES     ELTOBREL
00931                ADD 2 TO WS-CIA                                    ELTOBREL
00932                MOVE WS-SPECIAL-NEWBORN-COV                        ELTOBREL
00933                                 TO COF-DTL-LINE (WS-CIA)          ELTOBREL
00934             END-IF                                                ELTOBREL
00935                                                                   ELTOBREL
00936             ADD 1 TO WS-CIA                                       ELTOBREL
00937             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTOBREL
00938             MOVE 'GROUP' TO CMF-RECORD-PREFIX                     ELTOBREL
00939             MOVE 'MM-SPCL-NWBRN-COVERAGE'                         ELTOBREL
00940                                  TO CMF-ELEMENT-SYSTEM-NAME       ELTOBREL
00941             MOVE GCG-MM-SPCL-NWBRN-COVERAGE TO                    ELTOBREL
00942                  CMF-CODE-VALUE                                   ELTOBREL
00943                                                                   ELTOBREL
00944             PERFORM 9300-CALL-CODES-MANUAL                        ELTOBREL
00945             MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79           ELTOBREL
00946             IF FIRST-CHAR-SHOW-AS-IS                              ELTOBREL
00947                MOVE FIRST-THE-REST TO CMF-DESCR-LINE(1)           ELTOBREL
00948                PERFORM 9400-MOVE-TO-COFDTL                        ELTOBREL
00949                      VARYING WS-SUB-CMF FROM 1 BY 1               ELTOBREL
00950                      UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES       ELTOBREL
00951             ELSE                                                  ELTOBREL
00952                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
00953             END-IF                                                ELTOBREL
00954                                                                   ELTOBREL
00955             IF GCG-MM-SPCL-NWBRN-COVERAGE = '5'                   ELTOBREL
00956                ADD +1 TO WS-CIA                                   ELTOBREL
00957                MOVE GCG-NWBRN-AGE-LIMIT TO                        ELTOBREL
00958                     WS-NWBRN-AGE-LIMIT                            ELTOBREL
00959                MOVE WS-SPECIAL-AGE TO COF-DTL-LINE (WS-CIA)       ELTOBREL
00960             END-IF                                                ELTOBREL
00961         END-IF.                                                   ELTOBREL
00962                                                                   ELTOBREL
00963        MOVE 'N' TO CALL-ELUOUTPT-IND.                             ELTOBREL
00964        MOVE WS-CIA TO COF-NBR-DTL-LINES.                          ELTOBREL
00965        MOVE 'P' TO COF-FUNCTION.                                  ELTOBREL
00966        MOVE HEADER-P-IP-LINE-3 TO COF-HDR-LINE (2).               ELTOBREL
00967        MOVE 2 TO COF-NBR-HDR-LINES.                               ELTOBREL
00968        EXEC CICS LINK PROGRAM ('ELUOUTPT')                        ELTOBREL
00969                       COMMAREA (DFHCOMMAREA)                      ELTOBREL
00970                       END-EXEC.                                   ELTOBREL
00971                                                                   ELTOBREL
00972 ****************************************************************  ELTOBREL
00973 *   OB RELATED  INSTITUTIONAL (BUT NOT INPATIENT)  PROCESSING     ELTOBREL
00974 ****************************************************************  ELTOBREL
00975      TITLE 'INISTUTIONAL INPATIENT   ELTOBREL'.                   ELTOBREL
00976  2000-INSTITUTIONAL-IP.                                           ELTOBREL
00977                                                                   ELTOBREL
00978      MOVE '2000'  TO  WS-PARA-ID1.                                ELTOBREL
00979      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTOBREL
00980                                                                   ELTOBREL
00981      MOVE ' ' TO COF-FUNCTION.                                    ELTOBREL
00982                                                                   ELTOBREL
00983                                                                   ELTOBREL
00984      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTOBREL
00985                                                                   ELTOBREL
00986      PERFORM 2010-MOVE-IN-INST-IP-TABS                            ELTOBREL
00987         THRU 2010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTOBREL
00988                        UNTIL   WS-SUB  >     WS-INST-IP-CNT.      ELTOBREL
00989                                                                   ELTOBREL
00990      PERFORM 2020-CALL-COVERAGE                                   ELTOBREL
00991         THRU 2020-EXIT.                                           ELTOBREL
00992                                                                   ELTOBREL
00993      IF PVN-COVG-NONE                                             ELTOBREL
00994          GO TO 2000-EXIT.                                         ELTOBREL
00995                                                                   ELTOBREL
00996                                                                   ELTOBREL
00997      PERFORM 2030-FIND-FIRST-NONZERO                              ELTOBREL
00998         THRU 2030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTOBREL
00999                        UNTIL   WS-SUB  > WS-INST-IP-CNT.          ELTOBREL
01000                                                                   ELTOBREL
01001  2000-EXIT.  EXIT.                                                ELTOBREL
01002 /                                                                 ELTOBREL
01003  2010-MOVE-IN-INST-IP-TABS.                                       ELTOBREL
01004      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01005      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTOBREL
01006                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTOBREL
01007                                                                   ELTOBREL
01008      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTOBREL
01009                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTOBREL
01010                                                                   ELTOBREL
01011  2010-EXIT.  EXIT.                                                ELTOBREL
01012      SKIP3                                                        ELTOBREL
01013  2020-CALL-COVERAGE.                                              ELTOBREL
01014      MOVE '2020'  TO  WS-PARA-ID1.                                ELTOBREL
01015                                                                   ELTOBREL
01016      MOVE 'OBSTETRICAL RELATED SERVICES' TO  SSB-TOPIC-PHRASE.    ELTOBREL
01017                                                                   ELTOBREL
01018      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTOBREL
01019                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01020                     END-EXEC.                                     ELTOBREL
01021                                                                   ELTOBREL
01022      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
01023                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01024                     END-EXEC.                                     ELTOBREL
01025                                                                   ELTOBREL
01026      IF PVN-COVG-NONE                                             ELTOBREL
01027          GO TO 2020-EXIT.                                         ELTOBREL
01028                                                                   ELTOBREL
01029      MOVE +1  TO  WS-CIA.                                         ELTOBREL
01030                                                                   ELTOBREL
01031      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTOBREL
01032      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
01033          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTOBREL
01034                                                                   ELTOBREL
01035      INITIALIZE  PLS-PAYMENT-LEVEL-SWITCHES.                      ELTOBREL
01036                                                                   ELTOBREL
01037      MOVE '1' TO   PSP-PROVN-PRICING-METHD,                       ELTOBREL
01038                    PSP-TREAT-RESTRN-IND,                          ELTOBREL
01039                    PSP-TRANSF-OTHER-RESP-IND,                     ELTOBREL
01040                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTOBREL
01041                    PSP-SPILL-OVER-DED-APL-IND,                    ELTOBREL
01042                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTOBREL
01043                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTOBREL
01044                    PSP-PLACE-TREAT-ELIG-IND,                      ELTOBREL
01045                    PSP-CERTFN-REQRM-IND,                          ELTOBREL
01046                                                                   ELTOBREL
01047                    PSA-FLAT-RATE-PDM-AMT,                         ELTOBREL
01048                    PSA-ADDN-ALLOW-AMT-PER-DAY.                    ELTOBREL
01049                                                                   ELTOBREL
01050      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTOBREL
01051                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01052                     END-EXEC.                                     ELTOBREL
01053                                                                   ELTOBREL
01054      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTOBREL
01055      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
01056          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTOBREL
01057                                                                   ELTOBREL
01058  2020-EXIT.  EXIT.                                                ELTOBREL
01059      SKIP3                                                        ELTOBREL
01060  2030-FIND-FIRST-NONZERO.                                         ELTOBREL
01061      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01062                                                                   ELTOBREL
01063      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTOBREL
01064          NEXT SENTENCE                                            ELTOBREL
01065      ELSE                                                         ELTOBREL
01066          PERFORM 2100-BUILD-SCREEN-LINES                          ELTOBREL
01067             THRU 2100-EXIT.                                       ELTOBREL
01068                                                                   ELTOBREL
01069  2030-EXIT.  EXIT.                                                ELTOBREL
01070 /**************************************************************** ELTOBREL
01071 *  2100-BUILD-SCREEN-LINES       OB RELATED                     * ELTOBREL
01072 ***************************************************************** ELTOBREL
01073                                                                   ELTOBREL
01074  2100-BUILD-SCREEN-LINES.                                         ELTOBREL
01075      MOVE '2100'  TO  WS-PARA-ID1.                                ELTOBREL
01076      SET PLT-INDEX1  TO                                           ELTOBREL
01077              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTOBREL
01078                                                                   ELTOBREL
01079      IF WS-NOT-FIRST-TIME                                         ELTOBREL
01080         MOVE 'P'    TO COF-FUNCTION                               ELTOBREL
01081         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTOBREL
01082         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTOBREL
01083                          COMMAREA(DFHCOMMAREA)                    ELTOBREL
01084         END-EXEC                                                  ELTOBREL
01085      ELSE                                                         ELTOBREL
01086        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTOBREL
01087                                                                   ELTOBREL
01088      MOVE  +1  TO  WS-CIA.                                        ELTOBREL
01089                                                                   ELTOBREL
01090      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTOBREL
01091          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTOBREL
01092              SET PLT-INDEX2  TO  2                                ELTOBREL
01093          ELSE                                                     ELTOBREL
01094              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTOBREL
01095              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTOBREL
01096              GO TO 2100-EXIT                                      ELTOBREL
01097      ELSE                                                         ELTOBREL
01098          SET PLT-INDEX2  TO  1.                                   ELTOBREL
01099                                                                   ELTOBREL
01100      MOVE WS-INST-IP-CNT TO WS-LIST-BP-CNT.                       ELTOBREL
01101      PERFORM 6105-LIST-BEN-PROV                                   ELTOBREL
01102         THRU 6105-EXIT.                                           ELTOBREL
01103 *  **********************************************************     ELTOBREL
01104 *    THESE ARE THE VARIOUS FIELDS FROM THE BENIFIT PROVISONS      ELTOBREL
01105 *    FILE.                                                        ELTOBREL
01106 *  **********************************************************     ELTOBREL
01107                                                                   ELTOBREL
01108      PERFORM 6120-PRIC-METH                                       ELTOBREL
01109         THRU 6120-EXIT.                                           ELTOBREL
01110                                                                   ELTOBREL
01111      PERFORM 6110-PLACE-OF-TREATMENT                              ELTOBREL
01112         THRU 6110-EXIT.                                           ELTOBREL
01113                                                                   ELTOBREL
01114      PERFORM 6130-TREAT-RESTRN                                    ELTOBREL
01115         THRU 6130-EXIT.                                           ELTOBREL
01116                                                                   ELTOBREL
01117      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTOBREL
01118         THRU 6160-EXIT.                                           ELTOBREL
01119                                                                   ELTOBREL
01120      PERFORM 6115-CERT-REQ-IND                                    ELTOBREL
01121         THRU 6115-EXIT.                                           ELTOBREL
01122                                                                   ELTOBREL
01123      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTOBREL
01124         THRU 6165-EXIT.                                           ELTOBREL
01125                                                                   ELTOBREL
01126      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTOBREL
01127                                                                   ELTOBREL
01128  2100-EXIT.  EXIT.                                                ELTOBREL
01129                                                                   ELTOBREL
01130 ****************************************************************  ELTOBREL
01131 *          BENEFIT SCOPE ID                                    *  ELTOBREL
01132 ****************************************************************  ELTOBREL
01133  3300-BENEFIT-SCOPE-ID.                                           ELTOBREL
01134      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01135                                                                   ELTOBREL
01136      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01137         SET  PLT-INDEX2      TO  1                                ELTOBREL
01138         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTOBREL
01139                NOT = '0000' AND NOT = '00  '                      ELTOBREL
01140                AND NOT = LOW-VALUES                               ELTOBREL
01141            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTOBREL
01142                                                                   ELTOBREL
01143      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01144         SET PLT-INDEX2       TO 2                                 ELTOBREL
01145         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTOBREL
01146                NOT = '0000' AND NOT = '00  '                      ELTOBREL
01147                AND NOT = LOW-VALUES                               ELTOBREL
01148            MOVE WS-YES TO WS-DISPLAY-PAYMNT-BASED-TEXT.           ELTOBREL
01149                                                                   ELTOBREL
01150      IF WS-DISPLAY-PAYMNT-BASED-TEXT = WS-YES                     ELTOBREL
01151         ADD  +1              TO  WS-CIA                           ELTOBREL
01152         MOVE WS-PAYMNT-BASED TO  COF-DTL-LINE(WS-CIA).            ELTOBREL
01153                                                                   ELTOBREL
01154      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01155         SET  PLT-INDEX2       TO  1                               ELTOBREL
01156         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTOBREL
01157                NOT = '0000' AND NOT = '00  '                      ELTOBREL
01158                AND NOT = LOW-VALUES                               ELTOBREL
01159            MOVE 'BPD'           TO  CMF-RECORD-PREFIX             ELTOBREL
01160            MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTOBREL
01161                                 TO  CMF-CODE-VALUE                ELTOBREL
01162            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTOBREL
01163            PERFORM 9300-CALL-CODES-MANUAL                         ELTOBREL
01164            MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79            ELTOBREL
01165                                                                   ELTOBREL
01166            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01167                ADD 1 TO WS-CIA                                    ELTOBREL
01168                MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)         ELTOBREL
01169            END-IF                                                 ELTOBREL
01170                                                                   ELTOBREL
01171            IF FIRST-CHAR-SHOW-AS-IS                               ELTOBREL
01172               MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)           ELTOBREL
01173               PERFORM 9400-MOVE-TO-COFDTL                         ELTOBREL
01174                   VARYING WS-SUB-CMF FROM 1 BY 1                  ELTOBREL
01175                   UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES          ELTOBREL
01176            ELSE                                                   ELTOBREL
01177                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
01178            END-IF                                                 ELTOBREL
01179      END-IF.                                                      ELTOBREL
01180                                                                   ELTOBREL
01181                                                                   ELTOBREL
01182      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTOBREL
01183         SET PLT-INDEX2       TO  2                                ELTOBREL
01184         IF PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)               ELTOBREL
01185                NOT = '0000' AND NOT = '00  '                      ELTOBREL
01186                AND NOT = LOW-VALUES                               ELTOBREL
01187            MOVE 'BPD'           TO  CMF-RECORD-PREFIX             ELTOBREL
01188            MOVE PLD-BEN-SCOPE-ID(PLT-INDEX1, PLT-INDEX2)          ELTOBREL
01189                                 TO  CMF-CODE-VALUE                ELTOBREL
01190            MOVE 'BEN-SCOPE-ID'  TO  CMF-ELEMENT-SYSTEM-NAME       ELTOBREL
01191            PERFORM 9300-CALL-CODES-MANUAL                         ELTOBREL
01192            MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79            ELTOBREL
01193                                                                   ELTOBREL
01194            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01195                ADD 1 TO WS-CIA                                    ELTOBREL
01196                MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)          ELTOBREL
01197            END-IF                                                 ELTOBREL
01198                                                                   ELTOBREL
01199            IF FIRST-CHAR-SHOW-AS-IS                               ELTOBREL
01200               MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)           ELTOBREL
01201               PERFORM 9400-MOVE-TO-COFDTL                         ELTOBREL
01202                   VARYING WS-SUB-CMF FROM 1 BY 1                  ELTOBREL
01203                   UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES          ELTOBREL
01204            ELSE                                                   ELTOBREL
01205                PERFORM 9400-COMPRESS-STRING-MOVE                  ELTOBREL
01206            END-IF                                                 ELTOBREL
01207      END-IF.                                                      ELTOBREL
01208                                                                   ELTOBREL
01209                                                                   ELTOBREL
01210 ****************************************************************  ELTOBREL
01211 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTOBREL
01212 *         FOR 'C' BENEFIT PROVISION RECORD TYPES.              *  ELTOBREL
01213 ****************************************************************  ELTOBREL
01214  3500-EXCEPTION-SCHED.                                            ELTOBREL
01215      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01216      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01217         IF PLC-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
01218                                             NOT  =  ZERO          ELTOBREL
01219                                     AND NOT = LOW-VALUES          ELTOBREL
01220            MOVE 'Y' TO CALL-ELUOUTPT-IND                          ELTOBREL
01221                                                                   ELTOBREL
01222            ADD  +2 TO  WS-CIA                                     ELTOBREL
01223            MOVE WS-EXCEPTION-SCHED TO COF-DTL-LINE (WS-CIA)       ELTOBREL
01224            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01225                ADD 1 TO WS-CIA                                    ELTOBREL
01226                MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)         ELTOBREL
01227            END-IF                                                 ELTOBREL
01228            ADD  +1 TO  WS-CIA                                     ELTOBREL
01229            MOVE PLC-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTOBREL
01230                                 TO COF-DTL-LINE (WS-CIA)          ELTOBREL
01231         END-IF                                                    ELTOBREL
01232      END-IF.                                                      ELTOBREL
01233                                                                   ELTOBREL
01234      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01235      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01236         IF PLC-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
01237                                             NOT  =  ZERO          ELTOBREL
01238                                     AND NOT = LOW-VALUES          ELTOBREL
01239            IF NOT YES-CALL-ELUOUTPT                               ELTOBREL
01240                ADD +2 TO WS-CIA                                   ELTOBREL
01241                MOVE WS-EXCEPTION-SCHED TO COF-DTL-LINE (WS-CIA)   ELTOBREL
01242                MOVE 'Y' TO CALL-ELUOUTPT-IND                      ELTOBREL
01243            END-IF                                                 ELTOBREL
01244            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01245                ADD 1 TO WS-CIA                                    ELTOBREL
01246                MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)          ELTOBREL
01247            END-IF                                                 ELTOBREL
01248            ADD  +1 TO  WS-CIA                                     ELTOBREL
01249            MOVE PLC-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTOBREL
01250              TO COF-DTL-LINE (WS-CIA)                             ELTOBREL
01251         END-IF                                                    ELTOBREL
01252      END-IF.                                                      ELTOBREL
01253                                                                   ELTOBREL
01254      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01255         MOVE 'N' TO CALL-ELUOUTPT-IND                             ELTOBREL
01256         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
01257         MOVE 0 TO WS-CIA                                          ELTOBREL
01258      END-IF.                                                      ELTOBREL
01259 ****************************************************************  ELTOBREL
01260 *     MULTIPLE RELATED PROCEDURE PRICING INDICATOR             *  ELTOBREL
01261 *                                                              *  ELTOBREL
01262 ****************************************************************  ELTOBREL
01263  3600-REL-PROC-PRICE.                                             ELTOBREL
01264      MOVE '3600'            TO  WS-PARA-ID1.                      ELTOBREL
01265      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01266      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01267         AND                                                       ELTOBREL
01268             PLC-MULT-REL-PROC-IND (PLT-INDEX1, PLT-INDEX2) > '00' ELTOBREL
01269                                                                   ELTOBREL
01270          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTOBREL
01271          MOVE +2                    TO  WS-CIA                    ELTOBREL
01272          MOVE WS-MULT-PRICE     TO  COF-DTL-LINE (WS-CIA)         ELTOBREL
01273                                                                   ELTOBREL
01274          MOVE 'BPC' TO  CMF-RECORD-PREFIX                         ELTOBREL
01275          MOVE 'MULT-REL-PROC-IND'                                 ELTOBREL
01276                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01277          MOVE PLC-MULT-REL-PROC-IND (PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01278                TO  CMF-CODE-VALUE                                 ELTOBREL
01279                                                                   ELTOBREL
01280          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01281          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01282                                                                   ELTOBREL
01283          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01284              ADD 1 TO WS-CIA                                      ELTOBREL
01285              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTOBREL
01286          END-IF                                                   ELTOBREL
01287                                                                   ELTOBREL
01288          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01289             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01290             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01291                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01292                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01293          ELSE                                                     ELTOBREL
01294              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01295          END-IF                                                   ELTOBREL
01296      END-IF.                                                      ELTOBREL
01297                                                                   ELTOBREL
01298                                                                   ELTOBREL
01299      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01300      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
01301         AND                                                       ELTOBREL
01302         PLC-MULT-REL-PROC-IND (PLT-INDEX1, PLT-INDEX2) > '00'     ELTOBREL
01303                                                                   ELTOBREL
01304          IF NOT  YES-CALL-ELUOUTPT                                ELTOBREL
01305              MOVE 'Y'               TO  CALL-ELUOUTPT-IND         ELTOBREL
01306              MOVE +2                TO  WS-CIA                    ELTOBREL
01307              MOVE WS-MULT-PRICE     TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01308          END-IF                                                   ELTOBREL
01309                                                                   ELTOBREL
01310          MOVE 'BPC' TO  CMF-RECORD-PREFIX                         ELTOBREL
01311          MOVE 'MULT-REL-PROC-IND'                                 ELTOBREL
01312                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01313          MOVE PLC-MULT-REL-PROC-IND (PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01314               TO  CMF-CODE-VALUE                                  ELTOBREL
01315          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01316          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01317                                                                   ELTOBREL
01318          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01319              ADD 1 TO WS-CIA                                      ELTOBREL
01320              MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)            ELTOBREL
01321          END-IF                                                   ELTOBREL
01322                                                                   ELTOBREL
01323          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01324             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01325             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01326                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01327                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01328          ELSE                                                     ELTOBREL
01329              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01330          END-IF                                                   ELTOBREL
01331      END-IF.                                                      ELTOBREL
01332                                                                   ELTOBREL
01333      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01334         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
01335          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTOBREL
01336          MOVE 0 TO WS-CIA.                                        ELTOBREL
01337                                                                   ELTOBREL
01338      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBREL
01339         SET PLT-INDEX2  TO  2                                     ELTOBREL
01340      ELSE                                                         ELTOBREL
01341         SET PLT-INDEX2  TO  1.                                    ELTOBREL
01342                                                                   ELTOBREL
01343                                                                   ELTOBREL
01344      TITLE 'PROFESSIONAL - ELTOBREL'.                             ELTOBREL
01345  4000-PROFESSIONAL-IP.                                            ELTOBREL
01346 ****************************************************************  ELTOBREL
01347 *   4000-PROFESSIONAL-IP                                          ELTOBREL
01348 *   OBSETRICAL RELATED PROFESSIONAL INPATIENT PROCESSING          ELTOBREL
01349 ****************************************************************  ELTOBREL
01350                                                                   ELTOBREL
01351      MOVE '4000'  TO  WS-PARA-ID1.                                ELTOBREL
01352      MOVE 'Y'     TO  WS-FIRSTTIME-IND.                           ELTOBREL
01353                                                                   ELTOBREL
01354                                                                   ELTOBREL
01355      MOVE ' ' TO COF-FUNCTION.                                    ELTOBREL
01356                                                                   ELTOBREL
01357      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTOBREL
01358                                                                   ELTOBREL
01359      PERFORM 4010-MOVE-IN-PROF-IP-TABS                            ELTOBREL
01360         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTOBREL
01361                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTOBREL
01362                                                                   ELTOBREL
01363      PERFORM 4020-CALL-COVERAGE                                   ELTOBREL
01364         THRU 4020-EXIT.                                           ELTOBREL
01365                                                                   ELTOBREL
01366      IF PVN-COVG-NONE                                             ELTOBREL
01367          GO TO 4000-EXIT.                                         ELTOBREL
01368                                                                   ELTOBREL
01369      PERFORM 4030-FIND-FIRST-NONZERO                              ELTOBREL
01370         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTOBREL
01371                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTOBREL
01372                                                                   ELTOBREL
01373  4000-EXIT.  EXIT.                                                ELTOBREL
01374 /                                                                 ELTOBREL
01375  4010-MOVE-IN-PROF-IP-TABS.                                       ELTOBREL
01376      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01377      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTOBREL
01378                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTOBREL
01379                                                                   ELTOBREL
01380      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTOBREL
01381                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTOBREL
01382                                                                   ELTOBREL
01383  4010-EXIT.  EXIT.                                                ELTOBREL
01384      SKIP3                                                        ELTOBREL
01385  4020-CALL-COVERAGE.                                              ELTOBREL
01386      MOVE '4020'  TO  WS-PARA-ID1.                                ELTOBREL
01387                                                                   ELTOBREL
01388      MOVE 'OBSTETRICAL RELATED SERVICES' TO  SSB-TOPIC-PHRASE.    ELTOBREL
01389                                                                   ELTOBREL
01390      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTOBREL
01391                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01392                     END-EXEC.                                     ELTOBREL
01393                                                                   ELTOBREL
01394      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
01395                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01396                     END-EXEC.                                     ELTOBREL
01397                                                                   ELTOBREL
01398      IF PVN-COVG-NONE                                             ELTOBREL
01399          GO TO 4020-EXIT.                                         ELTOBREL
01400                                                                   ELTOBREL
01401      MOVE +1  TO  WS-CIA.                                         ELTOBREL
01402                                                                   ELTOBREL
01403      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTOBREL
01404      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
01405          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTOBREL
01406                                                                   ELTOBREL
01407      INITIALIZE PLS-PAYMENT-LEVEL-SWITCHES.                       ELTOBREL
01408                                                                   ELTOBREL
01409                                                                   ELTOBREL
01410      MOVE '1' TO   PSP-PROVN-PRICING-METHD,                       ELTOBREL
01411                    PSP-TREAT-RESTRN-IND,                          ELTOBREL
01412                    PSP-TRANSF-OTHER-RESP-IND,                     ELTOBREL
01413                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTOBREL
01414                    PSP-SPILL-OVER-DED-APL-IND,                    ELTOBREL
01415                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTOBREL
01416                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTOBREL
01417                    PSP-PLACE-TREAT-ELIG-IND,                      ELTOBREL
01418                    PSP-CERTFN-REQRM-IND,                          ELTOBREL
01419                                                                   ELTOBREL
01420                    PSC-BEN-SCOPE-ID,                              ELTOBREL
01421                    PSC-EXCP-SCHED-ID,                             ELTOBREL
01422                    PSC-MULT-REL-PROC-IND,                         ELTOBREL
01423                                                                   ELTOBREL
01424                    PSE-EXCP-SCHED-ID,                             ELTOBREL
01425                    PSE-BEN-MAX-VISITS-DAYS,                       ELTOBREL
01426                    PSE-BEN-MAX-VISITS-IND,                        ELTOBREL
01427                    PSE-MAX-AMT-PER-VISIT.                         ELTOBREL
01428                                                                   ELTOBREL
01429      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTOBREL
01430                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01431                     END-EXEC.                                     ELTOBREL
01432                                                                   ELTOBREL
01433      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTOBREL
01434      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
01435          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTOBREL
01436                                                                   ELTOBREL
01437  4020-EXIT.  EXIT.                                                ELTOBREL
01438      SKIP3                                                        ELTOBREL
01439  4030-FIND-FIRST-NONZERO.                                         ELTOBREL
01440      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01441                                                                   ELTOBREL
01442      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTOBREL
01443          NEXT SENTENCE                                            ELTOBREL
01444      ELSE                                                         ELTOBREL
01445          PERFORM 4100-BUILD-SCREEN-LINES                          ELTOBREL
01446             THRU 4100-EXIT.                                       ELTOBREL
01447                                                                   ELTOBREL
01448  4030-EXIT.  EXIT.                                                ELTOBREL
01449 /                                                                 ELTOBREL
01450  4100-BUILD-SCREEN-LINES.                                         ELTOBREL
01451      MOVE '4100'  TO  WS-PARA-ID1.                                ELTOBREL
01452      SET PLT-INDEX1  TO                                           ELTOBREL
01453              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTOBREL
01454                                                                   ELTOBREL
01455      IF WS-NOT-FIRST-TIME                                         ELTOBREL
01456         MOVE 'P'    TO COF-FUNCTION                               ELTOBREL
01457         MOVE WS-CIA TO COF-NBR-DTL-LINES                          ELTOBREL
01458         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTOBREL
01459                          COMMAREA(DFHCOMMAREA)                    ELTOBREL
01460         END-EXEC                                                  ELTOBREL
01461      ELSE                                                         ELTOBREL
01462        MOVE 'N'  TO  WS-FIRSTTIME-IND.                            ELTOBREL
01463                                                                   ELTOBREL
01464      MOVE  +1  TO  WS-CIA.                                        ELTOBREL
01465                                                                   ELTOBREL
01466      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTOBREL
01467          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTOBREL
01468              SET PLT-INDEX2  TO  2                                ELTOBREL
01469          ELSE                                                     ELTOBREL
01470              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTOBREL
01471              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTOBREL
01472              GO TO 4100-EXIT                                      ELTOBREL
01473      ELSE                                                         ELTOBREL
01474          SET PLT-INDEX2  TO  1.                                   ELTOBREL
01475                                                                   ELTOBREL
01476      MOVE WS-PROF-IP-CNT TO WS-LIST-BP-CNT.                       ELTOBREL
01477      PERFORM 6105-LIST-BEN-PROV                                   ELTOBREL
01478         THRU 6105-EXIT.                                           ELTOBREL
01479                                                                   ELTOBREL
01480 *  **********************************************************     ELTOBREL
01481 *    THESE ARE THE VARIOUS FIELDS FROM THE BENIFIT PROVISONS      ELTOBREL
01482 *    FILE.                                                        ELTOBREL
01483 *  **********************************************************     ELTOBREL
01484                                                                   ELTOBREL
01485      PERFORM 6120-PRIC-METH                                       ELTOBREL
01486         THRU 6120-EXIT.                                           ELTOBREL
01487                                                                   ELTOBREL
01488      PERFORM 6110-PLACE-OF-TREATMENT                              ELTOBREL
01489         THRU 6110-EXIT.                                           ELTOBREL
01490                                                                   ELTOBREL
01491      PERFORM 6130-TREAT-RESTRN                                    ELTOBREL
01492         THRU 6130-EXIT.                                           ELTOBREL
01493                                                                   ELTOBREL
01494      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTOBREL
01495         THRU 6160-EXIT.                                           ELTOBREL
01496                                                                   ELTOBREL
01497      PERFORM 6115-CERT-REQ-IND                                    ELTOBREL
01498         THRU 6115-EXIT.                                           ELTOBREL
01499                                                                   ELTOBREL
01500      PERFORM 6165-TRANS-OTHR-RESPON-IND                           ELTOBREL
01501         THRU 6165-EXIT.                                           ELTOBREL
01502                                                                   ELTOBREL
01503      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01504      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'C'                      ELTOBREL
01505         PERFORM 3300-BENEFIT-SCOPE-ID                             ELTOBREL
01506         PERFORM 3500-EXCEPTION-SCHED.                             ELTOBREL
01507 * MULTIPLE PRICING COMMENTED OUT AT AUGGIES REQUEST, 11/28/95     ELTOBREL
01508 *       PERFORM 3600-REL-PROC-PRICE.                              ELTOBREL
01509                                                                   ELTOBREL
01510      IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'E'                      ELTOBREL
01511         PERFORM 5500-EXCEPTION-SCHED                              ELTOBREL
01512         PERFORM 5600-MAX-VISIT                                    ELTOBREL
01513         PERFORM 5800-MAX-PER-VISIT.                               ELTOBREL
01514                                                                   ELTOBREL
01515      PERFORM 6200-PAY-CONSID-TEXT THRU 6200-EXIT.                 ELTOBREL
01516  4100-EXIT.  EXIT.                                                ELTOBREL
01517                                                                   ELTOBREL
01518 ****************************************************************  ELTOBREL
01519 *     E X C E P T I O N   S C H E D U L E   I N D I C A T O R  *  ELTOBREL
01520 *   PARAGRAPHS 5500- THRU 5900- WERE COPIED FROM ELTINPTS         ELTOBREL
01521 ****************************************************************  ELTOBREL
01522  5500-EXCEPTION-SCHED.                                            ELTOBREL
01523      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01524      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01525         IF PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
01526                                             NOT  =  ZERO          ELTOBREL
01527            ADD  +1                    TO  WS-CIA                  ELTOBREL
01528            MOVE 'Y' TO CALL-ELUOUTPT-IND                          ELTOBREL
01529            MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA)   ELTOBREL
01530            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01531                ADD 1 TO WS-CIA                                    ELTOBREL
01532                MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)         ELTOBREL
01533            END-IF                                                 ELTOBREL
01534            ADD  +1 TO  WS-CIA                                     ELTOBREL
01535            MOVE PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)        ELTOBREL
01536              TO COF-DTL-LINE (WS-CIA)                             ELTOBREL
01537         END-IF                                                    ELTOBREL
01538      END-IF.                                                      ELTOBREL
01539                                                                   ELTOBREL
01540      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01541      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01542         IF PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
01543                                             NOT  =  ZERO          ELTOBREL
01544            ADD  +1                    TO  WS-CIA                  ELTOBREL
01545            MOVE 'Y' TO CALL-ELUOUTPT-IND                          ELTOBREL
01546            MOVE WS-EXCEPTION-SCHED    TO  COF-DTL-LINE (WS-CIA)   ELTOBREL
01547            IF DISPLAY-BAS-SUP = 'Y'                               ELTOBREL
01548                ADD 1 TO WS-CIA                                    ELTOBREL
01549                MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)          ELTOBREL
01550            END-IF                                                 ELTOBREL
01551            ADD  +1 TO  WS-CIA                                     ELTOBREL
01552            MOVE  PLE-EXCP-SCHED-ID (PLT-INDEX1, PLT-INDEX2)       ELTOBREL
01553              TO COF-DTL-LINE (WS-CIA)                             ELTOBREL
01554         END-IF                                                    ELTOBREL
01555      END-IF.                                                      ELTOBREL
01556                                                                   ELTOBREL
01557      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01558         MOVE 'N' TO CALL-ELUOUTPT-IND                             ELTOBREL
01559         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
01560         MOVE 0 TO WS-CIA                                          ELTOBREL
01561      END-IF.                                                      ELTOBREL
01562 ****************************************************************  ELTOBREL
01563 *      M A X I M U M   V I S I T S   P E R   WHATEVER          *  ELTOBREL
01564 * NOTE: THE COBOL NAME FOR THE NUMBER OF VISITS                *  ELTOBREL
01565 ****************************************************************  ELTOBREL
01566  5600-MAX-VISIT.                                                  ELTOBREL
01567      MOVE 'N'  TO  CALL-ELUOUTPT-IND.                             ELTOBREL
01568      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)   NOT =  ZERO         ELTOBREL
01569         SET PLT-INDEX2 TO 1                                       ELTOBREL
01570         IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)         ELTOBREL
01571                                                NOT =  ZERO        ELTOBREL
01572           AND PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01573                                               NOT = LOW-VALUES    ELTOBREL
01574           AND PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01575                                               NOT = '0'           ELTOBREL
01576           MOVE 'Y' TO CALL-ELUOUTPT-IND                           ELTOBREL
01577           ADD 2 TO WS-CIA                                         ELTOBREL
01578           MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)              ELTOBREL
01579           IF DISPLAY-BAS-SUP = 'Y'                                ELTOBREL
01580              ADD 1 TO WS-CIA                                      ELTOBREL
01581              MOVE WS-BASIC-LIT TO COF-DTL-LINE(WS-CIA)            ELTOBREL
01582           END-IF                                                  ELTOBREL
01583           PERFORM 5610-CK-N-MOVE                                  ELTOBREL
01584      END-IF.                                                      ELTOBREL
01585                                                                   ELTOBREL
01586      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)   NOT  =   ZERO       ELTOBREL
01587         SET  PLT-INDEX2 TO  2                                     ELTOBREL
01588         IF PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)         ELTOBREL
01589                                                NOT =  ZERO        ELTOBREL
01590           AND PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01591                                                NOT = LOW-VALUES   ELTOBREL
01592           AND PLE-BEN-MAX-VISITS-IND(PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01593                                               NOT = '0'           ELTOBREL
01594             IF NOT YES-CALL-ELUOUTPT                              ELTOBREL
01595                MOVE 'Y' TO CALL-ELUOUTPT-IND                      ELTOBREL
01596                ADD 2 TO WS-CIA                                    ELTOBREL
01597                MOVE WS-MAX-VISITS TO COF-DTL-LINE(WS-CIA)         ELTOBREL
01598             END-IF                                                ELTOBREL
01599                                                                   ELTOBREL
01600             IF DISPLAY-BAS-SUP = 'Y'                              ELTOBREL
01601                ADD 1 TO WS-CIA                                    ELTOBREL
01602                MOVE WS-SUPP-LIT TO COF-DTL-LINE(WS-CIA)           ELTOBREL
01603             END-IF                                                ELTOBREL
01604             PERFORM 5610-CK-N-MOVE                                ELTOBREL
01605      END-IF.                                                      ELTOBREL
01606                                                                   ELTOBREL
01607      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01608         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
01609          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTOBREL
01610         MOVE 0 TO WS-CIA                                          ELTOBREL
01611      END-IF.                                                      ELTOBREL
01612                                                                   ELTOBREL
01613  5610-CK-N-MOVE.                                                  ELTOBREL
01614      MOVE PLE-BEN-MAX-VISITS-IND (PLT-INDEX1, PLT-INDEX2)         ELTOBREL
01615                           TO CMF-CODE-VALUE.                      ELTOBREL
01616      MOVE 'BPE'                     TO  CMF-RECORD-PREFIX.        ELTOBREL
01617      MOVE 'BEN-MAX-VISITS-IND' TO       CMF-ELEMENT-SYSTEM-NAME.  ELTOBREL
01618      PERFORM 9300-CALL-CODES-MANUAL.                              ELTOBREL
01619      ADD 1 TO WS-CIA.                                             ELTOBREL
01620                                                                   ELTOBREL
01621      IF PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1, PLT-INDEX2)          ELTOBREL
01622                                                    = ZEROS        ELTOBREL
01623         MOVE CMF-DESCR-LINE(1) TO COF-DTL-LINE(WS-CIA)            ELTOBREL
01624      ELSE                                                         ELTOBREL
01625         MOVE PLE-BEN-MAX-VISITS-DAYS (PLT-INDEX1,PLT-INDEX2)      ELTOBREL
01626              TO WS-DTL-MAX-DAYS                                   ELTOBREL
01627         MOVE CMF-DESCR-LINE(1) TO WS-DTL-MAX-IND                  ELTOBREL
01628         MOVE WS-MAX-DAYS TO COF-DTL-LINE(WS-CIA)                  ELTOBREL
01629                                                                   ELTOBREL
01630      END-IF.                                                      ELTOBREL
01631                                                                   ELTOBREL
01632 ****************************************************************  ELTOBREL
01633 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTOBREL
01634 ****************************************************************  ELTOBREL
01635  5800-MAX-PER-VISIT.                                              ELTOBREL
01636      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01637      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01638         IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTOBREL
01639                                             NOT  =  ZERO          ELTOBREL
01640          ADD +1                     TO  WS-CIA                    ELTOBREL
01641          MOVE WS-MAX-AMT-TEXT       TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01642          MOVE WS-BASIC-LIT          TO  WS-BASIC-SUPP             ELTOBREL
01643          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01644                                     TO  WS-EDIT-MAX-AMT           ELTOBREL
01645          ADD  +1                    TO  WS-CIA                    ELTOBREL
01646          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01647          PERFORM 9200-TEXT-OUTPUT-REQUEST.                        ELTOBREL
01648                                                                   ELTOBREL
01649      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01650      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT  =  ZERO          ELTOBREL
01651         IF PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)         ELTOBREL
01652                                              NOT  =  ZERO         ELTOBREL
01653          ADD +1                     TO  WS-CIA                    ELTOBREL
01654          MOVE WS-MAX-AMT-TEXT       TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01655          MOVE WS-SUPP-LIT     TO  WS-BASIC-SUPP                   ELTOBREL
01656          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTOBREL
01657                                     TO  WS-EDIT-MAX-AMT           ELTOBREL
01658          ADD  +1                    TO  WS-CIA                    ELTOBREL
01659          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01660          PERFORM 9200-TEXT-OUTPUT-REQUEST.                        ELTOBREL
01661                                                                   ELTOBREL
01662                                                                   ELTOBREL
01663      TITLE 'LIST OF BENEFIT PROVNS - ELTOBREL'.                   ELTOBREL
01664  6105-LIST-BEN-PROV.                                              ELTOBREL
01665 ****************************************************************  ELTOBREL
01666 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTOBREL
01667 ****************************************************************  ELTOBREL
01668      MOVE '6105'            TO  WS-PARA-ID1.                      ELTOBREL
01669                                                                   ELTOBREL
01670      MOVE  +2               TO  WS-CIA.                           ELTOBREL
01671      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTOBREL
01672      MOVE ZERO              TO  WS-SUB2.                          ELTOBREL
01673      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTOBREL
01674                             TO  WS-SUB3.                          ELTOBREL
01675                                                                   ELTOBREL
01676      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTOBREL
01677         THRU 6106-EXIT VARYING                                    ELTOBREL
01678                 PVN-BEN-PROVN-IDX FROM WS-SUB BY +1               ELTOBREL
01679         UNTIL   PVN-BEN-PROVN-IDX > WS-LIST-BP-CNT.               ELTOBREL
01680                                                                   ELTOBREL
01681      MOVE '6105'            TO  WS-PARA-ID1.                      ELTOBREL
01682      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTOBREL
01683      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTOBREL
01684                                                                   ELTOBREL
01685      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
01686                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
01687                     END-EXEC.                                     ELTOBREL
01688                                                                   ELTOBREL
01689  6105-EXIT.  EXIT.                                                ELTOBREL
01690  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTOBREL
01691                                                                   ELTOBREL
01692      MOVE '6106'            TO  WS-PARA-ID1.                      ELTOBREL
01693      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTOBREL
01694          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTOBREL
01695          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTOBREL
01696          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTOBREL
01697                            TO  CMF-CODE-VALUE                     ELTOBREL
01698          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTOBREL
01699          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTOBREL
01700 *        PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01701          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTOBREL
01702               THRU 9500-EXIT                                      ELTOBREL
01703          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTOBREL
01704          ADD  +1    TO  WS-SUB2                                   ELTOBREL
01705          IF WS-CIA  >  20  OR  =  20                              ELTOBREL
01706              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTOBREL
01707              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTOBREL
01708                             COMMAREA (DFHCOMMAREA)                ELTOBREL
01709                             END-EXEC                              ELTOBREL
01710              MOVE  +1  TO  WS-CIA.                                ELTOBREL
01711                                                                   ELTOBREL
01712  6106-EXIT.  EXIT.                                                ELTOBREL
01713                                                                   ELTOBREL
01714      TITLE 'PLACE OF TREATMENT IND - ELTOBREL'.                   ELTOBREL
01715  6110-PLACE-OF-TREATMENT.                                         ELTOBREL
01716 ****************************************************************  ELTOBREL
01717 *              P L A C E   O F   T R E A T M E N T             *  ELTOBREL
01718 *                                                              *  ELTOBREL
01719 *                                                              *  ELTOBREL
01720 ****************************************************************  ELTOBREL
01721      MOVE '6110'            TO  WS-PARA-ID1.                      ELTOBREL
01722      MOVE SPACES TO WS-TEMP-TEXT-AREA.                            ELTOBREL
01723      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01724      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01725         AND  PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)    ELTOBREL
01726               NOT =  ZERO                                         ELTOBREL
01727          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTOBREL
01728          MOVE +2                    TO  WS-CIA                    ELTOBREL
01729          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01730                                                                   ELTOBREL
01731          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
01732          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTOBREL
01733                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01734          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTOBREL
01735                TO  CMF-CODE-VALUE                                 ELTOBREL
01736                                                                   ELTOBREL
01737          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01738 ******                                                            ELTOBREL
01739          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01740                                                                   ELTOBREL
01741          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01742              ADD 1 TO WS-CIA                                      ELTOBREL
01743              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTOBREL
01744          END-IF                                                   ELTOBREL
01745                                                                   ELTOBREL
01746          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01747             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01748             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01749                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01750                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01751          ELSE                                                     ELTOBREL
01752              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01753          END-IF                                                   ELTOBREL
01754      END-IF.                                                      ELTOBREL
01755 ******                                                            ELTOBREL
01756      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01757      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
01758         AND   PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTOBREL
01759               NOT  =  ZERO                                        ELTOBREL
01760          IF NOT  YES-CALL-ELUOUTPT                                ELTOBREL
01761              MOVE 'Y'               TO  CALL-ELUOUTPT-IND         ELTOBREL
01762              MOVE +2                TO  WS-CIA                    ELTOBREL
01763              MOVE WS-SERVICES-RENDERED TO COF-DTL-LINE (WS-CIA)   ELTOBREL
01764          END-IF                                                   ELTOBREL
01765                                                                   ELTOBREL
01766          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
01767          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTOBREL
01768                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01769          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTOBREL
01770               TO  CMF-CODE-VALUE                                  ELTOBREL
01771          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01772          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01773                                                                   ELTOBREL
01774          MOVE SPACES TO WS-TEMP-TEXT-AREA                         ELTOBREL
01775          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01776              ADD 1 TO WS-CIA                                      ELTOBREL
01777              MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)            ELTOBREL
01778          END-IF                                                   ELTOBREL
01779                                                                   ELTOBREL
01780          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01781             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01782             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01783                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01784                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01785          ELSE                                                     ELTOBREL
01786              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01787          END-IF                                                   ELTOBREL
01788      END-IF.                                                      ELTOBREL
01789                                                                   ELTOBREL
01790      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01791         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
01792          PERFORM 9200-TEXT-OUTPUT-REQUEST.                        ELTOBREL
01793                                                                   ELTOBREL
01794      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBREL
01795         SET PLT-INDEX2  TO  2                                     ELTOBREL
01796      ELSE                                                         ELTOBREL
01797         SET PLT-INDEX2  TO  1.                                    ELTOBREL
01798                                                                   ELTOBREL
01799  6110-EXIT.  EXIT.                                                ELTOBREL
01800                                                                   ELTOBREL
01801  6115-CERT-REQ-IND.                                               ELTOBREL
01802 ****************************************************************  ELTOBREL
01803 *       C E R T I F I C A T E  R  E Q U I R E M E N T          *  ELTOBREL
01804 ****************************************************************  ELTOBREL
01805      MOVE '6115'            TO  WS-PARA-ID1.                      ELTOBREL
01806      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01807      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
01808         AND                                                       ELTOBREL
01809         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
01810               NOT = ZERO                                          ELTOBREL
01811                                                                   ELTOBREL
01812          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTOBREL
01813          MOVE +2                    TO  WS-CIA                    ELTOBREL
01814          MOVE WS-CERT-REQ TO  COF-DTL-LINE (WS-CIA)               ELTOBREL
01815                                                                   ELTOBREL
01816          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
01817          MOVE 'CERTFN-REQRM-IND'                                  ELTOBREL
01818                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01819          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBREL
01820                TO  CMF-CODE-VALUE                                 ELTOBREL
01821          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01822                                                                   ELTOBREL
01823 ******                                                            ELTOBREL
01824          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01825                                                                   ELTOBREL
01826          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01827              ADD 1 TO WS-CIA                                      ELTOBREL
01828              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTOBREL
01829          END-IF                                                   ELTOBREL
01830                                                                   ELTOBREL
01831          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01832             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01833             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01834                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01835                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01836          ELSE                                                     ELTOBREL
01837              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01838          END-IF                                                   ELTOBREL
01839      END-IF.                                                      ELTOBREL
01840 ******                                                            ELTOBREL
01841                                                                   ELTOBREL
01842      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01843      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
01844         AND  PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)        ELTOBREL
01845               NOT  =  ZERO                                        ELTOBREL
01846          IF NOT YES-CALL-ELUOUTPT                                 ELTOBREL
01847              MOVE 'Y'               TO  CALL-ELUOUTPT-IND         ELTOBREL
01848              MOVE +2                TO  WS-CIA                    ELTOBREL
01849              MOVE WS-CERT-REQ       TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
01850          END-IF                                                   ELTOBREL
01851                                                                   ELTOBREL
01852          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
01853          MOVE 'CERTFN-REQRM-IND'                                  ELTOBREL
01854                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
01855          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBREL
01856               TO  CMF-CODE-VALUE                                  ELTOBREL
01857          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
01858 ******                                                            ELTOBREL
01859          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
01860                                                                   ELTOBREL
01861          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
01862              ADD 1 TO WS-CIA                                      ELTOBREL
01863              MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)            ELTOBREL
01864          END-IF                                                   ELTOBREL
01865                                                                   ELTOBREL
01866          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
01867             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
01868             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
01869                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
01870                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
01871          ELSE                                                     ELTOBREL
01872              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
01873          END-IF                                                   ELTOBREL
01874      END-IF.                                                      ELTOBREL
01875 ******                                                            ELTOBREL
01876                                                                   ELTOBREL
01877      IF YES-CALL-ELUOUTPT                                         ELTOBREL
01878         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
01879         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
01880         MOVE 0 TO WS-CIA                                          ELTOBREL
01881      END-IF.                                                      ELTOBREL
01882                                                                   ELTOBREL
01883      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBREL
01884         SET PLT-INDEX2  TO  2                                     ELTOBREL
01885      ELSE                                                         ELTOBREL
01886         SET PLT-INDEX2  TO  1.                                    ELTOBREL
01887                                                                   ELTOBREL
01888  6115-EXIT.  EXIT.                                                ELTOBREL
01889                                                                   ELTOBREL
01890      TITLE 'PROVN PRICING METHOD - ELTOBREL'.                     ELTOBREL
01891  6120-PRIC-METH.                                                  ELTOBREL
01892 **---------------------------------------------------------------+ELTOBREL
01893 ** 6120-PRIC-METH.                                                ELTOBREL
01894 **   PROVISON PRICING METHOD AND:                                 ELTOBREL
01895 **     VARIABLE INDEMNTITY PERCENT OR ADDITIONAL PRICING PERCENT OELTOBREL
01896 **     FLAT RATE PER DIEM OR ADDITIONAL ALLOWANCE AMOUNT.         ELTOBREL
01897 **                                                                ELTOBREL
01898 ** NOTES:                                                         ELTOBREL
01899 **-  1)  THIS CODE WAS TAKEN FROM PROGRAM ELTPSYCH, AND MODIFIED  ELTOBREL
01900 **-      FOR ELTOBREL.  MODIFICATIONS INCLUDE:                    ELTOBREL
01901 **-      1) DELETED CODE FOR 'W' BP.                              ELTOBREL
01902 **-      2) LOOP TO DISPLAY ALL LINES FROM THE CODES MANUAL, AS   ELTOBREL
01903 **-         THEY APPEAR ON THE MANUAL.                            ELTOBREL
01904 **-      3) DISPLAY FIELD WS-PRCNT-PERDM-ALLOW ONLY IF IT IS NOT  ELTOBREL
01905 **-         BLANK.                                                ELTOBREL
01906 **-  2) I LEARNED THAT THE 4 FIELDS (VARIABLE INDEMNITY PERCENT,  ELTOBREL
01907 **-     ADDITIONAL PRICING PERCENT, FLAT RATE PER DIEM AND        ELTOBREL
01908 **-     ADDITIONAL ALLOWANCE AMOUNT) ARE VERY RARELY CODED BY SSD.ELTOBREL
01909 **-                                                               ELTOBREL
01910 **---------------------------------------------------------------+ELTOBREL
01911      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01912      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTOBREL
01913         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTOBREL
01914                                                              '19' ELTOBREL
01915         MOVE +2                    TO  WS-CIA                     ELTOBREL
01916         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTOBREL
01917         MOVE 'Y'  TO  CALL-ELUOUTPT-IND.                          ELTOBREL
01918                                                                   ELTOBREL
01919      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
01920      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTOBREL
01921         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTOBREL
01922                                                        '19' AND   ELTOBREL
01923         NOT YES-CALL-ELUOUTPT                                     ELTOBREL
01924         MOVE +2                    TO  WS-CIA                     ELTOBREL
01925         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA)              ELTOBREL
01926         MOVE 'Y'  TO  CALL-ELUOUTPT-IND.                          ELTOBREL
01927                                                                   ELTOBREL
01928 ******************************************************************ELTOBREL
01929 *  CHECK FOR POSSIBLE ERROR:   AN ERROR IS WHEN THERE IS A BASIC  ELTOBREL
01930 *     OR SUPPLEMENTAL CONTRACT, AND THE PROVIDER PRICING METHOD   ELTOBREL
01931 *     IS ZERO.                                                    ELTOBREL
01932 ******************************************************************ELTOBREL
01933      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01934      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTOBREL
01935         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTOBREL
01936                            AND                                    ELTOBREL
01937         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
01938         SET  PLT-INDEX2  TO  2                                    ELTOBREL
01939         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTOBREL
01940                                                             ZERO  ELTOBREL
01941            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTOBREL
01942            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTOBREL
01943            ADD +1  TO  WS-CIA.                                    ELTOBREL
01944                                                                   ELTOBREL
01945                                                                   ELTOBREL
01946      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01947      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTOBREL
01948         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTOBREL
01949                            AND                                    ELTOBREL
01950         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTOBREL
01951         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTOBREL
01952         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                   ELTOBREL
01953         ADD +1  TO  WS-CIA.                                       ELTOBREL
01954                                                                   ELTOBREL
01955      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTOBREL
01956         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
01957         SET  PLT-INDEX2  TO  2                                    ELTOBREL
01958         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTOBREL
01959                                                             ZERO  ELTOBREL
01960            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTOBREL
01961            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA)                ELTOBREL
01962            ADD +1  TO  WS-CIA.                                    ELTOBREL
01963                                                                   ELTOBREL
01964 ******************************************************************ELTOBREL
01965 *    CHECK BASIC CONTRACT INFORMATION                             ELTOBREL
01966 ******************************************************************ELTOBREL
01967      MOVE SPACES TO WS-PRCNT-PERDM-ALLOW.                         ELTOBREL
01968                                                                   ELTOBREL
01969      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
01970      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBREL
01971         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTOBREL
01972                                                            =  ZEROELTOBREL
01973            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
01974                                                            =  ZEROELTOBREL
01975               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTOBREL
01976            ELSE                                                   ELTOBREL
01977               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTOBREL
01978               MOVE                                                ELTOBREL
01979               PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
01980                                        TO  WS-PERCENTAGE          ELTOBREL
01981               MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW         ELTOBREL
01982         ELSE                                                      ELTOBREL
01983          MOVE '%'  TO  WS-PERCENT-SIGN                            ELTOBREL
01984          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
01985                                                 TO  WS-PERCENTAGE ELTOBREL
01986          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTOBREL
01987                                                                   ELTOBREL
01988      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTOBREL
01989       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTOBREL
01990        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTOBREL
01991                                                            =  ZEROELTOBREL
01992        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTOBREL
01993                                                            =  ZEROELTOBREL
01994         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTOBREL
01995                                                            =  ZEROELTOBREL
01996            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTOBREL
01997                                                            =  ZEROELTOBREL
01998               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTOBREL
01999            ELSE                                                   ELTOBREL
02000               MOVE                                                ELTOBREL
02001                   PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)   ELTOBREL
02002                                                  TO  WS-PER-DIEM  ELTOBREL
02003               MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW          ELTOBREL
02004         ELSE                                                      ELTOBREL
02005          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTOBREL
02006                                                 TO  WS-ALLOW      ELTOBREL
02007          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTOBREL
02008                                                                   ELTOBREL
02009                                                                   ELTOBREL
02010      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTOBREL
02011         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTOBREL
02012                                             ZERO AND  NOT =  '19' ELTOBREL
02013         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBREL
02014         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTOBREL
02015         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTOBREL
02016                                                    CMF-CODE-VALUE ELTOBREL
02017         PERFORM 9300-CALL-CODES-MANUAL                            ELTOBREL
02018 ******                                                            ELTOBREL
02019         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTOBREL
02020                                                                   ELTOBREL
02021         IF DISPLAY-BAS-SUP = 'Y'                                  ELTOBREL
02022             ADD 1 TO WS-CIA                                       ELTOBREL
02023             MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)            ELTOBREL
02024         END-IF                                                    ELTOBREL
02025                                                                   ELTOBREL
02026         IF FIRST-CHAR-SHOW-AS-IS                                  ELTOBREL
02027            MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)              ELTOBREL
02028            PERFORM 9400-MOVE-TO-COFDTL                            ELTOBREL
02029                VARYING WS-SUB-CMF FROM 1 BY 1                     ELTOBREL
02030                UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES             ELTOBREL
02031         ELSE                                                      ELTOBREL
02032             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTOBREL
02033         END-IF                                                    ELTOBREL
02034         IF WS-PRCNT-PERDM-ALLOW NOT = SPACES                      ELTOBREL
02035             ADD 1 TO WS-CIA                                       ELTOBREL
02036             MOVE WS-PRCNT-PERDM-ALLOW TO                          ELTOBREL
02037                  COF-DTL-LINE (WS-CIA)                            ELTOBREL
02038         END-IF                                                    ELTOBREL
02039       END-IF.                                                     ELTOBREL
02040 ******                                                            ELTOBREL
02041                                                                   ELTOBREL
02042 ******************************************************************ELTOBREL
02043 *    CHECK SUPPLEMENTAL CONTRACT INFORMATION                      ELTOBREL
02044 ******************************************************************ELTOBREL
02045      MOVE SPACES TO WS-PRCNT-PERDM-ALLOW.                         ELTOBREL
02046                                                                   ELTOBREL
02047      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
02048      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
02049         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTOBREL
02050                                                               ZEROELTOBREL
02051            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
02052                                                            =  ZEROELTOBREL
02053               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTOBREL
02054            ELSE                                                   ELTOBREL
02055               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTOBREL
02056          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
02057                                                  TO  WS-PERCENTAGEELTOBREL
02058          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW              ELTOBREL
02059         ELSE                                                      ELTOBREL
02060            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTOBREL
02061          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTOBREL
02062                                                 TO  WS-PERCENTAGE ELTOBREL
02063          MOVE WS-PERCENT-FLD TO WS-PRCNT-PERDM-ALLOW.             ELTOBREL
02064                                                                   ELTOBREL
02065      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
02066       IF PVN-BEN-FMT(PVN-BEN-PROVN-IDX) = 'A'                     ELTOBREL
02067        IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTOBREL
02068                                                            =  ZEROELTOBREL
02069        IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)    ELTOBREL
02070                                                            =  ZEROELTOBREL
02071         IF PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)     ELTOBREL
02072                                                            =  ZEROELTOBREL
02073            IF PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTOBREL
02074                                                            =  ZEROELTOBREL
02075               MOVE SPACES  TO  WS-PRCNT-PERDM-ALLOW               ELTOBREL
02076            ELSE                                                   ELTOBREL
02077             MOVE                                                  ELTOBREL
02078               PLA-FLAT-RATE-PDM-AMT(PLT-INDEX1, PLT-INDEX2)       ELTOBREL
02079                                                  TO  WS-PER-DIEM  ELTOBREL
02080          MOVE WS-PER-DIEM   TO WS-PRCNT-PERDM-ALLOW               ELTOBREL
02081         ELSE                                                      ELTOBREL
02082          MOVE PLA-ADDN-ALLOW-AMT-PER-DAY(PLT-INDEX1, PLT-INDEX2)  ELTOBREL
02083                                                 TO  WS-ALLOW      ELTOBREL
02084          MOVE WS-ALLOW      TO WS-PRCNT-PERDM-ALLOW.              ELTOBREL
02085                                                                   ELTOBREL
02086                                                                   ELTOBREL
02087      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTOBREL
02088         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTOBREL
02089                                             ZERO AND  NOT =  '19' ELTOBREL
02090         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBREL
02091         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTOBREL
02092         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTOBREL
02093                                                    CMF-CODE-VALUE ELTOBREL
02094         PERFORM 9300-CALL-CODES-MANUAL                            ELTOBREL
02095 ******                                                            ELTOBREL
02096         MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79               ELTOBREL
02097                                                                   ELTOBREL
02098         IF DISPLAY-BAS-SUP = 'Y'                                  ELTOBREL
02099             ADD 1 TO WS-CIA                                       ELTOBREL
02100             MOVE WS-SUPP-LIT TO COF-DTL-LINE (WS-CIA)             ELTOBREL
02101         END-IF                                                    ELTOBREL
02102                                                                   ELTOBREL
02103         IF FIRST-CHAR-SHOW-AS-IS                                  ELTOBREL
02104            MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)              ELTOBREL
02105            PERFORM 9400-MOVE-TO-COFDTL                            ELTOBREL
02106                VARYING WS-SUB-CMF FROM 1 BY 1                     ELTOBREL
02107                UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES             ELTOBREL
02108         ELSE                                                      ELTOBREL
02109             PERFORM 9400-COMPRESS-STRING-MOVE                     ELTOBREL
02110         END-IF                                                    ELTOBREL
02111         IF WS-PRCNT-PERDM-ALLOW NOT = SPACES                      ELTOBREL
02112             ADD 1 TO WS-CIA                                       ELTOBREL
02113             MOVE WS-PRCNT-PERDM-ALLOW TO                          ELTOBREL
02114                  COF-DTL-LINE (WS-CIA)                            ELTOBREL
02115         END-IF                                                    ELTOBREL
02116       END-IF.                                                     ELTOBREL
02117 ******                                                            ELTOBREL
02118                                                                   ELTOBREL
02119      IF YES-CALL-ELUOUTPT                                         ELTOBREL
02120         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
02121         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
02122         MOVE 0 TO WS-CIA.                                         ELTOBREL
02123                                                                   ELTOBREL
02124      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROS               ELTOBREL
02125         SET PLT-INDEX2 TO 2                                       ELTOBREL
02126      ELSE SET PLT-INDEX2 TO 1.                                    ELTOBREL
02127                                                                   ELTOBREL
02128  6120-EXIT.  EXIT.                                                ELTOBREL
02129                                                                   ELTOBREL
02130      TITLE 'TREATMENT RESTRICTION IND- ELTOBREL'.                 ELTOBREL
02131  6130-TREAT-RESTRN.                                               ELTOBREL
02132 ****************************************************************  ELTOBREL
02133 * 6130-TREATEMENT RESTRICTION INDICATOR                        *  ELTOBREL
02134 *  CHANGED TO SIMPLIFIY THE DISPLAY PROCESS.                   *  ELTOBREL
02135 ****************************************************************  ELTOBREL
02136      MOVE '6130'            TO  WS-PARA-ID1.                      ELTOBREL
02137      SET  PLT-INDEX2  TO  1.                                      ELTOBREL
02138      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)  NOT =  ZERO          ELTOBREL
02139         AND                                                       ELTOBREL
02140         PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)             ELTOBREL
02141             NOT = ZERO                                            ELTOBREL
02142          MOVE 'Y'                   TO  CALL-ELUOUTPT-IND         ELTOBREL
02143          MOVE +2                    TO  WS-CIA                    ELTOBREL
02144          MOVE WS-TREAT-RESTRN       TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
02145          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
02146          MOVE 'TREAT-RESTRN-IND'                                  ELTOBREL
02147                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
02148          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBREL
02149                TO  CMF-CODE-VALUE                                 ELTOBREL
02150          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
02151 ******                                                            ELTOBREL
02152          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
02153                                                                   ELTOBREL
02154          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
02155              ADD 1 TO WS-CIA                                      ELTOBREL
02156              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTOBREL
02157          END-IF                                                   ELTOBREL
02158                                                                   ELTOBREL
02159          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
02160             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
02161             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
02162                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
02163                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
02164          ELSE                                                     ELTOBREL
02165              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
02166          END-IF                                                   ELTOBREL
02167      END-IF.                                                      ELTOBREL
02168 ******                                                            ELTOBREL
02169                                                                   ELTOBREL
02170      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
02171      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTOBREL
02172         AND PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)         ELTOBREL
02173             NOT = ZERO                                            ELTOBREL
02174         IF NOT YES-CALL-ELUOUTPT                                  ELTOBREL
02175             MOVE 'Y'                TO  CALL-ELUOUTPT-IND         ELTOBREL
02176             MOVE +2                 TO  WS-CIA                    ELTOBREL
02177             MOVE WS-TREAT-RESTRN    TO  COF-DTL-LINE (WS-CIA)     ELTOBREL
02178         END-IF                                                    ELTOBREL
02179                                                                   ELTOBREL
02180          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTOBREL
02181          MOVE 'TREAT-RESTRN-IND'                                  ELTOBREL
02182                TO  CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
02183          MOVE PLP-TREAT-RESTRN-IND (PLT-INDEX1, PLT-INDEX2)       ELTOBREL
02184               TO  CMF-CODE-VALUE                                  ELTOBREL
02185          PERFORM 9300-CALL-CODES-MANUAL                           ELTOBREL
02186 ******                                                            ELTOBREL
02187          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
02188                                                                   ELTOBREL
02189          IF DISPLAY-BAS-SUP = 'Y'                                 ELTOBREL
02190              ADD 1 TO WS-CIA                                      ELTOBREL
02191              MOVE WS-BASIC-LIT TO COF-DTL-LINE (WS-CIA)           ELTOBREL
02192          END-IF                                                   ELTOBREL
02193                                                                   ELTOBREL
02194          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
02195             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
02196             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
02197                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
02198                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
02199          ELSE                                                     ELTOBREL
02200              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
02201          END-IF                                                   ELTOBREL
02202      END-IF.                                                      ELTOBREL
02203 ******                                                            ELTOBREL
02204                                                                   ELTOBREL
02205      IF YES-CALL-ELUOUTPT                                         ELTOBREL
02206         MOVE 'N'  TO  CALL-ELUOUTPT-IND                           ELTOBREL
02207          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTOBREL
02208          MOVE 0 TO WS-CIA.                                        ELTOBREL
02209                                                                   ELTOBREL
02210      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBREL
02211         SET PLT-INDEX2  TO  2                                     ELTOBREL
02212      ELSE                                                         ELTOBREL
02213         SET PLT-INDEX2  TO  1.                                    ELTOBREL
02214                                                                   ELTOBREL
02215  6130-EXIT.  EXIT.                                                ELTOBREL
02216      TITLE 'SPILLOVER COINSURANCE - ELTOBREL'.                    ELTOBREL
02217  6160-SPILLOVR-COINS-N-DEDUC.                                     ELTOBREL
02218 ****************************************************************  ELTOBREL
02219 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTOBREL
02220 ****************************************************************  ELTOBREL
02221      MOVE '6160'            TO  WS-PARA-ID1.                      ELTOBREL
02222      MOVE +1  TO  WS-CIA.                                         ELTOBREL
02223                                                                   ELTOBREL
02224      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
02225      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)  NOT =  ZERO         ELTOBREL
02226         AND  PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)ELTOBREL
02227              NOT =  '0'                                           ELTOBREL
02228         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBREL
02229         MOVE 'SPILL-OVER-COINS-APL-IND'                           ELTOBREL
02230               TO CMF-ELEMENT-SYSTEM-NAME                          ELTOBREL
02231         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTOBREL
02232               TO  CMF-CODE-VALUE                                  ELTOBREL
02233         ADD 2 TO WS-CIA                                           ELTOBREL
02234                                                                   ELTOBREL
02235         MOVE WS-SPILLOVER TO COF-DTL-LINE (WS-CIA)                ELTOBREL
02236         PERFORM 9300-CALL-CODES-MANUAL                            ELTOBREL
02237 ******                                                            ELTOBREL
02238          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
02239                                                                   ELTOBREL
02240          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
02241             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
02242             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
02243                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
02244                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
02245          ELSE                                                     ELTOBREL
02246              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
02247          END-IF                                                   ELTOBREL
02248                                                                   ELTOBREL
02249         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
02250         MOVE 0 TO WS-CIA                                          ELTOBREL
02251                                                                   ELTOBREL
02252      END-IF.                                                      ELTOBREL
02253 ******                                                            ELTOBREL
02254 ****************************************************************  ELTOBREL
02255 *          S P I L L O V E R   D E D U C T I B L E             *  ELTOBREL
02256 ****************************************************************  ELTOBREL
02257      MOVE +1  TO  WS-CIA.                                         ELTOBREL
02258                                                                   ELTOBREL
02259      SET  PLT-INDEX2  TO  2.                                      ELTOBREL
02260      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTOBREL
02261         AND  PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)  ELTOBREL
02262              NOT =  '0'                                           ELTOBREL
02263         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTOBREL
02264         MOVE 'SPILL-OVER-DED-APL-IND'                             ELTOBREL
02265               TO   CMF-ELEMENT-SYSTEM-NAME                        ELTOBREL
02266         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTOBREL
02267               TO  CMF-CODE-VALUE                                  ELTOBREL
02268         ADD 1 TO WS-CIA                                           ELTOBREL
02269         MOVE WS-SPILLOVER TO COF-DTL-LINE (WS-CIA)                ELTOBREL
02270         PERFORM 9300-CALL-CODES-MANUAL                            ELTOBREL
02271 ******                                                            ELTOBREL
02272          MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79              ELTOBREL
02273                                                                   ELTOBREL
02274          IF FIRST-CHAR-SHOW-AS-IS                                 ELTOBREL
02275             MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)             ELTOBREL
02276             PERFORM 9400-MOVE-TO-COFDTL                           ELTOBREL
02277                 VARYING WS-SUB-CMF FROM 1 BY 1                    ELTOBREL
02278                 UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES            ELTOBREL
02279          ELSE                                                     ELTOBREL
02280              PERFORM 9400-COMPRESS-STRING-MOVE                    ELTOBREL
02281          END-IF                                                   ELTOBREL
02282                                                                   ELTOBREL
02283         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTOBREL
02284         MOVE 0 TO WS-CIA                                          ELTOBREL
02285                                                                   ELTOBREL
02286      END-IF.                                                      ELTOBREL
02287 ******                                                            ELTOBREL
02288                                                                   ELTOBREL
02289      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTOBREL
02290         SET PLT-INDEX2  TO  2                                     ELTOBREL
02291      ELSE                                                         ELTOBREL
02292         SET PLT-INDEX2  TO  1.                                    ELTOBREL
02293                                                                   ELTOBREL
02294  6160-EXIT.  EXIT.                                                ELTOBREL
02295                                                                   ELTOBREL
02296                                                                   ELTOBREL
02297      TITLE 'TRANSFER TO OTHR RESPON - ELTOBREL'.                  ELTOBREL
02298  6165-TRANS-OTHR-RESPON-IND.                                      ELTOBREL
02299 ******************************************************************ELTOBREL
02300 *   T R A N S F E R   T O   O T H E R  R E S P O N S I B I L I T YELTOBREL
02301 *                     I N D I C A T O R                      9/89 ELTOBREL
02302 ******************************************************************ELTOBREL
02303      MOVE '6165'            TO  WS-PARA-ID1.                      ELTOBREL
02304                                                                   ELTOBREL
02305      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)          =  ZEROS    ELTOBREL
02306         IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)   NOT  = ZEROS    ELTOBREL
02307              SET PLT-INDEX2  TO  2                                ELTOBREL
02308          ELSE                                                     ELTOBREL
02309              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTOBREL
02310              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTOBREL
02311              GO TO 6165-EXIT                                      ELTOBREL
02312      ELSE                                                         ELTOBREL
02313          SET PLT-INDEX2  TO  1.                                   ELTOBREL
02314                                                                   ELTOBREL
02315      IF  PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) = ZEROELTOBREL
02316          GO TO 6165-EXIT.                                         ELTOBREL
02317                                                                   ELTOBREL
02318      MOVE +1  TO  WS-CIA.                                         ELTOBREL
02319      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTOBREL
02320                                                                   ELTOBREL
02321      MOVE 'TRANSF-OTHER-RESP-IND'  TO  CMF-ELEMENT-SYSTEM-NAME.   ELTOBREL
02322                                                                   ELTOBREL
02323      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)      ELTOBREL
02324           TO  CMF-CODE-VALUE.                                     ELTOBREL
02325                                                                   ELTOBREL
02326                                                                   ELTOBREL
02327      PERFORM 9300-CALL-CODES-MANUAL.                              ELTOBREL
02328 ******                                                            ELTOBREL
02329      MOVE CMF-DESCR-LINE(1) TO FIRST-CODE-CHAR79                  ELTOBREL
02330                                                                   ELTOBREL
02331      IF FIRST-CHAR-SHOW-AS-IS                                     ELTOBREL
02332         MOVE FIRST-THE-REST TO CMF-DESCR-LINE (1)                 ELTOBREL
02333         PERFORM 9400-MOVE-TO-COFDTL                               ELTOBREL
02334             VARYING WS-SUB-CMF FROM 1 BY 1                        ELTOBREL
02335             UNTIL WS-SUB-CMF > CMF-NBR-DESCR-LINES                ELTOBREL
02336      ELSE                                                         ELTOBREL
02337          PERFORM 9400-COMPRESS-STRING-MOVE                        ELTOBREL
02338      END-IF                                                       ELTOBREL
02339      PERFORM 9200-TEXT-OUTPUT-REQUEST.                            ELTOBREL
02340      MOVE 0 TO WS-CIA.                                            ELTOBREL
02341                                                                   ELTOBREL
02342  6165-EXIT.       EXIT.                                           ELTOBREL
02343                                                                   ELTOBREL
02344                                                                   ELTOBREL
02345 ******************************************************************ELTOBREL
02346 * 1)DISPLAY THE 'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY      ELTOBREL
02347 *      PHRASE.                                                    ELTOBREL
02348 * 2)DISPLAY THE 'SEE COINSURANCE, DEDUCTIBLE, ETC' PHRASE.        ELTOBREL
02349 ******************************************************************ELTOBREL
02350       TITLE 'DISPLAY COINS / OPX MESSAGE'.                        ELTOBREL
02351  6200-PAY-CONSID-TEXT.                                            ELTOBREL
02352      ADD +2 TO WS-CIA.                                            ELTOBREL
02353      MOVE WS-PVE TO COF-DTL-LINE (WS-CIA).                        ELTOBREL
02354      ADD 1 TO WS-CIA.                                             ELTOBREL
02355                                                                   ELTOBREL
02356      INITIALIZE TCAR-FROM-AREA.                                   ELTOBREL
02357      STRING WS-PAY-CONSDR-TEXT1 ' '                               ELTOBREL
02358             WS-PAY-CONSDR-TEXT2                                   ELTOBREL
02359                 DELIMITED BY SIZE INTO TCAR-FROM-AREA.            ELTOBREL
02360      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBREL
02361      MOVE +02              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTOBREL
02362      MOVE +79              TO  TCAR-OUTPUT-FIELD-1-LEN            ELTOBREL
02363                                TCAR-OUTPUT-FIELD-2-LEN.           ELTOBREL
02364      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBREL
02365      IF WS-CIA > 17                                               ELTOBREL
02366            PERFORM 9200-TEXT-OUTPUT-REQUEST                       ELTOBREL
02367            MOVE +1            TO WS-CIA.                          ELTOBREL
02368                                                                   ELTOBREL
02369      ADD +1                TO  WS-CIA.                            ELTOBREL
02370      MOVE TCAR-OPF-DATA(1) TO  COF-DTL-LINE(WS-CIA).              ELTOBREL
02371      ADD +1                TO  WS-CIA.                            ELTOBREL
02372      MOVE TCAR-OPF-DATA(2) TO  COF-DTL-LINE(WS-CIA).              ELTOBREL
02373      PERFORM 9200-TEXT-OUTPUT-REQUEST.                            ELTOBREL
02374                                                                   ELTOBREL
02375 * RE-INITIALIZE WS-CIA                                            ELTOBREL
02376      MOVE 0 TO WS-CIA.                                            ELTOBREL
02377  6200-EXIT.   EXIT.                                               ELTOBREL
02378                                                                   ELTOBREL
02379                                                                   ELTOBREL
02380 ****************************************************************  ELTOBREL
02381 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTOBREL
02382 ****************************************************************  ELTOBREL
02383  9100-HEADER-OUTPUT-REQUEST.                                      ELTOBREL
02384                                                                   ELTOBREL
02385      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTOBREL
02386      MOVE 'P'            TO  COF-FUNCTION.                        ELTOBREL
02387                                                                   ELTOBREL
02388      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
02389                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
02390                     END-EXEC.                                     ELTOBREL
02391                                                                   ELTOBREL
02392  9100-EXIT.  EXIT.                                                ELTOBREL
02393                                                                   ELTOBREL
02394 ****************************************************************  ELTOBREL
02395 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTOBREL
02396 ****************************************************************  ELTOBREL
02397  9200-TEXT-OUTPUT-REQUEST.                                        ELTOBREL
02398                                                                   ELTOBREL
02399      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTOBREL
02400      MOVE +0      TO  COF-NBR-HDR-LINES.                          ELTOBREL
02401      MOVE ' '     TO  COF-FUNCTION.                               ELTOBREL
02402                                                                   ELTOBREL
02403      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTOBREL
02404                     COMMAREA (DFHCOMMAREA)                        ELTOBREL
02405                     END-EXEC.                                     ELTOBREL
02406                                                                   ELTOBREL
02407                                                                   ELTOBREL
02408                                                                   ELTOBREL
02409                                                                   ELTOBREL
02410 ****************************************************************  ELTOBREL
02411 *          C A L L   C O D E S   M A N U A L                   *  ELTOBREL
02412 ****************************************************************  ELTOBREL
02413  9300-CALL-CODES-MANUAL.                                          ELTOBREL
02414      INITIALIZE CMF-RETURN-CODE,                                  ELTOBREL
02415                 TCAR-FROM-AREA.                                   ELTOBREL
02416                                                                   ELTOBREL
02417      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTOBREL
02418                       COMMAREA(DFHCOMMAREA)                       ELTOBREL
02419      END-EXEC.                                                    ELTOBREL
02420                                                                   ELTOBREL
02421      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTOBREL
02422      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
02423          ADDRESS OF CMF-DESCR.                                    ELTOBREL
02424                                                                   ELTOBREL
02425 ****************************************************************  ELTOBREL
02426 * 9400-MOVE-TO-COFDTL                                          *  ELTOBREL
02427 * THE PURPOSE OF THIS PARAGRAPH IS TO MOVE EACH LINE RETURNED  *  ELTOBREL
02428 * FROM THE CODES MANUAL TO THE ARRAY INTERFACE FOR PROGRAM     *  ELTOBREL
02429 * ELUOUTPT.  WS-CIA IS THE SUBSRIPT.                           *  ELTOBREL
02430 * RGO. 10/12/95.                                               *  ELTOBREL
02431 ****************************************************************  ELTOBREL
02432  9400-MOVE-TO-COFDTL.                                             ELTOBREL
02433                                                                   ELTOBREL
02434                                                                   ELTOBREL
02435      ADD +1 TO WS-CIA.                                            ELTOBREL
02436      MOVE CMF-DESCR-LINE (WS-SUB-CMF) TO                          ELTOBREL
02437           COF-DTL-LINE (WS-CIA).                                  ELTOBREL
02438                                                                   ELTOBREL
02439      IF WS-CIA > 20 OR = 20                                       ELTOBREL
02440          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTOBREL
02441          MOVE ' ' TO COF-FUNCTION                                 ELTOBREL
02442          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTOBREL
02443                         COMMAREA (DFHCOMMAREA)                    ELTOBREL
02444                         END-EXEC                                  ELTOBREL
02445          MOVE +1 TO WS-CIA                                        ELTOBREL
02446      END-IF.                                                      ELTOBREL
02447                                                                   ELTOBREL
02448 ****************************************************************  ELTOBREL
02449 * 9400-COMPRESS-STRING-MOVE                                    *  ELTOBREL
02450 * THE PURPOSE OF THIS PARAGRAPH IS TO COMPRESS WHAT IS RETURNED*  ELTOBREL
02451 * FROM THE CODES MANUAL, STRING IT INTO 79 CHARACTER LINES,    *  ELTOBREL
02452 * AND DISPLAY IT STARTING ON A NEW LINE.                       *  ELTOBREL
02453 * RGO. 11/09/95.                                               *  ELTOBREL
02454 ****************************************************************  ELTOBREL
02455  9400-COMPRESS-STRING-MOVE.                                       ELTOBREL
02456      MOVE 79 TO TCAR-OUTPUT-FIELD-1-LEN.                          ELTOBREL
02457      STRING CMF-DESCR-LINE(1), ' '                                ELTOBREL
02458         CMF-DESCR-LINE(2), ' '                                    ELTOBREL
02459         CMF-DESCR-LINE(3), ' '                                    ELTOBREL
02460         CMF-DESCR-LINE(4), ' '                                    ELTOBREL
02461         CMF-DESCR-LINE(5), ' '                                    ELTOBREL
02462         CMF-DESCR-LINE(6), ' '                                    ELTOBREL
02463         CMF-DESCR-LINE(7), ' '                                    ELTOBREL
02464         CMF-DESCR-LINE(8), ' '                                    ELTOBREL
02465         CMF-DESCR-LINE(9), ' '                                    ELTOBREL
02466         CMF-DESCR-LINE(10), ' '                                   ELTOBREL
02467         CMF-DESCR-LINE(11), ' '                                   ELTOBREL
02468         CMF-DESCR-LINE(12), ' '                                   ELTOBREL
02469         CMF-DESCR-LINE(13), ' '                                   ELTOBREL
02470         CMF-DESCR-LINE(14), ' '                                   ELTOBREL
02471         CMF-DESCR-LINE(15), ' '                                   ELTOBREL
02472         DELIMITED BY SIZE INTO TCAR-FROM-AREA.                    ELTOBREL
02473                                                                   ELTOBREL
02474      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBREL
02475      MOVE +15              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTOBREL
02476      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN,           ELTOBREL
02477                                TCAR-OUTPUT-FIELD-3-LEN,           ELTOBREL
02478                                TCAR-OUTPUT-FIELD-4-LEN,           ELTOBREL
02479                                TCAR-OUTPUT-FIELD-5-LEN,           ELTOBREL
02480                                TCAR-OUTPUT-FIELD-6-LEN,           ELTOBREL
02481                                TCAR-OUTPUT-FIELD-7-LEN,           ELTOBREL
02482                                TCAR-OUTPUT-FIELD-8-LEN,           ELTOBREL
02483                                TCAR-OUTPUT-FIELD-9-LEN,           ELTOBREL
02484                                TCAR-OUTPUT-FIELD-10-LEN,          ELTOBREL
02485                                TCAR-OUTPUT-FIELD-11-LEN,          ELTOBREL
02486                                TCAR-OUTPUT-FIELD-12-LEN,          ELTOBREL
02487                                TCAR-OUTPUT-FIELD-13-LEN,          ELTOBREL
02488                                TCAR-OUTPUT-FIELD-14-LEN,          ELTOBREL
02489                                TCAR-OUTPUT-FIELD-15-LEN.          ELTOBREL
02490      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBREL
02491                                                                   ELTOBREL
02492      IF WS-MOVE-LINES-TO-CIA                                      ELTOBREL
02493         PERFORM 9450-MOVE-STRUNG                                  ELTOBREL
02494            VARYING WS-SUB1 FROM 1 BY 1                            ELTOBREL
02495            UNTIL WS-SUB1 > TCAR-OUTPUT-FIELDS-USED                ELTOBREL
02496      END-IF.                                                      ELTOBREL
02497                                                                   ELTOBREL
02498  9450-MOVE-STRUNG.                                                ELTOBREL
02499      ADD 1 TO WS-CIA.                                             ELTOBREL
02500      MOVE TCAR-OPF-DATA(WS-SUB1) TO COF-DTL-LINE(WS-CIA).         ELTOBREL
02501                                                                   ELTOBREL
02502      IF WS-CIA > 20 OR = 20                                       ELTOBREL
02503          MOVE WS-CIA TO COF-NBR-DTL-LINES                         ELTOBREL
02504          MOVE ' ' TO COF-FUNCTION                                 ELTOBREL
02505          EXEC CICS LINK PROGRAM ('ELUOUTPT')                      ELTOBREL
02506                         COMMAREA (DFHCOMMAREA)                    ELTOBREL
02507                         END-EXEC                                  ELTOBREL
02508          MOVE +1 TO WS-CIA                                        ELTOBREL
02509      END-IF.                                                      ELTOBREL
02510                                                                   ELTOBREL
02511 ****************************************************************  ELTOBREL
02512 * 9400-CODES-MANUAL-LONG                                       *  ELTOBREL
02513 * NOTES ON HOW THIS WORKS:                                     *  ELTOBREL
02514 * 1) TCAR-OUTPUT-FIELD-COUNT DETERMINES HOW MANY LINES FROM THE*  ELTOBREL
02515 *    CODES MANUAL (INCLUDING BLANK LINES) IT WILL STRING       *  ELTOBREL
02516 *    TOGETHER.                                                 *  ELTOBREL
02517 * 2) TCAR-OUTPUT-FIELD-USED IS THE ACTUAL NUMBER OF LINES      *  ELTOBREL
02518 *    THE UNSTRING PROGRAM RETURNED.                            *  ELTOBREL
02519 * 3) WHAT THE HE#@ IS WS-TEMP-NOT-USED-CNT USED FOR, AND HOW   *  ELTOBREL
02520 *    DOES IT WORK?                                             *  ELTOBREL
02521 *    IT DETERMINES THE COLUMN THE INFORMATION WILL START.      *  ELTOBREL
02522 *    IT IS SUBTRACTED FROM 79, AND THEN THAT VALUE PLUS 1 IS THE *ELTOBREL
02523 *    COLUMN YOU WILL SEE STUFF. FOR EXAMPLE,                   *  ELTOBREL
02524 *    IF ...NOT-USED-CNT IS SET TO 70, THE INFO FROM THE CODE   *  ELTOBREL
02525 *    VALUE WILL START ON COLUMN 10.  WHATEVER                  *  ELTOBREL
02526 *                                                              *  ELTOBREL
02527 *                                                              *  ELTOBREL
02528 *                                                              *  ELTOBREL
02529 ****************************************************************  ELTOBREL
02530  9400-CODES-MANUAL-LONG.                                          ELTOBREL
02531      IF WS-TEMP-NOT-USED-CNT > ZERO                               ELTOBREL
02532          MOVE WS-TEMP-NOT-USED-CNT TO TCAR-OUTPUT-FIELD-1-LEN     ELTOBREL
02533          STRING CMF-DESCR-LINE(1), ' '                            ELTOBREL
02534             CMF-DESCR-LINE(2), ' '                                ELTOBREL
02535             CMF-DESCR-LINE(3), ' '                                ELTOBREL
02536             CMF-DESCR-LINE(4), ' '                                ELTOBREL
02537             CMF-DESCR-LINE(5), ' '                                ELTOBREL
02538             CMF-DESCR-LINE(6), ' '                                ELTOBREL
02539             CMF-DESCR-LINE(7), ' '                                ELTOBREL
02540             CMF-DESCR-LINE(8), ' '                                ELTOBREL
02541             CMF-DESCR-LINE(9), ' '                                ELTOBREL
02542             CMF-DESCR-LINE(10), ' '                               ELTOBREL
02543             CMF-DESCR-LINE(11), ' '                               ELTOBREL
02544             CMF-DESCR-LINE(12), ' '                               ELTOBREL
02545             CMF-DESCR-LINE(13), ' '                               ELTOBREL
02546             CMF-DESCR-LINE(14), ' '                               ELTOBREL
02547             CMF-DESCR-LINE(15), ' '                               ELTOBREL
02548             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTOBREL
02549                                                                   ELTOBREL
02550      IF WS-TEMP-NOT-USED-CNT = ZERO                               ELTOBREL
02551          MOVE +79 TO TCAR-OUTPUT-FIELD-1-LEN                      ELTOBREL
02552          STRING WS-TEMP-TEXT-AREA, ' '                            ELTOBREL
02553             CMF-DESCR-LINE(1), ' '                                ELTOBREL
02554             CMF-DESCR-LINE(2), ' '                                ELTOBREL
02555             CMF-DESCR-LINE(3), ' '                                ELTOBREL
02556             CMF-DESCR-LINE(4), ' '                                ELTOBREL
02557             CMF-DESCR-LINE(5), ' '                                ELTOBREL
02558             CMF-DESCR-LINE(6), ' '                                ELTOBREL
02559             CMF-DESCR-LINE(7), ' '                                ELTOBREL
02560             CMF-DESCR-LINE(8), ' '                                ELTOBREL
02561             CMF-DESCR-LINE(9), ' '                                ELTOBREL
02562             CMF-DESCR-LINE(10), ' '                               ELTOBREL
02563             CMF-DESCR-LINE(11), ' '                               ELTOBREL
02564             CMF-DESCR-LINE(12), ' '                               ELTOBREL
02565             CMF-DESCR-LINE(13), ' '                               ELTOBREL
02566             CMF-DESCR-LINE(14), ' '                               ELTOBREL
02567             CMF-DESCR-LINE(15), ' '                               ELTOBREL
02568             DELIMITED BY SIZE INTO TCAR-FROM-AREA.                ELTOBREL
02569      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBREL
02570      MOVE +07              TO  TCAR-OUTPUT-FIELD-COUNT.           ELTOBREL
02571      MOVE +79              TO  TCAR-OUTPUT-FIELD-2-LEN,           ELTOBREL
02572                                TCAR-OUTPUT-FIELD-3-LEN,           ELTOBREL
02573                                TCAR-OUTPUT-FIELD-4-LEN,           ELTOBREL
02574                                TCAR-OUTPUT-FIELD-5-LEN,           ELTOBREL
02575                                TCAR-OUTPUT-FIELD-6-LEN,           ELTOBREL
02576                                TCAR-OUTPUT-FIELD-7-LEN,           ELTOBREL
02577                                TCAR-OUTPUT-FIELD-8-LEN,           ELTOBREL
02578                                TCAR-OUTPUT-FIELD-9-LEN,           ELTOBREL
02579                                TCAR-OUTPUT-FIELD-10-LEN,          ELTOBREL
02580                                TCAR-OUTPUT-FIELD-11-LEN,          ELTOBREL
02581                                TCAR-OUTPUT-FIELD-12-LEN,          ELTOBREL
02582                                TCAR-OUTPUT-FIELD-13-LEN,          ELTOBREL
02583                                TCAR-OUTPUT-FIELD-14-LEN,          ELTOBREL
02584                                TCAR-OUTPUT-FIELD-15-LEN.          ELTOBREL
02585      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBREL
02586                                                                   ELTOBREL
02587      IF WS-MOVE-LINES-TO-CIA                                      ELTOBREL
02588         IF WS-TEMP-NOT-USED-CNT NOT = ZERO                        ELTOBREL
02589            COMPUTE WS-TEMP-NOT-USED-CNT = 79 -                    ELTOBREL
02590                                           WS-TEMP-NOT-USED-CNT    ELTOBREL
02591            PERFORM 9420-CONCATENATE-TO-TEMP-TEXT                  ELTOBREL
02592               VARYING WS-SUB1 FROM 1 BY 1                         ELTOBREL
02593               UNTIL WS-TEMP-NOT-USED-CNT >  +78                   ELTOBREL
02594            MOVE ZERO TO WS-TEMP-NOT-USED-CNT                      ELTOBREL
02595            MOVE WS-TEMP-TEXT-AREA TO COF-DTL-LINE(WS-CIA)         ELTOBREL
02596            ADD +1 TO WS-CIA                                       ELTOBREL
02597         ELSE                                                      ELTOBREL
02598           MOVE TCAR-OPF-DATA(1)  TO COF-DTL-LINE(WS-CIA)          ELTOBREL
02599           ADD +1 TO WS-CIA.                                       ELTOBREL
02600                                                                   ELTOBREL
02601      IF WS-MOVE-LINES-TO-CIA                                      ELTOBREL
02602         IF TCAR-OUTPUT-FIELDS-USED >  1                           ELTOBREL
02603            PERFORM 9440-MOVE-LINES-TO-CIA                         ELTOBREL
02604               VARYING WS-SUB1  FROM  2  BY  1                     ELTOBREL
02605               UNTIL  WS-SUB1 > TCAR-OUTPUT-FIELDS-USED            ELTOBREL
02606         ELSE                                                      ELTOBREL
02607           CONTINUE                                                ELTOBREL
02608      ELSE                                                         ELTOBREL
02609         MOVE 'Y' TO WS-MOVE-LINES-IND.                            ELTOBREL
02610                                                                   ELTOBREL
02611  9420-CONCATENATE-TO-TEMP-TEXT.                                   ELTOBREL
02612      ADD +1 TO WS-TEMP-NOT-USED-CNT.                              ELTOBREL
02613      MOVE TCAR-OPF-DIGIT(1, WS-SUB1) TO                           ELTOBREL
02614           WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT).                ELTOBREL
02615                                                                   ELTOBREL
02616  9440-MOVE-LINES-TO-CIA.                                          ELTOBREL
02617      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTOBREL
02618      IF WS-CIA > 19                                               ELTOBREL
02619             PERFORM 9200-TEXT-OUTPUT-REQUEST.                     ELTOBREL
02620      ADD +1  TO  WS-CIA.                                          ELTOBREL
02621                                                                   ELTOBREL
02622      TITLE ' CODES MANUAL INTERFACE  -- ELTDRUGS'.                ELTOBREL
02623  9500-CALL-CODES-MANUAL-LONG.                                     ELTOBREL
02624 ****************************************************************  ELTOBREL
02625 *                                                              *  ELTOBREL
02626 * C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  *  ELTOBREL
02627 *                                                              *  ELTOBREL
02628 ****************************************************************  ELTOBREL
02629      MOVE '9500'  TO  WS-PARA-ID2.                                ELTOBREL
02630                                                                   ELTOBREL
02631      INITIALIZE CMF-RETURN-CODE,                                  ELTOBREL
02632                 TCAR-FROM-AREA.                                   ELTOBREL
02633                                                                   ELTOBREL
02634      EXEC CICS  LINK  PROGRAM('ELUCMIF')  COMMAREA(DFHCOMMAREA)   ELTOBREL
02635      END-EXEC.                                                    ELTOBREL
02636                                                                   ELTOBREL
02637      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTOBREL
02638      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTOBREL
02639          ADDRESS OF CMF-DESCR.                                    ELTOBREL
02640                                                                   ELTOBREL
02641      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTOBREL
02642         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTOBREL
02643         STRING WS-TEMP-TEXT-AREA,  ' ',                           ELTOBREL
02644            CMF-DESCR-LINE(1),        ' ',                         ELTOBREL
02645            CMF-DESCR-LINE(2),        ' ',                         ELTOBREL
02646            CMF-DESCR-LINE(3)                                      ELTOBREL
02647            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA                ELTOBREL
02648      ELSE                                                         ELTOBREL
02649         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN    ELTOBREL
02650         STRING CMF-DESCR-LINE(1),        ' ',                     ELTOBREL
02651            CMF-DESCR-LINE(2),        ' ',                         ELTOBREL
02652            CMF-DESCR-LINE(3)                                      ELTOBREL
02653            DELIMITED BY SIZE  INTO  TCAR-FROM-AREA.               ELTOBREL
02654                                                                   ELTOBREL
02655      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTOBREL
02656                                                                   ELTOBREL
02657      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTOBREL
02658      MOVE +2  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTOBREL
02659      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN.                       ELTOBREL
02660      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTOBREL
02661                                                                   ELTOBREL
02662      IF WS-MOVE-LINES-TO-CIA                                      ELTOBREL
02663         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTOBREL
02664            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTOBREL
02665                                             WS-TEMP-NOT-USED-CNT  ELTOBREL
02666            MOVE '9550'  TO  WS-PARA-ID2                           ELTOBREL
02667            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTOBREL
02668               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTOBREL
02669                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTOBREL
02670            MOVE '9500'  TO  WS-PARA-ID2                           ELTOBREL
02671            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTOBREL
02672            ADD +1  TO  WS-CIA                                     ELTOBREL
02673            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTOBREL
02674         ELSE                                                      ELTOBREL
02675            ADD +1  TO  WS-CIA                                     ELTOBREL
02676            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTOBREL
02677                                                                   ELTOBREL
02678      IF WS-MOVE-LINES-TO-CIA                                      ELTOBREL
02679         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTOBREL
02680            ADD +1  TO  WS-CIA                                     ELTOBREL
02681            MOVE TCAR-OPF-DATA(2)  TO  COF-DTL-LINE(WS-CIA)        ELTOBREL
02682         ELSE                                                      ELTOBREL
02683            NEXT SENTENCE                                          ELTOBREL
02684      ELSE                                                         ELTOBREL
02685         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTOBREL
02686                                                                   ELTOBREL
02687  9500-EXIT.  EXIT.                                                ELTOBREL
02688                                                                   ELTOBREL
02689  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTOBREL
02690      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTOBREL
02691      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTOBREL
02692                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTOBREL
02693                                                                   ELTOBREL
02694  9550-EXIT.  EXIT.                                                ELTOBREL
02695      TITLE ' TEXT COMPRESSION AND EXPANSION'.                     ELTOBREL
02696      COPY ELSTCOMP.                                               ELTOBREL
02697                                                                   ELTOBREL
02698      TITLE ' ELTOBREL '.                                          ELTOBREL
