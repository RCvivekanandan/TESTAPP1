00001 *      LAST MAINTENANCE TIME: 13.03.46  DATE: 07/14/86            06/29/02
00002  IDENTIFICATION DIVISION.                                         ELTNURSE
00003  PROGRAM-ID.    ELTNURSE.                                            LV001
00004  AUTHOR.        LUCY TORRES.                                      ELTNURSE
00005  DATE-WRITTEN.  07/11/86                                          ELTNURSE
00006  DATE-COMPILED.                                                   ELTNURSE
00007      SKIP3                                                        ELTNURSE
00008 ****************************************************************  ELTNURSE
00009 *      ELTNURSE - ELS:  NURSING TOPIC PROGRAM                  *  ELTNURSE
00010 ****************************************************************  ELTNURSE
00011      SKIP3                                                        ELTNURSE
00012 ****************************************************************  ELTNURSE
00013 *              U P D A T E   H I S T O R Y                     *  ELTNURSE
00014 *                                                              *  ELTNURSE
00015 *   DATE    PGM  DESCRIPTION                                   *  ELTNURSE
00016 * --------  ---  --------------------------------------------- *  ELTNURSE
00017 * 07/11/86  LET  ORIGINAL VERSION                              *  ELTNURSE
00018 *                                                              *  ELTNURSE
00019 * 07/24/86  JTC  CHANGED THE PICTURE OF WS-EDIT-MAX-AMT        *  ELTNURSE
00020 *                FROM PIC -$$9.99 TO ZZ9.99-.                  *  ELTNURSE
00021 *                                                              *  ELTNURSE
00022 * 07/30/86  JTC  CORRECTED MOVE OF WS-EDIT-MAX-AMT             *  ELTNURSE
00023 *                TO COF-DTL-LINE.     CHANGED TO MOVE          *  ELTNURSE
00024 *                WS-MAX-AMT TO COF-DTL-LINE.                   *  ELTNURSE
00025 *                                                              *  ELTNURSE
00026 * 08/12/86  LET  DISCREPENCY #P1041 (210) CORRECTED SPELLING   *  ELTNURSE
00027 *                OF 'CERTIFICATION'.                           *  ELTNURSE
00028 *                USING 2ND HEADER LINE FROM PROLOG.            *  ELTNURSE
00029 *                INPATIENT & OUTPATIENT PROVISIONS ARE SPILT.  *  ELTNURSE
00030 *                                                              *  ELTNURSE
00031 * 09/19/86  NAC  VS COBOL II CONVERSION.                       *  ELTNURSE
00032 *                                                              *  ELTNURSE
00033 * 11/26/86  LET  CHANGED CODE TO ACCOMMODATE THE MOVING OF THE *  ELTNURSE
00034 *                CERTIFICATION REQUIREMENT INDICATOR FROM THE  *  ELTNURSE
00035 *                FORMAT TYPE SECTION TO THE COMMON SECTION.    *  ELTNURSE
00036 *                                                              *  ELTNURSE
00037 * 10/20/87  AKK  CHANGED 'THIS GROUP OF BENEFITS ARE HANDLED   *  ELTNURSE
00038 *                AS FOLLOWS' TO 'COVERED SERVICES ARE'.        *  ELTNURSE
00039 *                                                              *  ELTNURSE
00040 * 03/21/89  GEM  STORAGE MANAGEMENT ENHANCEMENTS.              *  ELTNURSE
00041 *                                                              *  ELTNURSE
00042 * 10/20/89  RKH  ADDED TRANSFER TO OTHER RESPON IND            *  ELTNURSE
00043 *                                                              *  ELTNURSE
00044 *                                                                 ELTNURSE
00045 * XXXXX 11/15/90  RKH  CHANGED TRANSFER TO OTHER RESPONSIBILITY   ELTNURSE
00046 *                      FROM A SINGLE POSITION TO ZEROS            ELTNURSE
00047 *                      (FIELD IS CURRENTLY TWO POSITIONS)         ELTNURSE
00048 *                                                                 ELTNURSE
00049 ****************************************************************  ELTNURSE
00050      SKIP3                                                        ELTNURSE
00051  ENVIRONMENT DIVISION.                                            ELTNURSE
00052      SKIP3                                                        ELTNURSE
00053  DATA DIVISION.                                                   ELTNURSE
00054  WORKING-STORAGE SECTION.                                         ELTNURSE
00055  01  WS-BEGIN                    PIC  X(24) VALUE                 ELTNURSE
00056          '** ELTNURSE WS BEGINS **'.                              ELTNURSE
00057 /                                                                 ELTNURSE
00058 ****************************************************************  ELTNURSE
00059 *      CONSTANTS, SWITCHES, HOLD-AREA, WORK-AREA               *  ELTNURSE
00060 ****************************************************************  ELTNURSE
00061  01  WORK-FIELDS.                                                 ELTNURSE
00062      05  WS-HEX-00               PIC  X(01).                      ELTNURSE
00063      05  WS-CHAR-0               PIC  X(01).                      ELTNURSE
00064      05  WS-CIA                  PIC S9(03) COMP VALUE +0.        ELTNURSE
00065      05  WS-SUB                  PIC S9(03) COMP VALUE +0.        ELTNURSE
00066      05  WS-SUB1                 PIC S9(03) COMP VALUE +0.        ELTNURSE
00067      05  WS-SUB2                 PIC S9(03) COMP VALUE +0.        ELTNURSE
00068      05  WS-SUB3                 PIC S9(03) COMP VALUE +0.        ELTNURSE
00069      05  WS-SUB4                 PIC S9(03) COMP VALUE +0.        ELTNURSE
00070      05  WS-TEMP-NOT-USED-CNT    PIC S9(03) COMP.                 ELTNURSE
00071      05  WS-PERCENT-FLD.                                          ELTNURSE
00072        10  WS-PERCENTAGE         PIC ZZ9.                         ELTNURSE
00073        10  WS-PERCENT-SIGN       PIC X.                           ELTNURSE
00074      05  WS-EXPLANATION-IND      PIC S9 COMP.                     ELTNURSE
00075          88  WS-EXPLANATION-PRODUCED       VALUE +1 THRU +3.      ELTNURSE
00076          88  WS-BASIC-EXPLANATION          VALUE +1, +3.          ELTNURSE
00077          88  WS-BASIC-ONLY-EXPLAIN         VALUE +1.              ELTNURSE
00078          88  WS-SUPP-EXPLANATION           VALUE +2 THRU +3.      ELTNURSE
00079          88  WS-SUPP-ONLY-EXPLAIN          VALUE +2.              ELTNURSE
00080          88  WS-NO-EXPLANATION             VALUE +0.              ELTNURSE
00081      05  WS-BASIC-EXPLAIN-CNT    PIC S9 COMP.                     ELTNURSE
00082      05  WS-SUPP-EXPLAIN-CNT     PIC S9 COMP.                     ELTNURSE
00083                                                                   ELTNURSE
00084  01  WS-WORK-AREA.                                                ELTNURSE
00085      05  WS-MAX-AMT.                                              ELTNURSE
00086          10  WS-BASIC-SUPP       PIC  X(17) VALUE SPACES.         ELTNURSE
00087          10  WS-EDIT-MAX-AMT     PIC  ZZ9.99-.                    ELTNURSE
00088                                                                   ELTNURSE
00089  01  WS-EXPLAINS.                                                 ELTNURSE
00090    05  WS-BASIC-EXPLAIN1         PIC X(79).                       ELTNURSE
00091    05  WS-BASIC-EXPLAIN2         PIC X(79).                       ELTNURSE
00092    05  WS-SUPP-EXPLAIN1          PIC X(79).                       ELTNURSE
00093    05  WS-SUPP-EXPLAIN2          PIC X(79).                       ELTNURSE
00094                                                                   ELTNURSE
00095  01  SWITCHES.                                                    ELTNURSE
00096      05  WS-FIRSTTIME-IND        PIC X(01).                       ELTNURSE
00097          88  WS-NOT-FIRST-TIME              VALUE 'N'.            ELTNURSE
00098      05  WS-ADD-A-BLANK-IND      PIC X(01).                       ELTNURSE
00099          88  WS-ADD-A-BLANK-LINE            VALUE 'Y'.            ELTNURSE
00100      05  WS-MOVE-LINES-IND       PIC X(01)  VALUE 'Y'.            ELTNURSE
00101          88  WS-MOVE-LINES-TO-CIA           VALUE 'Y'.            ELTNURSE
00102      05  WS-SAME-PROV-LINE-SW    PIC X(01)  VALUE 'N'.            ELTNURSE
00103          88  WS-SAME-PROV-PRT               VALUE 'Y'.            ELTNURSE
00104                                                                   ELTNURSE
00105 *--------------------------------------------------------------*  ELTNURSE
00106 * B E N E F I T   P R O V I S I O N   I D S   B Y   T Y P E    *  ELTNURSE
00107 *--------------------------------------------------------------*  ELTNURSE
00108  01  TABLE-MAX                   PIC S9(03) VALUE +1 COMP.        ELTNURSE
00109 * 1  REPRESENTS MAXIMUM BENEFIT PROVISIONS WITHIN A SINGLE ARRAY  ELTNURSE
00110                                                                   ELTNURSE
00111  01  WS-BEN-PROV-IDS.                                             ELTNURSE
00112      05  WS-INST-IP-CNT          PIC S9(03) VALUE +1 COMP.        ELTNURSE
00113      05  WS-INST-IP-TABS.                                         ELTNURSE
00114          10  FILLER              PIC  X(06) VALUE 'NRSI B'.       ELTNURSE
00115      05  WS-INST-IP-BP  REDEFINES  WS-INST-IP-TABS                ELTNURSE
00116                                  PIC  X(06) OCCURS 1 TIMES.       ELTNURSE
00117      05  WS-INST-OP-CNT          PIC S9(03) VALUE +1 COMP.        ELTNURSE
00118      05  WS-INST-OP-TABS.                                         ELTNURSE
00119          10  FILLER              PIC  X(06) VALUE 'NRSO B'.       ELTNURSE
00120      05  WS-INST-OP-BP  REDEFINES  WS-INST-OP-TABS                ELTNURSE
00121                                  PIC  X(06) OCCURS 1 TIMES.       ELTNURSE
00122      05  WS-PROF-IP-CNT          PIC S9(03) VALUE +1 COMP.        ELTNURSE
00123      05  WS-PROF-IP-TABS.                                         ELTNURSE
00124          10  FILLER              PIC  X(06) VALUE 'NRSI E'.       ELTNURSE
00125      05  WS-PROF-IP-BP  REDEFINES  WS-PROF-IP-TABS                ELTNURSE
00126                                  PIC  X(06) OCCURS 1 TIMES.       ELTNURSE
00127      05  WS-PROF-OP-CNT          PIC S9(03) VALUE +1 COMP.        ELTNURSE
00128      05  WS-PROF-OP-TABS.                                         ELTNURSE
00129          10  FILLER              PIC  X(06) VALUE 'NRSO E'.       ELTNURSE
00130      05  WS-PROF-OP-BP  REDEFINES  WS-PROF-OP-TABS                ELTNURSE
00131                                  PIC  X(06) OCCURS 1 TIMES.       ELTNURSE
00132                                                                   ELTNURSE
00133 /                                                                 ELTNURSE
00134 ****************************************************************  ELTNURSE
00135 *              HEADER AND LITERAL TEXT AREA                    *  ELTNURSE
00136 ****************************************************************  ELTNURSE
00137  01  HEADER-I-IP-LINE-3.                                          ELTNURSE
00138      05  FILLER                  PIC  X(20) VALUE SPACES.         ELTNURSE
00139      05  FILLER                  PIC  X(39) VALUE                 ELTNURSE
00140              'NURSING SERVICE INSTITUTIONAL INPATIENT'.           ELTNURSE
00141      05  FILLER                  PIC  X(20) VALUE LOW-VALUES.     ELTNURSE
00142                                                                   ELTNURSE
00143  01  HEADER-I-OP-LINE-3.                                          ELTNURSE
00144      05  FILLER                  PIC  X(19) VALUE SPACES.         ELTNURSE
00145      05  FILLER                  PIC  X(40) VALUE                 ELTNURSE
00146              'NURSING SERVICE INSTITUTIONAL OUTPATIENT'.          ELTNURSE
00147      05  FILLER                  PIC  X(20) VALUE LOW-VALUES.     ELTNURSE
00148                                                                   ELTNURSE
00149  01  HEADER-P-IP-LINE-3.                                          ELTNURSE
00150      05  FILLER                  PIC  X(20) VALUE SPACES.         ELTNURSE
00151      05  FILLER                  PIC  X(38) VALUE                 ELTNURSE
00152              'NURSING SERVICE PROFESSIONAL INPATIENT'.            ELTNURSE
00153      05  FILLER                  PIC  X(21) VALUE LOW-VALUES.     ELTNURSE
00154                                                                   ELTNURSE
00155  01  HEADER-P-OP-LINE-3.                                          ELTNURSE
00156      05  FILLER                  PIC  X(20) VALUE SPACES.         ELTNURSE
00157      05  FILLER                  PIC  X(39) VALUE                 ELTNURSE
00158              'NURSING SERVICE PROFESSIONAL OUTPATIENT'.           ELTNURSE
00159      05  FILLER                  PIC  X(20) VALUE LOW-VALUES.     ELTNURSE
00160                                                                   ELTNURSE
00161  01  WS-SERVICES-RENDERED.                                        ELTNURSE
00162      05  FILLER                  PIC  X(26) VALUE                 ELTNURSE
00163              'SERVICES MAY BE RENDERED: '.                        ELTNURSE
00164      05  FILLER                  PIC  X(53) VALUE LOW-VALUES.     ELTNURSE
00165                                                                   ELTNURSE
00166  01  WS-FOLLOWING-BEN.                                            ELTNURSE
00167      05  FILLER                  PIC  X(79) VALUE                 ELTNURSE
00168            'COVERED SERVICES ARE:  '.                             ELTNURSE
00169                                                                   ELTNURSE
00170  01  WS-PAYABLE-AS.                                               ELTNURSE
00171      10  FILLER                  PIC  X(40) VALUE                 ELTNURSE
00172          'THESE SERVICES ARE PRICED ACCORDING TO: '.              ELTNURSE
00173                                                                   ELTNURSE
00174  01  WS-CONTRACT-RELATED.                                         ELTNURSE
00175      05  FILLER                  PIC  X(49) VALUE                 ELTNURSE
00176            'THIS CONTRACT HAS RELATED SERVICE CONSIDERATIONS.'.   ELTNURSE
00177                                                                   ELTNURSE
00178  01  WS-BASIC.                                                    ELTNURSE
00179      05  WS-BASIC-LIT            PIC  X(16) VALUE                 ELTNURSE
00180              '         BASIC: '.                                  ELTNURSE
00181      05  WS-DTL-BASIC-LONG.                                       ELTNURSE
00182          15  WS-DTL-BASIC        PIC  X(50) VALUE SPACES.         ELTNURSE
00183          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTNURSE
00184                                                                   ELTNURSE
00185  01  WS-SUPPLEMENTAL.                                             ELTNURSE
00186      05  WS-SUPP-LIT             PIC  X(16) VALUE                 ELTNURSE
00187              '  SUPPLEMENTAL: '.                                  ELTNURSE
00188      05  WS-DTL-SUPP-LONG.                                        ELTNURSE
00189          15  WS-DTL-SUPPLEMENTAL PIC  X(50) VALUE SPACES.         ELTNURSE
00190          15  FILLER              PIC  X(13) VALUE LOW-VALUES.     ELTNURSE
00191                                                                   ELTNURSE
00192  01  WS-MAXIMUM-AMT.                                              ELTNURSE
00193      05  FILLER                  PIC  X(32) VALUE                 ELTNURSE
00194              'THE MAXIMUM AMOUNT PER VISIT IS '.                  ELTNURSE
00195      05  FILLER                  PIC  X(47) VALUE LOW-VALUES.     ELTNURSE
00196                                                                   ELTNURSE
00197  01  WS-PVE.                                                      ELTNURSE
00198      05  FILLER                  PIC  X(44) VALUE                 ELTNURSE
00199              'CHECK THE CONTRACT FOR PROVIDER ELIGIBILITY.'.      ELTNURSE
00200      05  FILLER                  PIC  X(35) VALUE LOW-VALUES.     ELTNURSE
00201                                                                   ELTNURSE
00202  01  WS-ACCUM-MSG1.                                               ELTNURSE
00203      05  FILLER                  PIC  X(79) VALUE                 ELTNURSE
00204      'SEE COINSURANCE, DEDUCTIBLE, MAXIMUMS AND OUT-OF-POCKET FOR ELTNURSE
00205 -    'CONSIDERATIONS.'.                                           ELTNURSE
00206                                                                   ELTNURSE
00207  01  WS-INDICES-PROBLEM.                                          ELTNURSE
00208      05  FILLER                  PIC  X(20) VALUE                 ELTNURSE
00209              'PROBLEM WITH INDICES'.                              ELTNURSE
00210      05  FILLER                  PIC  X(59) VALUE LOW-VALUES.     ELTNURSE
00211                                                                   ELTNURSE
00212  01  WS-CERT-REQ.                                                 ELTNURSE
00213      05  FILLER                   PIC  X(48) VALUE                ELTNURSE
00214              'THE CERTIFICATION REQUIRED FOR THIS SERVICE IS: '.  ELTNURSE
00215      05  FILLER                   PIC  X(31) VALUE LOW-VALUES.    ELTNURSE
00216                                                                   ELTNURSE
00217  01  WS-RECERT-REQ.                                               ELTNURSE
00218      05  FILLER                   PIC  X(56) VALUE                ELTNURSE
00219              'THE REQUIREMENT FOR RECERTIFICATION OF THIS SERVICE ELTNURSE
00220 -            'IS: '.                                              ELTNURSE
00221      05  FILLER                   PIC  X(23) VALUE LOW-VALUES.    ELTNURSE
00222                                                                   ELTNURSE
00223  01  WS-POSSIBLE-ERROR.                                           ELTNURSE
00224      05  FILLER                   PIC  X(50) VALUE                ELTNURSE
00225              'POSSIBLE ERROR CONDITION, PLEASE SEE A TECHINICIAN'.ELTNURSE
00226      05  FILLER                   PIC  X(29) VALUE LOW-VALUES.    ELTNURSE
00227                                                                   ELTNURSE
00228  01  WS-INVALID-REQ.                                              ELTNURSE
00229      05  FILLER                  PIC  X(37) VALUE                 ELTNURSE
00230              '*** I N V A L I D   R E Q U E S T ***'.             ELTNURSE
00231      05  FILLER                  PIC  X(42) VALUE LOW-VALUES.     ELTNURSE
00232                                                                   ELTNURSE
00233  01  WS-SPILLOVER.                                                ELTNURSE
00234      05  FILLER                  PIC  X(10) VALUE                 ELTNURSE
00235              'SPILLOVER '.                                        ELTNURSE
00236                                                                   ELTNURSE
00237  01  WS-OTHER-LITERALS.                                           ELTNURSE
00238    05  WS-NO-TABULAR1.                                            ELTNURSE
00239      10  FILLER                    PIC X(51)  VALUE               ELTNURSE
00240         '*** FOUND A GENERIC CONTRACT FILE INCONSISTANCY IN '.    ELTNURSE
00241      10  FILLER                    PIC X(22)  VALUE               ELTNURSE
00242         'GOING FROM BENEFIT ***'.                                 ELTNURSE
00243                                                                   ELTNURSE
00244    05  WS-NO-TABULAR2.                                            ELTNURSE
00245      10  FILLER                    PIC X(15)  VALUE               ELTNURSE
00246         '*** PROVISION: '.                                        ELTNURSE
00247      10  WS-NO-TAB-BEN-PR          PIC X(6).                      ELTNURSE
00248      10  FILLER                    PIC X VALUE SPACE.             ELTNURSE
00249      10  WS-NO-TAB-BEN-SLOT        PIC 9(7).                      ELTNURSE
00250      10  FILLER                    PIC X(13)  VALUE               ELTNURSE
00251         ' TO TABULAR: '.                                          ELTNURSE
00252      10  WS-NO-TAB-ID              PIC X(6).                      ELTNURSE
00253      10  FILLER                    PIC X VALUE SPACE.             ELTNURSE
00254      10  WS-NO-TAB-SLOT            PIC 9(7).                      ELTNURSE
00255      10  FILLER                    PIC X(23)  VALUE LOW-VALUES.   ELTNURSE
00256                                                                   ELTNURSE
00257    05  WS-TEMP-TEXT-AREA           PIC X(79).                     ELTNURSE
00258    05  FILLER        REDEFINES     WS-TEMP-TEXT-AREA.             ELTNURSE
00259      10  WS-TEMP-TEXT-CHAR         PIC X  OCCURS  79  TIMES.      ELTNURSE
00260                                                                   ELTNURSE
00261                                                                   ELTNURSE
00262 /                                                                 ELTNURSE
00263  LINKAGE SECTION.                                                 ELTNURSE
00264  01  DFHCOMMAREA.                                                 ELTNURSE
00265      COPY ELSCOMMC.                                               ELTNURSE
00266 /                                                                 ELTNURSE
00267      COPY ELSCIA2C.                                               ELTNURSE
00268                                                                   ELTNURSE
00269 ***  IO PARM AREA  ***                                            ELTNURSE
00270      COPY ELSIOPMC.                                               ELTNURSE
00271 /                                                                 ELTNURSE
00272      COPY ELSKEYSC.                                               ELTNURSE
00273 /                                                                 ELTNURSE
00274      COPY ELSOUTPC.                                               ELTNURSE
00275 /                                                                 ELTNURSE
00276      COPY ELSSSCBC.                                               ELTNURSE
00277 /                                                                 ELTNURSE
00278      COPY ELSCMIFC.                                               ELTNURSE
00279 /                                                                 ELTNURSE
00280      COPY ELSCMDSC.                                               ELTNURSE
00281 /                                                                 ELTNURSE
00282      COPY ELSPRVNC.                                               ELTNURSE
00283 /                                                                 ELTNURSE
00284 *** PAYMENT LEVEL FLD REQUEST INDS ***                            ELTNURSE
00285      COPY ELSPLGSW.                                               ELTNURSE
00286 *** BENEFIT PROVISION TABLE OF FLDS                               ELTNURSE
00287      COPY ELSPLGTB.                                               ELTNURSE
00288 /                                                                 ELTNURSE
00289      COPY ELSTCWAC.                                               ELTNURSE
00290 /                                                                 ELTNURSE
00291  PROCEDURE DIVISION.                                              ELTNURSE
00292  0000-MAINLINE.                                                   ELTNURSE
00293                                                                   ELTNURSE
00294      PERFORM 1000-INITIALIZATION                                  ELTNURSE
00295         THRU 1000-EXIT.                                           ELTNURSE
00296                                                                   ELTNURSE
00297      PERFORM 2000-PROCESS-RTN                                     ELTNURSE
00298         THRU 2000-EXIT.                                           ELTNURSE
00299                                                                   ELTNURSE
00300      SET CIA-ELSPRVN-DDN  TO TRUE.                                ELTNURSE
00301      SET CIA-STG-FREEMAIN TO TRUE.                                ELTNURSE
00302      EXEC CICS LINK                                               ELTNURSE
00303                PROGRAM('ELUSTGMG')                                ELTNURSE
00304                COMMAREA(DFHCOMMAREA)                              ELTNURSE
00305      END-EXEC.                                                    ELTNURSE
00306                                                                   ELTNURSE
00307                                                                   ELTNURSE
00308      EXEC CICS RETURN END-EXEC.                                   ELTNURSE
00309                                                                   ELTNURSE
00310      GOBACK.                                                      ELTNURSE
00311 /                                                                 ELTNURSE
00312  1000-INITIALIZATION.                                             ELTNURSE
00313 ****************************************************************  ELTNURSE
00314 *         INITIALIZATION FOR THE START OF THE PROGRAM.         *  ELTNURSE
00315 ****************************************************************  ELTNURSE
00316                                                                   ELTNURSE
00317      IF EIBCALEN NOT EQUAL LENGTH OF DFHCOMMAREA                  ELTNURSE
00318          EXEC CICS ABEND                                          ELTNURSE
00319                    ABCODE ('EL01')                                ELTNURSE
00320          END-EXEC                                                 ELTNURSE
00321      END-IF.                                                      ELTNURSE
00322                                                                   ELTNURSE
00323 ***  ADDRESS DATA AREAS USING PASSED POINTERS  ***                ELTNURSE
00324                                                                   ELTNURSE
00325      CALL 'ELUINISM' USING DFHCOMMAREA                            ELTNURSE
00326          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTNURSE
00327                                                                   ELTNURSE
00328      SET CIA-ELSSSCB-DDN TO TRUE.                                 ELTNURSE
00329      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00330          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTNURSE
00331                                                                   ELTNURSE
00332      SET CIA-ELSOUTP-DDN TO TRUE.                                 ELTNURSE
00333      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00334          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTNURSE
00335                                                                   ELTNURSE
00336      SET CIA-ELSKEYS-DDN TO TRUE.                                 ELTNURSE
00337      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00338          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTNURSE
00339                                                                   ELTNURSE
00340      SET CIA-ELSCMIF-DDN TO TRUE.                                 ELTNURSE
00341      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00342          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTNURSE
00343                                                                   ELTNURSE
00344      SET CIA-ELSTCWA-DDN TO TRUE.                                 ELTNURSE
00345      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00346          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTNURSE
00347                                                                   ELTNURSE
00348      SET CIA-ELSPLGSW-DDN TO TRUE.                                ELTNURSE
00349      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00350          ADDRESS OF PLS-PAYMENT-LEVEL-SWITCHES.                   ELTNURSE
00351                                                                   ELTNURSE
00352      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTNURSE
00353                                                                   ELTNURSE
00354      COMPUTE CIA-AREA-LEN = LENGTH OF PVN-FIXED-PART +            ELTNURSE
00355              (TABLE-MAX * LENGTH OF PVN-BEN-PROVN-TBL).           ELTNURSE
00356                                                                   ELTNURSE
00357      SET CIA-STG-GETMAIN TO TRUE.                                 ELTNURSE
00358      EXEC CICS LINK                                               ELTNURSE
00359                PROGRAM('ELUSTGMG')                                ELTNURSE
00360                COMMAREA(DFHCOMMAREA)                              ELTNURSE
00361      END-EXEC.                                                    ELTNURSE
00362                                                                   ELTNURSE
00363      SET CIA-ELSPRVN-DDN TO TRUE.                                 ELTNURSE
00364      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00365          ADDRESS OF PVN-BENEFIT-PROVISION-LIST.                   ELTNURSE
00366                                                                   ELTNURSE
00367      INITIALIZE CMF-CODES-MANUAL-INTERFACE.                       ELTNURSE
00368                                                                   ELTNURSE
00369  1000-EXIT.  EXIT.                                                ELTNURSE
00370 /                                                                 ELTNURSE
00371  2000-PROCESS-RTN.                                                ELTNURSE
00372 ****************************************************************  ELTNURSE
00373 *               NURSING TOPIC PROCESSING                       *  ELTNURSE
00374 ****************************************************************  ELTNURSE
00375                                                                   ELTNURSE
00376      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTNURSE
00377                       AND                                         ELTNURSE
00378         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTNURSE
00379          PERFORM 3000-INSTITUTIONAL-IP THRU 3000-EXIT.            ELTNURSE
00380                                                                   ELTNURSE
00381      IF (SSB-PROV-CLASS-INST    OR  SSB-PROV-CLASS-BOTH)          ELTNURSE
00382                       AND                                         ELTNURSE
00383         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTNURSE
00384          PERFORM 4000-INSTITUTIONAL-OP THRU 4000-EXIT.            ELTNURSE
00385                                                                   ELTNURSE
00386      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTNURSE
00387                       AND                                         ELTNURSE
00388         (SSB-SERV-CLASS-IP   OR  SSB-SERV-CLASS-BOTH)             ELTNURSE
00389          PERFORM 5000-PROFESSIONAL-IP THRU 5000-EXIT.             ELTNURSE
00390                                                                   ELTNURSE
00391      IF (SSB-PROV-CLASS-PROF   OR  SSB-PROV-CLASS-BOTH)           ELTNURSE
00392                       AND                                         ELTNURSE
00393         (SSB-SERV-CLASS-OP   OR  SSB-SERV-CLASS-BOTH)             ELTNURSE
00394          PERFORM 6000-PROFESSIONAL-OP THRU 6000-EXIT.             ELTNURSE
00395                                                                   ELTNURSE
00396      IF (SSB-PROV-CLASS-INST OR                                   ELTNURSE
00397          SSB-PROV-CLASS-BOTH OR                                   ELTNURSE
00398          SSB-PROV-CLASS-PROF)                                     ELTNURSE
00399                         AND                                       ELTNURSE
00400         (SSB-SERV-CLASS-IP   OR                                   ELTNURSE
00401          SSB-SERV-CLASS-OP   OR                                   ELTNURSE
00402          SSB-SERV-CLASS-BOTH)                                     ELTNURSE
00403            CONTINUE                                               ELTNURSE
00404      ELSE                                                         ELTNURSE
00405          MOVE ' '             TO  COF-FUNCTION                    ELTNURSE
00406          MOVE +0              TO  COF-NBR-HDR-LINES               ELTNURSE
00407          MOVE +2              TO  COF-NBR-DTL-LINES               ELTNURSE
00408          MOVE WS-INVALID-REQ  TO  COF-DTL-LINE (2)                ELTNURSE
00409          EXEC CICS  LINK  PROGRAM('ELUOUTPT')                     ELTNURSE
00410                           COMMAREA(DFHCOMMAREA)                   ELTNURSE
00411          END-EXEC                                                 ELTNURSE
00412      END-IF.                                                      ELTNURSE
00413                                                                   ELTNURSE
00414      MOVE 'E'   TO  COF-FUNCTION.                                 ELTNURSE
00415      MOVE ZERO  TO  COF-NBR-HDR-LINES, COF-NBR-DTL-LINES.         ELTNURSE
00416                                                                   ELTNURSE
00417      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
00418                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
00419      END-EXEC.                                                    ELTNURSE
00420                                                                   ELTNURSE
00421  2000-EXIT.  EXIT.                                                ELTNURSE
00422 /                                                                 ELTNURSE
00423 ****************************************************************  ELTNURSE
00424 *          NURSING INSTITUTIONAL INPATIENT PROCESSING          *  ELTNURSE
00425 ****************************************************************  ELTNURSE
00426  3000-INSTITUTIONAL-IP.                                           ELTNURSE
00427                                                                   ELTNURSE
00428      MOVE 'Y'                 TO  WS-FIRSTTIME-IND.               ELTNURSE
00429      MOVE HEADER-I-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTNURSE
00430                                                                   ELTNURSE
00431      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTNURSE
00432         THRU 9100-EXIT.                                           ELTNURSE
00433                                                                   ELTNURSE
00434      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
00435      PERFORM WITH TEST BEFORE                                     ELTNURSE
00436              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTNURSE
00437              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTNURSE
00438         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTNURSE
00439         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTNURSE
00440         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTNURSE
00441         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTNURSE
00442      END-PERFORM.                                                 ELTNURSE
00443      MOVE WS-INST-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
00444                                                                   ELTNURSE
00445                                                                   ELTNURSE
00446      PERFORM 3010-MOVE-IN-INST-IP-TABS                            ELTNURSE
00447         THRU 3010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
00448                        UNTIL   WS-SUB  >     WS-INST-IP-CNT.      ELTNURSE
00449                                                                   ELTNURSE
00450      PERFORM 3020-CALL-COVERAGE                                   ELTNURSE
00451         THRU 3020-EXIT.                                           ELTNURSE
00452                                                                   ELTNURSE
00453      IF PVN-COVG-NONE                                             ELTNURSE
00454          GO TO 3000-EXIT.                                         ELTNURSE
00455                                                                   ELTNURSE
00456      PERFORM 3030-FIND-FIRST-NONZERO                              ELTNURSE
00457         THRU 3030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
00458                        UNTIL   WS-SUB  > WS-INST-IP-CNT.          ELTNURSE
00459                                                                   ELTNURSE
00460  3000-EXIT.  EXIT.                                                ELTNURSE
00461 /                                                                 ELTNURSE
00462  3010-MOVE-IN-INST-IP-TABS.                                       ELTNURSE
00463      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
00464      MOVE WS-INST-IP-BP (WS-SUB)                                  ELTNURSE
00465                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTNURSE
00466                                                                   ELTNURSE
00467      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTNURSE
00468                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTNURSE
00469                                                                   ELTNURSE
00470  3010-EXIT.  EXIT.                                                ELTNURSE
00471      SKIP3                                                        ELTNURSE
00472  3020-CALL-COVERAGE.                                              ELTNURSE
00473                                                                   ELTNURSE
00474      MOVE 'NURSING SERVICES ' TO SSB-TOPIC-PHRASE.                ELTNURSE
00475                                                                   ELTNURSE
00476      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTNURSE
00477                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
00478                     END-EXEC.                                     ELTNURSE
00479                                                                   ELTNURSE
00480      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
00481                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
00482                     END-EXEC.                                     ELTNURSE
00483                                                                   ELTNURSE
00484      IF PVN-COVG-NONE                                             ELTNURSE
00485          GO TO 3020-EXIT.                                         ELTNURSE
00486                                                                   ELTNURSE
00487      MOVE +1  TO  WS-CIA.                                         ELTNURSE
00488                                                                   ELTNURSE
00489      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTNURSE
00490                    PSP-PROVN-PRICING-METHD,                       ELTNURSE
00491                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTNURSE
00492                    PSP-TRANSF-OTHER-RESP-IND,                     ELTNURSE
00493                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTNURSE
00494                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTNURSE
00495                    PSP-SPILL-OVER-DED-APL-IND,                    ELTNURSE
00496                    PSP-CERTFN-REQRM-IND,                          ELTNURSE
00497                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTNURSE
00498                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTNURSE
00499                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTNURSE
00500                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTNURSE
00501                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTNURSE
00502                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTNURSE
00503                    PSB-CERTN-REPETN-REQRD-IND,                    ELTNURSE
00504                    PSB-MAX-AMT-PER-VISIT.                         ELTNURSE
00505                                                                   ELTNURSE
00506      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTNURSE
00507                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
00508      END-EXEC.                                                    ELTNURSE
00509                                                                   ELTNURSE
00510      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTNURSE
00511      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
00512          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTNURSE
00513                                                                   ELTNURSE
00514  3020-EXIT.  EXIT.                                                ELTNURSE
00515 /                                                                 ELTNURSE
00516  3030-FIND-FIRST-NONZERO.                                         ELTNURSE
00517      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
00518                                                                   ELTNURSE
00519      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTNURSE
00520          CONTINUE                                                 ELTNURSE
00521      ELSE                                                         ELTNURSE
00522          PERFORM 3100-BUILD-SCREEN-LINES                          ELTNURSE
00523             THRU 3100-EXIT.                                       ELTNURSE
00524                                                                   ELTNURSE
00525  3030-EXIT.  EXIT.                                                ELTNURSE
00526 /                                                                 ELTNURSE
00527  3100-BUILD-SCREEN-LINES.                                         ELTNURSE
00528      SET PLT-INDEX1  TO                                           ELTNURSE
00529              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTNURSE
00530                                                                   ELTNURSE
00531      MOVE  +1  TO  WS-CIA.                                        ELTNURSE
00532                                                                   ELTNURSE
00533      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTNURSE
00534          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTNURSE
00535              SET PLT-INDEX2  TO  2                                ELTNURSE
00536          ELSE                                                     ELTNURSE
00537              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTNURSE
00538              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTNURSE
00539                 THRU 9200-EXIT                                    ELTNURSE
00540              GO TO 3100-EXIT                                      ELTNURSE
00541      ELSE                                                         ELTNURSE
00542          SET PLT-INDEX2  TO  1.                                   ELTNURSE
00543                                                                   ELTNURSE
00544      PERFORM 3105-LIST-BEN-PROV                                   ELTNURSE
00545         THRU 3105-EXIT.                                           ELTNURSE
00546                                                                   ELTNURSE
00547      PERFORM 3110-PLACE-OF-TREATMENT                              ELTNURSE
00548         THRU 3110-EXIT.                                           ELTNURSE
00549                                                                   ELTNURSE
00550      PERFORM 3120-PRIC-METH                                       ELTNURSE
00551         THRU 3120-EXIT.                                           ELTNURSE
00552                                                                   ELTNURSE
00553      PERFORM 3130-MAX-PER-VISIT                                   ELTNURSE
00554         THRU 3130-EXIT.                                           ELTNURSE
00555                                                                   ELTNURSE
00556      PERFORM 3140-CERTIFICATION                                   ELTNURSE
00557         THRU 3140-EXIT.                                           ELTNURSE
00558                                                                   ELTNURSE
00559      PERFORM 3150-RECERTIFICATION                                 ELTNURSE
00560         THRU 3150-EXIT.                                           ELTNURSE
00561                                                                   ELTNURSE
00562      PERFORM 3160-SPILLOVR-COINS-N-DEDUC                          ELTNURSE
00563         THRU 3160-EXIT.                                           ELTNURSE
00564                                                                   ELTNURSE
00565      PERFORM 3165-TRANS-OTHR-RESPON-IND  THRU                     ELTNURSE
00566              3165-EXIT.                                           ELTNURSE
00567                                                                   ELTNURSE
00568      PERFORM 7000-ALL-LEVEL-TABS                                  ELTNURSE
00569         THRU 7000-EXIT.                                           ELTNURSE
00570                                                                   ELTNURSE
00571  3100-EXIT.  EXIT.                                                ELTNURSE
00572 /                                                                 ELTNURSE
00573  3105-LIST-BEN-PROV.                                              ELTNURSE
00574 ****************************************************************  ELTNURSE
00575 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTNURSE
00576 ****************************************************************  ELTNURSE
00577      MOVE  +2               TO  WS-CIA.                           ELTNURSE
00578      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTNURSE
00579      MOVE ZERO              TO  WS-SUB2.                          ELTNURSE
00580      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTNURSE
00581                             TO  WS-SUB3.                          ELTNURSE
00582                                                                   ELTNURSE
00583      PERFORM 3106-ZERO-ALL-WITH-SAME-NO                           ELTNURSE
00584         THRU 3106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTNURSE
00585                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-IP-CNT.ELTNURSE
00586                                                                   ELTNURSE
00587      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
00588      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTNURSE
00589                                                                   ELTNURSE
00590      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
00591                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
00592                     END-EXEC.                                     ELTNURSE
00593                                                                   ELTNURSE
00594  3105-EXIT.  EXIT.                                                ELTNURSE
00595      SKIP3                                                        ELTNURSE
00596  3106-ZERO-ALL-WITH-SAME-NO.                                      ELTNURSE
00597                                                                   ELTNURSE
00598      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTNURSE
00599          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTNURSE
00600          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTNURSE
00601          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTNURSE
00602                            TO  CMF-CODE-VALUE                     ELTNURSE
00603          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTNURSE
00604          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTNURSE
00605          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00606             THRU 9500-EXIT                                        ELTNURSE
00607          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTNURSE
00608          ADD  +1    TO  WS-SUB2                                   ELTNURSE
00609          IF WS-CIA  >  20  OR  =  20                              ELTNURSE
00610              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTNURSE
00611              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTNURSE
00612                             COMMAREA (DFHCOMMAREA)                ELTNURSE
00613                             END-EXEC                              ELTNURSE
00614              MOVE  +1  TO  WS-CIA.                                ELTNURSE
00615                                                                   ELTNURSE
00616  3106-EXIT.  EXIT.                                                ELTNURSE
00617 /                                                                 ELTNURSE
00618  3110-PLACE-OF-TREATMENT.                                         ELTNURSE
00619 ****************************************************************  ELTNURSE
00620 *              P L A C E   O F   T R E A T M E N T             *  ELTNURSE
00621 ****************************************************************  ELTNURSE
00622      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00623      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00624                          AND                                      ELTNURSE
00625         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
00626                                                  NOT =  ZERO      ELTNURSE
00627          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00628          MOVE +2                    TO  WS-CIA                    ELTNURSE
00629          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00630                                                                   ELTNURSE
00631      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00632      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00633                           AND                                     ELTNURSE
00634         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
00635                                                 NOT  =  ZERO      ELTNURSE
00636                           AND                                     ELTNURSE
00637         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
00638          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00639          MOVE +2                    TO  WS-CIA                    ELTNURSE
00640          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00641                                                                   ELTNURSE
00642      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00643      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00644                           AND                                     ELTNURSE
00645         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
00646                                                  NOT  =  ZERO     ELTNURSE
00647          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
00648          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
00649                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00650          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
00651                               TO  CMF-CODE-VALUE                  ELTNURSE
00652          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
00653          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00654          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00655             THRU 9500-EXIT.                                       ELTNURSE
00656                                                                   ELTNURSE
00657      SET PLT-INDEX2  TO  2.                                       ELTNURSE
00658      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00659                           AND                                     ELTNURSE
00660         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
00661                                                 NOT  =  ZERO      ELTNURSE
00662          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
00663          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
00664                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00665          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
00666                               TO  CMF-CODE-VALUE                  ELTNURSE
00667          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
00668          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00669          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00670             THRU 9500-EXIT.                                       ELTNURSE
00671                                                                   ELTNURSE
00672      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
00673         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
00674          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
00675             THRU 9200-EXIT.                                       ELTNURSE
00676                                                                   ELTNURSE
00677      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
00678         SET PLT-INDEX2  TO  2                                     ELTNURSE
00679      ELSE                                                         ELTNURSE
00680         SET PLT-INDEX2  TO  1.                                    ELTNURSE
00681                                                                   ELTNURSE
00682  3110-EXIT.  EXIT.                                                ELTNURSE
00683 /                                                                 ELTNURSE
00684  3120-PRIC-METH.                                                  ELTNURSE
00685 ****************************************************************  ELTNURSE
00686 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTNURSE
00687 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTNURSE
00688 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTNURSE
00689 ****************************************************************  ELTNURSE
00690      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00691      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
00692         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
00693                                                              '19' ELTNURSE
00694         MOVE +2             TO  WS-CIA                            ELTNURSE
00695         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
00696         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
00697                                                                   ELTNURSE
00698      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00699      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTNURSE
00700         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
00701                                                        '19' AND   ELTNURSE
00702         NOT WS-ADD-A-BLANK-LINE                                   ELTNURSE
00703         MOVE +2             TO  WS-CIA                            ELTNURSE
00704         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
00705         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
00706                                                                   ELTNURSE
00707      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00708      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
00709         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
00710                            AND                                    ELTNURSE
00711         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00712         SET  PLT-INDEX2  TO  2                                    ELTNURSE
00713         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
00714                                                             ZERO  ELTNURSE
00715            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
00716            ADD +1  TO  WS-CIA                                     ELTNURSE
00717            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
00718                                                                   ELTNURSE
00719      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00720      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
00721         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
00722                            AND                                    ELTNURSE
00723         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTNURSE
00724         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTNURSE
00725         ADD +1  TO  WS-CIA                                        ELTNURSE
00726         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTNURSE
00727                                                                   ELTNURSE
00728      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTNURSE
00729         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00730         SET  PLT-INDEX2  TO  2                                    ELTNURSE
00731         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
00732                                                             ZERO  ELTNURSE
00733            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
00734            ADD +1  TO  WS-CIA                                     ELTNURSE
00735            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
00736                                                                   ELTNURSE
00737      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00738      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00739         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
00740                                                            =  ZEROELTNURSE
00741            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00742                                                            =  ZEROELTNURSE
00743               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
00744            ELSE                                                   ELTNURSE
00745               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
00746          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00747                                                  TO  WS-PERCENTAGEELTNURSE
00748         ELSE                                                      ELTNURSE
00749            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
00750          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00751                                                 TO  WS-PERCENTAGE.ELTNURSE
00752      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
00753         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
00754                                             ZERO AND  NOT =  '19' ELTNURSE
00755         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
00756         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
00757         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
00758                                                    CMF-CODE-VALUE ELTNURSE
00759         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
00760         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
00761         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
00762            THRU 9600-EXIT.                                        ELTNURSE
00763                                                                   ELTNURSE
00764      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00765      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00766         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTNURSE
00767                                                               ZEROELTNURSE
00768            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00769                                                            =  ZEROELTNURSE
00770               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
00771            ELSE                                                   ELTNURSE
00772               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
00773          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00774                                                  TO  WS-PERCENTAGEELTNURSE
00775         ELSE                                                      ELTNURSE
00776            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
00777          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
00778                                                 TO  WS-PERCENTAGE.ELTNURSE
00779      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTNURSE
00780         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
00781                                             ZERO AND  NOT =  '19' ELTNURSE
00782         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
00783         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
00784         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
00785                                                    CMF-CODE-VALUE ELTNURSE
00786         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTNURSE
00787         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
00788         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
00789            THRU 9600-EXIT.                                        ELTNURSE
00790                                                                   ELTNURSE
00791      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
00792          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTNURSE
00793          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
00794             THRU 9200-EXIT.                                       ELTNURSE
00795                                                                   ELTNURSE
00796      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
00797         SET PLT-INDEX2  TO  2                                     ELTNURSE
00798      ELSE                                                         ELTNURSE
00799         SET PLT-INDEX2  TO  1.                                    ELTNURSE
00800                                                                   ELTNURSE
00801  3120-EXIT.  EXIT.                                                ELTNURSE
00802 /                                                                 ELTNURSE
00803  3130-MAX-PER-VISIT.                                              ELTNURSE
00804 ****************************************************************  ELTNURSE
00805 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTNURSE
00806 ****************************************************************  ELTNURSE
00807      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00808      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00809                          AND                                      ELTNURSE
00810         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
00811                                                  NOT =  ZERO      ELTNURSE
00812          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00813          MOVE +2                    TO  WS-CIA                    ELTNURSE
00814          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00815                                                                   ELTNURSE
00816      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00817      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00818                           AND                                     ELTNURSE
00819         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
00820                                                 NOT  =  ZERO      ELTNURSE
00821                           AND                                     ELTNURSE
00822         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
00823          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00824          MOVE +2                    TO  WS-CIA                    ELTNURSE
00825          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00826                                                                   ELTNURSE
00827      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00828      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00829                           AND                                     ELTNURSE
00830         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
00831                                                  NOT  =  ZERO     ELTNURSE
00832          MOVE WS-BASIC-LIT     TO  WS-BASIC-SUPP                  ELTNURSE
00833          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
00834                                TO  WS-EDIT-MAX-AMT                ELTNURSE
00835          ADD  +1               TO  WS-CIA                         ELTNURSE
00836          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
00837                                                                   ELTNURSE
00838      SET PLT-INDEX2  TO  2.                                       ELTNURSE
00839      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00840                           AND                                     ELTNURSE
00841         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
00842                                                 NOT  =  ZERO      ELTNURSE
00843          MOVE WS-SUPP-LIT      TO  WS-BASIC-SUPP                  ELTNURSE
00844          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
00845                                TO  WS-EDIT-MAX-AMT                ELTNURSE
00846          ADD  +1               TO  WS-CIA                         ELTNURSE
00847          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
00848                                                                   ELTNURSE
00849      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
00850         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
00851          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
00852             THRU 9200-EXIT.                                       ELTNURSE
00853                                                                   ELTNURSE
00854      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
00855         SET PLT-INDEX2  TO  2                                     ELTNURSE
00856      ELSE                                                         ELTNURSE
00857         SET PLT-INDEX2  TO  1.                                    ELTNURSE
00858                                                                   ELTNURSE
00859  3130-EXIT.  EXIT.                                                ELTNURSE
00860 /                                                                 ELTNURSE
00861  3140-CERTIFICATION.                                              ELTNURSE
00862 ****************************************************************  ELTNURSE
00863 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTNURSE
00864 ****************************************************************  ELTNURSE
00865      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00866      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00867                          AND                                      ELTNURSE
00868         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
00869                                                  NOT =  '00'      ELTNURSE
00870          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00871          MOVE +2                    TO  WS-CIA                    ELTNURSE
00872          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00873                                                                   ELTNURSE
00874      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00875      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00876                           AND                                     ELTNURSE
00877         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
00878                                                 NOT  =  '00'      ELTNURSE
00879                           AND                                     ELTNURSE
00880         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
00881          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00882          MOVE +2                    TO  WS-CIA                    ELTNURSE
00883          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00884                                                                   ELTNURSE
00885      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00886      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00887                           AND                                     ELTNURSE
00888         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
00889                                                  NOT  =  '00'     ELTNURSE
00890          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTNURSE
00891          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
00892                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00893          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00894                               TO  CMF-CODE-VALUE                  ELTNURSE
00895          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
00896          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00897          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00898             THRU 9500-EXIT.                                       ELTNURSE
00899                                                                   ELTNURSE
00900      SET PLT-INDEX2  TO  2.                                       ELTNURSE
00901      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00902                           AND                                     ELTNURSE
00903         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
00904                                                 NOT  =  '00'      ELTNURSE
00905          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTNURSE
00906          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
00907                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00908          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00909                               TO  CMF-CODE-VALUE                  ELTNURSE
00910          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
00911          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00912          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00913             THRU 9500-EXIT.                                       ELTNURSE
00914                                                                   ELTNURSE
00915      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
00916         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
00917          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
00918             THRU 9200-EXIT.                                       ELTNURSE
00919                                                                   ELTNURSE
00920      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
00921         SET PLT-INDEX2  TO  2                                     ELTNURSE
00922      ELSE                                                         ELTNURSE
00923         SET PLT-INDEX2  TO  1.                                    ELTNURSE
00924                                                                   ELTNURSE
00925  3140-EXIT.  EXIT.                                                ELTNURSE
00926 /                                                                 ELTNURSE
00927  3150-RECERTIFICATION.                                            ELTNURSE
00928 ****************************************************************  ELTNURSE
00929 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTNURSE
00930 ****************************************************************  ELTNURSE
00931      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00932      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00933                          AND                                      ELTNURSE
00934         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00935                                                  NOT =  ZERO      ELTNURSE
00936          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00937          MOVE +2                    TO  WS-CIA                    ELTNURSE
00938          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00939                                                                   ELTNURSE
00940      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
00941      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00942                           AND                                     ELTNURSE
00943         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00944                                                 NOT  =  ZERO      ELTNURSE
00945                           AND                                     ELTNURSE
00946         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
00947          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
00948          MOVE +2                    TO  WS-CIA                    ELTNURSE
00949          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
00950                                                                   ELTNURSE
00951      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
00952      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
00953                           AND                                     ELTNURSE
00954         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00955                                                  NOT  =  ZERO     ELTNURSE
00956          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTNURSE
00957          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
00958                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00959          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
00960                               TO  CMF-CODE-VALUE                  ELTNURSE
00961          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
00962          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00963          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00964             THRU 9500-EXIT.                                       ELTNURSE
00965                                                                   ELTNURSE
00966      SET PLT-INDEX2  TO  2.                                       ELTNURSE
00967      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
00968                           AND                                     ELTNURSE
00969         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
00970                                                 NOT  =  ZERO      ELTNURSE
00971          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTNURSE
00972          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
00973                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
00974          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
00975                               TO  CMF-CODE-VALUE                  ELTNURSE
00976          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
00977          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
00978          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
00979             THRU 9500-EXIT.                                       ELTNURSE
00980                                                                   ELTNURSE
00981      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
00982         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
00983          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
00984             THRU 9200-EXIT.                                       ELTNURSE
00985                                                                   ELTNURSE
00986      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
00987         SET PLT-INDEX2  TO  2                                     ELTNURSE
00988      ELSE                                                         ELTNURSE
00989         SET PLT-INDEX2  TO  1.                                    ELTNURSE
00990                                                                   ELTNURSE
00991  3150-EXIT.  EXIT.                                                ELTNURSE
00992 /                                                                 ELTNURSE
00993  3160-SPILLOVR-COINS-N-DEDUC.                                     ELTNURSE
00994 ****************************************************************  ELTNURSE
00995 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTNURSE
00996 ****************************************************************  ELTNURSE
00997      MOVE +1  TO  WS-CIA.                                         ELTNURSE
00998                                                                   ELTNURSE
00999      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01000      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTNURSE
01001                            AND                                    ELTNURSE
01002         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTNURSE
01003                                                  NOT =  '0'       ELTNURSE
01004         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01005         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTNURSE
01006                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
01007         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTNURSE
01008                                                TO  CMF-CODE-VALUE ELTNURSE
01009         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTNURSE
01010         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTNURSE
01011         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
01012            THRU 9500-EXIT                                         ELTNURSE
01013         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
01014            THRU 9200-EXIT.                                        ELTNURSE
01015 ****************************************************************  ELTNURSE
01016 *          S P I L L O V E R   D E D U C T I B L E             *  ELTNURSE
01017 ****************************************************************  ELTNURSE
01018      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01019                                                                   ELTNURSE
01020      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01021      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTNURSE
01022                            AND                                    ELTNURSE
01023         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01024                                                  NOT =  '0'       ELTNURSE
01025         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01026         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTNURSE
01027                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
01028         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01029                                                 TO  CMF-CODE-VALUEELTNURSE
01030         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTNURSE
01031         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTNURSE
01032         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
01033            THRU 9500-EXIT                                         ELTNURSE
01034         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
01035            THRU 9200-EXIT.                                        ELTNURSE
01036                                                                   ELTNURSE
01037      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01038         SET PLT-INDEX2  TO  2                                     ELTNURSE
01039      ELSE                                                         ELTNURSE
01040         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01041                                                                   ELTNURSE
01042  3160-EXIT.  EXIT.                                                ELTNURSE
01043 /                                                                 ELTNURSE
01044 /                                                                 ELTNURSE
01045  3165-TRANS-OTHR-RESPON-IND.                                      ELTNURSE
01046 ****************************************************************  ELTNURSE
01047 *     TRANSFER TO OTHER RESPONSIBILITY INDICATOR               *  ELTNURSE
01048 ****************************************************************  ELTNURSE
01049      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01050                                                                   ELTNURSE
01051      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01052         SET PLT-INDEX2  TO  2                                     ELTNURSE
01053      ELSE                                                         ELTNURSE
01054         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01055                                                                   ELTNURSE
01056      IF PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2) =      ELTNURSE
01057         ZEROS                                                     ELTNURSE
01058         GO TO 3165-EXIT.                                          ELTNURSE
01059                                                                   ELTNURSE
01060      MOVE 'BP'  TO  CMF-RECORD-PREFIX.                            ELTNURSE
01061      MOVE 'TRANSF-OTHER-RESP-IND'  TO     CMF-ELEMENT-SYSTEM-NAME.ELTNURSE
01062      MOVE PLP-TRANSF-OTHER-RESP-IND (PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
01063           CMF-CODE-VALUE.                                         ELTNURSE
01064      MOVE SPACES                   TO  WS-TEMP-TEXT-AREA.         ELTNURSE
01065      MOVE +00                      TO  WS-TEMP-NOT-USED-CNT.      ELTNURSE
01066      PERFORM 9500-CALL-CODES-MANUAL-LONG THRU                     ELTNURSE
01067              9500-EXIT.                                           ELTNURSE
01068      PERFORM 9200-TEXT-OUTPUT-REQUEST  THRU                       ELTNURSE
01069              9200-EXIT.                                           ELTNURSE
01070                                                                   ELTNURSE
01071  3165-EXIT.  EXIT.                                                ELTNURSE
01072 /                                                                 ELTNURSE
01073 ****************************************************************  ELTNURSE
01074 *          NURSING INSTITUTIONAL OUTPATIENT PROCESSING         *  ELTNURSE
01075 ****************************************************************  ELTNURSE
01076  4000-INSTITUTIONAL-OP.                                           ELTNURSE
01077                                                                   ELTNURSE
01078      MOVE 'Y'                 TO  WS-FIRSTTIME-IND.               ELTNURSE
01079      MOVE HEADER-I-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTNURSE
01080                                                                   ELTNURSE
01081      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTNURSE
01082         THRU 9100-EXIT.                                           ELTNURSE
01083                                                                   ELTNURSE
01084      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
01085      PERFORM WITH TEST BEFORE                                     ELTNURSE
01086              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTNURSE
01087              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTNURSE
01088         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTNURSE
01089         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTNURSE
01090         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTNURSE
01091         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTNURSE
01092      END-PERFORM.                                                 ELTNURSE
01093      MOVE WS-INST-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
01094                                                                   ELTNURSE
01095                                                                   ELTNURSE
01096      PERFORM 4010-MOVE-IN-INST-OP-TABS                            ELTNURSE
01097         THRU 4010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
01098                        UNTIL   WS-SUB  >     WS-INST-OP-CNT.      ELTNURSE
01099                                                                   ELTNURSE
01100      PERFORM 4020-CALL-COVERAGE                                   ELTNURSE
01101         THRU 4020-EXIT.                                           ELTNURSE
01102                                                                   ELTNURSE
01103      IF PVN-COVG-NONE                                             ELTNURSE
01104          GO TO 4000-EXIT.                                         ELTNURSE
01105                                                                   ELTNURSE
01106      PERFORM 4030-FIND-FIRST-NONZERO                              ELTNURSE
01107         THRU 4030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
01108                        UNTIL   WS-SUB  > WS-INST-OP-CNT.          ELTNURSE
01109                                                                   ELTNURSE
01110  4000-EXIT.  EXIT.                                                ELTNURSE
01111 /                                                                 ELTNURSE
01112  4010-MOVE-IN-INST-OP-TABS.                                       ELTNURSE
01113      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01114      MOVE WS-INST-OP-BP (WS-SUB)                                  ELTNURSE
01115                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTNURSE
01116                                                                   ELTNURSE
01117      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTNURSE
01118                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTNURSE
01119                                                                   ELTNURSE
01120  4010-EXIT.  EXIT.                                                ELTNURSE
01121 /                                                                 ELTNURSE
01122  4020-CALL-COVERAGE.                                              ELTNURSE
01123                                                                   ELTNURSE
01124      MOVE 'NURSING SERVICES ' TO SSB-TOPIC-PHRASE.                ELTNURSE
01125                                                                   ELTNURSE
01126      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTNURSE
01127                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01128                     END-EXEC.                                     ELTNURSE
01129                                                                   ELTNURSE
01130      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
01131                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01132                     END-EXEC.                                     ELTNURSE
01133                                                                   ELTNURSE
01134      IF PVN-COVG-NONE                                             ELTNURSE
01135          GO TO 4020-EXIT.                                         ELTNURSE
01136                                                                   ELTNURSE
01137      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01138                                                                   ELTNURSE
01139      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTNURSE
01140                    PSP-PROVN-PRICING-METHD,                       ELTNURSE
01141                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTNURSE
01142                    PSP-TRANSF-OTHER-RESP-IND,                     ELTNURSE
01143                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTNURSE
01144                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTNURSE
01145                    PSP-SPILL-OVER-DED-APL-IND,                    ELTNURSE
01146                    PSP-CERTFN-REQRM-IND,                          ELTNURSE
01147                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTNURSE
01148                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTNURSE
01149                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTNURSE
01150                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTNURSE
01151                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTNURSE
01152                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTNURSE
01153                    PSB-CERTN-REPETN-REQRD-IND,                    ELTNURSE
01154                    PSB-MAX-AMT-PER-VISIT.                         ELTNURSE
01155                                                                   ELTNURSE
01156      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTNURSE
01157                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01158                     END-EXEC.                                     ELTNURSE
01159                                                                   ELTNURSE
01160      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTNURSE
01161      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
01162          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTNURSE
01163                                                                   ELTNURSE
01164  4020-EXIT.  EXIT.                                                ELTNURSE
01165 /                                                                 ELTNURSE
01166  4030-FIND-FIRST-NONZERO.                                         ELTNURSE
01167      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01168                                                                   ELTNURSE
01169      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTNURSE
01170          CONTINUE                                                 ELTNURSE
01171      ELSE                                                         ELTNURSE
01172          PERFORM 4100-BUILD-SCREEN-LINES                          ELTNURSE
01173             THRU 4100-EXIT.                                       ELTNURSE
01174                                                                   ELTNURSE
01175  4030-EXIT.  EXIT.                                                ELTNURSE
01176 /                                                                 ELTNURSE
01177  4100-BUILD-SCREEN-LINES.                                         ELTNURSE
01178      SET PLT-INDEX1  TO                                           ELTNURSE
01179              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTNURSE
01180                                                                   ELTNURSE
01181      MOVE  +1  TO  WS-CIA.                                        ELTNURSE
01182                                                                   ELTNURSE
01183      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      =  ZEROS        ELTNURSE
01184          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTNURSE
01185              SET PLT-INDEX2  TO  2                                ELTNURSE
01186          ELSE                                                     ELTNURSE
01187              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTNURSE
01188              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTNURSE
01189                 THRU 9200-EXIT                                    ELTNURSE
01190              GO TO 4100-EXIT                                      ELTNURSE
01191      ELSE                                                         ELTNURSE
01192          SET PLT-INDEX2  TO  1.                                   ELTNURSE
01193                                                                   ELTNURSE
01194      PERFORM 4105-LIST-BEN-PROV                                   ELTNURSE
01195         THRU 4105-EXIT.                                           ELTNURSE
01196                                                                   ELTNURSE
01197      PERFORM 4110-PLACE-OF-TREATMENT                              ELTNURSE
01198         THRU 4110-EXIT.                                           ELTNURSE
01199                                                                   ELTNURSE
01200      PERFORM 4120-PRIC-METH                                       ELTNURSE
01201         THRU 4120-EXIT.                                           ELTNURSE
01202                                                                   ELTNURSE
01203      PERFORM 4130-MAX-PER-VISIT                                   ELTNURSE
01204         THRU 4130-EXIT.                                           ELTNURSE
01205                                                                   ELTNURSE
01206      PERFORM 4140-CERTIFICATION                                   ELTNURSE
01207         THRU 4140-EXIT.                                           ELTNURSE
01208                                                                   ELTNURSE
01209      PERFORM 4150-RECERTIFICATION                                 ELTNURSE
01210         THRU 4150-EXIT.                                           ELTNURSE
01211                                                                   ELTNURSE
01212      PERFORM 4160-SPILLOVR-COINS-N-DEDUC                          ELTNURSE
01213         THRU 4160-EXIT.                                           ELTNURSE
01214                                                                   ELTNURSE
01215      PERFORM 3165-TRANS-OTHR-RESPON-IND  THRU                     ELTNURSE
01216              3165-EXIT.                                           ELTNURSE
01217                                                                   ELTNURSE
01218      PERFORM 7000-ALL-LEVEL-TABS                                  ELTNURSE
01219         THRU 7000-EXIT.                                           ELTNURSE
01220                                                                   ELTNURSE
01221  4100-EXIT.  EXIT.                                                ELTNURSE
01222 /                                                                 ELTNURSE
01223  4105-LIST-BEN-PROV.                                              ELTNURSE
01224 ****************************************************************  ELTNURSE
01225 *     L I S T   O F   B E N E F I T   P R O V I S I O N S      *  ELTNURSE
01226 ****************************************************************  ELTNURSE
01227      MOVE  +2               TO  WS-CIA.                           ELTNURSE
01228      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTNURSE
01229      MOVE ZERO              TO  WS-SUB2.                          ELTNURSE
01230      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTNURSE
01231                             TO  WS-SUB3.                          ELTNURSE
01232                                                                   ELTNURSE
01233      PERFORM 4106-ZERO-ALL-WITH-SAME-NO                           ELTNURSE
01234         THRU 4106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTNURSE
01235                        UNTIL   PVN-BEN-PROVN-IDX > WS-INST-OP-CNT.ELTNURSE
01236                                                                   ELTNURSE
01237      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01238      MOVE WS-CIA    TO  COF-NBR-DTL-LINES.                        ELTNURSE
01239                                                                   ELTNURSE
01240      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
01241                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01242                     END-EXEC.                                     ELTNURSE
01243                                                                   ELTNURSE
01244  4105-EXIT.  EXIT.                                                ELTNURSE
01245      SKIP3                                                        ELTNURSE
01246  4106-ZERO-ALL-WITH-SAME-NO.                                      ELTNURSE
01247                                                                   ELTNURSE
01248      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTNURSE
01249          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTNURSE
01250          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTNURSE
01251          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTNURSE
01252                            TO  CMF-CODE-VALUE                     ELTNURSE
01253          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01254          MOVE  +58         TO  WS-TEMP-NOT-USED-CNT               ELTNURSE
01255          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01256             THRU 9500-EXIT                                        ELTNURSE
01257          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTNURSE
01258          ADD  +1    TO  WS-SUB2                                   ELTNURSE
01259          IF WS-CIA  >  20  OR  =  20                              ELTNURSE
01260              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTNURSE
01261              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTNURSE
01262                             COMMAREA (DFHCOMMAREA)                ELTNURSE
01263                             END-EXEC                              ELTNURSE
01264              MOVE  +1  TO  WS-CIA.                                ELTNURSE
01265                                                                   ELTNURSE
01266  4106-EXIT.  EXIT.                                                ELTNURSE
01267 /                                                                 ELTNURSE
01268  4110-PLACE-OF-TREATMENT.                                         ELTNURSE
01269 ****************************************************************  ELTNURSE
01270 *              P L A C E   O F   T R E A T M E N T             *  ELTNURSE
01271 ****************************************************************  ELTNURSE
01272      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01273      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01274                          AND                                      ELTNURSE
01275         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01276                                                  NOT =  ZERO      ELTNURSE
01277          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01278          MOVE +2                    TO  WS-CIA                    ELTNURSE
01279          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01280                                                                   ELTNURSE
01281      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01282      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01283                           AND                                     ELTNURSE
01284         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01285                                                 NOT  =  ZERO      ELTNURSE
01286                           AND                                     ELTNURSE
01287         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
01288          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01289          MOVE +2                    TO  WS-CIA                    ELTNURSE
01290          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01291                                                                   ELTNURSE
01292      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01293      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01294                           AND                                     ELTNURSE
01295         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01296                                                  NOT  =  ZERO     ELTNURSE
01297          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
01298          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
01299                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01300          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01301                               TO  CMF-CODE-VALUE                  ELTNURSE
01302          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
01303          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01304          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01305             THRU 9500-EXIT.                                       ELTNURSE
01306                                                                   ELTNURSE
01307      SET PLT-INDEX2  TO  2.                                       ELTNURSE
01308      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01309                           AND                                     ELTNURSE
01310         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01311                                                 NOT  =  ZERO      ELTNURSE
01312          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
01313          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
01314                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01315          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01316                               TO  CMF-CODE-VALUE                  ELTNURSE
01317          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01318          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01319          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01320             THRU 9500-EXIT.                                       ELTNURSE
01321                                                                   ELTNURSE
01322      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01323         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
01324          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01325             THRU 9200-EXIT.                                       ELTNURSE
01326                                                                   ELTNURSE
01327      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01328         SET PLT-INDEX2  TO  2                                     ELTNURSE
01329      ELSE                                                         ELTNURSE
01330         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01331                                                                   ELTNURSE
01332  4110-EXIT.  EXIT.                                                ELTNURSE
01333 /                                                                 ELTNURSE
01334  4120-PRIC-METH.                                                  ELTNURSE
01335 ****************************************************************  ELTNURSE
01336 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTNURSE
01337 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTNURSE
01338 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTNURSE
01339 ****************************************************************  ELTNURSE
01340      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01341      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
01342         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01343                                                              '19' ELTNURSE
01344         MOVE +2             TO  WS-CIA                            ELTNURSE
01345         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
01346         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
01347                                                                   ELTNURSE
01348      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01349      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTNURSE
01350         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01351                                                        '19' AND   ELTNURSE
01352         NOT WS-ADD-A-BLANK-LINE                                   ELTNURSE
01353         MOVE +2             TO  WS-CIA                            ELTNURSE
01354         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
01355         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
01356                                                                   ELTNURSE
01357      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01358      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
01359         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
01360                            AND                                    ELTNURSE
01361         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01362         SET  PLT-INDEX2  TO  2                                    ELTNURSE
01363         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
01364                                                             ZERO  ELTNURSE
01365            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
01366            ADD +1  TO  WS-CIA                                     ELTNURSE
01367            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
01368                                                                   ELTNURSE
01369      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01370      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
01371         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
01372                            AND                                    ELTNURSE
01373         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     =  ZERO           ELTNURSE
01374         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTNURSE
01375         ADD +1  TO  WS-CIA                                        ELTNURSE
01376         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTNURSE
01377                                                                   ELTNURSE
01378      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZERO AND      ELTNURSE
01379         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01380         SET  PLT-INDEX2  TO  2                                    ELTNURSE
01381         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
01382                                                             ZERO  ELTNURSE
01383            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
01384            ADD +1  TO  WS-CIA                                     ELTNURSE
01385            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
01386                                                                   ELTNURSE
01387      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01388      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01389         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01390                                                            =  ZEROELTNURSE
01391            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01392                                                            =  ZEROELTNURSE
01393               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
01394            ELSE                                                   ELTNURSE
01395               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
01396          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01397                                                  TO  WS-PERCENTAGEELTNURSE
01398         ELSE                                                      ELTNURSE
01399            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
01400          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01401                                                 TO  WS-PERCENTAGE.ELTNURSE
01402      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
01403         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01404                                             ZERO AND  NOT =  '19' ELTNURSE
01405         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01406         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
01407         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
01408                                                    CMF-CODE-VALUE ELTNURSE
01409         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01410         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
01411         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
01412            THRU 9600-EXIT.                                        ELTNURSE
01413                                                                   ELTNURSE
01414      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01415      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01416         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTNURSE
01417                                                               ZEROELTNURSE
01418            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01419                                                            =  ZEROELTNURSE
01420               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
01421            ELSE                                                   ELTNURSE
01422               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
01423          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01424                                                  TO  WS-PERCENTAGEELTNURSE
01425         ELSE                                                      ELTNURSE
01426            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
01427          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
01428                                                 TO  WS-PERCENTAGE.ELTNURSE
01429      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO AND   ELTNURSE
01430         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01431                                             ZERO AND  NOT =  '19' ELTNURSE
01432         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01433         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
01434         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
01435                                                    CMF-CODE-VALUE ELTNURSE
01436         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTNURSE
01437         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
01438         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
01439            THRU 9600-EXIT.                                        ELTNURSE
01440                                                                   ELTNURSE
01441      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01442          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTNURSE
01443          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01444             THRU 9200-EXIT.                                       ELTNURSE
01445                                                                   ELTNURSE
01446      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01447         SET PLT-INDEX2  TO  2                                     ELTNURSE
01448      ELSE                                                         ELTNURSE
01449         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01450                                                                   ELTNURSE
01451  4120-EXIT.  EXIT.                                                ELTNURSE
01452 /                                                                 ELTNURSE
01453  4130-MAX-PER-VISIT.                                              ELTNURSE
01454 ****************************************************************  ELTNURSE
01455 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTNURSE
01456 ****************************************************************  ELTNURSE
01457      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01458      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01459                          AND                                      ELTNURSE
01460         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
01461                                                  NOT =  ZERO      ELTNURSE
01462          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01463          MOVE +2                    TO  WS-CIA                    ELTNURSE
01464          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01465                                                                   ELTNURSE
01466      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01467      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01468                           AND                                     ELTNURSE
01469         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
01470                                                 NOT  =  ZERO      ELTNURSE
01471                           AND                                     ELTNURSE
01472         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
01473          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01474          MOVE +2                    TO  WS-CIA                    ELTNURSE
01475          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01476                                                                   ELTNURSE
01477      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01478      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01479                           AND                                     ELTNURSE
01480         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
01481                                                  NOT  =  ZERO     ELTNURSE
01482          MOVE WS-BASIC-LIT     TO  WS-BASIC-SUPP                  ELTNURSE
01483          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
01484                                TO  WS-EDIT-MAX-AMT                ELTNURSE
01485          ADD  +1               TO  WS-CIA                         ELTNURSE
01486          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
01487                                                                   ELTNURSE
01488      SET PLT-INDEX2  TO  2.                                       ELTNURSE
01489      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01490                           AND                                     ELTNURSE
01491         PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
01492                                                 NOT  =  ZERO      ELTNURSE
01493          MOVE WS-SUPP-LIT      TO  WS-BASIC-SUPP                  ELTNURSE
01494          MOVE PLB-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
01495                                TO  WS-EDIT-MAX-AMT                ELTNURSE
01496          ADD  +1               TO  WS-CIA                         ELTNURSE
01497          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
01498                                                                   ELTNURSE
01499      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01500         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
01501          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01502             THRU 9200-EXIT.                                       ELTNURSE
01503                                                                   ELTNURSE
01504      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01505         SET PLT-INDEX2  TO  2                                     ELTNURSE
01506      ELSE                                                         ELTNURSE
01507         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01508                                                                   ELTNURSE
01509  4130-EXIT.  EXIT.                                                ELTNURSE
01510 /                                                                 ELTNURSE
01511  4140-CERTIFICATION.                                              ELTNURSE
01512 ****************************************************************  ELTNURSE
01513 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTNURSE
01514 ****************************************************************  ELTNURSE
01515      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01516      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01517                          AND                                      ELTNURSE
01518         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
01519                                                  NOT =  '00'      ELTNURSE
01520          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01521          MOVE +2                    TO  WS-CIA                    ELTNURSE
01522          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01523                                                                   ELTNURSE
01524      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01525      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01526                           AND                                     ELTNURSE
01527         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
01528                                                 NOT  =  '00'      ELTNURSE
01529                           AND                                     ELTNURSE
01530         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
01531          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01532          MOVE +2                    TO  WS-CIA                    ELTNURSE
01533          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01534                                                                   ELTNURSE
01535      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01536      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01537                           AND                                     ELTNURSE
01538         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
01539                                                  NOT  =  '00'     ELTNURSE
01540          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTNURSE
01541          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
01542                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01543          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01544                               TO  CMF-CODE-VALUE                  ELTNURSE
01545          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
01546          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01547          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01548             THRU 9500-EXIT.                                       ELTNURSE
01549                                                                   ELTNURSE
01550      SET PLT-INDEX2  TO  2.                                       ELTNURSE
01551      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01552                           AND                                     ELTNURSE
01553         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
01554                                                 NOT  =  '00'      ELTNURSE
01555          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTNURSE
01556          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
01557                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01558          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01559                               TO  CMF-CODE-VALUE                  ELTNURSE
01560          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01561          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01562          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01563             THRU 9500-EXIT.                                       ELTNURSE
01564                                                                   ELTNURSE
01565      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01566         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
01567          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01568             THRU 9200-EXIT.                                       ELTNURSE
01569                                                                   ELTNURSE
01570      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01571         SET PLT-INDEX2  TO  2                                     ELTNURSE
01572      ELSE                                                         ELTNURSE
01573         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01574                                                                   ELTNURSE
01575  4140-EXIT.  EXIT.                                                ELTNURSE
01576 /                                                                 ELTNURSE
01577  4150-RECERTIFICATION.                                            ELTNURSE
01578 ****************************************************************  ELTNURSE
01579 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTNURSE
01580 ****************************************************************  ELTNURSE
01581      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01582      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01583                          AND                                      ELTNURSE
01584         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01585                                                  NOT =  ZERO      ELTNURSE
01586          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01587          MOVE +2                    TO  WS-CIA                    ELTNURSE
01588          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01589                                                                   ELTNURSE
01590      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01591      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01592                           AND                                     ELTNURSE
01593         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01594                                                 NOT  =  ZERO      ELTNURSE
01595                           AND                                     ELTNURSE
01596         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
01597          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01598          MOVE +2                    TO  WS-CIA                    ELTNURSE
01599          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01600                                                                   ELTNURSE
01601      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01602      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO      ELTNURSE
01603                           AND                                     ELTNURSE
01604         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01605                                                  NOT  =  ZERO     ELTNURSE
01606          MOVE 'BPB'  TO  CMF-RECORD-PREFIX                        ELTNURSE
01607          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
01608                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01609          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
01610                               TO  CMF-CODE-VALUE                  ELTNURSE
01611          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
01612          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01613          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01614             THRU 9500-EXIT.                                       ELTNURSE
01615                                                                   ELTNURSE
01616      SET PLT-INDEX2  TO  2.                                       ELTNURSE
01617      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO       ELTNURSE
01618                           AND                                     ELTNURSE
01619         PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01620                                                 NOT  =  ZERO      ELTNURSE
01621          MOVE 'BPB'           TO  CMF-RECORD-PREFIX               ELTNURSE
01622          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
01623                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01624          MOVE PLB-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
01625                               TO  CMF-CODE-VALUE                  ELTNURSE
01626          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01627          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01628          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01629             THRU 9500-EXIT.                                       ELTNURSE
01630                                                                   ELTNURSE
01631      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01632         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
01633          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01634             THRU 9200-EXIT.                                       ELTNURSE
01635                                                                   ELTNURSE
01636      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01637         SET PLT-INDEX2  TO  2                                     ELTNURSE
01638      ELSE                                                         ELTNURSE
01639         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01640                                                                   ELTNURSE
01641  4150-EXIT.  EXIT.                                                ELTNURSE
01642 /                                                                 ELTNURSE
01643  4160-SPILLOVR-COINS-N-DEDUC.                                     ELTNURSE
01644 ****************************************************************  ELTNURSE
01645 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTNURSE
01646 ****************************************************************  ELTNURSE
01647      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01648                                                                   ELTNURSE
01649      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01650      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTNURSE
01651                            AND                                    ELTNURSE
01652         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTNURSE
01653                                                  NOT =  '0'       ELTNURSE
01654         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01655         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTNURSE
01656                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
01657         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTNURSE
01658                                                TO  CMF-CODE-VALUE ELTNURSE
01659         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTNURSE
01660         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTNURSE
01661         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
01662            THRU 9500-EXIT                                         ELTNURSE
01663         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
01664            THRU 9200-EXIT.                                        ELTNURSE
01665 ****************************************************************  ELTNURSE
01666 *          S P I L L O V E R   D E D U C T I B L E             *  ELTNURSE
01667 ****************************************************************  ELTNURSE
01668      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01669                                                                   ELTNURSE
01670      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01671      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT =  ZERO      ELTNURSE
01672                            AND                                    ELTNURSE
01673         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
01674                                                  NOT =  '0'       ELTNURSE
01675         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
01676         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTNURSE
01677                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
01678         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01679                                                 TO  CMF-CODE-VALUEELTNURSE
01680         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTNURSE
01681         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTNURSE
01682         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
01683            THRU 9500-EXIT                                         ELTNURSE
01684         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
01685            THRU 9200-EXIT.                                        ELTNURSE
01686                                                                   ELTNURSE
01687      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      =  ZEROES        ELTNURSE
01688         SET PLT-INDEX2  TO  2                                     ELTNURSE
01689      ELSE                                                         ELTNURSE
01690         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01691                                                                   ELTNURSE
01692  4160-EXIT.  EXIT.                                                ELTNURSE
01693 /                                                                 ELTNURSE
01694  5000-PROFESSIONAL-IP.                                            ELTNURSE
01695 ****************************************************************  ELTNURSE
01696 *       NURSING PROFESSIONAL INPATIENT PROCESSING              *  ELTNURSE
01697 ****************************************************************  ELTNURSE
01698                                                                   ELTNURSE
01699      MOVE 'Y'                 TO  WS-FIRSTTIME-IND.               ELTNURSE
01700      MOVE HEADER-P-IP-LINE-3  TO  COF-HDR-LINE (2).               ELTNURSE
01701                                                                   ELTNURSE
01702      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTNURSE
01703         THRU 9100-EXIT.                                           ELTNURSE
01704                                                                   ELTNURSE
01705      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
01706      PERFORM WITH TEST BEFORE                                     ELTNURSE
01707              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTNURSE
01708              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTNURSE
01709         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTNURSE
01710         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTNURSE
01711         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTNURSE
01712         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTNURSE
01713      END-PERFORM.                                                 ELTNURSE
01714      MOVE WS-PROF-IP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
01715                                                                   ELTNURSE
01716                                                                   ELTNURSE
01717      PERFORM 5010-MOVE-IN-PROF-IP-TABS                            ELTNURSE
01718         THRU 5010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
01719                        UNTIL   WS-SUB  >     WS-PROF-IP-CNT.      ELTNURSE
01720                                                                   ELTNURSE
01721      PERFORM 5020-CALL-COVERAGE                                   ELTNURSE
01722         THRU 5020-EXIT.                                           ELTNURSE
01723                                                                   ELTNURSE
01724      IF PVN-COVG-NONE                                             ELTNURSE
01725          GO TO 5000-EXIT.                                         ELTNURSE
01726                                                                   ELTNURSE
01727      PERFORM 5030-FIND-FIRST-NONZERO                              ELTNURSE
01728         THRU 5030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
01729                        UNTIL   WS-SUB  > WS-PROF-IP-CNT.          ELTNURSE
01730                                                                   ELTNURSE
01731  5000-EXIT.  EXIT.                                                ELTNURSE
01732 /                                                                 ELTNURSE
01733  5010-MOVE-IN-PROF-IP-TABS.                                       ELTNURSE
01734      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01735      MOVE WS-PROF-IP-BP (WS-SUB)                                  ELTNURSE
01736                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTNURSE
01737                                                                   ELTNURSE
01738      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTNURSE
01739                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTNURSE
01740                                                                   ELTNURSE
01741  5010-EXIT.  EXIT.                                                ELTNURSE
01742      SKIP3                                                        ELTNURSE
01743  5020-CALL-COVERAGE.                                              ELTNURSE
01744                                                                   ELTNURSE
01745      MOVE 'NURSING SERVICES ' TO SSB-TOPIC-PHRASE.                ELTNURSE
01746                                                                   ELTNURSE
01747      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTNURSE
01748                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01749                     END-EXEC.                                     ELTNURSE
01750                                                                   ELTNURSE
01751      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
01752                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01753                     END-EXEC.                                     ELTNURSE
01754                                                                   ELTNURSE
01755      IF PVN-COVG-NONE                                             ELTNURSE
01756          GO TO 5020-EXIT.                                         ELTNURSE
01757                                                                   ELTNURSE
01758      MOVE +1  TO  WS-CIA.                                         ELTNURSE
01759                                                                   ELTNURSE
01760      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTNURSE
01761                    PSP-PROVN-PRICING-METHD,                       ELTNURSE
01762                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTNURSE
01763                    PSP-TRANSF-OTHER-RESP-IND,                     ELTNURSE
01764                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTNURSE
01765                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTNURSE
01766                    PSP-SPILL-OVER-DED-APL-IND,                    ELTNURSE
01767                    PSP-CERTFN-REQRM-IND,                          ELTNURSE
01768                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTNURSE
01769                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTNURSE
01770                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTNURSE
01771                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTNURSE
01772                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTNURSE
01773                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTNURSE
01774                    PSE-CERTN-REPETN-REQRD-IND,                    ELTNURSE
01775                    PSE-MAX-AMT-PER-VISIT.                         ELTNURSE
01776                                                                   ELTNURSE
01777      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTNURSE
01778                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01779                     END-EXEC.                                     ELTNURSE
01780                                                                   ELTNURSE
01781      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTNURSE
01782      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
01783          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTNURSE
01784                                                                   ELTNURSE
01785  5020-EXIT.  EXIT.                                                ELTNURSE
01786 /                                                                 ELTNURSE
01787  5030-FIND-FIRST-NONZERO.                                         ELTNURSE
01788      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01789                                                                   ELTNURSE
01790      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTNURSE
01791          CONTINUE                                                 ELTNURSE
01792      ELSE                                                         ELTNURSE
01793          PERFORM 5100-BUILD-SCREEN-LINES                          ELTNURSE
01794             THRU 5100-EXIT.                                       ELTNURSE
01795                                                                   ELTNURSE
01796  5030-EXIT.  EXIT.                                                ELTNURSE
01797 /                                                                 ELTNURSE
01798  5100-BUILD-SCREEN-LINES.                                         ELTNURSE
01799      SET PLT-INDEX1  TO                                           ELTNURSE
01800              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTNURSE
01801                                                                   ELTNURSE
01802      MOVE  +1  TO  WS-CIA.                                        ELTNURSE
01803                                                                   ELTNURSE
01804      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTNURSE
01805          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZEROS      ELTNURSE
01806              SET PLT-INDEX2  TO  2                                ELTNURSE
01807          ELSE                                                     ELTNURSE
01808              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTNURSE
01809              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTNURSE
01810                 THRU 9200-EXIT                                    ELTNURSE
01811              GO TO 5100-EXIT                                      ELTNURSE
01812      ELSE                                                         ELTNURSE
01813          SET PLT-INDEX2  TO  1.                                   ELTNURSE
01814                                                                   ELTNURSE
01815      PERFORM 5105-LIST-BEN-PROV                                   ELTNURSE
01816         THRU 5105-EXIT.                                           ELTNURSE
01817                                                                   ELTNURSE
01818      PERFORM 5110-PLACE-OF-TREATMENT                              ELTNURSE
01819         THRU 5110-EXIT.                                           ELTNURSE
01820                                                                   ELTNURSE
01821      PERFORM 5120-PRIC-METH                                       ELTNURSE
01822         THRU 5120-EXIT.                                           ELTNURSE
01823                                                                   ELTNURSE
01824      PERFORM 5130-MAX-PER-VISIT                                   ELTNURSE
01825         THRU 5130-EXIT.                                           ELTNURSE
01826                                                                   ELTNURSE
01827      PERFORM 5140-CERTIFICATION                                   ELTNURSE
01828         THRU 5140-EXIT.                                           ELTNURSE
01829                                                                   ELTNURSE
01830      PERFORM 5150-RECERTIFICATION                                 ELTNURSE
01831         THRU 5150-EXIT.                                           ELTNURSE
01832                                                                   ELTNURSE
01833      PERFORM 5160-SPILLOVR-COINS-N-DEDUC                          ELTNURSE
01834         THRU 5160-EXIT.                                           ELTNURSE
01835                                                                   ELTNURSE
01836      PERFORM 3165-TRANS-OTHR-RESPON-IND  THRU                     ELTNURSE
01837              3165-EXIT.                                           ELTNURSE
01838                                                                   ELTNURSE
01839      PERFORM 7000-ALL-LEVEL-TABS                                  ELTNURSE
01840         THRU 7000-EXIT.                                           ELTNURSE
01841                                                                   ELTNURSE
01842  5100-EXIT.  EXIT.                                                ELTNURSE
01843 /                                                                 ELTNURSE
01844  5105-LIST-BEN-PROV.                                              ELTNURSE
01845 ****************************************************************  ELTNURSE
01846 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTNURSE
01847 ****************************************************************  ELTNURSE
01848      MOVE  +2               TO  WS-CIA.                           ELTNURSE
01849      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTNURSE
01850      MOVE ZERO              TO  WS-SUB2.                          ELTNURSE
01851      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTNURSE
01852                             TO  WS-SUB3.                          ELTNURSE
01853                                                                   ELTNURSE
01854      PERFORM 5106-ZERO-ALL-WITH-SAME-NO                           ELTNURSE
01855         THRU 5106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTNURSE
01856                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-IP-CNT.ELTNURSE
01857                                                                   ELTNURSE
01858      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
01859      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTNURSE
01860                                                                   ELTNURSE
01861      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
01862                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
01863                     END-EXEC.                                     ELTNURSE
01864                                                                   ELTNURSE
01865  5105-EXIT.  EXIT.                                                ELTNURSE
01866      SKIP3                                                        ELTNURSE
01867  5106-ZERO-ALL-WITH-SAME-NO.                                      ELTNURSE
01868                                                                   ELTNURSE
01869      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTNURSE
01870          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTNURSE
01871          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTNURSE
01872          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTNURSE
01873                            TO  CMF-CODE-VALUE                     ELTNURSE
01874          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01875          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTNURSE
01876          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01877             THRU 9500-EXIT                                        ELTNURSE
01878          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTNURSE
01879          ADD  +1    TO  WS-SUB2                                   ELTNURSE
01880          IF WS-CIA  >  20  OR  =  20                              ELTNURSE
01881              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTNURSE
01882              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTNURSE
01883                             COMMAREA (DFHCOMMAREA)                ELTNURSE
01884                             END-EXEC                              ELTNURSE
01885              MOVE  +1  TO  WS-CIA.                                ELTNURSE
01886                                                                   ELTNURSE
01887  5106-EXIT.  EXIT.                                                ELTNURSE
01888 /                                                                 ELTNURSE
01889  5110-PLACE-OF-TREATMENT.                                         ELTNURSE
01890 ****************************************************************  ELTNURSE
01891 *              P L A C E   O F   T R E A T M E N T             *  ELTNURSE
01892 ****************************************************************  ELTNURSE
01893      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01894      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
01895                          AND                                      ELTNURSE
01896         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01897                                                  NOT =  ZERO      ELTNURSE
01898          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01899          MOVE +2                    TO  WS-CIA                    ELTNURSE
01900          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01901                                                                   ELTNURSE
01902      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01903      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
01904                           AND                                     ELTNURSE
01905         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01906                                                 NOT  =  ZERO      ELTNURSE
01907                           AND                                     ELTNURSE
01908         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
01909          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
01910          MOVE +2                    TO  WS-CIA                    ELTNURSE
01911          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
01912                                                                   ELTNURSE
01913      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01914      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
01915                           AND                                     ELTNURSE
01916         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01917                                                  NOT  =  ZERO     ELTNURSE
01918          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
01919          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
01920                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01921          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01922                               TO  CMF-CODE-VALUE                  ELTNURSE
01923          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
01924          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01925          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01926             THRU 9500-EXIT.                                       ELTNURSE
01927                                                                   ELTNURSE
01928      SET PLT-INDEX2  TO  2.                                       ELTNURSE
01929      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
01930                           AND                                     ELTNURSE
01931         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
01932                                                 NOT  =  ZERO      ELTNURSE
01933          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
01934          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
01935                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
01936          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
01937                               TO  CMF-CODE-VALUE                  ELTNURSE
01938          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
01939          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
01940          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
01941             THRU 9500-EXIT.                                       ELTNURSE
01942                                                                   ELTNURSE
01943      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
01944         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
01945          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
01946             THRU 9200-EXIT.                                       ELTNURSE
01947                                                                   ELTNURSE
01948      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
01949         SET PLT-INDEX2  TO  2                                     ELTNURSE
01950      ELSE                                                         ELTNURSE
01951         SET PLT-INDEX2  TO  1.                                    ELTNURSE
01952                                                                   ELTNURSE
01953  5110-EXIT.  EXIT.                                                ELTNURSE
01954 /                                                                 ELTNURSE
01955  5120-PRIC-METH.                                                  ELTNURSE
01956 ****************************************************************  ELTNURSE
01957 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTNURSE
01958 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTNURSE
01959 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTNURSE
01960 ****************************************************************  ELTNURSE
01961      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01962      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
01963         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01964                                                              '19' ELTNURSE
01965         MOVE +2             TO  WS-CIA                            ELTNURSE
01966         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
01967         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
01968                                                                   ELTNURSE
01969      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
01970      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
01971         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
01972                                                        '19' AND   ELTNURSE
01973         NOT WS-ADD-A-BLANK-LINE                                   ELTNURSE
01974         MOVE +2             TO  WS-CIA                            ELTNURSE
01975         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
01976         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
01977                                                                   ELTNURSE
01978      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01979      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
01980         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
01981                            AND                                    ELTNURSE
01982         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
01983         SET  PLT-INDEX2  TO  2                                    ELTNURSE
01984         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
01985                                                             ZERO  ELTNURSE
01986            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
01987            ADD +1  TO  WS-CIA                                     ELTNURSE
01988            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
01989                                                                   ELTNURSE
01990      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
01991      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
01992         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
01993                            AND                                    ELTNURSE
01994         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTNURSE
01995         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTNURSE
01996         ADD +1  TO  WS-CIA                                        ELTNURSE
01997         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTNURSE
01998                                                                   ELTNURSE
01999      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTNURSE
02000         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02001         SET  PLT-INDEX2  TO  2                                    ELTNURSE
02002         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
02003                                                             ZERO  ELTNURSE
02004            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
02005            ADD +1  TO  WS-CIA                                     ELTNURSE
02006            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
02007                                                                   ELTNURSE
02008      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02009      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02010         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02011                                                            =  ZEROELTNURSE
02012            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02013                                                            =  ZEROELTNURSE
02014               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
02015            ELSE                                                   ELTNURSE
02016               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
02017          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02018                                                  TO  WS-PERCENTAGEELTNURSE
02019         ELSE                                                      ELTNURSE
02020            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
02021          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02022                                                 TO  WS-PERCENTAGE.ELTNURSE
02023      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
02024         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02025                                             ZERO AND  NOT =  '19' ELTNURSE
02026         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02027         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
02028         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
02029                                                    CMF-CODE-VALUE ELTNURSE
02030         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02031         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
02032         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
02033            THRU 9600-EXIT.                                        ELTNURSE
02034                                                                   ELTNURSE
02035      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02036      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02037         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTNURSE
02038                                                               ZEROELTNURSE
02039            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02040                                                            =  ZEROELTNURSE
02041               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
02042            ELSE                                                   ELTNURSE
02043               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
02044          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02045                                                  TO  WS-PERCENTAGEELTNURSE
02046         ELSE                                                      ELTNURSE
02047            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
02048          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02049                                                 TO  WS-PERCENTAGE.ELTNURSE
02050      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
02051         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02052                                             ZERO AND  NOT =  '19' ELTNURSE
02053         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02054         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
02055         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
02056                                                    CMF-CODE-VALUE ELTNURSE
02057         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTNURSE
02058         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
02059         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
02060            THRU 9600-EXIT.                                        ELTNURSE
02061                                                                   ELTNURSE
02062      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02063          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTNURSE
02064          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02065             THRU 9200-EXIT.                                       ELTNURSE
02066                                                                   ELTNURSE
02067      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02068         SET PLT-INDEX2  TO  2                                     ELTNURSE
02069      ELSE                                                         ELTNURSE
02070         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02071                                                                   ELTNURSE
02072  5120-EXIT.  EXIT.                                                ELTNURSE
02073 /                                                                 ELTNURSE
02074  5130-MAX-PER-VISIT.                                              ELTNURSE
02075 ****************************************************************  ELTNURSE
02076 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTNURSE
02077 ****************************************************************  ELTNURSE
02078      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02079      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02080                          AND                                      ELTNURSE
02081         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02082                                                  NOT =  ZERO      ELTNURSE
02083          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02084          MOVE +2                    TO  WS-CIA                    ELTNURSE
02085          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02086                                                                   ELTNURSE
02087      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02088      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02089                           AND                                     ELTNURSE
02090         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02091                                                 NOT  =  ZERO      ELTNURSE
02092                           AND                                     ELTNURSE
02093         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02094          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02095          MOVE +2                    TO  WS-CIA                    ELTNURSE
02096          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02097                                                                   ELTNURSE
02098      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02099      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02100                           AND                                     ELTNURSE
02101         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02102                                                  NOT  =  ZERO     ELTNURSE
02103          MOVE WS-BASIC-LIT     TO  WS-BASIC-SUPP                  ELTNURSE
02104          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
02105                                TO  WS-EDIT-MAX-AMT                ELTNURSE
02106          ADD  +1               TO  WS-CIA                         ELTNURSE
02107          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
02108                                                                   ELTNURSE
02109      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02110      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02111                           AND                                     ELTNURSE
02112         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02113                                                 NOT  =  ZERO      ELTNURSE
02114          MOVE WS-SUPP-LIT      TO  WS-BASIC-SUPP                  ELTNURSE
02115          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
02116                                TO  WS-EDIT-MAX-AMT                ELTNURSE
02117          ADD  +1               TO  WS-CIA                         ELTNURSE
02118          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
02119                                                                   ELTNURSE
02120      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02121         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02122          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02123             THRU 9200-EXIT.                                       ELTNURSE
02124                                                                   ELTNURSE
02125      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02126         SET PLT-INDEX2  TO  2                                     ELTNURSE
02127      ELSE                                                         ELTNURSE
02128         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02129                                                                   ELTNURSE
02130  5130-EXIT.  EXIT.                                                ELTNURSE
02131 /                                                                 ELTNURSE
02132  5140-CERTIFICATION.                                              ELTNURSE
02133 ****************************************************************  ELTNURSE
02134 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTNURSE
02135 ****************************************************************  ELTNURSE
02136      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02137      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02138                          AND                                      ELTNURSE
02139         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02140                                                  NOT =  '00'      ELTNURSE
02141          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02142          MOVE +2                    TO  WS-CIA                    ELTNURSE
02143          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02144                                                                   ELTNURSE
02145      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02146      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02147                           AND                                     ELTNURSE
02148         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02149                                                 NOT  =  '00'      ELTNURSE
02150                           AND                                     ELTNURSE
02151         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02152          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02153          MOVE +2                    TO  WS-CIA                    ELTNURSE
02154          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02155                                                                   ELTNURSE
02156      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02157      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02158                           AND                                     ELTNURSE
02159         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02160                                                  NOT  =  '00'     ELTNURSE
02161          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTNURSE
02162          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
02163                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02164          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02165                               TO  CMF-CODE-VALUE                  ELTNURSE
02166          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
02167          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02168          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02169             THRU 9500-EXIT.                                       ELTNURSE
02170                                                                   ELTNURSE
02171      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02172      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02173                           AND                                     ELTNURSE
02174         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02175                                                 NOT  =  '00'      ELTNURSE
02176          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTNURSE
02177          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
02178                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02179          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02180                               TO  CMF-CODE-VALUE                  ELTNURSE
02181          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02182          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02183          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02184             THRU 9500-EXIT.                                       ELTNURSE
02185                                                                   ELTNURSE
02186      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02187         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02188          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02189             THRU 9200-EXIT.                                       ELTNURSE
02190                                                                   ELTNURSE
02191      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02192         SET PLT-INDEX2  TO  2                                     ELTNURSE
02193      ELSE                                                         ELTNURSE
02194         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02195                                                                   ELTNURSE
02196  5140-EXIT.  EXIT.                                                ELTNURSE
02197 /                                                                 ELTNURSE
02198  5150-RECERTIFICATION.                                            ELTNURSE
02199 ****************************************************************  ELTNURSE
02200 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTNURSE
02201 ****************************************************************  ELTNURSE
02202      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02203      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02204                          AND                                      ELTNURSE
02205         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02206                                                  NOT =  ZERO      ELTNURSE
02207          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02208          MOVE +2                    TO  WS-CIA                    ELTNURSE
02209          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02210                                                                   ELTNURSE
02211      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02212      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02213                           AND                                     ELTNURSE
02214         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02215                                                 NOT  =  ZERO      ELTNURSE
02216                           AND                                     ELTNURSE
02217         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02218          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02219          MOVE +2                    TO  WS-CIA                    ELTNURSE
02220          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02221                                                                   ELTNURSE
02222      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02223      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02224                           AND                                     ELTNURSE
02225         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02226                                                  NOT  =  ZERO     ELTNURSE
02227          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTNURSE
02228          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
02229                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02230          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
02231                               TO  CMF-CODE-VALUE                  ELTNURSE
02232          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
02233          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02234          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02235             THRU 9500-EXIT.                                       ELTNURSE
02236                                                                   ELTNURSE
02237      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02238      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02239                           AND                                     ELTNURSE
02240         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02241                                                 NOT  =  ZERO      ELTNURSE
02242          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTNURSE
02243          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
02244                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02245          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
02246                               TO  CMF-CODE-VALUE                  ELTNURSE
02247          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02248          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02249          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02250             THRU 9500-EXIT.                                       ELTNURSE
02251                                                                   ELTNURSE
02252      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02253         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02254          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02255             THRU 9200-EXIT.                                       ELTNURSE
02256                                                                   ELTNURSE
02257      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02258         SET PLT-INDEX2  TO  2                                     ELTNURSE
02259      ELSE                                                         ELTNURSE
02260         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02261                                                                   ELTNURSE
02262  5150-EXIT.  EXIT.                                                ELTNURSE
02263 /                                                                 ELTNURSE
02264  5160-SPILLOVR-COINS-N-DEDUC.                                     ELTNURSE
02265 ****************************************************************  ELTNURSE
02266 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTNURSE
02267 ****************************************************************  ELTNURSE
02268      MOVE +1  TO  WS-CIA.                                         ELTNURSE
02269                                                                   ELTNURSE
02270      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02271      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTNURSE
02272                            AND                                    ELTNURSE
02273         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTNURSE
02274                                                  NOT =  '0'       ELTNURSE
02275         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02276         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTNURSE
02277                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
02278         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTNURSE
02279                                                TO  CMF-CODE-VALUE ELTNURSE
02280         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTNURSE
02281         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTNURSE
02282         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
02283            THRU 9500-EXIT                                         ELTNURSE
02284         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
02285            THRU 9200-EXIT.                                        ELTNURSE
02286 ****************************************************************  ELTNURSE
02287 *          S P I L L O V E R   D E D U C T I B L E             *  ELTNURSE
02288 ****************************************************************  ELTNURSE
02289      MOVE +1  TO  WS-CIA.                                         ELTNURSE
02290                                                                   ELTNURSE
02291      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02292      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTNURSE
02293                            AND                                    ELTNURSE
02294         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02295                                                  NOT =  '0'       ELTNURSE
02296         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02297         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTNURSE
02298                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
02299         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02300                                                 TO  CMF-CODE-VALUEELTNURSE
02301         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTNURSE
02302         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTNURSE
02303         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
02304            THRU 9500-EXIT                                         ELTNURSE
02305         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
02306            THRU 9200-EXIT.                                        ELTNURSE
02307                                                                   ELTNURSE
02308      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02309         SET PLT-INDEX2  TO  2                                     ELTNURSE
02310      ELSE                                                         ELTNURSE
02311         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02312                                                                   ELTNURSE
02313  5160-EXIT.  EXIT.                                                ELTNURSE
02314 /                                                                 ELTNURSE
02315  6000-PROFESSIONAL-OP.                                            ELTNURSE
02316 ****************************************************************  ELTNURSE
02317 *       NURSING PROFESSIONAL OUTPATIENT PROCESSING             *  ELTNURSE
02318 ****************************************************************  ELTNURSE
02319                                                                   ELTNURSE
02320      MOVE 'Y'                 TO  WS-FIRSTTIME-IND.               ELTNURSE
02321      MOVE HEADER-P-OP-LINE-3  TO  COF-HDR-LINE (2).               ELTNURSE
02322                                                                   ELTNURSE
02323      PERFORM 9100-HEADER-OUTPUT-REQUEST                           ELTNURSE
02324         THRU 9100-EXIT.                                           ELTNURSE
02325                                                                   ELTNURSE
02326      MOVE TABLE-MAX       TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
02327      PERFORM WITH TEST BEFORE                                     ELTNURSE
02328              VARYING PVN-BEN-PROVN-IDX FROM 1 BY 1                ELTNURSE
02329              UNTIL PVN-BEN-PROVN-IDX GREATER THAN TABLE-MAX       ELTNURSE
02330         INITIALIZE PVN-BEN-PROVN-ID  (PVN-BEN-PROVN-IDX)          ELTNURSE
02331         INITIALIZE PVN-COVG-SAME-AS  (PVN-BEN-PROVN-IDX)          ELTNURSE
02332         INITIALIZE PVN-SLOT-NBR-BAS  (PVN-BEN-PROVN-IDX)          ELTNURSE
02333         INITIALIZE PVN-SLOT-NBR-SUP  (PVN-BEN-PROVN-IDX)          ELTNURSE
02334      END-PERFORM.                                                 ELTNURSE
02335      MOVE WS-PROF-OP-CNT  TO  PVN-NBR-BEN-PROVN.                  ELTNURSE
02336                                                                   ELTNURSE
02337                                                                   ELTNURSE
02338      PERFORM 6010-MOVE-IN-PROF-OP-TABS                            ELTNURSE
02339         THRU 6010-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
02340                        UNTIL   WS-SUB  >     WS-PROF-OP-CNT.      ELTNURSE
02341                                                                   ELTNURSE
02342      PERFORM 6020-CALL-COVERAGE                                   ELTNURSE
02343         THRU 6020-EXIT.                                           ELTNURSE
02344                                                                   ELTNURSE
02345      IF PVN-COVG-NONE                                             ELTNURSE
02346          GO TO 6000-EXIT.                                         ELTNURSE
02347                                                                   ELTNURSE
02348      PERFORM 6030-FIND-FIRST-NONZERO                              ELTNURSE
02349         THRU 6030-EXIT VARYING WS-SUB FROM +1 BY +1               ELTNURSE
02350                        UNTIL   WS-SUB  > WS-PROF-OP-CNT.          ELTNURSE
02351                                                                   ELTNURSE
02352  6000-EXIT.  EXIT.                                                ELTNURSE
02353 /                                                                 ELTNURSE
02354  6010-MOVE-IN-PROF-OP-TABS.                                       ELTNURSE
02355      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
02356      MOVE WS-PROF-OP-BP (WS-SUB)                                  ELTNURSE
02357                TO  PVN-BEN-PROVN-ID (PVN-BEN-PROVN-IDX).          ELTNURSE
02358                                                                   ELTNURSE
02359      MOVE ZERO  TO  PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX),         ELTNURSE
02360                     PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX).         ELTNURSE
02361                                                                   ELTNURSE
02362  6010-EXIT.  EXIT.                                                ELTNURSE
02363      SKIP3                                                        ELTNURSE
02364  6020-CALL-COVERAGE.                                              ELTNURSE
02365                                                                   ELTNURSE
02366      MOVE 'NURSING SERVICES ' TO SSB-TOPIC-PHRASE.                ELTNURSE
02367                                                                   ELTNURSE
02368      EXEC CICS LINK PROGRAM ('ELGCOVER')                          ELTNURSE
02369                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
02370                     END-EXEC.                                     ELTNURSE
02371                                                                   ELTNURSE
02372      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
02373                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
02374                     END-EXEC.                                     ELTNURSE
02375                                                                   ELTNURSE
02376      IF PVN-COVG-NONE                                             ELTNURSE
02377          GO TO 6020-EXIT.                                         ELTNURSE
02378                                                                   ELTNURSE
02379      MOVE +1  TO  WS-CIA.                                         ELTNURSE
02380                                                                   ELTNURSE
02381      MOVE '1'  TO  PSP-PLACE-TREAT-ELIG-IND,                      ELTNURSE
02382                    PSP-PROVN-PRICING-METHD,                       ELTNURSE
02383                    PSP-ADDITIONAL-PRICING-PRCNT,                  ELTNURSE
02384                    PSP-TRANSF-OTHER-RESP-IND,                     ELTNURSE
02385                    PSP-VARIABLE-INDEMNITY-PRCNT,                  ELTNURSE
02386                    PSP-SPILL-OVER-COINS-APL-IND,                  ELTNURSE
02387                    PSP-SPILL-OVER-DED-APL-IND,                    ELTNURSE
02388                    PSP-CERTFN-REQRM-IND,                          ELTNURSE
02389                    PSP-BEN-TAB-PROVN-ID-AAR,                      ELTNURSE
02390                    PSP-BEN-TAB-PROVN-ID-ABM,                      ELTNURSE
02391                    PSP-BEN-TAB-PROVN-ID-ACL,                      ELTNURSE
02392                    PSP-BEN-TAB-PROVN-ID-ADL,                      ELTNURSE
02393                    PSP-BEN-TAB-PROVN-ID-AOL,                      ELTNURSE
02394                    PSP-BEN-TAB-PROVN-ID-PPF,                      ELTNURSE
02395                    PSE-CERTN-REPETN-REQRD-IND,                    ELTNURSE
02396                    PSE-MAX-AMT-PER-VISIT.                         ELTNURSE
02397                                                                   ELTNURSE
02398      EXEC CICS LINK PROGRAM ('ELUPLGRP')                          ELTNURSE
02399                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
02400                     END-EXEC.                                     ELTNURSE
02401                                                                   ELTNURSE
02402      SET CIA-ELSPLGTB-DDN TO TRUE.                                ELTNURSE
02403      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
02404          ADDRESS OF PLT-PAYMENT-LEVEL-TABLE.                      ELTNURSE
02405                                                                   ELTNURSE
02406  6020-EXIT.  EXIT.                                                ELTNURSE
02407      SKIP3                                                        ELTNURSE
02408  6030-FIND-FIRST-NONZERO.                                         ELTNURSE
02409      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
02410                                                                   ELTNURSE
02411      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = ZERO               ELTNURSE
02412          CONTINUE                                                 ELTNURSE
02413      ELSE                                                         ELTNURSE
02414          PERFORM 6100-BUILD-SCREEN-LINES                          ELTNURSE
02415             THRU 6100-EXIT.                                       ELTNURSE
02416                                                                   ELTNURSE
02417  6030-EXIT.  EXIT.                                                ELTNURSE
02418 /                                                                 ELTNURSE
02419  6100-BUILD-SCREEN-LINES.                                         ELTNURSE
02420      SET PLT-INDEX1  TO                                           ELTNURSE
02421              PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX).                ELTNURSE
02422                                                                   ELTNURSE
02423      MOVE  +1  TO  WS-CIA.                                        ELTNURSE
02424                                                                   ELTNURSE
02425      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX) = ZEROS              ELTNURSE
02426          IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX)     NOT  = ZEROS ELTNURSE
02427              SET PLT-INDEX2  TO  2                                ELTNURSE
02428          ELSE                                                     ELTNURSE
02429              MOVE WS-INDICES-PROBLEM  TO  COF-DTL-LINE (1)        ELTNURSE
02430              PERFORM 9200-TEXT-OUTPUT-REQUEST                     ELTNURSE
02431                 THRU 9200-EXIT                                    ELTNURSE
02432              GO TO 6100-EXIT                                      ELTNURSE
02433      ELSE                                                         ELTNURSE
02434          SET PLT-INDEX2  TO  1.                                   ELTNURSE
02435                                                                   ELTNURSE
02436      PERFORM 6105-LIST-BEN-PROV                                   ELTNURSE
02437         THRU 6105-EXIT.                                           ELTNURSE
02438                                                                   ELTNURSE
02439      PERFORM 6110-PLACE-OF-TREATMENT                              ELTNURSE
02440         THRU 6110-EXIT.                                           ELTNURSE
02441                                                                   ELTNURSE
02442      PERFORM 6120-PRIC-METH                                       ELTNURSE
02443         THRU 6120-EXIT.                                           ELTNURSE
02444                                                                   ELTNURSE
02445      PERFORM 6130-MAX-PER-VISIT                                   ELTNURSE
02446         THRU 6130-EXIT.                                           ELTNURSE
02447                                                                   ELTNURSE
02448      PERFORM 6140-CERTIFICATION                                   ELTNURSE
02449         THRU 6140-EXIT.                                           ELTNURSE
02450                                                                   ELTNURSE
02451      PERFORM 6150-RECERTIFICATION                                 ELTNURSE
02452         THRU 6150-EXIT.                                           ELTNURSE
02453                                                                   ELTNURSE
02454      PERFORM 6160-SPILLOVR-COINS-N-DEDUC                          ELTNURSE
02455         THRU 6160-EXIT.                                           ELTNURSE
02456                                                                   ELTNURSE
02457      PERFORM 3165-TRANS-OTHR-RESPON-IND  THRU                     ELTNURSE
02458              3165-EXIT.                                           ELTNURSE
02459                                                                   ELTNURSE
02460      PERFORM 7000-ALL-LEVEL-TABS                                  ELTNURSE
02461         THRU 7000-EXIT.                                           ELTNURSE
02462                                                                   ELTNURSE
02463  6100-EXIT.  EXIT.                                                ELTNURSE
02464 /                                                                 ELTNURSE
02465  6105-LIST-BEN-PROV.                                              ELTNURSE
02466 ****************************************************************  ELTNURSE
02467 *     L I S T   O F   B E N E F I T   P R O V I S O N S        *  ELTNURSE
02468 ****************************************************************  ELTNURSE
02469      MOVE  +2               TO  WS-CIA.                           ELTNURSE
02470      MOVE WS-FOLLOWING-BEN  TO  COF-DTL-LINE (WS-CIA).            ELTNURSE
02471      MOVE ZERO              TO  WS-SUB2.                          ELTNURSE
02472      MOVE PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)                    ELTNURSE
02473                             TO  WS-SUB3.                          ELTNURSE
02474                                                                   ELTNURSE
02475      PERFORM 6106-ZERO-ALL-WITH-SAME-NO                           ELTNURSE
02476         THRU 6106-EXIT VARYING PVN-BEN-PROVN-IDX FROM WS-SUB BY +1ELTNURSE
02477                        UNTIL   PVN-BEN-PROVN-IDX > WS-PROF-OP-CNT.ELTNURSE
02478                                                                   ELTNURSE
02479      SET PVN-BEN-PROVN-IDX TO WS-SUB.                             ELTNURSE
02480      MOVE WS-CIA     TO  COF-NBR-DTL-LINES.                       ELTNURSE
02481                                                                   ELTNURSE
02482      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
02483                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
02484                     END-EXEC.                                     ELTNURSE
02485                                                                   ELTNURSE
02486  6105-EXIT.  EXIT.                                                ELTNURSE
02487      SKIP3                                                        ELTNURSE
02488  6106-ZERO-ALL-WITH-SAME-NO.                                      ELTNURSE
02489                                                                   ELTNURSE
02490      IF PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX) = WS-SUB3            ELTNURSE
02491          MOVE 'BP'         TO  CMF-RECORD-PREFIX                  ELTNURSE
02492          MOVE 'BEN-PR-ID'  TO  CMF-ELEMENT-SYSTEM-NAME            ELTNURSE
02493          MOVE PVN-BEN-ID (PVN-BEN-PROVN-IDX)                      ELTNURSE
02494                            TO  CMF-CODE-VALUE                     ELTNURSE
02495          MOVE SPACES       TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02496          MOVE +58          TO  WS-TEMP-NOT-USED-CNT               ELTNURSE
02497          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02498             THRU 9500-EXIT                                        ELTNURSE
02499          MOVE ZERO  TO PVN-COVG-SAME-AS (PVN-BEN-PROVN-IDX)       ELTNURSE
02500          ADD  +1    TO  WS-SUB2                                   ELTNURSE
02501          IF WS-CIA  >  20  OR  =  20                              ELTNURSE
02502              MOVE WS-CIA  TO  COF-NBR-DTL-LINES                   ELTNURSE
02503              EXEC CICS LINK PROGRAM ('ELUOUTPT')                  ELTNURSE
02504                             COMMAREA (DFHCOMMAREA)                ELTNURSE
02505                             END-EXEC                              ELTNURSE
02506              MOVE  +1  TO  WS-CIA.                                ELTNURSE
02507                                                                   ELTNURSE
02508  6106-EXIT.  EXIT.                                                ELTNURSE
02509 /                                                                 ELTNURSE
02510  6110-PLACE-OF-TREATMENT.                                         ELTNURSE
02511 ****************************************************************  ELTNURSE
02512 *              P L A C E   O F   T R E A T M E N T             *  ELTNURSE
02513 ****************************************************************  ELTNURSE
02514      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02515      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02516                          AND                                      ELTNURSE
02517         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
02518                                                  NOT =  ZERO      ELTNURSE
02519          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02520          MOVE +2                    TO  WS-CIA                    ELTNURSE
02521          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02522                                                                   ELTNURSE
02523      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02524      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02525                           AND                                     ELTNURSE
02526         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
02527                                                 NOT  =  ZERO      ELTNURSE
02528                           AND                                     ELTNURSE
02529         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02530          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02531          MOVE +2                    TO  WS-CIA                    ELTNURSE
02532          MOVE WS-SERVICES-RENDERED  TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02533                                                                   ELTNURSE
02534      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02535      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02536                           AND                                     ELTNURSE
02537         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
02538                                                  NOT  =  ZERO     ELTNURSE
02539          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
02540          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
02541                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02542          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02543                               TO  CMF-CODE-VALUE                  ELTNURSE
02544          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
02545          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02546          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02547             THRU 9500-EXIT.                                       ELTNURSE
02548                                                                   ELTNURSE
02549      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02550      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02551                           AND                                     ELTNURSE
02552         PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)         ELTNURSE
02553                                                 NOT  =  ZERO      ELTNURSE
02554          MOVE 'BP'  TO  CMF-RECORD-PREFIX                         ELTNURSE
02555          MOVE 'PLACE-TREAT-ELIG-IND'                              ELTNURSE
02556                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02557          MOVE PLP-PLACE-TREAT-ELIG-IND (PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02558                               TO  CMF-CODE-VALUE                  ELTNURSE
02559          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02560          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02561          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02562             THRU 9500-EXIT.                                       ELTNURSE
02563                                                                   ELTNURSE
02564      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02565         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02566          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02567             THRU 9200-EXIT.                                       ELTNURSE
02568                                                                   ELTNURSE
02569      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02570         SET PLT-INDEX2  TO  2                                     ELTNURSE
02571      ELSE                                                         ELTNURSE
02572         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02573                                                                   ELTNURSE
02574  6110-EXIT.  EXIT.                                                ELTNURSE
02575 /                                                                 ELTNURSE
02576  6120-PRIC-METH.                                                  ELTNURSE
02577 ****************************************************************  ELTNURSE
02578 *  P R O V I S I O N   P R I C I N G   M E T H O D   A N D     *  ELTNURSE
02579 *    V A R I A B L E   I N D E M N I T Y   P E R C E N T   O R *  ELTNURSE
02580 *      A D D I T I O N A L   P R I C I N G   P E R C E N T     *  ELTNURSE
02581 ****************************************************************  ELTNURSE
02582      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02583      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02584         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02585                                                              '19' ELTNURSE
02586         MOVE +2             TO  WS-CIA                            ELTNURSE
02587         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
02588         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
02589                                                                   ELTNURSE
02590      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02591      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
02592         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02593                                                        '19' AND   ELTNURSE
02594         NOT WS-ADD-A-BLANK-LINE                                   ELTNURSE
02595         MOVE +2             TO  WS-CIA                            ELTNURSE
02596         MOVE 'Y'            TO  WS-ADD-A-BLANK-IND                ELTNURSE
02597         MOVE WS-PAYABLE-AS  TO  COF-DTL-LINE(WS-CIA).             ELTNURSE
02598                                                                   ELTNURSE
02599      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02600      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02601         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
02602                            AND                                    ELTNURSE
02603         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02604         SET  PLT-INDEX2  TO  2                                    ELTNURSE
02605         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
02606                                                             ZERO  ELTNURSE
02607            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
02608            ADD +1  TO  WS-CIA                                     ELTNURSE
02609            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
02610                                                                   ELTNURSE
02611      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02612      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02613         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =  ZERO  ELTNURSE
02614                            AND                                    ELTNURSE
02615         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) = ZERO                ELTNURSE
02616         MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC                  ELTNURSE
02617         ADD +1  TO  WS-CIA                                        ELTNURSE
02618         MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).                  ELTNURSE
02619                                                                   ELTNURSE
02620      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZERO AND            ELTNURSE
02621         PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02622         SET  PLT-INDEX2  TO  2                                    ELTNURSE
02623         IF PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  =     ELTNURSE
02624                                                             ZERO  ELTNURSE
02625            MOVE WS-POSSIBLE-ERROR  TO  WS-DTL-BASIC               ELTNURSE
02626            ADD +1  TO  WS-CIA                                     ELTNURSE
02627            MOVE WS-BASIC  TO  COF-DTL-LINE(WS-CIA).               ELTNURSE
02628                                                                   ELTNURSE
02629      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02630      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02631         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02632                                                            =  ZEROELTNURSE
02633            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02634                                                            =  ZEROELTNURSE
02635               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
02636            ELSE                                                   ELTNURSE
02637               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
02638          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02639                                                  TO  WS-PERCENTAGEELTNURSE
02640         ELSE                                                      ELTNURSE
02641            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
02642          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02643                                                 TO  WS-PERCENTAGE.ELTNURSE
02644      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02645         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02646                                             ZERO AND  NOT =  '19' ELTNURSE
02647         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02648         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
02649         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
02650                                                    CMF-CODE-VALUE ELTNURSE
02651         MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02652         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
02653         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
02654            THRU 9600-EXIT.                                        ELTNURSE
02655                                                                   ELTNURSE
02656      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02657      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02658         IF PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)  =ELTNURSE
02659                                                               ZEROELTNURSE
02660            IF PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02661                                                            =  ZEROELTNURSE
02662               MOVE SPACES  TO  WS-PERCENT-FLD                     ELTNURSE
02663            ELSE                                                   ELTNURSE
02664               MOVE '%'  TO  WS-PERCENT-SIGN                       ELTNURSE
02665          MOVE PLP-ADDITIONAL-PRICING-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02666                                                  TO  WS-PERCENTAGEELTNURSE
02667         ELSE                                                      ELTNURSE
02668            MOVE '%'  TO  WS-PERCENT-SIGN                          ELTNURSE
02669          MOVE PLP-VARIABLE-INDEMNITY-PRCNT(PLT-INDEX1, PLT-INDEX2)ELTNURSE
02670                                                 TO  WS-PERCENTAGE.ELTNURSE
02671      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO AND        ELTNURSE
02672         PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  NOT =    ELTNURSE
02673                                             ZERO AND  NOT =  '19' ELTNURSE
02674         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02675         MOVE 'PROVN-PRICING-METHD'  TO  CMF-ELEMENT-SYSTEM-NAME   ELTNURSE
02676         MOVE PLP-PROVN-PRICING-METHD(PLT-INDEX1, PLT-INDEX2)  TO  ELTNURSE
02677                                                    CMF-CODE-VALUE ELTNURSE
02678         MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                   ELTNURSE
02679         MOVE +63  TO  WS-TEMP-NOT-USED-CNT                        ELTNURSE
02680         PERFORM 9600-CODES-MANUAL-WITH-PERCENT                    ELTNURSE
02681            THRU 9600-EXIT.                                        ELTNURSE
02682                                                                   ELTNURSE
02683      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02684          MOVE 'N'  TO  WS-ADD-A-BLANK-IND                         ELTNURSE
02685          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02686             THRU 9200-EXIT.                                       ELTNURSE
02687                                                                   ELTNURSE
02688      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02689         SET PLT-INDEX2  TO  2                                     ELTNURSE
02690      ELSE                                                         ELTNURSE
02691         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02692                                                                   ELTNURSE
02693  6120-EXIT.  EXIT.                                                ELTNURSE
02694 /                                                                 ELTNURSE
02695  6130-MAX-PER-VISIT.                                              ELTNURSE
02696 ****************************************************************  ELTNURSE
02697 *      M A X I M U M   A M O U N T   P E R   V I S I T         *  ELTNURSE
02698 ****************************************************************  ELTNURSE
02699      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02700      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02701                          AND                                      ELTNURSE
02702         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02703                                                  NOT =  ZERO      ELTNURSE
02704          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02705          MOVE +2                    TO  WS-CIA                    ELTNURSE
02706          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02707                                                                   ELTNURSE
02708      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02709      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02710                           AND                                     ELTNURSE
02711         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02712                                                 NOT  =  ZERO      ELTNURSE
02713                           AND                                     ELTNURSE
02714         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02715          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02716          MOVE +2                    TO  WS-CIA                    ELTNURSE
02717          MOVE WS-MAXIMUM-AMT        TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02718                                                                   ELTNURSE
02719      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02720      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02721                           AND                                     ELTNURSE
02722         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02723                                                  NOT  =  ZERO     ELTNURSE
02724          MOVE WS-BASIC-LIT     TO  WS-BASIC-SUPP                  ELTNURSE
02725          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
02726                                TO  WS-EDIT-MAX-AMT                ELTNURSE
02727          ADD  +1               TO  WS-CIA                         ELTNURSE
02728          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
02729                                                                   ELTNURSE
02730      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02731      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02732                           AND                                     ELTNURSE
02733         PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)            ELTNURSE
02734                                                 NOT  =  ZERO      ELTNURSE
02735          MOVE WS-SUPP-LIT      TO  WS-BASIC-SUPP                  ELTNURSE
02736          MOVE PLE-MAX-AMT-PER-VISIT (PLT-INDEX1, PLT-INDEX2)      ELTNURSE
02737                                TO  WS-EDIT-MAX-AMT                ELTNURSE
02738          ADD  +1               TO  WS-CIA                         ELTNURSE
02739          MOVE WS-MAX-AMT       TO  COF-DTL-LINE (WS-CIA).         ELTNURSE
02740                                                                   ELTNURSE
02741      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02742         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02743          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02744             THRU 9200-EXIT.                                       ELTNURSE
02745                                                                   ELTNURSE
02746      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02747         SET PLT-INDEX2  TO  2                                     ELTNURSE
02748      ELSE                                                         ELTNURSE
02749         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02750                                                                   ELTNURSE
02751  6130-EXIT.  EXIT.                                                ELTNURSE
02752 /                                                                 ELTNURSE
02753  6140-CERTIFICATION.                                              ELTNURSE
02754 ****************************************************************  ELTNURSE
02755 *      C E R T I F I C A T I O N   I N D I C A T O R           *  ELTNURSE
02756 ****************************************************************  ELTNURSE
02757      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02758      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02759                          AND                                      ELTNURSE
02760         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02761                                                  NOT =  '00'      ELTNURSE
02762          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02763          MOVE +2                    TO  WS-CIA                    ELTNURSE
02764          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02765                                                                   ELTNURSE
02766      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02767      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02768                           AND                                     ELTNURSE
02769         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02770                                                 NOT  =  '00'      ELTNURSE
02771                           AND                                     ELTNURSE
02772         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02773          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02774          MOVE +2                    TO  WS-CIA                    ELTNURSE
02775          MOVE WS-CERT-REQ           TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02776                                                                   ELTNURSE
02777      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02778      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02779                           AND                                     ELTNURSE
02780         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02781                                                  NOT  =  '00'     ELTNURSE
02782          MOVE 'BP'   TO  CMF-RECORD-PREFIX                        ELTNURSE
02783          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
02784                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02785          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02786                               TO  CMF-CODE-VALUE                  ELTNURSE
02787          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
02788          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02789          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02790             THRU 9500-EXIT.                                       ELTNURSE
02791                                                                   ELTNURSE
02792      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02793      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02794                           AND                                     ELTNURSE
02795         PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)             ELTNURSE
02796                                                 NOT  =  '00'      ELTNURSE
02797          MOVE 'BP'            TO  CMF-RECORD-PREFIX               ELTNURSE
02798          MOVE 'CERTFN-REQRM-IND'                                  ELTNURSE
02799                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02800          MOVE PLP-CERTFN-REQRM-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02801                               TO  CMF-CODE-VALUE                  ELTNURSE
02802          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02803          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02804          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02805             THRU 9500-EXIT.                                       ELTNURSE
02806                                                                   ELTNURSE
02807      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02808         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02809          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02810             THRU 9200-EXIT.                                       ELTNURSE
02811                                                                   ELTNURSE
02812      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02813         SET PLT-INDEX2  TO  2                                     ELTNURSE
02814      ELSE                                                         ELTNURSE
02815         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02816                                                                   ELTNURSE
02817  6140-EXIT.  EXIT.                                                ELTNURSE
02818 /                                                                 ELTNURSE
02819  6150-RECERTIFICATION.                                            ELTNURSE
02820 ****************************************************************  ELTNURSE
02821 *      R E C E R T I F I C A T I O N   I N D I C A T O R       *  ELTNURSE
02822 ****************************************************************  ELTNURSE
02823      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02824      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02825                          AND                                      ELTNURSE
02826         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02827                                                  NOT =  ZERO      ELTNURSE
02828          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02829          MOVE +2                    TO  WS-CIA                    ELTNURSE
02830          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02831                                                                   ELTNURSE
02832      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02833      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02834                           AND                                     ELTNURSE
02835         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02836                                                 NOT  =  ZERO      ELTNURSE
02837                           AND                                     ELTNURSE
02838         NOT  WS-ADD-A-BLANK-LINE                                  ELTNURSE
02839          MOVE 'Y'                   TO  WS-ADD-A-BLANK-IND        ELTNURSE
02840          MOVE +2                    TO  WS-CIA                    ELTNURSE
02841          MOVE WS-RECERT-REQ         TO  COF-DTL-LINE (WS-CIA).    ELTNURSE
02842                                                                   ELTNURSE
02843      SET  PLT-INDEX2  TO  1.                                      ELTNURSE
02844      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02845                           AND                                     ELTNURSE
02846         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02847                                                  NOT  =  ZERO     ELTNURSE
02848          MOVE 'BPE'  TO  CMF-RECORD-PREFIX                        ELTNURSE
02849          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
02850                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02851          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
02852                               TO  CMF-CODE-VALUE                  ELTNURSE
02853          MOVE WS-BASIC-LIT  TO  WS-TEMP-TEXT-AREA                 ELTNURSE
02854          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02855          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02856             THRU 9500-EXIT.                                       ELTNURSE
02857                                                                   ELTNURSE
02858      SET PLT-INDEX2  TO  2.                                       ELTNURSE
02859      IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX) NOT = ZERO            ELTNURSE
02860                           AND                                     ELTNURSE
02861         PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02862                                                 NOT  =  ZERO      ELTNURSE
02863          MOVE 'BPE'           TO  CMF-RECORD-PREFIX               ELTNURSE
02864          MOVE 'CERTN-REPETN-REQRD-IND'                            ELTNURSE
02865                               TO  CMF-ELEMENT-SYSTEM-NAME         ELTNURSE
02866          MOVE PLE-CERTN-REPETN-REQRD-IND (PLT-INDEX1, PLT-INDEX2) ELTNURSE
02867                               TO  CMF-CODE-VALUE                  ELTNURSE
02868          MOVE WS-SUPP-LIT  TO  WS-TEMP-TEXT-AREA                  ELTNURSE
02869          MOVE +63  TO  WS-TEMP-NOT-USED-CNT                       ELTNURSE
02870          PERFORM 9500-CALL-CODES-MANUAL-LONG                      ELTNURSE
02871             THRU 9500-EXIT.                                       ELTNURSE
02872                                                                   ELTNURSE
02873      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02874         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02875          PERFORM 9200-TEXT-OUTPUT-REQUEST                         ELTNURSE
02876             THRU 9200-EXIT.                                       ELTNURSE
02877                                                                   ELTNURSE
02878      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02879         SET PLT-INDEX2  TO  2                                     ELTNURSE
02880      ELSE                                                         ELTNURSE
02881         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02882                                                                   ELTNURSE
02883  6150-EXIT.  EXIT.                                                ELTNURSE
02884 /                                                                 ELTNURSE
02885  6160-SPILLOVR-COINS-N-DEDUC.                                     ELTNURSE
02886 ****************************************************************  ELTNURSE
02887 *          S P I L L O V E R   C O I N S U R A N C E           *  ELTNURSE
02888 ****************************************************************  ELTNURSE
02889      MOVE +1  TO  WS-CIA.                                         ELTNURSE
02890                                                                   ELTNURSE
02891      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02892      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTNURSE
02893                            AND                                    ELTNURSE
02894         PLP-SPILL-OVER-COINS-APL-IND (PLT-INDEX1, PLT-INDEX2)     ELTNURSE
02895                                                  NOT =  '0'       ELTNURSE
02896         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02897         MOVE 'SPILL-OVER-COINS-APL-IND'  TO                       ELTNURSE
02898                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
02899         MOVE PLP-SPILL-OVER-COINS-APL-IND(PLT-INDEX1, PLT-INDEX2) ELTNURSE
02900                                                TO  CMF-CODE-VALUE ELTNURSE
02901         MOVE WS-SPILLOVER        TO  WS-TEMP-TEXT-AREA            ELTNURSE
02902         MOVE +69                 TO  WS-TEMP-NOT-USED-CNT         ELTNURSE
02903         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
02904            THRU 9500-EXIT                                         ELTNURSE
02905         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
02906            THRU 9200-EXIT.                                        ELTNURSE
02907 ****************************************************************  ELTNURSE
02908 *          S P I L L O V E R   D E D U C T I B L E             *  ELTNURSE
02909 ****************************************************************  ELTNURSE
02910      MOVE +1  TO  WS-CIA.                                         ELTNURSE
02911                                                                   ELTNURSE
02912      SET  PLT-INDEX2  TO  2.                                      ELTNURSE
02913      IF PVN-SLOT-NBR-SUP (PVN-BEN-PROVN-IDX) NOT = ZERO           ELTNURSE
02914                            AND                                    ELTNURSE
02915         PLP-SPILL-OVER-DED-APL-IND (PLT-INDEX1, PLT-INDEX2)       ELTNURSE
02916                                                  NOT =  '0'       ELTNURSE
02917         MOVE 'BP'  TO  CMF-RECORD-PREFIX                          ELTNURSE
02918         MOVE 'SPILL-OVER-DED-APL-IND'  TO                         ELTNURSE
02919                                           CMF-ELEMENT-SYSTEM-NAME ELTNURSE
02920         MOVE PLP-SPILL-OVER-DED-APL-IND(PLT-INDEX1, PLT-INDEX2)   ELTNURSE
02921                                                 TO  CMF-CODE-VALUEELTNURSE
02922         MOVE WS-SPILLOVER      TO  WS-TEMP-TEXT-AREA              ELTNURSE
02923         MOVE +69               TO  WS-TEMP-NOT-USED-CNT           ELTNURSE
02924         PERFORM 9500-CALL-CODES-MANUAL-LONG                       ELTNURSE
02925            THRU 9500-EXIT                                         ELTNURSE
02926         PERFORM 9200-TEXT-OUTPUT-REQUEST                          ELTNURSE
02927            THRU 9200-EXIT.                                        ELTNURSE
02928                                                                   ELTNURSE
02929      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX) = ZEROES              ELTNURSE
02930         SET PLT-INDEX2  TO  2                                     ELTNURSE
02931      ELSE                                                         ELTNURSE
02932         SET PLT-INDEX2  TO  1.                                    ELTNURSE
02933                                                                   ELTNURSE
02934  6160-EXIT.  EXIT.                                                ELTNURSE
02935 /                                                                 ELTNURSE
02936  7000-ALL-LEVEL-TABS.                                             ELTNURSE
02937 ****************************************************************  ELTNURSE
02938 *                  A A R   T A B U L A R                       *  ELTNURSE
02939 ****************************************************************  ELTNURSE
02940      SET PLT-INDEX2  TO  1.                                       ELTNURSE
02941      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02942         PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
02943                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
02944         MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02945         MOVE +1  TO  COF-NBR-DTL-LINES                            ELTNURSE
02946         MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1)             ELTNURSE
02947      ELSE                                                         ELTNURSE
02948         SET PLT-INDEX2  TO  2                                     ELTNURSE
02949         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
02950            PLP-BEN-TAB-PROVN-ID-AAR(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
02951                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
02952            MOVE 'Y'  TO  WS-ADD-A-BLANK-IND                       ELTNURSE
02953            MOVE +1  TO  COF-NBR-DTL-LINES                         ELTNURSE
02954            MOVE WS-CONTRACT-RELATED  TO  COF-DTL-LINE(1).         ELTNURSE
02955                                                                   ELTNURSE
02956      IF WS-ADD-A-BLANK-LINE                                       ELTNURSE
02957         MOVE 'N'  TO  WS-ADD-A-BLANK-IND                          ELTNURSE
02958         MOVE 1  TO  WS-CIA                                        ELTNURSE
02959         EXEC CICS  LINK  PROGRAM('ELUOUTPT')                      ELTNURSE
02960             COMMAREA(DFHCOMMAREA)                                 ELTNURSE
02961         END-EXEC.                                                 ELTNURSE
02962 *--------------------------------------------------------------*  ELTNURSE
02963 *                  P P F   T A B U L A R                       *  ELTNURSE
02964 *--------------------------------------------------------------*  ELTNURSE
02965      SET PLT-INDEX2  TO  1.                                       ELTNURSE
02966      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
02967         PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
02968                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
02969         MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TO ELTNURSE
02970                                             KWA-GCTABULR-KEY      ELTNURSE
02971         PERFORM 9900-GET-TABULAR-RECORD                           ELTNURSE
02972            THRU 9900-EXIT                                         ELTNURSE
02973         EXEC  CICS  LINK  PROGRAM('ELGPPF')                       ELTNURSE
02974               COMMAREA(DFHCOMMAREA)                               ELTNURSE
02975         END-EXEC                                                  ELTNURSE
02976      ELSE                                                         ELTNURSE
02977         SET PLT-INDEX2  TO  2                                     ELTNURSE
02978         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
02979            PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
02980                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
02981          MOVE PLP-BEN-TAB-PROVN-ID-PPF(PLT-INDEX1, PLT-INDEX2)  TOELTNURSE
02982                                             KWA-GCTABULR-KEY      ELTNURSE
02983            PERFORM 9900-GET-TABULAR-RECORD                        ELTNURSE
02984               THRU 9900-EXIT                                      ELTNURSE
02985            EXEC  CICS  LINK  PROGRAM('ELGPPF')                    ELTNURSE
02986                  COMMAREA(DFHCOMMAREA)                            ELTNURSE
02987            END-EXEC.                                              ELTNURSE
02988 *--------------------------------------------------------------*  ELTNURSE
02989 *                  P V E   T A B U L A R                       *  ELTNURSE
02990 *--------------------------------------------------------------*  ELTNURSE
02991                                                                   ELTNURSE
02992      MOVE  +2     TO  WS-CIA.                                     ELTNURSE
02993      MOVE WS-PVE  TO  COF-DTL-LINE (2).                           ELTNURSE
02994      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTNURSE
02995         THRU 9200-EXIT.                                           ELTNURSE
02996                                                                   ELTNURSE
02997 *--------------------------------------------------------------*  ELTNURSE
02998 *                  A B M   T A B U L A R                       *  ELTNURSE
02999 *--------------------------------------------------------------*  ELTNURSE
03000      SET PLT-INDEX2  TO  1.                                       ELTNURSE
03001      IF PVN-SLOT-NBR-BAS (PVN-BEN-PROVN-IDX)      NOT =  ZERO     ELTNURSE
03002                              AND                                  ELTNURSE
03003         PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
03004                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03005         MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TO ELTNURSE
03006                                              KWA-GCTABULR-KEY     ELTNURSE
03007         PERFORM 9900-GET-TABULAR-RECORD                           ELTNURSE
03008            THRU 9900-EXIT                                         ELTNURSE
03009         EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                     ELTNURSE
03010               COMMAREA(DFHCOMMAREA)                               ELTNURSE
03011         END-EXEC                                                  ELTNURSE
03012      ELSE                                                         ELTNURSE
03013         SET PLT-INDEX2  TO  2                                     ELTNURSE
03014         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
03015            PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
03016                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03017          MOVE PLP-BEN-TAB-PROVN-ID-ABM(PLT-INDEX1, PLT-INDEX2)  TOELTNURSE
03018                                             KWA-GCTABULR-KEY      ELTNURSE
03019            PERFORM 9900-GET-TABULAR-RECORD                        ELTNURSE
03020               THRU 9900-EXIT                                      ELTNURSE
03021            EXEC  CICS  LINK  PROGRAM('ELGMAXIM')                  ELTNURSE
03022                  COMMAREA(DFHCOMMAREA)                            ELTNURSE
03023            END-EXEC.                                              ELTNURSE
03024 *--------------------------------------------------------------*  ELTNURSE
03025 *                  A C L   T A B U L A R                       *  ELTNURSE
03026 *--------------------------------------------------------------*  ELTNURSE
03027      SET PLT-INDEX2  TO  1.                                       ELTNURSE
03028      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
03029         PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
03030                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03031         MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TO ELTNURSE
03032                                              KWA-GCTABULR-KEY     ELTNURSE
03033         PERFORM 9900-GET-TABULAR-RECORD                           ELTNURSE
03034            THRU 9900-EXIT                                         ELTNURSE
03035         EXEC  CICS  LINK  PROGRAM('ELGCOINS')                     ELTNURSE
03036               COMMAREA(DFHCOMMAREA)                               ELTNURSE
03037         END-EXEC                                                  ELTNURSE
03038      ELSE                                                         ELTNURSE
03039         SET PLT-INDEX2  TO  2                                     ELTNURSE
03040         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
03041            PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
03042                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03043          MOVE PLP-BEN-TAB-PROVN-ID-ACL(PLT-INDEX1, PLT-INDEX2)  TOELTNURSE
03044                                              KWA-GCTABULR-KEY     ELTNURSE
03045            PERFORM 9900-GET-TABULAR-RECORD                        ELTNURSE
03046               THRU 9900-EXIT                                      ELTNURSE
03047            EXEC  CICS  LINK  PROGRAM('ELGCOINS')                  ELTNURSE
03048                  COMMAREA(DFHCOMMAREA)                            ELTNURSE
03049            END-EXEC.                                              ELTNURSE
03050 *--------------------------------------------------------------*  ELTNURSE
03051 *                  A D L   T A B U L A R                       *  ELTNURSE
03052 *--------------------------------------------------------------*  ELTNURSE
03053      SET PLT-INDEX2  TO  1.                                       ELTNURSE
03054      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
03055         PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
03056                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03057         MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TO ELTNURSE
03058                                             KWA-GCTABULR-KEY      ELTNURSE
03059         PERFORM 9900-GET-TABULAR-RECORD                           ELTNURSE
03060            THRU 9900-EXIT                                         ELTNURSE
03061         EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                     ELTNURSE
03062               COMMAREA(DFHCOMMAREA)                               ELTNURSE
03063         END-EXEC                                                  ELTNURSE
03064      ELSE                                                         ELTNURSE
03065         SET PLT-INDEX2  TO  2                                     ELTNURSE
03066         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
03067            PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
03068                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03069          MOVE PLP-BEN-TAB-PROVN-ID-ADL(PLT-INDEX1, PLT-INDEX2)  TOELTNURSE
03070                                               KWA-GCTABULR-KEY    ELTNURSE
03071            PERFORM 9900-GET-TABULAR-RECORD                        ELTNURSE
03072               THRU 9900-EXIT                                      ELTNURSE
03073            EXEC  CICS  LINK  PROGRAM('ELGDEDBL')                  ELTNURSE
03074                  COMMAREA(DFHCOMMAREA)                            ELTNURSE
03075            END-EXEC.                                              ELTNURSE
03076 *--------------------------------------------------------------*  ELTNURSE
03077 *                  A O L   T A B U L A R                       *  ELTNURSE
03078 *--------------------------------------------------------------*  ELTNURSE
03079      SET PLT-INDEX2  TO  1.                                       ELTNURSE
03080      IF PVN-SLOT-NBR-BAS(PVN-BEN-PROVN-IDX)      NOT =  ZERO AND  ELTNURSE
03081         PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =   ELTNURSE
03082                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03083         MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TO ELTNURSE
03084                                             KWA-GCTABULR-KEY      ELTNURSE
03085         PERFORM 9900-GET-TABULAR-RECORD                           ELTNURSE
03086            THRU 9900-EXIT                                         ELTNURSE
03087         EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                     ELTNURSE
03088               COMMAREA(DFHCOMMAREA)                               ELTNURSE
03089         END-EXEC                                                  ELTNURSE
03090      ELSE                                                         ELTNURSE
03091         SET PLT-INDEX2  TO  2                                     ELTNURSE
03092         IF PVN-SLOT-NBR-SUP(PVN-BEN-PROVN-IDX)     NOT =  ZERO ANDELTNURSE
03093            PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  NOT =ELTNURSE
03094                    ZERO AND  NOT =  SPACES AND  NOT =  LOW-VALUES ELTNURSE
03095          MOVE PLP-BEN-TAB-PROVN-ID-AOL(PLT-INDEX1, PLT-INDEX2)  TOELTNURSE
03096                                              KWA-GCTABULR-KEY     ELTNURSE
03097            PERFORM 9900-GET-TABULAR-RECORD                        ELTNURSE
03098               THRU 9900-EXIT                                      ELTNURSE
03099            EXEC  CICS  LINK  PROGRAM('ELGOUTPX')                  ELTNURSE
03100                  COMMAREA(DFHCOMMAREA)                            ELTNURSE
03101            END-EXEC.                                              ELTNURSE
03102                                                                   ELTNURSE
03103 *--------------------------------------------------------------*  ELTNURSE
03104 *       G E N E R A L   A C C U M   M E S S A G E              *  ELTNURSE
03105 *--------------------------------------------------------------*  ELTNURSE
03106                                                                   ELTNURSE
03107      ADD   +2     TO  WS-CIA.                                     ELTNURSE
03108      MOVE WS-ACCUM-MSG1 TO  COF-DTL-LINE (WS-CIA).                ELTNURSE
03109      PERFORM 9200-TEXT-OUTPUT-REQUEST                             ELTNURSE
03110         THRU 9200-EXIT.                                           ELTNURSE
03111                                                                   ELTNURSE
03112  7000-EXIT.  EXIT.                                                ELTNURSE
03113 /                                                                 ELTNURSE
03114 ****************************************************************  ELTNURSE
03115 *          PRINT THE HEADER LINES FOR THE SCREEN               *  ELTNURSE
03116 ****************************************************************  ELTNURSE
03117  9100-HEADER-OUTPUT-REQUEST.                                      ELTNURSE
03118                                                                   ELTNURSE
03119      MOVE +2             TO  COF-NBR-HDR-LINES.                   ELTNURSE
03120      SET COF-NEW-PAGE TO TRUE.                                    ELTNURSE
03121                                                                   ELTNURSE
03122      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
03123                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
03124                     END-EXEC.                                     ELTNURSE
03125                                                                   ELTNURSE
03126  9100-EXIT.  EXIT.                                                ELTNURSE
03127      SKIP3                                                        ELTNURSE
03128 ****************************************************************  ELTNURSE
03129 *          PRINT THE TEXT INFORMATION FOR THE SCREEN           *  ELTNURSE
03130 ****************************************************************  ELTNURSE
03131  9200-TEXT-OUTPUT-REQUEST.                                        ELTNURSE
03132                                                                   ELTNURSE
03133      MOVE WS-CIA  TO  COF-NBR-DTL-LINES.                          ELTNURSE
03134      MOVE +0      TO  COF-NBR-HDR-LINES                           ELTNURSE
03135                       WS-CIA.                                     ELTNURSE
03136      SET COF-CONTINUE TO TRUE.                                    ELTNURSE
03137                                                                   ELTNURSE
03138      EXEC CICS LINK PROGRAM ('ELUOUTPT')                          ELTNURSE
03139                     COMMAREA (DFHCOMMAREA)                        ELTNURSE
03140                     END-EXEC.                                     ELTNURSE
03141                                                                   ELTNURSE
03142  9200-EXIT.  EXIT.                                                ELTNURSE
03143 /                                                                 ELTNURSE
03144  9500-CALL-CODES-MANUAL-LONG.                                     ELTNURSE
03145                                                                   ELTNURSE
03146      INITIALIZE CMF-RETURN-CODE                                   ELTNURSE
03147                 TCAR-FROM-AREA.                                   ELTNURSE
03148                                                                   ELTNURSE
03149      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTNURSE
03150                       COMMAREA(DFHCOMMAREA)                       ELTNURSE
03151      END-EXEC.                                                    ELTNURSE
03152                                                                   ELTNURSE
03153      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTNURSE
03154      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
03155          ADDRESS OF CMF-DESCR.                                    ELTNURSE
03156                                                                   ELTNURSE
03157      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTNURSE
03158         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTNURSE
03159         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTNURSE
03160      ELSE                                                         ELTNURSE
03161         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTNURSE
03162                                                                   ELTNURSE
03163      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTNURSE
03164          VARYING WS-SUB1 FROM 1 BY 1                              ELTNURSE
03165          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTNURSE
03166                                                                   ELTNURSE
03167      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTNURSE
03168                                                                   ELTNURSE
03169      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTNURSE
03170      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTNURSE
03171      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTNURSE
03172                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTNURSE
03173                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTNURSE
03174      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTNURSE
03175                                                                   ELTNURSE
03176                                                                   ELTNURSE
03177      IF WS-MOVE-LINES-TO-CIA                                      ELTNURSE
03178         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTNURSE
03179            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTNURSE
03180                                             WS-TEMP-NOT-USED-CNT  ELTNURSE
03181            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTNURSE
03182               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTNURSE
03183                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTNURSE
03184            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTNURSE
03185            ADD +1  TO  WS-CIA                                     ELTNURSE
03186            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTNURSE
03187         ELSE                                                      ELTNURSE
03188            ADD +1  TO  WS-CIA                                     ELTNURSE
03189            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTNURSE
03190                                                                   ELTNURSE
03191      IF WS-MOVE-LINES-TO-CIA                                      ELTNURSE
03192         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTNURSE
03193            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTNURSE
03194               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTNURSE
03195                              UNTIL   WS-SUB1 >                    ELTNURSE
03196                              TCAR-OUTPUT-FIELDS-USED              ELTNURSE
03197         ELSE                                                      ELTNURSE
03198            CONTINUE                                               ELTNURSE
03199      ELSE                                                         ELTNURSE
03200         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTNURSE
03201                                                                   ELTNURSE
03202  9500-EXIT.  EXIT.                                                ELTNURSE
03203                                                                   ELTNURSE
03204  9540-MOVE-LINES-OUT.                                             ELTNURSE
03205      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTNURSE
03206             ' ' DELIMITED BY SIZE                                 ELTNURSE
03207             CMF-DESCR-LINE (WS-SUB1) DELIMITED BY SIZE            ELTNURSE
03208      INTO TCAR-FROM-AREA.                                         ELTNURSE
03209                                                                   ELTNURSE
03210  9540-EXIT.  EXIT.                                                ELTNURSE
03211                                                                   ELTNURSE
03212  9550-CONCATENATE-TO-TEMP-TEXT.                                   ELTNURSE
03213      ADD +1  TO  WS-TEMP-NOT-USED-CNT.                            ELTNURSE
03214      MOVE TCAR-OPF-DIGIT(1, WS-SUB1)  TO                          ELTNURSE
03215                          WS-TEMP-TEXT-CHAR(WS-TEMP-NOT-USED-CNT). ELTNURSE
03216                                                                   ELTNURSE
03217  9550-EXIT.  EXIT.                                                ELTNURSE
03218                                                                   ELTNURSE
03219 /    C O D E S   M A N U A L   F O R   L O N G   D E S C R I P T  ELTNURSE
03220  9600-CODES-MANUAL-WITH-PERCENT.                                  ELTNURSE
03221                                                                   ELTNURSE
03222      INITIALIZE CMF-RETURN-CODE                                   ELTNURSE
03223                 TCAR-FROM-AREA.                                   ELTNURSE
03224                                                                   ELTNURSE
03225      EXEC CICS  LINK  PROGRAM('ELUCMIF')                          ELTNURSE
03226                       COMMAREA(DFHCOMMAREA)                       ELTNURSE
03227      END-EXEC.                                                    ELTNURSE
03228                                                                   ELTNURSE
03229      SET CIA-ELSCMDSC-DDN TO TRUE.                                ELTNURSE
03230      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
03231          ADDRESS OF CMF-DESCR.                                    ELTNURSE
03232                                                                   ELTNURSE
03233      IF WS-TEMP-NOT-USED-CNT  =  ZERO                             ELTNURSE
03234         MOVE +79  TO  TCAR-OUTPUT-FIELD-1-LEN                     ELTNURSE
03235         MOVE WS-TEMP-TEXT-AREA TO TCAR-FROM-AREA                  ELTNURSE
03236      ELSE                                                         ELTNURSE
03237         MOVE WS-TEMP-NOT-USED-CNT  TO  TCAR-OUTPUT-FIELD-1-LEN.   ELTNURSE
03238                                                                   ELTNURSE
03239      STRING TCAR-FROM-AREA DELIMITED BY '  '                      ELTNURSE
03240             WS-PERCENT-FLD DELIMITED BY SIZE                      ELTNURSE
03241      INTO TCAR-FROM-AREA.                                         ELTNURSE
03242                                                                   ELTNURSE
03243      PERFORM 9540-MOVE-LINES-OUT THRU 9540-EXIT                   ELTNURSE
03244          VARYING WS-SUB1 FROM 1 BY 1                              ELTNURSE
03245          UNTIL WS-SUB1 GREATER THAN CMF-NBR-DESCR-LINES.          ELTNURSE
03246                                                                   ELTNURSE
03247                                                                   ELTNURSE
03248      PERFORM TCPR-000-TEXT-COMPRESSION.                           ELTNURSE
03249                                                                   ELTNURSE
03250      MOVE TCAR-TO-SUB  TO  TCAR-L.                                ELTNURSE
03251      MOVE +4  TO  TCAR-OUTPUT-FIELD-COUNT.                        ELTNURSE
03252      MOVE +79  TO  TCAR-OUTPUT-FIELD-2-LEN,                       ELTNURSE
03253                    TCAR-OUTPUT-FIELD-3-LEN,                       ELTNURSE
03254                    TCAR-OUTPUT-FIELD-4-LEN.                       ELTNURSE
03255      PERFORM TCPR-000-TEXT-UNSTRING.                              ELTNURSE
03256                                                                   ELTNURSE
03257      IF WS-MOVE-LINES-TO-CIA                                      ELTNURSE
03258         IF WS-TEMP-NOT-USED-CNT  NOT =  ZERO                      ELTNURSE
03259            COMPUTE WS-TEMP-NOT-USED-CNT  =  79  -                 ELTNURSE
03260                                             WS-TEMP-NOT-USED-CNT  ELTNURSE
03261            PERFORM 9550-CONCATENATE-TO-TEMP-TEXT                  ELTNURSE
03262               THRU 9550-EXIT VARYING WS-SUB1 FROM 1 BY 1          ELTNURSE
03263                              UNTIL  WS-TEMP-NOT-USED-CNT  >  +78  ELTNURSE
03264            MOVE ZERO  TO  WS-TEMP-NOT-USED-CNT                    ELTNURSE
03265            ADD +1  TO  WS-CIA                                     ELTNURSE
03266            MOVE WS-TEMP-TEXT-AREA  TO  COF-DTL-LINE(WS-CIA)       ELTNURSE
03267         ELSE                                                      ELTNURSE
03268            ADD +1  TO  WS-CIA                                     ELTNURSE
03269            MOVE TCAR-OPF-DATA(1)  TO  COF-DTL-LINE(WS-CIA).       ELTNURSE
03270                                                                   ELTNURSE
03271      IF WS-MOVE-LINES-TO-CIA                                      ELTNURSE
03272         IF TCAR-OUTPUT-FIELDS-USED  >  1                          ELTNURSE
03273            PERFORM 9660-MOVE-LINES-TO-CIA                         ELTNURSE
03274               THRU 9660-EXIT VARYING WS-SUB1 FROM 2 BY 1          ELTNURSE
03275                              UNTIL   WS-SUB1 >                    ELTNURSE
03276                              TCAR-OUTPUT-FIELDS-USED              ELTNURSE
03277         ELSE                                                      ELTNURSE
03278            CONTINUE                                               ELTNURSE
03279      ELSE                                                         ELTNURSE
03280         MOVE 'Y'  TO  WS-MOVE-LINES-IND.                          ELTNURSE
03281                                                                   ELTNURSE
03282  9600-EXIT.  EXIT.                                                ELTNURSE
03283                                                                   ELTNURSE
03284  9660-MOVE-LINES-TO-CIA.                                          ELTNURSE
03285      ADD +1  TO  WS-CIA.                                          ELTNURSE
03286      MOVE TCAR-OPF-DATA(WS-SUB1)  TO  COF-DTL-LINE(WS-CIA).       ELTNURSE
03287                                                                   ELTNURSE
03288  9660-EXIT.  EXIT.                                                ELTNURSE
03289 /                                                                 ELTNURSE
03290 /                                                                 ELTNURSE
03291 ***************************************************************** ELTNURSE
03292 *            G E T   T A B U L A R   R E C O R D                  ELTNURSE
03293 *                                                                 ELTNURSE
03294 *    THIS ROUTINE READS THE TABULAR RECORD FOR THE PPF, PVE,      ELTNURSE
03295 *  AND THE ALL LEVEL TABULAR PROGRAMS WHICH WILL BE CALLED        ELTNURSE
03296 *  TO DISPLAY.                                                    ELTNURSE
03297 *                                                                 ELTNURSE
03298 ***************************************************************** ELTNURSE
03299  9900-GET-TABULAR-RECORD.                                         ELTNURSE
03300                                                                   ELTNURSE
03301      SET CIA-GCTABULR-DDN TO TRUE.                                ELTNURSE
03302      CALL 'ELUSETAD' USING DFHCOMMAREA                            ELTNURSE
03303          ADDRESS OF IOP-INPUT-OUTPUT-PARAMETERS.                  ELTNURSE
03304                                                                   ELTNURSE
03305      MOVE KWA-GCTABULR-KEY               TO IOP-FILE-KEY.         ELTNURSE
03306      SET CIA-GCTABULR-DDN                TO TRUE.                 ELTNURSE
03307      SET IOP-RD                          TO TRUE.                 ELTNURSE
03308      SET IOP-FCQ-NONE                    TO TRUE.                 ELTNURSE
03309      SET IOP-KVQ-NONE                    TO TRUE.                 ELTNURSE
03310      SET IOP-STG-MODE-LOCATE             TO TRUE.                 ELTNURSE
03311                                                                   ELTNURSE
03312      EXEC CICS LINK                                               ELTNURSE
03313                PROGRAM ('ELUIOPGM')                               ELTNURSE
03314                COMMAREA (DFHCOMMAREA)                             ELTNURSE
03315      END-EXEC.                                                    ELTNURSE
03316                                                                   ELTNURSE
03317      IF IOP-RC-NOTFND                                             ELTNURSE
03318         SET CIA-AB-NOTFND-GCTABULR TO TRUE                        ELTNURSE
03319         EXEC CICS ABEND                                           ELTNURSE
03320                   ABCODE(CIA-ABCODE)                              ELTNURSE
03321         END-EXEC                                                  ELTNURSE
03322      ELSE                                                         ELTNURSE
03323          IF NOT IOP-RC-OK                                         ELTNURSE
03324             SET CIA-AB-CRITIO TO TRUE                             ELTNURSE
03325             EXEC CICS ABEND                                       ELTNURSE
03326                       ABCODE(CIA-ABCODE)                          ELTNURSE
03327             END-EXEC                                              ELTNURSE
03328      END-IF.                                                      ELTNURSE
03329                                                                   ELTNURSE
03330  9900-EXIT.  EXIT.                                                ELTNURSE
03331 /                                                                 ELTNURSE
03332      COPY ELSTCOMP.                                               ELTNURSE
