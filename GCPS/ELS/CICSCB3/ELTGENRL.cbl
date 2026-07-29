00001   IDENTIFICATION DIVISION.                                        09/03/03
00002   PROGRAM-ID. ELTGENRL.                                           ELTGENRL
00003   AUTHOR. JOHN T CURIN, KEANE, INC.                                  LV002
00004   DATE-WRITTEN. 02/24/87.                                         ELTGENRL
00005   DATE-COMPILED.                                                  ELTGENRL
00006 ******************************************************************ELTGENRL
00007 *                                                                 ELTGENRL
00008 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*     ELTGENRL
00009 *       *-*  U P D A T E   H I S T O R Y         *-*      *-*     ELTGENRL
00010 *       *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*     ELTGENRL
00011 *                                                                 ELTGENRL
00012 **-CHG-NUM-* *-DATE-* *WHO* *---------DESCRIPTION---------------- ELTGENRL
00013 *                                                                 ELTGENRL
00014 *  1.01      05/03/88  AKK            CHANGED PLELITCOMP TO ELITCOELTGENRL
00015 *                                                                 ELTGENRL
00016 *  1.02      05/16/88  AKK            ADDED CODE TO CREATE TWO COLELTGENRL
00017 *                                     OUTPUT FORMAT               ELTGENRL
00018 *  1.03      11/02/89  RKH            CHANGED STORAGE MANAGER     ELTGENRL
00019 *                                                                 ELTGENRL
00020 *       13-AUG-2003 AKK       TESTING FOR ORDER OF COMPILE       *ELTGENRL
00021 *                                                                 ELTGENRL
00022 ******************************************************************ELTGENRL
00023                                                                   ELTGENRL
00024   ENVIRONMENT DIVISION.                                           ELTGENRL
00025   DATA DIVISION.                                                  ELTGENRL
00026   WORKING-STORAGE SECTION.                                        ELTGENRL
00027   01 WS-MISC.                                                     ELTGENRL
00028       05 WS-START-IT       PIC X(24) VALUE                        ELTGENRL
00029       '***ELTGENRL WS BEGINS***'.                                 ELTGENRL
00030                                                                   ELTGENRL
00031       05 WS-SUB            PIC S9999 COMP SYNC VALUE +0.          ELTGENRL
00032       05 WS-ADDN-CNT       PIC S9999 COMP SYNC VALUE +0.          ELTGENRL
00033       05 TCAR-HOLD-LINE   PIC X(46)            VALUE SPACES.      ELTGENRL
00034       05 TCAR-HOLD-LINEA  PIC X(46)            VALUE SPACES.      ELTGENRL
00035       05 WS-DISPLAY-ANNL-REINST-AMT PIC $,$$$,$$9-.               ELTGENRL
00036       05 WS-D-GOOD-HLTH-REINST-AFTR-BEN PIC $,$$$,$$9-.           ELTGENRL
00037       05 WS-ADDITIONAL-COUNTER PIC X(01).                         ELTGENRL
00038          88 DO-ADDN-LINE-CNT            VALUE 'Y'.                ELTGENRL
00039          88 NO-ADDN-LINE-CNT            VALUE 'N'.                ELTGENRL
00040       05 WS-TOB-IND    PIC X(02).                                 ELTGENRL
00041          88 TOB-SPECIAL-BC-VALUE VALUE '0D' '0E'                  ELTGENRL
00042                                        '0K' '0N'                  ELTGENRL
00043                                        '0Q' '0R'                  ELTGENRL
00044                                        '0U' '0V'                  ELTGENRL
00045                                        '0X' '02'                  ELTGENRL
00046                                        '05' '07'                  ELTGENRL
00047                                        '08' ' 4'.                 ELTGENRL
00048          88 TOB-SPECIAL-BS-VALUE VALUE ' C' '0C'                  ELTGENRL
00049                                        '0D' '0J'                  ELTGENRL
00050                                        '0K' '0L'                  ELTGENRL
00051                                        '0M' '0N'                  ELTGENRL
00052                                        '0P' '0Q'                  ELTGENRL
00053                                        '0S' '02'                  ELTGENRL
00054                                        '05' '07'                  ELTGENRL
00055                                        '08'.                      ELTGENRL
00056          88 TOB-SPECIAL-MM-VALUE VALUE '0B' '0C'                  ELTGENRL
00057                                        '0J' '0K'                  ELTGENRL
00058                                        '0M' '0N'                  ELTGENRL
00059                                        '0P' '02'                  ELTGENRL
00060                                        '03'.                      ELTGENRL
00061 *                                                                 ELTGENRL
00062   01  WS-DISPLAY-LINES.                                           ELTGENRL
00063       10 WS-DEPENDANT-HDR   PIC X(13) VALUE 'DEPENDANT AGE'.      ELTGENRL
00064       10 WS-FAMILY-SECURITY-HDR                                   ELTGENRL
00065                                 PIC X(25) VALUE                   ELTGENRL
00066          'FAMILY SECURITY PROVISION'.                             ELTGENRL
00067       10 WS-REINSTATEMENT   PIC X(25) VALUE 'REINSTATEMENT'.      ELTGENRL
00068       10 WS-TIMELY-FILING-HDR                                     ELTGENRL
00069                             PIC X(13) VALUE 'TIMELY FILING'.      ELTGENRL
00070       10 WS-TOB-HEADING1    PIC X(21)  VALUE                      ELTGENRL
00071                             'EXTENSION OF BENEFITS'.              ELTGENRL
00072       10 WS-TOB-HEADING2    PIC X(28)  VALUE                      ELTGENRL
00073                             'EXTENSION OF BENEFITS, CONTD'.       ELTGENRL
00074 *                                                                 ELTGENRL
00075   01  WS-OUTPUT-AREA.                                             ELTGENRL
00076       10  WS-LINE-CNT       PIC S9(04)  VALUE +0.                 ELTGENRL
00077       10  WS-OUTPUT         PIC X(1580) VALUE SPACES.             ELTGENRL
00078       10  WS-OUTPUT-ENTRY REDEFINES WS-OUTPUT                     ELTGENRL
00079                                     OCCURS 20 TIMES               ELTGENRL
00080                                     INDEXED BY WS-OUTPUT-IDX.     ELTGENRL
00081           15  WS-OUTPUT-LINE.                                     ELTGENRL
00082               20 WS-HEADING PIC X(30).                            ELTGENRL
00083               20 WS-DIVIDER PIC X(01).                            ELTGENRL
00084               20 FILLER     PIC X(01).                            ELTGENRL
00085               20 WS-TEXT    PIC X(47).                            ELTGENRL
00086 *                                                                 ELTGENRL
00087   01 WS-MASK-LINE.                                                ELTGENRL
00088       10  FILLER            PIC X(30)  VALUE SPACES.              ELTGENRL
00089       10  FILLER            PIC X(01)  VALUE '|'.                 ELTGENRL
00090       10  FILLER            PIC X(48)  VALUE SPACES.              ELTGENRL
00091 *                                                                 ELTGENRL
00092                                                                   ELTGENRL
00093 **            D I S P L A Y L I N E S                             ELTGENRL
00094   01 WS-ELS-DISPLAY-LINES.                                        ELTGENRL
00095                                                                   ELTGENRL
00096     05 WS-HDR-2-DEPENDENT.                                        ELTGENRL
00097       10 FILLER              PIC X(28) VALUE SPACES.              ELTGENRL
00098       10 FILLER              PIC X(18)                            ELTGENRL
00099           VALUE 'DEPENDENT COVERAGE'.                             ELTGENRL
00100       10 FILLER              PIC X(33) VALUE LOW-VALUES.          ELTGENRL
00101                                                                   ELTGENRL
00102     05 WS-HDR-2-TERM.                                             ELTGENRL
00103       10 FILLER              PIC X(18) VALUE SPACES.              ELTGENRL
00104       10 FILLER              PIC X(43) VALUE                      ELTGENRL
00105           'TERMINATION OF BENEFITS'.                              ELTGENRL
00106       10 FILLER              PIC X(18) VALUE LOW-VALUES.          ELTGENRL
00107                                                                   ELTGENRL
00108     05 WS-HDR-2-IB-TERM.                                          ELTGENRL
00109       10 FILLER              PIC X(18) VALUE SPACES.              ELTGENRL
00110       10 FILLER              PIC X(43) VALUE                      ELTGENRL
00111           'INSTITUTIONAL BASIC TERMINATION OF BENEFITS'.          ELTGENRL
00112       10 FILLER              PIC X(18) VALUE LOW-VALUES.          ELTGENRL
00113                                                                   ELTGENRL
00114     05 WS-HDR-2-SUPP-TERM.                                        ELTGENRL
00115       10 FILLER              PIC X(21) VALUE SPACES.              ELTGENRL
00116       10 FILLER              PIC X(36)                            ELTGENRL
00117           VALUE 'SUPPLEMENTAL TERMINATION OF BENEFITS'.           ELTGENRL
00118       10 FILLER              PIC X(22) VALUE LOW-VALUES.          ELTGENRL
00119                                                                   ELTGENRL
00120     05 WS-HDR-2-PB-TERM.                                          ELTGENRL
00121       10 FILLER              PIC X(18) VALUE SPACES.              ELTGENRL
00122       10 FILLER              PIC X(43) VALUE                      ELTGENRL
00123           ' PROFESSIONAL BASIC TERMINATION OF BENEFITS'.          ELTGENRL
00124       10 FILLER              PIC X(18) VALUE LOW-VALUES.          ELTGENRL
00125                                                                   ELTGENRL
00126     05 WS-HDR-2-FAMILY.                                           ELTGENRL
00127       10 FILLER              PIC X(28) VALUE SPACES.              ELTGENRL
00128       10 FILLER              PIC X(24)                            ELTGENRL
00129           VALUE 'FAMILY SECURITY COVERAGE'.                       ELTGENRL
00130       10 FILLER              PIC X(27) VALUE LOW-VALUES.          ELTGENRL
00131                                                                   ELTGENRL
00132     05 WS-HDR-2-TIMELY-FILING.                                    ELTGENRL
00133       10 FILLER              PIC X(34) VALUE SPACES.              ELTGENRL
00134       10 FILLER              PIC X(13)                            ELTGENRL
00135           VALUE 'TIMELY FILING'.                                  ELTGENRL
00136       10 FILLER              PIC X(33) VALUE LOW-VALUES.          ELTGENRL
00137                                                                   ELTGENRL
00138     05 WS-HDR-2-BENEFIT-REINSTATEMENT.                            ELTGENRL
00139       10 FILLER              PIC X(30) VALUE SPACES.              ELTGENRL
00140       10 FILLER              PIC X(21)                            ELTGENRL
00141           VALUE 'BENEFIT REINSTATEMENT'.                          ELTGENRL
00142       10 FILLER              PIC X(29) VALUE LOW-VALUES.          ELTGENRL
00143                                                                   ELTGENRL
00144     05 WS-DEPENDENT-MAXIMUM-AGE.                                  ELTGENRL
00145       10 DTL-DEP-MAX-AGE     PIC Z9.                              ELTGENRL
00146       10 FILLER              PIC X(107)   VALUE                   ELTGENRL
00147        ' IS THE MAXIMUM AGE FOR WHICH AN UNMARRIED DEPENDENT IS ELELTGENRL
00148 -       'IGIBLE FOR BENEFITS IF NOT DISABLED OR A STUDENT.'.      ELTGENRL
00149                                                                   ELTGENRL
00150     05 FILLER REDEFINES WS-DEPENDENT-MAXIMUM-AGE.                 ELTGENRL
00151        10 WS-DEP-SENTENCE1   PIC X(79).                           ELTGENRL
00152        10 WS-DEP-SENTENCE2   PIC X(30).                           ELTGENRL
00153                                                                   ELTGENRL
00154     05 WS-STUDENT-CERTIFICATION.                                  ELTGENRL
00155       10 FILLER              PIC X(36)    VALUE                   ELTGENRL
00156         'IF UNMARRIED DEPENDENT IS A STUDENT '.                   ELTGENRL
00157                                                                   ELTGENRL
00158     05 WS-STUDENT-MAXIMUM-AGE.                                    ELTGENRL
00159       10 DTL-STU-MAX-AGE     PIC Z9.                              ELTGENRL
00160       10 FILLER              PIC X(98)    VALUE                   ELTGENRL
00161        ' IS THE MAXIMUM AGE FOR WHICH AN UNMARRIED DEPENDENT WITH ELTGENRL
00162 -       'STUDENT STATUS IS ELIGIBLE FOR COVERAGE.'.               ELTGENRL
00163                                                                   ELTGENRL
00164     05 FILLER REDEFINES WS-STUDENT-MAXIMUM-AGE.                   ELTGENRL
00165        10 WS-STU-SENTENCE1   PIC X(78).                           ELTGENRL
00166        10 WS-STU-SENTENCE2   PIC X(22).                           ELTGENRL
00167                                                                   ELTGENRL
00168     05 WS-TERMINATION.                                            ELTGENRL
00169       10 FILLER              PIC X(96)    VALUE                   ELTGENRL
00170        'THE RULES FOR EXTENDING BENEFITS BEYOND THE TERMINATION DAELTGENRL
00171 -       'TE AND OR MEMBERSHIP CANCEL DATE ARE: '.                 ELTGENRL
00172                                                                   ELTGENRL
00173     05 FILLER REDEFINES WS-TERMINATION.                           ELTGENRL
00174        10 WS-TER-SENTENCE1   PIC X(79).                           ELTGENRL
00175        10 WS-TER-SENTENCE2   PIC X(17).                           ELTGENRL
00176                                                                   ELTGENRL
00177      05 WS-EXTEND-GENERAL-MSG.                                    ELTGENRL
00178        10 FILLER              PIC X(40)    VALUE                  ELTGENRL
00179          'THERE ARE NO EXTENSION OF BENEFITS RULES'.              ELTGENRL
00180                                                                   ELTGENRL
00181      05 WS-DEPEND-GENERAL-MSG.                                    ELTGENRL
00182        10 FILLER              PIC X(34)    VALUE                  ELTGENRL
00183          'DEPENDENT COVERAGE DOES NOT APPLY.'.                    ELTGENRL
00184                                                                   ELTGENRL
00185      05 WS-FAMILY-SECURITY.                                       ELTGENRL
00186       10 FILLER              PIC X(30)   VALUE                    ELTGENRL
00187         'IN THE EVENT OF THE SUBSCRIBER'.                         ELTGENRL
00188       10 FILLER              PIC X       VALUE QUOTE.             ELTGENRL
00189       10 FILLER              PIC X(30)   VALUE                    ELTGENRL
00190         'S DEATH, THE CONTINUATION OF B'.                         ELTGENRL
00191       10 FILLER              PIC X(30)   VALUE                    ELTGENRL
00192         'ENEFITS THAT EXIST FOR THE SUR'.                         ELTGENRL
00193       10 FILLER              PIC X(20)   VALUE                    ELTGENRL
00194         'VIVING MEMBERS ARE: '.                                   ELTGENRL
00195                                                                   ELTGENRL
00196     05 FILLER REDEFINES WS-FAMILY-SECURITY.                       ELTGENRL
00197        10 WS-SEC-SENTENCE1   PIC X(79).                           ELTGENRL
00198        10 WS-SEC-SENTENCE2   PIC X(32).                           ELTGENRL
00199                                                                   ELTGENRL
00200     05 WS-FAMILY-GENERAL-MSG.                                     ELTGENRL
00201       10 FILLER              PIC X(40)    VALUE                   ELTGENRL
00202         'FAMILY SECURITY COVERAGE DOES NOT APPLY.'.               ELTGENRL
00203                                                                   ELTGENRL
00204     05 WS-TIMELY-FIL-REQ.                                         ELTGENRL
00205       10 FILLER              PIC X(37)    VALUE                   ELTGENRL
00206          'THE REQUIREMENT FOR TIMELY FILING IS '.                 ELTGENRL
00207                                                                   ELTGENRL
00208     05 WS-TIMELY-FIL-GENERAL-MSG.                                 ELTGENRL
00209       10 FILLER              PIC X(43)    VALUE                   ELTGENRL
00210         'THERE ARE NO REQUIREMENTS FOR TIMELY FILING'.            ELTGENRL
00211                                                                   ELTGENRL
00212     05  WS-ANNUAL-BEN-REINSTATE PIC X(31)   VALUE                 ELTGENRL
00213          'ANNUAL REINSTATEMENT RULES ARE '.                       ELTGENRL
00214                                                                   ELTGENRL
00215     05  WS-ANNUAL-BEN-REINSTATE-AMT PIC X(31) VALUE               ELTGENRL
00216          'ANNUAL REINSTATEMENT AMOUNT IS '.                       ELTGENRL
00217                                                                   ELTGENRL
00218     05  WS-GOOD-HEALTH-REINSTATE  PIC X(36)   VALUE               ELTGENRL
00219          'GOOD HEALTH REINSTATEMENT RULES ARE '.                  ELTGENRL
00220                                                                   ELTGENRL
00221     05  WS-GOOD-HEALTH-REINSTATE-AMT PIC X(36) VALUE              ELTGENRL
00222          'GOOD HEALTH REINSTATEMENT AMOUNT IS '.                  ELTGENRL
00223                                                                   ELTGENRL
00224     05 WS-REINSTATE-GENERAL-MSG.                                  ELTGENRL
00225       10 FILLER              PIC X(44)    VALUE                   ELTGENRL
00226         'THERE ARE NO REQUIREMENTS FOR REINSTATEMENT.'.           ELTGENRL
00227                                                                   ELTGENRL
00228   01 WS-END                    PIC X(16)  VALUE                   ELTGENRL
00229       '*** W/S ENDS ***'.                                         ELTGENRL
00230                                                                   ELTGENRL
00231 /          L I N K A G E     S E C T I O N                        ELTGENRL
00232   LINKAGE SECTION.                                                ELTGENRL
00233   01 DFHCOMMAREA.                                                 ELTGENRL
00234       COPY ELSCOMMC.                                              ELTGENRL
00235       EJECT                                                       ELTGENRL
00236       COPY ELSCIA2C.                                              ELTGENRL
00237       EJECT                                                       ELTGENRL
00238       COPY ELSCMDSC.                                              ELTGENRL
00239       EJECT                                                       ELTGENRL
00240       COPY ELSCMIFC.                                              ELTGENRL
00241       EJECT                                                       ELTGENRL
00242       COPY ELSIOPMC.                                              ELTGENRL
00243       EJECT                                                       ELTGENRL
00244       COPY ELSKEYSC.                                              ELTGENRL
00245       EJECT                                                       ELTGENRL
00246       COPY ELSOUTPC.                                              ELTGENRL
00247       EJECT                                                       ELTGENRL
00248       COPY ELSSRTPC.                                              ELTGENRL
00249       EJECT                                                       ELTGENRL
00250       COPY ELSTCWAC.                                              ELTGENRL
00251       EJECT                                                       ELTGENRL
00252       COPY ELSSSCBC.                                              ELTGENRL
00253       EJECT                                                       ELTGENRL
00254   01 GROUP-SPECIFIC-RECORD.                                       ELTGENRL
00255       COPY GCGROUPC.                                              ELTGENRL
00256                                                                   ELTGENRL
00257 /***********************************************************      ELTGENRL
00258 *                                                          *      ELTGENRL
00259 *                    PROCEDURE DIVISION                    *      ELTGENRL
00260 *                                                          *      ELTGENRL
00261 ************************************************************      ELTGENRL
00262  PROCEDURE DIVISION.                                              ELTGENRL
00263                                                                   ELTGENRL
00264                                                                   ELTGENRL
00265 ************************************************************      ELTGENRL
00266 *                                                          *      ELTGENRL
00267 *        GENERAL ADMINISTRATION RULES                      *      ELTGENRL
00268 *                                                          *      ELTGENRL
00269 ************************************************************      ELTGENRL
00270  GENERAL-ADMINISTRATION-RULES.                                    ELTGENRL
00271                                                                   ELTGENRL
00272      PERFORM 1000-INITALIZE         THRU 1000-EXIT                ELTGENRL
00273                                                                   ELTGENRL
00274      PERFORM 2000-PROCESS-MAINLINE  THRU 2000-EXIT.               ELTGENRL
00275                                                                   ELTGENRL
00276      EXEC CICS  RETURN                                            ELTGENRL
00277                 END-EXEC.                                         ELTGENRL
00278                                                                   ELTGENRL
00279 /***********************************************************      ELTGENRL
00280 *                                                          *      ELTGENRL
00281 *        INITALIZATION OF AREAS                            *      ELTGENRL
00282 *                                                          *      ELTGENRL
00283 ************************************************************      ELTGENRL
00284  1000-INITALIZE.                                                  ELTGENRL
00285                                                                   ELTGENRL
00286 ************************************************************      ELTGENRL
00287 *                                                          *      ELTGENRL
00288 *        CHECK COMMAREA LENGTH                             *      ELTGENRL
00289 *                                                          *      ELTGENRL
00290 ************************************************************      ELTGENRL
00291                                                                   ELTGENRL
00292      IF EIBCALEN IS NOT EQUAL TO LENGTH OF DFHCOMMAREA            ELTGENRL
00293         EXEC CICS ABEND                                           ELTGENRL
00294                ABCODE('EL01')                                     ELTGENRL
00295         END-EXEC                                                  ELTGENRL
00296      END-IF.                                                      ELTGENRL
00297 ************************************************************      ELTGENRL
00298 *                                                          *      ELTGENRL
00299 *        EST ADR OF COMMON INTERFACE AREA                  *      ELTGENRL
00300 *                                                          *      ELTGENRL
00301 ************************************************************      ELTGENRL
00302                                                                   ELTGENRL
00303      CALL 'ELUINISM' USING   DFHCOMMAREA                          ELTGENRL
00304          ADDRESS OF CIA-ELS-COMMON-INTERFACE-AREA.                ELTGENRL
00305                                                                   ELTGENRL
00306 ************************************************************      ELTGENRL
00307 *                                                          *      ELTGENRL
00308 *        EST ADR OF SELECTOR CONTROL AREA                  *      ELTGENRL
00309 *                                                          *      ELTGENRL
00310 ************************************************************      ELTGENRL
00311                                                                   ELTGENRL
00312      SET CIA-ELSSSCB-DDN  TO TRUE.                                ELTGENRL
00313      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00314          ADDRESS OF SSB-SELECTOR-STATUS-CTL-BLK.                  ELTGENRL
00315                                                                   ELTGENRL
00316 ************************************************************      ELTGENRL
00317 *                                                          *      ELTGENRL
00318 *        EST ADR OF CODES MANUAL INTERFACE                 *      ELTGENRL
00319 *                                                          *      ELTGENRL
00320 ************************************************************      ELTGENRL
00321                                                                   ELTGENRL
00322      SET CIA-ELSCMIF-DDN  TO TRUE.                                ELTGENRL
00323      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00324          ADDRESS OF CMF-CODES-MANUAL-INTERFACE.                   ELTGENRL
00325                                                                   ELTGENRL
00326      SET CIA-ELSOUTP-DDN  TO TRUE.                                ELTGENRL
00327      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00328          ADDRESS OF COF-OUTPUT-INTERFACE.                         ELTGENRL
00329                                                                   ELTGENRL
00330      SET CIA-ELSKEYS-DDN  TO TRUE.                                ELTGENRL
00331      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00332          ADDRESS OF KWA-FILE-KEY-WORK-AREA.                       ELTGENRL
00333                                                                   ELTGENRL
00334      SET CIA-ELSTCWA-DDN  TO TRUE.                                ELTGENRL
00335      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00336          ADDRESS OF TCAR-COMPRESSION-WORK-AREA.                   ELTGENRL
00337                                                                   ELTGENRL
00338      SET CIA-ELSGRPSP-DDN  TO TRUE.                               ELTGENRL
00339      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
00340          ADDRESS OF GROUP-SPECIFIC-RECORD.                        ELTGENRL
00341                                                                   ELTGENRL
00342  1000-EXIT.     EXIT.                                             ELTGENRL
00343 /***********************************************************      ELTGENRL
00344 *                                                          *      ELTGENRL
00345 *        PROCESS MAINLINE                                  *      ELTGENRL
00346 *                                                          *      ELTGENRL
00347 ************************************************************      ELTGENRL
00348  2000-PROCESS-MAINLINE.                                           ELTGENRL
00349                                                                   ELTGENRL
00350                                                                   ELTGENRL
00351      SET NO-ADDN-LINE-CNT TO TRUE.                                ELTGENRL
00352      MOVE WS-MASK-LINE TO COF-MASK-LINE.                          ELTGENRL
00353                                                                   ELTGENRL
00354      IF SSB-SUB-TOPIC = 'DEP'                                     ELTGENRL
00355          PERFORM 2100-DEPENDENT-COVERAGE.                         ELTGENRL
00356                                                                   ELTGENRL
00357      IF SSB-SUB-TOPIC = 'TOB'                                     ELTGENRL
00358          PERFORM 2200-EXTENSION-OF-BENEFITS.                      ELTGENRL
00359                                                                   ELTGENRL
00360      IF SSB-SUB-TOPIC = 'FAMSEC'                                  ELTGENRL
00361          PERFORM 2300-FAMILY-SECURITY-PROVN.                      ELTGENRL
00362                                                                   ELTGENRL
00363      IF SSB-SUB-TOPIC = 'TIME'                                    ELTGENRL
00364          PERFORM 2400-TIMELY-FILING.                              ELTGENRL
00365                                                                   ELTGENRL
00366      IF SSB-SUB-TOPIC = 'REIN'                                    ELTGENRL
00367          PERFORM 2500-REINSTATEMENT-IND.                          ELTGENRL
00368                                                                   ELTGENRL
00369  2000-EXIT.     EXIT.                                             ELTGENRL
00370                                                                   ELTGENRL
00371 /***********************************************************      ELTGENRL
00372 *                                                          *      ELTGENRL
00373 *        DEPENDENT COVERAGE                                *      ELTGENRL
00374 *                                                          *      ELTGENRL
00375 ************************************************************      ELTGENRL
00376  2100-DEPENDENT-COVERAGE.                                         ELTGENRL
00377                                                                   ELTGENRL
00378      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00379      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00380      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00381      PERFORM 4010-MAJOR-HEADING-RTN.                              ELTGENRL
00382                                                                   ELTGENRL
00383      MOVE WS-DEPENDANT-HDR TO WS-HEADING   (WS-OUTPUT-IDX).       ELTGENRL
00384                                                                   ELTGENRL
00385      INITIALIZE COF-DTL.                                          ELTGENRL
00386      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00387      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00388                                                                   ELTGENRL
00389      IF GCG-DEP-MAX-AGE NOT = ZERO                                ELTGENRL
00390          PERFORM 2110-DEPENDENT-MAX-AGE-TEXT.                     ELTGENRL
00391                                                                   ELTGENRL
00392      IF GCG-DEP-TERMN-IND NOT = ZERO                              ELTGENRL
00393          PERFORM 2120-DEPEN-COV-TERMN-IND-TXT.                    ELTGENRL
00394                                                                   ELTGENRL
00395      IF GCG-STU-CERTN-REQRD-IND NOT = ZERO AND 'X'                ELTGENRL
00396          PERFORM 2130-STUDENT-CERT-TXT.                           ELTGENRL
00397                                                                   ELTGENRL
00398      IF GCG-STU-MAX-AGE NOT = ZERO                                ELTGENRL
00399          PERFORM 2140-STUDENT-MAX-AGE-TEXT.                       ELTGENRL
00400                                                                   ELTGENRL
00401      IF GCG-STU-HLTH-PLAN-IND NOT = ZERO                          ELTGENRL
00402          PERFORM 2150-STUDENT-HEALTH-PLAN-TEXT.                   ELTGENRL
00403                                                                   ELTGENRL
00404      IF GCG-DEP-MAX-AGE          = ZERO            AND            ELTGENRL
00405         GCG-DEP-TERMN-IND        = ZERO            AND            ELTGENRL
00406        (GCG-STU-CERTN-REQRD-IND  = ZERO OR 'X')    AND            ELTGENRL
00407         GCG-STU-MAX-AGE          = ZERO            AND            ELTGENRL
00408         GCG-STU-HLTH-PLAN-IND    = ZERO                           ELTGENRL
00409      THEN                                                         ELTGENRL
00410         PERFORM 2160-NO-DEPENDENT-COVERAGE.                       ELTGENRL
00411                                                                   ELTGENRL
00412      PERFORM 5700-COMP-PAGE-NORM.                                 ELTGENRL
00413                                                                   ELTGENRL
00414  2100-EXIT.     EXIT.                                             ELTGENRL
00415                                                                   ELTGENRL
00416  2110-DEPENDENT-MAX-AGE-TEXT.                                     ELTGENRL
00417                                                                   ELTGENRL
00418      MOVE GCG-DEP-MAX-AGE TO DTL-DEP-MAX-AGE.                     ELTGENRL
00419      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00420      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00421                                                                   ELTGENRL
00422      MOVE WS-DEP-SENTENCE1 TO TCAR-FROM-LINE(TCAR-FROM-SUB).      ELTGENRL
00423      ADD +1                TO TCAR-FROM-SUB.                      ELTGENRL
00424      MOVE WS-DEP-SENTENCE2 TO TCAR-FROM-LINE(TCAR-FROM-SUB).      ELTGENRL
00425                                                                   ELTGENRL
00426      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00427                                                                   ELTGENRL
00428      ADD +1 TO WS-SUB.                                            ELTGENRL
00429      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00430                                                                   ELTGENRL
00431      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00432          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00433                      UNTIL TCAR-FROM-SUB GREATER THAN             ELTGENRL
00434              TCAR-OUTPUT-FIELDS-USED  OR                          ELTGENRL
00435              TCAR-FROM-SUB GREATER THAN +20.                      ELTGENRL
00436                                                                   ELTGENRL
00437  2110-EXIT.     EXIT.                                             ELTGENRL
00438                                                                   ELTGENRL
00439  2120-DEPEN-COV-TERMN-IND-TXT.                                    ELTGENRL
00440                                                                   ELTGENRL
00441      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00442      MOVE 'DEP-TERMN-IND'      TO    CMF-ELEMENT-SYSTEM-NAME.     ELTGENRL
00443      MOVE GCG-DEP-TERMN-IND    TO    CMF-CODE-VALUE.              ELTGENRL
00444      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00445                                                                   ELTGENRL
00446      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00447          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00448                    WS-SUB GREATER THAN CMF-NBR-DESCR-LINES OR     ELTGENRL
00449                    WS-SUB GREATER THAN +19.                       ELTGENRL
00450                                                                   ELTGENRL
00451      ADD +1 TO WS-SUB.                                            ELTGENRL
00452      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00453                                                                   ELTGENRL
00454      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00455                                                                   ELTGENRL
00456      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00457          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00458                      UNTIL TCAR-FROM-SUB GREATER THAN             ELTGENRL
00459              TCAR-OUTPUT-FIELDS-USED  OR                          ELTGENRL
00460              TCAR-FROM-SUB GREATER THAN +20.                      ELTGENRL
00461                                                                   ELTGENRL
00462  2120-EXIT.     EXIT.                                             ELTGENRL
00463                                                                   ELTGENRL
00464                                                                   ELTGENRL
00465  2130-STUDENT-CERT-TXT.                                           ELTGENRL
00466                                                                   ELTGENRL
00467      MOVE 'STU-CERTN-REQRD-IND'   TO CMF-ELEMENT-SYSTEM-NAME.     ELTGENRL
00468      MOVE GCG-STU-CERTN-REQRD-IND TO CMF-CODE-VALUE.              ELTGENRL
00469      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00470      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00471                                                                   ELTGENRL
00472      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00473      MOVE WS-STUDENT-CERTIFICATION  TO                            ELTGENRL
00474                            TCAR-FROM-LINE(TCAR-FROM-SUB).         ELTGENRL
00475                                                                   ELTGENRL
00476      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00477          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00478                    WS-SUB GREATER THAN CMF-NBR-DESCR-LINES.       ELTGENRL
00479                                                                   ELTGENRL
00480      ADD +1 TO WS-SUB.                                            ELTGENRL
00481      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00482                                                                   ELTGENRL
00483      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00484                                                                   ELTGENRL
00485      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00486          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00487            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00488                  TCAR-OUTPUT-FIELDS-USED     OR                   ELTGENRL
00489                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00490  2130-EXIT.     EXIT.                                             ELTGENRL
00491                                                                   ELTGENRL
00492  2140-STUDENT-MAX-AGE-TEXT.                                       ELTGENRL
00493                                                                   ELTGENRL
00494      MOVE GCG-STU-MAX-AGE TO DTL-STU-MAX-AGE.                     ELTGENRL
00495      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00496      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00497                                                                   ELTGENRL
00498      MOVE WS-STU-SENTENCE1 TO TCAR-FROM-LINE(TCAR-FROM-SUB).      ELTGENRL
00499      ADD  +1               TO TCAR-FROM-SUB.                      ELTGENRL
00500      MOVE WS-STU-SENTENCE2 TO TCAR-FROM-LINE(TCAR-FROM-SUB).      ELTGENRL
00501                                                                   ELTGENRL
00502      ADD +1 TO WS-SUB.                                            ELTGENRL
00503      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00504                                                                   ELTGENRL
00505      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00506                                                                   ELTGENRL
00507      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00508          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00509            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00510                  TCAR-OUTPUT-FIELDS-USED OR                       ELTGENRL
00511                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00512                                                                   ELTGENRL
00513  2140-EXIT.     EXIT.                                             ELTGENRL
00514                                                                   ELTGENRL
00515  2150-STUDENT-HEALTH-PLAN-TEXT.                                   ELTGENRL
00516                                                                   ELTGENRL
00517      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00518      MOVE +1                    TO TCAR-FROM-SUB.                 ELTGENRL
00519      MOVE 'STU-HLTH-PLAN-IND'   TO CMF-ELEMENT-SYSTEM-NAME.       ELTGENRL
00520      MOVE GCG-STU-HLTH-PLAN-IND TO CMF-CODE-VALUE.                ELTGENRL
00521      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00522                                                                   ELTGENRL
00523      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00524          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00525                    WS-SUB GREATER THAN CMF-NBR-DESCR-LINES.       ELTGENRL
00526                                                                   ELTGENRL
00527      ADD +1 TO WS-SUB.                                            ELTGENRL
00528      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00529                                                                   ELTGENRL
00530      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00531                                                                   ELTGENRL
00532      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00533          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00534            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00535                  TCAR-OUTPUT-FIELDS-USED OR                       ELTGENRL
00536                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00537                                                                   ELTGENRL
00538  2150-EXIT.     EXIT.                                             ELTGENRL
00539                                                                   ELTGENRL
00540  2160-NO-DEPENDENT-COVERAGE.                                      ELTGENRL
00541                                                                   ELTGENRL
00542      SET WS-OUTPUT-IDX          TO 1.                             ELTGENRL
00543      MOVE '|'                   TO WS-DIVIDER (WS-OUTPUT-IDX).    ELTGENRL
00544      MOVE WS-DEPEND-GENERAL-MSG TO WS-TEXT (WS-OUTPUT-IDX).       ELTGENRL
00545                                                                   ELTGENRL
00546      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
00547      MOVE '|'                   TO WS-DIVIDER (WS-OUTPUT-IDX).    ELTGENRL
00548                                                                   ELTGENRL
00549  2160-EXIT.     EXIT.                                             ELTGENRL
00550                                                                   ELTGENRL
00551 /***********************************************************      ELTGENRL
00552 *                                                          *      ELTGENRL
00553 *        EXTENSION OF BENEFITS                             *      ELTGENRL
00554 *                                                          *      ELTGENRL
00555 ************************************************************      ELTGENRL
00556  2200-EXTENSION-OF-BENEFITS.                                      ELTGENRL
00557                                                                   ELTGENRL
00558      IF GCG-BC-TERMN-BEN-EXTEN-IND NOT = ZERO                     ELTGENRL
00559          PERFORM 2210-IB-TERM-BEN-EXTEN-TEXT.                     ELTGENRL
00560                                                                   ELTGENRL
00561      IF GCG-BS-TERMN-BEN-EXTEN-IND NOT = ZERO                     ELTGENRL
00562          PERFORM 2220-PB-TERM-BEN-EXTEN-TEXT.                     ELTGENRL
00563                                                                   ELTGENRL
00564      IF GCG-MM-TERMN-BEN-EXTEN-IND NOT = ZERO                     ELTGENRL
00565          PERFORM 2230-SUPP-TERM-BEN-EXTEN-TEXT.                   ELTGENRL
00566                                                                   ELTGENRL
00567      IF GCG-BC-TERMN-BEN-EXTEN-IND = ZERO  AND                    ELTGENRL
00568              GCG-BS-TERMN-BEN-EXTEN-IND = ZERO  AND               ELTGENRL
00569              GCG-MM-TERMN-BEN-EXTEN-IND = ZERO                    ELTGENRL
00570          PERFORM 2240-NO-EXTENSION-RULES-TEXT.                    ELTGENRL
00571                                                                   ELTGENRL
00572      PERFORM 5900-END-OUTPUT-PAGE.                                ELTGENRL
00573                                                                   ELTGENRL
00574  2200-EXIT.     EXIT.                                             ELTGENRL
00575                                                                   ELTGENRL
00576  2210-IB-TERM-BEN-EXTEN-TEXT.                                     ELTGENRL
00577                                                                   ELTGENRL
00578      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00579      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00580      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00581                                                                   ELTGENRL
00582      SET COF-NEW-PAGE TO TRUE.                                    ELTGENRL
00583      MOVE +3 TO COF-NBR-HDR-LINES.                                ELTGENRL
00584      MOVE WS-HDR-2-IB-TERM    TO COF-HDR-LINE(2).                 ELTGENRL
00585      MOVE ALL '-' TO COF-HDR-LINE (3).                            ELTGENRL
00586      MOVE +0 TO COF-NBR-DTL-LINES.                                ELTGENRL
00587                                                                   ELTGENRL
00588      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00589      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00590                                                                   ELTGENRL
00591      PERFORM 4020-TERM-BEN-RULES.                                 ELTGENRL
00592                                                                   ELTGENRL
00593      INITIALIZE COF-DTL.                                          ELTGENRL
00594      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00595      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00596                                                                   ELTGENRL
00597      MOVE 'BC-TERMN-BEN-EXTEN-IND'   TO CMF-ELEMENT-SYSTEM-NAME.  ELTGENRL
00598      MOVE GCG-BC-TERMN-BEN-EXTEN-IND TO CMF-CODE-VALUE            ELTGENRL
00599                                         WS-TOB-IND.               ELTGENRL
00600      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00601                                                                   ELTGENRL
00602      IF NOT TOB-SPECIAL-BC-VALUE                                  ELTGENRL
00603          PERFORM 4030-PROCES-TERM-BEN-NOR                         ELTGENRL
00604      ELSE                                                         ELTGENRL
00605          PERFORM 4050-PROCESS-TERM-BEN-WO-COMP.                   ELTGENRL
00606                                                                   ELTGENRL
00607  2210-EXIT.     EXIT.                                             ELTGENRL
00608                                                                   ELTGENRL
00609  2220-PB-TERM-BEN-EXTEN-TEXT.                                     ELTGENRL
00610                                                                   ELTGENRL
00611      MOVE SPACES TO WS-TOB-IND.                                   ELTGENRL
00612      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00613                                                                   ELTGENRL
00614      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00615      SET WS-OUTPUT-IDX        TO 1.                               ELTGENRL
00616      SET COF-NEW-PAGE         TO TRUE.                            ELTGENRL
00617      MOVE +3                  TO COF-NBR-HDR-LINES.               ELTGENRL
00618      MOVE WS-HDR-2-PB-TERM    TO COF-HDR-LINE(2).                 ELTGENRL
00619      MOVE ALL '-'             TO COF-HDR-LINE (3).                ELTGENRL
00620      MOVE +0                  TO COF-NBR-DTL-LINES.               ELTGENRL
00621      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00622                                                                   ELTGENRL
00623      MOVE +1                  TO TCAR-FROM-SUB.                   ELTGENRL
00624      PERFORM 4020-TERM-BEN-RULES.                                 ELTGENRL
00625                                                                   ELTGENRL
00626      INITIALIZE COF-DTL.                                          ELTGENRL
00627      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00628      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00629                                                                   ELTGENRL
00630      MOVE 'BS-TERMN-BEN-EXTEN-IND' TO   CMF-ELEMENT-SYSTEM-NAME.  ELTGENRL
00631      MOVE GCG-BS-TERMN-BEN-EXTEN-IND TO CMF-CODE-VALUE            ELTGENRL
00632                                         WS-TOB-IND.               ELTGENRL
00633      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00634                                                                   ELTGENRL
00635      IF NOT TOB-SPECIAL-BS-VALUE                                  ELTGENRL
00636          PERFORM 4030-PROCES-TERM-BEN-NOR                         ELTGENRL
00637      ELSE                                                         ELTGENRL
00638          PERFORM 4050-PROCESS-TERM-BEN-WO-COMP.                   ELTGENRL
00639                                                                   ELTGENRL
00640  2220-EXIT.     EXIT.                                             ELTGENRL
00641                                                                   ELTGENRL
00642  2230-SUPP-TERM-BEN-EXTEN-TEXT.                                   ELTGENRL
00643                                                                   ELTGENRL
00644      MOVE SPACES TO WS-TOB-IND.                                   ELTGENRL
00645      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00646                                                                   ELTGENRL
00647      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00648      SET WS-OUTPUT-IDX          TO 1.                             ELTGENRL
00649      SET COF-NEW-PAGE           TO TRUE.                          ELTGENRL
00650      MOVE +3                    TO COF-NBR-HDR-LINES.             ELTGENRL
00651      MOVE WS-HDR-2-SUPP-TERM    TO COF-HDR-LINE(2).               ELTGENRL
00652      MOVE ALL '-'               TO COF-HDR-LINE (3).              ELTGENRL
00653      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTGENRL
00654                                                                   ELTGENRL
00655      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00656      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00657      PERFORM 4020-TERM-BEN-RULES.                                 ELTGENRL
00658                                                                   ELTGENRL
00659      INITIALIZE COF-DTL.                                          ELTGENRL
00660      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00661      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00662                                                                   ELTGENRL
00663      MOVE 'MM-TERMN-BEN-EXTEN-IND' TO   CMF-ELEMENT-SYSTEM-NAME.  ELTGENRL
00664      MOVE GCG-MM-TERMN-BEN-EXTEN-IND TO CMF-CODE-VALUE            ELTGENRL
00665                                         WS-TOB-IND.               ELTGENRL
00666      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00667                                                                   ELTGENRL
00668      IF NOT TOB-SPECIAL-MM-VALUE                                  ELTGENRL
00669          PERFORM 4030-PROCES-TERM-BEN-NOR                         ELTGENRL
00670      ELSE                                                         ELTGENRL
00671          PERFORM 4050-PROCESS-TERM-BEN-WO-COMP.                   ELTGENRL
00672  2230-EXIT.     EXIT.                                             ELTGENRL
00673                                                                   ELTGENRL
00674  2240-NO-EXTENSION-RULES-TEXT.                                    ELTGENRL
00675                                                                   ELTGENRL
00676      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00677      SET COF-NEW-PAGE           TO TRUE.                          ELTGENRL
00678      MOVE +3                    TO COF-NBR-HDR-LINES.             ELTGENRL
00679      MOVE WS-HDR-2-TERM         TO  COF-HDR-LINE(2).              ELTGENRL
00680      MOVE ALL '-'               TO COF-HDR-LINE (3).              ELTGENRL
00681      MOVE +0                    TO COF-NBR-DTL-LINES.             ELTGENRL
00682      SET WS-OUTPUT-IDX          TO 1.                             ELTGENRL
00683      MOVE '|'                   TO WS-DIVIDER (WS-OUTPUT-IDX).    ELTGENRL
00684      MOVE WS-EXTEND-GENERAL-MSG TO COF-DTL-LINE  (WS-LINE-CNT).   ELTGENRL
00685                                                                   ELTGENRL
00686      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
00687      MOVE '|'                   TO WS-DIVIDER (WS-OUTPUT-IDX).    ELTGENRL
00688      PERFORM 5800-COMPLETE-PAGE.                                  ELTGENRL
00689                                                                   ELTGENRL
00690  2240-EXIT.     EXIT.                                             ELTGENRL
00691                                                                   ELTGENRL
00692 /***********************************************************      ELTGENRL
00693 *                                                          *      ELTGENRL
00694 *        FAMILY SECURITY PROVISION                         *      ELTGENRL
00695 *                                                          *      ELTGENRL
00696 ************************************************************      ELTGENRL
00697  2300-FAMILY-SECURITY-PROVN.                                      ELTGENRL
00698                                                                   ELTGENRL
00699      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00700      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00701      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00702      PERFORM 4010-MAJOR-HEADING-RTN.                              ELTGENRL
00703                                                                   ELTGENRL
00704      MOVE WS-FAMILY-SECURITY-HDR TO WS-HEADING (WS-OUTPUT-IDX).   ELTGENRL
00705                                                                   ELTGENRL
00706      INITIALIZE COF-DTL.                                          ELTGENRL
00707      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00708      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00709                                                                   ELTGENRL
00710      IF GCG-FAM-SECUR-COV-IND NOT = ZERO AND 'X'                  ELTGENRL
00711          PERFORM 2310-FAMLY-SEC-COVERAGE-TXT                      ELTGENRL
00712      ELSE                                                         ELTGENRL
00713          PERFORM 2320-NO-FAM-SEC-COVERAGE-TX.                     ELTGENRL
00714                                                                   ELTGENRL
00715      PERFORM 5700-COMP-PAGE-NORM.                                 ELTGENRL
00716                                                                   ELTGENRL
00717  2300-EXIT.     EXIT.                                             ELTGENRL
00718                                                                   ELTGENRL
00719  2310-FAMLY-SEC-COVERAGE-TXT.                                     ELTGENRL
00720                                                                   ELTGENRL
00721      MOVE 'FAM-SECUR-COV-IND'   TO CMF-ELEMENT-SYSTEM-NAME.       ELTGENRL
00722      MOVE GCG-FAM-SECUR-COV-IND TO CMF-CODE-VALUE.                ELTGENRL
00723      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00724                                                                   ELTGENRL
00725      MOVE +1               TO TCAR-FROM-SUB.                      ELTGENRL
00726                                                                   ELTGENRL
00727      MOVE WS-SEC-SENTENCE1 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGENRL
00728      ADD +1                TO TCAR-FROM-SUB.                      ELTGENRL
00729      MOVE WS-SEC-SENTENCE2 TO TCAR-FROM-LINE (TCAR-FROM-SUB).     ELTGENRL
00730                                                                   ELTGENRL
00731      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00732          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00733                  WS-SUB GREATER THAN CMF-NBR-DESCR-LINES.         ELTGENRL
00734                                                                   ELTGENRL
00735      ADD +1 TO WS-SUB.                                            ELTGENRL
00736      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00737                                                                   ELTGENRL
00738      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00739      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00740          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00741            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00742                  TCAR-OUTPUT-FIELDS-USED    OR                    ELTGENRL
00743                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00744  2310-EXIT.     EXIT.                                             ELTGENRL
00745                                                                   ELTGENRL
00746  2320-NO-FAM-SEC-COVERAGE-TX.                                     ELTGENRL
00747                                                                   ELTGENRL
00748      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00749      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTGENRL
00750      MOVE WS-FAMILY-GENERAL-MSG TO WS-TEXT                        ELTGENRL
00751                                                                   ELTGENRL
00752          (WS-OUTPUT-IDX).                                         ELTGENRL
00753      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
00754      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTGENRL
00755                                                                   ELTGENRL
00756  2320-EXIT.     EXIT.                                             ELTGENRL
00757                                                                   ELTGENRL
00758 /***********************************************************      ELTGENRL
00759 *                                                          *      ELTGENRL
00760 *        TIMELY FILING                                     *      ELTGENRL
00761 *                                                          *      ELTGENRL
00762 ************************************************************      ELTGENRL
00763  2400-TIMELY-FILING.                                              ELTGENRL
00764                                                                   ELTGENRL
00765      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00766      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00767      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00768      PERFORM 4010-MAJOR-HEADING-RTN.                              ELTGENRL
00769      MOVE WS-TIMELY-FILING-HDR TO WS-HEADING (WS-OUTPUT-IDX).     ELTGENRL
00770                                                                   ELTGENRL
00771      IF GCG-TIMELY-FILG-IND NOT = ZERO                            ELTGENRL
00772          PERFORM 2410-TIMELY-FILING-TXT                           ELTGENRL
00773      ELSE                                                         ELTGENRL
00774          PERFORM 2420-NO-TIMELY-FILING-TEXT.                      ELTGENRL
00775                                                                   ELTGENRL
00776      PERFORM 5700-COMP-PAGE-NORM.                                 ELTGENRL
00777                                                                   ELTGENRL
00778  2400-EXIT.     EXIT.                                             ELTGENRL
00779                                                                   ELTGENRL
00780  2410-TIMELY-FILING-TXT.                                          ELTGENRL
00781                                                                   ELTGENRL
00782      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00783                                                                   ELTGENRL
00784      INITIALIZE COF-DTL.                                          ELTGENRL
00785      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00786      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00787                                                                   ELTGENRL
00788      MOVE 'TIMELY-FILG-IND'   TO CMF-ELEMENT-SYSTEM-NAME.         ELTGENRL
00789      MOVE GCG-TIMELY-FILG-IND TO CMF-CODE-VALUE.                  ELTGENRL
00790      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00791      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00792                                                                   ELTGENRL
00793      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00794      MOVE WS-TIMELY-FIL-REQ TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTGENRL
00795      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00796          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00797                  WS-SUB GREATER THAN CMF-NBR-DESCR-LINES.         ELTGENRL
00798                                                                   ELTGENRL
00799      ADD +1 TO WS-SUB.                                            ELTGENRL
00800      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00801                                                                   ELTGENRL
00802      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00803                                                                   ELTGENRL
00804      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00805          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00806            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00807                  TCAR-OUTPUT-FIELDS-USED OR                       ELTGENRL
00808                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00809                                                                   ELTGENRL
00810  2410-EXIT.     EXIT.                                             ELTGENRL
00811                                                                   ELTGENRL
00812  2420-NO-TIMELY-FILING-TEXT.                                      ELTGENRL
00813                                                                   ELTGENRL
00814      SET WS-OUTPUT-IDX              TO 1.                         ELTGENRL
00815      MOVE '|'                       TO WS-DIVIDER (WS-OUTPUT-IDX).ELTGENRL
00816      MOVE WS-TIMELY-FIL-GENERAL-MSG TO WS-TEXT (WS-OUTPUT-IDX).   ELTGENRL
00817      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
00818      MOVE '|'                       TO WS-DIVIDER (WS-OUTPUT-IDX).ELTGENRL
00819                                                                   ELTGENRL
00820  2420-EXIT.     EXIT.                                             ELTGENRL
00821                                                                   ELTGENRL
00822 /***********************************************************      ELTGENRL
00823 *                                                          *      ELTGENRL
00824 *        REINSTATEMENT                                     *      ELTGENRL
00825 *                                                          *      ELTGENRL
00826 ************************************************************      ELTGENRL
00827  2500-REINSTATEMENT-IND.                                          ELTGENRL
00828                                                                   ELTGENRL
00829      PERFORM 3000-START-NEW-PAGE.                                 ELTGENRL
00830      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
00831      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00832      PERFORM 4010-MAJOR-HEADING-RTN.                              ELTGENRL
00833                                                                   ELTGENRL
00834      MOVE WS-REINSTATEMENT TO WS-HEADING (WS-OUTPUT-IDX).         ELTGENRL
00835                                                                   ELTGENRL
00836      INITIALIZE COF-DTL.                                          ELTGENRL
00837      MOVE SPACE      TO COF-FUNCTION.                             ELTGENRL
00838      MOVE 'GROUP'    TO CMF-RECORD-PREFIX.                        ELTGENRL
00839                                                                   ELTGENRL
00840      IF GCG-ANNL-REINST-IND NOT = ZERO                            ELTGENRL
00841          PERFORM 2510-ANNUAL-REINSTATE-RULES.                     ELTGENRL
00842                                                                   ELTGENRL
00843      IF GCG-ANNL-REINST-AMT NOT = ZERO                            ELTGENRL
00844          PERFORM 2520-ANNUAL-REINSTATE-AMOUNT.                    ELTGENRL
00845                                                                   ELTGENRL
00846      IF GCG-GOOD-HLTH-REINST-IND NOT = ZERO                       ELTGENRL
00847          PERFORM 2530-GOOD-HEALTH-REIN-RULES.                     ELTGENRL
00848                                                                   ELTGENRL
00849      IF GCG-GOOD-HLTH-REINST-AFTR-BEN NOT = ZERO                  ELTGENRL
00850          PERFORM 2540-GOOD-HEALTH-REIN-AMOUNT.                    ELTGENRL
00851                                                                   ELTGENRL
00852      IF GCG-ANNL-REINST-IND           = ZERO   AND                ELTGENRL
00853         GCG-ANNL-REINST-AMT           = ZERO   AND                ELTGENRL
00854         GCG-GOOD-HLTH-REINST-IND      = ZERO   AND                ELTGENRL
00855         GCG-GOOD-HLTH-REINST-AFTR-BEN = ZERO                      ELTGENRL
00856          PERFORM 2550-NO-REINSTATEMENT-REQUIRED.                  ELTGENRL
00857                                                                   ELTGENRL
00858      PERFORM 5700-COMP-PAGE-NORM.                                 ELTGENRL
00859                                                                   ELTGENRL
00860  2500-EXIT.     EXIT.                                             ELTGENRL
00861                                                                   ELTGENRL
00862  2510-ANNUAL-REINSTATE-RULES.                                     ELTGENRL
00863                                                                   ELTGENRL
00864      MOVE 'ANNL-REINST-IND'   TO CMF-ELEMENT-SYSTEM-NAME.         ELTGENRL
00865      MOVE GCG-ANNL-REINST-IND TO CMF-CODE-VALUE.                  ELTGENRL
00866      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00867                                                                   ELTGENRL
00868      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00869      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00870      MOVE WS-ANNUAL-BEN-REINSTATE TO                              ELTGENRL
00871                     TCAR-FROM-LINE (TCAR-FROM-SUB).               ELTGENRL
00872                                                                   ELTGENRL
00873      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00874          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00875                     WS-SUB GREATER THAN                           ELTGENRL
00876              CMF-NBR-DESCR-LINES.                                 ELTGENRL
00877                                                                   ELTGENRL
00878      ADD +1 TO WS-SUB.                                            ELTGENRL
00879      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
00880      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00881                                                                   ELTGENRL
00882      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00883          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00884            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00885                  TCAR-OUTPUT-FIELDS-USED     OR                   ELTGENRL
00886                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00887                                                                   ELTGENRL
00888  2510-EXIT.     EXIT.                                             ELTGENRL
00889                                                                   ELTGENRL
00890  2520-ANNUAL-REINSTATE-AMOUNT.                                    ELTGENRL
00891                                                                   ELTGENRL
00892      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00893      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00894      MOVE GCG-ANNL-REINST-AMT TO    WS-DISPLAY-ANNL-REINST-AMT.   ELTGENRL
00895                                                                   ELTGENRL
00896      MOVE WS-ANNUAL-BEN-REINSTATE-AMT                             ELTGENRL
00897                           TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTGENRL
00898      ADD +1               TO TCAR-FROM-SUB.                       ELTGENRL
00899      MOVE  WS-DISPLAY-ANNL-REINST-AMT                             ELTGENRL
00900                           TO TCAR-FROM-LINE (TCAR-FROM-SUB).      ELTGENRL
00901                                                                   ELTGENRL
00902      ADD +1               TO WS-SUB.                              ELTGENRL
00903      MOVE '.'             TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).  ELTGENRL
00904      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00905                                                                   ELTGENRL
00906      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00907          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00908            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00909                  TCAR-OUTPUT-FIELDS-USED OR                       ELTGENRL
00910                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00911                                                                   ELTGENRL
00912  2520-EXIT.     EXIT.                                             ELTGENRL
00913                                                                   ELTGENRL
00914  2530-GOOD-HEALTH-REIN-RULES.                                     ELTGENRL
00915                                                                   ELTGENRL
00916      MOVE 'GOOD-HLTH-REINST-IND' TO   CMF-ELEMENT-SYSTEM-NAME.    ELTGENRL
00917      MOVE GCG-GOOD-HLTH-REINST-IND TO CMF-CODE-VALUE.             ELTGENRL
00918      PERFORM 6000-LINK-TO-CODES-MANUAL.                           ELTGENRL
00919      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00920                                                                   ELTGENRL
00921      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00922      MOVE WS-GOOD-HEALTH-REINSTATE TO                             ELTGENRL
00923                          TCAR-FROM-LINE(TCAR-FROM-SUB).           ELTGENRL
00924      ADD +1 TO TCAR-FROM-SUB.                                     ELTGENRL
00925                                                                   ELTGENRL
00926      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
00927          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
00928                 WS-SUB GREATER THAN CMF-NBR-DESCR-LINES.          ELTGENRL
00929                                                                   ELTGENRL
00930      ADD +1               TO WS-SUB.                              ELTGENRL
00931      MOVE '.'             TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).  ELTGENRL
00932      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00933                                                                   ELTGENRL
00934      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00935          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00936            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00937                  TCAR-OUTPUT-FIELDS-USED       OR                 ELTGENRL
00938                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00939                                                                   ELTGENRL
00940  2530-EXIT.     EXIT.                                             ELTGENRL
00941                                                                   ELTGENRL
00942  2540-GOOD-HEALTH-REIN-AMOUNT.                                    ELTGENRL
00943                                                                   ELTGENRL
00944      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
00945      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
00946      MOVE GCG-GOOD-HLTH-REINST-AFTR-BEN TO                        ELTGENRL
00947          WS-D-GOOD-HLTH-REINST-AFTR-BEN.                          ELTGENRL
00948                                                                   ELTGENRL
00949      MOVE WS-GOOD-HEALTH-REINSTATE-AMT                            ELTGENRL
00950                                 TO TCAR-FROM-LINE (TCAR-FROM-SUB).ELTGENRL
00951      ADD +1                     TO TCAR-FROM-SUB.                 ELTGENRL
00952      MOVE WS-D-GOOD-HLTH-REINST-AFTR-BEN                          ELTGENRL
00953                                 TO TCAR-FROM-LINE (TCAR-FROM-SUB).ELTGENRL
00954                                                                   ELTGENRL
00955      ADD +1               TO WS-SUB.                              ELTGENRL
00956      MOVE '.'             TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).  ELTGENRL
00957      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
00958                                                                   ELTGENRL
00959      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
00960          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
00961            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
00962                  TCAR-OUTPUT-FIELDS-USED          OR              ELTGENRL
00963                  TCAR-FROM-SUB GREATER THAN +20.                  ELTGENRL
00964                                                                   ELTGENRL
00965  2540-EXIT.     EXIT.                                             ELTGENRL
00966                                                                   ELTGENRL
00967  2550-NO-REINSTATEMENT-REQUIRED.                                  ELTGENRL
00968                                                                   ELTGENRL
00969      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
00970      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTGENRL
00971      MOVE WS-REINSTATE-GENERAL-MSG TO WS-TEXT (WS-OUTPUT-IDX).    ELTGENRL
00972                                                                   ELTGENRL
00973      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
00974      MOVE '|' TO WS-DIVIDER (WS-OUTPUT-IDX).                      ELTGENRL
00975                                                                   ELTGENRL
00976  2550-EXIT.     EXIT.                                             ELTGENRL
00977                                                                   ELTGENRL
00978 /***********************************************************      ELTGENRL
00979 *                                                          *      ELTGENRL
00980 *        PAGE EJECT                                        *      ELTGENRL
00981 *                                                          *      ELTGENRL
00982 ************************************************************      ELTGENRL
00983  3000-START-NEW-PAGE.                                             ELTGENRL
00984      MOVE 'P'  TO  COF-FUNCTION.                                  ELTGENRL
00985      MOVE ZERO  TO  COF-NBR-HDR-LINES                             ELTGENRL
00986                     COF-NBR-DTL-LINES.                            ELTGENRL
00987      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
00988      MOVE ZERO TO WS-LINE-CNT.                                    ELTGENRL
00989                                                                   ELTGENRL
00990 /***********************************************************      ELTGENRL
00991 *                                                          *      ELTGENRL
00992 *        MOVE LINES OUT                                    *      ELTGENRL
00993 *                                                          *      ELTGENRL
00994 ************************************************************      ELTGENRL
00995  3010-MOVE-LINES-OUT.                                             ELTGENRL
00996                                                                   ELTGENRL
00997      ADD +1 TO WS-LINE-CNT.                                       ELTGENRL
00998                                                                   ELTGENRL
00999      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB)  TO                       ELTGENRL
01000                    WS-TEXT(WS-OUTPUT-IDX).                        ELTGENRL
01001                                                                   ELTGENRL
01002      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
01003                                                                   ELTGENRL
01004 ************************************************************      ELTGENRL
01005 *                                                          *      ELTGENRL
01006 *        MOVE DESC TO COMPRESS AREA                        *      ELTGENRL
01007 *                                                          *      ELTGENRL
01008 ************************************************************      ELTGENRL
01009  3330-MOVE-DESC-TO-COMP-AREA.                                     ELTGENRL
01010      ADD +1 TO TCAR-FROM-SUB.                                     ELTGENRL
01011      MOVE CMF-DESCR-LINE (WS-SUB) TO                              ELTGENRL
01012          TCAR-FROM-LINE (TCAR-FROM-SUB).                          ELTGENRL
01013                                                                   ELTGENRL
01014 /***********************************************************      ELTGENRL
01015 *                                                          *      ELTGENRL
01016 *        START   TEXT COMPRESSION                          *      ELTGENRL
01017 *                                                          *      ELTGENRL
01018 ************************************************************      ELTGENRL
01019  4000-INIT-TEXT-COMPRESSION.                                      ELTGENRL
01020      INITIALIZE TCAR-FROM-SUB                                     ELTGENRL
01021                 TCAR-FROM-AREA                                    ELTGENRL
01022                 TCAR-FROM-LENGTH.                                 ELTGENRL
01023                                                                   ELTGENRL
01024 /***********************************************************      ELTGENRL
01025 *                                                          *      ELTGENRL
01026 *     MAJOR HEADING ROUTINE                                *      ELTGENRL
01027 *                                                          *      ELTGENRL
01028 ************************************************************      ELTGENRL
01029  4010-MAJOR-HEADING-RTN.                                          ELTGENRL
01030      SET COF-NEW-PAGE TO TRUE.                                    ELTGENRL
01031      MOVE +3 TO COF-NBR-HDR-LINES.                                ELTGENRL
01032                                                                   ELTGENRL
01033      IF SSB-SUB-TOPIC = 'DEP'                                     ELTGENRL
01034         MOVE WS-HDR-2-DEPENDENT TO COF-HDR-LINE (2).              ELTGENRL
01035                                                                   ELTGENRL
01036      IF SSB-SUB-TOPIC = 'FAMSEC'                                  ELTGENRL
01037         MOVE WS-HDR-2-FAMILY TO COF-HDR-LINE (2).                 ELTGENRL
01038                                                                   ELTGENRL
01039      IF SSB-SUB-TOPIC = 'TIME'                                    ELTGENRL
01040         MOVE WS-HDR-2-TIMELY-FILING TO COF-HDR-LINE (2).          ELTGENRL
01041                                                                   ELTGENRL
01042      IF SSB-SUB-TOPIC = 'REIN'                                    ELTGENRL
01043         MOVE WS-HDR-2-BENEFIT-REINSTATEMENT TO COF-HDR-LINE (2).  ELTGENRL
01044                                                                   ELTGENRL
01045      MOVE ALL '-' TO COF-HDR-LINE (3).                            ELTGENRL
01046      MOVE +0      TO COF-NBR-DTL-LINES.                           ELTGENRL
01047                                                                   ELTGENRL
01048 /***********************************************************      ELTGENRL
01049 *                                                          *      ELTGENRL
01050 *        MOVE IN TOB RULES PHRASE                          *      ELTGENRL
01051 *                                                          *      ELTGENRL
01052 ************************************************************      ELTGENRL
01053  4020-TERM-BEN-RULES.                                             ELTGENRL
01054      MOVE WS-TER-SENTENCE1  TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTGENRL
01055                                                                   ELTGENRL
01056      ADD +1 TO TCAR-FROM-SUB.                                     ELTGENRL
01057      MOVE WS-TER-SENTENCE2  TO TCAR-FROM-LINE (TCAR-FROM-SUB).    ELTGENRL
01058      COMPUTE TCAR-FROM-LENGTH = TCAR-FROM-SUB * 79.               ELTGENRL
01059      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
01060      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
01061      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
01062      MOVE WS-TOB-HEADING1 TO WS-HEADING (WS-OUTPUT-IDX).          ELTGENRL
01063      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
01064          VARYING TCAR-FROM-SUB FROM 1 BY 1 UNTIL                  ELTGENRL
01065                  TCAR-FROM-SUB GREATER THAN                       ELTGENRL
01066                  TCAR-OUTPUT-FIELDS-USED        OR                ELTGENRL
01067                  TCAR-FROM-SUB GREATER THAN 20.                   ELTGENRL
01068                                                                   ELTGENRL
01069 ************************************************************      ELTGENRL
01070 *                                                          *      ELTGENRL
01071 *        PROCESS TOB NORMALLY                              *      ELTGENRL
01072 *                                                          *      ELTGENRL
01073 ************************************************************      ELTGENRL
01074  4030-PROCES-TERM-BEN-NOR.                                        ELTGENRL
01075      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
01076      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
01077          VARYING WS-SUB FROM 1 BY 1 UNTIL                         ELTGENRL
01078                 WS-SUB GREATER THAN CMF-NBR-DESCR-LINES OR        ELTGENRL
01079                 WS-SUB GREATER THAN +13.                          ELTGENRL
01080                                                                   ELTGENRL
01081      IF CMF-NBR-DESCR-LINES NOT GREATER +18                       ELTGENRL
01082           ADD +1 TO WS-SUB                                        ELTGENRL
01083           MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).         ELTGENRL
01084                                                                   ELTGENRL
01085      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
01086                                                                   ELTGENRL
01087      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
01088          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
01089                      UNTIL TCAR-FROM-SUB GREATER THAN             ELTGENRL
01090              TCAR-OUTPUT-FIELDS-USED                              ELTGENRL
01091                      OR WS-OUTPUT-IDX GREATER THAN +18.           ELTGENRL
01092                                                                   ELTGENRL
01093      IF CMF-NBR-DESCR-LINES GREATER +18   OR                      ELTGENRL
01094               WS-OUTPUT-IDX GREATER +18                           ELTGENRL
01095          PERFORM 5200-OUTPUT-FIRST-PORTION                        ELTGENRL
01096      ELSE                                                         ELTGENRL
01097          PERFORM 5800-COMPLETE-PAGE.                              ELTGENRL
01098                                                                   ELTGENRL
01099      IF CMF-NBR-DESCR-LINES GREATER +18                           ELTGENRL
01100          PERFORM 4040-CONT-PROCESS-NORMALLY.                      ELTGENRL
01101                                                                   ELTGENRL
01102      INITIALIZE WS-ADDN-CNT.                                      ELTGENRL
01103                                                                   ELTGENRL
01104                                                                   ELTGENRL
01105 ************************************************************      ELTGENRL
01106 *                                                          *      ELTGENRL
01107 *        CONTINUE PROCESSING NORMALLY                      *      ELTGENRL
01108 *                                                          *      ELTGENRL
01109 ************************************************************      ELTGENRL
01110  4040-CONT-PROCESS-NORMALLY.                                      ELTGENRL
01111      MOVE TCAR-OPF-DATA (TCAR-FROM-SUB) TO  TCAR-HOLD-LINE.       ELTGENRL
01112                                                                   ELTGENRL
01113      COMPUTE WS-ADDN-CNT = WS-ADDN-CNT - 1.                       ELTGENRL
01114      MOVE WS-ADDN-CNT TO WS-SUB.                                  ELTGENRL
01115      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
01116                                                                   ELTGENRL
01117      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
01118      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
01119      MOVE WS-TOB-HEADING2 TO WS-HEADING (WS-OUTPUT-IDX).          ELTGENRL
01120                                                                   ELTGENRL
01121      MOVE +1 TO TCAR-FROM-SUB.                                    ELTGENRL
01122      MOVE TCAR-HOLD-LINE TO TCAR-FROM-LINE (TCAR-FROM-SUB).       ELTGENRL
01123                                                                   ELTGENRL
01124      PERFORM 3330-MOVE-DESC-TO-COMP-AREA                          ELTGENRL
01125          VARYING WS-SUB FROM WS-SUB BY 1 UNTIL                    ELTGENRL
01126                 WS-SUB GREATER THAN CMF-NBR-DESCR-LINES OR        ELTGENRL
01127                 WS-SUB GREATER THAN +36.                          ELTGENRL
01128                                                                   ELTGENRL
01129      ADD +1 TO WS-SUB.                                            ELTGENRL
01130      MOVE '.' TO TCAR-FROM-LINE-LAST-DIGIT (WS-SUB).              ELTGENRL
01131      PERFORM 5000-COMPRESS-AND-UNSTRING.                          ELTGENRL
01132                                                                   ELTGENRL
01133      PERFORM 3010-MOVE-LINES-OUT                                  ELTGENRL
01134          VARYING TCAR-FROM-SUB FROM 1 BY 1                        ELTGENRL
01135            UNTIL TCAR-FROM-SUB GREATER THAN                       ELTGENRL
01136                  TCAR-OUTPUT-FIELDS-USED     OR                   ELTGENRL
01137                  WS-OUTPUT-IDX GREATER THAN +18.                  ELTGENRL
01138                                                                   ELTGENRL
01139      PERFORM 5800-COMPLETE-PAGE.                                  ELTGENRL
01140                                                                   ELTGENRL
01141 /***********************************************************      ELTGENRL
01142 *                                                          *      ELTGENRL
01143 *        PROCESS TOB WITHOUT COMPRESS ROUTINE              *      ELTGENRL
01144 *                                                          *      ELTGENRL
01145 ************************************************************      ELTGENRL
01146  4050-PROCESS-TERM-BEN-WO-COMP.                                   ELTGENRL
01147      PERFORM 4000-INIT-TEXT-COMPRESSION.                          ELTGENRL
01148                                                                   ELTGENRL
01149      PERFORM 4070-MOVE-DES-LINES-OUT                              ELTGENRL
01150          VARYING WS-SUB FROM 1 BY 1     UNTIL                     ELTGENRL
01151                  WS-SUB GREATER THAN CMF-NBR-DESCR-LINES          ELTGENRL
01152               OR WS-OUTPUT-IDX GREATER THAN +18.                  ELTGENRL
01153                                                                   ELTGENRL
01154      IF CMF-NBR-DESCR-LINES GREATER +18                           ELTGENRL
01155                    OR WS-OUTPUT-IDX GREATER +18                   ELTGENRL
01156          PERFORM 5200-OUTPUT-FIRST-PORTION                        ELTGENRL
01157      ELSE                                                         ELTGENRL
01158          PERFORM 5800-COMPLETE-PAGE.                              ELTGENRL
01159                                                                   ELTGENRL
01160      IF CMF-NBR-DESCR-LINES GREATER +18                           ELTGENRL
01161          PERFORM 4060-CONT-WO-COMP-RTN.                           ELTGENRL
01162                                                                   ELTGENRL
01163                                                                   ELTGENRL
01164 ************************************************************      ELTGENRL
01165 *                                                          *      ELTGENRL
01166 *        CONTINUE PROCESSING WITHOUT COMPRESS RTN          *      ELTGENRL
01167 *                                                          *      ELTGENRL
01168 ************************************************************      ELTGENRL
01169  4060-CONT-WO-COMP-RTN.                                           ELTGENRL
01170                                                                   ELTGENRL
01171      INITIALIZE WS-OUTPUT-AREA.                                   ELTGENRL
01172      SET WS-OUTPUT-IDX TO 1.                                      ELTGENRL
01173      MOVE WS-TOB-HEADING2 TO WS-HEADING   (WS-OUTPUT-IDX).        ELTGENRL
01174                                                                   ELTGENRL
01175      PERFORM 4070-MOVE-DES-LINES-OUT                              ELTGENRL
01176          VARYING WS-SUB FROM WS-SUB BY 1     UNTIL                ELTGENRL
01177              WS-SUB GREATER THAN  CMF-NBR-DESCR-LINES  OR         ELTGENRL
01178              WS-OUTPUT-IDX GREATER THAN +18.                      ELTGENRL
01179                                                                   ELTGENRL
01180      PERFORM 5800-COMPLETE-PAGE.                                  ELTGENRL
01181                                                                   ELTGENRL
01182 ************************************************************      ELTGENRL
01183 *                                                          *      ELTGENRL
01184 *        MOVE DESCR LINES OUT                              *      ELTGENRL
01185 *                                                          *      ELTGENRL
01186 ************************************************************      ELTGENRL
01187  4070-MOVE-DES-LINES-OUT.                                         ELTGENRL
01188      ADD +1 TO WS-LINE-CNT.                                       ELTGENRL
01189                                                                   ELTGENRL
01190      MOVE CMF-DESCR-LINE (WS-SUB) TO  WS-TEXT(WS-OUTPUT-IDX).     ELTGENRL
01191                                                                   ELTGENRL
01192      SET WS-OUTPUT-IDX UP BY 1.                                   ELTGENRL
01193                                                                   ELTGENRL
01194                                                                   ELTGENRL
01195 /***********************************************************      ELTGENRL
01196 *                                                          *      ELTGENRL
01197 *        COMPRESS AND UNSTRING                             *      ELTGENRL
01198 *                                                          *      ELTGENRL
01199 ************************************************************      ELTGENRL
01200  5000-COMPRESS-AND-UNSTRING.                                      ELTGENRL
01201                                                                   ELTGENRL
01202      CALL 'ELUTCOMP' USING TCAR-COMPRESSION-WORK-AREA.            ELTGENRL
01203                                                                   ELTGENRL
01204 *        SETUP OUTPUT FLD LNGTH                            *      ELTGENRL
01205                                                                   ELTGENRL
01206      MOVE TCAR-TO-SUB TO TCAR-L.                                  ELTGENRL
01207      MOVE +18 TO TCAR-OUTPUT-FIELD-COUNT.                         ELTGENRL
01208      MOVE +47 TO TCAR-OUTPUT-FIELD-1-LEN                          ELTGENRL
01209                  TCAR-OUTPUT-FIELD-2-LEN                          ELTGENRL
01210                  TCAR-OUTPUT-FIELD-3-LEN                          ELTGENRL
01211                  TCAR-OUTPUT-FIELD-4-LEN                          ELTGENRL
01212                  TCAR-OUTPUT-FIELD-5-LEN                          ELTGENRL
01213                  TCAR-OUTPUT-FIELD-6-LEN                          ELTGENRL
01214                  TCAR-OUTPUT-FIELD-7-LEN                          ELTGENRL
01215                  TCAR-OUTPUT-FIELD-8-LEN                          ELTGENRL
01216                  TCAR-OUTPUT-FIELD-9-LEN                          ELTGENRL
01217                  TCAR-OUTPUT-FIELD-10-LEN                         ELTGENRL
01218                  TCAR-OUTPUT-FIELD-11-LEN                         ELTGENRL
01219                  TCAR-OUTPUT-FIELD-12-LEN                         ELTGENRL
01220                  TCAR-OUTPUT-FIELD-13-LEN                         ELTGENRL
01221                  TCAR-OUTPUT-FIELD-14-LEN                         ELTGENRL
01222                  TCAR-OUTPUT-FIELD-15-LEN                         ELTGENRL
01223                  TCAR-OUTPUT-FIELD-16-LEN                         ELTGENRL
01224                  TCAR-OUTPUT-FIELD-17-LEN                         ELTGENRL
01225                  TCAR-OUTPUT-FIELD-18-LEN.                        ELTGENRL
01226                                                                   ELTGENRL
01227      CALL 'ELUTUNST' USING TCAR-COMPRESSION-WORK-AREA.            ELTGENRL
01228                                                                   ELTGENRL
01229 /************************************************************     ELTGENRL
01230 *                                                          *      ELTGENRL
01231 *        OUTPUT FIRST PORTION                              *      ELTGENRL
01232 *                                                          *      ELTGENRL
01233 ************************************************************      ELTGENRL
01234  5200-OUTPUT-FIRST-PORTION.                                       ELTGENRL
01235      PERFORM 5300-OUTPUT-TEXT                                     ELTGENRL
01236          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTGENRL
01237                       UNTIL WS-OUTPUT-IDX > WS-LINE-CNT.          ELTGENRL
01238      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
01239      MOVE ZERO  TO  COF-NBR-DTL-LINES.                            ELTGENRL
01240                                                                   ELTGENRL
01241 ************************************************************      ELTGENRL
01242 *                                                          *      ELTGENRL
01243 *        DO OUTPUT TXT                                     *      ELTGENRL
01244 *                                                          *      ELTGENRL
01245 ************************************************************      ELTGENRL
01246  5300-OUTPUT-TEXT.                                                ELTGENRL
01247      ADD +1             TO COF-NBR-DTL-LINES.                     ELTGENRL
01248      MOVE '|'           TO WS-DIVIDER(WS-OUTPUT-IDX).             ELTGENRL
01249      MOVE WS-OUTPUT-LINE (WS-OUTPUT-IDX)                          ELTGENRL
01250                     TO COF-DTL-LINE (COF-NBR-DTL-LINES).          ELTGENRL
01251                                                                   ELTGENRL
01252 /***********************************************************      ELTGENRL
01253 *                                                          *      ELTGENRL
01254 *        COMPLETE PAGE NORMALLY                            *      ELTGENRL
01255 *                                                          *      ELTGENRL
01256 ************************************************************      ELTGENRL
01257  5700-COMP-PAGE-NORM.                                             ELTGENRL
01258      PERFORM 5300-OUTPUT-TEXT                                     ELTGENRL
01259          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTGENRL
01260                       UNTIL WS-OUTPUT-IDX > WS-LINE-CNT +         ELTGENRL
01261              1.                                                   ELTGENRL
01262      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
01263      PERFORM 5900-END-OUTPUT-PAGE.                                ELTGENRL
01264      EJECT                                                        ELTGENRL
01265                                                                   ELTGENRL
01266                                                                   ELTGENRL
01267 ************************************************************      ELTGENRL
01268 *                                                          *      ELTGENRL
01269 *        COMPLETE PAGE                                     *      ELTGENRL
01270 *                                                          *      ELTGENRL
01271 ************************************************************      ELTGENRL
01272  5800-COMPLETE-PAGE.                                              ELTGENRL
01273      PERFORM 5300-OUTPUT-TEXT                                     ELTGENRL
01274          VARYING WS-OUTPUT-IDX FROM 1 BY 1                        ELTGENRL
01275               UNTIL WS-OUTPUT-IDX > WS-LINE-CNT + 1.              ELTGENRL
01276                                                                   ELTGENRL
01277      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
01278                                                                   ELTGENRL
01279                                                                   ELTGENRL
01280 ************************************************************      ELTGENRL
01281 *                                                          *      ELTGENRL
01282 *        END OUTPUT PAGE                                   *      ELTGENRL
01283 *                                                          *      ELTGENRL
01284 ************************************************************      ELTGENRL
01285  5900-END-OUTPUT-PAGE.                                            ELTGENRL
01286      PERFORM 5995-DISPLAY-LINE-AT-END.                            ELTGENRL
01287      SET COF-END TO TRUE.                                         ELTGENRL
01288      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
01289                                                                   ELTGENRL
01290                                                                   ELTGENRL
01291 ************************************************************      ELTGENRL
01292 *                                                          *      ELTGENRL
01293 *        DISPLAY LINE AT END OF OUTPUT                     *      ELTGENRL
01294 *                                                          *      ELTGENRL
01295 ************************************************************      ELTGENRL
01296  5995-DISPLAY-LINE-AT-END.                                        ELTGENRL
01297      MOVE +1 TO COF-NBR-DTL-LINES.                                ELTGENRL
01298      MOVE ALL '-' TO COF-DTL-LINE (COF-NBR-DTL-LINES).            ELTGENRL
01299      PERFORM 6010-LINK-TO-OUTPUT.                                 ELTGENRL
01300                                                                   ELTGENRL
01301 /***********************************************************      ELTGENRL
01302 *                                                          *      ELTGENRL
01303 *        LINK TO CODES MANUAL                              *      ELTGENRL
01304 *                                                          *      ELTGENRL
01305 ************************************************************      ELTGENRL
01306  6000-LINK-TO-CODES-MANUAL.                                       ELTGENRL
01307                                                                   ELTGENRL
01308      INITIALIZE CMF-RETURN-CODE.                                  ELTGENRL
01309                                                                   ELTGENRL
01310      EXEC CICS LINK                                               ELTGENRL
01311                PROGRAM ('ELUCMIF')                                ELTGENRL
01312                COMMAREA (DFHCOMMAREA)                             ELTGENRL
01313                LENGTH   (LENGTH OF DFHCOMMAREA)                   ELTGENRL
01314          END-EXEC.                                                ELTGENRL
01315                                                                   ELTGENRL
01316      SET CIA-ELSCMDSC-DDN  TO TRUE.                               ELTGENRL
01317      CALL 'ELUSETAD' USING   DFHCOMMAREA                          ELTGENRL
01318          ADDRESS OF CMF-DESCR.                                    ELTGENRL
01319                                                                   ELTGENRL
01320                                                                   ELTGENRL
01321 /***********************************************************      ELTGENRL
01322 *                                                          *      ELTGENRL
01323 *        LINK TO OUTPUT                                    *      ELTGENRL
01324 *                                                          *      ELTGENRL
01325 ************************************************************      ELTGENRL
01326  6010-LINK-TO-OUTPUT.                                             ELTGENRL
01327      EXEC CICS LINK PROGRAM  ('ELUOUTPT')                         ELTGENRL
01328                      COMMAREA (DFHCOMMAREA)                       ELTGENRL
01329                     LENGTH   (LENGTH OF DFHCOMMAREA)              ELTGENRL
01330            END-EXEC.                                              ELTGENRL
01331                                                                   ELTGENRL
01332 /***********************************************************      ELTGENRL
01333 *                                                          *      ELTGENRL
01334 *                         STOP RUN                         *      ELTGENRL
01335 *                                                          *      ELTGENRL
01336 ************************************************************      ELTGENRL
01337  GOBACK-PARAGRAPH.                                                ELTGENRL
01338      GOBACK.                                                      ELTGENRL
